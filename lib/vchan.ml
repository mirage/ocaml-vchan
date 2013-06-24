(*
 * Copyright (c) 2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


module Xs = Xs_client.Client (Xs_transport_lwt_unix_client)

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply";;
external ( $ ) : ('a -> 'b) -> 'a -> 'b = "%apply"


module Int32 = struct
  include Int32

  let ( - ) = Int32.sub
end

(* GCC atomic stuff *)

external atomic_or_fetch : char -> int -> unit = "stub_atomic_or_fetch" "noalloc"
external atomic_fetch_and : char -> int -> int = "stub_atomic_fetch_and" "noalloc"

(* left is client write, server read
   right is client read, server write *)

(* XXX: the xen headers do not use __attribute__(packed) *)

(* matches xen/include/public/io/libxenvchan.h:ring_shared *)
cstruct ring_shared {
  uint32_t cons;
  uint32_t prod
} as little_endian

(* matches xen/include/public/io/libxenvchan.h:vchan_interface *)
cstruct vchan_interface {
  uint8_t left[8];  (* ring_shared *)
  uint8_t right[8]; (* ring_shared *)
  uint16_t left_order;
  uint16_t right_order;
  uint8_t cli_live;
  uint8_t srv_live;
  uint8_t cli_notify;
  uint8_t srv_notify
} as little_endian

let get_ro v = get_vchan_interface_right_order v
let get_lo v = get_vchan_interface_left_order v
let get_lp v = get_ring_shared_prod (get_vchan_interface_left v)
let set_lp v = set_ring_shared_prod (get_vchan_interface_left v)
let get_rp v = get_ring_shared_prod (get_vchan_interface_right v)
let set_rp v = set_ring_shared_prod (get_vchan_interface_right v)
let get_lc v = get_ring_shared_cons (get_vchan_interface_left v)
let set_lc v = set_ring_shared_cons (get_vchan_interface_left v)
let get_rc v = get_ring_shared_cons (get_vchan_interface_left v)
let set_rc v = set_ring_shared_cons (get_vchan_interface_left v)

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type buffer_location =
  | Offset1024      (* 1024-byte ring at offset 1024 in shared page *)
  | Offset2048      (* 2048-byte ring at offset 2048 in shared page *)
  | External of int (* 2 ^^ x pages in use *)

let length_available_at_buffer_location = function
  | Offset1024 -> 1024
  | Offset2048 -> 2048
  | External x -> 1 lsl (x + 12)

(* Any more than (External 8) will generate too many grants to fit
   in the page, if both sides attempt it. *)
let max_buffer_location = External 8

let legal_buffer_locations = [
    Offset1024; Offset2048;
    External 0; External 1; External 2; External 3; External 4;
    External 5; External 6; External 7; External 8
]

let buffer_location_of_order = function
  | 10 -> Ok Offset1024
  | 11 -> Ok Offset2048
  | n when n >= 12 -> Ok (External (n - 12))
  | x -> Error (`Bad_order x)

let order_of_buffer_location = function
  | Offset1024 -> 10
  | Offset2048 -> 11
  | External n -> n + 12

type read_write = Read | Write

let bit_of_read_write = function Read -> 1 | Write -> 2

let update get set ring f = set ring (f (get ring))

let update_cli_notify = update get_vchan_interface_cli_notify set_vchan_interface_cli_notify
let update_srv_notify = update get_vchan_interface_srv_notify set_vchan_interface_srv_notify

let set_notify update ring rdwr =
  let bit = bit_of_read_write rdwr in
  update ring (fun x -> x lor bit)

let set_cli_notify = set_notify update_cli_notify
let set_srv_notify = set_notify update_srv_notify

let clear_notify update ring rdwr =
  let bit = bit_of_read_write rdwr in
  update ring (fun x -> x land (0xff lxor bit))

let clear_cli_notify = clear_notify update_cli_notify
let clear_srv_notify = clear_notify update_srv_notify


(* A single 'ring' is used for each direction of transfer *)
type ring = {
  data: Cstruct.t;            (* the shared data buffer *)
  pages: Cstruct.t list;      (* pages to be granted *)
}

(* Vchan peers are explicitly client or servers *)
type role =
  | Server of bool            (* true if we allow reconnection *)
  | Client

module type PAGE_ALLOCATOR = sig
  val get: ?pages_per_block:int -> unit -> Cstruct.t
  val to_pages: Cstruct.t -> Cstruct.t list
end

module Make (Io_page: PAGE_ALLOCATOR) = struct

(* The state of a single vchan peer *)
type t = {
  vchan_interface: Cstruct.t; (* the shared metadata *)
  role: role;
  read: ring; (* the ring where you read data from *)
  write: ring; (* the ring where you write data to *)
  evtchn_h: Eventchn.handle; (* handler to the Eventchn interface *)
  evtchn: Eventchn.t; (* Event channel to notify the other end *)
}

let rd_prod vch = match vch.role with
  | Client -> get_rp vch.vchan_interface
  | Server _ -> get_lp vch.vchan_interface

let rd_cons vch = match vch.role with
  | Client -> get_rc vch.vchan_interface
  | Server _ -> get_lc vch.vchan_interface

let wr_prod vch = match vch.role with
  | Client -> get_lp vch.vchan_interface
  | Server _ -> get_rp vch.vchan_interface

let set_wr_prod vch v = match vch.role with
  | Client -> set_lp vch.vchan_interface v
  | Server _ -> set_rp vch.vchan_interface v

let wr_cons vch = match vch.role with
  | Client -> get_lc vch.vchan_interface
  | Server _ -> get_rc vch.vchan_interface

let wr_ring_size vch = match vch.role with
  | Client -> 1 lsl get_lo vch.vchan_interface
  | Server _ -> 1 lsl get_ro vch.vchan_interface

let rd_ring_size vch = match vch.role with
  | Client -> 1 lsl get_ro vch.vchan_interface
  | Server _ -> 1 lsl get_lo vch.vchan_interface

let request_notify vch rdwr =
  let open Cstruct in
  let idx = match vch.role with Client -> 21 | Server _ -> 22 in
  atomic_or_fetch vch.vchan_interface.buffer.{idx} (bit_of_read_write rdwr);
  Xenctrl.xen_mb ()

let send_notify vch rdwr =
  let open Cstruct in
  let idx = match vch.role with Client -> 21 | Server _ -> 22 in
  Xenctrl.xen_mb ();
  let prev =
    atomic_fetch_and vch.vchan_interface.buffer.{idx} (bit_of_read_write rdwr) in
  if prev lor (bit_of_read_write rdwr) <> 0 then Eventchn.notify vch.evtchn_h vch.evtchn

let fast_get_data_ready vch request =
  let ready = Int32.(rd_prod vch - rd_cons vch) in
  if ready >= request then ready else
    (request_notify vch Write; Int32.(rd_prod vch - rd_cons vch))

let data_ready vch =
  request_notify vch Write;
  Int32.(rd_prod vch - rd_cons vch)

let fast_get_buffer_space vch request =
  let ready = wr_ring_size vch - Int32.(wr_prod vch - wr_cons vch) in
  if ready > request then ready else
    (
      request_notify vch Read;
      wr_ring_size vch - Int32.(wr_prod vch - wr_cons vch)
    )

let buffer_space vch =
  request_notify vch Read;
  Int32.(wr_ring_size vch - (wr_prod vch - wr_cons vch))

let wait vch = Activations.wait vch.evtchn

let _write_from vch buf off len =
  let real_idx = Int32.(logand (wr_prod vch) (wr_ring_size vch - 1l) |> to_int) in
  let avail_contig = wr_ring_size vch - real_idx in
  let avail_contig = if avail_contig > len then len else avail_contig in
  Xenctrl.xen_mb ();
  Cstruct.blit_from_string buf off vch.write.data real_idx avail_contig;
  (if avail_contig < len then (* We rolled across the end of the ring *)
    Cstruct.blit_from_string buf off vch.write.data 0 (len - avail_contig));
  Xenctrl.xen_wmb ();
  set_wr_prod vch (wr_prod vch + len);
  send_notify vch Write;
  len

let rec write_from vch buf off len =
  if not is_open vch then raise Not_connected
  else
    let avail = fast_get_buffer_space vch len in
    if len <= avail then Lwt.return (_write_from vch buf off len)
    else
      if len > wr_ring_size vch then raise Ring_too_small
      else wait vch >> write_from vch buf off len

type state =
  | Exited
  | Connected
  | WaitingForConnection

exception Not_connected of state
exception Ring_too_small

let state_of_live = function
  | 0 -> Ok Exited
  | 1 -> Ok Connected
  | 2 -> Ok WaitingForConnection
  | n -> Error (`Bad_live n)

let live_of_state = function
  | Exited -> 0
  | Connected -> 1
  | WaitingForConnection -> 2

let server ~domid ~xs_path ~read_size ~write_size ~allow_reconnection =
  (* The vchan convention is that the 'server' allocates and
     shares the pages with the 'client'. Note this is the
     reverse of the xen block protocol where the frontend
     (ie the 'client') shares pages with the backend (the
     'server') If we were to re-implement the block protocol
     over vchan, we should therefore make the frontend into
     the 'server' *)

  (* Allocate and initialise the shared page *)
  let v = Io_page.get () in
  set_lc v 0l;
  set_lp v 0l;
  set_rc v 0l;
  set_lc v 0l;
  set_vchan_interface_cli_live v (live_of_state WaitingForConnection);
  set_vchan_interface_srv_live v (live_of_state Connected);
  set_vchan_interface_cli_notify v 0;
  set_vchan_interface_srv_notify v 0;
  set_cli_notify v Write;

  (* Initialise the payload buffers *)
  let suitable_locations requested_size =
    let locs = List.filter
      (fun bl -> length_available_at_buffer_location bl >= requested_size)
      legal_buffer_locations in
    match locs with
      | []     -> max_buffer_location
      | h::t   -> h in
  (* Use the smallest amount of buffer space for read and write buffers.
     Since each of 'Offset1024' and 'Offset2048' refer to slots in the original shared page,
     read or write may use them but not both at once. If the requested size
     is very large, we'll use our maximum amount of buffer space rather than fail. *)
  let read_l, write_l = match suitable_locations read_size, suitable_locations write_size with
  (* avoid clashes for the slots in the original page *)
  | Offset1024, Offset1024 -> Offset1024, Offset2048
  | Offset2048, Offset1024 -> Offset2048, Offset1024
  | Offset1024, Offset2048 -> Offset1024, Offset2048
  | Offset2048, Offset2048 -> Offset2048, External 0
  | n                      -> n in

  set_vchan_interface_right_order v (order_of_buffer_location read_l);
  set_vchan_interface_left_order v (order_of_buffer_location write_l);

  let allocate_buffer_locations = function
  | Offset1024 -> Cstruct.sub v 1024 (length_available_at_buffer_location Offset1024), []
  | Offset2048 -> Cstruct.sub v 2048 (length_available_at_buffer_location Offset2048), []
  | External n ->
    let buffer = Io_page.get ~pages_per_block:(1 lsl n) () in
    let pages = Io_page.to_pages buffer in
    buffer, pages in

  let data, pages = allocate_buffer_locations write_l in
  let write = { data; pages } in
  let data, pages = allocate_buffer_locations read_l in
  let read = { data; pages } in

  (* Allocate the event channel *)
  let evtchn_h = Eventchn.init () in
  let evtchn = Eventchn.bind_unbound_port evtchn_h domid in

  (* Write the config to XenStore *)

  let role = Server allow_reconnection in
  { vchan_interface=v; role; read; write; evtchn_h; evtchn }

let ( >>= ) = Lwt.bind

let client ~domid ~xs_path =
  let get_gntref_and_evtchn () =
    Xs.make ()
    >>= fun xs_cli ->
    Xs.(wait xs_cli
          (fun xsh -> directory xsh xs_path >>= function
            | [a; b] ->     read xsh (xs_path ^ "ring-ref")
                        >>= fun rref -> read xsh (xs_path ^ "event-channel")
                        >>= fun evtchn -> Lwt.return (rref, evtchn)
            | _ -> Lwt.fail Xs_protocol.Eagain))
    >>= fun (gntref, evtchn) ->
    Lwt.return (Gnt.grant_table_index_of_string gntref, int_of_string evtchn)
  in
  get_gntref_and_evtchn () >>= fun (gntref, evtchn) ->
  (* Map the vchan interface page *)
  let gnt_iface = Gnt.Gnttab.interface_open () in
  let mapping = Gnt.Gnttab.(map gnt_iface { domid; ref=gntref } true) in
  let vchan_intf_page = match mapping with
    | None -> raise (Failure "Unable to map the vchan interface page.")
    | Some p -> Gnt.Gnttab.Local_mapping.to_buf p in
  let vchan_intf_cstruct = Cstruct.of_bigarray vchan_intf_page in

  (* Bind the event channel *)
  let evtchn_h = Eventchn.init () in
  let evtchn = Eventchn.bind_interdomain evtchn_h domid evtchn in

  (* Map the rings *)
  let rings_of_vchan_intf v =
    let lo = get_lo v in
    let ro = get_ro v in
    match lo, ro with
      | 10, 10 ->
        { data=Cstruct.sub v 1024 1024; pages=[] },
        { data=Cstruct.sub v 2048 1024; pages=[] }

      | 10, 11 ->
        { data=Cstruct.sub v 1024 1024; pages=[] },
        { data=Cstruct.sub v 2048 2048; pages=[] }

      | 11, 10 ->
        { data=Cstruct.sub v 2048 2048; pages=[] },
        { data=Cstruct.sub v 1024 1024; pages=[] }

      | 11, 11 ->
        let gntref = Gnt.grant_table_index_of_int32
          (Cstruct.LE.get_uint32 vchan_intf_cstruct sizeof_vchan_interface) in
        (match Gnt.Gnttab.(map gnt_iface { domid; ref=gntref } true) with
          | None -> failwith "Gnttab.map"
          | Some p -> let p = Gnt.Gnttab.Local_mapping.to_buf p in
            { data=Cstruct.sub v 2048 2048; pages=[] },
            { data=Cstruct.of_bigarray p; pages=[] })

      | n, m when n > 9 && m > 9 ->
        let pages_to_map order = 1 lsr (order - 12) in
        let lpages_nb = pages_to_map lo in
        let rpages_nb = pages_to_map ro in
        let lgrants = Array.make lpages_nb 0l in
        let rgrants = Array.make rpages_nb 0l in
        let lgrants =
          for i = 0 to lpages_nb - 1 do
            lgrants.(i) <- Cstruct.LE.get_uint32 vchan_intf_cstruct (sizeof_vchan_interface + i*4)
          done; List.map
            (fun gntref -> Gnt.Gnttab.({ domid; ref=Gnt.grant_table_index_of_int32 gntref }))
              (Array.to_list lgrants) in
        let rgrants =
          for i = 0 to rpages_nb - 1 do
            rgrants.(i) <- Cstruct.LE.get_uint32 vchan_intf_cstruct (sizeof_vchan_interface + lpages_nb*4 + i*4)
          done;  List.map
            (fun gntref -> Gnt.Gnttab.({ domid; ref=Gnt.grant_table_index_of_int32 gntref }))
              (Array.to_list rgrants) in
        let lgrants_pages = (match Gnt.Gnttab.(mapv gnt_iface lgrants true) with
          | None -> raise (Failure "Unable to map the left pages.")
          | Some p -> Cstruct.of_bigarray (Gnt.Gnttab.Local_mapping.to_buf p)) in
        let rgrants_pages = (match Gnt.Gnttab.(mapv gnt_iface lgrants true) with
          | None -> raise (Failure "Unable to map the right pages.")
          | Some p -> Cstruct.of_bigarray (Gnt.Gnttab.Local_mapping.to_buf p)) in
        { data=lgrants_pages; pages=Io_page.to_pages lgrants_pages },
        { data=rgrants_pages; pages=Io_page.to_pages rgrants_pages }

      | n, m -> failwith (Printf.sprintf "Invalid orders: left = %d, right = %d" lo ro) in

  let write, read = rings_of_vchan_intf vchan_intf_cstruct in
  Lwt.return { vchan_interface=vchan_intf_cstruct; role=Client; read; write; evtchn_h; evtchn }

(* TODO: Properly unallocate shared pages here. *)
let close vch = Lwt.return ()

let is_open vch =
  let client_state =
    match state_of_live (get_vchan_interface_cli_live vch.vchan_interface)
    with Ok st -> st | _ -> raise (Invalid_argument "cli_live")
  and server_state =
    match state_of_live (get_vchan_interface_srv_live vch.vchan_interface)
    with Ok st -> st | _ -> raise (Invalid_argument "srv_live") in
  match vch.role with
  | Server persist -> if persist then Connected else client_state
  | Client -> server_state


end
