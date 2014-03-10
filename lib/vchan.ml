(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply";;
external ( $ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let ( >>= ) = Lwt.bind

let i_int (i:int) = ignore i

module Int32 = struct
  include Int32

  let ( - ) = Int32.sub
  let ( + ) = Int32.add
end

module Opt = struct
  let map f o = match o with Some e -> Some (f e) | None -> None
  let iter f o = match o with Some e -> f e | None -> ()
end

(* GCC atomic stuff *)

external atomic_or_fetch : Cstruct.buffer -> int -> int -> int = "stub_atomic_or_fetch_uint8"
external atomic_fetch_and : Cstruct.buffer -> int -> int -> int = "stub_atomic_fetch_and_uint8"

open Vchan_wire

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

let bit_of_read_write = function Read -> 2 | Write -> 1

type server_params =
  {
    persist: bool;
    gntshr_h: Gnt.Gntshr.interface;
    shr_shr: Gnt.Gntshr.share;
    read_shr: Gnt.Gntshr.share option;
    write_shr: Gnt.Gntshr.share option
  }

type client_params =
  {
    gnttab_h: Gnt.Gnttab.interface;
    shr_map: Gnt.Gnttab.Local_mapping.t;
    read_map: Gnt.Gnttab.Local_mapping.t option;
    write_map: Gnt.Gnttab.Local_mapping.t option
  }

(* Vchan peers are explicitly client or servers *)
type role =
  (* true if we allow reconnection *)
  | Server of server_params
  | Client of client_params

module Make (Xs: Xs_client_lwt.S)(A:Vchan_t.ACTIVATIONS) = struct

  (* The state of a single vchan peer *)
  type t = {
    shared_page: Cstruct.t; (* the shared metadata *)
    role: role;
    read: Cstruct.t; (* the ring where you read data from *)
    write: Cstruct.t; (* the ring where you write data to *)
    evtchn_h: Eventchn.handle; (* handler to the Eventchn interface *)
    evtchn: Eventchn.t; (* Event channel to notify the other end *)
    mutable last_event: A.event;
  }

  type state =
    | Exited
    | Connected
    | WaitingForConnection

  exception Not_connected of state

  let state_of_live = function
    | 0 -> Ok Exited
    | 1 -> Ok Connected
    | 2 -> Ok WaitingForConnection
    | n -> Error (`Bad_live n)

  let live_of_state = function
    | Exited -> 0
    | Connected -> 1
    | WaitingForConnection -> 2

  let rd_prod vch = match vch.role with
    | Client _ -> get_rp vch.shared_page
    | Server _ -> get_lp vch.shared_page

  let rd_cons vch = match vch.role with
    | Client _ -> get_rc vch.shared_page
    | Server _ -> get_lc vch.shared_page

  let set_rd_cons vch = match vch.role with
    | Client _ -> set_rc vch.shared_page
    | Server _ -> set_lc vch.shared_page

  let wr_prod vch = match vch.role with
    | Client _ -> get_lp vch.shared_page
    | Server _ -> get_rp vch.shared_page

  let set_wr_prod vch v = match vch.role with
    | Client _ -> set_lp vch.shared_page v
    | Server _ -> set_rp vch.shared_page v

  let wr_cons vch = match vch.role with
    | Client _ -> get_lc vch.shared_page
    | Server _ -> get_rc vch.shared_page

  let wr_ring_size vch = match vch.role with
    | Client _ -> 1 lsl get_lo vch.shared_page
    | Server _ -> 1 lsl get_ro vch.shared_page

  let rd_ring_size vch = match vch.role with
    | Client _ -> 1 lsl get_ro vch.shared_page
    | Server _ -> 1 lsl get_lo vch.shared_page

  (* Request notify to the other endpoint. If client, request to server,
     and vice versa. *)
  let request_notify vch rdwr =
    let open Cstruct in
    (* This should be correct: client -> srv_notify | server -> cli_notify *)
    let idx = match vch.role with Client _ -> 23 | Server _ -> 22 in
    i_int (atomic_or_fetch vch.shared_page.buffer idx (bit_of_read_write rdwr))
  (* Xenctrl.xen_mb () *)

  let send_notify vch rdwr =
    let open Cstruct in
    (*  Xenctrl.xen_mb (); *)
    (* This should be correct: client -> cli_notify | server -> srv_notify *)
    let idx = match vch.role with Client _ -> 22 | Server _ -> 23 in
    let bit = bit_of_read_write rdwr in
    let prev =
      (* clear the bit and return previous value *)
      atomic_fetch_and vch.shared_page.buffer idx (lnot bit) in
    if prev land bit <> 0 then Eventchn.notify vch.evtchn_h vch.evtchn

  let fast_get_data_ready vch request =
    let ready = Int32.(rd_prod vch - rd_cons vch |> to_int) in
    if ready >= request then ready else
      (request_notify vch Write; Int32.(rd_prod vch - rd_cons vch |> to_int))

  let data_ready vch =
    request_notify vch Write;
    Int32.(rd_prod vch - rd_cons vch |> to_int)

  let fast_get_buffer_space vch request =
    let ready = wr_ring_size vch - Int32.(wr_prod vch - wr_cons vch |> to_int) in
    if ready > request then ready else
      (
        request_notify vch Read;
        wr_ring_size vch - Int32.(wr_prod vch - wr_cons vch |> to_int)
      )

  let buffer_space vch =
    request_notify vch Read;
    wr_ring_size vch - Int32.(wr_prod vch - wr_cons vch |> to_int)

  let state vch =
    let client_state =
      match state_of_live (get_vchan_interface_cli_live vch.shared_page)
      with Ok st -> st | _ -> raise (Invalid_argument "cli_live")
    and server_state =
      match state_of_live (get_vchan_interface_srv_live vch.shared_page)
      with Ok st -> st | _ -> raise (Invalid_argument "srv_live") in
    match vch.role with
    | Server { persist } -> if persist then Connected else client_state
    | Client _ -> server_state

  let _write_unsafe vch buf off len =
    let real_idx = Int32.(logand (wr_prod vch) (of_int (wr_ring_size vch) - 1l) |> to_int) in
    let avail_contig = wr_ring_size vch - real_idx in
    let avail_contig = if avail_contig > len then len else avail_contig in
    (*  Xenctrl.xen_mb (); *)
    Cstruct.blit_from_string buf off vch.write real_idx avail_contig;
    (if avail_contig < len then (* We rolled across the end of the ring *)
       Cstruct.blit_from_string buf (off + avail_contig) vch.write 0 (len - avail_contig));
    (*  Xenctrl.xen_wmb (); *)
    set_wr_prod vch Int32.(wr_prod vch + of_int len);
    send_notify vch Write;
    len

  let wait vch =
    A.after vch.evtchn vch.last_event
    >>= fun next ->
    vch.last_event <- next;
    Lwt.return ()

  let vchan_is_connected vch =
    match state vch with
    | Connected -> ()
    | x -> raise (Not_connected x)

  let rec write_from_exactly vch buf off len =
    vchan_is_connected vch;
    let avail = fast_get_buffer_space vch len in
    if len <= avail then Lwt.return (_write_unsafe vch buf off len |> i_int)
    else
    if len > wr_ring_size vch then raise End_of_file
    else wait vch >>= fun () -> write_from_exactly vch buf off len

  let write_from vch buf off len =
    vchan_is_connected vch;
    let rec inner pos =
      let avail = fast_get_buffer_space vch (len - pos) in
      let avail = if pos + avail > len then len - pos else avail in
      let pos = if avail > 0 then pos + _write_unsafe vch buf pos avail else pos in
      if pos = len then Lwt.return pos else wait vch >>= fun () -> inner pos
    in inner 0

  let write vch buf =
    let buflen = String.length buf in
    write_from vch buf 0 buflen >>= fun len -> Lwt.return (i_int len)

  (* This is unsafe because:

     - [len] is not checked at all, if it is bigger than the size of the
      ring the read will be corrupted.

     - There is no notion of "data ready" at all, this function just
       reads the content of the ring.

     - No check is performed to know if the connection is in the
       Connected state or not.

  *)
  let _read_unsafe_into vch buf off len =
    let real_idx = Int32.(logand (rd_cons vch) (of_int (rd_ring_size vch) - 1l) |> to_int) in
    let avail_contig = rd_ring_size vch - real_idx in
    let avail_contig = if avail_contig > len then len else avail_contig in
    (* Xenctrl.xen_rmb (); *)
    Cstruct.blit_to_string vch.read real_idx buf off avail_contig;
    (if avail_contig < len then
       Cstruct.blit_to_string vch.read 0 buf (off + avail_contig) (len - avail_contig)
    );
    (* Xenctrl.xen_mb (); *)
    set_rd_cons vch Int32.(rd_cons vch + of_int len);
    send_notify vch Read;
    len

  (* Reads exactly [len] bytes if [len < sizeof ring], raises
     End_of_file otherwise. *)
  let rec read_into_exactly vch buf off len =
    if state vch <> Connected then raise (Not_connected (state vch))
    else
      let avail = fast_get_data_ready vch len in
      if len <= avail then Lwt.return (_read_unsafe_into vch buf off len |> i_int)
      else
      if len > rd_ring_size vch then raise End_of_file
      else wait vch >>= fun () -> read_into_exactly vch buf off len

  (* Reads as much as it is possible, and return the number of bytes
     read. *)
  let rec read_into vch buf off len =
    if state vch <> Connected then raise (Not_connected (state vch))
    else
      let avail = fast_get_data_ready vch len in
      if avail = 0 then
        wait vch >>= fun () -> read_into vch buf off len
      else
        let len = if len > avail then avail else len in
        Lwt.return (_read_unsafe_into vch buf off len)

  let read vch count =
    let buf = String.create count in
    read_into vch buf 0 count >>= fun len -> Lwt.return (String.sub buf 0 len)

  let server ~evtchn_h ~domid ~xs_path ~read_size ~write_size ~persist =
    (* The vchan convention is that the 'server' allocates and
       shares the pages with the 'client'. Note this is the
       reverse of the xen block protocol where the frontend
       (ie the 'client') shares pages with the backend (the
       'server') If we were to re-implement the block protocol
       over vchan, we should therefore make the frontend into
       the 'server' *)

    (* Allocate and initialise the shared page *)
    let gntshr_h = Gnt.Gntshr.interface_open () in
    let shr_shr = Gnt.Gntshr.share_pages_exn gntshr_h domid 1 true in
    let v = Cstruct.of_bigarray Gnt.Gntshr.(shr_shr.mapping) in
    set_lc v 0l;
    set_lp v 0l;
    set_rc v 0l;
    set_lc v 0l;
    set_vchan_interface_cli_live v (live_of_state WaitingForConnection);
    set_vchan_interface_srv_live v (live_of_state Connected);
    set_vchan_interface_cli_notify v (bit_of_read_write Write);
    set_vchan_interface_srv_notify v 0;

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
    Printf.printf "Server: right_order = %d, left_order = %d\n%!"
      (order_of_buffer_location read_l) (order_of_buffer_location write_l);

    let allocate_buffer_locations = function
      | Offset1024 -> None, Cstruct.sub v 1024 (length_available_at_buffer_location Offset1024)
      | Offset2048 -> None, Cstruct.sub v 2048 (length_available_at_buffer_location Offset2048)
      | External n ->
        let share = Gnt.Gntshr.share_pages_exn gntshr_h domid (1 lsl n) true in
        let pages = Gnt.Gntshr.(share.mapping) in
        List.iter (fun r ->
            Printf.printf "allocate_buffer_locations: gntref = %d\n%!" r)
          Gnt.Gntshr.(share.refs);
        Some share, Cstruct.of_bigarray pages in

    let read_shr, read_buf = allocate_buffer_locations read_l in
    let write_shr, write_buf = allocate_buffer_locations write_l in
    let nb_read_pages = (match read_shr with None -> 0 | Some shr -> List.length Gnt.Gntshr.(shr.refs)) in
    let nb_write_pages = (match write_shr with None -> 0 | Some shr -> List.length Gnt.Gntshr.(shr.refs)) in

    (* Write the gntrefs to the shared page. Ordering is left, right. *)
    List.iteri
      (fun i ref ->
         Cstruct.LE.set_uint32 v (sizeof_vchan_interface+i*4) (Int32.of_int ref))
      (match read_shr with None -> [] | Some shr -> Gnt.Gntshr.(shr.refs));

    List.iteri
      (fun i ref ->
         Cstruct.LE.set_uint32 v (sizeof_vchan_interface+(i+nb_read_pages)*4)
           (Int32.of_int ref))
      (match write_shr with None -> [] | Some shr -> Gnt.Gntshr.(shr.refs));

    (* Allocate the event channel *)
    let evtchn = Eventchn.bind_unbound_port evtchn_h domid in

    (* Write the config to XenStore *)
    let ring_ref = Gnt.(string_of_int (List.hd shr_shr.Gntshr.refs)) in
    Xs.make ()
    >>= fun c ->
    Printf.printf "Writing config into the XenStore\n%!";
    Xs.(immediate c
          (fun h ->
             mkdir h xs_path >>= fun () ->
             write h (xs_path ^ "/ring-ref") ring_ref >>= fun () ->
             write h (xs_path ^ "/event-channel") (string_of_int (Eventchn.to_int evtchn))
          ))
    >>= fun () ->

    (* Return the shared structure *)
    Printf.printf "Shared page is:\n%!";
    Cstruct.hexdump (Cstruct.sub v 0 (sizeof_vchan_interface+4*(nb_read_pages+nb_write_pages)));

    let role = Server { gntshr_h; persist; shr_shr; read_shr; write_shr } in
    let last_event = A.program_start in
    Lwt.return { shared_page=v; role; read=read_buf; write=write_buf; 
                 evtchn_h; evtchn; last_event }

  let client ~evtchn_h ~domid ~xs_path =
    let get_gntref_and_evtchn () =
      Xs.make ()
      >>= fun xs_cli ->
      Xs.(wait xs_cli
            (fun xsh -> directory xsh xs_path >>= function
               | [a; b] -> read xsh (xs_path ^ "/ring-ref")
                 >>= fun rref -> read xsh (xs_path ^ "/event-channel")
                 >>= fun evtchn -> Lwt.return (rref, evtchn)
               | _ -> Lwt.fail Xs_protocol.Eagain))
      >>= fun (gntref, evtchn) ->
      Lwt.return (int_of_string gntref, int_of_string evtchn)
    in
    get_gntref_and_evtchn () >>= fun (gntref, evtchn) ->

    (* Map the vchan interface page *)
    Printf.printf "Client initializing: Received gntref = %s, evtchn = %d\n%!"
      (string_of_int gntref) evtchn;
    let gnttab_h = Gnt.Gnttab.interface_open () in
    let mapping = Gnt.Gnttab.(map_exn gnttab_h { domid; ref=gntref } true) in
    let vchan_intf_cstruct = Cstruct.of_bigarray
        (Gnt.Gnttab.Local_mapping.to_buf mapping) in

    (* Resizing vchan_intf_cstruct *)
    let nb_left_pages = 1 lsl (get_lo vchan_intf_cstruct - 12) in
    let nb_right_pages = 1 lsl (get_ro vchan_intf_cstruct - 12) in
    let vchan_intf_cstruct = Cstruct.sub vchan_intf_cstruct 0
        (sizeof_vchan_interface+4*(nb_left_pages+nb_right_pages)) in
    Printf.printf "Mapped the ring shared page:\n%!";
    Cstruct.hexdump vchan_intf_cstruct;

    (* Set initial values in vchan_intf *)
    set_vchan_interface_cli_live vchan_intf_cstruct 1;
    set_vchan_interface_srv_notify vchan_intf_cstruct (bit_of_read_write Write);

    (* Bind the event channel *)
    let evtchn = Eventchn.bind_interdomain evtchn_h domid evtchn in
    Printf.printf "Correctly bound evtchn number %d\n%!" (Eventchn.to_int evtchn);

    (* Map the rings *)
    let rings_of_vchan_intf v =
      let lo = get_lo v in
      let ro = get_ro v in
      match lo, ro with
      | 10, 10 -> (None, Cstruct.sub v 1024 1024), (None, Cstruct.sub v 2048 1024)
      | 10, 11 -> (None, Cstruct.sub v 1024 1024), (None, Cstruct.sub v 2048 2048)
      | 11, 10 -> (None, Cstruct.sub v 2048 2048), (None, Cstruct.sub v 1024 1024)
      | 11, 11 ->
        let gntref =
          (Cstruct.LE.get_uint32 vchan_intf_cstruct sizeof_vchan_interface |> Int32.to_int) in
        let mapping = Gnt.Gnttab.(map_exn gnttab_h { domid; ref=gntref } true) in
        (None, Cstruct.sub v 2048 2048),
        (Some mapping, Cstruct.of_bigarray (Gnt.Gnttab.Local_mapping.to_buf mapping))

      | n, m when n > 9 && m > 9 ->
        let lgrants = Array.make nb_left_pages 0 in
        let rgrants = Array.make nb_right_pages 0 in

        (* Reading grant refs from the shared page and load them into
           the arrays just created. *)
        let lgrants =
          for i = 0 to nb_left_pages - 1 do
            lgrants.(i) <- Cstruct.LE.get_uint32 vchan_intf_cstruct
                (sizeof_vchan_interface + i*4) |> Int32.to_int
          done;
          List.map (fun ref -> Gnt.Gnttab.({ domid; ref }))
            (Array.to_list lgrants) in
        let rgrants =
          for i = 0 to nb_right_pages - 1 do
            rgrants.(i) <- Cstruct.LE.get_uint32 vchan_intf_cstruct
                (sizeof_vchan_interface + nb_left_pages*4 + i*4) |> Int32.to_int
          done;
          List.map (fun ref -> Gnt.Gnttab.({ domid; ref }))
            (Array.to_list rgrants) in

        (* Use the grant refs arrays to map left and right pages. *)
        let lgrants_map =
          let mapping = Gnt.Gnttab.(mapv_exn gnttab_h lgrants true) in
          (Some mapping, Cstruct.of_bigarray (Gnt.Gnttab.Local_mapping.to_buf mapping)) in

        let rgrants_map =
          let mapping = Gnt.Gnttab.(mapv_exn gnttab_h rgrants true) in
          (Some mapping, Cstruct.of_bigarray (Gnt.Gnttab.Local_mapping.to_buf mapping)) in
        lgrants_map, rgrants_map

      | n, m -> failwith (Printf.sprintf "Invalid orders: left = %d, right = %d" lo ro) in

    let (w_map, w_buf), (r_map, r_buf) = rings_of_vchan_intf vchan_intf_cstruct in
    let role = Client { gnttab_h; shr_map=mapping; read_map=r_map; write_map=w_map } in
    Lwt.return { shared_page=vchan_intf_cstruct; role; read=r_buf; write=w_buf;
                 evtchn_h; evtchn; last_event=A.program_start }

  let close vch =
    (* C impl. notify before shutting down the event channel
       interface. *)
    Eventchn.notify vch.evtchn_h vch.evtchn;
    i_int (Eventchn.close vch.evtchn_h);
    match vch.role with
    | Client { gnttab_h; shr_map; read_map; write_map } ->
      Opt.iter (Gnt.Gnttab.unmap_exn gnttab_h) read_map;
      Opt.iter (Gnt.Gnttab.unmap_exn gnttab_h) write_map;
      Gnt.Gnttab.unmap_exn gnttab_h shr_map;
      Gnt.Gnttab.interface_close gnttab_h

    | Server { gntshr_h; shr_shr; read_shr; write_shr } ->
      Opt.iter (Gnt.Gntshr.munmap_exn gntshr_h) read_shr;
      Opt.iter (Gnt.Gntshr.munmap_exn gntshr_h) write_shr;
      Gnt.Gntshr.munmap_exn gntshr_h shr_shr;
      Gnt.Gntshr.interface_close gntshr_h

end
