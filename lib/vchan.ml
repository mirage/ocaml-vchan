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

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type buffer_location =
  | Offset1024      (* 1024-byte ring at offset 1024 in shared page *)
  | Offset2048      (* 2048-byte ring at offset 2048 in shared page *)
  | External of int (* 2 ^^ x pages in use *)

let rec raise_to_the_power a = function
  | 0 -> 1
  | b when b > 0 -> a * (raise_to_the_power a (b - 1))
  | _ -> raise (Invalid_argument "raise_to_the_power")
let ( ^^ ) = raise_to_the_power

let length_available_at_buffer_location = function
  | Offset1024 -> 1024
  | Offset2048 -> 2048
  | External x -> 2 ^^ x * 4096

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

type state =
  | Exited
  | Connected
  | WaitingForConnection

let state_of_live = function
  | 0 -> Ok Exited
  | 1 -> Ok Connected
  | 2 -> Ok WaitingForConnection
  | n -> Error (`Bad_live n)

let live_of_state = function
  | Exited -> 0
  | Connected -> 1
  | WaitingForConnection -> 2

type read_write = Read | Write

let bit_of_read_write = function
  | Read -> 1 | Write -> 2

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
  ring_shared: Cstruct.t;     (* producer and consumer indices *)
  data: Cstruct.t;            (* the shared data buffer *)
  pages: Cstruct.t list;      (* pages to be granted *)
}

(* Vchan peers are explicitly client or servers *)
type role =
  | Server of bool            (* true if we allow reconnection *)
  | Client

(* The state of a single vchan peer *)
type t = {
  vchan_interface: Cstruct.t; (* the shared metadata *)
  role: role;
  read: ring;
  write: ring;
}

module type PAGE_ALLOCATOR = sig
  val get: ?pages_per_block:int -> unit -> Cstruct.t
  val to_pages: Cstruct.t -> Cstruct.t list
end

module Make = functor(Io_page: PAGE_ALLOCATOR) -> struct

let listen ~read_size ~write_size ~allow_reconnection =
  (* The vchan convention is that the 'server' allocates and
     shares the pages with the 'client'. Note this is the
     reverse of the xen block protocol where the frontend
     (ie the 'client') shares pages with the backend (the
     'server') If we were to re-implement the block protocol
     over vchan, we should therefore make the frontend into
     the 'server' *)

  (* Allocate and initialise the shared page *)
  let vchan_interface = Io_page.get () in
  let left = get_vchan_interface_left vchan_interface in
  set_ring_shared_cons left 0l;
  set_ring_shared_prod left 0l;
  let right = get_vchan_interface_right vchan_interface in
  set_ring_shared_cons right 0l;
  set_ring_shared_prod right 0l;
  set_vchan_interface_cli_live vchan_interface (live_of_state WaitingForConnection);
  set_vchan_interface_srv_live vchan_interface (live_of_state Connected);
  set_vchan_interface_cli_notify vchan_interface 0;
  set_vchan_interface_srv_notify vchan_interface 0;
  set_cli_notify vchan_interface Write;

  (* Initialise the payload buffers *)
  let suitable_locations requested_size =
    List.filter (fun bl -> length_available_at_buffer_location bl >= requested_size)
      legal_buffer_locations in

  (* Use the smallest amount of buffer space for read and write buffers.
     Since each of 'Offset1024' and 'Offset2048' refer to slots in the original shared page,
     read or write may use them but not both at once. If the requested size
     is very large, we'll use our maximum amount of buffer space rather than fail. *)
  let read_l, write_l = match suitable_locations read_size, suitable_locations write_size with
  (* avoid clashes for the slots in the original page *)
  | Offset1024 :: _, Offset1024 :: second_best :: _ -> Offset1024, second_best
  | Offset2048 :: _, Offset2048 :: second_best :: _ -> Offset2048, second_best
  (* clauses are redundant because Offset1024 \in list implies Offset2048 \in list etc *)
  | Offset1024 :: _, Offset1024 :: []               -> Offset1024, Offset2048 (* redundant clause *)
  | Offset2048 :: _, Offset2048 :: []               -> Offset2048, External 1 (* redundant clause *)
  | x :: _         , y :: _                         -> x, y
  | []             , y :: _                         -> max_buffer_location, y
  | x :: _         , []                             -> x, max_buffer_location
  | []             , []                             -> max_buffer_location, max_buffer_location in

  (* for the server, "write" is "right" *)
  set_vchan_interface_right_order vchan_interface (order_of_buffer_location write_l);
  set_vchan_interface_left_order  vchan_interface (order_of_buffer_location read_l);

  let allocate_buffer_locations = function
  | Offset1024 -> Cstruct.sub vchan_interface 1024 (length_available_at_buffer_location Offset1024), []
  | Offset2048 -> Cstruct.sub vchan_interface 2048 (length_available_at_buffer_location Offset2048), []
  | External n ->
    let buffer = Io_page.get ~pages_per_block:(2 ^^ n) () in
    let pages = Io_page.to_pages buffer in
    buffer, pages in

  let data, pages = allocate_buffer_locations write_l in
  let write = {
    ring_shared = get_vchan_interface_right vchan_interface;
    data;
    pages;
  } in
  let data, pages = allocate_buffer_locations read_l in
  let read = {
    ring_shared = get_vchan_interface_left vchan_interface;
    data;
    pages;
  } in

  let role = Server allow_reconnection in
  { vchan_interface; role; read; write }
end


