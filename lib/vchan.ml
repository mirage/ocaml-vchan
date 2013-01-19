(*
 * Copyright (c) 2012 Citrix Systems Inc
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

type location =
  | Offset1024      (* ring at offset 1024 in shared page *)
  | Offset2048      (* ring at offset 2048 in shared page *)
  | External of int (* 2 ^^ x pages in use *)

let location_of_order = function
  | 10 -> Ok Offset1024
  | 11 -> Ok Offset2048
  | n when n >= 12 -> Ok (External (n - 12))
  | x -> Error (`Bad_order x)

let order_of_location = function
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


type t = {
  ring: Cstruct.t;
  evtchn: int;
  server: bool;
  persistent: bool;
  blocking: bool;
}
