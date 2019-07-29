(*
 * Copyright (C) Citrix Systems Inc.
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

open Sexplib.Std

(* FIXME: This should probably be pushed into xen-evtchn *)

open Lwt

type port = int [@@deriving sexp_of]

let port_of_string x = Ok (int_of_string x)
let string_of_port = string_of_int

let next_port = ref 0

type event = int [@@deriving sexp_of]
let initial = 0

module Lwt_condition = struct
  include Lwt_condition
  type _t = unit [@@deriving sexp]
  let sexp_of_t _ _ = sexp_of__t ()
end
type state =
  | Unbound
  | Closed
  | ConnectedTo of channel
and channel = {
  mutable events: event; (* incremented on send *)
  c: unit Lwt_condition.t;
  mutable state: state;
  port: port;
} [@@deriving sexp_of]

let create () =
  let port = !next_port in
  incr next_port;
  let events = initial in
  let c = Lwt_condition.create () in
  let state = Unbound in
  { events; c; state; port }

let rec recv channel event =
  if channel.events > event
  then return channel.events
  else
    Lwt_condition.wait channel.c >>= fun () ->
    recv channel event

let send channel = match channel.state with
  | Unbound ->
    (* This should never happen. This means there must be
       a protocol bug. *)
    failwith "send: channel is unbound"
  | Closed ->
    (* This will happen when signalling the other side of a
       connection to shutdown. It does not indicate a bug. *)
    ()
  | ConnectedTo otherend ->
    otherend.events <- otherend.events + 1;
    Lwt_condition.broadcast otherend.c ()

module IntMap = Map.Make(struct type t = int let compare (a: int) (b: int) = compare a b end)
let listening = ref IntMap.empty

let listen _ =
  let t = create () in
  listening := IntMap.add t.port t !listening;
  t.port, t

let nr_connected = ref 0
let assert_cleaned_up () =
  if !nr_connected <> 0
  then failwith (Printf.sprintf "%d event channels are still connected" !nr_connected);
  if !listening <> IntMap.empty
  then failwith (Printf.sprintf "%d" (IntMap.cardinal !listening))

let connect _ port =
  let other = IntMap.find port !listening in
  listening := IntMap.remove port !listening;
  let this = create () in
  this.state <- ConnectedTo other;
  other.state <- ConnectedTo this;
  incr nr_connected;
  this

let close t = match t.state with
  | ConnectedTo other ->
    other.state <- Closed;
    t.state <- Closed;
    decr nr_connected
  | _ -> ()
