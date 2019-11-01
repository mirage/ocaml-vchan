(*
 * Copyright (c) 2014 Citrix Systems Inc
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

type port = int [@@deriving sexp_of]

let port_of_string x =
  try
    Ok (int_of_string x)
  with _ ->
    let msg = Printf.sprintf "Valid ports must be integers; got %S" x in
    Error (`Msg msg)

let string_of_port = string_of_int

type channel = Eventchn.t
let sexp_of_channel x = Sexplib.Sexp.Atom (string_of_int (Eventchn.to_int x))

type event = OS.Activations.event
let sexp_of_event _ = Sexplib.Sexp.Atom "<event>"

let initial = OS.Activations.program_start

let recv = OS.Activations.after

let send channel =
  let h = Eventchn.init () in
  Eventchn.notify h channel

let listen domid =
  let h = Eventchn.init () in
  let port = Eventchn.bind_unbound_port h domid in
  Eventchn.unmask h port;
  Eventchn.to_int port, port

let connect domid port =
  let h = Eventchn.init () in
  let port' = Eventchn.bind_interdomain h domid port in
  Eventchn.unmask h port';
  port'

let close channel =
  let h = Eventchn.init () in
  Eventchn.unbind h channel
