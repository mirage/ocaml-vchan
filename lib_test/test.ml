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

module Config = struct
  open Lwt

  type t = {
    ring_ref: string;
    event_channel: string;
  }

  let tbl: (Vchan.Port.t, t) Hashtbl.t = Hashtbl.create 16

  let c = Lwt_condition.create ()

  let write ~client_domid ~port t =
    Hashtbl.replace tbl port t;
    return ()

  let read ~server_domid ~port =
    let rec loop () =
      if Hashtbl.mem tbl port
      then return (Hashtbl.find tbl port)
      else
        Lwt_condition.wait c >>= fun () ->
        loop () in
    loop ()

  let delete ~client_domid ~port =
    Hashtbl.remove tbl port;
    return ()
end

module Memory = struct
  type grant = int32

  let grant_of_int32 x = x
  let int32_of_grant x = x

  type share = {
    grants: grant list;
    mapping: Io_page.t;
  }

  let get =
    let g = ref Int32.zero in
    fun () ->
      g := Int32.succ !g;
      Int32.pred !g

  let rec get_n n =
    if n = 0 then [] else get () :: (get_n (n-1))

  let individual_pages = Hashtbl.create 16
  let big_mapping = Hashtbl.create 16

  let share ~domid ~npages ~rw =
    let mapping = Io_page.get npages in
    let grants = get_n npages in
    let share = { grants; mapping } in
    let pages = Io_page.to_pages mapping in
    List.iter (fun (grant, page) -> Hashtbl.replace individual_pages grant page) (List.combine grants pages);
    Hashtbl.replace big_mapping (List.hd grants) mapping;
    share

  let unshare share =
    List.iter (fun grant -> Hashtbl.remove individual_pages grant) share.grants;
    Hashtbl.remove big_mapping (List.hd share.grants)

  type mapping = Io_page.t

  let buf_of_mapping x = x

  let map ~domid:_ ~grant ~rw:_ =
    Hashtbl.find individual_pages grant

  let mapv ~grants ~rw:_ =
    let first = snd (List.hd grants) in
    Hashtbl.find big_mapping first

  let unmap _ = ()
end

module Events = struct
  open Lwt

  type port = int

  let port_of_string x = `Ok (int_of_string x)
  let string_of_port = string_of_int

  type channel = int
  let get =
    let g = ref 0 in
    fun () ->
      incr g;
      !g - 1

  type event = int
  let initial = 0

  let channels = Array.create 1024 0
  let c = Lwt_condition.create ()

  let rec recv channel event =
    if channels.(channel) > event
    then return channels.(channel)
    else
      Lwt_condition.wait c >>= fun () ->
      recv channel event

  let connected_to = Array.create 1024 (-1)

  let send channel =
    let listening = connected_to.(channel) in
    channels.(listening) <- channels.(listening) + 1;
    Lwt_condition.broadcast c ()

  let listen _ =
    let port = get () in
    port, port

  let connect _ port =
    let port' = get () in
    connected_to.(port') <- port;
    port'

  let close port =
    channels.(port) <- 0;
    connected_to.(port) <- (-1)
end


module Check_flow_compatible(F: V1_LWT.FLOW) = struct end

let () =
  let module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client) in
  let module M = Vchan.Connection.Make(Events_lwt_unix)(Memory_lwt_unix)(Vchan.Xenstore.Make(Xs)) in
  let module Test = Check_flow_compatible(M) in
  ()

module V = Vchan.Connection.Make(Events)(Memory)(Config)
