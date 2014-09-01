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

  let tbl: (Port.t, t) Hashtbl.t = Hashtbl.create 16

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

module Check_flow_compatible(F: V1_LWT.FLOW) = struct end

let () =
  let module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client) in
  let module M = Vchan.Connection.Make(Events_lwt_unix)(Memory_lwt_unix)(Xs) in
  let module Test = Check_flow_compatible(M) in
  ()
