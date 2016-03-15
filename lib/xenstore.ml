(*
 * Copyright (c) 2013,2014 Citrix Systems Inc
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
open Lwt
open Sexplib.Std

module Make(Xs: Xs_client_lwt.S) = struct

  type t = {
    ring_ref: string;
    event_channel: string;
  } [@@deriving sexp]

  let write ~client_domid ~port t =
    Xs.make ()
    >>= fun c ->
    Xs.(immediate c (fun h -> read h "domid")) >>= fun server_domid ->
    Xs.(immediate c (fun h -> getdomainpath h (int_of_string server_domid))) >>= fun domainpath ->
    let xs_path = Printf.sprintf "%s/data/vchan/%d/%s" domainpath client_domid (Port.to_string port) in
    let acl =
      Xs_protocol.ACL.({owner = int_of_string server_domid; other = NONE; acl = [ client_domid, READ ]}) in
    let info = [
      xs_path ^ "/ring-ref", t.ring_ref;
      xs_path ^ "/event-channel", t.event_channel;
    ] in
    Xs.(transaction c
          (fun h ->
             Lwt_list.iter_s (fun (k, v) ->
               write h k v >>= fun () ->
               setperms h k acl
             ) info
          )
    )

  let read ~server_domid ~port =
    Xs.make ()
    >>= fun c ->
    Xs.(immediate c (fun h -> read h "domid")) >>= fun client_domid ->
    Xs.(immediate c (fun h -> getdomainpath h server_domid)) >>= fun domainpath ->
    let xs_path = Printf.sprintf "%s/data/vchan/%s/%s" domainpath client_domid (Port.to_string port) in
    Xs.(wait c
      (fun xsh ->
        Lwt.catch
        (fun () ->
          read xsh (xs_path ^ "/ring-ref") >>= fun rref ->
          read xsh (xs_path ^ "/event-channel") >>= fun evtchn ->
          return (rref, evtchn)
        )(fun _ -> fail Xs_protocol.Eagain)))
    >>= fun (ring_ref, event_channel) ->
    return { ring_ref; event_channel }

  let delete ~client_domid ~port =
    Xs.make ()
    >>= fun c ->
    Xs.(immediate c (fun h -> read h "domid")) >>= fun server_domid ->
    Xs.(immediate c (fun h -> getdomainpath h (int_of_string server_domid))) >>= fun domainpath ->
    let xs_path = Printf.sprintf "%s/data/vchan/%d/%s" domainpath client_domid (Port.to_string port) in
    Xs.(transaction c
        (fun h ->
           rm h xs_path
           >>= fun () ->
           (* If there are no more connections to remote_domid, remove the whole directory *)
           let dir = Filename.dirname xs_path in
           directory h dir
           >>= function
           | [] -> rm h dir
           | _ -> return ()
        )
    )

end

