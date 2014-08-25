open Cmdliner
open Sexplib.Std
open Vchan_lwt_unix

type clisrv = Client | Server

let (>>=) = Lwt.bind

let listen =
  let doc = "Act as a server rather than a client." in
  Arg.(value & flag & info [ "l"; "listen"] ~doc)

let domid = Arg.(required & pos 0 (some int) None & info ~docv:"DOMID" ~doc:"Domain id of the remote endpoint." [])

let nodepath = Arg.(value & pos 1 (some string) None & info ~docv:"PATH" ~doc:"Xenstore path used to identify the connection (defaults to /local/domain/<domid>/data/vchan)." [])

let proxy (ic, oc) (stdin, stdout) =
  let rec proxy a b =
    Lwt_io.read_char a
    >>= fun c ->
    Lwt_io.write_char b c
    >>= fun () ->
    proxy a b in
  let (a: unit Lwt.t) = proxy stdin oc in
  let (b: unit Lwt.t) = proxy ic stdout in
  Lwt.join [a; b]

let client domid nodepath =
  Client.connect ~domid ~path:nodepath ()
  >>= fun (ic, oc) ->
  proxy (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Client.close (ic, oc)

let server domid nodepath =
  Server.connect ~domid ~path:nodepath ()
  >>= fun (ic, oc) ->
  proxy (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Server.close (ic, oc)

open Lwt

let node listen domid nodepath : unit = Lwt_main.run (
  let module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client) in
  ( match nodepath with
    | Some s -> return s
    | None ->
      ( if listen then begin
          Xs.make () >>= fun c ->
          Xs.(immediate c (fun h -> read h "domid")) >>= fun domid ->
          return (int_of_string domid)
        end else return domid ) >>= fun domid ->
      return ( Printf.sprintf "/local/domain/%d/data/vchan" domid ) )
  >>= fun nodepath ->
  (if listen then server else client) domid nodepath
)

let cmd =
  let doc = "Establish vchan connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to a remote Xen domain and transfer data over stdin/stdout, in a similar way to 'nc'";
  ] in
  Term.(pure node $ listen $ domid $ nodepath),
  Term.info "xencat" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
