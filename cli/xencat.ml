open Cmdliner
open Sexplib.Std
open Vchan_lwt_unix

let (>>=) = Lwt.bind

let listen =
  let doc = "Act as a server rather than a client." in
  Arg.(value & flag & info [ "l"; "listen"] ~doc)

let domid = Arg.(required & pos 0 (some int) None & info ~docv:"DOMID" ~doc:"Domain id of the remote endpoint." [])

let port =
  let port = Vchan.Port.of_string, fun f p -> Format.fprintf f "%s" (Vchan.Port.to_string p) in
  Arg.(required & pos 1 (some port) None & info ~docv:"PORT" ~doc:"Port id (unique to this client+server pair). Must only contain the following characters: [a-zA-Z0-9_-]" [])

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

let client domid port =
  Client.connect ~domid ~port ()
  >>= fun (ic, oc) ->
  Printf.fprintf stderr "Connected.\n%!";
  proxy (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Client.close (ic, oc)

let server domid port =
  Server.connect ~domid ~port ()
  >>= fun (ic, oc) ->
  Printf.fprintf stderr "Connected.\n%!";
  proxy (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Server.close (ic, oc)

open Lwt

let node listen domid port : unit = Lwt_main.run (
  (if listen then server else client) domid port
)

let cmd =
  let doc = "Establish vchan connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to a remote Xen domain and transfer data over stdin/stdout, in a similar way to 'nc'";
    `S "EXAMPLES";
    `P "To listen to an incoming connection from domid 2 on port 'hello':";
    `P "xencat -l 2 hello";
    `P "To connect to domid 1 on port 'hello':";
    `P "xencat 1 hello";
  ] in
  Term.(pure node $ listen $ domid $ port),
  Term.info "xencat" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
