open Cmdliner
open Sexplib.Std

type clisrv = Client | Server

let (>>=) = Lwt.bind

module Xs = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
module V = Vchan.Make(Unix_activations)(Xs)

let with_vchan clisrv evtchn_h domid nodepath f =
  (match clisrv with
   | Client ->
     V.client ~evtchn_h ~domid ~xs_path:nodepath
   | Server ->
     V.server ~evtchn_h ~domid ~xs_path:nodepath
       ~read_size:5000 ~write_size:5000 ~persist:true)
  >>= fun vch ->
  f vch

let server =
  let doc = "Act as a server rather than a client." in
  Arg.(value & flag & info [ "s"; "server"] ~doc)

let domid = Arg.(required & pos 0 (some int) None &
                   info ~docv:"DOMID" ~doc:"Domain id of the remote endpoint." [])

let nodepath = Arg.(value & opt (some string) None &
                    info ["n"; "nodepath"] ~docv:"NODEPATH" ~doc:"Full Xenstore path of the config (defaults to /local/domain/<domid>/data/vchan)." )

let buf = String.create 5000

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Eof -> Lwt.fail (Failure "End of file")
| `Error (`Not_connected state) -> Lwt.fail (Failure (Printf.sprintf "Not in a connected state: %s" (Sexplib.Sexp.to_string (V.sexp_of_state state))))

let with_vchan_f vch =
  let (_: unit Lwt.t) =
    let rec read_forever vch =
      V.read vch >>|= fun buf ->
      Printf.printf "%s%!" (Cstruct.to_string buf);
      read_forever vch in
    read_forever vch in
  let (_: unit Lwt.t) =
    let rec stdin_to_endpoint vch =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun line ->
      let line = line ^ "\n" in
      let buf = Cstruct.create (String.length line) in
      Cstruct.blit_from_string line 0 buf 0 (String.length line);
      V.write vch buf
      >>|= fun () ->
      stdin_to_endpoint vch in
    stdin_to_endpoint vch in
  let t, u = Lwt.task () in
  t

open Lwt

let node server domid nodepath : unit Lwt.t = Lwt_main.run (
  ( match nodepath with
    | Some s -> return s
    | None ->
      ( if server then begin
          Xs.make () >>= fun c ->
          Xs.(immediate c (fun h -> read h "domid")) >>= fun domid ->
          return (int_of_string domid)
        end else return domid ) >>= fun domid ->
      return ( Printf.sprintf "/local/domain/%d/data/vchan" domid ) )
  >>= fun nodepath ->
  (* Listen to incoming events. *)
  let evtchn_h = Eventchn.init () in
  let clisrv = if server then Server else Client in
  with_vchan clisrv evtchn_h domid nodepath with_vchan_f)

let cmd =
  let doc = "Establish vchan connections" in
  Term.(pure node $ server $ domid $ nodepath),
  Term.info "node" ~version:"0.1" ~doc

let () =
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
