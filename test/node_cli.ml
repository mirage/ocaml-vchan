open Cmdliner
open Node
open OS

type op = Read | Write

let clisrv_pa = function
  | "server" -> `Ok Server
  | "client" -> `Ok Client
  | _ -> `Error "invalid argument"

let clisrv_pr f = function
  | Server -> Format.pp_print_string f "server"
  | Client -> Format.pp_print_string f "client"

let rw_pa = function
  | "read" -> `Ok Read
  | "write" -> `Ok Write
  | _ -> `Error "invalid argument"

let rw_pr f = function
  | Read -> Format.pp_print_string f "read"
  | Write -> Format.pp_print_string f "write"

let clisrv_conv = clisrv_pa, clisrv_pr
let rw_conv = rw_pa, rw_pr

let clisrv = Arg.(required & pos 0 (some clisrv_conv) None &
                  info ~docv:"TYPE" ~doc:"Either client or server." [])

let rw = Arg.(required & pos 1 (some rw_conv) None &
              info ~docv:"ACTION" ~doc:"Either read or write." [])

let domid = Arg.(required & pos 2 (some int) None &
                info ~docv:"DOMID" ~doc:"Domain id of the remote endpoint." [])

let nodepath = Arg.(required & pos 3 (some string) None &
                    info ~docv:"NODEPATH" ~doc:"XenStore path of the config." [])

let buf = String.create 5000

let with_vchan_f op vch = match op with
  | Read ->
    let rec read_forever vch =
      V.read_into vch buf 0 5000
      >>= fun nb_read ->
      let string_to_print = String.sub buf 0 nb_read in
      Printf.printf "%s%!" string_to_print;
      read_forever vch
    in read_forever vch

  | Write ->
    Printf.printf "Please type something to send to the other endpoint.\n%!";
    let rec stdin_to_endpoint vch =
      Printf.printf "> %!";
      Lwt_io.read_line Lwt_io.stdin
      >>= fun line -> V.write vch (line ^ "\n")
      >>= fun () -> stdin_to_endpoint vch
    in
    stdin_to_endpoint vch

let node clisrv rw domid nodepath : unit Lwt.t = Lwt_main.run (
    (* Listen to incoming events. *)
    let evtchn_h = Eventchn.init () in
    Node.with_vchan clisrv evtchn_h domid nodepath (with_vchan_f rw))

let cmd =
  let doc = "Vchan testing" in
  Term.(pure node $ clisrv $ rw $ domid $ nodepath),
  Term.info "node" ~version:"0.1" ~doc

let () =
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
