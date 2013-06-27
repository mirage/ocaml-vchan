open Cmdliner

type clisrv = Client | Server

let clisrv_pa = function
  | "server" -> `Ok Server
  | "client" -> `Ok Client
  | _ -> `Error "invalid argument"

let clisrv_pr f = function
  | Server -> Format.pp_print_string f "server"
  | Client -> Format.pp_print_string f "client"

type rw = Read | Write

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


let node clisrv rw domid nodepath = ()

let cmd =
  let doc = "Vchan testing" in
  Term.(pure node $ clisrv $ rw $ domid $ nodepath),
  Term.info "node" ~version:"0.1" ~doc

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
