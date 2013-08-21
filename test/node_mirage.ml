open OS
open Node

(* Config **********************************************************)
let clisrv = Server
let remote_domid = 0 (* Serve dom0 *)
let blocking = false
(*******************************************************************)

let buf = String.create 5000

let rec echo vch =
  V.read_into vch buf 0 5000 >>= fun nb_read ->
  V.write_from_exactly vch buf 0 nb_read >>= fun () ->
  echo vch

let main () =
  Node.with_vchan blocking clisrv (Eventchn.init ()) remote_domid "data/vchan" echo
