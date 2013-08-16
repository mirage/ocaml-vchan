open OS
open Node

(* Config **********************************************************)
let clisrv = Server
let remote_domid = 0 (* Serve dom0 *)
(*******************************************************************)

let buf = String.create 5000

let rec echo vch =
  V.read_into vch buf 0 5000
  >>= fun nb_read ->
  (* Strip the final \n *)
  let string_to_echo = String.sub buf 0 (nb_read-1) in
  V.write vch string_to_echo
  >>= fun () -> echo vch

let main () =
  Xs.make () >>= fun c ->
  Xs.immediate c (fun h -> Xs.read h "domid") >>=
  fun domid ->
  Node.with_vchan clisrv (Eventchn.init ()) remote_domid "vchan" echo
