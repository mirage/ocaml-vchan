open OS
open Node

(* Config **********************************************************)
let clisrv = Server
let remote_domid = 0 (* Serve dom0 *)
let nodepath domid = Printf.sprintf "/local/domain/%s/vchan" domid
(*******************************************************************)

let buf = String.create 5000

let rec echo vch =
  Vchan.read_into vch buf 0 5000
  >>= fun nb_read ->
  (* Strip the final \n *)
  let string_to_echo = String.sub buf 0 (nb_read-1) in
  Vchan.write vch string_to_echo
  >>= fun () -> echo vch

let main () =
  Xs.make () >>= fun xsh ->
  Xs.immediate xsh (fun xsh -> Xs.read xsh "domid") >>=
  fun domid ->
  Node.with_vchan clisrv remote_domid (nodepath domid) echo
