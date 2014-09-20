open Lwt
open Printf

module VX = Vchan_xen

module Info = struct

  let get_my_domid xs =
    OS.Xs.(immediate xs (fun h -> read h "domid"))
    
  let register_me xs myname =
    get_my_domid xs >>= fun domid ->
    OS.Xs.(immediate xs (fun h -> write h ("/conduit/" ^ myname) domid))

  let get_domid_for xs name =
    OS.Xs.(immediate xs (fun h -> read h ("/conduit/" ^ name)))
end

module Server (C: V1_LWT.CONSOLE) = struct

  let safedir h d =
    printf "safe: reading %s\n%!" d;
    OS.Xs.(directory h d) >>= fun dirs ->
    let dirs = List.filter (fun p -> p <> "") dirs in
    match dirs with
    | [] -> print_endline "safedir restarting"; fail Xs_protocol.Eagain
    | hd::_ -> print_endline ("safedir returning " ^ hd); return hd
    
  let start c =
    lwt xs = OS.Xs.make () in
    let name = "foo_server" in
    Info.register_me xs name >>= fun () ->
    Info.get_my_domid xs >>= fun domid ->
    Console.log_s c "Server initialising" >>= fun () ->
    OS.Xs.(wait xs (fun h ->
      Console.log_s c "starting wait" >>= fun () ->
      safedir h (sprintf "/conduit/%s" name) >>= fun remote_name ->
      Console.log_s c (sprintf "found a name %s!" remote_name) >>= fun () ->
      safedir h (sprintf "/conduit/%s/%s" name remote_name) >>= fun port ->
      Console.log_s c (sprintf "port %s" port) >>= fun () ->
      OS.Xs.read h (sprintf "/conduit/%s" remote_name) >>= fun remote_domid ->
      Console.log_s c (sprintf "remote domid is %s" remote_domid) >>= fun () ->
      let remote_domid = int_of_string remote_domid in 
      Console.log_s c (sprintf "remote domid is %d and port is %s" remote_domid port) >>= fun () ->
      Vchan.Port.of_string port
      |> function
      |`Error e -> Console.log_s c e >>= fun () -> fail (Failure "error making port")
      |`Ok port ->
         Console.log_s c "creating server!!!" >>= fun () ->
         VX.server ~domid:remote_domid ~port ~read_size:4096 ~write_size:4096 () >>= fun t ->
         Console.log_s c "about to vx read" >>= fun () ->
         VX.read t
         >>= function
         |`Eof -> Console.log c "EOF"; OS.Time.sleep 5.
         |`Error _ -> Console.log c "ERR"; OS.Time.sleep 5.
         |`Ok buf ->
           let s = Cstruct.to_string buf in
           Console.log c s; OS.Time.sleep 5.
    ))

end

module Client (C: V1_LWT.CONSOLE) = struct

  let start c =
    lwt xs = OS.Xs.make () in
    let server_name = "foo_server" in
    let name = "foo_client" in
    let port = "flibble" in
    Console.log_s c "Client initialising" >>= fun () ->
    Info.register_me xs name >>= fun () ->
    OS.Xs.(immediate xs (fun h -> read h (sprintf "/conduit/%s" server_name))) >>= fun remote_domid ->
    let remote_domid = int_of_string remote_domid in
    OS.Xs.(immediate xs (fun h -> write h (sprintf "/conduit/%s/%s/%s" server_name name port) "theport"))
    >>= fun () -> 
    Vchan.Port.of_string port
    |> function
    |`Error _ -> fail (Failure "error making port")
    |`Ok port ->
    OS.Time.sleep 2.0 >>= fun () ->
    VX.client ~domid:remote_domid ~port ()
    >>= fun t ->
    Console.log_s c "Client connected" >>= fun () ->
    OS.Time.sleep 1.0 >>= fun () ->
    let buf = Io_page.(to_cstruct (get 1)) in
    Cstruct.blit_from_string "testing" 0 buf 0 7;
    let buf = Cstruct.sub buf 0 7 in
    VX.write t buf
    >>= function
    |`Eof -> Console.log c "EOF"; OS.Time.sleep 5.
    |`Error _ -> Console.log c "ERR"; OS.Time.sleep 5.
    |`Ok () -> Console.log c "OK"; OS.Time.sleep 5.
    
end
