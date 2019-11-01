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

module Server (C: Mirage_console_lwt.S) = struct

  let readdir h d =
    printf "safe: reading %s\n%!" d;
    OS.Xs.(directory h d) >>= fun dirs ->
    let dirs = List.filter (fun p -> p <> "") dirs in
    match dirs with
    | [] -> print_endline "readdir restarting"; fail Xs_protocol.Eagain
    | hd::_ -> print_endline ("readdir returning " ^ hd); return hd

  let rec read_all c t =
    VX.read t
    >>= function
    | Ok `Eof -> OS.Time.sleep_ns (Duration.of_sec 5) >>= fun () -> C.log c "EOF"
    | Error _ -> OS.Time.sleep_ns (Duration.of_sec 5) >>= fun () -> C.log c "ERR"
    | Ok (`Data buf) ->
      let s = Cstruct.to_string buf in
      C.log c s >>= fun () ->
      read_all c t

  let start c =
    OS.Xs.make () >>= fun xs ->
    let name = "foo_server" in
    Info.register_me xs name >>= fun () ->
    Info.get_my_domid xs >>= fun domid ->
    C.log c "Server initialising" >>= fun () ->
    OS.Xs.wait xs
      (fun h ->
         C.log c "starting wait" >>= fun () ->
         readdir h (sprintf "/conduit/%s" name) >>= fun remote_name ->
         C.log c (sprintf "found a name %s!" remote_name) >>= fun () ->
         readdir h (sprintf "/conduit/%s/%s" name remote_name) >>= fun port ->
         C.log c (sprintf "port %s" port) >>= fun () ->
         OS.Xs.read h (sprintf "/conduit/%s" remote_name) >>= fun remote_domid ->
         let remote_domid = int_of_string remote_domid in
         C.log c (sprintf "remote domid is %d and port is %s" remote_domid port) >>= fun () ->
         Vchan.Port.of_string port
         |> function
         | Error (`Msg e) ->
           C.log c e >>= fun () ->
           fail (Failure "error making port")
         | Ok port ->
           C.log c "creating server" >>= fun () ->
           VX.server ~domid:remote_domid ~port ~read_size:4096 ~write_size:4096 ()
           >>= read_all c
      )

end

module Client (C: Mirage_console_lwt.S) = struct

  let start c =
    OS.Xs.make () >>= fun xs ->
    let server_name = "foo_server" in
    let name = "foo_client" in
    let port = "flibble" in
    C.log c "Client initialising" >>= fun () ->
    Info.register_me xs name >>= fun () ->
    OS.Xs.(immediate xs (fun h -> read h (sprintf "/conduit/%s" server_name))) >>= fun remote_domid ->
    let remote_domid = int_of_string remote_domid in
    OS.Xs.(immediate xs (fun h -> write h (sprintf "/conduit/%s/%s/%s" server_name name port) "theport"))
    >>= fun () ->
    Vchan.Port.of_string port
    |> function
    | Error _ -> fail (Failure "error making port")
    | Ok port ->
      OS.Time.sleep_ns (Duration.of_sec 2) >>= fun () ->
      VX.client ~domid:remote_domid ~port ()
      >>= fun t ->
      C.log c "Client connected" >>= fun () ->
      let rec write num =
        let buf = Io_page.(to_cstruct (get 1)) in
        let s = sprintf "num is %d" num in
        let len = String.length s in
        Cstruct.blit_from_string s 0 buf 0 len;
        let buf = Cstruct.sub buf 0 len in
        VX.write t buf
        >>= function
        | Error _ -> OS.Time.sleep_ns (Duration.of_sec 5) >>= fun () -> C.log c "ERR"
        | Ok () -> OS.Time.sleep_ns (Duration.of_sec 5) >>= fun () -> write (num+1)
      in write 0

end
