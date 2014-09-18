open Lwt

module VX = Vchan_xen

module Server (C: V1_LWT.CONSOLE) = struct

  let start c =
    let domid = 4 (* TODO: from xenstore? *) in
    Console.log_s c "Server initialising" >>= fun () ->
    Vchan.Port.of_string "foo"
    |> function
    |`Error _ -> fail (Failure "error making port")
    |`Ok port ->
    VX.server ~domid ~port ()
    >>= fun t ->
    Console.log_s c "Server started" >>= fun () ->
    VX.read t
    >>= function
    |`Eof -> Console.log c "EOF"; OS.Time.sleep 5.
    |`Error _ -> Console.log c "ERR"; OS.Time.sleep 5.
    |`Ok buf ->
      let s = Cstruct.to_string buf in
      Console.log c s; OS.Time.sleep 5.

end

module Client (C: V1_LWT.CONSOLE) = struct

  let start c =
    let domid = 4 (* TODO: from xenstore? *) in
    Console.log_s c "Client initialising" >>= fun () ->
    Vchan.Port.of_string "foo"
    |> function
    |`Error _ -> fail (Failure "error making port")
    |`Ok port ->
    VX.client ~domid ~port ()
    >>= fun t ->
    Console.log_s c "Client connected" >>= fun () ->
    let buf = Io_page.(to_cstruct (get 1)) in
    Cstruct.blit_from_string "testing" 0 buf 0 7;
    let buf = Cstruct.sub buf 0 7 in
    VX.write t buf
    >>= function
    |`Eof -> Console.log c "EOF"; OS.Time.sleep 5.
    |`Error _ -> Console.log c "ERR"; OS.Time.sleep 5.
    |`Ok () -> Console.log c "OK"; OS.Time.sleep 5.
    
end
