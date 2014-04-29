open Mirage

let main =
  foreign "Echo.Main" (console @-> job)

let () =
  add_to_ocamlfind_libraries ["vchan"];
  register "echo" [
    main $ default_console 
  ]
