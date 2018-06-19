open Mirage


let main =
  foreign "Unikernel.Main" job

let () =
  register "my_app" [main]
