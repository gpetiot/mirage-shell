open Mirage


let main =
  foreign "Unikernel.Main" (block @-> console @-> fs @-> time @-> job)

let block_impl = block_of_file "disk.img"
let fs_impl = fat block_impl
    
let () =
  register "mirage-shell" [main $ block_impl $ default_console $ fs_impl $
			      default_time]
