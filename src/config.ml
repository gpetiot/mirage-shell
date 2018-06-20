
open Mirage

let main =
  foreign "Unikernel.Main" (console @-> (*fs @->*) time @-> job)

let fs_impl = fat_of_files ()
    
let () =
  register "mirage-shell" [main $ default_console $ (*fs_impl $*) default_time]
