open Lwt.Infix

module Main
    (C : Mirage_types_lwt.CONSOLE)
    ((*  (FS: Mirage_types_lwt.FS)*)
      T : Mirage_types_lwt.TIME) =
struct
  let process_string_input console str =
    let cmd = Command.parse str in
    match cmd with
    | Ok cmd ->
        Logs.info (fun f -> f "processing %a" Command.pp cmd) ;
        Command.run cmd
        >>= fun ret ->
        let pp fmt = function
          | Unix.WEXITED i ->
              Format.fprintf fmt "terminated normally and returned %i" i
          | Unix.WSIGNALED i -> Format.fprintf fmt "killed by signal %i" i
          | Unix.WSTOPPED i -> Format.fprintf fmt "stopped by signal %i" i
        in
        Logs.info (fun f -> f "%a" pp ret) ;
        Lwt.return_unit
    | Error err ->
        let str = Format.asprintf "error: %s" (Printexc.to_string err) in
        C.log console str

  let rec main_loop console =
    Utils.print "> "
    >>= fun () ->
    let line = Lwt_main.run (C.read console) in
    match line with
    | Ok (`Data data) ->
        let str = Cstruct.to_string data |> Utils.remove_cr in
        if str = "" then main_loop console
        else
          process_string_input console str
          >>= fun _ ->
          if Command.exit_shell () then C.disconnect console
          else main_loop console
    | Ok `Eof -> Lwt.return_unit
    | Error err ->
        let str = Format.asprintf "error: %a" C.pp_error err in
        C.log console str

  let start console (*_fs*) _t = main_loop console
end
