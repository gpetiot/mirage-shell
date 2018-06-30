
open Lwt.Infix


let remove_cr str =
  (* just in case, dunno if it will always be the case *)
  if String.get str ((String.length str) - 1) = '\n' then
    String.sub str 0 ((String.length str) - 1)
  else str

(* write a Cstruct.t to stdout (no CR) *)
(* to define print *)
let write_one buf =
  Lwt_cstruct.complete
    (fun frag ->
      let open Cstruct in
      Lwt_bytes.write Lwt_unix.stdout frag.buffer frag.off frag.len
    ) buf

(* write a string to stdout (no CR) *)
(* this could be in mirage_console *)
let print str = write_one (Cstruct.of_string str)


module Main
  (C: Mirage_types_lwt.CONSOLE)
  (*  (FS: Mirage_types_lwt.FS)*)
  (T: Mirage_types_lwt.TIME) =
struct

  let exit_shell = ref false

  let execute_builtin prgm _args =
    if prgm = "exit" then
      begin
	exit_shell := true;
	Lwt.return (Unix.WEXITED 0)
      end
    else
      Lwt.fail Command.Not_a_builtin

  open Command
	
  let rec run = function
    | Execute (prgm, args) ->
       Lwt.catch
	 (fun () -> execute_builtin prgm args)
	 (function
	 | Command.Not_a_builtin -> Lwt_process.exec (prgm, Array.of_list args)
	 | exn ->
	    begin
	      Logs.err (fun f -> f "uncaught exception from listen callback@\n\
                                    Exception: @[%s@]"
		(Printexc.to_string exn));
	      Lwt.return (Unix.WEXITED 0)
	    end
	 )
    | Pipe (_out_stream, _cmd1, _cmd2) ->
       (* TODO *)
       assert false
    | Junction (And, cmd1, cmd2) ->
       run cmd1 >>=
	 begin
	   fun status1 ->
	     match status1 with
	     | Unix.WEXITED 0 -> run cmd2
	     | _ -> Lwt.return status1
	 end
    | Junction (Or, cmd1, cmd2) ->
       run cmd1 >>=
	 begin
	   fun status1 ->
	     match status1 with
	     | Unix.WEXITED 0 -> Lwt.return status1
	     | _ -> run cmd2
	 end
    | Sequence (cmd1, Synchronous, Some cmd2) ->
       run cmd1 >>= fun _ -> run cmd2
    | Sequence (_, Synchronous, None) -> assert false (* not possible *)
    | Sequence (cmd1, Asynchronous, None) ->
       begin
	 Lwt.async (fun () -> run cmd1);
	 Lwt.return (Unix.WEXITED 0)
       end
    | Sequence (cmd1, Asynchronous, Some cmd2) ->
       begin
	 Lwt.async (fun () -> run cmd1);
	 run cmd2
       end
    
	
  let process_string_input console str =
    C.log console ("processing '" ^ str ^ "'...") >>= fun () ->
    let cmd = Command.parse str in
    begin
      match cmd with
      | Ok cmd ->
	 let str = Format.asprintf "%a" Command.pp_cmd cmd in
	 C.log console str >>= fun () ->
	 run cmd >>= fun ret ->
	 begin
	   let str = match ret with
	     | Unix.WEXITED i ->
		Format.asprintf "terminated normally and returned %i" i
	     | Unix.WSIGNALED i ->
		Format.asprintf "killed by signal %i" i
	     | Unix.WSTOPPED i ->
		Format.asprintf "stopped by signal %i" i
	   in
	   if !exit_shell then
	     Lwt.return_unit
	   else
	     C.log console str
	 end
      | Error msg -> C.log console msg
    end

      
  let rec main_loop console =
    print "> " >>= fun () ->
    let line = Lwt_main.run (C.read console) in
    match line with
    | Ok (`Data data) ->
       let str = Cstruct.to_string data |> remove_cr in
       if str = "" then
	 main_loop console
       else
	 begin
	   process_string_input console str >>= fun _ ->
	   if !exit_shell then
	     C.disconnect console
	   else
	     main_loop console
	 end
    | Ok (`Eof) -> Lwt.return_unit
    | Error err ->
       let str = Format.asprintf "error: %a" C.pp_error err in
       C.log console str
      
  let start console (*_fs*) _t = main_loop console

end
