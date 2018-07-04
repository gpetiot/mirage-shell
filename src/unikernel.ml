
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

  let rec read_stream st =
    Lwt_stream.get st >>= (function
    | Some str ->
       print (Format.asprintf "%s@\n" str) >>= fun () -> read_stream st
    | None -> Lwt.return_unit)
	
  let on_exit fd =
    let channel = Lwt_io.of_unix_fd ~mode:Lwt_io.input fd in
    let stream =  Lwt_io.read_lines channel in
    read_stream stream >>= fun () ->
    Lwt_io.close channel
      
  let run_basic_command ?stdin ?stdout ?stderr (prgm, args) =
    Lwt.catch
      (fun () -> execute_builtin prgm args)
      (function
      | Command.Not_a_builtin ->
	 begin
	   match stdout, stderr with
	   | Some `Keep, Some `Keep  ->
	      let (stdout_r, stdout_w) = Unix.pipe () in
	      let (stderr_r, stderr_w) = Unix.pipe () in
	      let cmd = Lwt_process.open_process_none
		?stdin ~stdout:(`FD_move stdout_w) ~stderr:(`FD_move stderr_w)
		(prgm, args) in
	      let st = Lwt_main.run cmd#status in
	      on_exit stdout_r >>= fun () ->
	      on_exit stderr_r >>= fun () ->
	      Lwt.return st

	   | Some `Keep, _  ->
	      let (stdout_r, stdout_w) = Unix.pipe () in
	      let cmd = Lwt_process.open_process_none
		?stdin ~stdout:(`FD_move stdout_w) ?stderr (prgm, args) in
	      let st = Lwt_main.run cmd#status in
	      on_exit stdout_r >>= fun () ->
	      Lwt.return st
		
	   | _, Some `Keep  ->
	      let (stderr_r, stderr_w) = Unix.pipe () in
	      let cmd = Lwt_process.open_process_none
		?stdin ?stdout ~stderr:(`FD_move stderr_w) (prgm, args) in
	      let st = Lwt_main.run cmd#status in
	      on_exit stderr_r >>= fun () ->
	      Lwt.return st
		
	   | _ ->
	     let cmd =
	       Lwt_process.open_process_none ?stdin ?stdout ?stderr (prgm, args)
	     in
	     let st = Lwt_main.run cmd#status in
	      Lwt.return st
	 end
      | exn ->
	 begin
	   Logs.err (fun f -> f "uncaught exception from listen callback@\n\
                                    Exception: @[%s@]"
	     (Printexc.to_string exn));
	   Lwt.return (Unix.WEXITED 0)
	 end
      )

      
  let rec run_pipe_command ?stdin ?stdout ?stderr = function
    | No_pipe x -> run_basic_command ?stdin ?stdout ?stderr x
    | Pipe (Stdout, cmd1, cmd2) ->
       let (stdout_r, stdout_w) = Unix.pipe () in
       run_basic_command ?stdin ~stdout:(`FD_move stdout_w) ?stderr cmd1 >>=
	 begin
	   fun _ ->
	     run_pipe_command ~stdin:(`FD_move stdout_r) ?stdout ?stderr cmd2
	 end
    | Pipe (Stdout_stderr, cmd1, cmd2) ->
       let (stdout_r, stdout_w) = Unix.pipe () in
       run_basic_command
	 ?stdin ~stdout:(`FD_copy stdout_w) ~stderr:(`FD_move stdout_w) cmd1 >>=
	 begin
	   fun _ ->
	     run_pipe_command ~stdin:(`FD_move stdout_r) ?stdout ?stderr cmd2
	 end

  let rec run_junction_command = function
    | No_junction x ->
       run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep x
    | Junction (And, cmd1, cmd2) ->
       run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep cmd1 >>=
	 begin
	   fun status1 ->
	     match status1 with
	     | Unix.WEXITED 0 -> run_junction_command cmd2
	     | _ -> Lwt.return status1
	 end
    | Junction (Or, cmd1, cmd2) ->
       run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep cmd1 >>=
	 begin
	   fun status1 ->
	     match status1 with
	     | Unix.WEXITED 0 -> Lwt.return status1
	     | _ -> run_junction_command cmd2
	 end

  let rec run_sequence_command = function
    | No_sequence x -> run_junction_command x
    | Sequence (cmd1, Synchronous, Some cmd2) ->
       run_junction_command cmd1 >>= fun _ -> run_sequence_command cmd2
    | Sequence (_, Synchronous, None) -> assert false (* not possible *)
    | Sequence (cmd1, Asynchronous, None) ->
       begin
	 Lwt.async (fun () -> run_junction_command cmd1);
	 Lwt.return (Unix.WEXITED 0)
       end
    | Sequence (cmd1, Asynchronous, Some cmd2) ->
       begin
	 Lwt.async (fun () -> run_junction_command cmd1);
	 run_sequence_command cmd2
       end

  let run = run_sequence_command
	
  let process_string_input console str =
    let cmd = Command.parse str in
    begin
      match cmd with
      | Ok cmd ->
	 Logs.info (fun f -> f "processing %a" Command.pp_cmd cmd);
	 run cmd >>= fun ret ->
	 begin
	   let pp fmt = function
	     | Unix.WEXITED i ->
		Format.fprintf fmt "terminated normally and returned %i" i
	     | Unix.WSIGNALED i ->
		Format.fprintf fmt "killed by signal %i" i
	     | Unix.WSTOPPED i ->
		Format.fprintf fmt "stopped by signal %i" i
	   in
	   Logs.info (fun f -> f "%a" pp ret);
	   Lwt.return_unit
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
