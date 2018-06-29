
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
  
  let process_string_input console str =
    let cmd = Command.parse str in
    begin
      match cmd with
      | Ok cmd ->
	 let str =
	   Format.fprintf Format.str_formatter "%a" Command.pp_cmd cmd;
	   Format.flush_str_formatter ()
	 in
	  C.log console str
      | Error msg -> C.log console msg
    end >>= fun () ->
    C.log console ("processing '" ^ str ^ "'...")
      
  let rec main_loop console =
    print "> " >>= fun () ->
    let line = Lwt_main.run (C.read console) in
    match line with
    | Ok (`Data data) ->
       let str = Cstruct.to_string data |> remove_cr in
       if str = "" then
	 main_loop console
       else
	 if str = "exit" then
	   C.disconnect console
	 else
	   let last_char = String.get str ((String.length str) - 1) in
	   if last_char = '&' then
	     (Lwt.async (fun () ->
	       C.log console ("input: '" ^ str ^ "' -> DETACH") >>= fun () ->
	       Lwt.catch (fun () -> process_string_input console str)
		 (fun ex ->
		   Logs.err (fun f -> f "uncaught exception \
                            from listen callback \
                            @\nException: @[%s@]"
                   (Printexc.to_string ex));
		   Lwt.return ()
		 )
	      );
	      main_loop console)
	   else
	     (C.log console ("input: '" ^ str ^ "'") >>= fun () ->
	      process_string_input console str >>= fun () ->
	      main_loop console)
    | Ok (`Eof) ->
       C.log console ("input: EOF")
    | Error err ->
       let str = Format.asprintf "error: %a" C.pp_error err in
       C.log console str
      
  let start console (*_fs*) _t = main_loop console

end
