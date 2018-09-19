type out_stream = Stdout | Stdout_stderr

type junction_op = And | Or

type synchronous_mode = Synchronous | Asynchronous

type basic_command = Lwt_process.command

type pipe_command =
  | No_pipe of basic_command
  | Pipe of out_stream * basic_command * pipe_command

type junction_command =
  | No_junction of pipe_command
  | Junction of junction_op * pipe_command * junction_command

type sequence_command =
  | No_sequence of junction_command
  | Sequence of junction_command * synchronous_mode * sequence_command option

type t = sequence_command

let pp_sep fmt () = Format.fprintf fmt " "

let pp_arg fmt arg = Format.fprintf fmt "'%s'" arg

let pp_args fmt args = Format.pp_print_list ~pp_sep pp_arg fmt args

let pp_synchro fmt x =
  Format.fprintf fmt "%c" (if x = Synchronous then ';' else '&')

let pp_basic_command fmt (prg, args) =
  Format.fprintf fmt "Basic ('%s' %a)" prg pp_args (Array.to_list args)

let rec pp_pipe_command fmt = function
  | No_pipe x -> Format.fprintf fmt "No_pipe (%a)" pp_basic_command x
  | Pipe (Stdout, cmd1, cmd2) ->
      Format.fprintf fmt "Pipe (%a | %a)" pp_basic_command cmd1 pp_pipe_command
        cmd2
  | Pipe (Stdout_stderr, cmd1, cmd2) ->
      Format.fprintf fmt "Pipe (%a |& %a)" pp_basic_command cmd1
        pp_pipe_command cmd2

let rec pp_junction_command fmt = function
  | No_junction x -> Format.fprintf fmt "No_junction (%a)" pp_pipe_command x
  | Junction (And, cmd1, cmd2) ->
      Format.fprintf fmt "Junction (%a && %a)" pp_pipe_command cmd1
        pp_junction_command cmd2
  | Junction (Or, cmd1, cmd2) ->
      Format.fprintf fmt "Junction (%a || %a)" pp_pipe_command cmd1
        pp_junction_command cmd2

let rec pp_sequence_command fmt = function
  | No_sequence x ->
      Format.fprintf fmt "No_sequence (%a)" pp_junction_command x
  | Sequence (cmd1, sm, Some cmd2) ->
      Format.fprintf fmt "Seq (%a %a %a)" pp_junction_command cmd1 pp_synchro
        sm pp_sequence_command cmd2
  | Sequence (cmd1, sm, None) ->
      Format.fprintf fmt "Seq (%a %a)" pp_junction_command cmd1 pp_synchro sm

let pp = pp_sequence_command

(* hypothesis: str does not contain ';' '&' '|' '|&' '||' '&&' *)
(* token separator is ' ' *)
let parse_execute str =
  let parts = String.split_on_char ' ' str in
  let parts = List.filter (fun x -> x <> "") parts in
  match parts with
  | [] -> Error (Failure "empty execute_cmd")
  | h :: t -> Ok (h, Array.of_list (h :: t))

(* hypothesis: str does not contain ';' '&' '||' '&&' *)
(* token separators are '|' '|&' *)
let rec parse_pipe str =
  try
    let i = String.index str '|' in
    try
      if str.[i + 1] = '&' then
        let str1 = String.sub str 0 i in
        let str2 = String.sub str (i + 2) (String.length str - i - 2) in
        let cmd1 = parse_execute str1 in
        let cmd2 = parse_pipe str2 in
        match (cmd1, cmd2) with
        | Ok cmd1, Ok cmd2 -> Ok (Pipe (Stdout_stderr, cmd1, cmd2))
        | _ -> Error (Failure "invalid pipe command")
      else
        let str1 = String.sub str 0 i in
        let str2 = String.sub str (i + 1) (String.length str - i - 1) in
        let cmd1 = parse_execute str1 in
        let cmd2 = parse_pipe str2 in
        match (cmd1, cmd2) with
        | Ok cmd1, Ok cmd2 -> Ok (Pipe (Stdout, cmd1, cmd2))
        | _ -> Error (Failure "invalid pipe command")
    with Invalid_argument _ -> Error (Failure "invalid pipe command")
  with Not_found -> (
    match parse_execute str with
    | Ok cmd -> Ok (No_pipe cmd)
    | Error x -> Error x )

let rec index_of_op str c start =
  try
    let i = String.index_from str start c in
    try if str.[i + 1] = c then Some i else index_of_op str c (i + 1)
    with Invalid_argument _ -> None
  with Not_found -> None

(* hypothesis: str does not contain ';' '&' *)
(* token separators are '||' '&&' *)
let rec parse_junction str =
  let i_and = index_of_op str '&' 0 in
  let i_or = index_of_op str '|' 0 in
  let parse_or i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i + 2) (String.length str - i - 2) in
    let cmd1 = parse_pipe str1 in
    let cmd2 = parse_junction str2 in
    match (cmd1, cmd2) with
    | Ok cmd1, Ok cmd2 -> Ok (Junction (Or, cmd1, cmd2))
    | _ -> Error (Failure "invalid OR command")
  in
  let parse_and i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i + 2) (String.length str - i - 2) in
    let cmd1 = parse_pipe str1 in
    let cmd2 = parse_junction str2 in
    match (cmd1, cmd2) with
    | Ok cmd1, Ok cmd2 -> Ok (Junction (And, cmd1, cmd2))
    | _ -> Error (Failure "invalid AND command")
  in
  match (i_and, i_or) with
  | None, None -> (
    match parse_pipe str with
    | Ok cmd -> Ok (No_junction cmd)
    | Error x -> Error x )
  | Some i, None -> parse_and i
  | None, Some i -> parse_or i
  | Some i, Some j -> if i < j then parse_and i else parse_or j

(* only for '&' *)
let rec index_of_sep str start =
  try
    let i = String.index_from str start '&' in
    try
      if str.[i - 1] = '|' || str.[i + 1] = '&' then index_of_sep str (i + 2)
      else Some i
    with Invalid_argument _ -> Some i
  with Not_found -> None

(* token separators are ';' '&' *)
let rec parse_sequence str =
  let i_semicolon = try Some (String.index str ';') with Not_found -> None in
  let i_and = index_of_sep str 0 in
  let parse_sync i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i + 1) (String.length str - i - 1) in
    let cmd1 = parse_junction str1 in
    let cmd2 = parse_sequence str2 in
    match (cmd1, cmd2) with
    | Ok cmd1, Ok cmd2 -> Ok (Sequence (cmd1, Synchronous, Some cmd2))
    | Ok cmd1, Error _ -> Ok (No_sequence cmd1)
    | _ -> Error (Failure "invalid synchronous sequence command")
  in
  let parse_async i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i + 1) (String.length str - i - 1) in
    let cmd1 = parse_junction str1 in
    let cmd2 = parse_sequence str2 in
    match (cmd1, cmd2) with
    | Ok cmd1, Ok cmd2 -> Ok (Sequence (cmd1, Asynchronous, Some cmd2))
    | Ok cmd1, Error _ -> Ok (Sequence (cmd1, Asynchronous, None))
    | _ -> Error (Failure "invalid asynchronous sequence command")
  in
  match (i_semicolon, i_and) with
  | None, None -> (
    match parse_junction str with
    | Ok cmd -> Ok (No_sequence cmd)
    | Error x -> Error x )
  | Some i, None -> parse_sync i
  | None, Some i -> parse_async i
  | Some i, Some j -> if i < j then parse_sync i else parse_async j

let parse = parse_sequence

exception Not_a_builtin

let r_exit_shell = ref false

let execute_builtin prgm _args =
  if prgm = "exit" then (
    r_exit_shell := true ;
    Lwt.return (Unix.WEXITED 0) )
  else Lwt.fail Not_a_builtin

open Lwt.Infix

let rec read_stream st =
  Lwt_stream.get st
  >>= function
  | Some str ->
      Utils.print (Format.asprintf "%s@\n" str) >>= fun () -> read_stream st
  | None -> Lwt.return_unit

let on_exit fd =
  let channel = Lwt_io.of_unix_fd ~mode:Lwt_io.input fd in
  let stream = Lwt_io.read_lines channel in
  read_stream stream >>= fun () -> Lwt_io.close channel

let run_basic_command ?stdin ?stdout ?stderr (prgm, args) =
  Lwt.catch
    (fun () -> execute_builtin prgm args)
    (function
      | Not_a_builtin -> (
        match (stdout, stderr) with
        | Some `Keep, Some `Keep ->
            let stdout_r, stdout_w = Unix.pipe () in
            let stderr_r, stderr_w = Unix.pipe () in
            let cmd =
              Lwt_process.open_process_none ?stdin ~stdout:(`FD_move stdout_w)
                ~stderr:(`FD_move stderr_w) (prgm, args)
            in
            let st = Lwt_main.run cmd#status in
            on_exit stdout_r
            >>= fun () -> on_exit stderr_r >>= fun () -> Lwt.return st
        | Some `Keep, _ ->
            let stdout_r, stdout_w = Unix.pipe () in
            let cmd =
              Lwt_process.open_process_none ?stdin ~stdout:(`FD_move stdout_w)
                ?stderr (prgm, args)
            in
            let st = Lwt_main.run cmd#status in
            on_exit stdout_r >>= fun () -> Lwt.return st
        | _, Some `Keep ->
            let stderr_r, stderr_w = Unix.pipe () in
            let cmd =
              Lwt_process.open_process_none ?stdin ?stdout
                ~stderr:(`FD_move stderr_w) (prgm, args)
            in
            let st = Lwt_main.run cmd#status in
            on_exit stderr_r >>= fun () -> Lwt.return st
        | _ ->
            let cmd =
              Lwt_process.open_process_none ?stdin ?stdout ?stderr (prgm, args)
            in
            let st = Lwt_main.run cmd#status in
            Lwt.return st )
      | exn ->
          Logs.err (fun f ->
              f "uncaught exception from listen callback@\nException: @[%s@]"
                (Printexc.to_string exn) ) ;
          Lwt.return (Unix.WEXITED 0))

let rec run_pipe_command ?stdin ?stdout ?stderr = function
  | No_pipe x -> run_basic_command ?stdin ?stdout ?stderr x
  | Pipe (Stdout, cmd1, cmd2) ->
      let stdout_r, stdout_w = Unix.pipe () in
      run_basic_command ?stdin ~stdout:(`FD_move stdout_w) ?stderr cmd1
      >>= fun _ ->
      run_pipe_command ~stdin:(`FD_move stdout_r) ?stdout ?stderr cmd2
  | Pipe (Stdout_stderr, cmd1, cmd2) ->
      let stdout_r, stdout_w = Unix.pipe () in
      run_basic_command ?stdin ~stdout:(`FD_copy stdout_w)
        ~stderr:(`FD_move stdout_w) cmd1
      >>= fun _ ->
      run_pipe_command ~stdin:(`FD_move stdout_r) ?stdout ?stderr cmd2

let rec run_junction_command = function
  | No_junction x ->
      run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep x
  | Junction (And, cmd1, cmd2) -> (
      run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep cmd1
      >>= fun status1 ->
      match status1 with
      | Unix.WEXITED 0 -> run_junction_command cmd2
      | _ -> Lwt.return status1 )
  | Junction (Or, cmd1, cmd2) -> (
      run_pipe_command ~stdin:`Keep ~stdout:`Keep ~stderr:`Keep cmd1
      >>= fun status1 ->
      match status1 with
      | Unix.WEXITED 0 -> Lwt.return status1
      | _ -> run_junction_command cmd2 )

let rec run_sequence_command = function
  | No_sequence x -> run_junction_command x
  | Sequence (cmd1, Synchronous, Some cmd2) ->
      run_junction_command cmd1 >>= fun _ -> run_sequence_command cmd2
  | Sequence (_, Synchronous, None) -> assert false (* not possible *)
  | Sequence (cmd1, Asynchronous, None) ->
      Lwt.async (fun () -> run_junction_command cmd1) ;
      Lwt.return (Unix.WEXITED 0)
  | Sequence (cmd1, Asynchronous, Some cmd2) ->
      Lwt.async (fun () -> run_junction_command cmd1) ;
      run_sequence_command cmd2

let run = run_sequence_command

let exit_shell () = !r_exit_shell
