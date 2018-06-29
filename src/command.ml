
type out_stream =
  | Stdout
  | Stdout_stderr
type junction_op =
  | And
  | Or
type synchronous_mode =
  | Synchronous
  | Asynchronous
      
(* constructors ordered by decreasing priority *)
type command =
  | Execute of string * string list
  | Pipe of out_stream * command * command
  | Junction of junction_op * command * command
  | Sequence of command * synchronous_mode * command option

let pp_sep fmt () = Format.fprintf fmt " "
let pp_arg fmt arg = Format.fprintf fmt "%s" arg
let pp_args fmt args = Format.pp_print_list ~pp_sep pp_arg fmt args
let pp_synchro fmt x =
  Format.fprintf fmt "%c" (if x = Synchronous then ';' else '&')
let rec pp_cmd fmt = function
  | Execute (prg, args) ->
     Format.fprintf fmt "Execute (%s %a)" prg pp_args args
  | Pipe (Stdout, cmd1, cmd2) ->
     Format.fprintf fmt "Pipe (%a | %a)" pp_cmd cmd1 pp_cmd cmd2
  | Pipe (Stdout_stderr, cmd1, cmd2) ->
     Format.fprintf fmt "Pipe (%a |& %a)" pp_cmd cmd1 pp_cmd cmd2
  | Junction (And, cmd1, cmd2) ->
     Format.fprintf fmt "Junction (%a && %a)" pp_cmd cmd1 pp_cmd cmd2
  | Junction (Or, cmd1, cmd2) ->
     Format.fprintf fmt "Junction (%a || %a)" pp_cmd cmd1 pp_cmd cmd2
  | Sequence (cmd1, sm, Some cmd2) ->
     Format.fprintf fmt "Seq (%a %a %a)" pp_cmd cmd1 pp_synchro sm pp_cmd cmd2
  | Sequence (cmd1, sm, None) ->
     Format.fprintf fmt "Seq (%a %a)" pp_cmd cmd1 pp_synchro sm

       
(* hypothesis: str does not contain ';' '&' '|' '|&' '||' '&&' *)
(* token separator is ' ' *)
let parse_execute str =
  let parts = String.split_on_char ' ' str in
  let parts = List.filter (fun x -> x <> "") parts in
  match parts with
  | [] -> Error "empty execute_cmd"
  | h :: t -> Ok (Execute (h, t))

(* hypothesis: str does not contain ';' '&' '||' '&&' *)
(* token separators are '|' '|&' *)
let rec parse_pipe str =
  try
    let i = String.index str '|' in
    try
      if String.get str (i+1) = '&' then
	let str1 = String.sub str 0 i in
	let str2 = String.sub str (i+2) ((String.length str) - i - 2) in
	let cmd1 = parse_execute str1 in
	let cmd2 = parse_pipe str2 in
	match cmd1, cmd2 with
	| Ok cmd1, Ok cmd2 -> Ok (Pipe (Stdout_stderr, cmd1, cmd2))
	| _ -> Error "invalid pipe command"
      else
	let str1 = String.sub str 0 i in
	let str2 = String.sub str (i+1) ((String.length str) - i - 1) in
	let cmd1 = parse_execute str1 in
	let cmd2 = parse_pipe str2 in
	match cmd1, cmd2 with
	| Ok cmd1, Ok cmd2 -> Ok (Pipe (Stdout, cmd1, cmd2))
	| _ -> Error "invalid pipe command"
    with
      Invalid_argument _ -> Error "invalid pipe command"
  with
    Not_found -> parse_execute str

      
let rec index_of_op str c start =
  try
    let i = String.index_from str start c in
    try
      if String.get str (i+1) = c then Some i
      else index_of_op str c (i+1)
    with Invalid_argument _ -> None
  with Not_found -> None

    
(* hypothesis: str does not contain ';' '&' *)
(* token separators are '||' '&&' *)
let rec parse_junction str =
  let i_and = index_of_op str '&' 0 in
  let i_or = index_of_op str '|' 0 in
  let parse_or i =
     let str1 = String.sub str 0 i in
     let str2 = String.sub str (i+2) ((String.length str) - i - 2) in
     let cmd1 = parse_pipe str1 in
     let cmd2 = parse_junction str2 in
     match cmd1, cmd2 with
     | Ok cmd1, Ok cmd2 -> Ok (Junction (Or, cmd1, cmd2))
     | _ -> Error "invalid OR command"
  in
  let parse_and i =
     let str1 = String.sub str 0 i in
     let str2 = String.sub str (i+2) ((String.length str) - i - 2) in
     let cmd1 = parse_pipe str1 in
     let cmd2 = parse_junction str2 in
     match cmd1, cmd2 with
     | Ok cmd1, Ok cmd2 -> Ok (Junction (And, cmd1, cmd2))
     | _ -> Error "invalid AND command"
  in
  match i_and, i_or with
  | None, None -> parse_pipe str
  | Some i, None -> parse_and i
  | None, Some i -> parse_or i
  | Some i, Some j -> if i < j then parse_and i else parse_or j


(* only for '&' *)
let rec index_of_sep str start =
  try
    let i = String.index_from str start '&' in
    try
      if String.get str (i+1) = '&' then index_of_sep str (i+2)
      else Some i
    with Invalid_argument _ -> Some i
  with Not_found -> None
    
(* token separators are ';' '&' *)
let rec parse_sequence str =
  let i_semicolon =
    try Some (String.index str ';')
    with Not_found -> None
  in
  let i_and = index_of_sep str 0 in
  let parse_sync i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i+1) ((String.length str) - i - 1) in
    let cmd1 = parse_junction str1 in
    let cmd2 = parse_sequence str2 in
    match cmd1, cmd2 with
    | Ok cmd1, Ok cmd2 -> Ok (Sequence (cmd1, Synchronous, Some cmd2))
    | Ok cmd1, Error _ -> Ok cmd1
    | _ -> Error "invalid synchronous sequence command"
  in
  let parse_async i =
    let str1 = String.sub str 0 i in
    let str2 = String.sub str (i+1) ((String.length str) - i - 1) in
    let cmd1 = parse_junction str1 in
    let cmd2 = parse_sequence str2 in
    match cmd1, cmd2 with
    | Ok cmd1, Ok cmd2 -> Ok (Sequence (cmd1, Asynchronous, Some cmd2))
    | Ok cmd1, Error _ -> Ok (Sequence (cmd1, Asynchronous, None))
    | _ -> Error "invalid asynchronous sequence command"
  in
  match i_semicolon, i_and with
  | None, None -> parse_junction str
  | Some i, None -> parse_sync i
  | None, Some i -> parse_async i
  | Some i, Some j -> if i < j then parse_sync i else parse_async j
     
let parse = parse_sequence
