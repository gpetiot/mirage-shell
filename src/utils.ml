
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

let print str = write_one (Cstruct.of_string str)
