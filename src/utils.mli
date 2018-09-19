(* remove the ending-CR if any *)
val remove_cr : string -> string

(* write a string to stdout (no CR) *)
(* this could be in mirage_console *)
val print : string -> unit Lwt.t
