
type t

val pp: Format.formatter -> t -> unit

val parse: string -> t Lwt.result

val run: t -> Unix.process_status Lwt.t

val exit_shell: unit -> bool
