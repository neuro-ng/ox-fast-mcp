exception Not_found_error of string
exception Tool_error of string * exn option

val not_found_error : string -> 'a
val tool_error : ?exn:exn -> string -> 'a 