open! Core

module type Handler = sig
  type t
end

module Level = struct
  module T = struct
    type t =
      | Debug
      | Info
      | Warning
      | Error
      | Critical
    [@@deriving sexp, compare, yojson]

    let level_value = function
      | Debug -> 0
      | Info -> 1
      | Warning -> 2
      | Error -> 3
      | Critical -> 4

    let compare t1 t2 =
      Int.compare (level_value t1) (level_value t2)
  end
  include T
  include Comparable.Make(T)

  let of_string = function
    | "DEBUG" -> Debug
    | "INFO" -> Info
    | "WARNING" -> Warning
    | "ERROR" -> Error
    | "CRITICAL" -> Critical
    | s -> failwith (sprintf "Invalid log level: %s" s)

  let to_string = function
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warning -> "WARNING"
    | Error -> "ERROR"
    | Critical -> "CRITICAL"

  let to_level = function
    | Debug -> `Debug
    | Info -> `Info
    | Warning -> `Warning
    | Error -> `Error
    | Critical -> `Critical

  let compare_level = compare

  let level_ge = (>=)
  let level_le = (<=)
  let level_gt = (>)
  let level_lt = (<)
  let level_eq = (=)
end 