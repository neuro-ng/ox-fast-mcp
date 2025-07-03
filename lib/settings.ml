type t = {
  deprecation_warnings : bool;
}

let create ?(deprecation_warnings=true) () = {
  deprecation_warnings;
}

let default = create ()

let get_deprecation_warnings t = t.deprecation_warnings

let set_deprecation_warnings _t value = 
  { deprecation_warnings = value } 