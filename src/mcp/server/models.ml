open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type initialization_options = {
  server_name : string;
  server_version : string;
  capabilities : Mcp.Types.server_capabilities;
  instructions : string option;
}
[@@deriving sexp, yojson]
