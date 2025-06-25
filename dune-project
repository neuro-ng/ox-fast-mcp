(lang dune 3.0)

(name ox-fast-mcp)

(package
 (name ox-fast-mcp)
 (synopsis "Fast, functional way to build MCP servers and clients in OCaml")
 (description "An OCaml implementation of FastMCP inspired by the Python library, providing a high-level, type-safe interface for building Model Context Protocol (MCP) servers and clients.")
 (depends 
  ocaml
  dune
  lwt
  lwt_ppx
  yojson
  ppx_yojson_conv
  ppx_deriving
  cohttp-lwt-unix
  uri
  logs
  fmt
  alcotest
  (odoc :with-doc))
 (license Apache-2.0)
 (authors "Norwen Gonio")
 (maintainers "neuro-ng@github.com")
 (source (github neuro-ng/ox-fast-mcp))) 