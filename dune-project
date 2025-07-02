(lang dune 3.0)

(name ox-fast-mcp)

(generate_opam_files true)

(package
 (name ox-fast-mcp)
 (synopsis "Fast MCP implementation in OCaml")
 (description "Fast MCP implementation in OCaml")
 (depends
  (ocaml (>= 4.14.0))
  dune
  core
  yojson
  uri
  re
  logs
  lwt
  lwt_ppx
  ppx_fields_conv
  ppx_jane
  ppx_deriving
  ppx_deriving_yojson
  cohttp-lwt-unix
  base64
  fmt
  (alcotest :with-test)
  (alcotest-async :with-test)
  (odoc :with-doc))
  (license MIT)
  (authors "Norwen Gonio")
  (maintainers "neuro-ng@github.com")
 (source (github neuro-ng/ox-fast-mcp)))
 