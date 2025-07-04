(lang dune 3.0)

(name ox-fast-mcp)

(generate_opam_files true)

(package
 (name ox-fast-mcp)
 (synopsis "OCaml implementation of FastMCP")
 (description "OCaml implementation of FastMCP using Jane Street libraries")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  (core (>= v0.16.0))
  (core_unix (>= v0.16.0))
  (async (>= v0.16.0))
  (ppx_jane (>= v0.16.0))
  (ppx_yojson_conv (>= v0.16.0))
  (yojson (>= 2.1.0))
  (expect_test_helpers_core :with-test)
  (ppx_expect :with-test)))
 