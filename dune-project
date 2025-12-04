(lang dune 3.0)

(name ox-fast-mcp)

(generate_opam_files true)

(package
 (name ox-fast-mcp)
 (synopsis "OCaml implementation of FastMCP")
 (description "OCaml implementation of FastMCP using Jane Street libraries")
 (depends
  (ocaml
   (>= 4.14.0))
  (ocaml-variants
   (>= 5.2.0+ox))
  (dune
   (>= 3.0))
  (core
   (>= v0.18~preview))
  (core_unix
   (>= v0.18~preview))
  (async
   (>= v0.18~preview))
  (ppx_jane
   (>= v0.18~preview))
  (yojson
   (>= 2.1.0))
  (base64
   (>= 3.5.0))
  lwt
  lwt_ppx
  (cohttp-lwt-unix
   (>= 5.0.0))
  (uri
   (>= 4.0.0))
  uri-sexp
  (opium
   (>= 0.20.0))
  (magic-mime
   (>= 1.0.0))
  (x509
   (>= 0.16.0))
  mirage-crypto
  mirage-crypto-pk
  jose
  (alcotest :with-test)
  (ounit2 :with-test)
  (expect_test_helpers_core :with-test)
  (expect_test_helpers_async :with-test)
  (ppx_expect :with-test)))
