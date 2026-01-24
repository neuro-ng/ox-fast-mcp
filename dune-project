(lang dune 3.0)

(name ox-fast-mcp)

(generate_opam_files true)

(package
 (name ox-fast-mcp)
 (synopsis "Fast, functional way to build MCP servers and clients in OCaml")
 (description
  "\n\n\nAn OCaml implementation of FastMCP inspired by the Python library, providing \na high-level, type-safe interface for building Model Context Protocol (MCP) \nservers and clients using functional programming principles.\n\nFeatures:\n- Fast: High-level interface with minimal boilerplate\n- Type-Safe: Leverages OCaml's strong type system  \n- Functional: Built with functional programming principles\n- TDD: Developed using Test-Driven Development\n- Complete: Full MCP protocol implementation for servers and clients\n- Interoperable: Compatible with other MCP implementations\n")
 (depends
  (stdio
   (and
    (>= v0.18~preview.130.36+326)
    :with-test))
  (ptime
   (>= 1.2.0))
  (ppx_yojson_conv_lib
   (>= v0.18~preview.130.36+326))
  (ppx_yojson_conv
   (>= v0.18~preview.130.36+326))
  (ppx_sexp_message
   (>= v0.18~preview.130.36+326))
  (ppx_sexp_conv
   (>= v0.18~preview.130.36+326))
  (ppx_let
   (>= v0.18~preview.130.36+326))
  (ppx_fields_conv
   (>= v0.18~preview.130.36+326))
  (ppx_compare
   (>= v0.18~preview.130.36+326))
  (parsexp
   (>= v0.18~preview.130.36+326))
  (mirage-crypto-rng
   (>= 0.11.3))
  (logs
   (>= 0.9.0))
  (digestif
   (>= 1.3.0))
  (cohttp-lwt
   (>= 6.1.1))
  (cohttp-async
   (>= 6.1.1))
  (cohttp
   (>= 6.1.1))
  (base
   (>= v0.18~preview.130.36+326))
  (async_log
   (>= v0.18~preview.130.36+326))
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
  re
  (websocket-async
   (>= 2.16))
  (alcotest :with-test)
  (ounit2 :with-test)
  (expect_test_helpers_core :with-test)
  (expect_test_helpers_async :with-test)
  (ppx_expect :with-test))
 (license Apache-2.0)
 (authors "Norwen Gonio")
 (homepage "https://github.com/neuro-ng/ox-fast-mcp")
 (maintainers "neuro-ng@github.com")
 (source
  (github neuro-ng/ox-fast-mcp)))
