open Core

(* Tests for icon support across all MCP object types *)

(* ============================================================================
   Icon Type Tests - Core functionality
   ============================================================================ *)

let%expect_test "icon creation - with all fields" =
  (* Test that icons can be created with all fields *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/icon.png";
      mime_type = Some "image/png";
      sizes = Some [ "48x48"; "96x96" ];
    }
  in
  print_s [%sexp (icon : Mcp.Types.icon)];
  [%expect
    {|
    ((src https://example.com/icon.png) (mime_type (image/png))
     (sizes ((48x48 96x96)))) |}]

let%expect_test "icon creation - with only src" =
  (* Test that icons work with only the src field *)
  let icon : Mcp.Types.icon =
    { src = "https://example.com/logo.svg"; mime_type = None; sizes = None }
  in
  print_s [%sexp (icon : Mcp.Types.icon)];
  [%expect {| ((src https://example.com/logo.svg) (mime_type ()) (sizes ())) |}]

let%expect_test "icon creation - with data URI" =
  (* Test using data URIs for icons *)
  let data_uri =
    "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0OCIgaGVpZ2h0PSI0OCI+PGNpcmNsZSBjeD0iMjQiIGN5PSIyNCIgcj0iMjAiIGZpbGw9IiM0Mjg1RjQiLz48L3N2Zz4="
  in
  let icon : Mcp.Types.icon =
    { src = data_uri; mime_type = Some "image/svg+xml"; sizes = None }
  in
  printf "Icon src starts with 'data:': %b\n"
    (String.is_prefix icon.src ~prefix:"data:");
  printf "Icon mime_type: %s\n" (Option.value icon.mime_type ~default:"(none)");
  [%expect
    {|
    Icon src starts with 'data:': true
    Icon mime_type: image/svg+xml |}]

let%expect_test "icon creation - with multiple sizes" =
  (* Test that multiple icon sizes can be specified *)
  let icon : Mcp.Types.icon =
    {
      src = "https://cdn.example.com/icons/app-icon.png";
      mime_type = Some "image/png";
      sizes = Some [ "48x48"; "96x96"; "144x144"; "any" ];
    }
  in
  printf "Icon has sizes: %b\n" (Option.is_some icon.sizes);
  (match icon.sizes with
  | None -> ()
  | Some sizes -> printf "Number of sizes: %d\n" (List.length sizes));
  [%expect {|
    Icon has sizes: true
    Number of sizes: 4 |}]

(* ============================================================================
   Icon JSON Serialization Tests
   ============================================================================ *)

let%expect_test "icon serialization - full icon" =
  (* Test that icons serialize correctly to JSON *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/icon.png";
      mime_type = Some "image/png";
      sizes = Some [ "48x48" ];
    }
  in
  let json = Mcp.Types.yojson_of_icon icon in
  print_endline (Yojson.Safe.to_string json);
  [%expect
    {| {"src":"https://example.com/icon.png","mimeType":"image/png","sizes":["48x48"]} |}]

let%expect_test "icon serialization - minimal icon" =
  (* Test that minimal icons serialize correctly *)
  let icon : Mcp.Types.icon =
    { src = "https://example.com/logo.svg"; mime_type = None; sizes = None }
  in
  let json = Mcp.Types.yojson_of_icon icon in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"src":"https://example.com/logo.svg"} |}]

let%expect_test "icon deserialization - from JSON" =
  (* Test that icons can be deserialized from JSON *)
  let json_str =
    {|{"src":"https://example.com/icon.png","mimeType":"image/png","sizes":["48x48","96x96"]}|}
  in
  let json = Yojson.Safe.from_string json_str in
  let icon = Mcp.Types.icon_of_yojson json in
  print_s [%sexp (icon : Mcp.Types.icon)];
  [%expect
    {|
    ((src https://example.com/icon.png) (mime_type (image/png))
     (sizes ((48x48 96x96)))) |}]

(* ============================================================================
   Icon Usage in MCP Types
   ============================================================================ *)

let%expect_test "implementation with icons" =
  (* Test that implementation (server info) can have icons *)
  let icon1 : Mcp.Types.icon =
    {
      src = "https://example.com/icon-48.png";
      mime_type = Some "image/png";
      sizes = Some [ "48x48" ];
    }
  in
  let icon2 : Mcp.Types.icon =
    {
      src = "https://example.com/icon-96.png";
      mime_type = Some "image/png";
      sizes = Some [ "96x96" ];
    }
  in
  let impl : Mcp.Types.implementation =
    {
      version = "1.0.0";
      website_url = Some "https://example.com";
      icons = Some [ icon1; icon2 ];
      base_metadata = { name = "my-server"; title = Some "My Server" };
    }
  in
  printf "Implementation has website_url: %b\n"
    (Option.is_some impl.website_url);
  printf "Implementation has icons: %b\n" (Option.is_some impl.icons);
  (match impl.icons with
  | None -> ()
  | Some icons -> printf "Number of icons: %d\n" (List.length icons));
  [%expect
    {|
    Implementation has website_url: true
    Implementation has icons: true
    Number of icons: 2 |}]

let%expect_test "tool with icons" =
  (* Test that tools can have icons *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/tool-icon.svg";
      mime_type = Some "image/svg+xml";
      sizes = None;
    }
  in
  let tool : Mcp.Types.tool =
    {
      description = Some "A test tool";
      input_schema = `Assoc [ ("type", `String "object") ];
      output_schema = None;
      annotations = None;
      icons = Some [ icon ];
      meta = None;
      base_metadata = { name = "test_tool"; title = Some "Test Tool" };
    }
  in
  printf "Tool has icons: %b\n" (Option.is_some tool.icons);
  printf "Tool name: %s\n" tool.base_metadata.name;
  [%expect {|
    Tool has icons: true
    Tool name: test_tool |}]

let%expect_test "resource with icons" =
  (* Test that resources can have icons *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/file-icon.png";
      mime_type = Some "image/png";
      sizes = Some [ "32x32" ];
    }
  in
  let resource : Mcp.Types.resource =
    {
      uri = "file:///path/to/file.txt";
      description = Some "A text file";
      mime_type = Some "text/plain";
      size = Some 1024;
      annotations = None;
      icons = Some [ icon ];
      meta = None;
      base_metadata = { name = "file.txt"; title = Some "File.txt" };
    }
  in
  printf "Resource has icons: %b\n" (Option.is_some resource.icons);
  printf "Resource URI: %s\n" resource.uri;
  [%expect
    {|
    Resource has icons: true
    Resource URI: file:///path/to/file.txt |}]

let%expect_test "resource_template with icons" =
  (* Test that resource templates can have icons *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/folder-icon.svg";
      mime_type = Some "image/svg+xml";
      sizes = None;
    }
  in
  let template : Mcp.Types.resource_template =
    {
      uri_template = "file:///{path}";
      description = Some "A file template";
      mime_type = None;
      annotations = None;
      icons = Some [ icon ];
      meta = None;
      base_metadata = { name = "file_template"; title = Some "File Template" };
    }
  in
  printf "Template has icons: %b\n" (Option.is_some template.icons);
  printf "Template URI: %s\n" template.uri_template;
  [%expect {|
    Template has icons: true
    Template URI: file:///{path} |}]

let%expect_test "prompt with icons" =
  (* Test that prompts can have icons *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/prompt-icon.png";
      mime_type = Some "image/png";
      sizes = Some [ "64x64" ];
    }
  in
  let prompt : Mcp.Types.prompt =
    {
      description = Some "A test prompt";
      arguments = None;
      icons = Some [ icon ];
      meta = None;
      base_metadata = { name = "test_prompt"; title = Some "Test Prompt" };
    }
  in
  printf "Prompt has icons: %b\n" (Option.is_some prompt.icons);
  printf "Prompt name: %s\n" prompt.base_metadata.name;
  [%expect {|
    Prompt has icons: true
    Prompt name: test_prompt |}]

(* ============================================================================
   Icon Edge Cases
   ============================================================================ *)

let%expect_test "icon with empty sizes list" =
  (* Test that icons work with empty sizes list *)
  let icon : Mcp.Types.icon =
    {
      src = "https://example.com/icon.png";
      mime_type = Some "image/png";
      sizes = Some [];
    }
  in
  printf "Icon has sizes field: %b\n" (Option.is_some icon.sizes);
  (match icon.sizes with
  | None -> printf "No sizes\n"
  | Some sizes -> printf "Sizes list length: %d\n" (List.length sizes));
  [%expect {|
    Icon has sizes field: true
    Sizes list length: 0 |}]

let%expect_test "types without icons work correctly" =
  (* Test that types work without icons (backward compatibility) *)
  let tool : Mcp.Types.tool =
    {
      description = Some "A tool without icons";
      input_schema = `Assoc [ ("type", `String "object") ];
      output_schema = None;
      annotations = None;
      icons = None;
      meta = None;
      base_metadata = { name = "simple_tool"; title = None };
    }
  in
  let resource : Mcp.Types.resource =
    {
      uri = "https://example.com/resource";
      description = None;
      mime_type = None;
      size = None;
      annotations = None;
      icons = None;
      meta = None;
      base_metadata = { name = "simple_resource"; title = None };
    }
  in
  printf "Tool has icons: %b\n" (Option.is_some tool.icons);
  printf "Resource has icons: %b\n" (Option.is_some resource.icons);
  [%expect {|
    Tool has icons: false
    Resource has icons: false |}]
