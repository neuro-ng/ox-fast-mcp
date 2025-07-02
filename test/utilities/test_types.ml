open Alcotest
open Lwt.Syntax
open Utilities.Types

(** Test type matching utilities (equivalent to Python is_class_member_of_type) *)
let test_content_type_matching () =
  let text_content = Text "hello" in
  let image_content = Image { 
    data = "test"; 
    mime_type = "image/png"; 
    annotations = None;
    format = None;
  } in
  let audio_content = Audio { 
    data = "test"; 
    mime_type = "audio/wav"; 
    annotations = None; 
    format = None;
  } in
  let file_content = File { 
    data = "test"; 
    mime_type = "application/pdf"; 
    name = None; 
    annotations = None; 
    format = None;
  } in
  
  (* Test same type matching *)
  check bool "text matches text" true (is_content_type_match text_content (Text ""));
  check bool "image matches image" true (is_content_type_match image_content (Image {
    data = "";
    mime_type = "";
    annotations = None;
    format = None;
  }));
  check bool "audio matches audio" true (is_content_type_match audio_content (Audio {
    data = "";
    mime_type = "";
    annotations = None;
    format = None;
  }));
  check bool "file matches file" true (is_content_type_match file_content (File {
    data = "";
    mime_type = "";
    name = None;
    annotations = None;
    format = None;
  }));
  
  (* Test different type matching *)
  check bool "text doesn't match image" false (is_content_type_match text_content image_content);
  check bool "image doesn't match audio" false (is_content_type_match image_content audio_content);
  check bool "audio doesn't match file" false (is_content_type_match audio_content file_content);
  check bool "file doesn't match text" false (is_content_type_match file_content text_content)

(** Test option type safety utilities (equivalent to Python issubclass_safe) *)
let test_option_type_safety () =
  let is_string_test s = String.length s > 0 in
  let is_positive_int i = i > 0 in
  
  check bool "some string passes test" true (is_option_type_safe (Some "hello") is_string_test);
  check bool "some empty string fails test" false (is_option_type_safe (Some "") is_string_test);
  check bool "none always fails" false (is_option_type_safe None is_string_test);
  
  check bool "some positive int passes" true (is_option_type_safe (Some 42) is_positive_int);
  check bool "some negative int fails" false (is_option_type_safe (Some (-1)) is_positive_int);
  check bool "none int fails" false (is_option_type_safe None is_positive_int)

(** Test content helper functions (equivalent to Python Image/Audio/File classes) *)
let test_content_helper_functions () =
  let text_content = Text "Hello, World!" in
  let image_content = Image { 
    data = "base64imagedata"; 
    mime_type = "image/jpeg"; 
    annotations = None;
    format = Some "jpeg";
  } in
  let audio_content = Audio { 
    data = "base64audiodata"; 
    mime_type = "audio/mp3"; 
    annotations = None;
    format = Some "mp3";
  } in
  let file_content = File { 
    data = "base64filedata"; 
    mime_type = "application/pdf"; 
    name = Some "document.pdf"; 
    annotations = None;
    format = Some "pdf";
  } in
  
  (* Test MIME type extraction *)
  check string "text mime type" "text/plain" (get_mime_type_from_content text_content);
  check string "image mime type" "image/jpeg" (get_mime_type_from_content image_content);
  check string "audio mime type" "audio/mp3" (get_mime_type_from_content audio_content);
  check string "file mime type" "application/pdf" (get_mime_type_from_content file_content);
  
  (* Test data extraction *)
  check string "text data" "Hello, World!" (get_content_data text_content);
  check string "image data" "base64imagedata" (get_content_data image_content);
  check string "audio data" "base64audiodata" (get_content_data audio_content);
  check string "file data" "base64filedata" (get_content_data file_content)

(** Test content creation utilities (equivalent to Python Image/Audio/File initialization) *)
let test_content_creation_utilities () =
  (* Test image creation *)
  let image_png = create_image_from_data "test_image_data" in
  let image_jpeg = create_image_from_data ~format:"jpeg" "test_image_data" in
  
  check string "default image format" "image/png" (get_mime_type_from_content image_png);
  check string "custom image format" "image/jpeg" (get_mime_type_from_content image_jpeg);
  check string "image data preserved" "test_image_data" (get_content_data image_png);
  
  (* Test audio creation *)
  let audio_wav = create_audio_from_data "test_audio_data" in
  let audio_mp3 = create_audio_from_data ~format:"mp3" "test_audio_data" in
  
  check string "default audio format" "audio/wav" (get_mime_type_from_content audio_wav);
  check string "custom audio format" "audio/mp3" (get_mime_type_from_content audio_mp3);
  check string "audio data preserved" "test_audio_data" (get_content_data audio_wav);
  
  (* Test file creation *)
  let file_default = create_file_from_data "test_file_data" in
  let file_pdf = create_file_from_data ~format:"pdf" ~name:"test.pdf" "test_file_data" in
  
  check string "default file format" "application/octet-stream" (get_mime_type_from_content file_default);
  check string "custom file format" "application/pdf" (get_mime_type_from_content file_pdf);
  check string "file data preserved" "test_file_data" (get_content_data file_default)

(** Test base64 encoding/decoding utilities (used by Image/Audio/File) *)
let test_base64_encoding () =
  let test_data = "Hello, World! This is a test string for base64 encoding." in
  
  (* Test encoding then decoding *)
  let encoded = base64_encode test_data in
  check bool "base64 encoded string not empty" true (String.length encoded > 0);
  
  let decoded_opt = base64_decode encoded in
  check bool "base64 decoding successful" true (Option.is_some decoded_opt);
  
  (match decoded_opt with
  | Some decoded ->
    check string "base64 roundtrip successful" test_data decoded
  | None ->
    check bool "base64 decoding should not fail" false true);
  
  (* Test invalid base64 decoding *)
  let invalid_decoded = base64_decode "invalid!base64@string#" in
  check bool "invalid base64 returns None" true (Option.is_none invalid_decoded)

(** Test parameter finding utilities (equivalent to Python find_kwarg_by_type) *)
let test_parameter_finding () =
  let params = [
    { param_name = "text"; param_type = "string"; param_required = true; param_description = None; param_default = None };
    { param_name = "count"; param_type = "integer"; param_required = false; param_description = None; param_default = None };
    { param_name = "enabled"; param_type = "boolean"; param_required = true; param_description = None; param_default = None };
    { param_name = "data"; param_type = "object"; param_required = false; param_description = None; param_default = None };
  ] in
  
  (* Test exact type matches *)
  let string_param = find_param_by_type params "string" in
  check bool "string parameter found" true (Option.is_some string_param);
  (match string_param with
   | Some param -> check string "string parameter name" "text" param.param_name
   | None -> fail "Expected string parameter");
  
  let integer_param = find_param_by_type params "integer" in
  check bool "integer parameter found" true (Option.is_some integer_param);
  (match integer_param with
   | Some param -> check string "integer parameter name" "count" param.param_name
   | None -> fail "Expected integer parameter");
  
  (* Test non-matching type *)
  let missing_param = find_param_by_type params "array" in
  check bool "non-existent parameter not found" true (Option.is_none missing_param)

(** Test content type creation and basic serialization *)
let test_content_type_creation () =
  (* Test text content *)
  let text_content = create_text_content "Hello, MCP!" in
  check bool "text content creation" true
    (match text_content with Text _ -> true | _ -> false);

  (* Test image content *)
  let image_content = create_image_content 
    ~data:"base64imagedata" 
    ~mime_type:"image/png" 
    ~format:"png"
    () in
  check bool "image content creation" true
    (match image_content with Image _ -> true | _ -> false);

  (* Test audio content *)
  let audio_content = create_audio_content 
    ~data:"base64audiodata" 
    ~mime_type:"audio/wav" 
    ~format:"wav"
    () in
  check bool "audio content creation" true
    (match audio_content with Audio _ -> true | _ -> false);

  (* Test file content *)
  let file_content = create_file_content 
    ~data:"base64filedata" 
    ~mime_type:"application/pdf" 
    ~name:"test.pdf"
    ~format:"pdf"
    () in
  check bool "file content creation" true
    (match file_content with File _ -> true | _ -> false)

(** Test content with annotations (equivalent to Python edge cases) *)
let test_content_with_annotations () =
  (* Test content with annotations *)
  let annotated_image = Image {
    data = "annotated_image_data";
    mime_type = "image/png";
    annotations = Some [("source", `String "camera"); ("quality", `Int 95)];
    format = Some "png";
  } in
  
  check string "annotated image mime type" "image/png" (get_mime_type_from_content annotated_image);
  check string "annotated image data" "annotated_image_data" (get_content_data annotated_image);
  
  (* Test file with name and annotations *)
  let complex_file = File {
    data = "complex_file_data";
    mime_type = "application/json";
    name = Some "config.json";
    annotations = Some [("version", `String "1.0"); ("encrypted", `Bool false)];
    format = Some "json";
  } in
  
  check string "complex file mime type" "application/json" (get_mime_type_from_content complex_file);
  check string "complex file data" "complex_file_data" (get_content_data complex_file);
  
  (* Test empty/minimal content *)
  let minimal_text = Text "" in
  check string "minimal text data" "" (get_content_data minimal_text)

(** Test MIME type detection from file extensions *)
let test_mime_type_from_extension () =
  (* Test image MIME types *)
  check string "PNG MIME type" "image/png" (get_mime_type_from_extension ".png");
  check string "JPEG MIME type" "image/jpeg" (get_mime_type_from_extension ".jpg");
  check string "JPEG MIME type alt" "image/jpeg" (get_mime_type_from_extension ".jpeg");
  check string "GIF MIME type" "image/gif" (get_mime_type_from_extension ".gif");
  check string "WebP MIME type" "image/webp" (get_mime_type_from_extension ".webp");
  
  (* Test audio MIME types *)
  check string "WAV MIME type" "audio/wav" (get_mime_type_from_extension ".wav");
  check string "MP3 MIME type" "audio/mpeg" (get_mime_type_from_extension ".mp3");
  check string "OGG MIME type" "audio/ogg" (get_mime_type_from_extension ".ogg");
  check string "M4A MIME type" "audio/mp4" (get_mime_type_from_extension ".m4a");
  check string "FLAC MIME type" "audio/flac" (get_mime_type_from_extension ".flac");
  
  (* Test file MIME types *)
  check string "TXT MIME type" "text/plain" (get_mime_type_from_extension ".txt");
  check string "PDF MIME type" "application/pdf" (get_mime_type_from_extension ".pdf");
  check string "JSON MIME type" "application/json" (get_mime_type_from_extension ".json");
  
  (* Test unknown extension *)
  check string "Unknown MIME type" "application/octet-stream" (get_mime_type_from_extension ".unknown")

(** Test error handling for missing data *)
let test_content_validation () =
  (* Test validation of content creation *)
  let valid_image = create_image_from_data "valid_data" in
  check bool "valid image created" true
    (match valid_image with Image _ -> true | _ -> false);
  
  let valid_audio = create_audio_from_data "valid_data" in
  check bool "valid audio created" true
    (match valid_audio with Audio _ -> true | _ -> false);
  
  let valid_file = create_file_from_data "valid_data" in
  check bool "valid file created" true
    (match valid_file with File _ -> true | _ -> false);
  
  (* Test parameter validation *)
  let empty_params = [] in
  let missing_param = find_param_by_type empty_params "string" in
  check bool "missing parameter returns None" true (Option.is_none missing_param)

(** Test resource types and functionality *)
let test_resource_types () =
  let text_resource = TextResource {
    text = "Hello, World!";
    mime_type = "text/plain";
  } in
  let* text_result = read_resource text_resource in
  (match text_result with
   | Ok (Text content) -> check string "text content" "Hello, World!" content
   | _ -> fail "Expected Text content");
  Lwt.return_unit

(** Test resource error handling *)
let test_resource_errors () =
  let missing_file = FileResource {
    path = "/nonexistent/file.txt";
    is_binary = false;
    mime_type = "text/plain";
  } in
  let* missing_result = read_resource missing_file in
  (match missing_result with
   | Error (ResourceNotFound _) -> check bool "missing file error" true true
   | _ -> fail "Expected ResourceNotFound error");
  Lwt.return_unit

(** Test content types with format *)
let test_content_with_format () =
  (* Test image with format *)
  let image_content = Image {
    data = "base64imagedata";
    mime_type = "image/png";
    annotations = None;
    format = Some "png";
  } in
  check string "image mime type" "image/png" (get_mime_type_from_content image_content);
  check string "image data" "base64imagedata" (get_content_data image_content);
  (match image_content with
   | Image { format = Some fmt; _ } -> check string "image format" "png" fmt
   | _ -> fail "Expected format field");

  (* Test audio with format *)
  let audio_content = Audio {
    data = "base64audiodata";
    mime_type = "audio/mp3";
    annotations = None;
    format = Some "mp3";
  } in
  check string "audio mime type" "audio/mp3" (get_mime_type_from_content audio_content);
  check string "audio data" "base64audiodata" (get_content_data audio_content);
  (match audio_content with
   | Audio { format = Some fmt; _ } -> check string "audio format" "mp3" fmt
   | _ -> fail "Expected format field");

  (* Test file with format *)
  let file_content = File {
    data = "base64filedata";
    mime_type = "application/pdf";
    name = Some "test.pdf";
    annotations = None;
    format = Some "pdf";
  } in
  check string "file mime type" "application/pdf" (get_mime_type_from_content file_content);
  check string "file data" "base64filedata" (get_content_data file_content);
  (match file_content with
   | File { format = Some fmt; name = Some n; _ } ->
     check string "file format" "pdf" fmt;
     check string "file name" "test.pdf" n
   | _ -> fail "Expected format and name fields")

(** Test content creation with format *)
let test_content_creation_with_format () =
  (* Test image creation with format *)
  let image = create_image_content
    ~data:"test_data"
    ~mime_type:"image/png"
    ~format:"png"
    () in
  (match image with
   | Image { format = Some fmt; _ } -> check string "created image format" "png" fmt
   | _ -> fail "Expected image with format");

  (* Test audio creation with format *)
  let audio = create_audio_content
    ~data:"test_data"
    ~mime_type:"audio/mp3"
    ~format:"mp3"
    () in
  (match audio with
   | Audio { format = Some fmt; _ } -> check string "created audio format" "mp3" fmt
   | _ -> fail "Expected audio with format");

  (* Test file creation with format *)
  let file = create_file_content
    ~data:"test_data"
    ~mime_type:"application/pdf"
    ~name:"test.pdf"
    ~format:"pdf"
    () in
  (match file with
   | File { format = Some fmt; _ } -> check string "created file format" "pdf" fmt
   | _ -> fail "Expected file with format")

(** Main test suite *)
let () = run "Types Module Tests" [
  ("Type Matching Utilities", [
    test_case "Content type matching" `Quick test_content_type_matching;
    test_case "Option type safety" `Quick test_option_type_safety;
  ]);
  ("Content Helper Functions", [
    test_case "Content helper functions" `Quick test_content_helper_functions;
    test_case "Content creation utilities" `Quick test_content_creation_utilities;
    test_case "Content with annotations" `Quick test_content_with_annotations;
  ]);
  ("Base64 and Encoding", [
    test_case "Base64 encoding/decoding" `Quick test_base64_encoding;
  ]);
  ("Parameter Utilities", [
    test_case "Parameter finding" `Quick test_parameter_finding;
  ]);
  ("Content Types", [
    test_case "Content type creation" `Quick test_content_type_creation;
  ]);
  ("MIME Type Detection", [
    test_case "MIME type from extensions" `Quick test_mime_type_from_extension;
  ]);
  ("Validation", [
    test_case "Content validation" `Quick test_content_validation;
  ]);
  ("Resource Types", [
    test_case "Resource type handling" `Quick (fun () -> Lwt_main.run (test_resource_types ()));
    test_case "Resource error handling" `Quick (fun () -> Lwt_main.run (test_resource_errors ()));
  ]);
  ("Content Types with Format", [
    test_case "Content with format" `Quick test_content_with_format;
    test_case "Content creation with format" `Quick test_content_creation_with_format;
  ]);
] 