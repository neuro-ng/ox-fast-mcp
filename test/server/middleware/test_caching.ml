(** Tests for response caching middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Caching

(* =============================================================================
   Tests for Shared_method_settings
   ============================================================================= *)

let%expect_test "Shared_method_settings - default enabled" =
  let settings = Shared_method_settings.default () in
  printf "is_enabled: %b\n" (Shared_method_settings.is_enabled settings);
  [%expect {| is_enabled: true |}]

let%expect_test "Shared_method_settings - can disable" =
  let settings = Shared_method_settings.create ~enabled:false () in
  printf "is_enabled: %b\n" (Shared_method_settings.is_enabled settings);
  [%expect {| is_enabled: false |}]

let%expect_test "Shared_method_settings - custom TTL" =
  let settings = Shared_method_settings.create ~ttl:120 () in
  printf "ttl: %d\n" (Shared_method_settings.get_ttl settings ~default:3600);
  [%expect {| ttl: 120 |}]

let%expect_test "Shared_method_settings - default TTL" =
  let settings = Shared_method_settings.default () in
  printf "ttl: %d\n" (Shared_method_settings.get_ttl settings ~default:3600);
  [%expect {| ttl: 3600 |}]

(* =============================================================================
   Tests for Call_tool_settings
   ============================================================================= *)

let%expect_test "Call_tool_settings - default matches all tools" =
  let settings = Call_tool_settings.default () in
  printf "matches 'add': %b\n" (Call_tool_settings.matches_tool "add" settings);
  printf "matches 'multiply': %b\n"
    (Call_tool_settings.matches_tool "multiply" settings);
  [%expect {|
    matches 'add': true
    matches 'multiply': true
    |}]

let%expect_test "Call_tool_settings - included_tools filtering" =
  let settings =
    Call_tool_settings.create ~included_tools:[ "add"; "subtract" ] ()
  in
  printf "matches 'add': %b\n" (Call_tool_settings.matches_tool "add" settings);
  printf "matches 'multiply': %b\n"
    (Call_tool_settings.matches_tool "multiply" settings);
  [%expect {|
    matches 'add': true
    matches 'multiply': false
    |}]

let%expect_test "Call_tool_settings - excluded_tools filtering" =
  let settings = Call_tool_settings.create ~excluded_tools:[ "multiply" ] () in
  printf "matches 'add': %b\n" (Call_tool_settings.matches_tool "add" settings);
  printf "matches 'multiply': %b\n"
    (Call_tool_settings.matches_tool "multiply" settings);
  [%expect {|
    matches 'add': true
    matches 'multiply': false
    |}]

let%expect_test "Call_tool_settings - excluded takes precedence over included" =
  let settings =
    Call_tool_settings.create ~included_tools:[ "add"; "multiply" ]
      ~excluded_tools:[ "add" ] ()
  in
  printf "matches 'add': %b\n" (Call_tool_settings.matches_tool "add" settings);
  printf "matches 'multiply': %b\n"
    (Call_tool_settings.matches_tool "multiply" settings);
  [%expect {|
    matches 'add': false
    matches 'multiply': true
    |}]

(* =============================================================================
   Tests for Cache_entry
   ============================================================================= *)

let%expect_test "Cache_entry - not expired within TTL" =
  let entry = Cache_entry.create ~value:"test_value" ~ttl_seconds:3600 in
  printf "is_expired: %b\n" (Cache_entry.is_expired entry);
  printf "has_value: %b\n" (Option.is_some (Cache_entry.get_if_valid entry));
  [%expect {|
    is_expired: false
    has_value: true
    |}]

let%expect_test "Cache_entry - gets value if valid" =
  let entry = Cache_entry.create ~value:"test_value" ~ttl_seconds:3600 in
  (match Cache_entry.get_if_valid entry with
  | Some value -> printf "value: %s\n" value
  | None -> printf "expired\n");
  [%expect {| value: test_value |}]

(* =============================================================================
   Tests for Get_statistics
   ============================================================================= *)

let%expect_test "Get_statistics - initial state" =
  let stats = Get_statistics.create () in
  printf "count: %d, hit: %d, miss: %d\n" stats.count stats.hit stats.miss;
  [%expect {| count: 0, hit: 0, miss: 0 |}]

let%expect_test "Get_statistics - record hit" =
  let stats = Get_statistics.create () in
  Get_statistics.record_hit stats;
  printf "count: %d, hit: %d, miss: %d\n" stats.count stats.hit stats.miss;
  [%expect {| count: 1, hit: 1, miss: 0 |}]

let%expect_test "Get_statistics - record miss" =
  let stats = Get_statistics.create () in
  Get_statistics.record_miss stats;
  printf "count: %d, hit: %d, miss: %d\n" stats.count stats.hit stats.miss;
  [%expect {| count: 1, hit: 0, miss: 1 |}]

(* =============================================================================
   Tests for Memory_cache
   ============================================================================= *)

let%expect_test "Memory_cache - put and get" =
  let cache = Memory_cache.create () in
  Memory_cache.put cache ~key:"test_key" ~value:"test_value" ~ttl_seconds:3600;
  (match Memory_cache.get cache "test_key" with
  | Some value -> printf "value: %s\n" value
  | None -> printf "not found\n");
  [%expect {| value: test_value |}]

let%expect_test "Memory_cache - get missing key" =
  let cache = Memory_cache.create () in
  (match Memory_cache.get cache "missing_key" with
  | Some _ -> printf "found\n"
  | None -> printf "not found\n");
  [%expect {| not found |}]

let%expect_test "Memory_cache - statistics after get miss" =
  let cache = Memory_cache.create () in
  let _ = Memory_cache.get cache "missing_key" in
  let stats = Memory_cache.statistics cache in
  printf "get count: %d, get miss: %d\n" stats.get.count stats.get.miss;
  [%expect {| get count: 1, get miss: 1 |}]

let%expect_test "Memory_cache - statistics after put and get hit" =
  let cache = Memory_cache.create () in
  Memory_cache.put cache ~key:"test_key" ~value:"test_value" ~ttl_seconds:3600;
  let _ = Memory_cache.get cache "test_key" in
  let stats = Memory_cache.statistics cache in
  printf "put count: %d\n" stats.put.count;
  printf "get count: %d, get hit: %d, get miss: %d\n" stats.get.count
    stats.get.hit stats.get.miss;
  [%expect {|
    put count: 1
    get count: 1, get hit: 1, get miss: 0
    |}]

(* =============================================================================
   Tests for Response_caching_config
   ============================================================================= *)

let%expect_test "Response_caching_config - default TTLs" =
  let config = Response_caching_config.create () in
  printf "list_tools_ttl: %d\n" (Response_caching_config.list_tools_ttl config);
  printf "list_resources_ttl: %d\n"
    (Response_caching_config.list_resources_ttl config);
  printf "list_prompts_ttl: %d\n"
    (Response_caching_config.list_prompts_ttl config);
  printf "call_tool_ttl: %d\n" (Response_caching_config.call_tool_ttl config);
  printf "read_resource_ttl: %d\n"
    (Response_caching_config.read_resource_ttl config);
  printf "get_prompt_ttl: %d\n" (Response_caching_config.get_prompt_ttl config);
  [%expect
    {|
    list_tools_ttl: 300
    list_resources_ttl: 300
    list_prompts_ttl: 300
    call_tool_ttl: 3600
    read_resource_ttl: 3600
    get_prompt_ttl: 3600
    |}]

let%expect_test "Response_caching_config - all enabled by default" =
  let config = Response_caching_config.create () in
  printf "list_tools_enabled: %b\n"
    (Response_caching_config.list_tools_enabled config);
  printf "list_resources_enabled: %b\n"
    (Response_caching_config.list_resources_enabled config);
  printf "call_tool_enabled: %b\n"
    (Response_caching_config.call_tool_enabled config);
  [%expect
    {|
    list_tools_enabled: true
    list_resources_enabled: true
    call_tool_enabled: true
    |}]

let%expect_test "Response_caching_config - tool filtering" =
  let config =
    Response_caching_config.create
      ~call_tool_settings:
        (Call_tool_settings.create ~excluded_tools:[ "bad_tool" ] ())
      ()
  in
  printf "matches 'add': %b\n"
    (Response_caching_config.matches_tool_cache_settings config "add");
  printf "matches 'bad_tool': %b\n"
    (Response_caching_config.matches_tool_cache_settings config "bad_tool");
  [%expect {|
    matches 'add': true
    matches 'bad_tool': false
    |}]

(* =============================================================================
   Tests for Cachable types
   ============================================================================= *)

let%expect_test "Cachable_read_resource_contents - wrap and unwrap" =
  let cached =
    Cachable_read_resource_contents.wrap ~content:"test content"
      ~mime_type:"text/plain" ()
  in
  let content, mime_type = Cachable_read_resource_contents.unwrap cached in
  printf "content: %s\n" content;
  printf "mime_type: %s\n" (Option.value mime_type ~default:"NONE");
  [%expect {|
    content: test content
    mime_type: text/plain
    |}]

let%expect_test "Cachable_read_resource_contents - get_size" =
  let cached = Cachable_read_resource_contents.wrap ~content:"short" () in
  let size = Cachable_read_resource_contents.get_size cached in
  printf "size > 0: %b\n" (size > 0);
  [%expect {| size > 0: true |}]

let%expect_test "Cachable_tool_result - wrap and unwrap" =
  let cached =
    Cachable_tool_result.wrap
      ~content_json:"[{\"type\":\"text\",\"text\":\"result\"}]"
      ~structured_content_json:"{\"key\":\"value\"}" ()
  in
  let content, structured, meta = Cachable_tool_result.unwrap cached in
  printf "content_json: %s\n" content;
  printf "structured_content_json: %s\n"
    (Option.value structured ~default:"NONE");
  printf "meta_json: %s\n" (Option.value meta ~default:"NONE");
  [%expect
    {|
    content_json: [{"type":"text","text":"result"}]
    structured_content_json: {"key":"value"}
    meta_json: NONE
    |}]

(* =============================================================================
   Tests for get_arguments_str
   ============================================================================= *)

let%expect_test "get_arguments_str - null" =
  printf "result: %s\n" (get_arguments_str None);
  [%expect {| result: null |}]

let%expect_test "get_arguments_str - simple object" =
  let args = Some (`Assoc [ ("a", `Int 5); ("b", `Int 3) ]) in
  printf "result: %s\n" (get_arguments_str args);
  [%expect {| result: {"a":5,"b":3} |}]

let%expect_test "get_arguments_str - nested object" =
  let args =
    Some (`Assoc [ ("param", `Assoc [ ("nested", `String "value") ]) ])
  in
  printf "result: %s\n" (get_arguments_str args);
  [%expect {| result: {"param":{"nested":"value"}} |}]
