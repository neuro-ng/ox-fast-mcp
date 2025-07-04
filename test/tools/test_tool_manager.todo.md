# Test Tool Manager TODO

## Missing Module Stubs Needed

1. Context Module:
   - [x] Create stub for Context type
   - [x] Add request_id, client_id fields
   - [x] Add session_data hash table
   - [x] Add tools_changed, resources_changed, prompts_changed flags

2. FastMCP Module:
   - [x] Create stub for FastMCP type
   - [x] Add tool_serializer support
   - [x] Add basic tool registration

3. Server Module:
   - [x] Create stub for Server type
   - [x] Add list_tools, get_tools functions
   - [x] Add call_tool function

## Test Categories to Implement

1. Basic Tool Management:
   - [x] Create tool manager
   - [x] Add tool
   - [x] Get tool
   - [x] Remove tool
   - [x] List tools
   - [x] Tool execution

2. Tool Function Types:
   - [x] Basic function
   - [x] Async function
   - [x] Callable object
   - [x] Async callable object
   - [x] Lambda functions

3. Tool Parameters:
   - [x] Required parameters
   - [x] Optional parameters
   - [x] Default values
   - [x] Complex types (lists, records)
   - [x] Context parameter handling

4. Error Handling:
   - [x] Tool errors
   - [x] Not found errors
   - [x] Validation errors
   - [x] Error masking

5. Duplicate Tool Behavior:
   - [x] Warn behavior
   - [x] Replace behavior
   - [x] Error behavior
   - [x] Ignore behavior

6. Tool Tags:
   - [x] Add tags to tools
   - [x] List tools by tag
   - [x] Empty tags
   - [x] Multiple tags

7. Tool Naming:
   - [x] Custom tool names
   - [x] Name conflicts
   - [x] Name validation

8. Function Signature Types:
   - [x] Test utilities.function_signature type
   - [x] Test template.function_signature type
   - [ ] Test function signature disambiguation
   - [ ] Test function signature conversion

## Test Helper Functions Needed

1. Tool Creation:
   - [x] create_test_tool
   - [x] create_async_test_tool
   - [x] create_callable_test_tool

2. Context Management:
   - [x] create_test_context
   - [x] with_test_context

3. Validation Helpers:
   - [x] assert_tool_equal
   - [x] assert_tool_list_equal
   - [x] assert_error_contains

## Jane Street Integration

1. Testing Framework:
   - [x] Use expect_test_helpers_core
   - [x] Use expect_test_helpers_async
   - [x] Use ppx_expect operators

2. Async Support:
   - [x] Convert Lwt to Async
   - [x] Use Async Deferred properly
   - [x] Handle async context properly

3. Error Handling:
   - [x] Use Error.t for results
   - [x] Use Monitor.try_with for exceptions
   - [x] Use structured errors

4. Data Structures:
   - [x] Use Core.Map consistently
   - [x] Use Core.Set for tags
   - [x] Use Core.String for comparisons

## Temporary Test Stubs to Replace/Enhance

1. Server Test Stub:
   ```ocaml
   module Server = struct
     type t = {
       mutable tools : Tool.t list;
       prefix : string option;
     }
     ...
   end
   ```
   - [ ] Add proper tool state management
   - [ ] Add server configuration options
   - [ ] Add proper error simulation
   - [ ] Add network latency simulation
   - [ ] Add server restart/recovery simulation

2. Context Test Stub:
   ```ocaml
   module Context = struct
     type t = {
       request_id : string option;
       client_id : string option;
       session_data : (string, string) Hashtbl.t;
       tools_changed : bool;
       resources_changed : bool;
       prompts_changed : bool;
     }
     ...
   end
   ```
   - [ ] Add session expiration simulation
   - [ ] Add concurrent access simulation
   - [ ] Add state corruption simulation
   - [ ] Add proper change tracking tests
   - [ ] Add session persistence tests

3. FastMCP Test Stub:
   ```ocaml
   module FastMCP = struct
     type t = {
       tool_serializer : Yojson.Safe.t -> string;
       tool_manager : Tool_manager.t;
     }
     ...
   end
   ```
   - [ ] Add tool registration hooks tests
   - [ ] Add serialization error tests
   - [ ] Add tool lifecycle tests
   - [ ] Add manager state tests
   - [ ] Add configuration validation tests

4. Test Helper Functions to Enhance:
   ```ocaml
   let create_test_tool name description = ...
   let create_async_test_tool name description = ...
   ```
   - [ ] Add more complex parameter types
   - [ ] Add error injection capabilities
   - [ ] Add performance measurement hooks
   - [ ] Add state tracking for test validation
   - [ ] Add cleanup verification

## Next Steps

1. Function Signature Testing:
   - [ ] Add tests for function signature type disambiguation
   - [ ] Add tests for function signature conversion
   - [ ] Add tests for function signature validation

2. Add Complex Type Support:
   - [x] Add list parameter support
   - [x] Add record type support
   - [x] Add variant type support

3. Add Server Integration:
   - [x] Implement Server stub
   - [x] Add server communication tests
   - [x] Test server error handling

4. Add Validation:
   - [x] Add parameter validation
   - [x] Add schema validation
   - [x] Add type checking

# Tool Manager Test Implementation Status

## Completed
- [x] Basic tool management tests (create, add, get, remove, list)
- [x] Tool function types tests (basic, async, callable object)
- [x] Error handling tests (not found, invalid args, missing args)
- [x] Duplicate tool behavior tests (warn, error, ignore)
- [x] Server integration tests (mount, list, call)
- [x] Tool tags tests (add with tags, list with tag filter)
- [x] Complex type support (list parameters)
- [x] Context handling tests
  - [x] Context parameter detection
  - [x] Context injection
  - [x] Optional context
  - [x] Context error handling
- [x] Custom tool names tests
  - [x] Add tool with custom name
  - [x] Call tool with custom name
  - [x] Replace tool with custom name
- [x] Tool error handling tests
  - [x] Tool error passthrough
  - [x] Exception conversion
  - [x] Masked error details

## In Progress
- [ ] Function signature type tests
  - [ ] Test utilities vs template type disambiguation
  - [ ] Test type conversion between variants
  - [ ] Test validation and error handling
  - [ ] Test schema generation for both types

## Missing Functionality
- [x] Tool schema generation from function types
- [x] Tool validation
- [x] Tool serialization customization
- [x] Tool annotations support
- [ ] Tool enable/disable functionality
- [x] Tool parameter validation
- [x] Tool result validation

## Stubs Needed
- [x] Server module
- [x] Context module
- [x] FastMCP module
- [x] Tool_types module
- [x] Tool_manager module
- [x] Exceptions module
- [x] Settings module
- [x] Utilities module

## Notes
- Using ppx_yojson_conv instead of ppx_deriving_yojson
- Using Jane Street libraries (Core, Async) instead of Lwt
- Using expect_test_helpers_core and ppx_expect for testing
- Need to add tests for function signature type disambiguation
- Need to add tests for template vs utilities type conversion 