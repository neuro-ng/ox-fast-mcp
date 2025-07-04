# Tool Manager TODO

## Python to OCaml Module Mappings

### Implemented
- `mcp.types` -> `/lib/mcp/types.ml`
  - [x] Removed sexp derivation in favor of yojson
  - [x] Fixed type definitions and derivers
- `fastmcp.exceptions` -> `/lib/exceptions.ml`
- `fastmcp.tools.tool` -> `/lib/tools/tool.ml`
- `fastmcp.settings` -> `/lib/settings.ml`
  - [x] Implemented `DuplicateBehavior` type
  - [x] Implemented `mask_error_details` setting
  - [x] Implemented `deprecation_warnings` setting
  - [x] Added global settings management

### Missing (Need Implementation)
- `fastmcp.utilities.logging` -> `/lib/utilities/logging.ml`
  - Need to implement `get_logger` function
  - Need to implement logging levels (warning, exception)
  - Need to implement structured logging with sexp support

- `fastmcp.server.server` -> `/lib/server/server.ml`
  - Interface defined in server.mli
  - Need to implement server communication methods
  - Need to implement tool management functions
  - Need to implement async server operations

## Missing Functions in tool_manager.ml

1. Tool Module:
   - [x] Implement proper JSON schema generation in `from_function` using `ppx_yojson_conv`
   - [ ] Add function name extraction in `from_function` using `Caml.Obj.extension_constructor`
   - [ ] Complete `enable` and `disable` functions with proper context handling
   - [ ] Add validation for tool parameters against JSON schema
   - [x] Fix type errors in function_signature handling
   - [x] Resolve ambiguity between template and utilities function_signature types

2. Server Integration:
   - [ ] Implement server communication in `load_tools`
   - [ ] Add proper server tool fetching in `get_tools_from_mounted_servers`
   - [ ] Complete server mounting functionality
   - [ ] Add server health checks and error recovery

## Type Conversions Needed

1. Python type hints to OCaml types:
   - [x] `Callable[..., Any]` -> `'a -> 'b Deferred.t`
   - [x] `ContentBlock` -> `Content_block.t`
   - [x] `ToolAnnotations` -> `(string * string) list`
   - [x] `DuplicateBehavior` -> OCaml variant type
   - [x] Fixed function_signature type definition and usage

2. Exception handling:
   - [x] Map Python exceptions to OCaml exceptions using `Not_found_s` and `Tool_error`
   - [x] Implement proper error masking with settings control

## Jane Street Library Integration

1. Replace custom implementations with Jane Street equivalents:
   - [x] Use `Base.Option` instead of custom option handling
   - [x] Use `Core.String` functions for string operations
   - [x] Use `Async` for asynchronous operations
   - [x] Use `Core.Map` for tool storage
   - [ ] Add `Core.Command` for CLI operations
   - [ ] Use `Core.Time` for timeouts and retries

2. PPX Updates:
   - [x] Replace `ppx_deriving_yojson` with `ppx_yojson_conv`
   - [x] Add `ppx_jane` for additional derivers
   - [x] Add `ppx_let` for better async syntax
   - [ ] Add `ppx_custom_printf` for better error messages
   - [ ] Add `ppx_fields_conv` for better record handling

## Testing

1. Add unit tests:
   - [x] Test tool registration and lookup
   - [x] Test server mounting and communication
   - [x] Test error handling and masking
   - [x] Test async operations and timeouts
   - [ ] Add tests for new function_signature handling
   - [ ] Add tests for template vs utilities type disambiguation

2. Add integration tests:
   - [x] Test tool execution across mounted servers
   - [x] Test error propagation and recovery
   - [ ] Test performance under load
   - [ ] Test template and utilities function_signature interop 

## Temporary Stubs to Replace

1. Server Module Stubs:
   - [ ] Replace Server.list_tools stub with real implementation
   - [ ] Replace Server.get_tools stub with real implementation
   - [ ] Replace Server.call_tool stub with real implementation
   - [ ] Add proper server error handling
   - [ ] Add server state management

2. Context Module Stubs:
   - [ ] Replace Context.create stub with real implementation
   - [ ] Add proper session management
   - [ ] Add context validation
   - [ ] Add context state persistence
   - [ ] Add proper change tracking

3. FastMCP Module Stubs:
   - [ ] Replace FastMCP.create stub with real implementation
   - [ ] Add proper tool serialization
   - [ ] Add tool registration hooks
   - [ ] Add tool lifecycle management 