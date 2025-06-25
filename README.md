# Ox Fast MCP 🚀

**The fast, functional way to build MCP servers and clients in OCaml**

An OCaml implementation of the [FastMCP Python library](https://github.com/jlowin/fastmcp), providing a high-level, type-safe interface for building Model Context Protocol (MCP) servers and clients using functional programming principles.

## Features ✨

- **🚀 Fast**: High-level interface with minimal boilerplate
- **🛡️ Type-Safe**: Leverages OCaml's strong type system
- **🔧 Functional**: Built with functional programming principles
- **🧪 TDD**: Developed using Test-Driven Development
- **📡 Complete**: Full MCP protocol implementation for servers and clients
- **🔗 Interoperable**: Compatible with other MCP implementations

## Installation 📦

```bash
# Using opam
opam install ox-fast-mcp

# Using dune (for development)
git clone https://github.com/neuron-ng/ox-fast-mcp.git
cd ox-fast-mcp
dune build
dune test
```

## Quick Start 🏃‍♂️

### Creating a Simple MCP Server

```ocaml
open Lwt.Syntax
open Ox_fast_mcp

let () =
  let server = Fast_mcp_server.create ~name:"Demo Server 🚀" () in
  
  (* Add a tool *)
  Fast_mcp_server.register_tool server
    ~name:"add"
    ~description:"Add two numbers"
    ~func:(fun json ->
      match json with
      | `Assoc [("a", `Int a); ("b", `Int b)] -> `Int (a + b)
      | _ -> `String "Invalid parameters");
  
  (* Add a resource *)
  Fast_mcp_server.register_resource server
    ~uri:"config://version"
    ~name:"Version Info"
    ~description:"Current version information"
    ~func:(fun () -> `String "1.0.0");
  
  (* Add a prompt *)
  Fast_mcp_server.register_prompt server
    ~name:"greet"
    ~description:"Generate a greeting"
    ~func:(fun json ->
      match json with
      | `Assoc [("name", `String name)] ->
        Printf.sprintf "Hello, %s! Welcome to our service." name
      | _ -> "Hello! Welcome to our service.");
  
  Printf.printf "Server created with %d tools, %d resources, %d prompts\n"
    (Fast_mcp_server.tool_count server)
    (Fast_mcp_server.resource_count server)
    (Fast_mcp_server.prompt_count server)
```

### Using the MCP Client

```ocaml
open Lwt.Syntax
open Ox_fast_mcp

let test_client server =
  Lwt_main.run (
    let* client = Mcp_client.create_in_memory server in
    
    (* List available tools *)
    let* tools = Mcp_client.list_tools client in
    Printf.printf "Available tools: %s\n" 
      (String.concat ", " (List.map (fun t -> t.name) tools));
    
    (* Call a tool *)
    let* result = Mcp_client.call_tool client "add" 
      (`Assoc [("a", `Int 5); ("b", `Int 3)]) in
    (match result with
     | Ok (Text s) -> Printf.printf "5 + 3 = %s\n" s
     | Error err -> Printf.printf "Error: %s\n" err
     | _ -> Printf.printf "Unexpected result format\n");
    
    (* Read a resource *)
    let* result = Mcp_client.read_resource client "config://version" in
    (match result with
     | Ok (Text s) -> Printf.printf "Version: %s\n" s
     | Error err -> Printf.printf "Error: %s\n" err
     | _ -> Printf.printf "Unexpected result format\n");
    
    let* () = Mcp_client.close client in
    Lwt.return_unit
  )
```

## Core Concepts 📚

### The FastMCP Server

The central object representing your MCP application:

```ocaml
let server = Fast_mcp_server.create 
  ~name:"My Server"
  ~instructions:"This server provides mathematical operations" 
  ()
```

### Tools

Tools allow LLMs to perform actions by executing your OCaml functions:

```ocaml
Fast_mcp_server.register_tool server
  ~name:"multiply"
  ~description:"Multiply two numbers"
  ~func:(fun json ->
    match json with
    | `Assoc [("a", `Int a); ("b", `Int b)] -> `Int (a * b)
    | _ -> `String "Error: Invalid parameters")
```

### Resources & Templates

Resources expose read-only data sources:

```ocaml
(* Static resource *)
Fast_mcp_server.register_resource server
  ~uri:"config://settings"
  ~name:"Settings"
  ~description:"Application settings"
  ~func:(fun () -> `Assoc [("debug", `Bool true)])

(* Dynamic resource template *)
Fast_mcp_server.register_resource_template server
  ~uri_pattern:"users://{user_id}/profile"
  ~name:"User Profile"
  ~description:"Get user profile by ID"
  ~func:(fun user_id ->
    `Assoc [("id", `String user_id); ("status", `String "active")])
```

### Prompts

Prompts define reusable message templates:

```ocaml
Fast_mcp_server.register_prompt server
  ~name:"summarize"
  ~description:"Generate a summary prompt"
  ~func:(fun json ->
    match json with
    | `Assoc [("text", `String text)] ->
      Printf.sprintf "Please summarize: %s" text
    | _ -> "Please provide text to summarize")
```

### Context Support

Tools can access execution context for advanced functionality:

```ocaml
Fast_mcp_server.register_tool_with_context server
  ~name:"context_tool"
  ~description:"Tool that uses execution context"
  ~func:(fun ctx json ->
    let request_id = match ctx.request_id with
      | Some id -> id
      | None -> "unknown"
    in
    `String (Printf.sprintf "Processing request %s" request_id))
```

## Protocol Support 📡

Ox Fast MCP implements the full Model Context Protocol specification:

- **JSON-RPC 2.0**: Complete request/response handling
- **Tools**: Function calling with parameter validation
- **Resources**: Static and templated data sources  
- **Prompts**: Reusable message templates
- **Error Handling**: Comprehensive error reporting
- **Transport Abstraction**: In-memory, STDIO, HTTP, SSE support

## Examples 💡

See the `examples/` directory for complete working examples:

- `calculator_server.ml`: Basic arithmetic server with tools, resources, and prompts

To run an example:

```bash
dune exec examples/calculator_server.exe
```

## Testing 🧪

Ox Fast MCP was built using Test-Driven Development (TDD). Run the comprehensive test suite:

```bash
# Run all tests
dune test

# Run specific test modules
dune exec test/test_mcp_types.exe
dune exec test/test_fast_mcp_server.exe
dune exec test/test_mcp_client.exe

# Run with verbose output
dune test --verbose
```

The test suite includes:

- **Type Tests**: JSON serialization, protocol types
- **Server Tests**: Tool/resource/prompt registration and execution
- **Client Tests**: Communication, error handling, concurrent operations
- **Integration Tests**: End-to-end server-client interaction

## Folder Structure 🏗️

```
Ox Fast MCP Library Structure:

├── lib/
│   ├── mcp_types.ml          # Core MCP protocol types
│   ├── fast_mcp_server.ml    # Server implementation
│   ├── mcp_client.ml         # Client implementation
│   └── ox_fast_mcp.ml        # Main library interface
├── test/
│   ├── test_mcp_types.ml     # Type system tests
│   ├── test_fast_mcp_server.ml # Server functionality tests
│   ├── test_mcp_client.ml    # Client functionality tests
│   └── run_tests.ml          # Test runner
└── examples/
    └── calculator_server.ml  # Example MCP server
```

## Comparison with FastMCP Python 🐍

| Feature | Ox Fast MCP (OCaml) | FastMCP (Python) |
|---------|---------------------|------------------|
| Type Safety | ✅ Compile-time | ❌ Runtime |
| Performance | ✅ Native code | ❌ Interpreted |
| Memory Safety | ✅ Built-in | ❌ Manual |
| Concurrency | ✅ Lwt async | ✅ asyncio |
| Error Handling | ✅ Result types | ❌ Exceptions |
| Pattern Matching | ✅ Native | ❌ Manual |

## Functional Programming Benefits 🧮

- **Immutability**: Safer concurrent programming
- **Pattern Matching**: Elegant JSON handling
- **Result Types**: Explicit error handling
- **Type Inference**: Less boilerplate code
- **Composability**: Easy to combine and test

## Contributing 🤝

We welcome contributions! This project follows TDD principles:

1. **Write tests first** for new functionality
2. **Implement** the minimal code to make tests pass
3. **Refactor** while keeping tests green
4. **Document** new features and APIs

```bash
# Development setup
git clone https://github.com/neuro-ng/ox-fast-mcp.git
cd ox-fast-mcp
dune build
dune test

# Before submitting PR
dune test
dune build @doc
```

### Development Workflow

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Implement the feature
5. Ensure all tests pass
6. Update documentation
7. Submit a pull request

## License 📄

Apache-2.0 License - see [LICENSE](LICENSE) file for details.

## Related Projects 🔗

- [FastMCP Python](https://github.com/jlowin/fastmcp) - Original Python implementation
- [Model Context Protocol](https://modelcontextprotocol.io/) - Official MCP specification
- [OCaml](https://ocaml.org/) - The OCaml programming language
- [OxCaml](https://oxcaml.org/) - JS OCaml extensions

## Version History 📈

- **v0.1.0**: Initial TDD implementation with core MCP functionality
  - Server creation and tool/resource/prompt registration  
  - In-memory client with full protocol support
  - Comprehensive test suite
  - Calculator example server

---

Built with ❤️ using OCaml and TDD principles 