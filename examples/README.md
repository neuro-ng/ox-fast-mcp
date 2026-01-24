# OxFastMCP Examples

This directory contains working examples demonstrating OxFastMCP server features. These examples show how to create MCP (Model Context Protocol) servers in OCaml using the OxFastMCP library.

## Quick Start

### Prerequisites

- OCaml 5.2.0+ with OxCaml variant
- Dune build system
- OxFastMCP library installed

### Building Examples

Build all examples:
```bash
dune build examples
```

Build a specific example:
```bash
dune build examples/calculator_server.exe
dune build examples/demo.exe
dune build examples/http_demo.exe
```

### Running Examples

Run with dune:
```bash
dune exec calculator-server
dune exec demo
dune exec http-demo
```

Run built executables directly:
```bash
./_build/default/examples/calculator_server.exe
./_build/default/examples/demo.exe
```

## Available Examples

### 1. Calculator Server (`calculator_server.ml`)

A simple MCP server providing basic arithmetic operations.

**Features:**
- Four arithmetic tools: `add`, `subtract`, `multiply`, `divide`
- STDIO transport for standard MCP client communication
- Error handling (e.g., division by zero)
- Number type coercion (handles both Float and Int inputs)

**Tools:**
- `add` - Add two numbers
- `subtract` - Subtract b from a
- `multiply` - Multiply two numbers
- `divide` - Divide a by b (with zero-division error handling)

**Example Usage:**
```bash
# Run the server
dune exec calculator-server

# Server will communicate via STDIO using MCP protocol
# Connect with any MCP-compatible client
```

**Code Highlights:**
```ocaml
(* Create server *)
let server = Server.Ox_fast_mcp.create 
  ~name:"calculator-server"
  ~version:"1.0.0"
  ()
in

(* Add a tool *)
Server.Ox_fast_mcp.add_simple_tool
  ~name:"add"
  ~description:"Add two numbers"
  ~handler:(fun params -> (* ... *))
  server;

(* Run with STDIO transport *)
Server.Ox_fast_mcp.run_async server ~transport:Stdio ()
```

---

### 2. Demo Server (`demo.ml`)

A comprehensive demonstration of OxFastMCP features including tools and resources.

**Features:**
- Multiple tools with different functionalities
- Resource serving (static information)
- Dynamic content generation
- Server metadata and introspection

**Tools:**
- `greet` - Personalized greeting
- `echo` - Echo back input
- `get_time` - Get current server time

**Resources:**
- `demo://info` - Server information (text/plain)

**Example Usage:**
```bash
dune exec demo
```

---

### 3. HTTP Demo Server (`http_demo.ml`)

Demonstrates HTTP/SSE transport for web-based MCP communication.

**Features:**
- HTTP server with SSE (Server-Sent Events) transport
- Web-accessible MCP endpoint
- Custom greeting tool

**Example Usage:**
```bash
dune exec http-demo

# Server starts on http://127.0.0.1:8000
# SSE endpoint: http://127.0.0.1:8000/sse
# Message endpoint: http://127.0.0.1:8000/messages
```

---

## Common Patterns

### Creating a Server

```ocaml
open! Core
open! Async

let server = Server.Ox_fast_mcp.create 
  ~name:"my-server"
  ~version:"1.0.0"
  ~instructions:"Description of what this server does"
  ()
```

### Adding Tools

**Simple tool with handler:**
```ocaml
Server.Ox_fast_mcp.add_simple_tool
  ~name:"tool_name"
  ~description:"What this tool does"
  ~handler:(fun params ->
    (* Process params and return result *)
    return (`Assoc [("result", `String "output")]))
  server
```

**Extracting parameters:**
```ocaml
match params with
| `Assoc fields ->
  let value = List.Assoc.find fields ~equal:String.equal "param_name" 
    |> Option.value ~default:(`String "default") 
  in
  (* Use value *)
| _ -> return (`Assoc [("error", `String "Expected object")])
```

### Adding Resources

**Static resource:**
```ocaml
Server.Ox_fast_mcp.add_simple_resource
  ~uri:"myscheme://resource-id"
  ~name:"Resource Name"
  ~description:"What this resource provides"
  ~mime_type:"text/plain"
  ~reader:(fun () -> return "Resource content")
  server
```

**Dynamic resource:**
```ocaml
Server.Ox_fast_mcp.add_simple_resource
  ~uri:"myscheme://dynamic"
  ~name:"Dynamic Resource"
  ~mime_type:"application/json"
  ~reader:(fun () ->
    let data = generate_current_data () in
    return (Yojson.Safe.to_string data))
  server
```

### Running the Server

**STDIO Transport (for CLI clients):**
```ocaml
Server.Ox_fast_mcp.run_async server ~transport:Stdio ()
```

**HTTP/SSE Transport (for web clients):**
```ocaml
Server.Ox_fast_mcp.run_async server 
  ~transport:Sse
  ~host:"127.0.0.1" 
  ~port:8000 
  ()
```

### Error Handling

```ocaml
~handler:(fun params ->
  try
    (* Process request *)
    let result = process params in
    return (`Assoc [("result", result)])
  with
  | Invalid_argument msg ->
    return (`Assoc [("error", `String msg)])
  | _ ->
    return (`Assoc [("error", `String "Unknown error")]))
```

---

## MCP Protocol Overview

The Model Context Protocol (MCP) is a standard for communication between AI models and context providers. OxFastMCP implements this protocol, allowing you to create servers that:

- **Provide Tools**: Functions that AI models can call
- **Serve Resources**: Static or dynamic content (files, data, etc.)
- **Offer Prompts**: Template prompts for AI interactions

### Key Concepts

**Tools** are functions exposed to MCP clients:
- Defined with name, description, and handler
- Receive JSON parameters
- Return JSON results

**Resources** are content providers:
- Identified by URIs
- Can be static or dynamically generated
- Support multiple MIME types

**Transports** define how servers communicate:
- **STDIO**: Standard input/output (for CLI tools)
- **HTTP/SSE**: Server-Sent Events over HTTP (for web clients)
- **Streamable HTTP**: Alternative HTTP-based transport

---

## API Reference

For complete API documentation, see:
- [Server Module Documentation](../src/server/server.mli)
- [Main README](../README.md)
- [MCP Specification](https://modelcontextprotocol.io)

---

## Building Your Own Server

1. **Create a new .ml file** in the examples directory
2. **Define your server** with name and tools
3. **Add to dune file**:
   ```dune
   (executable
    (public_name my-server)
    (name my_server)
    (modules my_server)
    (libraries ox_fast_mcp server core async))
   ```
4. **Build and run**:
   ```bash
   dune build examples/my_server.exe
   dune exec my-server
   ```

---

## Debugging

Enable verbose logging:
```bash
OCAMLRUNPARAM=b dune exec calculator-server
```

View build output:
```bash
dune build examples --verbose
```

---

## Next Steps

- Read the [MCP Specification](https://modelcontextprotocol.io)
- Explore the [OxFastMCP source code](../src/)
- Check out [test files](../test/) for more examples
- Join the OxFastMCP community for support

---

## License

See [LICENSE](../LICENSE) in the project root.
