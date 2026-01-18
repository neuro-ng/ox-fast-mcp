# OxFastMCP Component Architecture

This document describes the component architecture of OxFastMCP, an OCaml implementation of the Model Context Protocol (MCP).

## High-Level Architecture Overview

```mermaid
flowchart TB
    subgraph "Application Layer"
        CLI["CLI<br/>src/cli/cli.ml"]
    end

    subgraph "Server Layer"
        Server["Ox_fast_mcp Server<br/>src/server/server.ml"]
        HTTP["HTTP Application<br/>src/server/http.ml"]
        Context["Execution Context<br/>src/server/context.ml"]
        Proxy["Proxy Server<br/>src/server/proxy.ml"]
    end

    subgraph "Client Layer"
        Client["MCP Client<br/>src/client/client.ml"]
        Transports["Client Transports<br/>src/client/transports.ml"]
    end

    subgraph "MCP Core Layer"
        Types["MCP Types<br/>src/mcp/types.ml"]
        SharedSession["Shared Session<br/>src/mcp/shared/session.ml"]
        Message["Message Types<br/>src/mcp/shared/message.ml"]
    end

    CLI --> Server
    CLI --> Client
    Server --> Context
    Server --> HTTP
    Server --> Types
    Client --> Transports
    Client --> Types
    HTTP --> Types
    Proxy --> Server
    SharedSession --> Message
    SharedSession --> Types
```

## Detailed Component Dependencies

### Core Type System

```mermaid
flowchart LR
    subgraph "src/mcp"
        Types["types.ml<br/>MCP Protocol Types"]
        SharedSession["shared/session.ml<br/>Base Session"]
        SharedMessage["shared/message.ml<br/>Message Types"]
        SharedAuth["shared/auth.ml<br/>Auth Primitives"]
        SharedExceptions["shared/exceptions.ml<br/>Error Types"]
        SharedContext["shared/context.ml<br/>Context Types"]
        SharedMemory["shared/memory.ml<br/>Memory Management"]
        ToolValidation["shared/tool_name_validation.ml<br/>Name Validation"]
    end

    SharedSession --> Types
    SharedSession --> SharedMessage
    SharedAuth --> Types
    SharedExceptions --> Types
    SharedContext --> Types
    ToolValidation --> Types
```

### Server Architecture

```mermaid
flowchart TB
    subgraph "src/server"
        Server["server.ml<br/>Main Server"]
        HTTP["http.ml<br/>HTTP/SSE App"]
        Context["context.ml<br/>Execution Context"]
        Proxy["proxy.ml<br/>Proxy Server"]
        Elicitation["elicitation.ml<br/>User Elicitation"]
        InputValidation["input_validation.ml<br/>Request Validation"]
        SessionBridge["session_bridge.ml<br/>Session Async Bridge"]
        StdioTransport["stdio_transport.ml<br/>Stdio Transport"]
        OpenAPI["openapi.ml<br/>OpenAPI Generation"]
        Dependencies["dependencies.ml<br/>Dependency Injection"]
        LowLevel["low_level.ml<br/>Low-Level API"]
    end

    subgraph "src/server/auth"
        ServerAuth["auth.ml<br/>Server Auth"]
        OAuthProxy["oauth_proxy.ml<br/>OAuth Proxy"]
        OIDCProxy["oidc_proxy.ml<br/>OIDC Proxy"]
        JwtIssuer["jwt_issuer.ml<br/>JWT Token Issuer"]
        AuthMiddleware["middleware.ml<br/>Auth Middleware"]
        RedirectValidation["redirect_validation.ml<br/>URI Validation"]
    end

    subgraph "src/server/middleware"
        Middleware["middleware.ml<br/>Core Middleware"]
        Caching["caching.ml<br/>Caching Layer"]
        ErrorHandling["error_handling.ml<br/>Error Handler"]
        RateLimiting["rate_limiting.ml<br/>Rate Limiter"]
        Timing["timing.ml<br/>Request Timing"]
        ToolInjection["tool_injection.ml<br/>Tool Injection"]
        MiddlewareLogging["middleware_logging.ml<br/>Logging Middleware"]
    end

    Server --> Context
    Server --> HTTP
    Server --> SessionBridge
    Server --> StdioTransport
    Server --> Middleware
    HTTP --> ServerAuth
    HTTP --> OAuthProxy
    Context --> Elicitation
    Context --> InputValidation
    Context --> SessionBridge
    Proxy --> Server
    Proxy --> Context
    OAuthProxy --> JwtIssuer
    OAuthProxy --> RedirectValidation
    OIDCProxy --> RedirectValidation
    ServerAuth --> AuthMiddleware
    Middleware --> ErrorHandling
    Middleware --> Caching
    Middleware --> RateLimiting
    Middleware --> Timing
    Middleware --> ToolInjection
    Middleware --> MiddlewareLogging
```

### Client Architecture

```mermaid
flowchart TB
    subgraph "src/client"
        Client["client.ml<br/>MCP Client"]
        ClientTransports["transports.ml<br/>Transport Factory"]
        Logging["logging.ml<br/>Log Handler"]
        Sampling["sampling.ml<br/>Sampling Handler"]
        Progress["progress.ml<br/>Progress Handler"]
        Roots["roots.ml<br/>Roots Handler"]
        Messages["messages.ml<br/>Message Handler"]
    end

    subgraph "src/client/auth"
        Bearer["bearer.ml<br/>Bearer Auth"]
        ClientOAuth["oauth.ml<br/>OAuth Flow"]
    end

    subgraph "src/mcp/client"
        Session["session.ml<br/>Client Session"]
        SessionGroup["session_group.ml<br/>Session Group"]
    end

    subgraph "src/mcp/client/transports"
        SSE["sse.ml<br/>SSE Transport"]
        StreamableHTTP["streamable_http.ml<br/>StreamableHTTP"]
        Stdio["stdio.ml<br/>Stdio Transport"]
        WebSocket["websocket.ml<br/>WebSocket Transport"]
        SSEProtocol["sse_protocol.ml<br/>SSE Parser"]
    end

    subgraph "src/mcp/client/auth"
        OAuth2["oauth2.ml<br/>OAuth2 Client"]
        OAuth2Middleware["oauth2_middleware.ml<br/>OAuth2 MW"]
        CryptoUtils["crypto_utils.ml<br/>Crypto Utilities"]
        AuthUtils["utils.ml<br/>Auth Utilities"]
    end

    Client --> ClientTransports
    Client --> Session
    Client --> Logging
    Client --> Sampling
    Client --> Progress
    Client --> Roots
    Client --> Messages
    ClientTransports --> SSE
    ClientTransports --> StreamableHTTP
    ClientTransports --> Stdio
    ClientTransports --> WebSocket
    SSE --> SSEProtocol
    SessionGroup --> Session
    SessionGroup --> Stdio
    ClientOAuth --> OAuth2
    OAuth2 --> OAuth2Middleware
    OAuth2 --> CryptoUtils
    OAuth2 --> AuthUtils
    Bearer --> AuthUtils
```

### Component Managers

```mermaid
flowchart TB
    subgraph "src/tools"
        Tool["tool.ml<br/>Tool Definition"]
        ToolManager["tool_manager.ml<br/>Tool Manager"]
        ToolTypes["tool_types.ml<br/>Tool Types"]
        ToolTransform["tool_transform.ml<br/>Tool Transform"]
        ToolErrors["errors.ml<br/>Tool Errors"]
    end

    subgraph "src/prompts"
        Prompt["prompt.ml<br/>Prompt Definition"]
        PromptManager["prompt_manager.ml<br/>Prompt Manager"]
        PromptTypes["prompt_types.ml<br/>Prompt Types"]
    end

    subgraph "src/resources"
        Resource["resource.ml<br/>Resource Definition"]
        ResourceManager["resource_manager.ml<br/>Resource Manager"]
        ResourceTypes["resource_types.ml<br/>Resource Types"]
        Template["template.ml<br/>URI Templates"]
    end

    Tool --> ToolTypes
    ToolManager --> Tool
    ToolManager --> ToolTypes
    ToolTransform --> ToolTypes
    Prompt --> PromptTypes
    PromptManager --> Prompt
    PromptManager --> PromptTypes
    Resource --> ResourceTypes
    ResourceManager --> Resource
    ResourceManager --> ResourceTypes
    Template --> ResourceTypes
```

### Utilities Layer

```mermaid
flowchart TB
    subgraph "src/utilities"
        FmcpTypes["fmcp_types.ml<br/>FastMCP Types"]
        JsonSchema["json_schema.ml<br/>JSON Schema"]
        Log["log.ml<br/>Logging"]
        Cache["cache.ml<br/>Caching"]
        Components["components.ml<br/>Component Registry"]
        Manager["manager.ml<br/>Manager Base"]
        McpConfig["mcp_config.ml<br/>MCP Config"]
        SecretString["secret_string.ml<br/>Secret Storage"]
        NetworkUtils["network_utils.ml<br/>Network Utils"]
    end

    subgraph "src (root)"
        Settings["settings.ml<br/>Global Settings"]
        Exceptions["exceptions.ml<br/>Global Exceptions"]
        Version["version.ml<br/>Version Info"]
    end

    FmcpTypes --> JsonSchema
    Components --> Manager
    McpConfig --> SecretString
    Settings --> Exceptions
```

### MCP Server Session

```mermaid
flowchart TB
    subgraph "src/mcp/server"
        McpServerSession["session.ml<br/>Server Session"]
        McpServerModels["models.ml<br/>Server Models"]
        McpServerStdio["stdio.ml<br/>Stdio Transport"]
    end

    subgraph "src/mcp/server/auth"
        McpAuthProvider["provider.ml<br/>Auth Provider"]
    end

    subgraph "src/mcp/server/lowlevel"
        LowLevelServer["server.ml<br/>Low-Level Server"]
        HelperTypes["helper_types.ml<br/>Helper Types"]
    end

    McpServerSession --> McpServerModels
    McpServerSession --> McpServerStdio
    McpServerSession --> McpAuthProvider
    LowLevelServer --> HelperTypes
    LowLevelServer --> McpServerSession
```

## Complete System Call Flow

```mermaid
sequenceDiagram
    participant CLI
    participant Server as Ox_fast_mcp Server
    participant Context as Execution Context
    participant Middleware as Middleware Chain
    participant ToolManager as Tool Manager
    participant Session as MCP Session
    participant Transport as Transport Layer
    participant Types as MCP Types

    CLI->>Server: create()
    Server->>Context: create()
    Server->>ToolManager: create()
    Server->>Session: create()
    
    Note over CLI,Types: Request Processing Flow
    
    CLI->>Server: run_async()
    Server->>Transport: listen()
    Transport->>Session: receive_request()
    Session->>Types: parse_message()
    Session->>Middleware: call()
    Middleware->>Context: create_context()
    Middleware->>ToolManager: call_tool()
    ToolManager->>Types: serialize_response()
    ToolManager-->>Session: response
    Session-->>Transport: send_response()
```

```mermaid
sequenceDiagram
    participant Client
    participant Transports
    participant Session as Client Session
    participant Transport as SSE/HTTP/Stdio
    participant Types as MCP Types

    Client->>Transports: connect_session()
    Transports->>Transport: create()
    Transport->>Session: create_from_pipes()
    Session->>Types: create_initialize_request()
    Session->>Transport: send()
    Transport-->>Session: initialize_result
    Session-->>Client: connected

    Note over Client,Types: Tool Call Flow

    Client->>Session: call_tool()
    Session->>Types: create_request()
    Session->>Transport: send()
    Transport-->>Session: response
    Session->>Types: parse_response()
    Session-->>Client: call_tool_result
```

## Module Dependency Graph (Simplified)

```mermaid
graph TD
    subgraph "Entry Points"
        A[CLI]
        B[Server]
        C[Client]
    end

    subgraph "Core"
        D[MCP Types]
        E[Shared Session]
        F[Message]
    end

    subgraph "Server Components"
        G[HTTP Layer]
        H[Context]
        I[Middleware]
        J[Auth]
    end

    subgraph "Client Components"
        K[Transports]
        L[Session]
        M[Auth]
    end

    subgraph "Managers"
        N[Tools]
        O[Resources]
        P[Prompts]
    end

    subgraph "Utilities"
        Q[Types]
        R[Schema]
        S[Logging]
    end

    A --> B
    A --> C
    B --> D
    B --> G
    B --> H
    B --> I
    B --> J
    B --> N
    B --> O
    B --> P
    C --> D
    C --> K
    C --> L
    C --> M
    G --> D
    H --> D
    I --> D
    K --> L
    K --> E
    L --> E
    L --> F
    E --> D
    E --> F
    N --> Q
    O --> Q
    P --> Q
    Q --> R
```

## Directory Structure Summary

| Directory | Purpose | Key Modules |
|-----------|---------|-------------|
| `src/` | Root-level modules | `settings.ml`, `exceptions.ml`, `version.ml` |
| `src/cli/` | Command-line interface | `cli.ml` |
| `src/client/` | High-level MCP client | `client.ml`, `transports.ml` |
| `src/client/auth/` | Client authentication | `bearer.ml`, `oauth.ml` |
| `src/mcp/` | Core MCP protocol | `types.ml` |
| `src/mcp/client/` | Low-level client session | `session.ml`, `session_group.ml` |
| `src/mcp/client/transports/` | Transport implementations | `stdio.ml`, `sse.ml`, `streamable_http.ml`, `websocket.ml` |
| `src/mcp/client/auth/` | Client OAuth2 | `oauth2.ml`, `oauth2_middleware.ml` |
| `src/mcp/server/` | Low-level server session | `session.ml`, `stdio.ml` |
| `src/mcp/server/auth/` | Server auth provider | `provider.ml` |
| `src/mcp/server/lowlevel/` | Low-level server API | `server.ml` |
| `src/mcp/shared/` | Shared session primitives | `session.ml`, `message.ml`, `auth.ml`, `exceptions.ml` |
| `src/server/` | High-level FastMCP server | `server.ml`, `http.ml`, `context.ml`, `proxy.ml` |
| `src/server/auth/` | Server authentication | `oauth_proxy.ml`, `oidc_proxy.ml`, `jwt_issuer.ml` |
| `src/server/middleware/` | Server middleware | `middleware.ml`, `caching.ml`, `rate_limiting.ml` |
| `src/server/sampling/` | Server sampling | Sampling handlers |
| `src/tools/` | Tool management | `tool.ml`, `tool_manager.ml` |
| `src/prompts/` | Prompt management | `prompt.ml`, `prompt_manager.ml` |
| `src/resources/` | Resource management | `resource.ml`, `resource_manager.ml`, `template.ml` |
| `src/utilities/` | Shared utilities | `fmcp_types.ml`, `json_schema.ml`, `log.ml`, `cache.ml` |

## Key Design Patterns

1. **Layered Architecture**: Clear separation between high-level APIs (server/client) and low-level protocol implementation (mcp/)

2. **Manager Pattern**: Tools, Resources, and Prompts each have dedicated manager modules for registration, lookup, and execution

3. **Middleware Pipeline**: Server supports composable middleware for cross-cutting concerns (caching, rate limiting, logging, auth)

4. **Transport Abstraction**: Multiple transport protocols (Stdio, SSE, StreamableHTTP, WebSocket) behind a unified interface

5. **Session-based Communication**: Both client and server use session abstractions built on pipe-based message passing

6. **Type-Safe Protocol**: All MCP protocol messages use strongly-typed OCaml records with JSON serialization via `ppx_yojson_conv`

7. **Auth Header Wiring**: Bearer authentication automatically converted to HTTP headers via `auth_config_to_headers`

8. **Graceful Lifecycle Management**: Session groups implement proper shutdown (close stdin → SIGTERM → SIGKILL)

