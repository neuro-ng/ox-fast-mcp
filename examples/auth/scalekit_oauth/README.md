# Scalekit OAuth Example

This example demonstrates how to protect a FastMCP server using Scalekit OAuth 2.1 authentication.

## Prerequisites

1. A Scalekit account and project.
2. A configured application in Scalekit with:
   - Client ID and Client Secret
   - Redirect URI (e.g., `http://localhost:8080/callback` for CLI/local testing)

## Configuration

Set the following environment variables:

```bash
# Server Configuration (Required)
export SCALEKIT_ENVIRONMENT_URL="https://<your-env>.scalekit.com"
export SCALEKIT_RESOURCE_ID="<your-resource-id>"

# Client Configuration (Required for the client to authenticate)
export SCALEKIT_CLIENT_ID="<your-client-id>"
export SCALEKIT_CLIENT_SECRET="<your-client-secret>"
# OCaml client uses a local server for callback by default, ensure redirection allows it
# or configure relevant redirect variables if using a specific flow.
```

## Running the Example

### 1. Start the Server

```bash
dune exec examples/auth/scalekit_oauth/server.exe
```

The server will start on `http://localhost:8000`.

### 2. Run the Client

In a separate terminal:

```bash
dune exec examples/auth/scalekit_oauth/client.exe
```

The client will:
1. Initiate the OAuth flow (likely opening a browser for login).
2. Connect to the server with the obtained token.
3. Call the `echo` and `auth_status` tools.
