# WorkOS OAuth Example

This example demonstrates how to protect a FastMCP server using WorkOS OAuth.

## Prerequisites

1. A WorkOS account and project.
2. A configured application in WorkOS Connect.

## Configuration

Set the following environment variables:

```bash
# Server Configuration (Required)
export WORKOS_CLIENT_ID="<your-client-id>"
export WORKOS_CLIENT_SECRET="<your-client-secret>"
export WORKOS_AUTHKIT_DOMAIN="https://<your-app>.authkit.app"

# Optional
# export WORKOS_REQUIRED_SCOPES="openid,email,profile"
```

## Running the Example

### 1. Start the Server

```bash
dune exec examples/auth/workos_oauth/server.exe
```

The server will start on `http://localhost:8000`.

### 2. Run the Client

In a separate terminal:

```bash
dune exec examples/auth/workos_oauth/client.exe
```
