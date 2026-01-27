# GitHub OAuth Example (OCaml)

Demonstrates FastMCP server protection with GitHub OAuth.

## Setup

### 1. GitHub OAuth App

1. Create a GitHub OAuth App:
   - Go to GitHub Settings > Developer settings > OAuth Apps
   - Click "New OAuth App"
   - Application Name: `FastMCP Example`
   - Homepage URL: `http://localhost:8000`
   - Authorization callback URL: `http://localhost:8000/auth/callback`
   - Register application
   - Copy the Client ID and Client Secret

### 2. Environment Variables

Set the following environment variables:

```bash
export FASTMCP_SERVER_AUTH_GITHUB_CLIENT_ID="your-client-id"
export FASTMCP_SERVER_AUTH_GITHUB_CLIENT_SECRET="your-client-secret"
```

### 3. Build

Build the example:

```bash
dune build examples/auth/github_oauth
```

### 4. Run the Server

Start the OCaml server:

```bash
dune exec examples/auth/github_oauth/server.exe
```

### 5. Run the Client

In another terminal, run the OCaml client:

```bash
dune exec examples/auth/github_oauth/client.exe
```

The client will attempt to connect using the configured OAuth flow.
