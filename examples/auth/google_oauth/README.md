# Google OAuth Example (OCaml)

Demonstrates FastMCP server protection with Google OAuth.

## Setup

### 1. Google OAuth 2.0 Client

1. Create a Google OAuth 2.0 Client:
   - Go to [Google Cloud Console](https://console.cloud.google.com/)
   - Create or select a project
   - Go to APIs & Services > Credentials
   - Create OAuth 2.0 Client ID (Web application)
   - Add Authorized redirect URI: `http://localhost:8000/auth/callback`
   - Copy the Client ID and Client Secret

### 2. Environment Variables

Set the following environment variables:

```bash
export FASTMCP_SERVER_AUTH_GOOGLE_CLIENT_ID="your-client-id.apps.googleusercontent.com"
export FASTMCP_SERVER_AUTH_GOOGLE_CLIENT_SECRET="your-client-secret"
```

### 3. Build

Build the example:

```bash
dune build examples/auth/google_oauth
```

### 4. Run the Server

Start the OCaml server:

```bash
dune exec examples/auth/google_oauth/server.exe
```

### 5. Run the Client

In another terminal, run the OCaml client:

```bash
dune exec examples/auth/google_oauth/client.exe
```

The client will attempt to connect using the configured OAuth flow.
