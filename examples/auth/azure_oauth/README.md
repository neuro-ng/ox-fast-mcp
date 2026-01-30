# Azure OAuth Example (OCaml)

Demonstrates FastMCP server protection with Azure (Microsoft Entra) OAuth.

## Setup

### 1. Azure App Registration

1. Go to [Azure Portal → App registrations](https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade)
2. Click "New registration" and configure:
   - Name: Your app name
   - Supported account types: Choose based on your needs
   - Redirect URI: `http://localhost:8000/auth/callback` (or wherever your auth flow redirects, though this example relies on bearer tokens)
3. After creation, go to "Certificates & secrets" → "New client secret"
4. Note these values from the Overview page:
   - Application (client) ID
   - Directory (tenant) ID

### 2. Environment Variables

Set the following environment variables:

```bash
export FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID="your-client-id"
export FASTMCP_SERVER_AUTH_AZURE_CLIENT_SECRET="your-client-secret"
export FASTMCP_SERVER_AUTH_AZURE_TENANT_ID="your-tenant-id"
```

### 3. Build

Build the example:

```bash
dune build examples/auth/azure_oauth
```

### 4. Run the Server

Start the OCaml server:

```bash
dune exec examples/auth/azure_oauth/server.exe
```

### 5. Run the Client

In another terminal, run the OCaml client:

```bash
dune exec examples/auth/azure_oauth/client.exe
```

The client will attempt to connect using the configured OAuth flow/transport.
