# AWS Cognito OAuth Example (OCaml)

Demonstrates FastMCP server protection with AWS Cognito OAuth using OCaml.

## Setup

### 1. AWS Cognito Configuration

1. Create an AWS Cognito User Pool and App Client:
   - Go to [AWS Cognito Console](https://console.aws.amazon.com/cognito/)
   - Create a new User Pool or use an existing one
   - Create an App Client in your User Pool
   - Configure the App Client settings:
     - Enable "Authorization code grant" flow
     - Add Callback URL: `http://localhost:8000/auth/callback`
     - Configure OAuth scopes (at minimum: `openid`)
   - Note your User Pool ID, App Client ID, Client Secret, and Cognito Domain Prefix

### 2. Environment Variables

Set the following environment variables:

```bash
export FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID="your-user-pool-id"
export FASTMCP_SERVER_AUTH_AWS_COGNITO_AWS_REGION="your-aws-region"
export FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID="your-app-client-id"
export FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET="your-app-client-secret"
```

### 3. Build

Build the example:

```bash
dune build examples/auth/aws_oauth
```

### 4. Run the Server

Start the OCaml server:

```bash
dune exec examples/auth/aws_oauth/server.exe
```

### 5. Run the Client

In another terminal, run the OCaml client:

```bash
dune exec examples/auth/aws_oauth/client.exe
```

The client will attempt to authenticate using the configured OAuth flow.
