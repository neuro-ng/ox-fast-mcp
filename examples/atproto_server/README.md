# ATProto MCP Server Example

This is an OCaml implementation of an MCP server for Bluesky/AT Protocol integration, providing full access to Bluesky's social networking features through the Model Context Protocol.

## Status

**Current Implementation**: ✅ Production Ready

This implementation includes a fully functional AT Protocol client with real API calls to Bluesky. Core features are complete and ready for production use:

- ✅ Authentication and session management
- ✅ Post creation with rich text (auto URL detection, links, mentions)
- ✅ Timeline, notifications, and search
- ✅ Social actions (follow, like, repost)
- ✅ Profile information
- ✅ Thread creation

See [`ATPROTO.todo`](./ATPROTO.todo) for detailed implementation status and roadmap.

## Features

### Resources (Read-only Data)
- **atproto://profile/status** - Profile information and connection status
- **atproto://timeline** - Authenticated user's timeline feed  
- **atproto://notifications** - Recent notifications

### Tools (Actions)
- **post** - Create posts with optional rich text (links, mentions, auto URL detection)
- **search** - Search posts by text query
- **follow** - Follow users by handle
- **like** - Like posts by URI/CID
- **repost** - Repost content
- **create_thread** - Create sequential post threads

## Setup

1. Set environment variables:
```bash
export ATPROTO_HANDLE="your.handle@bsky.social"
export ATPROTO_PASSWORD="your-app-password"
export ATPROTO_PDS_URL="https://bsky.social"  # optional
```

2. Build and run:
```bash
# Build
dune build examples/atproto_server/atproto_server.exe

# Run
dune exec atproto-server
```

## Structure

```
atproto_server/
├── types.ml              # Type definitions (all ATProto response/request types)
├── settings.ml           # Configuration from environment variables
├── atproto_server.ml     # Main entry point with MCP server setup
└── dune                  # Build configuration
```

## Type Definitions

All types mirror the Python TypedDict definitions with OCaml records:
- `profile_info` - Profile status and metadata
- `post_result` - Post creation result
- `timeline_result` - Timeline feed data
- `search_result` - Search results
- Rich text types: `rich_text_link`, `rich_text_mention`
- Social action results: `follow_result`, `like_result`, `repost_result`
- `thread_result` - Thread creation result

All types use `[@@deriving yojson, sexp]` for JSON serialization compatible with MCP protocol.

## Implementation Notes

### Configuration (`settings.ml`)

Environment-based configuration similar to Python's pydantic-settings:
```ocaml
let settings = Settings.get_settings ()
(* Reads ATPROTO_HANDLE, ATPROTO_PASSWORD, ATPROTO_PDS_URL *)
```

### Type Safety

OCaml's type system provides compile-time guarantees that all JSON responses match the expected structure:
```ocaml
let profile = Types.{
  connected = true;
  handle = Some "user.bsky.social";
  (* ... *)
}
Types.profile_info_to_yojson profile  (* Auto-generated serializer *)
```

## Future Enhancements

The core AT Protocol functionality is complete. Planned enhancements include:

- **Media Support**: Image and video uploads with blob storage
- **Advanced Features**: Quote posts, proper reply threading, profile editing
- **Session Management**: Token refresh, session persistence
- **Additional Operations**: Unfollow, unlike, unrepost, mute/block
- **Performance**: Caching, rate limiting, batch operations

See [`ATPROTO.todo`](./ATPROTO.todo) for the complete roadmap and implementation status.

## Implementation Details

### Authentication Flow

The client implements OAuth-style authentication:
```ocaml
POST /xrpc/com.atproto.server.createSession
→ Returns { did, accessJwt, refreshJwt, handle }
```

All subsequent API calls include the JWT in the Authorization header.

### Rich Text Implementation

Posts support automatic URL detection and manual rich text facets:
- **Auto URL Detection**: Regex-based URL detection in post text
- **Manual Links**: Custom display text with target URLs
- **Mentions**: Handle-based mentions (@ references)
- **Facet Indexing**: UTF-8 byte position calculation for text ranges

### API Endpoints Used

- `com.atproto.server.createSession` - Authentication
- `com.atproto.identity.resolveHandle` - Handle to DID resolution  
- `com.atproto.repo.createRecord` - Create posts/likes/reposts/follows
- `app.bsky.actor.getProfile` - Profile information
- `app.bsky.feed.getTimeline` - Timeline feed
- `app.bsky.feed.searchPosts` - Post search
- `app.bsky.notification.listNotifications` - Notifications

See the [AT Protocol Documentation](https://atproto.com/docs) for endpoint specifications.

## Comparison with Python Version

| Python | OCaml | Notes |
|--------|-------|-------|
| `TypedDict` | `type t = { ... } [@@deriving yojson]` | Compile-time type safety |
| `pydantic.BaseSettings` | Manual `Sys.getenv` parsing | Simpler, direct approach |
| `atproto.Client` | Mock implementation | Real client needs HTTP bindings |
| `@atproto_mcp.tool` | `Server.Ox_fast_mcp.add_simple_tool` | Explicit registration |
| `@atproto_mcp.resource` | `Server.Ox_fast_mcp.add_simple_resource` | Explicit registration |

## License

See project [LICENSE](../../LICENSE).
