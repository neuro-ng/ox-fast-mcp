TODO List for FastMCP OCaml Implementation

Completed:
- [x] Basic settings type definition with Jane Street libraries
- [x] Settings interface (.mli) with proper encapsulation
- [x] Conversion to ppx_yojson_conv from ppx_deriving_yojson
- [x] Basic module structure for settings types
- [x] Default settings creation
- [x] Environment variable loading with multiple prefixes
- [x] Settings validation system
- [x] Settings persistence (load/save from file)
- [x] Basic logging configuration
- [x] Error handling for settings operations
- [x] Add support for .env file loading
- [x] Enhanced logging configuration with formatters
- [x] Python warnings system - Using Caml.Sys.enable_runtime_warnings
- [x] Exception handling with ExceptionGroup equivalent
- [x] Add support for nested settings with delimiters
- [x] Add field-level validation functions
- [x] Add runtime type checking for JSON deserialization

Missing Python to OCaml Equivalents:
- [x] pydantic.Field equivalent - Using Field_metadata module
- [x] pydantic_settings.BaseSettings - Implemented via Settings module
- [x] pydantic_settings.EnvSettingsSource - Implemented via load_from_env
- [x] pydantic.model_validator - Using OCaml's pattern matching and Result type
- [ ] inspect.cleandoc - No direct equivalent, using OCaml doc comments
- [x] Rich tracebacks - Using Printexc and Logs.fmt
- [x] Python warnings system - Using Caml.Sys.enable_runtime_warnings
- [x] Python ExceptionGroup - Using custom Exception_group module
- [ ] Python anyio taskgroups - Using Async's Deferred

Implementation Tasks:
- [x] Implement environment variable loading with multiple prefixes
- [x] Implement deprecated prefix warning system
- [x] Implement settings validation system
- [x] Add settings persistence/loading from file
- [x] Enhance logging configuration with formatters
- [x] Add support for .env file loading
- [x] Add unified error handling system
- [x] Add support for nested settings with delimiters
- [x] Add field-level validation functions
- [x] Add runtime type checking for JSON deserialization
- [ ] Add support for extra fields handling
- [ ] Add support for partial updates of nested models
- [ ] Add settings schema generation for documentation

Migration Notes:
- The Python implementation uses Pydantic extensively for settings management and validation
- OCaml implementation uses native type system and pattern matching for validation
- Using Core.Env for environment variable handling
- Using Logs library for logging system implementation
- Using Ppx_fields_conv for field access and iteration
- Using Yojson for JSON serialization/deserialization
- Using Core's Result type for error handling
- Using Caml.Sys for runtime warnings
- Using Logs.fmt for formatted logging output
- Using custom Exception_group for handling multiple errors
- Using Field_metadata for field validation and metadata
- Using Field_value for type-safe value handling
- Using Re.Str for pattern validation

Interface Design Decisions:
- Settings record type is marked private to ensure encapsulation
- All variant types are exposed for pattern matching
- JSON and sexp conversion is supported through ppx derivers
- Field access is provided through ppx_fields_conv
- Settings can only be created through the create function
- Global settings instance is exposed but immutable
- Environment variable loading is configurable with prefixes
- Error handling uses custom Settings_error type
- Dotenv support through separate Utilities.Dotenv module
- Enhanced logging with formatters and runtime warnings
- Unified error handling through Exception_group
- Nested settings support through Field_value and Field_metadata
- Field validation through composable validator functions

Next Priority Tasks:
1. Add support for extra fields handling
2. Add support for partial updates
3. Add schema generation
4. Add support for custom serializers/deserializers
5. Add support for field dependencies

Dependencies Added:
- logs: For structured logging
- logs.fmt: For log formatting
- fmt: For pretty printing
- fmt.tty: For terminal output
- core_unix: For file operations
- re: For pattern validation
- re.str: For string pattern matching

Module Organization:
- settings.ml: Main settings implementation
- settings.mli: Public interface
- utilities/dotenv.ml: .env file handling
- utilities/dotenv.mli: Dotenv interface
- utilities/nested_settings.ml: Nested settings implementation
- utilities/nested_settings.mli: Nested settings interface
- exceptions.ml: Error handling and ExceptionGroup
- exceptions.mli: Error handling interface

Python to OCaml Module Mapping:
- fastmcp.settings -> lib/settings.ml
- fastmcp.utilities.dotenv -> lib/utilities/dotenv.ml
- fastmcp.exceptions -> lib/exceptions.ml
- fastmcp.tools -> lib/tools/
- fastmcp.client.client -> lib/client/client.ml
- fastmcp.tools.tool -> lib/tools/tool.ml
- fastmcp.utilities.json_schema -> lib/utilities/json_schema.ml
- mcp.types -> lib/mcp/types.ml
- pydantic.fields -> lib/utilities/nested_settings.ml (Field_metadata module)
- pydantic.types -> lib/utilities/nested_settings.ml (Field_value module)

Testing Tasks:
- [x] Set up test directory structure
- [x] Configure dune for testing with ppx_jane
- [x] Add expect_test_helpers_core support
- [ ] Add test coverage for all settings modules
- [ ] Add test coverage for environment variable loading
- [ ] Add test coverage for dotenv loading
- [ ] Add test coverage for file loading/saving
- [ ] Add test coverage for validation
- [ ] Add test coverage for error handling
- [ ] Add test coverage for logging configuration
- [ ] Add test coverage for nested settings
- [ ] Add test coverage for field validation
- [ ] Add test coverage for JSON serialization
- [ ] Add test coverage for deprecated features
- [ ] Add test coverage for settings customization
- [ ] Add test coverage for settings inheritance
- [ ] Add test coverage for settings merging
- [ ] Add test coverage for settings cloning
- [ ] Add test coverage for settings comparison

Missing Test Equivalents:
- [ ] pytest.mark.parametrize - Using List.iter with test cases
- [ ] pytest.fixture - Using helper functions and before/after hooks
- [ ] pytest.raises - Using [%expect.unreachable] and error handling
- [ ] pytest.approx - Using Float.equal_within for float comparisons
- [ ] pytest.skip - Not directly supported, using conditional compilation
- [ ] pytest.xfail - Not directly supported, using [%expect] with failure message
- [ ] pytest.mark.slow - Using test tags and filtering
- [ ] pytest.mark.integration - Using test tags and filtering
- [ ] pytest.mark.asyncio - Using Async.Thread_safe for async tests
- [ ] unittest.mock - Using custom mock modules or functions

Test Infrastructure Tasks:
- [x] Set up inline tests with ppx_jane
- [x] Configure expect_test_helpers_core
- [ ] Add test utilities module
- [ ] Add mock module for external dependencies
- [ ] Add test data fixtures
- [ ] Add test environment setup/teardown
- [ ] Add test coverage reporting
- [ ] Add test documentation
- [ ] Add test categorization (unit, integration, etc.)
- [ ] Add test performance benchmarks
- [ ] Add test failure reporting
- [ ] Add test retry logic for flaky tests
- [ ] Add test parallelization support
- [ ] Add test result aggregation
- [ ] Add test result reporting
- [ ] Add test result analysis 