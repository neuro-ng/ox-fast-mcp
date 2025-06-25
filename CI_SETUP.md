# CI/CD Setup for ox-fast-mcp

This document describes the GitHub Actions CI/CD pipeline setup for the ox-fast-mcp project.

## Workflows

### 1. CI Workflow (`.github/workflows/ci.yml`)

**Triggers:**
- Push to `main` and `develop` branches
- Pull requests to `main` and `develop` branches
- Releases

**Jobs:**

#### build-and-test
- Tests on Ubuntu and macOS
- Tests OCaml versions: 4.14.x, 5.0.x, 5.1.x
- Installs dependencies
- Builds the project
- Runs test suite
- Builds documentation
- Validates opam file

#### lint
- Checks code formatting with ocamlformat
- Lints opam file
- Runs on Ubuntu with OCaml 5.1.x

#### examples
- Builds and tests example applications
- Runs calculator server and demo
- Ensures examples work correctly

#### publish
- Only runs on releases
- Generates release artifacts
- Uploads build artifacts

### 2. Release Workflow (`.github/workflows/release.yml`)

**Triggers:**
- Push of version tags (v*)

**Jobs:**

#### create-release
- Creates GitHub release from tag
- Extracts version information
- Sets up release notes

#### build-release
- Builds release binaries for Linux and macOS
- Creates release archives
- Uploads assets to GitHub release

### 3. Security Workflow (`.github/workflows/security.yml`)

**Triggers:**
- Push to main/develop branches
- Pull requests
- Weekly schedule (Mondays at 6 AM UTC)

**Jobs:**

#### dependency-scan
- Scans for dependency issues with opam-dune-lint
- Checks opam security advisories
- Validates package ecosystem health

#### code-quality
- Enforces code formatting standards
- Builds with strict warnings
- Ensures code quality standards

## Configuration Files

### `.ocamlformat`
- Defines code formatting standards
- Version 0.26.0 with default profile
- 80-character margin
- Specific formatting rules for OCaml code

### `.gitignore`
- Ignores build artifacts (`_build/`, `*.exe`, etc.)
- Ignores editor files (`.vscode/`, `.idea/`)
- Ignores OCaml compiled files
- Ignores opam local state

### `CHANGELOG.md`
- Follows Keep a Changelog format
- Referenced by release workflow
- Semantic versioning compliance

## Local Development

To test CI behavior locally:

```bash
# Install dependencies
opam install . --deps-only --with-test

# Build project
dune build

# Run tests
dune runtest

# Build examples
dune build examples/

# Check formatting
dune build @fmt

# Lint opam file
opam lint ox-fast-mcp.opam
```

## Release Process

1. Update `CHANGELOG.md` with new version changes
2. Update version in `ox-fast-mcp.opam` and `dune-project`
3. Commit changes
4. Create and push version tag: `git tag v0.1.0 && git push origin v0.1.0`
5. GitHub Actions will automatically:
   - Create GitHub release
   - Build and upload artifacts
   - Run all CI checks

## Badge Status

Add these badges to your README.md:

```markdown
![CI](https://github.com/neuro-ng/ox-fast-mcp/workflows/CI/badge.svg)
![Security](https://github.com/neuro-ng/ox-fast-mcp/workflows/Security/badge.svg)
```

## Troubleshooting

### Common Issues

1. **OCaml version compatibility**: The CI tests multiple OCaml versions. If builds fail, check compatibility.

2. **Formatting issues**: Run `dune build @fmt --auto-promote` to fix formatting.

3. **Dependency issues**: Update opam file if new dependencies are added.

4. **Test failures**: Ensure all tests pass locally before pushing.

### Debugging CI

- Check workflow logs in GitHub Actions tab
- Test commands locally using `opam exec -- <command>`
- Validate opam file with `opam lint ox-fast-mcp.opam` 