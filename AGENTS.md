# AGENTS.md

## Scope
This file applies to the Trndi repository root and its subdirectories.

## Working Rules
- Prefer the project Makefile instead of calling `lazbuild` directly.
- On Linux and macOS, use `make` targets for build and test workflows.
- On Windows, use `make.ps1` for the same workflows when available.
- When a task needs a build or test command, prefer the smallest relevant target such as `make`, `make debug`, `make test`, or `make test-nophp`.
- Use `make noext` when you need a build path that avoids mORMot2.
- Check `make help` before inventing a new command or target.

## Editing Guidance
- Keep changes focused and minimal.
- Preserve existing Pascal style and project layout.
- Avoid unrelated refactors or formatting-only edits.
- Update documentation when behavior or workflow changes.
- When behavior changes, update the relevant user-facing docs too, such as README.md, MANUAL.md, or guides/.
- Avoid editing generated outputs or build artifacts unless you are intentionally regenerating them.
- Follow the repo documentation standard in CONTRIBUTING.md: use PasDoc comments for public APIs in interfaces and banner-style comments in implementations.

## Validation
- After code changes, run the narrowest relevant build or test target first.
- Prefer `make test-nophp` for a quick local check when PHP services are not needed.
- Do not widen validation beyond the touched area unless the first check fails or the change reaches adjacent build/test surfaces.
- If a change touches build behavior, verify with the matching `make` target before widening scope.
