# AGENTS.md

## Scope
This file applies to the Trndi repository root and its subdirectories.

## Working Rules
- Prefer the project Makefile instead of calling `lazbuild` directly.
- On Linux and macOS, use `make` targets for build and test workflows.
- On Windows, use `make.ps1` for the same workflows when available.
- When a task needs a build or test command, prefer the smallest relevant target such as `make`, `make debug`, `make test`, or `make test-noserver`.
- Use `make noext` when you need a build path that avoids mORMot2.
- Check `make help` before inventing a new command or target.

## Branching & Pull Requests
- Always branch from `develop` and open PRs targeting `develop`, not `main`.

## Editing Guidance
- Keep changes focused and minimal.
- Preserve existing Pascal style and project layout. Formatting should adhere to the rules in `JCFSettings.xml`.
- Unit files must use the `.pp` extension.
- For cross-platform code, add platform-specific logic to `units/trndi/trndi.native.win.pp`, `trndi.native.linux.pp`, or `trndi.native.mac.pp`. Do not add `{$IFDEF}` platform blocks to `trndi.native.base`.
- Note the architectural pattern of breaking out code from large units (like `umain`) into `.inc` (include) files. Follow this pattern to keep main unit files manageable.
- Avoid unrelated refactors or formatting-only edits.
- Update documentation when behavior or workflow changes.
- When behavior changes, update the relevant user-facing docs too, such as README.md, MANUAL.md, or guides/.
- Avoid editing generated outputs or build artifacts unless you are intentionally regenerating them.
- Follow the repo documentation standard in CONTRIBUTING.md: use PasDoc comments for public APIs in interfaces and banner-style comments in implementations.
- All files must include the standard Trndi header (GPLv3 license and Medical Disclaimer), **except the `slicke.*` units** — see the carve-out below. When creating or modifying a file without a header, ensure the following is present at the top:
  ```pascal
  (*
   * Trndi
   * Medical and Non-Medical Usage Alert
   *
   * Copyright (c) Björn Lindh
   * GitHub: https://github.com/slicke/trndi
   *
   * This program is distributed under the terms of the GNU General Public License,
   * Version 3, as published by the Free Software Foundation. You may redistribute
   * and/or modify the software under the terms of this license.
   *
   * A copy of the GNU General Public License should have been provided with this
   * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
   *
   * ================================== IMPORTANT ==================================
   * MEDICAL DISCLAIMER:
   * - This software is NOT a medical device and must NOT replace official continuous
   *   glucose monitoring (CGM) systems or any healthcare decision-making process.
   * - The data provided may be delayed, inaccurate, or unavailable.
   * - DO NOT make medical decisions based on this software.
   * - VERIFY all data using official devices and consult a healthcare professional for
   *   medical concerns or emergencies.
   *
   * LIABILITY LIMITATION:
   * - The software is provided "AS IS" and without any warranty—expressed or implied.
   * - Users assume all risks associated with its use. The developers disclaim all
   *   liability for any damage, injury, or harm, direct or incidental, arising
   *   from its use.
   *
   * INSTRUCTIONS TO DEVELOPERS & USERS:
   * - Any modifications to this file must include a prominent notice outlining what was
   *   changed and the date of modification (as per GNU GPL Section 5).
   * - Distribution of a modified version must include this header and comply with the
   *   license terms.
   *
   * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
   *)
  ```
- **Exception — `slicke.*` units are Apache-2.0.** The reusable `slicke` toolkit (`units/slicke/**` and its `tests/mock/slicke.*` stubs) is licensed Apache License 2.0, not GPLv3, so it can be reused outside Trndi (Apache-2.0 is one-way compatible into Trndi's GPLv3). These units use a compact header instead of the block above — do **not** add the GPLv3 header or Medical Disclaimer to them:
  ```pascal
  (*
   * <unit>.pas
   * <one-line description>
   * Copyright (c) Björn Lindh
   * GitHub: https://github.com/slicke/trndi
   * License: Apache License 2.0
   *)
  ```

## Validation
- After code changes, run the narrowest relevant build or test target first.
- Prefer `make test-noserver` for a quick local check when the embedded test server is not needed.
- Do not widen validation beyond the touched area unless the first check fails or the change reaches adjacent build/test surfaces.
- If a change touches build behavior, verify with the matching `make` target before widening scope.
