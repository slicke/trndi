# Test mocks

This directory contains mock implementations of LCL and Trndi units used by the test suite. The mocks allow the tests to run in a console (headless) environment instead of using the GUI, so tests can be executed by the command-line test runner.

> Note: some mock files were generated with AI and have been reviewed; please check individual files when making modifications.

## License
The project is distributed under the terms of the GPLv3. If a mock file contains code that is derived from another project's original work, the original file's license applies to that portion — check the header comments in each mock for details.

## Notable mocks
- `trndi.native.mock.pp` — Mock implementation of the native driver. It fakes settings operations (dark mode, touch detection, etc.) and provides a GetURL implementation suitable for tests (uses fphttp client in the mock).
- `slicke.ux.alert.pp` — Suppresses message dialogs so tests don't require interactive GUI dialogs.
- `slicke.ux.native.pp` — Minimal native functions for tests (fonts and other UI details are not used in CLI tests).
- `razer.chroma.pp`, `razer.chroma.factory.pp` — Mocked Razer Chroma integration; tests do not perform real color changes.
- Form mocks — Some dialogs and forms (for example, settings dialog) are simplified so `umain` compiles in the test environment.
- Component mocks — GUI controls such as paintboxes and labels are simplified since they are not rendered during tests.

## Contributing
- If you add or change a mock, add a header comment documenting the origin and the applicable license.
- Add unit tests for any behavior you rely on in tests to avoid regressions.

If you think a mock is wrong or incomplete, open an issue or a pull request with a brief explanation and a test demonstrating the desired behavior.