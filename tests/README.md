# Tests

This folder contains Trndi’s FPCUnit test suite.

## What’s here

- `TrndiTest.lpi` / `TrndiTest.lpr`: Lazarus **GUI** test runner (uses `GuiTestRunner`).
- `trnditestcase1.pp`: API + Nightscout integration tests.
- `testserver/`: embedded Pascal "fake Nightscout" used by integration tests.
  - `pascal_testserver.pp`: in-process HTTP server unit (singleton; started by tests).
  - `test_server_helper.pp`: helper that starts/reuses the server before tests.
  - `PascalTestServer.lpi` / `PascalTestServer.lpr`: standalone server binary (useful with `TRNDI_TEST_SERVER_URL`).

## Build

### Build the GUI test runner (Qt6)

From the repository root:

```bash
lazbuild --widgetset=qt6 tests/TrndiTest.lpi
```

In VS Code you can also run the task: `Build TrndiTest (Qt6)`.

## Run

### GUI runner

### Run with a display

```bash
./tests/TrndiTest
```

### Run headless (CI)

Because this runner is a GUI app, headless environments should use Xvfb:

```bash
xvfb-run -a ./tests/TrndiTest
```

---

### Console runner (no GUI)

The console runner does not need a display and starts an embedded Pascal test server in-process when integration tests run.

Build:

```bash
lazbuild tests/TrndiTestConsole.lpi
```

Run:

```bash
./tests/TrndiTestConsole
```

You can also run the console tests using the project Makefile from the repository root:

```bash
# builds the console runner and runs it (spawns the embedded Pascal test server on-demand)
make test

# run console tests without starting the embedded server (uses TRNDI_NO_TESTSERVER=1)
make test-noserver
```

On Windows, the equivalent commands are `.\make.ps1 test` and `.\make.ps1 test-noserver` (no PHP required).

Integration tests get the embedded server automatically. You can override this with:

- `TRNDI_TEST_SERVER_URL=http://localhost:8080` — reuse an externally-running server (e.g. one started from `tests/testserver/PascalTestServer.lpi`). The helper waits for `/debug` before running tests.
- `TRNDI_NO_TESTSERVER=1` — skip integration tests that depend on the embedded server (used by `make test-noserver`).

The helper used by tests is `tests/testserver/test_server_helper.pp`. It is idempotent: the server is started once per process and reused across tests.

On Debian/Ubuntu:

```bash
sudo apt-get update
sudo apt-get install -y xvfb
```

On Fedora:

```bash
sudo dnf install -y xorg-x11-server-Xvfb
```

## Nightscout fake server

Some tests spin up the embedded Pascal HTTP server on `localhost:8080` and serve a minimal Nightscout-compatible API.

- `status.json` returns thresholds and server time.
- other endpoints return an entries array.
- access is allowed only when the request includes a header/value matching `sha1('test22')` (see `tests/testserver/pascal_testserver.pp`).
- the fake server respects the `count` query parameter (e.g. `?count=3`).

If port `8080` is busy, the affected tests will fail.

## Notes

- The Dexcom test is currently a smoke-test that expects `connect` to fail with dummy credentials.
- Prefer adding new tests that are deterministic and do not depend on external network access.
