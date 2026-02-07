# Tests

This folder contains Trndi’s FPCUnit test suite.

## What’s here

- `TrndiTest.lpi` / `TrndiTest.lpr`: Lazarus **GUI** test runner (uses `GuiTestRunner`).
- `trnditestcase1.pp`: API + Nightscout integration tests.
- `testserver/` + `router.php`: a small PHP "fake Nightscout" used by integration tests.

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

For tests that do not require the Nightscout/PHP integration you can use the console runner (no display needed):

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
# starts PHP test server automatically (when available)
make test

# run console tests without starting PHP (uses TRNDI_NO_PHP=1)
make test-nophp
```

This runner excludes the integration tests that spawn the PHP Nightscout fake server by default; use the GUI runner for those. If you want Nightscout/integration tests in the console runner you can now opt-in:

- Set `TRNDI_TEST_SERVER_URL` to point at an existing test server (e.g. `http://localhost:8080`) to reuse it, or
- Leave `TRNDI_TEST_SERVER_URL` unset and the console tests will start/stop the PHP fake server automatically when needed (requires `php` on PATH).

The helper used by tests is `tests/php_server_helper.pp` which waits for the server `/debug` endpoint before running tests.

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

Some tests spin up PHP’s built-in server on `localhost:8080` and serve a minimal Nightscout-compatible API.

- `status.json` returns thresholds and server time.
- other endpoints return an entries array.
- access is allowed only when the request includes a header/value matching `sha1('test22')` (see `testserver/index.php`).
- the fake server respects the `count` query parameter (e.g. `?count=3`).

If port `8080` is busy, the affected tests will fail.

## Notes

- The Dexcom test is currently a smoke-test that expects `connect` to fail with dummy credentials.
- Prefer adding new tests that are deterministic and do not depend on external network access.
