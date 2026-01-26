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

### Run with a display

```bash
./tests/TrndiTest
```

### Run headless (CI)

Because this runner is a GUI app, headless environments should use Xvfb:

```bash
xvfb-run -a ./tests/TrndiTest
```

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
