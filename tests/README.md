# Tests

This folder contains Trndi's FPCUnit test suite. It runs as a **console runner** (no
display required); integration tests start an embedded Pascal "fake Nightscout" server
in-process on demand.

## What's here

- `TrndiTestConsole.lpi` / `TrndiTestConsole.lpr`: the console test runner. It registers
  all test units (see the `uses` clause in `TrndiTestConsole.lpr`) and reports results to
  stdout, exiting non-zero on any failure or error.
- Test units (each a `*_tests.pp` / `*_test.pp` compiled into the runner), covering:
  - API clients: `api_general_tests`, `api_dexcom_tests`, `api_dexcom_new_tests`,
    `api_xdrip_tests`, `api_nightscout_tests`, `api_nightscout3_tests`
  - Trend / time logic: `dexcom_time_tests`, `dexcom_trend_tests`, `tandem_trend_tests`,
    `tandem_fixture_tests`
  - App / platform: `umain_tests`, `trndi_native_mock_tests`, `native_cookie_tests`,
    `system_media_controller_tests`
  - Debug regressions: `debug_intermit_test`, `debug_firstx_dexcom_test`,
    `debug_firstx_tandem_test`
- `testserver/`: the embedded fake Nightscout used by integration tests.
  - `pascal_testserver.pp`: in-process HTTP server unit (singleton; started by tests).
  - `test_server_helper.pp`: helper that starts/reuses the server before tests.
  - `PascalTestServer.lpi` / `PascalTestServer.lpr`: standalone server binary (useful with
    `TRNDI_TEST_SERVER_URL`).
- `mock/`: lightweight LCL / unit stubs so tests compile without a full GUI (see
  `mock/README.md`).
- `fixtures/`: sample CGM exports used by the fixture tests (see `fixtures/README.md`).

## Build & run

### From the repository root (recommended)

On Linux/macOS:

```bash
# build the console runner and run it (spawns the embedded Pascal test server on-demand)
make test

# build and run, skipping integration tests that need the embedded server
make test-noserver
```

On Windows:

```powershell
# build the console runner and run it (spawns the embedded Pascal test server on-demand)
.\make.ps1 test

# to skip integration tests, set the env var before running:
$env:TRNDI_NO_TESTSERVER = '1'; .\make.ps1 test
```

### Build / run manually

```bash
lazbuild tests/TrndiTestConsole.lpi
./tests/TrndiTestConsole          # (.exe on Windows)
```

The console runner does not need a display and starts the embedded Pascal test server
in-process when integration tests run.

## Test server environment variables

Integration tests get the embedded server automatically. You can override this with:

- `TRNDI_TEST_SERVER_URL=http://localhost:8080` — reuse an externally-running server (e.g.
  one started from `tests/testserver/PascalTestServer`). The helper waits for `/debug`
  before running tests.
- `TRNDI_NO_TESTSERVER=1` — skip integration tests that depend on the embedded server
  (used by `make test-noserver`).

The helper used by tests is `tests/testserver/test_server_helper.pp`. It is idempotent: the
server is started once per process and reused across tests.

## Nightscout fake server

The embedded Pascal HTTP server listens on `localhost:8080` and serves a minimal
Nightscout / xDrip-compatible API (see `tests/testserver/pascal_testserver.pp`).

- `/status.json` returns thresholds (`bgHigh`, `bgLow`, `bgTargetTop`, `bgTargetBottom`).
- entry endpoints (e.g. `/sgv.json`) return an entries array and respect the `count` query
  parameter (e.g. `?count=3`).
- `/debug` is an unauthenticated readiness probe.
- authenticated endpoints require a secret header whose value matches the hex SHA1 digest
  of `test22`.

If port `8080` is busy, the affected tests will fail.

## Notes

- The Dexcom test is currently a smoke-test that expects `connect` to fail with dummy
  credentials.
- Prefer adding new tests that are deterministic and do not depend on external network
  access. To wire a new test unit in, add it to the `uses` clause of
  `tests/TrndiTestConsole.lpr`.
</content>
</invoke>
