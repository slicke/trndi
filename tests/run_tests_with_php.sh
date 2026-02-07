#!/bin/sh
set -e

PORT=8080
ROOT="$(dirname "$0")/testserver"
ROUTER="$ROOT/index.php"
LOG=/tmp/trndi-php-testserver.log

if ! command -v php >/dev/null 2>&1; then
  echo "php not found on PATH; please install PHP to run integration tests"
  exit 1
fi

# Start PHP built-in server in background
php -S 127.0.0.1:$PORT -t "$ROOT" "$ROUTER" >"$LOG" 2>&1 &
PHP_PID=$!

# Ensure we kill PHP if this script exits
trap 'kill "$PHP_PID" 2>/dev/null || true; wait "$PHP_PID" 2>/dev/null || true' EXIT INT TERM

BASEURL="http://127.0.0.1:$PORT"

# Wait for /debug to be available (up to ~2s)
for i in $(seq 1 20); do
  if command -v curl >/dev/null 2>&1; then
    if curl --silent --fail "$BASEURL/debug" >/dev/null 2>&1; then
      break
    fi
  else
    if wget -q -O- "$BASEURL/debug" >/dev/null 2>&1; then
      break
    fi
  fi
  sleep 0.1
done

# Build console runner (if not built already)
if [ ! -x "./tests/TrndiTestConsole" ]; then
  echo "Building console test runner..."
  lazbuild tests/TrndiTestConsole.lpi
fi

# Run the tests (preserve exit status)
./tests/TrndiTestConsole
RESULT=$?

# Script will exit and trap will kill PHP server
exit $RESULT
