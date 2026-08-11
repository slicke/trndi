This folder contains fixture data used by test suites.

`tandem_1.csv` has been anonymized and deterministically randomized to remove PII and make tests reproducible.
- Seed used: 42
- Original file preserved as `tandem_1.csv.orig`
- Script used to generate: `tests/scripts/anonymize_tandem.py`

If you need to re-generate with a different seed, run:

    ./tests/scripts/anonymize_tandem.py

This file is checked into the repository intentionally (no PII remains).

## Synthetic payloads

`tandem_pumplogs.json` and `carelink_display_message.json` are **not** captures. They are written by hand to the shape of a real account's response, so they carry no PII to begin with and stay stable across runs.

`carelink_display_message.json` is one `display/message` response from a 780G in SmartGuard, holding a limits schedule, the sensor/pump housekeeping fields, a `markers` list and ~30 minutes of `sgs`. Two cautions for anyone reading values out of it:

- The **`AUTO_BASAL_DELIVERY` markers, the housekeeping fields and the limits schedule match a live EU capture.** The **`INSULIN` and `MEAL` marker shapes do not** — no capture we have contains one, so their field names (`deliveredFastAmount`, `programmedFastAmount`, `activationType`, `carbInput`, `amount`) follow the community CareLink clients and are still unconfirmed. Treat that half of the file as a hypothesis, not a reference.
- Timestamps are **fixed and naive local**, matching the payload (only `currentServerTime` is an epoch). Naive stamps are parsed without conversion, so assertions on them hold in any timezone. The flip side is that anything the driver computes relative to *now* — the auto-basal rate, which sums a one-hour window, and `timeDiff` — will not produce meaningful values from this file. The embedded test server in `tests/testserver/` generates the same payload with `Now`-relative stamps for exactly that reason; use it when the time window is what you are testing.

`tests/api_carelink_treatment_tests.pp` drives this fixture through `ApplyPayloadMetadata`, which is why that method is protected rather than private.
