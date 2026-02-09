This folder contains fixture data used by test suites.

`tandem_1.csv` has been anonymized and deterministically randomized to remove PII and make tests reproducible.
- Seed used: 42
- Original file preserved as `tandem_1.csv.orig`
- Script used to generate: `tests/scripts/anonymize_tandem.py`

If you need to re-generate with a different seed, run:

    ./tests/scripts/anonymize_tandem.py

This file is checked into the repository intentionally (no PII remains).
