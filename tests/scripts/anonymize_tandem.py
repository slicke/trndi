#!/usr/bin/env python3
"""Anonymize and deterministically randomize Tandem CSV fixture.

- Creates a backup file tandem_1.csv.orig (already created by caller if needed)
- Overwrites tandem_1.csv with anonymized content
- Replaces patient name, birthdate, report created date, serial numbers
- Randomizes EGV values while preserving trend; deterministic via seed
- Adds a comment line indicating seed used
"""
import csv
import random
from pathlib import Path

SEED = 42
SRC = Path(__file__).resolve().parents[1] / 'fixtures' / 'tandem_1.csv'

print(f"Processing {SRC}")

lines = SRC.read_text(encoding='utf-8').splitlines()
reader = csv.reader(lines)
rows = list(reader)

# Collect EGV indices
egv_rows = []
for i, row in enumerate(rows):
    if len(row) >= 5 and row[2] == 'EGV' and row[4].strip() != '':
        try:
            val = float(row[4])
            egv_rows.append((i, val))
        except ValueError:
            pass

if not egv_rows:
    print('No EGV rows found; aborting')
    exit(1)

# Generate new values
random.seed(SEED)
new_vals = []
prev_new = None
prev_orig = None
for idx, orig in egv_rows:
    if prev_new is None:
        # first value: small jitter
        new = orig + random.uniform(-0.05, 0.05)
    else:
        # follow original trend but smooth and add small noise
        trend = orig - prev_orig
        new = prev_new + 0.8 * trend + random.uniform(-0.05, 0.05)
    # clamp to reasonable mmol/L range
    new = max(2.0, min(22.0, new))
    # round to one decimal to match input format
    new = round(new, 1)
    new_vals.append((idx, new))
    prev_new = new
    prev_orig = orig

# Apply changes to rows
# Anonymize header fields
for i, row in enumerate(rows):
    if len(row) >= 2 and row[0] == 'Patientnamn':
        rows[i][1] = 'Anonymized Patient'
    if len(row) >= 2 and row[0] == 'Patientens födelsedatum':
        rows[i][1] = 'ANONYMIZED'
    if len(row) >= 2 and row[0] == 'Rapport skapad den':
        rows[i][1] = f'ANONYMIZED (seed={SEED})'
    # Replace serial numbers like 123456789 with 000000000
    if len(row) >= 2 and row[1] == '123456789':
        rows[i][1] = '000000000'

# Replace EGV values
for (i, _), (_, newval) in zip(egv_rows, new_vals):
    if len(rows[i]) >= 5:
        rows[i][4] = str(newval)

# Insert a comment line near top to indicate anonymization
# Find first non-empty line after initial header lines
insert_at = 2
comment = ['# Anonymized and deterministically randomized (seed=%d)' % SEED]
rows.insert(insert_at, comment)

# Write back to file
out_lines = []
for row in rows:
    # ensure we preserve trailing commas similar to original (many empty fields)
    # write joined row and preserve number of columns by padding
    # Determine max columns to preserve padding
    out_lines.append(','.join(row))

SRC.write_text('\n'.join(out_lines) + '\n', encoding='utf-8')
print('Wrote anonymized CSV')
