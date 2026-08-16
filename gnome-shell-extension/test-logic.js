#!/usr/bin/env node
// SPDX-License-Identifier: GPL-3.0-only
//
// Part of Trndi - https://github.com/slicke/trndi
// Copyright (c) Björn Lindh
//
// This program is distributed under the terms of the GNU General Public
// License, Version 3, as published by the Free Software Foundation. See
// LICENSE.md in the Trndi repository, or <https://www.gnu.org/licenses/gpl-3.0>.
//
// MEDICAL DISCLAIMER: Trndi is NOT a medical device. Readings shown may be
// delayed, inaccurate, or unavailable — never make medical decisions based on
// them. See DISCLAIMER.md.

// Test the staleness logic
function testStaleness(epochSeconds, freshMinutes, nowSeconds) {
  const isStale = (nowSeconds - epochSeconds) > (freshMinutes * 60);
  return isStale;
}

// Example from the file: 11.9, 1767628900, 2
const readingEpoch = 1767628900;
const freshMin = 2;
const now = 1767633437;

const readingAge = now - readingEpoch;
const readingAgeMinutes = readingAge / 60;
const thresholdSeconds = freshMin * 60;

console.log(`Reading epoch: ${readingEpoch}`);
console.log(`Current time: ${now}`);
console.log(`Reading age: ${readingAge} seconds (${readingAgeMinutes.toFixed(1)} minutes)`);
console.log(`Threshold: ${freshMin} minutes (${thresholdSeconds} seconds)`);
console.log(`Is stale: ${testStaleness(readingEpoch, freshMin, now)}`);
console.log(`Expected: true (reading is ${readingAgeMinutes.toFixed(1)} minutes old, threshold is ${freshMin} minutes)`);

// Test the panel label composition — mirrors extension.js. Line 4 of the cache
// file carries the trend arrow, and is absent (older Trndi) or empty (the user
// turned the badge trend off) rather than ever holding a placeholder.
function labelFor(value, arrow, isStale) {
  if (isStale)
    return '--';
  return arrow ? `${value} ${arrow}` : value;
}

const labelCases = [
  ['7.2', '↗', false, '7.2 ↗', 'arrow published'],
  ['7.2', '', false, '7.2', 'badge trend off (empty line 4)'],
  ['7.2', undefined, false, '7.2', 'older Trndi (no line 4)'],
  ['7.2', '↗', true, '--', 'stale wins over the arrow'],
];

let failures = 0;
for (const [value, arrow, isStale, expected, what] of labelCases) {
  const got = labelFor(value, arrow, isStale);
  const ok = got === expected;
  if (!ok)
    failures++;
  console.log(`${ok ? 'PASS' : 'FAIL'}: ${what} -> "${got}"${ok ? '' : ` (expected "${expected}")`}`);
}

if (failures > 0)
  process.exitCode = 1;
