#!/usr/bin/env node

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
