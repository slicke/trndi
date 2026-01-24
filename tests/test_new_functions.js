// Test script for new Trndi extension functions
// Tests: getCurrentReading(), getLimits(), getStatistics()

console.log("=== Testing New Trndi Functions ===\n");

// Test 1: getCurrentReading()
console.log("--- Test 1: getCurrentReading() ---");
const reading = Trndi.getCurrentReading();

if (reading === false) {
  console.log("❌ No readings available");
} else {
  console.log("✓ Current Reading:");
  console.log(`  System Unit: ${reading.value_system}`);
  console.log(`  mg/dL: ${reading.value_mgdl}`);
  console.log(`  mmol/L: ${reading.value_mmol.toFixed(1)}`);
  console.log(`  Delta mg/dL: ${reading.delta_mgdl > 0 ? '+' : ''}${reading.delta_mgdl}`);
  console.log(`  Delta mmol/L: ${reading.delta_mmol > 0 ? '+' : ''}${reading.delta_mmol.toFixed(1)}`);
  console.log(`  Direction: ${reading.direction}`);
  console.log(`  Age: ${reading.age_seconds} seconds`);
  console.log(`  Timestamp: ${reading.timestamp}`);
  
  // Test age warning
  if (reading.age_seconds > 300) {
    console.log("  ⚠️  Warning: Reading is over 5 minutes old!");
  }
  
  // Test rapid change detection
  if (reading.direction === "↓↓" && reading.value_mmol < 5.0) {
    console.log("  ⚠️  Alert: Rapid drop detected!");
  }
}

console.log("");

// Test 2: getLimits()
console.log("--- Test 2: getLimits() ---");
const limits = Trndi.getLimits();

console.log("✓ CGM Limits:");
console.log(`  Low: ${limits.low_mgdl} mg/dL (${limits.low_mmol.toFixed(1)} mmol/L)`);
console.log(`  High: ${limits.high_mgdl} mg/dL (${limits.high_mmol.toFixed(1)} mmol/L)`);

if (limits.low_range_mgdl !== 0) {
  console.log(`  Range Low: ${limits.low_range_mgdl} mg/dL (${limits.low_range_mmol.toFixed(1)} mmol/L)`);
} else {
  console.log("  Range Low: Not supported by API");
}

if (limits.high_range_mgdl !== 500) {
  console.log(`  Range High: ${limits.high_range_mgdl} mg/dL (${limits.high_range_mmol.toFixed(1)} mmol/L)`);
} else {
  console.log("  Range High: Not supported by API");
}

// Test reading against limits
if (reading !== false) {
  console.log("\n  Current reading status:");
  if (reading.value_mgdl > limits.high_mgdl) {
    console.log("  ❌ ABOVE HIGH LIMIT");
  } else if (reading.value_mgdl < limits.low_mgdl) {
    console.log("  ❌ BELOW LOW LIMIT");
  } else if (limits.low_range_mgdl !== 0 && limits.high_range_mgdl !== 500) {
    if (reading.value_mgdl >= limits.low_range_mgdl && 
        reading.value_mgdl <= limits.high_range_mgdl) {
      console.log("  ✓ IN TARGET RANGE");
    } else {
      console.log("  ⚠️  In acceptable range but outside target");
    }
  } else {
    console.log("  ✓ IN ACCEPTABLE RANGE");
  }
}

console.log("");

// Test 3: getStatistics() - Last 24 hours
console.log("--- Test 3: getStatistics(1440) - Last 24 hours ---");
const stats24h = Trndi.getStatistics(1440);

if (stats24h.readingCount < 1) {
  console.log("❌ No readings in last 24 hours");
} else {
  console.log(`✓ Statistics (${stats24h.readingCount} readings):`);
  console.log(`  Mean: ${stats24h.mean.toFixed(1)} ${Trndi.getUnit()}`);
  console.log(`  Median: ${stats24h.median.toFixed(1)} ${Trndi.getUnit()}`);
  console.log(`  Std Dev: ${stats24h.stdDev.toFixed(1)}`);
  console.log(`  CV: ${stats24h.cv.toFixed(1)}%`);
  console.log(`  Time in Range: ${stats24h.timeInRange.toFixed(0)}%`);
  console.log(`  Time Above: ${stats24h.timeAbove.toFixed(0)}%`);
  console.log(`  Time Below: ${stats24h.timeBelow.toFixed(0)}%`);
  
  // Analyze CV
  if (stats24h.cv > 36) {
    console.log("  ⚠️  High glucose variability (CV > 36%)");
  } else {
    console.log("  ✓ Good glucose stability (CV < 36%)");
  }
  
  // Analyze TIR
  if (stats24h.timeInRange < 70) {
    console.log("  ⚠️  Time in range below 70% target");
  } else {
    console.log("  ✓ Good time in range (>70%)");
  }
}

console.log("");

// Test 4: getStatistics() - Last 3 hours
console.log("--- Test 4: getStatistics(180) - Last 3 hours ---");
const stats3h = Trndi.getStatistics(180);

if (stats3h.readingCount < 1) {
  console.log("❌ No readings in last 3 hours");
} else {
  console.log(`✓ Recent Statistics (${stats3h.readingCount} readings):`);
  console.log(`  Mean: ${stats3h.mean.toFixed(1)} ${Trndi.getUnit()}`);
  console.log(`  Time in Range: ${stats3h.timeInRange.toFixed(0)}%`);
  console.log(`  Time Above: ${stats3h.timeAbove.toFixed(0)}%`);
  console.log(`  Time Below: ${stats3h.timeBelow.toFixed(0)}%`);
}

console.log("");

// Test 5: Advanced use case - Compare periods
console.log("--- Test 5: Period Comparison ---");
const stats6h = Trndi.getStatistics(360);
const stats12h = Trndi.getStatistics(720);

if (stats6h.readingCount > 0 && stats12h.readingCount > 0) {
  console.log("✓ Trend Analysis:");
  
  const meanDiff = stats6h.mean - stats12h.mean;
  console.log(`  Recent 6h avg: ${stats6h.mean.toFixed(1)}`);
  console.log(`  Recent 12h avg: ${stats12h.mean.toFixed(1)}`);
  console.log(`  Difference: ${meanDiff > 0 ? '+' : ''}${meanDiff.toFixed(1)}`);
  
  if (Math.abs(meanDiff) > 1.0) {
    if (meanDiff > 0) {
      console.log("  ⚠️  Glucose trending upward");
    } else {
      console.log("  ⚠️  Glucose trending downward");
    }
  } else {
    console.log("  ✓ Glucose relatively stable");
  }
  
  // Compare variability
  console.log(`  6h CV: ${stats6h.cv.toFixed(1)}%`);
  console.log(`  12h CV: ${stats12h.cv.toFixed(1)}%`);
}

console.log("");

// Test 6: Integration test - Alert logic
console.log("--- Test 6: Sample Alert Logic ---");

function checkAlerts() {
  const current = Trndi.getCurrentReading();
  if (current === false) return;
  
  const limits = Trndi.getLimits();
  const stats = Trndi.getStatistics(180); // 3 hours
  
  const alerts = [];
  
  // Check current level
  if (current.value_mgdl > limits.high_mgdl) {
    alerts.push("🔴 Current: HIGH");
  } else if (current.value_mgdl < limits.low_mgdl) {
    alerts.push("🔴 Current: LOW");
  }
  
  // Check rapid change
  if (current.direction === "↓↓" || current.direction === "↑↑") {
    alerts.push(`⚠️  Rapid change detected (${current.direction})`);
  }
  
  // Check data age
  if (current.age_seconds > 600) {
    alerts.push(`⚠️  Stale data (${Math.floor(current.age_seconds / 60)} min old)`);
  }
  
  // Check recent trends
  if (stats.readingCount > 5) {
    if (stats.timeBelow > 20) {
      alerts.push("⚠️  Frequent lows in last 3 hours");
    }
    if (stats.cv > 40) {
      alerts.push("⚠️  High variability in last 3 hours");
    }
  }
  
  if (alerts.length === 0) {
    console.log("✓ No alerts - all systems normal");
  } else {
    console.log("Active Alerts:");
    alerts.forEach(alert => console.log(`  ${alert}`));
  }
}

checkAlerts();

console.log("\n=== All Tests Complete ===");
