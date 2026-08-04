// Test script for new Trndi extension functions
// Tests: getCurrentReading(), getLimits(), getStatistics()

function test(){
console.push("=== Testing New Trndi Functions ===\n");

// Test 1: getCurrentReading()
console.push("--- Test 1: getCurrentReading() ---");
const reading = Trndi.getCurrentReading();

if (reading === false) {
  console.push("❌ No readings available");
} else {
  console.push("✓ Current Reading:");
  console.push(`  System Unit: ${reading.value_system}`);
  console.push(`  mg/dL: ${reading.value_mgdl}`);
  console.push(`  mmol/L: ${reading.value_mmol.toFixed(1)}`);
  console.push(`  Delta mg/dL: ${reading.delta_mgdl > 0 ? '+' : ''}${reading.delta_mgdl}`);
  console.push(`  Delta mmol/L: ${reading.delta_mmol > 0 ? '+' : ''}${reading.delta_mmol.toFixed(1)}`);
  console.push(`  Direction: ${reading.direction}`);
  console.push(`  Age: ${reading.age_seconds} seconds`);
  console.push(`  Timestamp: ${reading.timestamp}`);
  
  // Test age warning
  if (reading.age_seconds > 300) {
    console.push("  ⚠️  Warning: Reading is over 5 minutes old!");
  }
  
  // Test rapid change detection
  if (reading.direction === "↓↓" && reading.value_mmol < 5.0) {
    console.push("  ⚠️  Alert: Rapid drop detected!");
  }
}

console.logs();

// Test 2: getLimits()
console.push("--- Test 2: getLimits() ---");
const limits = Trndi.getLimits();

console.push("✓ CGM Limits:");
console.push(`  Low: ${limits.low_mgdl} mg/dL (${limits.low_mmol.toFixed(1)} mmol/L)`);
console.push(`  High: ${limits.high_mgdl} mg/dL (${limits.high_mmol.toFixed(1)} mmol/L)`);

if (limits.low_range_mgdl !== 0) {
  console.push(`  Range Low: ${limits.low_range_mgdl} mg/dL (${limits.low_range_mmol.toFixed(1)} mmol/L)`);
} else {
  console.push("  Range Low: Not supported by API");
}

if (limits.high_range_mgdl !== 500) {
  console.push(`  Range High: ${limits.high_range_mgdl} mg/dL (${limits.high_range_mmol.toFixed(1)} mmol/L)`);
} else {
  console.push("  Range High: Not supported by API");
}

// Test reading against limits
if (reading !== false) {
  console.push("\n  Current reading status:");
  if (reading.value_mgdl > limits.high_mgdl) {
    console.push("  ❌ ABOVE HIGH LIMIT");
  } else if (reading.value_mgdl < limits.low_mgdl) {
    console.push("  ❌ BELOW LOW LIMIT");
  } else if (limits.low_range_mgdl !== 0 && limits.high_range_mgdl !== 500) {
    if (reading.value_mgdl >= limits.low_range_mgdl && 
        reading.value_mgdl <= limits.high_range_mgdl) {
      console.push("  ✓ IN TARGET RANGE");
    } else {
      console.push("  ⚠️  In acceptable range but outside target");
    }
  } else {
    console.push("  ✓ IN ACCEPTABLE RANGE");
  }
}

console.logs();

// Test 3: getStatistics() - Last 24 hours
console.push("--- Test 3: getStatistics(1440) - Last 24 hours ---");
const stats24h = Trndi.getStatistics(1440);

if (stats24h.readingCount < 1) {
  console.push("❌ No readings in last 24 hours");
} else {
  console.push(`✓ Statistics (${stats24h.readingCount} readings):`);
  console.push(`  Mean: ${stats24h.mean.toFixed(1)} ${Trndi.getUnit()}`);
  console.push(`  Median: ${stats24h.median.toFixed(1)} ${Trndi.getUnit()}`);
  console.push(`  Std Dev: ${stats24h.stdDev.toFixed(1)}`);
  console.push(`  CV: ${stats24h.cv.toFixed(1)}%`);
  console.push(`  Time in Range: ${stats24h.timeInRange.toFixed(0)}%`);
  console.push(`  Time Above: ${stats24h.timeAbove.toFixed(0)}%`);
  console.push(`  Time Below: ${stats24h.timeBelow.toFixed(0)}%`);
  
  // Analyze CV
  if (stats24h.cv > 36) {
    console.push("  ⚠️  High glucose variability (CV > 36%)");
  } else {
    console.push("  ✓ Good glucose stability (CV < 36%)");
  }
  
  // Analyze TIR
  if (stats24h.timeInRange < 70) {
    console.push("  ⚠️  Time in range below 70% target");
  } else {
    console.push("  ✓ Good time in range (>70%)");
  }
}

console.push("");

// Test 4: getStatistics() - Last 3 hours
console.push("--- Test 4: getStatistics(180) - Last 3 hours ---");
const stats3h = Trndi.getStatistics(180);

if (stats3h.readingCount < 1) {
  console.push("❌ No readings in last 3 hours");
} else {
  console.push(`✓ Recent Statistics (${stats3h.readingCount} readings):`);
  console.push(`  Mean: ${stats3h.mean.toFixed(1)} ${Trndi.getUnit()}`);
  console.push(`  Time in Range: ${stats3h.timeInRange.toFixed(0)}%`);
  console.push(`  Time Above: ${stats3h.timeAbove.toFixed(0)}%`);
  console.push(`  Time Below: ${stats3h.timeBelow.toFixed(0)}%`);
}

console.logs();

// Test 5: Advanced use case - Compare periods
console.push("--- Test 5: Period Comparison ---");
const stats6h = Trndi.getStatistics(360);
const stats12h = Trndi.getStatistics(720);

if (stats6h.readingCount > 0 && stats12h.readingCount > 0) {
  console.push("✓ Trend Analysis:");
  
  const meanDiff = stats6h.mean - stats12h.mean;
  console.push(`  Recent 6h avg: ${stats6h.mean.toFixed(1)}`);
  console.push(`  Recent 12h avg: ${stats12h.mean.toFixed(1)}`);
  console.push(`  Difference: ${meanDiff > 0 ? '+' : ''}${meanDiff.toFixed(1)}`);
  
  if (Math.abs(meanDiff) > 1.0) {
    if (meanDiff > 0) {
      console.push("  ⚠️  Glucose trending upward");
    } else {
      console.push("  ⚠️  Glucose trending downward");
    }
  } else {
    console.push("  ✓ Glucose relatively stable");
  }
  
  // Compare variability
  console.push(`  6h CV: ${stats6h.cv.toFixed(1)}%`);
  console.push(`  12h CV: ${stats12h.cv.toFixed(1)}%`);
}

console.logs();;

// Test 6: Integration test - Alert logic
console.push("--- Test 6: Sample Alert Logic ---");

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
    console.push("✓ No alerts - all systems normal");
  } else {
    console.push("Active Alerts:");
    alerts.forEach(alert => console.push(`  ${alert}`));
  }
}

checkAlerts();

console.logs();
console.log("\n=== All Tests Complete ===");
}

setTimeout(test, 4200);
