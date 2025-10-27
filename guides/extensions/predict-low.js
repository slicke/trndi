// Example extension: Predict Low Blood Sugar
// This extension predicts future blood glucose readings and alerts if a low is predicted

// Configuration
const LOW_THRESHOLD_MMOL = 4.0;  // mmol/L
const LOW_THRESHOLD_MGDL = 72;   // mg/dL
const NUM_PREDICTIONS = 4;       // How many future readings to predict

// Track if we've already alerted to avoid spam
let hasAlerted = false;

function updateCallback(reading_system, reading_mgdl, reading_mmol, time) {
  // Get predictions
  const predictions = Trndi.predictReadings(NUM_PREDICTIONS);
  
  if (predictions.length === 0) {
    console.log("Insufficient data for predictions");
    return;
  }
  
  // Check which unit we're using
  const unit = Trndi.getUnit();
  const threshold = (unit === "mmol/L") ? LOW_THRESHOLD_MMOL : LOW_THRESHOLD_MGDL;
  const unitIdx = (unit === "mmol/L") ? 2 : 1;  // Index in prediction array
  
  // Check current reading
  const currentValue = (unit === "mmol/L") ? reading_mmol : reading_mgdl;
  
  // Look for predicted lows
  let willGoBelowThreshold = false;
  let timeToLow = -1;
  
  for (let i = 0; i < predictions.length; i++) {
    const predictedValue = predictions[i][unitIdx];
    
    if (predictedValue < threshold) {
      willGoBelowThreshold = true;
      timeToLow = i + 1;  // Number of readings until low
      break;
    }
  }
  
  // Alert if prediction shows low and we haven't already alerted
  if (willGoBelowThreshold && !hasAlerted && currentValue >= threshold) {
    const message = `⚠️ Prediction Alert!\n\n` +
                   `Current: ${currentValue.toFixed(1)} ${unit}\n` +
                   `Predicted to drop below ${threshold} ${unit}\n` +
                   `in approximately ${timeToLow} reading(s).\n\n` +
                   `Consider taking action to prevent low.`;
    
    Trndi.alert(message);
    hasAlerted = true;
    
    // Optional: Play a sound
    // Trndi.playSound('/path/to/alert.wav');
  }
  
  // Reset alert flag if current reading is already low or predictions are safe
  if (currentValue < threshold || !willGoBelowThreshold) {
    hasAlerted = false;
  }
  
  // Log predictions for debugging
  console.log(`Current: ${currentValue.toFixed(1)} ${unit}`);
  predictions.forEach((pred, idx) => {
    console.log(`  Prediction ${idx + 1}: ${pred[unitIdx].toFixed(1)} ${unit}`);
  });
}
