# Glucose Predictions

## ⚠️ IMPORTANT WARNING

**THIS IS A HIGHLY EXPERIMENTAL FEATURE!**

- **DO NOT depend on these predictions for medical decisions**
- **DO NOT use predictions in real-life diabetes management**
- **This feature is for research and testing purposes only**
- **We take NO RESPONSIBILITY for any consequences of using this feature**
- **Always consult your healthcare provider and use approved medical devices**

The prediction feature is provided as-is without any warranties. Use at your own risk.

---

## Overview

Trndi includes an experimental glucose prediction engine that attempts to forecast future blood glucose values based on recent readings and trends. This feature must be explicitly enabled in settings.

## How It Works

### Prediction Algorithm

The prediction engine uses recent CGM readings to calculate trends and project future glucose values. The algorithm:

1. **Analyzes recent readings** - Uses your latest CGM data points
2. **Calculates trends** - Determines the rate of change (mg/dL per minute)
3. **Projects forward** - Estimates glucose values at future time points
4. **Generates multiple predictions** - Creates forecasts for approximately 5, 10, and 15 minutes ahead

### Trend Classification

Predictions are classified into trend categories based on the rate of change (delta) in mg/dL over the prediction interval:

| Trend | Delta (mg/dL) | Arrow | Description |
|-------|---------------|-------|-------------|
| Double Up | ≥ +15 | ↑↑ | Rising very fast (≥3 mg/dL/min) |
| Single Up | +10 to +15 | ↑ | Rising fast (2-3 mg/dL/min) |
| Forty-Five Up | +5 to +10 | ↗ | Rising slowly (1-2 mg/dL/min) |
| Flat | -5 to +5 | → | Steady (-1 to +1 mg/dL/min) |
| Forty-Five Down | -10 to -5 | ↘ | Falling slowly (-2 to -1 mg/dL/min) |
| Single Down | -15 to -10 | ↓ | Falling fast (-3 to -2 mg/dL/min) |
| Double Down | ≤ -15 | ↓↓ | Falling very fast (≤-3 mg/dL/min) |

## Display Modes

### Full Mode (Default)

When predictions are enabled, the full mode displays detailed information for three time points:

```
⏱5' ↗ 145.2 | ⏱10' → 147.8 | ⏱15' ↘ 146.1
```

Each prediction shows:
- ⏱ Clock icon with minutes ahead
- Trend arrow (detailed: ↑↑, ↑, ↗, →, ↘, ↓, ↓↓)
- Predicted glucose value in your configured unit

### Short Mode

Short mode (`predictions.short`) provides a simplified at-a-glance view showing only the middle prediction (around 10 minutes ahead) with a larger, simplified arrow:

- **↗** - Glucose rising (any upward trend)
- **→** - Glucose steady (flat trend)
- **↘** - Glucose falling (any downward trend)

The simplified arrows map multiple trend levels:
- **Rising** (↗): Includes FortyFiveUp, SingleUp, and DoubleUp
- **Steady** (→): Includes Flat
- **Falling** (↘): Includes FortyFiveDown, SingleDown, and DoubleDown

The arrow is automatically scaled larger for better visibility.

## Configuration

### Enable Predictions

1. Open Settings
2. Navigate to the "Predictions" section
3. Check "Show glucose predictions"
4. (Optional) Check "Show only trend arrows (short mode)" for simplified display

### Settings

- `predictions.enable` - Enable/disable predictions (default: `false`)
- `predictions.short` - Use simplified arrow display (default: `false`)

## Limitations

- **Accuracy varies** - Predictions are estimates and may not reflect actual future values
- **Limited timeframe** - Only forecasts 5-15 minutes ahead
- **Requires recent data** - Needs sufficient recent CGM readings
- **Cannot predict events** - Does not account for food, insulin, exercise, or other factors
- **Stale data** - Predictions are not shown when CGM data is outdated

## Technical Details

### API Access

Extensions can access predictions via the JavaScript API:

```javascript
// Request 3 predictions
const predictions = api.predict(3);

// Each prediction contains:
// [value_in_current_unit, mgdl, mmol, timestamp]
```

See [Extensions API](API.md) for more details.

### Update Frequency

Predictions are recalculated whenever:
- New CGM readings arrive
- Settings are changed
- The display is refreshed

## Best Practices

1. **Never make treatment decisions** based solely on predictions
2. **Use as a supplementary tool** for awareness only
3. **Monitor actual CGM readings** as your primary data source
4. **Understand the limitations** of trend-based forecasting
5. **Disable if not needed** to reduce UI clutter

## Troubleshooting

### Predictions show "?"

- Not enough recent readings available
- Data quality issues
- API unable to generate prediction

### "Predictions Unavailable"

- Insufficient data points for prediction
- CGM connection issues
- Check that your data source is working correctly

---

**Remember: This feature is experimental. Always rely on your CGM device and healthcare provider's guidance for diabetes management decisions.**
