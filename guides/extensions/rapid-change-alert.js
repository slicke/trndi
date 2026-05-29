/* Rapid Change Alert
(c) Trndi sample extension
*/
// Warns when glucose is moving fast in either direction.
//
// Two layers:
//  1. Immediate delta:  delta between the last two readings.
//  2. Sustained trend:  linear prediction (next ~15 min) crosses a band.
//
// All thresholds are expressed in mmol/L per 5 minutes; the script
// converts to mg/dL automatically based on the active unit.

const RAPID_RISE_MMOL_PER_5  = 0.6;   // ~10.8 mg/dL per 5 min
const RAPID_FALL_MMOL_PER_5  = 0.5;   // ~9.0  mg/dL per 5 min
const COOLDOWN_MIN           = 10;    // don't re-alert within this window

let lastAlertAt = 0;  // ms timestamp

function nowMs() { return Date.now(); }

function withinCooldown() {
  return (nowMs() - lastAlertAt) < (COOLDOWN_MIN * 60 * 1000);
}

function fetchCallback(mgdl, mmol, dmgdl, dmmol, hasData) {
  if (!hasData) return;

  const unit       = Trndi.getUnit();
  const mmolBased  = (unit === "mmol/L");
  const delta      = mmolBased ? dmmol : dmgdl;
  const rapidRise  = mmolBased ? RAPID_RISE_MMOL_PER_5  : (RAPID_RISE_MMOL_PER_5  * 18);
  const rapidFall  = mmolBased ? RAPID_FALL_MMOL_PER_5  : (RAPID_FALL_MMOL_PER_5  * 18);
  const value      = mmolBased ? mmol : mgdl;

  if (withinCooldown()) return;

  // Layer 1: single-step delta
  if (delta >= rapidRise) {
    Trndi.attention("Rapid rise: +" + delta.toFixed(1) + " " + unit);
    Trndi.sayText("Glucose rising quickly.");
    lastAlertAt = nowMs();
    return;
  }
  if (-delta >= rapidFall) {
    Trndi.attention("Rapid fall: " + delta.toFixed(1) + " " + unit);
    Trndi.sayText("Glucose dropping quickly.");
    lastAlertAt = nowMs();
    return;
  }

  // Layer 2: trend over next ~3 readings (typically 15 min)
  const preds = Trndi.predictReadings(3);
  if (preds.length < 3) return;
  const idx       = mmolBased ? 2 : 1;
  const projected = preds[2][idx];
  const projDelta = projected - value;

  if (projDelta >= rapidRise * 2) {
    Trndi.attention("Trend rising fast → " + projected.toFixed(1) + " " + unit);
    lastAlertAt = nowMs();
  } else if (-projDelta >= rapidFall * 2) {
    Trndi.attention("Trend dropping fast → " + projected.toFixed(1) + " " + unit);
    lastAlertAt = nowMs();
  }
}
