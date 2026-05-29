/* Stale Reading Watchdog
(c) Trndi sample extension
*/
// Escalating alert when no fresh reading arrives within a window.
// Useful for catching sensor disconnects, dead phones, or lost uploaders.
//
// Tiers (minutes since last reading):
//   - WARN_AFTER:   silent system notification
//   - ALERT_AFTER:  spoken alert + popup
//   - PANIC_AFTER:  repeated popups every PANIC_REPEAT minutes

const WARN_AFTER    = 12;   // first nudge
const ALERT_AFTER   = 20;   // spoken alert
const PANIC_AFTER   = 35;   // repeated popups
const PANIC_REPEAT  = 10;   // minutes between panic popups

let lastWarnLevel  = 0;     // 0 none, 1 warned, 2 alerted, 3 panicking
let lastPanicAtMin = 0;     // minutes-since-reading at last panic popup

function fmtAge(seconds) {
  const m = Math.floor(seconds / 60);
  const s = seconds % 60;
  return m + "m " + s + "s";
}

function ageMinutes() {
  const r = Trndi.getCurrentReading();
  if (r === false) return -1;
  return Math.floor(r.age_seconds / 60);
}

function checkStale() {
  const mins = ageMinutes();
  if (mins < 0) return;

  // Reading is fresh again - reset state.
  if (mins < WARN_AFTER) {
    if (lastWarnLevel > 0) {
      console.log("Reading flow recovered after " + mins + " min");
    }
    lastWarnLevel = 0;
    lastPanicAtMin = 0;
    return;
  }

  if (mins >= PANIC_AFTER) {
    if (lastWarnLevel < 3 || (mins - lastPanicAtMin) >= PANIC_REPEAT) {
      Trndi.attention("No CGM data for " + mins + " minutes!");
      Trndi.sayText("Warning. No glucose data for " + mins + " minutes.");
      Trndi.alert("No reading for " + mins + " minutes.\nCheck sensor, phone, and uploader.");
      lastWarnLevel = 3;
      lastPanicAtMin = mins;
    }
    return;
  }

  if (mins >= ALERT_AFTER && lastWarnLevel < 2) {
    Trndi.sayText("Glucose reading is " + mins + " minutes old.");
    Trndi.attention("Stale reading: " + mins + " min old");
    lastWarnLevel = 2;
    return;
  }

  if (mins >= WARN_AFTER && lastWarnLevel < 1) {
    Trndi.attention("Reading is getting old (" + mins + " min)");
    lastWarnLevel = 1;
  }
}

// Run the check on every refresh tick...
function updateCallback() {
  checkStale();
}

// ...and also on a 60s timer, so we still escalate when no new
// reading arrives at all.
setInterval(checkStale, 60000);
