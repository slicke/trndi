/* Bedtime Check
(c) Trndi sample extension
*/
// Once per night, between BEDTIME_START and BEDTIME_END, look at the
// current reading and suggest action if you're trending out of a safe
// pre-sleep band. Designed to nudge once, not nag.

const BEDTIME_START_HOUR = 22;   // 22:00
const BEDTIME_END_HOUR   = 24;   // up to (but not including) midnight
const SAFE_LOW_MMOL      = 6.0;
const SAFE_HIGH_MMOL     = 10.0;

let lastNagDay = -1;             // day-of-year we last nagged

function dayOfYear(d) {
  const start = new Date(d.getFullYear(), 0, 0);
  const diff  = d - start;
  return Math.floor(diff / (1000 * 60 * 60 * 24));
}

function inBedtimeWindow(d) {
  const h = d.getHours();
  return h >= BEDTIME_START_HOUR && h < BEDTIME_END_HOUR;
}

function checkBedtime() {
  const now = new Date();
  if (!inBedtimeWindow(now)) return;

  const today = dayOfYear(now);
  if (today === lastNagDay) return;          // already nagged tonight

  const r = Trndi.getCurrentReading();
  if (r === false) return;
  if (r.age_seconds > 600) return;           // don't act on stale data

  const unit  = Trndi.getUnit();
  const mmol  = r.value_mmol;
  const arrow = r.direction || "";

  const isLowish    = mmol < SAFE_LOW_MMOL;
  const isHighish   = mmol > SAFE_HIGH_MMOL;
  const droppingFast = arrow.indexOf("↓") !== -1;
  const risingFast   = arrow.indexOf("↑") !== -1;

  let msg = null;

  if (isLowish || droppingFast) {
    msg = "Bedtime check: " + r.value_system.toFixed(1) + " " + unit + " " + arrow +
          "\nConsider a small snack before sleep.";
  } else if (isHighish || risingFast) {
    msg = "Bedtime check: " + r.value_system.toFixed(1) + " " + unit + " " + arrow +
          "\nConsider correcting before sleep.";
  }

  if (msg !== null) {
    Trndi.attention("Bedtime check");
    Trndi.alert(msg);
    lastNagDay = today;
  } else {
    // In-range silent acknowledgement: only logged, not popped up.
    console.log("Bedtime check OK: " + r.value_system.toFixed(1) + " " + unit);
    lastNagDay = today;
  }
}

// Check every 5 minutes — cheap, and the window itself gates the nag.
setInterval(checkBedtime, 5 * 60 * 1000);
