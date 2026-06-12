/* IFTTT Webhooks
(c) Trndi sample extension
@perms net, settings
*/
// Fires IFTTT Maker Webhook events on glucose conditions.
//
// Setup:
//   1. Connect the "Webhooks" service on IFTTT and copy your key from
//      https://ifttt.com/maker_webhooks/settings
//   2. Create applets using any subset of these event names:
//        trndi_low         - reading is at or below the low limit
//        trndi_high        - reading is at or above the high limit
//        trndi_rapid_rise  - glucose is rising fast
//        trndi_rapid_fall  - glucose is dropping fast
//        trndi_stale       - no fresh reading for STALE_AFTER_MIN minutes
//   3. On first load you'll be prompted for your key. It is stored under
//      `extval.ifttt.key` - clear that setting to be re-prompted.
//
// Each request is a GET to:
//   https://maker.ifttt.com/trigger/{event}/with/key/{KEY}
//     ?value1={glucose}&value2={unit}&value3={context}

const COOLDOWN_MIN          = 15;   // per-event minimum gap, minutes
const RAPID_RISE_MMOL_PER_5 = 0.6;  // ~10.8 mg/dL per 5 min
const RAPID_FALL_MMOL_PER_5 = 0.5;  // ~9.0 mg/dL per 5 min
const STALE_AFTER_MIN       = 20;   // fire trndi_stale after this many minutes

const lastFiredAt = {};             // event name -> ms timestamp

function getKey() {
  let key = Trndi.getSetting("extval.ifttt.key");
  if (key === false || key === "") {
    key = Trndi.prompt(
      "IFTTT Webhooks key",
      "Paste your IFTTT Maker key (ifttt.com/maker_webhooks/settings):",
      ""
    );
    if (!key) return false;
    Trndi.setSetting("extval.ifttt.key", key);
  }
  return key;
}

function withinCooldown(event) {
  const last = lastFiredAt[event] || 0;
  return (Date.now() - last) < (COOLDOWN_MIN * 60 * 1000);
}

function fire(event, value, unit, ctx) {
  if (withinCooldown(event)) return;
  const key = getKey();
  if (!key) return;

  const url = "https://maker.ifttt.com/trigger/"
            + encodeURIComponent(event)
            + "/with/key/" + encodeURIComponent(key)
            + "?value1=" + encodeURIComponent(value)
            + "&value2=" + encodeURIComponent(unit)
            + "&value3=" + encodeURIComponent(ctx || "");

  lastFiredAt[event] = Date.now();
  asyncGet(url)
    .then(()  => console.log("IFTTT fired: " + event))
    .catch(err => console.log("IFTTT error (" + event + "): " + err));
}

function fetchCallback(mgdl, mmol, dmgdl, dmmol, hasData) {
  if (!hasData) return;

  const unit      = Trndi.getUnit();
  const mmolBased = (unit === "mmol/L");
  const value     = mmolBased ? mmol  : mgdl;
  const delta     = mmolBased ? dmmol : dmgdl;

  const limits = Trndi.getLimits();
  const lo     = mmolBased ? limits.low_mmol  : limits.low_mgdl;
  const hi     = mmolBased ? limits.high_mmol : limits.high_mgdl;

  const r     = Trndi.getCurrentReading();
  const arrow = (r && r.direction) ? r.direction : "";

  if (value <= lo) {
    fire("trndi_low",  value.toFixed(1), unit, arrow);
  } else if (value >= hi) {
    fire("trndi_high", value.toFixed(1), unit, arrow);
  }

  const rapidRise = mmolBased ? RAPID_RISE_MMOL_PER_5 : (RAPID_RISE_MMOL_PER_5 * 18);
  const rapidFall = mmolBased ? RAPID_FALL_MMOL_PER_5 : (RAPID_FALL_MMOL_PER_5 * 18);

  if (delta  >= rapidRise) fire("trndi_rapid_rise", value.toFixed(1), unit, "+" + delta.toFixed(1));
  if (-delta >= rapidFall) fire("trndi_rapid_fall", value.toFixed(1), unit,        delta.toFixed(1));
}

function checkStale() {
  const r = Trndi.getCurrentReading();
  if (r === false) return;
  const mins = Math.floor(r.age_seconds / 60);
  if (mins >= STALE_AFTER_MIN) {
    fire("trndi_stale", mins, "minutes", "no fresh reading");
  }
}

setInterval(checkStale, 60000);
