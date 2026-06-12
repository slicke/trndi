/* Discord Webhooks
(c) Trndi sample extension
@perms net, settings
*/
// Posts to a Discord channel via webhook on glucose conditions.
//
// Setup:
//   1. In your Discord server: Server Settings -> Integrations -> Webhooks
//      -> New Webhook. Pick a channel and copy the webhook URL.
//   2. On first load Trndi prompts for the URL. It's stored under
//      `extval.discord.url` - clear that setting to be re-prompted.
//
// Messages are posted via POST to the webhook URL with a JSON body:
//   { "username": "...", "content": "...", "embeds": [ ... ] }

const COOLDOWN_MIN          = 15;   // per-event minimum gap, minutes
const RAPID_RISE_MMOL_PER_5 = 0.6;  // ~10.8 mg/dL per 5 min
const RAPID_FALL_MMOL_PER_5 = 0.5;  // ~9.0 mg/dL per 5 min
const STALE_AFTER_MIN       = 20;   // alert after this many minutes with no fresh reading
const USERNAME              = "Trndi";

// Colors are 0xRRGGBB integers (Discord embed convention)
const COLOR_LOW   = 0xE74C3C; // red
const COLOR_HIGH  = 0xE67E22; // orange
const COLOR_RISE  = 0xF1C40F; // yellow
const COLOR_FALL  = 0x3498DB; // blue
const COLOR_STALE = 0x95A5A6; // gray

const lastFiredAt = {}; // event name -> ms timestamp

function getUrl() {
  let url = Trndi.getSetting("extval.discord.url");
  if (url === false || url === "") {
    url = Trndi.prompt(
      "Discord webhook URL",
      "Paste the webhook URL from Server Settings -> Integrations -> Webhooks:",
      ""
    );
    if (!url) return false;
    Trndi.setSetting("extval.discord.url", url);
  }
  return url;
}

function withinCooldown(event) {
  const last = lastFiredAt[event] || 0;
  return (Date.now() - last) < (COOLDOWN_MIN * 60 * 1000);
}

function post(event, title, description, color) {
  if (withinCooldown(event)) return;
  const url = getUrl();
  if (!url) return;

  const payload = JSON.stringify({
    username: USERNAME,
    embeds: [{
      title: title,
      description: description,
      color: color,
      timestamp: new Date().toISOString()
    }]
  });

  lastFiredAt[event] = Date.now();
  asyncPost(url, payload)
    .then(()  => console.log("Discord posted: " + event))
    .catch(err => console.log("Discord error (" + event + "): " + err));
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

  const fmt = mmolBased ? value.toFixed(1) : String(Math.round(value));

  if (value <= lo) {
    post("low", "Low glucose",
      `${fmt} ${unit} ${arrow}`.trim(), COLOR_LOW);
  } else if (value >= hi) {
    post("high", "High glucose",
      `${fmt} ${unit} ${arrow}`.trim(), COLOR_HIGH);
  }

  const rapidRise = mmolBased ? RAPID_RISE_MMOL_PER_5 : (RAPID_RISE_MMOL_PER_5 * 18);
  const rapidFall = mmolBased ? RAPID_FALL_MMOL_PER_5 : (RAPID_FALL_MMOL_PER_5 * 18);

  if (delta >= rapidRise) {
    const sign = delta >= 0 ? "+" : "";
    post("rapid_rise", "Rapid rise",
      `${fmt} ${unit} (${sign}${delta.toFixed(1)} / 5 min)`, COLOR_RISE);
  }
  if (-delta >= rapidFall) {
    post("rapid_fall", "Rapid fall",
      `${fmt} ${unit} (${delta.toFixed(1)} / 5 min)`, COLOR_FALL);
  }
}

function checkStale() {
  const r = Trndi.getCurrentReading();
  if (r === false) return;
  const mins = Math.floor(r.age_seconds / 60);
  if (mins >= STALE_AFTER_MIN) {
    post("stale", "No fresh reading",
      `Last reading was ${mins} minutes ago.`, COLOR_STALE);
  }
}

setInterval(checkStale, 60000);
