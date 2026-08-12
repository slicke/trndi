# IFTTT Webhooks for Trndi

Fire IFTTT Maker Webhook events from Trndi when your glucose crosses
thresholds, moves fast, or goes stale. Wire those events to anything IFTTT
supports — smart bulbs, push notifications, Google Sheets logging, smart
speakers, SMS, etc.

## What it does

The extension watches every fetched reading and fires a webhook on:

| Event              | When                                                  |
|--------------------|-------------------------------------------------------|
| `trndi_low`        | Reading at or below your configured low limit         |
| `trndi_high`       | Reading at or above your configured high limit        |
| `trndi_rapid_rise` | Glucose rising fast (default ~0.6 mmol/L per 5 min)   |
| `trndi_rapid_fall` | Glucose dropping fast (default ~0.5 mmol/L per 5 min) |
| `trndi_stale`      | No fresh reading for 20 minutes                       |

Each event sends three values:

- `value1` — current glucose (or stale age in minutes), formatted
- `value2` — unit string (`mmol/L`, `mg/dL`, or `minutes`)
- `value3` — context (trend arrow, signed delta, or `"no fresh reading"`)

A 15-minute per-event cooldown prevents a flat-low reading from spamming
IFTTT on every refresh.

## Setup

### 1. Enable IFTTT Webhooks and get your key

1. Sign in at <https://ifttt.com>.
2. Connect the **Webhooks** service: <https://ifttt.com/maker_webhooks>.
3. Click **Documentation** (top-right) to see your key. It looks like
   `bX_xxxxxxxxxxxxxxxxxxxxxx` — copy it.

> Treat the key like a password. Anyone who has it can trigger your applets.

### 2. Create applets

For each event you care about, create an applet:

1. New Applet → **If This** → search **Webhooks** → *Receive a web request*.
2. **Event Name**: one of `trndi_low`, `trndi_high`, `trndi_rapid_rise`,
   `trndi_rapid_fall`, `trndi_stale`. Must match exactly.
3. **Then That**: pick any action (Notifications, Hue, Sheets, SMS…).
4. In the action, reference the values Trndi sends:
   - `{{Value1}}` — the glucose reading (or stale-age)
   - `{{Value2}}` — the unit
   - `{{Value3}}` — trend arrow / delta / context
   - `{{OccurredAt}}` — IFTTT-provided timestamp

Example notification text:
```
Glucose {{Value1}} {{Value2}} {{Value3}} ({{OccurredAt}})
```

You only need to create the applets you want — missing events are silently
dropped by IFTTT.

### 3. Install the extension

1. Open Trndi → right-click → *Settings* → note the **plugin folder** path.
2. Copy `ifttt-webhooks.js` from this directory into the plugin folder.
3. Restart Trndi. On first load it will:
   - Prompt to approve the `net` and `settings` permissions — click **Yes**.
   - Prompt for your IFTTT Maker key — paste and confirm.

The key is saved under `extval.ifttt.key`. To change it, edit that setting
(see *Settings storage* in the main README) or clear it and reload — Trndi
will prompt again.

## Tuning

All thresholds are at the top of `ifttt-webhooks.js`:

```javascript
const COOLDOWN_MIN          = 15;
const RAPID_RISE_MMOL_PER_5 = 0.6;
const RAPID_FALL_MMOL_PER_5 = 0.5;
const STALE_AFTER_MIN       = 20;
```

The low/high thresholds use your **Trndi limits** (configured in Settings),
not values in the script — so changing your limits automatically retunes
the alerts.

## Testing

You can fire any event manually from a browser or curl to confirm your
applet works before relying on it:

```
https://maker.ifttt.com/trigger/trndi_low/with/key/YOUR_KEY?value1=3.8&value2=mmol/L&value3=test
```

IFTTT applets typically fire within a few seconds.

## Troubleshooting

- **Nothing happens** — check Trndi's console for `IFTTT fired: …` or
  `IFTTT error (…)` log lines.
- **`asyncGet is not defined`** — the `@perms net` line was rejected.
  Reload and approve when prompted.
- **Wrong key** — clear `extval.ifttt.key` from settings and reload.
- **Applet ran but action didn't** — check the applet's *Activity* tab on
  ifttt.com; the run log shows what the action returned.

## Privacy note

When an event fires, your glucose value is sent to IFTTT's servers and
passes through any third-party services your applet uses. Don't enable
this if that's not acceptable for your data.
