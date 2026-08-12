# Discord Webhooks for Trndi

Post Discord messages from Trndi when your glucose crosses thresholds,
moves fast, or goes stale.

## What it does

The extension watches every fetched reading and posts on:

| Event        | When                                                  |
|--------------|-------------------------------------------------------|
| `low`        | Reading at or below your configured low limit         |
| `high`       | Reading at or above your configured high limit        |
| `rapid_rise` | Glucose rising fast (default ~0.6 mmol/L per 5 min)   |
| `rapid_fall` | Glucose dropping fast (default ~0.5 mmol/L per 5 min) |
| `stale`      | No fresh reading for 20 minutes                       |

Each post is a Discord embed with a colored sidebar (red/orange/yellow/
blue/gray), a title, and the current value, unit, and trend arrow.

A 15-minute per-event cooldown prevents a flat-low reading from spamming
the channel on every refresh.

## Setup

### 1. Create a Discord webhook

1. In your server: **Server Settings -> Integrations -> Webhooks**.
2. **New Webhook**, pick a channel, give it a name (e.g. "Trndi"), and
   optionally an avatar.
3. Click **Copy Webhook URL**.

> Treat the URL like a password. Anyone with it can post to that channel
> as your webhook.

### 2. Install the extension

1. Open Trndi -> right-click -> *Settings* -> note the **plugin folder**
   path.
2. Copy `discord-webhooks.js` from this directory into the plugin folder.
3. Restart Trndi. On first load it will:
   - Prompt to approve the `net` and `settings` permissions — click
     **Yes**.
   - Prompt for the webhook URL — paste and confirm.

The URL is saved under `extval.discord.url`. To change it, edit that
setting (see *Settings storage* in the main README) or clear it and
reload — Trndi will prompt again.

## Tuning

All thresholds are at the top of `discord-webhooks.js`:

```javascript
const COOLDOWN_MIN          = 15;
const RAPID_RISE_MMOL_PER_5 = 0.6;
const RAPID_FALL_MMOL_PER_5 = 0.5;
const STALE_AFTER_MIN       = 20;
const USERNAME              = "Trndi";
```

The low/high thresholds use your **Trndi limits** (configured in
Settings), not values in the script — so changing your limits
automatically retunes the alerts.

You can also change the embed colors:

```javascript
const COLOR_LOW   = 0xE74C3C;
const COLOR_HIGH  = 0xE67E22;
const COLOR_RISE  = 0xF1C40F;
const COLOR_FALL  = 0x3498DB;
const COLOR_STALE = 0x95A5A6;
```

## Testing

You can verify the webhook works before relying on the script — paste
this into a terminal (replace the URL):

```
curl -H "Content-Type: application/json" \
     -d '{"content":"hello from Trndi test"}' \
     https://discord.com/api/webhooks/.../...
```

If the message appears in your channel, Trndi's posts will too.

## Troubleshooting

- **Nothing happens** — check Trndi's console for `Discord posted: …` or
  `Discord error (…)` log lines.
- **`asyncPost is not defined`** — the `@perms net` line was rejected.
  Reload and approve when prompted.
- **`401` or `404` response** — the webhook was revoked or the URL is
  wrong. Recreate the webhook on Discord and clear
  `extval.discord.url`.
- **`429` (rate limited)** — you're posting too fast. Raise
  `COOLDOWN_MIN`.

## Privacy note

When an event fires, your glucose value is sent to Discord's servers and
posted to the configured channel. Anyone with access to that channel
will see it. Don't enable this if that's not acceptable for your data.

## Other webhook providers

`asyncPost` is generic — the same shape works for Slack
(`https://hooks.slack.com/services/...`, payload `{ "text": "..." }`),
Mattermost, Home Assistant webhooks, Microsoft Teams (incoming
webhooks), and so on. Copy this script, swap the URL prompt and the
payload shape, and you're done.
