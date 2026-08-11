# Medtronic CareLink (Follower) — Beta

Trndi can show CGM data from Medtronic pumps/sensors via the **CareLink follower API** — the same data the CareLink Connect phone app shows.

> ⚠️ **Beta.** This backend is under active development and needs testers. Data may lag behind the pump (it arrives when the patient's phone uploads, typically every ~5 minutes). The browser-capture login below is not a temporary rough edge — Medtronic's CAPTCHA means setup stays this way. As always: Trndi is not a medical device.

## How it works (and why setup is different)

Medtronic protects the CareLink login page with a CAPTCHA, so Trndi cannot log in with a username and password like other backends. Instead:

1. You log in **once** in your web browser and capture the resulting *token data*.
2. Trndi stores that token data (a JSON blob) as the credential.
3. Trndi keeps the session alive automatically by refreshing the token — indefinitely, as long as Trndi keeps running.

If Trndi is off for a while — sometimes just overnight — the token expires, and you repeat the one-time login.

## Which account to use

Use a **Care Partner** (follower) account that the patient has invited from the CareLink Connect app — not the patient's own account. This matches how the official follow apps work and doesn't interfere with the patient's session.

## Capturing the token data (login helper)

Trndi has a small **login helper** ([`tools/carelink-login`](../tools/carelink-login)) that does the browser login for you and captures the token. It needs **Node.js 22.12+** (it downloads a browser on first install). No Python, no OpenSSL.

The helper is compiled into Trndi. When you run it, Trndi writes a fresh copy into your **settings folder** (never into the program folder, which is often read-only), so the version always matches your Trndi build.

### The easy way: let Trndi run it

In Trndi's CareLink settings, click **Get CareLink token…**. If Node.js is installed, Trndi runs the helper for you — installing its dependencies, opening the browser for sign-in, and dropping the captured token straight into the credential field. Just sign in with your **Care Partner** account, solve the CAPTCHA, and click **Test**.

> Keep the Trndi settings window open while the browser sign-in is in progress; the first run also downloads a browser, which can take a minute.

### The manual way

If Node.js isn't found (or the automatic run fails), Trndi falls back to showing you the folder it wrote the helper into (under your settings folder) and the commands to run there yourself:

```
npm install                    # fetches dependencies
node carelink-login.mjs        # EU / rest of world
node carelink-login.mjs --us   # USA region
```

A browser window opens on the CareLink login page. Sign in with your **Care Partner** account and solve the CAPTCHA. The helper then prints a JSON block — `access_token`, `refresh_token`, `id_token`, `scope`, `client_id`. **Treat it like a password** and paste the whole block into the token field.

## Setting up Trndi

1. Right-click Trndi → **Settings**.
2. Choose **CareLink Follower (USA)** or **CareLink Follower (EU/Other)** as the system.
3. Click **Get CareLink token…** and sign in when the browser opens (Trndi fills in the token for you). If Node.js isn't installed, follow the manual steps above and paste the **entire JSON** into the token field yourself.
4. Click **Test** — Trndi resolves Medtronic's endpoints, refreshes the token and probes the data endpoint.
5. Save.

The username is captured from the token, so there is no username field to fill in.

Trndi rewrites the stored credential whenever Medtronic rotates the refresh token; don't be surprised if the stored value changes over time.

## Alternative: the Python helper

The older community helper from [carelink-python-client](https://github.com/ondrej1024/carelink-python-client) produces the same `logindata.json` and also works — paste its contents into the token field. The Node helper above is preferred (fewer dependencies, no OpenSSL).

### Optional keys in the token data

You may add these keys to the JSON blob to override defaults:

| Key | Meaning |
|-----|---------|
| `patientId` | Which followed patient to show (needed if the account follows several) |
| `role` | `carepartner` (default) or `patient` |
| `token_url` | Override the OAuth2 refresh endpoint |
| `data_url` | Override the data endpoint |

## Multiple patients

If the care partner account follows several patients, Trndi shows the first one the server returns and says so once in a notice. Set `patientId` in the token data (see the table above) to pin a different one.

There is no picker in the settings UI, and that is a deliberate choice rather than an omission: a Trndi window shows one person at a time, so the cleaner way to watch two people is a separate care partner account per patient, added under **Settings → Accounts** and run as its own window (see *Multi-User Mode* in [MANUAL.md](../MANUAL.md)). Each window then keeps its own thresholds and alarm settings. The `patientId` override exists for the case where one account already follows several patients and re-inviting is not practical.

## What Trndi shows

- Sensor glucose values (about 24 h of history from each fetch)
- The pump's own trend arrow for the latest reading
- **Your own high/low limits.** Trndi reads the limits set on the account and uses them as its high/low thresholds, so alerts and colouring follow what the CareLink app shows rather than generic defaults. The limits are a schedule; the block in force at the time of the fetch is the one applied.
- **Basal rate** (Menu → Basal rate). On a SmartGuard pump there is no fixed rate — it micro-boluses every five minutes — so this reports what was actually delivered over the last hour, in U/hr.
- **Insulin doses on the history graph**, if you turn them on (Settings → Display → *Show insulin doses on the history graph*). Doses appear as stems along the bottom of the graph, labelled with the number of units. A second checkbox adds the pump's own automatic doses; they are off by default because on a SmartGuard pump they arrive every few minutes and crowd out the boluses you actually gave.

  Stem heights are relative to the largest dose currently on screen, not to a fixed scale — read the labels, not the heights. The graph only shows doses for the period the pump last reported, so an empty stretch means "nothing was reported", never "no insulin was given".

- **Carbohydrates on the history graph** (Settings → Display → *Show carbohydrates on the history graph*), as orange discs in their own lane just above the bottom axis, labelled in grams. Grams and insulin units are different quantities, so carbs deliberately do not share the insulin scale — the disc grows with the amount, but the number is what to read.

  A meal you bolused for is usually reported twice, once as a meal entry and once as the carb figure on the bolus. Trndi shows it once: meal entries win, and a bolus's carbs are only added when no meal entry sits within 15 minutes of it. Two genuinely separate snacks a quarter of an hour apart will therefore merge — that is the deliberate trade, since counting one meal twice is the worse mistake. The same "empty means unreported" caveat as insulin applies.

Sensor life drives the sensor-expiry notifications (24/8/4/2/1 hours left) described in [Notifications.md](Notifications.md); `sensorDurationHours` counts **down**, confirmed against the app's own figure.

`pumpBatteryLevelPercent` drives the pump-battery notifications (20/15/10/5/2 percent) described in [Notifications.md](Notifications.md). How fine-grained that figure is has not been confirmed against a live account — the one captured payload reads 50, which a quarter-bucket scheme and a true percentage would both produce. If it is bucketed, only the lowest step is reachable here; worth checking against the pump display next time a battery runs down.

Parsed and available to the backend, but not shown in the UI yet: active insulin (IOB), and transmitter battery (`gstBatteryLevel`, where 255 is the device's "no reading" marker rather than a full battery).

## For testers: fixtures we need

Development of the parser runs against captured server responses. If you can help, capture these as raw JSON (browser dev tools → Network tab, or the python client's debug output), with your **region** (US/EU) and **pump model** noted:

1. **A manual-mode (non-SmartGuard) pump's data response.** This is the big one. On the auto-mode pump we have, `"basal"` comes back `null` and the delivery lives in the `markers` list instead; we have never seen what a manual-mode pump puts in `basal`, so that path is unimplemented.
2. **A response captured during sensor warm-up or right after a sensor change**, to confirm how gaps and sensor state are reported.
3. **Token refresh response** (the reply to the OAuth2 refresh request)
4. **`logindata.json` structure** with every secret replaced by `XXX` (field names matter, values don't)
5. A data response where readings are **missing or delayed**, if you catch one
6. **An `INSULIN` marker from a real bolus, and a `MEAL` marker from a real carb entry.** The bolus overlay reads `deliveredFastAmount` (falling back to `programmedFastAmount`) and treats `activationType: "AUTOCORRECTION"` as a pump-initiated dose; the carb overlay probes `amount`, `carbInput`, `carbs` and `mealAmount` in turn for the gram figure. Those field names come from the community CareLink clients rather than from a capture of our own. A payload containing a meal bolus would confirm them — check the log lines `CareLink.ExtractBoluses:` and `CareLink.ExtractCarbs:` and tell us whether the counts match what you actually entered, and whether any markers were skipped.

Redact before sharing: replace tokens, account ids, names and serial numbers with placeholders — keep the JSON structure and timestamps intact. Drop them in a GitHub issue or on [Discord](https://discord.gg/QXACfpcW).
