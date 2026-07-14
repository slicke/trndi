# Medtronic CareLink (Follower) — Experimental

Trndi can show CGM data from Medtronic pumps/sensors via the **CareLink follower API** — the same data the CareLink Connect phone app shows.

> ⚠️ **Experimental.** This backend is under active development and needs testers. Data may lag behind the pump (it arrives when the patient's phone uploads, typically every ~5 minutes). As always: Trndi is not a medical device.

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

## What Trndi shows

- Sensor glucose values (about 24 h of history from each fetch)
- The pump's own trend arrow for the latest reading
- Active insulin (IOB) is parsed and available to extensions/future UI

## For testers: fixtures we need

Development of the parser runs against captured server responses. If you can help, capture these as raw JSON (browser dev tools → Network tab, or the python client's debug output), with your **region** (US/EU) and **pump model** noted:

1. **Data response** from the display-message endpoint (the big payload with the `sgs` array) — ideally three captures: a normal day, during sensor warm-up, and right after a sensor change
2. **Token refresh response** (the reply to the OAuth2 refresh request)
3. **`logindata.json` structure** with every secret replaced by `XXX` (field names matter, values don't)
4. A data response where readings are **missing or delayed**, if you catch one

Redact before sharing: replace tokens, account ids, names and serial numbers with placeholders — keep the JSON structure and timestamps intact. Drop them in a GitHub issue or on [Discord](https://discord.gg/QXACfpcW).
