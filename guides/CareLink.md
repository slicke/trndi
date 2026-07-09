# Medtronic CareLink (Follower) — Experimental

Trndi can show CGM data from Medtronic pumps/sensors via the **CareLink follower API** — the same data the CareLink Connect phone app shows.

> ⚠️ **Experimental.** This backend is under active development and needs testers. Data may lag behind the pump (it arrives when the patient's phone uploads, typically every ~5 minutes). As always: Trndi is not a medical device.

## How it works (and why setup is different)

Medtronic protects the CareLink login page with a CAPTCHA, so Trndi cannot log in with a username and password like other backends. Instead:

1. You log in **once** in your web browser and capture the resulting *token data*.
2. You paste that token data (a JSON blob) into Trndi as the credential.
3. Trndi keeps the session alive automatically by refreshing the token — indefinitely, as long as Trndi runs at least once a week or so.

If Trndi is off for a long time the token expires, and you repeat the one-time capture.

## Which account to use

Use a **Care Partner** (follower) account that the patient has invited from the CareLink Connect app — not the patient's own account. This matches how the official follow apps work and doesn't interfere with the patient's session.

## Capturing the token data

The token data is the same `logindata.json` used by other community CareLink tools. The easiest path today is the login helper from [carelink-python-client](https://github.com/ondrej1024/carelink-python-client):

```
pip install -r requirements.txt
python carelink_carepartner_api_login.py
```

This opens a browser window; log in with the Care Partner account and solve the CAPTCHA. The script saves `logindata.json`.

The file contains fields like `access_token`, `refresh_token`, `client_id`, `client_secret` and `mag-identifier`. **Treat it like a password.**

## Setting up Trndi

1. Right-click Trndi → **Settings**.
2. Choose **CareLink Follower (USA)** or **CareLink Follower (EU/Other)** as the system.
3. Username: your CareLink account username — or leave it **empty**; Trndi reads it from the token data.
4. Credential: paste the **entire contents** of `logindata.json` (one JSON blob).
5. Click **Test** — Trndi will resolve Medtronic's endpoints, refresh the token and probe the data endpoint.
6. Save.

Trndi rewrites the stored credential whenever Medtronic rotates the refresh token; don't be surprised if the pasted value changes over time.

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
