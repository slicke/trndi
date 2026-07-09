# Trndi CareLink login helper

Captures the **CareLink token data** Trndi needs to follow a Medtronic pump/sensor,
without the old Python + Selenium + OpenSSL setup. It opens a browser for the
one-time login, grabs the result, and prints the token as JSON for you to paste
into Trndi.

## Why this exists

Medtronic protects the CareLink login with a CAPTCHA and only allows its mobile
app's custom-scheme redirect (`com.medtronic.carepartner:/sso`). That means the
login code can't be read by a normal browser or a background request — it has to
be caught inside a real browser at the moment of the redirect. This helper does
exactly that, then does the token exchange for you.

## Requirements

- **Node.js 18+** (uses the built-in `fetch`)
- **Puppeteer** (installed below; it downloads a browser the first time)
- A **Care Partner** (follower) CareLink account — not the patient's own account

## Install (once)

```
cd tools/carelink-login
npm install
```

## Run

EU / rest of world (default):

```
npm start
```

USA region:

```
npm run start:us
```

A browser window opens on the CareLink login page. Sign in with your Care
Partner account and solve the CAPTCHA. When you're done the window closes and the
token JSON is printed to the terminal, e.g.:

```json
{
  "access_token": "…",
  "refresh_token": "…",
  "id_token": "…",
  "scope": "profile openid offline_access",
  "client_id": "…"
}
```

Copy the whole JSON block and paste it into Trndi:
**Settings → CareLink Follower → the token/credential field → Test → Save.**

To save it straight to a file instead (progress text stays on stderr):

```
node carelink-login.mjs --us > token.json
```

## Notes

- **Treat the token like a password.** It grants read access to the followed
  patient's CGM data.
- Trndi keeps the session alive by refreshing the token automatically, so you
  normally only run this once. If Trndi is offline for a long time (about a week)
  the token can expire and you run this again.
- This helper only supports the current **Auth0** login, which is what all
  regions use now.
