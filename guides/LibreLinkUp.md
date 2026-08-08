# LibreLinkUp setup (FreeStyle Libre)

> **This backend is in alpha.** It may not work as intended, and Abbott can
> change the service without notice. Please report what you find.

Trndi reads FreeStyle Libre data through **LibreLinkUp**, Abbott's follower
service. LibreLinkUp is the app a parent or partner uses to watch someone
else's sensor — Trndi signs in as that follower and reads the share.

This means Trndi never talks to the sensor or to the LibreLink app directly.
Everything it shows has already been uploaded by the phone running LibreLink,
so a phone that is offline, out of range of the sensor, or has the app closed
stops the data reaching Trndi.

---

## Before you start

You need **two accounts**:

| Account | Who has it | What it does |
|---|---|---|
| LibreLink | The person wearing the sensor | Scans/streams the sensor, uploads readings |
| LibreLinkUp | You | Receives the share — this is what Trndi logs in as |

If you wear the sensor yourself and want Trndi on your own computer, you still
need a separate LibreLinkUp account with a different email address, and you
invite it from your own LibreLink app.

### Setting up sharing

1. In the **LibreLink** app (the sensor wearer's phone), open the menu and
   choose **Connected Apps** → **LibreLinkUp**, then **Add Connection**.
2. Enter the email address of the LibreLinkUp account.
3. Install **LibreLinkUp** on any phone, sign in with that email, and **accept
   the invitation**. This step matters: an invitation that has been sent but
   never accepted produces an account with no shares, and Trndi will report
   that no shared readings were found.
4. Confirm readings actually appear in LibreLinkUp before configuring Trndi.

---

## Configuring Trndi

Right-click the reading (or the "Setup" text) → **Settings**, then:

- **Backend**: `FreeStyle Libre (LibreLinkUp)`
- **LibreLinkUp Email**: the follower account's email
- **LibreLinkUp Password**: that account's password

There is no region to choose. Abbott runs a server per region, and a login
against the wrong one answers with the name of the right one — Trndi follows
that automatically, whether the account lives in the EU, US, Japan, Australia
or anywhere else Abbott operates.

---

## What Trndi shows

A single request returns both the current reading and about twelve hours of
history, so the value, the graph and the time-in-range figure all come from one
fetch.

- **Trend arrow** — Abbott's own arrow, on the current reading. LibreLinkUp has
  five arrows (↓ ↘ → ↗ ↑) and no double-arrow state, so Trndi will never show
  ↑↑ or ↓↓ on a live LibreLinkUp reading. Older readings in the graph carry no
  arrow from Abbott, so Trndi computes those from the rate of change.
- **HIGH / LOW** — FreeStyle Libre sensors report between 40 and 500 mg/dL
  (2.2–27.8 mmol/L). Outside that band Abbott sends a flag rather than a
  number, and Trndi displays HIGH or LOW instead of a value the sensor did not
  actually measure.
- **Thresholds** — your LibreLinkUp high and low alarm settings become Trndi's
  high/low thresholds, and your target range becomes the in-range band. Change
  them in LibreLinkUp and Trndi picks them up on the next connect.

---

## Multiple people

If the LibreLinkUp account follows more than one person, Trndi reads the first
share and says so once in a notice. There is currently no picker — Trndi shows
one value at a time, and a second follower account is the way to watch a
second person.

---

## Troubleshooting

**"No shared readings found"**
The invitation has not been accepted. Sign in to the LibreLinkUp *app* with
this account and accept it, then try Trndi again.

**"LibreLinkUp needs you to accept its terms of use / privacy policy"**
Abbott is holding the login behind a consent screen. Trndi cannot accept terms
on your behalf — open the LibreLinkUp app or [LibreView](https://www.libreview.com/)
and accept them there, then reconnect.

**"LibreLinkUp needs you to verify your email address"**
Check the inbox for the follower account and click Abbott's verification link.

**"LibreLinkUp rejected the email or password"**
Confirm you are using the **follower** account, not the LibreLink account on
the sensor wearer's phone. They are different logins even when they belong to
the same person.

**Readings stop updating, or Trndi reports rate limiting**
Abbott limits how often the follower API may be called. Trndi already
re-serves its last response for just under a minute and backs off when told
to, so this normally resolves itself. If several apps are polling the same
LibreLinkUp account, close the ones you do not need.

**Everything worked, then stopped after an update**
Abbott occasionally starts rejecting older client versions. The version Trndi
identifies as is the `LLU_VERSION` constant in
`units/trndi/api/trndi.api.librelinkup.pp`; if the API begins refusing every
request, that is the thing to bump. Please open an issue if you hit this.

---

## A note on the API

LibreLinkUp has no published API. This backend is built from the behaviour of
Abbott's own Android client, following the same route as the
[pylibrelinkup](https://github.com/robberwick/pylibrelinkup) project. Abbott can
change or break it without notice, which is one more reason Trndi is not
something to depend on for treatment decisions — see the disclaimer.
