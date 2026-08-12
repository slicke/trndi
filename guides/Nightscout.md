# Nightscout setup

Nightscout is your own server, so unlike the vendor followers Trndi supports,
what it can show you depends on what you have uploading to the site. A site fed
by a CGM bridge alone gives readings and thresholds; a site fed by AAPS, Loop or
xDrip+ also gives insulin, carbohydrates, pump housekeeping and a basal profile.
Trndi reads whatever is there and leaves out what is not.

---

## Which driver to choose

There are two entries in the backend list, and they are separate drivers rather
than versions of one:

| Backend | Nightscout API | Signs in with | Status |
|---|---|---|---|
| `NightScout` | `/api/v1/` | **API Secret** | Stable — use this for daily use |
| `NightScout v3` | `/api/v3/` | **Access token** | Beta |

Pick `NightScout` unless you specifically want what v3 adds. The extra
capabilities are listed under [What each driver reads](#what-each-driver-reads)
below; the short version is that only v3 reads treatments, pump status and the
basal profile.

**The two take different credentials, and they are not interchangeable.** The
v1 driver hashes whatever you give it and sends it as `API-SECRET`, so an access
token pasted there will not authenticate. The v3 driver exchanges what you give
it for a bearer token at `/api/v2/authorization/request/`, so an API secret
pasted there will not authenticate either. (The setup blurbs shown in the
settings dialog currently mention both for both drivers — the table above is
what the code does.)

---

## Before you start

You need:

- **Your site's URL**, e.g. `https://yoursite.up.railway.app`. Enter it with no
  trailing path — Trndi appends the API path itself.
- **The credential for your chosen driver:**
  - *For `NightScout`:* your **API Secret** — the same one you set as
    `API_SECRET` in the site's environment variables.
  - *For `NightScout v3`:* an **access token**. In your site, open
    **Admin → Subjects & Tokens**, create a subject with at least **readable**
    role, and copy its full access token — it looks like `trndi-a1b2c3d4e5f6`.
    Both `trndi-a1b2c3d4e5f6` and `token=trndi-a1b2c3d4e5f6` are accepted.

If your site is fully public (`AUTH_DEFAULT_ROLES=readable`), you can leave the
credential empty and Trndi will read it unauthenticated. The `NightScout`
driver simply sends no auth header in that case; whether v3 will serve an
unauthenticated read depends on how your site is configured.

---

## Configuring Trndi

Right-click the reading (or the "Setup" text) → **Settings**, then:

- **Backend**: `NightScout` or `NightScout v3`
- **NightScout URL**: your site's address
- **API Secret** / **Auth token**: as above

---

## What Trndi shows

**Your thresholds come from the site.** On connecting, Trndi reads
`settings.thresholds` from your Nightscout status and takes `bgHigh` / `bgLow`
as your high and low limits, and `bgTargetTop` / `bgTargetBottom` as your
personal range. You do not need to set these in Trndi — though you still can,
under Settings → Customization, if you want Trndi to disagree with your site.

**The clock is taken from the server.** Trndi compares your site's reported
time against the local one and works in the difference, so readings land at the
right point on the graph even if the two machines disagree.

### What each driver reads

| | `NightScout` | `NightScout v3` |
|---|---|---|
| Glucose readings and trends | yes | yes |
| High/low and range thresholds | yes | yes |
| Current basal rate (Service Menu) | yes | yes |
| Insulin doses on the history graph | — | yes |
| Carbohydrates on the history graph | — | yes |
| Reservoir, pump battery, sensor detail | — | yes |
| Basal profile strip on the history graph | — | yes |
| Fast retry while a reading is late | — | yes |

The v3-only rows all depend on your uploader. Treatments and device status are
read from the `treatments` and `devicestatus` collections, and what lands in
those is entirely down to what writes to your site: an AAPS or Loop rig
publishes reservoir, pump battery and suspend state, xDrip+ publishes sensor
session detail, and a site fed only by a CGM bridge publishes none of it. A
blank figure means "nothing was uploaded", never "zero".

Insulin doses and carbohydrates are off by default — turn them on under
Settings → Display. Doses an uploader marked as loop-initiated (`isSMB`,
`automatic`, or an `SMB` event type) are treated as automatic, so a looping
site's constant micro-boluses can be shown or hidden separately from the doses
you gave yourself.

On v3, an unchanged poll costs a single `lastModified` request, so when a
reading is overdue Trndi can retry on a tight cadence without hammering your
site.

---

## The basal strip, and what it does not mean

The history graph draws the basal schedule as a blue strip along the bottom.
This is the **programmed profile**, and it is worth being precise about what
that does and does not tell you:

- It is the profile **in force now** — `profile.json`'s store, keyed by
  `defaultProfile` — tiled across every day on screen. If you switched profiles
  during the period you are looking at, the earlier days are drawn with the
  schedule you are on today.
- **Nothing that departed from the schedule is in it.** Temporary basal rates,
  suspends, and every adjustment your loop made are absent. On a looping site
  the strip is what was *scheduled*, not what was *delivered* — and a loop
  spends its whole day overriding the schedule.
- The strip's height is scaled to the highest rate in your own profile, so its
  shape is readable whether your basal peaks at 0.4 or 4 U/hr. The legend states
  what full height stands for, e.g. `Basal (0−1.2 U/hr)`. Heights are only
  comparable within one graph.

The strip appears automatically when the site returns a profile; there is no
setting for it.

Service Menu → **Current Basal** answers a different question: it reports the
rate your profile has in force *at this moment*, read from the same profile.
(On a pump backend such as Tandem the same dialog can also show what the pump
was actually commanded to run — Nightscout does not report a commanded rate, so
only the one figure is shown.)

---

## Troubleshooting

**"No data" or an authorization error.** Check the credential matches the
driver — see the table at the top. A v1 API Secret in the v3 driver, or a v3
token in the v1 driver, both fail this way.

**Readings appear but no insulin, carbs or basal strip.** You are on the
`NightScout` driver; those are v3-only. Switch to `NightScout v3` — or, if you
are already on it, your site has nothing in `treatments`/`devicestatus` and no
profile, which is a question for whatever uploads to it.

**Readings are at the wrong time.** Trndi trusts your site's clock over the
local one. If the graph is shifted, check the time on the machine hosting
Nightscout.

**Thresholds are not what you set in Nightscout.** They are read once on
connecting. Restart Trndi after changing them on the site, or override them in
Settings → Customization.
