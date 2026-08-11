# Backends in Trndi

Trndi provides several backends, which can be confusing. Herein the different choices are explained.

## NightScout
Connects to your NightScout server, using the stable _version 1_ API.

## NightScout v3
This is a beta driver — it may not work as intended. Use plain _NightScout_ if you want the stable option.

Connects to your NightScout server, using the more modern _version 3_ API. However, this API does not support all features Trndi requires, which results in Trndi mixing requests to the version 1 and 3 APIs, depending on what data it needs.

### What Trndi reads from Nightscout v3

Beyond the glucose entries, this driver reads the `treatments` and `devicestatus` collections. Nightscout is a store rather than a device, so what comes back is whatever your uploader chose to write — a looping rig (AAPS, Loop, Trio) publishes a good deal, a site fed only by a CGM bridge publishes almost none of it, and nothing is inferred from a field that isn't there.

From `treatments`:

- **Insulin doses on the history graph**, if you turn them on (Settings → Display → *Show insulin doses on the history graph*). Anything a treatment records as `insulin` counts as a dose. Doses your loop gave itself — marked `isSMB`, `automatic`, or with an SMB event type — are filed as automatic and stay hidden unless you also tick the second checkbox, because a rig micro-bolusing every five minutes otherwise buries the boluses you gave yourself.
- **Carbohydrates on the history graph** (Settings → Display → *Show carbohydrates on the history graph*). A Nightscout treatment carries insulin and carbs on the same record, so a meal bolus contributes one dose and one meal, with no double-counting to reconcile.
- **Sensor age**, as a fallback, from the latest _Sensor Start_ / _Sensor Change_ event.

From `devicestatus`:

- **Reservoir level** (`pump.reservoir`), **pump battery** (`pump.battery.percent`) and **suspend state**, which drive the low-insulin and pump-battery notifications described in [Notifications.md](/guides/Notifications.md).
- **Sensor life remaining**, but only where a record states an actual expiry time (xDrip+/xdrip-js write one). A sensor _start_ is not enough: how long a session lasts depends on the sensor, and guessing it would announce a fresh sensor as an expiring one.

`uploader.battery` is deliberately ignored — that is the phone doing the uploading, and a flat phone is not a flat pump. Transmitter battery is left unreported for the same kind of reason: what Nightscout carries is a voltage, and a percentage derived from it would be Trndi's invention rather than a device reading.

Both collections are fetched together and cached for ten minutes, so a normal refresh still costs a single request. An empty stretch on the graph therefore means "nothing was reported for that period", never "no insulin was given".

## Dexcom _(USA/Outside USA)_
This is Trndi's original implementation of Dexcom. It is still updated when needed, and a very viable choice for stability.

## Dexcom New _(USA/Outside USA/Japan)_
This is a complete re-write of the Dexcom driver with some more modern concepts. It was loosely inspired by the _pydexcom_ library.
This backend also supports Japan, as a result. New features are primarily added to this driver.

## Tandem t:connect _(USA/EU)_
This is a beta driver — it may not work as intended.

This is Tandem's backend which gets data from Tandem Source. See the [setup guide](/guides/Tandem.md) for details.

Unfortunately, a bug in Tandem Source results in some individuals only getting readings once per hour. This is not a Trndi bug. When it happens, Trndi keeps showing the last reading it received and may report it as outdated.

## CareLink Follower _(USA/EU/Other)_
This is a beta driver — it may not work as intended. It is also a bit complicated to set up.

This is used for Medtronic's CareLink system.
Username/password login is not permitted by Medtronic, instead a browser _must_ be used and a CAPTCHA _must_ be solved.

Instead of a password, CareLink requires some [extra work](/guides/CareLink.md).

## FreeStyle Libre _(LibreLinkUp)_
This is an alpha driver — it may not work as intended.

This reads Abbott's FreeStyle Libre sensors through LibreLinkUp, Abbott's follower service. Trndi signs in as a follower, so sharing has to be set up first: the person wearing the sensor invites the follower account from the LibreLink app, and the invitation must be accepted once in LibreLinkUp.

Log in with the follower account's email and password. The region is resolved automatically — Abbott's login tells Trndi which of its regional servers the account belongs to. See the [setup guide](/guides/LibreLinkUp.md).

LibreLinkUp only publishes five trend arrows, so the double-arrow states never appear on a Libre reading, and values outside the sensor's 40–500 mg/dL range arrive as flags rather than numbers and display as HIGH/LOW.

## xDrip
Trndi can connect to xDrip over Wifi. This allows it to show readings from your phone, without any middleman.

Make sure Trndi and the phone running xDrip are on the same network, and that xDrip's web service is turned on.