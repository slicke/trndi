# Backends in Trndi

Trndi provides several backends, which can be confusing. Herein the different choices are explained.

## NightScout
Connects to your NightScout server, using the stable _version 1_ API.

## NightScout v3
Connects to your NightScout server, using the more modern _version 3_ API. However, this API does not support all features Trndi requires, which results in Trndi mixing requests to the version 1 and 3 APIs, depending on what data it needs.

## Dexcom _(USA/Outside USA)_
This is Trndi's original implementation of Dexcom. It is still updated when needed, and a very viable choice for stability.

## Dexcom New _(USA/Outside USA/Japan)_
This is a complete re-write of the Dexcom driver with some more modern concepts. It was loosely inspired by the _pydexcom_ library.
This backend also supports Japan, as a result. New features are primarily added to this driver.

## Tandem t:connect _(USA/EU)_
This is Tandem's backend which gets data from Tandem Source. See the [setup guide](/guides/Tandem.md) for details.

Unfortunately, a bug in Tandem Source results in some individuals only getting readings once per hour. This is not a Trndi bug.

## CareLink Follower _(USA/EU/Other)_
This is used for Medtronic's CareLink system. It's an experimental driver, which is a bit complicated to set up.
Username/password login is not permitted by Medtronic, instead a browser _must_ be used and a CAPTCHA _must_ be solved.

Instead of a password, CareLink requires some [extra work](/guides/CareLink.md).

## xDrip
Trndi can connect to xDrip over Wifi. This allows it to show readings from your phone, without any middleman.

Make sure Trndi and the phone running xDrip are on the same network, and that xDrip's web service is turned on.