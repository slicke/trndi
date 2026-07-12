# Backends in Trndi

Trndi provides several backends, which can be confusing. Herein the different choises are explained.

## NightScout
Connects to your NightScout server, using the stable _version 1_ API.

## NightScout v3
Connects to your NightScout server, using the more modern _version 3_ API. However, this API does not support all features Trndi requires, which results in Trndi mixing requests to the version 1 and 3 APIs, depending on what data it needs.

## Dexcom _(USA/Outside USA)_
This is Trndi's original implementation of Dexcom. It is still updated when needed, and a very viable choise for stability.

## Dexcom New _(USA/Outside USA/Japan)_
This is a complete re-write of the Dexcom driver with some more modern concepts. It was losely inspired by the _pydexcom_ library.
This backend also supports Japan, as a result.

## Tadem t:connect (USA/EU)
This is Tandem's backend which gets data from Tandem Source.
Unfortunately, a bug in Tandem Source, results in some individuals only getting readings once per hour. This is not Trndi bug.

## CareLink Follower (USA/EU/Other)
This is used for Medtronic's CareLink system. It's an experimental driver, which is a bit complicated to setup.
Username/password login is not permitted by Medtronic, instead a browser _must_ be used and a CAPTCHA _must_ be solved.

Instead of a password, CareLink requires some [extra work](/guides/CareLink.md).

## xDrip
Trndi can connect to xDrip over Wifi (or any IP anywhere). This allows it to show readings from your phone, without any middleman.