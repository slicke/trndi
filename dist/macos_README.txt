Trndi for macOS — first-launch instructions
============================================

Trndi is not signed with an Apple Developer ID certificate, and it is not
notarized by Apple. macOS therefore attaches a "quarantine" extended
attribute to the download and refuses to open the app, usually with a
message like:

    "Trndi" is damaged and can't be opened. You should move it to the Trash.
    "Trndi" cannot be opened because the developer cannot be verified.

The app is not damaged. You just need to clear the quarantine attribute
that macOS added to the downloaded file. This is a one-time step.


How to fix it
-------------

1. Move Trndi.app to /Applications (or wherever you want to keep it).
   If you downloaded the ZIP, extract it first.

2. Open Terminal (Applications -> Utilities -> Terminal).

3. Run ONE of these commands, depending on what you downloaded:

   For the .app bundle (from the .dmg or extracted from the .zip):
       xattr -c /Applications/Trndi.app

   For the plain Trndi binary (from the .zip):
       xattr -c /path/to/Trndi

   Replace the path with the actual location if you put it elsewhere.

4. Launch Trndi normally by double-clicking it.


What this command does
----------------------

`xattr -c` clears all extended attributes from the file, including the
`com.apple.quarantine` flag that Gatekeeper checks. It does not modify
the application itself and does not weaken system security for anything
else on your Mac.


Why this is necessary
---------------------

Distributing a notarized macOS app requires a paid Apple Developer
membership and a per-build notarization roundtrip with Apple's servers.
Trndi is a free, open-source project and does not go through that
process. The quarantine attribute is the only thing standing between
you and the app — once it is cleared, Trndi runs like any other app.
