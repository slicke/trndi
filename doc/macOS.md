# macOS notes

## Getting started
Trndi is not _signed_ (due to the cost of doing so), this means macOS will place it in _quarantine_. To use Trndi, you must run the following in a _Terminal_ to allow Trndi to run.
```bash
xattr -c /path/to/Trndi.app
```

Normally, when installed, this is
```bash
xattr -c /Applications/Trndi.app
```

## Window take-over
Trndi can color the entire window (including the title bar; where the close/minimize buttons are). If you prefer to have a normal title bar you can customize this in Settings > Colors > "Color the title bar".

## Notifications
Trndi will ask for notification permission on first launch (a system prompt). If you don't see toasts for high/low alerts, check System Settings → Notifications & Focus and make sure Trndi is allowed.

If permission is denied or the framework isn't available, Trndi falls back to older notification APIs, and finally to AppleScript.

## Multiple users
When more than one account exists, the active account's nickname is shown as a small pill at the right end of the title bar (in the account colour, where one is set); clicking it opens Settings. It rides in a native title-bar accessory view, so it follows the window and hides with the title bar in fullscreen — where the name falls back to the `[name] Trndi` window title instead. See [the multi-user guide](../guides/Multiuser.md).

After having setup multiple users, Trndi needs to be started for each user. This is an issue on macOS as opening an app will just display the already-running instance of it.

To circumvent this, you can open Trndi via the _Terminal_
```bash
open -n -a "Trndi"
```
The _-n_ tells macOS to start another instance of Trndi.