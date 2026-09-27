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

## Reading in the menu bar
Settings > Display > "Show reading in the menu bar" adds the current reading next to the clock, as a small pill in the same colours as the window: value, trend arrow (follows "Show trend arrow in taskbar/dock") and change. Until you choose otherwise it follows the Dock: on when the Dock is set to hide automatically (where the dock badge is never seen), off otherwise. During a data outage it shows `--` rather than the last value. Clicking it opens a menu to bring Trndi forward, hide the pill ("Hide from menu bar", which turns the setting off) or quit.

On a MacBook with a notch, macOS silently hides menu-bar items that no longer fit; if the pill does not appear, make room by removing other items.

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