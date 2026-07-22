# Windows notes

## Getting started
Trndi is not _signed_ (due to the cost of doing so), so Windows SmartScreen may warn that it's from an "unknown publisher" the first time you run it. Click _More info_, then _Run anyway_ to start Trndi.

## Window take-over
Trndi can color the entire window (including the title bar; where the close/minimize buttons are). If you prefer to have a normal title bar you can customize this in Settings > Colors > "Color the title bar".

## Multiple users
Each user gets their own color and nickname (Settings > Multi User). On Windows, the nickname appears as a colored badge in the top-right of the title bar — click it to jump straight to Settings.

When more than one user is configured, Trndi asks which one to use at start-up; this selection dialog shows up as its own entry in the taskbar.

## Notifications
Trndi shows toasts using Windows' built-in `ToastNotificationManager`, invoked via a short PowerShell script — no third-party install is required.

Because Trndi doesn't register its own App User Model ID, toasts may be attributed to "Windows PowerShell" in Action Center, and may not persist there on Windows 11. This is cosmetic; the alert itself still pops up normally with the Trndi icon.
