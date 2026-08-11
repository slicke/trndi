# Notifications
Trndi can show toasts (those small pop-ups near the clock) when you're high or low.

Enabling notifications is optional — you can use Trndi without them.

## What Trndi notifies about
- **High / low blood sugar** — `alerts.notice.hilo`, rate-limited by the alert engine's rules.
- **Missing or stale readings** — `alerts.notice.missing`.
- **Suspected sensor fault** — repeated large jumps between consecutive readings.
- **Low pump reservoir** — `alerts.notice.reservoir`, on by default. One toast each
  time the reservoir falls to 30, 25, 20, 15, 10 and 5 units. Only backends that
  report a reservoir (Tandem Source, CareLink) can trigger it; the rest leave the
  field at `DEVICE_STATUS_UNKNOWN` and are skipped, since a missing figure must
  never read as an empty cartridge.

  The ladder is evaluated after each successful fetch by `TfBG.CheckPumpReservoir`
  (`inc/umain_alerts.inc`) on top of `ReservoirShouldNotify` in
  `units/trndi/trndi.alert.engine.pp`. A step notifies once: it stays quiet until
  either a lower step is reached or the level climbs clear of the fired step by
  `RESERVOIR_REARM_MARGIN`, which is also what re-arms the ladder after a refill.
  The latch is persisted as `alerts.reservoir.step`, so restarting Trndi on a low
  cartridge does not repeat a warning that was already shown.
- **Sensor about to expire** — `alerts.notice.sensor`, on by default. One toast each
  time the remaining sensor life falls to 24, 8, 4, 2 and 1 hours. Only CareLink
  fills `sensorDurationHours`; Tandem's CGM events carry no session age and the
  plain CGM backends report none either, so they leave the field at
  `DEVICE_STATUS_UNKNOWN` and are skipped.

  Same shape as the reservoir ladder: `TfBG.CheckSensorExpiry`
  (`inc/umain_alerts.inc`) over `SensorExpiryShouldNotify`, latch persisted as
  `alerts.sensor.step`, and a sensor change re-arms the ladder once the figure is
  clear of the fired step by `SENSOR_EXPIRY_REARM_MARGIN` — no sensor-change event
  is needed from the payload. `sensorDurationHours` counts **down** (remaining
  life), confirmed against the CareLink app; a backend that reports elapsed time
  instead must convert before filling the field, or a fresh sensor would be
  announced as an expiring one.
- **Low pump battery** — `alerts.notice.battery`, on by default. One toast each
  time the pump battery falls to 20, 15, 10, 5 and 2 percent. Both pump backends
  fill `pumpBatteryPercent` — Tandem from the `ibc` property on its status and
  battery events, CareLink from `pumpBatteryLevelPercent` — and the plain CGM
  backends leave it at `DEVICE_STATUS_UNKNOWN` and are skipped.

  Same shape again: `TfBG.CheckPumpBattery` (`inc/umain_alerts.inc`) over
  `PumpBatteryShouldNotify`, latch persisted as `alerts.battery.step`. The one
  difference from the other two ladders is `PUMP_BATTERY_REARM_MARGIN`, which is
  wider (5 points) because a battery goes back up as a matter of routine where a
  cartridge does not: a t:slim on the charger for a few minutes must not re-arm a
  step it has already announced, while a real charge or a fresh cell does.

  How fine-grained CareLink's percentage is has **not** been confirmed against a
  live account — if it turns out to report in coarse buckets (25/50/75/100), only
  the lowest step is reachable there and the warning arrives late. Tandem's `ibc`
  is verified fine-grained (values like 80, 45, 35 in a live fetch).

## How Trndi chooses a backend
- Windows: Uses the built‑in WinRT toast API (`Windows.UI.Notifications.ToastNotificationManager`) via PowerShell — no third‑party module required.
- macOS: Uses the built‑in user notification center — no setup required.
- Linux: Auto‑selects between org.freedesktop.Notifications over D‑Bus (gdbus) on KDE/GNOME‑like desktops and notify-send elsewhere. Trndi detects this at runtime; no manual toggle is needed.

Tip: You can see which backend is active in logs or by observing which tool is invoked (gdbus vs notify-send) when a notification fires.

## Windows
Trndi shows toasts by invoking the WinRT `ToastNotificationManager` directly from a short PowerShell script. PowerShell ships with Windows, so no install step is required.

Because Trndi does not register its own AUMID (App User Model ID), toasts are dispatched under PowerShell's built‑in AUMID. Practical consequences:

- The toast pops as expected and shows the Trndi icon as the app logo.
- The notification may be attributed to "Windows PowerShell" in Action Center, and may not persist there on Windows 11.

If `powershell.exe` is missing from the system directory (very unusual), Trndi falls back to no notifications.

## macOS
Trndi uses the modern UserNotifications framework when available and will perform a best‑effort request for notification permission on startup (you may see the system permission prompt). If permission is denied or the framework isn’t available the app falls back to the older notification APIs and a final fallback to AppleScript.

Make sure notifications for the app are allowed under System Settings if you don’t see toasts.

## Linux
Trndi supports two common Linux paths and chooses automatically:

## Testing notifications
To test macOS notifications manually, run Trndi and trigger an alert (for example, temporarily change a threshold so an alert fires), or invoke the native method from debug code:

  TrndiNative.attention('Trndi test', 'This is a test notification');

If you don’t see a notification, check System Settings → Notifications & Focus and verify Trndi is allowed. If the app never shows the permission prompt, watch the logs for the startup authorization call (best‑effort request).

- KDE/GNOME‑like desktops (under Qt6 builds): Uses D‑Bus via gdbus with org.freedesktop.Notifications.
- Other desktops or when D‑Bus isn’t suitable: Falls back to notify-send.

Notes
- notify-send is usually provided by libnotify; install it from your distro if missing.
- If you don’t see notifications, check that your desktop has a notification service running and that the app isn’t muted or suppressed by Do Not Disturb.