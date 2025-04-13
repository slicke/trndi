# Notifications
Trndi can show toasts (those small pop-ups near the clock) when you're high or low. On Windows, Trndi uses the BurntToast library and on Linux Notify is used.

Enabling notifications is __optional__, and you don't need to!

## Windows
Trndi uses [BurntToast](https://www.powershellgallery.com/packages/BurntToast/0.8.5) to show notifications.

In PowerShell, run:
```Install-Module -Name BurntToast``` to install BurntToast. Note that this should be done in your _system_ and not in any open-source version you might have installed extra.

## Linux
Trndi uses [Notify-send](https://ss64.com/bash/notify-send.html) to display notifications.

Notify-send might already be included in your system, if not you can install it via your package manager.