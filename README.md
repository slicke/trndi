> ⚠️ _Quick intro:_ __Right-click__ on Trndiäs window to open it's menu for settings and other options!

[![Build & Release (All Platforms)](https://github.com/slicke/trndi/actions/workflows/build.yml/badge.svg?branch=main&event=push)](https://github.com/slicke/trndi/actions/workflows/build.yml)

![Trndi](doc/img/trndi-logo.png)

# Trndi - CGM data on Desktop and RaspberryPi<br><sup>_Nightscout - Dexcom - xDrip WiFi_</sup> 
## <b> 🪟 Windows - 🍎 macOS - 🐧 Linux - 🥧 RaspberryPi/ARM Linux </b>

![Windows](doc/img/img_win.png)
<br>Windows 11

![Linux](doc/img/img_lin.png)
<br>Fedora Linux

![Linux](doc/img/img_rpi.png)
<br>RaspberryPi (with touch screen)

### Introduction
Trndi is a _desktop app_ that shows the your blood sugar and graph. It works with  _Night Scout_ and _Dexcom Share_ at the moment.
It also supports the _xDrip_ app, connecting over the local network/WiFi.

# What differs Trndi from apps?
* __Natively__ runs on your computer without needing installation
* Runs out-of-the-box! Does not depend on other software to be installed _(on [Linux](#Linux-support), this may depend on your distro)_
* Supports __multiple languages__
* Runs on your __Raspberry Pi__ and other arm computers, optinally as a full-screen display
* Works on __touch-screen__ devices, such as a RaspberryPi with screen
* Supports modern ___JavaScript__ extensions_
* __Small and portable__
* Supports dark and light color modes
* Displays your readings in a __trend graph__
* __Free__ and open source
* Integrates with Windows, macOS and Linux special features such as the macOS dock, Windows taskbar and Linux notifications.
* Supports low, normal and high glood sugar colors. But also __custom ranges__ such as "normal, but on the edge to high"

### Multi-user support
Need to see more than one person? Trndi supports [parallel users](guides/Multiuser.md).
Setup the users in the _settings_. Don't forget to click _Save_ when editing users!

You need to start Trndi multiple times, each instance lets you choose a user. Just open the app multiple times and select a different account. Users can have different sources and runs JavaScript plugins independently.

# Usage
⚠️ Right-click on the reading on the screen to access the menu, this is how you control Trndi.

# Setup
Click/hold the reading (or "Setup" text) and choose settings to access settings.
* For NightScout, settings will be fetched from your server and auto-applied
* For Dexcom, see the __[Dexcom setup guide](guides/Dexcom.md)__. The backend does not support all features, but this can be fixed with some manual work.
* For xDrip, you need to turn on the local web server and use that IP/password
* For other backends, feel free to contribute a api driver. See [API Drivers](guides/API.md)

> _NOTE_ To receive notifications see [here](guides/Notifications.md)

> See a box instead of an icon? On Debian systems, you may need to install a graphic font. I recommend fonts-noto-color-emoji.

# Development
Development tools can be obtained via most distros package managers:
- Fedora/RHEL: ```dnf install lazarus```
- Ubuntu/Debian: ```apt install lazarus```
- FreeBSD: ```pkg install editors/lazarus```

## Building
### GUI
Use the Laarus IDE to build and/or develop the app, set release target in the _Project Options_.

### Command line

Build development:
```lazbuild Trndi.lpi``` 

Build release:
```lazbuild -dRelease Trndi.lpi``` 

Build to a release folder
```lazbuild -B output_directory Trndi.lpi``` 


## Dependancies
### JS
The JS engine, _QuickJS_, is linked into Trndi.

To compile Trndi with extensions support you need the relevant library files, placed in ../static when compiling.
> You can get the recommended libraries from https://synopse.info/files/mormot2static.7z - you may remove libraries you don't need!
Due to mormot2, the support library Trndi uses, it may currently not be possible to run extensions under ARM64 computers.

### Qt6 on Raspberry Pi
You need the libqt6pas and it's development packages. These are normally available with your distro. See the Linux section on how to install libqt6pas.

## Style
### Linter
Trndi uses __JEDI__ to format code, in Lazarus: Source > JEDI Code Format > Global Settings. Choose _JCFSettings.xml_

### Naming
Units should end with ```.pp```, 

### VSCode
Should you choose not to use Lazarus IDE, these are the recommendations for Visual Studio Code:
* Language Support: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal

* Formatting: Name: Pascal Formatter
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal-formatter

* Debugging: https://marketplace.visualstudio.com/items?itemName=CNOC.fpdebug

### History
Trndi2 is a rewrite, less bloated, version of Trndi 1, which was never released publically. Trndi 1 was originally called Dexmon (and only did Dexcom). The original idea spawns from an old app called TrayTrend which I made with a similar purpose.


### Testing
Trndi is actively tested on Windows (x64), Fedora Linux(amd64), Debian Linux(arm64 / Raspberry pi).

### IMPORTANT: RaspberryPi / ARM64
To compile with extension support, you have to add ```{$DEFINE LIBQUICKJSSTATIC}``` in ```mormot.defines.inc```. However extensions support in Linux is very experimental.

<a name="Linux-support"></a>
## Linux notes
If you find yourself having problems running the Qt6 version of Trndi, you might be missing the Qt6 framwork and/or the pascal headers.

#### Any distro
You can download them for any RPM/DEB distro in this [repo](https://github.com/davidbannon/libqt6pas/releases).

#### Debian
You can install the ```libqt6pas``` packade on Debian-based distros via ```apt```.

#### Fedora
You can install the ```qt6pas``` package in ```DNF```.

#### Arch
You can install the ```qt6pas``` package.

#### Others
Look for ```qt6pas``` or ```libqt6-pas``` in your package manager, or search for ```libQt6Pas.so```.

## Settings storage
Trndi stores settings per platform in the standard location:
- Windows: Registry under HKCU\Software\Trndi
- macOS: User defaults (Preferences) for the app
- Linux: Lazarus GetAppConfigFile path, typically ~/.config/Trndi/trndi.ini in section [trndi]

On Linux, this is a single INI file consistently used for Get/Set/Delete operations — no legacy paths or multiple files.

## Contributing
Contributions are welcome. Please read [CONTRIBUTING](CONTRIBUTING.md) for coding style, PasDoc guidelines, and how to generate the developer docs for native, extensions, and API units.

If you're adding or modifying an API driver, start with the [API guide](guides/API.md) for the contract and examples, then follow the API section in CONTRIBUTING.