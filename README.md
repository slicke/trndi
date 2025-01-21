![Trndi](/trndi-logo.png)
![Window](/doc/ux.png)
![Window](/doc/js.png)

# _<small>Nightscout / Dexcom / Local </small>_ CGM data viewer
Trndi is an app that shows the current blood sugar from _Night Scout_ or _Dexcom Share_.
If you use _xDrip_ and need a non-internet source, Trndi supports that too.

# What differs Trndi from apps?
### Fully native
* Trndi runs natively on your computer, without needing any dependencies on __Windows__ and __macOS__. (__Linux__ might need packages OpenSSL to be installed). BSD and othre systems too.
### Devices
* Trndi runs on your __Raspberry Pi__ and other arm computers
* Trndi works on touch-screen devices
* * Trndi runs on most processors (x86, x64, Apple Sillicon, ARM, _Unoficcialy: Sparc, PowerPC, 
### Extensions
* Trndi supports modern ___JavaScript__ extensions_, built-in on most platforms.
### Multi-user support
* Got more than one diabetic? Trndi supports parallel users. 


## Note on parallel users
 Trndi has no GUI at this time to add users.
 * To add users, open Trndis config file and add
```
[users]
names=one,two
```
* On Windows, open the registry and find the ```HKEY_CURRENT_USER\Software\Trndi``` key. There, add a string value called ```users.names```.

The value has to be comma separated. Start multiple Trndis and select which user to use during start-up.

# Setup
Hold the reading (or "Setup" text) and choose settings to access settings.

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
The JS engine, _QuickJS_, is linked into Trndi.

You need the relevant library files, placed in ../static.
> You can get the recommended libraries from https://synopse.info/files/mormot2static.7z - you may remove libraries you don't need!
Due to mormot2, the support library Trndi uses, it may currently not be possible to run extensions under ARM64 computers.

## Style
### Linter
Trndi uses __JEDI__ to format code, the binary is located in the FPC bin folder.

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
Trndi is actively tested on Windows (amd64), Fedora Linux(amd64), Debian Linux(arm64 / Raspberry pi).