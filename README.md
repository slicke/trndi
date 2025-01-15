![Trndi](/trndi-logo.png)
![Window](/doc/ux.png)
![Window](/doc/js.png)

# _<small>Nightscout / Dexcom / Local </small>_ CGM data viewer
Trndi is an app that shows the current blood sugar from _Night Scout_ or _Dexcom Share_.
If you use _xDrip_ and need a non-internet source, Trndi supports that too.

# What differs Trndi from apps?
* Trndi runs natively on your computer, without needing any dependencies on __Windows__ and __macOS*__. __Depending on distro, __Linux__ might need packages to be installed.
> _* Pre-built macOS binaries are not provided right now, as I lack a Mac_.
* Trndi runs on your __Raspberry Pi__
* Trndi works on touch-screen devices
* Trndi runs on most processors
  * Intel/AMD 32 and 64 bit
  * Apple Sillicon
  * ARM
  * _Unoficcialy:_
    * _Sparc_
    * _PowerPC_
* Trndi supports, among others:
    * Windows
    * macOS
    * GNU/Linux
    * FreeBSD
    * _Unoficcialy:_
      * _Windows embedded_
* Trndi supports modern ___JavaScript__ in it's extensions_, via the _QuickJS_ runtime.

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

## Style
### Linter
Trndi uses __ptop__ to format code, the binary is located in the FPC bin folder.

In lazarus, calling ptp is formatted as:
```-c $ProjPath()\style.ptop.cfg $EdFile() $EdFile()```

From command line, ran in the project folder:
```ptop -c style.cfg file.pp file.pp```

### Naming
Units should end with ```.pp```, 

### VSCode
Should you choose not to use Lazarus IDE, these are the recommendations for Visual Studio Code:
* Language Support: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal

* Formatting: Name: Pascal Formatter
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal-formatter
> * Indent: 2
> * Wrap: 80
> * Engine: ptop
> * Engine parameters: *project*\style.ptop.cfg
> * Engine path: *path*\fpc\3.2.2\bin\x86_64-win64\ptop-exe

* Debugging: https://marketplace.visualstudio.com/items?itemName=CNOC.fpdebug

### History
Trndi2 is a strip-down of Trndi 1, which was never released publically. Trndi 1 was originally called Dexmon (and only did Dexcom). The original idea spawns from an old app called TrayTrend which I made with a similar purpose.