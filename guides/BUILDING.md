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

### IMPORTANT: RaspberryPi / ARM64
To compile with extension support, you have to add ```{$DEFINE LIBQUICKJSSTATIC}``` in ```mormot.defines.inc```. However extensions support in Linux is very experimental and might not work. Support is dependent on the mORMot2 project, which headers we use to connect to QuickJS.

### VSCode
Should you choose not to use Lazarus IDE, these are the recommendations for Visual Studio Code:
* Language Support: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal

* Formatting: Name: Pascal Formatter
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal-formatter

* Debugging: https://marketplace.visualstudio.com/items?itemName=CNOC.fpdebug
