## Dependancies

### mORMot2
Trndi depends on mORMot2 for the JavaScript engine, it does not use anything else from the library at this time. It can be installed via the __Online Package Manager__ in Lazarus, or [GitHub](https://github.com/synopse/mORMot2).

### JavaScript (if built with Extensions on)
> Ignore this part if building with the "No Ext" mode

The JS engine, __QuickJS__, is _linked_ into Trndi statically.

To compile Trndi with extensions support you need the relevant QuickJS library files. 

* They are expected to be in in in ```../static``` when compiling from GitHub. 
* When using the _Online Package Manager_ in Lazarus, they can be placed in  ```~/.lazarus/onlinepackagemanager/packages/mORMot2/static```, or on Windows: ```C:\Users\<you>\AppData\Local\Lazarus\onlinepackagemanager\packages\mORMot2\static```


You can get the recommended libraries from https://synopse.info/files/mormot2static.7z. You only need to keep the QuickJS files. You can check the linker's output to see which files that is.

### Building without extensions
When building without extensions, you need to install the mORMot2 library - or __remove it__ from the _Project Inspector_. Trndi will build fine without changing the project, if mORMot2 is available.

### Qt6
You need __libqt6pas__, and it's development packages. These are normally available with your distro. See the _Linux section in [README.md](/README.md)_ on how to install libqt6pas.

## Code Style
### Linter
Trndi uses __JEDI__ to format code, in Lazarus: Source > JEDI Code Format > Global Settings. Choose _JCFSettings.xml_ from this repo.

### Naming
Units should end with ```.pp```, 

### VSCode
Should you choose not to use Lazarus IDE, these are the recommendations for Visual Studio Code:
* Language Support: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal

* Formatting: Name: Pascal Formatter
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal-formatter

* Debugging: https://marketplace.visualstudio.com/items?itemName=CNOC.fpdebug
* Debugging with GDB: https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug