# Getting started with Plugins
Full reference of functions in [Extensions Functions](Extensions_functions.md)

### Plugin folder
Plugins are placed in the _plugin folder_. You can find out where it is in Trndi's settings:

![Window](../doc/img/ext.png)

### Plugin support
Trndi can be built without plugin support aswell; if so, the box is grayed-out:

> Official versions of Trndi support extensions on Windows and Linux (__amd64/x64__). Due to limitations in the engine Trndi uses, extensions are not supported on __arm__(64) platforms such as RaspberryPi and macOS.

![Window](../doc/img/no_ext.png)

# Creating/Installing a plugin
To create, or install, a plugin - create/move a ```.js``` file in/to the plugin folder. It will automatically load on the next start of Trndi.
> If you are using multiple users in Trndi, plugins will run independently, "sandboxed", per user.

# Writing an extension
 See the full reference of functions in [Extensions Functions](Extensions_functions.md)

# Info and Copyright
To have your extensions show their name and copyright, add a header at the very start:
```
/* My Extension
(c) My Name*/
```
If no /* is present, the plugin will not show any info. If your first row is another comment it might be shown as copyright.