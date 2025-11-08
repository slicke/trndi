# Getting started with Plugins
Full reference of functions in [Extensions Functions](Extensions_functions.md)

### Plugin folder
The _plugin folder_ is located in your user's application data folder. You can find out where exactly in Trndi's settings, see below.

![Window](../doc/img/ext.png)

### Plugin support
In some cases Trndi can be built without plugin support; this will be shown in the settings also. Official versions of Trndi support extensions on Windows and Linux (amd64/x64). Due to limitations in the engine Trndi uses, extensions are not supported on arm platforms such as RaspberryPi and macOS.

![Window](../doc/img/no_ext.png)

# Creating/Installing a plugin
To create, or install, a plugin - create/move a ```.js``` file in the plugin folder. It will automatically load on the next start of Trndi.
> If you are using multiple users in Trndi, plugins will run independently of each other on each instance of Trndi.

### Custom levels (Aka the "Dexcom example")
This example demonstrates how to set custom high, low and a custom range via a plugin.
> A custom range is a range within the limits of high/low, where the blood sugar is considered optimal.

While this plugin can be used for any backend, it's especially useful for _Dexcom_ as the backend does _not provide this info_:

```javascript
setLimits(3.2, 10.4);
```
In this example, we declare that:
* Readings under 3.2 are low
* Readings over 10.4 are high
> These settings will override the settings in Trndi's settings window.

We can also set the custom range, by adding more parameters:
```javascript
setLimits(3.2, 10.4, 4.1, 8.7);
```
This adds:
* We prefer values over 4.1
* We prefer values under 8.7

Save his line in a file ending with ```.js```, such as ```ranges.js``` in the plugin folder.
### Color example
We can also change the high/low colors displayed:
```javascript
Trndi.setLevelColor('#7cd55d','#d55d5d', '#5dc6d5'); // Set colors for okay readings, high readings and low readings with HTML colors
```
> This will override any colors chosen in Trndi's settings!

You can also set colors for the dots in the graph, by adding three more colors:
```javascript
Trndi.setLevelColor('#7cd55d','#d55d5d', '#5dc6d5', // Set colors for okay readings, high readings and low readings with HTML colors
                    '#7cd55d','#612828', '#5d75d5'); // Colors for the dots (ok, hi, lo)
```

Lastly, you can set the warning colors for when the blood sugar exceeds or goes under the "preferred" level:
```javascript
Trndi.setLevelColor('#7cd55d','#d55d5d', '#5dc6d5', // Set colors for okay readings, high readings and low readings with HTML colors
                    '#7cd55d','#612828', '#5d75d5', // Colors for the dots (ok, hi, lo)
                    '#ffbfbf', '#bffff9'); // Color for the custom levels set in NightScout (or via JS) (hi, lo)
```

### See the full reference of functions in [Extensions Functions](Extensions_functions.md)

# Info and Copyright
To have your extensions show their name and copyright, add a header at the very start:
```
/* My Extension
(c) My Name*/
```