# Trndi Extensions API
Trndi supports ES2023, and provides these functions in addition to it:

# Trndi functions
These functions are avilable via Trndi., such as Trndi.alert:
### alert
#### Show an alert
```javascript
Trndi.alert("hello")
```
Result: none
### confirm
#### Asks yes or no
```javascript
Trndi.alert("Yes or no?")
```
Result: ```true``` or ```false```
### prompt
#### Shows an input field
```javascript
Trndi.prompt("Enter a value", "We're asking for a value", "default value")
```
Result: string
### select
#### Shows a list of options
```javascript
Trndi.select("Select your choise", "Choose one of these", ...list of string options);
```
Result: integer
### Trndi.log
_See console.log_
### console.log
Prints out data to the user
### setBadgeSize
```javascript
Trndi.setBadgeSize(0.9, 9);
```
Sets size of the reading overlay for Windows
Above: Set size to 90% of the Windows app icon with font 9.
### getUnit
#### Get the current measure unit
```javascript
Trndi.getUnit
```
Returns if the user is using mg/dL or mmol/L
### getLocale
#### Get the current app language
```javascript
Trndi.getLocale
```
Returns a lang code (such as sv or en)
### setLevelColor
#### Sets the UX colors
```javascript
Trndi.setLevelColor('#7cd55d','#d55d5d', '#5dc6d5',// Readings (ok, hi, lo))
              '#7cd55d','#612828', '#5d75d5', // Colors for the dots (ok, hi, lo)
              '#ffbfbf', '#bffff9'); // Color for the custom levels set in NightScout (or via JS) (hi, lo)
``` 


# Promises
These are promises, not prefixed with Trndi.:
### asyncGet 
#### Fetches a URL
```javascript
asyncGet("https://sample-files.com/downloads/documents/txt/simple.txt")
  .then(result => console.log(result))
  .catch(error => console.log(`Error: ${error}`));
  ``` 
### runCMD 
#### Runs a program locally
```runCMD('appname.exe') ``` 
```runCMD('appname.exe', 'parameter', 'sign for parameter separation') ``` 
ex:
```javascript
runCMD("explorer.exe")
  .then(result => result ? 'Success' : 'Process returned false')
  .catch(error => console.log(`Error!`));
```
### setLimits - 
#### Set the limits for high/low, and ranges
```setLimits(low, high, low-range, high-range)```
> Note: parameters 3 and 4 are optional!
_Use floats for mmol/L and integers for mg/dL!_
```javascript
  setLimits(3.2, 16.4, 4.1, 12.7).then(() =>Trndi.alert("Custom limits set"));
```