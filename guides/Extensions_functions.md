# Trndi Extensions API
Trndi supports ES2023, and provides these functions in addition to it:

## Contents

 - [Trndi functions](#trndi-functions)
   - [alert](#alert)
   - [confirm](#confirm)
   - [prompt](#prompt)
   - [select](#select)
   - [log](#log)
   - [console.log](#consolelog)
   - [htmlMsg](#htmlmsg)
   - [htmlDlg](#htmldlg)   
   - [htmlYesNo](#htmlyesno)      
   - [setBadgeSize](#setbadgesize)
   - [setDotSize](#setdotsize)
   - [setDotAdjust](#setdotadjust)
   - [getUnit](#getunit)
   - [getLocale](#getlocale)
   - [setLevelColor](#setlevelcolor)
   - [setTimeAndRange](#settimeandrange)
   - [playSound](#playsound)
   - [sayText](#saytext)
   - [setOverrideThresholdMinutes](#setoverridethresholdminutes)
   - [setClockInterval](#setclockinterval)
   - [predictReadings](#predictreadings)
 - [Promises (global)](#promises-global)
   - [asyncGet](#asyncget)
   - [jsonGet](#jsonget)
   - [runCMD](#runcmd)
   - [setLimits](#setlimits)
 - [Callbacks](#callbacks)
   - [updateCallback](#updatecallback)
   - [fetchCallback](#fetchcallback)
   - [dotClicked](#dotclicked)
   - [uxClick](#uxclick)
   - [clockView](#clockview)
 - [User info](#user-info)
   - [getCurrentUser](#getcurrentuser)
   - [getCurrentNickname](#getcurrentnickname)
 - [Examples](#examples)


## Trndi functions
These functions are available via `Trndi.*`, such as `Trndi.alert(...)`:
### alert
#### Show an alert
```javascript
Trndi.alert("hello")
```
Result: none
### confirm
#### Asks yes or no
```javascript
Trndi.confirm("Yes or no?")
```
Result: `true` or `false`
### prompt
#### Shows an input field
```javascript
Trndi.prompt("Enter a value", "We're asking for a value", "default value")
```
Result: string
### select
#### Shows a list of options
```javascript
Trndi.select("Select your choice", "Choose one of these", ...listOfStringOptions);
```
Result: integer (zero-based index)
### log
_See `console.log`_
### console.log
```javascript
console.log(something);
```
Prints out data to the user
### htmlMsg
```javascript
Trndi.htmlMsg('Window Title', 'Title', 'Description', '<b>HTML</b> box content', 1)
```
Shows a HTML content box, last parameter is the scale of the window (it can be scrolled, but you might want to scale it too)
### htmlDlg
```javascript
Trndi.htmlDlg('Window Title', '<b>HTML</b> box content', 1)
```
Shows a full HTML dialog with all content HTML. Last parameter is scale.
### htmlYesNo
```javascript
const res = Trndi.htmlYesNo('Window Title', '<b>Load config?</b><br>Load configuration again?', 1)
console.log("Response was yes?" + (res ? 'yes' : 'no'))
```
Shows a full HTML dialog with all content HTML. User can click yes or no
### setBadgeSize
```javascript
Trndi.setBadgeSize(0.9, 9);
```
Sets size of the reading overlay for Windows
Above: Set size to 90% of the Windows app icon with font 9.
### setDotSize
```javascript
Trndi.setDotSize(2);
```
Sets scale of the trend dots. 2 = 2x etc.
### setDotAdjust
```javascript
Trndi.setDotAdjust(0.1);
```
Multiplier on where the dots are drawn on screen up/down. Minus = up, plus = down
### getUnit
#### Get the current measure unit
```javascript
Trndi.getUnit()
-> "mmol/L"
```
Returns the current unit: `"mg/dL"` or `"mmol/L"`.

#### Get the current reading
```javascript
Trndi.getReading(true)
-> 5.5
```
Returns the current reading, pass ```true``` for mmol/L or ```false``` for mg/dL. Returns ```false``` when no reading is available.

### getLocale
#### Get the current app language
```javascript
Trndi.getLocale()
-> "sv"
```
Returns a language code (such as `sv` or `en`).

### getBuild
Returns Trndi's build number
```javascript
Trndi.getBuild()
-> "200"
```

### getCurrentAPI
Returns the active glucose source
```javascript
Trndi.getCurrentAPI()
-> "NightScout v3"
```

### Get a settings value
Gets a settings file/registry value
```javascript
Trndi.getSetting("font.arrow") // String
-> "Segoe UI"
Trndi.getSetting("webserver.enable") // Boolean
-> "true"
Trndi.getSetting("non.exsting.key") // Not found
-> false
```

### Set a settings value
Sets a settings file/registry value
```javascript
Trndi.setSetting("extval.myext.property", "true")
```
Stores a settings value, you can store anything under ```extval```.*, modifying other keys will cause Trndi to ask the user's approval before saving!

### setLevelColor
#### Sets the UX colors
```javascript
Trndi.setLevelColor('#7cd55d','#d55d5d', '#5dc6d5',// Readings (ok, hi, lo))
              '#7cd55d','#612828', '#5d75d5', // Colors for the dots (ok, hi, lo)
              '#ffbfbf', '#bffff9'); // Color for the custom levels set in NightScout (or via JS) (hi, lo)
``` 
### setTimeAndRange
#### Sets the max minutes to fetch and max readings to fetch (subject to which metric the API uses)
```javascript
Trndi.setTimeAndRange(20, 4); // Sets the max time to fetch to 20 minutes and the range to 4 readings
```
### playSound
#### Plays an audio file
```javascript
Trndi.playSound('C:\\file.wav')
```
### sayText
#### Reads a text aloud
```javascript
Trndi.sayText('High sugar!')
```

### attention
Displays a system notification
```javascript
Trndi.attention("Hello there!")
```

<a name="overridemins"></a>
### setOverrideThresholdMinutes
#### Sets number of minutes before a reading is considered old
> ⚠️ WARNING ⚠️ Allowing a reading to be considered recent for many minutes can be dangerous!
```javascript
Trndi.setOverrideThresholdMinutes(15) // Shows the "No fresh readings" box after 16 minutes. Cannot be set below 6.
```

### setClockInterval
Sets the interval when the clock is shown (if enabled), and for how long.
```javascript
Trndi.setClockInterval(100000,10000); // Show clock every 100 sec and for 10 sec. NOTE the values cannot be the same or the clock will always show
```

### predictReadings
#### Predict future blood glucose readings
Predicts future blood glucose values based on recent trends using linear regression.

```javascript
// Predict the next 3 readings (default)
const predictions = Trndi.predictReadings();

// Predict the next 5 readings
const predictions = Trndi.predictReadings(5);
```

**Returns:** Array of predictions, where each prediction is an array:
- `[0]`: Predicted value in current unit (mg/dL or mmol/L)
- `[1]`: Predicted value in mg/dL
- `[2]`: Predicted value in mmol/L  
- `[3]`: Predicted timestamp (TDateTime)

**Example:**
```javascript
function updateCallback(reading_system, reading_mgdl, reading_mmol, time) {
  const predictions = Trndi.predictReadings(3);
  
  if (predictions.length > 0) {
    console.log("Current:", reading_mmol, "mmol/L");
    predictions.forEach((pred, idx) => {
      console.log(`Prediction ${idx + 1}:`, pred[2].toFixed(1), "mmol/L");
    });
    
    // Alert if predicted to go low within next 3 readings
    const willGoBelowFour = predictions.some(pred => pred[2] < 4.0);
    if (willGoBelowFour) {
      Trndi.alert("Warning: Glucose predicted to drop below 4.0 mmol/L soon!");
    }
  }
}
```

**Notes:**
- Requires at least 3 recent readings for prediction
- Returns empty array if insufficient data
- Predictions are based on linear trend and don't account for meals, insulin, or other factors
- Accuracy decreases for predictions further into the future
- Maximum 20 predictions can be requested

## Promises (global)
These are global promises, not prefixed with `Trndi.`:
### asyncGet 
#### Fetches a URL
> Note that theres a size limit of the response, see jsonGet for complex JSONs
```javascript
asyncGet("https://sample-files.com/downloads/documents/txt/simple.txt")
  .then(result => console.log(result))
  .catch(error => console.log(`Error: ${error}`));
  ``` 
### jsonGet 
#### Fetches a URL and extracts a JSON path
```javascript
asyncGet("https://some-json", "item.subitem")
  .then(result => console.log(result))
  .catch(error => console.log(`Error: ${error}`));
  ``` 
### runCMD 
#### Runs a program locally
```javascript
runCMD('appname.exe')
runCMD('appname.exe', 'parameter', 'sign for parameter separation')
```
ex:
```javascript
runCMD("explorer.exe")
  .then(result => result ? 'Success' : 'Process returned false')
  .catch(error => console.log(`Error!`));
```
### setLimits - 
#### Set the limits for high/low, and ranges
```javascript
setLimits(low, high, lowRange, highRange)
```
> Note: parameters 3 and 4 are optional!
_Use floats for mmol/L and integers for mg/dL!_
```javascript
  setLimits(3.2, 16.4, 4.1, 12.7).then(() =>Trndi.alert("Custom limits set"));
```

## Callbacks
> _NOTE:_ Simply add a function, named the same as a callback below, to have it triggered
### updateCallback
#### This function is called when the main loop updates the reading
```updateCallback(reading_system, reading_mgdl, reading_mmol, time)```

### fetchCallback
#### This function is called everytime a reading is fetched
```fetchCallback(reading_mgdl, reading_mmol, delta_mgdl, delta_mmol, has_data)```

### dotClicked
#### This function is called everytime a trend dot is clicked
```dotClicked(open, mgdl, mmol, time)```
```javascript
function dotClicked(open, mgdl, mmol, time){console.log("Is the dot now showing the value? " + open, "Reading " + mmol, "Time: " + time )} // if the dot is "open" its showing the reading, not the dot icon
```

### uxClick
#### This function is called when the user triggers a UX message
```uxClick(element, value)```
```javascript
function uxClick(element, value, ...values){
  console.log('clicked', element, 'value of dialog box', value)
  return true; // false suppresses Trndi's own dialog
  // element =
  // "tir" - Time in range was clicked, value = time span, value2 = percent
  // "no-reading" - The "No reading" popup is clicked, no args
  // "range" - Over/Under range bar is clicked, value = true if the reading is high
}
```

### clockView
#### Replace the clock when "Show clock every 20 seconds" is on
```clockView(glucose_mgdl, glucose_mmol, time)```
```javascript
function clockView(glucose, time){
  const user = Trndi.getCurrentUser(); // Returns the username, if running multiple accounts
  if (user) return user;
  return "Hello"; // Shows Hello instead of the clock every 20 seconds
}
```

## User info
### getCurrentUser
#### Returns the username of the current user (or `false` when not in multi-user mode)
```javascript
const user = Trndi.getCurrentUser();
if (user === false)
  console.log("Not multi user!");
else if (user === '')
  console.log("Default user");
else
  console.log(`Logged in as ${user}`);
```

### getCurrentNickname
#### Returns the nickname for the current user (or `false` if not in multi-user mode)
```javascript
const nick = Trndi.getCurrentNickname();
// See getCurrentUser for usage pattern
```

## Examples
```javascript
// Localized greeting example using getLocale
const lang = Trndi.getLocale();
const strs = {
  sv: { Hey: "Hej och välkommen" },
  en: { Hey: "Hello and welcome" },
};
Trndi.alert(strs[lang]?.Hey || "Hello and welcome!");
```