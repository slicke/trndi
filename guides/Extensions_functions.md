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
   - [console.push](#consolepush)
   - [console.logs](#consolelogs)
   - [htmlMsg](#htmlmsg)
   - [htmlDlg](#htmldlg)   
   - [htmlYesNo](#htmlyesno)      
   - [setBadgeSize](#setbadgesize)
   - [setDotSize](#setdotsize)
   - [setDotAdjust](#setdotadjust)
   - [getUnit](#getunit)
   - [getLocale](#getlocale)
   - [getCurrentReading](#getcurrentreading)
   - [getLimits](#getlimits)
   - [getStatistics](#getstatistics)
   - [setLevelColor](#setlevelcolor)
   - [setTimeAndRange](#settimeandrange)
   - [playSound](#playsound)
   - [sayText](#saytext)
   - [setOverrideThresholdMinutes](#setoverridethresholdminutes)
   - [setClockInterval](#setclockinterval)
   - [predictReadings](#predictreadings)
   - [getBasalRate](#getbasalrate)
   - [setTimeout](#settimeout)
   - [setInterval](#setinterval)
   - [clearTimeout](#cleartimeout)
   - [clearInterval](#clearinterval)
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
### console.push
```javascript
console.push("message 1");
console.push("message 2");
```
Accumulates messages in an internal buffer without showing a popup. Use this when you want to collect multiple log messages and display them all at once with `console.logs()`.

**Parameters:**
- Message(s) to add to the buffer (same as `console.log`)

**Returns:** none

**Example:**
```javascript
console.push("Starting process...");
console.push("Step 1 complete");
console.push("Step 2 complete");
console.logs();  // Shows all 3 messages in one popup
```

### console.logs
```javascript
console.logs();
```
Displays all buffered messages (accumulated via `console.push()`) in a single popup, then clears the buffer. If no messages are buffered, displays "(no messages buffered)".

**Parameters:** none

**Returns:** none

**Use case:** Avoids multiple popups when logging multiple messages during extension execution.

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

### getCurrentReading
#### Get comprehensive current reading with metadata
Returns a complete object with the current glucose reading and all associated metadata.

```javascript
const reading = Trndi.getCurrentReading();
if (reading === false) {
  console.log("No readings available");
} else {
  console.log(`Value: ${reading.value_mmol} mmol/L`);
  console.log(`Delta: ${reading.delta_mmol}`);
  console.log(`Direction: ${reading.direction}`);
  console.log(`Age: ${reading.age_seconds} seconds`);
}
```

**Returns:** Object with properties or `false` if no reading available:
- `value_system`: Value in current unit (mg/dL or mmol/L based on user setting)
- `value_mgdl`: Value in mg/dL (integer)
- `value_mmol`: Value in mmol/L (float)
- `delta_mgdl`: Change since last reading in mg/dL (integer)
- `delta_mmol`: Change since last reading in mmol/L (float)
- `direction`: Trend arrow (↑, ↗, →, ↘, ↓)
- `timestamp`: TDateTime timestamp
- `age_seconds`: Age of reading in seconds

**Example:**
```javascript
function updateCallback() {
  const reading = Trndi.getCurrentReading();
  if (reading !== false) {
    if (reading.age_seconds > 300) {
      Trndi.alert("Reading is over 5 minutes old!");
    }
    if (reading.direction === "↓↓" && reading.value_mmol < 5.0) {
      Trndi.alert("Rapid drop detected!");
    }
  }
}
```

### getLimits
#### Get current CGM threshold limits
Returns the configured high/low limits and target ranges.

```javascript
const limits = Trndi.getLimits();
console.log(`High limit: ${limits.high_mmol} mmol/L`);
console.log(`Low limit: ${limits.low_mmol} mmol/L`);
```

**Returns:** Object with limit properties:
- `low_mgdl`: Low threshold in mg/dL
- `high_mgdl`: High threshold in mg/dL
- `low_mmol`: Low threshold in mmol/L
- `high_mmol`: High threshold in mmol/L
- `low_range_mgdl`: Target range lower bound in mg/dL (0 if not supported)
- `high_range_mgdl`: Target range upper bound in mg/dL (500 if not supported)
- `low_range_mmol`: Target range lower bound in mmol/L
- `high_range_mmol`: Target range upper bound in mmol/L

**Example:**
```javascript
const limits = Trndi.getLimits();
const reading = Trndi.getReading(true); // mmol/L

if (reading > limits.high_mmol) {
  console.log("Above high limit!");
} else if (reading < limits.low_mmol) {
  console.log("Below low limit!");
} else if (reading >= limits.low_range_mmol && reading <= limits.high_range_mmol) {
  console.log("In target range!");
}
```

### getStatistics
#### Get statistical analysis of recent readings
Calculate comprehensive statistics from readings within a specified time period.

```javascript
// Get stats for last 24 hours (default)
const stats = Trndi.getStatistics();

// Get stats for last 3 hours
const stats3h = Trndi.getStatistics(180);
```

**Parameters:**
- `minutes` (optional): Number of minutes to analyze (default: 1440 = 24 hours)

**Returns:** Object with statistical properties:
- `mean`: Average glucose value in current unit
- `median`: Median glucose value in current unit
- `stdDev`: Standard deviation
- `cv`: Coefficient of variation (CV%) - lower is better, <36% is good
- `timeInRange`: Percentage of readings within target range
- `timeAbove`: Percentage of readings above high limit
- `timeBelow`: Percentage of readings below low limit
- `readingCount`: Number of readings analyzed

**Example:**
```javascript
function updateCallback() {
  const stats = Trndi.getStatistics(1440); // Last 24 hours
  
  if (stats.readingCount < 10) {
    console.log("Not enough data for statistics");
    return;
  }
  
  console.log(`24h Average: ${stats.mean.toFixed(1)} mmol/L`);
  console.log(`Standard Deviation: ${stats.stdDev.toFixed(1)}`);
  console.log(`CV: ${stats.cv.toFixed(1)}%`);
  console.log(`Time in Range: ${stats.timeInRange.toFixed(0)}%`);
  
  if (stats.cv > 36) {
    Trndi.alert("High glucose variability detected (CV > 36%)");
  }
  
  if (stats.timeInRange < 70) {
    console.log("Time in range below target (70%)");
  }
}
```

**Notes:**
- Statistics are calculated in the user's current unit setting
- Requires at least one reading in the specified time period
- Time percentages are based on reading count, not actual time elapsed
- CV (Coefficient of Variation) is a key metric: <36% indicates stable glucose control

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

### getBasalRate
#### Get current basal rate from the CGM backend
Retrieves the current basal insulin rate from the backend server (e.g., Nightscout).

```javascript
const basal = Trndi.getBasalRate();
if (basal === false) {
  console.log("Basal rate not available");
} else {
  console.log(`Current basal rate: ${basal} U/hr`);
}
```

**Returns:** 
- Float value representing current basal rate in U/hr (units per hour)
- `false` if basal rate is unavailable or not supported by the backend

**Backend Support:**
- **Nightscout/Nightscout v3**: Fetches from profile.json endpoint ✅
- **Other backends**: Returns `false` (not implemented)

**Example - Display basal with insulin on board:**
```javascript
const basal = Trndi.getBasalRate();
if (basal !== false) {
  Trndi.alert(`Current basal: ${basal.toFixed(2)} U/hr`);
}
```

**Note:** The function returns the basal rate defined in the active profile at the current time. For pump users with temp basals, this shows the scheduled rate, not the active temporary rate.

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

### setTimeout
#### Schedule a function to run once after a delay
Executes a function after a specified delay in milliseconds.

```javascript
// Define a named function (required - anonymous functions are not supported)
function showAlert() {
  Trndi.alert("5 seconds have passed!");
}

// Schedule it to run after 5 seconds
const timerId = setTimeout(showAlert, 5000);

// Another example
function LogMessageToFile() {
  console.log("Timer executed!");
}
setTimeout(logMessage, 3000);
```

**Parameters:**
- `callback`: **Named function** to execute (anonymous/arrow functions are not supported)
- `delay`: Time in milliseconds to wait before execution

**Returns:** Timer ID (BigInt) that can be used with `clearTimeout()`

**Notes:**
- **Important:** Only named functions are supported. Arrow functions and anonymous functions will be rejected
- Pass the function name without parentheses: `setTimeout(myFunction, 1000)` not `setTimeout(myFunction(), 1000)`
- The timer is automatically cleaned up after the function executes
- Minimum delay is system-dependent but typically 1ms
- Timer continues even if the extension is reloaded (cleanup required)

### setInterval
#### Schedule a function to run repeatedly at fixed intervals
Executes a function repeatedly with a fixed delay between each execution.

```javascript
// Define a named function (required - anonymous functions are not supported)
let count = 0;
function updateCounter() {
  count++;
  console.log(`Count: ${count}`);
  
  // Stop after 10 iterations
  if (count >= 10) {
    clearInterval(intervalId);
  }
}

// Start the interval
const intervalId = setInterval(updateCounter, 1000);

// Monitor glucose and alert on trends
function checkGlucoseTrends() {
  const predictions = Trndi.predictReadings(3);
  if (predictions.some(pred => pred[2] < 4.0)) {
    Trndi.alert("Low glucose predicted!");
  }
}
setInterval(checkGlucoseTrends, 60000); // Check every minute
```

**Parameters:**
- `callback`: **Named function** to execute (anonymous/arrow functions are not supported)
- `interval`: Time in milliseconds between each execution

**Returns:** Timer ID (BigInt) that can be used with `clearInterval()`

**Notes:**
- **Important:** Only named functions are supported. Arrow functions and anonymous functions will be rejected
- Pass the function name without parentheses: `setInterval(myFunction, 1000)` not `setInterval(myFunction(), 1000)`
- Function executes repeatedly until cleared with `clearInterval()`
- The interval is the delay between executions, not including function runtime
- Important: Always clear intervals when no longer needed to avoid memory leaks

### clearTimeout
#### Cancel a scheduled timeout
Cancels a timer created with `setTimeout()` before it executes.

```javascript
// Define and schedule a timeout
function showMessage() {
  Trndi.alert("This will not show");
}
const timerId = setTimeout(showMessage, 5000);

// Cancel it before it fires
clearTimeout(timerId);
```

**Parameters:**
- `timerId`: The timer ID returned by `setTimeout()`

**Returns:** Nothing

**Notes:**
- Safe to call with an invalid or already-fired timer ID (no-op)
- Has no effect on timers created with `setInterval()`

### clearInterval
#### Cancel a repeating interval
Stops a timer created with `setInterval()` from executing further.

```javascript
let count = 0;
function incrementCounter() {
  count++;
  if (count >= 5) {
    clearInterval(intervalId); // Stop after 5 iterations
  }
}
const intervalId = setInterval(incrementCounter, 1000);

// Or cancel it externally
function stopInterval() {
  clearInterval(intervalId);
}
setTimeout(stopInterval, 10000); // Stop after 10 seconds maximum
```

**Parameters:**
- `timerId`: The timer ID returned by `setInterval()`

**Returns:** Nothing

**Notes:**
- Safe to call with an invalid timer ID (no-op)
- Important: Always clear intervals to prevent resource leaks
- Has no effect on timers created with `setTimeout()`

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