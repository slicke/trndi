# API Support

Trndi supports multiple backends, due to it's _API specification_:

# The TrndiAPI
> <small>Location: ```trndi.api.pp```</small>

TrndiAPI is the main class, of which API drivers inherit. Each API must provide, or fake:

### The constructor
```pascal
constructor create(user, pass, extra: string);
```
  * _user:_ Name, IP or similar
  * _pass_: Password, API key or similar
  * _extra_: A third field can be used for extra data, if absolutely needed. The Dexcom driver uses this to set locale.
Trndi will always pass user and pass, and by default empty extras

### The connection
```pascal
 function connect: boolean;
 ```
A function to connect/start using the remote API

### Getting data
```pascal
function getReadings(min, maxNum: integer; extras: string = ''): BGResults;
```
  * _min_: Number of minutes to load (from the API:s reading history)
  * _maxNum_: The desired amount of readings (if available)
  * _extras_: Optional parameter, eg used by NightScout so decending classes (xDrip) can use the same function


If these three functions are defined, the API driver will work.

# Native
TrndiNative is a class, used to perform Windows/Mac/Linux/etc specific actions, abstracted. It's largely used to handle HTTP(S) requests, so that they will be ran via Windows API, macOS native API etc instead of relying on other libraries.

As long as you run ```inherited;``` in your constructor, you will have access to the ```Native``` variable.

You should also set your _user agent_ and _base URL_ before, in the constructor:
```pascal
baseUrl := 'https://service.net/API/';
ua := 'Uploader/3.0.2.11 Darwin/14.0.0';
```

### Request
The request function makes a HTTP/S request.
```pascal
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''): string;
```
 * _post_: Set true to perform a HTTP POST, istead of GET
 * _endpoint_: The endpoint to hit, note that we already have the base
 * _params_: A list of key=value parameters
 * _jsondata_: This is the request bod
 * _heder_: Set a header, eg API tokeny

 # Properties
 After having the functions checked out, there are also properties you need to set:
 * cgmHi - The "HI" value
 * cgmLO - The "LO" value
 * cgmRangeHI - The personal HI
 * cgmRangeLO - The personal LO

 You also habe "free" access to these values:
 * offset - timezone offset
 * timezone - Local timezone offset in seconds
 And these functions:
 * JSToDateTime - Convert milliseconds timestamp
 * getBaseTime - Current time as a timestamp, adjusted with time difference
 * encodeStr - URL safe

 # Summary
 Inherit _TrndiAPI_, declare a _constructor_, a _connect_ function and a _getReadings_ function-