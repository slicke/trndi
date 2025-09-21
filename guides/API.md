# API Support

Trndi supports multiple backends via its API specification.

# The TrndiAPI
> Location: `units/trndi/api/trndi.api.pp`

`TrndiAPI` is the base class that API drivers inherit. Each API driver must implement:

### Constructor
```pascal
constructor create(user, pass, extra: string);
```
- user: Name, URL/IP, or similar identifier
- pass: Password, API key, or similar secret
- extra: Optional extra data (e.g., Dexcom uses this for locale)

Trndi always passes user and pass. `extra` defaults to empty unless needed.

### Connection
```pascal
function connect: boolean;
```
Establish connectivity and perform any initial probing.

### Getting data
```pascal
function getReadings(minNum, maxNum: integer; extras: string = ''): BGResults;
```
- minNum: Number of minutes to load from the API’s reading history
- maxNum: Desired number of readings (if available)
- extras: Optional parameter; e.g., Nightscout uses it to allow xDrip to share the same implementation

Advanced overload (used by the base class and some drivers):
```pascal
function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults;
```
This variant also returns the raw response via `res`.

If these three functions are provided, the API driver will work.

# Native
`TrndiNative` provides platform-specific features under a common API. It’s often used for HTTP(S) so requests run via native APIs (WinHTTP, NS, etc.) instead of third-party libs.

Call `inherited;` in your constructor. You’ll have access to the `native` field.

Set a user agent and base URL in your constructor:
```pascal
baseUrl := 'https://service.net/API/';
ua := 'Uploader/3.0.2.11 Darwin/14.0.0';
```

### Request
`request` makes an HTTP/S request.
```pascal
function request(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  const header: string = ''): string;
```
- post: True for HTTP POST; False for GET
- endpoint: Relative path (joined with `baseUrl`)
- params: List of `key=value` pairs
- jsondata: Optional request body
- header: Optional header as `Key=Value`

# Properties
You should set these thresholds (see `CGMCore`):
- cgmHi – Global high boundary (BGHIGH)
- cgmLo – Global low boundary (BGLOW)
- cgmRangeHi – Personalized in-range upper bound (optional; 500 disables)
- cgmRangeLo – Personalized in-range lower bound (optional; 0 disables)

Helpers:
- offset – Time difference (seconds) calculated by the implementation
- timezone – Write-only property to set TZ offset (minutes); stored internally as seconds

Utility functions:
- JSToDateTime – Convert JavaScript ms timestamp to `TDateTime`
- getBaseTime – Current adjusted time as a Unix timestamp (seconds)
- encodeStr – URL-encode (percent-encode) a string

# Summary
Inherit `TrndiAPI`, implement `create`, `connect`, and `getReadings`. Use the `native` helper for HTTP.