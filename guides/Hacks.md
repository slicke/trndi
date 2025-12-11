# Trndi hacks
Running without extensions? These are hacks you can do using the config file!

## Linux
Trndi saves it's configuration in 
```bash
~/.config/Trndi.cfg
```
> If your system defaults have changed this might be located elsewhere. Look for your default config folder.

## Windows
Trndi stores it's settings in the registry under
```powershell
HKEY_CURRENT_USER\SOFTWARE\Trndi
```

## macOS
On macOS, Trndi stores it's settings in the app's preference file

# Color notice
Colors are stored as integer representations of ```TColor```.
Use Lazarus, C++ Builder or Delphi to figure them out.

To convert from/to CSS coloring, use the JavaScript code in the bottom of the file.

# Hacks

## Override the 6-30 minute block for readings
> This is not recommended as it might not be safe!

Trndi limits you to between 6-30 before a value is "old" and the "No recent readings" warning shows. This can be overridden:
```ini
system.fresh_threshold=31
```

## Override the 100% badge size (the reading icon on Trndi's app icon)
```ini
ux.badge_size=100
```

## Override the number readings fetched
Trndi defaults to a set amount of minutes it fetches (1440), this only affects time-in-range - not the graph!
```ini
remote.max_min=2000
```
To make this work you must also set a limit on the number of readings fetched (default 25). This is ignored by some APIs, but required here for consistency:
```ini
remote.max_result=400
```

## Resetting Trndi
#### Delete all keys and Trndi will reset to factory-default settings
<br><br>

# Shift magic
* Holding shift on start will trigger big/touch screen mode, making dialogs larger
* Shift-right clicking on Trndi will add the "Service menu" option, it adds debugging options and hidden features, though it's not recommended you use it on production environments!
# Helpers

How to convert TColors:
```javascript
/**
 * Convert a stored TColor integer string (e.g. "16711680" or "-16711681")
 * into a CSS hex string and an rgb() string.
 *
 * TColor in LCL/VCL is usually encoded as 0x00BBGGRR (low byte = red).
 */
function tColorToCss(colorStr) {
  if (!colorStr && colorStr !== '0') return null;
  const n = Number(colorStr);
  if (!Number.isFinite(n)) return null;

  // Convert to unsigned 32-bit (handles negative system-color ints)
  const u = n >>> 0;

  // Mask to 24 bits (ignore any high/alpha byte or flags)
  const rgb24 = u & 0x00FFFFFF;

  const r = rgb24 & 0xFF;
  const g = (rgb24 >> 8) & 0xFF;
  const b = (rgb24 >> 16) & 0xFF;

  const toHex = v => v.toString(16).padStart(2, '0');
  const hex = `#${toHex(r)}${toHex(g)}${toHex(b)}`;
  const rgb = `rgb(${r}, ${g}, ${b})`;

  // isSystemColor is true when original signed value was negative
  const isSystemColor = n < 0;

  return { hex, rgb, r, g, b, isSystemColor };
}
```
```javascript
/**
 * Convert CSS color (hex "#RRGGBB" or "rgb(r,g,b)") to the decimal TColor string
 * used by SetColorSetting (TColor as 32-bit integer with layout 0x00BBGGRR).
 *
 * Note: many Pascal/Win32/LCL COLORREF/TColor values use low byte = red,
 * next = green, next = blue (0x00BBGGRR). So:
 *  - red  (#ff0000)  => 255
 *  - blue (#0000ff)  => 16711680
 */
function cssToTColorString(input) {
  if (!input) return null;
  input = input.trim().toLowerCase();

  let r, g, b;

  // hex: #RRGGBB or RRGGBB
  const hexMatch = input.match(/^#?([0-9a-f]{6})$/i);
  if (hexMatch) {
    const v = parseInt(hexMatch[1], 16);
    // hex v is 0xRRGGBB, extract bytes:
    r = (v >> 16) & 0xFF;
    g = (v >> 8) & 0xFF;
    b = v & 0xFF;
  } else {
    // rgb() formats: rgb(255,0,0) or rgba(...)
    const rgbMatch = input.match(/^rgba?\(\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*(?:,\s*[\d.]+\s*)?\)$/i);
    if (rgbMatch) {
      r = Number(rgbMatch[1]);
      g = Number(rgbMatch[2]);
      b = Number(rgbMatch[3]);
      if (![r, g, b].every(v => Number.isFinite(v) && v >= 0 && v <= 255)) return null;
    } else {
      return null; // unsupported format
    }
  }

  // Build TColor: low byte = red, next = green, next = blue (0x00BBGGRR)
  const tcolor = ((b & 0xFF) << 16) | ((g & 0xFF) << 8) | (r & 0xFF);

  // tcolor is in the 0..0x00FFFFFF range and therefore non-negative;
  // Pascal code stored Integer(val) then IntToStr, so decimal string is fine.
  return String(tcolor);
}
```