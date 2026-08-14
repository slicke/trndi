# WLED Lamp for Trndi

Mirror your glucose level onto a [WLED](https://kno.wled.ge) lamp or LED
strip on the local network — ambient lighting you can read from across the
room, on ~$10 of hardware, with no cloud involved.

## What it does

The extension listens for Trndi's `levelCallback` and pushes a matching
state to the device:

| Level        | Lamp                    |
|--------------|-------------------------|
| `high`       | Solid red               |
| `range-high` | Solid orange            |
| `low`        | Breathing blue          |
| `range-low`  | Solid light blue        |
| `normal`     | Off (configurable)      |
| `stale`      | Slow dim blink          |

The state is re-sent on every reading, so a lamp that was powered off or
rebooted catches up within one update.

## Setup

### 1. Get a WLED device

Any ESP32/ESP8266 running WLED works — prebuilt lamps exist, or flash your
own from <https://install.wled.me>. Note its IP address or mDNS name
(e.g. `192.168.1.50` or `wled.local`).

### 2. Install the extension

1. Open Trndi → right-click → *Settings* → note the **plugin folder** path.
2. Copy `wled.js` from this directory into the plugin folder.
3. Restart Trndi. On first load it will:
   - Prompt to approve the `net` and `settings` permissions — click **Yes**.
   - Prompt for the device address — enter it and confirm.

The address is saved under `extval.wled.host`. To change it, clear that
setting and reload — Trndi will prompt again.

## Tuning

The knobs are at the top of `wled.js`:

```javascript
const BRIGHTNESS = 128;      // 1-255
const NORMAL_COLOR = false;  // false = off when in range; or e.g. [0, 255, 0]
```

Colors and effects per level live in the `LEVELS` table right below —
`col` is `[r, g, b]`, `fx` is a WLED effect id (0 = solid, 1 = blink,
2 = breathe) and `sx` its speed.

The high/low classification uses your **Trndi limits** (configured in
Settings), so changing your limits automatically retunes the lamp.

## Testing

Confirm the device answers before relying on it:

```
curl -X POST http://wled.local/json/state -d '{"on":true,"seg":[{"col":[[255,0,0]]}]}'
```

## Troubleshooting

- **Nothing happens** — check Trndi's console for `WLED error: …` lines,
  and that the PC and the device are on the same network/VLAN.
- **Lamp stays on the old color** — the device only updates when a reading
  arrives; wait for the next update or check the address setting.
- **mDNS name not found** — some networks block mDNS; use the IP address
  instead (give the device a DHCP reservation).

## Privacy note

Everything stays on your local network — the extension talks only to the
device address you configure.
