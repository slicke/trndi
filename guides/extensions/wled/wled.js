/*
@name WLED Lamp
@copyright (c) Trndi sample extension
@description Mirrors the glucose level onto a WLED lamp or LED strip.
@perms net, settings
*/
// Mirrors the glucose level onto a WLED device (https://kno.wled.ge) on the
// local network - a lamp that is red while high and pulses blue while low is
// visible from across the room.
//
// Setup:
//   1. Flash/attach a WLED device and note its IP or mDNS name (e.g.
//      "192.168.1.50" or "wled.local").
//   2. On first load you'll be prompted for the address. It is stored
//      privately for this extension - clear it with
//      Trndi.storage.remove("host") to be re-prompted.
//
// The level is re-sent on every reading (levelCallback fires each update),
// so a lamp that was powered off catches up on the next reading.

const BRIGHTNESS = 128;      // 1-255
const NORMAL_COLOR = false;  // false = lamp off when in range; or e.g. [0, 255, 0]

// WLED effect ids: 0 = solid, 1 = blink, 2 = breathe
const LEVELS = {
  "high":       { col: [255, 0, 0],   fx: 0 },              // solid red
  "range-high": { col: [255, 128, 0], fx: 0 },              // solid orange
  "low":        { col: [0, 0, 255],   fx: 2, sx: 200 },     // breathing blue
  "range-low":  { col: [0, 128, 255], fx: 0 },              // solid light blue
  "stale":      { col: [64, 64, 64],  fx: 1, sx: 32 }       // slow dim blink
};

let promptedThisSession = false;

function getHost() {
  let host = Trndi.storage.get("host");
  if (host === false || host === "") {
    if (promptedThisSession) return false; // don't nag every reading
    promptedThisSession = true;
    host = Trndi.prompt(
      "WLED Lamp",
      "Address of your WLED device (IP or hostname):",
      "wled.local"
    );
    if (!host) return false;
    Trndi.storage.set("host", host);
  }
  return host;
}

function send(state) {
  const host = getHost();
  if (!host) return;

  Trndi.net.fetch("http://" + host + "/json/state", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(state),
    timeout: 5000
  }).catch(err => console.log("WLED error: " + err));
}

function levelCallback(level, previous) {
  const fx = LEVELS[level];

  if (fx)
    send({ on: true, bri: BRIGHTNESS, seg: [{ col: [fx.col], fx: fx.fx, sx: fx.sx || 128 }] });
  else if (NORMAL_COLOR)
    send({ on: true, bri: BRIGHTNESS, seg: [{ col: [NORMAL_COLOR], fx: 0 }] });
  else if (level !== previous)
    send({ on: false }); // back in range - turn the lamp off once
}
