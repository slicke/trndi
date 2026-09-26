/*
@name Module demo
@copyright (c) Trndi sample extension
@description Shows an extension split into ES modules: the entry file imports helpers from lib/.
*/
// Install: copy module-demo.js AND the lib/ folder into the plugin folder.
// Trndi loads only top-level .js files as extensions, so lib/format.js is
// reached through the import below and never runs on its own.

import Trndi, { data } from "trndi";
import { formatReading, describeTrend } from "./lib/format.js";

// Exported functions are published as Trndi callbacks, so this replaces the
// clock text like a top-level `function clockView()` would in a classic script.
export function clockView() {
  const reading = data.current();
  return reading ? formatReading(reading, Trndi.getUnit()) : "--";
}

// Not exported: private to this module, invisible to other extensions.
function announce(reading) {
  console.push(`Module demo: ${formatReading(reading, Trndi.getUnit())} ${describeTrend(reading)}`);
}

Trndi.on("reading", () => {
  const reading = data.current();
  if (reading) announce(reading);
});
