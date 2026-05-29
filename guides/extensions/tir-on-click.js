/* TIR On Click
(c) Trndi sample extension
*/
// Click the Time-in-Range bar to get a clean HTML report comparing
// the last 3h / 24h / 7d windows side by side.

function fmtPct(p) {
  if (typeof p !== "number" || isNaN(p)) return "—";
  return p.toFixed(0) + "%";
}

function fmtNum(n, digits) {
  if (typeof n !== "number" || isNaN(n)) return "—";
  return n.toFixed(digits);
}

function rowFor(label, mins) {
  const s = Trndi.getStatistics(mins);
  if (!s || s.readingCount === 0) {
    return "<tr><td>" + label + "</td><td colspan='5' style='color:#888'>no data</td></tr>";
  }
  return "<tr>" +
    "<td><b>" + label + "</b></td>" +
    "<td>" + fmtNum(s.mean, 1)   + "</td>" +
    "<td>" + fmtNum(s.stdDev, 1) + "</td>" +
    "<td>" + fmtNum(s.cv, 1)     + "%</td>" +
    "<td style='color:#2a9d2a'>" + fmtPct(s.timeInRange) + "</td>" +
    "<td style='color:#d33'>"    + fmtPct(s.timeAbove)   + " / " +
                                   fmtPct(s.timeBelow)   + "</td>" +
    "</tr>";
}

function uxClick(element, value) {
  if (element !== "tir") return false;       // let other elements behave normally

  const unit = Trndi.getUnit();
  const html =
    "<h3 style='margin:0 0 8px 0'>Glucose statistics (" + unit + ")</h3>" +
    "<table cellspacing='6' cellpadding='4' style='font-family:sans-serif'>" +
    "<tr style='background:#eee'>" +
      "<th>Window</th><th>Mean</th><th>SD</th><th>CV</th>" +
      "<th>TIR</th><th>Hi / Lo</th>" +
    "</tr>" +
    rowFor("3 hours", 180) +
    rowFor("24 hours", 1440) +
    rowFor("7 days", 60 * 24 * 7) +
    "</table>" +
    "<p style='color:#666;font-size:90%;margin-top:10px'>" +
    "Tip: CV under 36% is a common target for stable control." +
    "</p>";

  Trndi.htmlMsg("Time in Range", "Glucose statistics", "Across multiple windows", html, 1);
  return true;   // suppress Trndi's default popup
}
