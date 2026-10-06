//
// * Trndi
// * Medical and Non-Medical Usage Alert
// *
// * Copyright (c) Björn Lindh
// * GitHub: https://github.com/slicke/trndi
// *
// * This program is distributed under the terms of the GNU General Public License,
// * Version 3, as published by the Free Software Foundation. You may redistribute
// * and/or modify the software under the terms of this license.
// *
// * A copy of the GNU General Public License should have been provided with this
// * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
// *
// * ================================== IMPORTANT ==================================
// * MEDICAL DISCLAIMER:
// * - This software is NOT a medical device and must NOT replace official continuous
// *   glucose monitoring (CGM) systems or any healthcare decision-making process.
// * - The data provided may be delayed, inaccurate, or unavailable.
// * - DO NOT make medical decisions based on this software.
// * - VERIFY all data using official devices and consult a healthcare professional for
// *   medical concerns or emergencies.
// *
// * LIABILITY LIMITATION:
// * - The software is provided "AS IS" and without any warranty—expressed or implied.
// * - Users assume all risks associated with its use. The developers disclaim all
// *   liability for any damage, injury, or harm, direct or incidental, arising
// *   from its use.
// *
// * INSTRUCTIONS TO DEVELOPERS & USERS:
// * - Any modifications to this file must include a prominent notice outlining what was
// *   changed and the date of modification (as per GNU GPL Section 5).
// * - Distribution of a modified version must include this header and comply with the
// *   license terms.
// *
// * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
// *
// * MODIFICATION NOTICE (GPLv3 Section 5):
// * - 2026-10-07: Added the Summary card (from /report) and the History card
// *   (the /history.png image, fetched with the token and shown as a blob),
// *   with a shared time-window picker; both follow each new reading.
//
//
// GENERATED FILE - do not edit. Source: assets/web_dashboard.html;
// regenerate with `make dashboard` (Unix) or `.\make.ps1 dashboard` (Windows).
//
// The embedded web dashboard served by trndi.webserver.threaded at GET /.
WEB_DASHBOARD_HTML =
  '<!doctype html>' + LineEnding +
  '<html lang="en">' + LineEnding +
  '<head>' + LineEnding +
  '<meta charset="utf-8">' + LineEnding +
  '<meta name="viewport" content="width=device-width,initial-scale=1">' + LineEnding +
  '<meta name="color-scheme" content="light dark">' + LineEnding +
  '<title>Trndi</title>' + LineEnding +
  '<style>' + LineEnding +
  ':root{--bg:#f3f4f7;--card:#fff;--fg:#1c1f26;--muted:#69707f;--line:#e2e5eb;--ok:#2a9354;--warn:#c98a00;--lo:#d24238;--hi:#d97b00;--accent:#3466d1;--chip-on:#d9f2e3;--chip-on-fg:#1c6b3c}' + LineEnding +
  '@media(prefers-color-scheme:dark){:root{--bg:#12141a;--card:#1b1e27;--fg:#e7e9ef;--muted:#98a0b0;--line:#2a2f3b;--ok:#3ec46f;--warn:#e0a91f;--lo:#ff6b61;--hi:#ffa03c;--accent:#6b93ff;--chip-on:#1e4a30' +
  ';--chip-on-fg:#8fe0ad}}' + LineEnding +
  '*{box-sizing:border-box}' + LineEnding +
  'body{margin:0;font:15px/1.4 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;background:var(--bg);color:var(--fg)}' + LineEnding +
  'header,main{max-width:720px;margin:0 auto;padding:0 16px}' + LineEnding +
  'header{display:flex;align-items:center;justify-content:space-between;padding-top:14px;padding-bottom:6px}' + LineEnding +
  'h1{font-size:18px;margin:0;letter-spacing:.02em}' + LineEnding +
  'h2{font-size:12px;margin:0 0 10px;color:var(--muted);text-transform:uppercase;letter-spacing:.06em}' + LineEnding +
  'main{display:grid;gap:12px;padding-bottom:28px}' + LineEnding +
  '.card{background:var(--card);border:1px solid var(--line);border-radius:14px;padding:16px}' + LineEnding +
  '.chip{font-size:12px;padding:3px 10px;border-radius:999px;background:var(--line);color:var(--muted);white-space:nowrap}' + LineEnding +
  '.chip.on{background:var(--chip-on);color:var(--chip-on-fg)}' + LineEnding +
  '.chip.bad{background:var(--lo);color:#fff}' + LineEnding +
  '.hero{display:flex;align-items:center;gap:14px;flex-wrap:wrap}' + LineEnding +
  '.val{font-size:76px;font-weight:700;line-height:1;font-variant-numeric:tabular-nums;letter-spacing:-.02em}' + LineEnding +
  '.arrow{font-size:44px;line-height:1}' + LineEnding +
  '.sub{color:var(--muted)}' + LineEnding +
  '.lvl-low .val,.lvl-low .arrow{color:var(--lo)}' + LineEnding +
  '.lvl-high .val,.lvl-high .arrow{color:var(--hi)}' + LineEnding +
  '.lvl-range_low .val,.lvl-range_high .val{color:var(--warn)}' + LineEnding +
  '.lvl-normal .val,.lvl-normal .arrow{color:var(--ok)}' + LineEnding +
  'svg{width:100%;height:160px;display:block;margin-top:12px;overflow:visible}' + LineEnding +
  '.row{display:flex;gap:8px;flex-wrap:wrap;align-items:center}' + LineEnding +
  'button{font:inherit;padding:7px 12px;border-radius:9px;border:1px solid var(--line);background:var(--card);color:var(--fg);cursor:pointer}' + LineEnding +
  'button.primary{background:var(--accent);border-color:var(--accent);color:#fff}' + LineEnding +
  'button:disabled{opacity:.45;cursor:default}' + LineEnding +
  'input[type=number],input[type=password]{font:inherit;width:6.5em;padding:6px 8px;border-radius:8px;border:1px solid var(--line);background:var(--bg);color:var(--fg)}' + LineEnding +
  'input[type=password]{width:14em;max-width:100%}' + LineEnding +
  'label{display:inline-flex;align-items:center;gap:6px}' + LineEnding +
  'fieldset{border:0;padding:0;margin:0 0 10px}' + LineEnding +
  'fieldset:disabled{opacity:.6}' + LineEnding +
  '.pred{display:flex;gap:8px;overflow-x:auto;padding-bottom:2px}' + LineEnding +
  '.pred div{min-width:72px;text-align:center;padding:8px 6px;border-radius:10px;background:var(--bg)}' + LineEnding +
  '.pred b{display:block;font-size:20px;font-variant-numeric:tabular-nums}' + LineEnding +
  '.pred small{color:var(--muted)}' + LineEnding +
  'button.sel{background:var(--accent);border-color:var(--accent);color:#fff}' + LineEnding +
  '.stats{display:grid;grid-template-columns:repeat(auto-fill,minmax(104px,1fr));gap:8px;margin-top:10px}' + LineEnding +
  '.stats div{background:var(--bg);border-radius:10px;padding:8px 6px;text-align:center}' + LineEnding +
  '.stats b{display:block;font-size:20px;font-variant-numeric:tabular-nums}' + LineEnding +
  '.stats small{color:var(--muted)}' + LineEnding +
  '.spark{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:20px;line-height:1;letter-spacing:1px;margin:12px 0 0;color:var(--ok);overflow-x:auto;white-space:nowrap}' + LineEnding +
  '#hist{width:100%;height:auto;display:block;border-radius:10px;background:#fff}' + LineEnding +
  'ul{margin:8px 0 0;padding-left:18px;color:var(--muted)}' + LineEnding +
  '.hidden{display:none}' + LineEnding +
  '.note{font-size:13px;color:var(--muted);margin:8px 0 0}' + LineEnding +
  '.err{color:var(--lo)}' + LineEnding +
  'footer{max-width:720px;margin:0 auto;padding:0 16px 24px;font-size:12px;color:var(--muted)}' + LineEnding +
  'a{color:var(--accent)}' + LineEnding +
  '</style>' + LineEnding +
  '</head>' + LineEnding +
  '<body>' + LineEnding +
  '<header><h1>Trndi</h1><span id="conn" class="chip">Connecting</span></header>' + LineEnding +
  '<main>' + LineEnding +
  '<section id="auth" class="card hidden">' + LineEnding +
  '<h2>Access token</h2>' + LineEnding +
  '<p class="note">This Trndi asks for its web token (the <code>webserver.token</code> setting). It is stored only in this browser.</p>' + LineEnding +
  '<form id="authForm" class="row"><input id="token" type="password" autocomplete="off" placeholder="Token"><button class="primary">Connect</button></form>' + LineEnding +
  '</section>' + LineEnding +
  '<section class="card" id="heroCard">' + LineEnding +
  '<div class="hero"><span class="val" id="val">--</span><span class="arrow" id="arrow"></span>' + LineEnding +
  '<div><div id="delta" class="sub"></div><div id="ago" class="sub"></div></div></div>' + LineEnding +
  '<svg id="chart" viewBox="0 0 600 160" preserveAspectRatio="none" aria-hidden="true"></svg>' + LineEnding +
  '<p id="status" class="note"></p>' + LineEnding +
  '</section>' + LineEnding +
  '<section class="card"><h2>Forecast</h2><div class="pred" id="pred"><span class="sub">No forecast</span></div></section>' + LineEnding +
  '<section class="card hidden" id="summaryCard"><h2>Summary</h2>' + LineEnding +
  '<div class="row" id="win"><button data-w="180">3 h</button><button data-w="360">6 h</button><button data-w="720">12 h</button><button data-w="1440">24 h</button><button data-w="0" class="sel">All</but' +
  'ton></div>' + LineEnding +
  '<div class="stats" id="stats"></div>' + LineEnding +
  '<p id="spark" class="spark"></p>' + LineEnding +
  '<p id="sumNote" class="note"></p>' + LineEnding +
  '</section>' + LineEnding +
  '<section class="card hidden" id="historyCard"><h2>History</h2>' + LineEnding +
  '<img id="hist" alt="History graph">' + LineEnding +
  '<p class="note">The same graph as the History window, with any insulin and carbohydrate overlays enabled in Settings.</p>' + LineEnding +
  '</section>' + LineEnding +
  '<section class="card"><h2>Alerts</h2>' + LineEnding +
  '<div class="row"><span id="snooze">Alerts active</span>' + LineEnding +
  '<span id="snoozeBtns" class="row hidden"><button data-m="15">15 min</button><button data-m="30">30 min</button><button data-m="60">60 min</button><button data-m="0">Resume</button></span></div>' + LineEnding +
  '<ul id="alerts"></ul>' + LineEnding +
  '</section>' + LineEnding +
  '<section class="card hidden" id="settingsCard"><h2>Settings</h2>' + LineEnding +
  '<form id="settings">' + LineEnding +
  '<fieldset id="fields">' + LineEnding +
  '<div class="row"><label><input type="radio" name="unit" value="mmol"> mmol/L</label><label><input type="radio" name="unit" value="mgdl"> mg/dL</label></div>' + LineEnding +
  '<p><label><input type="checkbox" name="ov"> Custom high/low limits</label></p>' + LineEnding +
  '<div class="row"><label>Low <input type="number" name="lo" step="any" min="0"></label><label>High <input type="number" name="hi" step="any" min="0"></label></div>' + LineEnding +
  '<p><label><input type="checkbox" name="rg"> Custom in-range band</label></p>' + LineEnding +
  '<div class="row"><label>From <input type="number" name="rlo" step="any" min="0"></label><label>To <input type="number" name="rhi" step="any" min="0"></label></div>' + LineEnding +
  '<p><label><input type="checkbox" name="pred"> Show predictions</label></p>' + LineEnding +
  '</fieldset>' + LineEnding +
  '<div class="row"><button class="primary" id="save">Save</button><span id="saveMsg" class="note"></span></div>' + LineEnding +
  '<p id="ro" class="note hidden">Read only: changing settings needs a connection from this computer (localhost) or a configured token.</p>' + LineEnding +
  '</form>' + LineEnding +
  '</section>' + LineEnding +
  '</main>' + LineEnding +
  '<footer>Not a medical device. Verify every value on your official CGM device. &middot; <a href="/health">/health</a></footer>' + LineEnding +
  '<script>' + LineEnding +
  '(function(){' + LineEnding +
  '"use strict";' + LineEnding +
  'var $=function(id){return document.getElementById(id)};' + LineEnding +
  'var ARROWS=["\u21C8","\u2191","\u2197","\u2192","\u2198","\u2193","\u21CA","?","X"];' + LineEnding +
  'var MGDL=18.0156;' + LineEnding +
  'var token=null;' + LineEnding +
  'try{token=localStorage.getItem("trndi.token")||""}catch(e){token=""}' + LineEnding +
  'var unit="mmol",settings=null,readings=[],preds=[],last=null,alerts=[],es=null,hasApi=true;' + LineEnding +
  'var win=0,histUrl=null,histBusy=false,histAt=0;' + LineEnding +
  'function hdr(){return token?{"Authorization":"Bearer "+token}:{}}' + LineEnding +
  'function api(path,opts){opts=opts||{};opts.headers=Object.assign({},opts.headers||{},hdr());' + LineEnding +
  ' return fetch(path,opts).then(function(r){if(r.status===401){needToken();throw new Error("unauthorized")}return r})}' + LineEnding +
  'function needToken(){$("auth").classList.remove("hidden");setConn("Token required","");if(es){es.close();es=null}}' + LineEnding +
  'function setConn(t,cls){var c=$("conn");c.textContent=t;c.className="chip "+cls}' + LineEnding +
  'function toUnit(mgdl){return unit==="mmol"?Math.round(mgdl/MGDL*10)/10:Math.round(mgdl)}' + LineEnding +
  'function toMgdl(v){return unit==="mmol"?Math.round(v*MGDL):Math.round(v)}' + LineEnding +
  'function hhmm(d){return new Date(d).toLocaleTimeString([],{hour:"2-digit",minute:"2-digit"})}' + LineEnding +
  'function render(){' + LineEnding +
  ' var card=$("heroCard");' + LineEnding +
  ' if(!last){$("val").textContent="--";return}' + LineEnding +
  ' card.className="card lvl-"+(last.level||"normal");' + LineEnding +
  ' $("val").textContent=unit==="mmol"?last.mmol:last.mgdl;' + LineEnding +
  ' $("arrow").textContent=ARROWS[last.trend]||"";' + LineEnding +
  ' var d=unit==="mmol"?parseFloat(last.mmol_delta):last.mgdl_delta;' + LineEnding +
  ' $("delta").textContent=(d>0?"+":"")+(unit==="mmol"?d.toFixed(1):d)+" "+(unit==="mmol"?"mmol/L":"mg/dL");' + LineEnding +
  ' renderAgo();' + LineEnding +
  '}' + LineEnding +
  'function renderAgo(){' + LineEnding +
  ' if(!last)return;' + LineEnding +
  ' var min=Math.round((Date.now()-new Date(last.timestamp_utc))/60000);' + LineEnding +
  ' $("ago").textContent=min<1?"just now":min+" min ago"+(min>15?" \u00B7 stale":"");' + LineEnding +
  '}' + LineEnding +
  'function renderPred(){' + LineEnding +
  ' var el=$("pred");el.innerHTML="";' + LineEnding +
  ' if(!preds.length){el.innerHTML="<span class=sub>No forecast</span>";return}' + LineEnding +
  ' preds.slice(0,6).forEach(function(p){var d=document.createElement("div");' + LineEnding +
  '  d.innerHTML="<b>"+(unit==="mmol"?p.mmol:p.mgdl)+"</b><small>"+hhmm(p.timestamp_utc)+"</small>";el.appendChild(d)});' + LineEnding +
  ' drawChart();' + LineEnding +
  '}' + LineEnding +
  'function renderAlerts(){' + LineEnding +
  ' var ul=$("alerts");ul.innerHTML="";' + LineEnding +
  ' alerts.forEach(function(a){var li=document.createElement("li");' + LineEnding +
  '  li.textContent=hhmm(a.time_utc)+" \u2014 "+a.kinds.join(", ").replace(/_/g," ")+" ("+(unit==="mmol"?a.reading.mmol:a.reading.mgdl)+")";ul.appendChild(li)});' + LineEnding +
  '}' + LineEnding +
  'function drawChart(){' + LineEnding +
  ' var svg=$("chart");var W=600,H=160,PL=34,PR=8,PT=8,PB=18;svg.innerHTML="";' + LineEnding +
  ' var pts=readings.map(function(r){return{t:new Date(r.timestamp_utc).getTime(),v:r.mgdl}}).sort(function(a,b){return a.t-b.t});' + LineEnding +
  ' var pp=preds.map(function(r){return{t:new Date(r.timestamp_utc).getTime(),v:r.mgdl}}).sort(function(a,b){return a.t-b.t});' + LineEnding +
  ' if(!pts.length)return;' + LineEnding +
  ' var now=Date.now(),t0=Math.min(now-3*3600e3,pts[0].t),t1=Math.max(now,pp.length?pp[pp.length-1].t:now);' + LineEnding +
  ' var th=settings&&settings.thresholds||{lo:70,hi:180};' + LineEnding +
  ' var vmax=Math.max(th.hi+40,Math.max.apply(null,pts.concat(pp).map(function(p){return p.v}))+20);' + LineEnding +
  ' var vmin=Math.min(40,th.lo-10);' + LineEnding +
  ' var X=function(t){return PL+(t-t0)/(t1-t0)*(W-PL-PR)},Y=function(v){return PT+(vmax-v)/(vmax-vmin)*(H-PT-PB)};' + LineEnding +
  ' var s="";' + LineEnding +
  ' function rect(y1,y2,fill){s+="<rect x="+PL+" y="+Y(y2).toFixed(1)+" width="+(W-PL-PR)+" height="+(Y(y1)-Y(y2)).toFixed(1)+" fill="+fill+" />"}' + LineEnding +
  ' if(th.range_lo!=null&&th.range_hi!=null)rect(th.range_lo,th.range_hi,"rgba(42,147,84,.12)");' + LineEnding +
  ' function hline(v,color,label){var y=Y(v).toFixed(1);s+="<line x1="+PL+" x2="+(W-PR)+" y1="+y+" y2="+y+" stroke="+color+" stroke-dasharray=\"4 4\" stroke-width=1 />";' + LineEnding +
  '  s+="<text x=0 y="+(+y+4)+" font-size=11 fill=\"var(--muted)\">"+label+"</text>"}' + LineEnding +
  ' hline(th.hi,"var(--hi)",toUnit(th.hi));hline(th.lo,"var(--lo)",toUnit(th.lo));' + LineEnding +
  ' var poly=pts.map(function(p){return X(p.t).toFixed(1)+","+Y(p.v).toFixed(1)}).join(" ");' + LineEnding +
  ' s+="<polyline points=\""+poly+"\" fill=none stroke=\"var(--fg)\" stroke-width=2 stroke-linejoin=round />";' + LineEnding +
  ' if(pp.length){var lastp=pts[pts.length-1];var dp=[lastp].concat(pp).map(function(p){return X(p.t).toFixed(1)+","+Y(p.v).toFixed(1)}).join(" ");' + LineEnding +
  '  s+="<polyline points=\""+dp+"\" fill=none stroke=\"var(--muted)\" stroke-width=2 stroke-dasharray=\"3 4\" />"}' + LineEnding +
  ' pts.forEach(function(p){var c=p.v<=th.lo?"var(--lo)":p.v>=th.hi?"var(--hi)":"var(--ok)";' + LineEnding +
  '  s+="<circle cx="+X(p.t).toFixed(1)+" cy="+Y(p.v).toFixed(1)+" r=3 fill="+c+" />"});' + LineEnding +
  ' for(var t=Math.ceil(t0/3600e3)*3600e3;t<=t1;t+=3600e3)s+="<text x="+X(t).toFixed(1)+" y="+(H-4)+" font-size=11 fill=\"var(--muted)\" text-anchor=middle>"+hhmm(t)+"</text>";' + LineEnding +
  ' svg.innerHTML=s;' + LineEnding +
  '}' + LineEnding +
  'function renderReport(j){' + LineEnding +
  ' var el=$("stats");el.innerHTML="";$("spark").textContent="";$("sumNote").textContent="";' + LineEnding +
  ' if(!j.valid){el.innerHTML="<span class=sub>No readings in this window</span>";return}' + LineEnding +
  ' var u=j.unit==="mgdl"?"mg/dL":"mmol/L";' + LineEnding +
  ' function tile(v,l){var d=document.createElement("div");d.innerHTML="<b></b><small></small>";d.firstChild.textContent=v;d.lastChild.textContent=l;el.appendChild(d)}' + LineEnding +
  ' tile(Math.round(j.in_limits_percent)+"%","In range");' + LineEnding +
  ' tile(j.mean,"Mean "+u);' + LineEnding +
  ' tile(j.gmi_percent+"%","GMI");' + LineEnding +
  ' tile(j.cv_percent+"%","CV");' + LineEnding +
  ' tile(j.lowest.value,"Low "+hhmm(j.lowest.at_utc));' + LineEnding +
  ' tile(j.highest.value,"High "+hhmm(j.highest.at_utc));' + LineEnding +
  ' $("spark").textContent=j.sparkline;' + LineEnding +
  ' $("sumNote").textContent=j.count+" readings, "+hhmm(j.first_utc)+" – "+hhmm(j.last_utc)+", "+Math.round(j.coverage_percent)+"% coverage. Descriptive only: not a medical assessment.";' + LineEnding +
  '}' + LineEnding +
  'function loadReport(){' + LineEnding +
  ' if(!hasApi)return Promise.resolve();' + LineEnding +
  ' return api("/report"+(win?"?minutes="+win:"")).then(function(r){' + LineEnding +
  '  if(r.status===501){hasApi=false;return null}' + LineEnding +
  '  return r.ok?r.json():null}).then(function(j){if(j){$("summaryCard").classList.remove("hidden");renderReport(j)}}).catch(function(){})' + LineEnding +
  '}' + LineEnding +
  'function loadHistory(force){' + LineEnding +
  ' if(!hasApi||histBusy)return Promise.resolve();' + LineEnding +
  ' if(!force&&Date.now()-histAt<30000)return Promise.resolve();' + LineEnding +
  ' // The card may still be hidden, so size the image from the column: main''s' + LineEnding +
  ' // 16px gutters and the card''s 16px padding plus 1px border on each side.' + LineEnding +
  ' var css=Math.max(240,Math.min(720,window.innerWidth)-66),dpr=window.devicePixelRatio||1;' + LineEnding +
  ' var w=Math.max(240,Math.min(2400,Math.round(css*dpr))),h=Math.max(160,Math.min(1600,Math.round(w*0.55)));' + LineEnding +
  ' histBusy=true;' + LineEnding +
  ' return api("/history.png?width="+w+"&height="+h+(win?"&minutes="+win:"")).then(function(r){' + LineEnding +
  '  if(r.status===501){hasApi=false;return null}' + LineEnding +
  '  return r.ok?r.blob():null}).then(function(b){' + LineEnding +
  '  if(!b)return;' + LineEnding +
  '  var img=$("hist"),u=URL.createObjectURL(b);' + LineEnding +
  '  img.onload=function(){if(histUrl&&histUrl!==u)URL.revokeObjectURL(histUrl);histUrl=u};' + LineEnding +
  '  img.src=u;$("historyCard").classList.remove("hidden");histAt=Date.now()' + LineEnding +
  ' }).catch(function(){}).then(function(){histBusy=false})' + LineEnding +
  '}' + LineEnding +
  'function loadReadings(){' + LineEnding +
  ' return api("/glucose").then(function(r){return r.ok?r.json():{}}).then(function(j){' + LineEnding +
  '  readings=Object.keys(j).filter(function(k){return /^\d+$/.test(k)}).map(function(k){return j[k]});' + LineEnding +
  '  if(readings.length&&!last)last=readings[0];' + LineEnding +
  '  render();drawChart()}).catch(function(){})' + LineEnding +
  '}' + LineEnding +
  'function applySettings(s){' + LineEnding +
  ' settings=s;unit=s.unit==="mgdl"?"mgdl":"mmol";' + LineEnding +
  ' var f=$("settings");' + LineEnding +
  ' f.unit.value=unit;f.ov.checked=!!s.override.enabled;f.rg.checked=!!s.override.range;f.pred.checked=!!s.predictions;' + LineEnding +
  ' f.lo.value=toUnit(s.override.lo);f.hi.value=toUnit(s.override.hi);f.rlo.value=toUnit(s.override.range_lo);f.rhi.value=toUnit(s.override.range_hi);' + LineEnding +
  ' f.lo.step=f.hi.step=f.rlo.step=f.rhi.step=unit==="mmol"?"0.1":"1";' + LineEnding +
  ' $("fields").disabled=!s.writable;$("save").disabled=!s.writable;' + LineEnding +
  ' $("ro").classList.toggle("hidden",!!s.writable);' + LineEnding +
  ' $("snoozeBtns").classList.toggle("hidden",!s.writable);' + LineEnding +
  ' if(s.snooze)showSnooze(s.snooze);' + LineEnding +
  ' render();renderPred();renderAlerts();drawChart();' + LineEnding +
  '}' + LineEnding +
  'function loadSettings(){' + LineEnding +
  ' return api("/settings").then(function(r){' + LineEnding +
  '  if(r.status===501){hasApi=false;return null}' + LineEnding +
  '  return r.ok?r.json():null}).then(function(s){if(s){$("settingsCard").classList.remove("hidden");applySettings(s)}}).catch(function(){})' + LineEnding +
  '}' + LineEnding +
  'function showSnooze(s){$("snooze").textContent=s.active?"Snoozed until "+hhmm(s.until_utc):"Alerts active"}' + LineEnding +
  'function connect(){' + LineEnding +
  ' if(es)es.close();' + LineEnding +
  ' es=new EventSource("/events"+(token?"?token="+encodeURIComponent(token):""));' + LineEnding +
  ' es.onopen=function(){setConn("Live","on")};' + LineEnding +
  ' es.onerror=function(){setConn("Reconnecting","")};' + LineEnding +
  ' es.addEventListener("reading",function(e){last=JSON.parse(e.data);render();loadReadings();loadReport();loadHistory()});' + LineEnding +
  ' es.addEventListener("predict",function(e){preds=JSON.parse(e.data).predictions||[];renderPred()});' + LineEnding +
  ' es.addEventListener("status",function(e){var s=JSON.parse(e.data);' + LineEnding +
  '  $("status").textContent=(s.connected?"Connected to CGM source":"Not connected")+(s.fresh?"":" \u00B7 data is not fresh")+(s.detail?" \u00B7 "+s.detail:"")});' + LineEnding +
  ' es.addEventListener("snooze",function(e){showSnooze(JSON.parse(e.data))});' + LineEnding +
  ' es.addEventListener("alert",function(e){alerts.unshift(JSON.parse(e.data));alerts=alerts.slice(0,8);renderAlerts()});' + LineEnding +
  '}' + LineEnding +
  'function boot(){' + LineEnding +
  ' api("/health").then(function(r){if(!r.ok)throw new Error("health");$("auth").classList.add("hidden");' + LineEnding +
  '  return loadSettings().then(loadReadings).then(loadReport).then(function(){return loadHistory(true)}).then(connect)' + LineEnding +
  ' }).catch(function(e){if(e.message!=="unauthorized")setConn("Offline","bad")});' + LineEnding +
  '}' + LineEnding +
  '$("authForm").addEventListener("submit",function(e){e.preventDefault();token=$("token").value.trim();' + LineEnding +
  ' try{localStorage.setItem("trndi.token",token)}catch(x){}boot()});' + LineEnding +
  '$("settings").addEventListener("submit",function(e){' + LineEnding +
  ' e.preventDefault();var f=e.target;unit=f.unit.value;' + LineEnding +
  ' var body={unit:unit,predictions:f.pred.checked,override:{enabled:f.ov.checked,lo:toMgdl(+f.lo.value),hi:toMgdl(+f.hi.value),' + LineEnding +
  '  range:f.rg.checked,range_lo:toMgdl(+f.rlo.value),range_hi:toMgdl(+f.rhi.value)}};' + LineEnding +
  ' $("saveMsg").textContent="Saving\u2026";$("saveMsg").className="note";' + LineEnding +
  ' api("/settings",{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify(body)}).then(function(r){return r.json().then(function(j){return{ok:r.ok,j:j}})})' + LineEnding +
  ' .then(function(x){if(x.ok){applySettings(x.j);$("saveMsg").textContent="Saved"}else{$("saveMsg").textContent=x.j.error||"Rejected";$("saveMsg").className="note err"}' + LineEnding +
  '  loadReadings();loadReport();loadHistory(true)}).catch(function(){$("saveMsg").textContent="Failed";$("saveMsg").className="note err"});' + LineEnding +
  '});' + LineEnding +
  '$("settings").unit.forEach(function(r){r.addEventListener("change",function(){' + LineEnding +
  ' var f=$("settings"),old=unit;unit=r.value;if(old===unit)return;' + LineEnding +
  ' ["lo","hi","rlo","rhi"].forEach(function(n){var v=+f[n].value;f[n].value=unit==="mmol"?Math.round(v/MGDL*10)/10:Math.round(v*MGDL)});' + LineEnding +
  ' f.lo.step=f.hi.step=f.rlo.step=f.rhi.step=unit==="mmol"?"0.1":"1";render();renderPred();drawChart()})});' + LineEnding +
  '$("snoozeBtns").addEventListener("click",function(e){var b=e.target.closest("button");if(!b)return;' + LineEnding +
  ' api("/snooze",{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify({minutes:+b.dataset.m})})' + LineEnding +
  ' .then(function(r){return r.json()}).then(function(j){if(j.snooze)showSnooze(j.snooze)}).catch(function(){})});' + LineEnding +
  '$("win").addEventListener("click",function(e){var b=e.target.closest("button");if(!b)return;' + LineEnding +
  ' win=+b.dataset.w;Array.prototype.forEach.call($("win").children,function(c){c.classList.toggle("sel",c===b)});' + LineEnding +
  ' loadReport();loadHistory(true)});' + LineEnding +
  'setInterval(renderAgo,10000);' + LineEnding +
  'boot();' + LineEnding +
  '})();' + LineEnding +
  '</script>' + LineEnding +
  '</body>' + LineEnding +
  '</html>' + LineEnding +
  '';
