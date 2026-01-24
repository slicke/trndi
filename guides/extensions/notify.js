/* Notify extension for Nightscout
(c) slicke / No copyright
*/
function updateCallback(curr, mgdl, mmol, ts){
    if (mmol > 7)
        runCMD("notify-send 'High BG' 'Current: " + mmol.toFixed(1) + " mmol/L'");
}