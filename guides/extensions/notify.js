/* Notify extension for Nightscout
(c) slicke / No copyright
@perms exec
*/
function updateCallback(curr, mgdl, mmol, ts){
    if (mmol > 7)
        runCMD("notify-send 'High BG' 'Current: " + mmol.toFixed(1) + " mmol/L'");
}