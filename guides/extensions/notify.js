/*
@name Notify
@copyright (c) slicke / No copyright
@description Sends a desktop notification via notify-send on high readings.
@perms exec
*/
function updateCallback(curr, mgdl, mmol, ts){
    if (mmol > 7)
        runCMD("notify-send 'High BG' 'Current: " + mmol.toFixed(1) + " mmol/L'");
}