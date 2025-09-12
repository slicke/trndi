unit trndi.strings;

interface

resourcestring
  // Menu items
RS_REFRESH = 'Updated: %s | Refreshing: %s';
RS_LIMIT_EXPLAIN_TITLE = 'Limit Explanation';
RS_LIMIT_EXPLAIN_TEXT = 'Hi = When BG is considered high'#10 +
  'Lo = When BG is considered low'#10#10 +
  'Ranges: Defines "desirable" levels within normal. Not supported by all backends';
RS_HI_LEVEL = 'Hi > %.1f';
RS_LO_LEVEL = 'Lo < %.1f';
RS_RANGE_HI = 'Range Hi > %.1f';
RS_RANGE_LO = 'Range Lo < %.1f';
RS_RANGE_HI_UNSUPPORTED = 'Hi range not supported by API';
RS_RANGE_LO_UNSUPPORTED = 'Lo range not supported by API';

  // Error messages
RS_SETUP = 'Setup';
RS_NO_BACKEND = 'No readings recieved. '#10'Has your CGM synced recently? '#10'Is the remote server working?';
RS_MISSING_LABEL = 'Label %s is missing!';
RS_MULTIPLE_ACCOUNTS = 'Trndi found multiple accounts. Please choose one for this instance';
RS_USER_CAPTION = '[%s] %s';
RS_UNKNOWN_TIME = 'Loading';

  // Range messages
RS_RANGE_EXPLANATION = 'In addition to high and low levels, you have set a personal range within "OK". You are now %s that range';
RS_OVER = 'over';
RS_UNDER = 'under';
RS_HIGH = 'HIGH';
RS_LOW = 'LOW';
RS_OFF_HI = 'Over range';
RS_OFF_LO = 'Under range';
RS_RANGE_COLOR = 'The entire window will now change color when out of range, instead of just displaying a colored notification in the top. Applies after the next sync.';
RS_TIR_M = 'This is your time in range for the last %d minutes';
RS_TIR_H1 = 'This is your time in range for the last hour and %d minutes';
RS_TIR_H = 'This is your time in range for the last %d hours and %d minutes';

RS_NO_BOOT_READING = 'No fresh data is available.'#10'Trndi will attemp to fetch data until recent values are available';

// Time messages
RS_OUTDATED_TIME = '%s (%d.%.2d ago)';
RS_LATEST_READING = 'Latest Reading: Value = %.2f, Date = %s';

RS_RESTART_APPLY = 'Trndi must be restarted for settings to take effect';
RS_QUIT_CAPTION =  'Exit Trndi?';
RS_QUIT_MSG = 'Quit the app?';
RS_LAST_UPDATE = '%d min';
RS_COMPUTE_FAILED_AGO = 'Long ago';
RS_FORCE_QUIT_SETUP = 'Trndi will now shut down to apply settings, please re-start it manually!';

RS_WARN_BG_HI = 'High blood sugar! Currently: %s';
RS_WARN_BG_LO = 'Low blood sugar! Currently: %s';

RS_MULTIUSER_BOX = 'Trndi found multiple accounts. Please choose one for this instance, or leave empty for default';
RS_MULTIUSER_BOX_TITLE = 'User select';

RS_FONT_ERROR = 'You''re missing the standard font "%s". Trndi will work as intended, but some icons may be missing and/or distorted!';
RS_TRNDI_GIHUB = 'Trndi on GitHub';

RS_RHISTORY = 'Reading history';
RS_RH_TITLE = 'Historical readings';
RS_RH_INFO = 'These are the messages currently held in Trndi''s log';
RS_RH_READING = 'Reading';
RS_RH_TIME = 'Time';
RS_RH_UNKNOWN = 'Unknown';

RS_HISTORY_ITEM = 'Selected Reading: %s | Difference: %s | Trend: %s | RSSI: %s | Noise: %s | Source: %s | Device: %s';

// Config box
RS_tpoCenter = 'Desktop Center';
RS_tpoBottomLeft = 'Bottom Left';
RS_tpoBottomRight = 'Bottom Right';
RS_tpoCustom = 'Last position';
RS_tpoTopRight = 'Top Right';
RS_noPlugins = 'Built Without Support';
RS_MULTINAME = 'Showing data for: "%s"';
RS_MULTINAME_DEF = 'Showing the standard user';

RS_SPLASH_LOADING = 'Loading Extension: ';
RS_SPLASH_LOADING_INIT  = 'Loading Extensions...';


// EXT
sExtErr = 'Extension Error';
sExtMsg = 'Extension Message';
sExtConfirm = 'Extension Confirmation';
sExtEvent = 'Extension Event';
sExtWarn = 'Extension Warning';
sExtFile = 'File "%s" not found';

sExtUserInfo = 'User Information';

sTypeErrCapt = 'A data type differes from what was expected';
sTypeErrDesc = 'The extension was stopped';
sTypeErrmsg = 'A data type was expected, but another was found';
sPromErrCapt = 'The asyncronous function %s failed to complete';

sNoTrace      = 'No stack trace available';
sUnknownErr   = 'Unknown error';
sStackErrMsg  = 'Error: %s '#13#10'Stack: %s %s';
sLogRecevive  = 'Output through console.log has been received';
sLogDesc      = 'Trndi has captured data sent to console.log in a JavaScript extension';
sStackFailed  = 'An error occurred, and the stacktrace could not be loaded. Showing backtrace.';
sDataTypeErr  = 'Datatype %s was not expected, expected in function %s';
sDataTypeErrPos = 'Datatype %s was not expected, expected in function %s, parameter %d';
sDataTypeErrFunc = 'Datatype "%s" was not expected, in function "%s". Expected "%s"';

sRSSI = 'RSSI (Signal Strength): %d%%';
sNoise = 'Noise: %d';
sDevice = 'Device: %s';

sRefrshQ = 'Refresh?';
sForceRefresh = 'Forcefully refresh the reading?';

sAnnounceOn = 'Announcing readings aloud';
sAnnounceOff = 'No longer announcing readings aloud';

sDotSize =  'Dot Size';
sCustomiseDotSize = 'Customize dot size';
sEnterDotSize= 'Enter a value, the dot size will multiply by this';

sChangesSave = 'Save?';
sChangesRemember = 'Remember setting?';
sChangesApply = 'Apply this value when Trndi starts?';

implementation

end.
