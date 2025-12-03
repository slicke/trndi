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
RS_LAST_RECIEVE = 'Last reading was %s (%s)';
RS_DAYS_AGO = '%d days ago';
RS_LAST_RECIEVE_NO = 'There''s no last reading to show';
RS_LAST_RECIEVE_AGE = 'Threshold: %d minutes';
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
RS_TIR_M = 'This is your time in range for the last %d minutes'+#10+#10+'Your low limit is %.1f, high is %.1f'+#10+'Your low range is %.1f, high is %.1f';
RS_TIR_H1 = 'This is your time in range for the last hour and %d minutes'+#10+#10+'Your low limit is %.1f, high is %.1f'+#10+'Your low range is %.1f, high is %.1f';
RS_TIR_H = 'This is your time in range for the last %d hours and %d minutes'+#10+#10+'Your low limit is %.1f, high is %.1f'+#10+'Your low range is %.1f, high is %.1f';
RS_DIFF = 'This is the change from the latest reading';

RS_NO_BOOT_READING = 'No fresh data is available.'#10'Trndi will attemp to fetch data until recent values are available';

// Time messages
RS_OUTDATED_TIME = '%s (%d.%.2d min ago)';
RS_LATEST_READING = 'Latest Reading: Value = %.2f, Date = %s';

RS_RESTART_APPLY = 'Trndi must be restarted for settings to take effect';
RS_QUIT_CAPTION =  'Exit Trndi?';
RS_QUIT_MSG = 'Quit the app?';
RS_LAST_UPDATE = '%d min';
RS_COMPUTE_FAILED_AGO = 'Long ago';
RS_FORCE_QUIT_SETUP = 'Trndi will now shut down to apply settings, please re-start it manually!';
RS_SETTINGS_SAVE = 'Save settings?';
RS_SETTINGS_SAVE_DESC = 'Would you like to save any changes made?';

RS_WARN_BG_HI = 'High blood sugar! Currently: %s';
RS_WARN_BG_LO = 'Low blood sugar! Currently: %s';

RS_WARN_BG_HI_TITLE = 'Blood sugar high';
RS_WARN_BG_LO_TITLE = 'Blood sugar low';

RS_MULTIUSER_BOX = 'Trndi found multiple accounts. Please choose one for this instance, or continue with the default account';
RS_MULTIUSER_BOX_TITLE = 'User select';

RS_FONT_ERROR = 'You''re missing the standard font "%s". Trndi will work as intended, but some icons may be missing and/or distorted!';
RS_TRNDI_GIHUB = 'Trndi on GitHub';

RS_RHISTORY = 'Reading history';
RS_RH_TITLE = 'Historical readings';
RS_RH_INFO = 'These are the messages currently held in Trndi''s log';
RS_RH_READING = 'Reading';
RS_RH_TIME = 'Time';
RS_RH_UNKNOWN = 'Unknown';

RS_HISTORY_ITEM = 'Selected Reading: %s'+sLineBreak+'Difference: %s'+sLineBreak+'Trend: %s'+sLineBreak+'RSSI: %s'+sLineBreak+'Noise: %s'+sLineBreak+'Source: %s'+sLineBreak+'Device: %s';

RS_PREDICTIONS_UNAVAILABLE = 'Predictions unavailable';

RS_CUSTOM_DOTS = 'Custom Size [%.2f]';
// Config box
RS_tpoCenter = 'Desktop Center';
RS_tpoBottomLeft = 'Bottom Left';
RS_tpoBottomRight = 'Bottom Right';
RS_tpoCustom = 'Last position';
RS_tpoTopRight = 'Top Right';
RS_noPlugins = 'Built Without Support';
RS_MULTINAME = 'Showing data for: "%s"';
RS_MULTINAME_NAMED = 'Showing data for: %s (username "%s")';
RS_MULTINAME_DEF = 'Showing the standard user';
RS_MULTINAME_DEF_NAMED = 'Showing %s (standard user)';

RS_SPLASH_LOADING = 'Loading Extension: ';
RS_SPLASH_LOADING_INIT  = 'Loading Extensions...';

RS_CLEANUP = 'Wait. Shutting down extensions engine...';
RS_CLEANUP_WAIT = 'Please wait up to 20 seconds...';

RS_RIGHT_CLICK = 'The settings dialog will now open, in the future please right-click the reading to open the menu.';

RS_PREDICT = 'These are your predicted readings; Time-difference: predicted-reading';

RS_SAFEMODE = 'Safe Mode Enabled';
RS_SAFEMODE_DESC = 'Trndi is running in <b>Safe Mode</b>!<ul><li>No start-up actions from extensions will run</li><li>You cannot use, and no actions will trigger from, extensions</li></ul><br>Extensions will load next run, unless you press <b>CTRL</b>!';

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
sForceRefreshCached = 'Last update was %d seconds ago. Wait %d more seconds for fresh data, or force refresh now?';
sForceRefreshDetail = 'Force refresh will bypass the cache and make an immediate API call. Waiting allows the system to use cached data efficiently.';

sAnnounceOn = 'Announcing readings aloud';
sAnnounceOff = 'No longer announcing readings aloud';

sDotSize =  'Dot Size';
sCustomiseDotSize = 'Customize dot size';
sEnterDotSize= 'Enter a value, the dot size will multiply by this';

sChangesSave = 'Save?';
sChangesRemember = 'Remember setting?';
sChangesApply = 'Apply this value when Trndi starts?';

sReadingHere = 'Reading here: %s at %.2d:%.2d';

sProblematicFullscreen = 'Your Window Manager might have issues with full screen modes. You may need to toggle "no borders" for a complete full screen!';

implementation

end.
