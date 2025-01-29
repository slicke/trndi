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
RS_NO_BACKEND = 'No readings recieved. There might be a problem with the backend, or you might not have any recent readings';
RS_MISSING_LABEL = 'Label %s is missing!';
RS_MULTIPLE_ACCOUNTS = 'Trndi found multiple accounts. Please choose one for this instance';
RS_USER_CAPTION = '[%s] %s';

  // Range messages
RS_RANGE_EXPLANATION = 'In addition to high and low levels, you have set a personal range within "OK". You are now %s that range';
RS_OVER = 'over';
RS_UNDER = 'under';

// Time messages
RS_OUTDATED_TIME = '%s (%d.%.2d ago)';
RS_LATEST_READING = 'Latest Reading: Value = %.2f, Date = %s';

RS_RESTART_APPLY = 'Trndi must be restarted for settings to take effect';
RS_QUIT_CAPTION =  'Exit Trndi?';
RS_QUIT_MSG = 'Quit the app?';
RS_LAST_UPDATE = '%d min';
RS_FORCE_QUIT_SETUP = 'Trndi will now shut down to apply settings, please re-start it manually!';

RS_WARN_BG_HI = 'High blood sugar! Currently: %s';
RS_WARN_BG_LO = 'Low blood sugar! Currently: %s';

implementation

end.
