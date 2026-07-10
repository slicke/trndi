(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)

unit trndi.strings;

interface

resourcestring
  // Menu items
RS_REFRESH = '⟳ Updated: <b>%s</b> <i>(Next update: %s)</i>';
RS_REFRESH_PLAIN = '⟳ Updated: %s (Next update: %s)';
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
RS_NO_BACKEND = 'No readings received.'+sLineBreak+sLineBreak+'Has your CGM synced recently? '+sLineBreak+'Is the remote server working?';
RS_LAST_RECIEVE = 'Last reading was %s (%s)';
RS_DAYS_AGO = '%d days ago';
RS_LAST_RECIEVE_NO = 'There''s no last reading to show';
RS_LAST_RECIEVE_AGE = 'Threshold: %d minutes';
RS_MISSING_LABEL = 'Label %s is missing!';
RS_MULTIPLE_ACCOUNTS = 'Trndi found multiple accounts. Please choose one for this instance';
RS_USER_CAPTION = '[%s] %s';
RS_UNKNOWN_TIME = 'Loading';
RS_NO_INTERNET = 'Your internet connection is not working, Trndi cannot fetch readings!';
RS_DNS_INTERNET_OK = 'Your internet connection is working!';
  // Range messages
RS_RANGE_EXPLANATION = 'In addition to high and low levels, you have set a personal range within "OK". You are now %s that range';
RS_OVER = 'over';
RS_UNDER = 'under';
RS_HIGH = 'HIGH';
RS_LOW = 'LOW';
RS_OFF_HI = 'Over range';
RS_OFF_LO = 'Under range';
RS_RANGE_COLOR = 'The entire window will now change color when out of range, instead of just displaying a colored notification in the top. Applies after the next sync.';
RS_TIR_M = 'This is your time in range for the last %s minutes';
RS_TIR_H1 = 'This is your time in range for the last hour and %s minutes';
RS_TIR_H = 'This is your time in range for the last %s hours and %s minutes';
RS_TIR_ROW2 = 'Your low limit is %.1f, high is %.1f';
RS_TIR_ROW3 = 'Your low range is %.1f, high is %.1f';
RS_DIFF = 'This is the change from the latest reading';

RS_SPEAK_HIGH = 'High';
RS_SPEAK_LOW = 'Low';
RS_SPEAK_GOOD = 'Good';
RS_SPEAK_GHIGH = 'Going high';
RS_SPEAK_GLOW = 'Going low';

RS_NO_BOOT_READING = 'No recent data available!';

RS_SERVICE_SYSINFO = '%s (%s)'#10'%s'#10'Default Separator: %s';
RS_SERVICE_ADJUST = 'Adjust: %.2f / Scale: %.2fx';
RS_SERVICE_DOT_ADJUST = 'Dot Adjustment';
RS_SERVICE_DOT_ADJUST_ADD = 'Add dot adjustment';
RS_SERVICE_DOT_ADJUST_DESC = 'You can enter plus or minus. Plus = down. 0 = neutral';

RS_SERVICE_PREDICT_UNABLE = 'Unable to predict: %s';
RS_SERVICE_PREDICTIONS = 'Predictions:';
RS_SERVICE_PREDICT_POINT = 'Reading %d: %.1f %s at %s';

RS_PREDICT_AMOUNT_CAPTION = 'Force predictions';
RS_PREDICT_AMOUNT_TITLE = 'Force a prediction';
RS_PREDICT_AMOUNT_DESC = 'Enter the amount of readings to predict (1-20)';

// Time messages
RS_OUTDATED_TIME = '%s (%d.%.2d min ago)';
RS_LATEST_READING = 'Latest Reading: Value = %.2f, Date = %s';

RS_RESTART_APPLY = 'Trndi must be restarted for settings to take effect';
RS_SETTINGS_SAVED = 'Settings saved and applied. Some settings may require a restart';
RS_QUIT_CAPTION =  'Exit Trndi?';
RS_QUIT_MINIMIZE_TITLE = 'Quit or Minimize?';
RS_QUIT_MINIMIZE = 'Would you like to minimize to the Dock, or close Trndi?';
RS_QUIT_MINIMIZE_WIN = 'Would you like Trndi to to hide in the Taskbar, or close?';
RS_QUIT_MSG = 'Quit the app?';
RS_LAST_UPDATE = '%d min';
RS_COMPUTE_FAILED_AGO = 'Long ago';
RS_FORCE_QUIT_SETUP = 'Trndi will now shut down to apply settings, please re-start it manually!';
RS_SETTINGS_SAVE = 'Save settings?';
RS_SETTINGS_SAVE_DESC = 'Would you like to save any changes made?';

RS_LO_PREDICT = 'Low Predicted in %d minutes!';
RS_HI_PREDICT = 'High Predicted in %d minutes!';

RS_LO_PREDICT_SOON = 'Low Predicted soon!';
RS_HI_PREDICT_SOON = 'High Predicted soon!';

RS_WARN_BG_HI = 'High blood sugar! Currently: %s';
RS_WARN_BG_LO = 'Low blood sugar! Currently: %s';

RS_WARN_BG_HI_TITLE = 'Blood sugar high';
RS_WARN_BG_LO_TITLE = 'Blood sugar low';

RS_MULTIUSER_BOX = 'Trndi found multiple accounts. Please choose one for this instance, or continue with the default account';
RS_MULTIUSER_BOX_TITLE = 'User select';

RS_FONT_ERROR = 'You''re missing the standard font "%s". Trndi will work as intended, but some icons may be missing and/or distorted!';
RS_TRNDI_GIHUB = 'Trndi on GitHub';

RS_MENU_SETTINGS = 'Settings';
RS_MENU_FORCE = 'Force update';
RS_MENU_HELP = 'Help';
RS_MENU_NAME = 'Glucose';

RS_RHISTORY = 'Reading history';
RS_RH_TITLE = 'Historical readings';
RS_RH_INFO = 'These are the messages currently held in Trndi''s log';
RS_RH_READING = 'Reading';
RS_RH_TIME = 'Time';
RS_RH_UNKNOWN = 'Unknown';

RS_HISTORY_ITEM = '» Selected Reading: %s'+sLineBreak+'Δ Difference: %s'+sLineBreak+'↔ Trend: %s'+sLineBreak+'📻 RSSI: %s'+sLineBreak+'𐩘 Noise: %s'+sLineBreak+'ℹ Source: %s'+sLineBreak+'💻 Device: %s';

RS_PREDICTIONS_UNAVAILABLE = 'Predictions unavailable';
RS_PREDICTIONS_STABLE = 'No change predicted →';

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

RS_EXTFAILED = 'The following extension file returned an error while loading: %s';

RS_INIT_TRNDI = 'Starting Trndi...';
RS_INIT_MEDIA = 'Starting Media Backend...';
RS_INIT_FONTS = 'Processing fonts...';
RS_INIT_LINUX = 'Attempting to adjust Linux overrides...';
RS_INIT_MAC = 'Attempting to adjust macOS overrides...';
RS_INIT_UI = 'Initializing UI...';
RS_INIT_PROFILES = 'Processing user profiles...';
RS_INIT_LOCALE = 'Setting up locale settings...';
RS_INIT_LICENSE = 'Verifying license...';
RS_INIT_UNIT = 'Determining glucose unit...';
RS_INIT_INITIAL = 'Preparing initial setup...';
RS_INIT_API = 'Initializing API...';
RS_INIT_CONNECT = 'Attempting to connect...';
RS_INIT_UX = 'Initializing UX...';
RS_INIT_CUSTOM_OVERRIDES = 'Initializing custom overrides...';
RS_INIT_FETCH = 'Fetching first reading...';
RS_INIT_FIRST = 'Processing first reading...';
RS_INIT_CHROMA = 'Initializing Chroma...';

RS_CLEANUP = 'Wait. Shutting down extensions engine...';
RS_CLEANUP_WAIT = 'Please wait up to %d seconds...';

RS_RIGHT_CLICK = 'The settings dialog will now open, in the future please right-click the reading to open the menu.';

RS_PREDICT = 'Your predicted future readings are shown on the lower-right-hand-side of the window; the difference from the last reading is shown in the middle';

RS_NODATA_ERROR = 'No data, error(?):';
RS_NODATA_NONE = '<none>';

RS_EXT_SHUTDOWN = 'Shutting down extensions...';
RS_SAFEMODE = 'Safe Mode Enabled';
RS_SAFEMODE_DESC = 'Trndi is running in <b>Safe Mode</b>!<ul><li>No start-up actions from extensions will run</li><li>You cannot use, and no actions will trigger from, extensions</li></ul><br>Extensions will load next run, unless you press <b>CTRL</b>!';

RS_API_NOTICE = 'Data source notice';
RS_ATTENTION_MISSING = 'Outdated Readings!';
RS_ATTENTION_MISSING_DESC = 'Readings are not fresh, have your phone lost connection to the sensor?';
RS_SENSOR_FAULT_SUSPECT = 'Sensor may be faulty: multiple abrupt reading jumps detected.';
RS_ATTENTION_SENSOR_FAULT = 'Possible sensor fault';
RS_ATTENTION_SENSOR_FAULT_DESC = 'Trndi detected repeated large jumps between consecutive readings. Verify with your official CGM app/device.';
RS_ALERT_SNOOZE = 'Snooze alerts';
RS_ALERT_SNOOZE_ACTIVE = 'Snoozed until %s';
RS_ALERT_SNOOZE_FOR = 'Alerts snoozed for %d minutes';
RS_ALERT_SNOOZE_OFF = 'Alert snooze disabled';
RS_ALERT_SNOOZE_RESUME = 'Resume alerts';
RS_ALERT_SNOOZE_NOT_ACTIVE = 'Snooze is not active';
RS_CONN_OK = 'OK';
RS_CONN_RETRYING = 'Retrying';
RS_CONN_AUTH_EXPIRED = 'Auth expired';
RS_CONN_RATE_LIMITED = 'Rate-limited';
RS_CONN_CLICK_FOR_DETAILS = 'Click for details';
RS_CONN_NO_DETAILS = 'No connection details available yet.';
RS_CONN_DETAILS = 'Connection status: %s'+sLineBreak+sLineBreak+'Details:'+sLineBreak+'%s';
RS_CONN_OK_INFO = 'Connection is currently healthy. Detailed diagnostics will appear here if a connectivity issue occurs.';

// EXT
RS_EXT_ERR = 'Extension Error';
RS_EXT_MSG = 'Extension Message';
RS_EXT_CONFIRM = 'Extension Confirmation';
RS_EXT_EVENT = 'Extension Event';
RS_EXT_WARN = 'Extension Warning';
RS_EXT_FILE = 'File "%s" not found';

RS_EXT_USER_INFO = 'User Information';

RS_TYPE_ERR_CAPT = 'A data type differs from what was expected';
RS_TYPE_ERR_DESC = 'The extension was stopped';
RS_TYPE_ERROR_MSG = 'A data type was expected, but another was found';
RS_PROM_ERR_CAPT = 'The asynchronous function %s failed to complete';

RS_NO_TRACE      = 'No stack trace available';
RS_UNKNOWN_ERR   = 'Unknown error';
RS_STACK_ERR_MSG  = 'Error: %s '#13#10'Stack: %s %s';
RS_LOG_RECEIVE  = 'Output through console.log has been received';
RS_LOG_DESC      = 'Trndi has captured data sent to console.log in a JavaScript extension';
RS_LOG_EMPTY_MSG  = '<No message provided>';
RS_LOG_NO_BUFFERED= 'No buffered console messages to show.';
RS_STACK_FAILED  = 'An error occurred, and the stacktrace could not be loaded. Showing backtrace.';
RS_DATA_TYPE_ERR  = 'Datatype %s was not expected, expected in function %s';
RS_DATA_TYPE_ERR_POS = 'Datatype %s was not expected, expected in function %s, parameter %d';
RS_DATA_TYOE_ERR_FUNC = 'Datatype "%s" was not expected, in function "%s". Expected "%s"';

RS_UX_RSSI = '📻 RSSI (Signal Strength): %d%%';
RS_UX_NOISE = '𐩘 Noise: %d';
RS_UX_DEVICE = '💻 Device: %s';
RS_UX_SENSOR_EXPIRY = '⏱️ Sensor: %s';
RS_UX_API = '⚙️ Trndi API Interface: %s';
RS_UX_TRANSMITTER_INFO = 'Transmitter information';

RS_REFRESH_PROMPT = 'Refresh?';
RS_FORCE_REFRESH = 'Forcefully refresh the reading?';
RS_FORCE_REFRESH_CACHED = 'Last update was %d seconds ago. Wait %d more seconds for fresh data, or force refresh now?';
RS_FORCE_REFRESH_DETAIL = 'Force refresh will bypass the cache and make an immediate API call. Waiting allows the system to use cached data efficiently.';

RS_UX_ANNOUNCE_ON = 'Announcing readings aloud';
RS_UX_ANNOUNCE_OFF = 'No longer announcing readings aloud';

RS_UX_DOT_SIZE =  'Dot Size';
RS_UX_CUSTOMIZE_DOT_SIZE = 'Customize dot size';
RS_UX_ENTER_DOT_SIZE= 'Enter a value, the dot size will multiply by this';

RS_UX_TREND_WINDOW = 'Trend window';
RS_UX_TREND_WINDOW_FMT = '%d min (%d dots)';

RS_SERVICE_DOT_COUNT = 'Set Dot Count';
RS_SERVICE_DOT_COUNT_TITLE = 'Number of trend dots';
RS_SERVICE_DOT_COUNT_DESC = 'Enter how many trend dots to show (1-%d).';

RS_UX_CHANGES_SAVE = 'Save?';
RS_UX_CHANGES_REMEMBER = 'Remember setting?';
RS_CHANGES_APPLY = 'Apply this value when Trndi starts?';

RS_UX_READING_HERE = 'Reading here: %s at %.2d:%.2d';
RS_UX_PREDICTION_HERE = 'Prediction here: %s at %.2d:%.2d';

RS_DATE_PICKER_CAPTION = 'Choose a date';
RS_DATE_PICKER_TITLE = 'Choose a date to show readings from';
RS_DATE_PICKER_DESC_LIMITED = '%s supports a max time of %d days';
RS_DATE_PICKER_DESC_UNLIMITED = '%s time limit depends on server configuration';
RS_DATE_PICKER_NO_READINGS = 'No readings returned for the selected date range.';
RS_DATE_PICKER_NO_READINGS_ERR = 'No readings returned: %s';

// Welcome wizard
RS_WIZARD_TITLE       = 'Welcome to Trndi';
RS_WIZARD_STEP_FMT    = 'Step %d of %d';
RS_WIZARD_BACK        = '← Back';
RS_WIZARD_NEXT        = 'Next →';
RS_WIZARD_FINISH      = 'Finish';
RS_WIZARD_SOURCE      = 'Data source:';
RS_WIZARD_TEST        = 'Test connection';
RS_WIZARD_TESTING     = 'Testing…';
RS_WIZARD_TEST_OK     = '✓ Connected!';
RS_WIZARD_TEST_FAIL   = '✗ Could not connect.';
RS_WIZARD_NEED_ADDR   = 'Please enter a server address or username.';
RS_WIZARD_UNIT_HEAD   = 'Choose your preferred glucose unit:';
RS_WIZARD_UNIT_MMOL   = 'mmol/L  (e.g. 5.5)';
RS_WIZARD_UNIT_MGDL   = 'mg/dL  (e.g. 100)';
RS_WIZARD_WELCOME_HEAD = 'Let''s get connected!';
RS_WIZARD_WELCOME_BODY =
  'Trndi shows your continuous glucose monitor readings right on your desktop.' +
  sLineBreak + sLineBreak +
  'This wizard will connect Trndi to your CGM data source. You will need your ' +
  'server address (or username) and API key / password.' +
  sLineBreak + sLineBreak +
  'All settings can be changed later via right-click → Settings.';
RS_WIZARD_TREND_HEAD  = 'Trend window';
RS_WIZARD_TREND_BODY  = 'Choose how many readings to show in the main graph.';
RS_WIZARD_TREND_30    = '30 min  (6 readings)';
RS_WIZARD_TREND_50    = '50 min  (10 readings)';
RS_WIZARD_TREND_90    = '90 min  (18 readings)';
RS_WIZARD_TREND_120   = '2 hours  (24 readings)';
RS_WIZARD_TREND_180   = '3 hours  (36 readings)';
RS_WIZARD_THRESH_HEAD = 'Alert thresholds';
RS_WIZARD_THRESH_BODY =
  'Set the glucose levels at which Trndi should alert you.' +
  sLineBreak + sLineBreak +
  'These defaults are a common starting point — adjust to match your personal targets.';
RS_WIZARD_THRESH_HI   = 'High alert';
RS_WIZARD_THRESH_LO   = 'Low alert';

implementation

end.
