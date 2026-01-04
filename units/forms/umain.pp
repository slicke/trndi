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

unit umain;

{$I ../../inc/native.inc}

{$ifdef Darwin}
{$modeswitch objectivec1}
{$endif}


interface

uses
trndi.strings, LCLTranslator, Classes, Menus, SysUtils, Forms, Controls,
Graphics, Dialogs, StdCtrls, ExtCtrls,
trndi.api.dexcom, trndi.api.nightscout, trndi.api.nightscout3, trndi.types,
Math, DateUtils, FileUtil, LclIntf, TypInfo, LResources,
slicke.ux.alert, slicke.ux.native, usplash, Generics.Collections, trndi.funcs,
Trndi.native.base, trndi.shared, buildinfo, fpjson, jsonparser,
SystemMediaController,
{$ifdef TrndiExt}
trndi.Ext.Engine, trndi.Ext.jsfuncs, trndi.ext.promise, mormot.core.base,
{$endif}
{$ifdef Darwin}
CocoaAll, MacOSAll,
Sockets,
BaseUnix,
netdb,
{$endif}
{$ifdef LINUX}
kdebadge,
Sockets,
netdb,
{$endif}
{$ifdef Windows}
winsock,
{$endif}
LazFileUtils, uconf, trndi.native, Trndi.API,
trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug_custom, trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_missing, trndi.api.debug_perfect, trndi.api.debug_firstmissing,{$endif}
{$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
StrUtils, TouchDetection, ufloat, uhistorygraph, LCLType, trndi.webserver.threaded, RazerChromaFactory, RazerChroma;

{** Main application unit exposing the primary UI and helpers for the
  Trndi application. This unit defines the `TfBG` form which handles
  presentation of CGM readings, predictions, alerts, and integrations
  with backends and the built-in web server.

  Keep PasDoc blocks in the interface section to ensure they are
  included in generated documentation.
}
type
TFloatIntDictionary = specialize TDictionary<single, integer>;
  // Specialized TDictionary
{$ifdef DARWIN}
TDotControl = TLabel;
{$else}
TDotControl = TPaintBox;
{$endif}
  // Procedures which are applied to the trend drawing
TTrendProc = procedure(l: TDotControl; c, ix: integer) of object;
TTrendProcLoop = procedure(l: TDotControl; c, ix: integer;
  ls: array of TDotControl) of object;
TrndiPos = (tpoCenter = 0, tpoBottomLeft = 1, tpoBottomRight = 2,
  tpoCustom = 3, tpoTopRight = 4);
TPONames = array[TrndiPos] of string;

var
TrndiPosNames: TPONames = (RS_tpoCenter, RS_tpoBottomLeft,
  RS_tpoBottomRight, RS_tpoCustom, RS_tpoTopRight);
const
  // Public timing constants used across the unit/interface
  MILLIS_PER_MINUTE = 60000; // Milliseconds in a minute
  CLOCK_INTERVAL_MS = 20000; // Default clock interval used for the clock tick

type
  { TfBG }

{$ifdef Darwin}
{ Custom NSApplicationDelegate class }
TMyAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
public
  function applicationShouldHandleReopen_hasVisibleWindows(
    sender: NSApplication; hasVisibleWindows: Boolean): Boolean; message 'applicationShouldHandleReopen:hasVisibleWindows:';
  function applicationDockMenu(sender: NSApplication): NSMenu; message 'applicationDockMenu:';
    procedure miSettingsMacClick(sender: id); message 'miSettings:';
end;

{** Darwin-specific declarations above }
{$endif}

{** Application main form.
  The `TfBG` class is responsible for the primary user interface: displaying
  current glucose readings, managing alerts, trends/dots, and exposing
  small helpers used by the application web server for remote clients.
  Important UI lifecycle methods (e.g., FormCreate, FormShow, FormDestroy)
  are documented to clarify their role during initialization and shutdown.
}
TfBG = class(TForm)
  apMain: TApplicationProperties;
  bSettings: TButton;
  bTouchMenu: TButton;
  bTouchSettings: TButton;
  bMenuPanelClose: TButton;
  bTouchFull: TButton;
  lPredict: TLabel;
  miDNS: TMenuItem;
  miDotSmall: TMenuItem;
  miPredict: TMenuItem;
  pnTouchContents: TPanel;
  pnTouchMenu: TPanel;
  pnWarnlast: TLabel;
  lRef: TLabel;
  lDot10: TDotControl;
  lDot2: TDotControl;
  lDot3: TDotControl;
  lDot4: TDotControl;
  lDot5: TDotControl;
  lDot6: TDotControl;
  lDot7: TDotControl;
  lDot8: TDotControl;
  lDot9: TDotControl;
  lMissing: TLabel;
  lTir: TLabel;
  lAgo: TLabel;
  miADotAdjust: TMenuItem;
  miDotsInView: TMenuItem;
  miExit: TMenuItem;
  miCustomDots: TMenuItem;
  miDebugBackend: TMenuItem;
  miDotVal: TMenuItem;
  miATouchAuto: TMenuItem;
  miATouchNo: TMenuItem;
  miATouchYes: TMenuItem;
  miASystemInfo: TMenuItem;
  miADots: TMenuItem;
  miATouch: TMenuItem;
  miAdvanced: TMenuItem;
  lDot1: TDotControl;
  misep: TMenuItem;
  miSplit6: TMenuItem;
  miSplit5: TMenuItem;
  miAnnounce: TMenuItem;
  miDotHuge: TMenuItem;
  miDotBig: TMenuItem;
  miDotNormal: TMenuItem;
  miDotSize: TMenuItem;
  miAlternate: TMenuItem;
  miHistory: TMenuItem;
  miClock: TMenuItem;
  miRangeColor: TMenuItem;
  miPref: TMenuItem;
  miFloatOn: TMenuItem;
  pnOffReading: TPanel;
  pnWarning: TPanel;
  pnMultiUser: TPanel;
  pnOffRangeBar: TPanel;
  Separator1: TMenuItem;
  miBorders: TMenuItem;
  miFullScreen: TMenuItem;
  miOnTop: TMenuItem;
  miRefresh: TMenuItem;
  miSplit4: TMenuItem;
  miLimitExplain: TMenuItem;
  miSplit3: TMenuItem;
  miRangeLo: TMenuItem;
  miRangeHi: TMenuItem;
  miSplit2: TMenuItem;
  miLO: TMenuItem;
  miHi: TMenuItem;
  miInfo: TMenuItem;
  miSplit1: TMenuItem;
  miForce: TMenuItem;
  pnOffRange: TPanel;
  lArrow: TLabel;
  lDiff: TLabel;
  lVal: TLabel;
  miSettings: TMenuItem;
  pmSettings: TPopupMenu;
  mSplit5: TMenuItem;
  Separator2: TMenuItem;
  Separator4: TMenuItem;
  tAgo: TTimer;
  tClock: TTimer;
  tPing: TTimer;
  tSetup: TTimer;
  tInit: TTimer;
  tSwap: TTimer;
  tResize: TTimer;
  tMissed: TTimer;
  tTouch: TTimer;
  tMain: TTimer;
  mi24h: TMenuItem;
    {** Recompute and apply layout offsets for all graph elements.
      This adjusts trend dots, labels and other elements when the UI size or
      dot-count changes to keep everything visually aligned.
     }
    procedure AdjustGraph;
    procedure bMenuPanelCloseClick(Sender: TObject);
  procedure bSettingsClick(Sender: TObject);
  procedure bTouchFullClick(Sender: TObject);
  procedure bTouchMenuClick(Sender: TObject);
  procedure bTouchSettingsClick(Sender: TObject);
  procedure fbReadingsDblClick(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormClick(Sender: TObject);
  procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  {** Initialize UI state, load configuration, start any required services,
      and prepare connections to backends and extensions.
      Called by the framework when the form instance is created.
   }
  procedure FormCreate(Sender: TObject);
  procedure FormDblClick(Sender: TObject);
  {** Shutdown and cleanup all resources, including timers, web server, and
      persistent connections. Called when the form is destroyed.
   }
  procedure FormDestroy(Sender: TObject);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure DotPaint(Sender: TObject);
  procedure lDiffClick(Sender: TObject);
  procedure lPredictClick(Sender: TObject);
  procedure miDNSClick(Sender: TObject);
  procedure miDotNormalDrawItem(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AState: TOwnerDrawState);
  procedure miDotNormalMeasureItem(Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: integer);
  procedure miDotsInViewClick(Sender: TObject);
  procedure miDotSmallClick(Sender: TObject);
  procedure miExitClick(Sender: TObject);
  procedure miCustomDotsClick(Sender: TObject);
  procedure miATouchAutoClick(Sender: TObject);
  procedure miADotAdjustClick(Sender: TObject);
  procedure miADotScaleClick(Sender: TObject);
  procedure miADotsClick(Sender: TObject);
  procedure miASystemInfoClick(Sender: TObject);
  procedure miATouchClick(Sender: TObject);
  procedure miATouchNoClick(Sender: TObject);
  procedure miATouchYesClick(Sender: TObject);
  procedure miDebugBackendClick(Sender: TObject);
  procedure miPredictClick(Sender: TObject);
  procedure pmSettingsClose(Sender: TObject);
  procedure pnWarningClick(Sender: TObject);
  procedure pnWarningPaint(Sender: TObject);
  procedure speakReading;
  procedure FormMouseLeave(Sender: TObject);
  procedure FormMouseMove(Sender: TObject;{%H-}Shift: TShiftState; X, Y: integer);
  procedure FormResize(Sender: TObject);
  {** Called when the form becomes visible after creation; performs final
      placement operations and triggers initial UI refreshes once all controls
      are initialized and loaded.
   }
  procedure FormShow(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure lAgoClick(Sender: TObject);
  procedure lArrowClick(Sender: TObject);
  procedure lDiffDblClick(Sender: TObject);
  procedure lDot7DblClick(Sender: TObject);
  procedure lgMainClick(Sender: TObject);
  procedure lTirClick(Sender: TObject);
  procedure lValClick(Sender: TObject);
  procedure lValDblClick(Sender: TObject);
  procedure lValMouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
  procedure lValStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
  procedure miAnnounceClick(Sender: TObject);
  procedure miAlternateClick(Sender: TObject);
  procedure miClockClick(Sender: TObject);
  procedure miDotNormalClick(Sender: TObject);
  procedure miFloatOnClick(Sender: TObject);
  procedure mi24hClick(Sender: TObject);
  procedure miHistoryClick(Sender: TObject);
  procedure miRangeColorClick(Sender: TObject);
  procedure miBordersClick(Sender: TObject);
  procedure miForceClick(Sender: TObject);
  procedure miLimitExplainClick(Sender: TObject);
  procedure miOnTopClick(Sender: TObject);
  procedure miSettingsClick(Sender: TObject);
  procedure onTrendClick(Sender: TObject);
  procedure pnOffReadingPaint(Sender: TObject);
  procedure pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: integer);
  procedure pmSettingsPopup(Sender: TObject);
  procedure pnMultiUserClick(Sender: TObject);
  procedure pnOffRangeClick(Sender: TObject);
  procedure tAgoTimer(Sender: TObject);
  procedure tClockTimer(Sender: TObject);
  procedure tEdgesTimer(Sender: TObject);
  procedure tInitTimer(Sender: TObject);
  procedure tPingTimer(Sender: TObject);
  procedure tResizeTimer(Sender: TObject);
  procedure tMainTimer(Sender: TObject);
  procedure tMissedTimer(Sender: TObject);
  procedure tSetupTimer(Sender: TObject);
  procedure tSwapTimer(Sender: TObject);
  procedure tTouchTimer(Sender: TObject);
  procedure TfFloatOnHide(Sender: TObject);
private
  FStoredWindowInfo: record // Saved geometry and window state for restore/toggle
    Left, Top, Width, Height: integer;
    WindowState: TWindowState;
    BorderStyle: TFormBorderStyle;
    FormStyle: TFormStyle;
    Initialized: boolean;
    end;
  titlecolor: boolean;
  FShuttingDown: boolean; // Flag to prevent recursive shutdown calls

    // Performance optimization fields
  FCachedTextWidth: integer;
  FCachedTextHeight: integer;
  FCachedFontSize: integer;
  FCachedFontName: string;
  FLastReadingsHash: cardinal; // Hash of last readings for change detection
  FLastAPICall: TDateTime; // Timestamp of the last successful API call
  FCachedReadings: array of BGReading; // Readings saved from last fetch
  FLastUIColor: TColor;
  FLastUICaption: string;
  FLastTir: string;
  FLastTirColor: TColor;

    // Array to hold references to lDot1 - lDot10
  TrendDots: array[1..10] of TDotControl;
  multi: boolean; // Multi user
  multinick: string;
  MediaController: TSystemMediaController;
  FWebServer: TObject; // TTrndiWebServer - using TObject to avoid circular dependency
  tWebServerStart: TTimer;

  Chroma: TRazerChromaBase;

  function dotsInView: integer;
  function setColorMode: boolean;
  function setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
  function setSingleColorMode: boolean;
  procedure SetLang;
  procedure fixWarningPanel;
  procedure showWarningPanel(const message: string;
    clearDisplayValues: boolean = false);
  procedure CalcRangeTime;
  function GetValidatedPosition: TrndiPos;
    {** Acquire the latest reading(s) and process them for display.
        @param(boot  Treat as initial load to bypass some caches.)
        @returns(True when the UI should be refreshed based on new values.)
     }
  function updateReading(boot: boolean = false): boolean;
  {** Map incoming readings to the visual trend slots. This function sorts and
      anchors readings to a fixed grid (5-minute intervals) and updates the
      state of `TrendDots` accordingly.
      @param(Readings The array of BGReading to map.)
   }
  procedure PlaceTrendDots(const Readings: array of BGReading);
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TDotControl; c, ix: integer; {%H-}ls: array of TDotControl);
  procedure HideDot(l: TDotControl; {%H-}c, {%H-}ix: integer);
  procedure showDot(l: TDotControl; {%H-}c, {%H-}ix: integer);
  procedure ResizeDot(l: TDotControl; {%H-}c, ix: integer);
  procedure initDot(l: TDotControl; c, ix: integer);
  procedure ExpandDot(l: TDotControl; c, ix: integer);
  procedure CreateTrendDots;
  procedure placeForm;
  {** Analyze a generated glyph bitmap for the dot character to compute a
      vertical offset that aligns limit lines with the visual center of
      a drawn dot across widgetset/font differences.
      @returns(integer offset in pixels.)
   }
  function CalculateDotVisualOffset: integer;

    // Helper methods for update procedure
  {** Fetch readings from the configured data source(s) and validate them.
      The resulting values are stored in `FCachedReadings` on success.
      @returns(True if a valid dataset was retrieved.)
   }
  function FetchAndValidateReadings: boolean;
  {** Under-the-hood implementation of FetchAndValidateReadings.
      @param(ForceRefresh  Force bypass of any caching.)
      @returns(True on successful fetch/validation.)
   }
  function DoFetchAndValidateReadings(const ForceRefresh: boolean): boolean;
    // Common implementation
  {** Process the current reading and update any internal state derived from it
      (e.g., predicted values, cached metadata). This is a small helper used
      by the main update loop to keep logic modular.
   }
  procedure ProcessCurrentReading;
  function IsDataFresh: boolean;
  {** Configure the timer for the next update based on the last reading time.
      This helps reduce unnecessary polling by setting intelligent intervals
      based on recency and backend requirements.
   }
  procedure SetNextUpdateTimer(const LastReadingTime: TDateTime);
  {** Apply visual changes following the latest readings, such as redraws and
      color/label updates depending on thresholds and state.
   }
  procedure UpdateUIBasedOnGlucose;
  {** Perform deferred work required after the main UI update chain has finished.
   }
  procedure CompleteUIUpdate;
  {** Final housekeeping after the UI update pipeline completes. Ensures that
      timers, overlays and other stateful UI pieces are synchronized after
      a new reading or manual refresh.
   }
  procedure FinalizeUIUpdate;
  procedure HandleHighGlucose(const {%H-}reading: BGReading);
  procedure HandleLowGlucose(const {%H-}reading: BGReading);
  procedure HandleNormalGlucose(const reading: BGReading);
  procedure UpdateOffRangePanel(const Value: double);
  procedure DisplayLowRange;
  procedure DisplayHighRange;
  {** Called when the update flow is completed to set final badges, floating
      window and other OS integrations. Ensures internal caches are updated
      so that redundant repaints are avoided until next change.
   }
  procedure FinalizeUpdate;
  procedure UpdateFloatingWindow;
  procedure UpdateUIColors;
  {** Compute an appropriate foreground (text) color for the provided background.
      Uses lightness heuristics to choose whether to darken or lighten the
      base color and returns a safe contrasting foreground color.
      @param(BgColor The background color to calculate contrast for.)
      @returns(TColor suitable for text over the given background.)
   }
  function GetTextColorForBackground(const BgColor: TColor;
    const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
  {** Adjust a base color so that it remains visible/readable against a
      specified background color by darkening or lightening accordingly.
      @param(PreferLighter Try to prefer a lighter color if set True.)
      @returns(Adjusted foreground color.)
   }
  function GetAdjustedColorForBackground(const BaseColor: TColor;
    const BgColor: TColor; const DarkenFactor: double = 0.6;
    const LightenFactor: double = 0.4; const PreferLighter: boolean = false): TColor;

    // Web server methods
    {** Initialize and start the integrated web server which provides basic
      HTTP endpoints for reading data and predictions.
     }
  procedure StartWebServer;
    {** Stop and tear down the integrated web server.
     }
  procedure StopWebServer;
    {** Return the current readings formatted as BGResults for use by the web API.
      @returns(Array of BGReading representing the current reading(s).)
     }
  function GetCurrentReadingForWeb: BGResults;
    {** Return predictions formatted as BGResults for web clients.
      Useful for simple web-based display of short-term predictions.
     }
  function GetPredictionsForWeb: BGResults;
    {** Indicates whether the web server is active and serving connections.
      @returns(True when a server is running.)
     }
  function WebServerActive: boolean;
  procedure tWebServerStartTimer(Sender: TObject);

  {** Refresh trend-related UI elements like labels and trend markers.
      Called after readings are processed to ensure the trend visuals match
      the calculated BGTrend values.
   }
  procedure UpdateTrendElements;
  {** Refresh labels and menu captions that display API-derived thresholds
      and other backend metadata (e.g., cgmHi/cgmLo values).
   }
  procedure UpdateApiInformation;
  {** Recompute layout and sizes of UI elements when the form size or scale
      changes. This includes updating of panels, labels and dot metrics.
   }
  procedure ResizeUIElements;
  {** Iterate over all trend dots and update their position and visibility
      based on the data in `TrendDots[].Hint`. This only adjusts visuals and
      doesn't fetch data from backends.
   }
  procedure UpdateTrendDots;
  {** Scale a TLabel font size to fit within its bounds using a binary search.
      This utility is used across many UI labels to ensure readable and
      consistent visual sizes across different screen resolutions.
   }
  procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
    customTl: TTextLayout = tlCenter);

    // Performance optimization methods
  {** Compute a simple hash of an array of readings used for change detection
      to avoid unnecessary UI updates when data didn't change.
   }
  function CalculateReadingsHash(const Readings: array of BGReading): cardinal;
  {** Determine whether the UI needs to refresh by comparing cached state
      to new candidate state (colors, captions, TIR). This reduces flicker
      and expensive redraws.
   }
  function ShouldUpdateUI(const NewColor: TColor; const NewCaption: string;
    const NewTIR: string; const NewTIRColor: TColor): boolean;
  {** Store the current UI state values for later comparisons in ShouldUpdateUI.
   }
  procedure CacheUIState(const UIColor: TColor; const UICaption: string;
    const UITir: string; const UITirColor: TColor);
  function FetchAndValidateReadingsForced: boolean;
    // Force fresh API call bypassing cache

  procedure HandleLatestReadingFreshness(const LatestReading: BGReading;
    CurrentTime: TDateTime);
  procedure ProcessTimeIntervals(const SortedReadings: array of BGReading;
    CurrentTime: TDateTime);
  {** Update a specific UI trend label (slot) to reflect a BGReading.
      Sets hint, caption, tag and positions the label according to its value.
      @param(SlotIndex Index of the visual slot to update (0=rightmost/most recent)).
      @param(Reading The BGReading to display.)
      @returns(True when the slot was successfully updated.)
   }
  function UpdateLabelForReading(SlotIndex: integer;
    const Reading: BGReading): boolean;
  {** Choose a color to display for a reading based on threshold configuration.
      This uses the current API's `cgmLo/cgmHi` and `cgmRangeLo/cgmRangeHi` to
      map a reading value to a foreground color suitable for text/labels.
   }
  function DetermineColorForReading(const Reading: BGReading): TColor;
  {** Toggle full-screen mode; handles saving/restoring window placement and
      adjusts window border/style for the requested state. Platform-specific
      behaviors (macOS) are delegated to ToggleFullscreenMac.
   }
  procedure DoFullScreen;
  {** Fetch and display short-term predictions (e.g., 5/10/15 minute values)
      in `lPredict`. Predictions are optional and controlled by user settings;
      when unavailable, the label is hidden.
   }
  procedure UpdatePredictionLabel;
  {$ifdef DARWIN}
  procedure ToggleFullscreenMac;
  {$endif}

  {$ifdef TrndiExt}
  {** Load and initialize optional extensions engine and JS functions.
      This is only compiled when `TrndiExt` is enabled and will create the
      extension engine instance and register callbacks used by UI and APIs.
   }
  procedure LoadExtensions;
  {$endif}

    // FormCreate refactored methods
  {$ifdef DARWIN}
  procedure InitializePlatformMenus;
  {$endif}
  {$ifdef X_LINUXBSD}
  procedure InitializeLinuxPlatform;
  {$endif}
  procedure InitializeUIComponents;
  procedure InitializeSplashScreen;
  procedure LoadUserProfile;
  procedure CheckAndAcceptLicense;
  function InitializeAPI: boolean;
  {** Check GitHub releases for newer versions of Trndi and optionally notify
      the user. When `ShowUpToDateMessage` is True, a message will be shown
      when the application is already current.
   }
  procedure CheckForUpdates(ShowUpToDateMessage: boolean = false);
public
  firstboot: boolean;
    {** Generic application exception handler used for reporting unhandled
      exceptions during runtime to a unified error dialog.
     }
    procedure AppExceptionHandler(Sender: TObject; {%H-}E: Exception);
    procedure onGH({%H-}Sender: TObject);
    {** Return the most recent reading (the newest element in `bgs`).
      Caller should ensure `bgs` is not empty (use `tryLastReading` first).
     }
    function lastReading: BGReading;
    {** Safely get the most recent reading as an out parameter.
      @returns(True when a reading was present, False when `bgs` is empty.)
     }
    function tryLastReading(out bg: BGReading): boolean;
end;


{$ifdef DARWIN}
function CFStringCreateWithUTF8String(const utf8Str: pansichar): CFStringRef; external name '_CFStringCreateWithUTF8String';
{$endif}

var
customTitlebar: boolean = true;
clockInterval: integer = CLOCK_INTERVAL_MS;
clockDisplay: integer = 5000;
fSplash: TfSplash;
native: TrndiNative;
{$ifdef X_LINUXBSD}
isWSL : boolean = false;
{$endif}
applocale: string;
dotscale: single = 1;
badge_adjust: single = 0;
tir_icon: boolean = true;
highAlerted: boolean = false; // A high alert is active
lowAlerted: boolean = false; // A low alert is active
perfectTriggered: boolean = false; // A perfect reading is active
PaintRange: boolean = true;
PaintRangeCGMRange: boolean = true; // Show cgmRangeLo/cgmRangeHi inner threshold lines
PaintRangeLines: boolean = false;
PredictGlucoseReading: boolean = false;
PredictShortMode: boolean = false;
// 'small' or 'big' to control single-arrow prediction size
PredictShortSize: integer = 1; // 1=small, 2=medium, 3=big
  // Show threshold lines (if false, only filled areas are drawn)
semiTouchMode: boolean = false; // Disables some touch elements while on touch
{$ifdef darwin}
MacAppDelegate: TMyAppDelegate;
upMenu: TMenuItem;
DOT_OFFSET_RANGE: integer = 0; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
{$ifdef windows}
DOT_OFFSET_RANGE: integer = 3; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
{$ifdef X_LINUXBSD}
DOT_OFFSET_RANGE: integer = -15; // Fine-tune vertical alignment of threshold lines with dots
{$endif}

var
last_popup: TDateTime = 0;
bg_alert: boolean = false;
  // If the BG is high/low since before, so we don't spam notifications
placed: boolean = false; // If the window has been placed at setup

username: string = '';
lastup: tdatetime;
  // Colors (b)lood(g)lucose (c)olor XX
  // In range
bg_color_ok: TColor = $0000DC84;
bg_color_ok_txt: TColor = $00F2FFF2;
  // Hi
bg_color_hi: TColor = $0007DAFF;
bg_color_hi_txt: TColor = $000052FB;
  // Low
bg_color_lo: TColor = $00FFBE0B;
bg_color_lo_txt: TColor = $00FFFEE9;

  // Personal hi
bg_rel_color_lo: TColor = $00A859EE;
bg_rel_color_lo_txt: TColor = $002D074E;
  // Personal low
bg_rel_color_hi: TColor = $0072C9DE;
bg_rel_color_hi_txt: TColor = $001C6577;
  // When the TIR is bad
bad_tir: integer = 5;
  // WHen the TIR is good
good_tir: integer = 75;

tir_bg: TColor = -1;
tir_custom_bg: TColor = -1;

fBG: TfBG;
api: TrndiAPI;
un: BGUnit = BGUnit.mmol;
bgs: BGResults;
{$ifdef TrndiExt}
jsFuncs: TJSfuncs;
{$endif}
badge_width: double = 0.8;
badge_font: integer = 8;

  // Touch screen
StartTouch: TDateTime;
IsTouched: boolean;
HasTouch: boolean;
HasMultiTouch: boolean;
touchHelper: TTouchDetector;

privacyMode: boolean = false;

  // Handle dragging on window
DraggingWin: boolean;
PX, PY: integer;

{$ifdef X_LINUXBSD}
IsRaspberry: boolean;
{$endif}

implementation

{$R *.lfm}

{$I ../../inc/tfuncs.inc}
{$I ../../inc/umain_ext.inc}
{$I ../../inc/umain_helpers.inc}
{$I ../../inc/umain_init.inc}

const
  MIN_REFRESH_INTERVAL_MS = 120000; // 2 minutes
  REFRESH_RESYNC_BUFFER_MS = 15000; // Additional buffer to allow backend sync
  BADGE_FLASH_DURATION_HIGH_MS = 15000;
  BADGE_FLASH_REPEAT_DELAY_HIGH_MS = 450;
  BADGE_FLASH_DURATION_LOW_MS = 20000;
  BADGE_FLASH_REPEAT_DELAY_LOW_MS = 400;
  BADGE_FLASH_DURATION_OK_MS = 6000;
  BADGE_FLASH_REPEAT_DELAY_OK_MS = 500;

function CurrentHistoryGraphPalette: THistoryGraphPalette;
begin
  Result.Range := bg_color_ok;
  Result.RangeHigh := bg_rel_color_hi;
  Result.RangeLow := bg_rel_color_lo;
  Result.High := bg_color_hi;
  Result.Low := bg_color_lo;
  Result.Unknown := RGBToColor(180, 180, 180);
end;

procedure TfBG.FormDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

function TfBG.tryLastReading(out bg: BGReading): boolean;
begin
  Result := (bgs <> nil) and (length(bgs) > 0);
  if Result then
    bg := lastReading;
end;

function TfBG.lastReading: BGReading;
begin
  try
    Result := bgs[Low(bgs)];
  finally
  end;
end;

procedure TfBG.FormDestroy(Sender: TObject);
begin
  // Ensure shutdown flag is set
  FShuttingDown := true;

  // Stop web server first to prevent callbacks during shutdown
  StopWebServer;


  if assigned(chroma) then
    chroma.free;

  // These should already be freed in FormClose, but check just to be safe
  if assigned(native) then
  begin
    native.Free;
    native := nil;
  end;
  if assigned(api) then
  begin
    api.Free;
    api := nil;
  end;

  // Note: TTrndiExtEngine.ReleaseInstance is already called in FormClose
  // The extension engine should already be shut down properly
end;

procedure TfBG.speakReading;
begin
  if not privacyMode then
    native.Speak(lVal.Caption)
  else
    case lastReading.level of
    trndi.types.BGHigh:
      native.speak('High');
    trndi.types.BGLOW:
      native.speak('Low');
    trndi.types.BGRange:
      native.speak('Good');
    trndi.types.BGRangeHI:
      native.speak('Going high');
    trndi.types.BGRangeLO:
      native.speak('Going low');
    end;
end;

procedure TfBG.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
  #27:
  begin
    lDiffDblClick(self);
    key := #0; // Disable future escapes
  end;
  'f', 'F':
    lDiffDblClick(self);
  's', 'S':
    speakReading;
  'A', 'a':
    miAnnounce.Click;
  'R', 'r':
    if slicke.UX.alert.UXDialog(uxdAuto, sRefrshQ, sForceRefresh,
      [mbYes, mbNo]) = mrYes then
      miForce.Click;
  'I', 'i':
    miSettings.Click;
  'U', 'u':
    CheckForUpdates(true);
  end;
end;

// DotPaint: Custom paint handler for trend dot TPaintBox controls

// NOTE (macOS/Cocoa): Historically, `TPaintBox.OnPaint` could fail under the
// Cocoa widgetset with a CheckDC()/TCocoaContext error.
//
// Current workaround: on macOS we use `TLabel` for trend dots
// (see `TDotControl = TLabel` under `{$ifdef DARWIN}`), so we avoid custom
// canvas painting for that control.
//
// If you *do* use a `TPaintBox`-based implementation on macOS in the future and
// run into Cocoa paint issues again, building with the Qt widgetset is an
// alternative workaround.
procedure TfBG.DotPaint(Sender: TObject);
var
  tw, th: integer;
  S, fontn: string;
  L: TDotControl;
  hasfont: boolean;
  needsRecalc: boolean;
begin
  L := Sender as TDotControl;
  S := L.Caption;
  
  {$ifdef DARWIN}
  // macOS: Use TLabel instead of TPaintBox to avoid Cocoa CheckDC issues
  // TLabel handles rendering automatically via Caption property
  L.AutoSize := true;
  L.Alignment := taCenter;
  L.Layout := tlCenter;
  Exit;
  {$endif}
  
  L.AutoSize := false;

  if S = DOT_GRAPH then
    hasFont := FontGUIInList(fontn)
  else
    hasFont := FontTXTInList(fontn);

  // Check if we need to recalculate font metrics
  needsRecalc := (FCachedFontSize <> L.Font.Size) or (FCachedFontName <> fontn) or
    (FCachedTextWidth = 0) or (FCachedTextHeight = 0) or (S <> DOT_GRAPH);
  // Always recalculate for non-dot text

  with L.Canvas do
  begin
    // Transparent background
    Brush.Style := bsClear;

    if hasFont then
      Font.Name := fontn;

    Font.Size := L.Font.Size;

    // Use cached measurements if available, otherwise calculate
    if needsRecalc then
    begin
      FCachedTextWidth := TextWidth(S);
      FCachedTextHeight := TextHeight(S);
      // Only cache for dot characters, not expanded text
      if S = DOT_GRAPH then
      begin
        FCachedFontSize := L.Font.Size;
        FCachedFontName := fontn;
      end;
    end;

    // Size the paintbox to fit the text (use fresh calculation for non-dots)
    if S = DOT_GRAPH then
    begin
      L.Width := FCachedTextWidth;
      L.Height := FCachedTextHeight;
    end
    else
    begin
      // For expanded text, always use fresh measurements with padding
      L.Width := TextWidth(S) + 4;
      L.Height := TextHeight(S) + 2;
    end;

    // Draw at (0,0); since control fits text, no centering or offsets needed
    TextOut(0, 0, S);
  end;
end;

procedure TfBG.lDiffClick(Sender: TObject);
begin
  ShowMessage(RS_DIFF);
end;

procedure TfBG.lPredictClick(Sender: TObject);
begin
  SHowMessage(RS_PREDICT);
end;

procedure TfBG.miDNSClick(Sender: TObject);
begin
  tPingTimer(miDNS);
end;

procedure TfBG.miDotNormalDrawItem(Sender: TObject; ACanvas: TCanvas;
ARect: TRect; AState: TOwnerDrawState);
begin

end;

procedure TfBG.miDotNormalMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
begin

end;

procedure TfBG.miDotsInViewClick(Sender: TObject);
var
  i: integer;
begin
  i := dotsInView;
  ShowMessage(IfThen(i = 0, 'Yes', 'Offset: ' + i.toString));
end;

procedure TfBG.miDotSmallClick(Sender: TObject);
begin

end;

procedure TfBG.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfBG.miCustomDotsClick(Sender: TObject);
var
  mr: TModalResult;
  dots: single;
begin
  dots := ExtNumericInput(uxdAuto,sDotSize,sCustomiseDotSize, sEnterDotSize,dotscale,true,mr);
  if mr = mrOk then
  begin
    native.SetSetting('ux.dot_scale', dots.tostring);
    dotscale := dots;
  end;
end;

procedure TfBG.miATouchAutoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;

  native.touchOverride := tbUnknown;
end;

procedure TfBG.miADotAdjustClick(Sender: TObject);
var
  mr: TModalResult;
  da: single;
begin
  da := (ExtNumericInput(uxdAuto, RS_SERVICE_DOT_ADJUST, RS_SERVICE_DOT_ADJUST_ADD,
    RS_SERVICE_DOT_ADJUST_DESC, DOT_ADJUST *
    100, false, mr) / 100);

  if mr = mrOk then
  begin
    DOT_ADJUST := da;

    if ExtMsg(uxdAuto, sChangesSave, sChangesRemember, sChangesApply,
      FormatFloat('0.00', da), uxclWhite, uxclRed, [mbYes, mbNo]) = mrYes then
      native.SetFloatSetting('ux.dot_adjust', da);
  end;
end;

procedure TfBG.miADotScaleClick(Sender: TObject);
var
  mr: TModalResult;
begin
  //  dotscale := round(ExtNumericInput(uxdAuto, 'Dot Adjustment','Add dot adjustment','You can enter plus or minus (+/- 0.x)',dotscale,false,mr));
end;

procedure TfBG.miADotsClick(Sender: TObject);
begin
  ShowMessage(Format(RS_SERVICE_ADJUST, [DOT_ADJUST * 100, dotscale]));
end;

procedure TfBG.miASystemInfoClick(Sender: TObject);
var
  sysver, platformInfo: string;
  {$ifdef Linux}
  ver: string;
  {$endif}
begin
  {$if defined(LCLWin32)}
  sysver := SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
  {$elseif defined(Linux)}
  platformInfo := getlinuxdistro(ver);
  sysver := platformInfo + ' ' + ver;
  {$endif}

  {$if defined(LCLQt6)}
  platformInfo := 'QT6 - ' + qtVersion + ' - ' + sysver;
  {$elseif defined(LCLGTK2)}
  platformInfo := 'GTK2 - '  + sysver;
  {$elseif defined(LCLGTK3)}
  platformInfo := 'GTK3';
  {$elseif defined(LCLWIN32)}
  platformInfo := 'Windows Native - ' + sysver;
  {$elseif defined(LCLCocoa)}
  platformInfo := 'macOS Native';
  {$else}
  platformInfo := 'unsupportd widgetset';
  {$endif}

  ShowMessage(Format(RS_SERVICE_SYSINFO, [{$I %FPCTargetOS%}, {$I %FPCTargetCPU%}, platformInfo, DefaultFormatSettings.DecimalSeparator]));

end;

procedure TfBG.miATouchClick(Sender: TObject);
begin

end;

procedure TfBG.miATouchNoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbFalse;

end;

procedure TfBG.miATouchYesClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbTrue;
end;

procedure TfBG.miDebugBackendClick(Sender: TObject);
begin
  miDebugBackend.Checked := not miDebugBackend.Checked;
end;

procedure TfBG.miPredictClick(Sender: TObject);
var
  bgr: BGResults;
  i: integer;
  msg: string;
  mr: TModalResult;
begin
  i := ExtIntInput(uxdAuto, RS_PREDICT_AMOUNT_CAPTION, RS_PREDICT_AMOUNT_TITLE, RS_PREDICT_AMOUNT_DESC, 3, mr);
  if (mr <> mrOK) or (i < 1) then
    i := 3;
  if i > 20 then
    i := 20;

  if not api.predictReadings(i, bgr) then
  begin
    ShowMessage(Format(RS_SERVICE_PREDICT_UNABLE, [api.errormsg]));
    Exit;
  end;

  msg := RS_SERVICE_PREDICTIONS + LineEnding;
  for i := 0 to High(bgr) do
    msg := msg + Format(RS_SERVICE_PREDICT_POINT, [
      i + 1,
      bgr[i].convert(un),
      BG_UNIT_NAMES[un],
      FormatDateTime('hh:nn', bgr[i].date)  // Show time
      ]) + LineEnding;

  ShowMessage(msg);
end;

procedure TfBG.pmSettingsClose(Sender: TObject);
begin
  last_popup := now;
end;

procedure TfBG.pnWarningClick(Sender: TObject);
begin
  {$ifdef TrndiExt}
  if not funcBool('uxClick',
    ['no-reading'], true) then
    Exit;
  {$endif}
  ShowMessage(RS_NO_BOOT_READING);
end;

procedure TfBG.pnWarningPaint(Sender: TObject);
const
  radius = 20;
var
  P: TPanel;
  {$ifndef X_WIN}
  lValRelativeX, lValRelativeY: integer;
  {$endif}
begin
  // Use manual drawing for rounded corners on all platforms
  // This prevents interference with scaling operations
  P := TPanel(Sender);

  with P.Canvas do
  begin
    // First, fill with dark background color in corners
    Brush.Color := fBG.Color;
    FillRect(0, 0, P.Width, P.Height);

    // Now draw the rounded panel
    Brush.Color := P.Color;
    Pen.Color := clBlack;
    Pen.Width := 1;
    RoundRect(0, 0, P.Width, P.Height, Radius, Radius);

    {$ifndef X_WIN}
    // On non-Windows platforms, ApplyAlphaControl doesn't work, so draw lVal as backdrop
    // to simulate transparency effect
    if lVal.Visible and (lVal.Caption <> '') then
    begin
      // Calculate lVal position relative to pnWarning
      lValRelativeX := lVal.Left - P.Left;
      lValRelativeY := lVal.Top - P.Top;

      // Set up text rendering to match lVal
      Font.Assign(lVal.Font);

      // Simulate the original lVal text at 92% transparency (0.92 where 1=fully transparent)
      // Formula: blended = (originalTextColor * opacity + panelColor * transparency)
      // where transparency = 0.92 and opacity = 0.08
      Font.Color := lclintf.RGB(Round(GetRValue(lVal.Font.Color) * 0.18 +
        GetRValue(P.Color) * 0.82), Round(GetGValue(lVal.Font.Color) *
        0.18 + GetGValue(P.Color) * 0.82), Round(GetBValue(lVal.Font.Color) *
        0.18 + GetBValue(P.Color) * 0.82));

      Brush.Style := bsClear; // Transparent background for text

      // Calculate proper text positioning based on lVal's alignment
      case lVal.Alignment of
      taLeftJustify:
        TextOut(lValRelativeX, lValRelativeY, lVal.Caption);
      taRightJustify:
        TextOut(lValRelativeX + lVal.Width - TextWidth(lVal.Caption),
          lValRelativeY, lVal.Caption);
      taCenter:
        TextOut(lValRelativeX + (lVal.Width - TextWidth(lVal.Caption)) div
          2, lValRelativeY, lVal.Caption);
      end;

      // Restore brush style
      Brush.Style := bsSolid;
    end;
    {$endif}
  end;
end;

procedure TfBG.FormMouseLeave(Sender: TObject);
begin

end;

procedure TfBG.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if DraggingWin then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
    tTouch.Enabled := false; // Dont popup stuff while moving
    // Use the resize timer with a very short interval for smooth panel scaling during drag
    // This prevents too frequent calls while still providing responsive scaling
    if not tResize.Enabled then
    begin
      tResize.Interval := 50; // Very short interval during dragging for responsiveness
      tResize.Enabled := true;
    end;
  end
  else
  // Don't enable touch timer on general mouse movement - only on actual lVal interaction
// The timer will be enabled in lValMouseDown when there's actual touch/click on lVal
  ;
end;

// FormCloseQuery event handler - called BEFORE FormClose
procedure TfBG.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$ifdef TrndiExt}
  // Show immediate user feedback for shutdown process using big lVal text
  lVal.Caption := 'Shutting down extensions...';
  lVal.Visible := true;
  Application.ProcessMessages; // Ensure caption is updated immediately
  
  // Set global shutdown flag immediately to prevent any new JS operations
  try
    trndi.ext.engine.SetGlobalShutdown;
  except
    // Ignore any errors during shutdown flag setting
  end;
  {$endif}

  // CRITICAL: Set Application.Terminated FIRST to prevent any QuickJS operations
  // This is called even earlier than FormClose for maximum protection
  Application.Terminate;  // This sets Application.Terminated := True

  // Always allow close to proceed
  CanClose := true;
end;

// FormClose event handler
procedure TfBG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{$ifdef Darwin}
var
  mr: TModalResult;
  {$endif}
begin
  // Prevent recursive shutdown calls
  if FShuttingDown then
    Exit;
  FShuttingDown := true;

  // Save window position if using custom positioning
  if GetValidatedPosition = tpoCustom then
  begin
    native.SetSetting('position.last.left', IntToStr(Left));
    native.SetSetting('position.last.top', IntToStr(Top));
  end;

  {$ifdef Darwin}
  if not firstboot then
    if self.Showing then
    begin
      mr := UXDialog(uxdAuto, 'Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',
        [mbClose, mbUXMinimize, mbCancel]);
      case mr of
      mrClose:
        CloseAction := caFree;
      mrCancel: 
      begin
        FShuttingDown := false; // Reset flag if user cancels
        Abort;
      end;
      else
      begin
        CloseAction := caHide;
        FShuttingDown := false; // Reset flag for hide
        Exit;
      end;
      end;

    end;
  {$else}

  if not firstboot then
    if UXDialog(uxdAuto, RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], uxmtOK) = mrNo then
    begin
      FShuttingDown := false; // Reset flag if user cancels
      Abort;
    end;

  // Explicitly set CloseAction to ensure the form is actually freed
  CloseAction := caFree;
  {$endif}

  {$ifdef TrndiExt}
  // NOTE: Application.Terminate is now called in FormCloseQuery for earlier detection
  
  // CRITICAL: Signal extension shutdown FIRST before stopping timers
  // This prevents new async operations from starting
  try
    // Signal shutdown to all extension components immediately
    SetExtShuttingDown(true);
    
    // Give more time for background threads to see the shutdown signal
    Application.ProcessMessages;
    Sleep(150);  // Increased even more
    Application.ProcessMessages;
    
    // Now stop all timers to prevent new operations
    if Assigned(tAgo) then
      tAgo.Enabled := false;
    if Assigned(tClock) then
      tClock.Enabled := false;
    if Assigned(tSwap) then
      tSwap.Enabled := false;
    if Assigned(tResize) then
      tResize.Enabled := false;
    if Assigned(tMissed) then
      tMissed.Enabled := false;
    if Assigned(tTouch) then
      tTouch.Enabled := false;
    if Assigned(tMain) then
      tMain.Enabled := false;
    
    // Process any remaining messages/synchronize calls with extended wait
    Application.ProcessMessages;
    Sleep(250);  // Increased significantly 
    Application.ProcessMessages;
    Sleep(150);  // Additional wait
    Application.ProcessMessages;
    
    // Update user feedback before engine cleanup
    lVal.Caption := RS_CLEANUP;
    actOnTrend(@HideDot);
    lTir.Hide;
    lArrow.hide;
//    lDiff.Hide;
    lDiff.Caption := RS_CLEANUP_WAIT;
    lAgo.hide;
    lPredict.hide;
    Application.ProcessMessages;
    
    // Now safely shutdown the extension engine
    TTrndiExtEngine.ReleaseInstance;
  except
    // Ignore shutdown errors - the OS will clean up remaining resources
  end;
  {$endif}

  // Stop web server timer if it's still running
  if Assigned(tWebServerStart) then
  begin
    tWebServerStart.Enabled := false;
    FreeAndNil(tWebServerStart);
  end;
  
  // Stop web server
  StopWebServer;

  // Let normal form closure process continue with CloseAction := caFree
end;

procedure TfBG.fbReadingsDblClick(Sender: TObject);
begin

end;

procedure TfBG.FormActivate(Sender: TObject);
begin
  StopFlashing;
end;

procedure TfBG.FormClick(Sender: TObject);
begin
  StopFlashing;
end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  l: TDotControl;
  da: integer;
begin
  for l in TrendDots do
    l.top := l.top + round(ClientHeight * DOT_ADJUST);

  da := dotsInView;

  if da <> 0 then
    for l in TrendDots do
      l.top := l.top - da// da is negative on top so this is valid both ways
  ;

  lRef.Top := lDot1.Top;
  lRef.Caption := lDot1.Hint;
  // Redraw form so range indicators follow dot positions (Update forces immediate paint)
  Update;
  
  // Force all dots to repaint on top of the filled areas (after form has painted)
  for l in TrendDots do
    if l.Visible then
      l.Repaint;

end;

procedure TfBG.bMenuPanelCloseClick(Sender: TObject);
begin
  pnTouchMenu.Hide;
end;

procedure TfBG.FormPaint(Sender: TObject);
var
  loY, hiY, rangeLoY, rangeHiY: integer;
  cnv: TCanvas;
  lineColor: TColor;
  drawLo, drawHi, drawRangeLo, drawRangeHi: boolean;
  tmp: integer;
  clientH: integer;
  dotHeight: integer;
  bmp: TBitmap;
  {$IFDEF DEBUG}
  debugY: integer;
  debugValue: single;
  debugText: string;
  i: integer;
  {$ENDIF}
// Helper to map a BG value in internal units to a Y coordinate matching SetPointHeight
function ValueToY(const Value: single): integer;
  var
    Padding, UsableHeight, Position: integer;
    v: single;
  begin
    // clientH is determined in the outer scope to match SetPointHeight's clientHeight
    Padding := Round(clientH * 0.1);
    UsableHeight := clientH - (Padding * 2);
    v := Value;
    if v < BG_API_MIN then
      v := BG_API_MIN;
    if v > BG_API_MAX then
      v := BG_API_MAX;
    Position := Padding + Round((v - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
      UsableHeight);
    Result := (clientH - Position) - 1;
  end;

begin
  // Only draw when form has been created and API thresholds available
  if not Assigned(Self) then
    Exit;

  // Exit early if nothing is enabled
  if not (PaintRange or PaintRangeCGMRange) then
    Exit;

  cnv := Self.Canvas;
  // Prefer the TrendDots parent client height if available so mapping matches SetPointHeight
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) and
    Assigned(TrendDots[1].Parent) then
    clientH := TrendDots[1].Parent.ClientHeight
  else
    clientH := Self.ClientHeight;

  // Get dot height for centering lines through the middle of dots
  // Calculate using a temporary bitmap to avoid affecting actual dots during paint
  // IMPORTANT: Must match the exact calculation used in ResizeDot to ensure alignment
  dotHeight := 0;
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Canvas.Font.Assign(TrendDots[1].Font);
      // Use the same font size formula as ResizeDot: (lVal.Font.Size div 8) * dotscale
      bmp.Canvas.Font.Size := round(Max((lVal.Font.Size div 8) * dotscale, 28));
      // Use the same height calculation as ResizeDot: Max(TextHeight, Font.Size)
      dotHeight := Max(bmp.Canvas.TextHeight(DOT_GRAPH), bmp.Canvas.Font.Size);
    finally
      bmp.Free;
    end;
  end;

  // Decide whether to draw low/high range indicators (0 disables)
  drawLo := (Assigned(api) and (api.cgmLo <> 0) and PaintRange);
  drawHi := (Assigned(api) and (api.cgmHi <> 0) and PaintRange);
  drawRangeLo := (Assigned(api) and (api.cgmRangeLo <> 0) and PaintRangeCGMRange);
  drawRangeHi := (Assigned(api) and (api.cgmRangeHi <> 0) and PaintRangeCGMRange);

  // Calculate Y positions for low and high thresholds
  // These are the RAW positions matching where the TOP of a dot control would be placed
  if drawLo then
    loY := ValueToY(api.cgmLo * BG_CONVERTIONS[mmol][mgdl]);

  if drawHi then
    hiY := ValueToY(api.cgmHi * BG_CONVERTIONS[mmol][mgdl]);

  // Calculate Y positions for range low and high thresholds
  if drawRangeLo then
    rangeLoY := ValueToY(api.cgmRangeLo * BG_CONVERTIONS[mmol][mgdl]);

  if drawRangeHi then
    rangeHiY := ValueToY(api.cgmRangeHi * BG_CONVERTIONS[mmol][mgdl]);

  // Fill the area between low and high thresholds with a darkened background color
  // Use RAW Y positions (matching the top of where dots would be)
  if drawLo and drawHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.9)
    else
      cnv.Brush.Color := BlendColors(tir_bg, fBG.Color, 0.3); // 30% tir_bg, 70% background
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, hiY, Self.ClientWidth, loY));
  end;

  // Fill the area between inner range thresholds with a lightened background color
  // Use RAW Y positions (matching the top of where dots would be)
  if drawRangeLo and drawRangeHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_custom_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.85)
    else
    if tir_bg = -1 then
      cnv.Brush.Color := BlendColors(tir_custom_bg, fBG.Color, 0.3) // Blend with background
    else
      cnv.Brush.Color := BlendColors(tir_custom_bg, BlendColors(tir_bg, fBG.Color, 0.3), 0.3)// Blend tir_custom_bg with the base layer (either blended tir_bg or fBG.Color)
// Blend with tir_bg layer
    ;
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, rangeHiY, Self.ClientWidth, rangeLoY));
  end;

  // Use semi-transparent versions of range colors
  cnv.Brush.Style := bsClear;

  // Only draw lines if PaintRangeLines is enabled
  if PaintRangeLines then
  begin
    if drawLo then
    begin
      // Draw horizontal line for low threshold
      // Add dotHeight offset so line goes through the center of dots
      lineColor := LightenColor(bg_rel_color_lo);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      tmp := loY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawHi then
    begin
      // Draw horizontal line for high threshold
      // Add dotHeight offset so line goes through the center of dots
      lineColor := LightenColor(bg_rel_color_hi);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      tmp := hiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    // Draw range threshold lines (inner thresholds within the main range)
    if drawRangeLo then
    begin
      // Draw horizontal line for range low threshold
      // Add dotHeight offset so line goes through the center of dots
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeLoY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawRangeHi then
    begin
      // Draw horizontal line for range high threshold
      // Add dotHeight offset so line goes through the center of dots
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeHiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;
  end;

  {$IFDEF DEBUG}
  // Draw debug gridlines showing mmol/L values (5, 10, 15, 20)
  cnv.Brush.Style := bsClear;
  cnv.Pen.Style := psDot;
  cnv.Pen.Width := 1;
  cnv.Pen.Color := LightenColor(fBG.Color, 0.3);
  cnv.Font.Size := 8;
  cnv.Font.Color := LightenColor(fBG.Color, 0.4);
  
  for i := 1 to 4 do
  begin
    debugValue := i * 5.0; // 5, 10, 15, 20 mmol/L
    if (debugValue >= BG_API_MIN) and (debugValue <= BG_API_MAX) then
    begin
      // debugValue is already in mmol, which is the internal API unit, so no conversion needed
      debugY := ValueToY(debugValue) + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, debugY);
      cnv.LineTo(Self.ClientWidth, debugY);
      debugText := Format('%.0f', [debugValue]);
      // Draw text on right side, centered vertically on the line
      cnv.TextOut(Self.ClientWidth - cnv.TextWidth(debugText) - 5, 
                  debugY - (cnv.TextHeight(debugText) div 2), 
                  debugText);
    end;
  end;
  {$ENDIF}
end;

procedure TfBG.bSettingsClick(Sender: TObject);
begin
  ShowMessage(RS_RIGHT_CLICK);
  miSettings.Click;
end;

procedure TfBG.bTouchFullClick(Sender: TObject);
begin
  miFullScreen.Click;
  pnTouchMenu.Hide;
end;

procedure TfBG.bTouchMenuClick(Sender: TObject);
var
  p: TPoint;
begin
  p := Mouse.CursorPos;
  pmSettings.PopUp(p.X, p.Y)
end;

procedure TfBG.bTouchSettingsClick(Sender: TObject);
begin
  miSettings.Click;
end;

// Create trend dot controls dynamically at runtime
// Uses TLabel on macOS (Darwin) to avoid Cocoa CheckDC bugs, TPaintBox elsewhere
procedure TfBG.CreateTrendDots;
var
  i: integer;
  dotArray: array[1..NUM_DOTS] of ^TDotControl;
begin
  // Set up array of pointers to each dot field
  dotArray[1] := @lDot1;
  dotArray[2] := @lDot2;
  dotArray[3] := @lDot3;
  dotArray[4] := @lDot4;
  dotArray[5] := @lDot5;
  dotArray[6] := @lDot6;
  dotArray[7] := @lDot7;
  dotArray[8] := @lDot8;
  dotArray[9] := @lDot9;
  dotArray[10] := @lDot10;

  for i := 1 to NUM_DOTS do
  begin
    {$ifdef DARWIN}
    // macOS: Create TLabel to avoid TPaintBox.OnPaint Cocoa bugs
    dotArray[i]^ := TLabel.Create(Self);
    with TLabel(dotArray[i]^) do
    begin
      Parent := Self;
      AutoSize := true;
      Alignment := taCenter;
      Layout := tlCenter;
      Transparent := true;
    end;
    {$else}
    // Other platforms: Use TPaintBox with custom painting
    dotArray[i]^ := TPaintBox.Create(Self);
    with TPaintBox(dotArray[i]^) do
    begin
      Parent := Self;
      AutoSize := false;
      OnPaint := @DotPaint;
    end;
    {$endif}
    
    // Common properties for both control types
    with dotArray[i]^ do
    begin
      Left := 72;
      Top := 191;
      Width := 33;
      Height := 49;
      Visible := false;
      Caption := DOT_GRAPH;
      PopupMenu := pmSettings;
      OnClick := @onTrendClick;
    end;
    
    // Add to TrendDots array for easy access
    TrendDots[i] := dotArray[i]^;
  end;
  
  LogMessage('Trend dots created dynamically');
end;

procedure TfBG.initDot(l: TDotControl; c, ix: integer);
begin
  {$ifdef Windows}
  l.Font.Name := 'DejaVu Sans Mono';
  {$endif}
end;

// Calculate the vertical offset needed to align limit lines with the visual center of the dot character
// by analyzing where the first non-transparent pixel appears in the rendered dot
function TfBG.CalculateDotVisualOffset: integer;
var
  bmp: TBitmap;
  y, x: integer;
  fontSize: integer;
  halfHeight: integer;
  firstPixelY: integer;
  bgColor, pixelColor: TColor;
  found: boolean;
  fontName: string;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    try
      // Use a reasonable font size for measurement (similar to what dots use)
      fontSize := 24;
      bmp.Width := fontSize * 2;
      bmp.Height := fontSize * 2;

      {$ifdef DARWIN}
      // macOS: Force handle allocation before canvas operations
      bmp.Canvas.Handle; // This ensures the bitmap context is properly created
      {$endif}

      // Set background color and clear bitmap
      bgColor := clWhite;
      bmp.Canvas.Brush.Color := bgColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

      // Set font properties matching the dots - use GUI font on all platforms
      if FontGUIInList(fontName) then
        bmp.Canvas.Font.Name := fontName
      else
        bmp.Canvas.Font.Name := 'default';
      bmp.Canvas.Font.Size := fontSize;
      bmp.Canvas.Font.Color := clBlack;

      // Draw the dot character centered
      bmp.Canvas.TextOut(fontSize div 2, fontSize div 2, DOT_GRAPH);

      // Find the first row with a non-background pixel (scanning from top)
      firstPixelY := -1;
      found := false;

      for y := 0 to bmp.Height - 1 do
      begin
        for x := 0 to bmp.Width - 1 do
        begin
          pixelColor := bmp.Canvas.Pixels[x, y];
          // Check if pixel is significantly different from background
          if pixelColor <> bgColor then
          begin
            firstPixelY := y;
            found := true;
            break;
          end;
        end;
        if found then
          break;
      end;

      // Calculate offset based on where we found the first pixel
      if found then
      begin
        // The visual center should be at halfHeight
        halfHeight := bmp.Height div 2;
        // If first pixel is below center, offset is positive (move lines down)
        // If first pixel is above center, offset is negative (move lines up)
        Result := firstPixelY - halfHeight;
        LogMessage(Format(
          'DOT_VISUAL_OFFSET calculated: font=%s, firstPixel=%d, halfHeight=%d, offset=%d',
          [
          fontName, firstPixelY, halfHeight, Result]));
      end;
    except
      on E: Exception do
      begin
        {$ifdef DARWIN}
        // On macOS, log canvas errors but don't crash
        LogMessage('CalculateDotVisualOffset error on macOS: ' + E.Message);
        Result := 0; // Use default offset
        {$else}
        raise; // Re-raise on other platforms
        {$endif}
      end;
    end;

  finally
    bmp.Free;
  end;
end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TDotControl; c, ix: integer);
var
  isDot: boolean;
begin

  if privacyMode then // We dont show values while in privacy mode
    exit;

  if l.hint = '' then  // Don't process non-value items
    Exit;

  // Check if label currently shows a dot
  isDot := l.Caption = DOT_GRAPH;

  // Handle differently based on position in trend sequence
  if ix = NUM_DOTS then
    // Latest reading: toggle between fresh indicator and dot
    l.Caption := IfThen(isDot, DOT_FRESH, DOT_GRAPH)
  else
    // Earlier readings: toggle between actual value and dot
    l.Caption := IfThen(isDot, l.Hint, DOT_GRAPH);

  //  l.Caption := IfThen(isDot, l.Caption, l.Caption);
  // Adjust size based on current state
  if not isDot then
  begin // Returning to dot
    ResizeDot(l, c, ix);
    l.Font.Size := round((ClientWidth div 24) * dotscale);
  end
  else
  begin
    // Expanding to show actual value - need to resize for text
    l.font.Size := (lVal.Font.Size div c);
    // Clear cached font metrics to force recalculation with new text/font
    FCachedTextWidth := 0;
    FCachedTextHeight := 0;
    FCachedFontSize := 0;
    FCachedFontName := '';
    // Force repaint with new dimensions - DotPaint will recalculate size
    l.Invalidate;
  end;

end;

// Hides a dot
procedure TfBG.HideDot(l: TDotControl; c, ix: integer);
begin
  l.Visible := false;
end;

// Shows a dot only if it has valid data (non-empty Hint)
procedure TfBG.ShowDot(l: TDotControl; c, ix: integer);
begin
  // Only show dots that have been populated with data
  if l.Hint <> '' then
    l.Visible := true;
end;

// Scales a dot's font size
procedure TfBG.ResizeDot(l: TDotControl; c, ix: integer);
var
  th, tw, minSize: integer;
begin
  {$ifdef DARWIN}
  // macOS: TLabel handles sizing automatically via AutoSize
  l.AutoSize := true;
  l.Font.Size := round(Max((lVal.Font.Size div 8) * dotscale, 28)); // Ensure minimum font size
  // Force immediate size calculation by triggering a layout update
  l.AdjustSize;
  LogMessage(Format('TrendDots[%d] resized with Font Size = %d, Height=%d (AutoSize).',
    [ix, l.Font.Size, l.Height]));
  {$else}
  l.AutoSize := false;
  l.Font.Size := round(Max((lVal.Font.Size div 8) * dotscale, 28)); // Ensure minimum font size
  // Tighten control size to actual text metrics of the dot glyph
  tw := l.Canvas.TextWidth(DOT_GRAPH);
  th := l.Canvas.TextHeight(DOT_GRAPH);
  minSize := Max(th, l.Font.Size);
  l.Width := tw;
  l.Height := minSize;
  LogMessage(Format('TrendDots[%d] resized with Font Size = %d, W=%d, H=%d.',
    [ix, l.Font.Size, l.Width, l.Height]));
  {$endif}
end;

// Sets the width (NOT the font) of a dot
procedure TfBG.SetDotWidth(l: TDotControl; c, ix: integer; ls: array of TDotControl);
var
  spacing: integer;
begin
  // Calculate spacing based on label width to prevent overlap
  spacing := (fBG.Width - (c * l.Width)) div (c + 1);

  // Position each label with equal spacing from the left
  l.Left := spacing + (spacing + l.Width) * (ix - 1);
  LogMessage(Format('TrendDots[%d] positioned at Left = %d.', [ix, l.Left]));
end;

// FormResize event handler
procedure TfBG.FormResize(Sender: TObject);
begin
  if Sender = lval then
    tResize.OnTimer(self)
  else
  begin
    // Normal resize handling - let the timer handle dragging vs non-dragging logic
    if not tResize.Enabled then
      tResize.Enabled := true
    else
    if not DraggingWin then
    begin
      // Only restart timer when not dragging to prevent rapid resize interruptions
      tResize.Enabled := false;
      tResize.Enabled := true;
    end;

    if not DraggingWin then
    begin
      // Only hide labels during resize if they don't have valid content
      if lVal.Caption = '' then
        lVal.Visible := false;
      lAgo.Visible := false;
      lTir.Visible := false;
    end;

    // Apply alpha control only - rounded corners are handled by pnWarningPaint
    ApplyAlphaControl(pnWarning, 235);
  end;
end;

procedure TfBG.FormShow(Sender: TObject);
begin
  placeForm;
  placed := true;
  lVal.font.Quality := fqCleartype;
  
  // Check for updates on startup (non-blocking)
  CheckForUpdates;
end;

procedure TfBG.lAgoClick(Sender: TObject);
var
  displayMsg: string;
  i: integer;
begin
  if firstboot then
    exit; // Dont trigger lastReading

  displayMsg := miRefresh.Caption;

  if lastReading.getRSSI(i) then
    displayMsg += sHTMLLineBreak + Format(sRSSI, [i]);
  if lastReading.getNoise(i) then
    displayMsg += sHTMLLineBreak + Format(sNoise, [i]);

  displayMsg += sHTMLLineBreak + Format(sDevice, [lastReading.sensor]);

  ExtHTML(uxdAuto, sTransmitterInfo, displayMsg, [mbOK], uxmtInformation);

end;

procedure TfBG.lArrowClick(Sender: TObject);
begin

end;

{$ifdef DARWIN}
procedure TfBG.ToggleFullscreenMac;
var
  macWindow: NSWindow;
begin
  // Try to get the main window or fallback to the key window
  macWindow := NSApplication.sharedApplication.mainWindow;
  if macWindow = nil then
    macWindow := NSApplication.sharedApplication.keyWindow;

  // If still nil, try to bridge from the Lazarus form handle
  if macWindow = nil then
    macWindow := NSWindow(Tfbg(self).Handle);

  if Assigned(macWindow) then
    macWindow.toggleFullScreen(nil)
  else
    ShowMessage('Unable to toggle fullscreen. Main window not found.');
end;
{$endif}

procedure TfBG.lDiffDblClick(Sender: TObject);
begin
  DoFullscreen;
end;

procedure TfBG.DoFullScreen;
var
  IsCurrentlyFullscreen: boolean;
  SavedBounds: TRect;
begin
  {$ifdef DARWIN}
  ToggleFullscreenMac;
  {$endif}
  // Store the current form bounds before making any changes
  SavedBounds := BoundsRect;

  // Determine if form is currently in fullscreen mode
  // This needs to check multiple conditions as WindowState alone isn't reliable
  IsCurrentlyFullscreen := (BorderStyle = bsNone) and
    ((WindowState = wsFullScreen) or (BoundsRect.Width >= Screen.Width) and
    (BoundsRect.Height >= Screen.Height));

  // Remember the window position for restoration
  if not FStoredWindowInfo.Initialized and not IsCurrentlyFullscreen then
  begin
    FStoredWindowInfo.Left := Left;
    FStoredWindowInfo.Top := Top;
    FStoredWindowInfo.Width := Width;
    FStoredWindowInfo.Height := Height;
    FStoredWindowInfo.WindowState := WindowState;
    FStoredWindowInfo.BorderStyle := BorderStyle;
    FStoredWindowInfo.FormStyle := FormStyle;
    FStoredWindowInfo.Initialized := true;
  end;

  if IsCurrentlyFullscreen then
  begin
    // First, change the window state to normal
    WindowState := wsNormal;
    Application.ProcessMessages;
    Screen.Cursor := crDefault;

    // Then restore border styles
    BorderStyle := bsSizeToolWin;
    FormStyle := fsNormal;
    Application.ProcessMessages;

    // Restore previous window size and position
    if FStoredWindowInfo.Initialized then
    begin
      SetBounds(FStoredWindowInfo.Left, FStoredWindowInfo.Top,
        FStoredWindowInfo.Width, FStoredWindowInfo.Height);
      WindowState := FStoredWindowInfo.WindowState;
      FStoredWindowInfo.Initialized := false;
    end
    else
    begin
      // Fallback if stored position is not available
      Width := Max(Screen.Width div 5, 200);
      Height := Max(Screen.Height div 5, 300);
      Top := (Screen.Height div 2) - (Height div 2);
      Left := (Screen.Width div 2) - (Width div 2);
    end;
  end
  else
  begin
    // Enter fullscreen mode

    // First, make sure we're in normal state to avoid issues
    if WindowState = wsMaximized then
      WindowState := wsNormal;
    Application.ProcessMessages;

    // Remove borders first
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    Application.ProcessMessages;

    // Finally set to fullscreen
    WindowState := wsFullScreen;

    // Force repaint to ensure display updates properly
    Invalidate;

    Screen.Cursor := crNone;
    // FIx Raspbian issues
    if IsProblematicWM then
      if not IsSemiProblematicWM then
      begin
        miBorders.Checked := false;
        BorderStyle := bsToolWindow;
      end;
  end;

  // Adjust for dark mode if applicable
  setColorMode;
end;

procedure TfBG.lDot7DblClick(Sender: TObject);
begin
end;

// Empty event handler
procedure TfBG.lgMainClick(Sender: TObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.lTirClick(Sender: TObject);
var
  minTotal, hours, mins, rangeMinutes: integer;
  msg: string;
  hi, lo, rhi, rlo: double;
begin
  minTotal := MinutesBetween(now, bgs[High(bgs)].date);
  rangeMinutes := native.GetIntSetting('range.time', 9999);
  if minTotal > rangeMinutes then // our custom range is smaller than the available data
    minTotal := rangeMinutes;

  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'tir',
    mintotal,
    lTir.Caption
    ], true) then
    Exit;
  {$endif}


  hi := api.cgmHi * BG_CONVERTIONS[un][mgdl];
  lo := api.cgmLo * BG_CONVERTIONS[un][mgdl];
  rhi := api.cgmRangeHi * BG_CONVERTIONS[un][mgdl];
  rlo := api.cgmRangeLo * BG_CONVERTIONS[un][mgdl];

  if minTotal < 60 then
    msg := Format(RS_TIR_M, [minTotal, lo, hi, rlo, rhi])
  else
  begin
    hours := minTotal div 60;
    mins := minTotal mod 60;
    if hours < 2 then
      msg := Format(RS_TIR_H1, [mins, lo, hi, rlo, rhi])
    else
      msg := Format(RS_TIR_H, [hours, mins, lo, hi, rlo, rhi]);
  end;

  ShowMessage(msg + LineEnding);
end;

// Handle lVal click
procedure TfBG.lValClick(Sender: TObject);
begin
  if (lVal.Caption = RS_SETUP) then
    miSettings.Click;
  if firstboot then
    exit;
  StopFlashing;
end;

procedure TfBG.lValDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

// Handle mouse down on lVal
procedure TfBG.lValMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  if ((Button = mbLeft) and (self.BorderStyle = bsNone)) or (Button = mbMiddle) then
  begin   // Handle window moving
    DraggingWin := true;
    PX := X;
    PY := Y;
    if not hasTouch then
      Exit;
  end;


  if not hasTouch then
    Exit;

  // Handle touch screens
  StartTouch := Now;
  IsTouched := true;
  tTouch.Enabled := true;

end;

// Handle mouse up on lVal
procedure TfBG.lValMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  IsTouched := false;
  tTouch.Enabled := false;
  StartTouch := 0; // Reset the touch start time

  if DraggingWin then
  begin
    DraggingWin := false;
    // Restore normal timer interval
    tResize.Interval := 500; // Back to normal interval from form design
    // Trigger a full resize operation now that dragging is complete
    // This ensures all UI elements are properly scaled for the final window size
    FormResize(Self);
  end;
end;

// Empty drag event handler
procedure TfBG.lValStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.miAnnounceClick(Sender: TObject);
begin
  miAnnounce.Checked := not miAnnounce.Checked;
  native.SetBoolSetting('main.announce', miAnnounce.Checked);
  native.speak(IfThen(miAnnounce.Checked, sAnnounceOn, sAnnounceOff));
end;

procedure TfBG.miAlternateClick(Sender: TObject);
begin
  miAlternate.Checked := not miAlternate.Checked;
  tSwap.Enabled := miAlternate.Checked;
end;

procedure TfBG.miClockClick(Sender: TObject);
begin
  miClock.Checked := not miClock.Checked;
  tClock.Enabled := miClock.Checked;
  native.SetBoolSetting('main.clock', tClock.Enabled);
end;

procedure TfBG.miDotNormalClick(Sender: TObject);
var
  fmt: TFormatSettings;
begin
  fmt.DecimalSeparator := '.';
  dotscale := StrToFloat((Sender as TMenuItem).Hint, fmt);
  native.SetSetting('ux.dot_scale', dotscale.toString);
  FormResize(fBG);
end;

procedure TfBG.TfFloatOnHide(Sender: TObject);
begin
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.miFloatOnClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  if fFloat.Showing then
    fFloat.Hide
  else
  begin
    fFloat.Show;
    if not assigned(fFloat.onhide) then
      ffloat.OnHide := @TfFloatOnHide;
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lArrow.Caption := lArrow.Caption;
    if pnMultiUser.Visible then
    begin
      fFloat.pnMultiUser.Visible := true;
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
    end;
  end;
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.mi24hClick(Sender: TObject);
var
  res: string;
  readings: BGResults;
begin
  if api = nil then
  begin
    ShowMessage('API not initialized yet.');
    Exit;
  end;

  readings := api.getReadings(1440, 288, '', res);

  if Length(readings) = 0 then
  begin
    if res <> '' then
      ShowMessage('No readings returned: ' + res)
    else
      ShowMessage('No readings returned for the last 24 hours.');
    Exit;
  end;

  ShowHistoryGraph(readings, un, CurrentHistoryGraphPalette);
end;

procedure TfBG.miHistoryClick(Sender: TObject);
begin
  ShowHistoryGraph(bgs, un, CurrentHistoryGraphPalette);
end;

procedure TfBG.miRangeColorClick(Sender: TObject);
begin
  miRangeColor.Checked := not miRangeColor.Checked;
  miForce.Click;
  native.SetBoolSetting('ux.range_color', miRangeColor.Checked);
end;

procedure TfBG.miBordersClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  miBorders.Checked := not miBorders.Checked;
  if miBorders.Checked then
    self.BorderStyle := bsNone
  else
    {$ifdef DARWIN}
    BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}

  setColorMode;
end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
const
  API_CACHE_SECONDS = 10; // Same as in FetchAndValidateReadings
var
  secondsSinceLastCall: integer;
  waitTime: integer;
  msg: string;
  Result: integer;
begin
  if firstboot then
    exit;
  
  // Check if we have made an API call before
  if FLastAPICall = 0 then
  begin
    // No previous API call, just do a normal update
    updateReading;
    Exit;
  end;
  
  // Check if we're still within the cache window
  secondsSinceLastCall := SecondsBetween(Now, FLastAPICall);
  if secondsSinceLastCall < API_CACHE_SECONDS then
  begin
    waitTime := API_CACHE_SECONDS - secondsSinceLastCall;
    msg := Format(sForceRefreshCached, [secondsSinceLastCall, waitTime]);

    // Use ExtMsg with custom buttons for better UX
    Result := slicke.UX.alert.ExtMsg(uxdAuto, sRefrshQ, msg,
      sForceRefreshDetail, '', uxclWhite, uxclRed, [mbRetry, mbCancel],
      uxmtInformation);

    if Result = mrRetry then // mbRetry returns mrRetry
    begin
      // User chose to force - bypass cache completely
      FetchAndValidateReadingsForced;
      // Update the rest of the UI manually since we bypassed normal flow
      CompleteUIUpdate;
    end;
    // If Cancel was chosen, do nothing - let user wait
  end
  else
    updateReading// Cache has expired, normal update is fine
  ;
end;

// Explain limit menu click
procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
  //  MessageDlg('Trndi', RS_LIMIT_EXPLAIN_TEXT, mtInformation, [mbOK], '');
  ShowMessage(RS_LIMIT_EXPLAIN_TEXT);
end;

procedure TfBG.miOnTopClick(Sender: TObject);
{$ifdef LCLQt6}
var
  QtWidget: TQtWidget;
  flags: nativeuint;
  sessionType: string;
const
  // Qt::WindowStaysOnTopHint (use numeric mask to avoid missing symbol on some setups)
  Qt_WindowStaysOnTopHint = $00008000;
  {$endif}
begin
  miOnTop.Checked := not miOnTop.Checked;
  if miOnTop.Checked then
  begin
    {$ifdef LCLQt6}
    // Use fsStayOnTop for Qt widgetset; it's better respected by some WMs.
    self.FormStyle := fsStayOnTop;
    {$else}
    self.FormStyle := fsSystemStayOnTop;
    {$endif}
  end
  else
    self.FormStyle := fsNormal;

  {$ifdef LCLQt6}
  // KDE/Qt may ignore LCL's FormStyle mapping; force Qt's WindowStaysOnTopHint.
  // Detect session type: Wayland sessions often prevent external programs from
  // forcing window stacking. Inform the user instead of attempting external hacks.
  sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
  if LowerCase(sessionType) = 'wayland' then
  begin
    LogMessage('OnTop requested but session is Wayland; compositor may ignore programmatic hints.');
    // Friendly guidance for users running KDE/Wayland
    if miOnTop.Checked then
      ShowMessage(RS_WAYLAND);
    // Continue with LCL/Qt hints — they may or may not be honored.
  end;
  try
    if HandleAllocated then
    begin
      QtWidget := TQtWidget(Handle);
      if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
      begin
        flags := QtWidget.windowFlags;
        if miOnTop.Checked then
        begin
          QtWidget.setWindowFlags(flags or Qt_WindowStaysOnTopHint);
          // Use LCL methods to bring the form forward and ensure it's visible
          Self.Visible := true;
          Self.Show;
          Self.BringToFront;
          Self.SetFocus;
          // Also request the application to bring windows forward
          try
            Application.BringToFront;
          except end;
          // If the window is off-screen (some WMs may move it), center it as a safeguard
          if (Left < -1000) or (Top < -1000) or (Left > Screen.Width) or (Top > Screen.Height) then
          begin
            Width := Min(Width, Screen.Width - 40);
            Height := Min(Height, Screen.Height - 40);
            Left := (Screen.Width - Width) div 2;
            Top := (Screen.Height - Height) div 2;
            Application.ProcessMessages;
          end;
        end
        else
          QtWidget.setWindowFlags(flags and (not Qt_WindowStaysOnTopHint));
      end;
    end;
  except
    // Ignore any failures - fallback is LCL behavior
  end;
  {$endif}

  setColorMode;
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  fConf: TfConf;
  lastUsers: integer;

procedure LoadUserSettings(f: TfConf);
  var
    remoteType: string;
    userNamesCSV: string;
    posName: string;
    i: integer;
    posValue: integer;
    po: TrndiPos;
    sizeVal: integer;
  begin
    with f, native do
    begin
      // Remote and user settings
      remoteType := GetSetting('remote.type');
      cbSys.ItemIndex := 0; // Default driver
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = remoteType then
          cbSys.ItemIndex := i;
      f.cbSysChange(f); // Update labels
      eAddr.Text := GetSetting('remote.target');
      ePass.Text := GetSetting('remote.creds');
      rbUnit.ItemIndex := IfThen(GetSetting('unit', 'mmol') = 'mmol', 0, 1);
      spTHRESHOLD.Value := native.GetIntSetting('system.fresh_threshold',
        DATA_FRESHNESS_THRESHOLD_MINUTES);

      // Override range settings
      if api = nil then
      begin
        fsLo.Value := GetIntSetting('override.lo', 60);
        fsHi.Value := GetIntSetting('override.hi', 180);
        fsLoRange.Value := GetIntSetting('override.rangelo', 80);
        fsHiRange.Value := GetIntSetting('override.rangehi', 160);
      end
      else
      begin
        fsLo.Value := GetIntSetting('override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting('override.hi', api.cgmHi);
        fsLoRange.Value := GetIntSetting('override.rangelo', api.cgmRangeLo);
        fsHiRange.Value := GetIntSetting('override.rangehi', api.cgmRangeHi);
      end;

      cbTIR.Checked := native.GetBoolSetting('range.custom', true);
      seTir.Value := native.GetIntSetting('range.time', 9999);

      cbTirIcon.checked := native.GetBoolSetting('range.tir_icon', false);

      cbOffBar.Checked := native.GetBoolSetting('ux.off_bar', false);
      cbPaintHiLo.Checked := native.GetBoolSetting('ux.paint_range', true);
      cbPaintLines.Checked := native.GetBoolSetting('ux.paint_range_lines', false);
      cbPaintHiLoRange.Checked :=
        native.GetBoolSetting('ux.paint_range_cgmrange', false);
      edCommaSep.Text := GetCharSetting('locale.separator', '.');
      edTray.Value := GetIntSetting('ux.badge_size', 0);

      if CheckSetting('unit', 'mmol', 'mmol') then
        rbUnitClick(Self);

      cbCust.Checked := GetBoolSetting('override.enabled');
      cbCustRange.Checked := GetBoolSetting('override.range');
      cbPredictions.Checked := GetBoolSetting('predictions.enable');
      cbWebAPI.Checked := GetBoolSetting('webserver.enable');
      cbPredictShort.Checked := GetBoolSetting('predictions.short');
      // Load short arrow size
      // Load and clamp numeric size setting, mapping to combobox index
      sizeVal := native.GetIntSetting('predictions.short.size', 1);
      if sizeVal < 1 then
        sizeVal := 1;
      if sizeVal > 3 then
        sizeVal := 3;
      cbPredictShortSize.ItemIndex := sizeVal - 1;

      edMusicHigh.Text := GetSetting('media.url_high', '');
      edMusicLow.Text := GetSetting('media.url_low', '');
      edMusicPerfect.Text := GetSetting('media.url_perfect', '');

      edURLHigh.Text := GetSetting('url_remote.url_high', '');
      edURLLow.Text := GetSetting('url_remote.url_low', '');
      edURLPerfect.Text := GetSetting('url_remote.url_perfect', '');

      cbChroma.Checked := GetBoolSetting('razer.enabled', false);
      cbChromaNormal.Checked := GetBoolSetting('razer.normal', false);

      cbMusicPause.Checked := GetBoolSetting('media.pause');
      fsHi.Enabled := cbCust.Checked;
      fsLo.Enabled := cbCust.Checked;
      fsHiRange.Enabled := cbCustRange.Checked;
      fsLoRange.Enabled := cbCustRange.Checked;

      // User customizations
      userNamesCSV := GetRootSetting('users.names', '');
      lbUsers.Clear;
      lbUsers.Items.CommaText := userNamesCSV;
      if lbUsers.Items.Count < 1 then
        lbUsers.Enabled := false;

      lbUsers.Items.Add('- ' + RS_DEFAULT_ACCOUNT + ' -');
      cbUserColor.Checked := native.GetRootSetting('users.colorbox', 'true') = 'true';
      // Load position settings
      posValue := native.GetIntSetting('position.main', Ord(tpoCenter));

      cbPos.Items.Clear;
      for po in TrndiPos do
      begin
        posName := TrndiPosNames[po];
        cbPos.Items.Add(posName);

        // Match enum order with saved position
        if Ord(po) = posValue then
          cbPos.ItemIndex := Ord(po);
      end;

      // Fallback to first item if no valid match
      if cbPos.ItemIndex = -1 then
        cbPos.ItemIndex := 0;

      cbSize.Checked := GetBoolSetting('size.main');
      cbFlashHi.Checked := native.getBoolSetting('alerts.flash.high', false);
      cbFlashLow.Checked := native.getBoolSetting('alerts.flash.low', false);
      cbFlashPerfect.Checked := native.getBoolSetting('alerts.flash.perfect', false);
    end;

  end;

procedure LoadLanguageSettings(f: TfConf);
  var
    i: integer;
    langEntry: string;
  begin
    with f, native do
    begin
      // Load language files
      ListLanguageFiles(cbLang.Items, GetLangPath);
      cbLang.Items.Add('Trndi.en');
      cbLang.Items.Add('Trndi.auto');
      for i := 0 to cbLang.Items.Count - 1 do
      begin
        langEntry := cbLang.Items[i];
        cbLang.Items[i] := ExtractDelimited(2, langEntry, ['.']);
        langEntry := cbLang.Items[i];
        cbLang.Items[i] := Format('%s (%s)', [GetLanguageName(langEntry), langEntry]);
        if CheckSetting('locale', '', langEntry) then
          cbLang.ItemIndex := i;
      end;
      if cbLang.ItemIndex = -1 then
        cbLang.ItemIndex := cbLang.Items.Count - 1;
    end;
  end;

procedure SetupUIElements(f: TfConf);
  const
    dotdef = $2b24;
  begin
    with f do
    begin
      f.lDot.Caption := native.GetWideCharSetting('font.dot', System.widechar(dotdef));
      eDot.Text := native.GetSetting('font.dot', '2B24');
      lDotNow.Caption := native.GetWideCharSetting('font.dot_fresh',
        System.widechar(dotdef));
      eDotNow.Text := native.GetSetting('font.dot_fresh', '2600');
      f.lDot1.Caption := f.lDot.Caption;
      f.lDot2.Caption := f.lDot.Caption;
      f.lDot3.Caption := f.lDot.Caption;
      f.lDotCurr.Caption := f.lDotNow.Caption;
      f.Invalidate;

      // UI updates
      lVal.Font.Name := Self.lVal.Font.Name;
      lArrow.Font.Name := Self.lArrow.Font.Name;
      lAgo.Font.Name := Self.lAgo.Font.Name;
      lVal.Font.Color := Self.lVal.Font.Color;
      lArrow.Font.Color := Self.lArrow.Font.Color;
      lAgo.Font.Color := Self.lAgo.Font.Color;
      lVal.Caption := Self.lVal.Caption;
      lArrow.Caption := Self.lArrow.Caption;
      lAgo.Caption := Self.lAgo.Caption;
      pnDisplay.Color := Self.Color;
      pnDisplay.Font := fBG.Font;
      pnFonts.Font := eAddr.Font; // Override fBG.font for the panel

      lDot1.Font.Color := self.lDot1.Font.Color;
      lDot2.Font.Color := self.lDot1.Font.Color;
      lDot3.Font.Color := self.lDot1.Font.Color;

      cl_ok_bg.ButtonColor := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
      cl_hi_bg.ButtonColor := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
      cl_lo_bg.ButtonColor := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);

      cl_ok_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
      cl_hi_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
      cl_lo_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

      cl_hi_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
      cl_lo_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

      cl_hi_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
      cl_lo_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);

      cbTitleColor.Checked := native.GetBoolSetting('ux.title_color', true);

      cbTirColor.Checked := native.GetBoolSetting('ux.tir_color_on');
      cbTirColorCustom.Checked := native.GetBoolSetting('ux.tir_color_custom_on');

      cbTirColorBg.Checked := not cbTirColor.Checked;
      cbTirColorBgCustom.Checked := not cbTirColorCustom.Checked;

      cbTirBar.ButtonColor := native.GetColorSetting('ux.tir_color');
      cbTirBarCustom.ButtonColor := native.GetColorSetting('ux.tir_color_custom');
    end;
  end;

procedure SetupExtensions(f: TfConf);
  begin
    with f, native do
    begin
      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
      {$else}
      eExt.Text := '- ' + RS_noPlugins + ' -';
      eExt.Enabled := false;
      {$endif}
      cbPrivacy.Checked := GetBoolSetting('ext.privacy');
    end;
  end;

procedure SetupTouchAndNotifications(f: TfConf);
  begin
    with f, native do
    begin
      cbTouch.Checked := HasTouch;
      cbMultiTouch.Checked := HasMultiTouch;
    end;
  end;

procedure SaveUserSettings(f: TfConf);
  var
    langCode: string;
    i: integer;
  begin
    with f, native do
    begin
      if TryStrToInt('$' + eDot.Text, i) then
        SetWideCharSetting('font.dot', System.widechar(i));
      if TryStrToInt('$' + eDotNow.Text, i) then
        SetWideCharSetting('font.dot_fresh', System.widechar(i));

      SetSetting('font.val', lVal.Font.Name);
      SetSetting('font.arrow', lArrow.Font.Name);
      SetSetting('font.ago', lAgo.Font.Name);
      langCode := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);
      SetSetting('locale', langCode);
      native.SetSetting('position.main', IntToStr(cbPos.ItemIndex));
      native.setBoolSetting('size.main', cbSize.Checked);
      native.setBoolSetting('alerts.flash.high', cbFlashHi.Checked);
      native.setBoolSetting('alerts.flash.low', cbFlashLow.Checked);
      native.setBoolSetting('alerts.flash.perfect', cbFlashPerfect.Checked);

      for i := lbUsers.Items.Count - 1 downto 0 do
        if lbUsers.items[i][1] = '-' then
          lbUsers.items.Delete(i);

      // Handle user list changes
      if lbUsers.Count > 0 then
        SetSetting('users.names', lbUsers.Items.CommaText)
      else
        SetSetting('users.names', '');

      native.SetRootSetting('users.colorbox', IfThen(cbUserColor.Checked,
        'true', 'false'));

      // Save remote and override settings
      SetSetting('remote.type', cbSys.Text);
      SetSetting('remote.target', eAddr.Text);
      SetSetting('remote.creds', ePass.Text);
      SetSetting('unit', IfThen(rbUnit.ItemIndex = 0, 'mmol', 'mgdl'));
      SetBoolSetting('ext.privacy', cbPrivacy.Checked);

      SetSetting('system.fresh_threshold', IntToStr(spTHRESHOLD.Value));

      // Save unit-specific settings
      if rbUnit.ItemIndex = 0 then
      begin // mmol
        SetSetting('override.lo', Round(fsLo.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.hi', Round(fsHi.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value * TrndiAPI.toMGDL).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(fsLo.Value).ToString);
        SetSetting('override.hi', Round(fsHi.Value).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value).ToString);
      end;

      native.SetBoolSetting('range.custom', cbTIR.Checked);
      native.SetBoolSetting('range.tir_icon', cbTirIcon.checked);

      native.SetSetting('range.time', IntToStr(seTir.Value));
      native.SetBoolSetting('ux.off_bar', cbOffBar.Checked);
      native.SetBoolSetting('ux.paint_range', cbPaintHiLo.Checked);
      native.SetBoolSetting('ux.paint_range_lines', cbPaintLines.Checked);
      native.SetBoolSetting('ux.paint_range_cgmrange', cbPaintHiLoRange.Checked);
      native.SetSetting('locale.separator', edCommaSep.Text);
      native.SetSetting('ux.badge_size', edTray.Value.ToString);

      SetBoolSetting('override.enabled', cbCust.Checked);
      SetBoolSetting('override.range', cbCustRange.Checked);
      SetBoolSetting('predictions.enable', cbPredictions.Checked);
      SetBoolSetting('webserver.enable', cbWebAPI.Checked);
      SetBoolSetting('predictions.short', cbPredictShort.Checked);
      if cbPredictShortSize.ItemIndex >= 0 then
        SetSetting('predictions.short.size', IntToStr(cbPredictShortSize.ItemIndex + 1));
      SetSetting('media.url_high', edMusicHigh.Text);
      SetSetting('media.url_low', edMusicLow.Text);
      SetSetting('media.url_perfect', edMusicPerfect.Text);

      SetSetting('url_remote.url_high', edURLHigh.Text);
      SetSetting('url_remote.url_low', edURLLow.Text);
      SetSetting('url_remote.url_perfect', edURLPerfect.Text);

      SetBoolSetting('razer.enabled', cbChroma.Checked);
      SetBoolSetting('razer.normal', cbChromaNormal.Checked);

      SetBoolSetting('media.pause', cbMusicPause.Checked);

      SetColorSetting('ux.bg_color_ok', cl_ok_bg.ButtonColor);
      SetColorSetting('ux.bg_color_hi', cl_hi_bg.ButtonColor);
      SetColorSetting('ux.bg_color_lo', cl_lo_bg.ButtonColor);

      SetColorSetting('ux.bg_color_ok_txt', cl_ok_txt.ButtonColor);
      SetColorSetting('ux.bg_color_hi_txt', cl_hi_txt.ButtonColor);
      SetColorSetting('ux.bg_color_lo_txt', cl_lo_txt.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi', cl_hi_bg_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo', cl_lo_bg_cust.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi_txt', cl_hi_txt_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo_txt', cl_lo_txt_cust.ButtonColor);

      native.SetBoolSetting('ux.title_color', cbTitleColor.Checked);

      native.SetBoolSetting('ux.tir_color_on', cbTirColor.Checked);
      native.SetBoolSetting('ux.tir_color_custom_on', cbTirColorCustom.Checked);

      native.SetColorSetting('ux.tir_color', cbTirBar.ButtonColor);
      native.SetColorSetting('ux.tir_color_custom', cbTirBarCustom.ButtonColor);
    end;
  end;

var
  extensionsPath: string;
  i: integer;
  distro: string;
begin
  fConf := TfConf.Create(Self);
  try
    with native do
    begin
      fConf.cbSys.Items.Clear;
      // Populate available backends
      fConf.cbSys.Items.AddStrings([
        API_NS,
        API_NS3,
        API_DEX_USA,
        API_DEX_EU,
        API_XDRIP
      ]);
      {$ifdef DEBUG}
      fConf.cbSys.Items.AddStrings(API_DEBUG);
      {$endif}
    end;

     fConf.chroma := TRazerChromaFactory.CreateInstance;
     if not fconf.chroma.Initialize then
       fConf.lbChroma.Items.Add('No Razer driver detected')
     else begin
       for i := 0 to fConf.Chroma.GetDeviceCount - 1 do
        fConf.lbChroma.Items.Add(fConf.Chroma.GetDevice(i).Name);
     end;
      fConf.Chroma.Free;

    // Initialize form with user settings
    LoadUserSettings(fConf);
    LoadLanguageSettings(fConf);
    SetupUIElements(fConf);
    SetupExtensions(fConf);
    SetupTouchAndNotifications(fConf);

    // Store current user count for comparison later
    lastUsers := fConf.lbUsers.Count;

    {$if defined(X_PC)}
    fConf.lOS.Caption := GetLinuxDistro(distro) + ' ' + distro;

      {$if defined(LCLQt6)}
      fConf.lWidgetset.Caption := 'QT6 ' + qtVersion;
      {$elseif defined(LCLGTK2)}
      fConf.lWidgetset.Caption := 'GTK2';
      {$elseif defined(LCLGTK3)}
      fConf.lWidgetset.Caption := 'GTK3';
      {$endif}
    {$elseif defined(X_MAC)}
    fConf.lOS.Caption := 'macOS';
    fConf.lWidgetset.Caption := 'Native Apple Coca';
    {$else}
    fConf.lOS.Caption := 'Windows'  + SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
    fConf.lWidgetset.Caption := 'Native Windows';
    {$endif}
    fConf.lArch.Caption := {$I %FPCTargetOS%} + '/' + {$I %FPCTARGETCPU%};
    // Show dialog (use safe helper that handles problematic WMs)

    {$ifdef TrndiExt}
    extensionsPath := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
    // Find extensions folder
    ForceDirectoriesUTF8(extensionsPath);
    // Create the directory if it doesn't exist
    fConf.lbExtensions.Items := FindAllFiles(extensionsPath, '*.js', false);
    fConf.lExtCount.Caption := Format(RS_ExtCount, [fConf.lbExtensions.Count]);
    {$else}
    fConf.tsExt.Enabled := false;
    fConf.tsExt.Visible := false;
    fConf.lbExtensions.items.add('- Extensions disabled -');
    {$endif}
    ShowFormModalSafe(fConf);

    if not firstboot then
    begin
      if IsProblematicWM then
        fBG.Hide;
      if ExtText(uxdAuto, RS_SETTINGS_SAVE,RS_SETTINGS_SAVE_DESC,[mbYes, mbNo]) <> mrYes then
      begin
        fBG.Show;
        Exit; // FConf.Free will run later
      end;
      fBG.Show;
    end;
    // Reload settings, needed on X_PC
    native.ReloadSettings;

    // Reload prediction settings
    PredictGlucoseReading := native.GetBoolSetting('predictions.enable', false);
    PredictShortMode := native.GetBoolSetting('predictions.short', false);
    PredictShortSize := native.GetIntSetting('predictions.short.size', 1);
    if PredictShortSize < 1 then
      PredictShortSize := 1
    else if PredictShortSize > 3 then
      PredictShortSize := 3;
    lPredict.Visible := PredictGlucoseReading;
    // Recalculate layout and scale to account for potential short-size change
    Self.OnResize(nil);

    // Save settings when dialog closes
    SaveUserSettings(fConf);
    ShowMessage(RS_RESTART_APPLY);

    if firstboot then
      exit;
    //    SetLang;
    miForce.Click;
  finally
    fConf.Free;
  end;
end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
var
  isdot: boolean;
  l: TDotControl;
  {$ifdef TrndiExt}
  fs: TFormatSettings;
  {$endif}
begin
  l := Sender as TDotControl;

  actOnTrend(@ExpandDot);
  isDot := UnicodeSameText(l.Caption, DOT_GRAPH);

  {$ifdef TrndiExt}
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  callFunc('dotClicked',[
    isDot, // is the dot "open" as in viewing the value
    StrToFloat(l.Hint) * BG_CONVERTIONS[mgdl][un],
    StrToFloat(l.Hint) * BG_CONVERTIONS[mmol][un],
    l.tag
    ]);
  {$endif}

  if isDot then
    tResize.OnTimer(self);
end;

procedure TfBG.pnOffReadingPaint(Sender: TObject);
var
  Panel: TPanel;
  X, Y: integer;
  TextStr: string;
  TextW, TextH: integer;
begin
  Panel := Sender as TPanel;
  TextStr := Panel.Caption;
  if TextStr = '' then
    Exit;
  with Panel.Canvas do
  begin
    Brush.Color := Panel.Color;
    Brush.Style := bsSolid;
    FillRect(Classes.Rect(0, 0, Panel.Width, Panel.Height));
    Font.Assign(Panel.Font);
    Font.Orientation := 900; // 2700;

    font.Height := panel.Width;
    Brush.Style := bsClear;
    TextW := TextWidth(TextStr);
    TextH := TextHeight(TextStr);
    // Center
    X := (Panel.Width - TextH) div 2;
    Y := (Panel.Height + TextW) div 2;
    TextOut(X, Y, TextStr);
  end;
end;

procedure TfBG.pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
const
  PaddingHorizontal = 40; // Additional width padding for icons or spacing
  PaddingVertical = 8;    // Vertical padding for spacing
  MinItemWidth = 100;     // Minimum width for menu items
  MinItemHeight = 25;     // Minimum height for menu items
begin
  // Validate the input
  if not (Sender is TMenuItem) then
    Exit;

  with TMenuItem(Sender) do
  begin
    // Skip separators (e.g., "-")
    if Caption = '-' then
      Exit;

    // Set desired font properties
    with ACanvas.Font do
    begin
      Name := 'Arial';
      Size := 16;
      Style := [fsBold];
    end;

    // Calculate text dimensions
    AWidth := ACanvas.TextWidth(Caption) + PaddingHorizontal;
    AHeight := ACanvas.TextHeight(Caption) + PaddingVertical;

    // Enforce minimum dimensions
    AWidth := Max(AWidth, MinItemWidth);
    AHeight := Max(AHeight, MinItemHeight);
  end;
end;

procedure TfBG.pmSettingsPopup(Sender: TObject);
var
  shifted: boolean;
{$ifdef LCLQt6}
function SafeQtStyle(Handle: QWidgetH; const Style: string): boolean;
  var
    ws: widestring;
  begin
    Result := false;

    // Safey checks for Qt6
    if Handle = nil then
      Exit;
    if PtrUInt(Handle) < $1000 then
      Exit;

    try
      // Test a simple operation
      if not QWidget_isEnabled(Handle) and QWidget_isEnabled(Handle) then
        Exit; // If false, the handle is not OK

      ws := UTF8Decode(Style);
      QWidget_setStyleSheet(Handle, PWideString(@ws));
      Result := true;

    except
      Result := false;
    end;
  end;

function ShiftKeyPressed: boolean;
var
  modifiers: QtKeyboardModifiers;
begin
  modifiers := QGuiApplication_keyboardModifiers;
  Result := (modifiers and QtShiftModifier) <> 0;
end;
{$else}
function ShiftKeyPressed: boolean;
begin
  Result := ssShift in GetKeyShiftState;
end;
{$endif}
var
  tpb: TDotControl;
  H, M: integer;
begin
  // Shift down
  Application.ProcessMessages;
  try
    shifted := ShiftKeyPressed;
  except
    shifted := false;
  end;
  miAdvanced.Visible := shifted;
  miATouch.Checked := HasTouch;
  {$ifdef DEBUG}
  miDebugBackend.Visible := true;
  {$endif}

  miDotNormal.Checked := false;
  miDotBig.Checked := false;
  miDotHuge.Checked := false;
  miDotVal.Checked := false;

  if dotscale = 1 then
    miDotNormal.Checked := true
  else
  if dotscale = 2 then
    miDotBig.Checked := true
  else
  if dotscale = 3 then
    miDotHuge.Checked := true
  else
  if dotscale = 0.7 then
    miDotSmall.Checked := true
  else
  begin
    miCustomDots.Checked := true;
    miCustomDots.Caption := Format(RS_CUSTOM_DOTS, [dotscale]);
  end;

  if (Sender as TPopupMenu).PopupComponent is TDotControl then
  begin
    tpb := (Sender as TPopupMenu).PopupComponent as TDotControl;
    H := tpb.Tag div 100;
    M := tpb.Tag mod 100;

    miDotVal.Visible := true;
    miDotVal.Caption := Format(sReadingHere, [tpb.hint, H, M]);
  end
  else
    miDotVal.Visible := false;
  last_popup := now;
end;

procedure TfBG.pnMultiUserClick(Sender: TObject);
begin
  if username <> '' then
  begin
    if multinick = username then
      ShowMessage(Format(RS_MULTINAME, [username]))
    else
      ShowMessage(Format(RS_MULTINAME_NAMED, [multinick, username]));
  end
  else
  if multinick <> RS_DEFAULT_ACCOUNT then
    ShowMessage(Format(RS_MULTINAME_DEF_NAMED, [multinick]))
  else
    ShowMessage(RS_MULTINAME_DEF);
end;
// Handle off range panel click
procedure TfBG.pnOffRangeClick(Sender: TObject);
var
  ishigh: boolean;
begin
  ishigh := (Sender as TPanel).Color = bg_rel_color_hi;
  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'range',
    ishigh
    ], true) then
    Exit;
  {$endif}

  ShowMessage(Format(RS_RANGE_EXPLANATION, [IfThen(ishigh, RS_OVER, RS_UNDER)]));
end;

procedure TfBG.tAgoTimer(Sender: TObject);
var
  d: TDateTime;
  min: int64;
begin
  if firstboot then
    exit; // Dont run on first boot

  if sizeof(bgs) < 1 then
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO
  else
  try
    d := lastReading.date; // Last reading time
    min := MilliSecondsBetween(Now, d) div MILLIS_PER_MINUTE;  // Minutes since last
    {$ifndef lclgtk2}// UTF support IS LIMITED
    lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
    {$else}
    lAgo.Caption := '⌚ ' + Format(RS_LAST_UPDATE, [min]);
    {$endif}
  except
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
  end;
end;

procedure TfBG.tClockTimer(Sender: TObject);
var
  clockText: string;
  {$ifdef TrndiExt}
  ex: boolean; // If the function exists
  {$endif}
begin
  tClock.Enabled := false;
  if tClock.Interval <> clockDisplay then
  begin
    {$ifdef TrndiExt}
    clockText := '';
      // Check if JS engine is still available before calling
    clockText := callFuncWithBGReadings('clockView', [DateTimeToStr(Now)], ex);
    if ex = false then
      lval.caption := FormatDateTime(FormatSettings.ShortTimeFormat, Now)
    else
      lval.caption := clockText;
    {$else}
    lval.Caption := FormatDateTime(FormatSettings.ShortTimeFormat, Now);
    {$endif}
    tClock.Interval := clockDisplay;
    lArrow.Visible := false;
  end
  else
  begin
    lval.Caption := lval.hint;
    tClock.Interval := clockInterval;
    lArrow.Visible := true;
  end;
  tClock.Enabled := true;
end;

procedure TfBG.tEdgesTimer(Sender: TObject);
begin

end;

procedure TfBG.tInitTimer(Sender: TObject);
begin
  tInit.Enabled := false;
  self.Width := self.Width + 1;
  // Force a natural resize call, calling onResize doesnt work
end;

procedure TfBG.tPingTimer(Sender: TObject);
{$ifdef Windows}
 function IsURLReachable(const URL: string; Port: Word = 80): Boolean;
var
  XSocket: TSocket;
  Addr: TSockAddr;
  HostEnt: PHostEnt;
begin
  Result := False;

  XSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if XSocket = INVALID_SOCKET then
    Exit;

  try
    try
      // DNS resolution
      HostEnt := gethostbyname(PChar(URL));
      if HostEnt = nil then
        Exit;

      // Set up address
      FillByte(Addr, SizeOf(Addr), 0);
      PSockAddrIn(@Addr)^.sin_family := AF_INET;
      PSockAddrIn(@Addr)^.sin_port := htons(Port);
      PSockAddrIn(@Addr)^.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;

      // Try to connect
      if connect(XSocket, Addr, SizeOf(Addr)) = 0 then
        Result := True;
    except
      Result := False;
    end;
  finally
    closesocket(XSocket);
  end;
end;
{$else}
function IsURLReachable(const URL: string; Port: Word = 80): Boolean;
var
  Socket: Longint;
  Addr: TInetSockAddr;
  HostAddr: THostAddr;
begin
  Result := False;

  Socket := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Socket < 0 then
    Exit;

  try
    try
      HostAddr := StrToHostAddr(URL);

      if HostAddr.s_addr = 0 then
        Exit;

      FillByte(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(Port);
      Addr.sin_addr := HostAddr;

      if fpConnect(Socket, @Addr, SizeOf(Addr)) = 0 then
        Result := True;
    except
      Result := False;
    end;
  finally
    CloseSocket(Socket);
  end;
end;
{$endif}
function IsInternetOnline: Boolean;
var
  IPs: array of string;
  i: Integer;
begin
  IPs := [
    '8.8.8.8',       // Google DNS
    '1.1.1.1',       // Cloudflare DNS
    '208.67.222.222' // OpenDNS
  ];

  for i := Low(IPs) to High(IPs) do
  begin
    if IsURLReachable(IPs[i], 53) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;
begin
  tPing.Enabled := false;
if not IsInternetOnline then
  ShowMessage(RS_NO_INTERNET)
else if (sender = miDNS) then
  ShowMessage(RS_DNS_INTERNET_OK);

  tPing.Enabled := true;
end;

procedure TfBG.tResizeTimer(Sender: TObject);
begin
  tResize.Enabled := false;

  if DraggingWin then
  begin
    // During dragging, only update the warning panel and essential elements
    // Skip complex operations that could interfere with dragging performance
    if pnWarning.Visible then
    begin
      fixWarningPanel;
      ApplyAlphaControl(pnWarning, 235);
    end;
    Exit; // Don't do full resize operations during dragging
  end;

  // Normal full resize operations when not dragging
  actOnTrend(@HideDot);

  // Update trend related elements
  UpdateTrendElements;

  if not Assigned(api) then
    Exit;

  // Update meny and intervall
  UpdateApiInformation;

  if not api.active then
    Exit;

  // Adjust UI based on form size
  ResizeUIElements;

  // Place the dots
  UpdateTrendDots;
  actOnTrend(@showDot);

  // Post-process the dots
  AdjustGraph;

  fixWarningPanel;

  if lDot1.Caption <> DOT_GRAPH then
    actOnTrend(@ExpandDot);
end;

procedure TfBG.UpdateTrendElements;
begin
  // Change dot placement
  actOnTrend(@SetDotWidth);

  // Change dot size
  actOnTrend(@ResizeDot);
end;

procedure TfBG.UpdateApiInformation;
begin
  // Update high and low info label/menu item
  miHi.Caption := Format(RS_HI_LEVEL, [api.cgmHi * BG_CONVERTIONS[un][mgdl]]);
  miLo.Caption := Format(RS_LO_LEVEL, [api.cgmLo * BG_CONVERTIONS[un][mgdl]]);

  // Manage top intervall
  if api.cgmRangeHi <> 500 then
    miRangeHi.Caption := Format(RS_RANGE_HI, [api.cgmRangeHi * BG_CONVERTIONS[un][mgdl]])
  else
    miRangeHi.Caption := RS_RANGE_HI_UNSUPPORTED;

  // Hantera nedre intervall
  if api.cgmRangeLo <> 0 then
    miRangeLo.Caption := Format(RS_RANGE_LO, [api.cgmRangeLo * BG_CONVERTIONS[un][mgdl]])
  else
  begin
    miRangeLo.Caption := RS_RANGE_LO_UNSUPPORTED;
    miRangeColor.Enabled := false;
  end;
end;

procedure TfBG.ResizeUIElements;
const
  // Numerator for width/height ratios; denominator = 12
  PRED_WIDTH_NUM: array[1..3] of integer = (6, 8, 9);
  PRED_HEIGHT_NUM: array[1..3] of integer = (2, 3, 4);
var
  wnum, hnum: integer;
begin
  // Anpassa storleken på panelen
  pnOffRange.Height := ClientHeight div 10;

  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  CenterPanelToCaption(pnOffRange);
  pnOffRangeBar.Height := pnOffRange.Height div 4;
  pnOffRangeBar.Width := ClientWidth + 10;

  // Anpassa huvudetiketterna
  ScaleLbl(lVal);

  // Konfigurera differensvisning
  lDiff.Width := ClientWidth;
  lDiff.Height := (ClientHeight div 9) - 10;
  lDiff.Top := ClientHeight - lDiff.Height + 1;
  ScaleLbl(lDiff);

  // Konfigurera tidsvisning
  lAgo.Width := ClientWidth div 2;
  lAgo.Height := ClientHeight div 9;
  lAgo.Left := 5;
  lAgo.Top := 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  ScaleLbl(lAgo, taLeftJustify);

  // Konfigurera trendpil

  lArrow.Height := ClientHeight;
  lArrow.Width := ClientWidth;
  lArrow.left := 0;
  lArrow.top := 0;
  ScaleLbl(lArrow);

  // TIR
  ScaleLbl(lTir);
  lTir.BringToFront;
  lTir.AutoSize := false;
  lTir.font := lAgo.Font;
  lTir.Height := lTir.Canvas.TextHeight(lTir.Caption);
  lTir.Width := lTir.Canvas.TextWidth(lTir.Caption);
  {$ifdef LCLQt6}
  if isWSL then
    lTir.Width := 50;
  {$endif}
  lTir.left := ClientWidth - lTir.Width - 5;
  lTir.top := 0;


  //  lArrow.font.color := LightenColor(fBG.Color, 0.8);
  //  ScaleLbl(lArrow, taRightJustify, tlTop);
  lArrow.OptimalFill := true;

  pnMultiUser.Width := min(max(clientwidth div 30, 5), 50);
  pnMultiUser.Height := clientheight;
  pnMultiUser.top := 0;
  pnMultiUser.left := 0;
  
  // Position and scale lPredict in lower-right corner
  if Assigned(lPredict) and lPredict.Visible then
  begin
    if PredictShortMode then
    begin
      // Short mode: change size depending on setting predictions.short.size (1..3)
      wnum := PRED_WIDTH_NUM[Max(Min(PredictShortSize, 3), 1)];
      hnum := PRED_HEIGHT_NUM[Max(Min(PredictShortSize, 3), 1)];
      lPredict.Width := (ClientWidth * wnum) div 12;
      lPredict.Height := (ClientHeight * hnum) div 12;
    end
    else
    begin
      // Full mode: normal size for detailed predictions
      lPredict.Width := ClientWidth div 3;
      lPredict.Height := ClientHeight div 12;
    end;
    lPredict.Left := ClientWidth - lPredict.Width - 5;
    lPredict.Top := ClientHeight - lPredict.Height - 5;
    ScaleLbl(lPredict, taRightJustify, tlBottom);
  end;
end;

procedure TfBG.UpdateTrendDots;
var
  Dot: TDotControl;
  Value: single; // Parsed from hint (user unit), then normalized to mmol/L
  ok: boolean;
  wasVisible: boolean;
begin
  for Dot in TrendDots do
  begin
    // Remember if dot was marked visible by PlaceTrendDots
    wasVisible := Dot.Visible;
    
    ok := TryStrToFloat(Dot.Hint, Value, native.locale);

    Dot.Font.Size := round((ClientWidth div 24) * dotscale);
    if ok then
    begin
      // Normalize value to mmol/L for placement math; the API may return
      // values in mg/dL while positioning calculation assumes mmol/L units.
      if un = mgdl then
        Value := Value * BG_CONVERTIONS[mmol][mgdl];

      // Use same placement routine as initial draw to keep behavior consistent
      if Assigned(Dot.Parent) then
        SetPointHeight(Dot, Value, Dot.Parent.ClientHeight)
      else
        SetPointHeight(Dot, Value, fBG.ClientHeight);

      Dot.Visible := true;
    end
    else
    if Dot.Hint <> '' then
    begin
      // Hint is set but can't be parsed - this is a problem
      // Keep visibility as it was set by PlaceTrendDots, but log the issue
      LogMessage(Format('Warning: Could not parse Hint "%s" for dot. Keeping visibility=%s',
        [Dot.Hint, BoolToStr(wasVisible, true)]));
      Dot.Visible := wasVisible;
    end
    else
      Dot.Visible := false// No hint means no data for this dot
    ;
  end;
end;

procedure TfBG.ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
begin
  if not ALabel.Visible then
    ALabel.Visible := true;

  if ALabel.Caption = '' then
    Exit; // No text

  // Check size
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 250;
    ALabel.Height := 250;
  end;

  // Format
  ALabel.AutoSize := false;
  ALabel.WordWrap := false;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  // Check visibility
  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  // Max size
  MaxWidth := ALabel.Width - 4; // Add padding
  MaxHeight := ALabel.Height - 4;

  // Find best font size
  Low := 1;
  High := 150;
  OptimalSize := 1;

  while Low <= High do
  begin
    Mid := (Low + High) div 2;
    ALabel.Font.Size := Mid;

    TextWidth := ALabel.Canvas.TextWidth(ALabel.Caption);
    TextHeight := ALabel.Canvas.TextHeight(ALabel.Caption);

    if (TextWidth <= MaxWidth) and (TextHeight <= MaxHeight) then
    begin
      OptimalSize := Mid;
      Low := Mid + 1;
    end
    else
      High := Mid - 1;
  end;

  // Set best font size
  ALabel.Font.Size := OptimalSize;

  // Se till att inställningarna används
  ALabel.Refresh;
end;
// Update remote on timer
procedure TfBG.tMainTimer(Sender: TObject);
{$ifdef TrndiExt}
var
  bgvals: JSValueRaw;
  {$endif}
begin
  updateReading;
  {$ifdef TrndiExt}
  try

    bgvals := getBGResults;
    callFuncWithBGReadings('updateCallback', [DateTimeToStr(Now)]);
  finally
  end;
  {$endif}
end;

procedure TfBG.tMissedTimer(Sender: TObject);
var
  d: TDateTime;
  min, sec: int64;
begin
  if firstboot then
    exit;

  d := lastReading.date; // Last reading time

  min := MilliSecondsBetween(Now, d) div MILLIS_PER_MINUTE;  // Minutes since last
  sec := (MilliSecondsBetween(Now, d) mod MILLIS_PER_MINUTE) div 1000; // Seconds since last

  lDiff.Caption := Format(RS_OUTDATED_TIME, [FormatDateTime('H:mm', d), min, sec]);
end;

procedure TfBG.tSetupTimer(Sender: TObject);
begin
  tSetup.Enabled := false;
  UpdateUIColors; // Force an update of TIR
end;

procedure TfBG.tSwapTimer(Sender: TObject);
var
  c: TColor;
  currentCaption: string;
begin
  // Dont swap when the reading is old
  if fsStrikeOut in Lval.Font.Style then
  begin
    lArrow.Visible := true;
    Exit;
  end;

  tSwap.Enabled := false;
  currentCaption := lval.Caption;

  if currentCaption.IndexOf(':') > 0 then // Clock showing
    UpdateUIColors   // Resets standard coloring
  else
  if lVal.font.color <> lArrow.font.color then
  begin // Neutral mode
    lArrow.BringToFront;
    c := lArrow.Font.Color;
    lArrow.Font.color := lVal.Font.color;
    lVal.Font.color := c;
  end
  else
  begin
    UpdateUIColors;   // Resets standard coloring
    lArrow.SendToBack;
    lVal.BringToFront;
    lArrow.SendToBack;
  end;
  tSwap.Enabled := true;
end;

// Handle a touch screen's long touch
procedure TfBG.tTouchTimer(Sender: TObject);
var
  p: TPoint;
  touchDuration: integer;
begin
  tTouch.Enabled := false;
  // Ensure we have a valid touch state and touch screen capability
  if IsTouched and HasTouch and (StartTouch > 0) then
  begin
    // Check if this is actually a long press (at least 450ms to account for timer precision)
    touchDuration := MilliSecondsBetween(Now, StartTouch);
    if touchDuration >= 450 then
    begin
      p := Mouse.CursorPos;
      if SecondsBetween(Now, last_popup) > 2 then
      begin
        last_popup := now;
        if ((not native.HasTouchScreen) or semiTouchMode) then begin
          sleep(100); // Let getshiftstate catch up
          Application.ProcessMessages;
          pmSettings.PopUp(p.X, p.Y);
        end
        else begin
          pnTouchMenu.Width := Width;
          pnTouchMenu.Height := Height;
          pnTouchMenu.top := 0;
          pnTouchMenu.left := 0;
          pnTouchMenu.Font.Color := clBlack; // Fix RPi issue
          bTouchFull.width := width div 3;
          bTouchSettings.height := height div 4;
          bMenuPanelClose.height := height div 4;
          bTouchFull.font.size := lDiff.Font.size;
          bTouchSettings.font.size := lDiff.Font.size;
          bMenuPanelClose.font.size := lDiff.Font.size;
          bTouchMenu.font.size := lDiff.Font.size;
          pnTouchMenu.Show;
        end;
      end;
    end;
  end;
end;

// Request data from the backend and update GUI
procedure TfBG.ProcessCurrentReading;
var
  reading: BGReading;
begin
  if firstboot then
    exit;
  reading := lastReading;

  // Update value label
  if not privacyMode then
  begin
    if reading.val > api.limitHI then
      lVal.Caption := RS_HIGH
    else
    if reading.val < api.limitLO then
      lVal.Caption := RS_LOW
    else
      lVal.Caption := reading.format(un, BG_MSG_SHORT, BGPrimary);
  end
  else
    lVal.Caption := '';

  lval.hint := lval.Caption;

  // Update other UI elements
  lDiff.Caption := reading.format(un, BG_MSG_SIG_SHORT, BGDelta);
  lArrow.Caption := reading.trend.Img;
  lVal.Font.Style := [];

  // Log latest reading
  LogMessage(Format(RS_LATEST_READING, [reading.val, DateTimeToStr(reading.date)]));

  // Announce
  if miAnnounce.Checked then
    speakReading;

  // Set next update time
  SetNextUpdateTimer(reading.date);
end;

function TfBG.IsDataFresh: boolean;
var
  reading: BGReading;   // Holder for the latest (newest) reading
  i: integer;     // Temp int used when checking if caption starts with a digit
begin
  if firstboot then
    exit;

  reading := lastReading; // Pick the most recent reading from the buffer

  // Consider data fresh if the latest reading is within the configured threshold (in minutes)
  Result := MinutesBetween(Now, reading.date) <= DATA_FRESHNESS_THRESHOLD_MINUTES;

  if not Result then
  begin
    tMissed.OnTimer(tMissed);         // Immediately trigger the "missed" handler
    lVal.Font.Style := [fsStrikeOut]; // Strike through the value to indicate stale data
    // If the value label isn't numeric (e.g., "Setup") or arrow is a placeholder, show neutral placeholders
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then
    begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';           // Placeholder when data is stale
      lArrow.Caption := '';           // Hide trend arrow when stale
    end;
    setColorMode(clBlack);
    lVal.Font.Color := clWhite;       // High-contrast text on dark background
    tMissed.Enabled := true;          // Keep the missed timer running
    lArrow.Caption := '';             // Dont show arrow when not fresh
    native.setBadge('--', clBlack, badge_width + badge_adjust,
      badge_font + round(badge_adjust * 10)); // Update system/taskbar badge
    native.StopBadgeFlash;            // Stop any flashing when stale
  end
  else
  begin
    tMissed.Enabled := false;         // Data is fresh; stop missed timer
    bg_alert := true;                 // Allow alerts again
    // Reset font color when data becomes fresh again (it may have been set to white when stale)
    if lVal.Font.Color = clWhite then
      lVal.Font.Color := GetTextColorForBackground(fBG.color);
  end;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
var
  secondsSinceLast: int64; // Seconds since last reading
  intervalMs: int64; // Computed interval in milliseconds
begin
  if firstboot then
    exit;
  tMain.Enabled := false;
  secondsSinceLast := SecondsBetween(LastReadingTime, now); // Seconds since last
  intervalMs := min(BG_REFRESH, BG_REFRESH - (secondsSinceLast * 1000));
  // 5 min or less if there's a recent reading
  intervalMs := max(MIN_REFRESH_INTERVAL_MS, intervalMs); // Don't allow too small refresh time (min 2 minutes)

  tMain.Interval := intervalMs + REFRESH_RESYNC_BUFFER_MS; // Add buffer to allow sync
  tMain.Enabled := true;

  miRefresh.Caption := Format(RS_REFRESH, [TimeToStr(LastReadingTime),
    TimeToStr(IncMilliSecond(Now, tMain.Interval))]);
  {$ifdef Darwin}
  upMenu.Caption:= miRefresh.Caption;
  {$endif}
end;

// Calculate time in range
procedure tfBG.CalcRangeTime;
var
  reading: BGReading; //< Holder for current reading iteration
  range, //< The OK range in percent
  ok, //< OK count
  no, //< Not OK count
  rangeMinutes: integer; //< Time window in minutes to inspect (custom setting)
  ranges: set of trndi.types.BGValLevel; //< Types of readings to count as OK
begin
  ok := 0;
  no := 0;
  if native.GetBoolSetting('range.custom', false) then
    ranges := [BGRange, BGRangeHI, BGRangeLO]
  else
    ranges := [BGRange];

  rangeMinutes := native.GetIntSetting('range.time', 9999);

  for reading in bgs do
    if reading.date >= IncMinute(now, rangeMinutes * -1) then
      if reading.level in ranges then
        Inc(ok)
      else
        Inc(no);

  if (ok + no) > 0 then
  begin
    range := round((ok / (ok + no)) * 100);
    if tir_icon then
      case range of
        100..MaxInt: lTir.Caption := '👌'; // MaxInt is unneeded really
        80..99: lTir.Caption := '😃';
        50..79: lTir.Caption := '😶';
        30..49: lTir.Caption := '😥';
        10..29: lTir.Caption := '😞';
        0..9: lTir.Caption := '😓';
      end
    else
      lTir.Caption := range.toString + '%';
  end
  else
  begin
    range := 0;
    lTir.Caption := range.toString + '-%';
  end;


  lTir.Hint := range.toString;
  lTir.Visible := true; // Ensure TIR label is visible after calculation
end;

function TfBG.updateReading(boot: boolean = false): boolean;
begin
  Result := false;
  lAgo.Caption := '⟳' + lAgo.Caption;

  native.start;
  lastup := 0;

  if not tAgo.Enabled then
    lAgo.Caption := '🕑 ' + RS_UNKNOWN_TIME;


  // Fetch readings and exit if no data
  if not FetchAndValidateReadings then
    Exit;

  // Process the newest reading
  ProcessCurrentReading;

  // Handle data freshness
  if not IsDataFresh then
  begin
    if not boot then
      Exit;
  end
  else
    Result := true;

  // Complete the UI update sequence
  FinalizeUIUpdate;

  {$ifdef TrndiExt}
  // Check if JS engine is still available before calling
  callFunc('fetchCallback',[
    bgs[0].format(mgdl, BG_MSG_SHORT), //mgdl reading
    bgs[0].format(mmol, BG_MSG_SHORT), //mmol reading
    bgs[0].format(mgdl, BG_MSG_SIG_SHORT, BGDelta), //mgdl diff
    bgs[0].format(mmol, BG_MSG_SHORT, BGDelta), //mmol diff
    not bgs[0].empty // has reading?
    ]);
  {$endif}
end;

procedure TfBG.FinalizeUpdate;
begin
  lastup := Now;

  // Handle privacy mode display
  if privacyMode then
  begin
    lval.Caption := privacyIcon(lastReading.level);
    lVal.hint := lval.Caption;
  end;

  // Update timers and UI
  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  Self.OnResize(lVal);

  // Update floating window if assigned
  UpdateFloatingWindow;

  // Update prediction label if enabled
  UpdatePredictionLabel;

  // Update text colors based on background only if UI changed
  if ShouldUpdateUI(fBG.Color, lVal.Caption, lTir.Caption, lTir.Color) then
  begin
    UpdateUIColors;
    CacheUIState(fBG.Color, lVal.Caption, lTir.Caption, lTir.color);
  end;

  // Update system integration
  native.setBadge(lVal.Caption, fBG.Color
    {$ifdef lclwin32}
    ,badge_width,badge_font
    {$endif}
    );
  native.done;

  if privacyMode then
    DOT_ADJUST := randomrange(-3, 3) / 10;

end;

procedure TfBG.UpdatePredictionLabel;
var
  bgr: BGResults;
  pred1, pred2, pred3: string;
  lastReadingTime: TDateTime;
  lastReadingValue: single;
  i, closest5, closest10, closest15: integer;
  diff5, diff10, diff15, currentDiff: integer;
  delta: single;
  trend: BGTrend;
  minutes: integer;
  validCount: integer;
begin
  // Safety check - api might not be initialized yet
  if not Assigned(api) then
  begin
    lPredict.Visible := false;
    Exit;
  end;
  
  if not PredictGlucoseReading then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  lPredict.Visible := true;

  // Request more predictions to get ones closer to 5, 10, 15 minutes
  if not api.predictReadings(9, bgr) then
  begin
    lPredict.Caption := RS_PREDICTIONS_UNAVAILABLE;
    Exit;
  end;

  // Get the last reading time and value to calculate trend
  lastReadingTime := lastReading.date;
  lastReadingValue := lastReading.convert(mgdl);

  // Find predictions closest to 5, 10, and 15 minutes
  // Ensure we pick different predictions for each slot
  closest5 := -1;
  closest10 := -1;
  closest15 := -1;
  diff5 := MaxInt;
  diff10 := MaxInt;
  diff15 := MaxInt;

  for i := 0 to High(bgr) do
  begin
    currentDiff := Round(MinutesBetween(bgr[i].date, lastReadingTime));
    
    // Skip predictions that are too close to current time (less than 2 minutes ahead)
    if currentDiff < 2 then
      continue;
    
    // Find closest to 5 minutes
    if Abs(currentDiff - 5) < diff5 then
    begin
      diff5 := Abs(currentDiff - 5);
      closest5 := i;
    end;
    
    // Find closest to 10 minutes (but different from closest5)
    if (Abs(currentDiff - 10) < diff10) and (i <> closest5) then
    begin
      diff10 := Abs(currentDiff - 10);
      closest10 := i;
    end;
    
    // Find closest to 15 minutes (but different from closest5 and closest10)
    if (Abs(currentDiff - 15) < diff15) and (i <> closest5) and (i <> closest10) then
    begin
      diff15 := Abs(currentDiff - 15);
      closest15 := i;
    end;
  end;

  // Check if we have at least one valid prediction
  validCount := 0;
  if closest5 >= 0 then
    Inc(validCount);
  if closest10 >= 0 then
    Inc(validCount);
  if closest15 >= 0 then
    Inc(validCount);
  
  if validCount = 0 then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  // Format predictions with clock emoji, trend arrows, and values
  if PredictShortMode then
  begin
    // Short mode: show only middle prediction with simplified arrow
    if closest10 >= 0 then
    begin
      delta := bgr[closest10].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      // Map to simplified arrows: up=↗, flat=→, down=↘
      case trend of
      TdDoubleUp, TdSingleUp, TdFortyFiveUp:
        lPredict.Caption := '↗';
      TdFlat:
        lPredict.Caption := '→';
      TdFortyFiveDown, TdSingleDown, TdDoubleDown:
        lPredict.Caption := '↘';
      else
        lPredict.Caption := '?';
      end;
    end
    else
      lPredict.Caption := '?';
  end
  else
  begin
    // Full mode: show time, trend, and value
    if closest5 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest5].date, lastReadingTime));
      delta := bgr[closest5].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred1 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest5].convert(un)]);
    end
    else
      pred1 := '?';

    if closest10 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest10].date, lastReadingTime));
      delta := bgr[closest10].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred2 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest10].convert(un)]);
    end
    else
      pred2 := '?';

    if closest15 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest15].date, lastReadingTime));
      delta := bgr[closest15].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred3 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest15].convert(un)]);
    end
    else
      pred3 := '?';

    lPredict.Caption := Format('%s | %s | %s', [pred1, pred2, pred3]);
  end;
end;

procedure TfBG.CompleteUIUpdate;
begin
  // Process the current reading and update UI
  ProcessCurrentReading;
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.FinalizeUIUpdate;
begin
  // Complete UI update sequence after data processing
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.UpdateFloatingWindow;
begin
  if Assigned(fFloat) then
  begin
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lArrow.Caption := lArrow.Caption;
  end;
end;

procedure TfBG.UpdateUIBasedOnGlucose;
var
  reading: BGReading;
  col: TColor;
  txt: string;
begin
  reading := lastReading;

  if reading.val >= api.cgmHi then
    HandleHighGlucose(reading)
  else
  if reading.val <= api.cgmLo then
    HandleLowGlucose(reading)
  else
    HandleNormalGlucose(reading);

  pnOffReading.Visible := native.GetBoolSetting('ux.off_bar', false);
  case reading.level of
  trndi.types.BGHigh:
    txt := RS_HIGH;
  trndi.types.BGLOW:
    txt := RS_LOW;
  trndi.types.BGRange:
  begin
    txt := '';
    pnOffReading.Visible := false;
  end;
  trndi.types.BGRangeHI:
    txt := RS_OFF_HI;
  trndi.types.BGRangeLO:
    txt := RS_OFF_LO;
  end;

  pnOffReading.Caption := txt;

  pnOffReading.color := GetAdjustedColorForBackground(fBG.Color, fBG.Color);
  pnOffReading.font.color := GetTextColorForBackground(pnOffReading.Color, 0, 0.7);
end;

procedure TfBG.HandleHighGlucose(const reading: BGReading);
var
  url: string;
  doFlash: boolean;
begin
  setColorMode(bg_color_hi);

  if not bg_alert then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_HI_TITLE),
      Format(RS_WARN_BG_HI, [lVal.Caption]));

  if highAlerted then
    Exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  if native.TryGetSetting('media.url_high', url) then
  begin
    highAlerted := true;
    MediaController.PlayTrackFromURL(url);
  end;
  if native.TryGetSetting('url_remote.url_high', url) then
  begin
    highAlerted := true;
    native.getURL(url, url);
  end;

  if assigned(native) and assigned(chroma) then
  if native.GetBoolSetting('razer.enabled', false) and chroma.Initialized then
  {$ifdef Windows}
    Chroma.SetStaticAll(clRazerRed);
  {$else}
      Chroma.SetBreathDualAll(clRazerRed, clRazerBlack);
  {$endif}

  doFlash := native.GetBoolSetting('alerts.flash.high', false);
  if (not highAlerted) and doFlash then
    native.StartBadgeFlash(lVal.Caption, bg_color_hi, BADGE_FLASH_DURATION_HIGH_MS, BADGE_FLASH_REPEAT_DELAY_HIGH_MS);
end;

procedure TfBG.HandleLowGlucose(const reading: BGReading);
var
  url: string;
  doFlash: boolean;
begin
  SetColorMode(bg_color_lo);

  if not bg_alert then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_LO_TITLE),
      Format(RS_WARN_BG_LO, [lVal.Caption]));

  if lowAlerted then
    exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  if native.TryGetSetting('media.url_low', url) then
  begin
    lowAlerted := true;
    MediaController.PlayTrackFromURL(url);
  end;
  if native.TryGetSetting('url_remote.url_low', url) then
  begin
    lowAlerted := true;
    native.getURL(url, url);
  end;
  doFlash := native.GetBoolSetting('alerts.flash.low', false);
  if (not lowAlerted) and doFlash then
    native.StartBadgeFlash(lVal.Caption, bg_color_lo, BADGE_FLASH_DURATION_LOW_MS, BADGE_FLASH_REPEAT_DELAY_LOW_MS);

  if assigned(native) and assigned(chroma) then
    if native.GetBoolSetting('razer.enabled', false) and chroma.Initialized then
    {$ifdef Windows}
      Chroma.SetStaticAll(clRazerBlue);
    {$else}
      Chroma.SetBreathDualAll(clRazerBlue, clRazerBlack);
    {$endif}
end;

procedure TfBG.HandleNormalGlucose(const reading: BGReading);
var
  formattedReading, url: string;
  parsedInt: integer;
  parsedFloat: single;
  go: boolean = false;
begin
  bg_alert := false;
  SetColorMode(bg_color_ok);
  highAlerted := false;
  lowAlerted := false;
  native.StopBadgeFlash; // cease alerts when normal

  if un = mmol then
  begin
    formattedReading := reading.format(mmol, BG_MSG_SHORT, BGPrimary);
    if (TryStrToFloat(formattedReading, parsedFloat, native.locale)) and (parsedFloat = 5.5) then
      go := true
    else
      perfectTriggered := false;
  end
  else
  begin
    formattedReading := reading.format(mgdl, BG_MSG_SHORT, BGPrimary);
    if (TryStrToInt(formattedReading, parsedInt)) and (parsedInt = 100) then
      go := true
    else
      perfectTriggered := false;
  end;

  if go and (not perfectTriggered) then
  begin
    perfectTriggered := true;

    if native.TryGetSetting('media.url_perfect', url) then
      MediaController.PlayTrackFromURL(url);
    if native.TryGetSetting('url_remote.url_high', url) then
      native.getURL(url, url);

    if native.GetBoolSetting('alerts.flash.perfect', false) then
      native.StartBadgeFlash(lVal.Caption, bg_color_ok, BADGE_FLASH_DURATION_OK_MS, BADGE_FLASH_REPEAT_DELAY_OK_MS);
    // subtle celebratory pulse
  end;

 if assigned(native) and assigned(chroma) then
    if chroma.Initialized and native.GetBoolSetting('razer.enabled', false) and native.GetBoolSetting('razer.normal', false) then
      Chroma.SetStaticAll(clRazerGreen);


  UpdateOffRangePanel(reading.val);
end;

procedure TfBG.fixWarningPanel;
var
  padding: integer;
  calculatedFontSize: integer;
  bg: BGReading;
  last, val: string;
begin
  // Calculate padding consistently
  padding := (ClientWidth div 25);
  if not native.HasTouchScreen then
    padding := padding * 2;

  // First, set panel dimensions and ensure it's properly configured
  pnwarning.AutoSize := false;
  pnwarning.Width := ClientWidth - (padding * 2);  // Use padding on both sides
  pnWarning.left := padding;
  pnwarning.top := padding;
  pnWarning.Height := ClientHeight - (padding * 2);  // Use padding on top and bottom

  // Configure the main warning label first
  if Pos(sLineBreak, lMissing.Caption) < 1 then // Ugly solution
    lMissing.Caption := '🕑' + sLineBreak + lMissing.Caption;

  lMissing.AutoSize := false;
  lMissing.left := 5;
  lMissing.top := 5;
  lMissing.Width := pnWarning.Width - 10;
  lMissing.Height := pnWarning.Height - 60; // Leave room for pnWarnLast at bottom
  lMissing.wordwrap := true;
  lMissing.OptimalFill := true;

  // Use the same scaling method as resize operations for consistency
  // This ensures the font size matches what you get when manually resizing
  ScaleLbl(lMissing, taCenter, tlCenter);

  // Ensure font color is visible
  if native.HasTouchScreen then
    pnWarning.Font.Color := pnWarning.color;

  // Configure other UI elements
  pnOffReading.Height := ClientHeight;
  pnOffReading.ClientWidth := ClientWidth div 35;

  if tryLastReading(bg) then
  begin
    val := bg.format(un, BG_MSG_SHORT);
    last := HourOf(bg.date).ToString + ':' + MinuteOf(bg.date).tostring;
    if DateOf(bg.date) <> Dateof(now) then
      last := last + ' ' + Format(RS_DAYS_AGO, [DaysBetween(now, bg.date)]);
    pnWarnLast.Caption := Format(RS_LAST_RECIEVE, [val, last]);
  end
  else
    pnWarnLast.Caption := RS_LAST_RECIEVE_NO;

  pnWarnLast.Caption := pnWarnLast.Caption + LineEnding +
    Format(RS_LAST_RECIEVE_AGE, [DATA_FRESHNESS_THRESHOLD_MINUTES]);

  // Set pnWarnLast font size relative to main font, but with bounds checking
  pnWarnLast.font.size := Max(8, Min(20, calculatedFontSize div 3));
  pnWarning.Canvas.Font.size := pnWarnLast.font.size;

  // Position pnWarnLast at bottom of panel
  pnWarnLast.Height := pnWarning.Canvas.TextHeight(pnWarnLast.Caption) * 2;
  pnWarnLast.Width := Min(pnWarning.Width - 10,
    pnWarning.Canvas.TextWidth(pnWarnLast.Caption) + 10);
  pnWarnLast.left := 5;
  pnWarnLast.top := pnWarning.Height - pnWarnLast.Height - 5;
end;

procedure TfBG.showWarningPanel(const message: string;
clearDisplayValues: boolean = false);
var
  i: integer;
begin
  pnWarning.Visible := true;
  tPing.Enabled := true;  // Enable network ping check when no readings available
  pnWarning.Caption := '⚠️ ' + message;

  if clearDisplayValues then
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then
    begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;

  pnWarning.Caption := '';  // Clear for now
  fixWarningPanel;
  // Ensure visual effects are applied when showing the panel
  ApplyAlphaControl(pnWarning, 235);

  // Force a complete UI update to ensure proper rendering on all platforms
  pnWarning.Refresh;
  lMissing.Refresh;
end;

function TfBG.DoFetchAndValidateReadings(const ForceRefresh: boolean): boolean;
{$ifdef DEBUG}
var
  res: string;
{$endif}
const
  API_CACHE_SECONDS = 10; // Don't call API more than once per 10 seconds
begin
  Result := false;

  if api = nil then
    Exit;

  // Performance optimization: use cached readings if recent (unless forced)
  if (not ForceRefresh) and (SecondsBetween(Now, FLastAPICall) < API_CACHE_SECONDS) and
    (Length(FCachedReadings) > 0) then
  begin
    bgs := FCachedReadings;
    Result := true;
    Exit;
  end;

  {$ifdef DEBUG}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
  if miDebugBackend.Checked then
    if Showing then
      if res.IsEmpty then
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '[empty!]', res)
      else
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '', res, uxmtCustom, 10);
  {$ELSE}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}

  // Cache the API call and results (used to reduce repeated calls when polling)
  FLastAPICall := Now;
  SetLength(FCachedReadings, Length(bgs));
  if Length(bgs) > 0 then
    Move(bgs[0], FCachedReadings[0], Length(bgs) * SizeOf(BGReading));

  // Reapply override settings after API fetch (API may have set its own defaults)
  // These settings allow users to override server-provided thresholds locally.
  if native.GetBoolSetting('override.enabled') then
  begin
    api.cgmLo := native.GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := native.GetIntSetting('override.hi', api.cgmHi);
  end;
  if native.GetBoolSetting('override.range') then
  begin
    api.cgmRangeLo := native.GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := native.GetIntSetting('override.rangehi', api.cgmRangeHi);
  end;

  pnWarning.Visible := false;
  tPing.Enabled := false;  // Disable network ping check when readings are available
  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    showWarningPanel(RS_NO_BACKEND, true);
    Exit;
  end;

  LogMessage(Format('DoFetchAndValidateReadings: Got %d readings from API', [Length(bgs)]));

  // Call the method to place the points
  PlaceTrendDots(bgs);
  Result := true;
end;

function TfBG.FetchAndValidateReadings: boolean;
begin
  Result := DoFetchAndValidateReadings(false); // Use cached data if available
end;

function TfBG.FetchAndValidateReadingsForced: boolean;
begin
  Result := DoFetchAndValidateReadings(true); // Force fresh API call, bypass cache
end;

procedure TfBG.UpdateOffRangePanel(const Value: double);
var
  on: boolean = true;
begin
  if (Value >= api.cgmHi) or (Value <= api.cgmLo) then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    if Assigned(fFloat) then
    begin
      ffloat.lRangeDown.Visible := false;
      ffloat.lRangeUp.Visible := false;
    end;
  end
  else
  if Value <= api.cgmRangeLo then
    DisplayLowRange
  else
  if Value >= api.cgmRangeHi then
    DisplayHighRange
  else
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    on := false;
  end;

  // Apply range color if option is enabled
  if on and miRangeColor.Checked then
    SetColorMode(pnOffRange.Color);
end;

procedure TfBG.UpdateUIColors;
const
  clGoodGreen = $00005900;
var
  r: integer;
begin
  lVal.Font.Color := GetTextColorForBackground(fBG.color);
  //  lVal.BringToFront;
  //  lArrow.Font.Color := LightenColor(fbg.color, 0.3); // GetTextColorForBackground(fBG.color, 0, 0.9);
  lArrow.Font.Color := LightenColor(fBG.color, 0.5);
  lArrow.SendToBack;
  lDiff.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lAgo.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lTir.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lPredict.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);


  if TryStrToInt(lTir.hint, r) then
    if r < bad_tir then // If the value is under the limit for "bad"
      lTir.Font.color := GetAdjustedColorForBackground(clMaroon,
        fBG.Color, 0.6, 0.4, true)
    else
    if r >= good_tir then
      lTir.Font.color := GetAdjustedColorForBackground(clGoodGreen,
        fBG.Color, 0.6, 0.4, true)// Check time in range
  ;

end;

function TfBG.GetTextColorForBackground(const BgColor: TColor;
const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
begin
  if IsLightColor(BgColor) then
    Result := DarkenColor(BgColor, DarkenFactor)
  else
    Result := LightenColor(BgColor, LightenFactor);
end;

function TfBG.GetAdjustedColorForBackground(const BaseColor: TColor;
const BgColor: TColor; const DarkenFactor: double = 0.6;
const LightenFactor: double = 0.4; const PreferLighter: boolean = false): TColor;
begin
  if PreferLighter then
    Result := LightenColor(BaseColor, LightenFactor)// Tvinga ljusare förgrund

  else
  if IsLightColor(BgColor) then
    Result := DarkenColor(BaseColor, DarkenFactor)
  else
    Result := LightenColor(BaseColor, LightenFactor);
end;

procedure TfBG.DisplayLowRange;
begin
  pnOffRange.Color := bg_rel_color_lo;
  pnOffRangeBar.Color := bg_rel_color_lo;
  pnOffRange.Font.Color := bg_rel_color_lo_txt;
  if miRangeColor.Checked then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRangeBar.Visible := true;
  pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
      ffloat.lRangeDown.Visible := true;
    ffloat.Font.color := bg_rel_color_lo_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

procedure TfBG.DisplayHighRange;
begin
  pnOffRange.Color := bg_rel_color_hi;
  pnOffRangeBar.Color := bg_rel_color_hi;
  pnOffRange.Font.Color := bg_rel_color_hi_txt;
  if miRangeColor.Checked then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
      ffloat.lRangeUp.Visible := true;
    ffloat.Font.color := bg_rel_color_hi_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

// PlaceTrendDots method to map readings to TrendDots
procedure TfBG.PlaceTrendDots(const Readings: array of BGReading);
var
  SortedReadings: array of BGReading;
  currentTime: TDateTime;
  currentHash: cardinal;
begin
  if Length(Readings) = 0 then
    Exit;

  // Performance optimization: check if data actually changed
  currentHash := CalculateReadingsHash(Readings);
  if currentHash = FLastReadingsHash then
    Exit; // No change, skip processing

  FLastReadingsHash := currentHash;

  // Prepare
  currentTime := Now;
  SetLength({%H-}SortedReadings, Length(Readings));
  Move(Readings[0], SortedReadings[0], Length(Readings) * SizeOf(BGReading));

  // Sort readings Descending (newest first)
  SortReadingsDescending(SortedReadings);

  // Manage the last readings freshness
  HandleLatestReadingFreshness(SortedReadings[0], currentTime);

  // Process every interval
  ProcessTimeIntervals(SortedReadings, currentTime);
end;

procedure TfBG.HandleLatestReadingFreshness(const LatestReading: BGReading;
CurrentTime: TDateTime);
var
  isFresh: boolean;
begin
  isFresh := MinutesBetween(CurrentTime, LatestReading.date) <=
    DATA_FRESHNESS_THRESHOLD_MINUTES;

  if Assigned(TrendDots[10]) then
  begin
    TrendDots[10].Visible := isFresh;

    if isFresh then
      LogMessage('TrendDots[10] shown as latest reading is fresh.')
    else
      LogMessage('TrendDots[10] hidden due to outdated reading.');
  end;
end;

procedure TfBG.ProcessTimeIntervals(const SortedReadings: array of BGReading;
CurrentTime: TDateTime);
var
  slotIndex, i, labelNumber, searchStart: integer;
  slotStart, slotEnd, anchorTime: TDateTime;
  found: boolean;
  reading: BGReading;
  l: TDotControl;
begin
  if Length(SortedReadings) = 0 then
    Exit;

  searchStart := 0;
  // Anchor intervals to the expected grid time, not directly to the latest
  // reading. This keeps the 5-minute slots stable ("...:00, :05, :10, ...")
  // and allows the rightmost slot to be empty when the latest reading is
  // older than one interval.

  // Example:
  //   Latest reading at 11:03.
  //   We snap CurrentTime to the nearest lower 5-minute boundary (10:58).
  //   Slot 0 then represents 10:53-10:58 and is empty.
  //   The reading at 11:03 will land in a later slot instead of being
  //   forced into the first one.

  // Snap the *visual* anchor to a 5-minute grid based on CurrentTime.
  // We keep seconds/milliseconds at 0 to avoid flicker.
  anchorTime := RecodeMinute(CurrentTime,
    (MinuteOf(CurrentTime) div INTERVAL_MINUTES) * INTERVAL_MINUTES);
  anchorTime := RecodeSecond(anchorTime, 0);
  anchorTime := RecodeMilliSecond(anchorTime, 0);
  
  LogMessage(Format('PlaceTrendDots: Processing %d readings, anchor=%s (latest=%s)', 
    [Length(SortedReadings), DateTimeToStr(anchorTime), DateTimeToStr(SortedReadings[0].date)]));

  for slotIndex := 0 to NUM_DOTS - 1 do
  begin
  // Set start and end time for the interval, anchored to the snapped grid time
  // (see comment above). For slot 0, this is the most recent visual 5-minute
  // bucket, which may be empty if no reading falls inside it.
    slotEnd := IncMinute(anchorTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    found := false;

    LogMessage(Format('Searching slot %d (TrendDots[%d]): %s to %s', 
      [slotIndex, NUM_DOTS - slotIndex, DateTimeToStr(slotStart), DateTimeToStr(slotEnd)]));

    // Search for the most recent reading within this interval
    for i := searchStart to High(SortedReadings) do
    begin
      reading := SortedReadings[i];

      // For slot 0, be more lenient with the upper bound to catch the anchor reading
      // Use a 10 second epsilon to handle timing variations
      if reading.date > slotEnd + (10 / 86400) then
      begin
        LogMessage(Format('  Reading at %s is too new (>%.3f sec after slot end), skipping', 
          [DateTimeToStr(reading.date), (reading.date - slotEnd) * 86400]));
        Continue;
      end;

      // Check if reading falls within this interval BEFORE checking if it's too old
      // This ensures boundary readings (exactly at slotStart) get matched
      if (reading.date >= slotStart - (10 / 86400)) and (reading.date <= slotEnd + (10 / 86400)) then
      begin
        LogMessage(Format('  Found match at %s (value: %.1f, diff from slotEnd: %.1f sec)', 
          [DateTimeToStr(reading.date), reading.val, (slotEnd - reading.date) * 86400]));
        found := UpdateLabelForReading(slotIndex, reading);
        if found then
        begin
          // Only advance searchStart if this reading is NOT on a slot boundary
          // Boundary readings (at slotStart) should be available for the next slot too
          if Abs(reading.date - slotStart) > (10 / 86400) then
            searchStart := i + 1
          else
            LogMessage(Format('  Reading at boundary (%.2f sec from slotStart), not advancing searchStart',
              [(reading.date - slotStart) * 86400]))// Reading is at the boundary - next slot should also check it
          ;
          Break; // Move to next interval
        end;
        // If user selected largest size, make the font bold for better visibility
        if PredictShortSize >= 3 then
          lPredict.Font.Style := [fsBold]
        else
          lPredict.Font.Style := [];
      end
      else
      if reading.date < slotStart - (10 / 86400) then
      begin
        // Stop if we've gone past this interval into older readings
        LogMessage(Format('  Reading at %s is too old (%.3f sec before slot start), stopping', 
          [DateTimeToStr(reading.date), (slotStart - reading.date) * 86400]));
        Break;
      end;
    end;

    // Hide label if no reading found in this interval
    if not found then
    begin
      labelNumber := NUM_DOTS - slotIndex;
      l := TrendDots[labelNumber];

      if Assigned(l) then
      begin
        l.Visible := false;
        l.Hint := '';  // Clear hint to prevent UpdateTrendDots from re-showing stale data
        LogMessage(Format('TrendDots[%d] hidden as no reading found in interval.',
          [labelNumber]));
      end;
    end;
  end;
  
  // Summary log
  LogMessage(Format('PlaceTrendDots complete: anchor=%s', [DateTimeToStr(anchorTime)]));
end;

function TfBG.UpdateLabelForReading(SlotIndex: integer;
const Reading: BGReading): boolean;
var
  labelNumber: integer;
  l: TDotControl;
  H, M, S, MS: word;
begin
  if firstboot then
    exit;
  Result := false;

  // Mappa slotIndex till etikettens nummer (0 -> lDot10, 1 -> lDot9, ..., 9 -> lDot1)
  labelNumber := NUM_DOTS - SlotIndex;
  l := TrendDots[labelNumber];

  if Assigned(l) then
  begin
    // Uppdatera etikettens egenskaper baserat på läsningen
    l.Visible := true;
    l.Hint := Reading.format(un, BG_MSG_SHORT, BGPrimary);

    DecodeTime(reading.date, H, M, S, MS);
    l.Tag := H * 100 + M; // 10:00 = 1000

    l.Caption := DOT_GRAPH;
    
    {$ifdef DARWIN}
    // On macOS, force TLabel to calculate its size before positioning
    l.AdjustSize;
    {$endif}

    // Use the dot's parent client height to align placement with visibility checks
    setPointHeight(l, Reading.convert(mmol), l.Parent.ClientHeight);

    // Sätt färger baserat på värdet
    l.Font.Color := DetermineColorForReading(Reading);

    LogMessage(Format('TrendDots[%d] updated with reading at %s (Value: %.2f).',
      [labelNumber, DateTimeToStr(Reading.date), Reading.val]));
    l.BringToFront;
    Result := true;
  end;
end;

function TfBG.DetermineColorForReading(const Reading: BGReading): TColor;
begin
  if Reading.val >= api.cgmHi then
    Result := bg_color_hi
  else
  if Reading.val <= api.cgmLo then
    Result := bg_color_lo
  else
  begin
    Result := bg_color_ok_txt;

    if Reading.val <= api.cgmRangeLo then
      Result := bg_rel_color_lo
    else
    if Reading.val >= api.cgmRangeHi then
      Result := bg_rel_color_hi;
  end;

  Result := LightenColor(Result, -0.8);
end;

function TfBG.setColorMode: boolean;
begin
  Result := setColorMode(clFuchsia, true);
end;

function TfBG.setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
begin
  if not nocolor then
    fBG.Color := bg;

  if not multi // not more users than 1
    and titlecolor // we have chosen to color the window
    and miRangeColor.Checked then
  begin // We color the entire window
    Result := SetSingleColorMode;
    Exit;
  end
  else
  if not miRangeColor.Checked // We are not coloring the entire window
    and pnOffRange.Visible then
    if native.SetTitleColor(handle, pnOffRange.Color,
      IfThen(IsLightColor(pnOffRange.Color), clBlack, clWhite)) then
      Exit;

  if customTitleBar and (pnMultiUser.Color <> clBlack) then
    if native.SetTitleColor(handle, pnMultiUser.Color,
      IfThen(IsLightColor(pnMultiUser.Color), clBlack, clWhite)) then
      Exit(true)// Safe that black = standard
  ;

  if native.isDarkMode then
    native.setDarkMode
    {$ifdef windows}
    (self.Handle)
  {$endif}
  ;

  Result := false;
end;

function TfBG.setSingleColorMode: boolean;
begin
  Result := native.SetTitleColor(handle, fBG.color,
    IfThen(IsLightColor(fBG.color), clBlack, clWhite));
end;

// Performance optimization methods
function TfBG.CalculateReadingsHash(const Readings: array of BGReading): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(Readings) do
    Result := Result xor cardinal(Trunc(Readings[i].val * 100)) xor
      cardinal(Trunc(Readings[i].date * 86400));
end;

function TfBG.ShouldUpdateUI(const NewColor: TColor; const NewCaption: string;
const NewTir: string; const NewTirColor: TColor): boolean;
begin
  Result := (NewColor <> FLastUIColor) or (NewCaption <> FLastUICaption) or
    (NewTir <> FLastTir) or (NewTirColor <> FLastTirColor);
end;

procedure TfBG.CacheUIState(const UIColor: TColor; const UICaption: string;
const UITir: string; const UITirColor: TColor);
begin
  FLastUIColor := UIColor;
  FLastUICaption := UICaption;
  FLastTir := UITir;
  FLastTirColor := UITirColor;
end;

procedure TfBG.CheckForUpdates(ShowUpToDateMessage: boolean = false);
var
  res, rn, r, newVersionMessage: string;
  rok: boolean;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  latestRelease: string;
begin
  try
    // Fetch latest release info from GitHub
    TrndiNative.getURL('https://api.github.com/repos/slicke/trndi/releases/latest', res);
    
    // Try to get the latest release name from JSON
    latestRelease := 'Unknown';
    JsonData := nil;
    try
      JsonData := GetJSON(res);
      if Assigned(JsonData) and (JsonData is TJSONObject) then
      begin
        JsonObj := TJSONObject(JsonData);
        latestRelease := JsonObj.Get('name', JsonObj.Get('tag_name', 'Unknown'));
      end;
    except
      latestRelease := 'Parse error';
    end;
    
    // Free the JSON data after we're done with it
    if Assigned(JsonData) then
      FreeAndNil(JsonData);
    
    rok := HasNewerRelease(res, rn, false);

    if rok then
    begin
      r := GetNewerVersionURL(res);
      if r = '' then
        r := 'https://github.com/slicke/trndi/releases/latest';
      newVersionMessage := Format(RS_NEWVER, [rn]);
      if UXDialog(uxdAuto, RS_NEWVER_CAPTION, newVersionMessage, [mbYes, mbNo], mtInformation) = mrYes then
        OpenURL(r);
    end
    else
    if ShowUpToDateMessage then
      if BUILD_NUMBER = 'dev' then
        ShowMessage('Up to date (Dev Build)' + LineEnding + 
          'Build: ' + BUILD_NUMBER + LineEnding +
          'Branch: ' + GIT_BRANCH + LineEnding +
          'Build Date: ' + {$I %DATE%} + ' ' + {$I %TIME%} + LineEnding +
          'Latest release: ' + latestRelease + LineEnding + LineEnding +
          'Note: Dev builds are always considered "newer" than releases')
      else
        ShowMessage(RS_UPTODATE);// Show debug info for dev builds

    // Silently ignore if up to date when ShowUpToDateMessage is false
  except
    on E: Exception do
      if ShowUpToDateMessage then
        ShowMessage('Update check failed: ' + E.Message);
  end;
end;

end.
