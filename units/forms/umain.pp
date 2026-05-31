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
trndi.api.dexcom, trndi.api.dexcomNew, trndi.api.tandem, trndi.api.nightscout, trndi.api.nightscout3, trndi.types,
Math, DateUtils, FileUtil, LclIntf, TypInfo, LResources,
slicke.ux.alert, slicke.ux.native, usplash, Generics.Collections, trndi.funcs, trndi.log,
Trndi.native.base, trndi.shared, buildinfo, fpjson, jsonparser,
slicke.systemmediacontroller,
{$ifdef TrndiExt}
trndi.Ext.Engine, trndi.Ext.jsfuncs, trndi.ext.promise, trndi.ext.perm,
trndi.ext.grant, mormot.core.base, mormot.lib.quickjs,
{$endif}
{$ifdef Darwin}
CocoaAll, MacOSAll,
BaseUnix,
Sockets,
netdb,
{$endif}
{$ifdef LINUX}
linutils.kdebadge,
Sockets,
netdb,
BaseUnix,
{$endif}
{$if defined(BSD) and not defined(DARWIN)}
linutils.kdebadge,
Sockets,
netdb,
BaseUnix,
{$endif}
{$ifdef HAIKU}
Sockets,
netdb,
BaseUnix,
{$endif}
{$ifdef Windows}
winsock,
{$endif}
LazFileUtils, uconf, uwizard, trndi.native, Trndi.API,
trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug_custom, trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_lowsoon, trndi.api.debug_sensorexpiry, trndi.api.debug_missing, trndi.api.debug_firstXmissing, trndi.api.debug_intermittentmissing, trndi.api.debug_perfect, trndi.api.debug_firstmissing, trndi.api.debug_secondmissing, trndi.api.debug_slow, trndi.api.debug_faultysensor, trndi.api.debug_latemissing,{$endif}
{$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
StrUtils, slicke.touchdetection, ufloat, uhistorygraph, LCLType, trndi.webserver.threaded, razer.chroma.factory, razer.chroma,
trndi.theme, trndi.alert.engine,
IntfGraphics, FPImage, GraphType;


{$I ../../inc/defines.inc}
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
TfBG = class;
TConnectivityCheckThread = class(TThread)
private
  FOwner: TfBG;
  FIsDNSRequest: boolean;
  FOnline: boolean;
  procedure ApplyResult;
protected
  procedure Execute; override;
public
  constructor Create(AOwner: TfBG; IsDNSRequest: boolean);
end;

TPredictionThread = class(TThread)
private
  FOwner: TfBG;
  FNumPredictions: integer;
  FPredictionLastReadingTime: TDateTime;
  FSuccess: boolean;
  FPreds: BGResults;
  procedure ApplyResult;
protected
  procedure Execute; override;
public
  constructor Create(AOwner: TfBG; NumPredictions: integer);
end;

{**
  Background fetch of CGM readings. Moves the synchronous @code(api.getReadings)
  call off the main thread so the UI (splash on boot, form on refresh) stays
  responsive while the network round-trip is in flight.

  @code(Boot=True) preserves the legacy "two quick attempts" semantics that
  used to live in @code(FormCreate): if the first fetch returns empty, the
  thread retries once immediately before reporting failure.

  When @code(Execute) finishes, @code(ApplyResult) is dispatched to the main
  thread to copy the readings into @code(bgs) and run the existing post-fetch
  UI sequence (@code(ApplyFetchedReadings), @code(ProcessCurrentReading),
  @code(FinalizeReadingUpdate)).
}
TGlucoseFetchThread = class(TThread)
private
  FOwner: TfBG;
  FBoot: boolean;
  FForce: boolean;
  FResults: BGResults;
  FErrorMsg: string;
  {$ifdef DEBUG}
  FDiag: string;
  {$endif}
  procedure ApplyResult;
protected
  procedure Execute; override;
public
  constructor Create(AOwner: TfBG; Boot: boolean; Force: boolean);
end;

{**
  Which menu opened the history-graph fetch. Drives the empty-data messages
  and whether the prediction overlay is auto-added once the graph is shown.
}
THistoryFetchMode = (hfm24h, hfmDatePicker);

{**
  Background fetch of a custom-window glucose history for the history graph.
  Mirrors TGlucoseFetchThread but takes its window (minutes, max readings)
  by value and feeds @code(ShowHistoryGraph) on the main thread instead of
  the live UI's @code(bgs) slot.
}
THistoryFetchThread = class(TThread)
private
  FOwner: TfBG;
  FMinutes: integer;
  FMaxReadings: integer;
  FMode: THistoryFetchMode;
  FResults: BGResults;
  FErrorMsg: string;
  procedure ApplyResult;
protected
  procedure Execute; override;
public
  constructor Create(AOwner: TfBG; Minutes, MaxReadings: integer;
    Mode: THistoryFetchMode);
end;
TDotControl = TPaintBox;
  // Severity of an active warning. Drives panel layout, colors, and opacity.
  // wsInfo:     soft prediction (look-ahead, > 3 min)        — slim amber banner
  // wsSoon:     imminent prediction (≤ 3 min)                — slim orange banner with pulse
  // wsCritical: no recent data / backend failure              — full-cover red card
TWarnSeverity = (wsInfo, wsSoon, wsCritical);
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
CLOCK_INTERVAL_MS = 20000; // Default clock interval used for the clock tick

type
  { TfBG }

{$ifdef DARWIN}
{$I ../../inc/mac/ns_app.inc}
{$endif}


{** Application main form.
  The `TfBG` class is responsible for the primary user interface: displaying
  current glucose readings, managing alerts, trends/dots, and exposing
  small helpers used by the application web server for remote clients.
  Important UI lifecycle methods (e.g., FormCreate, FormShow, FormDestroy)
  are documented to clarify their role during initialization and shutdown.
}
TDotInfo = record
  Top: integer;
  Height: integer;
  Visible: boolean;
end;

TfBG = class(TForm)
  apMain: TApplicationProperties;
  bSettings: TButton;
  bTouchMenu: TButton;
  bTouchSettings: TButton;
  bMenuPanelClose: TButton;
  bTouchFull: TButton;
  lPredict: TLabel;
  miGuidelines: TMenuItem;
  miBasalRate: TMenuItem;
  miReadingsSince: TMenuItem;
  miExtLog: TMenuItem;
  miSep1: TMenuItem;
  miDNS: TMenuItem;
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
  lInternet: TLabel;
  lTir: TLabel;
  lAgo: TLabel;
  miADotAdjust: TMenuItem;
  miDotsInView: TMenuItem;
  miExit: TMenuItem;
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
  miSplit6: TMenuItem;
  miSplit5: TMenuItem;
  miHistory: TMenuItem;
  miPref: TMenuItem;
  miFloatOn: TMenuItem;
  pnOffReading: TPanel;
  pnNextProgress: TPanel;
  pnWarning: TPanel;
  pnMultiUser: TPanel;
  pnOffRangeBar: TPanel;
  Separator1: TMenuItem;
  miFullScreen: TMenuItem;
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
  tProgress: TTimer;
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
  miAlertSnooze: TMenuItem;
  miAlertSnooze15: TMenuItem;
  miAlertSnooze30: TMenuItem;
  miAlertSnooze60: TMenuItem;
  miAlertSnoozeOff: TMenuItem;
  procedure APIReceiver(const msg: string; etype: TrndiAPIMsg);
    {** Recompute and apply layout offsets for all graph elements.
      This adjusts trend dots, labels and other elements when the UI size or
      dot-count changes to keep everything visually aligned.
     }
  procedure AdjustGraph;
  procedure bMenuPanelCloseClick({%H-}Sender: TObject);
  procedure bSettingsClick({%H-}Sender: TObject);
  procedure bTouchFullClick({%H-}Sender: TObject);
  procedure bTouchMenuClick({%H-}Sender: TObject);
  procedure bTouchSettingsClick({%H-}Sender: TObject);
  procedure pnTouchButtonMouseDown({%H-}Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  procedure fbReadingsDblClick({%H-}Sender: TObject);
  procedure FormActivate({%H-}Sender: TObject);
  procedure FormClick({%H-}Sender: TObject);
  procedure FormCloseQuery({%H-}Sender: TObject; var CanClose: boolean);
  procedure FormClose({%H-}Sender: TObject; var {%H-}CloseAction: TCloseAction);
  {** Initialize UI state, load configuration, start any required services,
      and prepare connections to backends and extensions.
      Called by the framework when the form instance is created.
   }
  procedure FormCreate({%H-}Sender: TObject);
  procedure FormDblClick({%H-}Sender: TObject);
  {** Shutdown and cleanup all resources, including timers, web server, and
      persistent connections. Called when the form is destroyed.
   }
  procedure FormDestroy({%H-}Sender: TObject);
  procedure FormKeyPress({%H-}Sender: TObject; var Key: char);
  procedure DotPaint({%H-}Sender: TObject);
  procedure lDiffClick({%H-}Sender: TObject);
  procedure lPredictClick({%H-}Sender: TObject);
  procedure miBasalRateClick({%H-}Sender: TObject);
  procedure AutoEnableBasalOverlay;
  procedure AutoAddPredictionOverlay;
  procedure miDNSClick({%H-}Sender: TObject);
  procedure miDotsInViewClick({%H-}Sender: TObject);
  procedure miExitClick({%H-}Sender: TObject);
  procedure miDotWindowClick(Sender: TObject);
  procedure BuildDotWindowMenu;
  procedure miATouchAutoClick({%H-}Sender: TObject);
  procedure miADotAdjustClick({%H-}Sender: TObject);
  procedure miADotScaleClick({%H-}Sender: TObject);
  procedure miADotsClick({%H-}Sender: TObject);
  procedure miASystemInfoClick({%H-}Sender: TObject);
  procedure miATouchClick({%H-}Sender: TObject);
  procedure miATouchNoClick({%H-}Sender: TObject);
  procedure miATouchYesClick({%H-}Sender: TObject);
  procedure miDebugBackendClick({%H-}Sender: TObject);
  procedure miExtLogClick({%H-}Sender: TObject);
  procedure miGuidelinesClick({%H-}Sender: TObject);
  procedure miPredictClick({%H-}Sender: TObject);
  procedure miReadingsSinceClick({%H-}Sender: TObject);
  procedure pmSettingsClose({%H-}Sender: TObject);
  procedure pnWarningClick({%H-}Sender: TObject);
  procedure pnWarningPaint({%H-}Sender: TObject);
  procedure pnNextProgressPaint({%H-}Sender: TObject);
  procedure speakReading;
  procedure FormMouseLeave({%H-}Sender: TObject);
  procedure FormMouseMove(Sender: TObject;{%H-}Shift: TShiftState; X, Y: integer);
  procedure FormResize({%H-}Sender: TObject);
  {** Called when the form becomes visible after creation; performs final
      placement operations and triggers initial UI refreshes once all controls
      are initialized and loaded.
   }
  procedure FormShow({%H-}Sender: TObject);
  procedure FormPaint({%H-}Sender: TObject);
  procedure lAgoClick({%H-}Sender: TObject);
  procedure lArrowClick({%H-}Sender: TObject);
  procedure lDiffDblClick({%H-}Sender: TObject);
  procedure lDot7DblClick({%H-}Sender: TObject);
  procedure lgMainClick({%H-}Sender: TObject);
  procedure lTirClick({%H-}Sender: TObject);
  procedure lValClick({%H-}Sender: TObject);
  procedure lValDblClick({%H-}Sender: TObject);
  procedure lValMouseDown({%H-}Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
  procedure lValStartDrag({%H-}Sender: TObject; var {%H-}DragObject: TDragObject);
  procedure miAnnounceClick({%H-}Sender: TObject);
  procedure miFloatOnClick({%H-}Sender: TObject);
  procedure mi24hClick({%H-}Sender: TObject);
  procedure miAlertSnooze15Click({%H-}Sender: TObject);
  procedure miAlertSnooze30Click({%H-}Sender: TObject);
  procedure miAlertSnooze60Click({%H-}Sender: TObject);
  procedure miAlertSnoozeOffClick({%H-}Sender: TObject);
  procedure lInternetClick({%H-}Sender: TObject);
  procedure miHistoryClick({%H-}Sender: TObject);
  procedure miForceClick({%H-}Sender: TObject);
  procedure miLimitExplainClick({%H-}Sender: TObject);
  procedure miSettingsClick({%H-}Sender: TObject);
  {$ifdef DARWIN}
  procedure ShowAboutDialog({%H-}Sender: TObject);
  procedure CheckForUpdatesMenuClick({%H-}Sender: TObject);
  {$endif}
  procedure onTrendClick({%H-}Sender: TObject);
  procedure pnOffReadingPaint({%H-}Sender: TObject);
  procedure pmSettingsMeasureItem({%H-}Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: integer);
  procedure pmSettingsPopup({%H-}Sender: TObject);
  procedure pnMultiUserClick({%H-}Sender: TObject);
  procedure pnOffRangeClick({%H-}Sender: TObject);
  procedure tAgoTimer({%H-}Sender: TObject);
  procedure tProgressTimer({%H-}Sender: TObject);
  procedure tClockTimer({%H-}Sender: TObject);
  procedure tEdgesTimer({%H-}Sender: TObject);
  procedure tInitTimer({%H-}Sender: TObject);
  procedure tPingTimer({%H-}Sender: TObject);
  procedure tResizeTimer({%H-}Sender: TObject);
  procedure tMainTimer({%H-}Sender: TObject);
  procedure tMissedTimer({%H-}Sender: TObject);
  procedure tSetupTimer({%H-}Sender: TObject);
  procedure tSwapTimer({%H-}Sender: TObject);
  procedure tTouchTimer({%H-}Sender: TObject);
  procedure UpdateConnectionBadgeBackgroundBounds;
  procedure TfFloatOnHide(Sender: TObject);
  {$ifdef DEBUG}
  procedure miDebugUXMsgClick(Sender: TObject);
  procedure miDebugLogClick(Sender: TObject);
  procedure miDebugLoadTextClick(Sender: TObject);
  procedure miDebugLogAPIClick(Sender: TObject);
  {$endif}
  {$ifdef TEST}
  // Test accessors
{$I ../../tests/inc/umain_fbg.inc}
  {$endif}
private
  function AlertsSnoozed: boolean;
  function GetControlAbsolutePos(const Ctrl: TControl): TPoint;
  function GetPanelClientPos(const Ctrl: TControl; const Panel: TPanel): TPoint;
  function PointInsideRoundedRect(const X, Y, W, H, R: integer): boolean;
  function GetSensorBadgeText: string;
  procedure HideConnectionBadge;
  procedure EnsureConnectionBadgeControls;
  procedure ApplyConnectionBadgeLayout(const DisplayStatus: string;
    const DisplayColor: TColor; const IsCentered: boolean);
  procedure EnsureWarningDots;
  procedure SyncWarningOverlayDots(const P: TPanel);
  procedure EnsureWarningLabels;
  procedure SyncWarningOverlayLabels(const P: TPanel);
  procedure SetAlertSnoozeMinutes(const Minutes: integer);
  procedure UpdateAlertSnoozeMenu;
  function ClassifyConnectionStatus(const ErrorText: string): string;
  procedure SetConnectionBadge(const StatusText: string; const BadgeColor: TColor);
  procedure ShutdownBackgroundThreads;
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
  FCloseAfterFormCreate: boolean; // Flag to close form after initialization completes

    // Performance optimization fields
  FLastReadingsHash: cardinal; // Hash of last readings for change detection
  FLastAPICall: TDateTime; // Timestamp of the last successful API call
  FCachedReadings: array of BGReading; // Readings saved from last fetch
  FLastUIColor: TColor;
  FLastUICaption: string;
  FLastTir: string;
  FLastTirColor: TColor;
  FLastConnectionStatus: string;
  FLastConnectionDetail: string;
  FConnectivityThread: TConnectivityCheckThread;
  FPingThread: TConnectivityCheckThread;
  FPredictionThread: TPredictionThread;
  FGlucoseFetchThread: TGlucoseFetchThread;
  FFetchInFlight: boolean;  // True while a TGlucoseFetchThread is running.
                            // Reads/writes happen on the main thread only.
  FBootFetchPending: boolean; // True while the splash-time first fetch is in
                              // flight; gates the boot-failure warning panel.
  FHistoryFetchThread: THistoryFetchThread;
  FHistoryFetchInFlight: boolean; // True while a THistoryFetchThread is
                                  // running. Reads/writes on the main
                                  // thread only; gates the menu items so a
                                  // second click can't queue a duplicate.
  FFetchThreadDetached: boolean; // Set when shutdown gave up waiting on the
                                 // fetch worker and detached it; gates the
                                 // api/native free in FormDestroy so the
                                 // still-running worker doesn't UAF.
  tUpdateCheck: TTimer;       // One-shot deferred CheckForUpdates trigger so
                              // the synchronous HTTPS call doesn't block the
                              // form's first WM_PAINT.
  FUpdateCheckScheduled: boolean;
  tBootFetch: TTimer;         // One-shot trigger for the first async fetch;
                              // fires only after Application.Run is pumping
                              // the message loop so the form has painted.
  FInternetBadgeBg: TShape;
  FInternetBadgeShadow: TShape;
  FWarningDots: array of TDotControl;
  FWarningLabels: array[1..4] of TLabel; // lArrow, lVal, lDiff, lAgo overlay
  FWarnSeverity: TWarnSeverity; // Current warning level — drives layout in fixWarningPanel
  FWarnExpanded: boolean;       // Inline-expand toggle (set by pnWarningClick)
  FWarnBannerBaseH: integer;    // Collapsed banner height (px) — read by pnWarningPaint
  FLastTimerTick: TDateTime; // Last timer tick for wake detection
  FForceRefresh: boolean; // Force bypass of cached API reads on wake
  FLastFetchHadData: boolean; // true when last API call returned ≥1 readings (used to avoid reusing cache after an empty fetch)
  FCachedReadingsValid: boolean; // Fast-path flag to avoid lock when cache is empty (volatile-like)

  FReadingsLock: TRTLCriticalSection; // Protect cached readings shared with web server thread

    // Dynamic array; allocated 1-based (index 0 unused). Size = ActiveDots+1.
  TrendDots: array of TDotControl;
  FDotWindowMenu: TMenuItem; // Trend window submenu (built once on first popup)
  multi: boolean; // Multi user
  multinick: string;
  MediaController: TSystemMediaController;
  FWebServer: TObject; // TTrndiWebServer - using TObject to avoid circular dependency
  tWebServerStart: TTimer;

  Chroma: TRazerChromaBase;
  FAlertEngine: TAlertEngine;
  procedure PersistAlertState(Sender: TObject);
  procedure tUpdateCheckTimer(Sender: TObject);
  procedure tBootFetchTimer(Sender: TObject);

  function dotsInView: integer;
  function setColorMode: boolean;
  function setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
  function setSingleColorMode: boolean;
  procedure SetLang;
  procedure fixWarningPanel;
  procedure showWarningPanel(const message: string;
    severity: TWarnSeverity = wsCritical;
    clearDisplayValues: boolean = false; showDetails: boolean = true);
  procedure hideWarningPanel;
  procedure GetWarnSeverityVisual(severity: TWarnSeverity;
    out tintColor: TColor; out opacity: byte; out isFullCover: boolean);
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
  {** Return the mean as a formatted string using the requested unit.
      - mmol/L: one decimal place
      - mg/dL: integer (no decimals)
      - NumReadings (default NUM_DOTS): number of visual slots to average.
        If NumReadings = -1 the function will average *all* available non-empty
        readings in `bgs`.
      Returns empty string when no readings are available. */}
  function BGMean(const UnitPref: BGUnit = BGUnit.mmol; const NumReadings: integer = NUM_DOTS): string;
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TDotControl; c, ix: integer; {%H-}ls: array of TDotControl);
  procedure HideDot(l: TDotControl; {%H-}c, {%H-}ix: integer);
  procedure showDot(l: TDotControl; {%H-}c, {%H-}ix: integer);
  procedure ResizeDot(l: TDotControl; {%H-}c, ix: integer);
  procedure initDot(l: TDotControl; c, ix: integer);
  procedure ExpandDot(l: TDotControl; c, ix: integer);
  procedure ApplyTrendDotTopOffset(const Offset: integer);
  procedure RepaintVisibleTrendDots;
  procedure CreateTrendDots;
  procedure placeForm;

    {** Kick off an asynchronous fetch. The actual network call runs on a
        background @link(TGlucoseFetchThread) and the result is applied via
        @link(ApplyFetchedReadings) on the main thread. If a fetch is
        already in flight the request is dropped (returns @false). }
  function RequestAsyncFetch(Boot: boolean; Force: boolean): boolean;
    {** Kick off an asynchronous history-graph fetch on a
        @link(THistoryFetchThread). Disables the history menu items while
        the worker is in flight; re-enables them from ApplyResult. Returns
        @false if a request was already pending or the API isn't ready. }
  function RequestHistoryFetch(Minutes, MaxReadings: integer;
    Mode: THistoryFetchMode): boolean;
    {** Apply a set of readings returned by the worker thread. Performs all
        of the post-fetch UI work (cache copy, overrides, warning panel,
        alert engine, dots). Runs only on the main thread. Returns @true
        when fresh data was placed in @code(bgs). }
  function ApplyFetchedReadings(const Readings: BGResults;
    const ErrorMsg: string; Boot: boolean): boolean;
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
  function FinalizeReadingUpdate(const Boot: boolean): boolean;
  function IsSensorFaultSuspected(const Readings: array of BGReading): boolean;
  {** Apply visual changes following the latest readings, such as redraws and
      color/label updates depending on thresholds and state.
   }
  procedure UpdateUIBasedOnGlucose;
  {** Final housekeeping after the UI update pipeline completes. Ensures that
      timers, overlays and other stateful UI pieces are synchronized after
      a new reading or manual refresh.
   }
  procedure FinalizeUIUpdate;
  procedure HandleHighGlucose(const {%H-}reading: BGReading);
  procedure HandleLowGlucose(const {%H-}reading: BGReading);
  procedure HandleNormalGlucose(const reading: BGReading);
  procedure ApplyChromaAlertAction(const ActionSettingKey: string;
    const DefaultAction: string; const AColor: TRGBColor);
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
  {** Apply configuration changes that can take effect immediately without restart.
      This includes fonts, colors, display options, predictions, and UI preferences.
      Called after saving settings to provide instant feedback to the user.
   }
  procedure ApplySettingsInstantly;
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
  {** Blend a foreground color with a background color using alpha ratio.
      Used for rendering semi-transparent text overlays.
      @param(ForeColor The original foreground/text color.)
      @param(BgColor The background color to blend with.)
      @param(AlphaRatio The blending ratio (0..1) where 0 is fully original, 1 is fully background.)
      @returns(Blended TColor result.)
   }
  function BlendFontColorWithBackground(const ForeColor: TColor;
    const BgColor: TColor; const AlphaRatio: double): TColor;
  {** Render a single warning label (lArrow, lVal, lDiff, or lAgo) onto the warning panel.
      Handles font assignment, color blending, text width calculation, and alignment-based positioning.
      @param(LabelControl The label control to render.)
      @param(P The parent panel whose canvas will be drawn to.)
      @param(BgColor The background color used for color blending.)
      @param(AlphaRatio The blending ratio for transparency.)
   }
  procedure RenderWarningLabel(const LabelControl: TLabel; const P: TPanel;
    const BgColor: TColor; const AlphaRatio: double);

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
  {** Recalculate left of lTir when next progress bar is visible }
  procedure nextProgressChange;
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
    customTl: TTextLayout = tlCenter; allowCustom: boolean = true; padding: integer = 0);

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
  procedure RenderPredictionCache(const bgr: BGResults);
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
  procedure InitializePlatformMenus;
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
  procedure actOnMediaController(act: TMediaControllerAct; arg: string = '');
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

  {** Return the most recent reading with data in it (the newest element in `bgs` that's not empty).
      Caller should ensure `bgs` is not empty (use `tryLastReading` first).
     }
  function lastDataReading: BGReading;

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

tir_icon: boolean = true;
perfectTriggered: boolean = false; // A perfect reading is active
UseRangeColor: boolean = true;
PaintRange: boolean = true;
PaintRangeCGMRange: boolean = false; // Show cgmRangeLo/cgmRangeHi inner threshold lines
PaintRangeLines: boolean = false;
PredictGlucoseReading: boolean = false;
PredictShortMode: boolean = false;
// 'small' or 'big' to control single-arrow prediction size
PredictShortSize: integer = 1; // 1=small, 2=medium, 3=big
PredictShortFullArrows: boolean = false; // Use full UTF arrow set in short mode
PredictShortShowValue: boolean = false; // Show predicted value with clock icon in short mode
PredictShortMinutes: integer = 10; // Prediction horizon (5, 10, or 15 minutes)
// Cache for dynamic prediction time updates
PredictionCache: BGResults; // Cached prediction readings
PredictionLastReadingTime: TDateTime; // Time of the reading used for predictions
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
upMenu: TMenuItem;
DOT_OFFSET_RANGE: integer = -15; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
{$ifdef HAIKU}
DOT_OFFSET_RANGE: integer = -15; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
DOT_LINES: boolean = false;  // Guidelines what value is where
DELTA_MAX: integer = 2;
DIFF_ALIGN: TAlignment = taCenter;
{$ifdef DEBUG}
debug_load_text: boolean = false;
{$endif}



var
last_popup: TDateTime = 0;
placed: boolean = false; // If the window has been placed at setup
WarnShowDetails: boolean = true; // controls whether fixWarningPanel adds reading/age info
FWarnMessage: string = '';        // raw warning text; rebuilt by fixWarningPanel on resize

username: string = '';
lastup: tdatetime;
  // Colors (b)lood(g)lucose (c)olor XX — defaults from TrndiThemeModern.
  // Users with stored 'ux.bg_color_*' settings keep their own values.
  // In range
bg_color_ok: TColor = $0060AE27;     // Emerald    #27AE60
bg_color_ok_txt: TColor = $00E8FBE8; // Light green
  // Hi
bg_color_hi: TColor = $00227EE6;     // Pumpkin    #E67E22
bg_color_hi_txt: TColor = $00001B2D; // Very dark brown
  // Low
bg_color_lo: TColor = $002B39C0;     // Pomegranate #C0392B
bg_color_lo_txt: TColor = $00EEEBFF; // Very light pink

  // Personal hi (approaching upper limit)
bg_rel_color_hi: TColor = $00129CF3;     // Sunflower   #F39C12
bg_rel_color_hi_txt: TColor = $00001B2D; // Very dark brown
  // Personal low (approaching lower limit)
bg_rel_color_lo: TColor = $00AD448E;     // Amethyst    #8E44AD
bg_rel_color_lo_txt: TColor = $00F5E5F3; // Light lavender
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
TouchMenuMouseDownFired: boolean; // prevents OnClick double-fire after OnMouseDown
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

// tfuncs.inc must precede the const block: it contains a uses clause (valid
// only at the very start of the implementation section when TrndiExt is on).
{$I ../../inc/tfuncs.inc}

const
MIN_REFRESH_INTERVAL_MS = 120000; // 2 minutes
REFRESH_RESYNC_BUFFER_MS = 15000; // Additional buffer to allow backend sync
BADGE_FLASH_DURATION_HIGH_MS = 15000;
BADGE_FLASH_REPEAT_DELAY_HIGH_MS = 450;
BADGE_FLASH_DURATION_LOW_MS = 20000;
BADGE_FLASH_REPEAT_DELAY_LOW_MS = 400;
BADGE_FLASH_DURATION_OK_MS = 6000;
BADGE_FLASH_REPEAT_DELAY_OK_MS = 500;
DEFAULT_PREDICTION_FUTURE_LIMIT = 7;

// Standalone helpers referenced by multiple include files below.
function CaptionStartsWithDigit(const S: string): boolean;
begin
  Result := false;
  if Length(S) = 0 then
    Exit;
  Result := S[1] in ['0'..'9'];
end;

function CurrentHistoryGraphPalette: THistoryGraphPalette;
begin
  Result.Range := bg_color_ok;
  Result.RangeHigh := bg_rel_color_hi;
  Result.RangeLow := bg_rel_color_lo;
  Result.High := bg_color_hi;
  Result.Low := bg_color_lo;
  Result.Unknown := RGBToColor(180, 180, 180);
end;

{$I ../../inc/umain_ext.inc}
{$I ../../inc/umain_async.inc}
{$I ../../inc/umain_helpers.inc}
{$I ../../inc/umain_init.inc}
{$I ../../inc/umain_alerts.inc}
{$I ../../inc/umain_dots.inc}
{$I ../../inc/umain_glucose.inc}
{$I ../../inc/umain_menu.inc}
{$I ../../inc/umain_paint.inc}
{$I ../../inc/umain_settings.inc}
{$I ../../inc/umain_timers.inc}

procedure TfBG.FormDblClick({%H-}Sender: TObject);
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
  // If there are no readings available, return an empty initialized reading
  // Caller should normally use tryLastReading() first, but be defensive here.
  if (bgs = nil) or (Length(bgs) = 0) then
  begin
    TrndiDLog('lastReading: requested but no readings available');
    Result.Init(un,un,'missing data');
    Exit;
  end;

  // Return the oldest element (Low index) as the last reading
  Result := bgs[Low(bgs)];
end;

function TfBG.lastDataReading: BGReading;
var
  i: integer;
begin
  result := lastReading;
  try
    for i := Low(bgs) to High(bgs) do
      if not bgs[i].empty then
      begin
        result := bgs[i];
        Exit;
      end;
  finally
  end;
end;

procedure TfBG.FormDestroy({%H-}Sender: TObject);
begin
  // Ensure shutdown flag is set
  FShuttingDown := true;

  ShutdownBackgroundThreads;

  // Stop web server first to prevent callbacks during shutdown
  StopWebServer;

  DoneCriticalSection(FReadingsLock);


  FreeAndNil(FAlertEngine);

  if assigned(chroma) then
    chroma.free;

  // These should already be freed in FormClose, but check just to be safe.
  // If ShutdownBackgroundThreads detached a still-running fetch worker,
  // leak api/native deliberately: the worker is still inside
  // api.getReadings on those objects, and freeing them here would crash on
  // its continuation. The process is exiting; the OS reclaims the memory.
  if FFetchThreadDetached then
    TrndiDLog('FormDestroy: skipping api/native free — detached fetch worker still using them')
  else
  begin
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
      native.speak(RS_SPEAK_HIGH);
    trndi.types.BGLOW:
      native.speak(RS_SPEAK_LOW);
    trndi.types.BGRange:
      native.speak(RS_SPEAK_GOOD);
    trndi.types.BGRangeHI:
      native.speak(RS_SPEAK_GHIGH);
    trndi.types.BGRangeLO:
      native.speak(RS_SPEAK_GLOW);
    end;
end;

procedure TfBG.FormKeyPress({%H-}Sender: TObject; var Key: char);
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
  lVal.Caption := RS_EXT_SHUTDOWN;
  lVal.Visible := true;
  Application.ProcessMessages; // Ensure caption is updated immediately
  
  // Set global shutdown flag immediately to prevent any new JS operations
  try
    trndi.ext.engine.SetGlobalShutdown;
  except
    // Ignore any errors during shutdown flag setting
  end;
  {$endif}

  // Always allow close to proceed. Actual termination is decided in FormClose
  // after the user has confirmed they really want to quit.
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
    native.SetSetting('position.last.left', Left);
    native.SetSetting('position.last.top', Top);
  end;

  {$ifdef Darwin}
  if not firstboot then
    if self.Showing then
    begin
      mr := UXDialog(uxdAuto, RS_QUIT_MINIMIZE_TITLE, RS_QUIT_MINIMIZE,
        [mbClose, mbUXMinimize, mbCancel]);
      case mr of
      mrClose:
      begin
        Application.Terminate; // signal shutdown only when actually closing
        CloseAction := caFree;
      end;
      mrCancel: 
      begin
        FShuttingDown := false; // Reset flag if user cancels
        CloseAction := caNone;
        Exit;
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
      CloseAction := caNone;
      Exit;
    end;

  Application.Terminate; // signal shutdown only when actually closing

  // Explicitly set CloseAction to ensure the form is actually freed
  CloseAction := caFree;
  {$endif}

  // Disable timers before shutting down threads to prevent new threads from being created
  if Assigned(tPing) then
    tPing.Enabled := false;
  if Assigned(tAgo) then
    tAgo.Enabled := false;
  if Assigned(tProgress) then
    tProgress.Enabled := false;

  ShutdownBackgroundThreads;

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
    if Assigned(tProgress) then
      tProgress.Enabled := false;
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
    lDiff.Caption := Format(RS_CLEANUP_WAIT, [20]);
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

  // Ensure desktop indicators/readers don't keep showing stale data after shutdown
  try
    native.WriteCurrentIndicatorCache('', 0, 0);
  except
    // ignore errors during shutdown
  end;

  // Let normal form closure process continue with CloseAction := caFree
end;

procedure TfBG.ShutdownBackgroundThreads;
const
  // Budget for waiting on the fetch worker before detaching. Network calls
  // can outlive this; on timeout we hand the thread off to the runtime and
  // let it expire on its own (or get killed by process termination).
  FETCH_SHUTDOWN_TIMEOUT_MS = 3000;
var
  deadline: TDateTime;
begin
  // Disable and free the deferred GitHub releases check first. Its handler
  // does a synchronous HTTPS call that would block the ProcessMessages pumps
  // below if a queued WM_TIMER fires mid-teardown. Clearing the scheduled
  // flag matches the create-once gate in FormShow.
  if Assigned(tUpdateCheck) then
  begin
    tUpdateCheck.Enabled := false;
    FreeAndNil(tUpdateCheck);
  end;
  FUpdateCheckScheduled := false;

  if Assigned(FConnectivityThread) then
  begin
    FConnectivityThread.Terminate;
    while not FConnectivityThread.Finished do
      Application.ProcessMessages;
    FConnectivityThread.WaitFor;
    FreeAndNil(FConnectivityThread);
  end;

  if Assigned(FPingThread) then
  begin
    FPingThread.Terminate;
    while not FPingThread.Finished do
      Application.ProcessMessages;
    FPingThread.WaitFor;
    FreeAndNil(FPingThread);
  end;

  if Assigned(FPredictionThread) then
  begin
    FPredictionThread.Terminate;
    while not FPredictionThread.Finished do
      Application.ProcessMessages;
    FPredictionThread.WaitFor;
    FreeAndNil(FPredictionThread);
  end;

  if Assigned(FGlucoseFetchThread) then
  begin
    // Terminate only sets a flag; the worker is typically blocked inside the
    // synchronous api.getReadings call which can't be interrupted from here.
    // Real cancellation would require an HTTP abort hook in every TrndiAPI
    // backend (deferred). For now, bound the wait so a slow/dead backend
    // doesn't hang shutdown; on timeout, detach the worker and let it finish
    // its network call in the background. FFetchThreadDetached then gates
    // the api/native free in FormDestroy below to avoid use-after-free.
    FGlucoseFetchThread.Terminate;
    deadline := IncMilliSecond(Now, FETCH_SHUTDOWN_TIMEOUT_MS);
    while (not FGlucoseFetchThread.Finished) and (Now < deadline) do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;
    if FGlucoseFetchThread.Finished then
    begin
      FGlucoseFetchThread.WaitFor;
      FreeAndNil(FGlucoseFetchThread);
    end
    else
    begin
      TrndiDLog('ShutdownBackgroundThreads: fetch worker still in api.getReadings after timeout; detaching');
      FGlucoseFetchThread.FreeOnTerminate := true;
      FGlucoseFetchThread := nil;
      FFetchThreadDetached := true;
    end;
  end;

  if Assigned(FHistoryFetchThread) then
  begin
    // Same shape as FGlucoseFetchThread: history fetches sit inside the same
    // uninterruptible api.getReadings call, so bound the wait and detach on
    // timeout. A detached history worker still touches api when it returns,
    // so it gates FFetchThreadDetached too (same api/native lifetime).
    FHistoryFetchThread.Terminate;
    deadline := IncMilliSecond(Now, FETCH_SHUTDOWN_TIMEOUT_MS);
    while (not FHistoryFetchThread.Finished) and (Now < deadline) do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;
    if FHistoryFetchThread.Finished then
    begin
      FHistoryFetchThread.WaitFor;
      FreeAndNil(FHistoryFetchThread);
    end
    else
    begin
      TrndiDLog('ShutdownBackgroundThreads: history worker still in api.getReadings after timeout; detaching');
      FHistoryFetchThread.FreeOnTerminate := true;
      FHistoryFetchThread := nil;
      FFetchThreadDetached := true;
    end;
  end;
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

procedure TfBG.bMenuPanelCloseClick(Sender: TObject);
begin
  if TouchMenuMouseDownFired then begin TouchMenuMouseDownFired := false; Exit; end;
  pnTouchMenu.Hide;
end;
procedure TfBG.bSettingsClick(Sender: TObject);
begin
  ShowMessage(RS_RIGHT_CLICK);
  miSettings.Click;
end;

procedure TfBG.bTouchFullClick(Sender: TObject);
begin
  if TouchMenuMouseDownFired then begin TouchMenuMouseDownFired := false; Exit; end;
  miFullScreen.Click;
  pnTouchMenu.Hide;
end;

procedure TfBG.bTouchMenuClick(Sender: TObject);
var
  p: TPoint;
begin
  if TouchMenuMouseDownFired then begin TouchMenuMouseDownFired := false; Exit; end;
  p := Mouse.CursorPos;
  pmSettings.PopUp(p.X, p.Y)
end;

procedure TfBG.bTouchSettingsClick(Sender: TObject);
begin
  if TouchMenuMouseDownFired then begin TouchMenuMouseDownFired := false; Exit; end;
  miSettings.Click;
end;

// Single OnMouseDown handler shared by all pnTouchMenu buttons.
// OnMouseDown fires on first touch contact regardless of where the finger
// lifts, making it reliable on Qt/Linux where QPushButton::clicked() requires
// the release point to still be inside the button hit rect.
procedure TfBG.pnTouchButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if Button <> mbLeft then Exit;
  TouchMenuMouseDownFired := true;
  if Sender = bMenuPanelClose then
    pnTouchMenu.Hide
  else if Sender = bTouchFull then
  begin
    miFullScreen.Click;
    pnTouchMenu.Hide;
  end
  else if Sender = bTouchMenu then
  begin
    p := Mouse.CursorPos;
    pmSettings.PopUp(p.X, p.Y);
  end
  else if Sender = bTouchSettings then
    miSettings.Click;
end;
procedure TfBG.FormResize(Sender: TObject);
var
  warnTintTmp: TColor;
  warnAlphaTmp: byte;
  warnFullCoverTmp: boolean;
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

    // Apply alpha control only - rounded corners are handled by pnWarningPaint.
    // Opacity is derived from the current severity (see GetWarnSeverityVisual).
    if pnWarning.Visible then
    begin
      GetWarnSeverityVisual(FWarnSeverity, warnTintTmp, warnAlphaTmp, warnFullCoverTmp);
      ApplyAlphaControl(pnWarning, warnAlphaTmp);
    end;

    // Keep the thin left-side progress bar sized with the form
    if Assigned(pnNextProgress) then
    begin
      pnNextProgress.Height := ClientHeight;
      pnNextProgress.Width := Max(6, ClientWidth div 40);
      pnNextProgress.Left := 0;
      nextProgressChange;
    end;

    // Keep the connection badge background aligned while the label moves.
    UpdateConnectionBadgeBackgroundBounds;
  end;
end;

procedure TfBG.FormShow(Sender: TObject);
begin
  placeForm;
  placed := true;
  lVal.font.Quality := fqCleartype;

  // Ensure the next-reading progress panel is correctly positioned
  if Assigned(pnNextProgress) then
  begin
    pnNextProgress.Height := ClientHeight;
    pnNextProgress.Width := Max(6, ClientWidth div 40);
    pnNextProgress.Left := 0;
    pnNextProgress.BringToFront;
    pnNextProgress.Visible := native.GetBoolSetting('main.next_progress', false);
  end;
  
  // Check if we need to close after FormCreate completed (e.g., fresh install with no config)
  if FCloseAfterFormCreate then
  begin
    FCloseAfterFormCreate := false;
    Close;
    Exit;
  end;
  
  // Defer the GitHub releases check off the FormShow path. TrndiNative.getURL
  // is a synchronous HTTPS call (typ. hundreds of ms, much longer if the
  // remote stalls). Running it inline blocks the form's first WM_PAINT,
  // which on slow networks shows up as "icon in taskbar but window invisible"
  // for the duration of the request. A one-shot TTimer reuses the existing
  // pattern for tWebServerStart below; the OnTimer handler frees itself.
  if not FUpdateCheckScheduled then
  begin
    FUpdateCheckScheduled := true;
    tUpdateCheck := TTimer.Create(Self);
    tUpdateCheck.Interval := 1500;
    tUpdateCheck.OnTimer := @tUpdateCheckTimer;
    tUpdateCheck.Enabled := true;
  end;
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
        BorderStyle := bsToolWindow;
      end;
  end;

  // Adjust for dark mode if applicable
  setColorMode;
end;
{$ifdef LCLQt6}
function TryQtStartSystemMove(AHandle: THandle): boolean;
var
  QtWidget: TQtWidget;
  sessionType: string;
  qwin: QWindowH;
begin
  Result := false;
  sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
  if LowerCase(sessionType) <> 'wayland' then
    Exit;

  if AHandle = 0 then
    Exit;

  QtWidget := TQtWidget(AHandle);
  if not Assigned(QtWidget) or not Assigned(QtWidget.Widget) then
    Exit;

  qwin := QWidget_windowHandle(QtWidget.Widget);
  if qwin = nil then
    Exit;

  Result := QWindow_startSystemMove(qwin);
end;
{$endif}

procedure TfBG.lValMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
{$ifdef LCLQt6}
var
  QtWidget: TQtWidget;
  sessionType: string;
{$endif}
begin
  if ((Button = mbLeft) and (self.BorderStyle = bsNone)) or (Button = mbMiddle) then
  begin   // Handle window moving
    PX := X;
    PY := Y;

    {$ifdef LCLQt6}
    sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
    if LowerCase(sessionType) = 'wayland' then
      if TryQtStartSystemMove(Handle) then
        Exit// Try a safe helper that attempts to trigger a compositor move.
// The real implementation may require a small native wrapper; this stub
// avoids compile errors on older LCLs and returns false when not supported.
// compositor will handle moving
    ;
    {$endif}

    DraggingWin := true;
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

{$ifdef TEST}
{$I ../../tests/inc/umain_implementation.inc}
{$endif}

end.

