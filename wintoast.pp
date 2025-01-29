unit winToast;

interface

uses
  Windows, SysUtils, Classes, ComObj, ActiveX;

type
  TToastNotification = class
  private
    FAppID: string;
    FNotifier: IToastNotifier;
    FFactory: IToastNotificationFactory;
    procedure InitializeInterfaces;
  public
    constructor Create(const AppID: string);
    destructor Destroy; override;
    procedure ShowGlucoseNotification(GlucoseValue: Double);
  end;

implementation

constructor TToastNotification.Create(const AppID: string);
begin
  inherited Create;
  FAppID := AppID;
  InitializeInterfaces;
end;

procedure TToastNotification.InitializeInterfaces;
var
  Unknown: IUnknown;
begin
  // Create the Toast Notification Manager
  if Failed(CoCreateInstance(CLSID_ToastNotificationManager, nil,
    CLSCTX_INPROC_SERVER, IID_IUnknown, Unknown)) then
    raise Exception.Create('Failed to create Toast Notification Manager');

  // Get the notifier interface
  if Failed(Unknown.QueryInterface(IID_IToastNotifier, FNotifier)) then
    raise Exception.Create('Failed to get IToastNotifier interface');

  // Get the factory interface
  if Failed(Unknown.QueryInterface(IID_IToastNotificationFactory, FFactory)) then
    raise Exception.Create('Failed to get IToastNotificationFactory interface');
end;

procedure TToastNotification.ShowGlucoseNotification(GlucoseValue: Double);
var
  Notification: IToastNotification;
  XMLDoc: string;
  MmolValue: Double;
  XMLContent: PWideChar;
begin
  try
    // Convert mg/dL to mmol/L
    MmolValue := GlucoseValue * 0.0555;

    // Format the notification XML
    XMLDoc := Format(TOAST_TEMPLATE, [
      FormatFloat('0.0', GlucoseValue),
      FormatFloat('0.1', MmolValue)
    ]);

    // Convert to PWideChar for COM
    XMLContent := PWideChar(WideString(XMLDoc));

    // Create notification from XML
    Notification := FFactory.CreateToastNotification(XMLContent);

    // Show notification
    if Failed(FNotifier.Show(Notification)) then
      raise Exception.Create('Failed to show notification');
  except
    on E: Exception do
      raise Exception.Create('Toast notification error: ' + E.Message);
  end;
end;

destructor TToastNotification.Destroy;
begin
  // Release interfaces
  FFactory := nil;
  FNotifier := nil;
  inherited;
end;

end.

