unit slicke.wintools.d2d;

interface

implementation

end.
                        {
uses
  Windows, SysUtils, ExtCtrls;


const
  DWRITE_FACTORY_TYPE_SHARED = 0;
  DWRITE_FACTORY_TYPE_ISOLATED = 1;
  DXGI_FORMAT_UNKNOWN = 0;
  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT = $00000004;

  IID_ID2D1Factory: TGUID = '{06152247-6F50-465A-9245-118BFD3B6007}';
  IID_IDWriteFactory: TGUID = '{B859EE5A-D838-4B5B-A2E8-1AD2BAA0EF42}';

  var
  // Create Direct2D and DirectWrite factories once
  D2DFactoryInitialized: boolean = false;
  DWFactoryInitialized: Boolean = False;

type

   UTFCanvas = TPanel;


  // Framåtreferenser för gränssnitt och typer
  ID2D1Factory = interface;
  ID2D1HwndRenderTarget = interface;

  // Grundläggande enums
  D2D1_FACTORY_TYPE = (D2D1_FACTORY_TYPE_SINGLE_THREADED, D2D1_FACTORY_TYPE_MULTI_THREADED);
  D2D1_RENDER_TARGET_TYPE = (D2D1_RENDER_TARGET_TYPE_DEFAULT, D2D1_RENDER_TARGET_TYPE_SOFTWARE, D2D1_RENDER_TARGET_TYPE_HARDWARE);
  D2D1_ALPHA_MODE = (D2D1_ALPHA_MODE_UNKNOWN, D2D1_ALPHA_MODE_PREMULTIPLIED, D2D1_ALPHA_MODE_STRAIGHT, D2D1_ALPHA_MODE_IGNORE);

  // Grundläggande färgstruktur
  D2D1_COLOR_F = record
    r, g, b, a: Single;
  end;

  // Pixel Format-struktur för att specificera alpha-mode och format
  D2D1_PIXEL_FORMAT = record
    format: UINT;
    alphaMode: D2D1_ALPHA_MODE;
  end;

  // Render Target Properties och HWND Properties
  D2D1_RENDER_TARGET_PROPERTIES = record
    _type: D2D1_RENDER_TARGET_TYPE;
    pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX, dpiY: Single;
  end;

  D2D1_HWND_RENDER_TARGET_PROPERTIES = record
    hwnd: HWND;
    pixelSize: TSize;
    presentOptions: UINT;
  end;

    TD2D1CreateFactory = function(
    factoryType: D2D1_FACTORY_TYPE;
    const riid: TGUID;
    pFactoryOptions: Pointer;
    out ppIFactory: ID2D1Factory
  ): HRESULT; stdcall;

  TDWriteCreateFactory = function(
    factoryType: UINT;
    const iid: TGUID;
    out factory: IUnknown
  ): HRESULT; stdcall;

  // Minimal definition av ID2D1Factory för att skapa HwndRenderTarget
  ID2D1Factory = interface(IUnknown)
    ['{06152247-6F50-465A-9245-118BFD3B6007}']
    function CreateHwndRenderTarget(
      const renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
      const hwndRenderTargetProperties: D2D1_HWND_RENDER_TARGET_PROPERTIES;
      out hwndRenderTarget: ID2D1HwndRenderTarget
    ): HRESULT; stdcall;
  end;


  // Minimal definition av ID2D1SolidColorBrush
  ID2D1SolidColorBrush = interface(IUnknown)
    ['{2CD906A9-12E2-11DC-9FED-001143A055F9}']
  end;

  // Minimal definition av IDWriteTextFormat
  IDWriteTextFormat = interface(IUnknown)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
  end;


    IDWriteFactory = interface(IUnknown)
    ['{B859EE5A-D838-4B5B-A2E8-1AD2BAA0EF42}']
    function CreateTextFormat(
      const fontFamilyName: PWideChar;
      fontCollection: Pointer;
      fontWeight, fontStyle, fontStretch: Cardinal;
      fontSize: Single;
      const localeName: PWideChar;
      out textFormat: IDWriteTextFormat
    ): HRESULT; stdcall;
  end;

  D2D1_RECT_F = record
    left: Single;
    top: Single;
    right: Single;
    bottom: Single;
  end;

    D2D1_MATRIX_3X2_F = record
    m11, m12: Single;
    m21, m22: Single;
    dx, dy: Single;
  end;

   ID2D1Brush = interface(IUnknown)
    ['{2CD906A8-12E2-11DC-9FED-001143A055F9}']
    procedure SetOpacity(opacity: Single); stdcall;
    function GetOpacity: Single; stdcall;
    procedure SetTransform(const transform: D2D1_MATRIX_3X2_F); stdcall;
    procedure GetTransform(out transform: D2D1_MATRIX_3X2_F); stdcall;
  end;

      // Minimal definition av ID2D1RenderTarget
  ID2D1RenderTarget = interface(IUnknown)
    ['{2CD90694-12E2-11DC-9FED-001143A055F9}']
    procedure BeginDraw(); stdcall;
    function EndDraw(tag1: Pointer; tag2: Pointer): HRESULT; stdcall;
    procedure Clear(const clearColor: D2D1_COLOR_F); stdcall;

     procedure DrawText(
    const string_: PWideChar;            // Texten som ska ritas
    stringLength: UINT32;                // Antal tecken i texten
    textFormat: IDWriteTextFormat;       // Textformat (typsnitt, storlek, etc.)
    const layoutRect: D2D1_RECT_F;       // Rektangel inom vilken texten ritas
    defaultForegroundBrush: ID2D1Brush;  // Borste för textfärg
    options: UINT32 = 0;                 // Ritalternativ (t.ex. färgade teckensnitt)
    measuringMode: UINT32 = 0            // Mätläge för text (t.ex. IDWriteMeasuringMode_Natural)
  ); stdcall;
  end;

  // Gränssnitt för Direct2D render target på en Windows-fönsterhanterare (HWND)
  ID2D1HwndRenderTarget = interface(ID2D1RenderTarget)
    ['{2CD90698-12E2-11DC-9FED-001143A055F9}']
  end;


  // Förenklad extern funktion för att skapa en Direct2D Factory
  function D2D1CreateFactory(
    factoryType: D2D1_FACTORY_TYPE;
    const riid: TGUID;
    pFactoryOptions: Pointer;
    out ppIFactory: ID2D1Factory
  ): HRESULT; stdcall; external 'd2d1.dll' name 'D2D1CreateFactory';


 function DWriteCreateFactory(
  factoryType: UINT;
  const iid: TGUID;
  out factory: IUnknown
): HRESULT; stdcall; external 'dwrite.dll' name 'DWriteCreateFactory';


procedure ShowEmojiInPanel(Panel: TPanel; const EmojiCode: UnicodeString);
implementation

procedure ShowEmojiInPanel(Panel: TPanel; const EmojiCode: UnicodeString);
var
  D2DFactory: ID2D1Factory;
  RenderTarget: ID2D1HwndRenderTarget;
  TextFormat: IDWriteTextFormat;
  D2DRenderProps: D2D1_RENDER_TARGET_PROPERTIES;
  D2DPixelFormat: D2D1_PIXEL_FORMAT;
  D2DHwndProps: D2D1_HWND_RENDER_TARGET_PROPERTIES;
  TextRect: D2D1_RECT_F;
  DWFactory: IUnknown;
  DWFactoryAsIDWrite: IDWriteFactory;
  BackgroundColor: D2D1_COLOR_F;

begin
  // Initialize Direct2D and DirectWrite factories only once
  if not D2DFactoryInitialized then
  begin
    if D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, ID2D1Factory, nil, D2DFactory) <> S_OK then
      raise Exception.Create('Failed to create Direct2D factory.');

    if DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, DWFactory) <> S_OK then
      raise Exception.Create('Failed to create DirectWrite factory.');

    DWFactoryAsIDWrite := DWFactory as IDWriteFactory;
    D2DFactoryInitialized := True;
    DWFactoryInitialized := True;
  end;

  // ... (rest of the code, including creating render target and text format)

  // Clear the render target only if necessary
  if not Panel.Parent.Enabled then
    RenderTarget.Clear(BackgroundColor);

  // ... (rest of the code, including drawing the emoji)

  // Release resources when done (optional, but recommended for proper cleanup)
//  RenderTarget.EndDraw;
//  TextFormat.
end;

end.                   }

