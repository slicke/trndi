unit slicke.wintools.2d2;

interface

uses
  Windows, SysUtils, ActiveX;

type
  // Basic color structure for Direct2D
  D2D1_COLOR_F = record
    r: Single;
    g: Single;
    b: Single;
    a: Single;
  end;

  // Rectangle structure with floating point values
  D2D1_RECT_F = record
    left: Single;
    top: Single;
    right: Single;
    bottom: Single;
  end;

  // Pixel Format-struktur
  D2D1_PIXEL_FORMAT = record
    format: UINT;
    alphaMode: UINT;
  end;

  // Enums for factory and render target types
  D2D1_FACTORY_TYPE = (
    D2D1_FACTORY_TYPE_SINGLE_THREADED,
    D2D1_FACTORY_TYPE_MULTI_THREADED
  );

  D2D1_RENDER_TARGET_TYPE = (
    D2D1_RENDER_TARGET_TYPE_DEFAULT,
    D2D1_RENDER_TARGET_TYPE_SOFTWARE,
    D2D1_RENDER_TARGET_TYPE_HARDWARE
  );

  D2D1_RENDER_TARGET_USAGE = (
    D2D1_RENDER_TARGET_USAGE_NONE,
    D2D1_RENDER_TARGET_USAGE_FORCE_BITMAP_REMOTING,
    D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE
  );

  D2D1_ALPHA_MODE = (
    D2D1_ALPHA_MODE_UNKNOWN,
    D2D1_ALPHA_MODE_PREMULTIPLIED,
    D2D1_ALPHA_MODE_STRAIGHT,
    D2D1_ALPHA_MODE_IGNORE
  );

  // Render Target Properties
  D2D1_RENDER_TARGET_PROPERTIES = record
    _type: D2D1_RENDER_TARGET_TYPE;
    pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
    usage: D2D1_RENDER_TARGET_USAGE;
    minLevel: UINT;
  end;

  // HWND Render Target Properties
  D2D1_HWND_RENDER_TARGET_PROPERTIES = record
    hwnd: HWND;
    pixelSize: TSize;
    presentOptions: UINT;
  end;

  // Interface for DirectWrite text format
  IDWriteTextFormat = interface(IUnknown)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
    // Methods for IDWriteTextFormat can be added here if needed
  end;

  // Interface for ID2D1Resource
  ID2D1Resource = interface(IUnknown)
    ['{2CD90691-12E2-11DC-9FED-001143A055F9}']
    procedure GetFactory(out factory: ID2D1Factory); stdcall;
  end;

  // Interface for Direct2D brush
  ID2D1Brush = interface(ID2D1Resource)
    ['{2CD906A8-12E2-11DC-9FED-001143A055F9}']
  end;

  // Interface for Direct2D solid color brush
  ID2D1SolidColorBrush = interface(ID2D1Brush)
    ['{2CD906A9-12E2-11DC-9FED-001143A055F9}']
  end;

  // Interface for ID2D1RenderTarget
  ID2D1RenderTarget = interface(ID2D1Resource)
    ['{2CD90694-12E2-11DC-9FED-001143A055F9}']
    function CreateSolidColorBrush(
      const color: D2D1_COLOR_F;
      const brushProperties: Pointer;
      out solidColorBrush: ID2D1SolidColorBrush
    ): HRESULT; stdcall;
    procedure BeginDraw(); stdcall;
    function EndDraw(tag1: Pointer; tag2: Pointer): HRESULT; stdcall;
    procedure Clear(const clearColor: D2D1_COLOR_F); stdcall;
    procedure DrawText(
      const string_: PWideChar;
      stringLength: UINT32;
      textFormat: IDWriteTextFormat;
      const layoutRect: D2D1_RECT_F;
      defaultForegroundBrush: ID2D1Brush;
      options: UINT;
      measuringMode: UINT
    ); stdcall;
  end;

  // Interface for ID2D1Factory
  ID2D1Factory = interface(IUnknown)
    ['{06152247-6F50-465A-9245-118BFD3B6007}']
    function ReloadSystemMetrics(): HRESULT; stdcall;
    procedure GetDesktopDpi(out dpiX, dpiY: Single); stdcall;
    function CreateHwndRenderTarget(
      const renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
      const hwndRenderTargetProperties: D2D1_HWND_RENDER_TARGET_PROPERTIES;
      out hwndRenderTarget: ID2D1HwndRenderTarget
    ): HRESULT; stdcall;
  end;

  // Functions for creating Direct2D and DirectWrite factories
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

implementation

end.

