unit JumpListManager;

{$mode objfpc}{$H+}

interface

uses
  Windows, ShlObj, ActiveX, ComObj, Classes, SysUtils;

type
  TJumpListManager = class
  private
    FAppId: WideString;
    FDestList: ICustomDestinationList;
    FObjectArray: IObjectArray;
    FCurrentCategory: string;
    FMaxItems: Cardinal;
    FRemovedItems: Cardinal;
    function GetShellLink(const Title, Path, Args, IconPath: string; IconIndex: Integer): IShellLink;
  public
    constructor Create(const AppId: string);
    destructor Destroy; override;

    function Initialize: Boolean;
    function BeginCategory(const CategoryName: string): Boolean;
    function AddTask(const Title, Path, Args, IconPath: string; IconIndex: Integer): Boolean;
    function AddDestination(const FilePath: string): Boolean;
    function CommitCategory: Boolean;
    function CommitList: Boolean;

    // Windows 11-specifika funktioner
    function AddJumpItem(const Title, Path, Args, IconPath: string; IconIndex: Integer): Boolean;
    function ClearAllCategories: Boolean;
    function SetAppUserModelProperties(const DisplayName, IconPath: string): Boolean;
  end;

implementation

uses
  JwaWinType, PropSys; // Requires jwa-win32api package

{ TJumpListManager }

constructor TJumpListManager.Create(const AppId: string);
begin
  inherited Create;
  FAppId := AppId;

  // Initialisera COM
  CoInitialize(nil);
end;

destructor TJumpListManager.Destroy;
begin
  FDestList := nil;
  FObjectArray := nil;

  // Release COM
  CoUninitialize;
  inherited;
end;

function TJumpListManager.Initialize: Boolean;
var
  HR: HRESULT;
begin
  Result := False;

  // Create Custom Destination List
  HR := CoCreateInstance(CLSID_DestinationList, nil, CLSCTX_INPROC_SERVER,
                       IID_ICustomDestinationList, FDestList);
  if Failed(HR) then
    Exit;

  // Set AppID (important for Windows 11 to associate the app correctly)
  HR := FDestList.SetAppID(PWideChar(FAppId));
  if Failed(HR) then
    Exit;

  // Start building the list
  HR := FDestList.BeginList(FMaxItems, IID_IObjectArray, FObjectArray);
  Result := Succeeded(HR);
end;

function TJumpListManager.GetShellLink(const Title, Path, Args, IconPath: string;
  IconIndex: Integer): IShellLink;
var
  ShellLink: IShellLink;
  PropertyStore: IPropertyStore;


