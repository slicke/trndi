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
  JwaWinType, PropSys; // Kräver jwa-win32api-paketet

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

  // Frigör COM
  CoUninitialize;
  inherited;
end;

function TJumpListManager.Initialize: Boolean;
var
  HR: HRESULT;
begin
  Result := False;

  // Skapa Custom Destination List
  HR := CoCreateInstance(CLSID_DestinationList, nil, CLSCTX_INPROC_SERVER,
                       IID_ICustomDestinationList, FDestList);
  if Failed(HR) then
    Exit;

  // Sätt AppID (viktig för Windows 11 för att associera appen korrekt)
  HR := FDestList.SetAppID(PWideChar(FAppId));
  if Failed(HR) then
    Exit;

  // Börja bygga listan
  HR := FDestList.BeginList(FMaxItems, IID_IObjectArray, FObjectArray);
  Result := Succeeded(HR);
end;

function TJumpListManager.GetShellLink(const Title, Path, Args, IconPath: string;
  IconIndex: Integer): IShellLink;
var
  ShellLink: IShellLink;
  PropertyStore: IPropertyStore;


