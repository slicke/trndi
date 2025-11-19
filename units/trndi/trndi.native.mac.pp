(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * GitHub: https://github.com/slicke/trndi
 *)
unit trndi.native.mac;

{**
  @abstract(macOS-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeMac) which derives from
  @link(TTrndiNativeBase) and implements:
  - Text-to-speech via the built-in @code(say) command
  - Enabling dark appearance via @code(SimpleDarkMode)
  - Dock badge label updates
  - Simple HTTP GET using an NS-based helper

  Use the façade unit @code(trndi.native) which provides the platform alias.
}

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, NSMisc, ns_url_request, CocoaAll, SimpleDarkMode,
  trndi.native.base;

type
  {!
    @abstract(macOS implementation of @link(TTrndiNativeBase).)
    Relies on system tools for speech and dark appearance toggling.
  }
  TTrndiNativeMac = class(TTrndiNativeBase)
  public
    {** Speak @param(Text) using the built-in 'say' command.
        Note: This call blocks until speech completes; dispatch from a
        background thread if you need non-blocking UI. }
    procedure Speak(const Text: string); override;
    {** Enable dark appearance for the app UI via SimpleDarkMode.
        @returns(True once the request is made) }
    class function setDarkMode: boolean;

  // Settings API overrides (plist-backed in app config directory)
  {** Read a string from the plist store; returns @param(def) when missing.
    Keys are scoped by @link(TTrndiNativeBase.buildKey). }
    function GetSetting(const keyname: string; def: string = '';
      global: boolean = False): string; override;
  {** Write a string to the plist store under the scoped key. }
    procedure SetSetting(const keyname: string; const val: string;
      global: boolean = False); override;
  {** Delete a setting by removing it from the plist store. }
    procedure DeleteSetting(const keyname: string; global: boolean = False); override;
  {** Reload plist cache from disk. }
    procedure ReloadSettings; override;
    // Badge
    {** Set the dock tile badge label (text only). }
    procedure SetBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    {** Ignore extra params and delegate to simple overload. }
    procedure SetBadge(const Value: string; BadgeColor: TColor;
      badge_size_ratio: double; min_font_size: integer); overload; override;
  {** Simple HTTP GET using NS HTTP helper with default UA.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
    class function getURL(const url: string; out res: string): boolean; override;
    {** True if AppleInterfaceStyle indicates dark mode. }
    class function isDarkMode: boolean; override;
    {** NSUserNotificationCenter is available on macOS. }
    class function isNotificationSystemAvailable: boolean; override;
    {** Identify the notification backend on macOS ('NSUserNotification'). }
    class function getNotificationSystem: string; override;
  end;

implementation

uses
  Process, DOM, XMLRead, XMLWrite, LazFileUtils;

const
  MAC_PLIST_FILENAME = 'Trndi.plist';

var
  GPlistCache: TStringList = nil;
  GPlistLoaded: boolean = false;

function MacConfigDirectory: string;
var
  dir: string;
begin
  dir := GetAppConfigDirUTF8(false, false);

  // Lazarus may return an empty string when the bundle lacks metadata; fall back to the
  // standard per-user Application Support folder in that case.
  if Trim(dir) = '' then
    dir := GetUserDir + 'Library/Application Support/Trndi';

  if not ForceDirectoriesUTF8(dir) then
  begin
    // As a last resort, use a writable per-user folder so settings are not lost.
    dir := GetUserDir + '.trndi';
    if not ForceDirectoriesUTF8(dir) then
      dir := GetTempDirUTF8;
  end;

  Result := IncludeTrailingPathDelimiter(dir);
end;

function MacConfigPlistPath: string;
begin
  Result := MacConfigDirectory + MAC_PLIST_FILENAME;
end;

function NextElementSibling(Node: TDOMNode): TDOMNode;
begin
  Result := Node;
  repeat
    if Result = nil then
      Exit(nil);
    Result := Result.NextSibling;
  until (Result = nil) or (Result.NodeType = ELEMENT_NODE);
end;

function ExtractPlistValue(ValueNode: TDOMNode): string;
begin
  if not Assigned(ValueNode) then
    Exit('');

  if SameText(ValueNode.NodeName, 'string') or
     SameText(ValueNode.NodeName, 'integer') or
     SameText(ValueNode.NodeName, 'real') or
     SameText(ValueNode.NodeName, 'data') then
    Result := ValueNode.TextContent
  else
  if SameText(ValueNode.NodeName, 'true') then
    Result := 'true'
  else
  if SameText(ValueNode.NodeName, 'false') then
    Result := 'false'
  else
    Result := ValueNode.TextContent;
end;

procedure LoadPlistIntoCache(list: TStringList);
var
  path: string;
  doc: TXMLDocument;
  dictNode, node, valueNode: TDOMNode;
  keyName, valueText: string;
begin
  if list = nil then
    Exit;

  list.Clear;
  path := MacConfigPlistPath;
  if not FileExistsUTF8(path) then
    Exit;

  doc := nil;
  try
    ReadXMLFile(doc, path);
  except
    Exit;
  end;

  try
    if not Assigned(doc.DocumentElement) then
      Exit;

    if not SameText(doc.DocumentElement.NodeName, 'plist') then
      Exit;

    dictNode := nil;
    node := doc.DocumentElement.FirstChild;
    while node <> nil do
    begin
      if (node.NodeType = ELEMENT_NODE) and SameText(node.NodeName, 'dict') then
      begin
        dictNode := node;
        Break;
      end;
      node := node.NextSibling;
    end;

    if dictNode = nil then
      Exit;

    node := dictNode.FirstChild;
    while node <> nil do
    begin
      if (node.NodeType = ELEMENT_NODE) and SameText(node.NodeName, 'key') then
      begin
        keyName := Trim(node.TextContent);
        valueNode := NextElementSibling(node);
        valueText := ExtractPlistValue(valueNode);
        if keyName <> '' then
          list.Values[keyName] := valueText;
      end;
      node := node.NextSibling;
    end;
  finally
    doc.Free;
  end;
end;

procedure EnsurePlistCache;
begin
  if GPlistCache = nil then
  begin
    GPlistCache := TStringList.Create;
    GPlistCache.NameValueSeparator := '=';
    GPlistCache.CaseSensitive := false;
    GPlistCache.Sorted := false;
  end;

  if not GPlistLoaded then
  begin
    LoadPlistIntoCache(GPlistCache);
    GPlistLoaded := true;
  end;
end;

procedure SavePlistCache;
var
  doc: TXMLDocument;
  plistElem, dictElem, keyElem, valueElem: TDOMElement;
  i: integer;
  key, value: string;
  path: string;
begin
  if GPlistCache = nil then
    Exit;

  doc := TXMLDocument.Create;
  try
    plistElem := doc.CreateElement('plist');
    plistElem.SetAttribute('version', '1.0');
    doc.AppendChild(plistElem);

    dictElem := doc.CreateElement('dict');
    plistElem.AppendChild(dictElem);

    for i := 0 to GPlistCache.Count - 1 do
    begin
      key := GPlistCache.Names[i];
      if key = '' then
        Continue;
      value := GPlistCache.ValueFromIndex[i];

      keyElem := doc.CreateElement('key');
      keyElem.AppendChild(doc.CreateTextNode(key));
      dictElem.AppendChild(keyElem);

      valueElem := doc.CreateElement('string');
      valueElem.AppendChild(doc.CreateTextNode(value));
      dictElem.AppendChild(valueElem);
    end;

    path := MacConfigPlistPath;
    ForceDirectoriesUTF8(ExtractFilePath(path));
    WriteXMLFile(doc, path);
  finally
    doc.Free;
  end;
end;
{------------------------------------------------------------------------------
  Speak
  -----
  Use the built-in 'say' tool to speak text (synchronous).
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.Speak(const Text: string);
var
  o: string;
begin
  // Use the built-in macOS speech synthesis
  // Note: This is synchronous; consider running in a background process
  // if you need to keep the UI responsive during long messages.
  RunCommand('/usr/bin/say', [Text], o);
end;

{------------------------------------------------------------------------------
  getURL
  ------
  Simple HTTP GET using NS-based helper; returns response text or error.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
  httpClient := TNSHTTPSendAndReceive.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'GET';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);

      if httpClient.SendAndReceive(send, response, headers) then
      begin
        res := Trim(response.DataString);
        Result := True;
      end
      else
      begin
        // Normalize an error: LastErrMsg usually contains the reason
        res := Trim(httpClient.LastErrMsg);
        Result := False;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    httpClient.Free;
    send.Free;
    response.Free;
    headers.Free;
  end;
end;

{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Detect macOS dark appearance via AppleInterfaceStyle preference.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.isDarkMode: boolean;
begin
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  -----------------------------
  NSUserNotificationCenter is present on macOS.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.isNotificationSystemAvailable: boolean;
begin
  Result := True; // NSUserNotificationCenter exists
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Identify the macOS notification backend used by this implementation.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.getNotificationSystem: string;
begin
  Result := 'NSUserNotification';
end;


{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Enable dark appearance for the app using SimpleDarkMode.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.setDarkMode: boolean;
begin
  // Enable dark appearance for the app's UI via SimpleDarkMode
  SimpleDarkMode.EnableAppDarkMode;
  Result := True;
end;

{------------------------------------------------------------------------------
  SetBadge (simple)
  -----------------
  Set the dock tile badge label with the provided value.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor);
var
  NSS: NSString;
begin
  NSS := NSSTR(Value);
  NSApp.dockTile.setBadgeLabel(NSS);
  NSS.Release;
end;

{------------------------------------------------------------------------------
  SetBadge (full signature)
  -------------------------
  Ignore extra parameters and delegate to the simple overload on macOS.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor;
  badge_size_ratio: double; min_font_size: integer);
begin
  // Ignore extra params on macOS and delegate to the simple overload
  SetBadge(Value, BadgeColor);
end;

{------------------------------------------------------------------------------
  GetSetting / SetSetting / DeleteSetting / ReloadSettings
  -------------------------------------------------------
  Plist-backed settings stored in the app config directory.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.GetSetting(const keyname: string; def: string;
  global: boolean): string;
var
  key: string;
  idx: integer;
  legacy: string;
begin
  key := buildKey(keyname, global);
  EnsurePlistCache;
  idx := GPlistCache.IndexOfName(key);
  if idx <> -1 then
    Result := GPlistCache.ValueFromIndex[idx]
  else
  begin
    legacy := GetPrefString(key);
    if legacy <> '' then
    begin
      GPlistCache.Values[key] := legacy;
      SavePlistCache;
      Result := legacy;
    end
    else
      Result := def;
  end;
end;

procedure TTrndiNativeMac.SetSetting(const keyname: string; const val: string;
  global: boolean);
var
  key: string;
begin
  key := buildKey(keyname, global);
  EnsurePlistCache;
  GPlistCache.Values[key] := val;
  SavePlistCache;
end;

procedure TTrndiNativeMac.DeleteSetting(const keyname: string; global: boolean);
var
  key: string;
  idx: integer;
begin
  key := buildKey(keyname, global);
  EnsurePlistCache;
  idx := GPlistCache.IndexOfName(key);
  if idx <> -1 then
  begin
    GPlistCache.Delete(idx);
    SavePlistCache;
  end;
end;

procedure TTrndiNativeMac.ReloadSettings;
begin
  GPlistLoaded := false;
  EnsurePlistCache;
end;

finalization
  FreeAndNil(GPlistCache);

end.
