; Inno Setup script for Trndi.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Trndi"
#define MyAppPublisher "Björn Lindh"
#define MyAppURL "https://github.com/slicke/trndi"
#define MyAppExeName "Trndi.exe"

; Auto-derive version from the built EXE; CI may override with /DMyAppVersion=...
#ifndef MyAppVersion
  #define ExeVersion GetVersionNumbersString("..\" + MyAppExeName)
  #if ExeVersion == ""
    #define MyAppVersion "10.0"
  #else
    #define MyAppVersion ExeVersion
  #endif
#endif

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{2DC38820-32FA-4243-9788-9BCF396588FD}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
UninstallDisplayIcon={app}\{#MyAppExeName}
UninstallDisplayName={#MyAppName} {#MyAppVersion}
; "ArchitecturesAllowed=x64compatible" specifies that Setup cannot run
; on anything but x64 and Windows 11 on Arm.
ArchitecturesAllowed=x64compatible
; "ArchitecturesInstallIn64BitMode=x64compatible" requests that the
; install be done in "64-bit mode" on x64 or Windows 11 on Arm,
; meaning it should use the native 64-bit Program Files directory and
; the 64-bit view of the registry.
ArchitecturesInstallIn64BitMode=x64compatible
MinVersion=10.0
DisableProgramGroupPage=yes
LicenseFile=..\LICENSE.md
; Uncomment the following line to run in non administrative install mode (install for current user only).
;PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputDir=.
OutputBaseFilename=TrndiSetup
SetupIconFile=..\Trndi.ico
SolidCompression=yes
WizardStyle=modern
; Detect a running Trndi during install/uninstall and offer to close it,
; so upgrades don't fail with file-in-use errors.
CloseApplications=yes
RestartApplications=yes
; Embed version metadata in the setup EXE itself (shown in Explorer
; properties and used by SmartScreen reputation).
VersionInfoVersion={#MyAppVersion}
VersionInfoProductVersion={#MyAppVersion}
VersionInfoCompany={#MyAppPublisher}
VersionInfoDescription={#MyAppName} Setup
VersionInfoProductName={#MyAppName}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "swedish"; MessagesFile: "compiler:Languages\Swedish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\lang\*"; DestDir: "{app}\lang"; Flags: ignoreversion recursesubdirs createallsubdirs
; CareLink login helper (sources only — node_modules is installed by the user
; per the guide). The app looks for it in {app}\tools\carelink-login.
Source: "..\tools\carelink-login\carelink-login.mjs"; DestDir: "{app}\tools\carelink-login"; Flags: ignoreversion
Source: "..\tools\carelink-login\package.json"; DestDir: "{app}\tools\carelink-login"; Flags: ignoreversion
Source: "..\tools\carelink-login\package-lock.json"; DestDir: "{app}\tools\carelink-login"; Flags: ignoreversion
Source: "..\tools\carelink-login\README.md"; DestDir: "{app}\tools\carelink-login"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

