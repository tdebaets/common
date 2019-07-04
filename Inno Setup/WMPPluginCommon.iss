[Files]
Source: "{#AddBackslash(ExtractFilePath(__PATHFILENAME__))}..\AppCompatShims\WMPx64PluginFix.sdb"; DestDir: "{tmp}"; Flags: deleteafterinstall; MinVersion: 0,6.0; Check: IsWin64

[Run]
Filename: reg.exe; Parameters: "copy HKCU\SOFTWARE\Microsoft\MediaPlayer\UIPlugins HKCU\SOFTWARE\Wow6432Node\Microsoft\MediaPlayer\UIPlugins /s /f"; Flags: runhidden; MinVersion: 0,6.0; Check: "IsWin64 and not Wow6432UIPluginsKeyExists"; 
Filename: sdbinst.exe; Parameters: "-q ""{tmp}\WMPx64PluginFix.sdb"""; Flags: runhidden; MinVersion: 0,6.0; Check: IsWin64; 
Filename: {code:GetWMPExePath}; Description: {cm:RunNow}; Flags: nowait postinstall skipifsilent; Check: WMPExePathExists; Languages: 

[CustomMessages]
URL=http://www.bm-productions.tk
WMPPathNotFound=Setup was unable to find Windows Media Player on this computer.%nPlease make sure that at least version 11 of Windows Media Player is installed, this is required to use {#SetupSetting("AppName")}.
RunNow=&Run Windows Media Player now
WMPOrWMCRunningMessage=%1 has detected that Windows Media Player or Windows Media Center is currently running.%n%nPlease close all instances of these applications now, then click OK to continue, or Cancel to exit.

[Code]
const
  WMPlayerExe = 'wmplayer.exe';
  WMPAppPathKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\' + WMPlayerExe;
  WMPMutex = 'Microsoft_WMP_70_CheckForOtherInstanceMutex';
  WMPClass = 'WMPlayerApp';
  WMPDefaultExePath = '{pf}\Windows Media Player\' + WMPlayerExe;

var
  WMPExePath: String;

function ExpandEnvironmentStrings(Src: PChar; Dst: PChar; Size: Longword): Longword;
    external 'ExpandEnvironmentStringsA@kernel32.dll stdcall';

function ExpandEnvStr(var SysPath: String): Boolean;
var
  RealPath: String;
  BufSize: Integer;
begin
  BufSize := 1024;
  RealPath := StringOfChar(' ', BufSize);
  BufSize := ExpandEnvironmentStrings(PChar(SysPath), RealPath, BufSize);
  Result := BufSize > 0;
  if Result then
    SysPath := CastIntegerToString(CastStringToInteger(RealPath));
end;

function CheckWMPRunning: Boolean;
var
  WMPRunning: Boolean;
  AppTitle: String;
begin
  Result := True;
  if IsUninstaller then
    AppTitle := SetupMessage(msgUninstallAppTitle)
  else
    AppTitle := SetupMessage(msgSetupAppTitle);
  while True do begin
    WMPRunning := CheckForMutexes(WMPMutex) or (FindWindowByClassName(WMPClass) <> 0);
    if not WMPRunning then
      Exit
    else begin
      if MsgBox(FmtMessage(CustomMessage('WMPOrWMCRunningMessage'), [AppTitle]),
          mbError, MB_OKCANCEL) = IDCANCEL then begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function WMPExePathExists: Boolean;
begin
  Result := (WMPExePath <> '') and FileExists(WMPExePath);
end;

procedure ProcessWMPPath(var Path: String);
begin
  Path := RemoveQuotes(Path);
  ExpandEnvStr(Path);
end;

function FindWMPExe: Boolean;
begin
  Result := True;
  WMPExePath := '';
  RegQueryStringValue(HKEY_LOCAL_MACHINE, WMPAppPathKey, '', WMPExePath);
  ProcessWMPPath(WMPExePath);
  if WMPExePathExists then
    Exit;
  RegQueryStringValue(HKEY_CURRENT_USER, WMPAppPathKey, '', WMPExePath);
  ProcessWMPPath(WMPExePath);
  if WMPExePathExists then
    Exit;
  WMPExePath := ExpandConstant(WMPDefaultExePath);
  if WMPExePathExists then
    Exit;
  MsgBox(CustomMessage('WMPPathNotFound'), mbCriticalError, MB_OK);
  Result := False;
end;

procedure ShowWMPPathNotFoundError;
begin
  MsgBox(CustomMessage('WMPPathNotFound'), mbCriticalError, MB_OK);
end;

function GetWMPPath(Param: String): String;
begin
  Result := ExtractFileDir(WMPExePath);
end;

function GetWMPExePath(Param: String): String;
begin
  Result := WMPExePath;
end;

function Wow6432UIPluginsKeyExists: Boolean;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER,
      'SOFTWARE\Wow6432Node\Microsoft\MediaPlayer\UIPlugins');
end;

function IsUpgrade: Boolean;
var
  PrevPath: String;
begin
  PrevPath := WizardForm.PrevAppDir;
  Result := (PrevPath <> '');
end;
