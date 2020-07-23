;****************************************************************************
;*
;* Copyright 2019 Tim De Baets
;*
;* Licensed under the Apache License, Version 2.0 (the "License");
;* you may not use this file except in compliance with the License.
;* You may obtain a copy of the License at
;*
;*     http://www.apache.org/licenses/LICENSE-2.0
;*
;* Unless required by applicable law or agreed to in writing, software
;* distributed under the License is distributed on an "AS IS" BASIS,
;* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;* See the License for the specific language governing permissions and
;* limitations under the License.
;*
;****************************************************************************
;*
;* Common Inno Setup script for WMP plug-ins
;*
;****************************************************************************

[Run]
Filename: reg.exe; Parameters: "copy HKCU\SOFTWARE\Microsoft\MediaPlayer\UIPlugins HKCU\SOFTWARE\Wow6432Node\Microsoft\MediaPlayer\UIPlugins /s /f"; Flags: runhidden; MinVersion: 0,6.0; Check: "IsWin64 and not Wow6432UIPluginsKeyExists"; 
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

function ExpandEnvironmentStrings(Src: String; Dst: String;
    Size: Longword): Longword;
    external 'ExpandEnvironmentStringsW@kernel32.dll stdcall';

function ExpandEnvStr(var SysPath: String): Boolean;
var
  RealPath: String;
  BufSize: Integer;
begin
  BufSize := 1024;
  RealPath := StringOfChar(' ', BufSize);
  BufSize := ExpandEnvironmentStrings(SysPath, RealPath, BufSize);
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
