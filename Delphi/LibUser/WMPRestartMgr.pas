(****************************************************************************
 *
 * Copyright 2021-2022 Tim De Baets
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ****************************************************************************
 *
 * Windows Media Player Restart Manager class
 *
 ****************************************************************************)

unit WMPRestartMgr;

// TODO: test: not restarting WMP when first run under a fresh user account? (command-line related?)
// TODO: force WMPStateRestore to always restore all state regardless of current settings (by passing new command-line param)

interface               

uses Windows, Messages, SysUtils, ShellAPI, ActiveX, WMPUtil, Common2,
    CommonUnicode, CmnFunc2, PathFunc;

type
  TWMPRestartMgrErrorReason = (wmprmeNoExitProcessHook, wmprmeNoWMPlayerApp,
      wmprmeIsBurning, wmprmeIsRipping, wmprmeRemoteApps, wmprmeRestartPending,
      wmprmeCreateProcFailed, wmprmeCreateTimerFailed, wmprmeTimeout);

type
  TWMPRestartMgrError = class
  private
    fReason: TWMPRestartMgrErrorReason;
    fErrorCode: Integer;
  public
    constructor Create(Reason: TWMPRestartMgrErrorReason; ErrorCode: Integer);
    function ToString: String;
    property Reason: TWMPRestartMgrErrorReason read fReason;
    property ErrorCode: Integer read fErrorCode;
  end;

type
  IWMPRestartMgrEvents = interface
    procedure OnRestartError(hWndParent: HWND; Error: TWMPRestartMgrError);
    function OnRemoteAppsFound(hWndParent: HWND;
        const RemoteAppNames: TStringArray): Boolean;
  end;

type
  TWMPRestartMgr = class
  private
    fPassOrigParams: Boolean;
    fEvents: IWMPRestartMgrEvents;
    fExitProcessHooked: Boolean;
    fpStub_TimerProc: Pointer;
    fpStub_ErrorThreadProc: Pointer;
    fRestarting: Boolean;
    fHandlingExitProcess: Boolean;
    fhRestarterProc: THandle;
    function CheckRemoteApps(hWndParent: HWND; Core: IWMPCoreSafe;
        var IsCanceled: Boolean): Boolean;
    function GetRemoteApps(Core: IWMPCoreSafe; var Count: Integer;
        pNames: PStringArray): Boolean;
    procedure OnError(hWndParent: HWND; Reason: TWMPRestartMgrErrorReason;
        ErrorCode: Integer);
    procedure TimerProc(hwnd: HWND; uMsg, idEvent: UINT; dwTime: DWORD); stdcall;
    function ErrorThreadProc(Error: TWMPRestartMgrError): DWORD; stdcall;
  public
    constructor Create(Events: IWMPRestartMgrEvents);
    destructor Destroy; override;
    procedure Restart(hWndParent: HWND; Core: IWMPCoreSafe;
        var IsCanceled: Boolean);
    procedure SetExitProcessHooked;
    procedure OnExitProcess;
    property PassOrigParams: Boolean read fPassOrigParams write fPassOrigParams;
  end;

function RestartWMP(Wnd: HWnd; Instance: HInst; CmdLine: PChar;
    nCmdShow: Integer): BOOL; stdcall;

implementation

uses ClassCallback, WMPLib_TLB, WMPUndocumented;

const
  AppRestarterTimeout = 5000; // milliseconds
  WMPExitTimeout = 10000; // milliseconds
  RestartAtomName = 'WMPRestartMgr_{EAE2AD4D-6455-443c-AC9F-F892E8021556}';
  Rundll32 = 'rundll32.exe';
  Rundll32Params = '%s,RestartWMP %u %u %s';

const
  RestartErrorStrs: array[TWMPRestartMgrErrorReason] of String = (
    'wmprmeNoExitProcessHook', 'wmprmeNoWMPlayerApp', 'wmprmeIsBurning',
    'wmprmeIsRipping', 'wmprmeRemoteApps', 'wmprmeRestartPending',
    'wmprmeCreateProcFailed', 'wmprmeCreateTimerFailed', 'wmprmeTimeout'
  );

constructor TWMPRestartMgrError.Create(Reason: TWMPRestartMgrErrorReason;
    ErrorCode: Integer);
begin
  inherited Create;
  fReason := Reason;
  fErrorCode := ErrorCode;
end;

function TWMPRestartMgrError.ToString: String;
begin
  Result := RestartErrorStrs[fReason];
end;

constructor TWMPRestartMgr.Create(Events: IWMPRestartMgrEvents);
begin
  inherited Create;
  fPassOrigParams := False;
  fEvents := Events;
  fExitProcessHooked := False;
  fRestarting := False;
  fHandlingExitProcess := False;
  fhRestarterProc := INVALID_HANDLE_VALUE;
  fpStub_TimerProc := CreateStub(Self, @TWMPRestartMgr.TimerProc);
  fpStub_ErrorThreadProc := CreateStub(Self, @TWMPRestartMgr.ErrorThreadProc);
end;

destructor TWMPRestartMgr.Destroy;
begin
  DisposeAndNilStub(fpStub_ErrorThreadProc);
  DisposeAndNilStub(fpStub_TimerProc);
  if fhRestarterProc <> INVALID_HANDLE_VALUE then
    CloseHandle(fhRestarterProc);
  fEvents := nil;
  inherited;
end;

procedure TWMPRestartMgr.Restart(hWndParent: HWND; Core: IWMPCoreSafe;
    var IsCanceled: Boolean);
var
  hWMPlayerApp: HWND;
  RestartAtom: ATOM;
  Error: Boolean;
begin
  IsCanceled := False;
  Error := True; // assume failure
  if not fExitProcessHooked then begin
    OnError(hWndParent, wmprmeNoExitProcessHook, 0);
    Exit;
  end;
  if not CheckRemoteApps(hWndParent, Core, IsCanceled) then begin
    OnError(hWndParent, wmprmeRemoteApps, 0);
    Exit;
  end;
  if IsCanceled then
    Exit;
  if FindAtom(RestartAtomName) <> 0 then begin
    OnError(hWndParent, wmprmeRestartPending, 0);
    Exit;
  end;
  if IsWMPBurning(Core) then begin
    OnError(hWndParent, wmprmeIsBurning, 0);
    Exit;
  end;
  if IsWMPRipping(Core) then begin
    OnError(hWndParent, wmprmeIsRipping, 0);
    Exit;
  end;
  hWMPlayerApp := GetWMPlayerAppWindow;
  if hWMPlayerApp = 0 then begin
    OnError(hWndParent, wmprmeNoWMPlayerApp, 0);
    Exit;
  end;
  RestartAtom := AddAtom(RestartAtomName);
  try
    // TODO: check for syncing?
    if SetTimer(0, 0, WMPExitTimeout, fpStub_TimerProc) = 0 then begin
      OnError(hWndParent, wmprmeCreateTimerFailed, GetLastError);
      Exit;
    end;
    Error := False;
    fRestarting := True;
    CloseWMP(hWMPlayerApp);
  finally
    if Error and (RestartAtom <> 0) then
      DeleteAtom(RestartAtom);
  end;
end;

procedure TWMPRestartMgr.OnExitProcess;
var
  QuotedCmdLine: WideString;
  Params: WideString;
  Result: Integer;
begin
  if fRestarting then begin
    fHandlingExitProcess := True;
    if fPassOrigParams then
      QuotedCmdLine := QuoteArgvForCmdLine(GetCommandLineW)
    else
      QuotedCmdLine := AddQuotes(ParamStr(0)); // TODO: add unicode support?
    Params := RealWideFormat(Rundll32Params,
        [AddQuotes(GetModuleName(hInstance)), GetCurrentProcessId,
         AppRestarterTimeout, QuotedCmdLine]);
    Result := ExecuteProcess(AddBackslash(GetSystemDir) + Rundll32, Params, '',
        SW_SHOWNORMAL, nil);
    if Result <> 0 then begin
      OnError(0, wmprmeCreateProcFailed, Result);
      Exit;
    end;
  end;
end;

function TWMPRestartMgr.CheckRemoteApps(hWndParent: HWND;
    Core: IWMPCoreSafe; var IsCanceled: Boolean): Boolean;
var
  NumRemoteApps: Integer;
  RemoteAppNames: TStringArray;
begin
  Result := True;
  while True do begin
    if not GetRemoteApps(Core, NumRemoteApps, @RemoteAppNames) then
      Break;
    if NumRemoteApps = 0 then
      Break;
    if not Assigned(fEvents) then begin
      Result := False;
      Break;
    end;
    if not fEvents.OnRemoteAppsFound(hWndParent, RemoteAppNames) then begin
      IsCanceled := True;
      Break;
    end;
  end;
end;

function TWMPRestartMgr.GetRemoteApps(Core: IWMPCoreSafe; var Count: Integer;
    pNames: PStringArray): Boolean;
  procedure HandleRemoteLocation(RemoteObj: IUnknown);
  var
    OleObject: IOleObject;
    ClientSite: IOleClientSite;
    ServiceProvider: IServiceProvider;
    RemoteServices: IWMPRemoteMediaServices;
    ServiceType, AppName: WideString;
  begin
    if not Assigned(RemoteObj) then
      Exit;
    if not Succeeded(RemoteObj.QueryInterface(IOleObject, OleObject)) then
      Exit;
    if not Succeeded(OleObject.GetClientSite(ClientSite)) then
      Exit;
    if not Succeeded(ClientSite.QueryInterface(IServiceProvider,
        ServiceProvider)) then
      Exit;
    // On WMP 11, the following call (for an actual remote app that isn't a
    // deskband), always fails
    // TODO: try to find out why (WMP11)
    if not Succeeded(ServiceProvider.QueryService(SID_SWMPRemoteMediaServices,
        IWMPRemoteMediaServices, RemoteServices)) then
      Exit;
    if not Succeeded(RemoteServices.GetServiceType(ServiceType)) then
      Exit;
    if CompareText(ServiceType, WMPServiceType_RemoteDeskband) = 0 then
      Exit;
    if not Succeeded(RemoteServices.GetApplicationName(AppName)) then
      Exit;
    Inc(Count);
    if Assigned(pNames) then
      AddToStringArray(pNames^, AppName);
  end;
var
  PluginMgr: IWMPPluginMgr;
  NumLocations, i: Integer;
  RemoteObj: IUnknown;
  RequestType: Integer;
begin
  Result := False;
  if Assigned(pNames) then
    SetLength(pNames^, 0);
  Count := 0;
  if not Succeeded(Core.QueryInterface(IWMPPluginMgr, PluginMgr)) then
    Exit;
  // On WMP 11, the following call returns a count of 0 when only the deskband is
  // enabled
  // TODO: try to find out why (WMP11)
  if not Succeeded(PluginMgr.getRemoteLocationsCount(NumLocations)) then
    Exit;
  for i := 0 to NumLocations - 1 do begin
    if not Succeeded(PluginMgr.getRemoteLocationInfo(i, RemoteObj,
        RequestType)) then
      Continue;
    HandleRemoteLocation(RemoteObj);
  end;
  Result := True;
end;

procedure TWMPRestartMgr.SetExitProcessHooked;
begin
  fExitProcessHooked := True;
end;

function TWMPRestartMgr.ErrorThreadProc(Error: TWMPRestartMgrError): DWORD;
    stdcall;
begin
  Result := 0;
  if not Assigned(Error) then
    Exit;
  if Assigned(fEvents) then
    fEvents.OnRestartError(0, Error);
  FreeAndNil(Error);
end;

procedure TWMPRestartMgr.OnError(hWndParent: HWND;
    Reason: TWMPRestartMgrErrorReason; ErrorCode: Integer);
var
  Error: TWMPRestartMgrError;
  hThread: THandle;
  ThreadID: DWORD;
begin
  fRestarting := False;
  if Assigned(fEvents) then begin
    Error := TWMPRestartMgrError.Create(Reason, ErrorCode);
    try
      if fHandlingExitProcess then begin
        // We're just about to exit. If we would show a message box here, it would
        // be dismissed immediately because of the WM_QUIT message that is still
        // on the message queue. As a workaround, we create a helper thread to
        // handle the error. That way, message boxes can still be displayed.
        hThread := CreateThread(nil, 0, fpStub_ErrorThreadProc, Error, 0, ThreadID);
        if hThread = 0 then begin
          FreeAndNil(Error);
          Exit;
        end;
        try
          WaitForSingleObject(hThread, INFINITE);
        finally
          CloseHandle(hThread);
        end;
      end
      else
        fEvents.OnRestartError(hWndParent, Error);
    finally
      // helper thread consumes Error object
      if not fHandlingExitProcess then
        FreeAndNil(Error);
    end;
  end;
end;

procedure TWMPRestartMgr.TimerProc(hwnd: HWND; uMsg, idEvent: UINT;
    dwTime: DWORD);
var
  RestartAtom: ATOM;
begin
  if fHandlingExitProcess then
    Exit;
  KillTimer(0, idEvent);
  RestartAtom := FindAtom(RestartAtomName);
  if RestartAtom <> 0 then
    DeleteAtom(RestartAtom);
  OnError(0, wmprmeTimeout, 0);
end;

//
// Following code runs in the context of the restarter helper application, which
// is currently rundll32.
//

procedure RestartWMPInternal(const CmdLine: WideString);
var
  NumParams: Integer;
  ppParams: PPWideChar;
  Params: PPWideCharArray;
  WaitPID: DWORD;
  Timeout: Cardinal;
  hProc: THandle;
  WMPCmdLine: WideString;
begin
  ppParams := CommandLineToArgvW(PWideChar(CmdLine), NumParams);
  if not Assigned(ppParams) then begin
    ExitCode := 1;
    Exit;
  end;
  try
    Params := PPWideCharArray(ppParams);
    if NumParams < 4 then begin
      ExitCode := 2;
      Exit;
    end;
    WaitPID := StrToIntDef(Params[1], 0);
    if WaitPID = 0 then begin
      ExitCode := 3;
      Exit;
    end;
    Timeout := StrToIntDef(Params[2], 0);
    if Timeout = 0 then begin
      ExitCode := 4;
      Exit;
    end;
    WMPCmdLine := Params[3];
    if Trim(WMPCmdLine) = '' then begin
      ExitCode := 5;
      Exit;
    end;
    hProc := OpenProcess(SYNCHRONIZE, False, WaitPID);
    if hProc = 0 then begin
      if GetLastError <> ERROR_INVALID_PARAMETER then begin
        // OpenProcess failed for another reason than 'PID doesn't exist'
        ExitCode := 6;
        Exit;
      end;
    end
    else try
      // Rundll32 creates its own window, so we still need to pump messages while
      // waiting for the process to close
      if MsgWaitForObjectWithTimeout(hProc, Timeout) <> WAIT_OBJECT_0 then begin
        ExitCode := 7;
        Exit;
      end;
    finally
      CloseHandle(hProc);
    end;
    if not RunCmdLine(WMPCmdLine) then begin
      ExitCode := 8;
      Exit;
    end;
    ExitCode := 0;
  finally
    LocalFree(HLOCAL(ppParams));
  end;
end;

function RestartWMP(Wnd: HWnd; Instance: HInst; CmdLine: PChar;
    nCmdShow: Integer): BOOL; stdcall;
var
  FullCmdLine: WideString;
begin
  Result := True;
  // CommandLineToArgvW requires the first parameter to be the executable path,
  // otherwise the remaining parameters won't be parsed correctly.
  FullCmdLine := AddQuotes(ParamStr(0)) + ' ' + String(CmdLine);
  RestartWMPInternal(FullCmdLine);
end;

end.
