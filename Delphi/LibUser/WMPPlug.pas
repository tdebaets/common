(****************************************************************************
 *
 * Copyright 2020-2022 Tim De Baets
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
 * Base classes for Windows Media Player UI Plugin implementations
 *
 ****************************************************************************)

unit WMPPlug;

interface

uses Windows, Messages, ComObj, ComServ, ActiveX, ClassCallback, WMPPlug_TLB,
    WMPLib_TLB, WMPUtil, WMPUndocumented, NewDialogs, SysUtils, Common2,
    HighDPIFunc;

type
  TWMPPlug = class(TComObject, IWMPPluginUI)
  private

    fCore: IWMPCoreSafe;
    fWMPPlayerApp: IWMPPlayerApplicationSafe;

    fWMPlayerAppWnd: HWND;
    fWMPNowPlayingWnd: HWND;
    fWMPVersion: Byte;

    fPlugWindow: Integer;
    fWndProcStub: Pointer;

    fWMPLocLoaded: Boolean;
    fhWMPLoc: HMODULE;

    procedure ReleaseCore;

    function GetPlugWindow: Integer;
    function GetIsPlayerDocked: Boolean;

    function WndProc(hWnd, uMsg, wParam, lParam: Integer): Integer; stdcall;

  protected

    PlugName: String;

    procedure Load; virtual;
    procedure Release; virtual;

    function ShouldShowPluginSettings: Boolean; virtual;
    function ShowPluginSettings(hWndParent: HWND): HResult;
    procedure DoShowPluginSettings(hWndParent: HWND); virtual;

    function OnMessage(var Msg: TMessage): Boolean; virtual;

    function GetWMPlayerAppWnd: HWND;
    function GetWMPNowPlayingWnd: HWND;
    function GetWMPVersion: Byte;
    function GethWMPLoc: HMODULE;

    // IWMPPluginUI
    function SetCore(const pCore: IWMPCore): HResult; virtual; stdcall;
    function CreateProc(hwndParent: HWND;
        out phwndWindow: HWND): HResult; virtual; stdcall;
    function DestroyProc: HResult; virtual; stdcall;
    function DisplayPropertyPage(hwndParent: HWND): HResult; virtual; stdcall;
    function GetProperty(pwszName: PWideChar;
        out pvarProperty: OleVariant): HResult; virtual; stdcall;
    function SetProperty(pwszName: PWideChar;
        var pvarProperty: OleVariant): HResult; virtual; stdcall;
    function TranslateAccelerator(var lpMsg: tagMSG): HResult; virtual; stdcall;

    property PlugWindow: Integer read GetPlugWindow;

  public

    procedure Initialize; override;
    destructor Destroy; override;

    function WMPMsgBox(AhWndOwner: Integer; const Prompt: String;
        bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
        const Caption: String; Context: Integer): Boolean; overload;
    function WMPMsgBox(const Prompt: String;
        bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
        const Caption: String; Context: Integer): Boolean; overload;

    procedure CallAsync(const Method: TMethodFunc);

    function GetModalhWndParent: HWND;

    property Core: IWMPCoreSafe read fCore;

    property WMPlayerAppWnd: HWND read GetWMPlayerAppWnd;
    property WMPNowPlayingWnd: HWND read GetWMPNowPlayingWnd;
    property WMPVersion: Byte read GetWMPVersion;
    property hWMPLoc: HMODULE read GethWMPLoc;
    property IsPlayerDocked: Boolean read GetIsPlayerDocked;

  end;

  TWMPPlugEvents = class(TWMPPlug, IWMPEventsNew)
  private

    fEventsConn: Integer;
    
  protected

    procedure Load; override;
    procedure Release; override;

    procedure EnableEvents;
    procedure DisableEvents;

    procedure OpenStateChange(NewState: Integer); virtual; stdcall;
    procedure PlayStateChange(NewState: TWMPPlayState); virtual; stdcall;
    procedure AudioLanguageChange(LangID: Integer); virtual; stdcall;
    procedure StatusChange; virtual; stdcall;
    procedure ScriptCommand(const scType, Param: WideString); virtual; stdcall;
    procedure NewStream; virtual; stdcall;
    procedure Disconnect(Result: Integer); virtual; stdcall;
    procedure Buffering(Start: WordBool); virtual; stdcall;
    procedure Error; virtual; stdcall;
    procedure Warning(WarningType, Param: Integer;
      const Description: WideString); virtual; stdcall;
    procedure EndOfStream(Result: Integer); virtual; stdcall;
    procedure PositionChange(oldPosition, newPosition: Double); virtual; stdcall;
    procedure MarkerHit(MarkerNum: Integer); virtual; stdcall;
    procedure DurationUnitChange(NewDurationUnit: Integer); virtual; stdcall;
    procedure CdromMediaChange(CdromNum: Integer); virtual; stdcall;
    procedure PlaylistChange(Playlist: IDispatch;
      change: WMPPlaylistChangeEventType); virtual; stdcall;
    procedure CurrentPlaylistChange(change: WMPPlaylistChangeEventType); virtual; stdcall;
    procedure CurrentPlaylistItemAvailable(const bstrItemName: WideString); virtual; stdcall;
    procedure MediaChange(Item: IDispatch); virtual; stdcall;
    procedure CurrentMediaItemAvailable(const bstrItemName: WideString); virtual; stdcall;
    procedure CurrentItemChange(pdispMedia: IDispatch); virtual; stdcall;
    procedure MediaCollectionChange; virtual; stdcall;
    procedure MediaCollectionAttributeStringAdded(const bstrAttribName,
      bstrAttribVal: WideString); virtual; stdcall;
    procedure MediaCollectionAttributeStringRemoved(const bstrAttribName,
      bstrAttribVal: WideString); virtual; stdcall;
    procedure MediaCollectionAttributeStringChanged(const bstrAttribName,
      bstrOldAttribVal, bstrNewAttribVal: WideString); virtual; stdcall;
    procedure PlaylistCollectionChange; virtual; stdcall;
    procedure PlaylistCollectionPlaylistAdded(const bstrPlaylistName: WideString); virtual; stdcall;
    procedure PlaylistCollectionPlaylistRemoved(const bstrPlaylistName: WideString); virtual; stdcall;
    procedure PlaylistCollectionPlaylistSetAsDeleted(const bstrPlaylistName: WideString;
      varfIsDeleted: WordBool); virtual; stdcall;
    procedure ModeChange(const ModeName: WideString;
      NewValue: WordBool); virtual; stdcall;
    procedure MediaError(pMediaObject: IDispatch); virtual; stdcall;
    procedure OpenPlaylistSwitch(pItem: IDispatch); virtual; stdcall;
    procedure DomainChange(const strDomain: WideString); virtual; stdcall;
    procedure SwitchedToPlayerApplication; virtual; stdcall;
    procedure SwitchedToControl; virtual; stdcall;
    procedure PlayerDockedStateChange; virtual; stdcall;
    procedure PlayerReconnect; virtual; stdcall;
    procedure Click(nButton, nShiftState: Smallint;
      fX, fY: Integer); virtual; stdcall;
    procedure DoubleClick(nButton, nShiftState: Smallint;
      fX, fY: Integer); virtual; stdcall;
    procedure KeyDown(nKeyCode, nShiftState: Smallint); virtual; stdcall;
    procedure KeyPress(nKeyAscii: Smallint); virtual; stdcall;
    procedure KeyUp(nKeyCode, nShiftState: Smallint); virtual; stdcall;
    procedure MouseDown(nButton, nShiftState: Smallint;
      fX, fY: Integer); virtual; stdcall;
    procedure MouseMove(nButton, nShiftState: Smallint;
      fX, fY: Integer); virtual; stdcall;
    procedure MouseUp(nButton, nShiftState: Smallint;
      fX, fY: Integer); virtual; stdcall;
  end;

function RegisterWMPPlugin(const CLSID: TGUID;
    const FriendlyName, Description: String; Capabilities: DWORD;
    NotifyWMP: Boolean): HResult;
function UnregisterWMPPlugin(const CLSID: TGUID; NotifyWMP: Boolean): HResult;

implementation

// No need to use RegisterWindowMessage - these messages are sent to our own
// private plug-in window.
const
  WM_MSGBOX = WM_USER + 1874;
  WM_RUNASYNC = WM_USER + 1875;

const
  LoadingExceptionMsg =
      'An unexpected error has occurred while loading %s. ' +
      'Please contact the author if the problem persists.' + CrLf2 +
      '%s';

function GetPlugClassName(PlugName: String): String;
begin
  Result := PChar(PlugName + 'Class');
end;

{ TWMPPlug }

procedure TWMPPlug.Initialize;
begin
  inherited;
  //ReleaseCore;
  fCore := nil;
  fWMPPlayerApp := nil;
  fWMPlayerAppWnd := 0;
  fWMPNowPlayingWnd := 0;
  fWMPVersion := 0;
  fPlugWindow := 0;
  fWndProcStub := CreateStub(Self, @TWMPPlug.WndProc);
  fWMPLocLoaded := False;
  fhWMPLoc := 0;
  PlugName := 'WMP Plugin';
  LoadHighDPIFunc;
end;

destructor TWMPPlug.Destroy;
begin
  if fPlugWindow <> 0 then begin
    DestroyWindow(fPlugWindow);
    UnregisterClass(PChar(GetPlugClassName(PlugName)), hInstance);
    fPlugWindow := 0;
  end;
  DisposeAndNilStub(fWndProcStub);
  ReleaseCore;
  fWMPPlayerApp := nil;
  if fhWMPLoc <> 0 then
    FreeLibrary(fhWMPLoc);
  inherited;
end;

procedure TWMPPlug.Load;
begin
end;

procedure TWMPPlug.Release;
begin
end;

function TWMPPlug.ShouldShowPluginSettings: Boolean;
begin
  Result := True;
end;

function TWMPPlug.ShowPluginSettings(hWndParent: HWND): HResult;
var
  PrevDpiContext: DPI_AWARENESS_CONTEXT;
begin
  PrevDpiContext := 0;
  if ShouldShowPluginSettings then begin
    // WMP creates its Options dialog with DPI_AWARENESS_CONTEXT_UNAWARE, so if
    // our own settings dialog is opened from there, it would inherit the same
    // DPI context by default. This would be incorrect (since we do the scaling
    // ourselves) and would result in an oversized dialog. So we need to
    // explicitly set DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE during creation of
    // our settings dialog (DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 works too,
    // but the former was chosen for compatibility with earlier versions of Win10).
    if Assigned(SetThreadDpiAwarenessContext) then begin
      PrevDpiContext :=
          SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE);
    end;
    try
      DoShowPluginSettings(hWndParent);
    finally
      if Assigned(SetThreadDpiAwarenessContext) and (PrevDpiContext <> 0) then
        SetThreadDpiAwarenessContext(PrevDpiContext);
    end;
  end;
  Result := S_OK;
end;

procedure TWMPPlug.DoShowPluginSettings(hWndParent: HWND);
begin
end;

function TWMPPlug.OnMessage(var Msg: TMessage): Boolean;
begin
  Result := False; // not handled
end;

procedure TWMPPlug.ReleaseCore;
begin
  if Assigned(fCore) then
    Release;
  fCore := nil;
end;

function TWMPPlug.GetWMPlayerAppWnd: HWND;
begin
  if fWMPlayerAppWnd = 0 then
    fWMPlayerAppWnd := GetWMPlayerAppWindow;
  Result := fWMPlayerAppWnd;
end;

function TWMPPlug.GetWMPNowPlayingWnd: HWND;
begin
  if (fWMPNowPlayingWnd = 0)
      // the Now Playing window can get recreated when switching from/to Skin mode
      or not IsWindow(fWMPNowPlayingWnd) then
    fWMPNowPlayingWnd := GetWMPNowPlayingWindow;
  Result := fWMPNowPlayingWnd;
end;

function TWMPPlug.GetModalhWndParent: HWND;
begin
  Result := GetWMPModalhWndParent(GetWMPlayerAppWnd, GetWMPNowPlayingWnd);
end;

function TWMPPlug.GetWMPVersion: Byte;
begin
  if fWMPVersion = 0 then begin
    if Assigned(fCore) then
      fWMPVersion := GetWMPMainVersion(fCore.Get_versionInfo);
  end;
  Result := fWMPVersion;
end;

function TWMPPlug.GethWMPLoc: HMODULE;
begin
  if not fWMPLocLoaded then begin
    fWMPLocLoaded := True;
    fhWMPLoc := LoadResourceDll(WMPLocDll);
  end;
  Result := fhWMPLoc;
end;

function TWMPPlug.GetPlugWindow: Integer;
var
  ClassName: String;
  WndClass: TWndClass;
begin
  if fPlugWindow = 0 then try
    Result := 0;
    ClassName := GetPlugClassName(PlugName);
    with WndClass do begin
      Style := 0;
      lpfnWndProc := fWndProcStub;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hIcon := 0;
      hCursor := 0;
      hbrBackground := 0;
      lpszMenuName := nil;
      lpszClassName := PChar(ClassName);
      hInstance := SysInit.HInstance;
    end;
    if RegisterClass(WndClass) = 0 then begin
      Debug('Failed to register class');
      Exit;
    end;
    fPlugWindow := CreateWindow(PChar(ClassName), nil, 0, 0, 0, 0, 0, 0, 0,
        hInstance, nil);
    if fPlugWindow = 0 then begin
      Debug('Failed to create window');
      Exit;
    end;
  finally
    if fPlugWindow = 0 then
      UnregisterClass(PChar(ClassName), hInstance);
  end;
  Result := fPlugWindow;
end;

function TWMPPlug.WMPMsgBox(AhWndOwner: Integer; const Prompt: String;
    bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
    const Caption: String; Context: Integer): Boolean;
var
  MsgDlg: TPJMessageDialoghWnd;
begin
  // Don't show any dialogs when WMP is invisibly docked by a remote WMP
  // control, e.g. by Media Center.
  if not IsPlayerDocked and (PlugWindow <> 0) then begin
    MsgDlg := TPJMessageDialoghWnd.Create(nil);
    with MsgDlg do begin
      MakeSound := True;
      Text := Prompt;
      ButtonGroup := bgButtons;
      IconKind := miStyle;
      Title := Caption;
      HelpContext := Context;
      hWndOwner := AhWndOwner;
    end;
    PostMessage(PlugWindow, WM_MSGBOX, Integer(MsgDlg), 0);
    Result := True;
  end
  else
    Result := False;
end;

function TWMPPlug.WMPMsgBox(const Prompt: String;
    bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
    const Caption: String; Context: Integer): Boolean;
begin
  // TODO: use GetModalhWndParent instead?
  Result := WMPMsgBox(WMPlayerAppWnd, Prompt, bgButtons, miStyle, Caption,
      Context);
end;

procedure TWMPPlug.CallAsync(const Method: TMethodFunc);
var
  pMethod: ^TMethod;
begin
  if PlugWindow <> 0 then begin
    New(pMethod);
    pMethod^ := TMethod(Method);
    PostMessage(PlugWindow, WM_RUNASYNC, Integer(pMethod), 0);
  end;
end;

function TWMPPlug.GetIsPlayerDocked: Boolean;
var
  CoreExternal: IWMPCoreExternal;
  Player: IUnknown;
  IsDocked: WordBool;
begin
  if not Assigned(fWMPPlayerApp) and Assigned(fCore) then begin
    if Succeeded(fCore.QueryInterface(IWMPCoreExternal, CoreExternal)) then begin
      if Succeeded(CoreExternal.GetPlayerApplicationObject(Player)) then
        Player.QueryInterface(IWMPPlayerApplication, fWMPPlayerApp);
    end;
  end;
  // GetPlayerApplicationObject returns nil when there's no remoted WMP control
  if Assigned(fWMPPlayerApp)
      and Succeeded(fWMPPlayerApp.Get_playerDocked(IsDocked)) then
    Result := IsDocked
  else
    Result := False;
end;

function TWMPPlug.WndProc(hWnd, uMsg, wParam, lParam: Integer): Integer; stdcall;
var
  MsgDlg: TPJMessageDialoghWnd;
  Method: PMethod;
  Msg: TMessage;
begin
  case uMsg of
    WM_MSGBOX: begin
      MsgDlg := TPJMessageDialoghWnd(wParam);
      if Assigned(MsgDlg) then begin
        Result := MsgDlg.Execute;
        FreeAndNil(MsgDlg);
      end
      else
        Result := 0;
    end;
    WM_RUNASYNC: begin
      Method := PMethod(wParam);
      TMethodFunc(Method^); // this line calls the method
      Dispose(Method);
      Result := 0;
    end
    else begin
      FillChar(Msg, SizeOf(Msg), 0);
      Msg.Msg := uMsg;
      Msg.WParam := wParam;
      Msg.LParam := lParam;
      if OnMessage(Msg) then
        Result := Msg.Result
      else
        Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
    end;
  end;
end; 

{ IWMPPluginUI }

function TWMPPlug.CreateProc(hwndParent: HWND;
    out phwndWindow: HWND): HResult; stdcall;
begin 
  Result:= S_OK;
end;

function TWMPPlug.DestroyProc: HResult;
begin
  Result:= S_OK;
end;

function TWMPPlug.DisplayPropertyPage(hwndParent: HWND): HResult; stdcall;
begin
  // WMP will try to show config on first run (PLUGIN_FLAGS_LAUNCHPROPERTYPAGE),
  // even if this run is by a remoted WMP control (e.g. Media Center)
  // so do nothing if we're running remoted.
  if IsPlayerDocked then
    Result := S_OK
  else
    Result := ShowPluginSettings(hwndParent);
end;

function TWMPPlug.GetProperty(pwszName: PWideChar;
    out pvarProperty: OleVariant): HResult;
begin 
  try
    if pwszName = PLUGIN_MISC_QUERYDESTROY then begin
      pvarProperty := '';
      Result := S_OK;
    end
    else
      Result := E_NOTIMPL;
  except
    Result := E_FAIL;
  end;
end;

function TWMPPlug.SetCore(const pCore: IWMPCore): HResult;
var
  ExceptionStr: String;
begin
  ReleaseCore;
  try
    fCore := pCore as IWMPCoreSafe;
    if Assigned(fCore) then
      Load;
    Result := S_OK;
  except
    ExceptionStr := ExceptionErrorMessage(ExceptObject, ExceptAddr);
    WMPMsgBox(GetModalhWndParent,
        Format(LoadingExceptionMsg, [PlugName, ExceptionStr]),
        bgOK, miError, PlugName, 0);
    Result := E_FAIL;
  end;
end; 

function TWMPPlug.SetProperty(pwszName: PWideChar;
    var pvarProperty: OleVariant): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWMPPlug.TranslateAccelerator(var lpMsg: tagMSG): HResult;
begin
  Result := S_FALSE;
end;

{ TWMPPlugEvents }

procedure TWMPPlugEvents.Load;
begin
  inherited;
  fEventsConn := 0;
  EnableEvents;
end;

procedure TWMPPlugEvents.Release;
begin
  inherited;
  DisableEvents;
end;

procedure TWMPPlugEvents.EnableEvents;
begin
  if Assigned(Core) then
    InterfaceConnect(Core, IWMPEvents, Self, fEventsConn);
end;

procedure TWMPPlugEvents.DisableEvents;
begin
  if Assigned(Core) and (fEventsConn <> 0) then begin
    InterfaceDisconnect(Core, IWMPEvents, fEventsConn);
    fEventsConn := 0;
  end;
end;

procedure TWMPPlugEvents.OpenStateChange(NewState: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.PlayStateChange(NewState: TWMPPlayState); stdcall;
begin
end;

procedure TWMPPlugEvents.AudioLanguageChange(LangID: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.StatusChange; stdcall;
begin
end;

procedure TWMPPlugEvents.ScriptCommand(const scType, Param: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.NewStream; stdcall;
begin
end;

procedure TWMPPlugEvents.Disconnect(Result: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.Buffering(Start: WordBool); stdcall;
begin
end;

procedure TWMPPlugEvents.Error; stdcall;
begin
end;

procedure TWMPPlugEvents.Warning(WarningType, Param: Integer;
    const Description: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.EndOfStream(Result: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.PositionChange(oldPosition, newPosition: Double); stdcall;
begin
end;

procedure TWMPPlugEvents.MarkerHit(MarkerNum: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.DurationUnitChange(NewDurationUnit: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.CdromMediaChange(CdromNum: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.PlaylistChange(Playlist: IDispatch;
    change: WMPPlaylistChangeEventType); stdcall;
begin
end;

procedure TWMPPlugEvents.CurrentPlaylistChange(change: WMPPlaylistChangeEventType); stdcall;
begin
end;

procedure TWMPPlugEvents.CurrentPlaylistItemAvailable(const bstrItemName: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.MediaChange(Item: IDispatch); stdcall;
begin
end;

procedure TWMPPlugEvents.CurrentMediaItemAvailable(const bstrItemName: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.CurrentItemChange(pdispMedia: IDispatch); stdcall;
begin
end;

procedure TWMPPlugEvents.MediaCollectionChange; stdcall;
begin
end;

procedure TWMPPlugEvents.MediaCollectionAttributeStringAdded(const bstrAttribName,
    bstrAttribVal: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.MediaCollectionAttributeStringRemoved(const bstrAttribName,
    bstrAttribVal: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.MediaCollectionAttributeStringChanged(const bstrAttribName,
    bstrOldAttribVal, bstrNewAttribVal: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.PlaylistCollectionChange; stdcall;
begin
end;

procedure TWMPPlugEvents.PlaylistCollectionPlaylistAdded(const bstrPlaylistName: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.PlaylistCollectionPlaylistRemoved(const bstrPlaylistName: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.PlaylistCollectionPlaylistSetAsDeleted(const bstrPlaylistName: WideString;
    varfIsDeleted: WordBool); stdcall;
begin
end;

procedure TWMPPlugEvents.ModeChange(const ModeName: WideString; NewValue: WordBool); stdcall;
begin
end;

procedure TWMPPlugEvents.MediaError(pMediaObject: IDispatch); stdcall;
begin
end;

procedure TWMPPlugEvents.OpenPlaylistSwitch(pItem: IDispatch); stdcall;
begin
end;

procedure TWMPPlugEvents.DomainChange(const strDomain: WideString); stdcall;
begin
end;

procedure TWMPPlugEvents.SwitchedToPlayerApplication; stdcall;
begin
end;

procedure TWMPPlugEvents.SwitchedToControl; stdcall;
begin
end;

procedure TWMPPlugEvents.PlayerDockedStateChange; stdcall;
begin
end;

procedure TWMPPlugEvents.PlayerReconnect; stdcall;
begin
end;

procedure TWMPPlugEvents.Click(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.DoubleClick(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.KeyDown(nKeyCode, nShiftState: Smallint); stdcall;
begin
end;

procedure TWMPPlugEvents.KeyPress(nKeyAscii: Smallint); stdcall;
begin
end;

procedure TWMPPlugEvents.KeyUp(nKeyCode, nShiftState: Smallint); stdcall;
begin
end;

procedure TWMPPlugEvents.MouseDown(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.MouseMove(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPPlugEvents.MouseUp(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

function RegisterWMPPlugin(const CLSID: TGUID;
    const FriendlyName, Description: String; Capabilities: DWORD;
    NotifyWMP: Boolean): HResult;
var
   Key: HKEY;
   Res: DWORD;
begin
  Res := RegCreateKeyEx(HKEY_LOCAL_MACHINE,
      PChar(PLUGIN_INSTALLREGKEY + '\' + GUIDToString(CLSID)), 0, nil, 0,
      KEY_WRITE, nil, Key, nil);
  if Res = ERROR_SUCCESS then try
    RegSetValueEx(Key, PLUGIN_INSTALLREGKEY_FRIENDLYNAME, 0, REG_SZ,
        PChar(FriendlyName), Length(FriendlyName) + 1);
    RegSetValueEx(Key, PLUGIN_INSTALLREGKEY_DESCRIPTION, 0, REG_SZ,
        PChar(Description), Length(Description) + 1);
    RegSetValueEx(Key, PLUGIN_INSTALLREGKEY_CAPABILITIES, 0, REG_DWORD,
        @Capabilities, SizeOf(Capabilities));
    Result := ComServ.DllRegisterServer;
    if NotifyWMP and Succeeded(Result) then
      WMPNotifyPluginAddRemove;
  finally
    RegCloseKey(Key);
  end
  else begin
    MessageBox(GetDesktopWindow, PChar(SysErrorMessage(Res)), 'Error',
        MB_OK or MB_ICONERROR);
    Result := E_FAIL;
  end;
end;

function UnregisterWMPPlugin(const CLSID: TGUID; NotifyWMP: Boolean): HResult;
begin
  if RegDeleteKey(HKEY_LOCAL_MACHINE,
      PChar(PLUGIN_INSTALLREGKEY + '\' + GUIDToString(CLSID))) =
      ERROR_SUCCESS then begin
    Result := ComServ.DllUnregisterServer;
    if NotifyWMP and Succeeded(Result) then
      WMPNotifyPluginAddRemove;
  end
  else
    Result := E_FAIL;
end;

end.
 