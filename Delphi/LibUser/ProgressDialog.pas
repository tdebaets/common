(****************************************************************************
 *
 * Originally based (with permission) on a sample provided by Eric
 * Lawrence (@ericlaw)
 * Modifications are Copyright 2016 Tim De Baets
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
 * VCL wrapper for IProgressDialog interface
 *
 ****************************************************************************)

unit ProgressDialog;

interface

uses Windows, Messages, SysUtils, ActiveX, Classes, Graphics, Controls,
    OleCtrls, ShellApi, CmnFunc2, Forms, Common2, CtrlsCommon;

const
  LIBID_ProgressDialog: TGUID = '{2F2719A2-83CC-11D3-A08C-0040F6A4BFEC}';
  IID_IProgressDialog: TGUID = '{EBBC7C04-315E-11D2-B62F-006097DF5BD4}';
  CLASS_ProgressDialog: TGUID = '{F8383852-FCD3-11D1-A6B9-006097DF5BD4}';

type
  TCommonAVI = (aviNone, aviFindFolder, aviFindFile, aviFindComputer,
      aviCopyFiles, aviCopyFile, aviRecycleFile, aviEmptyRecycle, aviDeleteFile);

// Constants for enum PROGDLG_FLAGS
type
  PROGDLG_FLAGS = TOleEnum;
const
  PROGDLG_NORMAL = $00000000;
  PROGDLG_MODAL = $00000001;
  PROGDLG_AUTOTIME = $00000002;
  PROGDLG_NOTIME = $00000004;
  PROGDLG_NOMINIMIZE = $00000008;
  PROGDLG_NOPROGRESSBAR = $00000010;

// Constants for enum PDTIMER_FLAGS
type
  PDTIMER_FLAGS = TOleEnum;

Const
  PDTIMER_RESET = $00000001;

type
  IProgressDialog = interface;

// Interface: IProgressDialog
  IProgressDialog = interface(IUnknown)
    ['{EBBC7C04-315E-11D2-B62F-006097DF5BD4}']
    function StartProgressDialog(hwndParent: Integer;
        const punkEnableModless: IUnknown; dwFlags: PROGDLG_FLAGS;
        pvResevered: Pointer): HResult; stdcall;
    function StopProgressDialog: HResult; stdcall;
    function SetTitle(pwzTitle: PWideChar): HResult; stdcall;
    function SetAnimation(hInstAnimation: Integer;
        idAnimation: Integer): HResult; stdcall;
    function HasUserCancelled: BOOL; stdcall;
    function SetProgress(dwCompleted: Integer;
        dwTotal: Integer): HResult; stdcall;
    function SetProgress64(ullCompleted: Currency;
        ullTotal: Currency): HResult; stdcall;
    function SetLine(dwLineNum: Integer; pwzString: PWideChar;
        fCompactPath: BOOL; pvResevered: Pointer): HResult; stdcall;
    function SetCancelMsg(pwzCancelMsg: PWideChar;
        pvResevered: Pointer): HResult; stdcall;
    function Timer(dwTimerAction: PDTIMER_FLAGS;
        pvResevered: Pointer): HResult; stdcall;
  end;

type
  TProgressDialog = class(TComponent)
  private
    fShellModule: HMODULE;
    fTitle: WideString;
    fCommonAVI: TCommonAVI;
    fLine1: WideString;
    fLine2: WideString;
    fFooter: WideString;
    fCancelMsg: WideString;
    fMax: Integer;
    fPosition: Integer;
    fhWndParent: HWND;
    fProghWnd: HWND;
    fModal: Boolean;
    fAutoCalcFooter: Boolean;
    fCompactLine1, fCompactLine2, fCompactFooter: Boolean;
    fProgressDialog: IProgressDialog;
    //fWasDisabled: Boolean;
    function GetShellModule: HMODULE;
  protected
    function GetCancelled: Boolean;
    procedure SetTitle(Value: WideString);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetCancel(Value: WideString);

    procedure DisableSystemMenuItems;
    procedure HideFromTaskbar;
    procedure HideAnimationControl;
  public
    procedure SetLine1(Value: WideString; Compact: Boolean = False);
    procedure SetLine2(Value: WideString; Compact: Boolean = False);
    procedure SetFooter(Value: WideString; Compact: Boolean = False);
    constructor Create(AOwner: TComponent); override;
    procedure Execute(hWndParent: HWND); overload;
    procedure Execute; overload;
    procedure Stop;
    procedure Step;
    destructor Destroy; override;
  published
    property Cancelled: Boolean read GetCancelled;
    property Title: WideString read fTitle write SetTitle;
    property CommonAVI: TCommonAVI read FCommonAVI write FCommonAVI default aviNone;
    property Max: Integer read fMax write SetMax default 100;
    property Position: Integer read fPosition write SetPosition;
    property Modal: Boolean read fModal write fModal default False;
    property AutoCalcFooter: Boolean read fAutoCalcFooter write fAutoCalcFooter;
    property TextCancel: WideString read fCancelMsg write SetCancel;
  end;

procedure Register;

implementation

uses ComObj, CommCtrl, ClassCallback, MonitorFunc;

const
  CommonAVIId: array[TCommonAVI] of Integer =
      (0, 150, 151, 152, 160, 161, 162, 163, 164);

const
  PROP_PREVWNDPROC: PChar = '{9904DEBE-CA17-434B-B464-7769A79DD3B0}';
  PROP_PROGRESSDIALOG: PChar = '{CF1BBBC5-7126-457b-BD89-F65497F8C832}';

var
  IsWin2000OrLower: Boolean;

constructor TProgressDialog.Create(AOwner: TComponent);
begin
  if Assigned(AOwner) and not AOwner.InheritsFrom(TWinControl) then begin
    raise Exception.CreateFmt('Error: Component must be owned by TForm or ' +
        'descendent of TForm %s', [AOwner.Classname]);
  end;
  inherited Create(AOwner);
  fhWndParent := 0;
  fProghWnd := 0;
  fCompactLine1 := False;
  fCompactLine2 := False;
  fCompactFooter := False;
end;

destructor TProgressDialog.Destroy;
begin
  Stop;
  if fProghWnd <> 0 then
    RemoveProp(fProghWnd, PROP_PROGRESSDIALOG);
  inherited Destroy;
end;

function TProgressDialog.GetShellModule: HMODULE;
begin
  if fShellModule = 0 then begin
    fShellModule := SafeLoadLibrary(shell32, SEM_NOOPENFILEERRORBOX);
    if fShellModule <= HINSTANCE_ERROR then
      fShellModule := 0;
  end;
  Result := fShellModule;
end;

procedure TProgressDialog.Stop;
var
  ThreadId: Integer;
begin
  if Assigned(fProgressDialog) then begin
    if fModal and (fhWndParent <> 0) {and not fWasDisabled} then
      EnableWindow(fhWndParent, True); // prevent fhWndParent from losing focus
    // prevent the user from seeing the cancel msg on normal stop (see SendMessage below)
    fProgressDialog.SetCancelMsg('', nil);
    if Succeeded(fProgressDialog.StopProgressDialog) then begin
      fProgressDialog := nil;
      ProcessAppMessages;
      if fProghWnd <> 0 then begin
        if IsWin2000OrLower then begin
          // on XP, the dialog sometimes doesn't disappear when clicking Cancel
          // don't use this on Vista or higher, as it causes too many other issues there
          ThreadId := GetWindowThreadProcessId(fProghWnd, nil);
          if ThreadId <> 0 then
            PostThreadMessage(ThreadId, WM_QUIT, 0, 0);
        end;
        // stop here if ProcessAppMessages encountered a WM_QUIT message
        if Application.Terminated then
          Exit;
        // prevent delay when interrupting the progress (especially on Vista)
        SendMessage(fProghWnd, WM_COMMAND, IDCANCEL, 0);
        while IsWindow(fProghWnd) do begin
          if not HandleAppMessage then
            Break;
        end;
      end;
    end;
  end;
end;

function TProgressDialog.GetCancelled: Boolean;
begin
  if Assigned(fProgressDialog) then begin
    Application.ProcessMessages;
    Result := fProgressDialog.HasUserCancelled;
  end
  else
    Result := True;
end;

procedure TProgressDialog.SetTitle(Value: WideString);
begin
  fTitle := Value;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetTitle(PWideChar(fTitle));
    Application.ProcessMessages;
  end;
end;

procedure TProgressDialog.SetLine1(Value: WideString; Compact: Boolean = False);
begin
  fLine1 := Value;
  fCompactLine1 := Compact;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetLine(1, PWideChar(fLine1), Compact, nil);
    Application.ProcessMessages;
  end;
end;

procedure TProgressDialog.SetLine2(Value: WideString; Compact: Boolean = False);
begin
  fLine2 := Value;
  fCompactLine2 := Compact;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetLine(2, PWideChar(fLine2), Compact, nil);
    Application.ProcessMessages;
  end;
end;

procedure TProgressDialog.SetFooter(Value: WideString; Compact: Boolean = False);
begin
  fFooter := Value;
  fCompactFooter := Compact;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetLine(3, PWideChar(fFooter), Compact, nil);
    Application.ProcessMessages;
  end;
end;

procedure TProgressDialog.SetCancel(Value: WideString);
begin
  fCancelMsg := Value;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetCancelMsg(PWideChar(fCancelMsg), nil);
    Application.ProcessMessages;
  end;
end;

function DialogWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM;
    lParam: LPARAM): LRESULT; stdcall;
var
  PrevWndProc: Pointer;
  hOwner: Integer;
  ProgDialog: TProgressDialog;
begin
  PrevWndProc := Pointer(GetProp(hWnd, PROP_PREVWNDPROC));
  Result := CallWindowProc(PrevWndProc, hWnd, Msg, wParam, lParam);
  if ((Msg = WM_SHOWWINDOW) and not LongBool(WParam))
      or ((Msg = WM_WINDOWPOSCHANGING)
      and (PWindowPos(lParam).flags and SWP_HIDEWINDOW <> 0)) then begin
    hOwner := GetWindow(hWnd, GW_OWNER);
    if (hOwner <> 0) and not WindowOwnsVisibleWindows(hOwner, hWnd) then
      EnableWindow(hOwner, TRUE);
  end
  else if (Msg = WM_COMMAND) and (wParam = IDCANCEL) and IsWin2000OrLower then begin
    ProgDialog := TProgressDialog(GetProp(hWnd, PROP_PROGRESSDIALOG));
    if Assigned(ProgDialog) and Assigned(ProgDialog.fProgressDialog) then
      ProgDialog.SetLine1(ProgDialog.fCancelMsg);
  end;
end;

procedure TProgressDialog.Execute(hWndParent: HWND);
var
  dwFlags: Integer;
  OleWindow: IOleWindow;
  PrevProghWnd: HWND;
  PrevWndProc: Pointer;
begin
  fhWndParent := hWndParent;
  dwFlags := PROGDLG_NOMINIMIZE;
  if fAutoCalcFooter then
    dwFlags := dwFlags or PROGDLG_AUTOTIME;
  if fModal then
    dwFlags := dwFlags or PROGDLG_MODAL;
//  if fModal then
//    fWasDisabled := EnableWindow(fhWndParent, False)
//  else
//    fWasDisabled := False;
  fProgressDialog := CreateComObject(CLASS_ProgressDialog) as IProgressDialog;
  with fProgressDialog do begin
    SetTitle(PWideChar(fTitle));
    if fCommonAVI <> aviNone then
      SetAnimation(GetShellModule, CommonAVIId[fCommonAVI]);
    SetLine(1, PWideChar(fLine1), fCompactLine1, nil);
    SetLine(2, PWideChar(fLine2), fCompactLine2, nil);
    if not fAutoCalcFooter then
      SetLine(3, PWideChar(fFooter), fCompactFooter, nil);
    SetCancelMsg(PWideChar(fCancelMsg), nil);
    fProghWnd := 0;
    StartProgressDialog(hWndParent, nil, dwFlags, nil);
  end;
  if Succeeded(fProgressDialog.QueryInterface(IOleWindow, OleWindow)) then begin
    PrevProghWnd := fProghWnd;
    if Succeeded(OleWindow.GetWindow(fProghWnd)) then begin
      if fProghWnd <> PrevProghWnd then begin
        PrevWndProc := Pointer(GetWindowLong(fProghWnd, GWL_WNDPROC));
        SetProp(fProghWnd, PROP_PREVWNDPROC, Cardinal(PrevWndProc));
        if IsWin2000OrLower then
          SetProp(fProghWnd, PROP_PROGRESSDIALOG, Cardinal(Self));
        SetWindowLong(fProghWnd, GWL_WNDPROC, Integer(@DialogWndProc));
      end;
      DisableSystemMenuItems;
      HideFromTaskbar;
      if fCommonAvi = aviNone then
        HideAnimationControl;
      CenterWindow(fProghWnd, hWndParent);
    end;
  end;
  Application.ProcessMessages;
end;

procedure TProgressDialog.Execute;
begin
  Execute((Owner as TWinControl).Handle);
end;

procedure TProgressDialog.SetPosition(Value: Integer);
begin
  fPosition := Value;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetProgress(fPosition, fMax);
    Application.ProcessMessages;
  end;
end;

procedure TProgressDialog.SetMax(Value: Integer);
Begin
  fMax := Value;
  if Assigned(fProgressDialog) then begin
    fProgressDialog.SetProgress(fPosition, fMax);
    Application.ProcessMessages;
  end;
End;

procedure TProgressDialog.Step;
begin
  SetPosition(fPosition + 1);
end;

procedure TProgressDialog.DisableSystemMenuItems;
var
  hSysMenu: HMENU;
begin
  if fProghWnd <> 0 then begin
    hSysMenu := GetSystemMenu(fProghWnd, False);
    if hSysMenu <> 0 then begin
      EnableMenuItem(hSysMenu, SC_MINIMIZE,
          MF_BYCOMMAND or MF_DISABLED or MF_GRAYED);
      EnableMenuItem(hSysMenu, SC_MAXIMIZE,
          MF_BYCOMMAND or MF_DISABLED or MF_GRAYED);
      EnableMenuItem(hSysMenu, SC_SIZE,
          MF_BYCOMMAND or MF_DISABLED or MF_GRAYED);
      EnableMenuItem(hSysMenu, SC_RESTORE,
          MF_BYCOMMAND or MF_DISABLED or MF_GRAYED);
    end;
  end;
end;

procedure TProgressDialog.HideFromTaskbar;
begin
  if fProghWnd <> 0 then begin
    // make sure the dialog isn't shown in the taskbar (Vista/7)
    SetWindowLong(fProghWnd, GWL_EXSTYLE,
        GetWindowLong(fProghWnd, GWL_EXSTYLE) and not WS_EX_APPWINDOW);
  end;
end;

type
  TMoveChildWindowsInfo = record
    hParent: HWND;
    hExclude: HWND;
    OffsetX, OffsetY: Integer;
  end;
  PMoveChildWindowsInfo = ^TMoveChildWindowsInfo;

procedure TProgressDialog.HideAnimationControl;
  function MoveChildWindowsProc(hWnd: HWND;
      Info: PMoveChildWindowsInfo): BOOL; stdcall;
  var
    Rect: TRect;
  begin
    if (hWnd <> Info.hExclude) and (GetParent(hWnd) = Info.hParent) then begin
      if GetWindowRect(hWnd, Rect)
          and Windows.ScreenToClient(Info.hParent, Rect.TopLeft) then begin
        SetWindowPos(hWnd, 0, Rect.Left + Info.OffsetX, Rect.Top + Info.OffsetY,
            0, 0,
            SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_NOZORDER);
      end;
    end;
    Result := True;
  end;
var
  hAnimate: HWND;
  AnimateRect, DialogRect: TRect;
  MoveChildInfo: TMoveChildWindowsInfo;
begin
  if fProghWnd <> 0 then begin
    hAnimate := FindWindowEx(fProghWnd, 0, ANIMATE_CLASS, nil);
    if hAnimate <> 0 then begin
      ShowWindow(hAnimate, SW_HIDE);
      if GetWindowRect(hAnimate, AnimateRect)
          and Windows.ScreenToClient(fProghWnd, AnimateRect.BottomRight) then begin
        with MoveChildInfo do begin
          hParent := fProghWnd;
          hExclude := hAnimate;
          OffsetX := 0;
          OffsetY := -AnimateRect.Bottom;
        end;
        EnumChildWindows(fProghWnd, @MoveChildWindowsProc,
            Integer(@MoveChildInfo));
      end;
      if GetWindowRect(fProghWnd, DialogRect) then with DialogRect do begin
        SetWindowPos(fProghWnd, 0, 0, 0,
            Right - Left, Bottom - Top - AnimateRect.Bottom,
            SWP_NOOWNERZORDER or SWP_NOMOVE or SWP_NOZORDER);
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('System', [TProgressDialog]);
end;

initialization
  IsWin2000OrLower := IsWinNT and (Win32MajorVersion <= 5);

end.
