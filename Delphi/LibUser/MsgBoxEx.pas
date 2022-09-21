(****************************************************************************
 *
 * Copyright 2022 Tim De Baets
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
 * TMsgBoxEx and TMsgBoxExVista components
 *
 ****************************************************************************)

unit MsgBoxEx;

interface

uses Windows, Messages, Classes, ComObj, NewDialogs, Common2, NewCommCtrl,
    TaskDialog;

type
  TOnClickLink = procedure(Sender: TObject; hWnd: HWND;
      const URL: WideString) of object;

type
  TMsgBoxEx = class(TPJMessageDialoghWnd)
  private
    fpHookProcStub: Pointer;
    fHook: HHOOK;
    fShowLink: Boolean;
    fLinkAvailable: Boolean;
    fLinkhWnd: HWND;
    fLinkCaption: WideString;
    fOnClickLink: TOnClickLink;
    fShowCheck: Boolean;
    fChecked: Boolean;
    fCheckhWnd: HWND;
    fCheckCaption: WideString;
    procedure OnDialogInit(hWnd: HWND);
    procedure OnDialogDestroy(hWnd: HWND);
    procedure OnDialogNotify(hWnd: HWND; pNMHdr: PNMHdr);
    function IsOurDialog(hWnd: HWND): Boolean;
  protected
    function GetCheckCaption: WideString;
    function MsgBoxHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): Integer; stdcall;
    procedure ClickLink(hWnd: HWND; const URL: WideString);
    property HelpContext; // used internally, so hide it from the rest
    function DoMessageBoxIndirect(var Params: TMsgBoxParams): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Integer; override;
    property ShowLink: Boolean read fShowLink write fShowLink;
    property LinkCaption: WideString read fLinkCaption write fLinkCaption;
    property OnClickLink: TOnClickLink read fOnClickLink write fOnClickLink;
    property ShowCheck: Boolean read fShowCheck write fShowCheck;
    property CheckCaption: WideString read fCheckCaption write fCheckCaption;
    property Checked: Boolean read fChecked write fChecked;
  end;

  TMsgBoxExVista = class(TMsgBoxEx)
  private
    fhComCtl32: HMODULE;
    fTaskDialogIndirect: TTaskDialogIndirect;
    fpCallbackStub: Pointer;
  protected
    function TaskDialogCallback(hWnd: HWND; msg: UINT; wParam: WPARAM;
        lParam: LPARAM; lpRefData: Pointer): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Integer; override;
  end;

function ChkMsgBox(const Prompt: String; bgButtons: TPJMsgDlgButtonGroup;
    miStyle: TPJMsgDlgIconKind; const Caption: String; const ChkCaption: String;
    var AChecked: Boolean): Integer;
function ChkMsgBoxhWnd(AhWndOwner: HWND; const Prompt: String;
    bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
    const Caption: String; const ChkCaption: String;
    var AChecked: Boolean): Integer;

implementation

uses ClassCallback;

const
  MsgBoxCommandIds: array[0..10] of Integer = (
    // as defined in WinUser.h
    IDOK,
    IDCANCEL,
    IDABORT,
    IDRETRY,
    IDIGNORE,
    IDYES,
    IDNO,
    IDCLOSE,
    IDHELP,
    IDTRYAGAIN,
    IDCONTINUE
  );
  IDTEXT = $FFFF;

const
  DefaultCaption = '&Don''t display this message again';

{ TMsgBoxEx }

constructor TMsgBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  fShowCheck := True;
  fpHookProcStub := CreateStub(Self, @TMsgBoxEx.MsgBoxHook);
  fLinkAvailable := CheckCommonControl(ICC_LINK_CLASS);
  fShowLink := False;
end;

destructor TMsgBoxEx.Destroy;
begin
  DisposeAndNilStub(fpHookProcStub);
  inherited;
end;

function TMsgBoxEx.IsOurDialog(hWnd: HWND): Boolean;
var
  pParams: PMsgBoxParams;
begin
  Result := (PChar(GetClassLong(hWnd, GCW_ATOM)) = WC_DIALOG);
  if Result then begin
    pParams := PMsgBoxParams(GetWindowLong(hWnd, GWL_USERDATA));
    Result := Assigned(pParams) and (pParams.dwContextHelpId = DWORD(Self));
  end;
end;

procedure TMsgBoxEx.OnDialogInit(hWnd: HWND);
const
  ButtonClassName = 'BUTTON';

var
  ScaleBaseUnitX, ScaleBaseUnitY: Integer;

  function ScaleX(X: Integer): Integer;
  begin
    Result := MulDiv(X, ScaleBaseUnitX, OrigBaseUnitX);
  end;
  function ScaleY(Y: Integer): Integer;
  begin
    Result := MulDiv(Y, ScaleBaseUnitY, OrigBaseUnitY);
  end;

var
  hStatic: Integer;
  StaticRect: TRect;
  StaticWidth: Integer;
  hFont: Integer;
  i: Integer;
  hButtons: array[0..Length(MsgBoxCommandIds) - 1] of Integer;
  ButtonFound: Boolean;
  hButton: Integer;
  ButtonRect: TRect;
  DialogRect: TRect;
  ButtonY, ShiftY, MarginY: Integer;
  LinkHeight: Integer;
begin
  ScaleBaseUnitX := 0;
  ScaleBaseUnitY := 0;
  if not IsOurDialog(hWnd) then
    Exit;
  FillChar(hButtons, SizeOf(hButtons), 0);
  hStatic := GetDlgItem(hWnd, IDTEXT);
  if hStatic = 0 then
    Exit;

  ButtonFound := False;
  ButtonY := 0;
  for i := 0 to Length(MsgBoxCommandIds) - 1 do begin
    hButton := GetDlgItem(hWnd, MsgBoxCommandIds[i]);
    hButtons[i] := hButton;
    if (hButton <> 0) and not ButtonFound then begin
      ButtonFound := True;
      GetWindowRect(hButton, ButtonRect);
      ScreenToClient(hWnd, ButtonRect.TopLeft);
      ButtonY := ButtonRect.Top;
    end;
  end;
  if not ButtonFound then
    Exit;

  hFont := SendMessage(hStatic, WM_GETFONT, 0, 0);
  CalculateBaseUnitsFromFontHandle(hFont, ScaleBaseUnitX, ScaleBaseUnitY);
  GetWindowRect(hStatic, StaticRect);
  ScreenToClient(hWnd, StaticRect.TopLeft);
  ScreenToClient(hWnd, StaticRect.BottomRight);
  StaticWidth := StaticRect.Right - StaticRect.Left;
  MarginY := ButtonY - StaticRect.Bottom;
  ShiftY := StaticRect.Bottom;

  if fLinkAvailable and fShowLink then begin
    Inc(MarginY, ScaleY(16));
    Inc(ShiftY, ScaleY(5));
    fLinkhWnd := CreateWindowEx(0, WC_LINK, PChar(String(fLinkCaption)),
        WS_CHILD or WS_TABSTOP,
        StaticRect.Left, ShiftY, StaticWidth, ScaleY(15), hWnd, 0, hInstance, nil);
    if fLinkhWnd = 0 then
      Exit;
    LinkHeight := SendMessage(fLinkhWnd, LM_GETIDEALHEIGHT, 0, 0);
    if LinkHeight > 0 then begin
      SetWindowPos(fLinkhWnd, 0, 0, 0, StaticWidth, LinkHeight,
          SWP_NOMOVE or SWP_NOZORDER);
    end;
    Inc(ShiftY, ScaleY(15));
    SendMessage(fLinkhWnd, WM_SETFONT, hFont, 0);
    ShowWindow(fLinkhWnd, SW_SHOWNORMAL);
  end;

  if fShowCheck then begin
    if fShowLink then
      Inc(ShiftY, ScaleY(10))
    else
      Inc(ShiftY, ScaleY(13));
    Inc(MarginY, ScaleY(23));
    fCheckhWnd := CreateWindowEx(0, ButtonClassName, PChar(String(GetCheckCaption)),
        WS_CHILD or WS_TABSTOP or BS_AUTOCHECKBOX,
        StaticRect.Left, ShiftY, staticWidth, ScaleY(15), hWnd, 0, hInstance, nil);
    if fCheckhWnd = 0 then
      Exit;
    SendMessage(fCheckhWnd, WM_SETFONT, hFont, 0);
    SendMessage(fCheckhWnd, BM_SETCHECK, Integer(fChecked), 0);
    ShowWindow(fCheckhWnd, SW_SHOWNORMAL);
  end;

  ButtonY := StaticRect.Bottom + MarginY;
  for i := 0 to Length(hButtons) - 1 do begin
    if hButtons[i] <> 0 then begin
      GetWindowRect(hButtons[i], ButtonRect);
      ScreenToClient(hWnd, ButtonRect.TopLeft);
      SetWindowPos(hButtons[i], 0, ButtonRect.Left, ButtonY, 0, 0,
          SWP_NOSIZE or SWP_NOZORDER);
    end;
  end;
  
  GetWindowRect(hWnd, DialogRect);
  SetWindowPos(hWnd, 0, 0, 0,
        DialogRect.Right - DialogRect.Left,
        (DialogRect.Bottom - DialogRect.Top) + (ButtonY - ButtonRect.Top),
        SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TMsgBoxEx.OnDialogDestroy(hWnd: HWND);
begin
  if IsOurDialog(hWnd) then begin
    fChecked := (SendMessage(fCheckhWnd, BM_GETCHECK, 0, 0) = BST_CHECKED);
    fCheckhWnd := 0;
    fLinkhWnd := 0;
  end;
end;

procedure TMsgBoxEx.OnDialogNotify(hWnd: HWND; pNMHdr: PNMHdr);
var
  pLink: PNMLink;
begin
  if (pNMHdr.hwndFrom = fLinkhWnd)
      and ((pNMHdr.code = NM_CLICK) or (pNMHdr.code = NM_RETURN)) then begin
    pLink := PNMLink(pNMHdr);
    ClickLink(hWnd, pLink.item.szUrl);
  end;
end;

function TMsgBoxEx.MsgBoxHook(nCode: Integer; wParam: WPARAM;
    lParam: LPARAM): Integer; stdcall;
var
  pCWPRet: PCWPRetStruct;
begin
  Result := CallNextHookEx(fHook, nCode, wParam, lParam);
  if nCode >= 0 then begin
    pCWPRet := PCWPRetStruct(lParam);
    case pCWPRet.message of
      WM_INITDIALOG:
        OnDialogInit(pCWPRet.hwnd);
      WM_DESTROY:
        OnDialogDestroy(pCWPRet.hwnd);
      WM_NOTIFY:
        OnDialogNotify(pCWPRet.hwnd, PNMHdr(pCWPRet.lParam))
    end;
  end;
end;

function TMsgBoxEx.DoMessageBoxIndirect(var Params: TMsgBoxParams): Integer;
begin
  // MB_HELP and lpfnMsgBoxCallback are set by TPJMessageDialog when HelpContext
  // is nonzero, unset them again
  Params.dwStyle := Params.dwStyle and not MB_HELP;
  Params.lpfnMsgBoxCallback := nil;
  Result := inherited DoMessageBoxIndirect(Params);
end;

function TMsgBoxEx.Execute: Integer;
begin
  fHook := SetWindowsHookEx(WH_CALLWNDPROCRET, fpHookProcStub, 0,
      GetCurrentThreadId);
  try
    HelpContext := Integer(Self); // use context ID to uniquely identify this msgbox
    Result := inherited Execute;
    HelpContext := 0;
  finally
    UnhookWindowsHookEx(fHook);
    fHook := 0;
  end;
end;

function TMsgBoxEx.GetCheckCaption: WideString;
begin
  if fCheckCaption = '' then
    Result := DefaultCaption
  else
    Result := fCheckCaption;
end;

procedure TMsgBoxEx.ClickLink(hWnd: HWND; const URL: WideString);
begin
  if Assigned(fOnClickLink) then
    fOnClickLink(Self, hWnd, URL);
end;

function ChkMsgBox(const Prompt: String; bgButtons: TPJMsgDlgButtonGroup;
    miStyle: TPJMsgDlgIconKind; const Caption: String; const ChkCaption: String;
    var AChecked: Boolean): Integer;
begin
  Result := ChkMsgBoxHwnd(0, Prompt, bgButtons, miStyle, Caption, ChkCaption,
      AChecked);
end;

function ChkMsgBoxhWnd(AhWndOwner: HWND; const Prompt: String;
    bgButtons: TPJMsgDlgButtonGroup; miStyle: TPJMsgDlgIconKind;
    const Caption: String; const ChkCaption: String; var AChecked: Boolean): Integer;
begin
  with TMsgBoxEx.Create(nil) do try
    MakeSound := True;
    Text := Prompt;
    ButtonGroup := bgButtons;
    IconKind := miStyle;
    Title := Caption;
    hWndOwner := AhWndOwner;
    CheckCaption := ChkCaption;
    Checked := AChecked;
    Result := Execute;
    AChecked := Checked;
  finally
    Free;
  end;
end;

{ TMsgBoxExVista }

constructor TMsgBoxExVista.Create(AOwner: TComponent);
begin
  inherited;
  fhComCtl32 := SafeLoadLibrary(comctl32);
  if fhComCtl32 <> 0 then
    fTaskDialogIndirect := GetProcAddress(fhComCtl32, 'TaskDialogIndirect')
  else
    fTaskDialogIndirect := nil;
  fpCallbackStub := CreateStub(Self, @TMsgBoxExVista.TaskDialogCallback);
end;

destructor TMsgBoxExVista.Destroy;
begin
  DisposeAndNilStub(fpCallbackStub);
  fTaskDialogIndirect := nil;
  if fhComCtl32 <> 0 then
    FreeLibrary(fhComCtl32);
  inherited;
end;

const
  TaskDialogButtons: array[TPJMsgDlgButtonGroup] of Integer = (
      0,                                                          // bgAbortRetryIgnore
      TDCBF_OK_BUTTON,                                            // bgOK
      TDCBF_OK_BUTTON or TDCBF_CANCEL_BUTTON,                     // bgOKCancel
      TDCBF_RETRY_BUTTON or TDCBF_CANCEL_BUTTON,                  // bgRetryCancel
      TDCBF_YES_BUTTON or TDCBF_NO_BUTTON,                        // bgYesNo
      TDCBF_YES_BUTTON or TDCBF_NO_BUTTON or TDCBF_CANCEL_BUTTON  // bgYesNoCancel
  );
  TaskDialogIcons: array[TPJMsgDlgIconKind] of PWideChar = (
      TD_WARNING_ICON,      // miWarning
      TD_INFORMATION_ICON,  // miInfo
      // TD_QUESTION_ICON doesn't exist because it's advised against by MS's UI guidelines
      // http://msdn.microsoft.com/en-us/library/aa511277.aspx#questiongl
      TD_INFORMATION_ICON,  // miQuestion
      TD_ERROR_ICON,        // miError
      nil,                  // miUser
      nil                   // miNone
  );

function TMsgBoxExVista.Execute: Integer;
var
  Config: TTaskDialogConfig;
  WindowTitle, Content, VerifText: WideString;
begin
  if not Assigned(fTaskDialogIndirect) then
    Result := inherited Execute
  else begin
    Result := 0;
    FillChar(Config, SizeOf(Config), 0);
    with Config do begin
      cbSize := SizeOf(Config);
      hwndParent := GetHWND;
      hInstance := SysInit.HInstance;
      dwFlags := TDF_ENABLE_HYPERLINKS;
      if fChecked then
        IncludeFlag(dwFlags, TDF_VERIFICATION_FLAG_CHECKED);
      dwCommonButtons := TaskDialogButtons[ButtonGroup];
      if Title <> '' then
        WindowTitle := Title
      else
        WindowTitle := GetDefaultTitle;
      pszWindowTitle := PWideChar(WindowTitle);
      if IconKind = miUser then
        pszMainIcon := MAKEINTRESOURCEW(IconResource)
      else
        pszMainIcon := TaskDialogIcons[IconKind];
      pszMainInstruction := nil;
      Content := Text;
      if fShowLink then
        Content := Content + CrLf2 + fLinkCaption;
      pszContent := PWideChar(Content);
      cButtons := 0;
      nDefaultButton := 0; // first button is default
      cRadioButtons := 0;
      if fShowCheck then begin
        VerifText := GetCheckCaption;
        pszVerificationText := PWideChar(VerifText);
        //pszExpandedInformation := PWideChar(WideString(fLinkCaption));
      end;
      pfCallback := fpCallbackStub;
    end;
    OleCheck(fTaskDialogIndirect(Config, @Result, nil, @fChecked));
  end;
end;

function TMsgBoxExVista.TaskDialogCallback(hWnd: HWND; msg: UINT; wParam: WPARAM;
    lParam: LPARAM; lpRefData: Pointer): HResult; stdcall;
begin
  case msg of
    TDN_HYPERLINK_CLICKED:
      ClickLink(hWnd, PWideChar(lParam));
  end;
  Result := S_OK;
end;

end.
