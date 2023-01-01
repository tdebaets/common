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
 * TDialogForm class for showing Delphi forms as Win32 dialogs
 *
 ****************************************************************************)

unit DialogForm;

// TODO: Vista: fix weird bugs when changing windows theme while dialog open/WMP running
// TODO: look into weird hiding/showing accelerator behaviour
// see http://blogs.msdn.com/b/oldnewthing/archive/2013/05/17/10419502.aspx
// TODO: support poDefault/poDefaultSizeOnly?

interface

uses Windows, Messages, Forms, Controls, Classes, Common2, CtrlsCommon,
    SysUtils, UxTheme;

type
  TDialogFormClass = class of TDialogForm;
  
  TDialogForm = class(TForm)
  private
    fhWndParent: HWND;
    fDialogHandle: HWND;
    fDialogActive: Boolean;
    fDialogProcStub: Pointer;
    fOrigWndProc: Pointer;
    fObjectInstance: Pointer;
    fBorderX, fBorderY: Integer;
    fInitialBoundsSet: Boolean;
    fShowingAsDialog: Boolean;
    fWasDisabled: Boolean;
  protected
    DialogResult: Integer;
    pDialogData: Pointer;
    Resizable: Boolean;
    function DialogProc(hWnd, uMsg, wParam, lParam: Integer): LongBool; stdcall;
    procedure DialogWndProc(var Message: TMessage); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetDialogBounds(AWidth, AHeight: Integer);
    function GetCaption: WideString;
    procedure SetCaption(const NewCaption: WideString);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    property DialogParent: HWND read fhWndParent;
    property DialogHandle: HWND read fDialogHandle;
  public
    constructor Create(hWndParent: HWND; pData: Pointer); reintroduce; overload;
    constructor Create(hWndParent: HWND); reintroduce; overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Show;
    function ShowAsDialog: Integer;
    procedure Close;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function ScaleX(X: Integer): Integer;
    function ScaleY(Y: Integer): Integer;
    function DescaleX(X: Integer): Integer;
    function DescaleY(Y: Integer): Integer;
  published
    { Make TCustomForm members public }
    //property Action;
    property ActiveControl;
    property Align;
    //property Anchors;
    property AutoScroll;
    //property AutoSize;
    //property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    //property BorderWidth;
    property Caption: WideString read GetCaption write SetCaption;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ClientHeight;
    property ClientWidth;
    property Color;
    //property Constraints;
    property Ctl3D;
    //property UseDockManager;
    //property DefaultMonitor;
    //property DockSite;
    //property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    //property FormStyle;
    property HelpFile;
    property HorzScrollBar;
    property Icon;
    property KeyPreview;
    property Menu;
    //property OldCreateOrder;
    property ObjectMenuItem;
    //property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    //property Position;
    property PrintScale;
    property Scaled;
    property ShowHint;
    property VertScrollBar;
    //property Visible;
    //property WindowState;
    //property WindowMenu;
    property OnActivate;
    //property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    //property OnConstrainedResize;
    //property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    //property OnDockDrop;
    //property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnMouseWheel;
    //property OnMouseWheelDown;
    //property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    //property OnShortCut;
    property OnShow;
    //property OnStartDock;
    //property OnUnDock;
  end;

function ShowDialogForm(Form: TDialogFormClass; hWndParent: HWND): Integer;
function ShowDialogFormData(Form: TDialogFormClass; hWndParent: HWND;
    pDialogData: Pointer): Integer;

implementation

uses ClassCallback, MonitorFunc;

{$R DialogForm.res}

const
  IDD_DLG = 1000;

constructor TDialogForm.Create(hWndParent: HWND; pData: Pointer);
var
  ThemesAvailable: Boolean;
  MsgBoxFont: TLogFontW;
begin
  fDialogProcStub := CreateStub(Self, @TDialogForm.DialogProc);
  fObjectInstance := MakeObjectInstance(DialogWndProc);
  fhWndParent := hWndParent;
  pDialogData := pData;
  Resizable := False;
  fInitialBoundsSet := False;
  fShowingAsDialog := False;
  fWasDisabled := False;
  fDialogHandle := CreateDialogW(hInstance, PWideChar(IDD_DLG), hWndParent,
      fDialogProcStub);
  if fDialogHandle = 0 then
    raise Exception.Create('CreateDialog failed');
  inherited CreateParented(fDialogHandle);
  fDialogActive := True;
  DialogResult := 0;
  ThemesAvailable := InitThemeLibrary;
  if CompareText(Font.Name, 'MS Shell Dlg 2') = 0 then begin
    if ThemesAvailable and Assigned(GetWindowTheme)
        and Assigned(GetThemeSysFont) then begin
      if Succeeded(GetThemeSysFont(GetWindowTheme(fDialogHandle), TMT_MSGBOXFONT,
          MsgBoxFont)) then
        Font.Handle := CreateFontIndirectW(MsgBoxFont);
    end;
  end;
end;

constructor TDialogForm.Create(hWndParent: HWND);
begin
  Create(hWndParent, nil);
end;

procedure TDialogForm.AfterConstruction;
var
  Style: Integer;
  RectClient, RectWindow: TRect;
begin
  inherited;
  Assert(BorderStyle = bsDialog, 'Incorrect BorderStyle'); 
  Left := 0;
  Top := 0;
  // Following lines trigger RecreateWnd, which exposes a bug in ThemeManager
  // see TDialogForm.CreateParams for a (faster) alternative
  {Position := poDesigned;
  BorderStyle := bsNone;
  BorderIcons := [];}
  if Resizable then begin
    Style := GetWindowLongW(fDialogHandle, GWL_STYLE);
    // Combining WS_SYSMENU with WS_THICKFRAME has the side-effect that resizing
    // doesn't work anymore, see WM_NCLBUTTONDOWN handler
    SetWindowLongW(fDialogHandle, GWL_STYLE,
        (Style {and not WS_SYSMENU}) or WS_THICKFRAME);
  end;
  SetWindowTextW(fDialogHandle, PWideChar(Caption));
  // Dummy call to SetWindowPos, which may be necessary for Windows to set the
  // actual client rect (and therefore, border)
  SetWindowPos(fDialogHandle, 0, 0, 0,
      inherited Width,
      inherited Height,
      SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER);
  // Account for thick glass border on Vista+
  Windows.GetClientRect(fDialogHandle, RectClient);
  Windows.GetWindowRect(fDialogHandle, RectWindow);
  fBorderX := (RectWindow.Right - RectWindow.Left) - RectClient.Right;
  fBorderY := (RectWindow.Bottom - RectWindow.Top) - RectClient.Bottom;
  SetDialogBounds(inherited Width, inherited Height);
  fInitialBoundsSet := True;  
end;

destructor TDialogForm.Destroy;
begin
  // Important: destroy the base form before destroying the parent dialog, and
  // only then free the DialogProc- and WindowProc-related members
  inherited;
  DestroyWindow(fDialogHandle);
  SetWindowLongW(fDialogHandle, GWL_WNDPROC, Integer(@DefWindowProc));
  FreeThemeLibrary;
  if Assigned(fDialogProcStub) then
    DisposeAndNilStub(fDialogProcStub);
  if Assigned(fObjectInstance) then begin
    FreeObjectInstance(fObjectInstance);
    fObjectInstance := nil;
  end;
end;

function TDialogForm.Show;
begin
  fShowingAsDialog := False;
  inherited;
end;

function TDialogForm.ShowAsDialog: Integer;
begin
  fShowingAsDialog := True;
  inherited Show;
  SetFocus;
  // DS_CENTER doesn't work correctly if the size of the dialog is changed in
  // WM_INITDIALOG
  ClipOrCenterWindowToMonitor(fDialogHandle, MONITOR_CENTER or MONITOR_WORKAREA);
  ShowWindow(fDialogHandle, SW_SHOWNORMAL);
  if fhWndParent <> 0 then
    fhWndParent := GetAncestor(fhWndParent, GA_ROOT);
  fWasDisabled := EnableWindow(fhWndParent, False);
  repeat
    // TODO: restore original exit code?
    if not HandleAppMessage then
      Break;
  until not fDialogActive;
  Result := DialogResult;
  // Note: re-enabling our owner window and hiding ourselves doesn't happen here,
  // but in TDialogForm.Close
  // We can't just set ParentWindow to 0 because then Delphi destroys our
  // window handle
  ParentWindow := GetDesktopWindow;
end;

procedure TDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if fDialogHandle <> 0 then with Params do begin
    ExStyle := 0;
    Style := Style or (WS_CHILD or WS_GROUP or WS_TABSTOP);
    X := 0;
    Y := 0;
    Style := Style and not WS_POPUP;
    Style := Style and not (WS_CAPTION or WS_BORDER);
    Style := Style and not (WS_CAPTION or WS_THICKFRAME);
    Style := Style and not (WS_POPUP or WS_CAPTION);
    Style := Style and not (WS_DLGFRAME or DS_MODALFRAME);
    Style := Style and not WS_MINIMIZEBOX;
    Style := Style and not WS_MAXIMIZEBOX;
    Style := Style and not WS_MINIMIZE;
    Style := Style and not WS_MAXIMIZE;
    Style := Style and not WS_SYSMENU;
    Style := Style and not WS_VISIBLE;
  end;
end;

procedure TDialogForm.SetDialogBounds(AWidth, AHeight: Integer);
begin
  SetWindowPos(fDialogHandle, 0, 0, 0,
      AWidth + fBorderX,
      AHeight + fBorderY,
      SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER);
end;

function TDialogForm.DialogProc(hWnd, uMsg, wParam,
    lParam: Integer): LongBool; stdcall;
var
  SizingEdge: Integer;
begin
  Result := FALSE;
  case uMsg of
    WM_INITDIALOG: begin
      fOrigWndProc := Pointer(GetWindowLongW(hWnd, GWL_WNDPROC));
      SetWindowLongW(hWnd, GWL_WNDPROC, Longint(fObjectInstance));
    end;
    WM_CLOSE:
      Close;
    WM_DESTROY:
      fDialogHandle := 0;
    WM_SIZE: begin
      if fInitialBoundsSet then begin
        ClientHeight := HiWord(lParam);
        ClientWidth := LoWord(lParam);
      end;
    end;
    WM_NCLBUTTONDOWN: begin
      if Resizable then begin
        // Because WS_SYSMENU is combined with WS_THICKFRAME, we need to do the
        // resizing manually
        SizingEdge := 0;
        case wParam of
          HTTOP: SizingEdge := WMSZ_TOP;
          HTTOPLEFT: SizingEdge := WMSZ_TOPLEFT;
          HTTOPRIGHT: SizingEdge := WMSZ_TOPRIGHT;
          HTLEFT: SizingEdge := WMSZ_LEFT;
          HTRIGHT: SizingEdge := WMSZ_RIGHT;
          HTBOTTOM: SizingEdge := WMSZ_BOTTOM;
          HTBOTTOMLEFT: SizingEdge := WMSZ_BOTTOMLEFT;
          HTBOTTOMRIGHT: SizingEdge := WMSZ_BOTTOMRIGHT;
        end;
        if SizingEdge <> 0 then
          SendMessage(hWnd, WM_SYSCOMMAND, SC_SIZE or SizingEdge, lParam);
      end;
    end;
    WM_GETMINMAXINFO: begin
      with PMinMaxInfo(lParam)^, Constraints do begin
        with ptMinTrackSize do begin
          if MinWidth > 0 then
            X := ScaleX(MinWidth + fBorderX);
          if MinHeight > 0 then
            Y := ScaleY(MinHeight + fBorderY);
        end;
        with ptMaxTrackSize do begin
          if MaxWidth > 0 then
            X := ScaleX(MaxWidth + fBorderX);
          if MaxHeight > 0 then
            Y := ScaleY(MaxHeight + fBorderY);
        end;
      end;
    end;
  end;
end;

procedure TDialogForm.DialogWndProc(var Message: TMessage);
begin
  with Message do
    Result := CallWindowProcW(fOrigWndProc, fDialogHandle, Msg, WParam, LParam);
end;

function TDialogForm.GetCaption: WideString;
begin
  Result := inherited Caption;
end;

procedure TDialogForm.SetCaption(const NewCaption: WideString);
begin
  inherited Caption := NewCaption;
  if fDialogHandle <> 0 then
    SetWindowTextW(fDialogHandle, PWideChar(NewCaption));
end;

function TDialogForm.GetWidth: Integer;
var
  R: TRect;
begin
  if fInitialBoundsSet then begin
    Windows.GetClientRect(fDialogHandle, R);
    Result := R.Right - R.Left;
  end
  else
    Result := inherited Width;
end;

procedure TDialogForm.SetWidth(Value: Integer);
begin
  if fInitialBoundsSet then
    SetDialogBounds(Value, Height)
  else
    inherited Width := Value;
end;

function TDialogForm.GetHeight: Integer;
var
  R: TRect;
begin
  if fInitialBoundsSet then begin
    Windows.GetClientRect(fDialogHandle, R);
    Result := R.Bottom - R.Top;
  end
  else
    Result := inherited Height;
end;

procedure TDialogForm.SetHeight(Value: Integer);
begin
  if fInitialBoundsSet then
    SetDialogBounds(Width, Value)
  else
    inherited Height := Value;
end;

procedure TDialogForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(0, 0, AWidth, AHeight);
end;

procedure TDialogForm.Close;
var
  CloseAction: TCloseAction;
begin
  // Calling the inherited Close when showing as dialog causes the form to
  // become hidden prematurely
  if not fShowingAsDialog then begin
    inherited;
    Exit;
  end;
  
  if not CloseQuery then
    Exit;
  // We can only hide the dialog after the owner window has been re-enabled
  CloseAction := caHide;
  DoClose(CloseAction);
  if (CloseAction <> caNone) and (fDialogHandle <> 0) then begin
    if not fWasDisabled then
      EnableWindow(fhWndParent, True);
    ShowWindow(fDialogHandle, SW_HIDE);
    Hide;
    fDialogActive := False;
  end;
end;

// For the following funcs, we don't take form font width/height into account as
// this gives incorrect results on Windows 7
function TDialogForm.ScaleX(X: Integer): Integer;
begin
  Result := MulDiv(X, Screen.PixelsPerInch, BasePixelsPerInch);
end;

function TDialogForm.ScaleY(Y: Integer): Integer;
begin
  Result := MulDiv(Y, Screen.PixelsPerInch, BasePixelsPerInch);
end;

function TDialogForm.DescaleX(X: Integer): Integer;
begin
  Result := MulDiv(X, BasePixelsPerInch, Screen.PixelsPerInch);
end;

function TDialogForm.DescaleY(Y: Integer): Integer;
begin
  Result := MulDiv(Y, BasePixelsPerInch, Screen.PixelsPerInch);
end;

function ShowDialogFormData(Form: TDialogFormClass; hWndParent: HWND;
    pDialogData: Pointer): Integer;
begin
  with Form.Create(hWndParent, pDialogData) do try
    Result := ShowAsDialog;
  finally
    Free;
  end;
end;

function ShowDialogForm(Form: TDialogFormClass; hWndParent: HWND): Integer;
begin
  Result := ShowDialogFormData(Form, hWndParent, nil);
end;

end.
