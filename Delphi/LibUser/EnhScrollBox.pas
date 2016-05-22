(****************************************************************************
 *
 * Copyright 2016 Tim De Baets
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
 * TEnhScrollBox VCL component
 *
 ****************************************************************************)

unit EnhScrollBox;

interface

uses Windows, Classes, Messages, Controls, Forms, FormDefs, ActiveX, CtrlsCommon,
    Common2;

type
  TEnhScrollBox = class(TScrollBox)
  private
    fFocusOnClick: Boolean;
    fpWinEventStub: Pointer;
    fhWinEventHook: THandle;
    procedure DoFocusOnClick;
    procedure HookWinEvents;
    procedure UnhookWinEvents;
    procedure WinEventProc(hWinEventHook: THandle; event: DWORD;
        hwnd: HWND; idObject, idChild: Longint;
        idEventThread, dwmsEventTime: DWORD); stdcall;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure CMAutoScrollInView(var Message: TCMAutoScrollInView);
        message CM_AUTO_SCROLL_IN_VIEW;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
    procedure Click; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FocusOnClick: Boolean read fFocusOnClick write fFocusOnClick
        default True;
  end;

procedure Register;

implementation

uses SysLink, ClassCallback, OleAccDLL, Accessibility_TLB;

constructor TEnhScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  fFocusOnClick := True;
  fpWinEventStub := CreateStub(Self, @TEnhScrollBox.WinEventProc);
  fhWinEventHook := 0;
end;

destructor TEnhScrollBox.Destroy;
begin
  inherited;
  // dispose stub *after* calling the inherited method to make sure that
  // UnhookWinEvents has been called first
  DisposeStub(fpWinEventStub);
  fpWinEventStub := nil;
end;

procedure TEnhScrollBox.DoFocusOnClick;
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do begin
    if (Controls[i] is TWinControl) and (TWinControl(Controls[i]).Focused) then
      Exit;
  end;
  for i := 0 to ControlCount - 1 do begin
    if Controls[i] is TWinControl then with TWinControl(Controls[i]) do begin
      if TabOrder = 0 then begin
        SetFocus;
        Exit;
      end;
    end;
  end;
  // Fallback: set focus on ourselves. The downside of this is that there's no
  // indication of the focus to the user.
  SetFocus;
end;

procedure TEnhScrollBox.HookWinEvents;
var
  ProcID, ThreadID: Cardinal;
begin
  ThreadID := GetWindowThreadProcessId(Handle, @ProcID);
  fhWinEventHook := SetWinEventHook(EVENT_OBJECT_FOCUS, EVENT_OBJECT_FOCUS, 0,
      fpWinEventStub, ProcID, ThreadID, WINEVENT_OUTOFCONTEXT);
end;

procedure TEnhScrollBox.UnhookWinEvents;
begin
  if fhWinEventHook <> 0 then begin
    UnhookWinEvent(fhWinEventHook);
    fhWinEventHook := 0;
  end;
end;

procedure TEnhScrollBox.WinEventProc(hWinEventHook: THandle; event: DWORD;
    hwnd: HWND; idObject, idChild: Longint;
    idEventThread, dwmsEventTime: DWORD); stdcall;
var
  hr: HResult;
  Accessible: IAccessible;
  VarChild: OleVariant;
  Left, Top, Width, Height: Integer;
  URLRect: TRect;
begin
  if (event = EVENT_OBJECT_FOCUS) and (idObject = Longint(OBJID_CLIENT))
      and (idChild <> CHILDID_SELF) // URLs only, not the SysLink itself
      and (hwnd <> 0) and (GetParent(hwnd) = Handle) then begin
    VariantInit(VarChild);
    hr := AccessibleObjectFromEvent(hwnd, idObject, idChild, Accessible,
        VarChild);
    if Succeeded(hr) and Assigned(Accessible) then begin
      if Succeeded(Accessible.accLocation(Left, Top, Width, Height,
          VarChild)) then begin
        URLRect := Rect(Left, Top, Left + Width, Top + Height);
        URLRect.TopLeft := ScreenToClient(URLRect.TopLeft);
        URLRect.BottomRight := ScreenToClient(URLRect.BottomRight);
        ScrollRectInView(Self, URLRect);
      end;
    end;
  end;
end;

procedure TEnhScrollBox.WMNCDestroy(var Message: TWMNCDestroy);
begin
  // DestroyWnd/DestroyWindowHandle only gets called when the handle is
  // recreated! To handle control destruction, we need to cleanup on
  // WM_NCDESTROY.
  UnhookWinEvents;
  inherited;
end;

procedure TEnhScrollBox.CMAutoScrollInView(var Message: TCMAutoScrollInView);
begin
  // When a TSysLink control in the scrollbox gains focus, we don't want
  // TCustomForm.SetFocusedControl to call AutoScrollInView on the whole TSysLink
  // (we take care of focusing the individual links ourselves).
  // Overriding AutoScrollInView isn't possible, because that method isn't declared
  // as virtual yet in Delphi 4.
  // So we rely on a custom modification in SetFocusedControl to send a special-
  // purpose message and to prevent default behavior when that message's result
  // is nonzero. See TCustomForm.SetFocusedControl in LibFixed/Forms.pas.
  if Assigned(Message.Control) then begin
    if Message.Control is TSysLink then
      Message.Result := 1;
  end;
end;

function TEnhScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
    MousePos: TPoint): Boolean;
var
  NumLines, ScrollReq, i: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then begin
    if DWORD(Mouse.WheelScrollLines) = WHEEL_PAGESCROLL then begin
      // page scrolling
      NumLines := 1;
      if WheelDelta < 0 then
        ScrollReq := SB_PAGEDOWN
      else
        ScrollReq := SB_PAGEUP;
    end
    else begin
      // line scrolling
      NumLines := Abs(Mouse.WheelScrollLines * (WheelDelta div WHEEL_DELTA));
      if WheelDelta < 0 then
        ScrollReq := SB_LINEDOWN
      else
        ScrollReq := SB_LINEUP;
    end;
    for i := 1 to NumLines do
      Perform(WM_VSCROLL, ScrollReq, 0);
    Result := True;
  end;
end;

procedure TEnhScrollBox.Click;
begin
  inherited;
  if fFocusOnClick then
    DoFocusOnClick;
end;

procedure TEnhScrollBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  HookWinEvents;
end;

procedure TEnhScrollBox.DestroyWindowHandle;
begin
  UnhookWinEvents;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('BM', [TEnhScrollBox]);
end;

end.
