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
    procedure CMAutoScrollInView(var Message: TCMAutoScrollInView);
        message CM_AUTO_SCROLL_IN_VIEW;
    procedure WinEventProc(hWinEventHook: THandle; event: DWORD;
        hwnd: HWND; idObject, idChild: Longint;
        idEventThread, dwmsEventTime: DWORD); stdcall;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
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
  DisposeStub(fpWinEventStub);
  fpWinEventStub := nil;
  inherited;
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
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then begin
    if Shift * [ssShift..ssCtrl] = [] then begin
      VertScrollBar.Position := VertScrollBar.Position - WheelDelta;
      Result := True;
    end;
  end;
end;

procedure TEnhScrollBox.Click;
begin
  inherited;
  if fFocusOnClick then
    DoFocusOnClick;
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

procedure TEnhScrollBox.CreateWnd;
var
  ProcID, ThreadID: Cardinal;
begin
  inherited;
  ThreadID := GetWindowThreadProcessId(Handle, @ProcID);
  fhWinEventHook := SetWinEventHook(EVENT_OBJECT_FOCUS, EVENT_OBJECT_FOCUS, 0,
      fpWinEventStub, ProcID, ThreadID, WINEVENT_OUTOFCONTEXT);
end;

procedure TEnhScrollBox.DestroyWnd;
begin
  if fhWinEventHook <> 0 then begin
    UnhookWinEvent(fhWinEventHook);
    fhWinEventHook := 0;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('BM', [TEnhScrollBox]);
end;

end.
