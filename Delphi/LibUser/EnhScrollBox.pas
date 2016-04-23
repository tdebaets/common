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

uses Windows, Classes, Messages, Controls, Forms, FormDefs, Common2;

type
  TEnhScrollBox = class(TScrollBox)
  private
    fFocusOnClick: Boolean;
    procedure DoFocusOnClick;
    procedure CMAutoScrollInView(var Message: TCMAutoScrollInView);
        message CM_AUTO_SCROLL_IN_VIEW;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    property FocusOnClick: Boolean read fFocusOnClick write fFocusOnClick
        default True;
  end;

procedure Register;

implementation

uses SysLink;

constructor TEnhScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  fFocusOnClick := True;
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

procedure Register;
begin
  RegisterComponents('BM', [TEnhScrollBox]);
end;

end.
