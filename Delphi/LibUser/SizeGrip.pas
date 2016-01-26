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
 * TSizeGrip VCL component
 *
 ****************************************************************************)

unit SizeGrip;

interface

uses Windows, Classes, Controls, SysUtils;

type
  TSizeGrip = class(TWinControl)
  protected
    procedure CMDesignHitTest(var Message: TCMDesignHitTest);
        message CM_DESIGNHITTEST;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TSizeGrip]);
end;

procedure TSizeGrip.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

procedure TSizeGrip.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (csDesigning in ComponentState) and Assigned(Parent) then begin
    ALeft := Parent.ClientWidth - Width;
    ATop := Parent.ClientHeight - Height;
    AWidth := Width;
    AHeight := Height;
  end;
  inherited;
end;

constructor TSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csOpaque, csFixedWidth, csFixedHeight];
  Anchors := [akRight, akBottom];
  Cursor := crSizeNWSE;
end;

procedure TSizeGrip.CreateParams(var Params: TCreateParams);
var
  R: TRect;
begin
  inherited;
  CreateSubClass(Params, 'SCROLLBAR');
  Params.Style := Params.Style or WS_CLIPSIBLINGS or SBS_SIZEGRIP or
      SBS_SIZEBOXBOTTOMRIGHTALIGN;
  if not Windows.GetClientRect(Params.WndParent, R) then
    RaiseLastWin32Error;
  Params.X := R.Left;
  Params.Y := R.Top;
  Params.Width := R.Right - R.Left;
  Params.Height := R.Bottom - R.Top;
end;

end.
