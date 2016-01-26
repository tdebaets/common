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
 * TUpDown32 VCL Component
 *
 ****************************************************************************)

unit UpDown32;

interface

uses Windows, Messages, Classes, SysUtils, Controls, StdCtrls, ComCtrls;

const
  UDM_GETPOS32 = WM_USER + 114;
  UDM_GETRANGE32 = WM_USER + 112;
  UDM_SETPOS32 = WM_USER + 113;
  UDM_SETRANGE32 = WM_USER + 111;

type
  TUpDown32 = class(TCustomUpDown)
  private
    FMin32: Integer;
    FMax32: Integer;
    FPosition32: Integer;
    FOnClick: TUDClickEvent;
    function GetPos: Integer;
    procedure OnScroll;
    function GetPosition32: Integer;
    procedure SetMin32(Value: Integer);
    procedure SetMax32(Value: Integer);
    procedure SetPosition32(Value: Integer);
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  protected
    procedure CreateWnd; override;
    procedure Click(Button: TUDBtnType); override;
    procedure ClickNew(Button: TUDBtnType); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    property Min32: Integer read FMin32 write SetMin32;
    property Max32: Integer read FMax32 write SetMax32;
    property Position32: Integer read GetPosition32 write SetPosition32;
    property OnClick: TUDClickEvent read FOnClick write FOnClick;
  published
    property AlignButton;
    property Anchors;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Increment;
    property Constraints;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TUpDown32.Create(AOwner: TComponent);
begin
  inherited;
  FMax32 := 100;
end;

procedure TUpDown32.CreateWnd;
begin
  inherited;
  SendMessage(Handle, UDM_SETRANGE32, FMin32, FMax32);
  SendMessage(Handle, UDM_SETPOS32, 0, FPosition32);
end;

procedure TUpDown32.Click(Button: TUDBtnType);
begin
  // do nothing
end;

procedure TUpDown32.ClickNew(Button: TUDBtnType);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button);
end;

function TUpDown32.GetPos: Integer;
begin
  Result := SendMessage(Handle, UDM_GETPOS32, 0, 0);
end;

procedure TUpDown32.OnScroll;
var
  Pos: Integer;
begin
  Pos := GetPos;
  if Pos > FPosition32 then
    ClickNew(btNext)
  else if Pos < FPosition32 then
    ClickNew(btPrev);
  FPosition32 := Pos;
end;

procedure TUpDown32.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if Message.ScrollCode = SB_THUMBPOSITION then
    OnScroll;
end;

procedure TUpDown32.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if Message.ScrollCode = SB_THUMBPOSITION then
    OnScroll;
end;

function TUpDown32.GetPosition32: Integer;
begin
  if HandleAllocated then begin
    Result := GetPos;
    FPosition32 := Result;
  end
  else
    Result := FPosition32;
end;

procedure TUpDown32.SetMin32(Value: Integer);
begin
  if Value <> FMin32 then begin
    FMin32 := Value;
    if HandleAllocated then
      SendMessage(Handle, UDM_SETRANGE32, FMin32, FMax32);
  end;
end;

procedure TUpDown32.SetMax32(Value: Integer);
begin
  if Value <> FMax32 then begin
    FMax32 := Value;
    if HandleAllocated then
      SendMessage(Handle, UDM_SETRANGE32, FMin32, FMax32);
  end;
end;

procedure TUpDown32.SetPosition32(Value: Integer);
begin
  if Value <> FPosition32 then begin
    FPosition32 := Value;
    if (csDesigning in ComponentState) and (Associate <> nil) then begin
      if Associate is TCustomEdit then
        TCustomEdit(Associate).Text := IntToStr(FPosition32);
    end;
    if HandleAllocated then
      SendMessage(Handle, UDM_SETPOS32, 0, FPosition32);
  end;
end;

end.
