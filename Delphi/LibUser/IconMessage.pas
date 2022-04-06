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
 * TIconMessage VCL component
 *
 ****************************************************************************)

unit IconMessage;

interface

uses Messages, Windows, SysUtils, Classes, Controls, Forms, Math, AppIcon,
    SysLink, Common2;

const
  CM_ADJUSTBOUNDS = WM_USER + 1;

type
  TIconMessage = class;

  TIconMessageMargins = class(TPersistent)
  private
    FOwner: TIconMessage;
    FMargins: TRect;
    function GetMargins(Index: Integer): Integer;
    procedure SetMargins(Index, Value: Integer);
  public
    constructor Create(Owner: TIconMessage);
  published
    property Left: Integer index 0 read GetMargins write SetMargins default 8;
    property Right: Integer index 1 read GetMargins write SetMargins default 8;
    property Top: Integer index 2 read GetMargins write SetMargins default 5;
    property Bottom: Integer index 3 read GetMargins write SetMargins default 5;
  end;

  TIconMessage = class(TWinControl)
  private
    FIcon: TAppIcon;
    FLink: TSysLink;
    FMargins: TIconMessageMargins;
    FAdjustingBounds: Boolean;
    procedure AdjustBounds;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMAdjustBounds(var Message: TMessage); message CM_ADJUSTBOUNDS;
    procedure SetMargins(const Value: TIconMessageMargins);
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign); 
    // Icon properties
    function GetCommonIcon: TCommonIcon;
    procedure SetCommonIcon(Value: TCommonIcon);
    function GetResourceID: Cardinal;
    procedure SetResourceID(Value: Cardinal);
    function GetResourceName: String;
    procedure SetResourceName(const Value: String);
    // SysLink properties
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    function GetOnClickLink: TOnClickLink;
    procedure SetOnClickLink(Value: TOnClickLink);
    function GetTabStop: Boolean;
    procedure SetTabStop(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Margins: TIconMessageMargins read FMargins write SetMargins;
    property Align: TAlign read GetAlign write SetAlign;
    // Icon properties
    property CommonIcon: TCommonIcon read GetCommonIcon write SetCommonIcon;
    property ResourceID: Cardinal read GetResourceID write SetResourceID;
    property ResourceName: String read GetResourceName write SetResourceName;
    // SysLink properties
    property Caption: TCaption read GetCaption write SetCaption;
    property OnClickLink: TOnClickLink read GetOnClickLink write SetOnClickLink;
    property TabStop: Boolean read GetTabStop write SetTabStop;

    property TabOrder;
    property Visible;
  end;

procedure Register;

implementation

constructor TIconMessageMargins.Create(Owner: TIconMessage);
begin
  inherited Create;
  FOwner := Owner;
  FMargins.Left := 8;
  FMargins.Right := 8;
  FMargins.Top := 5;
  FMargins.Bottom := 5;
end;

function TIconMessageMargins.GetMargins(Index: Integer): Integer;
begin
  case Index of
    0: Result := FMargins.Left;
    1: Result := FMargins.Right;
    2: Result := FMargins.Top;
    3: Result := FMargins.Bottom;
    else
      Result := 0;
  end;
end;

procedure TIconMessageMargins.SetMargins(Index, Value: Integer);
begin
  case Index of
    0: FMargins.Left := Value;
    1: FMargins.Right := Value;
    2: FMargins.Top := Value;
    3: FMargins.Bottom := Value;
  end;
  FOwner.AdjustBounds;
end;

type
  TOwnedSysLink = class(TSysLink)
  private
    FOwner: TIconMessage;
  protected
    procedure CreateWnd; override;
  end;

procedure TOwnedSysLink.CreateWnd;
begin
  inherited;
  FOwner.AdjustBounds;
end;

constructor TIconMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks,
      csReplicatable];
  Width := 185;
  Height := 0; // work around alignment bug
  FMargins := TIconMessageMargins.Create(Self);
  FAdjustingBounds := False;
  FIcon := TAppIcon.Create(Self);
  FIcon.Parent := Self;
  FIcon.IconSize := icon16x16;
  FIcon.Width := 16;
  FIcon.Height := 16;
  FIcon.Visible := True;
  FLink := TOwnedSysLink.Create(Self);
  TOwnedSysLink(FLink).FOwner := Self;
  FLink.Parent := Self;
  FLink.Height := 30;
  FLink.AutoHeight := True;
  FLink.Visible := True;
  AdjustBounds;
end;

destructor TIconMessage.Destroy;
begin
  FreeAndNil(FIcon);
  FreeAndNil(FLink);
  FreeAndNil(FMargins);
  inherited;
end;

procedure TIconMessage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TIconMessage.CreateWnd;
begin
  inherited;
  AdjustBounds;
end;

procedure TIconMessage.AdjustBounds;
var
  IdealHeight: Integer;
begin
  if FAdjustingBounds then
    Exit;
  FAdjustingBounds := True;
  try
    if Assigned(FIcon) and Assigned(FLink) then begin
      FIcon.Top := FMargins.FMargins.Top;
      FLink.Top := FMargins.FMargins.Top;
      FIcon.Left := FMargins.FMargins.Left;
      FLink.Left := FIcon.Left + FIcon.Width + 4;
      FLink.Width := Max(0, Width - FLink.Left - FMargins.FMargins.Right);
      if FLink.LinkAvailable then begin
        IdealHeight := FMargins.FMargins.Top + Max(FLink.Height, FIcon.Height) +
            FMargins.FMargins.Bottom;
      end
      else begin
        IdealHeight := Max(Height,
            FMargins.FMargins.Top + FIcon.Height + FMargins.FMargins.Bottom);
        FLink.Height := Height - FMargins.FMargins.Top - FMargins.FMargins.Bottom;
      end;
      inherited SetBounds(Left, Top, Width, IdealHeight);
    end;
  finally
    FAdjustingBounds := False;
  end;
end;

procedure TIconMessage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not FAdjustingBounds and HandleAllocated then begin
    // If we set a new height by passing it to SetBounds here, then it won't
    // be taken into account by e.g. alignment.
    PostMessage(Handle, CM_ADJUSTBOUNDS, 0, 0);
  end;
end;

procedure TIconMessage.SetMargins(const Value: TIconMessageMargins);
begin
  FMargins.Assign(Value);
  AdjustBounds;
end;

function TIconMessage.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TIconMessage.SetAlign(Value: TAlign);
begin
  inherited Align := Value;
  AdjustBounds;
end;

procedure TIconMessage.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TIconMessage.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TIconMessage.CMAdjustBounds(var Message: TMessage);
begin
  AdjustBounds;
end;

procedure TIconMessage.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TIconMessage.DoEnter;
begin
  FLink.SetFocus;
end;

function TIconMessage.GetCommonIcon: TCommonIcon;
begin
  Result := FIcon.CommonIcon;
end;

procedure TIconMessage.SetCommonIcon(Value: TCommonIcon);
begin
  FIcon.CommonIcon := Value;
end;

function TIconMessage.GetResourceID: Cardinal;
begin
  Result := FIcon.ResourceID;
end;

procedure TIconMessage.SetResourceID(Value: Cardinal);
begin
  FIcon.ResourceID := Value;
end;

function TIconMessage.GetResourceName: String;
begin
  Result := FIcon.ResourceName;
end;

procedure TIconMessage.SetResourceName(const Value: String);
begin
  FIcon.ResourceName := Value;
end;

function TIconMessage.GetCaption: TCaption;
begin
  Result := FLink.Caption;
end;

procedure TIconMessage.SetCaption(const Value: TCaption);
begin
  FLink.Caption := Value;
end;

function TIconMessage.GetOnClickLink: TOnClickLink;
begin
  Result := FLink.OnClickLink;
end;

procedure TIconMessage.SetOnClickLink(Value: TOnClickLink);
begin
  FLink.OnClickLink := Value;
end;

function TIconMessage.GetTabStop: Boolean;
begin
  Result := FLink.TabStop;
end;

procedure TIconMessage.SetTabStop(Value: Boolean);
begin
  FLink.TabStop := Value;
end;

procedure Register;
begin
  RegisterComponents('BM', [TIconMessage]);
end;

end.
