{***************************************************************
 *
 * Unit Name: AppIcon
 * Purpose : Contains the AppIcon component which can be used
 * to display an Icon on a form from your project's
 * resource file. Set the property ResourceName to
 * the name of the resource you want to load. It
 * will be displayed at run time.
 * Author : John Long <johnwlong@mail.com>
 * History : ??/??/2001 Created.
 * Notes : Please email all significant changes to me at my email address
above.
 * Copyright© 2001-2002, John W. Long
 ****************************************************************}

(****************************************************************************
 *
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
 ****************************************************************************)

unit AppIcon;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Math, Common2;

type
  TIconSize = (icon16x16, icon24x24, icon32x32, icon48x48, iconCustom);
  TAppIcon = class(TGraphicControl)
  private
    { Private declarations }
    FIconHandle: HICON;
    FIconWidth: Integer;
    FIconHeight: Integer;
    FIconSize: TIconSize;
    FResourceName: String;
    FResourceID: Cardinal;
    FCommonIcon: TCommonIcon;
    procedure ReloadIcon;
    procedure SetIconHandle(const Handle: HICON);
    procedure SetIconWidth(const Value: Integer);
    procedure SetIconHeight(const Value: Integer);
    procedure SetIconSize(const Value: TIconSize);
    procedure SetResourceName(const Value: String);
    procedure SetResourceID(const Value: Cardinal);
    procedure SetCommonIcon(const Value: TCommonIcon);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeIconSize(Size: TPoint);
    property IconHandle: HICON read FIconHandle write SetIconHandle;
    procedure Paint; override;
    procedure LoadFromResourceIDWithInstance(hMod: HINST; ResID: Cardinal);
    procedure LoadFromResourceID(ResID: Cardinal);
    procedure LoadFromResourceNameWithInstance(hMod: HINST;
        const ResName: String);
    procedure LoadFromResourceName(const ResName: String);
  published
    { Published declarations }
    property IconWidth: Integer read FIconWidth write SetIconWidth;
    property IconHeight: Integer read FIconHeight write SetIconHeight;
    property IconSize: TIconSize read FIconSize write SetIconSize;
    property ResourceName: String read FResourceName write SetResourceName;
    property ResourceID: Cardinal read FResourceID write SetResourceID;
    property CommonIcon: TCommonIcon read FCommonIcon write SetCommonIcon
        default ciNone;
    property Anchors;
    property Align;
    property Constraints;
    property ShowHint;
    property Cursor;
    property Hint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Wiseheart', [TAppIcon]);
end;

{ TAppIcon }

constructor TAppIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResourceName := '';
  FResourceID := 0;
  FIconHandle := 0;
  ChangeIconSize(Point(32, 32));
  FIconSize := icon32x32;
  FCommonIcon := ciNone;
  Width := 32;
  Height := 32;
end;

procedure TAppIcon.SetIconHandle(const Handle: HICON);
begin
  FIconHandle := Handle;
  FCommonIcon := ciNone;
  Repaint;
end;

procedure TAppIcon.ReloadIcon;
var
  IconName: PChar;
  hInst: Integer;
begin
  if FIconHandle <> 0 then
    DestroyIcon(FIconHandle);
  if FCommonIcon <> ciNone then
    FIconHandle := LoadCommonIcon(FCommonIcon, FIconWidth, FIconHeight)
  else begin
    hInst := MainInstance;
    if csDesigning in ComponentState then
      IconName := 'MAINICON'
    else if FResourceName <> '' then
      IconName := PChar(FResourceName)
    else
      IconName := PChar(FResourceID);
    FIconHandle := LoadImage(hInst, IconName, IMAGE_ICON,
        FIconWidth, FIconHeight, LR_DEFAULTCOLOR);
  end;
  Repaint;
end;

procedure TAppIcon.ChangeIconSize(Size: TPoint);
begin
  FIconWidth := Max(Size.x, 16);
  FIconHeight := Max(Size.y, 16);
  if (FIconSize <> iconCustom) and
      ((FIconWidth <> 16) or (FIconHeight <> 16)) and
      ((FIconWidth <> 24) or (FIconHeight <> 24)) and
      ((FIconWidth <> 32) or (FIconHeight <> 32)) and
      ((FIconWidth <> 48) or (FIconHeight <> 48)) then
    FIconSize:= iconCustom;
  ReloadIcon;
end;

procedure TAppIcon.SetIconHeight(const Value: Integer);
begin
  ChangeIconSize(Point(FIconWidth, Value));
end;

procedure TAppIcon.SetIconWidth(const Value: Integer);
begin
  ChangeIconSize(Point(Value, FIconHeight));
end;

procedure TAppIcon.SetIconSize(const Value: TIconSize);
begin
  FIconSize := Value;
  case FIconSize of
    icon16x16: ChangeIconSize(Point(16, 16));
    icon24x24: ChangeIconSize(Point(24, 24));
    icon32x32: ChangeIconSize(Point(32, 32));
    icon48x48: ChangeIconSize(Point(48, 48));
  end;
end;

procedure TAppIcon.Paint;
begin
  inherited Paint;
  if FIconHandle > 0 then begin
    DrawIconEx(Canvas.Handle,
        (Width - FIconWidth) div 2, (Height - FIconHeight) div 2,
        FIconHandle, FIconWidth, FIconHeight, 0, 0, DI_NORMAL);
  end;
  if csDesigning in ComponentState then begin
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
  end;
end;

destructor TAppIcon.Destroy;
begin
  inherited Destroy;
  if FIconHandle <> 0 then
    DestroyIcon(FIconHandle);
end;

procedure TAppIcon.SetResourceName(const Value: String);
begin
  FResourceName := Value;
  ReloadIcon;
end;

procedure TAppIcon.SetResourceID(const Value: Cardinal);
begin
  FResourceID := Value;
  ReloadIcon;
end;

procedure TAppIcon.SetCommonIcon(const Value: TCommonIcon);
begin
  FCommonIcon := Value;
  ReloadIcon;
end;

procedure TAppIcon.LoadFromResourceIDWithInstance(hMod: HINST; ResID: Cardinal);
begin
  IconHandle := LoadImage(hMod, PChar(ResID), IMAGE_ICON,
      FIconWidth, FIconHeight, LR_DEFAULTCOLOR);
end;

procedure TAppIcon.LoadFromResourceID(ResID: Cardinal);
begin
  LoadFromResourceIDWithInstance(hInstance, ResID);
end;

procedure TAppIcon.LoadFromResourceNameWithInstance(hMod: HINST;
    const ResName: String);
begin
  IconHandle := LoadImage(hMod, PChar(ResName), IMAGE_ICON,
      FIconWidth, FIconHeight, LR_DEFAULTCOLOR);
end;

procedure TAppIcon.LoadFromResourceName(const ResName: String);
begin
  LoadFromResourceNameWithInstance(hInstance, ResName);
end;

end.
