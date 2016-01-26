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
 * TSysLink VCL component
 *
 ****************************************************************************)

unit SysLink;

interface

uses Windows, SysUtils, Classes, Controls, StdCtrls, NewCommCtrl, Messages,
    ComStrs, Common2;

type
  TOnClickLink = procedure(Sender: TObject; Index: Integer;
      const AID, AURL: String; var Visited: Boolean) of object;

  TCustomSysLink = class(TWinControl)
  private
    FLinkAvailable: Boolean;
    FOnClickLink: TOnClickLink;
    FAlignment: TAlignment;
    FAutoHeight: Boolean;
    procedure SetOnClickLink(const Value: TOnClickLink);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure AdjustBounds;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoHeight(Value: Boolean);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    property Alignment: TAlignment read FAlignment write SetAlignment
        default taLeftJustify;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight
        default True;
    procedure WndProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property LinkAvailable: Boolean read FLinkAvailable;
  published
    property OnClickLink: TOnClickLink read FOnClickLink write SetOnClickLink;
  end;

  TSysLink = class(TCustomSysLink)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoHeight;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnClickLink;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Win32', [TSysLink]);
end;

{ TCustomSysLink }

constructor TCustomSysLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csOpaque,
      csReplicatable, csDoubleClicks];
  Width := 65;
  Height := 17;
  FAutoHeight := True;
  AdjustBounds;
end;

procedure TCustomSysLink.CreateWnd;
begin
  inherited;
  AdjustBounds;
end;

procedure TCustomSysLink.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FLinkAvailable := CheckCommonControl(ICC_LINK_CLASS);
  if FLinkAvailable then begin
    CreateSubClass(Params, WC_LINK);
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
  end
  else begin
    CreateSubClass(Params, 'STATIC');
    //Params.Style := Params.Style or SS_LEFTNOWORDWRAP;
  end;
end;

procedure TCustomSysLink.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TCustomSysLink.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TCustomSysLink.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TCustomSysLink.AdjustBounds;
{var
  DC: HDC;
  SaveFont: HFont;
  TextSize: TSize;}
begin
  {if not (csReading in ComponentState) and FAutoSize then begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextExtentPoint32(DC, PChar(Caption), Length(Caption), TextSize);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    SetBounds(Left, Top,
        TextSize.cx + (GetSystemMetrics(SM_CXBORDER) * 4),
        TextSize.cy + (GetSystemMetrics(SM_CYBORDER) * 4));
  end;}
  SetBounds(Left, Top, Width, Height);
end;

procedure TCustomSysLink.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  IdealHeight: Integer;
  Size: TSize;
begin
  if FLinkAvailable and FAutoHeight then begin
    // LM_GETIDEALSIZE is documented to only work on Vista+, but already works
    // on XP too
    IdealHeight := Perform(LM_GETIDEALSIZE, Width, Integer(@Size));
    inherited SetBounds(ALeft, ATop, AWidth, IdealHeight);
  end
  else
    inherited;
end;

procedure TCustomSysLink.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TCustomSysLink.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then begin
    FAutoHeight := Value;
    if Value then
      AdjustBounds;
  end;
end;

procedure TCustomSysLink.SetOnClickLink(const Value: TOnClickLink);
begin
  FOnClickLink := Value;
end;

procedure TCustomSysLink.WndProc(var Msg: TMessage);
var
  pLink: PNMLink;
  Visited: Boolean;
  Item: TLItem;
begin
  case Msg.Msg of
    CN_NOTIFY:
      case TWMNotify(Msg).NMHdr.code of
        NM_CLICK, NM_RETURN: begin
          pLink := PNMLINK(TMessage(Msg).LParam);
          if Assigned(FOnClickLink) then begin
            Visited := True;
            FOnClickLink(Self, pLink.item.iLink,
                         WideCharToString(pLink.item.szID),
                         WideCharToString(pLink.item.szUrl), Visited);
            if Visited then begin
              FillChar(Item, SizeOf(Item), 0);
              Item.mask := LIF_ITEMINDEX or LIF_STATE;
              Item.iLink := pLink.item.iLink;
              Item.stateMask := LIS_VISITED;
              if LongBool(SendMessage(Handle, LM_GETITEM, 0,
                  Integer(@Item))) then begin
                Item.state := Item.state or LIS_VISITED;
                SendMessage(Handle, LM_SETITEM, 0, Integer(@Item));
              end;
            end;
          end;
        end;
      end;
    WM_GETDLGCODE:
      Msg.Result := DLGC_WANTALLKEYS {or DLGC_WANTTAB};
    else
      inherited;
  end;
end;

end.
