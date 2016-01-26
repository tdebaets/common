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
 * TCaptionBevel VCL component
 *
 ****************************************************************************)

unit CaptionBevel;

interface

uses Windows, Classes, Messages, ExtCtrls, Controls, Graphics, SysUtils;

type
  TCaptionBevel = class(TGraphicControl)
  private
    FStyle: TBevelStyle;
    FCaption : TCaption;
    FSpace: Integer;
    FAutoHeight: Boolean;
    function GetTransparent: Boolean;
    procedure SetStyle(Value: TBevelStyle);
    procedure SetCaption(Value : TCaption);
    procedure SetSpace(Value: Integer);
    procedure SetAutoHeight(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure DoDrawText(var Rect: TRect; Flags: Word);
    procedure AdjustBounds;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property ParentShowHint;
    property ShowHint;
    property Style: TBevelStyle read FStyle write SetStyle default bsLowered;
    property Visible;
    property Caption: TCaption read FCaption write SetCaption;
    property Font;
    property Space: Integer read FSpace write SetSpace default 3;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
  end;

procedure Register;

implementation

constructor TCaptionBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  FStyle := bsLowered;
  Width := 100;
  Height := 10;
  Space := 3;
  FAutoHeight := True;
end;

function TCaptionBevel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TCaptionBevel.SetStyle(Value: TBevelStyle);
begin
  if Value <> FStyle then begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TCaptionBevel.SetSpace(Value: Integer);
begin
  if FSpace <> Value then begin
    FSpace := Value;
    Invalidate;
  end;
end;

procedure TCaptionBevel.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Text := Value;
    Repaint;
    //AdjustBounds;
  end;
end;

procedure TCaptionBevel.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then begin
    FAutoHeight := Value;
    AdjustBounds;
  end;
end;

procedure TCaptionBevel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TCaptionBevel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TCaptionBevel.DoDrawText(var Rect: TRect; Flags: Word);
begin
  Flags := Flags or DT_NOPREFIX;
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(FCaption), Length(FCaption), Rect, Flags);
end;

procedure TCaptionBevel.AdjustBounds;
var
  DC: HDC;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and FAutoHeight then begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT));
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    SetBounds(Left, Top, Width, Rect.Bottom);
  end;
end;

procedure TCaptionBevel.Paint;
var
  TextH, TextW: Integer;
  LineT: Integer;
  R: TRect;
  C: array[Byte] of Char;
  CLen: Integer;
  Color1, Color2: TColor;
  CaptionExists: Boolean;
  TextLeft: Integer;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure DrawBevelText;
  begin
    with Canvas do begin
      StrPCopy(C, Text);
      if C[0] <> #0 then begin
        StrPCopy(C, Text);
        CLen := StrLen(C);
        R := Rect(0, 0, 0, TextH);
        DrawText(Canvas.Handle, C, CLen, R, DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
        Brush.Style := bsClear;
        TextW := R.Right;
        DrawText(Canvas.Handle, C, CLen, R, DT_LEFT or DT_SINGLELINE);
      end;
    end;
  end;

begin
  CaptionExists := (Caption <> '');

  with Canvas do begin
    if not Transparent then begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Pen.Width := 1;
    if CaptionExists then begin
      Font := Self.Font;
      TextH := TextHeight(Caption);
      R := Rect(0, (TextH Div 2) - 1 , Width, Height);
    end
    else
      R := Rect(0, 0, Width, Height);

    if FStyle = bsLowered then begin
      Color1 := clBtnShadow;
      Color2 := clBtnHighlight;
    end
    else begin
      Color1 := clBtnHighlight;
      Color2 := clBtnShadow;
    end;
    if CaptionExists then begin
      DrawBevelText;
      TextLeft := R.Right + FSpace;
      LineT := R.Bottom - 6;
    end
    else begin
      TextLeft := 0;
      LineT := 0;
    end;
    BevelLine(Color1, TextLeft, LineT, Width, LineT);
    BevelLine(Color2, TextLeft, LineT + 1, Width, LineT + 1);
  end;
end;

procedure TCaptionBevel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure TCaptionBevel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure Register;
begin
  RegisterComponents('BM', [TCaptionBevel]);
end;

end.

