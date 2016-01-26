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
 * TMarqueeBar VCL component
 *
 ****************************************************************************)

unit MarqueeBar;

interface

uses Classes, Controls, ExtCtrls, Windows, Messages, Graphics, ComCtrls,
    CommCtrl;

const
  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = (WM_USER + 10);

  CCM_FIRST          = $2000;          // Common control shared messages
  CCM_SETBKCOLOR     = CCM_FIRST + 1;  // lParam is bkColor

  PBM_SETBARCOLOR    = WM_USER+9;      // lParam = bar color
  PBM_SETBKCOLOR     = CCM_SETBKCOLOR; // lParam = bkColor

  DEF_COLOR     = clBtnFace;
  DEF_FORECOLOR = clHighlight;
  DEF_INTERVAL  = 500;

type
  TBorderStyle = (bsNone, bsSingle);
  TMBStyle = (mbComCtl6, mbComCtlOld);

type
  TMarqueeBar = class(TWinControl)
  private
    FAnimate: Boolean;
    FBorderStyle: TBorderStyle;
    FColor: TColor;
    FForeColor: TColor;
    FInterval: Cardinal;
    FStyle: TMBStyle;
    FTimer: TTimer;
    ComCtl: Boolean;
    CellHeight, CellWidth, Cells, Margin: Integer;
    Position: Integer;
    FCanvas: TCanvas;
    procedure SetAnimate(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColor(Value: TColor);
    procedure SetForeColor(Value: TColor);
    procedure SetInterval(Value: Cardinal);
    procedure SetStyle(Value: TMBStyle);
    procedure CalcCellSize;
    procedure OnTimer(Sender: TObject);
    procedure DoAnimate;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Align;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
        default bsSingle;
    property Color: TColor read FColor write SetColor default DEF_COLOR;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ForeColor: TColor read FForeColor write SetForeColor
        default DEF_FORECOLOR;
    property Hint;
    property Interval: Cardinal read FInterval write SetInterval
        default DEF_INTERVAL;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style: TMBStyle read FStyle write SetStyle default mbComCtl6;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Frame3D(Canvas: TCanvas; var Rect: TRect;
    TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

procedure Register;
begin
  RegisterComponents('BM', [TMarqueeBar]);
end;

//
// TMarqueeBar
//

constructor TMarqueeBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := ControlStyle + [csOpaque];
  Width := 150;
  Height := GetSystemMetrics(SM_CYVSCROLL);
  FInterval := DEF_INTERVAL;
  FAnimate := False;
  FBorderStyle := bsSingle;
  FColor := DEF_COLOR;
  FForeColor := DEF_FORECOLOR;
  FStyle := mbComCtl6;
  ComCtl := True;
  Position := -1;
end;

destructor TMarqueeBar.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TMarqueeBar.SetInterval(Value: Cardinal);
begin
  FInterval := Value;
  if FAnimate and not (csDesigning in ComponentState) then
    FTimer.Interval := Value;
end;

procedure TMarqueeBar.SetAnimate(Value: Boolean);
begin
  if Value <> FAnimate then begin
    if not (csDesigning in ComponentState) then begin
      if Value then begin
        FTimer := TTimer.Create(Self);
        FTimer.Interval := FInterval;
        //Position := -1;
        FTimer.OnTimer := OnTimer;
        FTimer.Enabled := True;
      end
      else
        FTimer.Free;
    end;
    FAnimate := Value;
  end;
end;

procedure TMarqueeBar.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  if not ComCtl then
    Paint;
end;

procedure TMarqueeBar.SetColor(Value: TColor);
begin
  FColor := Value;
  if ComCtl then begin
    if HandleAllocated then
      SendMessage(Handle, PBM_SETBKCOLOR, 0, ColorToRGB(Value));
  end
  else
    Paint;
end;

procedure TMarqueeBar.SetForeColor(Value: TColor);
begin
  FForeColor := Value;
  if ComCtl then begin
    if HandleAllocated then
      SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(Value));
  end
  else
    Paint;
end;

procedure TMarqueeBar.SetStyle(Value: TMBStyle);
begin
  if Value <> FStyle then begin
    FStyle := Value;
    ComCtl := (FStyle = mbComCtl6);
    RecreateWnd;
  end;
end;

procedure TMarqueeBar.CalcCellSize;
begin
  CellHeight := Height + 1 - Margin * 2;
  CellWidth := Round(Int(CellHeight * (2 / 3)));
  Cells := Round(Int((Width - Margin * 2 + 1) / (CellWidth + 1) + 1));
end;

procedure TMarqueeBar.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TMarqueeBar.PaintWindow(DC: HDC);
begin
  if ComCtl then
    inherited PaintWindow(DC)
  else begin
    FCanvas.Lock;
    try
      FCanvas.Handle := DC;
      try
        Paint;
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
    end;
  end;
end;

procedure TMarqueeBar.Paint;
var
  Rect: TRect;
begin
  if ComCtl then
    Exit;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FColor;
  Canvas.FillRect(ClientRect);
  if FBorderStyle = bsSingle then begin
    Rect := ClientRect;
    Canvas.Pen.Style := psSolid;
    Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
    Margin := 2;
  end
  else
    Margin := 0;
  CalcCellSize;
  Canvas.Pen.Style := psClear;
  DoAnimate;
end;

procedure TMarqueeBar.CreateParams(var Params: TCreateParams);
begin
  if ComCtl then begin
    if not InitCommonControl(ICC_PROGRESS_CLASS) then
      Style := mbComCtlOld;
  end;
  inherited CreateParams(Params);
  if ComCtl then begin
    InitCommonControls;
    CreateSubClass(Params, PROGRESS_CLASS);
    Params.Style := Params.Style or PBS_MARQUEE;
  end;
end;

procedure TMarqueeBar.CreateWnd;
begin
  inherited CreateWnd;
  if ComCtl then begin
    if csDesigning in ComponentState then begin
      SendMessage(Handle, PBM_SETRANGE32, 0, 100);
      SendMessage(Handle, PBM_SETPOS, 50, 0);
    end;
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FForeColor));
    SendMessage(Handle, PBM_SETBKCOLOR, 0, ColorToRGB(FColor));
  end;
end;

procedure TMarqueeBar.OnTimer(Sender: TObject);
begin
  if ComCtl then
    SendMessage(Handle, PBM_SETPOS, 100, 0)
  else begin
    Inc(Position);
    DoAnimate;
  end;
end;

procedure TMarqueeBar.DoAnimate;
const
  n = 5;
var
  i: Integer;
  x, w: Integer;
  Wrap: Boolean;
begin
  if Position = Cells - 2 then
    Position := -2;
  x := Margin;
  for i := 1 to Cells do begin
    Wrap := (Position + n) - Cells > i - 1;
    if ( (Position < i) and (i <= (Position + n)) ) or Wrap then
      Canvas.Brush.Color := FForeColor
    else
      Canvas.Brush.Color := FColor;
    w := CellWidth;
    if x + w > Width - Margin + 1 then
      w := Width - x - Margin + 1;
    Canvas.Rectangle(x, Margin, x + w, Margin + CellHeight);
    x := x + CellWidth + 1;
  end;
end;

procedure TMarqueeBar.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
var
  Br: HBRUSH;
begin
  if ComCtl and (csDesigning in ComponentState) then begin
    Br := CreateSolidBrush(ColorToRGB(FColor));
    try
      FillRect(Msg.DC, ClientRect, Br);
    finally
      DeleteObject(Br);
    end;
  end;
  Msg.Result := 1;
end;

end.
