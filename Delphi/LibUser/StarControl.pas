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
 * TStarControl VCL component
 * Loosely based on CIEStarControl by ieaeitsch -
 * http://www.codeproject.com/Articles/5886/StarControl
 *
 ****************************************************************************)

unit StarControl;

interface

uses Windows, Messages, Classes, StdCtrls, Controls, ImgList, Graphics, Math,
    Common2;

type
  TStarState = (stStar, stStarBorder, stRed, stBorder, stThickBorder);

type
  TStarControl = class(TCustomStaticText)
  private
    FCanvas: TCanvas;
    FStars: Integer;
    FMaxNumberOfStars: Integer;
    FMouseOver: Boolean;
    FStarsThickBorder: Integer;
    FAutoSize: Boolean;
    FImageChangeLink: TChangeLink;
    FStarImages: TCustomImageList;
    FStarSpace: Integer;
    FOnChange: TNotifyEvent;
    //procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic);
        message CN_CTLCOLORSTATIC;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    //procedure DrawStar(nID: Cardinal; const rctDest, rctSrc: TRect);
    procedure SetNumberOfStars(Stars: Integer);
    procedure SetMaxNumberOfStars(Stars: Integer);
    procedure SetAutoSize(Value: Boolean);
    procedure SetStarImages(Value: TCustomImageList);
    procedure SetStarSpace(Value: Integer);
    procedure ImageListChange(Sender: TObject);
  protected
    procedure Paint; virtual;
    //procedure PaintWindow(DC: HDC); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
    procedure AdjustBounds;
    procedure Loaded; override;
    procedure Change; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Stars: Integer read FStars write SetNumberOfStars default 0;
    property MaxNumberOfStars: Integer read FMaxNumberOfStars
        write SetMaxNumberOfStars default 5;
    property StarSpace: Integer read FStarSpace write SetStarSpace
        default 2;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property StarImages: TCustomImageList read FStarImages write SetStarImages;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Enabled;
  end;

procedure Register;

implementation

uses HandCursor;

constructor TStarControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FStars := 0;
  FMaxNumberOfStars := 5;
  FStarsThickBorder := 0;
  FMouseOver := False;
  FAutoSize := True;
  FStarSpace := 2;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  DoubleBuffered := True;
  LoadHandCursor;
  Cursor := crHand;
  inherited AutoSize := False;
  AdjustBounds;
end;

destructor TStarControl.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

{procedure TStarControl.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;}

procedure TStarControl.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := Message.ChildDC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
  Message.Result := GetStockObject(NULL_BRUSH);
end;

procedure TStarControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

{procedure TStarControl.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;}

procedure TStarControl.Paint;
  procedure DrawStar(State: TStarState; NumStar, YPos: Integer);
  var
    X: Integer;
  begin
    X := (FStarImages.Width + FStarSpace) * NumStar;
    FStarImages.Draw(Canvas, X, YPos, Integer(State), Enabled);
  end;
var
  ClientRect: TRect;
  YPos: Integer;
  i: Integer;
begin
  ClientRect := GetClientRect;
//  Canvas.Brush.Style := bsSolid;
//  Canvas.Brush.Color := Color;
//  Canvas.FillRect(ClientRect);
  if not Assigned(FStarImages)
      or (FStarImages.Count <= Integer(High(TStarState))) then
    Exit;
  // calcuate the horizontal position
  YPos := ((ClientRect.Bottom - ClientRect.Top) - FStarImages.Height) div 2;
  // paint the stars
  if not FMouseOver then begin
    // if no mouse is over then paint only the numer of stars and finish
    for i := 0 to FStars - 1 do begin
      // do we paint the last possible star
      if i < FMaxNumberOfStars - 1 then begin
        // a yellow star - it's not the last possible star
        //DrawStar(IDB_BITMAP_STAR, GetDestRect(i, YPos), SourceRect)
        DrawStar(stStar, i, YPos);
      end
      else if FMaxNumberOfStars < FStars then begin
        // a red star, because we should paint more stars than possible
        //DrawStar(IDB_BITMAP_STAR_RED, GetDestRect(i, YPos), SourceRect);
        DrawStar(stRed, i, YPos);
        Break;
      end
      else begin
        // a yellow star - it's the last possible star and the last we should paint
        //DrawStar(IDB_BITMAP_STAR, GetDestRect(i, YPos), SourceRect);
        DrawStar(stStar, i, YPos);
      end;
    end;
    for i := FStars to FMaxNumberOfStars - 1 do begin
      //DrawStar(IDB_BITMAP_BORDER, GetDestRect(i, YPos), SourceRect);
      DrawStar(stBorder, i, YPos);
    end;
  end
  else begin
    // mouse is over the control
    // so paint first the stars with thick border
    // that are all from the left to the mouseposition (calculated in MouseMove)
    for i := 0 to FStarsThickBorder - 1 do begin
      // decide whether we have to take a thick border with start
      if i < FStars then begin
        //DrawStar(IDB_BITMAP_STAR_BORDER, GetDestRect(i, YPos), SourceRect)
        DrawStar(stStarBorder, i, YPos);
      end
      // or without star
      else begin
        //DrawStar(IDB_BITMAP_BORDER_THICK, GetDestRect(i, YPos), SourceRect);
        DrawStar(stStarBorder, i, YPos);
      end;
    end;
    // so now we have to fill up with thin borders
    for i := FStarsThickBorder to FMaxNumberOfStars - 1 do begin
      // decide whether we have to take a thin border with star
      if i < FStars then begin
        if (i = FMaxNumberOfStars - 1) and (FStars > FMaxNumberOfStars) then begin
          // a red star
          //DrawStar(IDB_BITMAP_STAR_RED, GetDestRect(i, YPos), SourceRect)
          DrawStar(stRed, i, YPos);
        end
        else begin
          // a yellow star
          //DrawStar(IDB_BITMAP_STAR, GetDestRect(i, YPos), SourceRect);
          DrawStar(stStar, i, YPos);
        end;
      end
      // or without star
      else begin
        //DrawStar(IDB_BITMAP_BORDER, GetDestRect(i, YPos), SourceRect);
        DrawStar(stThickBorder, i, YPos);
      end;
    end;
  end;
end;

// draw the star
{procedure TStarControl.DrawStar(nID: Cardinal; const rctDest, rctSrc: TRect);
var
  dcStar: HDC;
  bmpStar, bmpOld: HBITMAP;
begin
  // create a compatible dc
  dcStar := CreateCompatibleDC(FCanvas.Handle);
  try
    // load the bitmap
    bmpStar := LoadBitmap(hInstance, MAKEINTRESOURCE(nID));
    if bmpStar = 0 then
      Exit;
    try
      // select the bitmap
      bmpOld := SelectObject(dcStar, bmpStar);
      try
        // draw the star
        Common2.TransparentBlt(FCanvas.Handle,
            rctDest.left,
            rctDest.top,
            rctDest.right,
            rctDest.bottom,
            dcStar,
            rctSrc.left,
            rctSrc.top,
            rctSrc.right,
            rctSrc.bottom,
            RGB(255, 255, 255));
      finally
        // select the old bitmap
        SelectObject(dcStar, bmpOld);
      end;
    finally
      DeleteObject(bmpStar);
    end;
  finally
    DeleteDC(dcStar);
  end;
end;}

procedure TStarControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  if not Assigned(FStarImages) then
    Exit;
  Pt := Point(X, Y);
  // if the mouse is over the star control...
  if PtInRect(ClientRect, Pt) then begin
    // ...set the flag to true and...
    FMouseOver := True;
    // ...capture the mouse and...
    SetCapture(Handle);
    // set the mouse icon
    //::SetCursor( ::AfxGetApp()->LoadStandardCursor( IDC_HAND ) );
    // ..calculate the number of thick borders to paint
    //Dec(Pt.x, (FStarImages.Width {/* + IE_STARCONTROL_STARS_SPACE */}) div 3);
    FStarsThickBorder := Pt.x div (FStarImages.Width + FStarSpace) + 1;
    // check the maximum number of stars
    FStarsThickBorder := Min(FStarsThickBorder, FMaxNumberOfStars);
  end
  // if the mouse left the area of our star control...
  else begin
    // ...set the flag to false and release the mouse
    FMouseOver := False;
    ReleaseCapture;
    // set the mouse icon
    //::SetCursor( ::AfxGetApp()->LoadStandardCursor( IDC_ARROW ) );
  end;
  // redraw the window to see the changes
  Repaint;
  inherited;
end;

// it got clicked with the mouse over the control so update the number of stars
procedure TStarControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  if FStars <> FStarsThickBorder then begin
    FStars := FStarsThickBorder;
    Change;
    // redraw the window to see the new number of stars in the control
    Repaint;
  end;
  inherited;
end;

procedure TStarControl.SetNumberOfStars(Stars: Integer);
begin
  FStars := Stars;
  Repaint;
end;

procedure TStarControl.SetMaxNumberOfStars(Stars: Integer);
begin
  FMaxNumberOfStars := Stars;
  Repaint;
  // resize the control if we should
  AdjustBounds;
end;

procedure TStarControl.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if Value then
      AdjustBounds;
  end;
end;

procedure TStarControl.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TStarControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TStarControl.AdjustBounds;
var
  cx, cy: Integer;
begin
  if not (csReading in ComponentState) and FAutoSize
      and Assigned(FStarImages) then begin
    // resize the control to fit the number of stars
    cx := FMaxNumberOfStars * FStarImages.Width +
        (FMaxNumberOfStars - 1) * FStarSpace;
    cy := FStarImages.Height;
    SetBounds(Left, Top, cx, cy);
  end;
end;

procedure TStarControl.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = StarImages) then
    StarImages := nil;
end;

procedure TStarControl.SetStarImages(Value: TCustomImageList);
begin
  if StarImages <> nil then
    StarImages.UnRegisterChanges(FImageChangeLink);
  FStarImages := Value;
  if StarImages <> nil then begin
    StarImages.RegisterChanges(FImageChangeLink);
    StarImages.FreeNotification(Self);
  end;
  Repaint;
  AdjustBounds;
end;

procedure TStarControl.SetStarSpace(Value: Integer);
begin
  FStarSpace := Value;
  Repaint;
  AdjustBounds;
end;

procedure TStarControl.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure Register;
begin
  RegisterComponents('BM', [TStarControl]);
end;

end.
