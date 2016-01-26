{***************************************************************************}
{  Date: 17.12.97                                            Time: 17:30:40 }
{  Copyright (c)1996-1997                                                   }
{  © by Dr.Plass                                                DanHint3.0  }
{                                                                           }
{                                              for DELPHI 3, Win95 (Not NT!)}
{  feel free to contact me:                                                 }
{  Peter.Plass@FH-Zwickau.de                                                }
{  http://www.fh-zwickau.de/~pp/tm.htm                                      }
{                                                                           }
{  All Rights Reserved.                                                     }
{  This component can be freely used and distributed in commercial and      }
{  private environments, provided this notice is not modified in any way.   }
{                                                                           }
{      based on Dan Ho's component DanHint v1.02 (4.5.96)                   }
{      --> danho@cs.nthu.edu.tw                                             }
{                                                                           }
{                                                                           }
{                                                                           }
{***************************************************************************}

unit Danhint;
interface

{$IFDEF WIN32}
  {$R danhint.r32}
{$ELSE}
  {$R danhint.r16}
{$ENDIF}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms;

const
 cAbout = 'TDanHint3.0(32): © 1997 by Dr.Plass';

type
  THintDirection=(hdUpRight,hdUpLeft,hdDownRight,hdDownLeft);
  TOnSelectHintDirection=procedure(HintControl:TControl;
                         var HintDirection:THintDirection)of object;

  TDanHint = class(TComponent)
  private
   FHintDirection        :THintDirection;
   FHintColor            :TColor;
   FHintShadowColor      :TColor;
   FHintFont             :TFont;
   FHintPauseTime        :Integer;
   FRound                :Integer;
   FAbout                :String;
   FActive               :Boolean;
   FDepth                :Integer;
   FOnSelectHintDirection:TOnSelectHintDirection;

   procedure SetShowHint(Value:Boolean);
   procedure SetHintDirection(Value:THintDirection);
   procedure SetHRound(Value:Integer);
   procedure SetHActive(Value:Boolean);
   procedure SetHDepth(Value:Integer);
   procedure SetHintColor(Value:TColor);
   procedure SetHintShadowColor(Value:TColor);
   procedure SetHintFont(Value:TFont);
   procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
   procedure SetHintPauseTime(Value:Integer);
  protected
   procedure FuAbout(value :String);
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure Loaded;override;
   procedure SetNewHintFont;
  published
   Property About      : String read FAbout write FuAbout;
   property HintDirection:THintDirection read FHintDirection write SetHintDirection default hdUpRight;
   property HintColor:TColor read FHintColor write SetHintColor default clYellow;
   property HintShadowColor:TColor read FHintShadowColor write SetHintShadowColor default clPurple;
   property HintRadius:Integer read FRound write SetHRound default 9;
   property HintWidth:Integer read FDepth write SetHDepth default 100;
   property HintActive:Boolean read FActive write SetHActive default False;
   property HintFont:TFont read FHintFont write SetHintFont;
   property HintPauseTime:Integer read FHintPauseTime write SetHintPauseTime default 1200;
   property OnSelectHintDirection:TOnSelectHintDirection read FOnSelectHintDirection write FOnSelectHintDirection;
  end;

  TNewHint = class(THintWindow)
  private
   FDanHint      :TDanHint;
   FHintDirection:THintDirection;

   procedure SelectProperHintDirection(ARect:TRect);
   procedure CheckUpRight(Spot:TPoint; FirstCall: Boolean);
   procedure CheckUpLeft(Spot:TPoint; FirstCall: Boolean);
   procedure CheckDownRight(Spot:TPoint; FirstCall: Boolean);
   procedure CheckDownLeft(Spot:TPoint; FirstCall: Boolean);
   function FindDanHint:TDanHint;
  protected
   function BetweenToken(var S: String; Sep: Char):String;
   function FindToken(var S: String; Sep: Char): String;
   function TokenCount(S: String; Sep: Char):Integer;
   procedure Paint;override;
   procedure CreateParams(var Params: TCreateParams);override;
   procedure WndProc(var Message: TMessage); override;
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);override;
   property HintDirection:THintDirection read FHintDirection write FHintDirection default hdUpRight;
  published
 end;

procedure Register;

implementation

const
 SHADOW_WIDTH=6;
 N_PIXELS    =0;  {frei zwischen cursor und hint}
var
 MemBmp             :TBitmap;
 UpRect,DownRect    :TRect;
 SelectHintDirection:THintDirection;
 ShowPos            :TPoint;
 HRound             :Integer;
 HActive            :Boolean;
 HDepth             :Integer;

  function GetCursorHeightMargin: Integer;
  var
    IconInfo: TIconInfo;
    BitmapInfoSize: Cardinal;
    BitmapBitsSize: Cardinal;
    Bitmap: PBitmapInfoHeader;
    Bits: Pointer;
    BytesPerScanline, ImageSize: Integer;

      function FindScanline(Source: Pointer; MaxLen: Cardinal;
        Value: Cardinal): Cardinal; assembler;
      asm
              PUSH    ECX
              MOV     ECX,EDX
              MOV     EDX,EDI
              MOV     EDI,EAX
              POP     EAX
              REPE    SCASB
              MOV     EAX,ECX
              MOV     EDI,EDX
      end;

  begin
    { Default value is entire icon height }
    Result := GetSystemMetrics(SM_CYCURSOR);
    if GetIconInfo(GetCursor, IconInfo) then
    try
      GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
      Bitmap := AllocMem(BitmapInfoSize + BitmapBitsSize);
      try
        Bits := Pointer(Cardinal(Bitmap) + BitmapInfoSize);
        if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and
          (Bitmap^.biBitCount = 1) then
        begin
          { Point Bits to the end of this bottom-up bitmap }
          with Bitmap^ do
          begin
            BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
            ImageSize := biWidth * BytesPerScanline;
            Bits := Pointer(Cardinal(Bits) + BitmapBitsSize - ImageSize);
            { Use the width to determine the height since another mask bitmap
              may immediately follow }
            Result := FindScanline(Bits, ImageSize, $FF);
            { In case the and mask is blank, look for an empty scanline in the
              xor mask. }
            if (Result = 0) and (biHeight >= 2 * biWidth) then
              Result := FindScanline(Pointer(Integer(Bits) - ImageSize),
                ImageSize, $00);
            Result := Result div BytesPerScanline;
          end;
          Dec(Result, IconInfo.yHotSpot);
        end;
      finally
        FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
      end;
    finally
      if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
      if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
    end;
  end;

function PosRev(Substr: string; S: string): Integer;
var
  i: Integer;
  Tmp: String;
begin
Result := 0;
for i := Length(S) downto 1 do begin
  Tmp := Copy(S, i, Length(S) - i + 1);
  if Pos(Substr, Tmp) > 0 then begin
    Result := i;
    Break;
  end;
end;
end;

procedure Register;
begin
 RegisterComponents('Plass',[TDanHint]);
end;

procedure TDanHint.SetNewHintFont;
var
 I:Integer;
begin
 for I:=0 to Application.ComponentCount-1 do
  if Application.Components[I] is TNewHint then begin
   TNewHint(Application.Components[I]).Canvas.Font.Assign(FHintFont);
   Exit;
  end;
end;

constructor TDanHint.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FHintDirection       :=hdUpRight;
 FHintColor           :=clYellow;
 FHintShadowColor     :=clPurple;
 Application.HintPause:=FHintPauseTime;
 FHintFont            :=TFont.Create;
 FHintFont.Name       :='Arial';
 FHintPauseTime       :=1000;
 FDepth               :=250;
 HDepth               :=FDepth;
 FRound               :=18;
 HRound               :=FRound;
 HActive              :=FActive;
 FHintFont.Size       :=8;
 FHintFont.Color      :=clBlack;
 FHintFont.Pitch      :=fpDefault;
 FHintFont.Style      :=FHintFont.Style+[fsItalic];
 fAbout               := '';
 SetShowHint(HActive);
end;

destructor TDanHint.Destroy;
begin
 FHintFont.Free;
 inherited Destroy;
end;

procedure TDanHint.Loaded;
begin
 inherited Loaded;
 SetShowHint(FActive);
end;

procedure TDanHint.SetHintDirection(Value:THintDirection);
begin
 if Value <> FHintDirection then
  FHintDirection:=Value;
end;

procedure TDanHint.SetHRound(Value:Integer);
begin
 if Value <> HRound then begin
  FRound:=Value;
  HRound:=FRound;
 end;
end;

procedure TDanHint.SetHActive(Value:Boolean);
begin
 if (csDesigning in ComponentState) then
  //MessageDlg('not allowed!!'+ #10#13+'while designing', mtInformation,[mbOk], 0)
 else if Value <> FActive then begin
  FActive:=Value;
  hActive:=FActive;
  SetShowHint(FActive);
 end;
end;

procedure TDanHint.SetShowHint(Value:Boolean);
begin
 if not (csDesigning in ComponentState) then
  //Value:=False
 begin
   if Value then HintWindowClass :=TNewHint
   else          HintWindowClass :=THintWindow;
   Application.ShowHint:=not Application.ShowHint;
   Application.ShowHint:=not Application.ShowHint;
   SetNewHintFont;
  end;
end;

procedure TDanHint.SetHintColor(Value:TColor);
begin
 if Value <> FHintColor then
  FHintColor:=Value;
end;

procedure TDanHint.SetHintShadowColor(Value:TColor);
begin
 if Value <> FHintShadowColor then
  FHintShadowColor:=Value;
end;

procedure TDanHint.SetHintFont(Value:TFont);
begin
 FHintFont.Assign(Value);
 SetShowHint(FActive);
end;

procedure TDanHint.FuAbout(value:String);
begin
 if value <> fAbout then begin
  fAbout := cAbout;
  {MessageDlg('Komponente: TDanHint3.0[32]         '+ #10#13+ #10#13+
             '©1997 by Dr.Plass                   '+ #10#13+
             'Peter.Plass@fh-zwickau.de           '+ #10#13+
             'http://www.fh-zwickau.de/~pp/tm.htm ',
              mtInformation, [mbOk],0);}
  end;
end;

procedure TDanHint.CMFontChanged(var Message:TMessage);
begin
 inherited;
 SetShowHint(FActive);
end;

procedure TDanHint.SetHDepth(Value:Integer);
begin
 if Value<>FDepth then begin
  if Value > 300 then Value:=300;
  FDepth:=Value;
  hDepth:=FDepth;
 end;
end;

procedure TDanHint.SetHintPauseTime(Value:Integer);
begin
 if Value<>FHintPauseTime then begin
  FHintPauseTime       :=Value;
  Application.HintPause:=Value;
 end;
end;

function TNewHint.FindDanHint:TDanHint;
var
 I:Integer;
begin
 Result:=nil;
 for I:=0 to Application.MainForm.ComponentCount-1 do
  if Application.MainForm.Components[I] is TDanHint then begin
   Result:=TDanHint(Application.MainForm.Components[I]);
   Exit;
  end;
end;

constructor TNewHint.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 ControlStyle:=ControlStyle-[csOpaque];
 with Canvas do begin
  Brush.Style          :=bsClear;
  Brush.Color          :=clBackground;
  {Application.HintColor:=clBackground;}
 end;
 FHintDirection:=hdUpRight;
end;

destructor TNewHint.Destroy;
begin
 inherited Destroy;
end;

procedure TNewHint.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 with Params do begin
  Style := WS_POPUP OR WS_DISABLED {Style-WS_BORDER};
  {Add the above makes the beneath window overlap hint}
  WindowClass.Style := WindowClass.Style OR CS_SAVEBITS;
 end;
end;

procedure TNewHint.WndProc(var Message: TMessage);
begin
if Message.Msg = WM_SETCURSOR then
  ReleaseHandle;
inherited WndProc(Message);
end;

procedure TNewHint.Paint;
var
 R        :TRect;
 //CCaption :String;
 FillRegion,
 ShadowRgn:HRgn;
 AP       :array[0..2] of TPoint; {Points of the Arrow }
 SP       :array[0..2] of TPoint; {Points of the Shadow}
 X,Y      :Integer;
 AddNum   :Integer; {Added num for hdDownXXX }
begin
 R:=ClientRect; {R is for Text output}
 inc(R.Left,14);
 inc(R.Top,8);

 AddNum:=0;
 if FHintDirection>=hdDownRight then AddNum:=15;
 Inc(R.Top,AddNum);

 case HintDirection of
  hdUpRight:begin
             AP[0]:=Point(10,Height-15);
             AP[1]:=Point(20,Height-15);
             AP[2]:=Point(0,Height);

             SP[0]:=Point(12,Height-15);
             SP[1]:=Point(25,Height-15);
             SP[2]:=Point(12,Height);
            end;
  hdUpLeft: begin
             AP[0]:=Point(Width-SHADOW_WIDTH-20,Height-15);
             AP[1]:=Point(Width-SHADOW_WIDTH-10,Height-15);
             AP[2]:=Point(Width-SHADOW_WIDTH,Height);

             SP[0]:=Point(Width-SHADOW_WIDTH-27,Height-15);
             SP[1]:=Point(Width-SHADOW_WIDTH-5,Height-15);
             SP[2]:=Point(Width-SHADOW_WIDTH,Height);
            end;
hdDownRight:begin
             AP[0]:=Point(10,15);
             AP[1]:=Point(20,15);
             AP[2]:=Point(0,0);

             { for hdDownXXX, SP not used now }
             SP[0]:=Point(12,Height-15);
             SP[1]:=Point(25,Height-15);
             SP[2]:=Point(12,Height);
            end;
hdDownLeft: begin
             AP[0]:=Point(Width-SHADOW_WIDTH-20,15);
             AP[1]:=Point(Width-SHADOW_WIDTH-10,15);
             AP[2]:=Point(Width-SHADOW_WIDTH,0);

             { for hdDownXXX, SP not used now }
             SP[0]:=Point(12,Height-15);
             SP[1]:=Point(25,Height-15);
             SP[2]:=Point(12,Height);
            end;
  end;

  {Draw Shadow of the Hint Rect}
  if (FHintDirection<=hdUpLeft) then begin
   ShadowRgn:=CreateRoundRectRgn(10,8,Width,Height-8,HRound-1,HRound-1);
   for X:=Width-SHADOW_WIDTH-8 to Width do
    for Y:=8 to Height-14 do begin
     if (Odd(X)=Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
      MemBmp.Canvas.Pixels[X,Y]:=FDanHint.HintShadowColor;
    end;
   for X:=10 to Width do
    for Y:=Height-14 to Height-9 do begin
     if (Odd(X)=Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
       MemBmp.Canvas.Pixels[X,Y]:=FDanHint.HintShadowColor;
    end;


  end else begin { for hdDownXXX }
   ShadowRgn:=CreateRoundRectRgn(10,8+15,Width,Height-2,HRound-1,HRound-1);
   for X:=Width-SHADOW_WIDTH-8 to Width do
    for Y:=8+15 to Height-8 do begin
     if (Odd(X)=Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
      MemBmp.Canvas.Pixels[X,Y]:=FDanHint.HintShadowColor;
    end;
   for X:=10 to Width do
    for Y:=Height-8 to Height-2 do begin
     if (Odd(X)=Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
      MemBmp.Canvas.Pixels[X,Y]:=FDanHint.HintShadowColor;
    end;
  end;
  DeleteObject(ShadowRgn);

  { Draw the shadow of the arrow }
  if (HintDirection<=hdUpLeft) then begin
   ShadowRgn:=CreatePolygonRgn(SP,3,WINDING);
    for X:=SP[0].X to SP[1].X do
     for Y:=SP[0].Y to SP[2].Y do begin
      if (Odd(X)=Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
       MemBmp.Canvas.Pixels[X,Y]:=FDanHint.HintShadowColor;
     end;
     DeleteObject(ShadowRgn);
   end;

  { Draw HintRect }
  MemBmp.Canvas.Pen.Color:=clBlack;
  MemBmp.Canvas.Pen.Style:=psSolid;
  MemBmp.Canvas.Brush.Color:=FDanHint.HintColor;
  MemBmp.Canvas.Brush.Style:=bsSolid;
  if (FHintDirection<=hdUpLeft) then
       MemBmp.Canvas.RoundRect(0,0,Width-SHADOW_WIDTH,Height-14,HRound,HRound)
  else MemBmp.Canvas.RoundRect(0,0+AddNum,Width-SHADOW_WIDTH,Height-14+6,HRound,HRound);

  { Draw Hint Arrow }
  MemBmp.Canvas.Pen.Color:=FDanHint.HintColor;
  MemBmp.Canvas.MoveTo(AP[0].X,AP[0].Y);
  MemBmp.Canvas.LineTo(AP[1].X,AP[1].Y);
  MemBmp.Canvas.Pen.Color:=clBlack;
  FillRegion:=CreatePolygonRgn(AP,3,WINDING);
  FillRgn(MemBmp.Canvas.Handle,FillRegion,MemBmp.Canvas.Brush.Handle);
  DeleteObject(FillRegion);
  MemBmp.Canvas.LineTo(AP[2].X,AP[2].Y);
  MemBmp.Canvas.LineTo(AP[0].X,AP[0].Y);

  {SetBkMode makes DrawText's text be transparent }

  SetBkMode(MemBmp.Canvas.Handle,TRANSPARENT);
  MemBmp.Canvas.Font.Assign(FDanHint.HintFont);
  DrawText(MemBmp.Canvas.Handle, PChar(Caption), -1, R,
    DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  Canvas.CopyMode:=cmSrcCopy;
  Canvas.CopyRect(ClientRect,MemBmp.Canvas,ClientRect);
  MemBmp.Free;
end;

procedure TNewHint.CheckUpLeft(Spot:TPoint; FirstCall: Boolean);
var
 Width,Height:Integer;
begin
 Dec(Spot.Y,N_PIXELS);
 Width:=UpRect.Right-UpRect.Left;
 Height:=UpRect.Bottom-UpRect.Top;
 if FirstCall and ((Spot.X+SHADOW_WIDTH-Width)<0) then begin
  Inc(Spot.Y,N_PIXELS);{back tp original}
  CheckUpRight(Spot, False);
  Exit;
 end;
 if (Spot.Y-Height)<0 then begin
  Inc(Spot.Y,N_PIXELS);
  CheckDownLeft(Spot, False);
  Exit;
 end;
 SelectHintDirection:=hdUpLeft;
 ShowPos.X:=Spot.X+SHADOW_WIDTH-Width;
 ShowPos.Y:=Spot.Y-Height;
end;

procedure TNewHint.CheckUpRight(Spot:TPoint; FirstCall: Boolean);
var
 Width,Height:Integer;
begin
 Dec(Spot.Y,N_PIXELS);
 Width:=UpRect.Right-UpRect.Left;
 Height:=UpRect.Bottom-UpRect.Top;
 if FirstCall and ((Spot.X+Width)>Screen.Width) then begin
  Inc(Spot.Y,N_PIXELS);
  CheckUpLeft(Spot, False);
  Exit;
 end;
 if (Spot.Y-Height)<0 then begin
  Inc(Spot.Y,N_PIXELS);
  CheckDownRight(Spot, False);
  Exit;
 end;
 SelectHintDirection:=hdUpRight;
 ShowPos.X:=Spot.X;
 ShowPos.Y:=Spot.Y-Height;
end;

procedure TNewHint.CheckDownRight(Spot:TPoint; FirstCall: Boolean);
var
 Width,Height:Integer;
begin
 Inc(Spot.Y,N_PIXELS*3);
 Width:=DownRect.Right-DownRect.Left;
 Height:=DownRect.Bottom-DownRect.Top;
 if FirstCall and ((Spot.X-SHADOW_WIDTH+Width)>Screen.Width) then begin
  Dec(Spot.Y,N_PIXELS*3);
  CheckDownLeft(Spot, False);
  Exit;
 end;
 if (Spot.Y+Height)>Screen.Height then begin
  Dec(Spot.Y,N_PIXELS*3);
  CheckUpRight(Spot, False);
  Exit;
 end;
 SelectHintDirection:=hdDownRight;
 ShowPos.X:=Spot.X;
 ShowPos.Y:=Spot.Y;
end;

procedure TNewHint.CheckDownLeft(Spot:TPoint; FirstCall: Boolean);
var
 Width,Height:Integer;
begin
 Inc(Spot.Y,N_PIXELS*3);
 Width:=DownRect.Right-DownRect.Left;
 Height:=DownRect.Bottom-DownRect.Top;
 if FirstCall and ((Spot.X+SHADOW_WIDTH-Width)<0) then begin
  Dec(Spot.Y,N_PIXELS*3);
  CheckDownRight(Spot, False);
  Exit;
 end;
 if (Spot.Y+Height)>Screen.Height then begin
  Dec(Spot.Y,N_PIXELS*3);
  CheckUpLeft(Spot, False);
  Exit;
 end;
 SelectHintDirection:=hdDownLeft;
 ShowPos.X:=Spot.X+SHADOW_WIDTH-Width;
 ShowPos.Y:=Spot.Y;
end;

procedure TNewHint.SelectProperHintDirection(ARect:TRect);
var
 Spot             :TPoint;
 OldHintDirection,
 SendHintDirection:THintDirection;
 HintControl      :TControl;
begin
 GetCursorPos(Spot);
 //Spot.y := Spot.y + GetSystemMetrics(SM_CYCURSOR);
 HintCOntrol:=FindDragTarget(Spot,True);
 Inc(ARect.Right,20+SHADOW_WIDTH);
 Inc(ARect.Bottom,30);
 UpRect:=ARect;
 Inc(ARect.Bottom,9);
 DownRect:=ARect;
 OldHintDirection :=FDanHint.HintDirection;
 SendHintDirection:=FDanHint.HintDirection;

 {Tricky, why here can't use FDanHint.OnSe...? }
 if Assigned(FDanHint.FOnSelectHintDirection) then begin
  FDanHint.FOnSelectHintDirection(HintControl,SendHintDirection);
  FDanHint.HintDirection:=SendHintDirection;
 end;
 case FDanHint.HintDirection of
  hdUpRight:CheckUpRight(Spot, True);
   hdUpLeft:CheckUpLeft(Spot, True);
hdDownRight:CheckDownRight(Spot, True);
 hdDownLeft:CheckDownLeft(Spot, True);
 end;
 FDanHint.HintDirection:=OldHintDirection;
end;

function TNewHint.FindToken(var S: String; Sep: Char): String;
var               {S. rechts nach token Result.links vor}
 I : Word;
begin
 I:=Pos(Sep,S);
 if I<>0 then begin
  Result:=Copy(S,1,I-1);
  Delete(S,1,I);
 end else begin
  Result:=S;
  S:='';
 end;
end;

function TNewHint.BetweenToken(var S: String; Sep: Char):String;
var
 Token: String;
begin
 Token := FindToken(S,Sep);
 Result:= FindToken(S,Sep);
end;

function TNewHint.TokenCount(S: String; Sep: Char):Integer;
begin
 Result:=0;
 while S<>''do begin
  FindToken(S,Sep);
  inc(Result);
 end;
 dec(Result);
end;

procedure TNewHint.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
var
 ScreenDC :HDC;
 LeftTop  :TPoint;
 tmpWidth,
 i,temp,
 old,z,t,
 new,korr,
 tmpHeight:Integer;
 s2       :String;
 s        :TStringList;
begin

 if NOT HActive then exit;
  if hDepth>300 then hDepth:=300;
 MemBmp :=TBitmap.Create;
 Caption:=AHint;
 s :=TStringList.Create;
 s2:=' '+ AHint;

 with Rect do begin
  tmpWidth :=Right -Left;
  tmpHeight:=Bottom-Top;

  i:=Canvas.TextHeight(AHint);
  korr:=round(tmpHeight/i)-1;
  dec(korr,TokenCount(AHint,#13));

  if(tmpWidth)>hDepth then begin
    caption:='**';i:=0;
    while (caption<>'') do begin
     caption:=BetweenToken(s2,' ');
     if s2<>' 'then s2:=' '+s2;
     if caption<>''then begin
      s.add(caption);inc(i);
     end;
    end;

    old:=0;temp:=0;
    for z:=0 to i-1 do begin
     temp:=Canvas.TextWidth(s.strings[z])+6;
     if temp>old then old:=temp;
    end;
    if temp>hDepth then temp:=old
    else temp:=hDepth;

    old:=-1;new:=0;z:=0;
    while z<i do begin
     s2:=s.strings[z];t:=z+1;

     while (Canvas.TextWidth(s2)<=(temp-6)) AND (t<i) do begin
      s2:=s2+' '+s[t];inc(t);
     end;

     if (t>z+1) AND (t<=i) AND (Canvas.TextWidth(s2)>(temp-6)) then begin
      Delete(s2,PosRev(s[t-1],s2)-1,length(s[t-1])+1);
      z:=t-2;dec(t);
     end;
     caption:=caption+s2;
     if ((z<i-1) AND (t<i)) then caption:=caption+#13;
     if Canvas.TextWidth(s2)+6>new then new:=Canvas.TextWidth(s2)+6;
     inc(old);
     if (z>=i-1) or (t>=i) then break;
     inc(z);
    end;

    s.Free;
    Right:=Left+new+6;
    INC(Bottom,(old-korr)*Canvas.TextHeight(AHint));
   end; {there's no reason to do}
 end; {with rect}

 {add by Dan from Here }
 FDanHint:=FindDanHint;
 SelectProperHintDirection(Rect);
 HintDirection:=SelectHintDirection;

 {if the following changes, make sure to modify
  SelectProperHintDirection also}
 Inc(Rect.Right,20+SHADOW_WIDTH);
 Inc(Rect.Bottom,30);
 if (FHintDirection>=hdDownRight) then Inc(Rect.Bottom,9);

 {to expand the rect}
 tmpWidth   :=Rect.Right-Rect.Left;
 tmpHeight  :=Rect.Bottom-Rect.Top;
 Rect.Left  :=ShowPos.X;
 if FHintDirection >= hdDownRight then begin
   Inc(ShowPos.y, GetCursorHeightMargin);
   if AData <> nil then ShowPos.y := Integer(Pointer(AData));
 end;
 Rect.Top   :=ShowPos.Y;
 Rect.Right :=Rect.Left+tmpWidth;
 Rect.Bottom:=Rect.Top+tmpHeight;
 BoundsRect :=Rect;

 MemBmp.Width :=Width;
 MemBmp.Height:=Height;


 ScreenDC:=CreateDC( 'DISPLAY',nil,nil,nil);
 LeftTop.X:=0;
 LeftTop.Y:=0;
 LeftTop:=ClientToScreen(LeftTop);
 {use MemBmp to store the original bitmap on screen }
 BitBlt(MemBmp.Canvas.Handle,0,0,Width,Height,ScreenDC,LeftTop.X,LeftTop.Y,SRCCOPY);
 {SetBkMode(Canvas.Handle,TRANSPARENT);}
 SetWindowPos(Handle, HWND_TOPMOST, ShowPos.X, ShowPos.Y, 0,0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
 BitBlt(Canvas.Handle,0,0,Width,Height,MemBmp.Canvas.Handle,0,0,SRCCOPY);
 DeleteDC(ScreenDC);
end;
end.





