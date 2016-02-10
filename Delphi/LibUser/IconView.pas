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
 * TIconView VCL component
 *
 ****************************************************************************)

{$I Compilers.inc}

unit IconView;

interface

uses Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, Forms,
    ThemeSrv;

type
  TCustomIconView = class(TWinControl)
  private
    fBorderStyle: TStaticBorderStyle;
    fIconHandle: HICON;
    fIconHeight: Integer;
    fIconWidth: Integer;
    fReleaseHandle: Boolean;
    fTransparent: Boolean;
    FCanvas: TCanvas;
    FOldHandle: HWND;
    FNewWndProc, FOldWndProc: Pointer;
    FBrush: hBrush;
    //FThemed: Boolean;
    procedure SetBorderStyle(Value: TStaticBorderStyle);
    procedure SetIconHandle(Value: HICON);
    procedure SetTransparent(Value: Boolean);
    procedure NewWndProc(var Message: TMessage);
    procedure UpdateBackgroundBrush;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CNCtlColorStatic(var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    property BorderStyle: TStaticBorderStyle
        read fBorderStyle write SetBorderStyle default sbsNone;
    property ReleaseHandle: Boolean
        read fReleaseHandle write fReleaseHandle default True;
    property Transparent: Boolean
        read fTransparent Write SetTransparent default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IconHandle: HICON read fIconHandle write SetIconHandle;
    property ImageHeight: Integer read fIconHeight;
    property ImageWidth: Integer read fIconWidth;
    procedure LoadFromResourceIDWithInstance(hMod: Integer; ResID: Integer;
        Size: Integer);
    procedure LoadFromResourceID(ResID: Integer; Size: Integer);
    procedure LoadFromResourceNameWithInstance(hMod: Integer;
        const ResName: string; Size: Integer);
    procedure LoadFromResourceName(const ResName: string; Size: Integer);
    procedure SetParent(AParent: TWinControl); override;
  published
    property Width default 32;
    property Height default 32;
  end;

  TIconView = class(TCustomIconView)
  published
    property Align;
    {$IFDEF DELPHI_4_UP}
    property Anchors;
    {$ENDIF}
    {$IFDEF DELPHI_4_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property BorderStyle;
    property Color;
    {$IFDEF DELPHI_4_UP}
    property Constraints;
    {$ENDIF}
    property DragCursor;
    {$IFDEF DELPHI_4_UP}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ReleaseHandle;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    {$IFDEF DELPHI_5_UP}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF DELPHI_4_UP}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI_4_UP}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BM', [TIconView]);
end;

{ TIconView }

constructor TCustomIconView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := [csCaptureMouse, csClickEvents, csReplicatable, csDoubleClicks];
  Width := 32;
  Height := 32;
  fReleaseHandle := True;
  fTransparent := False;
  fOldHandle := 0;
  fNewWndProc := MakeObjectInstance(NewWndProc);
  fOldWndProc := nil;
end;

destructor TCustomIconView.Destroy;
begin
  if fOldHandle <> 0 then
    SetWindowLong(fOldHandle, GWL_WNDPROC, Integer(FOldWndProc));
  FreeObjectInstance(fNewWndProc);
  fNewWndProc := nil;
  IconHandle := 0;
  fCanvas.Free;
  inherited Destroy;
end;

procedure TCustomIconView.CreateParams(var Params: TCreateParams);
const
  Borders: array[TStaticBorderStyle] of DWORD = (0, WS_BORDER, SS_SUNKEN);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  Params.Style := Params.Style or SS_NOTIFY or SS_ICON or SS_CENTERIMAGE or
      Borders[BorderStyle];
  //UpdateBackgroundBrush;
  //SetWindowLong(Parent.Handle, GWL_STYLE,
  //    GetWindowLong(Parent.Handle, GWL_STYLE) and not WS_CLIPCHILDREN);
end;

procedure TCustomIconView.CreateWnd;
begin
  inherited CreateWnd;
  if IconHandle <> 0 then
    SendMessage(Handle, STM_SETIMAGE, IMAGE_ICON, IconHandle);
  UpdateBackgroundBrush;
end;

procedure TCustomIconView.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomIconView.PaintWindow(DC: HDC);
begin
  inherited;
  fCanvas.Lock;
  try
    fCanvas.Handle := DC;
    try
      Self.Paint;
    finally
      fCanvas.Handle := 0;
    end;
  finally
    fCanvas.Unlock;
  end;
end;

procedure TCustomIconView.Paint;
begin
  if (csDesigning in ComponentState) and (fBorderStyle = sbsNone) then begin
    with Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  end;
end;


procedure TCustomIconView.SetBorderStyle(Value: TStaticBorderStyle);
begin
  if BorderStyle <> Value then begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomIconView.SetIconHandle(Value: HICON);
var
  OldIconHandle: THandle;
begin
  if IconHandle <> Value then begin
    OldIconHandle := IconHandle;
    fIconHandle := Value;
    if HandleAllocated then
      SendMessage(Handle, STM_SETIMAGE, IMAGE_ICON, IconHandle);
    //RecreateWnd;
    //UpdateBackgroundBrush;
    if (OldIconHandle <> 0) and ReleaseHandle then
      DestroyIcon(OldIconHandle);
  end;
end;

procedure TCustomIconView.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then begin
    fTransparent := Value;
    RecreateWnd;
  end;
end;

procedure TCustomIconView.LoadFromResourceIDWithInstance(hMod: Integer;
    ResID: Integer; Size: Integer);
begin
  SetIconHandle(LoadImage(hMod, PChar(ResID), IMAGE_ICON, Size, Size,
      LR_DEFAULTCOLOR));
end;

procedure TCustomIconView.LoadFromResourceID(ResID: Integer; Size: Integer);
begin
  LoadFromResourceIDWithInstance(hInstance, ResID, Size);
end;

procedure TCustomIconView.LoadFromResourceNameWithInstance(hMod: Integer;
    const ResName: string; Size: Integer);
begin
  SetIconHandle(LoadImage(hMod, PChar(ResName), IMAGE_ICON, Size, Size,
      LR_DEFAULTCOLOR));
end;

procedure TCustomIconView.LoadFromResourceName(const ResName: string; Size: Integer);
begin
  LoadFromResourceNameWithInstance(hInstance, ResName, Size);
end;

procedure TCustomIconView.SetParent(AParent: TWinControl);
begin
  if fOldHandle <> 0 then
    SetWindowLong(fOldHandle, GWL_WNDPROC, Integer(fOldWndProc));
  inherited SetParent(AParent);
  if Assigned(AParent) and not (csDesigning in ComponentState) then begin
    fOldHandle := AParent.Handle;
    fOldWndProc := Pointer(GetWindowLong(AParent.Handle, GWL_WNDPROC));
    SetWindowLong(AParent.Handle, GWL_WNDPROC, Integer(fNewWndProc));
    UpdateBackgroundBrush;
  end
  else
    FOldHandle := 0;
end;

procedure TCustomIconView.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SIZE, WM_PAINT:
      UpdateBackgroundBrush;
  end;
  //Parent.WindowProc(Message);
  with Message do
    Result := CallWindowProc(fOldWndProc, fOldHandle, Msg, WParam, LParam);
end;

procedure TCustomIconView.UpdateBackgroundBrush;
var
  rc: TRect;
  DC, DCMem: hDC;
  Bmp, BmpOld: hBitmap;
begin
  if not Transparent then
    Exit;
  //FThemed := UseThemes;
  if fBrush <> 0 then
    DeleteObject(fBrush);
  fBrush := 0;
  if ThemeServices.ThemesEnabled then begin
    GetWindowRect(Parent.Handle, rc);
    DC := GetDC(Parent.Handle);
    DCMem := CreateCompatibleDC(DC);
    Bmp := CreateCompatibleBitmap(DC, rc.right - rc.left, rc.bottom - rc.top);
    BmpOld := SelectObject(DCMem, Bmp);
    SendMessage(Parent.Handle, WM_PRINTCLIENT, DCMem, PRF_ERASEBKGND or PRF_CLIENT or PRF_NONCLIENT);
    fBrush := CreatePatternBrush(Bmp);
    SelectObject(DCMem, BmpOld);
    DeleteObject(Bmp);
    DeleteDC(DCMem);
    ReleaseDC(Parent.Handle, DC);
  end;
end;

procedure TCustomIconView.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
var
  rc: TRect;
begin
  if ThemeServices.ThemesEnabled and fTransparent then begin
    SetBkMode(Msg.ChildDC, Windows.TRANSPARENT);
    GetWindowRect(Msg.ChildWnd, rc);
    MapWindowPoints(0, Parent.Handle, rc, 2);
    SetBrushOrgEx(Msg.ChildDC, -rc.left, -rc.top, nil);
    Msg.Result := FBrush;
  end
  else
    inherited;
end;

end.
