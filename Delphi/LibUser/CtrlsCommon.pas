(****************************************************************************
 *
 * Copyright 2016-2022 Tim De Baets
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
 * VCL component utility code
 *
 ****************************************************************************)

unit CtrlsCommon;

interface

uses Windows, Messages, CommCtrl, ComCtrls, Controls, Forms, StdCtrls, Classes,
    Consts, Graphics, ImgList, Buttons, Common2, shlwapi, EnhListView;

const
  BCM_FIRST = $00001600;
  BCM_GETIDEALSIZE = BCM_FIRST + $0000001;
  BCM_SETSHIELD = BCM_FIRST + $000C;

function ProcessAppMessages: Boolean;
function HandleAppMessage: Boolean;
procedure PostActivateAppMessage;

procedure ConvertTo32BitImageList(const ImageList: TImageList);
function AddResIconToImageList(ResourceID: PChar; ImageList: TImageList): Integer;

type
  TResizeImageListFlag = (rilfForceStretch);
  TResizeImageListFlags = set of TResizeImageListFlag;

procedure ResizeImageListForHighDPI(ImgList: TImageList;
    Flags: TResizeImageListFlags = []);

procedure ResizeBitBtnGlyphsForHighDPI(Btn: TBitBtn);

function FitOnCanvas(Control: TGraphicControl; MyString: String): String;
procedure SetElevationRequiredState(aControl: TWinControl; Required: Boolean);

type
  TEnumControlProc = function(Control: TControl; Data: Pointer): Boolean of object;

function EnumControls(Container: TWinControl; Proc: TEnumControlProc;
    Data: Pointer): Boolean;

procedure AutosizeListViewColumns(ListView: TCustomListView);

procedure SetMemoHeightInLines(Memo: TCustomMemo; Lines: Cardinal);

procedure ScrollRectInView(ScrollBox: TScrollBox; Rect: TRect);

implementation

function ProcessAppMessages: Boolean;
begin
  Application.ProcessMessages;
  if Application.Terminated then begin
    // WM_QUIT was received, repost it for the outer modal loop
    // TODO: restore original exit code?
    PostQuitMessage(0);
    Result := False;
  end
  else
    Result := True;
end;

function HandleAppMessage: Boolean;
begin
  Application.HandleMessage;
  if Application.Terminated then begin
    // WM_QUIT was received, repost it for the outer modal loop
    // TODO: restore original exit code?
    PostQuitMessage(0);
    Result := False;
  end
  else
    Result := True;
end;

procedure PostActivateAppMessage;
begin
  // If ThemeMgr is being used in a DLL and together with TDllAppWindow, it
  // usually won't be triggered to recreate window handles because there are no
  // application messages (like e.g. CM_ACTIVATE in a regular app). So we need
  // to post at least 1 message to the main application window, and CM_ACTIVATE
  // seems like the most obvious choice.
  if Application.Handle <> 0 then
    PostMessage(Application.Handle, CM_ACTIVATE, 0, 0);
end;

procedure ConvertTo32BitImageList(const ImageList: TImageList);
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
var
  TempList: TImageList;
begin
  if Assigned(ImageList) then begin
    TempList := TImageList.Create(nil);
    try
      TempList.Assign(ImageList);
      with ImageList do begin
        Handle := ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked],
            0, AllocBy);
        if not HandleAllocated then
          raise EInvalidOperation.Create(SInvalidImageList);
      end;
      Imagelist.AddImages(TempList);
    finally
      TempList.Free;
    end;
  end;
end;

function AddResIconToImageList(ResourceID: PChar; ImageList: TImageList): Integer;
var
  IconHandle: HICON;
begin
  Result := -1;
  IconHandle := LoadImage(hInstance, ResourceID, IMAGE_ICON,
      ImageList.Width, ImageList.Height, LR_DEFAULTCOLOR);
  if IconHandle = 0 then
    Exit;
  try
    Result := ImageList_AddIcon(ImageList.Handle, IconHandle);
  finally
    DestroyIconSafe(IconHandle);
  end;
end;

// Heavily modified version of the ResizeImageListImagesforHighDPI function by
// Žarko Gajic - Resizing TImageList Bitmaps to Fit High-DPI Scaling Size (for
// Menus, Toolbars, Trees, etc.)
// http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc
procedure ResizeImageListForHighDPI(ImgList: TImageList;
    Flags: TResizeImageListFlags = []);
  procedure SetBitmapDimensions(Bitmap: TBitmap; Width, Height: Integer);
  begin
    Bitmap.Width := Width;
    Bitmap.Height := Height;
  end;
  procedure ClearBitmap(Bitmap: TBitmap);
  begin
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
  end;
  procedure DrawImageListImageToBitmap(ImgList: TImageList; Index: Integer;
      ImageType: TImageType; Bitmap: TBitmap);
  const
    Images: array[TImageType] of Longint = (0, ILD_MASK);
  begin
    ImageList_DrawEx(ImgList.Handle, Index, Bitmap.Canvas.Handle, 0, 0,
        Bitmap.Width, Bitmap.Height, CLR_NONE, CLR_NONE, Images[ImageType]);
  end;
const
  CenterThreshold = 150; // percentage
var
  NewWidth, NewHeight: Integer;
  TempImgList: TImageList;
  i: Integer;
  ImgBmp, MaskBmp: TBitmap;
  ResizedImgBmp, ResizedMaskBmp: TBitmap; // resized (or centered) image/mask
begin
  if Screen.PixelsPerInch = BasePixelsPerInch then
    Exit;
  NewWidth := MulDiv(ImgList.Width, Screen.PixelsPerInch, BasePixelsPerInch);
  NewHeight := MulDiv(ImgList.Height, Screen.PixelsPerInch, BasePixelsPerInch);
  if ImgList.Count = 0 then begin
    // Optimization: no need to do any resizing when the image list is empty -
    // just change the width and height and exit.
    ImgList.Width := NewWidth;
    ImgList.Height := NewHeight;
    Exit;
  end;
  TempImgList := TImageList.CreateSize(ImgList.Width, ImgList.Height);
  ImgBmp := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  ResizedImgBmp := TBitmap.Create;
  ResizedMaskBmp := TBitmap.Create;
  try
    // Copy all images from ImgList to TempImgList
    TempImgList.AddImages(imgList);
    // Set size to match DPI size (like 250% of 16px = 40px)
    // This clears all existing images in ImgList
    ImgList.Width := NewWidth;
    ImgList.Height := NewHeight;
    SetBitmapDimensions(ImgBmp, TempImgList.Width, TempImgList.Height);
    SetBitmapDimensions(MaskBmp, TempImgList.Width, TempImgList.Height);
    SetBitmapDimensions(ResizedImgBmp, ImgList.Width, ImgList.Height);
    SetBitmapDimensions(ResizedMaskBmp, ImgList.Width, ImgList.Height);
    // Add images back to ImgList stretched (if DPI scaling > 150%) or centered
    // (if DPI scaling <= 150%)
    for i := 0 to TempImgList.Count - 1 do begin
      ClearBitmap(ImgBmp);
      DrawImageListImageToBitmap(TempImgList, i, itImage, ImgBmp);
      ClearBitmap(MaskBmp);
      DrawImageListImageToBitmap(TempImgList, i, itMask, MaskBmp);
      if not (rilfForceStretch in Flags)
          and (Screen.PixelsPerInch * 100 / BasePixelsPerInch <=
              CenterThreshold) then begin
        // Center if <= 150%
        ClearBitmap(ResizedImgBmp);
        with ResizedImgBmp do begin
          Canvas.Draw(
              (Width - ImgBmp.Width) div 2,
              (Height - ImgBmp.Height) div 2,
              ImgBmp);
        end;
        ClearBitmap(ResizedMaskBmp);
        with ResizedMaskBmp do begin
          Canvas.Draw(
              (Width - MaskBmp.Width) div 2,
              (Height - MaskBmp.Height) div 2,
              MaskBmp);
        end;
      end
      else begin
        // Stretch if > 150%
        with ResizedImgBmp do
          Canvas.StretchDraw(Rect(0, 0, Width, Height), ImgBmp);
        with ResizedMaskBmp do
          Canvas.StretchDraw(Rect(0, 0, Width, Height), MaskBmp);
      end;
      ImgList.Add(ResizedImgBmp, ResizedMaskBmp);
    end;
  finally
    FreeAndNil(ResizedMaskBmp);
    FreeAndNil(ResizedImgBmp);
    FreeAndNil(MaskBmp);
    FreeAndNil(ImgBmp);
    FreeAndNil(TempImgList);
  end;
end;

procedure ResizeBitBtnGlyphsForHighDPI(Btn: TBitBtn);
var
  Bmp: TBitmap;
begin
  if Btn.Glyph.Empty then
    Exit;
  Bmp := TBitmap.Create;
  with Bmp do try
    Width := MulDiv(Btn.Glyph.Width, Screen.PixelsPerInch, BasePixelsPerInch);
    Height := MulDiv(Btn.Glyph.Height, Screen.PixelsPerInch, BasePixelsPerInch);
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.StretchDraw(Rect(0, 0, Width, Height), Btn.Glyph);
    Btn.Glyph.Assign(Bmp);
  finally
    FreeAndNil(Bmp);
  end;
end;

type
  TGraphicControlCast = class(TGraphicControl);

function FitOnCanvas(Control: TGraphicControl; MyString: String): String;
begin
  SetString(Result, PChar(MyString), Length(MyString));
  // this might be necessary if the control hasn't been painted yet
  TGraphicControlCast(Control).Canvas.Font := TGraphicControlCast(Control).Font;
  PathCompactPath(TGraphicControlCast(Control).Canvas.Handle, PChar(Result),
      Control.Width);
end;

function FitOnCanvasOld(Control: TGraphicControl; MyString: String): String;
var
  MyRect : TRect;
begin
  MyRect := Control.ClientRect;
  Dec(MyRect.Right, 3); // safety margin
  SetString(Result, PChar(MyString), Length(MyString));
  // this might be necessary if the control hasn't been painted yet
  TGraphicControlCast(Control).Canvas.Font := TGraphicControlCast(Control).Font;
  DrawTextEx(TGraphicControlCast(Control).Canvas.Handle, PChar(Result),
      Length(Result), MyRect,
      DT_CALCRECT or // prevent actual drawing
      DT_MODIFYSTRING or DT_NOPREFIX or DT_PATH_ELLIPSIS, NIL);
end;

procedure SetElevationRequiredState(aControl: TWinControl; Required: Boolean);
begin
  SendMessage(aControl.Handle, BCM_SETSHIELD, 0, Integer(Required));
end;

function EnumControls(Container: TWinControl; Proc: TEnumControlProc;
    Data: Pointer): Boolean;
var
  i: Integer;
  Control: TControl;
begin
  Result := False;
  for i := 0 to Container.ControlCount - 1 do begin
    Control := Container.Controls[i];
    if not Proc(Control, Data) then
      Exit;
    if Control is TWinControl then begin
      if not EnumControls(TWinControl(Control), Proc, Data) then
        Exit;
    end;
  end;
  Result := True;
end;

type
  THackListView = class(TCustomListView);
  THackCustomEnhListView = class(TCustomEnhListView);

procedure AutosizeListViewColumns(ListView: TCustomListView);
const
  SortArrowSpace = 30;
var
  i: Integer;
  Col: TListColumn;
  AutoWidth, TotalWidth, RemainingWidth: Integer;
  ExtraWidth: Integer;
  LastVisibleColumnIdx: Integer;
begin
  ListView.HandleNeeded;
  TotalWidth := 0;
  if (ListView is TCustomEnhListView)
      and THackCustomEnhListView(ListView).ShowSortArrows then
    ExtraWidth := SortArrowSpace
  else
    ExtraWidth := 0;
  with THackListView(ListView) do begin
    LastVisibleColumnIdx := Columns.Count - 1;
    for i := Columns.Count - 1 downto 0 do begin
      if Columns[i].Width > 0 then begin
        LastVisibleColumnIdx := i;
        Break;
      end;
    end;
    for i := 0 to Columns.Count - 1 do begin
      Col := Columns[i];
      if Col.Width = 0 then
        Continue;
      Col.Width := ColumnTextWidth;
      AutoWidth := Col.Width;
      Col.Width := ColumnHeaderWidth;
      if AutoWidth > Col.Width + ExtraWidth then
        Col.Width := AutoWidth
      else if i < LastVisibleColumnIdx then
        Col.Width := Col.Width + ExtraWidth; // required - unset negative width!
      Inc(TotalWidth, Col.Width);
      if i = LastVisibleColumnIdx then begin
        RemainingWidth := ClientWidth - TotalWidth;
        if RemainingWidth > 0 then
          Col.Width := Col.Width + RemainingWidth
        else if AutoWidth > Col.Width + ExtraWidth then
          Col.Width := AutoWidth
        else
          Col.Width := Col.Width; // required - unset negative width!
      end;
    end;
  end;
end;

type
  THackCustomMemo = class(TCustomMemo);

procedure SetMemoHeightInLines(Memo: TCustomMemo; Lines: Cardinal);
var
  EditRect, TextRect: TRect;
  DC: HDC;
  SaveFont: HFont;
  S: String;
begin
  Memo.HandleNeeded;
  Memo.Perform(EM_GETRECT, 0, Integer(@EditRect));
  DC := GetDC(Memo.Handle);
  try
    SaveFont := SelectObject(DC, THackCustomMemo(Memo).Font.Handle);
    S := DupeString(CrLf, Lines);
    TextRect := EditRect;
    DrawTextEx(DC, PChar(S), Length(S), TextRect,
        DT_CALCRECT or DT_EDITCONTROL or DT_WORDBREAK or DT_NOPREFIX, nil);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(Memo.Handle, DC);
  end;
  Memo.Height := Memo.Height + TextRect.Bottom - EditRect.Bottom;
end;

// Almost literally copy-pasted from Delphi 4's TScrollingWinControl.ScrollInView,
// but this takes a TRect (with client coordinates) as parameter instead of a
// TControl.
procedure ScrollRectInView(ScrollBox: TScrollBox; Rect: TRect);
begin
  with ScrollBox do begin
    Dec(Rect.Left, HorzScrollBar.Margin);
    Inc(Rect.Right, HorzScrollBar.Margin);
    Dec(Rect.Top, VertScrollBar.Margin);
    Inc(Rect.Bottom, VertScrollBar.Margin);
    if Rect.Left < 0 then
      with HorzScrollBar do Position := Position + Rect.Left
    else if Rect.Right > ClientWidth then
    begin
      if Rect.Right - Rect.Left > ClientWidth then
        Rect.Right := Rect.Left + ClientWidth;
      with HorzScrollBar do Position := Position + Rect.Right - ClientWidth;
    end;
    if Rect.Top < 0 then
      with VertScrollBar do Position := Position + Rect.Top
    else if Rect.Bottom > ClientHeight then
    begin
      if Rect.Bottom - Rect.Top > ClientHeight then
        Rect.Bottom := Rect.Top + ClientHeight;
      with VertScrollBar do Position := Position + Rect.Bottom - ClientHeight;
    end;
  end;
end;

end.
