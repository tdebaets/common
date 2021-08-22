(****************************************************************************
 *
 * Copyright 2021 Tim De Baets
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
 * TDetailMessageBoxForm class
 *
 ****************************************************************************)

unit DetailMessageBoxForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogForm, ExtCtrls, ThemeMgr, StdCtrls, ComCtrls, CtrlsCommon, Common2,
  ImgList, EnhListView, ExtListView, SizeGrip, SysLink, ClipBrd, CommCtrl;

type
  TDetailMessageBoxForm = class(TDialogForm)
    imgIcon: TImage;
    lblMessage: TLabel;
    lvDetails: TdfsExtListView;
    ThemeManager1: TThemeManager;
    BottomPanel: TPanel;
    btnOK: TButton;
    SizeGrip1: TSizeGrip;
    lnkCopy: TSysLink;
    Images: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BottomPanelResize(Sender: TObject);
    procedure lnkCopyClickLink(Sender: TObject; Index: Integer; const AID,
      AURL: String; var Visited: Boolean);
    procedure FormResize(Sender: TObject);
  private
    fpWidth: PInteger;
    fpHeight: PInteger;
    fDetailIcon: TCommonIcon;
  public
    constructor Create(hWndParent: HWND; pWidth, pHeight: PInteger;
        const Title, MessageText: String; MainIcon: PChar;
        DetailIcon: TCommonIcon); reintroduce;
    procedure AddDetail(const Fields: array of String); overload;
    procedure AddDetail(const Fields: array of String; Icon: TCommonIcon); overload;
    procedure ClearDetails;
    procedure SetColumns(const Columns: array of String);
    procedure SetDetailIcon(ResourceID: PChar);
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

implementation

const
  MessageEpilog = CrLf + 'Please refer to the list below for more details.';

{$R DetailMessageBoxForm.dfm}

constructor TDetailMessageBoxForm.Create(hWndParent: HWND;
    pWidth, pHeight: PInteger; const Title, MessageText: String; MainIcon: PChar;
    DetailIcon: TCommonIcon);
var
  CommonIcon: TCommonIcon;
  IconHandle: HICON;
begin
  inherited Create(hWndParent);
  fpWidth := pWidth;
  fpHeight := pHeight;
  fDetailIcon := DetailIcon;
  Caption := Title;
  lblMessage.Caption := MessageText + MessageEpilog;
  imgIcon.Picture.Icon.Handle := LoadIcon(0, MainIcon);
  ConvertTo32BitImageList(Images);
  Images.Clear;
  for CommonIcon := Low(TCommonIcon) to High(TCommonIcon) do begin
    if CommonIcon = ciNone then begin
      // Load a dummy (OEM) icon for ciNone to add to the image list. This way,
      // the indices in the image list are consistent with the corresponding
      // TCommonIcon values.
      IconHandle := LoadImage(0, MakeIntResource(OIC_SAMPLE), IMAGE_ICON,
          16, 16, LR_SHARED);
    end
    else
      IconHandle := LoadCommonIcon(CommonIcon, 16, 16);
    ImageList_AddIcon(Images.Handle, IconHandle);
    // OEM icons shouldn't be destroyed
    if CommonIcon <> ciNone then
      DestroyIcon(IconHandle);
  end;
end;

procedure TDetailMessageBoxForm.FormCreate(Sender: TObject);
begin
  Resizable := True;
  if IsWindowsVistaOrHigher then
    Color := clWindow;
end;

procedure TDetailMessageBoxForm.FormShow(Sender: TObject);
var
  PrevWidth, PrevHeight: Integer;
begin
  if Assigned(fpWidth) then
    PrevWidth := fpWidth^
  else
    PrevWidth := 0;
  if Assigned(fpHeight) then
    PrevHeight := fpHeight^
  else
    PrevHeight := 0;
  if PrevWidth > 0 then
    Width := ScaleX(PrevWidth);
  if PrevHeight > 0 then
    Height := ScaleY(PrevHeight);
  //lvDetails.Resort; // not necessary as AutoResort is True
  AutosizeListViewColumns(lvDetails);
  MessageBeep(MB_ICONEXCLAMATION);
end;

procedure TDetailMessageBoxForm.AddDetail(const Fields: array of String);
begin
  AddDetail(Fields, fDetailIcon);
end;

procedure TDetailMessageBoxForm.AddDetail(const Fields: array of String;
    Icon: TCommonIcon);
var
  i: Integer;
begin
  with lvDetails.Items.Add do begin
    Caption := Fields[0];
    if Length(Fields) > 1 then begin
      for i := Low(Fields) + 1 to High(Fields) do
        SubItems.Add(Fields[i]);
    end;
    ImageIndex := Integer(Icon);
  end;
end;

procedure TDetailMessageBoxForm.ClearDetails;
begin
  lvDetails.Items.Clear;
end;

procedure TDetailMessageBoxForm.SetColumns(const Columns: array of String);
var
  i: Integer;
begin
  lvDetails.Columns.Clear;
  for i := Low(Columns) to High(Columns) do begin
    with lvDetails.Columns.Add do begin
      Caption := Columns[i];
      if (i > Low(Columns)) or (i < High(Columns)) then
        Width := -2;
    end;
  end;
  lvDetails.LastColumnClicked := 0; // show sort arrow
end;

procedure TDetailMessageBoxForm.SetDetailIcon(ResourceID: PChar);
begin
  Images.Clear;
  AddResIconToImageList(ResourceID, Images);
end;

procedure TDetailMessageBoxForm.BeginUpdate;
begin
  lvDetails.BeginUpdate;
end;

procedure TDetailMessageBoxForm.EndUpdate;
begin
  lvDetails.EndUpdate;
end;

procedure TDetailMessageBoxForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TDetailMessageBoxForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(fpWidth) then
    fpWidth^ := DescaleX(Width);
  if Assigned(fpHeight) then
    fpHeight^ := DescaleY(Height);
end;

procedure TDetailMessageBoxForm.BottomPanelResize(Sender: TObject);
begin
  btnOK.Left := (BottomPanel.Width - btnOK.Width) div 2;
end;

procedure TDetailMessageBoxForm.lnkCopyClickLink(Sender: TObject;
  Index: Integer; const AID, AURL: String; var Visited: Boolean);
var
  Text: String;
  Row, Col: Integer;
begin
  Text := '';
  for Row := 0 to lvDetails.Items.Count - 1 do begin
    if Row > 0 then
      Text := Text + CrLf;
    with lvDetails.Items[Row] do begin
      Text := Text + Caption;
      for Col := 0 to SubItems.Count - 1 do
        Text := Text + ChrTab + SubItems[Col];
    end;
  end;
  Clipboard.AsText := Text;
end;

procedure TDetailMessageBoxForm.FormResize(Sender: TObject);
begin
  lnkCopy.Repaint; // prevent glitches
end;

end.
