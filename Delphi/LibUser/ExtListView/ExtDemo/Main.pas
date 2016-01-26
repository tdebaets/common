(******************************************************************************)
(* TdfsExtListView component demo.                                            *)
(* This demo illustrates the following features of the TdfsExtListView        *)
(* component:                                                                 *)
(*  + Automatic column sorting of string, date/time and numeric values        *)
(*    toggling between ascending and descending order with no code.           *)
(*  + Automatic saving and restoring of the column widths and the column      *)
(*    ordering between sessions.                                              *)
(*  + Ability to set and clear all of the extended styles at run-time so you  *)
(*    can see their effects.                                                  *)
(*  + Ability to set the hover time before lvxTrackSelect kicks in and        *)
(*    autoselects the item.                                                   *)
(*  + Shows how to use the CheckComCtlVersion method to ensure a specific     *)
(*    version of COMCTL32.DLL is installed.                                   *)
(*  + Shows usage of OnMarqueeBegin event to inhibit drag selection of items. *)
(*  + Shows how to use GetSubItemAt method to find specific text at X,Y pos.  *)
(*  + Shows how to change column order in code.                               *)
(*  + Shows how to set subitem images (SubItem_ImageIndex property).  Enable  *)
(*    lvxSubitemImages in ExtendedStyles.                                     *)
(******************************************************************************)

{$I DFS.INC}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF DFS_COMPILER_4_UP}
  ImgList,
{$ENDIF}
  StdCtrls, ExtCtrls, ExtListView, Spin, ComCtrls, CommCtrl, EnhListView;

type
  TForm1 = class(TForm)
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    IconSpacingX: TSpinEdit;
    IconSpacingY: TSpinEdit;
    ScrollBox1: TScrollBox;
    GridLines: TCheckBox;
    SubItemImages: TCheckBox;
    CheckBoxes: TCheckBox;
    TrackSelect: TCheckBox;
    HeaderDragDrop: TCheckBox;
    FullRowSelect: TCheckBox;
    OneClickActivate: TCheckBox;
    TwoClickActivate: TCheckBox;
    StatusBar: TStatusBar;
    Button1: TButton;
    Label6: TLabel;
    cbxNoDrag: TCheckBox;
    ExtListView: TdfsExtListView;
    ImageList1: TImageList;
    FlatScrollBar: TCheckBox;
    UnderlineHot: TCheckBox;
    UnderlineCold: TCheckBox;
    Label5: TLabel;
    HoverTime: TSpinEdit;
    RequireCOMCTL: TCheckBox;
    ccMajorHi: TEdit;
    Label7: TLabel;
    ccMajorLo: TEdit;
    Label8: TLabel;
    ccMinorHi: TEdit;
    Label9: TLabel;
    ccMinorLo: TEdit;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GridLinesClick(Sender: TObject);
    procedure SubItemImagesClick(Sender: TObject);
    procedure CheckBoxesClick(Sender: TObject);
    procedure TrackSelectClick(Sender: TObject);
    procedure HeaderDragDropClick(Sender: TObject);
    procedure FullRowSelectClick(Sender: TObject);
    procedure OneClickActivateClick(Sender: TObject);
    procedure TwoClickActivateClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure IconSpacingChange(Sender: TObject);
    procedure ExtListViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure ExtListViewMarqueeBegin(Sender: TObject; var CanBegin: Boolean);
    procedure FlatScrollBarClick(Sender: TObject);
    procedure UnderlineHotClick(Sender: TObject);
    procedure UnderlineColdClick(Sender: TObject);
    procedure HoverTimeChange(Sender: TObject);
    procedure RequireCOMCTLClick(Sender: TObject);
    procedure ccValueChange(Sender: TObject);
  private
    procedure CheckComCtlVersion;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.FormCreate(Sender: TObject);
var
  x, y: integer;
begin
  ComboBox1.ItemIndex := 2;

  // Stick some date/time values in column #2.
  ExtListView.Items[0].SubItems[0] := DateTimeToStr(Now);
  ExtListView.Items[1].SubItems[0] := DateTimeToStr(Now+2);
  ExtListView.Items[2].SubItems[0] := DateTimeToStr(Now+0.0001);
  ExtListView.Items[3].SubItems[0] := DateTimeToStr(Now-190.002);

  // Give everyone some subitem images
  for x := 0 to 3 do
    for y := 0 to 2 do
      // Item #x, subitem #y, image index #y+1
      ExtListView.SubItem_ImageIndex[x, y] := y+1;
end;

procedure TForm1.GridLinesClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxGridLines]
  else
    ExtendedStyles := ExtendedStyles - [lvxGridLines];
end;

procedure TForm1.SubItemImagesClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxSubItemImages]
  else
    ExtendedStyles := ExtendedStyles - [lvxSubItemImages];
end;

procedure TForm1.CheckBoxesClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxCheckBoxes]
  else
    ExtendedStyles := ExtendedStyles - [lvxCheckBoxes];
end;

procedure TForm1.TrackSelectClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxTrackSelect]
  else
    ExtendedStyles := ExtendedStyles - [lvxTrackSelect];
end;

procedure TForm1.HeaderDragDropClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxHeaderDragDrop]
  else
    ExtendedStyles := ExtendedStyles - [lvxHeaderDragDrop];
end;

procedure TForm1.FullRowSelectClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxFullRowSelect]
  else
    ExtendedStyles := ExtendedStyles - [lvxFullRowSelect];
end;

procedure TForm1.OneClickActivateClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxOneClickActivate]
  else
    ExtendedStyles := ExtendedStyles - [lvxOneClickActivate];
end;

procedure TForm1.TwoClickActivateClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxTwoClickActivate]
  else
    ExtendedStyles := ExtendedStyles - [lvxTwoClickActivate];
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: ExtListView.ViewStyle := vsIcon;
    1: ExtListView.ViewStyle := vsList;
    2: ExtListView.ViewStyle := vsReport;
    3: ExtListView.ViewStyle := vsSmallIcon;
  end;
end;

procedure TForm1.IconSpacingChange(Sender: TObject);
var
  X, Y: integer;
begin
  try
    X := IconSpacingX.Value;
    Y := IconSpacingY.Value;
    with ExtListView do begin
      SetIconSpacing(X, Y);
      if ViewStyle = vsIcon then begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
        try
          ViewStyle := vsSmallIcon;
          ViewStyle := vsIcon;
        finally
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
        end;
      end;
    end;
  except
    // conversion error, ignore it.
  end;
end;

procedure TForm1.ExtListViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SubItemText: string;
begin
  SubItemText := ExtListView.GetSubItemAt(X, Y);
  if SubItemText <> '' then
    SubItemText := 'SubItem = ' + SubItemText;
  StatusBar.SimpleText := SubItemText;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Columns: array[0..3] of integer;
  Tmp: integer;
begin
  if ExtListView.GetColumnOrder(4, Columns) then
  begin
    Tmp := Columns[0];
    Columns[0] := Columns[3];
    Columns[3] := Tmp;
    Tmp := Columns[1];
    Columns[1] := Columns[2];
    Columns[2] := Tmp;
    ExtListView.SetColumnOrder(4, Columns);
  end;
end;

procedure TForm1.ExtListViewMarqueeBegin(Sender: TObject; var CanBegin: Boolean);
begin
  CanBegin := not cbxNoDrag.Checked;
end;

procedure TForm1.FlatScrollBarClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxFlatScrollBar]
  else
    ExtendedStyles := ExtendedStyles - [lvxFlatScrollBar];
end;

procedure TForm1.UnderlineHotClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxUnderlineHot]
  else
    ExtendedStyles := ExtendedStyles - [lvxUnderlineHot];
end;

procedure TForm1.UnderlineColdClick(Sender: TObject);
begin
  with ExtListView do
  if (Sender as TCheckBox).Checked then
    ExtendedStyles := ExtendedStyles + [lvxUnderlineCold]
  else
    ExtendedStyles := ExtendedStyles - [lvxUnderlineCold];
end;

procedure TForm1.HoverTimeChange(Sender: TObject);
begin
  ExtListView.HoverTime := HoverTime.Value;
end;




procedure TForm1.RequireCOMCTLClick(Sender: TObject);
begin
  if RequireCOMCTL.Checked then
  begin
    ccMajorHi.Enabled := TRUE;
    ccMajorLo.Enabled := TRUE;
    ccMinorHi.Enabled := TRUE;
    ccMinorLo.Enabled := TRUE;
    
    CheckComCtlVersion;
  end else begin
    ccMajorHi.Enabled := FALSE;
    ccMajorLo.Enabled := FALSE;
    ccMinorHi.Enabled := FALSE;
    ccMinorLo.Enabled := FALSE;
  end;
end;

procedure TForm1.CheckComCtlVersion;
begin
  if not ExtListView.CheckComCtlVersion(StrToIntDef(ccMajorHi.Text, 4),
     StrToIntDef(ccMajorLo.Text, 7), StrToIntDef(ccMinorHi.Text, 0),
     StrToIntDef(ccMinorLo.Text, 0)) then
    MessageDlg('The version of COMCTL32.DLL installed on this machine is too' +
       ' old.', mtWarning, [mbOk], 0);
end;


procedure TForm1.ccValueChange(Sender: TObject);
begin
  CheckComCtlVersion;
end;

end.
