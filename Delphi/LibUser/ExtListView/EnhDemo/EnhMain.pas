(******************************************************************************)
(* TdfsEnhListView component demo.                                            *)
(* This demo illustrates the following features of the TdfsEnhListView        *)
(* component:                                                                 *)
(*  + Automatic column sorting of string, date and numeric values toggling    *)
(*    between ascending and descending order.                                 *)
(*  + Sort column and direction indicator in column header with NO code.      *)
(*  + Owner draw event that doesn't actually have to do any drawing in order  *)
(*    to change the appearance.  You have control of the canvas, so you can   *)
(*    simply make changes to it to affect the font in the default drawing.    *)
(*  + Owner drawn header that modifies the appearance without actually doing  *)
(*    all of the drawing.  Illustrates how to have images in the column       *)
(*    headers without using the COMCTL32.DLL update!                          *)
(*  + Automatic saving and restoring of the column widths between sessions.   *)
(******************************************************************************)

{$I DFS.INC}

unit EnhMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF DFS_COMPILER_4_UP}
  ImgList,
{$ENDIF}  
  ComCtrls, EnhListView, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    AnEnhListView: TdfsEnhListView;
    ColumnImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure AnEnhListViewDrawItem(Control: TWinControl;
      var Canvas: TCanvas; Index: Integer; Rect: TRect;
      State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: Boolean);
    procedure AnEnhListViewDrawSubItem(Control: TWinControl;
      var Canvas: TCanvas; Index, SubItem: Integer; Rect: TRect;
      State: TOwnerDrawState; var DefaultDrawing: Boolean);
    procedure AnEnhListViewDrawHeader(Control: TWinControl;
      var Canvas: TCanvas; Index: Integer; var Rect: TRect; Selected: Boolean;
      var DefaultDrawing: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
  procedure AddDemoLine(ACaption: string; ANum: integer; ADate: TDateTime);
  begin
    with AnEnhListView.Items.Add do
    begin
      Caption := ACaption;
      ImageIndex := GetTickCount mod 3; { Pick a semi-random image }
      SubItems.Add(IntToStr(ANum));
      SubItems.Add(DateToStr(ADate));
    end;
  end;
begin
  // Note that I am NOT using ELV.Items.BeginUpdate.  ELV.BeginUpdate does the
  // same thing, but it also prevents resorting as well.  Otherwise, the list
  // would get resorted each time an item was added.  Very inefficient.
  AnEnhListView.BeginUpdate;
  try
    AddDemoLine('Some text', 391, Now);
    AddDemoLine('More text', 1931, Now-2);
    AddDemoLine('Other stuff', 175, Now+10);
    AddDemoLine('Enhancing', 1, Now-5);
    AddDemoLine('isn''t', 2865, Now+4);
    AddDemoLine('it?', -9, Now);
  finally
    AnEnhListView.EndUpdate;
  end;
end;

procedure TForm1.AnEnhListViewDrawItem(Control: TWinControl;
  var Canvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState;
  var DefaultDrawing, FullRowSelect: Boolean);
begin
  DefaultDrawing := TRUE;  // let the componet draw it for us.
  FullRowSelect := TRUE; // I like full row selection
  // Make the selected item bold italics just to show how easy it is
  if (AnEnhListView.Selected <> NIL) and
     (AnEnhListView.Selected.Index = Index) then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold, fsItalic];
end;

procedure TForm1.AnEnhListViewDrawSubItem(Control: TWinControl;
  var Canvas: TCanvas; Index, SubItem: Integer; Rect: TRect;
  State: TOwnerDrawState; var DefaultDrawing: Boolean);
begin
  DefaultDrawing := TRUE;
  { Draw the numbers in red if not selected }
  if (SubItem = 0) and not (odSelected in State) then
    Canvas.Font.Color := clRed;
end;

procedure TForm1.AnEnhListViewDrawHeader(Control: TWinControl;
  var Canvas: TCanvas; Index: Integer; var Rect: TRect; Selected: Boolean;
  var DefaultDrawing: Boolean);
var
  Offset: integer;
begin
  // Draw an image in the header
  Offset := (Rect.Bottom - Rect.Top - ColumnImageList.Height) div 2;
  ColumnImageList.Draw(Canvas, Rect.Left + 4, Rect.Top + Offset, Index);
  inc(Rect.Left, ColumnImageList.Width + 4);

  DefaultDrawing := TRUE;
  Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
  if Selected then
    Canvas.Font.Color := clRed;
end;


end.
