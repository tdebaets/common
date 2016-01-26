unit danunit;

interface

uses
  WinProcs,WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Danhint, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    BitBtn1     : TBitBtn;
    ComboBox2   : TComboBox;
    ColorDialog1: TColorDialog;
    BitBtn2     : TBitBtn;
    BitBtn3     : TBitBtn;
    FontDialog1 : TFontDialog;
    Edit1       : TEdit;
    BitBtn4     : TBitBtn;
    Edit2       : TEdit;
    Memo1       : TMemo;
    CheckBox1   : TCheckBox;
    Edit3       : TEdit;
    DanHint1    : TDanHint;
    Button1: TButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation
{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
 close;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
 ColorDialog1.Color:=DanHint1.HintColor;
 if ColorDialog1.execute then
  DanHint1.HintColor := ColorDialog1.color;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
 if FontDialog1.execute then
  DanHint1.HintFont := FontDialog1.Font;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
 if Edit1.Text <>'' then DanHint1.HintPauseTime := StrToInt(Edit1.Text);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
 ColorDialog1.Color:=DanHint1.HintShadowColor;
 if ColorDialog1.execute then
  DanHint1.HintShadowColor := ColorDialog1.color;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
 DanHint1.HintDirection:= THintDirection(ComboBox2.ItemIndex);
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
 if Edit2.Text <> '' then  DanHint1.HintRadius := StrtoInt(Edit2.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 with DanHint1 do begin
  Edit2.Text := IntToStr(HintRadius);
  Edit3.Text := IntToStr(HintWidth);
  Edit1.Text := IntToStr(HintPauseTime);
  ComboBox2.ItemIndex := Integer(HintDirection);
  FontDialog1.Font    := HintFont;
  CheckBox1.Checked   := DanHint1.HintActive;
 end;
 Memo1.Hint:='Hello, World! '+ #13+
             'This is a Custom Delphi Component! '+
             'A new type of Hint Tips... '+
             'Designed by Dan Ho and modified by Dr. Peter Plass '+#13+
             'danho@cs.nthu.edu.tw / peter.plass@fh-zwickau.de';
end;

procedure TForm1.Memo1Change(Sender: TObject);
var
 i :Integer;
 s : string;
begin
 s := '';
 for i:=0 to Memo1.Lines.Count-1 do
  s := s + '' + Memo1.Lines[i];
 Memo1.Hint := s;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
 with DanHint1 do
  HintActive := CheckBox1.checked;
end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
 if Edit3.Text<>''then  DanHint1.HintWidth:=StrToInt(Edit3.Text);
end;
procedure TForm1.Button1Click(Sender: TObject);
begin
 MessageDlg('Demosoftware'+ #10#13+
            'zur Komponente: TDanHint3.0[32]      '+ #10#13+ #10#13+
            '© 1997 by Dr.Plass                   '+ #10#13+
            'Peter.Plass@fh-zwickau.de            '+ #10#13+
            'http://www.fh-zwickau.de/~pp/tm.htm  ',
            mtInformation, [mbOk],0);
end;
end.
