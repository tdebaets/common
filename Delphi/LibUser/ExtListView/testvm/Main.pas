 (*
description:

dependencies:

 Markus Stephany's MASKSEARCH utility unit is needed. You may find it on DSP
 in the MSTGREP.ZIP archive in Delphi 2.0 freeware section.

copyright & license
*)

{------------------------------------------------------------------------------
  Modifications made by Brad Stowers for compatibility with TdfsExtListView v3.x
 ------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtListView, StdCtrls, ExtCtrls, CommCtrl,ShellApi,
  FileListing, FileCtrl, Buttons, EnhListView;

type
  PVirtualItem = ^TVirtualItem;
  TVirtualItem = packed record
    ImageIndex: integer;
    Fname : string;
    State: UINT;
    SubText1: string;
    SubText2: string;
  end;

  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    FileList: TdfsExtListView;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Dirs: TDirectoryListBox;
    Panel4: TPanel;
    Drives: TDriveComboBox;
    Mask: TEdit;
    Splitter2: TSplitter;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    StatPanel: TPanel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileListColumnClick(Sender: TObject; Column: TListColumn);
    procedure FileListEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure DirsChange(Sender: TObject);
    procedure MaskKeyPress(Sender: TObject; var Key: Char);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FileListClick(Sender: TObject);
    procedure FileListVMCacheHint(Sender: TObject;
      var HintInfo: TNMCacheHint);
    procedure FormShow(Sender: TObject);
    procedure FileListVMGetItemInfo(Sender: TObject; Item,
      SubItem: Integer; var Mask: TLVVMMaskItems; var Image, Param, State,
      StateMask, Indent: Integer; var Text: String);
  private
    Files : TFileListing;
    NumItems: integer;
    HomeDir,Fmask : string;
  public
    FileCount : integer;
    procedure GetFileList(Dir,Mask : string);
    function GetSystemIndex(FileName:string):integer;
    procedure SetupSysImage;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

function TMainForm.GetSystemIndex(FileName:string):integer;
{Returns the index to the system image and the file type
 string in one call}
var
  Fileinfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName),0,Fileinfo,sizeof(TSHFileInfo),
   SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) <> 0 then
  begin
     Result := Fileinfo.IIcon;
  end else Result := 0;
end;

procedure TMainForm.SetupSysImage;
{Retrieves the handles to the small and large system
 icon lists}
var
  AHandle: DWORD;
  Fileinfo: TSHFileInfo;
begin
  FileList.SmallImages := TImageList.Create(self);
  AHandle := SHGetFileInfo('',
    0, Fileinfo, sizeof(TSHFileInfo),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  if AHandle <> 0 then begin
    FileList.SmallImages.Handle := AHandle;
    FileList.SmallImages.ShareImages := True;
  end;
  FileList.LargeImages := TImageList.Create(self);
   AHandle := SHGetFileInfo('',
    0, Fileinfo, sizeof(TSHFileInfo),
    SHGFI_LARGEICON or SHGFI_SYSICONINDEX
  );
  if AHandle <> 0 then begin
    FileList.LargeImages.Handle := AHandle;
    FileList.LargeImages.ShareImages := True;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Mask.left := 0;
  Mask.width := panel4.width-2;
  Fmask := '*.*';
  Files := TFileListing.Create;
  SetupSysImage;
  HomeDir := ExtractFileDir(Application.ExeName);
  Dirs.Directory := HomeDir;
 end;

procedure TMainForm.GetFileList(Dir,Mask : string);
var
  Rslt : integer;
  rec : TSearchRec;
  Fobj : TFileObject;
begin
  FileCount := 0;
  Files.Clear;
  Rslt := FindFirst(AddSlash(Dir)+Mask,faAnyFile,Rec);
  While Rslt = 0 do with Rec do
    begin
      if (Name[1] <> '.') and
       (Attr and faDirectory <> faDirectory) then
         begin
           Fobj := TFileObject.Create;
           Fobj.Name := Name;
           Fobj.Size := Size;
           Fobj.Date := Time;
           Files.Add(Fobj);
           Inc(FileCount);
         end;
      Rslt := FindNext(Rec);
    end;
  FindClose(Rec);
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
begin
  ShowMessage(Files.fullname(FileList.ELV_GetNextItem(-1, sdAll, [isFocused])));
end;

procedure TMainForm.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := true;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FileList.SmallImages.free;
  FileList.LargeImages.free;
  Files.Free;
end;

procedure TMainForm.FileListColumnClick(Sender: TObject;
  Column: TListColumn);
const
  Ascend : boolean = false;
var  
  count : integer;
  
begin
  Ascend := not Ascend;
  // Toggle sort arrow direction
  FileList.CurrentSortAscending := Ascend;
  Case Column.index of
    0 : Files.SortFiles(fsName,Ascend);
    1 : Files.SortFiles(fsSize,Ascend);
    2 : Files.SortFiles(fsDate,Ascend);
    3 : Files.SortFiles(fsType,Ascend);
  end;
  count := FileList.items.count;
  FileList.Items.Clear;
  FileList.SetItemCountEx(count, [lvsicfNoScroll]);
end;

procedure TMainForm.FileListEdited(Sender: TObject; Item: TListItem;
  var S: String);
var
  index : integer;
  Fobj : TFileObject;
begin
  index := FileList.ELV_GetNextItem(-1, sdAll, [isFocused]);
  Fobj := TfileObject(files[index]);
  Fobj.name := s;
end;

procedure TMainForm.DirsChange(Sender: TObject);
begin
  NumItems := Files.GetFiles(Dirs.Directory,Fmask);
  label2.caption :=
   Commastr(files.DirBytes)+ ' bytes in '+ inttostr(numitems) + ' files';
  FileList.SetItemCountEx(NumItems, [lvsicfNoScroll]);
  StatPanel.caption := ' '+UpperCase(Dirs.Directory);
end;

procedure TMainForm.MaskKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
    begin
      key := #0;
      Fmask := Mask.Text;
      DirsChange(self);
    end;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  case Tspeedbutton(Sender).tag of
    0: FileList.ViewStyle := vsReport;
    1: FileList.ViewStyle := vsList;
    2: FileList.ViewStyle := vsSmallIcon;
    3: FileList.ViewStyle := vsIcon;
  end;
end;

procedure TMainForm.FileListClick(Sender: TObject);
begin
  Label3.caption := inttostr(FileList.selcount)+' files selected';
end;

procedure TMainForm.FileListVMCacheHint(Sender: TObject;
  var HintInfo: TNMCacheHint);
var
  i : integer;
  FObj : TFileObject;
  Fname : string;
begin
  for i := HintInfo.ifrom to HintInfo.iTo do
    begin
      Fobj := TFileObject(Files[i]);
      Fname := AddSlash(Files.Directory)+Fobj.Name;
      if Fobj.Imgindex = -1 then // we haven't painted this one before, get it
        Fobj.Imgindex := GetSystemIndex(Fname);
    end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DirsChange(Self);  // populate with initial directory at startup.
end;

procedure TMainForm.FileListVMGetItemInfo(Sender: TObject; Item,
  SubItem: Integer; var Mask: TLVVMMaskItems; var Image, Param, State,
  StateMask, Indent: Integer; var Text: String);
var
  Fobj : TfileObject;
  fName : string;

function FixName(Name : string) : string;
begin
  Result := LowerCase(name);
  Result[1] := UpCase(Result[1]);
end;

begin
  Fobj := Files[item];
  fName:= AddSlash(files.Directory)+Fobj.name;
  if lvifImage in Mask then
    begin
      image := Fobj.imgindex;
    end;
  if lvifText in Mask then with Fobj do
    begin
      case SubItem of
        0: Text :=  FixName(Fobj.Name);
        1: Text :=  Commastr(size);
        2: Text :=  DateTimeToStr(Date);
        3: Text :=  Ftype;
      else Text := '';
      end;
    end;
  //if lvifParam in Mask then Param := 0;
{  if lvifState in Mask then
    State := State or AnItem.State; }
end;

END.

