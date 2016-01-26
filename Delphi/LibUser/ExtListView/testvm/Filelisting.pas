unit FileListing;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls;

type
  TFileType =({ftDirectory,}ftArchive,ftReadonly,
              ftSystem,ftHidden,{ftCompressed,}ftTemporary,ftAll);
  TFileTypes = Set of TFileType;
  TFileObject = class
    Name,FType : string;
    Date : TDateTime;
    Size,
    ImgIndex,
    Attr : integer;
  end;
  TFileSortType = (fsNone,fsName,fsSize,fsDate,fsType);
  TFileListing = class(TList)
    Directory : string;
    DirBytes : integer;
    DefaultSort : TFileSortType;
    Ascend : boolean;
    FileTypes : TFileTypes;
    function GetFiles(Dir,Mask : string) : integer;
    function FileName(index:integer):string;
    function FullName(index:integer):string;
    procedure ClearList;
    procedure SortFiles(SortType:TFileSortType; Ascend:boolean);
    Destructor Destroy; Override;
  end;

function AddSlash(Const Path : String) : String;
function CommaStr(N : Longint) : String;

implementation

uses
  ShellApi,
  MaskSearch;


Var
  AscendSort : boolean;

function CommaStr(N : Longint) : String;
Var
  S : String;
  r : single;
Begin
  r := N;
  FmtStr(s,'%.0n',[r]);
  result := s;
End;

function AddSlash(Const Path : String) : String;
begin
  if Path[Length(Path)] = '\' then
    result := Path else result := Path + '\';
end;

function ValidNumber(const S: string; var V: extended): boolean;
var
  NumCode: integer;
begin
  Val(S, V, NumCode);
  Result := (NumCode = 0);
end;

function ValidDate(const S: string; var D: TDateTime): boolean;
begin
  try
    D := StrToDate(S);
    Result := TRUE;
  except
    D := 0;
    Result := FALSE;
  end;
end;

function CompareInt(I1,I2 : integer) : integer;
begin
  if I1 > I2 then result := 1 else
   if I1 < I2 then result := -1 else
     Result := 0;
end;

function NameSort(item1,item2:pointer) : integer;
begin
  Result := AnsiCompareText(TFileObject(item1).name,
                            TFileObject(item2).name);
  if not AscendSort then Result := -Result;
end;

function DateSort(item1,item2:pointer) : integer;
begin
  if TFileObject(item1).date > TFileObject(item2).date then
    result := 1 else if
     TFileObject(item1).date < TFileObject(item2).date then
       result := -1 else result := 0;
 // Result := CompareInt(TFileObject(item1).date,
 //                       TFileObject(item2).date);
  if not AscendSort then Result := -Result;
end;

function SizeSort(item1,item2:pointer) : integer;
begin
  Result := CompareInt(TFileObject(item1).size,
                        TFileObject(item2).size);
  if not AscendSort then Result := -Result;
end;

function TypeSort(item1,item2:pointer) : integer;
begin
  Result := AnsiCompareText(TFileObject(item1).FType,TFileObject(item2).FType);
  if not AscendSort then Result := -Result;
end;

function TFileListing.FileName(index:integer):string;
var
  Fo : TfileObject;
begin
  Fo := TFileObject(items[index]);;
  result := Fo.name;
end;

function TFileListing.FullName(index:integer):string;
var
  Fo : TFileObject;
begin
  Fo := TFileObject(items[index]);
  result := AddSlash(Directory)+Fo.name;
end;

procedure TFileListing.SortFiles(SortType:TFileSortType; ascend : boolean);
begin
  AscendSort := Ascend;
  case SortType of
    fsName : Sort(NameSort);
    fsDate : Sort(DateSort);
    fsSize : Sort(SizeSort);
    fsType : Sort(TypeSort);
  end;
//
end;

procedure TFileListing.ClearList;
var
  i : integer;
  Fo : TfileObject;
begin
  for i := 0 to count-1 do
    begin
      Fo := TFileObject(items[i]);
      SetLength(Fo.Name,0);
      Fo.Free;
    end;
  Clear;
end;

function TFileListing.GetFiles(Dir,Mask : string) : integer;
var
  Rslt : integer;
  rec : TSearchRec;
  Fobj : TFileObject;
  MaskList : TStringList;

function GetImageIndex(const Filename: String): Integer;
var
  Fileinfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, Fileinfo,
   sizeof(TSHFileInfo),
   SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) <> 0 then
     begin
       Result := Fileinfo.IIcon;
     end else Result := 0;
end;

function GetFileType(const Filename: String): string;
var
  Fileinfo: TSHFileInfo;
begin
  Result := '';
  if SHGetFileInfo(PChar(FileName),0,Fileinfo,sizeof(TSHFileInfo),
   SHGFI_USEFILEATTRIBUTES or SHGFI_TYPENAME) <> 0 then
      begin
        Result := FileInfo.szTypeName;
      end else Result := '';
end;

begin
  Count := 0;
  Directory := Dir;
  DirBytes := 0;
  ClearList;
  MaskList := TstringList.create;
  SetFilters(Mask,MaskList,true);
  Rslt := FindFirst(AddSlash(Dir)+'*.*',faAnyFile,Rec);
  While Rslt = 0 do with Rec do
    begin
      if (Name[1] <> '.') and
       (Attr and faDirectory <> faDirectory) and
        CmpMask(Name,MaskList,true) then
         begin
           Fobj := TFileObject.Create;
           Fobj.Name := Name;
           Fobj.Size := Size;
           Fobj.Date := FileDateToDateTime(Time);
           Fobj.Attr := Attr;
           // We'll only get the image index when the listview asks to paint it.
           Fobj.ImgIndex := -1; // GetImageIndex(AddBackSlash(Dir)+Name);
           // We have to get this now or sorting won't work.
           Fobj.FType := GetFileType(Dir+'\'+Name);
           Add(Fobj);
           Inc(DirBytes,Size);
         end;
      Rslt := FindNext(Rec);
    end;
  FindClose(Rec);
  Result := Count;
  MaskList.Free;
end;

destructor TFileListing.Destroy;
begin
  SetLength(Directory,0);
  ClearList;
  inherited Destroy;
end;

function CheckAttributes(Att :DWord; Typ :TFileTypes) :boolean;
begin
  if not (ftAll in Typ) then
  begin
    Result := true;
    if (Att and file_attribute_Archive) = file_attribute_Archive then
      Result := Result and (ftArchive in Typ);
    if (Att and file_attribute_Readonly) = file_attribute_Readonly then
      Result := Result and (ftArchive in Typ);
    if (Att and file_attribute_Hidden) = file_attribute_Hidden then
      Result := Result and (ftHidden in Typ);
    if (Att and file_attribute_System) = file_attribute_System then
      Result := Result and (ftSystem in Typ);
    if (Att and file_attribute_Temporary) = file_attribute_Temporary then
      Result := Result and (ftTemporary in Typ);
  end
  else
    Result := true; // ftAll allows any file
end;


END.
