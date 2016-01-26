// The caching is very primitive, and probably won't even help all that much in a real
// world situation.  Enhancing the caching mechanism is left as an exercise for the user.

{$DEFINE DEBUG}

unit VMMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtListView, StdCtrls, ExtCtrls, CommCtrl, EnhListView;

type
  { Don't use huge pointers in here.  It'll leak when the record is destroyed }
  { To avoid the leak, you'd have to reset all the strings back to '' before  }
  { disposing of it.                                                          }
  PVirtualItem = ^TVirtualItem;
  TVirtualItem = packed record
    ImageIndex: integer;
    Title: string[255];
    State: UINT;
    SubText1: string[255];
    SubText2: string[255];
  end;

  TForm1 = class(TForm)
    AnExtListView: TdfsExtListView;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure AnExtListViewVMCacheHint(Sender: TObject;
      var HintInfo: TNMCacheHint);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AnExtListViewVMGetItemInfo(Sender: TObject; Item,
      SubItem: Integer; var Mask: TLVVMMaskItems; var Image: Integer;
      var Param: Longint; var State, StateMask, Indent: Integer;
      var Text: string);
  private
    CacheStart,
    CacheStop: integer;
    ItemCache: TList;
    NumItems: integer;
  public
    procedure PrepCache(FromIndex, ToIndex: integer);
    function GetVirtualItem(Item: integer): TVirtualItem;
    function CreateVirtualItem(Item: integer): PVirtualItem;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  TmpIcon: TIcon;
begin
  ItemCache := NIL;
  ComboBox1.ItemIndex := 2;
  with AnExtListView do begin
    LargeImages := TImageList.Create(Self);
    with LargeImages do begin
      Width := GetSystemMetrics(SM_CXICON);
      Height := GetSystemMetrics(SM_CYICON);
      AddIcon(Application.Icon);
      TmpIcon := TIcon.Create;
      TmpIcon.Handle := LoadIcon(0, MakeIntResource(IDI_APPLICATION));
      AddIcon(TmpIcon);
    end;

    SmallImages := TImageList.Create(Self);
    with SmallImages do begin
      Width := GetSystemMetrics(SM_CXSMICON);
      Height := GetSystemMetrics(SM_CYSMICON);
      AddIcon(Application.Icon);
      AddIcon(TmpIcon);
      TmpIcon.Free;
    end;
  end;

  CacheStart := -1;
  CacheStop := -1;
  NumItems := 500;
  AnExtListView.SetItemCountEx(NumItems, [lvsicfNoScroll]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  x: integer;
begin
  if ItemCache <> NIL then
  begin
    for x := ItemCache.Count-1 downto 0 do begin
      Dispose(ItemCache[x]);
      {$IFDEF DEBUG}
      OutputDebugString(PChar('disposing at ' + inttostr(x) + #13#10));
      {$ENDIF}
      ItemCache.Delete(x);
    end;
    ItemCache.Free;
  end;
  AnExtListView.LargeImages.Free;
  AnExtListView.SmallImages.Free;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: AnExtListView.ViewStyle := vsIcon;
    1: AnExtListView.ViewStyle := vsList;
    2: AnExtListView.ViewStyle := vsReport;
    3: AnExtListView.ViewStyle := vsSmallIcon;
  end;
end;

procedure TForm1.PrepCache(FromIndex, ToIndex: integer);
var
  x: integer;
begin
  if ItemCache = NIL then ItemCache := TList.Create;
  if (FromIndex < CacheStart) or (ToIndex > CacheStop) then begin
    // Free up the old cache items
    if CacheStart > -1 then
      for x := ItemCache.Count-1 downto 0 do begin
        Dispose(ItemCache[x]);
        {$IFDEF DEBUG}
        OutputDebugString(PChar('disposing at ' + inttostr(x) + #13#10));
        {$ENDIF}
        ItemCache.Delete(x);
      end;
    // load the new cache items
    CacheStart := FromIndex;
    CacheStop := ToIndex;
    for x := CacheStart to CacheStop do
    begin
      ItemCache.Add(CreateVirtualItem(x));
      {$IFDEF DEBUG}
      OutputDebugString(PChar('adding at ' + inttostr(x) + #13#10));
      {$ENDIF}
    end;
  end;
end;

function TForm1.GetVirtualItem(Item: integer): TVirtualItem;
var
  TmpItem: PVirtualItem;
begin
  if (Item < CacheStart) or (Item > CacheStop) then begin
    TmpItem := CreateVirtualItem(Item);
    Result := TmpItem^;
    Dispose(TmpItem);
  end else
    Result := PVirtualItem(ItemCache[Item-CacheStart])^;
end;

function TForm1.CreateVirtualItem(Item: integer): PVirtualItem;
begin
  New(Result);
  with Result^ do begin
    if odd(Item) then
      ImageIndex := 1
    else
      ImageIndex := 0;
    State := 0;
    Title := 'VM Test Item #'+IntToStr(Item);
    SubText1 := 'Item #'+IntToStr(Item)+': Subitem 1';
    SubText2 := 'Item #'+IntToStr(Item)+': Subitem 2';
  end;
end;


procedure TForm1.AnExtListViewVMCacheHint(Sender: TObject;
  var HintInfo: TNMCacheHint);
begin
  with HintInfo do
    PrepCache(iFrom, iTo);
end;

procedure TForm1.AnExtListViewVMGetItemInfo(Sender: TObject; Item,
  SubItem: Integer; var Mask: TLVVMMaskItems; var Image: Integer;
  var Param: Longint; var State, StateMask, Indent: Integer;
  var Text: string);
var
  AnItem: TVirtualItem;
begin
  AnItem := GetVirtualItem(Item);
  if lvifText in Mask then
    case SubItem of
      0: Text := AnItem.Title;
      1: Text := AnItem.SubText1;
      2: Text := AnItem.SubText2;
    else
      Text := '';
    end;
  if lvifImage in Mask then
    Image := AnItem.ImageIndex;
  if lvifParam in Mask then
    Param := 0;
  if lvifState in Mask then
    State := State or AnItem.State;
  if lvifIndent in Mask then
    if odd(Item) then { Just to show indenting, no real reason for it }
      Indent := 1
    else
      Indent := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ListView_SetItemState(AnExtListView.Handle, -1, LVIS_SELECTED, LVIS_SELECTED);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ListView_SetItemState(AnExtListView.Handle, -1, 0, LVIS_SELECTED);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: integer;
  s: string;
begin
  s := 'Selected indices: ';
  { Look for next item starting at the top (-1) }
  i := AnExtListView.ELV_GetNextItem(-1, sdAll, [isSelected]);
  while i > -1 do
  begin
    s := s + IntToStr(i) + ' ';
    { Look for next item starting at last index found }
    i := AnExtListView.ELV_GetNextItem(i, sdAll, [isSelected]);
  end;
  ShowMessage(s);
end;

end.

