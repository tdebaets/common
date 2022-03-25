(****************************************************************************
 *
 * Copyright 2016-2018 Tim De Baets
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
 * TExtChkListView VCL component
 *
 ****************************************************************************)

// TODO: handle space-bar triggered checking similar to mouse-triggered checking
//    item should only be checked on spacebar button up, a la windows update list
//    do this for main item as well as subitem checkboxes
// TODO: add option to auto-select item on check/uncheck
// TODO: add support for hot-tracking checkboxes?
// (see removed comments in change history involving FHotIndex + removed code
// involving FLastMouseMoveIndex)

{$I DFS.INC}

unit ExtChkListView;

interface

uses ExtListView, EnhListView, Windows, Graphics, StdCtrls, Classes, Controls,
    ComCtrls, NewCommCtrl, Messages, SysUtils, ComStrs, Forms, Math, UxThemeISX,
    Common2;

type
  PItemParamInfo = ^TItemParamInfo;
  TItemParamInfo = packed record
    Index: Integer;
    ItemState: Integer;
    lItemLParam: Integer;
    Enabled: Boolean;
  end;

type
  TCheckState = (csNormal, csHot, csPressed, csDisabled);

  TLVItemPrePaintEvent = function(Control: TWinControl;
      var NMLVCD: TNMLVCustomDraw; var FontChanged: Boolean): Boolean of object;
  TLVCheckingEvent = function(Sender: TObject;
      ItemIndex: Integer): Boolean of object;
  TLVSubItemCheckingEvent = function(Sender: TObject;
      ItemIndex, SubItemIndex: Integer): Boolean of object;
  TLVBeforeSelectItemEvent = procedure(Sender: TObject; Item: TListItem;
      Selected: Boolean; var AllowSelect: Boolean) of object;

  TCustomExtChkListView = class;

  THackCustomListView = class(TCustomListView);

  TExtChkListColumn = class(TdfsExtListColumn)
  private
    FShowChecks: Boolean;
    procedure SetShowChecks(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShowChecks: Boolean read FShowChecks write SetShowChecks
        default False;
  end;

  TChkListItem = class(TListItem)
  private
    FListView: TCustomExtChkListView;
    FEnabled: Boolean;
    FGrayed: Boolean;
    FCheckedSubItems: TByteSet;
    FDisabledSubItems: TByteSet;
    procedure SetEnabled(Value: Boolean);
    procedure SetGrayed(Value: Boolean);
    function GetSubItemChecked(SubItem: Integer): Boolean;
    procedure SetSubItemChecked(SubItem: Integer; Checked: Boolean);
    function GetSubItemEnabled(SubItem: Integer): Boolean;
    procedure SetSubItemEnabled(SubItem: Integer; Enabled: Boolean);
  public
    constructor Create(Owner: TListItems);
    procedure Assign(Source: TPersistent); override;
    property SubItem_Checked[SubItem: Integer]: Boolean
        read GetSubItemChecked write SetSubItemChecked;
    property SubItem_Enabled[SubItem: Integer]: Boolean
        read GetSubItemEnabled write SetSubItemEnabled;
  published
    property Enabled: Boolean
        read FEnabled write SetEnabled;
    // The Grayed property is not to be confused with the 'disabled' state.
    // Grayed=True means an indeterminate state (mostly used for 3-state
    // checkboxes).
    property Grayed: Boolean
        read FGrayed write SetGrayed;
  end;

  TCheckBoxOptions = class(TPersistent)
  private
    FListView: TCustomListView;
    FFlat: Boolean;
    FLeftMargin: Integer;
    FThemed: Boolean;
    FHighLight: Boolean;
    FGrayedImages: Boolean; // TODO: rename (DisabledImages?) to prevent confusion with TChkListItem
    FCheckOnItemClick: Boolean;
    procedure SetFlat(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetThemed(Value: Boolean);
    procedure SetHighLight(Value: Boolean);
    procedure SetGrayedImages(Value: Boolean);
  public
    constructor Create(Owner: TCustomExtChkListView);
  published
    property Flat: Boolean
        read FFlat write SetFlat default True;
    property LeftMargin: Integer
        read FLeftMargin write SetLeftMargin default 6;
    property Themed: Boolean
        read FThemed write SetThemed default True;
    property HighLight: Boolean
        read FHighLight write SetHighLight default True;
    property GrayedImages: Boolean
        read FGrayedImages write SetGrayedImages default True;
    property CheckOnItemClick: Boolean
        read FCheckOnItemClick Write FCheckOnItemClick default False;
  end;

  TCustomExtChkListView = class(TCustomExtListView)
  private
    FOnItemPrePaint: TLVItemPrePaintEvent;
    FOnItemChecking: TLVCheckingEvent;
    FOnSubItemChecking: TLVSubItemCheckingEvent;
    FOnBeforeSelectItem: TLVBeforeSelectItemEvent;
    FCheckWidth, FCheckHeight: Integer;
    FCheckBoxOptions: TCheckBoxOptions;
    FDisabledColor: TColor;
    FHideItemFocusRect: Boolean;
    FSpaceDown: Boolean;
    FCaptureIndex: Integer; // -1 while not capturing the mouse for an item
    FCaptureSubItemIndex: Integer; // -1 while not capturing the mouse for an item
    FFocusedSubItemIndex: Integer; // -1 while there's no subitem having focus
    FThemeData: HTHEME;
    FAlphaBlend: TAlphaBlend;
    FMSImgLib: Integer;
    FIsVistaOrHigher: Boolean;
    FMemStream: TMemoryStream;
    FBeforePrimaryColumnIdx: Integer;
    FProcessItemChecked: Boolean;
    function GetItemState(Item: TListItem): TCheckBoxState;
    procedure GetCheckRect(const IconRect: TRect; var CheckRect: TRect);
    procedure GetStateRect(Index: Integer; const IconRect: TRect;
        var StateRect: TRect);
    procedure GetSubItemCheckRect(const BoundsRect: TRect; var CheckRect: TRect);
    procedure DrawCheck(hDC: Integer; const CheckRect: TRect;
        State: TCheckBoxState; CheckState: TCheckState);
    procedure DrawDisabledIcon(hDC: Integer; Index: Integer;
        const IconRect: TRect);
    function BeginUpdateWhenHandle: Boolean;
    procedure UpdateItemWhenHandle(ItemIndex: Integer);
    procedure UpdateItemCheck(Index: Integer);
    procedure UpdateSubItemWhenHandle(ItemIndex, SubItemIndex: Integer);
    procedure UpdateColumn(Index: Integer);
    procedure EndCapture(Cancel: Boolean);
    function CanFocusItem(Index: Integer): Boolean;
    function CanFocusSubItem(Item: TListItem; SubItem: Integer): Boolean;
    function GetPrevFocusableSubItem: Integer;
    function GetNextFocusableSubItem: Integer;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetSubItemChecked(Index, SubItemIndex: Integer): Boolean;
    procedure SetSubItemChecked(Index, SubItemIndex: Integer; Checked: Boolean);
    function GetSubItemEnabled(Index, SubItemIndex: Integer): Boolean;
    procedure SetSubItemEnabled(Index, SubItemIndex: Integer; Enabled: Boolean);
    function GetChecksEnabled: Boolean;
    function ColumnHasSubItemChecks(Index: Integer): Boolean;
    procedure DoSubItemCheckClick(Item: TListItem; SubItem: Integer);
    function OnSubItemCheckRect(X, Y: Integer; var SubItem: Integer): Boolean;
    procedure SetHideItemFocusRect(Value: Boolean);
    procedure UpdateBeforePrimaryColumnIdx;
    function NMCustomDraw(NMCustomDraw: PNMCustomDraw): Integer;
    function LVNItemChanging(NMListView: PNMListView): Boolean;
    procedure LVNItemChanged(NMListView: PNMListView);
    function HDNEndDrag: Integer;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure LVMSetColumnOrderArray(var Message: TMessage);
        message LVM_SETCOLUMNORDERARRAY;
    procedure LVMSetExtendedListViewStyle(var Message: TMessage);
        message LVM_SETEXTENDEDLISTVIEWSTYLE;
    procedure UpdateThemeData(const Close, Open: Boolean);
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
        message CM_WANTSPECIALKEY;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function DoMouseDown(Button: TMouseButton; X, Y: Integer): Boolean;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetCheckBoxOptions(Value: TCheckBoxOptions);
    function GetExtListColumnClass: TCollectionItemClass; override;
    function CreateListItem: TListItem; override;
    procedure ItemChecked(ItemIndex: integer; Checked: Boolean); override;
    procedure LVItemPostPaint(const NMLVCD: TNMLVCustomDraw;
        var DrawIcon: Boolean); virtual;
    function ItemChecking(Item: TListItem): Boolean; virtual;
    function SubItemChecking(Item: TListItem; SubItem: Integer): Boolean; virtual;
    procedure ItemCheckedManually; virtual;
    property OnItemPrePaint: TLVItemPrePaintEvent
        read FOnItemPrePaint write FOnItemPrePaint;
    property OnItemChecking: TLVCheckingEvent
        read FOnItemChecking write FOnItemChecking;
    property OnSubItemChecking: TLVSubItemCheckingEvent
        read FOnSubItemChecking write FOnSubItemChecking;
    property OnBeforeSelectItem: TLVBeforeSelectItemEvent
        read FOnBeforeSelectItem write FOnBeforeSelectItem;
    property CheckBoxOptions: TCheckBoxOptions
        read FCheckBoxOptions write SetCheckBoxOptions;
    property ItemEnabled[Index: Integer]: Boolean
        read GetItemEnabled write SetItemEnabled;
    property SubItem_Checked[Index, SubItemIndex: Integer]: Boolean
        read GetSubItemChecked write SetSubItemChecked;
    property SubItem_Enabled[Index, SubItemIndex: Integer]: Boolean
        read GetSubItemEnabled write SetSubItemEnabled;
    property DisabledColor: TColor
        read FDisabledColor write FDisabledColor default clGrayText;
    property HideItemFocusRect: Boolean
        read FHideItemFocusRect write SetHideItemFocusRect default False;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function OnStateIcon(X, Y: Integer): Boolean;
  end;

  TExtChkListView = class(TCustomExtChkListView)
  public
    // new
    property ItemEnabled;
    property SubItem_Checked;
    property SubItem_Enabled;

    // inherited
    property LastColumnClicked;
    property CurrentColumnWidth;
    property HeaderHandle;
    property SubItem_BoundsRect;
    property SubItem_IconRect;
    property SubItem_LabelRect;
    property SubItem_SelectBoundsRect;
    property HotItem;
    property HotCursor;
    property WorkArea;
    property IsChecked;
    property SubItem_ImageIndex;
    property SelectionMark;
    property ItemIndent;
    property CurrentSortAscending;
  published
    // new
    property CheckBoxOptions;
    property OnItemPrePaint;
    property OnItemChecking;
    property OnSubItemChecking;
    property OnBeforeSelectItem;
    property DisabledColor;
    property HideItemFocusRect;

    // inherited
    property Columns;
    property ColumnSearch;
    property HideSelection;
    property ExtendedStyles;
    property VirtualMode;
    property HoverTime;
    property RequireComCtlUpdate;
{$IFDEF BACKGROUND_FIXED}
    property BackgroundImage;
{$ENDIF}
    property NoColumnResize;
    property SaveSettings;
    property ColumnsFormat;

    property OnItemChecked;
    property OnMarqueeBegin;
    property OnItemActivate;
    property OnHotTrack;
    property OnInfoTip;
    property OnVMGetItemInfo;
    property OnVMCacheHint;
    property OnVMFindItem;
    property OnVMStateChanged;
    property OnVMCaptionEdited;
    
    property ShowSortArrows;

    // TCustomListView
    property AutoColumnSort;
    property AutoSortStyle;
    property AutoResort;
    property AutoSortAscending;
    property ReverseSortArrows;
    property Style;

    property OnDrawHeader;
    property OnMeasureItem;
    property OnDrawItem;
    property OnDrawSubItem;
    property OnAfterDefaultDrawItem;
    property OnSortItems;
    property OnSortBegin;
    property OnSortFinished;
    property OnEditCanceled;


    property Align;
{$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode;
{$ENDIF}
    property BorderStyle;
{$IFDEF DFS_COMPILER_4_UP}
    property BorderWidth;
{$ENDIF}
    property Color;
    property ColumnClick;
{$IFDEF DFS_COMPILER_4_UP}
    property Constraints;
{$ENDIF}
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
{$IFDEF DFS_COMPILER_4_UP}
    property DragKind;
{$ENDIF}
    property ReadOnly default False;
    property Enabled;
    property Font;
    property IconOptions;
    property Items;
    property AllocBy;
    property MultiSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
{$IFDEF DFS_COMPILER_4_UP}
    property OnEndDock;
{$ENDIF}
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnGetImageIndex;
{$IFDEF DFS_COMPILER_5_UP}
    property OnGetSubItemImage;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF DFS_COMPILER_4_UP}
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
{$ENDIF}
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
{$IFDEF DFS_COMPILER_4_UP}
    property ParentBiDiMode;
{$ENDIF}
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property LargeImages;
    property SmallImages;
    property StateImages;

    property DoubleBuffered;
  end;

procedure Register;

implementation

uses TmSchemaISX;

{ TCheckBoxOptions }

constructor TCheckBoxOptions.Create(Owner: TCustomExtChkListView);
begin
  inherited Create;
  if not Assigned(Owner) then
    raise Exception.Create(sInvalidOwner);
  FListView := Owner;
  FFlat := True;
  FLeftMargin := 6;
  FThemed := True;
  FHighLight := True;
  FGrayedImages := True;
  FCheckOnItemClick := False;
end;

procedure TCheckBoxOptions.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then begin
    FFlat := Value;
    THackCustomListView(FListView).RecreateWnd;
  end;
end;

procedure TCheckBoxOptions.SetLeftMargin(Value: Integer);
begin
  if Value <> FLeftMargin then begin
    FLeftMargin := Value;
    FListView.Invalidate;
  end;
end;

procedure TCheckBoxOptions.SetThemed(Value: Boolean);
begin
  if Value <> FThemed then begin
    FThemed := Value;
    THackCustomListView(FListView).RecreateWnd;
  end;
end;

procedure TCheckBoxOptions.SetHighLight(Value: Boolean);
begin
  if Value <> FHighLight then begin
    FHighLight := Value;
    FListView.Invalidate;
  end;
end;

procedure TCheckBoxOptions.SetGrayedImages(Value: Boolean);
begin
  if Value <> FGrayedImages then begin
    FGrayedImages := Value;
    FListView.Invalidate;
  end;
end;

{ TExtChkListColumn }

constructor TExtChkListColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FShowChecks := False;
end;

procedure TExtChkListColumn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TExtChkListColumn then
    ShowChecks := TExtChkListColumn(Source).ShowChecks;
end;

procedure TExtChkListColumn.SetShowChecks(Value: Boolean);
begin
  if Value <> FShowChecks then begin
    FShowChecks := Value;
    with Collection as TdfsExtListColumns do begin
      if Assigned(ListView) then with ListView as TCustomExtChkListView do begin
        if Index = FCaptureSubItemIndex then begin
          FCaptureIndex := -1;
          FCaptureSubItemIndex := -1;
        end;
        UpdateColumn(Index);
      end;
    end;
  end;
end;

{ TChkListItem }

const
  DefaultStates: array[Boolean] of TCheckBoxState =
      (cbUnchecked, cbChecked);

constructor TChkListItem.Create(Owner: TListItems);
begin
  inherited Create(Owner);
  FListView := Owner.Owner as TCustomExtChkListView;
  FEnabled := True;
  FGrayed := False;
  FCheckedSubItems := [];
  FDisabledSubItems := [];
end;

procedure TChkListItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  FListView.UpdateItemWhenHandle(Self.Index);
end;

procedure TChkListItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TChkListItem then with TChkListItem(Source) do begin
    Self.Enabled := Enabled;
    Self.Grayed := Grayed;
  end;
end;

procedure TChkListItem.SetGrayed(Value: Boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    FListView.FProcessItemChecked := False;
    try
      Checked := False;
    finally
      FListView.FProcessItemChecked := True;
    end;
    FListView.UpdateItemWhenHandle(Self.Index);
  end;
end;

function TChkListItem.GetSubItemChecked(SubItem: Integer): Boolean;
begin
  if SubItem <= High(Byte) then
    Result := (Byte(SubItem) in FCheckedSubItems)
  else
    Result := False;
end;

procedure TChkListItem.SetSubItemChecked(SubItem: Integer; Checked: Boolean);
begin
  if SubItem <= High(Byte) then begin
    if Checked then
      Include(FCheckedSubItems, Byte(SubItem))
    else
      Exclude(FCheckedSubItems, Byte(SubItem));
    FListView.UpdateSubItemWhenHandle(Self.Index, SubItem);
  end;
end;

function TChkListItem.GetSubItemEnabled(SubItem: Integer): Boolean;
begin
  if SubItem <= High(Byte) then
    Result := not (Byte(SubItem) in FDisabledSubItems)
  else
    Result := True;
end;

procedure TChkListItem.SetSubItemEnabled(SubItem: Integer; Enabled: Boolean);
begin
  if SubItem <= High(Byte) then begin
    if Enabled then
      Exclude(FDisabledSubItems, Byte(SubItem))
    else begin
      // if we're disabling the currently focused subitem, remove focus
      if (FListView.ItemFocused = Self)
          and (FListView.FFocusedSubItemIndex = SubItem) then
        FListView.FFocusedSubItemIndex := -1;
      Include(FDisabledSubItems, Byte(SubItem));
    end;
    FListView.UpdateSubItemWhenHandle(Self.Index, SubItem);
  end;
end;

{ TCustomExtChkListView }

constructor TCustomExtChkListView.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  with Graphics.TBitmap.Create do try
    Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
    FCheckWidth := Width div 4;
    FCheckHeight := Height div 3;
  finally
    Free;
  end;
  FCheckBoxOptions := TCheckBoxOptions.Create(Self);
  FCaptureIndex := -1;
  FCaptureSubItemIndex := -1;
  FFocusedSubItemIndex := -1;
  FDisabledColor := clGrayText;
  FHideItemFocusRect := False;
  FBeforePrimaryColumnIdx := 0; // assume primary column is first
  FProcessItemChecked := True;
  FAlphaBlend := nil;
  FMSImgLib := LoadLibrary( 'msimg32.dll');
  if FMSImgLib <> 0 then
    FAlphaBlend := GetProcAddress(FMSImgLib, 'AlphaBlend');
  FIsVistaOrHigher := IsWindowsVistaOrHigher;
end;

procedure TCustomExtChkListView.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  SendMessage(Handle, CCM_SETVERSION, 5, 0);
  UpdateThemeData(True, True);
end;

procedure TCustomExtChkListView.CreateWnd;
var
  i: Integer;
  Count: Integer;
  BoolVal: Boolean;
  ByteSet: TByteSet;
begin
  inherited CreateWnd;
  if Assigned(FMemStream) then begin
    FMemStream.Read(Count, SizeOf(Count));
    // Items are sorted in TCustomEnhListView.Loaded, so make sure that this
    // method already has been called, otherwise the item enabled states would
    // be restored in the wrong order!
    // If UpdateCount > 0, the listview is still being created during which
    // sorting is suppressed.
    if (Count > 0) and (csLoading in ComponentState)
        and (UpdateCount < 1) then begin
      Items.Clear;
      raise EComponentError.CreateFmt('%s: cannot restore item states while ' +
          'component is still loading', [Name]);
    end;
    for i := 0 to Count - 1 do
        with Items[i] as TChkListItem do begin
      // restore item enabled state
      FMemStream.Read(BoolVal, SizeOf(BoolVal));
      Enabled := BoolVal;
      // restore subitem checked states
      FMemStream.Read(ByteSet, SizeOf(ByteSet));
      FCheckedSubItems := ByteSet;
      // restore subitem enabled states
      FMemStream.Read(ByteSet, SizeOf(ByteSet));
      FDisabledSubItems := ByteSet;
    end;
    FreeAndNil(FMemStream);
  end;
end;

procedure TCustomExtChkListView.DestroyWnd;
var
  i: Integer;
  Count: Integer;
begin
  if Assigned(FMemStream) then
    FMemStream.Size := 0
  else
    FMemStream := TMemoryStream.Create;
  Count := Items.Count;
  FMemStream.Write(Count, SizeOf(Count));
  for i := 0 to Items.Count - 1 do
      with Items[i] as TChkListItem do begin
    // save item enabled state
    FMemStream.Write(Enabled, SizeOf(Enabled));
    // save subitem checked states
    FMemStream.Write(FCheckedSubItems, SizeOf(FCheckedSubItems));
    // save subitem enabled states
    FMemStream.Write(FDisabledSubItems, SizeOf(FDisabledSubItems));
  end;
  FMemStream.Position := 0;
  inherited DestroyWnd;
end;

destructor TCustomExtChkListView.Destroy;
begin
  UpdateThemeData(True, False);
  if FMSImgLib <> 0 then begin
    FreeLibrary(FMSImgLib);
    FMSImgLib := 0;
  end;
  FreeAndNil(FMemStream);
  FreeAndNil(FCheckBoxOptions);
  inherited Destroy;
end;

function TCustomExtChkListView.GetChecksEnabled: Boolean;
begin
  Result := not (ViewStyle = vsIcon) and (lvxCheckBoxes in ExtendedStyles);
end;

function TCustomExtChkListView.ColumnHasSubItemChecks(Index: Integer): Boolean;
var
  Column: TdfsExtListColumn;
begin
  Result := False;
  if ViewStyle <> vsReport then
    Exit;
  if (Index < 0) or (Index >= ColumnsFormat.Count) then
    Exit;
  Column := ColumnsFormat[Index];
  if not (Column is TExtChkListColumn) then
    Exit;
  Result := TExtChkListColumn(Column).ShowChecks;
end;

procedure TCustomExtChkListView.DoSubItemCheckClick(Item: TListItem;
    SubItem: Integer);
begin
  if not ColumnHasSubItemChecks(SubItem) then
    Exit;
  with Item as TChkListItem do begin
    if not SubItem_Enabled[SubItem] then
      Exit;
    SubItem_Checked[SubItem] := not SubItem_Checked[SubItem]
  end;
end;

procedure TCustomExtChkListView.SetHideItemFocusRect(Value: Boolean);
begin
  if Value <> FHideItemFocusRect then begin
    FHideItemFocusRect := Value;
    if Assigned(ItemFocused) then
      UpdateItemWhenHandle(ItemFocused.Index);
  end;
end;

procedure TCustomExtChkListView.UpdateThemeData(const Close, Open: Boolean);
begin
  if not FCheckBoxOptions.Themed then begin
    // TODO: missing call to CloseThemeData?
    FThemeData := 0;
    Exit;
  end;
  
  if Close then begin
    if FThemeData <> 0 then begin
      CloseThemeData(FThemeData);
      FThemeData := 0;
    end;
  end;

  if Open then begin
    if UseThemes then
      FThemeData := OpenThemeData(Handle, 'Button')
    else
      FThemeData := 0;
  end;
end;

function TCustomExtChkListView.GetItemState(Item: TListItem): TCheckBoxState;
const
  CheckBoxStates: array[Boolean, Boolean] of TCheckBoxState =
      ((cbUnchecked, cbGrayed), (cbChecked, cbChecked));
begin
  // Using Checked directly doesn't seem to work
  if Item is TChkListItem then with Item as TChkListItem do
    Result := CheckBoxStates[IsChecked[Index]][Grayed]
  else
    Result := DefaultStates[IsChecked[Item.Index]];
end;

procedure TCustomExtChkListView.UpdateBeforePrimaryColumnIdx;
var
  ColCount: Integer;
  ColArray: PIntArray;
  PrimaryColumnIdx: Integer;
  i: Integer;
begin
  ColCount := Columns.Count;
  GetMem(ColArray, SizeOf(Integer) * ColCount);
  try
    GetColumnOrder(ColCount, ColArray^);
    PrimaryColumnIdx := 0;
    for i := 0 to ColCount - 1 do begin
      if ColArray[i] = 0 then begin
        PrimaryColumnIdx := i;
        Break;
      end;
    end;
    if PrimaryColumnIdx = 0 then
      FBeforePrimaryColumnIdx := 0
    else
      FBeforePrimaryColumnIdx := ColArray[PrimaryColumnIdx - 1];
  finally
    FreeMem(ColArray);
  end;
end;

procedure TCustomExtChkListView.LVMSetColumnOrderArray(var Message: TMessage);
begin
  inherited;
  UpdateBeforePrimaryColumnIdx;
end;

{ Workaround for a bug in TListItem.SetChecked: it will always set the
  LVS_EX_CHECKBOXES style, even if it's already set. This causes unnecessary
  flicker and all checkboxes being reset to unchecked state.
  This message handler will only forward LVM_SETEXTENDEDLISTVIEWSTYLE messages
  setting LVS_EX_CHECKBOXES if the new style differs from the old. }
procedure TCustomExtChkListView.LVMSetExtendedListViewStyle(var Message: TMessage);
var
  Styles: Integer;
begin
  if Message.LParam and LVS_EX_CHECKBOXES <> 0 then begin
    Styles := ListView_GetExtendedListViewStyle(Handle);
    if Styles <> Message.LParam then
      inherited;
  end
  else
    inherited;
end;

procedure TCustomExtChkListView.GetCheckRect(const IconRect: TRect;
    var CheckRect: TRect);
var
  MarginTop: Integer;
begin
  CheckRect := IconRect;
  Dec(CheckRect.Left, FCheckWidth + 10);
  Inc(CheckRect.Left, CheckBoxOptions.LeftMargin);
  CheckRect.Right := CheckRect.Left + FCheckWidth;
  MarginTop := (RectHeight(CheckRect) - FCheckHeight) div 2;
  MarginTop := Max(MarginTop, 0);
  Inc(CheckRect.Top, MarginTop);
  CheckRect.Bottom := CheckRect.Top + FCheckHeight;
end;

procedure TCustomExtChkListView.GetStateRect(Index: Integer;
    const IconRect: TRect; var StateRect: TRect);
var
  BoundsRect: TRect;
begin
  BoundsRect := Rect(0, 0, 0, 0);
  StateRect.Top := IconRect.Top;
  StateRect.Bottom := IconRect.Bottom;
  StateRect.Left := 0;
  if (ViewStyle <> vsReport) or (FBeforePrimaryColumnIdx = 0) then begin
    // primary column is the first, so left bound of state icon = left bound of
    // whole item
    ListView_GetItemRect(Handle, Index, BoundsRect, LVIR_BOUNDS);
    StateRect.Left := BoundsRect.Left;
  end
  else begin
    // primary column is not first, so left bound of state icon = right bound of
    // previous column
    ListView_GetSubItemRect(Handle, Index, FBeforePrimaryColumnIdx, LVIR_BOUNDS,
        @BoundsRect);
    StateRect.Left := BoundsRect.Right;
  end;
  StateRect.Right := IconRect.Left;
end;

procedure TCustomExtChkListView.GetSubItemCheckRect(const BoundsRect: TRect;
    var CheckRect: TRect);
var
  MarginTop: Integer;
begin
  CheckRect := BoundsRect;
  Inc(CheckRect.Left, (RectWidth(BoundsRect) - FCheckWidth) div 2);
  CheckRect.Right := CheckRect.Left + FCheckWidth;
  MarginTop := (RectHeight(CheckRect) - FCheckHeight) div 2;
  MarginTop := Max(MarginTop, 0);
  Inc(CheckRect.Top, MarginTop);
  CheckRect.Bottom := CheckRect.Top + FCheckHeight;
end;

procedure TCustomExtChkListView.DrawCheck(hDC: Integer; const CheckRect: TRect;
    State: TCheckBoxState; CheckState: TCheckState);
var
  uState: Cardinal;
const
  FrameControlStates: array[TCheckState] of Cardinal =
    (0, 0, DFCS_PUSHED, DFCS_INACTIVE);
  CheckStateIds: array [TCheckBoxState, TCheckState] of Integer =
  (
    (CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED),
    (CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED),
    (CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
begin
  if FThemeData = 0 then begin
    case State of
      cbChecked: uState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked: uState := DFCS_BUTTONCHECK;
      else
        uState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    if CheckBoxOptions.Flat then
      uState := uState or DFCS_FLAT;
    uState := uState or FrameControlStates[CheckState];
    DrawFrameControl(hDC, CheckRect, DFC_BUTTON, uState);
  end
  else begin
    DrawThemeBackGround(FThemeData, hDC, BP_CHECKBOX,
        CheckStateIds[State][CheckState], CheckRect, @CheckRect);
  end;
end;

procedure TCustomExtChkListView.DrawDisabledIcon(hDC: Integer; Index: Integer;
    const IconRect: TRect);
var
  MarginTop: Integer;
  Rect: TRect;
begin
  if not Assigned(SmallImages) or (SmallImages.Handle = 0) then
    Exit;
  FillRect(hDC, IconRect, ColorToRGB(Color));
  Rect := IconRect;
  // In Large Fonts mode, the height of the item can be larger than the
  // small images height. In that case, Windows Vista and higher will draw
  // the icons centered.
  // So we need to center as well when drawing the (disabled) icon ourselves.
  if FIsVistaOrHigher then begin
    MarginTop := (RectHeight(Rect) - SmallImages.Height) div 2;
    MarginTop := Max(MarginTop, 0);
    Inc(Rect.Top, MarginTop);
  end;
  // TODO: use ImageList_DrawIndirect with ILS_SATURATE on comctl32 version 6?
  // see https://stackoverflow.com/questions/6003018/make-disabled-menu-and-toolbar-images-look-better
  ImageList_DrawEx(SmallImages.Handle, Items[Index].ImageIndex, hDC,
      Rect.Left, Rect.Top, SmallImages.Width, SmallImages.Height,
      CLR_DEFAULT, ColorToRGB(clGrayText), ILD_BLEND50);
end;

function TCustomExtChkListView.NMCustomDraw(NMCustomDraw: PNMCustomDraw): Integer;
  function GetItem: TChkListItem;
  begin
    if (NMCustomDraw.lItemLParam <> 0)
        and (TObject(NMCustomDraw.lItemLParam) is TChkListItem) then
      Result := TChkListItem(NMCustomDraw.lItemLParam)
    else
      Result := nil;
  end;
  function IsItemEnabled(Item: TChkListItem): Boolean;
  begin
    if Assigned(Item) then
      Result := Item.Enabled
    else
      Result := True;
  end;
  procedure HandleItemPrePaint;
  var
    Item: TChkListItem;
    NewFont: Boolean;
  begin
    NewFont := False;
    if FHideItemFocusRect then begin
      with NMCustomDraw^ do
        uItemState := uItemState and not CDIS_FOCUS;
    end;
    Item := GetItem;
    if not IsItemEnabled(Item) then begin
      PNMLVCustomDraw(NMCustomDraw).clrText := ColorToRGB(FDisabledColor);
      NewFont := True;
    end;
    if Assigned(FOnItemPrePaint) then begin
      if not FOnItemPrePaint(Self, PNMLVCustomDraw(NMCustomDraw)^, NewFont) then
        Result := Result or CDRF_SKIPDEFAULT;
    end;
    // Setting the item state here causes unattractive 'delayed' icons when
    // painted in LVItemPostPaint.
    Result := Result or CDRF_NOTIFYPOSTPAINT;
    if NewFont then
      Result := Result or CDRF_NEWFONT;
    Result := Result or CDRF_NOTIFYSUBITEMDRAW;
  end;
  procedure HandleItemPostPaint;
  var
    DrawIcon: Boolean;
    Item: TChkListItem;
    IconRect, CheckRect, StateRect: TRect;
    CheckState: TCheckState;
    IsSelected: Boolean;
  begin
    DrawIcon := True;
    LVItemPostPaint(PNMLVCustomDraw(NMCustomDraw)^, DrawIcon);
    if not GetChecksEnabled then
      Exit;
    Item := GetItem;
    if Assigned(Item) then with NMCustomDraw^ do begin
      // setting the item state here causes unattractive 'delayed' icons
      // when painted in LVItemPostPaint
      if not ListView_GetItemRect(Handle, dwItemSpec, IconRect, LVIR_ICON) then
        Exit;
      // prevent drawing on the column header
      if (IconRect.Top < 5) and (ViewStyle = vsReport) and ShowColumnHeaders then
        Exit;
      // erase existing state icon (checkmark)
      GetStateRect(dwItemSpec, IconRect, StateRect);
      FillRect(hDC, StateRect, Brush.Handle);
      GetCheckRect(IconRect, CheckRect);
      if not IsItemEnabled(Item) then
        CheckState := csDisabled
      else if (Integer(dwItemSpec) = FCaptureIndex)
          and (FCaptureSubItemIndex = 0) then
        CheckState := csPressed
      else
        CheckState := csNormal;
      DrawCheck(hDC, CheckRect, GetItemState(Item), CheckState);
      IsSelected := (ListView_GetItemState(Handle, dwItemSpec, LVIS_SELECTED) =
          LVIS_SELECTED);
      if IsSelected and Focused
          and ((FThemeData <> 0) or CheckBoxOptions.HighLight) then
        AlphaHighLight(hDC, CheckRect, FAlphaBlend)
      else if DrawIcon and CheckBoxOptions.GrayedImages
          and not IsItemEnabled(Item) then
        DrawDisabledIcon(hDC, dwItemSpec, IconRect);
    end;
  end;
  procedure HandleSubItemPostPaint;
  var
    SubItemRect, CheckRect: TRect;
    Item: TChkListItem;
    IsSelected: Boolean;
    State: TCheckBoxState;
    CheckState: TCheckState;
  begin
    with NMCustomDraw^, PNMLVCustomDraw(NMCustomDraw)^ do begin
      // Comctl32 can send us 'fake' custom draw notifications
      // (via CLVDrawItemManager::BeginFakeItemDraw - CIFakeCustomDrawNotify),
      // such as for computing a column's width, or while pausing the mouse
      // cursor over a subitem when the lvxInfoTip or lvxLabelTip extended style
      // is set.
      // In this case, most fields in NMLVCustomDraw (like uItemState, clrText,
      // clrTextBk...) will be left uninitialized so if we would use these fields
      // we would access garbage data. Comctl32 *does* however initialize the rc
      // field to all zeroes when sending these fake notifications, so we can
      // check that field to detect this and completely skip the drawing.
      // This can be easily reproduced by pausing the cursor over a selected
      // subitem because then the CDIS_SELECTED flag in uItemState suddenly won't
      // be set anymore.
      // IsRectEmpty(rc) always returns TRUE on XP because the rect height is
      // always 0 there. So check just the width instead.
      if RectWidth(rc) = 0 then
        Exit;
      if ColumnHasSubItemChecks(iSubItem) then begin
        Item := GetItem;
        if not Assigned(Item) then
          Exit;
        IsSelected := (uItemState and CDIS_SELECTED) <> 0;
        // Can't use rc here for XP compatibility (see above)
        if not ListView_GetSubItemRect(hdr.hwndFrom, dwItemSpec, iSubItem,
            LVIR_BOUNDS, @SubItemRect) then
          Exit;
        GetSubItemCheckRect(SubItemRect, CheckRect);
        if Item.SubItem_Checked[iSubItem] then
          State := cbChecked
        else
          State := cbUnchecked;
        if not Item.SubItem_Enabled[iSubItem] then
          CheckState := csDisabled
        else if (Integer(dwItemSpec) = FCaptureIndex)
            and (iSubItem = FCaptureSubItemIndex) then
          CheckState := csPressed
        else
          CheckState := csNormal;
        DrawCheck(hdc, CheckRect, State, CheckState);
        if Focused then begin
          if IsSelected and ((FThemeData <> 0) or CheckBoxOptions.HighLight) then
            AlphaHighLight(hdc, CheckRect, FAlphaBlend);
          // Can't check uItemState for CDIS_FOCUS here because we may have
          // removed that state in HandleItemPrePaint
          if Item.SubItem_Enabled[iSubItem] and (Item = ItemFocused)
              and (iSubItem = FFocusedSubItemIndex) then begin
            InflateRect(CheckRect, 2, 2);
            // Prevent the focus rect from crossing the subitem's bounding rect
            IntersectRect(CheckRect, CheckRect, SubItemRect);
            DrawFocusRect(hdc, CheckRect);
          end;
        end;
      end;
    end;
  end;
begin
  Result := CDRF_DODEFAULT;
  case NMCustomDraw.dwDrawStage of
    CDDS_PREPAINT: begin
      Result := Result or CDRF_NOTIFYITEMDRAW;
      Result := Result or CDRF_NOTIFYPOSTPAINT;
    end;
    CDDS_ITEMPREPAINT:
      HandleItemPrePaint;
    CDDS_ITEMPOSTPAINT:
      HandleItemPostPaint;
    CDDS_ITEMPREPAINT or CDDS_SUBITEM:
      Result := Result or CDRF_NOTIFYPOSTPAINT;
    CDDS_ITEMPOSTPAINT or CDDS_SUBITEM:
      HandleSubItemPostPaint;
  end;
end;

function TCustomExtChkListView.LVNItemChanging(NMListView: PNMListView): Boolean;
begin
  Result := True; // default: allow the change
  with NMListView^ do begin
    if Assigned(FOnBeforeSelectItem) and (uChanged = LVIF_STATE) then begin
      if (uOldState and LVIS_SELECTED <> 0) and
        (uNewState and LVIS_SELECTED = 0) then
        FOnBeforeSelectItem(Self, Items[iItem], False, Result)
      else if (uOldState and LVIS_SELECTED = 0) and
        (uNewState and LVIS_SELECTED <> 0) then
        FOnBeforeSelectItem(Self, Items[iItem], True, Result);
    end;
  end;
end;

procedure TCustomExtChkListView.LVNItemChanged(NMListView: PNMListView);
var
  Item: TListItem;
begin
  with NMListView^ do begin
    if (iItem < 0) or (iItem >= Items.Count) then
      Exit;
    Item := Items[iItem];
    if (uChanged and LVIF_STATE <> 0) then begin
      if (uOldState and LVIS_FOCUSED = 0)
          and (uNewState and LVIS_FOCUSED <> 0) then begin
        // focused item changed, check if we can still focus the subitem
        if FFocusedSubItemIndex >= 0 then begin
          if not CanFocusSubItem(Item, FFocusedSubItemIndex) then
            FFocusedSubItemIndex := -1;
        end;
      end;
    end;
  end;
end;

function TCustomExtChkListView.HDNEndDrag: Integer;
begin
  // post a bogus LVM_SETCOLUMNORDERARRAY to trigger the update of the index of
  // the column before the primary one (let the message loop process other
  // messages first to change the actual column order returned by
  // LVM_GETCOLUMNORDERARRAY)
  PostMessage(Handle, LVM_SETCOLUMNORDERARRAY, 0, 0);
  Result := Integer(False);
end;

procedure TCustomExtChkListView.WMNotify(var Message: TWMNotify);
begin
  inherited;
  with Message do begin
    case NMHdr^.code of
      HDN_ENDDRAG:
        Result := HDNEndDrag;
    end;
  end;
end;

procedure TCustomExtChkListView.CNNotify(var Message: TWMNotify);
begin
  inherited;
  with Message do begin
    case NMHdr^.code of
      NM_CUSTOMDRAW:
        Result := NMCustomDraw(PNMCustomDraw(NMHdr));
      LVN_ITEMCHANGING: begin
        if not LVNItemChanging(PNMListView(NMHdr)) then
          Result := Integer(True); // prevent the change
      end;
      LVN_ITEMCHANGED:
        LVNItemChanged(PNMListView(NMHdr));
    end;
  end;
end;

procedure TCustomExtChkListView.SetCheckBoxOptions(Value: TCheckBoxOptions);
begin
  with FCheckBoxOptions do begin
    Flat := Value.Flat;
    LeftMargin := Value.LeftMargin;
    Themed := Value.Themed;
    HighLight := Value.HighLight;
    GrayedImages := Value.GrayedImages;
    CheckOnItemClick := Value.CheckOnItemClick;
  end;
end;

function TCustomExtChkListView.GetItemEnabled(Index: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  Item := Items[Index];
  if Item is TChkListItem then
    Result := TChkListItem(Item).Enabled;
end;

procedure TCustomExtChkListView.SetItemEnabled(Index: Integer; Value: Boolean);
var
  Item: TListItem;
begin
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    TChkListItem(Item).Enabled := Value;
end;

function TCustomExtChkListView.GetSubItemChecked(Index: Integer;
    SubItemIndex: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := False;
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    Result := TChkListItem(Item).SubItem_Checked[SubItemIndex];
end;

procedure TCustomExtChkListView.SetSubItemChecked(Index, SubItemIndex: Integer;
    Checked: Boolean);
var
  Item: TListItem;
begin
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    TChkListItem(Item).SubItem_Checked[SubItemIndex] := Checked;
end;

function TCustomExtChkListView.GetSubItemEnabled(Index: Integer;
    SubItemIndex: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    Result := TChkListItem(Item).SubItem_Enabled[SubItemIndex];
end;

procedure TCustomExtChkListView.SetSubItemEnabled(Index, SubItemIndex: Integer;
    Enabled: Boolean);
var
  Item: TListItem;
begin
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    TChkListItem(Item).SubItem_Enabled[SubItemIndex] := Enabled;
end;

function TCustomExtChkListView.BeginUpdateWhenHandle: Boolean;
begin
  // BeginUpdate() internally allocates a window handle if this hasn't happened
  // yet. If this method is being called early in the component's initialization
  // phase, a handle already gets created and immediately destroyed again (due
  // to a RecreateWnd() call when reading properties from the form's resource).
  // Destroying the window handle saves the initial component's dimensions to a
  // memory stream. After this, these dimensions are scaled when being displayed
  // on a high DPI monitor (still before the window handle is recreated). When
  // the handle is subsequently recreated again, the old (non-scaled) dimensions
  // are incorrectly being restored. To prevent this, we only call BeginUpdate()
  // when a window handle is allocated.
  Result := HandleAllocated;
  if Result then
    BeginUpdate;
end;

procedure TCustomExtChkListView.UpdateItemWhenHandle(ItemIndex: Integer);
begin
  // See comment in BeginUpdateWhenHandle method
  if not HandleAllocated then
    Exit;
  UpdateItems(ItemIndex, ItemIndex);
end;

// Workaround to redraw only the checkmark of an item, and not the whole item
procedure TCustomExtChkListView.UpdateItemCheck(Index: Integer);
var
  Checked: Boolean;
begin
  Checked := ListView_GetCheckState(Handle, Index) <> 0;
  ListView_SetCheckState(Handle, Index, not Checked);
  ListView_SetCheckState(Handle, Index, Checked);
end;

procedure TCustomExtChkListView.UpdateSubItemWhenHandle(ItemIndex,
    SubItemIndex: Integer);
begin
  UpdateItemWhenHandle(ItemIndex); // TODO: improve
end;

procedure TCustomExtChkListView.UpdateColumn(Index: Integer);
var
  NeedsEndUpdate: Boolean;
  OrigWidth: TWidth;
begin
  if Index >= Columns.Count then
    Exit;
  NeedsEndUpdate := BeginUpdateWhenHandle;
  try
    with Column[Index] do begin
      // For lack of a better way to update a column, we simply do it by
      // changing the width to 0, and then changing it back.
      OrigWidth := WidthType;
      Width := 0;
      Width := OrigWidth;
    end;
  finally
    if NeedsEndUpdate then
      EndUpdate;
  end;
end;

procedure TCustomExtChkListView.EndCapture(Cancel: Boolean);
var
  ItemIdx, SubItemIdx: Integer;
  Checked: Boolean;
  Item: TListItem;
begin
  ItemIdx := FCaptureIndex;
  SubItemIdx := FCaptureSubItemIndex;
  if ItemIdx >= 0 then begin
    FSpaceDown := False;
    FCaptureIndex := -1;
    FCaptureSubItemIndex := -1;
    if GetChecksEnabled and (SubItemIdx = 0) then begin
      // main item checkbox was clicked
      if not Cancel and ItemEnabled[ItemIdx] then begin
        Checked := (ListView_GetCheckState(Handle, ItemIdx) <> 0);
        // Changing Items[ItemIdx].Checked causes lots of flicker on XP!!! - see
        // LVMSetExtendedListViewStyle
        ListView_SetCheckState(Handle, ItemIdx, not Checked);
        ItemCheckedManually;
      end
      else
        UpdateItemCheck(ItemIdx);
    end
    else if ItemIdx < Items.Count then begin
      // subitem checkbox was clicked
      Item := Items[ItemIdx];
      // subitem enabled/disabled state gets checked in DoSubItemCheckClick
      if not Cancel then
        DoSubItemCheckClick(Item, SubItemIdx);
      UpdateSubItemWhenHandle(Item.Index, SubItemIdx);
    end;
  end;
  if MouseCapture then
    MouseCapture := False;
end;

function TCustomExtChkListView.CanFocusItem(Index: Integer): Boolean;
begin
  Result := Self.Enabled and ItemEnabled[Index];
end;

function TCustomExtChkListView.CanFocusSubItem(Item: TListItem;
    SubItem: Integer): Boolean;
begin
  Result := False;
  if not Self.Enabled then
    Exit;
  if not (Item is TChkListItem) then
    Exit;
  Result := TChkListItem(Item).SubItem_Enabled[SubItem];
end;

function TCustomExtChkListView.GetPrevFocusableSubItem: Integer;
var
  FocusItem: TListItem;
  i: Integer;
begin
  Result := -1;
  FocusItem := ItemFocused;
  if not Assigned(FocusItem) then
    Exit;
  for i := FFocusedSubItemIndex - 1 downto 0 do begin
    if ColumnHasSubItemChecks(i) and CanFocusSubItem(FocusItem, i) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TCustomExtChkListView.GetNextFocusableSubItem: Integer;
var
  FocusItem: TListItem;
  i: Integer;
begin
  Result := -1;
  FocusItem := ItemFocused;
  if not Assigned(FocusItem) then
    Exit;
  for i := FFocusedSubItemIndex + 1 to ColumnsFormat.Count - 1 do begin
    if ColumnHasSubItemChecks(i) and CanFocusSubItem(FocusItem, i) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TCustomExtChkListView.OnStateIcon(X, Y: Integer): Boolean;
var
  HTI: TLVHitTestInfo;
begin
  Result := True;
  FillChar(HTI, SizeOf(HTI), 0);
  // TODO: move this check
  if CheckBoxOptions.CheckOnItemClick then
    Exit;
  HTI.pt.x := X;
  HTI.pt.y := Y;
  ListView_HitTest(Handle, HTI);
  Result := (HTI.flags = LVHT_ONITEMSTATEICON);
end;

function TCustomExtChkListView.OnSubItemCheckRect(X, Y: Integer;
    var SubItem: Integer): Boolean;
var
  HTI: TLVHitTestInfo;
  SubItemRect, CheckRect: TRect;
begin
  Result := False;
  FillChar(HTI, SizeOf(HTI), 0);
  HTI.pt.x := X;
  HTI.pt.y := Y;
  ListView_SubItemHitTest(Handle, @HTI);
  if (HTI.iItem < 0) or (HTI.iSubItem <= 0) then
    Exit;
  if (HTI.flags and LVHT_ONITEMLABEL) = 0 then
    Exit;
  if not ListView_GetSubItemRect(Handle, HTI.iItem, HTI.iSubItem, LVIR_BOUNDS,
      @SubItemRect) then
    Exit;
  GetSubItemCheckRect(SubItemRect, CheckRect);
  InflateRect(CheckRect, 2, 2);
  if not PtInRect(CheckRect, Point(X, Y)) then
    Exit;
  SubItem := HTI.iSubItem;
  Result := True;
end;

procedure TCustomExtChkListView.KeyDown(var Key: Word; Shift: TShiftState);
var
  FocusItem: TListItem;
begin
  if (Key = VK_SPACE) and not IsEditing and not (ssAlt in Shift)
      and (FCaptureIndex < 0) and not FSpaceDown then begin
    FocusItem := ItemFocused;
    if Assigned(FocusItem) and (FFocusedSubItemIndex >= 0)
        and ColumnHasSubItemChecks(FFocusedSubItemIndex)
        and CanFocusSubItem(FocusItem, FFocusedSubItemIndex) then begin
      // subitem checkbox has focus
      if not SubItemChecking(FocusItem, FFocusedSubItemIndex) then
        Exit;
      FCaptureIndex := FocusItem.Index;
      FCaptureSubItemIndex := FFocusedSubItemIndex;
      FSpaceDown := True;
      KeyUp(Key, Shift);
    end
    else if GetChecksEnabled and Assigned(Selected)
        and CanFocusItem(Selected.Index) then begin
      // main item checkbox has focus
      if not ItemChecking(Selected) then
        Exit;  
      FCaptureIndex := Selected.Index;
      FCaptureSubItemIndex := 0;
      FSpaceDown := True;
      KeyUp(Key, Shift);
    end;
  end;
  inherited;
end;

procedure TCustomExtChkListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and not IsEditing and FSpaceDown
      and (FCaptureIndex >= 0) then
    EndCapture(False)
end;

function TCustomExtChkListView.DoMouseDown(Button: TMouseButton;
    X, Y: Integer): Boolean;
var
  Item: TListItem;
  SubItem: Integer;
begin
  Result := True;
  Item := GetItemAt(X, Y);
  if (Button in [mbLeft, mbRight]) and Assigned(Item)
      and not FSpaceDown then begin
    if GetChecksEnabled and OnStateIcon(X, Y) then begin
      // main item checkbox was clicked
      Result := False; // prevent default listview handling from changing the item
      if not CanFocusItem(Item.Index) then
        Exit;
      if not ItemChecking(Item) then
        Exit;
      SetFocus;
      if not MouseCapture then
        MouseCapture := True;
      FCaptureIndex := Item.Index;
      FCaptureSubItemIndex := 0;
      UpdateItemCheck(Item.Index);
      MouseUp(Button, [], X, Y);
    end
    else if OnSubItemCheckRect(X, Y, SubItem)
        and ColumnHasSubItemChecks(SubItem) then begin
      // subitem checkbox was clicked
      // TODO: make optional
      Result := False; // prevent default listview handling from selecting the item
      if not CanFocusSubItem(Item, SubItem) then
        Exit;
      if not SubItemChecking(Item, SubItem) then
        Exit;
      SetFocus;
      if not MouseCapture then
        MouseCapture := True;
      FCaptureIndex := Item.Index;
      FCaptureSubItemIndex := SubItem;
      ItemFocused := Item;
      FFocusedSubItemIndex := SubItem;
      UpdateSubItemWhenHandle(Item.Index, SubItem);
    end;
  end;
end;

procedure TCustomExtChkListView.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
const
  VirtKeys: array[Boolean, TMouseButton] of Integer =
  (
    (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON),
    (VK_RBUTTON, VK_LBUTTON, VK_MBUTTON)
  );
var
  SwapButtons, CancelCapture: Boolean;
  Item: TListItem;
  SubItem: Integer;
begin
  SwapButtons := LongBool(GetSystemMetrics(SM_SWAPBUTTON));
  if GetKeyState(VirtKeys[SwapButtons, Button]) < 0 then
    Exit;
  if (Button in [mbLeft, mbRight]) and not FSpaceDown
      and (FCaptureIndex >= 0) then begin
    Item := GetItemAt(X, Y);
    CancelCapture := not Assigned(Item) or (Item.Index <> FCaptureIndex);
    if FCaptureSubItemIndex > 0 then begin
      // subitem checkboxes
      CancelCapture := CancelCapture or not OnSubItemCheckRect(X, Y, SubItem)
          or (SubItem <> FCaptureSubItemIndex);
      EndCapture(CancelCapture);
    end
    else if GetChecksEnabled then begin
      // main item checkboxes
      CancelCapture := CancelCapture or not OnStateIcon(X, Y);
      EndCapture(CancelCapture);
    end;
  end;
  inherited;
end;

procedure TCustomExtChkListView.CMEnter(var Message: TCMEnter);
var
  FocusItem: TListItem;
begin
  FocusItem := ItemFocused;
  if Assigned(FocusItem) then begin
    FFocusedSubItemIndex := GetNextFocusableSubItem;
    if FFocusedSubItemIndex >= 0 then
      UpdateSubItemWhenHandle(FocusItem.Index, FFocusedSubItemIndex);
  end;
end;

procedure TCustomExtChkListView.CMExit(var Message: TCMExit);
var
  CancelCapture: Boolean;
begin
  CancelCapture := (not FSpaceDown or (GetKeyState(VK_MENU) >= 0));
  EndCapture(CancelCapture);
  FFocusedSubItemIndex := -1;
  inherited;
end;

procedure TCustomExtChkListView.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
  CancelCapture: Boolean;
  FocusItem: TListItem;
begin
  if IsEditing then begin
    inherited;
    Exit;
  end;
  if Message.CharCode = VK_SPACE then with Message do begin
    ShiftState := KeyDataToShiftState(KeyData);
    KeyDown(CharCode, ShiftState);
    Exit;
  end
  else if Message.CharCode = VK_TAB then begin
    FocusItem := ItemFocused;
    if Assigned(FocusItem) then begin
      if GetKeyState(VK_SHIFT) < 0 then
        FFocusedSubItemIndex := GetPrevFocusableSubItem
      else
        FFocusedSubItemIndex := GetNextFocusableSubItem;
      if FFocusedSubItemIndex >= 0 then
        UpdateSubItemWhenHandle(FocusItem.Index, FFocusedSubItemIndex);
    end;
    Exit;
  end
  else if not Message.CharCode in [VK_DOWN, VK_RIGHT, VK_UP, VK_LEFT] then begin
    if FSpaceDown then
      EndCapture(True);
    inherited;
    Exit;
  end
  else
    inherited;
  CancelCapture := not FSpaceDown;
  EndCapture(CancelCapture);
end;

procedure TCustomExtChkListView.LVItemPostPaint(const NMLVCD: TNMLVCustomDraw;
    var DrawIcon: Boolean);
begin
  // dummy, to be overridden
end;

function TCustomExtChkListView.ItemChecking(Item: TListItem): Boolean;
begin
  Result := True;
  if Assigned(FOnItemChecking) then
    Result := FOnItemChecking(Self, Item.Index);
end;

function TCustomExtChkListView.SubItemChecking(Item: TListItem;
    SubItem: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnSubItemChecking) then
    Result := FOnSubItemChecking(Self, Item.Index, SubItem);
end;

procedure TCustomExtChkListView.ItemCheckedManually;
begin
  // dummy, to be overridden
end;

procedure TCustomExtChkListView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if DoMouseDown(mbLeft, Message.XPos, Message.YPos) then
    inherited;
end;

procedure TCustomExtChkListView.WMRButtonDown(var Message: TWMRButtonDown);
begin
  if DoMouseDown(mbRight, Message.XPos, Message.YPos) then
    inherited;
end;

procedure TCustomExtChkListView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if DoMouseDown(mbLeft, Message.XPos, Message.YPos) then
    inherited;
end;

procedure TCustomExtChkListView.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
  if DoMouseDown(mbRight, Message.XPos, Message.YPos) then
    inherited;
end;

function TCustomExtChkListView.GetExtListColumnClass: TCollectionItemClass;
begin
  Result := TExtChkListColumn;
end;

function TCustomExtChkListView.CreateListItem: TListItem;
begin
  Result := TChkListItem.Create(Items);
end;

procedure TCustomExtChkListView.ItemChecked(ItemIndex: Integer;
    Checked: Boolean);
begin
  if FProcessItemChecked then begin
    FProcessItemChecked := False;
    try
      if Items[ItemIndex] is TChkListItem then
        TChkListItem(Items[ItemIndex]).FGrayed := False;
    finally
      FProcessItemChecked := True;
    end;
  end;
  inherited;
end;

procedure TCustomExtChkListView.WMThemeChanged(var Message: TMessage);
begin
  // don't Run to Cursor into this function, it will interrupt the theme change
  UpdateThemeData(True, True);
  inherited;
end;

procedure TCustomExtChkListView.CMWantSpecialKey(var Message: TCMWantSpecialKey);
var
  SubItem: Integer;
begin
  inherited;
  // ignore when Ctrl is pressed, eg. Ctrl+Tab is used for tab controls
  if GetKeyState(VK_CONTROL) < 0 then
    Exit;
  if Message.CharCode = VK_TAB then begin
    // Return a nonzero value if this key should be handled by us. This will
    // cause the handler in the VCL to indicate that the app hasn't processed
    // the message yet.
    if GetKeyState(VK_SHIFT) < 0 then
      SubItem := GetPrevFocusableSubItem
    else
      SubItem := GetNextFocusableSubItem;
    if SubItem >= 0 then
      Message.Result := 1;
  end;
end;

procedure Register;
begin
  RegisterComponents('BM', [TExtChkListView]);
end;

initialization
  // TODO: skip when IsLibrary
  InitThemeLibrary;

end.
