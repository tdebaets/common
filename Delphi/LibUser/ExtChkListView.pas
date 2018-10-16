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
  TLVBeforeSelectItemEvent = procedure(Sender: TObject; Item: TListItem;
      Selected: Boolean; var AllowSelect: Boolean) of object;

  TCustomExtChkListView = class;

  THackCustomListView = class(TCustomListView);

  TChkListItem = class(TListItem)
  private
    FEnabled: Boolean;
    FGrayed: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetGrayed(Value: Boolean);
  public
    constructor Create(Owner: TListItems);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
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
    FOnBeforeSelectItem: TLVBeforeSelectItemEvent;
    FCheckWidth, FCheckHeight: Integer;
    FCheckBoxOptions: TCheckBoxOptions;
    FDisabledColor: TColor;
    FHideItemFocusRect: Boolean;
    FSpaceDown: Boolean;
    FCaptureIndex: Integer;
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
    procedure DrawCheck(hDC: Integer; const CheckRect: TRect;
        State: TCheckBoxState; CheckState: TCheckState);
    procedure DrawDisabledIcon(hDC: Integer; Index: Integer;
        const IconRect: TRect);
    procedure UpdateItemCheck(Index: Integer);
    procedure EndCapture(Cancel: Boolean);
    function CanFocusItem(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetChecksEnabled: Boolean;
    procedure SetHideItemFocusRect(Value: Boolean);
    procedure UpdateBeforePrimaryColumnIdx;
    function NMCustomDraw(NMCustomDraw: PNMCustomDraw): Integer;
    function HDNEndDrag: Integer;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
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
  protected
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function DoMouseDown(Button: TMouseButton; X, Y: Integer): Boolean;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetCheckBoxOptions(Value: TCheckBoxOptions);
    function CreateListItem: TListItem; override;
    procedure ItemChecked(ItemIndex: integer; Checked: Boolean); override;
    procedure LVItemPostPaint(const NMLVCD: TNMLVCustomDraw;
        var DrawIcon: Boolean); virtual;
    function ItemChecking(Item: TListItem): Boolean; virtual;
    procedure ItemCheckedManually; virtual;
    property OnItemPrePaint: TLVItemPrePaintEvent
        read FOnItemPrePaint write FOnItemPrePaint;
    property OnItemChecking: TLVCheckingEvent
        read FOnItemChecking write FOnItemChecking;
    property OnBeforeSelectItem: TLVBeforeSelectItemEvent
        read FOnBeforeSelectItem write FOnBeforeSelectItem;
    property CheckBoxOptions: TCheckBoxOptions
        read FCheckBoxOptions write SetCheckBoxOptions;
    property ItemEnabled[Index: Integer]: Boolean
        read GetItemEnabled write SetItemEnabled;
    property DisabledColor: TColor
        read FDisabledColor write FDisabledColor default clGrayText;
    property HideItemFocusRect: Boolean
        read FHideItemFocusRect write SetHideItemFocusRect default False;
  public
    function OnStateIcon(X, Y: Integer): Boolean;
  end;

  TExtChkListView = class(TCustomExtChkListView)
  public
    // new
    property ItemEnabled;

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

{ TChkListItem }

const
  DefaultStates: array[Boolean] of TCheckBoxState =
      (cbUnchecked, cbChecked);

constructor TChkListItem.Create(Owner: TListItems);
begin
  inherited Create(Owner);
  FEnabled := True;
  FGrayed := False;
end;

procedure TChkListItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  ListView.UpdateItems(Self.Index, Self.Index);
end;

procedure TChkListItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TChkListItem then with TChkListItem(Source) do begin
    Self.Enabled := Enabled;
    Self.Grayed := Grayed;
  end;
end;

destructor TChkListItem.Destroy;
begin
  // TODO: remove?
  inherited;
end;

procedure TChkListItem.SetGrayed(Value: Boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    TCustomExtChkListView(ListView).FProcessItemChecked := False;
    try
      Checked := False;
    finally
      TCustomExtChkListView(ListView).FProcessItemChecked := True;
    end;
    ListView.UpdateItems(Self.Index, Self.Index);
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
  Value: Boolean;
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
    for i := 0 to Count - 1 do begin
      FMemStream.Read(Value, SizeOf(Value));
      TChkListItem(Items[i]).Enabled := Value;
    end;
    FreeAndNil(FMemStream);
  end;
end;

procedure TCustomExtChkListView.DestroyWnd;
var
  i: Integer;
  Count: Integer;
  Value: Boolean;
begin
  if Assigned(FMemStream) then
    FMemStream.Size := 0
  else
    FMemStream := TMemoryStream.Create;
  Count := Items.Count;
  FMemStream.Write(Count, SizeOf(Count));
  for i := 0 to Items.Count - 1 do begin
    Value := TChkListItem(Items[i]).Enabled;
    FMemStream.Write(Value, SizeOf(Value));
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

procedure TCustomExtChkListView.SetHideItemFocusRect(Value: Boolean);
begin
  if Value <> FHideItemFocusRect then begin
    FHideItemFocusRect := Value;
    if Assigned(ItemFocused) then
      UpdateItems(ItemFocused.Index, ItemFocused.Index);
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
      else if Integer(dwItemSpec) = FCaptureIndex then
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
var
  Item: TChkListItem;
  NewFont: Boolean;
begin
  Result := CDRF_DODEFAULT;
  with NMCustomDraw^ do begin
    case dwDrawStage of
      CDDS_PREPAINT: begin
        Result := Result or CDRF_NOTIFYITEMDRAW;
        Result := Result or CDRF_NOTIFYPOSTPAINT;
      end;
      CDDS_ITEMPREPAINT: begin
        // TODO: also move to separate procedure
        NewFont := False;
        if FHideItemFocusRect then
          uItemState := uItemState and not CDIS_FOCUS;
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
      end;
      CDDS_ITEMPOSTPAINT:
        HandleItemPostPaint;
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
var
  AllowSelect: Boolean;
begin
  inherited;
  with Message do begin
    case NMHdr^.code of
      NM_CUSTOMDRAW:
        Result := NMCustomDraw(PNMCustomDraw(NMHdr));
      LVN_ITEMCHANGING: begin
        with PNMListView(NMHdr)^ do begin
          if Assigned(FOnBeforeSelectItem) and (uChanged = LVIF_STATE) then begin
            AllowSelect := True;
            if (uOldState and LVIS_SELECTED <> 0) and
              (uNewState and LVIS_SELECTED = 0) then
              FOnBeforeSelectItem(Self, Items[iItem], False, AllowSelect)
            else if (uOldState and LVIS_SELECTED = 0) and
              (uNewState and LVIS_SELECTED <> 0) then
              FOnBeforeSelectItem(Self, Items[iItem], True, AllowSelect);
            if not AllowSelect then
              Result := 1;
          end;
        end;
      end;
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
  if (Index < 0) or (Index > Items.Count - 1) then
    Exit;
  Item := Items[Index];
  if Item is TChkListItem then
    TChkListItem(Item).Enabled := Value;
end;

// workaround to redraw only the checkmark of an item, and not the whole item
procedure TCustomExtChkListView.UpdateItemCheck(Index: Integer);
var
  Checked: Boolean;
begin
  Checked := ListView_GetCheckState(Handle, Index) <> 0;
  ListView_SetCheckState(Handle, Index, not Checked);
  ListView_SetCheckState(Handle, Index, Checked);
end;

procedure TCustomExtChkListView.EndCapture(Cancel: Boolean);
var
  ItemIdx: Integer;
  Checked: Boolean;
begin
  ItemIdx := FCaptureIndex;
  if ItemIdx >= 0 then begin
    FSpaceDown := False;
    FCaptureIndex := -1;
    if not Cancel and ItemEnabled[ItemIdx] then begin
      Checked := (ListView_GetCheckState(Handle, ItemIdx) <> 0);
      // Changing Items[ItemIdx].Checked causes lots of flicker on XP!!! - see
      // LVMSetExtendedListViewStyle
      ListView_SetCheckState(Handle, ItemIdx, not Checked);
      ItemCheckedManually;
    end
    else
      UpdateItemCheck(ItemIdx);
  end;
  if MouseCapture then
    MouseCapture := False;
end;

function TCustomExtChkListView.CanFocusItem(Index: Integer): Boolean;
begin
  Result := Self.Enabled and ItemEnabled[Index];
end;

function TCustomExtChkListView.OnStateIcon(X, Y: Integer): Boolean;
var
  HTI: TLVHitTestInfo;
begin
  Result := True;
  // TODO: move this check
  if CheckBoxOptions.CheckOnItemClick then
    Exit;
  HTI.pt.x := X;
  HTI.pt.y := Y;
  ListView_HitTest(Handle, HTI);
  Result := (HTI.flags = LVHT_ONITEMSTATEICON);
end;

procedure TCustomExtChkListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if GetChecksEnabled and (Key = VK_SPACE)
      and not IsEditing and not (ssAlt in Shift)
      and Assigned(Selected) and (FCaptureIndex < 0)
      and CanFocusItem(Selected.Index) then begin
    if not FSpaceDown then begin
      if not ItemChecking(Selected) then
        Exit;  
      FCaptureIndex := Selected.Index;
      FSpaceDown := True;
      KeyUp(Key, Shift);
    end;
  end;
  inherited;
end;

procedure TCustomExtChkListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if GetChecksEnabled and (Key = VK_SPACE) and not IsEditing
      and FSpaceDown and (FCaptureIndex >= 0) then
    EndCapture(False);
  inherited;
end;

function TCustomExtChkListView.DoMouseDown(Button: TMouseButton;
    X, Y: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  if not GetChecksEnabled then
    Exit;
  Item := GetItemAt(X, Y);
  if (Button in [mbLeft, mbRight]) and Assigned(Item)
      and OnStateIcon(X, Y) then begin
    if not FSpaceDown then begin
      Result := False;
      if not CanFocusItem(Item.Index) then
        Exit;
      if not ItemChecking(Item) then
        Exit;
      if not MouseCapture then
        MouseCapture := True;
      FCaptureIndex := Item.Index;
      UpdateItemCheck(Item.Index);
      MouseUp(Button, [], X, Y);
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
begin
  SwapButtons := LongBool(GetSystemMetrics(SM_SWAPBUTTON));
  if GetKeyState(VirtKeys[SwapButtons, Button]) < 0 then
    Exit;
  if GetChecksEnabled and (Button in [mbLeft, mbRight]) and not FSpaceDown
      and (FCaptureIndex >= 0) then begin
    Item := GetItemAt(X, Y);
    CancelCapture := (not Assigned(Item) or (Item.Index <> FCaptureIndex)
        or not OnStateIcon(X, Y));
    EndCapture(CancelCapture);
  end;
  inherited;
end;

procedure TCustomExtChkListView.CMExit(var Message: TCMExit);
var
  CancelCapture: Boolean;
begin
  CancelCapture := (not FSpaceDown or (GetKeyState(VK_MENU) >= 0));
  EndCapture(CancelCapture);
  inherited;
end;

procedure TCustomExtChkListView.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
  CancelCapture: Boolean;
begin
  if not GetChecksEnabled or IsEditing then begin
    inherited;
    Exit;
  end;
  if Message.CharCode = VK_SPACE then with Message do begin
    ShiftState := KeyDataToShiftState(KeyData);
    KeyDown(CharCode, ShiftState);
    Exit;
  end;
  if not Message.CharCode in [VK_TAB, VK_DOWN, VK_RIGHT, VK_UP, VK_LEFT] then begin
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

procedure Register;
begin
  RegisterComponents('BM', [TExtChkListView]);
end;

initialization
  // TODO: skip when IsLibrary
  InitThemeLibrary;

end.
