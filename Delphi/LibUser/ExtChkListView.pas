(****************************************************************************
 *
 * Copyright 2016 Tim De Baets
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

{$I DFS.INC}

unit ExtChkListView;

interface

uses ExtListView, EnhListView, Windows, Graphics, StdCtrls, Classes, Controls,
    ComCtrls, NewCommCtrl, Messages, SysUtils, ComStrs, Forms, Math, UxThemeISX,
    Common2;

const
  LVIS_DISABLED = $40;

const
  AC_SRC_OVER = $00;

type
  TBLENDFUNCTION = record
   BlendOp: BYTE;
   BlendFlags: BYTE;
   SourceConstantAlpha: BYTE;
   AlphaFormat: BYTE;
  end;
  TAlphaBlend = function(
      hdcDest: HDC;             // handle to destination DC
      nXOriginDest,            // x-coord of upper-left corner
      nYOriginDest,            // y-coord of upper-left corner
      nWidthDest,              // destination width
      nHeightDest: Integer;     // destination height
      hdcSrc:HDC;              // handle to source DC
      nXOriginSrc,             // x-coord of upper-left corner
      nYOriginSrc,             // y-coord of upper-left corner
      nWidthSrc,               // source width
      nHeightSrc: Integer;      // source height
      blendFunction: TBLENDFUNCTION  // alpha-blending function
    ): Boolean; stdcall;


type
  PItemParamInfo = ^TItemParamInfo;
  TItemParamInfo = packed record
    Index: Integer;
    ItemState: Integer;
    //ImageIdx: Integer;
    lItemLParam: Integer;
    Enabled: Boolean;
  end;

type
  TCheckBoxState2 = (cb2Normal, cb2Hot, cb2Pressed, cb2Disabled);

  TLVItemPrePaintEvent = function(Control: TWinControl; var NMLVCD: TNMLVCustomDraw;
      var FontChanged: Boolean): Boolean of object;
  TLVCheckingEvent = function(Sender: TObject; ItemIndex: Integer): Boolean of object;
  //TLVIsItemEnabledEvent = function(Control: TWinControl; Item: TListItem): Boolean of object;
  TLVBeforeSelectItemEvent = procedure(Sender: TObject; Item: TListItem;
      Selected: Boolean; var AllowSelect: Boolean) of object;

  TCustomExtChkListView = class;

  TChkListItem = class(TListItem)
  private
    FEnabled: Boolean;
    FGrayed: Boolean;
    //FItemInfo: TItemParamInfo;
    procedure SetEnabled(Value: Boolean);
    function GetGrayed: Boolean;
    procedure SetGrayed(Value: Boolean);
    {function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);}
  public
    constructor Create(AOwner: TListItems);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    //property Checked read GetChecked write SetChecked;
    property Grayed: Boolean read GetGrayed write SetGrayed;
  end;

  TCheckBoxOptions = class(TPersistent)
  private
    FListView: TCustomListView;
    FFlat: Boolean;
    FLeftMargin{, FBottomMargin}: Integer;
    FThemed: Boolean;
    FHighLight: Boolean;
    FGrayedImages: Boolean;
    FCheckOnItemClick: Boolean;
    procedure SetFlat(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    //procedure SetBottomMargin(Value: Integer);
    procedure SetThemed(Value: Boolean);
    procedure SetHighLight(Value: Boolean);
    procedure SetGrayedImages(Value: Boolean);
  public
    constructor Create(AOwner: TCustomExtChkListView);
  published
    property Flat: Boolean read FFlat write SetFlat default True;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 6;
    //property BottomMargin: Integer read FBottomMargin write SetBottomMargin default 2;
    property Themed: Boolean read FThemed write SetThemed default True;
    property HighLight: Boolean read FHighLight write SetHighLight default True;
    property GrayedImages: Boolean read FGrayedImages write SetGrayedImages default True;
    property CheckOnItemClick: Boolean read FCheckOnItemClick Write FCheckOnItemClick default False;
  end;

  TCustomExtChkListView = class(TCustomExtListView)
  private
    FOnItemPrePaint: TLVItemPrePaintEvent;
    FOnItemChecking: TLVCheckingEvent;
    FOnBeforeSelectItem: TLVBeforeSelectItemEvent;
    //FIsItemEnabled: TLVIsItemEnabledEvent;
    FCheckWidth, FCheckHeight: Integer;
    FCheckBoxOptions: TCheckBoxOptions;
    FSpaceDown: Boolean;
    FLastMouseMoveIndex: Integer;
    FCaptureIndex: Integer;
    FDisabledColor: TColor;
    FThemeData: HTHEME;
    FAlphaBlend: TAlphaBlend;
    FMSImgLib: Integer;
    FIsVistaOrHigher: Boolean;
    FMemStream: TMemoryStream;
    FBeforePrimaryColumnIdx: Integer;
    FProcessItemChecked: Boolean;
    function NMCustomDraw(NMCustomDraw: PNMCustomDraw): Integer;
    function HDNEndDrag: Integer;
    //function LVNItemChanging(NMListView: PNMListView): Boolean;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    function GetItemState(Item: TListItem): TCheckBoxState;
    procedure DrawCheck(hDC: Integer; Index: Integer; State: TCheckBoxState;
        IEnabled: Boolean; DrawIcon: Boolean);
    procedure UpdateItemCheck(ItemIdx: Integer);
    procedure EndCapture(Cancel: Boolean);
    function CanFocusItem(Item: Integer): Boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    function GetItemEnabled(Index: integer): Boolean;
    procedure SetItemEnabled(Index: integer; Value: Boolean);
    function Checks: Boolean;
    procedure UpdateBeforePrimaryColumnIdx;
    procedure LVMSetColumnOrderArray(var Message: TMessage);
        message LVM_SETCOLUMNORDERARRAY;
    procedure LVMSetExtendedListViewStyle(var Message: TMessage);
        message LVM_SETEXTENDEDLISTVIEWSTYLE;
    procedure UpdateThemeData(const Close, Open: Boolean);
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  public
    function OnStateIcon(X, Y: Integer): Boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function DoMouseDown(Button: TMouseButton; X, Y: Integer): Boolean;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    //property IsItemEnabled: TLVIsItemEnabledEvent
    //    read FIsItemEnabled write FIsItemEnabled;
    property CheckBoxOptions: TCheckBoxOptions
        read FCheckBoxOptions write SetCheckBoxOptions;
    property ItemEnabled[Index: Integer]: Boolean
        read GetItemEnabled write SetItemEnabled;
    property DisabledColor: TColor
        read FDisabledColor write FDisabledColor default clGrayText;
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
    //property IsItemEnabled;
    property DisabledColor;

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

    // New Events
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

    // Publish inherited protected properties
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

{type
  TCheckState = (csUnchecked, csUncheckedHot, csUncheckedPressed, csUncheckedDisabled,
      csChecked, csCheckedHot, csCheckedPressed, csCheckedDisabled);

const
  DFCS_BUTTONCHECKED = DFCS_BUTTONCHECK or DFCS_CHECKED;
  CheckStyles: array [TCheckState] of Integer =
  (
    DFCS_BUTTONCHECK, 0, DFCS_BUTTONCHECK or DFCS_PUSHED, DFCS_BUTTONCHECK or DFCS_INACTIVE
    ,DFCS_BUTTONCHECKED, 0, DFCS_BUTTONCHECKED or DFCS_PUSHED, DFCS_BUTTONCHECKED or DFCS_INACTIVE
  );
  CheckStylesThemed: array [TCheckState] of Integer =
  (
    CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED
    ,CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED
    //,(CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
}

procedure AlphaHighlight(hDC: Integer; R: TRect; AlphaBlend: TAlphaBlend);
var
  Bmp: TBitmap;
  Alpha: TBlendFunction;
  cx, cy: Integer;
  Rect: TRect;
begin
  if @AlphaBlend = nil then
    Exit;
  cx := R.Right - R.Left;
  cy := R.Bottom - R.Top;
  with Rect do begin
    Left := 0;
    Top := 0;
    Right := cx;
    Bottom := cy;
  end;
  Bmp := TBitmap.Create;
  try
    with Bmp do begin
      Height := cy;
      Width := cx;
      Canvas.Brush.Style := bsSolid;
      Bmp.Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(Rect);
    end;
    with Alpha do begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 127;
      AlphaFormat := 0;
    end;
    AlphaBlend(hDC, R.Left, R.Top, cx, cy, Bmp.Canvas.Handle, 0, 0, cx, cy, Alpha);
  finally
    Bmp.Free;
  end;
end;

function IsCheckedState(State: Integer): Boolean;
begin
  if State <> 0 then
    Result := ((State and LVIS_STATEIMAGEMASK) shr 12) - 1 <> 0
  else
    Result := False;
end;

{ TCheckBoxOptions }

constructor TCheckBoxOptions.Create(AOwner: TCustomExtChkListView);
begin
  inherited Create;
  if AOwner = nil then
    raise Exception.Create(sInvalidOwner);
  FListView := AOwner;
  FFlat := True;
  FLeftMargin := 6;
  //FBottomMargin := 2;
  FThemed := True;
  FHighLight := True;
  FGrayedImages := True;
  FCheckOnItemClick := False;
end;

procedure TCheckBoxOptions.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then begin
    FFlat := Value;
    TCustomExtChkListView(FListView).RecreateWnd;
  end;
end;

procedure TCheckBoxOptions.SetLeftMargin(Value: Integer);
begin
  if Value <> FLeftMargin then begin
    FLeftMargin := Value;
    FListView.Invalidate;
  end;
end;

{procedure TCheckBoxOptions.SetBottomMargin(Value: Integer);
begin
  if Value <> FBottomMargin then begin
    FBottomMargin := Value;
    FListView.Invalidate;
  end;
end;}

procedure TCheckBoxOptions.SetThemed(Value: Boolean);
begin
  if Value <> FThemed then begin
    FThemed := Value;
    TCustomExtChkListView(FListView).RecreateWnd;
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

{TChkListItem}

const
  DefaultStates: array[Boolean] of TCheckBoxState =
      (cbUnchecked, cbChecked);

constructor TChkListItem.Create(AOwner: TListItems);
begin
  inherited Create(AOwner);
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
  if Source is TChkListItem then begin
    Self.Enabled := (Source as TChkListItem).Enabled;
    Self.Grayed := (Source as TChkListItem).Grayed;
  end;
end;

destructor TChkListItem.Destroy;
begin
  inherited;
end;

function TChkListItem.GetGrayed: Boolean;
begin
  {if TCustomExtChkListView(ListView).IsChecked[Index] then
    FGrayed := False;}
  Result := FGrayed;
end;

procedure TChkListItem.SetGrayed(Value: Boolean);
begin
  //Checked := FState = cbChecked;
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

{function TChkListItem.GetChecked: Boolean;
begin
  Result := inherited Checked;
end;

procedure TChkListItem.SetChecked(Value: Boolean);
begin
  inherited Checked := Value;
end;}

{TCustomExtChkListView}

constructor TCustomExtChkListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  FBeforePrimaryColumnIdx := 0; // assume primary column is first
  FProcessItemChecked := True;
  @FAlphaBlend := nil;
  FMSImgLib := LoadLibrary( 'msimg32.dll');
  if FMSImgLib <> 0 then
    @FAlphaBlend := GetProcAddress(FMSImgLib, 'AlphaBlend');
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
  if FMemStream <> nil then begin
    FMemStream.Read(Count, SizeOf(Count));
    // Items are sorted in TCustomEnhListView.Loaded, so make sure that this
    // method already has been called, otherwise the item enabled states would
    // be restored in the wrong order!
    if (Count > 0) and (csLoading in ComponentState) then begin
      Items.Clear;
      raise EComponentError.CreateFmt('%s: cannot restore item states while ' +
          'component is still loading', [Name]);
    end;
    for i := 0 to Count - 1 do begin
      FMemStream.Read(Value, SizeOf(Value));
      TChkListItem(Items[i]).Enabled := Value;
    end;
    FMemStream.Free;
    FMemStream := nil;
  end;
end;

procedure TCustomExtChkListView.DestroyWnd;
var
  i: Integer;
  Count: Integer;
  Value: Boolean;
begin
  if FMemStream = nil then
    FMemStream := TMemoryStream.Create
  else
    FMemStream.Size := 0;
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
  if FMSImgLib <> 0 then
    FreeLibrary(FMSImgLib);
  FMemStream.Free;
  FCheckBoxOptions.Free;
  UpdateThemeData(True, False);
  inherited Destroy;
end;

function TCustomExtChkListView.Checks: Boolean;
begin
  Result := not (ViewStyle = vsIcon) and (lvxCheckBoxes in ExtendedStyles);
end;

procedure TCustomExtChkListView.UpdateThemeData(const Close, Open: Boolean);
begin
  if not FCheckBoxOptions.Themed then begin
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
  ColCount: integer;
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

procedure TCustomExtChkListView.DrawCheck(hDC: Integer; Index: Integer;
    State: TCheckBoxState; IEnabled: Boolean; DrawIcon: Boolean);
var
  uState: Integer;
  StateId: Integer;
  IconRect, CheckRect, BoundsRect, StateRect: TRect;
  Selected: Boolean;
  MarginTop: Integer;
const
  CheckStateIds: array [TCheckBoxState, TCheckBoxState2] of Integer =
  (
    (CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED),
    (CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED),
    (CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
  {CheckStateIds: array [Boolean, TCheckBoxState2] of TCheckState =
  (
    (csUnchecked, csUncheckedHot, csUncheckedPressed, csUncheckedDisabled)
    ,(csChecked, csCheckedHot, csCheckedPressed, csCheckedDisabled)
  );}
begin
  if not ListView_GetItemRect(Handle, Index, IconRect, LVIR_ICON) then
    Exit;
  // prevent drawing on the column header
  if (IconRect.Top < 5) and (ViewStyle = vsReport) and ShowColumnHeaders then
    Exit;
  CheckRect := IconRect;
  // Erase existing state icon (checkmark)
  StateRect.Top := IconRect.Top;
  StateRect.Bottom := IconRect.Bottom;
  StateRect.Left := 0;
  if (ViewStyle <> vsReport) or (FBeforePrimaryColumnIdx = 0) then begin
    // primary column is the first, so left bound of state icon = left bound whole item
    ListView_GetItemRect(Handle, Index, BoundsRect, LVIR_BOUNDS);
    StateRect.Left := BoundsRect.Left;
  end
  else begin
    // primary column is not first, so left bound of state icon = right bound of previous column
    ListView_GetSubItemRect(Handle, Index, FBeforePrimaryColumnIdx, LVIR_BOUNDS,
        @BoundsRect);
    StateRect.Left := BoundsRect.Right;
  end;
  StateRect.Right := IconRect.Left;
  FillRect(hDC, StateRect, Brush.Handle);
  //FillRect(hDC, StateRect, DKGRAY_BRUSH);
  CheckRect.Left := CheckRect.Left - FCheckWidth - 10;
  //R.Right := R.Right - (R.Right - R.Left);
  CheckRect.Left := CheckRect.Left + CheckBoxOptions.LeftMargin;
  CheckRect.Right := CheckRect.Left + FCheckWidth;
  MarginTop := (CheckRect.Bottom - CheckRect.Top - FCheckHeight) div 2;
  MarginTop := Max(MarginTop, 0);
  Inc(CheckRect.Top, MarginTop);
  CheckRect.Bottom := CheckRect.Top + FCheckHeight;
  //R.Bottom := R.Bottom - CheckBoxOptions.BottomMargin;
  //R.Top := R.Bottom - FCheckHeight;
  {if not ItemEnabled[Index] then
    CheckState := CheckStateIds[IsChecked[Index]][cb2Disabled]
  else if Index = FCaptureIndex then begin
    if FSpaceDown or (FLastMouseMoveIndex = Index) then
      CheckState := CheckStateIds[IsChecked[Index]][cb2Pressed]
    else
      CheckState := CheckStateIds[IsChecked[Index]][cb2Hot]
    {else if (FCaptureIndex < 0) and (Index = FHotIndex) then
      StateId := ButtonStateIds[IsChecked[Index]][cb2Hot]
    else
      CheckState := CheckStateIds[IsChecked[Index]][cb2Normal];
  end;
  ImageList_Draw(FCheckImages.Handle, Integer(CheckState), hDC, R.Left, R.Top, ILD_NORMAL);}
  if FThemeData = 0 then begin
    case State of
      cbChecked: uState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked: uState := DFCS_BUTTONCHECK;
      else
        uState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    {if IsChecked[Index] then
      uState := DFCS_BUTTONCHECK or DFCS_CHECKED
    else
      uState := DFCS_BUTTONCHECK;}
    if CheckBoxOptions.Flat then
      uState := uState or DFCS_FLAT;
    if not IEnabled then
      uState := uState or DFCS_INACTIVE;
    if (FCaptureIndex = Index) and (FSpaceDown or (FLastMouseMoveIndex = Index)) then
      uState := uState or DFCS_PUSHED;
    DrawFrameControl(hDC, CheckRect, DFC_BUTTON, uState);
  end
  else begin
    if not IEnabled then
      StateId := CheckStateIds[State][cb2Disabled]
    else if Index = FCaptureIndex then
      if FSpaceDown or (FLastMouseMoveIndex = Index) then
        StateId := CheckStateIds[State][cb2Pressed]
      else
        StateId := CheckStateIds[State][cb2Hot]
  //  else if (FCaptureIndex < 0) and (Index = FHotIndex) then
  //    StateId := CheckStateIds[IsChecked[Index]][cb2Hot]
    else
      StateId := CheckStateIds[State][cb2Normal];
    //if IsThemeBackgroundPartiallyTransparent(FThemeData, PartId, StateId) then
      //DrawThemeParentBackground(Handle, hdc, @R);
    DrawThemeBackGround(FThemeData, hDC, BP_CHECKBOX, StateId, CheckRect,
        @CheckRect);
  end;
  Selected := (ListView_GetItemState(Handle, Index, LVIS_SELECTED) = LVIS_SELECTED);
  if Selected and Focused and
      ((FThemeData <> 0) or CheckBoxOptions.HighLight) then
    AlphaHighLight(hdc, CheckRect, FAlphaBlend)
  else if (SmallImages <> nil) and (SmallImages.Handle <> 0) then begin
    if DrawIcon and CheckBoxOptions.GrayedImages and not IEnabled then begin
      FillRect(hDC, IconRect, ColorToRGB(Color));
      // In Large Fonts mode, the height of the item can be larger than the
      // small images height. In that case, Windows Vista and higher will draw
      // the icons centered.
      // So we need to center as well when drawing the (disabled) icon ourselves.
      if FIsVistaOrHigher then begin
        MarginTop := (IconRect.Bottom - IconRect.Top - SmallImages.Height) div 2;
        MarginTop := Max(MarginTop, 0);
        Inc(IconRect.Top, MarginTop);
      end;
      ImageList_DrawEx(SmallImages.Handle, Items[Index].ImageIndex, hDC,
          IconRect.Left, IconRect.Top, SmallImages.Width, SmallImages.Height,
          CLR_DEFAULT, ColorToRGB(clGrayText), ILD_BLEND50);
    end;
  end;
end;

function TCustomExtChkListView.NMCustomDraw(NMCustomDraw: PNMCustomDraw): Integer;
var
  Item: TChkListItem;
  NewFont: Boolean;
  //IEnabled: Boolean;
  DrawIcon: Boolean;

  procedure GetItem;
  begin
    if (NMCustomDraw.lItemLParam <> 0)
        and (TObject(NMCustomDraw.lItemLParam) is TChkListItem) then
      Item := TChkListItem(NMCustomDraw.lItemLParam)
    else
      Item := nil;
  end;

  function IsItemEnabled: Boolean;
  begin
    if Assigned(Item) then
      Result := TChkListItem(NMCustomDraw.lItemLParam).Enabled
    else
      //Result := ItemEnabled[dwItemSpec];
      Result := True;
  end;
  
begin
  with NMCustomDraw^ do begin
    Result := CDRF_DODEFAULT;
    if dwDrawStage = CDDS_PREPAINT then begin
      Result := Result or CDRF_NOTIFYITEMDRAW;
      Result := Result or CDRF_NOTIFYPOSTPAINT;
    end
    else if dwDrawStage = CDDS_ITEMPREPAINT then begin
      NewFont := False;
      GetItem;
      if not IsItemEnabled then begin
        PNMLVCustomDraw(NMCustomDraw).clrText := ColorToRGB(FDisabledColor);
        NewFont := True;
      end;
      if Assigned(FOnItemPrePaint) then begin
        if not FOnItemPrePaint(Self, PNMLVCustomDraw(NMCustomDraw)^, NewFont) then
          Result := Result or CDRF_SKIPDEFAULT;
      end;
      //IEnabled := ItemEnabled[dwItemSpec];
      // setting the item state causes unattractive 'delayed' icons when painted in LVItemPostPaint
      {if Assigned(Item) and Checks then begin
        Item.FItemInfo.Index := dwItemSpec;
        Item.FItemInfo.ItemState := SendMessage(Handle, LVM_GETITEMSTATE, dwItemSpec, LVIS_STATEIMAGEMASK);
        Item.FItemInfo.lItemLParam := lItemLParam;
        Item.FItemInfo.Enabled := IEnabled;
        ListView_SetItemState(Handle, dwItemSpec, 0, LVIS_STATEIMAGEMASK);
      end;}
      Result := Result or CDRF_NOTIFYPOSTPAINT;
      if NewFont then
        Result := Result or CDRF_NEWFONT;
    end
    else if dwDrawStage = CDDS_ITEMPOSTPAINT then begin
      if Checks then begin
        GetItem;
        if Assigned(Item) {and (Item.FItemInfo.Index = dwItemSpec)} then begin
          // setting the item state here causes unattractive 'delayed' icons
          // when painted in LVItemPostPaint
          //ListView_SetItemState(Handle, dwItemSpec, Item.FItemInfo.ItemState, LVIS_STATEIMAGEMASK);
          DrawIcon := True;
          LVItemPostPaint(PNMLVCustomDraw(NMCustomDraw)^, DrawIcon);
          DrawCheck(hDC, dwItemSpec, GetItemState(Item), IsItemEnabled, DrawIcon);
        end;
      end
      else
        LVItemPostPaint(PNMLVCustomDraw(NMCustomDraw)^, DrawIcon);
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

{function TCustomExtChkListView.LVNItemChanging(NMListView: PNMListView): Boolean;
begin
  Result := False; // allow change by default
  if Assigned(FOnItemChecking)
      and ((NMListView.uChanged and LVIF_STATE) <> 0)
      and ((NMListView.uOldState and LVIS_STATEIMAGEMASK) <> 0)
      and ((NMListView.uNewState and LVIS_STATEIMAGEMASK) <> 0) then begin
    if IsCheckedState(NMListView.uOldState) <>
        IsCheckedState(NMListView.uNewState) then
      Result := FOnItemChecking(Self, NMListView.iItem);
  end;
  if Result then
    EndCapture(True);
end;}

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
        //Result := Integer(LVNItemChanging(PNMListView(NMHdr)));
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
    //BottomMargin := Value.BottomMargin;
    Themed := Value.Themed;
    HighLight := Value.HighLight;
    GrayedImages := Value.GrayedImages;
    CheckOnItemClick := Value.CheckOnItemClick;
  end;
end;

{function TCustomExtChkListView.GetItemEnabled(Index: Integer): Boolean;
begin
  //Result := ListView_GetItemState(Handle, Index, LVIS_DISABLED) and LVIS_DISABLED = 0;
  Result := True;
  if Assigned(FIsItemEnabled) then
    Result := FIsItemEnabled(Self, Items[Index]);
end;}

{procedure TCustomExtChkListView.SetItemEnabled(Index: Integer; Value: Boolean);
var
  Mask: Integer;
  Data: Integer;
begin
  Mask := LVIS_DISABLED;
  if Value then
    Data := 0
  else
    Data := Mask;
  ListView_SetItemState(Handle, Index, Data, LVIS_DISABLED);
  //if HandleAllocated then
    UpdateItems(Index, Index)
end;}

function TCustomExtChkListView.GetItemEnabled(Index: integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  Item := Items[Index];
  if Item is TChkListItem then
    Result := TChkListItem(Item).Enabled;
  {if Assigned(FIsItemEnabled) then
    Result := FIsItemEnabled(Self, Items[Index])
  else begin
    if (Index < 0) or (Index > Items.Count - 1) then
      Exit;
    Item := Pointer(Items[Index]);
    Result := (FDisabledItems.IndexOf(Item) = -1);
  end;}
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
procedure TCustomExtChkListView.UpdateItemCheck(ItemIdx: Integer);
var
  Checked: Boolean;
begin
  Checked := ListView_GetCheckState(Handle, ItemIdx) <> 0;
  ListView_SetCheckState(Handle, ItemIdx, not Checked);
  ListView_SetCheckState(Handle, ItemIdx, Checked);
end;

procedure TCustomExtChkListView.EndCapture(Cancel: Boolean);
var
  //InvalidateItem: Boolean;
  Item: Integer;
  Checked: Boolean;
begin
  Item := FCaptureIndex;
  if Item >= 0 then begin
    //InvalidateItem := FSpaceDown or (FCaptureIndex = FLastMouseMoveIndex)
    //    {or (FThemeData <> 0)};
    FSpaceDown := False;
    FCaptureIndex := -1;
    FLastMouseMoveIndex := -1;
    if not Cancel and ItemEnabled[Item] then begin
      Checked := (ListView_GetCheckState(Handle, Item) <> 0);
      ListView_SetCheckState(Handle, Item, not Checked);
      // using Items[Item].Checked causes lots of flicker on XP!!! - see
      // LVMSetExtendedListViewStyle
      //Items[Item].Checked := not Items[Item].Checked
      ItemCheckedManually;
    end
    else {if InvalidateItem then} begin
      //UpdateItems(Item, Item);
      UpdateItemCheck(Item);
    end;
  end;
  if MouseCapture then
    MouseCapture := False;
end;

function TCustomExtChkListView.CanFocusItem(Item: Integer): Boolean;
begin
  Result := Self.Enabled and ItemEnabled[Item];
end;

function TCustomExtChkListView.OnStateIcon(X, Y: Integer): Boolean;
var
  HTI: TLVHitTestInfo;
begin
  Result := True;
  if CheckBoxOptions.CheckOnItemClick then
    Exit;
  Result := False;
  HTI.pt.x := X;
  HTI.pt.y := Y;
  ListView_HitTest(Handle, HTI);
  if HTI.flags = LVHT_ONITEMSTATEICON then
    Result := True;
end;

procedure TCustomExtChkListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Checks and (Key = VK_SPACE) and not IsEditing and not (ssAlt in Shift)
      and (Selected <> nil) and (FCaptureIndex < 0) and
      CanFocusItem(Selected.Index) then begin
    //if FWantTabs then begin
      if not FSpaceDown then begin
        if not ItemChecking(Selected) then
          Exit;  
        FCaptureIndex := Selected.Index;
        FSpaceDown := True;
//        UpdateItems(Selected.Index, Selected.Index);
        KeyUp(Key, Shift);
        //if (FHotIndex <> ItemIndex) and (FHotIndex <> -1) and (FThemeData <> 0) then
        //  InvalidateCheck(FHotIndex);
      end;
    //end
  end;
  inherited;
end;

procedure TCustomExtChkListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Checks and (Key = VK_SPACE) and not IsEditing {and FWantTabs}
      and FSpaceDown and (FCaptureIndex >= 0) then begin
    EndCapture(False);
    //if (FHotIndex <> -1) and (FThemeData <> 0) then
    //  InvalidateCheck(FHotIndex);
  end;
  inherited;
end;

function TCustomExtChkListView.DoMouseDown(Button: TMouseButton;
    X, Y: Integer): Boolean;
var
  Item: TListItem;
begin
  Result := True;
  if not Checks then
    Exit;
  Item := GetItemAt(X, Y);
  if (Button in [mbLeft, mbRight]) and (Item <> nil)
      and OnStateIcon(X, Y) then begin
    //if FWantTabs then begin
    if not FSpaceDown then begin
      Result := False;
      if not CanFocusItem(Item.Index) then
        Exit;
      if not ItemChecking(Item) then
        Exit;
      if not MouseCapture then
        MouseCapture := True;
      FCaptureIndex := Item.Index;
      FLastMouseMoveIndex := Item.Index;
//      UpdateItems(Item.Index, Item.Index);
      UpdateItemCheck(Item.Index);
      MouseUp(Button, [], X, Y);
    end;
    //end
  end;
end;

procedure TCustomExtChkListView.WMMouseMove(var Message: TWMMouseMove);
var
  Item: TListItem;
  //NewHotIndex: Integer;
  //Rect: TRect;
begin
  inherited;
  if not Checks then
    Exit;
  Item := GetItemAt(Message.Pos.x, Message.Pos.y);

  if (FCaptureIndex >= 0) and (Item <> nil) then begin
    if not FSpaceDown and (Item.Index <> FLastMouseMoveIndex) then begin
      //if (FLastMouseMoveIndex = FCaptureIndex) or (Item.Index = FCaptureIndex) then begin
        //FCaptureIndex := -1;
        //UpdateItems(Item.Index, Item.Index);
      //end;
      FLastMouseMoveIndex := Item.Index;
    end
  end;

  {NewHotIndex := -1;
  if (Index <> -1) and CanFocusItem(Index) then begin
    Rect := ItemRect(Index);
    Indent := (FOffset * 2 + FCheckWidth);
    if FWantTabs or ((Pos.X >= Rect.Left + Indent * ItemLevel[Index]) and
        (Pos.X < Rect.Left + Indent * (ItemLevel[Index] + 1))) then
      NewHotIndex := Index;
  end;
  UpdateHotIndex(NewHotIndex);}
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
  Item: TListItem;
begin
  if GetKeyState(VirtKeys[LongBool(GetSystemMetrics(SM_SWAPBUTTON)),
      Button]) < 0 then
    Exit;
  if Checks and (Button in [mbLeft, mbRight]) and not FSpaceDown
      and (FCaptureIndex >= 0) then begin
    Item := GetItemAt(X, Y);
    if (Item <> nil) and OnStateIcon(X, Y) then begin
      EndCapture(Item.Index <> FCaptureIndex);
      //UpdateItems(FCaptureIndex, FCaptureIndex);
      {if (FHotIndex <> -1) and (FThemeData <> 0) then
        InvalidateCheck(FHotIndex);}
    end else
      EndCapture(True);
    {else begin
      Index := FCaptureIndex;
      FCaptureIndex := -1;
      UpdateItems(Index, Index);
    end;}
  end;
  inherited;
end;

procedure TCustomExtChkListView.CMExit(var Message: TCMExit);
begin
  EndCapture(not FSpaceDown or (GetKeyState(VK_MENU) >= 0));
  inherited;
end;

procedure TCustomExtChkListView.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin
  if not Checks or IsEditing then begin
    inherited;
    Exit;
  end;
  if Message.CharCode = VK_SPACE then with Message do begin
    //inherited;
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
  EndCapture(not FSpaceDown);
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
    Checked: boolean);
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
  { Don't Run to Cursor into this function, it will interrupt up the theme change }
  UpdateThemeData(True, True);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('BM', [TExtChkListView]);
end;

initialization
  InitThemeLibrary;

end.
