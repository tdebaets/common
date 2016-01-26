unit NewCheckListBox;

{ TNewCheckListBox by Martijn Laan for My Inno Setup Extensions
  See www.wintax.nl/isx for more information
  Based on TPBCheckListBox by Patrick Brisacier and TCheckListBox by Borland

  Note: TNewCheckListBox uses Items.Objects to store the item state. Don't use
  Item.Objects yourself, use ItemObject instead.

  Note 2: The 'official' way to draw flat checkboxes, the DFCS_FLAT state, is
  kind of ugly, so I used some code found in TCheckListBox by Borland to draw
  better looking flat checkboxes.

  $Id: NewCheckListBox.pas,v 1.3 2001/07/01 21:58:42 jr Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TItemState = class
  public
    State: TCheckBoxState;
    Enabled: Boolean;
    Obj: TObject;
    SubItem: String;
  end;

  TDrawItemEvent2 = procedure(Control: TWinControl; Index: Integer; ACanvas: TCanvas; Rect: TRect;
   State: TOwnerDrawState) of object;

  TNewCheckListBox = class(TCustomListBox)
  private
    FCheckWidth: Integer;
    FCheckHeight: Integer;
    FStateList: TList;

    FAllowGrayed: Boolean;
    FFlat: Boolean;
    FOffset: Integer;

    FOnClickCheck: TNotifyEvent;

    FOnDrawItem2: TDrawItemEvent2;

    procedure CreateObject(Index: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure InvalidateCheck(Index:Integer);
  protected
    procedure SetChecked(Index: Integer; const AChecked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const AEnabled: Boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetState(Index: Integer; const AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetObject(Index: Integer; const AObject: TObject);
    function GetObject(Index: Integer): TObject;
    procedure SetSubItem(Index: Integer; const ASubItem: String);
    function GetSubItem(Index: Integer): String;
    procedure SetOffset(AnOffset: Integer);
    procedure SetFlat(Value: Boolean);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemObject[Index: Integer]: TObject read GetObject write SetObject;
    property ItemSubItem[Index: Integer]: String read GetSubItem write SetSubItem;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Offset: Integer read FOffset write SetOffset default 4;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
     property OnDrawItem2: TDrawItemEvent2 read FOnDrawItem2 write FOnDrawItem2;

    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style default lbOwnerDrawFixed;
    property TabOrder;
    property TabWidth;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

const
  OBM_CHECKBOXES = 32759;

constructor TNewCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with TBitmap.Create do begin
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
  end;

  FStateList := TList.Create;
  FOffset := 4;
  Style := lbOwnerDrawFixed;
end;

destructor TNewCheckListBox.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStateList.Count - 1 do
    TItemState(FStateList[i]).Free;
  FStateList.Free;
  inherited Destroy;
end;

procedure TNewCheckListBox.CreateObject(Index: Integer);
var
  ItemState: TItemState;
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  if Items.Objects[Index] = nil then begin
    ItemState := TItemState.Create;
    ItemState.Enabled := True;
    Items.Objects[Index] := ItemState;
    FStateList.Add(ItemState);
  end;
end;

procedure TNewCheckListBox.SetChecked(Index: Integer; const AChecked: Boolean);
begin
  if AChecked then
    SetState(Index, cbChecked)
  else
    SetState(Index, cbUnChecked);
end;

function TNewCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  Result := GetState(Index) <> cbUnchecked;
end;

procedure TNewCheckListBox.SetItemEnabled(Index: Integer; const AEnabled: Boolean);
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  CreateObject(Index);
  if TItemState(Items.Objects[Index]).Enabled <> AEnabled then begin
    TItemState(Items.Objects[Index]).Enabled := AEnabled;
    InvalidateCheck(Index);
  end;
end;

function TNewCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Items.Count) then
  begin
    Result := True;
    Exit;
  end;
  CreateObject(Index);
  Result := TItemState(Items.Objects[Index]).Enabled;
end;

procedure TNewCheckListBox.SetState(Index: Integer; const AState: TCheckBoxState);
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  CreateObject(Index);
  if TItemState(Items.Objects[Index]).State <> AState then begin
    TItemState(Items.Objects[Index]).State := AState;
    InvalidateCheck(Index);
  end;
end;

function TNewCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if (Index < 0) or (Index >= Items.Count) then
  begin
    Result := cbUnchecked;
    Exit;
  end;
  CreateObject(Index);
  Result := TItemState(Items.Objects[Index]).State;
end;

procedure TNewCheckListBox.SetObject(Index: Integer; const AObject: TObject);
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  CreateObject(Index);
  if TItemState(Items.Objects[Index]).Obj <> AObject then
    TItemState(Items.Objects[Index]).Obj := AObject;
end;

function TNewCheckListBox.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Items.Count) then
  begin
    Result := nil;
    Exit;
  end;
  CreateObject(Index);
  Result := TItemState(Items.Objects[Index]).Obj;
end;

procedure TNewCheckListBox.SetSubItem(Index: Integer; const ASubItem: String);
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  CreateObject(Index);
  if TItemState(Items.Objects[Index]).SubItem <> ASubItem then
    TItemState(Items.Objects[Index]).SubItem := ASubItem;
end;

function TNewCheckListBox.GetSubItem(Index: Integer): String;
begin
  if (Index < 0) or (Index >= Items.Count) then
  begin
    Result := '';
    Exit;
  end;
  CreateObject(Index);
  Result := TItemState(Items.Objects[Index]).SubItem;
end;

procedure TNewCheckListBox.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TNewCheckListBox.SetOffset(AnOffset: Integer);
begin
  if FOffset <> AnOffset then begin
    FOffset := AnOffset;
    Invalidate;
  end;
end;

procedure TNewCheckListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight(' ');
  if ItemHeight < 16 then
    ItemHeight := 16;
end;

procedure TNewCheckListBox.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
    rcItem.Left := rcItem.Left + FCheckWidth + 2*FOffset;
  inherited;
end;

procedure TNewCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Disabled: Boolean;
  uState: Integer;
  CheckRect: TRect;
  OldColor: COLORREF;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;
  ItemState: TItemState;
  SubItemSize: TSize;
  SubItemRect: TRect;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else begin
    if Assigned(OnDrawItem2) then
      OnDrawItem2(Self, Index, Canvas, Rect, State);
    CreateObject(Index);
    ItemState := TItemState(Self.Items.Objects[Index]);

    Disabled := not Enabled or not ItemState.Enabled;

    case ItemState.State of
      cbChecked:
        uState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked:
        uState := DFCS_BUTTONCHECK;
      else
        uState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    //if FFlat then
    //  uState := uState or DFCS_FLAT;
    if Disabled then
      uState := uState or DFCS_INACTIVE;

    with Canvas do begin
      Rect.Left := Rect.Left - FCheckWidth - 2*FOffset;

      CheckRect.Left := Rect.Left + FOffset;
      CheckRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - FCheckHeight) div 2;
      CheckRect.Bottom := CheckRect.Top + FCheckHeight;
      CheckRect.Right := CheckRect.Left + FCheckWidth;

      if FFlat then begin
        SaveRgn := CreateRectRgn(0,0,0,0);
        GetClipRgn(Handle, SaveRgn);
        with CheckRect do
          Rgn := CreateRectRgn(Left+2, Top+2, Right-2, Bottom-2);
        SelectClipRgn(Handle, Rgn);
        DeleteObject(Rgn);
      end else
        SaveRgn := 0; //surpress bogus warning

      DrawFrameControl(Handle, CheckRect, DFC_BUTTON, uState);

      if FFlat then begin
        SelectClipRgn(Handle, SaveRgn);
        DeleteObject(SaveRgn);
        OldBrushStyle := Brush.Style;
        OldBrushColor := Brush.Color;
        OldPenColor := Pen.Color;
        Brush.Style := bsClear;
        Pen.Color := clBtnShadow;
        with CheckRect do
          Rectangle(Left+1, Top+1, Right-1, Bottom-1);
        Brush.Style := OldBrushStyle;
        Brush.Color := OldBrushColor;
        Pen.Color := OldPenColor;
      end;

      Rect.Left := CheckRect.Right + FOffset;
      FillRect(Rect);
      Rect.Left := Rect.Left + 1;

      OldColor := GetTextColor(Handle);
      if Disabled and not (odSelected in State) then
        SetTextColor(Handle, GetSysColor(COLOR_GRAYTEXT));

      if ItemState.SubItem <> '' then begin
        GetTextExtentPoint32(Handle, PChar(ItemState.SubItem), Length(ItemState.SubItem), SubItemSize);
        SubItemSize.cx := SubItemSize.cx + 2*FOffset;
        SubItemRect := Rect;
        SubItemRect.Left := SubItemRect.Right - SubItemSize.cx + FOffset;
        DrawText(Handle, PChar(ItemState.SubItem), -1, SubItemRect, DT_NOCLIP or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);
        Rect.Right := Rect.Right - SubItemSize.cx;
      end;

      DrawText(Handle, PChar(Items[Index]), -1, Rect, DT_NOCLIP or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS);

      SetTextColor(Handle, OldColor);
    end;
  end;
end;

procedure TNewCheckListBox.InvalidateCheck(Index:Integer);
var
  IRect: TRect;
begin
  IRect := ItemRect(Index);
  Inc(IRect.Left, FOffset);
  IRect.Right := IRect.Left + ItemHeight;
  InvalidateRect(Handle, @IRect, False);
end;

procedure TNewCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Rect: TRect;
begin
  Index := ItemAtPos(Point(X, Y), True);
  if Index <> -1 then begin
    if Enabled and ItemEnabled[Index] then begin
      Rect := ItemRect(Index);
      if (Button = mbLeft) and (ssDouble in Shift) or ((X >= Rect.Left + FOffset) and (X < Rect.Left + FOffset + (Rect.Bottom - Rect.Top - 2)))  then
      begin
        case State[Index] of
        cbUnchecked:
          if FAllowGrayed then
            State[Index] := cbGrayed
          else
            State[Index] := cbChecked;
        cbChecked:
          State[Index] := cbUnchecked;
        cbGrayed:
          State[Index] := cbChecked;
        end;
        InvalidateCheck(Index);
        if Assigned(FOnClickCheck) then
          FOnClickCheck(Self);
      end;
    end;
  end;
  inherited;
end;

procedure TNewCheckListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ItemIndex <> -1 then begin
    if Enabled and ItemEnabled[ItemIndex] then begin
      if Key = VK_SPACE then begin
        case State[ItemIndex] of
        cbUnchecked:
          if FAllowGrayed then
            State[ItemIndex] := cbGrayed
          else
            State[ItemIndex] := cbChecked;
        cbChecked:
          State[ItemIndex] := cbUnchecked;
        cbGrayed:
          State[ItemIndex] := cbChecked;
        end;
        InvalidateCheck(ItemIndex);
        if Assigned(FOnClickCheck) then
          FOnClickCheck(Self);
      end;
    end;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('JR', [TNewCheckListBox]);
end;

end.
