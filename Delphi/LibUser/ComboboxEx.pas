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
 * TComboBoxEx VCL component
 *
 ****************************************************************************)

unit ComboboxEx;

interface

uses Windows, Controls, Classes, Messages, ImgList, NewCommCtrl, ComStrs,
    Graphics, Forms, StdCtrls, Common2;

type

  TComboBoxExStyle = (csDropDown, csSimple, csDropDownList);

  TCustomComboBoxEx = class(TWinControl)
  private
    FItems: TStrings;
    FCanvas: TCanvas;
    FStyle: TComboBoxExStyle;
    FMaxLength: Integer;
    FDropDownCount: Integer;
    FImages: TImageList;
    FItemHeight: Integer;
    FEditHandle: HWnd;
    FComboHandle: HWnd;
    FEditInstance: Pointer;
    FComboInstance: Pointer;
    FDefEditProc: Pointer;
    FDefComboProc: Pointer;
    FChangeLink: TChangeLink;
    FIsFocused: Boolean;
    FFocusChanged: Boolean;
    FSaveItems: TStrings;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure AdjustDropDown;
    procedure EditWndProc(var Message: TMessage);
    procedure ChildComboWndProc(var Message: TMessage);
    function GetDroppedDown: Boolean;
    function GetItemIndex: Integer;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetSorted: Boolean;
    procedure SetDroppedDown(Value: Boolean);
    procedure SetItems(Value: TStrings);
    procedure SetItemIndex(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelText(const Value: string);
    procedure SetSorted(Value: Boolean);
    function  GetItemHeight: Integer;
    procedure SetMaxLength(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetImageIndex(Index: Integer; const ImageIndex: Integer);
    function GetImageIndex(Index: Integer): Integer;
    procedure SetSelectedIndex(Index: Integer; const ImageIndex: Integer);
    function GetSelectedIndex(Index: Integer): Integer;
    procedure SetOverlayIndex(Index: Integer; const ImageIndex: Integer);
    function GetOverlayIndex(Index: Integer): Integer;
    procedure SetIndent(Index: Integer; const Indent: Integer);
    function GetIndent(Index: Integer): Integer;
    function GetText: TCaption;
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function GetTextLen: Integer;
    procedure SetText(const Value: TCaption);
    procedure ImageListChange(Sender: TObject);
    procedure WMCreate(var Message: TWMCreate); message WM_CREATE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMDeleteItem(var Message: TWMDeleteItem); message WM_DELETEITEM;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
        ComboProc: Pointer); virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Change; dynamic;
    procedure DropDown; dynamic;
    procedure SetStyle(Value: TComboBoxExStyle); virtual;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 8;
    property EditHandle: HWnd read FEditHandle;
    property ItemHeight: Integer read GetItemHeight;
    property ComboHandle: HWnd read FComboHandle;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ParentColor default False;
    //property Sorted: Boolean read GetSorted write SetSorted default False;
    property Style: TComboBoxExStyle read FStyle write SetStyle default csDropDown;
    property Images: TImageList read FImages write SetImages;
    property Text: TCaption read GetText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SelectAll;
    procedure SetImageList(Value: HImageList);
    function Add(const S: string; const Image, Selected, Overlay, Ind: Integer): Integer;
    procedure Insert(Index: Integer; const S: string; const Image, Selected, Overlay, Ind: Integer);
    property Canvas: TCanvas read FCanvas;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property ImageIndex[Index: Integer]: Integer read GetImageIndex write SetImageIndex;
    property SelectedIndex[Index: Integer]: Integer read GetSelectedIndex write SetSelectedIndex;
    property OverlayIndex[Index: Integer]: Integer read GetOverlayIndex write SetOverlayIndex;
    property Indent[Index: Integer]: Integer read GetIndent write SetIndent;
    property Sorted: Boolean read GetSorted write SetSorted;
  published
    property TabStop default True;
  end;

  TComboBoxEx = class(TCustomComboBoxEx)
  published
    property Style; {Must be published before Items}
    property Images;
    property Color;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

  THackEdit = class(TCustomEdit);

function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);

procedure Register;

implementation

uses Consts;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then
    InitCommonControls;
end;

procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create(SInvalidComCtl32);
end;

function HasPopup(Control: TControl): Boolean;
begin
  Result := True;
  while Control <> nil do begin
    if THackEdit(Control).PopupMenu <> nil then
      Exit
    else
      Control := Control.Parent;
  end;
  Result := False;
end;

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

  TComboBoxExStrings = class(TStrings)
  private
    FComboBox: TCustomComboBoxEx;
    FShadowItems: TStringList;
    function InsertItem(Index: Integer; const S: string;
        const Image, Selected, Overlay, Ind: Integer): Integer;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    constructor Create(Owner: TCustomComboBoxEx);
    destructor Destroy; override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

{ TComboBoxExStrings }

constructor TComboBoxExStrings.Create(Owner: TCustomComboBoxEx);
begin
  inherited Create;
  FCombobox := Owner;
  FShadowItems := TStringList.Create;
  FShadowItems.Duplicates := dupAccept;
  // CBS_SORT not available for ComboBoxEx, so we need to sort manually
  FShadowItems.Sorted := False;
end;

destructor TComboBoxExStrings.Destroy;
begin
  FreeAndNil(FShadowItems);
  inherited;
end;

function TComboBoxExStrings.GetCount: Integer;
begin
  Result := SendMessage(FComboBox.Handle, CB_GETCOUNT, 0, 0);
end;

function TComboBoxExStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
  Len: Integer;
begin
  Len := SendMessage(FComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
  if Len = CB_ERR then
    Len := 0;
  SendMessage(FComboBox.Handle, CB_GETLBTEXT, Index, Longint(@Text));
  SetString(Result, Text, Len);
end;

function TComboBoxExStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(SendMessage(FComboBox.Handle, CB_GETITEMDATA, Index, 0));
end;

procedure TComboBoxExStrings.PutObject(Index: Integer; AObject: TObject);
begin
  SendMessage(FComboBox.Handle, CB_SETITEMDATA, Index, Longint(AObject));
end;

function TComboBoxExStrings.InsertItem(Index: Integer; const S: string;
    const Image, Selected, Overlay, Ind: Integer): Integer;
var
  SortIndex: Integer;
  CBItem: TComboBoxExItem;
begin
  SortIndex := FShadowItems.Add(S);
  if FShadowItems.Sorted then begin
    if Index = -1 then
      Index := SortIndex
    else
      Error(SSortedListError, 0)
  end;
  with CBItem do begin
    mask := CBEIF_TEXT or CBEIF_IMAGE or CBEIF_SELECTEDIMAGE or CBEIF_OVERLAY
        or CBEIF_INDENT;
    iItem := Index;
    pszText := PChar(S);
    //cchTextMax := Length(S);
    iSelectedImage := Selected;
    iImage := Image;
    iOverlay := Overlay + 1;
    iIndent := Ind;
  end;
  Result := SendMessage(FComboBox.Handle, CBEM_INSERTITEM, 0, Longint(@CBItem));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

function TComboBoxExStrings.Add(const S: string): Integer;
begin
  Result := InsertItem(-1, S, -1, -1, -1, -1);
end;

procedure TComboBoxExStrings.Insert(Index: Integer; const S: string);
begin
  InsertItem(Index, S, -1, -1, -1, -1);
end;

procedure TComboBoxExStrings.Delete(Index: Integer);
begin
  FShadowItems.Delete(Index);
  SendMessage(FComboBox.Handle, CBEM_DELETEITEM, Index, 0);
end;

procedure TComboBoxExStrings.Clear;
var
  S: string;
begin
  FShadowItems.Clear;
  S := FComboBox.Text;
  SendMessage(FComboBox.Handle, CB_RESETCONTENT, 0, 0);
  FComboBox.Text := S;
  FComboBox.Update;
end;

procedure TComboBoxExStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(FComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if Updating then
    FShadowItems.BeginUpdate
  else begin
    FShadowItems.EndUpdate;
    FComboBox.Refresh;
  end;
end;

{ TCustomComboBoxEx }

constructor TCustomComboBoxEx.Create(AOwner: TComponent);
const
  ComboBoxStyle = [csCaptureMouse, csSetCaption, csDoubleClicks,
    csFixedHeight, csReflector];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := ComboBoxStyle
  else
    ControlStyle := ComboBoxStyle + [csFramed];
  Width := 145;
  Height := 25;
  TabStop := True;
  ParentColor := False;
  FItems := TComboBoxExStrings.Create(Self);
  FCanvas := TControlCanvas.Create;
  FItemHeight := 16;
  FStyle := csDropDown;
  FEditInstance := MakeObjectInstance(EditWndProc);
  FComboInstance := MakeObjectInstance(ChildComboWndProc);
  FDropDownCount := 8;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
end;

destructor TCustomComboBoxEx.Destroy;
begin
  if HandleAllocated then
    DestroyWindowHandle;
  FreeObjectInstance(FComboInstance);
  FreeObjectInstance(FEditInstance);
  FCanvas.Free;
  FItems.Free;
  FSaveItems.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TCustomComboBoxEx.Clear;
begin
  SetTextBuf('');
  FItems.Clear;
end;

procedure TCustomComboBoxEx.SelectAll;
begin
  HandleNeeded;
  SendMessage(FEditHandle, EM_SETSEL, 0, -1);
  //SendMessage(Handle, CB_SETEDITSEL, 0, $FFFF0000);
end;

function TCustomComboBoxEx.GetDroppedDown: Boolean;
begin
  Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0));
end;

procedure TCustomComboBoxEx.SetDroppedDown(Value: Boolean);
begin
  SendMessage(Handle, CB_SHOWDROPDOWN, Longint(Value), 0);
end;

function TCustomComboBoxEx.GetItemIndex: Integer;
begin
  Result := SendMessage(Handle, CB_GETCURSEL, 0, 0);
end;

procedure TCustomComboBoxEx.SetItemIndex(Value: Integer);
begin
  SendMessage(Handle, CB_SETCURSEL, Value, 0);
end;

function TCustomComboBoxEx.GetSelStart: Integer;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Result), 0);
end;

procedure TCustomComboBoxEx.SetSelStart(Value: Integer);
{var
  Selection: TSelection;}
begin
  HandleNeeded;
  SendMessage(FEditHandle, EM_SETSEL, Value, Value);
  {Selection.StartPos := Value;
  Selection.EndPos := Value;}
  //SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;

function TCustomComboBoxEx.GetSelLength: Integer;
var
  Selection: TSelection;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Selection.StartPos),
    Longint(@Selection.EndPos));
  Result := Selection.EndPos - Selection.StartPos;
end;

procedure TCustomComboBoxEx.SetSelLength(Value: Integer);
var
  Selection: TSelection;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Selection.StartPos),
      Longint(@Selection.EndPos));
  Selection.EndPos := Selection.StartPos + Value;
  HandleNeeded;
  SendMessage(FEditHandle, EM_SETSEL, Selection.StartPos, Selection.EndPos);
  //SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;

function TCustomComboBoxEx.GetSelText: string;
begin
  Result := '';
  if FStyle < csDropDownList then
    Result := Copy(Text, GetSelStart + 1, GetSelLength);
end;

procedure TCustomComboBoxEx.SetSelText(const Value: string);
begin
  if FStyle < csDropDownList then begin
    HandleNeeded;
    SendMessage(FEditHandle, EM_REPLACESEL, 0, Longint(PChar(Value)));
  end;
end;

procedure TCustomComboBoxEx.SetMaxLength(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FMaxLength <> Value then begin
    FMaxLength := Value;
    if HandleAllocated then
      SendMessage(Handle, CB_LIMITTEXT, Value, 0);
  end;
end;

function TCustomComboBoxEx.GetSorted: Boolean;
begin
  Result := TComboBoxExStrings(FItems).FShadowItems.Sorted;
end;

procedure TCustomComboBoxEx.SetSorted(Value: Boolean);
begin
  TComboBoxExStrings(FItems).Clear;
  TComboBoxExStrings(FItems).FShadowItems.Sorted := Value;
end;

procedure TCustomComboBoxEx.SetStyle(Value: TComboBoxExStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    if Value = csSimple then
      ControlStyle := ControlStyle - [csFixedHeight] else
      ControlStyle := ControlStyle + [csFixedHeight];
    RecreateWnd;
  end;
end;

function TCustomComboBoxEx.GetItemHeight: Integer;
begin
  Result := Perform(CB_GETITEMHEIGHT, 0, 0);
end;

procedure TCustomComboBoxEx.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TCustomComboBoxEx.Add(const S: string;
    const Image, Selected, Overlay, Ind: Integer): Integer;
begin
  Result := TComboBoxExStrings(FItems).InsertItem(-1, S, Image, Selected,
      Overlay, Ind);
end;

procedure TCustomComboBoxEx.Insert(Index: Integer; const S: string;
    const Image, Selected, Overlay, Ind: Integer);
begin
  TComboBoxExStrings(FItems).InsertItem(Index, S, Image, Selected, Overlay, Ind);
end;

procedure TCustomComboBoxEx.SetImageList(Value: HImageList);
begin
  HandleNeeded;
  if HandleAllocated then
    Perform(CBEM_SETIMAGELIST, 0, Value);
end;

procedure TCustomComboBoxEx.ImageListChange(Sender: TObject);
var
  ImageHandle: HImageList;
begin
  if HandleAllocated then begin
    ImageHandle := TImageList(Sender).Handle;
    if Sender = Images then
      SetImageList(ImageHandle)
  end;
end;

procedure TCustomComboBoxEx.SetImages(Value: TImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FChangeLink);
  FImages := Value;
  if Images <> nil then begin
    Images.RegisterChanges(FChangeLink);
    SetImageList(Images.Handle)
  end
  else
    SetImageList(0);
end;

procedure TCustomComboBoxEx.SetImageIndex(Index: Integer;
    const ImageIndex: Integer);
var
  CBEItem: TComboBoxExItem;
begin
  CBEItem.mask := CBEIF_IMAGE or CBEIF_SELECTEDIMAGE;
  CBEItem.iItem := Index;
  CBEItem.iImage := ImageIndex;
  CBEItem.iSelectedImage := ImageIndex;
  SendMessage(Handle, CBEM_SETITEM, 0, Longint(@CBEItem));
end;

function TCustomComboBoxEx.GetImageIndex(Index: Integer): Integer;
var
  CBEItem: TComboBoxExItem;
begin
  Result := -1;
  CBEItem.iItem := Index;
  CBEItem.mask := CBEIF_IMAGE;
  if LongBool(SendMessage(Handle, CBEM_GETITEM, 0, Longint(@CBEItem))) then
    Result := CBEItem.iImage
end;

procedure TCustomComboBoxEx.SetSelectedIndex(Index: Integer;
    const ImageIndex: Integer);
var
  CBEItem: TComboBoxExItem;
begin
  CBEItem.mask := CBEIF_SELECTEDIMAGE;
  CBEItem.iItem := Index;
  CBEItem.iSelectedImage := ImageIndex;
  SendMessage(Handle, CBEM_SETITEM, 0, Longint(@CBEItem));
end;

function TCustomComboBoxEx.GetSelectedIndex(Index: Integer): Integer;
var
  CBEItem: TComboBoxExItem;
begin
  Result := -1;
  CBEItem.iItem := Index;
  CBEItem.mask := CBEIF_SELECTEDIMAGE;
  if LongBool(SendMessage(Handle, CBEM_GETITEM, 0, Longint(@CBEItem))) then
    Result := CBEItem.iSelectedImage
end;

procedure TCustomComboBoxEx.SetOverlayIndex(Index: Integer;
    const ImageIndex: Integer);
var
  CBEItem: TComboBoxExItem;
begin
  CBEItem.mask := CBEIF_OVERLAY;
  CBEItem.iItem := Index;
  CBEItem.iOverlay := ImageIndex + 1;
  SendMessage(Handle, CBEM_SETITEM, 0, Longint(@CBEItem));
end;

function TCustomComboBoxEx.GetOverlayIndex(Index: Integer): Integer;
var
  CBEItem: TComboBoxExItem;
begin
  Result := -1;
  CBEItem.iItem := Index;
  CBEItem.mask := CBEIF_OVERLAY;
  if LongBool(SendMessage(Handle, CBEM_GETITEM, 0, Longint(@CBEItem))) then
    Result := CBEItem.iOverlay - 1;
end;

procedure TCustomComboBoxEx.SetIndent(Index: Integer; const Indent: Integer);
var
  CBEItem: TComboBoxExItem;
begin
  CBEItem.mask := CBEIF_INDENT;
  CBEItem.iItem := Index;
  CBEItem.iIndent := Indent;
  SendMessage(Handle, CBEM_SETITEM, 0, Longint(@CBEItem));
end;

function TCustomComboBoxEx.GetIndent(Index: Integer): Integer;
var
  CBEItem: TComboBoxExItem;
begin
  Result := -1;
  CBEItem.iItem := Index;
  CBEItem.mask := CBEIF_INDENT;
  if LongBool(SendMessage(Handle, CBEM_GETITEM, 0, Longint(@CBEItem))) then
    Result := CBEItem.iIndent
end;

function TCustomComboBoxEx.GetTextLen: Integer;
begin
  Result := SendMessage(EditHandle, WM_GETTEXTLENGTH, 0, 0);
end;

function TCustomComboBoxEx.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := SendMessage(EditHandle, WM_GETTEXT, BufSize, Longint(Buffer));
end;

function TCustomComboBoxEx.GetText: TCaption;
var
  Len: Integer;
begin
  Len := GetTextLen;
  SetString(Result, PChar(nil), Len);
  if Len <> 0 then
    GetTextBuf(Pointer(Result), Len + 1);
end;

procedure TCustomComboBoxEx.SetText(const Value: TCaption);
begin
  if GetText <> Value then
    SetTextBuf(PChar(Value));
end;

procedure TCustomComboBoxEx.CreateParams(var Params: TCreateParams);
const
  ComboBoxExStyles: array[TComboBoxExStyle] of Cardinal = (
      CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST);
begin
  CheckCommonControl(ICC_USEREX_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_COMBOBOXEX);
  Params.Style := Params.Style or WS_VSCROLL or ComboBoxExStyles[FStyle]
      or CBS_AUTOHSCROLL;
end;

procedure TCustomComboBoxEx.CreateWnd;
var
  ChildHandle: THandle;
  //Style: Integer;
begin
  inherited CreateWnd;
  SendMessage(Handle, CB_LIMITTEXT, FMaxLength, 0);
  if FSaveItems <> nil then begin
    FItems.Assign(FSaveItems);
    FSaveItems.Free;
    FSaveItems := nil;
  end;
  FEditHandle := 0;
  FComboHandle := 0;
  ChildHandle := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
  if ChildHandle <> 0 then begin
    FComboHandle := ChildHandle;
    FDefComboProc := Pointer(GetWindowLong(FComboHandle, GWL_WNDPROC));
    SetWindowLong(FComboHandle, GWL_WNDPROC, Longint(FComboInstance));
    EnableWindow(FComboHandle, Enabled);
  end;
//  style := GetWindowLong(FComboHandle, GWL_STYLE);
//  Style := Style or CBS_SORT;
//  SetWindowLong(fcombohandle, GWL_STYLE, style);
  if FStyle in [csDropDown, csSimple] then begin
    ChildHandle := SendMessage(Handle, CBEM_GETEDITCONTROL, 0, 0);
    if ChildHandle <> 0 then begin
      FEditHandle := ChildHandle;
      FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
      SetWindowLong(FEditHandle, GWL_WNDPROC, Longint(FEditInstance));
      EnableWindow(FEditHandle, Enabled);
    end;
  end;
  if NewStyleControls and (FEditHandle <> 0) then
    SendMessage(FEditHandle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0);
end;

procedure TCustomComboBoxEx.DestroyWnd;
begin
  if FItems.Count > 0 then begin
    FSaveItems := TStringList.Create;
    FSaveItems.Assign(FItems);
  end;
  inherited DestroyWnd;
end;

procedure TCustomComboBoxEx.WMCreate(var Message: TWMCreate);
begin
  inherited;
  SetWindowText(Handle, WindowText);
end;

procedure TCustomComboBoxEx.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Style = csSimple then begin
    FillRect(Message.DC, ClientRect, Parent.Brush.Handle);
    Message.Result := 1;
  end
  else
    DefaultHandler(Message);
end;

procedure TCustomComboBoxEx.WMDeleteItem(var Message: TWMDeleteItem);
begin
  DefaultHandler(Message);
end;

procedure TCustomComboBoxEx.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if DroppedDown then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomComboBoxEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FComboHandle <> 0 then
    EnableWindow(FComboHandle, Enabled);
  if FEditHandle <> 0 then
    EnableWindow(FEditHandle, Enabled);
end;

procedure TCustomComboBoxEx.CMCancelMode(var Message: TCMCancelMode);
begin
  if Message.Sender <> Self then
    Perform(CB_SHOWDROPDOWN, 0, 0);
end;

procedure TCustomComboBoxEx.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
    RecreateWnd;
  inherited;
end;

procedure TCustomComboBoxEx.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if not NewStyleControls and (Style < csDropDownList) then
    Invalidate;
end;

procedure TCustomComboBoxEx.ChildComboWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_SYSCOMMAND then begin
    WndProc(Message);
    Exit;
  end;
  ComboWndProc(Message, FComboHandle, FDefComboProc);
end;

procedure TCustomComboBoxEx.EditWndProc(var Message: TMessage);
var
  P: TPoint;
  Form: TCustomForm;
begin
  if Message.Msg = WM_SYSCOMMAND then begin
    WndProc(Message);
    Exit;
  end
  else if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then begin
    Form := GetParentForm(Self);
    if (Form <> nil) and Form.WantChildKey(Self, Message) then
      Exit;
  end;
  ComboWndProc(Message, FEditHandle, FDefEditProc);
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
      if DragMode = dmAutomatic then begin
        GetCursorPos(P);
        P := ScreenToClient(P);
        SendMessage(FEditHandle, WM_LBUTTONUP, 0,Longint(PointToSmallPoint(P)));
        BeginDrag(False);
      end;
    end;
    WM_SETFONT: begin
      if NewStyleControls then
        SendMessage(FEditHandle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0);
    end;
  end;
end;

procedure TCustomComboBoxEx.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
    ComboProc: Pointer);
var
  Point: TPoint;
  Form: TCustomForm;
begin
  try
    with Message do begin
      case Msg of
        WM_SETFOCUS: begin
          Form := GetParentForm(Self);
          if (Form <> nil) and not Form.SetFocusedControl(Self) then
            Exit;
        end;
        WM_KILLFOCUS: begin
          if csFocusing in ControlState then
            Exit;
        end;
        WM_KEYDOWN, WM_SYSKEYDOWN: begin
          if DoKeyDown(TWMKey(Message)) then
            Exit;
        end;
        WM_CHAR: begin
          if DoKeyPress(TWMKey(Message)) then
            Exit;
          if ((TWMKey(Message).CharCode = VK_RETURN) or
              (TWMKey(Message).CharCode = VK_ESCAPE))
              and DroppedDown then begin
            DroppedDown := False;
            Exit;
          end;
        end;
        WM_KEYUP, WM_SYSKEYUP: begin
          if DoKeyUp(TWMKey(Message)) then
            Exit;
        end;
        WM_MOUSEMOVE:
          Application.HintMouseMessage(Self, Message);
        WM_RBUTTONUP: begin
          if HasPopup(Self) then begin
            with TWMRButtonUp(Message) do begin
              Point.X := Pos.X;
              Point.Y := Pos.Y;
              MapWindowPoints(ComboWnd, Handle, Point, 1);
              Pos.X := Point.X;
              Pos.Y := Point.Y;
            end;
            WndProc(Message);
            Exit;
          end;
        end;
        WM_GETDLGCODE: begin
          if DroppedDown then begin
            Result := DLGC_WANTALLKEYS;
            Exit;
          end;
        end;
        WM_NCHITTEST: begin
          if csDesigning in ComponentState then begin
            Result := HTTRANSPARENT;
            Exit;
          end;
        end;
        CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN, CN_SYSCHAR: begin
          WndProc(Message);
          Exit;
        end;
      end;
      Result := CallWindowProc(ComboProc, ComboWnd, Msg, WParam, LParam);
      if (Msg = WM_LBUTTONDBLCLK) and (csDoubleClicks in ControlStyle) then
        DblClick;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomComboBoxEx.WndProc(var Message: TMessage);
begin
  {for auto drag mode, let listbox handle itself, instead of TControl}
  if not (csDesigning in ComponentState) and
      ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
      not Dragging then begin
    if DragMode = dmAutomatic then begin
      if IsControlMouseMsg(TWMMouse(Message)) then
        Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);  {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  with Message do begin
    case Msg of
      WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC: begin
        SetTextColor(WParam, ColorToRGB(Font.Color));
        SetBkColor(WParam, ColorToRGB(Brush.Color));
        Result := Brush.Handle;
        Exit;
      end;
      CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC: begin
        if not NewStyleControls and (Style < csDropDownList) then begin
          Result := Parent.Brush.Handle;
          Exit;
        end;
      end;
      WM_CHAR: begin
        if DoKeyPress(TWMKey(Message)) then
          Exit;
        if ((TWMKey(Message).CharCode = VK_RETURN) or
            (TWMKey(Message).CharCode = VK_ESCAPE))
            and DroppedDown then begin
          DroppedDown := False;
          Exit;
        end;
      end;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomComboBoxEx.AdjustDropDown;
var
  ItemCount: Integer;
begin
  ItemCount := FItems.Count;
  if ItemCount > DropDownCount then
    ItemCount := DropDownCount;
  if ItemCount < 1 then
    ItemCount := 1;
  SetWindowPos(FComboHandle, 0, 0, 0,
      Width,
      ItemHeight * ItemCount + Height + 2,
      SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOREDRAW
      or SWP_HIDEWINDOW);
  SetWindowPos(FComboHandle, 0, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE
      or SWP_NOREDRAW or SWP_SHOWWINDOW);
end;

procedure TCustomComboBoxEx.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_DBLCLK:
      DblClick;
    CBN_EDITCHANGE:
      Change;
    CBN_DROPDOWN: begin
      FFocusChanged := False;
      DropDown;
      AdjustDropDown;
      if FFocusChanged then begin
        PostMessage(Handle, WM_CANCELMODE, 0, 0);
        if not FIsFocused then
          PostMessage(ComboHandle, CB_SHOWDROPDOWN, 0, 0);
      end;
    end;
    CBN_SELCHANGE: begin
      //Text := Items[ItemIndex];
      Click;
      Change;
    end;
    CBN_SETFOCUS: begin
      FIsFocused := True;
      FFocusChanged := True;
      SetIme;
    end;
    CBN_KILLFOCUS: begin
      FIsFocused := False;
      FFocusChanged := True;
      ResetIme;
    end;
  end;
end;

procedure TCustomComboBoxEx.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomComboBoxEx.DropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCustomComboBoxEx.WMLButtonDown(var Message: TWMLButtonDown);
var
  Form: TCustomForm;
begin
  if (DragMode = dmAutomatic) and (Style = csDropDownList) and
      (Message.XPos < (Width - GetSystemMetrics(SM_CXHSCROLL))) then begin
    SetFocus;
    BeginDrag(False);
    Exit;
  end;
  inherited;
  if MouseCapture then begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl <> Self) then
      MouseCapture := False;
  end;
end;

procedure Register;
begin
  RegisterComponents('BM', [TComboBoxEx]);
end;

end.
