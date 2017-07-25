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
 * TSpinIntEdit VCL component
 *
 ****************************************************************************)

unit SpinIntEdit;

{ This is a quickly hacked TSpinEdit replacement; cut and pasted
  from existing source code with no claims for elegance.
  It is designed for quick'n'dirty use in GExperts, no more, no less. }

interface

uses Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ComCtrls, Common2,
    UpDown32;

const
  ECM_FIRST = $00001500;
  EM_SHOWBALLOONTIP = (ECM_FIRST + 3);     // Show a balloon tip associated to the edit control
  EM_HIDEBALLOONTIP = (ECM_FIRST + 4);     // Hide any balloon tip associated with the edit control

type
  THideBalloonEvent = procedure(Sender: TObject; After: Boolean) of object;

type
  TSpinIntEdit = class(TCustomEdit)
  private
    FUpDown: TUpDown32;
    FValue: Integer;
    FIncrement: Integer;
    FAllowPlusMin: Boolean;
    FUpDownKeys: Boolean;
    //FOldWndProc: TWndMethod;
    FOnHideBalloon: THideBalloonEvent;
    FLButtonDown: Boolean;
    FFirstFocusClick: Boolean;
    function GetMaximum: Integer;
    function GetMinimum: Integer;
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    function GetEmpty: Boolean;
    procedure SetEmpty(Empty: Boolean);
    function IsWithInMinMax(const Value: Integer): Boolean;
    procedure SetAllowPlusMin(const Value: Boolean);
  private
    procedure WMCut(var Message: TWMPaste); message WM_CUT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    //procedure EMShowBalloonTip(var Message: TMessage); message EM_SHOWBALLOONTIP;
    procedure EMHideBalloonTip(var Message: TMessage); message EM_HIDEBALLOONTIP;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    //procedure WndProc(var Message: TMessage);
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Change; override;

    procedure AdjustSpin;
    procedure CreateSpin;
    procedure DestroySpin;

    procedure SetEditRect;

    function GetValue: Integer;
    procedure SetValue(Value: Integer);

    function TextToInt(const Value: string): Integer;
    function IntToText(const Value: Integer): String;

    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);

    procedure Increase(Increment: Integer); virtual;

    function IsValidChar(const Key: Char): Boolean; virtual;
    function IsValidData(const NewData: String): Boolean; virtual;
    function IsValidValueString(const ValueString: String): Boolean; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure UpdateValue; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // the following property isn't published for backwards compatibility
    property Empty: Boolean read GetEmpty write SetEmpty;
  published
    property Maximum: Integer read GetMaximum write SetMaximum default 100;
    property Minimum: Integer read GetMinimum write SetMinimum default 0;
    property Value: Integer read GetValue write SetValue;
    property AllowPlusMin: Boolean read FAllowPlusMin write SetAllowPlusMin
        default False;
    property UpDownKeys: Boolean read FUpDownKeys write FUpDownKeys
        default True;
    property OnHideBalloon: THideBalloonEvent read FOnHideBalloon
        write FOnHideBalloon;
    property Anchors;
    property TabOrder;
    property Enabled;
    property AutoSelect;
    property OnChange;
    property OnKeyDown;
  end;


procedure Register;

implementation

uses
  ClipBrd;

procedure Register;
begin
  RegisterComponents('GExperts', [TSpinIntEdit]);
end;


procedure TSpinIntEdit.WMPaste(var Message: TWMPaste);
var
  ClipStr: string;
begin
  if ReadOnly then
    Exit;
  with Clipboard do begin
    Open;
    try
      ClipStr := '';
      if HasFormat(CF_TEXT) then
        ClipStr := AsText;
    finally
      Close;
    end;
  end;
  if not IsValidData(ClipStr) then
    Exit;
  inherited;
end;

procedure TSpinIntEdit.WMCut(var Message: TWMPaste);
begin
  if ReadOnly then
    Exit;
  inherited;
end;

procedure TSpinIntEdit.CMEnter(var Message: TCMGotFocus);
begin
//  if AutoSelect {and not (csLButtonDown in ControlState)} then
//    SelectAll;
  inherited;
end;

procedure TSpinIntEdit.Change;
begin
  inherited Change;
  UpdateValue;
end;

procedure TSpinIntEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateValue;
end;

procedure TSpinIntEdit.CreateParams(var Params: TCreateParams);
const
  NumberStyle: array[Boolean] of Cardinal = (0, ES_NUMBER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or NumberStyle[not FAllowPlusMin]
      or WS_CLIPCHILDREN;
end;

procedure TSpinIntEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FUpDownKeys then begin
    case Key of
      VK_UP:    Increase(FIncrement);
      VK_DOWN:  Increase(-FIncrement);
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TSpinIntEdit.KeyPress(var Key: Char);
begin
  if FAllowPlusMin then begin
    if not IsValidData(Key) then
      Key := #0;
  end;
  inherited KeyPress(Key)
end;

function TSpinIntEdit.IsValidData(const NewData: String): Boolean;
var
  EditText: String[20];
  EditStart: DWORD;
  EditEnd: DWORD;
  i: Integer;
begin
  Result := True;
  { always allow #8 = Delete as a valid key }
  if NewData = #8 then
    Exit;
  { verify that all keys }
  for i := 1 to Length(NewData) do
    Result := Result and IsValidChar(NewData[i]);
  { terminate early if even the characters(s) are not OK }
  if not Result then
    Exit;
  EditText[0] := Char(SendMessage(Handle, WM_GETTEXT, SizeOf(EditText) - 1,
      Integer(@EditText[1])));
  SendMessage(Handle, EM_GETSEL, Integer(@EditStart), Integer(@EditEnd));
  { Remove currently selected text from edit control }
  Delete(EditText, EditStart + 1, EditEnd - EditStart);
  { Insert typed character at caret position }
  Insert(NewData, EditText, EditStart + 1);
  Result := IsValidValueString(EditText);
end;


constructor TSpinIntEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse];
  FIncrement := 1;
  FAllowPlusMin := False;
  FUpDownKeys := True;
  CreateSpin;
  //FOldWndProc := WindowProc;
  //WindowProc := WndProc;
  FLButtonDown := False;
  FFirstFocusClick := False;
end;

destructor TSpinIntEdit.Destroy;
begin
  DestroySpin;
  inherited Destroy;
end;

procedure TSpinIntEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TSpinIntEdit.Loaded;
begin
  inherited Loaded;
  {if Text = '' then
    Text := '0';}
end;

procedure TSpinIntEdit.CreateSpin;
begin
  if FUpDown = nil then begin
    FUpDown := TUpDown32.Create(Self);
    with FUpDown do begin
      Parent := Self;
      OnMouseDown := Self.OnMouseDown;
      OnClick := UpDownClick;
      SetBounds(0, 0, Width, Height);
      Visible := Self.Visible;
    end;
  end;
end;

procedure TSpinIntEdit.DestroySpin;
begin
  FUpDown.Free;
  FUpDown := nil;
end;

procedure TSpinIntEdit.SetEditRect;
var
  Loc: TRect;
begin
  if FUpDown <> nil then begin
    Loc.Bottom := ClientHeight + 1; { +1 is a work-around for Windows paint bug }
    Loc.Right := ClientWidth - FUpDown.Width - 2;
    Loc.Top := 0;
    Loc.Left := 0;
  end
  else begin
    Loc.Bottom := ClientHeight;
    Loc.Right := ClientWidth;
    Loc.Top := 0;
    Loc.Left := 0;
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TSpinIntEdit.UpClick(Sender: TObject);
begin
  Increase(FIncrement);
end;

procedure TSpinIntEdit.DownClick(Sender: TObject);
begin
  Increase(-FIncrement);
end;

procedure TSpinIntEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btPrev: DownClick(Sender);
    btNext: UpClick(Sender);
  else
    Assert(False);
  end;
end;

procedure TSpinIntEdit.AdjustSpin;
begin
  if NewStyleControls then
    FUpDown.SetBounds(Width - 19, 0, 15, Height - 4)
  else
    FUpDown.SetBounds(Width - 15, 0, 15, Height);
  SetEditRect;
end;

procedure TSpinIntEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  AdjustSpin;
end;

procedure TSpinIntEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  FFirstFocusClick := FLButtonDown;
  inherited;
  //SelectAll; // doesn't work here
end;

procedure TSpinIntEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  FFirstFocusClick := False;
  inherited;
end;

procedure TSpinIntEdit.WMLButtonDown(var Message: TMessage);
begin
  FLButtonDown := True;
  try
    inherited;
  finally
    FLButtonDown := False;
  end;
end;

procedure TSpinIntEdit.WMLButtonUp(var Message: TMessage);
begin
  inherited;
  if FFirstFocusClick then begin
    if AutoSelect and (SelLength = 0) then
      SelectAll;
    FFirstFocusClick := False;
  end;
end;

//procedure TSpinIntEdit.WndProc(var Message: TMessage);
//var
//  oldsellength: Integer;
//begin
//  if Message.Msg = CM_ENTER then
//    Exit;
////  if Message.Msg = WM_SETFOCUS then begin
////    Message.Result := DefWindowProc(Handle, Message.Msg, Message.WParam,
////        Message.LParam);
////    Exit;
////  end;
//  oldsellength := sellength;
//  FOldWndProc(Message);
//  if (Message.msg <> 176) and (oldsellength <> sellength) then
//    debug(inttostr(message.msg));
////  if SelLength > 0 then
////    debug('ok')
////  else
////    debug('fail');
//end;

function TSpinIntEdit.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TSpinIntEdit.SetValue(Value: Integer);
begin
  try
    Text := IntToText(Value);
    FValue := Value;
  except
    Text := '0';
    FValue := 0;
  end;
  FUpDown.Position32 := Value;
end;

function TSpinIntEdit.IsValidChar(const Key: Char): Boolean;
begin
  Result := (Key in ['0'..'9']);
  if not Result and FAllowPlusMin then
    Result := Key in ['+', '-'];
end;

function TSpinIntEdit.IsValidValueString(const ValueString: string): Boolean;
begin
  Result := IsWithInMinMax(TextToInt(ValueString));
end;

function TSpinIntEdit.TextToInt(const Value: string): Integer;
begin
  Result := 0;
  if Value <> '' then try
    Result := StrToInt(Value)
  except
    on E: EConvertError do
      // nothing
    else
      raise;
  end;
end;

function TSpinIntEdit.IntToText(const Value: Integer): string;
begin
  Result := IntToStr(Value)
end;

procedure TSpinIntEdit.UpdateValue;
begin
  FValue := TextToInt(Text);
  FUpDown.Position32 := FValue;
end;

procedure TSpinIntEdit.Increase(Increment: Integer);
var
  Temp: Integer;
begin
  Temp := FValue + Increment;
  if Temp > Maximum then
    SetValue(Maximum)
  else if Temp < Minimum then
    SetValue(Minimum)
  else
    SetValue(Temp);
  Modified := True; // don't do this in SetValue; Modified should only be set on user input
end;

function TSpinIntEdit.GetMaximum: Integer;
begin
  Result := FUpDown.Max32;
end;

function TSpinIntEdit.GetMinimum: Integer;
begin
  Result := FUpDown.Min32;
end;

procedure TSpinIntEdit.SetMaximum(const Value: Integer);
begin
  FUpDown.Max32 := Value;
end;

procedure TSpinIntEdit.SetMinimum(const Value: Integer);
begin
  FUpDown.Min32 := Value;
end;

function TSpinIntEdit.IsWithInMinMax(const Value: Integer): Boolean;
begin
  Result := ((Value >= Minimum) and (Value <= Maximum));
end;

function TSpinIntEdit.GetEmpty: Boolean;
begin
  Result := (Text = '');
end;

procedure TSpinIntEdit.SetEmpty(Empty: Boolean);
begin
  if Empty then
    Text := ''
  else
    Text := IntToStr(FValue);
end;

procedure TSpinIntEdit.SetAllowPlusMin(const Value: Boolean);
begin
  if Value <> FAllowPlusMin then begin
    FAllowPlusMin := Value;
    RecreateWnd;
  end;
end;

{procedure TSpinIntEdit.EMShowBalloonTip(var Message: TMessage);
begin
  inherited;
end;}

procedure TSpinIntEdit.EMHideBalloonTip(var Message: TMessage);
var
  Event: THideBalloonEvent;
begin
  Event := FOnHideBalloon;
  if Assigned(Event) then
    Event(Self, False);
  try
    inherited;
  finally
    if Assigned(Event) then
      Event(Self, True);
  end;
end;

procedure TSpinIntEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FUpDown.Enabled := Enabled;
end;

end.
