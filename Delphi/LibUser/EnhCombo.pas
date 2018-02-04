(****************************************************************************
 *
 * Copyright 2018 Tim De Baets
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
 * Enhanced TTntComboBox VCL component
 * Contains portions of EnhCombo.pas - Copyright © 1998 Brendan V. Delumpa
 * http://www.delphicorner.f9.co.uk/articles/comps11.htm
 *
 ****************************************************************************)

unit EnhCombo;

interface

uses Windows, Messages, Classes, Controls, TntStdCtrls, Forms;

const
  CB_SETMINVISIBLE = $1701;

type
  TEnhCombo = class(TTntComboBox)
  private
    FItemWidth : Integer;
    FDropDownFixedWidth: Integer;
    FOnCloseUp: TNotifyEvent;
    FModified: Boolean;
    //FSettingFocus: Boolean;
    FComCtlVersion6: Boolean;
    procedure SetDropDownFixedWidth(const Value: Integer);
    //function GetTextWidth(S : String) : Integer;
    function GetListHandle: HWND;
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    function GetDropDownCount: Integer;
    procedure SetDropDownCount(Value: Integer);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure DropDown; override;
    procedure CloseUp;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Change; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
        ComboProc: Pointer); override;
  public
    constructor Create(AOwner : TComponent); override;
    property ItemWidth : Integer read FItemWidth write FItemWidth;
    property ListHandle: HWND read GetListHandle;
  published
    property DropDownFixedWidth : Integer read FDropDownFixedWidth
                                          write SetDropDownFixedWidth;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property Modified: Boolean read GetModified write SetModified;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount;
  end;

procedure Register;

implementation

function CheckDLLVersion(const DLLName: string; MajorHi, MajorLo,
   MinorHi, MinorLo: word): boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  V1, V2, V3, V4: word;
begin
  Result := FALSE;
  VerInfoSize := GetFileVersionInfoSize(PChar(DLLName), Dummy);
  if VerInfoSize = 0 then
    exit;
  GetMem(VerInfo, VerInfoSize);
  if not assigned(VerInfo) then
    exit;
  try
    if GetFileVersionInfo(PChar(DLLName), 0, VerInfoSize, VerInfo) then
    begin
      if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
      begin
        with VerValue^ do
        begin
          V1 := dwFileVersionMS shr 16;
          V2 := dwFileVersionMS and $FFFF;
          V3 := dwFileVersionLS shr 16;
          V4 := dwFileVersionLS and $FFFF;
        end;
        { This would be SO much easier with D4's int64 type... }
        if V1 < MajorHi then
          Result := FALSE
        else if V1 > MajorHi then
          Result := TRUE
        else begin
          if V2 < MajorLo then
            Result := FALSE
          else if V2 > MajorLo then
            Result := TRUE
          else begin
            if V3 < MinorHi then
              Result := FALSE
            else if V3 > MinorHi then
              Result := TRUE
            else begin
              if V4 < MinorLo then
                Result := FALSE
              else if V4 > MinorLo then
                Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(VerInfo, VerInfoSize);
  end;
end;

function CheckComCtlVersion(MajorHi, MajorLo, MinorHi, MinorLo: word): boolean;
begin
  Result := CheckDLLVersion('COMCTL32.DLL', MajorHi, MajorLo, MinorHi, MinorLo);
end;

constructor TEnhCombo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FDropDownFixedWidth := 0;
  FComCtlVersion6 := CheckComCtlVersion(6, 0, 0, 0);
  //FSettingFocus := False;
end;

procedure TEnhCombo.DropDown;
var
  I, TextWidth: Integer;
  CXVScroll: Integer;
begin
  inherited DropDown;
  ItemWidth := 0;
  {Check to see if DropDownFixed Width > 0. Then just set the
   width of the list box. Otherwise, loop through the items
   and set the width of the list box to 8 pixels > than the
   widest string to buffer the right side. Anything less than
   8 for some reason touches the end of the item on high-res
   monitor settings.}
  if (FDropDownFixedWidth > 0) then
    Self.Perform(CB_SETDROPPEDWIDTH, FDropDownFixedWidth, 0)
  else begin
    CXVScroll := GetSystemMetrics(SM_CXVSCROLL);
    Canvas.Handle := GetDC(Handle);
    try
      SelectObject(Canvas.Handle, Font.Handle);
      for I := 0 to Items.Count - 1 do begin
        TextWidth := Canvas.TextWidth(Items[I]);
        Inc(TextWidth, CXVScroll + 8);
        if (TextWidth > ItemWidth) then
          ItemWidth := TextWidth;
      end;
      Self.Perform(CB_SETDROPPEDWIDTH, ItemWidth, 0);
    finally
      ReleaseDC(Handle, Canvas.Handle);
    end;
  end;
end;

{function TEnhCombo.GetTextWidth(S : String) : Integer;
begin
  Result := TForm(Owner).Canvas.TextWidth(S);
end;}

procedure TEnhCombo.SetDropDownFixedWidth(const Value: Integer);
begin
  FDropDownFixedWidth := Value;
end;

function TEnhCombo.GetListHandle: HWND;
var
  Info: TComboBoxInfo;
begin
  HandleNeeded;
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  if GetComboBoxInfo(Handle, Info) then
    Result := Info.hwndList
  else
    Result := 0;
end;

procedure TEnhCombo.CloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TEnhCombo.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_CLOSEUP:
      CloseUp;
  end;
  inherited;
end;

function TEnhCombo.GetModified: Boolean;
begin
  Result := FModified;
  if HandleAllocated and (EditHandle <> 0) then
    Result := Result or (SendMessage(EditHandle, EM_GETMODIFY, 0, 0) <> 0);
end;

procedure TEnhCombo.SetModified(Value: Boolean);
begin
  if HandleAllocated and (EditHandle <> 0) then
    SendMessage(EditHandle, EM_SETMODIFY, Byte(Value), 0);
  FModified := Value;
end;

function TEnhCombo.GetDropDownCount: Integer;
begin
  Result := inherited DropDownCount;
end;

procedure TEnhCombo.SetDropDownCount(Value: Integer);
begin
  if FComCtlVersion6 and (Value <> DropDownCount) then begin
    HandleNeeded;
    SendMessage(Handle, CB_SETMINVISIBLE, Value, 0);
  end;
  inherited DropDownCount := Value;
end;

procedure TEnhCombo.CreateWnd;
begin
  inherited;
  Modified := FModified;
  if FComCtlVersion6 then
    SendMessage(Handle, CB_SETMINVISIBLE, DropDownCount, 0);
end;

procedure TEnhCombo.DestroyWnd;
begin
  FModified := Modified;
  inherited;
end;

procedure TEnhCombo.Change;
begin
  Modified := True;
  inherited;
end;

procedure TEnhCombo.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
    ComboProc: Pointer);
begin
  with Message do begin
    case Msg of
      EM_SETSEL: begin
//        if FSettingFocus then
//          Exit;
        // workaround for a combo box bug: when resizing the combo box, all text
        // gets selected
        // see https://forums.codegear.com/thread.jspa?threadID=12375
        if not Focused and (WParam = 0)
            and ((LParam = MaxInt) or (LParam = Length(Text))) then
          Exit;
      end;
//      WM_SETFOCUS, WM_KILLFOCUS: begin
//        if ComboWnd = EditHandle then begin
//          FSettingFocus := True;
//          try
//            inherited;
//          finally
//            FSettingFocus := False;
//          end;
//          Exit;
//        end;
//      end;
    end;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('BD', [TEnhCombo]);
end;

end.
