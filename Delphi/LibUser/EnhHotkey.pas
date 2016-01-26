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
 * TEnhHotkey VCL component
 *
 ****************************************************************************)

unit EnhHotkey;

interface

uses Windows, Messages, Classes, Controls, ComCtrls, Menus, Common2;

type
  TEnhHotkey = class(THotkey)
  private
    FOnChange: TNotifyEvent;
    FCreating: Boolean;
    FCatchAllKeys: Boolean;
    function DoKeyDown(var Message: TWMKey): Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKey); message WM_SYSKEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Change; dynamic;
    procedure CreateWnd; override;
  published
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property CatchAllKeys: Boolean read FCatchAllKeys write FCatchAllKeys;
  end;

procedure Register;

implementation

procedure TEnhHotkey.CreateWnd;
begin
  FCreating := True;
  try
    inherited CreateWnd;
  finally
    FCreating := False;
  end;
end;

procedure TEnhHotkey.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEnhHotkey.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) and not FCreating then
    Change;
end;

function TEnhHotkey.DoKeyDown(var Message: TWMKey): Boolean;
var
  Key: Word;
  Shift: TShiftState;
begin
  case Message.CharCode of
    VK_RETURN,
    VK_TAB,
    VK_SPACE,
    VK_DELETE,
    VK_ESCAPE,
    VK_BACK: begin
      Result := True;
      ShortCutToKey(Hotkey, Key, Shift);
      Key := Message.CharCode;
      Hotkey := ShortCut(Key, Shift);
      if IsExtendedKey(Key) then
        Modifiers := Modifiers + [hkExt];
      Change; // need to fire OnChange ourselves
      inherited DoKeyDown(Message);
    end;
    else
      Result := False;
  end;
end;

procedure TEnhHotkey.WMKeyDown(var Message: TWMKey);
begin
  if not DoKeyDown(Message) then
    inherited;
end;

procedure TEnhHotkey.WMSysKeyDown(var Message: TWMKey);
begin
  if not DoKeyDown(Message) then
    inherited;
end;

procedure TEnhHotkey.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FCatchAllKeys then begin
    Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS
        or DLGC_WANTTAB;
  end;
end;

procedure Register;
begin
  RegisterComponents('BM', [TEnhHotkey]);
end;

end.
