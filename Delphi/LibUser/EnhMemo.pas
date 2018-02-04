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
 * Enhanced TTntMemo VCL component
 *
 ****************************************************************************)

unit EnhMemo;

interface

uses Windows, Messages, Classes, Controls, TntStdCtrls;

type
  TEnhMemo = class(TTntMemo)
  protected
    procedure SetHeightInLines(Lines: Cardinal);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
        message CM_WANTSPECIALKEY;
  public
    property HeightInLines: Cardinal write SetHeightInLines;
  end;

procedure Register;

implementation

uses CtrlsCommon;

procedure TEnhMemo.SetHeightInLines(Lines: Cardinal);
begin
  SetMemoHeightInLines(Self, Lines);
end;

procedure TEnhMemo.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TEnhMemo.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if (Message.CharCode = VK_RETURN) and WantReturns then
    Message.Result := 1;
end;

procedure Register;
begin
  RegisterComponents('tdebaets', [TEnhMemo]);
end;

end.
