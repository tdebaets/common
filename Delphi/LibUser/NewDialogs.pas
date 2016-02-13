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
 * Message dialog box functions
 *
 ****************************************************************************)

unit NewDialogs;

interface

uses Controls, PJMessageDialog;

{ PJMessageDialog forward declarations, so that units that are only importing
  this unit keep compiling. }

type
  TPJMsgDlgIconKind = PJMessageDialog.TPJMsgDlgIconKind;
const
  miWarning   = PJMessageDialog.miWarning;
  miInfo      = PJMessageDialog.miInfo;
  miQuestion  = PJMessageDialog.miQuestion;
  miError     = PJMessageDialog.miError;
  miUser      = PJMessageDialog.miUser;
  miNone      = PJMessageDialog.miNone;

type
  TPJMsgDlgButtonGroup = PJMessageDialog.TPJMsgDlgButtonGroup;
const
  bgAbortRetryIgnore  = PJMessageDialog.bgAbortRetryIgnore;
  bgOK                = PJMessageDialog.bgOK;
  bgOKCancel          = PJMessageDialog.bgOKCancel;
  bgRetryCancel       = PJMessageDialog.bgRetryCancel;
  bgYesNo             = PJMessageDialog.bgYesNo;
  bgYesNoCancel       = PJMessageDialog.bgYesNoCancel;

type
  TPJMsgDlgDefButton = PJMessageDialog.TPJMsgDlgDefButton;
const
  dbDefButton1  = PJMessageDialog.dbDefButton1;
  dbDefButton2  = PJMessageDialog.dbDefButton2;
  dbDefButton3  = PJMessageDialog.dbDefButton3;
  dbDefButton4  = PJMessageDialog.dbDefButton4;

{ New functions }

function MsgBox(Prompt: String; bgButtons: TPJMsgDlgButtonGroup;
    miStyle: TPJMsgDlgIconKind; Caption: TCaption; Context: Integer): Integer;
procedure SimpleMsg(Prompt: String; Caption: TCaption);

implementation

function MsgBox(Prompt: String; bgButtons: TPJMsgDlgButtonGroup;
    miStyle: TPJMsgDlgIconKind; Caption: TCaption; Context: Integer): Integer;
begin
  with TPJMessageDialog.Create(nil) do begin
    MakeSound := False;
    Text := Prompt;
    ButtonGroup := bgButtons;
    IconKind := miStyle;
    Title := Caption;
    HelpContext := Context;
    Result := Execute;
    Free;
  end;
end;

procedure SimpleMsg(Prompt: String; Caption: TCaption);
begin
  with TPJMessageDialog.Create(nil) do begin
    MakeSound := False;
    Text := Prompt;
    ButtonGroup := bgOK;
    IconKind := miNone;
    Title := Caption;
    Execute;
    Free;
  end;
end;

end.
