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
 * Helper to use the predefined Windows hand cursor
 *
 ****************************************************************************)

unit HandCursor;

interface

uses Windows, Forms;

const
  crHand = 90;

procedure LoadHandCursor;

implementation

var
  Loaded: Boolean = False;

procedure LoadHandCursor;
var
  Cur: HCURSOR;
begin
  if not Loaded then begin
    Loaded := True;
    Cur := LoadCursor(0, PChar(IDC_HAND));
    if Cur = 0 then
      Cur := LoadCursor(hInstance, 'HAND');
    Screen.Cursors[crHand] := Cur;
  end;
end;

initialization
  if not IsLibrary then
    LoadHandCursor;

end.
