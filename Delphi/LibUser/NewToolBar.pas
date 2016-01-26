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
 * TNewToolBar VCL component
 *
 ****************************************************************************)

unit NewToolBar;

interface

uses Windows, SysUtils, Controls, Classes, Messages, ComCtrls, CommCtrl,
    NewCommCtrl;

type
  TNewToolBar = class(TToolBar)
  protected
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure Register;

implementation

procedure TNewToolBar.CreateParams(var Params: TCreateParams);
begin
  //Params.Style := Params.Style or CCS_VERT;
  Params.Style := Params.Style or CCS_LEFT or CCS_NOPARENTALIGN;
  inherited;
end;

procedure TNewToolBar.CNNotify(var Message: TWMNotify);
var
  i: Integer;
begin
  inherited;
  with Message do begin
    if NMHdr^.code <> NM_CUSTOMDRAW then
      Exit;
    with PNMCustomDraw(NMHdr)^ do begin
      if dwDrawStage = CDDS_PREPAINT then
        Result := CDRF_DODEFAULT or CDRF_NOTIFYPOSTPAINT
      else if dwDrawStage = CDDS_POSTPAINT then begin
        for i := 0 to ButtonCount - 1 do begin
          if Buttons[i].Style = tbsSeparator then
            FillRect(hDC, Buttons[i].BoundsRect, CreateSolidBrush(Color));
        end;
        Result := CDRF_DODEFAULT;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('BM', [TNewToolBar]);
end;

end.
