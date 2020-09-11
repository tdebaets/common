(****************************************************************************
 *
 * Copyright 2020 Tim De Baets
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
 * Windows 10 High DPI API declarations
 *
 ****************************************************************************)

unit HighDPIFunc;

interface

uses Windows;

type
  DPI_AWARENESS_CONTEXT = type THandle;
const
  DPI_AWARENESS_CONTEXT_UNAWARE              = DPI_AWARENESS_CONTEXT(-1);
  DPI_AWARENESS_CONTEXT_SYSTEM_AWARE         = DPI_AWARENESS_CONTEXT(-2);
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE    = DPI_AWARENESS_CONTEXT(-3);
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 = DPI_AWARENESS_CONTEXT(-4);

var
  SetThreadDpiAwarenessContext: function(dpiContext: DPI_AWARENESS_CONTEXT): DPI_AWARENESS_CONTEXT; stdcall;

function LoadHighDPIFunc: Boolean;

implementation

var
  HighDPILoaded: Boolean = False;

function LoadHighDPIFunc: Boolean;
var
  hUser32: HMODULE;
begin
  if not HighDPILoaded then begin
    HighDPILoaded := True;
    hUser32 := GetModuleHandle(user32);
    if hUser32 <> 0 then
      @SetThreadDpiAwarenessContext := GetProcAddress(hUser32,
          'SetThreadDpiAwarenessContext');
  end;
  Result := Assigned(SetThreadDpiAwarenessContext);
end;

end.
