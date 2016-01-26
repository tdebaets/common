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
 * PSAPI and Tool Help API declarations
 *
 ****************************************************************************)

unit ProcFunc;

interface

uses Windows, TlHelp32;

type
  CreateToolhelp32SnapshotType = function(dwFlags, th32ProcessID: DWORD): THandle; stdcall;
  Process32FirstType = function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
  Process32NextType = function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
  Module32FirstType = function(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
  Module32NextType = function(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
  Thread32FirstType = function(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall;
  Thread32NextType = function(hSnapshot: THandle; var lpte: TThreadENtry32): BOOL; stdcall;


  PHInst = ^HInst;
  TEnumProcesses = function(pidList: PInteger; cb: Integer;
      var cbNeeded: Integer): Boolean; stdcall;
  TEnumProcessModules = function(hProcess: THandle; moduleList: PHInst;
      cb: Integer; var cbNeeded: Integer): Boolean; stdcall;
  TGetModuleFileNameExA = function(hProcess: THandle; module: HInst;
      FileName: PChar; size: Integer): Integer; stdcall;

  TGetProcessImageFileName = function(hProcess: THandle; lpImageFileName: PChar;
      nSize: DWORD): DWORD; stdcall;

var
  xCreateToolhelp32Snapshot: CreateToolhelp32SnapshotType = nil;
  xProcess32First: Process32FirstType = nil;
  xProcess32Next: Process32NextType = nil;
  xModule32First: Module32FirstType = nil;
  xModule32Next: Module32NextType = nil;
  xThread32First: Thread32FirstType = nil;
  xThread32Next: Thread32NextType = nil;

  EnumProcesses: TEnumProcesses = nil;
  EnumProcessModules: TEnumProcessModules = nil;
  GetModuleFileNameExA: TGetModuleFileNameExA = nil;
  //GetProcessImageFileName : TGetProcessImageFileName = nil;

function LoadPsApiFunc: Boolean;
function LoadTlHelpFunc: Boolean;

implementation

var
  TLHelpLoaded: Boolean = False;
  PsApiLoaded: Boolean = False;
  PsApiLib: hModule = 0;

function LoadPsApiFunc: Boolean;
begin
  Result := False;
  if not PsApiLoaded then begin
    PsApiLoaded := True;
    PsApiLib := LoadLibrary('psapi.dll');
    if PsApiLib = 0 then
      Exit
    else begin
      @EnumProcessModules := GetProcAddress(PsApiLib, 'EnumProcessModules');
      @EnumProcesses := GetProcAddress(PsApiLib, 'EnumProcesses');
      @GetModuleFileNameExA := GetProcAddress(PsApiLib, 'GetModuleFileNameExA');
      {@GetProcessImageFileName := GetProcAddress(PsApiLib,
          'GetProcessImageFileNameA');}
    end;
  end;
  // GetProcessImageFileName is XP/2003+ only, so don't check for it here
  if not ( Assigned(EnumProcessModules) and Assigned(GetModuleFileNameExA) and
      Assigned(EnumProcesses) ) then
    Exit;
  Result := True;
end;

procedure UnloadPsApi;
begin
  PsApiLoaded := False;
  if PsApiLib <> 0 then begin
    FreeLibrary(PsApiLib);
    PsApiLib := 0;
  end;
end;

function LoadTlHelpFunc: Boolean;
var
  hKernel: THandle;
begin
  Result := False;
  if not TLHelpLoaded then begin
    TlHelpLoaded := True;
    hKernel := GetModuleHandle(kernel32);
    if hKernel <> 0 then begin
      @xCreateToolhelp32Snapshot := GetProcAddress(hKernel, 'CreateToolhelp32Snapshot');
      @xProcess32First := GetProcAddress(hKernel, 'Process32First');
      @xProcess32Next := GetProcAddress(hKernel, 'Process32Next');
      @xModule32First := GetProcAddress(hKernel, 'Module32First');
      @xModule32Next := GetProcAddress(hKernel, 'Module32Next');
      @xThread32First := GetProcAddress(hKernel, 'Thread32First');
      @xThread32Next := GetProcAddress(hKernel, 'Thread32Next');
    end;
  end;
  if not ( Assigned(xCreateToolhelp32Snapshot) and
      Assigned(xProcess32First) and Assigned(xProcess32Next) and
      Assigned(xModule32First) and Assigned(xModule32Next) and
      Assigned(xThread32First) and Assigned(xThread32Next) ) then
    Exit;
  Result := True;
end;

initialization

finalization
  UnloadPsApi;

end.
