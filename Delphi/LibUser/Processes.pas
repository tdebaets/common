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
 * Process-related utility code
 *
 ****************************************************************************)

unit Processes;

interface

uses Windows, SysUtils, TlHelp32, ProcFunc, Common2, PathFunc, CmnFunc2;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

type
  TEnumProcessProc = function(ProcID: Integer; const Filename: String;
      Data: Pointer): Boolean;

function EnumProcessesAll(EnumProc: TEnumProcessProc; Data: Pointer): Integer;
function EnumProcessesNT(EnumProc: TEnumProcessProc; Data: Pointer): Integer;

function NewProcessFilenameFuncAvailable: Boolean;
function GetProcessFilename(ProcID, ProcHandle: Integer;
    var Filename: string): Integer;
function GetProcessCreationTime(ProcessHandle: Integer;
    var CreationTime: TFileTime): Boolean;

function EnableProcessPrivilege(Privilege: PChar): Integer;
function EnableDebugPrivilege: Integer;
function EnableSecurityPrivilege: Integer;

function ProcessIDToStr(ProcID: Integer): String;

function LoadWinSta: Boolean;
procedure UnloadWinSta;
function GetProcessSid(ProcessId, ProcessHandle: Integer;
    var Sid: PSID): Boolean;
function GetUserAndDomainFromSid(Sid: PSID; var User, Domain: string): Boolean;
procedure CheckSid(Sid: PSID; var System, CurrentUser: Boolean);
  
implementation

uses NtdllApi, Wow64, Int64Em;

function EnumProcessesAll(EnumProc: TEnumProcessProc; Data: Pointer): Integer;
var
  PI32: TProcessentry32;
  hSnap: THandle;
begin
  if IsWinNT and (Win32MajorVersion < 5) then // NT only
    Result := EnumProcessesNT(EnumProc, Data)
  else begin  // 9x and 2k or higher
    Result := 1;
    if not LoadTlHelpFunc then
      Exit;
    Result := 2;
    hSnap := xCreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnap = 0 then
      Exit;
    Result := 3;
    try
      PI32.dwSize := SizeOf(PI32);
      if xProcess32First(hSnap, PI32) then begin
        Result := 4;
        repeat
          if PI32.th32ProcessID <> 0 then begin
            if not EnumProc(PI32.th32ProcessID, PI32.szExeFile, Data) then begin
              Result := 0;
              Exit;
            end;
          end;
        until not xProcess32Next(hSnap, PI32);
        Result := -1;
      end;
    finally
      CloseHandle(hSnap);
    end;
  end;
end;

function EnumProcessesNT(EnumProc: TEnumProcessProc; Data: Pointer): Integer;
var
  PID, i, cbNeeded, PIDCount: Integer;
  PIDList: PInteger;
begin
  PIDList := nil;
  Result := 1;
  if not LoadPsApiFunc then
    Exit;
  Result := 2;
  ReallocMem(PIDList, 65536);
  try
    if not EnumProcesses(PIDList, 65536, cbNeeded) then
      Exit;
    ReallocMem(PIDList, cbNeeded);
    PIDCount := cbNeeded div SizeOf(Integer);
    // Walk the list
    for i := 0 to Pred(PIDCount) do begin
      // Get PID
      PID := PInteger(PChar(PIDList) + i * SizeOf(Integer))^;
      if PID <> 0 then begin
        if not EnumProc(PID, '', Data) then begin
          Result := 0;
          Exit;
        end;
      end;
    end;
    Result := -1;
  finally
    ReallocMem(PIDList,0);
  end;
end;

var
  NtQueryInformationProcess: TNtQueryInformationProcess = nil;
  NtWow64QueryInformationProcess64: TNtWow64QueryInformationProcess64 = nil;
  NtWow64ReadVirtualMemory64: TNtWow64ReadVirtualMemory64 = nil;
  NtdllFuncLoaded: Boolean = False;

function LoadNtdllFunc: Boolean;
var
  hLib: THandle;
begin
  Result := False;
  if not NtdllFuncLoaded then begin
    NtdllFuncLoaded := True;
    hLib := GetModuleHandle(ntdll);
    if hLib = 0 then
      Exit;
    @NtQueryInformationProcess := GetProcAddress(hLib,
        'NtQueryInformationProcess');
    if IsWin64 then begin
      @NtWow64QueryInformationProcess64 := GetProcAddress(hLib,
          'NtWow64QueryInformationProcess64');
      @NtWow64ReadVirtualMemory64 := GetProcAddress(hLib,
          'NtWow64ReadVirtualMemory64');
    end;
  end;
  Result := Assigned(NtQueryInformationProcess);
end;


type
  TQueryFullProcessImageName = function(hProcess: THandle; dwFlags: DWORD;
      lpExeName: PChar; var lpdwSize: DWORD): BOOL; stdcall;

const
  PROCESS_NAME_NATIVE = 1;

var
  QueryFullProcessImageName: TQueryFullProcessImageName = nil;
  QueryFullProcessImageNameLoaded: Boolean = False;

function LoadProcessFilenameFunc: Boolean;
begin
  Result := False;
  if not QueryFullProcessImageNameLoaded then begin
    @QueryFullProcessImageName := GetProcAddress(GetModuleHandle(kernel32),
        'QueryFullProcessImageNameA');
    QueryFullProcessImageNameLoaded := True;
  end;
  if not Assigned(QueryFullProcessImageName) then begin
    if not LoadPsApiFunc then
      Exit;
  end;
  Result := True;
end;

function NewProcessFilenameFuncAvailable: Boolean;
begin
  LoadProcessFilenameFunc;
  Result := Assigned(QueryFullProcessImageName)
      {or Assigned(GetProcessImageFileName)};
end;

type
  TProcessInfoType = (piFilename, piCommandLine, piWorkingDir);
  TProcessInfoTypes = set of TProcessInfoType;
  
  TProcessInfo = record
    Filename: String;
    CommandLine: String;
    WorkingDir: String;
  end;

function GetProcessInformationNT64(hProc: Integer; InfoTypes: TProcessInfoTypes;
    var ProcInfo: TProcessInfo): Integer;
var
  Info64: TProcessBasicInformation64;
  RealLength64: Integer64;
  Peb64: TPeb64;
  Params64: TRtlUserProcessParameters64;

  function ReadUnicodeString(const Str: TNtUnicodeString64): String;
  var
    Len: Integer;
    Buffer: PWideChar;
  begin
    Result := '';
    if (Str.Buffer.Lo = 0) and (Str.Buffer.Hi = 0) then
      Exit;
    Len := Str.Length;
    if Len = 0 then
      Exit;
    GetMem(Buffer, (Len + 1) * SizeOf(WideChar));
    try
      if not NT_SUCCESS(NtWow64ReadVirtualMemory64(hProc, Str.Buffer, Buffer,
          IntegerToInteger64(Len * SizeOf(WideChar)), RealLength64)) then
        Exit;
      Buffer[RealLength64.Lo div 2] := #0;
      Result := Buffer;
    finally
      FreeMem(Buffer);
    end;
  end;
  
begin
  Result := 20;
  if not LoadNtdllFunc then
    Exit;
  Result := 21;
  if not Assigned(NtWow64QueryInformationProcess64)
      or not Assigned(NtWow64ReadVirtualMemory64) then
    Exit;
  Result := 22;
  if not NT_SUCCESS(NtWow64QueryInformationProcess64(hProc,
      ProcessBasicInformation, Info64, SizeOf(Info64), RealLength64)) then
    Exit;
  Result := 23;
  if not NT_SUCCESS(NtWow64ReadVirtualMemory64(hProc, Info64.PebBaseAddress,
      @Peb64, IntegerToInteger64(SizeOf(Peb64)), RealLength64)) then
    Exit;
  Result := 24;
  if not NT_SUCCESS(NtWow64ReadVirtualMemory64(hProc, Peb64.ProcessParameters,
      @Params64, IntegerToInteger64(SizeOf(Params64)), RealLength64)) then
    Exit;
  if piFilename in InfoTypes then
    ProcInfo.Filename := ReadUnicodeString(Params64.ImagePathName);
  if piCommandLine in InfoTypes then
    ProcInfo.CommandLine := ReadUnicodeString(Params64.CommandLine);
  if piWorkingDir in InfoTypes then
    ProcInfo.WorkingDir := ReadUnicodeString(Params64.CurrentDirectory.DosPath);
  Result := 0;
end;

function GetProcessFilenameNT(hProc: Integer; var Filename: String): Integer;
const
  Prefix = '\??\';
  SystemRoot = '\systemroot\';
var
  szName: array [0..MAX_PATH] of Char;
  ProcInfo: TProcessInfo;
begin
  Filename := '';
  if not IsWin64 or Wow64Process(hProc) then begin
    if GetModuleFileNameExA(hProc, 0, szName, sizeof(szName)) <> 0 then begin
      Filename := StrPas(szName);
      Result := 0;
    end
    else
      Result := 10;
  end
  else begin
    Result := GetProcessInformationNT64(hProc, [piFilename], ProcInfo);
    if Result = 0 then begin
      Filename := ProcInfo.Filename;
      if Filename = '' then
        Result := 11
      else
        Result := 0;
    end;
  end;
  if Result = 0 then begin
    if Copy(Filename, 1, Length(Prefix)) = Prefix then
      Delete(Filename, 1, Length(Prefix));
    if AnsiCompareText(Copy(Filename, 1, Length(SystemRoot)),
        SystemRoot) = 0 then begin
      Delete(Filename, 1, Length(SystemRoot));
      Filename := AddBackslash(GetWinDir) + Filename;
    end;
  end;
end;

function GetProcessFilename(ProcID, ProcHandle: Integer;
    var Filename: string): Integer;
var
  Handle: THandle;
  NameLen: Cardinal;
  szName: array[0..MAX_PATH] of Char;
begin
  Result := 1;
  if not LoadProcessFilenameFunc then
    Exit;
  Result := 2;
  if ProcHandle = 0 then begin
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False,
        ProcID);
  end
  else
    Handle := ProcHandle;
  if Handle = 0 then
    Exit;
  Result := 3;
  try
    if Assigned(QueryFullProcessImageName) then begin
      NameLen := SizeOf(szName);
      if QueryFullProcessImageName(ProcHandle, 0, szName, NameLen) then begin
        Filename := StrPas(szName);
        Result := 0;
      end;
    end
    {else if Assigned(GetProcessImageFileName)
        and (GetProcessImageFileName(handle, szName, SizeOf(szName)) <> 0) then begin
      Filename := StrPas(szName);
      Result := 0;
    end}
    else
      Result := GetProcessFilenameNT(Handle, Filename); 
      //GetLongFilename(Filename); // uses too much CPU
  finally
    if ProcHandle = 0 then
      CloseHandle(Handle);
  end;
end;

function GetProcessCreationTime(ProcessHandle: Integer;
    var CreationTime: TFileTime): Boolean;
var
  ExitTime, KernelTime, UserTime: TFileTime;
begin
  Result := GetProcessTimes(ProcessHandle, CreationTime, ExitTime, KernelTime,
      UserTime);
end;

const
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';

function AdjustTokenPrivileges2(TokenHandle: THandle; DisableAllPrivileges: BOOL;
    const NewState: TTokenPrivileges; BufferLength: DWORD;
    PreviousState: Integer; var ReturnLength: DWORD): BOOL; stdcall;
    external advapi32 name 'AdjustTokenPrivileges';

function SetPrivilege(hToken: THandle; Privilege: PChar;
    bEnablePrivilege: BOOL): Integer;
var
  tp         : TTokenPrivileges;
  luid       : TLargeInteger;
  tpPrevious : TTokenPrivileges;
  cbPrevious : DWORD;
begin
  //Result := 0;
  //if not IsWinNT then Exit;
  cbPrevious := sizeof(TTokenPrivileges);
  Result := 03;
  if not LookupPrivilegeValue(nil, Privilege, luid) then
    Exit;
  //
  // first pass.  get current privilege setting
  //
  tp.PrivilegeCount           := 1;
  tp.Privileges[0].Luid       := luid;
  tp.Privileges[0].Attributes := 0;
  Result := 04;
  AdjustTokenPrivileges(hToken, FALSE, tp, sizeof(TTokenPrivileges), tpPrevious,
      cbPrevious);
  if GetLastError() <> ERROR_SUCCESS then
    Exit;
  //
  // second pass.  set privilege based on previous setting
  //
  tpPrevious.PrivilegeCount       := 1;
  tpPrevious.Privileges[0].Luid   := luid;
  with tpPrevious.Privileges[0] do begin
    if bEnablePrivilege then
      Attributes := Attributes or SE_PRIVILEGE_ENABLED
    else
      Attributes := Attributes and (not SE_PRIVILEGE_ENABLED);
  end;
  Result := 05;
  AdjustTokenPrivileges2(hToken, FALSE, tpPrevious, cbPrevious, 0, cbPrevious);
  if GetLastError() <> ERROR_SUCCESS then
    Exit;
  Result := 00;
end;

function EnableProcessPrivilege(Privilege: PChar): Integer;
var
  hToken: THandle;
begin
  Result := 1;
  if not OpenProcessToken(GetCurrentProcess(),
      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
    Exit;
  try
    Result := SetPrivilege(hToken, Privilege, True);
  finally
    CloseHandle(hToken);
  end;
end;

function EnableDebugPrivilege: Integer;
begin
  Result := EnableProcessPrivilege(SE_DEBUG_NAME);
end;

function EnableSecurityPrivilege: Integer;
begin
  Result := EnableProcessPrivilege(SE_SECURITY_NAME);
end;

function ProcessIDToStr(ProcID: Integer): String;
begin
  if IsWinNT then
    Result := IntToStr(ProcID)
  else
    Result := IntToHex(ProcID, 8);
end;

type
  TWinStationGetProcessSid = function(hServer: DWORD; dwPID: DWORD;
    ProcessStartTime: TFileTime; pProcessUserSid: PSID; var dwSidSize: DWORD):
    Boolean; stdcall;

var
  WinStaLoaded: Boolean = False;
  WinStaLib: hModule = 0;
  WinStationGetProcessSid: TWinStationGetProcessSid = nil;

function LoadWinSta: Boolean;
begin
  Result := False;
  if not WinStaLoaded then begin
    WinStaLoaded := True;
    WinStaLib := LoadLibrary('winsta.dll');
    if WinStaLib = 0 then
      Exit
    else
      @WinStationGetProcessSid := GetProcAddress(WinStaLib,
          'WinStationGetProcessSid');
  end;
  if not Assigned(WinStationGetProcessSid) then
    Exit;
  Result := True;
end;

procedure UnloadWinSta;
begin
  WinStaLoaded := False;
  if WinStaLib <> 0 then begin
    FreeLibrary(WinStaLib);
    WinStaLib := 0;
  end;
end;

function WinStaGetProcessSid(ProcessId, ProcessHandle: Integer;
  var Sid: PSID): Boolean;
var
  SidSize: DWORD;
  CreationTime: TFileTime;
begin
  Result := False;
  if not LoadWinsta then
    Exit;
  if not GetProcessCreationTime(ProcessHandle, CreationTime) then
    Exit;
  SidSize := 0;
  if WinStationGetProcessSid(0, ProcessId, CreationTime, Sid, SidSize)
    or (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
    Exit;
  GetMem(Sid, SidSize);
  if not WinStationGetProcessSid(0, ProcessId, CreationTime, Sid, SidSize) then
    FreeMem(Sid)
  else
    Result := True;
end;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
      (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_LOCAL_SYSTEM_RID = $00000012;
  SECURITY_LOCAL_SERVICE_RID = $00000013;
  SECURITY_NETWORK_SERVICE_RID = $00000014;

function GetProcessSid(ProcessId, ProcessHandle: Integer; var Sid: PSID): Boolean;
var
  hProc: Integer;
  SystemSid: PSID;
  SidLength: Integer;
begin
  Result := False;
  if ProcessHandle = 0 then
    hProc := OpenProcess(PROCESS_QUERY_INFORMATION, False, ProcessId)
  else
    hProc := ProcessHandle;
  if hProc <> 0 then try
    if WinStaGetProcessSid(ProcessId, hProc, Sid) then
      Result := True
    else if GetProcessSidToken(hProc, Sid) then
      Result := True
    else if (ProcessId = 4) or (ProcessId = 8) then begin
      // both functions can fail for System (procid 4 or 8), so create the SID ourselves
      if AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 1,
          SECURITY_LOCAL_SYSTEM_RID, 0, 0, 0, 0, 0, 0, 0, SystemSid) then try
        SidLength := GetLengthSid(SystemSid);
        GetMem(Sid, SidLength);
        if CopySid(SidLength, Sid, SystemSid) then
          Result := True
        else
          FreeMem(Sid);
      finally
        FreeSid(SystemSid);
      end;
    end;
  finally
    if ProcessHandle = 0 then
      CloseHandle(hProc);
  end;
end;

function GetUserAndDomainFromSid(Sid: PSID; var User, Domain: String): Boolean;
var
  snu: SID_NAME_USE;
  UserSize, DomainSize: DWORD;
begin
  Result := False;
  if Sid <> nil then begin
    UserSize := 0;
    DomainSize := 0;
    LookupAccountSid(nil, Sid, nil, UserSize, nil, DomainSize, snu);
    if (UserSize <> 0) and (DomainSize <> 0) then begin
      SetLength(User, UserSize);
      SetLength(Domain, DomainSize);
      if LookupAccountSid(nil, Sid, PChar(User), UserSize,
          PChar(Domain), DomainSize, snu) then begin
        Result := True;
        User := StrPas(PChar(User));
        Domain := StrPas(PChar(Domain));
      end;
    end;
  end;
end;

procedure CheckSid(Sid: PSID; var System, CurrentUser: Boolean);
  function EqualKnownSid(SubAuthority: Integer): Boolean;
  var
    KnownSid: PSID;
  begin
    Result := False;
    KnownSid := nil;
    if AllocateAndInitializeSid(SECURITY_NT_AUTHORITY,
        1, SubAuthority, 0, 0, 0, 0, 0, 0, 0, KnownSid) then try
      Result := EqualSid(Sid, KnownSid);
    finally
      FreeSid(KnownSid);
    end;
  end;
var
  OurSid: PSID;
begin
  System := EqualKnownSid(SECURITY_LOCAL_SYSTEM_RID)
      or EqualKnownSid(SECURITY_LOCAL_SERVICE_RID)
      or EqualKnownSid(SECURITY_NETWORK_SERVICE_RID);
  if System then
    CurrentUser := False
  else begin
    if GetProcessSidToken(GetCurrentProcess, OurSid) then try
      CurrentUser := EqualSid(Sid, OurSid);
    finally
      FreeMem(OurSid);
    end;
  end;
end;

end.
 