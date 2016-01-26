unit Wow64;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Wow64 helper functions, taken from Inno Setup's RedirFunc.pas and Main.pas
}

interface

uses Windows, CmnFunc2;

type
  TPreviousFsRedirectionState = record
    DidDisable: Boolean;
    OldValue: Pointer;
  end;

var
  IsWin64: Boolean = False;

function DisableFsRedirectionIf(const Disable: Boolean;
    var PreviousState: TPreviousFsRedirectionState): Boolean;
procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);
function Wow64Process(hProcess: THandle): Boolean;

function NewFileExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;

implementation

var
  Wow64DisableWow64FsRedirectionFunc: function(var OldValue: Pointer): BOOL; stdcall = nil;
  Wow64RevertWow64FsRedirectionFunc: function(OldValue: Pointer): BOOL; stdcall;
  IsWow64ProcessFunc: function(hProcess: THandle;
      var Wow64Process: BOOL): BOOL; stdcall = nil;
  FsRedirectionFunctionsAvailable: Boolean = False;

function Wow64Process(hProcess: THandle): Boolean;
var
  Wow64Process: BOOL;
begin
  Result := Assigned(IsWow64ProcessFunc) and
      IsWow64ProcessFunc(hProcess, Wow64Process) and
      Wow64Process
end;

procedure InitIsWin64;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: TSystemInfo); stdcall;
begin
  { The system is considered a "Win64" system if all of the following
    conditions are true:
    1. GetNativeSystemInfo is available.
    2. IsWow64Process is available, and returns True for the current process.
    3. Wow64DisableWow64FsRedirection is available.
    4. Wow64EnableWow64FsRedirection is available.
    The system does not have to be one of the known 64-bit architectures
    (AMD64, IA64) to be considered a "Win64" system. }

  IsWin64 := False;
  KernelModule := GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc := GetProcAddress(KernelModule, 'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    IsWow64ProcessFunc := GetProcAddress(KernelModule, 'IsWow64Process');
    if Wow64Process(GetCurrentProcess) then begin
      if FsRedirectionFunctionsAvailable then
        IsWin64 := True;
    end;
  end;
end;

function DisableFsRedirectionIf(const Disable: Boolean;
    var PreviousState: TPreviousFsRedirectionState): Boolean;
{ If Disable is False, the function does not change the redirection state and
  always returns True.
  If Disable is True, the function attempts to disable WOW64 file system
  redirection, so that c:\windows\system32 goes to the 64-bit System directory
  instead of the 32-bit one.
  Returns True if successful, False if not (which normally indicates that
  either the user is running 32-bit Windows, or a 64-bit version prior to
  Windows Server 2003 SP1). For extended error information when False is
  returned, call GetLastError. }
begin
  PreviousState.DidDisable := False;
  if not Disable then
    Result := True
  else begin
    if FsRedirectionFunctionsAvailable then begin
      { Note: Disassembling Wow64DisableWow64FsRedirection and the Rtl function
        it calls, it doesn't appear as if it can ever actually fail on 64-bit
        Windows. But it always fails on the 32-bit version of Windows Server
        2003 SP1 (with error code 1 - ERROR_INVALID_FUNCTION). }
      Result := Wow64DisableWow64FsRedirectionFunc(PreviousState.OldValue);
      if Result then
        PreviousState.DidDisable := True;
    end
    else begin
      { The functions do not exist prior to Windows Server 2003 SP1 }
      SetLastError(ERROR_INVALID_FUNCTION);
      Result := False;
    end;
  end;
end;

procedure RestoreFsRedirection(const PreviousState: TPreviousFsRedirectionState);
{ Restores the previous WOW64 file system redirection state after a call to
  DisableFsRedirectionIf. There is no indication of failure (which is
  extremely unlikely). }
begin
  if PreviousState.DidDisable then
    Wow64RevertWow64FsRedirectionFunc(PreviousState.OldValue);
end;

function NewFileExistsRedir(const DisableFsRedir: Boolean; const Filename: String): Boolean;
var
  PrevState: TPreviousFsRedirectionState;
  ErrorCode: DWORD;
begin
  if not DisableFsRedirectionIf(DisableFsRedir, PrevState) then begin
    Result := False;
    Exit;
  end;
  try
    Result := NewFileExists(Filename);
    ErrorCode := GetLastError;
  finally
    RestoreFsRedirection(PrevState);
  end;
  SetLastError(ErrorCode);
end;

initialization
  Wow64DisableWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64DisableWow64FsRedirection');
  Wow64RevertWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64RevertWow64FsRedirection');
  FsRedirectionFunctionsAvailable := Assigned(Wow64DisableWow64FsRedirectionFunc) and
    Assigned(Wow64RevertWow64FsRedirectionFunc);
  InitIsWin64;

end.
