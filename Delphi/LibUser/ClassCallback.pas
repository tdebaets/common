(****************************************************************************
 *
 * Copyright 2016-2022 Tim De Baets
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
 * Helper functions to use class methods as callbacks
 *
 ****************************************************************************)

unit ClassCallback;

interface

uses Windows, SysUtils, SyncObjs, Common2;

function CreateStub(Obj: TObject; MethodPtr: Pointer;
    ThisCall: Boolean; pData: PPointer = nil): Pointer; overload;
function CreateStub(Obj: TObject; MethodPtr: Pointer;
    pData: PPointer = nil): Pointer; overload;
function CreateNoFreeStub(Obj: TObject; MethodPtr: Pointer;
    ThisCall: Boolean; pData: PPointer = nil): Pointer;
procedure DisposeStub(pStub: Pointer);
procedure DisposeAndNilStub(var pStub: Pointer);
procedure RestoreNoFreeStub(pStub: Pointer; pOrigProc: Pointer);
function GetStubMethodName(pStub: Pointer): ShortString;

implementation

{$DEFINE VIRTUALMEMSTUB}

{ ----------------------------------------------------------------------------- }
{ This is a piece of magic by Jeroen Mineur.  Allows a class method to be used  }
{ as a callback. Create a stub using CreateStub with the instance of the object }
{ the callback should call as the first parameter and the method as the second  }
{ parameter, ie @TForm1.MyCallback or declare a type of object for the callback }
{ method and then use a variable of that type and set the variable to the       }
{ method and pass it:                                                           }
{                                                                               }
{ DON'T FORGET TO DEFINE THE FUNCTION METHOD AS "STDCALL" FOR A WINDOWS         }
{  CALLBACK.  ALL KINDS OF WEIRD THINGS CAN HAPPEN IF YOU DON'T                 }
{                                                                               }
{ type                                                                          }
{   TEnumWindowsFunc = function (AHandle: hWnd; Param: lParam): BOOL of object; stdcall; }
{                                                                               }
{  TForm1 = class(TForm)                                                        }
{  private                                                                      }
{    function EnumWindowsProc(AHandle: hWnd; Param: lParam): BOOL; stdcall;     }
{  end;                                                                         }
{                                                                               }
{  var                                                                          }
{    MyFunc: TEnumWindowsFunc;                                                  }
{    Stub: pointer;                                                             }
{  begin                                                                        }
{    MyFunct := EnumWindowsProc;                                                }
{    Stub := CreateStub(Self, MyFunct);                                         }
{     ....                                                                      }
{  or                                                                           }
{                                                                               }
{  var                                                                          }
{    Stub: pointer;                                                             }
{  begin                                                                        }
{    Stub := CreateStub(Self, @TForm1.EnumWindowsProc);                         }
{     ....                                                                      }
{  Now Stub can be passed as the callback pointer to any windows API            }
{  Don't forget to call Dispose Stub when not needed                            }
{ ----------------------------------------------------------------------------- }

const
  AsmPopEDX = $5A;
  AsmMovEAX = $B8;
  AsmPushEAX = $50;
  AsmPushECX = $51;
  AsmPushEDX = $52;
  AsmJmpShort = $E9;
  AsmNop = $90;

type
  TStub = packed record
    PopEDX: Byte;
    MovEAX: Byte;
    DataPointer: Pointer;
    PushEAX: Byte;
    PushECX: Byte;
    MovEAX2: Byte;
    SelfPointer: Pointer;
    PushEAX2: Byte;
    PushEDX: Byte;
    JmpShort: Byte;
    Displacement: Integer;
    MethodName: ShortString;
  end;
  PStub = ^TStub;

var
  hCodeHeap: THandle = 0;
  hNoFreeCodeHeap: THandle = 0;
  HeapLock: TCriticalSection = nil;

procedure AssembleStub(pStub: PStub; Obj: TObject; MethodPtr: Pointer;
    ThisCall: Boolean; pData: PPointer);
begin
  if Assigned(Obj) then begin
    // Pop the return address off the stack
    pStub.PopEDX := AsmPopEDX;

    if Assigned(pData) then begin
      // Push the data pointer on the stack
      pStub.MovEAX := AsmMovEAX;
      pStub.DataPointer := nil;
      pStub.PushEAX := AsmPushEAX;
      pData^ := @pStub.DataPointer;
    end
    else begin
      pStub.MovEAX := AsmNop;
      FillChar(pStub.DataPointer, SizeOf(pStub.DataPointer), AsmNop);
      pStub.PushEAX := AsmNop;
    end;

    if ThisCall then begin
      // Push the this pointer (ecx) on the stack
      pStub.PushECX := AsmPushECX
    end
    else
      pStub.PushECX := AsmNop;

    // Push the object pointer on the stack
    pStub.MovEAX2 := AsmMovEAX;
    pStub.SelfPointer := Obj;
    pStub.PushEAX2 := AsmPushEAX;
    // Push the return address back on the stack
    pStub.PushEDX := AsmPushEDX;

    // Note: this will be an empty string if MethodPtr doesn't point to a
    // *published* method.
    pStub.MethodName := Obj.MethodName(MethodPtr);
  end
  else
    FillChar(pStub^, SizeOf(pStub^), AsmNop);
    
  // Jump to the 'real' procedure, the method or the original function.
  pStub.JmpShort := AsmJmpShort;
  pStub.Displacement := (Integer(MethodPtr) - Integer(@(pStub.JmpShort))) -
      (SizeOf(pStub.JmpShort) + SizeOf(pStub.Displacement));
end;

{ ----------------------------------------------------------------------------- }
function CreateStubInternal(Obj: TObject; MethodPtr: Pointer;
    ThisCall, NoFree: Boolean; pData: PPointer): Pointer;
var
  phHeap: PHandle;
  Stub: PStub;
begin
  // Allocate memory for the stub
  // 1/10/04 Support for 64 bit, executable code must be in virtual space
  // currently New/Dispose use Virtual space but a replacement memory manager
  // may not
  {$IFDEF VIRTUALMEMSTUB}
  // VirtualAlloc allocates a 4KB page on each call, so using a private heap is
  // much tidier
  //Stub:=VirtualAlloc(nil, SizeOf(TStub), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if NoFree then
    phHeap := @hNoFreeCodeHeap
  else
    phHeap := @hCodeHeap;
  HeapLock.Enter;
  try
    if phHeap^ = 0 then begin
      phHeap^ := HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, 0);
      if phHeap^ = 0 then
        RaiseLastWin32Error;
    end;
  finally
    HeapLock.Release;
  end;
  Stub := HeapAlloc(phHeap^, 0, SizeOf(TStub));
  if not Assigned(Stub) then
    RaiseLastWin32Error;
  {$ELSE}
  New(Stub);
  {$ENDIF}
  AssembleStub(Stub, Obj, MethodPtr, ThisCall, pData);
  // Return a pointer to the stub
  Result := Stub;
end;

function CreateStub(Obj: TObject; MethodPtr: Pointer;
    ThisCall: Boolean; pData: PPointer = nil): Pointer;
begin
  Result := CreateStubInternal(Obj, MethodPtr, ThisCall, False, pData);
end;

function CreateStub(Obj: TObject; MethodPtr: Pointer;
    pData: PPointer = nil): Pointer;
begin
  Result := CreateStubInternal(Obj, MethodPtr, False, False, pData);
end;

function CreateNoFreeStub(Obj: TObject; MethodPtr: Pointer;
    ThisCall: Boolean; pData: PPointer = nil): Pointer;
begin
  Result := CreateStubInternal(Obj, MethodPtr, ThisCall, True, pData);
end;

procedure RestoreNoFreeStub(pStub: Pointer; pOrigProc: Pointer);
begin
  AssembleStub(pStub, nil, pOrigProc, False, nil);
end;

{ ----------------------------------------------------------------------------- }


{ ----------------------------------------------------------------------------- }
procedure DisposeStubInternal(Stub: PStub);
begin
  Stub.MethodName := '';
  // 1/10/04 Support for 64 bit, executable code must be in virtual space
  // currently New/Dispose use Virtual space but a replacement memory manager
  // may not
  {$IFDEF VIRTUALMEMSTUB}
  //VirtualFree(Stub, SizeOf(TStub), MEM_DECOMMIT);
  HeapFree(hCodeHeap, 0, Stub);
  {$ELSE}
  Dispose(Stub);
  {$ENDIF}
end;

procedure DisposeStub(pStub: Pointer);
begin
  DisposeStubInternal(pStub);
end;

procedure DisposeAndNilStub(var pStub: Pointer);
begin
  DisposeStubInternal(pStub);
  pStub := nil;
end;

function GetStubMethodNameInternal(Stub: PStub): ShortString;
begin
  Result := Stub.MethodName;
end;

function GetStubMethodName(pStub: Pointer): ShortString;
begin
  Result := GetStubMethodNameInternal(pStub);
end;

initialization
  HeapLock := TCriticalSection.Create;

finalization
  HeapLock.Enter;
  try
    if hCodeHeap <> 0 then
      HeapDestroy(hCodeHeap);
    // don't free the no-free heap
  finally
    HeapLock.Release;
  end;
  FreeAndNil(HeapLock);

end.
