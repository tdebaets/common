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
 * Ntdll API declarations
 *
 ****************************************************************************)

unit NtdllApi;

interface

uses Windows, Int64Em;

function NT_SUCCESS(Status: Integer): WordBool;

type
  Pointer64 = Integer64;
  THandle64 = Pointer64;
  PWideChar64 = Pointer64;

type
  NTSTATUS = Longint;

const
  STATUS_SUCCESS           = NTSTATUS(0);

  STATUS_NO_MORE_DATA      = NTSTATUS($8000001a);
  STATUS_NO_MORE_FILES     = NTSTATUS($80000006);
  STATUS_INVALID_PARAMETER = NTSTATUS($C000000D);

  STATUS_INFO_LENGTH_MISMATCH =  NTSTATUS($C0000004);

type
  TRtlNtStatusToDosError = function( const Status : NTSTATUS ) : DWORD; stdcall;


type
  TNtAnsiString = packed record
    Length        : Word;
    MaximumLength : Word;
    Buffer        : PChar;
  end;
  PNtAnsiString = ^TNtAnsiString;
  ANSI_STRING = TNtAnsiString;

  TNtUnicodeString = packed record
    Length        : Word;
    MaximumLength : Word;
    Buffer        : PWideChar;
  end;
  UNICODE_STRING = TNtUnicodeString;
  PNtUnicodeString = ^TNtUnicodeString;

  TNtUnicodeString64 = packed record
    Length        : Word;
    MaximumLength : Word;
    Padding2      : array[0..3] of Byte; // 8-byte aligned
    Buffer        : PWideChar64;
  end;
  UNICODE_STRING64 = TNtUnicodeString64;
  PNtUnicodeString64 = ^TNtUnicodeString64;

type
  TRtlAnsiStringToUnicodeString = function( DestinationString : PNtUnicodeString; SourceString : PNtAnsiString; AllocateDestinationString : Boolean ) : NTSTATUS; stdcall;
  TRtlFreeUnicodeString = procedure( UnicodeString : PNtUnicodeString ); stdcall;


type
  TNtLoadDriver = function( DriverServiceName : PNtUnicodeString ) : NTSTATUS; stdcall;
  TNtUnloadDriver = function( DriverServiceName : PNtUnicodeString ) : NTSTATUS; stdcall;


type

  TNtObjectAttributes = packed record
    Length                   : ULONG; // = SizeOf(OBJECT_ATTRIBUTES)

    // Optionally specifies a handle to a directory obtained by a preceding call to NtCreateFile.
    // If this value is NULL, the ObjectName member must be a fully qualified file specification
    // that includes the full path to the target file.
    // If this value is nonNULL, the ObjectName member specifies a file name relative to this directory.
    RootDirectory            : THandle;

    ObjectName               : PNtUnicodeString;

    Attributes               : ULONG;
    SecurityDescriptor       : Pointer; // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService : Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  OBJECT_ATTRIBUTES = TNtObjectAttributes;
  PNtObjectAttributes = ^TNtObjectAttributes;

// TNtObjectAttributes.Attributes
const
  OBJ_INHERIT          = $00000002;
  OBJ_PERMANENT        = $00000010;
  OBJ_EXCLUSIVE        = $00000020;
  OBJ_CASE_INSENSITIVE = $00000040;
  OBJ_OPENIF           = $00000080;
  OBJ_OPENLINK         = $00000100;
  OBJ_VALID_ATTRIBUTES = $000001F2;

  procedure InitializeObjectAttributes(
      InitializedAttributes : PNtObjectAttributes;
      pObjectName : PNtUnicodeString;
      const uAttributes : ULONG;
      const hRootDirectory : THandle;
      pSecurityDescriptor : PSECURITY_DESCRIPTOR
    );

{ NtQueryInformation constants }

const
  ProcessBasicInformation = 0;
  ProcessDebugPort = 7;

{ NtQueryInformation types }

type
  TProcessBasicInformation = packed record
    ExitStatus: Integer;
    PebBaseAddress: Pointer;
    AffinityMask: Integer;
    BasePriority: Integer;
    UniqueProcessID: Integer;
    InheritedFromUniqueProcessID: Integer;
  end;

type
  TProcessBasicInformation64 = packed record
    ExitStatus: Integer;
    Padding1: array[0..3] of Byte; // 8-byte aligned
    PebBaseAddress: Pointer64;
    AffinityMask: Integer64;
    BasePriority: Integer;
    Padding2: array[0..3] of Byte;
    UniqueProcessID: Integer64;
    InheritedFromUniqueProcessID: Integer64;
  end;

  TNtQueryInformationProcess =
      function(hProcess: THandle; ProcessInformationClass: Integer;
          var ProcessInformation; ProcessInformationLength: Integer;
          var ReturnLength: Integer): NTSTATUS; stdcall;

const
  SystemProcessInformation = 5;

type
  TIoCounters = packed record
    ReadOperationCount: Comp;
    WriteOperationCount: Comp;
    OtherOperationCount: Comp;
    ReadTransferCount: Comp;
    WriteTransferCount: Comp;
    OtherTransferCount: Comp;
  end;
  
  PSystemThreadInformation = ^TSystemThreadInformation;
  TSystemThreadInformation = packed record
    ProcessorTime               : TFileTime;
    UserTime                    : TFileTime;
    CreateTime                  : TFileTime;
    dWaitTime                   : DWORD;
    StartAddress                : DWORD;
    ProcessID                   : DWORD;
    ThreadID                    : DWORD;
    CurrentPriority             : DWORD;
    BasePriority                : DWORD;
    ContextSwitchesPerSec       : DWORD;
    ThreadState                 : DWORD;
    ThreadWaitReason            : DWORD;
    u15                         : DWORD;
  end;

  PSystemProcessInformation = ^TSystemProcessInformation;
  TSystemProcessInformation = packed record
    NextOffset                  : DWORD;
    ThreadCount                 : DWORD;
    u2, u3, u4, u5, u6, u7      : DWORD;
    CreateTime                  : TFileTime;
    UserTime                    : TFileTime;
    KernelTime                  : TFileTime;
    usName                      : TNtUnicodeString;
    BasePriority                : DWORD;
    dUniqueProcessId            : DWORD;
    dInheritedFromUniqueProcessId: DWORD;
    dHandleCount                : DWORD;
    u20, u21                    : DWORD;
    PeekVirtualSize             : DWORD;
    VirtualSize                 : DWORD;
    PageFaultCountPerSec        : DWORD;
    PeakWorkingSetSize          : DWORD;
    WorkingSetState             : DWORD;
    PeekPagedPoolUsage          : DWORD;
    PagedPoolUsage              : DWORD;
    PeekNonPagedPoolUsage       : DWORD;
    NonPagedPoolUsage           : DWORD;
    u31                         : DWORD;
    PeakPagefileUsage           : DWORD;
    u33                         : DWORD;
  end;
  PSystemProcessInformationNT4 = ^TSystemProcessInformationNT4;
  TSystemProcessInformationNT4 = packed record
    Process: TSystemProcessInformation;
    ThreadInfos: array[0..0] of TSystemThreadInformation;
  end;
  PSystemProcessInformationNT5 = ^TSystemProcessInformationNT5;
  TSystemProcessInformationNT5 = packed record
    Process: TSystemProcessInformation;
    IoCounters: TIoCounters;
    ThreadInfos: array[0..0] of TSystemThreadInformation;
  end;
  {TSystemProcessInformation = packed record
    dNext: Integer;
    dThreadCount: Integer;
    dReserved01, dReserved02, dReserved03, dReserved04, dReserved05, dReserved06: Integer;
    //qCreateTime, qUserTime, qKernelTime: Int64;
    qCreateTime, qUserTime, qKernelTime: Comp;
    usName: TNtUnicodeString;
    BasePriority: DWord;
    dUniqueProcessId: DWord;
    dInheritedFromUniqueProcessId: DWord;
    dHandleCount: DWord;
    dReserved07: DWord;
    dReserved08: DWord;
    VmCounters: array[0..10] of DWord;
    dCommitCharge: DWord;
  end;}


  TNtQuerySystemInformation =
      function(sic: DWord; Buffer: Pointer; BufSize: DWord;
          var BytesReturned: DWord): NTStatus; stdcall;

type
  PPebLdrData = ^TPebLdrData;
  _PEB_LDR_DATA = packed record
    Length                         : Cardinal;
    Initialized                    : LongBool;
    SsHandle                       : THandle;
    InLoadOrderModuleList          : TListEntry;
    InMemoryOrderModuleList        : TListEntry;
    InInitializationOrderModuleList: TListEntry;
  end;
  TPebLdrData = _PEB_LDR_DATA;

type
  PPebLdrData64 = Pointer64;

type
  PCurDir = ^TCurDir;
  _CURDIR = packed record
    DosPath: TNtUnicodeString;
    Handle : THandle;
  end;
  TCurDir = _CURDIR;

type
  PCurDir64 = ^TCurDir64;
  _CURDIR64 = packed record
    DosPath: TNtUnicodeString64;
    Handle : THandle64;
  end;
  TCurDir64 = _CURDIR64;

const
  PROCESS_PARAMETERS_NORMALIZED = 1;

type
  PRtlUserProcessParameters = ^TRtlUserProcessParameters;
  _RTL_USER_PROCESS_PARAMETERS = record
    MaximumLength    : Cardinal;
    Length           : Cardinal;
    Flags            : Cardinal;
    DebugFlags       : Cardinal;
    ConsoleHandle    : THandle;
    ConsoleFlags     : Cardinal;
    StandardInput    : THandle;
    StandardOutput   : THandle;
    StandardError    : THandle;
    CurrentDirectory : TCurDir;
    DllPath          : TNtUnicodeString;
    ImagePathName    : TNtUnicodeString;
    CommandLine      : TNtUnicodeString;
    Environment      : Pointer;
    StartingX        : Cardinal;
    StartingY        : Cardinal;
    CountX           : Cardinal;
    CountY           : Cardinal;
    CountCharsX      : Cardinal;
    CountCharsY      : Cardinal;
    FillAttribute    : Cardinal;
    WindowFlags      : Cardinal;
    ShowWindowFlags  : Cardinal;
    WindowTitle      : TNtUnicodeString;
    DesktopInfo      : TNtUnicodeString;
    ShellInfo        : TNtUnicodeString;
    RuntimeData      : TNtUnicodeString;
    //CurrentDirectores: array [0..31] of TRtlDriveLetterCurDir;
  end;
  TRtlUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;

type
  PRtlUserProcessParameters64 = Pointer64;
  _RTL_USER_PROCESS_PARAMETERS64 = packed record
    MaximumLength    : Cardinal;
    Length           : Cardinal;
    Flags            : Cardinal;
    DebugFlags       : Cardinal;
    ConsoleHandle    : THandle64;
    ConsoleFlags     : Cardinal;
    Padding5         : array[0..3] of Byte; // 8-byte aligned
    StandardInput    : THandle64;
    StandardOutput   : THandle64;
    StandardError    : THandle64;
    CurrentDirectory : TCurDir64;
    DllPath          : TNtUnicodeString64;
    ImagePathName    : TNtUnicodeString64;
    CommandLine      : TNtUnicodeString64;
    Environment      : Pointer64;
    StartingX        : Cardinal;
    StartingY        : Cardinal;
    CountX           : Cardinal;
    CountY           : Cardinal;
    CountCharsX      : Cardinal;
    CountCharsY      : Cardinal;
    FillAttribute    : Cardinal;
    WindowFlags      : Cardinal;
    ShowWindowFlags  : Cardinal;
    Padding14        : array[0..3] of Byte;
    WindowTitle      : TNtUnicodeString64;
    DesktopInfo      : TNtUnicodeString64;
    ShellInfo        : TNtUnicodeString64;
    RuntimeData      : TNtUnicodeString64;
    //CurrentDirectores: array [0..31] of TRtlDriveLetterCurDir;
  end;
  TRtlUserProcessParameters64 = _RTL_USER_PROCESS_PARAMETERS64;

type
  PPebFreeBlock = ^TPebFreeBlock;
  _PEB_FREE_BLOCK = record
    Next: PPebFreeBlock;
    Size: Cardinal;
  end;
  TPebFreeBlock = _PEB_FREE_BLOCK;

type
  PPebFreeBlock64 = Pointer64;

type
  PPeb = ^TPeb;
  _PEB = packed record
    InheritedAddressSpace         : Boolean;
    ReadImageFileExecOptions      : Boolean;
    BeingDebugged                 : Boolean;
    SpareBool                     : Boolean;
    Mutant                        : Pointer;  // THandle
    ImageBaseAddress              : Pointer;
    Ldr                           : PPebLdrData;
    ProcessParameters             : PRtlUserProcessParameters;
    SubSystemData                 : Pointer;
    ProcessHeap                   : Pointer;  // THandle
    FastPebLock                   : Pointer;
    FastPebLockRoutine            : Pointer;
    FastPebUnlockRoutine          : Pointer;
    EnvironmentUpdateCount        : Cardinal;
    KernelCallbackTable           : Pointer;
    case Integer of
      4: (
        EventLogSection           : Pointer;   // THandle
        EventLog                  : Pointer);  // THandle
      5: (
        SystemReserved            : array [0..1] of Cardinal;
  { end; }
    FreeList                      : PPebFreeBlock;
    TlsExpansionCounter           : Cardinal;
    TlsBitmap                     : Pointer;
    TlsBitmapBits                 : array [0..1] of Cardinal;
    ReadOnlySharedMemoryBase      : Pointer;
    ReadOnlySharedMemoryHeap      : Pointer;
    ReadOnlyStaticServerData      : ^Pointer;
    AnsiCodePageData              : Pointer;
    OemCodePageData               : Pointer;
    UnicodeCaseTableData          : Pointer;
    NumberOfProcessors            : Cardinal;
    NtGlobalFlag                  : Cardinal;
    Unknown                       : Cardinal;
    CriticalSectionTimeout        : TLargeInteger;
    HeapSegmentReserve            : Cardinal;
    HeapSegmentCommit             : Cardinal;
    HeapDeCommitTotalFreeThreshold: Cardinal;
    HeapDeCommitFreeBlockThreshold: Cardinal;
    NumberOfHeaps                 : Cardinal;
    MaximumNumberOfHeaps          : Cardinal;
    ProcessHeaps                  : ^Pointer;
    GdiSharedHandleTable          : Pointer;
    ProcessStarterHelper          : Pointer;
    GdiDCAttributeList            : Cardinal;
    LoaderLock                    : Pointer;
    OSMajorVersion                : Cardinal;
    OSMinorVersion                : Cardinal;
    OSBuildNumber                 : Word;
    OSCSDVersion                  : Word;
    OSPlatformId                  : Cardinal;
    ImageSubsystem                : Cardinal;
    ImageSubsystemMajorVersion    : Cardinal;
    ImageSubsystemMinorVersion    : Cardinal;
    ImageProcessAffinityMask      : Cardinal;
    GdiHandleBuffer               : array [0..33] of Cardinal;
    { Windows 2000 - begin }
    PostProcessInitRoutine        : ^Pointer;  // ^function
    TlsExpansionBitmap            : Pointer;
    TlsExpansionBitmapBits        : array [0..31] of Cardinal;
    SessionId                     : Cardinal;
    AppCompatInfo                 : Pointer;
    CSDVersion                    : TNtUnicodeString);
    { Windows 2000 - end }
  end;
  TPeb = _PEB;

type
  PPeb64 = Pointer64;
  _PEB64 = packed record
    InheritedAddressSpace         : Boolean;
    ReadImageFileExecOptions      : Boolean;
    BeingDebugged                 : Boolean;
    SpareBool                     : Boolean;
    Padding                       : array[0..3] of Byte; // 8-byte aligned
    Mutant                        : Pointer64;  // THandle
    ImageBaseAddress              : Pointer64;
    Ldr                           : PPebLdrData64;
    ProcessParameters             : PRtlUserProcessParameters64;
    SubSystemData                 : Pointer64;
    ProcessHeap                   : Pointer64;  // THandle
    FastPebLock                   : Pointer64;
    FastPebLockRoutine            : Pointer64;
    FastPebUnlockRoutine          : Pointer64;
    Spare                         : array [0..3] of Pointer64;
    FreeList                      : PPebFreeBlock64;
    TlsExpansionCounter           : Cardinal;
    Padding2                      : array[0..3] of Byte;
    TlsBitmap                     : Pointer64;
    TlsBitmapBits                 : array [0..1] of Cardinal;
    ReadOnlySharedMemoryBase      : Pointer64;
    ReadOnlySharedMemoryHeap      : Pointer64;
    ReadOnlyStaticServerData      : ^Pointer64;
    Padding3                      : array[0..3] of Byte;
    AnsiCodePageData              : Pointer64;
    OemCodePageData               : Pointer64;
    UnicodeCaseTableData          : Pointer64;
    {NumberOfProcessors            : Cardinal;
    NtGlobalFlag                  : Cardinal;
    Unknown                       : Cardinal;}
    CriticalSectionTimeout        : TLargeInteger;
  end;
  TPeb64 = _PEB64;

type

  PClientID = ^TClientID;
  TClientID = packed record
    UniqueProcess : DWORD;
    UniqueThread  : DWORD;
  end;

  TNtOpenThread =
      function(phThread: PHandle; AccessMask: DWORD;
          ObjectAttributes: PNtObjectAttributes;
          ClientID: PClientID): NTStatus; stdcall;

type
  TNtWow64QueryInformationProcess64 =
      function(hProcess: THandle; ProcessInformationClass: Integer;
          var ProcessInformation; ProcessInformationLength: Integer;
          var ReturnLength: Integer64): NTSTATUS; stdcall;
  TNtWow64ReadVirtualMemory64 =
      function(hProcess: THandle; BaseAddress: Pointer64; Buffer: Pointer;
          BufferSize: Integer64; var NumberOfBytesRead: Integer64): NTSTATUS;
          stdcall;

const
  ntdll = 'ntdll.dll';

implementation

function NT_SUCCESS(Status: Integer): WordBool; 
begin 
  Result := Status >= 0; 
end;

procedure InitializeObjectAttributes(InitializedAttributes : PNtObjectAttributes;
    pObjectName : PNtUnicodeString; const uAttributes : ULONG;
    const hRootDirectory : THandle; pSecurityDescriptor : PSECURITY_DESCRIPTOR);
begin
  with InitializedAttributes^ do begin
    Length := SizeOf(TNtObjectAttributes);
    ObjectName := pObjectName;
    Attributes := uAttributes;
    RootDirectory := hRootDirectory;
    SecurityDescriptor := pSecurityDescriptor;
    SecurityQualityOfService := nil;
  end;
end;

end.
 