// (c) Alex Konshin    5 jul 2000      mailto:alexk@mtgroup.ru

// ntdll.dll functions definitions - NT Native API

// Some of these functions are described in Win NT/2000 DDK (Rtl*,Zw* functions)

// see also http://www.sysinternals.com/ntdll.htm

unit NativeApi;



interface

uses Windows;

{$Z4} // important!

type
  PLARGE_INTEGER = ^TLARGEINTEGER;

//-------------------------------------------------------------
type
  NTSTATUS = LongInt;

const
  STATUS_SUCCESS           = NTSTATUS(0);

  STATUS_NO_MORE_DATA      = NTSTATUS($8000001a);
  STATUS_NO_MORE_FILES     = NTSTATUS($80000006);
  STATUS_INVALID_PARAMETER = NTSTATUS($C000000D);

function RtlNtStatusToDosError( const Status : NTSTATUS ) : DWORD; stdcall;

//-------------------------------------------------------------
// Counted String
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
  PUNICODE_STRING = ^TNtUnicodeString;

procedure RtlInitString( DestinationString : PNtAnsiString; SourceString : PChar ); stdcall;
procedure RtlInitAnsiString( DestinationString : PNtAnsiString; SourceString : PChar ); stdcall;
procedure RtlCopyString( DestinationString : PNtAnsiString; SourceString : PNtAnsiString ); stdcall;
function  RtlUpperChar ( Character : Char ) : Char; stdcall;
function  RtlCompareString( String1, String2 : PNtAnsiString; CaseInSensitive : Boolean ) : LongInt; stdcall;
function  RtlEqualString( String1, String2 : PNtAnsiString; CaseInSensitive : Boolean ) : Boolean; stdcall;
procedure RtlUpperString( DestinationString : PNtAnsiString; SourceString : PNtAnsiString ); stdcall;
procedure RtlFreeAnsiString( NtAnsiString : PNtAnsiString ); stdcall;

//-------------------------------------------------------------
// NLS String functions
procedure RtlInitUnicodeString( DestinationString : PNtUnicodeString; SourceString : PWideChar ); stdcall;
function  RtlAnsiStringToUnicodeString( DestinationString : PNtUnicodeString; SourceString : PNtAnsiString; AllocateDestinationString : Boolean ) : NTSTATUS; stdcall;
function  RtlUnicodeStringToAnsiString( DestinationString : PNtAnsiString; SourceString : PNtUnicodeString; AllocateDestinationString : Boolean ) : NTSTATUS; stdcall;
function  RtlCompareUnicodeString( String1, String2 : PNtUnicodeString; CaseInSensitive : Boolean ) : LongInt; stdcall;
function  RtlEqualCompareUnicodeString( String1, String2 : PNtUnicodeString; CaseInSensitive : Boolean ) : Boolean; stdcall;
function  RtlPrefixUnicodeString( String1, String2 : PNtUnicodeString; CaseInSensitive : Boolean ) : Boolean; stdcall;
function  RtlUpcaseUnicodeString( DestinationString : PNtUnicodeString; SourceString : PNtUnicodeString; AllocateDestinationString : Boolean ) : NTSTATUS; stdcall;
procedure RtlCopyUnicodeString( DestinationString : PNtUnicodeString; SourceString : PNtUnicodeString ); stdcall;
function  RtlAppendUnicodeStringToString( DestinationString : PNtUnicodeString; SourceString : PNtUnicodeString ): NTSTATUS; stdcall;
function  RtlAppendUnicodeToString( DestinationString : PNtUnicodeString; SourceString : PWideChar ): NTSTATUS; stdcall;
function  RtlUpcaseUnicodeChar( SourceCharacter : WideChar ) : WideChar; stdcall;
procedure RtlFreeUnicodeString( UnicodeString : PNtUnicodeString ); stdcall;
function  RtlxAnsiStringToUnicodeSize( NtAnsiString : PNtAnsiString ) : DWORD; stdcall;
//function  RtlAnsiStringToUnicodeSize( NtAnsiString : PNtAnsiString ) : DWORD; stdcall;


//=============================================================
// The following are definitions for documented and undocumented native APIs
// and structures.

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
  POBJECT_ATTRIBUTES = ^TNtObjectAttributes;

// TNtObjectAttributes.Attributes
const
  OBJ_INHERIT          = $00000002;
  OBJ_PERMANENT        = $00000010;
  OBJ_EXCLUSIVE        = $00000020;
  OBJ_CASE_INSENSITIVE = $00000040;
  OBJ_OPENIF           = $00000080;
  OBJ_OPENLINK         = $00000100;
  OBJ_VALID_ATTRIBUTES = $000001F2;

//-------------------------------------------------------------
type
  TIoStatusBlock = packed record
    Status      : NTSTATUS;
    Information : ULONG;
  end;
  IO_STATUS_BLOCK = TIoStatusBlock;
  PIoStatusBlock = ^TIoStatusBlock;

// TIoStatusBlock.Information value
// Define the I/O status information return values for NtCreateFile/NtOpenFile
const
  FILE_SUPERSEDED     = $00000000;
  FILE_OPENED         = $00000001;
  FILE_CREATED        = $00000002;
  FILE_OVERWRITTEN    = $00000003;
  FILE_EXISTS         = $00000004;
  FILE_DOES_NOT_EXIST = $00000005;


//-------------------------------------------------------------
// Define the file attributes values
//
// Note:  0x00000008 is reserved for use for the old DOS VOLID (volume ID)
//        and is therefore not considered valid in NT.
//
// Note:  0x00000010 is reserved for use for the old DOS SUBDIRECTORY flag
//        and is therefore not considered valid in NT.  This flag has
//        been disassociated with file attributes since the other flags are
//        protected with READ_ and WRITE_ATTRIBUTES access to the file.
//
// Note:  Note also that the order of these flags is set to allow both the
//        FAT and the Pinball File Systems to directly set the attributes
//        flags in attributes words without having to pick each flag out
//        individually.  The order of these flags should not be changed!
//
const
  FILE_ATTRIBUTE_READONLY            = $00000001;
  FILE_ATTRIBUTE_HIDDEN              = $00000002;
  FILE_ATTRIBUTE_SYSTEM              = $00000004;
//OLD DOS VOLID                        $00000008
  FILE_ATTRIBUTE_DIRECTORY           = $00000010;
  FILE_ATTRIBUTE_ARCHIVE             = $00000020;
  FILE_ATTRIBUTE_DEVICE              = $00000040;
  FILE_ATTRIBUTE_NORMAL              = $00000080;

  FILE_ATTRIBUTE_TEMPORARY           = $00000100;
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  FILE_ATTRIBUTE_COMPRESSED          = $00000800;

  FILE_ATTRIBUTE_OFFLINE             = $00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;

  FILE_ATTRIBUTE_VALID_FLAGS     = $00007fb7;
  FILE_ATTRIBUTE_VALID_SET_FLAGS = $000031a7;
//-------------------------------------------------------------

const
  FILE_READ_DATA        = $0001; // file & pipe
  FILE_LIST_DIRECTORY   = $0001; // directory

  FILE_WRITE_DATA       = $0002; // file & pipe
  FILE_ADD_FILE         = $0002; // directory

  FILE_APPEND_DATA          = $0004; // file
  FILE_ADD_SUBDIRECTORY     = $0004; // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe

  FILE_READ_EA              = $0008; // file & directory
  FILE_WRITE_EA             = $0010; // file & directory

  FILE_EXECUTE              = $0020; // file
  FILE_TRAVERSE             = $0020; // directory

  FILE_DELETE_CHILD         = $0040; // directory
  FILE_READ_ATTRIBUTES      = $0080; // all
  FILE_WRITE_ATTRIBUTES     = $0100; // all

  FILE_ALL_ACCESS      = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $01FF;
  FILE_GENERIC_READ    = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE   = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;


//-------------------------------------------------------------
// Define the create disposition values
  FILE_SUPERSEDE           = $00000000;
  FILE_OPEN                = $00000001;
  FILE_CREATE              = $00000002;
  FILE_OPEN_IF             = $00000003;
  FILE_OVERWRITE           = $00000004;
  FILE_OVERWRITE_IF        = $00000005;
  FILE_MAXIMUM_DISPOSITION = $00000005;

//-------------------------------------------------------------
// Define the create/open option flags
const
  FILE_DIRECTORY_FILE              = $00000001;
  FILE_WRITE_THROUGH               = $00000002;
  FILE_SEQUENTIAL_ONLY             = $00000004;
  FILE_NO_INTERMEDIATE_BUFFERING   = $00000008;

  FILE_SYNCHRONOUS_IO_ALERT        = $00000010;
  FILE_SYNCHRONOUS_IO_NONALERT     = $00000020;
  FILE_NON_DIRECTORY_FILE          = $00000040;
  FILE_CREATE_TREE_CONNECTION      = $00000080;

  FILE_COMPLETE_IF_OPLOCKED        = $00000100;
  FILE_NO_EA_KNOWLEDGE             = $00000200;
  FILE_OPEN_FOR_RECOVERY           = $00000400;
  FILE_RANDOM_ACCESS               = $00000800;

  FILE_DELETE_ON_CLOSE             = $00001000;
  FILE_OPEN_BY_FILE_ID             = $00002000;
  FILE_OPEN_FOR_BACKUP_INTENT      = $00004000;
  FILE_NO_COMPRESSION              = $00008000;

  FILE_RESERVE_OPFILTER            = $00100000;
  FILE_OPEN_REPARSE_POINT          = $00200000;
  FILE_OPEN_NO_RECALL              = $00400000;
  FILE_OPEN_FOR_FREE_SPACE_QUERY   = $00800000;

  FILE_COPY_STRUCTURED_STORAGE     = $00000041;
  FILE_STRUCTURED_STORAGE          = $00000441;

  FILE_VALID_OPTION_FLAGS          = $00ffffff;
  FILE_VALID_PIPE_OPTION_FLAGS     = $00000032;
  FILE_VALID_MAILSLOT_OPTION_FLAGS = $00000032;
  FILE_VALID_SET_FLAGS             = $00000036;

//-------------------------------------------------------------
// Object Manager Object Type Specific Access Rights.
const
  OBJECT_TYPE_CREATE     = $0001;
  OBJECT_TYPE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1;

// Object Manager Directory Specific Access Rights.
  DIRECTORY_QUERY               = $0001;
  DIRECTORY_TRAVERSE            = $0002;
  DIRECTORY_CREATE_OBJECT       = $0004;
  DIRECTORY_CREATE_SUBDIRECTORY = $0008;
  DIRECTORY_ALL_ACCESS          = STANDARD_RIGHTS_REQUIRED or $F;

// Object Manager Symbolic Link Specific Access Rights.
  SYMBOLIC_LINK_QUERY      = $0001;
  SYMBOLIC_LINK_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1;

const
  DUPLICATE_CLOSE_SOURCE    = $00000001;
  DUPLICATE_SAME_ACCESS     = $00000002;
  DUPLICATE_SAME_ATTRIBUTES = $00000004;

//-------------------------------------------------------------
// Section Information Structures.
type
  TSectionInherit = (ViewNone,ViewShare,ViewUnmap);
  SECTION_INHERIT = TSectionInherit;

// Section Access Rights.
const
  SECTION_QUERY       = $0001;
  SECTION_MAP_WRITE   = $0002;
  SECTION_MAP_READ    = $0004;
  SECTION_MAP_EXECUTE = $0008;
  SECTION_EXTEND_SIZE = $0010;

  SECTION_ALL_ACCESS  = STANDARD_RIGHTS_REQUIRED or SECTION_QUERY or SECTION_MAP_WRITE or SECTION_MAP_READ or SECTION_MAP_EXECUTE or SECTION_EXTEND_SIZE;
  SEGMENT_ALL_ACCESS  = SECTION_ALL_ACCESS;

  PAGE_NOACCESS          = $01;
  PAGE_READONLY          = $02;
  PAGE_READWRITE         = $04;
  PAGE_WRITECOPY         = $08;
  PAGE_EXECUTE           = $10;
  PAGE_EXECUTE_READ      = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD             = $100;
  PAGE_NOCACHE           = $200;
  PAGE_WRITECOMBINE      = $400;

  MEM_COMMIT             = $1000;
  MEM_RESERVE            = $2000;
  MEM_DECOMMIT           = $4000;
  MEM_RELEASE            = $8000;
  MEM_FREE               = $10000;
  MEM_PRIVATE            = $20000;
  MEM_MAPPED             = $40000;
  MEM_RESET              = $80000;
  MEM_TOP_DOWN           = $100000;
  MEM_LARGE_PAGES        = $20000000;
  MEM_4MB_PAGES          = $80000000;
  SEC_RESERVE            = $4000000;
  PROCESS_DUP_HANDLE     = $0040;
  PROCESS_ALL_ACCESS     = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $FFF;
//-------------------------------------------------------------
// Process Information Classes
type
  TProcessInfoClass = (ProcessBasicInformation,ProcessQuotaLimits,
    ProcessIoCounters,ProcessVmCounters,
    ProcessTimes,ProcessBasePriority,ProcessRaisePriority,
    ProcessDebugPort,ProcessExceptionPort,
    ProcessAccessToken,ProcessLdtInformation,
    ProcessLdtSize,ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers,ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch,ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup,ProcessPriorityClass,
    ProcessWx86Information,ProcessHandleCount,
    ProcessAffinityMask,ProcessPriorityBoost,
    ProcessDeviceMap,ProcessSessionInformation,
    ProcessForegroundInformation,ProcessWow64Information,
    MaxProcessInfoClass);
  PROCESSINFOCLASS =  TProcessInfoClass;

// Thread Information Classes
  TThreadInfoClass = (ThreadBasicInformation,ThreadTimes,ThreadPriority,
    ThreadBasePriority,ThreadAffinityMask,
    ThreadImpersonationToken,ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,ThreadEventPair_Reusable,
    ThreadQuerySetWin32StartAddress,ThreadZeroTlsCell,
    ThreadPerformanceCount,ThreadAmILastThread,
    ThreadIdealProcessor,ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,ThreadIsIoPending,
    ThreadHideFromDebugger,MaxThreadInfoClass);
  THREADINFOCLASS = TThreadInfoClass;

//-------------------------------------------------------------
type
  TFileInformationClass = (__FileInformationNone,FileDirectoryInformation,
    FileFullDirectoryInformation,FileBothDirectoryInformation,
    FileBasicInformation,FileStandardInformation,
    FileInternalInformation,FileEaInformation,
    FileAccessInformation,FileNameInformation,
    FileRenameInformation,FileLinkInformation,
    FileNamesInformation,FileDispositionInformation,
    FilePositionInformation,FileFullEaInformation,
    FileModeInformation,FileAlignmentInformation,
    FileAllInformation,FileAllocationInformation,
    FileEndOfFileInformation,FileAlternateNameInformation,
    FileStreamInformation,FilePipeInformation,
    FilePipeLocalInformation,FilePipeRemoteInformation,
    FileMailslotQueryInformation,FileMailslotSetInformation,
    FileCompressionInformation,FileCopyOnWriteInformation,
    FileCompletionInformation,FileMoveClusterInformation,
    FileOleClassIdInformation,FileOleStateBitsInformation,
    FileNetworkOpenInformation,FileObjectIdInformation,
    FileOleAllInformation,FileOleDirectoryInformation,
    FileContentIndexInformation,FileInheritContentIndexInformation,
    FileOleInformation,FileMaximumInformation
  );
  FILE_INFORMATION_CLASS = TFileInformationClass;
  PFileInformationClass = ^TFileInformationClass;

// Define the various structures which are returned on query operations
type
  TFileBasicInformation = record
    CreationTime   : TLargeInteger;
    LastAccessTime : TLargeInteger;
    LastWriteTime  : TLargeInteger;
    ChangeTime     : TLargeInteger;
    FileAttributes : ULONG;
  end;
  FILE_BASIC_INFORMATION = TFileBasicInformation;
  PFileBasicInformation = ^TFileBasicInformation;

  TFileStandardInformation = packed record
    AllocationSize : TLargeInteger;
    EndOfFile      : TLargeInteger;
    NumberOfLinks  : ULONG;
    DeletePending  : Boolean;
    Directory      : Boolean;
  end;
  FILE_STANDARD_INFORMATION = TFileStandardInformation;
  PFileStandardInformation = ^TFileStandardInformation;

  TFilePositionInformation = record
    CurrentByteOffset : TLargeInteger;
  end;
  FILE_POSITION_INFORMATION = TFilePositionInformation;
  PFilePositionInformation = ^TFilePositionInformation;

  TFileAlignmentInformation = record
    AlignmentRequirement : ULONG;
  end;
  FILE_ALIGNMENT_INFORMATION = TFileAlignmentInformation;
  PFileAlignmentInformation = ^TFileAlignmentInformation;

  TFileNameInformation = packed record
    FileNameLength : ULONG;
    FileName       : Array[0..0] of WideChar;
  end;
  FILE_NAME_INFORMATION = TFileNameInformation;
  PFileNameInformation = ^TFileNameInformation;

  TFileNetworkOpenInformation = record
    CreationTime   : TLargeInteger;
    LastAccessTime : TLargeInteger;
    LastWriteTime  : TLargeInteger;
    ChangeTime     : TLargeInteger;
    AllocationSize : TLargeInteger;
    EndOfFile      : TLargeInteger;
    FileAttributes : ULONG;
  end;
  FILE_NETWORK_OPEN_INFORMATION = TFileNetworkOpenInformation;
  PFileNetworkOpenInformation = ^TFileNetworkOpenInformation;

  TFileAttributeTagInformation = packed record
    FileAttributes : ULONG;
    ReparseTag : ULONG;
  end;
  FILE_ATTRIBUTE_TAG_INFORMATION = TFileAttributeTagInformation;
  PFileAttributeTagInformation = ^TFileAttributeTagInformation;

  TFileDispositionInformation = packed record
    DeleteFile : Boolean;
  end;
  FILE_DISPOSITION_INFORMATION = TFileDispositionInformation;
  PFileDispositionInformation = ^TFileDispositionInformation;

  TFileEndOfFileInformation = record
    EndOfFile : TLargeInteger;
  end;
  FILE_END_OF_FILE_INFORMATION = TFileEndOfFileInformation;
  PFileEndOfFileInformation = ^TFileEndOfFileInformation;

  TFileFullEAIinformation = packed record
    NextEntryOffset : ULONG;
    Flags           : Byte;
    EaNameLength    : Byte;
    EaValueLength   : Word;
    EaName          : Array[0..0] of Char;
  end;
  FILE_FULL_EA_INFORMATION = TFileFullEAIinformation;
  PFileFullEAIinformation = ^TFileFullEAIinformation;

type
  TFileDirectoryInformation = packed record
    NextEntryOffset : ULONG;
    FileIndex       : ULONG;
    CreationTime    : TLargeInteger;
    LastAccessTime  : TLargeInteger;
    LastWriteTime   : TLargeInteger;
    ChangeTime      : TLargeInteger;
    EndOfFile       : TLargeInteger;
    AllocationSize  : TLargeInteger;
    FileAttributes  : ULONG;
    FileNameLength  : ULONG;
    FileName : Array[0..0] of WideChar;
  end;
  FILE_DIRECTORY_INFORMATION = TFileDirectoryInformation;
  PFileDirectoryInformation = ^TFileDirectoryInformation;

type
  TIoApcRoutine = procedure ( ApcContext : Pointer; IoStatusBlock : PIoStatusBlock; Reserved : ULONG ); stdcall;

const
  NonPagedPool = 0;
  PagedPool = 1;
  NonPagedPoolMustSucceed = 2;
  DontUseThisType = 3;
  NonPagedPoolCacheAligned = 4;
  PagedPoolCacheAligned = 5;
  NonPagedPoolCacheAlignedMustS = 6;
  MaxPoolType = 7;
  NonPagedPoolSession = 32;
  PagedPoolSession = NonPagedPoolSession + 1;
  NonPagedPoolMustSucceedSession = PagedPoolSession + 1;
  DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1;
  NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1;
  PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1;
  NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1;

type
  POOL_TYPE = NonPagedPool..NonPagedPoolCacheAlignedMustSSession;

//-------------------------------------------------------------
// NtCreateFile either causes a new file or directory to be created,
// or it opens an existing file, device, directory, or volume,
// giving the caller a handle for the file object.
// This handle can be used by subsequent calls to manipulate data
// within the file or the file object's state or attributes.
// For example, a driver might call this routine during initialization
//  to open a file of microcode for its device.
function NtCreateFile(
  // Points to a variable that receives the file handle if the call is successful.
  var FileHandle      : THandle;

  // Specifies the type of access that the caller requires to the file or directory.
  const DesiredAccess : ACCESS_MASK;

  // Pointer to a structure that a caller initializes with InitializeObjectAttributes
  var ObjectAttributes    : TNtObjectAttributes;

  // Points to a variable that receives the final completion status
  // and information about the requested operation.
  var IoStatusBlock   : TIoStatusBlock;

  // Optionally specifies the initial allocation size in bytes for the file.
  // A nonzero value has no effect unless the file is being created, overwritten,
  // or superseded.
  AllocationSize      : PLARGE_INTEGER;

  // Explicitly specified attributes are applied only when the file is created,
  // superseded, or, in some cases, overwritten.
  const FileAttributes: ULONG;

  // Specifies the type of share access that the caller would like to the file.
  // Device and intermediate drivers usually set ShareAccess to zero, which gives
  // the caller exclusive access to the open file.
  const ShareAccess       : ULONG;

  // Specifies what to do, depending on whether the file already exists
  const CreateDisposition : ULONG;

  // Specifies the options to be applied when creating or opening the file
  const CreateOptions     : ULONG;

  // For device and intermediate drivers, this parameter must be a NULL pointer
  EaBuffer          : Pointer;

  // For device and intermediate drivers, this parameter must be zero
  EaLength          : ULONG

) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtOpenFile opens an existing file, device, directory, or volume,
// and returns a handle for the file object
function NtOpenFile(
  // Points to a variable that receives the file handle if the call is successful.
  var FileHandle : THandle;

  // Specifies the type of access that the caller requires to the file or directory.
  const  DesiredAccess : ACCESS_MASK;

  // Pointer to a structure that a caller initializes with InitializeObjectAttributes
  var ObjectAttributes : TNtObjectAttributes;

  // Points to a variable that receives the final completion status
  // and information about the requested operation.
  var IoStatusBlock : TIoStatusBlock;

  // Specifies the type of share access that the caller would like to the file.
  // Device and intermediate drivers usually set ShareAccess to zero, which gives
  // the caller exclusive access to the open file.
  const ShareAccess : ULONG;

  // Specifies the options to be applied when opening the file
  const OpenOptions : ULONG
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtQueryInformationFile returns various kinds of information about a given file object
function NtQueryInformationFile(
  const FileHandle : THandle;

  // Points to a variable that receives the final completion status and information about the operation.
  var IoStatusBlock : TIoStatusBlock;

  // Points to a caller-allocated buffer or variable that receives the desired information about the file.
  // The contents of FileInformation are defined by the FileInformationClass parameter.
  FileInformation : Pointer;

  // Specifies the size in bytes of FileInformation, which the caller should set according to the given FileInformationClass.
  Length : ULONG;

  // Specifies the type of information to be returned about the file.
  FileInformationClass : TFileInformationClass
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtSetInformationFile changes various kinds of information about a given file object.
function NtSetInformationFile(
  FileHandle : THandle;

  // Points to a variable that receives the final completion status and information about the operation.
  var IoStatusBlock : TIoStatusBlock;

  // Points to a buffer or variable containing the information to be set for the file.
  // The contents of FileInformation are defined by the FileInformationClass parameter.
  // Setting any member of the structure in this buffer or variable to zero tells NtSetInformationFile
  // to leave the current information about the file for that member unchanged.
  FileInformation : Pointer;

  // Specifies the size in bytes of FileInformation, which the caller should set according to the given FileInformationClass.
  Length : ULONG;

  // Specifies the type of information to be reset for file.
  FileInformationClass: TFileInformationClass
) : NTSTATUS; stdcall;


//-------------------------------------------------------------
// Data can be read from an opened file using NtReadFile
function NtReadFile(
  FileHandle : THandle;

  // Specifies an optional handle for an event to be set to the signaled
  // state after the read operation completes.
  // Device and intermediate drivers should set this parameter to NULL.
  Event : THandle;

  // Device and intermediate drivers should set this pointer to NULL.
  ApcRoutine : TIoApcRoutine;

  // Device and intermediate drivers should set this pointer to NULL.
  ApcContext:Pointer;

  // Points to a variable that receives the final completion status and information about the operation.
  var IoStatusBlock : TIoStatusBlock;

  // Pointer to a caller-allocated buffer that receives the data read from the file.
  Buffer : Pointer;

  // Specifies the size in bytes of the given Buffer.
  // A successful call to ZwReadFile returns the given number of bytes from the file,
  // unless this routine reaches the end of file first.
  Length : ULONG;

  // Pointer to a variable that specifies the starting byte offset in the file
  // where the read operation will begin.
  // If an attempt is made to read beyond the end of the file, NtReadFile returns an error.
  ByteOffset : PLARGE_INTEGER;

  // Device and intermediate drivers should set this pointer to NULL.
  Key : PDWORD
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// Data can be written to an open file using NtWriteFile.
function NtWriteFile(
  FileHandle : THandle;

  // Specifies an optional handle for an event to be set to the signaled
  // state after operation completes.
  // Device and intermediate drivers should set this parameter to NULL.
  Event : THandle;

  // Device and intermediate drivers should set this pointer to NULL.
  ApcRoutine : TIoApcRoutine;

  // Device and intermediate drivers should set this pointer to NULL.
  ApcContext:Pointer;

  // Points to a variable that receives the final completion status and information about the operation.
  var IoStatusBlock : TIoStatusBlock;

  // Pointer to a caller-allocated buffer containing the data to be written to the file.
  Buffer : Pointer;

  // Specifies the size in bytes of the given Buffer.
  // A successful call to NtWriteFile transfers the given number of bytes to the file.
  // If necessary, the length of the file is extended.
  Length : ULONG;

  // Pointer to a variable that specifies the starting byte offset in the file
  // where the read operation will begin.
  // If an attempt is made to read beyond the end of the file, NtReadFile returns an error.
  ByteOffset : PLARGE_INTEGER;

  // Device and intermediate drivers should set this pointer to NULL.
  Key : PDWORD
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtClose closes object handles.
// A named object is not actually deleted until all of its valid handles are closed
// and no referenced pointers remain.
function NtClose( Handle : THandle ) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtCreateDirectoryObject creates or opens a directory object, which is a container for other objects.
function NtCreateDirectoryObject(
  // Points to a variable that receives the directory object handle if the call is successful.
  var DirectoryHandle : THandle;

  // Specifies the type of access that the caller requires to the directory object.
  // This value is compared with the granted access on an existing directory object.
  const DesiredAccess : ACCESS_MASK;

  // Points to a structure that specifies the object's attributes,
  // which has already been initialized with InitializeObjectAttributes.
  var ObjectAttributes : TNtObjectAttributes
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtOpenDirectoryObject opens a directory object, which is a container for other objects.
function NtOpenDirectoryObject(
  // Points to a variable that receives the directory object handle if the call is successful.
  var DirectoryHandle : THandle;

  // Specifies the type of access that the caller requires to the directory object.
  // This value is compared with the granted access on an existing directory object.
  const DesiredAccess : ACCESS_MASK;

  // Points to a structure that specifies the object's attributes,
  // which has already been initialized with InitializeObjectAttributes.
  var ObjectAttributes : TNtObjectAttributes
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// warning: reconstruction
//
// NtQueryDirectoryObject returns various kinds of information about a given directory object
type
  TDirectoryInformationClass = TFileInformationClass; // !!!! It is wrong

  TDirectoryInformationType1 = packed record
    ObjectName     : TNtUnicodeString;
    ObjectTypeName : TNtUnicodeString;
  end;

function NtQueryDirectoryObject(
  const FileHandle : THandle;

  // Points to a caller-allocated buffer or variable that receives the desired information about the directory.
  // The contents of FileInformation are defined by the FileInformationClass parameter.
  DirectoryInformation : Pointer;

  // Specifies the size in bytes of DirectoryInformation,
  // which the caller should set according to the given DirectoryInformationClass.
  const Length : ULONG;

  // Specifies the type of information to be returned about the file.
  DirectoryInformationClass : TDirectoryInformationClass; // ??? = 1

  // False for first call
  RestartScan : Boolean;

  var dwIndex : DWORD;

  var cbBytesReturned : DWORD

) : NTSTATUS; stdcall;


//-------------------------------------------------------------
function NtQueryDirectoryFile(
  const FileHandle : THandle;

  // Specifies an optional handle for an event to be set to the signaled
  // state after operation completes.
  // Device and intermediate drivers should set this parameter to NULL.
  const Event : THandle;

  // set this pointer to NULL.
  ApcRoutine : TIoApcRoutine;

  // set this pointer to NULL.
  ApcContext : Pointer;

  // Points to a variable that receives the final completion status and information about the operation.
  var IoStatusBlock : TIoStatusBlock;

  // Points to a caller-allocated buffer or variable that receives the desired information about file from directory.
  // The contents of FileInformation are defined by the FileInformationClass parameter.
  FileInformation : Pointer;

  // Specifies the size in bytes of FileInformation, which the caller should set according to the given FileInformationClass.
  Length : ULONG;

  // Specifies the type of information to be returned about the file.
  FileInformationClass: TFileInformationClass;

  ReturnSingleEntry : Boolean;

  FileName : PNtUnicodeString;

  // False for first call
  RestartScan : Boolean
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtMakeTemporaryObject changes the attributes of an object to make it temporary.
function NtMakeTemporaryObject( Handle : THandle ) : NTSTATUS; stdcall;

function NtCreateSection(
  SectionHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  var ObjectAttributes: TNtObjectAttributes;
  SectionSize: PLARGE_INTEGER;
  Protect: ULONG;
  Attributes: ULONG;
  FileHandle: THandle
): NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtOpenSection opens a handle for an existing section object.
function NtOpenSection(
  // Points to a variable that will receive the section object handle if this call is successful.
  out SectionHandle : THandle;

  // Specifies a mask representing the requested access to the object.
  const DesiredAccess : ACCESS_MASK;

  // Points to the initialized object attributes of the section to be opened.
  ObjectAttributes : TNtObjectAttributes
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtMapViewOfSection maps a view of a section into the virtual address space of a subject process.
function NtMapViewOfSection(
  // Is the handle returned by a successful call to NtOpenSection.
  SectionHandle : THandle;

  // Is the handle of an opened process object, representing the process for which the view should be mapped.
  ProcessHandle : THandle;

  // Points to a variable that will receive the base address of the view.
  // If the initial value of this argument is nonNULL,
  // the view is allocated starting at the specified virtual address
  // rounded down to the next 64-kilobyte address boundary.
  var BaseAddress : Pointer;

  // Specifies the number of high-order address bits that must be zero
  // in the base address of the section view.
  // The value of this argument must be less than 21 and is used only
  // when the operating system determines where to allocate the view, as when BaseAddress is NULL.
  ZeroBits : ULONG;

  // Specifies the size, in bytes, of the initially committed region of the view.
  // CommitSize is only meaningful for page-file backed sections.
  // For mapped sections, both data and image are always committed at section creation time.
  // This parameter is ignored for mapped files. This value is rounded up to the next host-page-size boundary.
  CommitSize : ULONG;

  // Points to the offset, in bytes, from the beginning of the section to the view.
  // If this pointer is nonNULL, the given value is rounded down to the next allocation granularity size boundary.
  SectionOffset : PLARGE_INTEGER;

  // Points to a variable that will receive the actual size, in bytes, of the view.
  // If the value of this parameter is zero, a view of the section will be mapped starting
  // at the specified section offset and continuing to the end of the section.
  // Otherwise, the initial value of this argument specifies the size of the view,
  // in bytes, and is rounded up to the next host page-size boundary.
  ViewSize : DWORD;

  // Specifies how the view is to be shared by a child process
  // created with a create process operation.
  // Device and intermediate drivers should set this parameter to ViewNone.
  InheritDisposition : SECTION_INHERIT;

  // A set of flags that describes the type of allocation to be performed for the specified region of pages.
  AllocationType : ULONG;

  // Specifies the protection for the region of initially committed pages.
  // Device and intermediate drivers should set this value to PAGE_READWRITE.
  Protect : ULONG
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtUnmapViewOfSection unmaps a view of a section from the virtual address space of a subject process.
function NtUnmapViewOfSection(
  // Specifies an open handle of the process that was passed in a preceding call to NtMapViewOfSection.
  const ProcessHandle : THandle;

  // Points to the base virtual address of the view that is to be unmapped.
  // This value can be any virtual address within the view.
  const BaseAddress : Pointer
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtSetInformationThread can be called to set the priority of a thread for which the caller has a handle.
function NtSetInformationThread(
  // Is the open handle for a thread.
  ThreadHandle : THandle;

  // Is one of the system-defined values ThreadPriority or ThreadBasePriority.
  ThreadInformationClass : THREADINFOCLASS;

  // Points to a variable specifying the information to be set.
  // If ThreadInformationClass is ThreadPriority, this value must be > LOW_PRIORITY and <= HIGH_PRIORITY.
  // If ThreadInformationClass is ThreadBasePriority, this value must fall within the system's
  // valid base priority range and the original priority class for the given thread:
  // that is, if a thread's priority class is variable, that thread's base priority cannot be reset to a real-time priority value and vice versa.
  ThreadInformation : Pointer;

  // Is the size in bytes of ThreadInformation, which must be at least sizeof(KPRIORITY).
  ThreadInformationLength : ULONG
) : NTSTATUS; stdcall;


type
  _KEY_INFORMATION_CLASS = (
    KeyBasicInformation,
    KeyNodeInformation,
    KeyFullInformation,
    KeyNameInformation);
  KEY_INFORMATION_CLASS = _KEY_INFORMATION_CLASS;

  _KEY_VALUE_INFORMATION_CLASS = (
    KeyValueBasicInformation,
    KeyValueFullInformation,
    KeyValuePartialInformation,
    KeyValueFullInformationAlign64,
    KeyValuePartialInformationAlign64);
  KEY_VALUE_INFORMATION_CLASS = _KEY_VALUE_INFORMATION_CLASS;

{function NtCreateKey(
  out KeyHandle: THandle;
  const DesiredAccess : ACCESS_MASK;
  ObjectAttributes : TNtObjectAttributes;
  TitleIndex:ULONG;
  ObjectClass : PNtUnicodeString;
  CreateOptions:ULONG;
   Disposition:PULONG
 ) : NTSTATUS; stdcall;

function NtOpenKey(
  out KeyHandle : THandle;
   DesiredAccess : ACCESS_MASK;
  ObjectAttributes : PNtObjectAttributes
) : NTSTATUS; stdcall;

function NtDeleteKey( KeyHandle : THandle ) : NTSTATUS; stdcall;

function NtEnumerateKey(
  KeyHandle : THandle;
   Index:ULONG;
   KeyInformationClass : KEY_INFORMATION_CLASS;
   KeyInformation : Pointer;
   Length : ULONG;
   ResultLength : PDWORD
) : NTSTATUS; stdcall;

function NtEnumerateValueKey(
   KeyHandle : THandle;
   Index : ULONG;
   KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
   KeyValueInformation : Pointer;
   Length:ULONG;
  ResultLength : PULONG
) : NTSTATUS; stdcall;

function NtFlushKey( KeyHandle : THandle ) : NTSTATUS; stdcall;

function NtQueryKey(
  KeyHandle : THandle;
  KeyInformationClass : KEY_INFORMATION_CLASS;
  KeyInformation:Pointer;
  Length : ULONG;
   ResultLength : PDWORD
) : NTSTATUS; stdcall;

function NtQueryValueKey(
  KeyHandle : THandle;
  ValueName : PNtUnicodeString;
  KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
  KeyValueInformation : Pointer;
  Length : ULONG;
  ResultLength : PDWORD
) : NTSTATUS; stdcall;

function NtSetValueKey(
  KeyHandle : THandle;
  ValueName : PNtUnicodeString;
  TitleIndex : ULONG;
  Typ : ULONG;
  Data : Pointer;
  DataSize : ULONG
) : NTSTATUS; stdcall;}

//-------------------------------------------------------------
// NtOpenSymbolicLinkObject returns a handle to an existing symbolic link.
function NtOpenSymbolicLinkObject(
  // Points to a returned handle for the symbolic link object specified in ObjectAttributes if the call was successful.
  var LinkHandle : THandle;

  // Specifies the type of access that the caller requires to the key.
  // This is most commonly GENERIC_READ access such that the returned handle can be used with NtQuerySymbolicLinkObject.
  const DesiredAccess : ACCESS_MASK;

  // Points to the initialized object attributes for the symbolic link being opened.
  // An ObjectName string for the symbolic link must be specified.
  ObjectAttributes : TNtObjectAttributes
) : NTSTATUS; stdcall;

//-------------------------------------------------------------
// NtQuerySymbolicLinkObject returns a Unicode string containing the target of the symbolic link.
function NtQuerySymbolicLinkObject(
  // Specifies a valid handle to an open symbolic link object obtained by calling ZwOpenSymbolicLinkObject.
  LinkHandle : THandle;

  // Points to an initialized Unicode string that contains the target of the symbolic link,
  // specified by LinkHandle, if the call was successful.
  LinkTarget : TNtUnicodeString;

  // Optionally, points to a unsigned long integer that on input contains the maximum number of bytes
  // to copy into the Unicode string at LinkTarget.
  // On output, the unsigned long integer contains the length of the Unicode string
  // naming the target of the symbolic link.
  ReturnedLength : PDWORD
) : NTSTATUS; stdcall;

function NtLoadDriver(
    DriverServiceName : PNtUnicodeString
) : NTSTATUS; stdcall;

function NtUnloadDriver(
    DriverServiceName : PNtUnicodeString
) : NTSTATUS; stdcall;

{
function NtCreateTimer(
  out TimerHandle : THandle;
  const DesiredAccess : ACCESS_MASK;
  ObjectAttributes : TNtObjectAttributes;
  const TimerType : TIMER_TYPE
) : NTSTATUS; stdcall;

function NtOpenTimer(
  out TimerHandle : THandle;
  const DesiredAccess : ACCESS_MASK;
  ObjectAttributes : TNtObjectAttributes
) : NTSTATUS; stdcall;

function NtCancelTimer(
  TimerHandle : THandle;
  CurrentState : PBoolean
) : NTSTATUS; stdcall;

function NtSetTimer(
  TimerHandle : THandle;
  DueTime : PLARGE_INTEGER;
  TimerApcRoutine : PTIMER_APC_ROUTINE;
  TimerContext : Pointer;
  WakeTimer : Boolean;
  Period : DWORD;
  PreviousState : PBoolean
) : NTSTATUS; stdcall;
}

type
  _OBJECT_INFORMATION_CLASS = (
    ObjectBasicInformation,
    ObjectNameInformation,
    ObjectTypeInformation,
    ObjectAllTypesInformation,
    ObjectHandleInformation);
  OBJECT_INFORMATION_CLASS = _OBJECT_INFORMATION_CLASS;

function NtQueryObject(ObjectHandle: THandle;
    ObjectInformationClass: OBJECT_INFORMATION_CLASS;
    ObjectInformation: Pointer; ObjectInformationLength: ULONG;
    ReturnLength: PULONG): NTSTATUS; stdcall;
function NtSetInformationObject(ObjectHandle: THandle;
    ObjectInformationClass: OBJECT_INFORMATION_CLASS;
    ObjectInformation: Pointer;
    ObjectInformationLength: ULONG): NTSTATUS; stdcall;

type
  _OBJECT_BASIC_INFORMATION = record // Information Class 0
    Attributes: ULONG;
    GrantedAccess: ACCESS_MASK;
    HandleCount: ULONG;
    PointerCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    Reserved: array [0..2] of ULONG;
    NameInformationLength: ULONG;
    TypeInformationLength: ULONG;
    SecurityDescriptorLength: ULONG;
    CreateTime: TLargeInteger;
  end;
  OBJECT_BASIC_INFORMATION = _OBJECT_BASIC_INFORMATION;
  POBJECT_BASIC_INFORMATION = ^OBJECT_BASIC_INFORMATION;

  _OBJECT_NAME_INFORMATION = record // Information Class 1
    ObjectName: UNICODE_STRING;
  end;
  OBJECT_NAME_INFORMATION = _OBJECT_NAME_INFORMATION;
  POBJECT_NAME_INFORMATION = ^OBJECT_NAME_INFORMATION;

  _OBJECT_TYPE_INFORMATION = record // Information Class 2
    Name: UNICODE_STRING;
    ObjectCount: ULONG;
    HandleCount: ULONG;
    Reserved1: array [0..3] of ULONG;
    PeakObjectCount: ULONG;
    PeakHandleCount: ULONG;
    Reserved2: array [0..3] of ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: TGenericMapping;
    ValidAccess: ULONG;
    Unknown: UCHAR;
    MaintainHandleDatabase: ByteBool;
    Reserved3: array [0..1] of UCHAR;
    PoolType: POOL_TYPE;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
  end;
  OBJECT_TYPE_INFORMATION = _OBJECT_TYPE_INFORMATION;
  POBJECT_TYPE_INFORMATION = ^OBJECT_TYPE_INFORMATION;

  _OBJECT_ALL_TYPES_INFORMATION = record // Information Class 3
    NumberOfTypes: ULONG;
    TypeInformation: OBJECT_TYPE_INFORMATION;
  end;
  OBJECT_ALL_TYPES_INFORMATION = _OBJECT_ALL_TYPES_INFORMATION;
  POBJECT_ALL_TYPES_INFORMATION = ^OBJECT_ALL_TYPES_INFORMATION;

  _OBJECT_HANDLE_ATTRIBUTE_INFORMATION = record // Information Class 4
    Inherit: ByteBool;
    ProtectFromClose: ByteBool;
  end;
  OBJECT_HANDLE_ATTRIBUTE_INFORMATION = _OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  POBJECT_HANDLE_ATTRIBUTE_INFORMATION = ^OBJECT_HANDLE_ATTRIBUTE_INFORMATION;

//-------------------------------------------------------------
procedure InitializeObjectAttributes(
  InitializedAttributes : PNtObjectAttributes;
  pObjectName : PNtUnicodeString;
  const uAttributes : ULONG;
  const hRootDirectory : THandle;
  pSecurityDescriptor : PSECURITY_DESCRIPTOR
);

const
  REG_LINK_VALUE_NAME: PWideChar = 'SymbolicLinkValue'; // found by tenox
  REG_OPTION_CREATE_LINK = 2; // this is defined in MSVC 2.0 but not after
  REG_OPTION_OPEN_LINK = $100; // found by tommy

function NtCreateKey(KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES; TitleIndex: ULONG;
    Class_: PUNICODE_STRING; CreateOptions: ULONG; Disposition: PULONG): NTSTATUS; stdcall;
function NtOpenKey(KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
function NtDeleteKey(KeyHandle: THandle): NTSTATUS; stdcall;
function NtFlushKey(KeyHandle: THandle): NTSTATUS; stdcall;
function NtEnumerateKey(KeyHandle: THandle; Index: ULONG;
    KeyInformationClass: KEY_INFORMATION_CLASS; KeyInformation: Pointer;
    KeyInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
function NtSetValueKey(KeyHandle: THandle; ValueName: PUNICODE_STRING;
    TitleIndex: ULONG; Type_: ULONG; Data: Pointer; DataSize: ULONG): NTSTATUS; stdcall;
function NtQueryValueKey(KeyHandle: THandle; ValueName: PUNICODE_STRING;
    KeyValueInformationClass: KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation: Pointer; KeyValueInformationLength: ULONG;
    ResultLength: PULONG): NTSTATUS; stdcall;


//-------------------------------------------------------------
var
  NLS_MB_CODE_PAGE_TAG : Boolean; // TRUE -> Multibyte CP, FALSE -> Singlebyte
//NLS_MB_OEM_CODE_PAGE_TAG : Boolean; // TRUE -> Multibyte CP, FALSE -> Singlebyte

//=============================================================

function NT_SUCCESS(Status: Integer): WordBool; 

const
  ntdll = 'ntdll.dll';
  
implementation

{var
   AnsiCPInfo: TCPInfo;}

function  RtlNtStatusToDosError; external ntdll name 'RtlNtStatusToDosError';

procedure RtlInitString; external ntdll name 'RtlInitString';
procedure RtlInitAnsiString;  external ntdll name 'RtlInitAnsiString';
procedure RtlCopyString;  external ntdll name 'RtlCopyString';
function  RtlUpperChar;  external ntdll name 'RtlUpperChar';
function  RtlCompareString;  external ntdll name 'RtlCompareString';
function  RtlEqualString;  external ntdll name 'RtlEqualString';
procedure RtlUpperString;  external ntdll name 'RtlUpperString';
procedure RtlFreeAnsiString;  external ntdll name 'RtlFreeAnsiString';

procedure RtlInitUnicodeString;  external ntdll name 'RtlInitUnicodeString';
function  RtlAnsiStringToUnicodeString;  external ntdll name 'RtlAnsiStringToUnicodeString';
function  RtlUnicodeStringToAnsiString;  external ntdll name 'RtlUnicodeStringToAnsiString';
function  RtlCompareUnicodeString;  external ntdll name 'RtlCompareUnicodeString';
function  RtlEqualCompareUnicodeString;  external ntdll name 'RtlEqualCompareUnicodeString';
function  RtlPrefixUnicodeString;  external ntdll name 'RtlPrefixUnicodeString';
function  RtlUpcaseUnicodeString;  external ntdll name 'RtlUpcaseUnicodeString';
procedure RtlCopyUnicodeString;  external ntdll name 'RtlCopyUnicodeString';
function  RtlAppendUnicodeStringToString;  external ntdll name 'RtlAppendUnicodeStringToString';
function  RtlAppendUnicodeToString;  external ntdll name 'RtlAppendUnicodeToString';
function  RtlUpcaseUnicodeChar;  external ntdll name 'RtlUpcaseUnicodeChar';
procedure RtlFreeUnicodeString;  external ntdll name 'RtlFreeUnicodeString';
function  RtlxAnsiStringToUnicodeSize;  external ntdll name 'RtlxAnsiStringToUnicodeSize';

function NtCreateFile;  external ntdll name 'NtCreateFile';
function NtOpenFile;  external ntdll name 'NtOpenFile';
function NtQueryInformationFile;  external ntdll name 'NtQueryInformationFile';
function NtSetInformationFile;  external ntdll name 'NtSetInformationFile';
function NtWriteFile;  external ntdll name 'NtWriteFile';
function NtReadFile;  external ntdll name 'NtReadFile';
function NtClose;  external ntdll name 'NtClose';
function NtCreateDirectoryObject;  external ntdll name 'NtCreateDirectoryObject';
function NtOpenDirectoryObject;  external ntdll name 'NtOpenDirectoryObject';
function NtQueryDirectoryObject;  external ntdll name 'NtQueryDirectoryObject';
function NtQueryDirectoryFile; external ntdll name 'NtQueryDirectoryFile';
function NtMakeTemporaryObject;  external ntdll name 'NtMakeTemporaryObject';
function NtCreateSection;  external ntdll name 'NtCreateSection';
function NtOpenSection;  external ntdll name 'NtOpenSection';
function NtMapViewOfSection;  external ntdll name 'NtMapViewOfSection';
function NtUnmapViewOfSection;  external ntdll name 'NtUnmapViewOfSection';
function NtOpenSymbolicLinkObject;  external ntdll name 'NtOpenSymbolicLinkObject';
function NtQuerySymbolicLinkObject;  external ntdll name 'NtQuerySymbolicLinkObject';
function NtSetInformationThread;  external ntdll name 'NtSetInformationThread';
function NtLoadDriver; external ntdll Name 'NtLoadDriver';
function NtUnloadDriver; external ntdll Name 'NtUnloadDriver';

function NtQueryObject; external ntdll name 'NtQueryObject';
function NtSetInformationObject; external ntdll name 'NtSetInformationObject';

function NtCreateKey; external ntdll name 'NtCreateKey';
function NtOpenKey; external ntdll name 'NtOpenKey';
function NtDeleteKey; external ntdll name 'NtDeleteKey';
function NtFlushKey; external ntdll name 'NtFlushKey';
function NtEnumerateKey; external ntdll name 'NtEnumerateKey';
function NtSetValueKey; external ntdll name 'NtSetValueKey';
function NtQueryValueKey; external ntdll name 'NtQueryValueKey';

//-------------------------------------------------------------
{function RtlAnsiStringToUnicodeSize(  NtAnsiString : PNtAnsiString ) : DWORD; stdcall;
begin
  if NtAnsiString=nil then Result := 0
  else if NLS_MB_CODE_PAGE_TAG then Result := RtlxAnsiStringToUnicodeSize(NtAnsiString)
  else Result := (NtAnsiString^.Length+1)*AnsiCPInfo.MaxCharSize;
end;}
//-------------------------------------------------------------
procedure InitializeObjectAttributes(
  InitializedAttributes : PNtObjectAttributes;
  pObjectName : PNtUnicodeString;
  const uAttributes : ULONG;
  const hRootDirectory : THandle;
  pSecurityDescriptor : PSECURITY_DESCRIPTOR
);
begin
  with InitializedAttributes^ do
  begin
    Length := SizeOf(TNtObjectAttributes);
    ObjectName := pObjectName;
    Attributes := uAttributes;
    RootDirectory := hRootDirectory;
    SecurityDescriptor := pSecurityDescriptor;
    SecurityQualityOfService := nil;
  end;
end;

function NT_SUCCESS(Status: Integer): WordBool; 
begin 
  Result := Status >= 0; 
end; 


//=============================================================
{initialization
  Windows.GetCPInfo(CP_ACP,AnsiCPInfo);
  with AnsiCPInfo do NLS_MB_CODE_PAGE_TAG := MaxCharSize<>1;}
end.
