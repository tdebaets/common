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
 * Misc. utility code
 *
 ****************************************************************************)

unit Common2;

interface

// TODO: remove Classes? (initialization/finalization)
uses Windows, Messages, CommCtrl, SysUtils, ShlObj, ShellApi, ActiveX, Classes,
    Math, ComObj, ShFolder, PathFunc, Graphics, WinCrypt, WinSvc, TypInfo,
    Registry, MyRegistry, EZDSLHsh, CmnFunc2;

type
  PPointer = ^Pointer;
  PCardinal = ^Cardinal;
  PComp = ^Comp;
  PPChar = ^PChar;
  PBoolean = ^Boolean;
  PLongBool = ^LongBool;
  PHWND = ^HWND;

type
  TByteSet = set of Byte;

const
  WM_UNINITMENUPOPUP = $00000125;

const
  WM_NCUAHDRAWCAPTION = 174;
  WM_NCUAHDRAWFRAME = 175;

const
  LVM_GETGROUPINFO = LVM_FIRST + 149;
  LVM_ENABLEGROUPVIEW = LVM_FIRST + 157;
  LVM_SETVIEW = LVM_FIRST + 142;

const
  LVIF_GROUPID = $00000100;
  I_GROUPIDCALLBACK = -1;
  I_GROUPIDNONE = -2;
  LVGS_HIDDEN = $00000002;

type
  tagLVGROUP = packed record
    cbSize: UINT;
    mask: UINT;
    pszHeader: PWideChar;
    cchHeader: Integer;
    pszFooter: PWideChar;
    cchFooter: Integer;
    iGroupId: Integer;
    stateMask: UINT;
    state: UINT;
    uAlign: UINT;
  end;
  TLVGroup = tagLVGROUP;

const
  LVGF_STATE = $00000004;
  LVGF_GROUPID = $00000010;

const
  LV_VIEW_DETAILS = $00000001;
  LV_VIEW_ICON = $00000000;
  LV_VIEW_LIST = $00000003;
  LV_VIEW_MAX = $00000004;
  LV_VIEW_SMALLICON = $00000002;
  LV_VIEW_TILE = $00000004;

const
  COLOR_HOTLIGHT = 26;

const
  PSCB_INITIALIZED = 1;
  PSCB_PRECREATE = 2;

const
  PSP_PREMATURE = $00000400;

const
  IDC_HAND = 32649;

const
  SM_CXPADDEDBORDER = 92;

const
  CrLf = #13#10;
  CrLf2 = CrLf + CrLf;
  ChrTab = #9;

const
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $00000020;

const
  MAKE_HRESULT_SUCCESS = (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16);
  MAKE_HRESULT_ERROR = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16);

const
  HEAP_CREATE_ENABLE_EXECUTE = $00040000;
  HEAP_ZERO_MEMORY = $00000008;

const
  MAPVK_VK_TO_VSC    = 0;
  MAPVK_VSC_TO_VK    = 1;
  MAPVK_VK_TO_CHAR   = 2;
  MAPVK_VSC_TO_VK_EX = 3;
  MAPVK_VK_TO_VSC_EX = 4;

const
  INVALID_FILE_ATTRIBUTES = DWORD(-1);

const
  IDTRYAGAIN = 10;
  IDCONTINUE = 11;

const
  ONE_SEC = 1000;

type
  PMethod = ^TMethod;
  TMethodFunc = procedure of object;

type
  TPWideCharArray = array[0..MaxInt div SizeOf(PWideChar) - 1] of PWideChar;
  PPWideCharArray = ^TPWideCharArray;

type
  TCancelResult = (crFail, crSuccess, crCancelled);
  TSuccessOrCancel = crSuccess..crCancelled;

var
  IsWinNT: Boolean;

var
  WindowsTimeFormat: string;

// wrong DLL in Windows.pas
function TransparentBlt(DC: HDC; p2, p3, p4, p5: Integer; DC6: HDC;
    p7, p8, p9, p10: Integer; p11: UINT): BOOL; stdcall;
    external 'msimg32.dll' name 'TransparentBlt';

const
  VAR_FOURDIGITYEARS = $0040;

// wrong function name in ActiveX.pas
function VarBStrFromDate(dateIn: TOleDate; lcid: TLCID; dwFlags: Longint;
    out bstrOut: WideString): HResult; stdcall;
    external 'oleaut32.dll' name 'VarBstrFromDate';

// version in WinSvc.pas doesn't allow nil for lpServiceConfig
function QueryServiceConfigAllowNil(hService: SC_HANDLE;
    lpServiceConfig: PQueryServiceConfig; cbBufSize: DWORD;
    var pcbBytesNeeded: DWORD): BOOL; stdcall;
    external advapi32 name 'QueryServiceConfigA';

function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: PChar;
    out phModule: HMODULE): BOOL; stdcall;
    external Kernel32 name 'GetModuleHandleExA';

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
    dwThreadId: DWORD): THandle; stdcall;
    external Kernel32;

type
  TSetDllDirectory = function(lpPathName: PAnsiChar): BOOL; stdcall;

procedure FreeAndNil(var Obj);
procedure OutException(const E: Exception; ProcName: string);
function ExceptionErrorMessage(ExceptObject: TObject;
    ExceptAddr: Pointer): String; overload;
function GetComMethod(Obj: Pointer; MethodIndex: Integer): Pointer;
function HookComMethod(Obj: Pointer; MethodIndex: Integer; NewAddress: Pointer;
    OldAddress: PPointer; const DebugStr: String = ''): Integer;
function ThisCall(This: Pointer; Func: Pointer; Args: array of const): Integer;

function IIf(Condition: Boolean; const TrueVal, FalseVal: String): String;
function IIfInt(Condition: Boolean; TrueVal, FalseVal: Integer): Integer;
procedure Debug(const Text: String);
procedure DebugFmt(const Fmt: String; const Args: array of const);
procedure DebugWithTID(const Text: String);

function AllocStringRec(const Str: String): Pointer;
procedure FreeStringRec(pRec: Pointer);
function GetString(pRec: Pointer): String;

type
  TQueryInterface = function(This: Pointer; IID: PGUID;
      Obj: Pointer): Integer; stdcall;

function RefIUnknown(const Intf: IUnknown): Pointer;
procedure ReleaseIUnknown(P: Pointer);
procedure InterfaceConnect(const Source: IUnknown; const IID: TIID;
  const Sink: IUnknown; var Connection: Longint);
procedure InterfaceDisconnect(const Source: IUnknown; const IID: TIID;
  var Connection: Longint);

// COM

function CoCreateInstanceAsAdmin(Handle: HWND; const ClassID, IID: TGuid;
    out ppv): HResult;
function OpenCOMObjectKey(Registry: TRegistry; const CLSID: TGUID;
    RegView: TRegView = rvDefault): Boolean;
function GetCOMObjectInprocServerPath(Registry: TMyRegistry): String; overload;
function GetCOMObjectInprocServerPath(const CLSID: TGUID;
    RegView: TRegView = rvDefault): String; overload

// Menus

function GetMenuItemCaption(Menu: HMENU; Item: Integer;
    ByPosition: Boolean): String;
function GetMenuItemPositionFromID(hMenu: HMENU; ID: Cardinal): Integer;
function MenuItemExists(hMenu: HMENU; ID: Cardinal): Boolean;
procedure ClearMenuItems(hMenu: HMENU);
function IsSubmenuItem(hMenu: Integer; uItem: Cardinal;
    ByPosition: Boolean): Boolean;
function IsSeparatorItem(hMenu: Integer; uItem: Cardinal;
    ByPosition: Boolean): Boolean;

// Files

function GetExeDisplayName(const Filename: String): String;
function ExecuteFile(hwnd: HWND; const FileName, Params, DefaultDir: string;
    ShowCmd: Integer): Boolean;
function ExecuteProcess(const FileName, Params, DefaultDir: WideString;
    ShowCmd: Integer; phProc: PHandle): Integer;
function RunCmdLine(const CmdLine: WideString): Boolean;
function GetLongFilename(var Path: String): Boolean;
function IsFileInUse(const Filename: WideString): Boolean;
function NewPathExtractExt(const Filename: String): String;
function FileSize(const Filename: String; var Size: Int64): Boolean;

// Dirs

const
  CSIDL_LOCAL_APPDATA = $0000001c;

function GetShellFolderByCSIDL(Folder: Integer; const Create: Boolean): String;
function GetParentDirectory(const Path: String): String;

// Windows

const
  WS_EX_NOACTIVATE = $8000000;

const
  WC_EDIT = 'Edit';

const
  GA_PARENT = 1;
  GA_ROOT = 2;
  GA_ROOTOWNER = 3;

function GetAncestor (hwnd : Integer; gaFlags : Integer) : Integer; stdcall;
    external user32 name 'GetAncestor'

function GetShellWindow: HWND; stdcall; external user32;

const
  OrigBaseUnitX = 6;
  OrigBaseUnitY = 13;
  BasePixelsPerInch = 96;

function GetDlgUnits(hwnd: Integer): Integer;
procedure CalculateBaseUnitsFromFontHandle(hFont: HFONT; var X, Y: Integer);
procedure CalculateBaseUnitsFromFont(Font: TFont; var X, Y: Integer);
function WindowText(hWnd: HWND): String;
function WindowClassName(hWnd: HWND): String;
procedure WindowTextAndClassName(hWnd: HWND; var Text, ClassName: String);
procedure AutoSizeControl(hwnd: HWND; const Text: String;
    ExtraX, ExtraY: Integer);
function WindowOwnsVisibleWindows(Wnd, hWndIgnore: HWND): Boolean;
function WindowGetVisibleOwnedGrandchild(hWnd: HWND): HWND;
function IsCurrentProcessInForeground: Boolean;
function GetTopFocusableWindow: HWND;
function MAKE_X_Y_LPARAM(x, y: Smallint): LPARAM;

type
  TWndMethod = procedure(var Message: TMessage) of object;

function AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);
procedure DeallocateHWndSafe(var Wnd: HWND);

// Strings

{Splits a string containing designated
separators into tokens and adds
them to MyStringList NOTE: MyStringList
must be Created before being passed to this
procedure and Freed after use}
procedure Split(const S: String; Separator: Char; MyStringList: TStringList);

function HexToInt(hexvar: string): integer;
function FindInArray(const Str: String; const Arr: array of String): Boolean;
function IsPrefix(const Str, Prefix: String): Boolean;
function StripMnemonic(var Str: String): Char;
function IsWhiteSpace(C: Char): Boolean;
function DupeString(const AText: string; ACount: Integer): String;
function ScanDateWithFormat(const S, Format: string; const Separator: Char;
    var Date: TDateTime): Boolean;
function StripLineBreaks(const S: string): string;
function SelectStr(const Str: String;
    const Cases, Outputs: array of String): String;
function SameText(const Str1, Str2: String): Boolean;
function BooleanToStr(B: Boolean): string;
procedure MoveWideChar(const Source; var Dest; Count: Integer);
function IsAllCaps(const Str: String): Boolean;
function ConvertAllCaps(var Str: String): Boolean;
function GetCommandLineArgs(pCmdLine: PChar): PChar;

type
  UTF8String = AnsiString;

function Utf8Decode(const S: UTF8String): WideString;
function Utf8Encode(const WS: WideString): UTF8String;

// DLLs

function GetModuleName(Module: HMODULE): String;
function LockModuleIntoProcess(Module: HMODULE): Boolean;
function GetModuleFromAddress(Address: Pointer): HMODULE;
function GetModuleNameFromAddress(Address: Pointer): String;
function SafeLoadLibrary(const Filename: String;
    ErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE;
function LoadResourceDll(const Path: String): Integer;

// Resources

function LoadResString(hInstance: Integer; ID: Integer): string;
function ParseResURL(var URL: String; const FilenameHint: String;
    var ResourceDll: String): Boolean;
function MAKELANGID (p, s : word) : Integer;
function GetFileVersion(const FileName: string): Integer;
function GetModuleFixedFileInfo(hMod: HMODULE;
    var FixedFileInfo: TVSFixedFileInfo): Boolean;

type
  TCommonIcon = (ciNone, ciProgram, ciWarning, ciQuestion, ciError, ciInfo);

function LoadCommonIcon(Icon: TCommonIcon; X, Y: Integer): THandle;

function DestroyIconSafe(var IconHandle: HICON): Boolean;

// Math

type
  ECryptError = class(Exception);
  
function GetRandomSeed: LongWord; // throws ECryptError
function Remainder(x, y: Integer): Integer;
function Card(var tar; size: Integer): Integer;
function SumWord(const Data: array of Word): Integer;
function IsSubRangeMember(pInfo: PTypeInfo; const Value: Variant): Boolean;
function GetEnumName(pInfo: PTypeInfo; const Value: Variant): String;
procedure IncludeFlag(var Flags: Cardinal; Flag: Cardinal);

// Arrays

type
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;

type
  TStringArray = array of String;
  PStringArray = ^TStringArray;

function CopyStringArray(const C: array of String): TStringArray;
procedure AddToStringArray(var Arr: TStringArray; const Str: String);

// Security

function ConvertSidToStringSid(Sid: PSID; var StringSid: LPTSTR): BOOL; stdcall;

type
  PTOKEN_USER = ^TOKEN_USER;
  _TOKEN_USER = record
    User: TSidAndAttributes;
  end;
  TOKEN_USER = _TOKEN_USER;

function GetProcessSidToken(ProcessHandle: Integer; var Sid: PSID): Boolean;

// Registry

function RegKeyExists(const RootKey: DWORD; const SubKeyName: String): Boolean;
function CreateRegistryLink(const LinkKey, TargetKey: WideString): Boolean;
function RemoveRegistryLink(const LinkKey: WideString): Boolean;
function GetPathFromHandle(hObject: THandle; var Path: WideString): Boolean;
function NormalizeNTUserRegPath(const RegPath: String;
    var NormalizedPath: String): Boolean;

// Controls

function CheckCommonControl(CC: Integer): Boolean;

// Graphics

const
  AC_SRC_OVER = $00;

type
  TBLENDFUNCTION = record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
  end;
  TAlphaBlend = function(
    hdcDest: HDC;                 // handle to destination DC
    nXOriginDest,                 // x-coord of upper-left corner
    nYOriginDest,                 // y-coord of upper-left corner
    nWidthDest,                   // destination width
    nHeightDest: Integer;         // destination height
    hdcSrc:HDC;                   // handle to source DC
    nXOriginSrc,                  // x-coord of upper-left corner
    nYOriginSrc,                  // y-coord of upper-left corner
    nWidthSrc,                    // source width
    nHeightSrc: Integer;          // source height
    blendFunction: TBLENDFUNCTION // alpha-blending function
  ): Boolean; stdcall;

procedure AlphaHighlight(hDC: Integer; R: TRect; AlphaBlend: TAlphaBlend);

// Misc

function IsExtendedKey(VirtualKeyCode: Cardinal): Boolean;
function GetDlgCodeMaskFromKey(VirtualKeyCode: Cardinal): Cardinal;

function ExpandEnvVars(const Str: string): string;

function IsWindowsMeOrHigher: Boolean;
function IsWindows2kOrHigher: Boolean;
function IsWindowsVistaOrHigher: Boolean;
function IsWindows7OrHigher: Boolean;

function GetElapsedTicks(LastTick: Cardinal): Cardinal;
function MsgWaitForObjectWithTimeout(hHandle: THandle;
    dwTimeout: Cardinal): DWORD;

function IsWin32Success(ErrorCode: DWORD): Boolean;

function RectWidth(const R: TRect): Integer;
function RectHeight(const R: TRect): Integer;

function IsValidPtr(Ptr: Pointer): Boolean;
function ComparePointers(P1, P2: Pointer): Integer;
function InterlockedExchangePointer(var Target: Pointer;
    Value: Pointer): Pointer;

procedure FreeAndNilPIDL(var PIDL: PItemIDList);

function GetTaskbarWindow: HWND;
function RegisterTaskbarCreatedMsg: UINT;

procedure GetFormatSettings2;

type
   TString = class(TObject)
   private
     fStr: String;
   public
     constructor Create(const AStr: String) ;
     property Str: String read FStr write FStr;
   end;

type
{ TInterfacedObject provides a threadsafe default implementation
  of IInterface.  You should use TInterfaceObject as the base class
  of objects implementing interfaces.  }
  PNewInterfacedObject = ^TNewInterfacedObject;
  TNewInterfacedObject = class(TObject, IUnknown)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  public
    {procedure AfterConstruction; override;
    procedure BeforeDestruction; override;}
    constructor Create;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  end;

  TNewInterfacedClass = class of TNewInterfacedObject;

  TDummyInterfacedObject = class(TObject, IUnknown)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

type
  TMyHashTable = class(TThreadSafeHashTable)
  protected
     //procedure DisposeDataProc (aData : pointer);
  public
    constructor Create;
  end;

implementation

uses DzURL, ShLwApi, ShlObj2, NativeApi;

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

function GetComMethod(Obj: Pointer; MethodIndex: Integer): Pointer;
var
  PVFuncTable: Pointer;
  TableEntry: Pointer;
begin
  Result := nil;
  try
    PVFuncTable := PPointer(Obj)^;
  except
    Exit;
  end;
  try
    TableEntry := Pointer(Integer(PVFuncTable) + MethodIndex * SizeOf(Pointer));
  except
    Exit;
  end;
  try
    Result := PPointer(TableEntry)^;
  except
    Exit;
  end;
end;

function HookComMethod(Obj: Pointer; MethodIndex: Integer; NewAddress: Pointer;
    OldAddress: PPointer; const DebugStr: String = ''): Integer;
var
  HasDebugStr: Boolean;
  pVFuncTable: Pointer;
  TableEntry: PPointer;
  OrigAddress: Pointer;
  Bytes: Cardinal;
begin
  HasDebugStr := (DebugStr <> '');
  if HasDebugStr then
    DebugWithTID('Hooking ' + DebugStr);
  {Result := 1;
  if OldAddress <> nil then try
    OldAddress^ := nil;
  except
    Exit;
  end;}
  Result := 2;
  try
    pVFuncTable := PPointer(Obj)^;
  except
    Exit;
  end;
  Result := 3;
  try
    TableEntry := PPointer(Integer(pVFuncTable) + MethodIndex * SizeOf(Pointer));
  except
    Exit;
  end;
  Result := 4;
  try
    OrigAddress := PPointer(TableEntry)^;
  except
    Exit;
  end;
  if HasDebugStr then begin
    DebugFmt('Original address: %x, new address: %x',
        [Integer(OrigAddress), Integer(NewAddress)]);
    DebugFmt('Original address located in module: %s',
        [GetModuleNameFromAddress(OrigAddress)]);
  end;
  Result := 5;
  if OrigAddress = NewAddress then begin
    // don't hook twice (prevent endless loop)
    if HasDebugStr then
      Debug('Already hooked, exiting');
    Exit;
  end;
  Result := 6;
  Bytes := 0;
  // For multithreaded COM objects, important to write old function address
  // *before* patching the function table!
  // Also using InterlockedExchangePointer to be really sure that other threads
  // 'see' the old address before we patch.
  if Assigned(OldAddress) then
    InterlockedExchangePointer(OldAddress^, OrigAddress);
  // Finally, patch the function table.
  // Using WriteProcessMemory because (unlike a direct write) it handles the
  // case where the function table resides in memory not marked as writeable.
  if WriteProcessMemory(GetCurrentProcess, TableEntry, @NewAddress,
      SizeOf(Pointer), Bytes) and (Bytes = SizeOf(Pointer)) then begin
    Result := 0;
  end;
end;

function ThisCall(This: Pointer; Func: Pointer; Args: array of const): Integer;
var
  i: Integer;
  pArg: Pointer;
  iArg: Integer;
  VarRec: TVarRec;
begin
  for i := High(Args) downto Low(Args) do begin
    VarRec := TVarRec(Args[i]);
    case VarRec.VType of
      vtInteger:     pArg := @VarRec.VInteger;
      //vtBoolean:     pArg := @VarRec.VBoolean;
      //vtChar:        pArg := @VarRec.VChar;
      //vtExtended:    pArg := VarRec.VExtended;
      vtString:      pArg := @VarRec.VString;
      vtPointer:     pArg := @VarRec.VPointer;
      vtPChar:       pArg := @VarRec.VPChar;
      vtObject:      pArg := @VarRec.VObject;
      vtClass:       pArg := @VarRec.VClass;
      //vtWideChar:    pArg := @VarRec.VWideChar;
      vtPWideChar:   pArg := @VarRec.VPWideChar;
      vtAnsiString:  pArg := @VarRec.VAnsiString;
      //vtCurrency:    pArg := VarRec.VCurrency;
      vtVariant:     pArg := @VarRec.VVariant;
      vtInterface:   pArg := @VarRec.VInterface;
      vtWideString:  pArg := @VarRec.VWideString;
      //vtInt64:       pArg := VarRec.VInt64;
      else
        raise Exception.Create('Unsupported type');
    end;
    iArg := PInteger(pArg)^;
    asm
      push iArg;
    end;
  end;
  asm
    mov ecx, This;
    call Func;
    mov [Result], eax;
  end;
end;

procedure OutException(const E: Exception; ProcName: string);
begin
  OutputDebugString(PChar( 'Exception in ' + ProcName + ': ' + E.ClassName +
      ': ' + E.Message));
end;

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer): String;
var
  Len: Integer;
  Buffer: array[0..1023] of Char;
begin
  Len := SysUtils.ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer,
      SizeOf(Buffer));
  SetString(Result, Buffer, (Len - 1) div SizeOf(Char));
end;

function GetDlgUnits(hwnd: Integer): Integer;
var
  Rect: TRect;
  Res: LongRec;
begin
  Rect.Left := 4;
  Rect.Top := 8;
  Rect.Right := 0;
  Rect.Bottom := 0;
  MapDialogRect(hwnd, Rect);
  Res.Lo := Rect.Left;
  Res.Hi := Rect.Top;
  Result := Integer(Res);
end;

procedure CalculateBaseUnitsFromFontHandle(hFont: HFONT; var X, Y: Integer);
var
  DC: HDC;
  Size: TSize;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, hFont);
    { Based on code from Q145994: }
    GetTextExtentPoint(DC,
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 52, Size);
    X := (Size.cx div 26 + 1) div 2;
    GetTextMetrics(DC, TM);
    Y := TM.tmHeight;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure CalculateBaseUnitsFromFont(Font: TFont; var X, Y: Integer);
begin
  CalculateBaseUnitsFromFontHandle(Font.Handle, X, Y);
end;

function WindowText(hWnd: HWND): String;
var
  TextLen: Longint;
begin
  TextLen := SendMessage(hWnd, WM_GETTEXTLENGTH, 0, 0);
  Result := StringOfChar(#0, TextLen);
  SendMessage(hWnd, WM_GETTEXT, TextLen + 1, Integer(PChar(Result)));
end;

function WindowClassName(hWnd: HWND): String;
var
  ClassName: array[0..MAX_PATH] of Char;
begin
  FillChar(ClassName, SizeOf(ClassName), 0);
  if GetClassName(hWnd, ClassName, SizeOf(ClassName)) > 0 then
    Result := ClassName
  else
    Result := '';
end;

procedure WindowTextAndClassName(hWnd: HWND; var Text, ClassName: String);
begin
  Text := WindowText(hWnd);
  ClassName := WindowClassName(hWnd);
end;

procedure AutoSizeControl(hwnd: HWND; const Text: String;
    ExtraX, ExtraY: Integer);
var
  DC: HDC;
  TextSize: TSize;
  Font, SaveFont: hFont;
begin
  //Text := WindowText(hwnd);
  DC := GetDC(0);
  try
    Font := SendMessage(hwnd, WM_GETFONT, 0, 0);
    SaveFont := SelectObject(DC, Font);
    try
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), TextSize);
      SetWindowPos(hwnd, 0, 0, 0,
        TextSize.cx + ExtraX, TextSize.cy + ExtraY,
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOZORDER);
    finally
      SelectObject(DC, SaveFont);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

type
  TWindowGetVisibleOwnedWindowInfo = record
    Wnd: HWND;
    hWndIgnore: HWND;
    hOwnedWnd: HWND;
  end;
  PWindowGetVisibleOwnedWindowInfo = ^TWindowGetVisibleOwnedWindowInfo;

function WindowGetVisibleOwnedWindowProc(hWin: HWND;
    Info: PWindowGetVisibleOwnedWindowInfo): BOOL; stdcall;
var
  Style: Integer;
begin
  Result := True;
  if (hWin <> Info.hWndIgnore) and IsWindowVisible(hWin) then begin
    if GetWindow(hWin, GW_OWNER) = Info.Wnd then begin
      Style := GetWindowLong(hWin, GWL_STYLE);
      if (Style and WS_POPUPWINDOW <> 0)
          or (Style and WS_OVERLAPPEDWINDOW <> 0) then begin
        Info.hOwnedWnd := hWin;
        Result := False;
      end;
    end;
  end;
end;

function WindowOwnsVisibleWindows(Wnd, hWndIgnore: HWND): Boolean;
var
  ThreadId: Integer;
  Info: TWindowGetVisibleOwnedWindowInfo;
begin
  ThreadId := GetWindowThreadProcessId(Wnd, nil);
  Info.Wnd := Wnd;
  Info.hWndIgnore := hWndIgnore;
  Info.hOwnedWnd := 0;
  EnumThreadWindows(ThreadId, @WindowGetVisibleOwnedWindowProc, Integer(@Info));
  Result := (Info.hOwnedWnd <> 0);
end;

function WindowGetVisibleOwnedGrandchild(hWnd: HWND): HWND;
const
  MaxIterations = 10;
var
  ThreadId: Integer;
  i: Integer;
  Info: TWindowGetVisibleOwnedWindowInfo;
begin
  Result := hWnd;
  ThreadId := GetWindowThreadProcessId(hWnd, nil);
  for i := 1 to MaxIterations do begin
    Info.Wnd := Result;
    Info.hWndIgnore := 0;
    Info.hOwnedWnd := 0;
    EnumThreadWindows(ThreadId, @WindowGetVisibleOwnedWindowProc, Integer(@Info));
    if Info.hOwnedWnd = 0 then
      Break
    else
      Result := Info.hOwnedWnd;
  end;
end;

function IsCurrentProcessInForeground: Boolean;
var
  ForegroundWnd: HWND;
  ProcId: Cardinal;
begin
  Result := False;
  ForegroundWnd := GetForegroundWindow;
  if ForegroundWnd <> 0 then begin
    if GetWindowThreadProcessId(ForegroundWnd, @ProcId) <> 0 then
      Result := (ProcId = GetCurrentProcessId);
  end;
end;

function MAKE_X_Y_LPARAM(x, y: Smallint): LPARAM;
begin
  // Counterpart of the Win32 GET_X_LPARAM/GET_Y_LPARAM macros.
  // Needed to put this in a seperate procedure because directly passing negative
  // values to MAKELPARAM doesn't compile.
  Result := MAKELPARAM(x, y);
end;

function ExecuteFile(hwnd: HWND; const FileName, Params, DefaultDir: string;
    ShowCmd: Integer): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  ZeroMemory(@Sei, Sizeof(Sei));
  Sei.cbSize := Sizeof(Sei);
  Sei.fMask := SEE_MASK_DOENVSUBST {or SEE_MASK_NOCLOSEPROCESS};
  if hwnd = 0 then
    Sei.fMask := Sei.fMask or SEE_MASK_FLAG_NO_UI;
  Sei.Wnd := hwnd;
  Sei.lpFile := PChar(FileName);
  Sei.lpParameters := PChar(Params);
  Sei.lpDirectory := PChar(DefaultDir);
  Sei.nShow := ShowCmd;
  Result := ShellExecuteEx(@Sei);
  //CloseHandle(Sei.hProcess);
  //Result := ShellExecute(0, nil, PChar(FileName), PChar(Params), PChar(DefaultDir), ShowCmd) > 32;
end;

function ExecuteProcess(const FileName, Params, DefaultDir: WideString;
    ShowCmd: Integer; phProc: PHandle): Integer;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: WideString;
  pDefaultDir: PWideChar;
begin
  if Filename <> '' then
    CmdLine := AddQuotes(Filename) + ' ' + Params;
  // CreateProcess fails on a zero length current directory
  if DefaultDir <> '' then
    pDefaultDir := PWideChar(DefaultDir)
  else
    pDefaultDir := nil;
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowCmd;
  end;
  SetLastError(0);
  if CreateProcessW(nil, PWideChar(CmdLine), nil, nil, False, 0, nil,
      pDefaultDir, SUInfo, ProcInfo) then begin
    CloseHandle(ProcInfo.hThread);
    if Assigned(phProc) then
      phProc^ := ProcInfo.hProcess
    else
      CloseHandle(ProcInfo.hProcess);
    Result := 0;
  end
  else
    Result := GetLastError;
end;

function RunCmdLine(const CmdLine: WideString): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  Result := CreateProcessW(nil, PWideChar(CmdLine), nil, nil, False, 0, nil, nil,
      StartupInfo, ProcessInfo);
  if Result then begin
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

function GetTopFocusableWindowProc(hWin: HWND; pResult: PInteger): BOOL; stdcall;
var
  ExStyle: Integer;
  Rect: TRect;
begin
  Result := True;
  if IsWindowVisible(hWin) and IsWindowEnabled(hWin) then begin
    ExStyle := GetWindowLong(hWin, GWL_EXSTYLE);
    if (ExStyle and (WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST)) = 0 then begin
      GetWindowRect(hWin, Rect);
      if (Rect.Right - Rect.Left > 0) and (Rect.Bottom - Rect.Top > 0) then begin
        pResult^ := hWin;
        Result := False;
      end;
    end;
  end;
end;

// this function returns the top z-order window handle that can be passed to
// SetForegroundWindow, and tries to do the same as the old GetNextQueueWindow
// function
// see http://groups.google.com/group/microsoft.public.win32.programmer.ui/browse_frm/thread/4925892d358afb97/2211552ee03e3b4f
function GetTopFocusableWindow: HWND;
begin
  Result := 0;
  EnumWindows(@GetTopFocusableWindowProc, Integer(@Result));
end;

function GetLongPathNameA(lpszShortPath: PChar; lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall; external kernel32;

function GetLongFilename(var Path: String): Boolean;
var
  Res: Integer;
  Buf: array[0..MAX_PATH] of Char;
begin
  Result := False;
  Res := GetLongPathNameA(PChar(Path), Buf, SizeOf(Buf));
  if (Res > 0) and (Res < SizeOf(Buf)) then begin
    SetString(Path, Buf, Res);
    Result := True;
  end;
end;

function IsFileInUse(const Filename: WideString): Boolean;
var
  hFile: THandle;
begin
  hFile := CreateFileW(PWideChar(Filename), GENERIC_READ, 0, nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then
    Result := (GetLastError = ERROR_SHARING_VIOLATION)
  else begin
    CloseHandle(hFile);
    Result := False;
  end;
end;

// like PathExtractExt, but the result doesn't include the separating period.
function NewPathExtractExt(const Filename: String): String;
begin
  Result := PathExtractExt(Filename);
  if Length(Result) > 0 then
    Delete(Result, 1, 1);
end;

function FileSize(const Filename: String; var Size: Int64): Boolean;
var
  hFile: THandle;
  SizeLo, SizeHi: DWORD;
begin
  hFile := CreateFile(PChar(Filename), GENERIC_READ,
      FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile <> INVALID_HANDLE_VALUE then try
    SizeLo := GetFileSize(hFile, @SizeHi);
    Result := not ((SizeLo = INVALID_FILE_SIZE) and (GetLastError <> 0));
    if Result then with TULargeInteger(Size) do begin
      LowPart := SizeLo;
      HighPart := SizeHi;
    end;
  finally
    CloseHandle(hFile);
  end
  else
    Result := False;
end;

//returns the parent directory for the
//provided "path" (file or directory)
function GetParentDirectory(const Path: String): String;
begin
  Result := PathExpand(AddBackSlash(Path) + '..');
end;

function IIf(Condition: Boolean; const TrueVal, FalseVal: String): String;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

function IIfInt(Condition: Boolean; TrueVal, FalseVal: Integer): Integer;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

procedure Debug(const Text: String);
begin
  OutputDebugString(PChar(Text));
end;

procedure DebugFmt(const Fmt: String; const Args: array of const);
begin
  Debug(Format(Fmt, Args));
end;

procedure DebugWithTID(const Text: String);
begin
  DebugFmt('%s - %x', [Text, GetCurrentThreadID]);
end;

type
  PStringRec = ^TStringRec;
  TStringRec = packed record
    Str: String;
  end;

function AllocStringRec(const Str: String): Pointer;
var
  pResult: PStringRec;
begin
  New(pResult);
  pResult.Str := Str;
  Result := pResult;
end;

procedure FreeStringRec(pRec: Pointer);
var
  pStrRec: PStringRec;
begin
  pStrRec := PStringRec(pRec);
  pStrRec.Str := ''; // not really needed, but just to be sure
  Dispose(pStrRec);
end;

function GetString(pRec: Pointer): String;
begin
  Result := PStringRec(pRec).Str;
end;

// Cast an interface to a Pointer such that the reference
// count is incremented and the interface will not be freed
// until you call ReleaseIUnknown.
function RefIUnknown(const Intf: IUnknown): Pointer;
begin
  Intf._AddRef;               // Increment the reference count.
  Result := Pointer(Intf);    // Save the interface pointer.
end;
 
// Release the interface whose value is stored in the pointer P.
procedure ReleaseIUnknown(P: Pointer);
var
  Intf: IUnknown;
begin
  Pointer(Intf) := P;
  // Delphi releases the interface when Intf goes out of scope.
end;

{ Connect an IConnectionPoint interface }

procedure InterfaceConnect(const Source: IUnknown; const IID: TIID;
    const Sink: IUnknown; var Connection: Longint);
var
  CPC: IConnectionPointContainer;
  CP: IConnectionPoint;
begin
  Connection := 0;
  if Succeeded(Source.QueryInterface(IConnectionPointContainer, CPC)) then begin
    if Succeeded(CPC.FindConnectionPoint(IID, CP)) then
      CP.Advise(Sink, Connection);
  end;
end;

{ Disconnect an IConnectionPoint interface }

procedure InterfaceDisconnect(const Source: IUnknown; const IID: TIID;
    var Connection: Longint);
var
  CPC: IConnectionPointContainer;
  CP: IConnectionPoint;
begin
  if Connection <> 0 then begin
    if Succeeded(Source.QueryInterface(IConnectionPointContainer, CPC)) then begin
      if Succeeded(CPC.FindConnectionPoint(IID, CP)) then begin
        if Succeeded(CP.Unadvise(Connection)) then
          Connection := 0;
      end;
    end;
  end;
end;

{procedure TNewInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TNewInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    Error(reInvalidPtr);
end;}

constructor TNewInterfacedObject.Create;
begin
  inherited;
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TNewInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TNewInterfacedObject(Result).FRefCount := 1;
end;

function TNewInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TNewInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TNewInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TDummyInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TDummyInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TDummyInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;

constructor TMyHashTable.Create;
var
  Table: THashTable;
begin
  inherited Create(True);
  Table := AcquireAccess;
  try
    //Table.DisposeData := DisposeDataProc;
    Table.IgnoreCase := True;
  finally
    ReleaseAccess;
  end;
end;

type
  PBindOpts3 = ^TBindOpts3;
  tagBIND_OPTS3 = record
    cbStruct: DWORD;
    grfFlags: DWORD;
    grfMode: DWORD;
    dwTickCountDeadline: DWORD;
    dwTrackFlags: DWORD;
    dwClassContext: DWORD;
    locale: LCID;
    pServerInfo: Pointer; //DUMMY!! //COSERVERINFO * ;
    hwnd: HWND;
  end;
  TBindOpts3 = tagBIND_OPTS3;
  BIND_OPTS3 = TBindOpts3;

function NewCoGetObject(pazName: PWideChar; pBindOptions: PBindOpts3;
    const iid: TIID; out ppv): HResult; stdcall; external 'ole32.dll'
    name 'CoGetObject';

function CoCreateInstanceAsAdmin(Handle: HWND; const ClassID, IID: TGuid;
    out ppv): HResult;
var
  StrCLSID: array[0..50] of WideChar;
  BindOpts: TBindOpts3;
  MonikerName: WideString;
begin
  StringFromGUID2(ClassID, StrCLSID, SizeOf(StrCLSID) div SizeOf(WideChar));
  ZeroMemory(@BindOpts, Sizeof(TBindOpts3));
  BindOpts.cbStruct := Sizeof(TBindOpts3);
  BindOpts.hwnd := Handle;
  BindOpts.dwClassContext := CLSCTX_LOCAL_SERVER;
  MonikerName := 'Elevation:Administrator!new:' + StrCLSID;
  Result := NewCoGetObject(PWideChar(MonikerName), @BindOpts, IID, ppv);
end;

function OpenCOMObjectKey(Registry: TRegistry; const CLSID: TGUID;
    RegView: TRegView = rvDefault): Boolean;
var
  KeyHandle: HKEY;
begin
  Result := False;
  if RegOpenKeyExView(RegView, HKEY_CLASSES_ROOT,
      PChar('CLSID\' + GUIDToString(CLSID)), 0, KEY_READ,
      KeyHandle) <> ERROR_SUCCESS then
    Exit;
  Registry.CloseKey;
  // Small hack because TRegistry doesn't allow us to pass a custom
  // samDesired parameter (to access the 64-bit registry view) for the
  // RegOpenKeyEx call. So we open the key ourselves first and then set the
  // returned handle as RootKey so that we can still use
  // TMyRegistry.ReadExpandStringSafe.
  Registry.RootKey := KeyHandle;
  Result := Registry.OpenKeyReadOnly('');
end;

function GetCOMObjectInprocServerPath(Registry: TMyRegistry): String; overload;
begin
  if Registry.OpenKeyReadOnly('InprocServer32') then
    Result := RemoveQuotes(Registry.ReadExpandStringSafe('', ''))
  else
    Result := '';
end;

function GetCOMObjectInprocServerPath(const CLSID: TGUID;
    RegView: TRegView = rvDefault): String; overload;
var
  Registry: TMyRegistry;
begin
  Registry := TMyRegistry.Create;
  try
    if OpenCOMObjectKey(Registry, CLSID, RegView) then
      Result := GetCOMObjectInprocServerPath(Registry)
    else
      Result := '';
  finally
    FreeAndNil(Registry);
  end;
end;

function GetMenuItemCaption(Menu: HMENU; Item: Integer;
    ByPosition: Boolean): String;
var
  MenuItemInfo: TMenuItemInfo;
  Buffer: array[0..255] of Char;
begin
  FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_TYPE;
  MenuItemInfo.dwTypeData := Buffer;
  MenuItemInfo.cch := SizeOf(Buffer);
  if GetMenuItemInfo(Menu, Item, ByPosition, MenuItemInfo) and
    (MenuItemInfo.fType = MFT_STRING) then
    Result := Buffer
  else
    Result := '';
end;

function GetMenuItemPositionFromID(hMenu: HMENU; ID: Cardinal): Integer;
var
  ItemCount, i: Integer;
begin
  Result := -1;
  ItemCount := GetMenuItemCount(hMenu);
  if ItemCount < 0 then
    Exit;
  for i := 0 to ItemCount - 1 do begin
    if GetMenuItemID(hMenu, i) = ID then begin
      Result := i;
      Exit;
    end;
  end;
end;

function MenuItemExists(hMenu: HMENU; ID: Cardinal): Boolean;
var
  MenuItem: TMenuItemInfo;
begin
  FillChar(MenuItem, SizeOf(MenuItem), 0);
  MenuItem.cbSize := SizeOf(MenuItem);
  Result := GetMenuItemInfo(hMenu, ID, False, MenuItem);
end;

procedure ClearMenuItems(hMenu: HMENU);
begin
  while GetMenuItemCount(hMenu) > 0 do
    DeleteMenu(hMenu, 0, MF_BYPOSITION);
end;

function IsSubmenuItem(hMenu: Integer; uItem: Cardinal;
    ByPosition: Boolean): Boolean;
var
  ItemInfo: TMenuItemInfo;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.cbSize := SizeOf(ItemInfo);
  ItemInfo.fMask := MIIM_SUBMENU;
  GetMenuItemInfo(hMenu, uItem, ByPosition, ItemInfo);
  Result := (ItemInfo.hSubMenu <> 0);
end;

function IsSeparatorItem(hMenu: Integer; uItem: Cardinal;
    ByPosition: Boolean): Boolean;
var
  ItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.cbSize := SizeOf(ItemInfo);
  ItemInfo.fMask := MIIM_TYPE;
  if not GetMenuItemInfo(hMenu, uItem, ByPosition, ItemInfo) then
    Exit;
  Result := ((ItemInfo.fType and MFT_SEPARATOR) <> 0);
end;

function GetExeDisplayName(const Filename: String): String;
var
  Ext: String;
begin
  if Filename = '' then begin
    Result := '';
    Exit;
  end;
  Result := ExtractFileName(Filename);
  Ext := ExtractFileExt(Filename);
  Result := Copy(Result, 1, Length(Result) - Length(Ext));
  if Result = '' then
    Exit;
  if AnsiCompareStr(Result, AnsiUpperCase(Result)) = 0 then
    Result := AnsiLowerCase(Result);
  if AnsiCompareStr(Result, AnsiLowerCase(Result)) = 0 then
    Result := AnsiUpperCase(Result[1])
      + AnsiLowerCase(Copy(Result, 2, Length(Result) - 1));
  if Ext <> '' then
    Result := Result + AnsiLowerCase(Ext);
end;

function GetShellFolderByCSIDL(Folder: Integer; const Create: Boolean): String;
const
  CSIDL_FLAG_CREATE = $8000;
  SHGFP_TYPE_CURRENT = 0;
var
  Res: HRESULT;
  Buf: array[0..MAX_PATH-1] of Char;
  osvi: TOsVersionInfo;
begin
  if Create then
    Folder := Folder or CSIDL_FLAG_CREATE;

  { Work around a nasty bug in Windows Vista (still present in SP1) and
    Windows Server 2008: When a folder ID resolves to the root directory of a
    drive ('X:\') and the CSIDL_FLAG_CREATE flag is passed, SHGetFolderPath
    fails with code 0x80070005.
    So on Vista only, first try calling the function without CSIDL_FLAG_CREATE.
    If and only if that fails, call it again with the flag.
    Note: The calls *must* be issued in this order; if it's called with the
    flag first, it seems to permanently cache the failure code, causing future
    calls that don't include the flag to fail as well. }
  Res := E_FAIL;  { always issue the call below }
  if Folder and CSIDL_FLAG_CREATE <> 0 then begin
    FillChar(osvi, SizeOf(osvi), 0);
    osvi.dwOSVersionInfoSize := SizeOf(osvi);
    GetVersionEx(osvi);
    if osvi.dwMajorVersion = 6 then
      Res := SHGetFolderPath(0, Folder and not CSIDL_FLAG_CREATE, 0,
          SHGFP_TYPE_CURRENT, Buf);
  end;

  if Res <> S_OK then
    Res := SHGetFolderPath(0, Folder, 0, SHGFP_TYPE_CURRENT, Buf);
  if Res = S_OK then
    Result := RemoveBackslashUnlessRoot(PathExpand(Buf))
  else begin
    Result := '';
    {LogFmt('Warning: SHGetFolderPath failed with code 0x%.8x on folder 0x%.4x',
      [Res, Folder]);}
  end;
end;


const
  InstanceCount = 313;

{ Object instance management }

type
  PObjectInstance = ^TObjectInstance;
  TObjectInstance = packed record
    Code: Byte;
    Offset: Integer;
    case Integer of
      0: (Next: PObjectInstance);
      1: (Method: TWndMethod);
  end;

type
  PInstanceBlock = ^TInstanceBlock;
  TInstanceBlock = packed record
    Next: PInstanceBlock;
    Code: array[1..2] of Byte;
    WndProcPtr: Pointer;
    Instances: array[0..InstanceCount] of TObjectInstance;
  end;

var
  InstBlockList: PInstanceBlock;
  InstFreeList: PObjectInstance;

{ Standard window procedure }
{ In    ECX = Address of method pointer }
{ Out   EAX = Result }

function StdWndProc(Window: HWND; Message, WParam: Longint;
    LParam: Longint): Longint; stdcall; assembler;
asm
        XOR     EAX,EAX
        PUSH    EAX
        PUSH    LParam
        PUSH    WParam
        PUSH    Message
        MOV     EDX,ESP
        MOV     EAX,[ECX].Longint[4]
        CALL    [ECX].Pointer
        ADD     ESP,12
        POP     EAX
end;

{ Allocate an object instance }
function CalcJmpOffset(Src, Dest: Pointer): Longint;
begin
  Result := Longint(Dest) - (Longint(Src) + 5);
end;

function MakeObjectInstance(Method: TWndMethod): Pointer;
const
  BlockCode: array[1..2] of Byte = (
    $59,       { POP ECX }
    $E9);      { JMP StdWndProc }
  PageSize = 4096;
var
  Block: PInstanceBlock;
  Instance: PObjectInstance;
begin
  if InstFreeList = nil then begin
    Block := VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Block^.Next := InstBlockList;
    Move(BlockCode, Block^.Code, SizeOf(BlockCode));
    Block^.WndProcPtr := Pointer(CalcJmpOffset(@Block^.Code[2], @StdWndProc));
    Instance := @Block^.Instances;
    repeat
      Instance^.Code := $E8;  { CALL NEAR PTR Offset }
      Instance^.Offset := CalcJmpOffset(Instance, @Block^.Code);
      Instance^.Next := InstFreeList;
      InstFreeList := Instance;
      Inc(Longint(Instance), SizeOf(TObjectInstance));
    until Longint(Instance) - Longint(Block) >= SizeOf(TInstanceBlock);
    InstBlockList := Block;
  end;
  Result := InstFreeList;
  Instance := InstFreeList;
  InstFreeList := Instance^.Next;
  Instance^.Method := Method;
end;

{ Free an object instance }

procedure FreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then begin
    PObjectInstance(ObjectInstance)^.Next := InstFreeList;
    InstFreeList := ObjectInstance;
  end;
end;


var
  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindow');

function AllocateHWnd(Method: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName,
      TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
      '', WS_POPUP {!!!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(Method) then
    SetWindowLong(Result, GWL_WNDPROC, Longint(MakeObjectInstance(Method)));
end;

procedure DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  DestroyWindow(Wnd);
  if Instance <> @DefWindowProc then
    FreeObjectInstance(Instance);
end;

procedure DeallocateHWndSafe(var Wnd: HWND);
begin
  if Wnd <> 0 then begin
    DeallocateHWnd(Wnd);
    Wnd := 0;
  end;
end;

function GetNextToken(const S: String; Separator: Char;
    var StartPos: Integer): String;
var
  Index: Integer;
begin
  Result := '';
  {Step over repeated separators}
  while (S[StartPos] = Separator) and (StartPos <= Length(S)) do
    StartPos := StartPos + 1;
  if StartPos > Length(S) then
    Exit;
  {Set Index to StartPos}
  Index := StartPos;
  {Find the next Separator}
  while (S[Index] <> Separator) and (Index <= Length(S)) do
    Index := Index + 1;
  {Copy the token to the Result}
  Result := Copy(S, StartPos, Index - StartPos);
  {SetStartPos to next Character after the Separator}
  StartPos := Index + 1;
end;

procedure Split(const S: String; Separator: Char; MyStringList: TStringList);
var
  Start: Integer;
begin
  Start := 1;
  while Start <= Length(S) do
    MyStringList.Add(GetNextToken(S, Separator, Start));
end;

function AddToken(const aToken, S: String; Separator: Char;
    StringLimit: Integer): String;
begin
  if Length(aToken) + Length(S) < StringLimit then begin
    {Add a separator unless the Result string is empty}
    if S = '' then
      Result := ''
    else
      Result := S + Separator;
    {Add the token}
    Result := Result + aToken;
  end
  else begin
    {if the StringLimit would be exceeded, raise an exception}
    raise Exception.Create('Cannot add token');
  end;
end;

function HexToInt(hexvar: String): Integer;
begin
  HexToInt := StrToInt('$' + hexvar);
end; 

function FindInArray(const Str: String; const Arr: array of String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(Arr) to High(Arr) do begin
    if Str = Arr[i] then
      Exit;
  end;
  Result := False;
end;

function IsPrefix(const Str, Prefix: String): Boolean;
begin
  Result := (StrLIComp(PChar(Str), PChar(Prefix), Length(Prefix)) = 0);
end;

function StripMnemonic(var Str: String): Char;
var
  pStr: PChar;
begin
  // directly using PChar(Str) causes problems later on (null char)
  pStr := PChar(Str);
  Result := SHStripMneumonic(pStr);
  Str := pStr;
end;

function IsWhiteSpace(C: Char): Boolean;
begin
  Result := (Ord(c) <= 32);
end;

function DupeString(const AText: string; ACount: Integer): String;
var
  i: Integer;
begin
  Result := '';
  if ACount < 1 then
    Exit;
  for i := 1 to ACount do
    Result := Result + AText;
end;

{ String to date/time conversions }

function CurrentYear: Word;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;

type
  TDateOrder = (doMDY, doDMY, doYMD);

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; var Pos: Integer;
    var Number: Word; var CharCount: Byte): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) and (N < 1000) do begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then begin
    Inc(Pos);
    Result := True;
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := doMDY;
  I := 1;
  while I <= Length(DateFormat) do begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
  Result := doMDY;
end;

procedure ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not (S[Pos] in ['0'..'9']) do begin
    if S[Pos] in LeadBytes then
      Inc(Pos);
    Inc(Pos);
  end;
end;

function GetEraYearOffset(const Name: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EraNames) to High(EraNames) do begin
    if EraNames[I] = '' then
      Break;
    if AnsiStrPos(PChar(EraNames[I]), PChar(Name)) <> nil then begin
      Result := EraYearOffsets[I];
      Exit;
    end;
  end;
end;

function ScanDateWithFormat(const S, Format: string; const Separator: Char;
    var Date: TDateTime): Boolean;
var
  DateOrder: TDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  EraName : string;
  EraYearOffset: Integer;
  CenturyBase: Integer;
  Pos: Integer;

  function EraToYear(Year: Integer): Integer;
  begin
    if SysLocale.PriLangID = LANG_KOREAN then begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
    Result := Year + EraYearOffset;
  end;

begin
  Y := 0;
  M := 0;
  D := 0;
  YearLen := 0;
  Pos := 1;
  Result := False;
  DateOrder := GetDateOrder(Format);
  EraYearOffset := 0;
  if Format[1] = 'g' then begin  // skip over prefix text
    ScanToNumber(S, Pos);
    EraName := Trim(Copy(S, 1, Pos-1));
    EraYearOffset := GetEraYearOffset(EraName);
  end
  else begin
    if AnsiPos('e', Format) > 0 then
      EraYearOffset := EraYearOffsets[1];
  end;
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, Separator) and
      ScanNumber(S, Pos, N2, L2)) then
    Exit;
  if ScanChar(S, Pos, Separator) then begin
    if not ScanNumber(S, Pos, N3, L3) then
      Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      doYMD: begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else if (YearLen <= 2) then begin
      CenturyBase := CurrentYear - TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end
  else begin
    Y := CurrentYear;
    if DateOrder = doDMY then begin
      D := N1;
      M := N2;
    end
    else begin
      M := N1;
      D := N2;
    end;
  end;
  try
    Date := EncodeDate(Y, M, D);
    Result := True;
  except
    on E: EConvertError do
      Result := False;
    else raise;
  end;
end;

function StripLineBreaks(const S: string): string;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do begin
    case Result[i] of
      #10, #13:
        Delete(Result, i, 1);
    end;
  end;
end;

function SelectStr(const Str: String;
    const Cases, Outputs: array of String): String;
var
  LowStr: String;
  i: Integer;
begin
  LowStr := LowerCase(Str);
  for i := 0 to High(Cases) do begin
    if LowStr = Cases[i] then begin
      Result := Outputs[i];
      Exit;
    end;
  end;
  Result := Outputs[High(Outputs)];
end;

function SameText(const Str1, Str2: String): Boolean;
begin
  Result := (CompareText(Str1, Str2) = 0);
end;

function BooleanToStr(B: Boolean): string;
const
  Bools: array [Boolean] of string = ('False', 'True');
begin
  Result := Bools[B];
end;

procedure MoveWideChar(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count * SizeOf(WideChar));
end;

function IsAllCaps(const Str: String): Boolean;
begin
  // case sensitive text comparison
  Result := (AnsiCompareStr(Str, AnsiUpperCase(Str)) = 0);
end;

function ConvertAllCaps(var Str: String): Boolean;
const
  NonWhitespaceChars = ['''', '´', '`'];
var
  i: Integer;
  Whitespace: Boolean;
begin
  if (Str <> '') then begin
    Result := True;
    Str := AnsiLowerCase(Str);
    Whitespace := True;
    for i := 1 to Length(Str) do begin
      //if not (Str[i] in ['a'..'z']) then
      if not ((Str[i] in NonWhitespaceChars) or IsCharAlpha(Str[i])) then
        Whitespace := True
      else if Whitespace then begin
        Str[i] := UpCase(Str[i]);
        Whitespace := False;
      end;
    end;
  end
  else
    Result := False;
end;

function GetCommandLineArgs(pCmdLine: PChar): PChar;
begin
  Result := pCmdLine;
  // This is based on Wine's implementation of CommandLineToArgvW
  if Result^ = '"' then begin
    // The executable path ends at the next quote, no matter what
    Inc(Result);
    while Result^ <> #0 do begin
      if Result^ = '"' then begin
        Inc(Result);
        Break;
      end;
      Inc(Result);
    end;
  end
  else begin
    // The executable path ends at the next space, no matter what
    while (Result^ <> #0) and (Result^ <> ' ') and (Result^ <> ChrTab) do
      Inc(Result);
  end;
  // skip to the first argument, if any
  while (Result^ = ' ') or (Result^ = ChrTab) do
    Inc(Result);
end;

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar;
    SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;
  count := 0;
  i := 0;
  if Dest <> nil then begin
    while (i < SourceChars) and (count < MaxDestBytes) do begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then begin
        if count + 3 > MaxDestBytes then
          Break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count + 1] := Char($80 or ((c shr 6) and $3F));
        Dest[count + 2] := Char($80 or (c and $3F));
        Inc(count, 3);
      end
      else begin //  $7F < Source[i] <= $7FF
        if count + 2 > MaxDestBytes then
          Break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count + 1] := Char($80 or (c and $3F));
        Inc(count, 2);
      end;
    end;
    if count >= MaxDestBytes then
      count := MaxDestBytes - 1;
    Dest[count] := #0;
  end
  else begin
    while i < SourceChars do begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count + 1; // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar;
    SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then begin
    while (i < SourceBytes) and (count < MaxDestChars) do begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then
      count := MaxDestChars - 1;
    Dest[count] := #0;
  end
  else begin
    while (i < SourceBytes) do begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count + 1;
end;

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp) + 1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then
    Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp) + 1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function GetModuleName(Module: HMODULE): String;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName,
      Windows.GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;

type
  TGetModuleHandleExA = function(dwFlags: DWORD; lpModuleName: PAnsiChar;
    out phModule: HMODULE): BOOL; stdcall;

const
  GET_MODULE_HANDLE_EX_FLAG_PIN = $00000001;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS = $00000004;

function LockModuleIntoProcess(Module: HMODULE): Boolean;
var
  ModuleName: String;
  GetModuleHandleExA: TGetModuleHandleExA;
  hMod: HMODULE;
begin
  ModuleName := GetModuleName(Module);
  @GetModuleHandleExA := GetProcAddress(GetModuleHandle(kernel32),
      'GetModuleHandleExA');
  if @GetModuleHandleExA <> nil then begin
    Result := GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_PIN,
        PChar(ModuleName), hMod);
  end
  else begin
    // prevent unloading by incrementing refcount
    Result := (LoadLibrary(PChar(ModuleName)) <> 0);
  end;
end;

function GetModuleFromAddress(Address: Pointer): HMODULE;
var
  GetModuleHandleExA: TGetModuleHandleExA;
begin
  Result := 0;
  @GetModuleHandleExA := GetProcAddress(GetModuleHandle(kernel32),
      'GetModuleHandleExA');
  if @GetModuleHandleExA <> nil then
    GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, Address, Result);
end;

function GetModuleNameFromAddress(Address: Pointer): String;
var
  hMod: HMODULE;
begin
  hMod := GetModuleFromAddress(Address);
  if hMod <> 0 then
    Result := GetModuleName(hMod)
  else
    Result := '(unknown)';
end;

var
  SetDllDirectory: TSetDllDirectory = nil;

function SafeLoadLibrary(const Filename: String;
    ErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE;
begin
  if not Assigned(@SetDllDirectory) then begin
    @SetDllDirectory := GetProcAddress(GetModuleHandle(Kernel32),
        'SetDllDirectoryA');
  end;
  if Assigned(@SetDllDirectory) then begin
    // remove the current dir from the default DLL search order (for safety)
    SetDllDirectory('');
  end;
  Result := CmnFunc2.SafeLoadLibrary(Filename, ErrorMode);
end;

function IsExtendedKey(VirtualKeyCode: Cardinal): Boolean;
begin
  // http://www.ffuts.org/blog/mapvirtualkey-getkeynametext-and-a-story-of-how-to/
  // as far as I know, there's no other way than to hardcode the extended keys
  case VirtualKeyCode of
    VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, // arrow keys
    VK_PRIOR, VK_NEXT, // page up and page down
    VK_END, VK_HOME,
    VK_INSERT, VK_DELETE,
    VK_DIVIDE, // numpad slash
    VK_NUMLOCK:
      Result := True;
    else
      Result := False;
  end;
end;

function GetDlgCodeMaskFromKey(VirtualKeyCode: Cardinal): Cardinal;
begin
  Result := 0;
  case VirtualKeyCode of
    VK_TAB:
      Result := DLGC_WANTTAB;
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
      Result := DLGC_WANTARROWS;
    VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
      Result := DLGC_WANTALLKEYS;
  end;
end;

function ExpandEnvVars(const Str: string): string;
var
  BufSize: Integer; // size of expanded string
begin
  // Get required buffer size 
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  if BufSize > 0 then begin
    // Read expanded string into result string
    SetLength(Result, BufSize); 
    ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
    Result := Trim(Result); // required; otherwise Length will return the wrong value
  end
  else begin
    // Trying to expand empty string
    Result := '';
  end;
end;

function IsWindowsMeOrHigher: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  if IsWinNT then
    Result := False
  else begin
    FillChar(VerInfo, SizeOf(VerInfo), 0);
    VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
    GetVersionEx(VerInfo);
    Result := ((VerInfo.dwMajorVersion = 4) and (VerInfo.dwMinorVersion >= 90))
        or (VerInfo.dwMajorVersion > 4); // shouldn't happen but check anyway
  end;
end;

function IsWindows2kOrHigher: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  if IsWinNT then begin
    FillChar(VerInfo, SizeOf(VerInfo), 0);
    VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
    GetVersionEx(VerInfo);
    Result := (VerInfo.dwMajorVersion >= 5);
  end
  else
    Result := False;
end;

function IsWindowsVistaOrHigher: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  if IsWinNT then begin
    FillChar(VerInfo, SizeOf(VerInfo), 0);
    VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
    GetVersionEx(VerInfo);
    Result := (VerInfo.dwMajorVersion >= 6);
  end
  else
    Result := False;
end;

function IsWindows7OrHigher: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  if IsWinNT then begin
    FillChar(VerInfo, SizeOf(VerInfo), 0);
    VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
    GetVersionEx(VerInfo);
    Result := ((VerInfo.dwMajorVersion = 6) and (VerInfo.dwMinorVersion >= 1))
        or (VerInfo.dwMajorVersion > 6);
  end
  else
    Result := False;
end;

function LoadResourceDll(const Path: String): Integer;
begin
  Result := LoadLibraryEx(PChar(Path), 0,
      LOAD_LIBRARY_AS_DATAFILE or LOAD_LIBRARY_AS_IMAGE_RESOURCE);
end;

function LoadResString(hInstance: Integer; ID: Integer): string;
var
  Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, LoadString(hInstance, ID, Buffer, SizeOf(Buffer)));
end;

function ParseResURL(var URL: String; const FilenameHint: String;
    var ResourceDll: String): Boolean;
const
  ResIdentifier = 'res://';
var
  Components: TStringList;
  Filename, ResId: String;
  ResIdNum: Integer;
  hDll: Integer;
  ResString: String;
begin
  if not IsPrefix(URL, ResIdentifier) then
    Result := True
  else begin
    Result := False;
    Delete(URL, 1, Length(ResIdentifier));
    Components := TStringList.Create;
    try
      Split(URL, '/', Components);
      if (Components.Count < 2) or (Components.Count > 3) then
        Exit;
      Filename := URLDecode(Components[0]);
      ResId := URLDecode(Components[Components.Count - 1]);
    finally
      FreeAndNil(Components);
    end;
    if (Trim(Filename) = '') or (Trim(ResId) = '') then
      Exit;
    if ResId[1] <> '#' then
      Exit; // string resource identifiers can only be numerical
    Delete(ResId, 1, 1);
    ResIdNum := StrToIntDef(ResId, -1);
    if ResIdNum = -1 then
      Exit;
    hDll := LoadResourceDll(Filename);
    if (hDll = 0) and (FilenameHint <> '') then begin
      {OldCurDir := GetCurrentDir;
      if not SetCurrentDir(DirectoryHint) then
        Exit;
      try}
        hDll := LoadResourceDll(FilenameHint);
      {finally
        SetCurrentDir(OldCurDir);
      end;}
    end;
    if hDll = 0 then
      Exit;
    try
      ResString := LoadResString(hDll, ResIdNum);
      if Trim(ResString) <> '' then begin
        StripMnemonic(ResString);
        URL := ResString;
        ResourceDll := Filename;
        Result := True;
      end;
    finally
      FreeLibrary(hDll);
    end;
  end;
end;

function MAKELANGID (p, s : word) : Integer;
begin
  Result := (s shl 10) or p
end;

function GetFileVersion(const FileName: string): Integer;
var
  VersionInfoSize, Temp, VersionValueSize: DWORD;
  VersionInfo: Pointer;
  VersionValue: PVSFixedFileInfo;
begin
  Result := 0;
  VersionInfoSize := GetFileVersionInfoSize(PChar(FileName), Temp);
  if VersionInfoSize = 0 then
    Exit;
  GetMem(VersionInfo, VersionInfoSize);
  try
    GetFileVersionInfo(PChar(FileName), 0, VersionInfoSize, VersionInfo);
    VerQueryValue(VersionInfo, '\', Pointer(VersionValue), VersionValueSize);
    Result := VersionValue.dwFileVersionMS;
  finally
    FreeMem(VersionInfo, VersionInfoSize);
  end;
end;

function GetModuleFixedFileInfo(hMod: HMODULE;
    var FixedFileInfo: TVSFixedFileInfo): Boolean;

  function WideStrsEqual(P1, P2: PWideChar): Boolean;
    function WideUpCase(C: WideChar): WideChar;
    begin
      Result := C;
      if (Result >= 'a') and (Result <= 'z') then
        Dec(Result, Ord('a') - Ord('A'));
    end;
  begin
    while True do begin
      if WideUpCase(P1^) <> WideUpCase(P2^) then begin
        Result := False;
        Exit;
      end;
      if P1^ = #0 then
        Break;
      Inc(P1);
      Inc(P2);
    end;
    Result := True;
  end;

  procedure BumpToDWordBoundary(var P: Pointer);
  begin
    if Cardinal(P) and 3 <> 0 then
      Cardinal(P) := (Cardinal(P) or 3) + 1;
  end;

  function QueryValue(P: Pointer; Path: PWideChar; var Buf: Pointer;
      var BufLen: Cardinal): Boolean;
  var
    EndP: Pointer;
    ValueLength: Cardinal;
  begin
    Result := False;
    Cardinal(EndP) := Cardinal(P) + PWord(P)^;
    Inc(PWord(P));
    ValueLength := PWord(P)^;
    Inc(PWord(P));
    Inc(PWord(P));
    if WideStrsEqual(PWideChar(P), Path) then begin
      Inc(PWideChar(P), lstrlenW(P) + 1);
      BumpToDWordBoundary(P);
      Inc(Path, lstrlenW(Path) + 1);
      if Path^ = #0 then begin
        { Found the requested value }
        Buf := P;
        BufLen := ValueLength;
        Result := True;
      end
      else begin
        { Handle children.
          Note: Like VerQueryValue, we always treat ValueLength as a byte count
          when looking for child nodes. Many resource compilers, including
          Borland's, wrongly set ValueLength to a *character* count on string
          nodes. But since we never try to query for a child of a string node,
          that doesn't matter here. }
        Inc(Cardinal(P), ValueLength);
        BumpToDWordBoundary(P);
        while Cardinal(P) < Cardinal(EndP) do begin
          Result := QueryValue(P, Path, Buf, BufLen);
          if Result then
            Exit;
          Inc(Cardinal(P), PWord(P)^);
          BumpToDWordBoundary(P);
        end;
      end;
    end;
  end;

var
  hRes: HRSRC;
  hResData: HGLOBAL;
  VersRes: Pointer;
  pFixedFileInfo: PVSFixedFileInfo;
  ValueLen: Cardinal;
begin
  Result := False;
  hRes := FindResource(hMod, PChar(VS_VERSION_INFO), RT_VERSION);
  if hRes <> 0 then begin
    hResData := LoadResource(hMod, hRes);
    if hResData <> 0 then try
      VersRes := LockResource(hResData);
      if Assigned(VersRes) then begin
        if not QueryValue(VersRes, 'VS_VERSION_INFO'#0, Pointer(pFixedFileInfo),
            ValueLen) then
          Exit; // Unexpected version resource format
        if pFixedFileInfo.dwSignature <> $FEEF04BD then
          Exit; // Unexpected version resource format
        FixedFileInfo := pFixedFileInfo^;
        Result := True;
      end;
    finally
      FreeResource(hResData);
    end;
  end;
end;

function LoadCommonIcon(Icon: TCommonIcon; X, Y: Integer): THandle;
const
  CommonIconIDs: array[TCommonIcon] of Integer = (
      0, 100, 101, 102, 103, 104
  );
begin
  if Icon = ciNone then
    Result := 0
  else begin
    // LoadImage with OEM icons doesn't return the actual 16x16 version, but the
    // 32x32 version scaled down to 16x16
    Result := LoadImage(GetModuleHandle(User32), PChar(CommonIconIDs[Icon]),
        IMAGE_ICON, X, Y, LR_DEFAULTCOLOR);
  end;
end;

function DestroyIconSafe(var IconHandle: HICON): Boolean;
begin
  if IconHandle = 0 then begin
    Result := False;
    Exit;
  end;
  Result := DestroyIcon(IconHandle);
  IconHandle := 0;
end;

function GetRandomSeed: LongWord;
  function TryAcquireContext(var hProv: HCRYPTPROV): DWORD;
  begin
    if CryptAcquireContext(@hProv, {Container}nil, {Provider}nil, PROV_RSA_FULL,
        {Flags}0) then
      Result := ERROR_SUCCESS
    else begin
      Result := GetLastError;
      if Result = NTE_BAD_KEYSET then begin
        if CryptAcquireContext(@hProv, {Container}nil, {Provider}nil,
            PROV_RSA_FULL, CRYPT_NEWKEYSET) then
          Result := ERROR_SUCCESS
        else
          Result := GetLastError;
      end;
    end;
  end;
var
  LastError: DWORD;
  hProv: HCRYPTPROV;
begin
  LastError := TryAcquireContext(hProv);
  if not IsWin32Success(LastError) then begin
    // This error can be triggered by temporarily renaming this registry key:
    // HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Cryptography\Defaults\Provider Types\Type 001
    // -> CryptAcquireContext failed (0x80090017).
    raise ECryptError.CreateFmt('CryptAcquireContext failed (0x%.8x).', [LastError]);
  end;
  try
    if not CryptGenRandom(hProv, SizeOf(Result), @Result) then begin
      // last error may get overwritten in CreateFmt method so save it first
      LastError := GetLastError;
      raise ECryptError.CreateFmt('CryptGenRandom failed (0x%.8x).', [LastError]);
    end;
  finally
    CryptReleaseContext(hProv, {dwFlags}0);
  end;
end;

function Remainder(x, y: Integer): Integer;
begin
  Result := x - Floor(x / y) * y;
end;

function Card(var tar; size: Integer): Integer;
const
  NibbleTable: array[0..15] of Integer =
      (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
var
  Data: PByteArray;
  i,b: Integer;
begin
  Result := 0;
  Data := @TByteArray(tar)[size];
  i := -size;
  while i < 0 do begin
    b := Data[i];
    Inc(Result, NibbleTable[b and $F] + NibbleTable[(b shr 4) and $F]);
    Inc(i);
  end;
end;

function SumWord(const Data: array of Word): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Data) to High(Data) do
    Result := Result + Data[I]
end;

resourcestring
  sNotAnEnum = '%s: argument must be an enum type; %s is of type %s';

function IsSubRangeMember(pInfo: PTypeInfo; const Value: Variant): Boolean;
var
  pData: PTypeData;
begin
  if pInfo.Kind <> tkEnumeration then begin
    raise EConvertError.CreateFmt(sNotAnEnum,
        ['IsSubRangeMember', pInfo.Name,
         GetEnumName(TypeInfo(TTypeKind), pInfo.Kind)]);
  end;
  pData := GetTypeData(pInfo);
  Result := (Value >= pData.MinValue) and (Value <= pData.MaxValue);
end;

function GetEnumName(pInfo: PTypeInfo; const Value: Variant): String;
begin
  // Wrapper function around TypInfo.GetEnumName that prevents us from having to
  // explicitly cast Value to Integer.
  // This wrapper also performs error checking for the supplied type and
  // out-of-range values, while the original function throws an access violation
  // or returns garbage in those cases.
  if pInfo.Kind <> tkEnumeration then begin
    raise EConvertError.CreateFmt(sNotAnEnum,
        ['GetEnumName', pInfo.Name,
         TypInfo.GetEnumName(TypeInfo(TTypeKind), Integer(pInfo.Kind))]);
  end;
  if IsSubRangeMember(pInfo, Value) then
    Result := TypInfo.GetEnumName(pInfo, Value)
  else
    Result := Format('<out of %s range (%d)>', [pInfo.Name, Integer(Value)]);
end;

procedure IncludeFlag(var Flags: Cardinal; Flag: Cardinal);
begin
  Flags := Flags or Flag;
end;

function CopyStringArray(const C: array of String): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, Length(C));
  for i := 0 to Length(C) - 1 do
    Result[i] := C[i];
end;

procedure AddToStringArray(var Arr: TStringArray; const Str: String);
begin
  SetLength(Arr, Length(Arr) + 1);
  Arr[High(Arr)] := Str;
end;

function ConvertSidToStringSid; external advapi32 name 'ConvertSidToStringSidA';

function GetProcessSidToken(ProcessHandle: Integer; var Sid: PSID): Boolean;
var
  hToken: THandle;
  cbBuf: Cardinal;
  ptiUser: PTOKEN_USER;
  bSuccess: Boolean;
  SidLength: Integer;
begin
  Result := False;
  if OpenProcessToken(ProcessHandle, TOKEN_READ or TOKEN_QUERY, hToken) then try
    bSuccess := GetTokenInformation(hToken, TokenUser, nil, 0, cbBuf);
    ptiUser  := nil;
    try
      while (not bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do begin
        ReallocMem(ptiUser, cbBuf);
        bSuccess := GetTokenInformation(hToken, TokenUser, ptiUser, cbBuf, cbBuf);
      end;
      if not bSuccess then
        Exit;
      SidLength := GetLengthSid(ptiUser.User.Sid);
      GetMem(Sid, SidLength);
      if CopySid(SidLength, Sid, ptiUser.User.Sid) then
        Result := True
      else
        FreeMem(Sid);
    finally
      if ptiUser <> nil then
        FreeMem(ptiUser);
    end;
  finally
    CloseHandle(hToken);
  end
end;

function RegKeyExists(const RootKey: DWORD; const SubKeyName: String): Boolean;
var
  K: HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(SubkeyName), 0, KEY_QUERY_VALUE, K) =
      ERROR_SUCCESS then begin
    RegCloseKey(K);
    Result := True;
  end
  else
    Result := False;
end;

function CreateRegistryLink(const LinkKey, TargetKey: WideString): Boolean;
var
  hKey: THandle;
  KeyName, ValueName: UNICODE_STRING;
  ObjAttribs: OBJECT_ATTRIBUTES;
  Disposition: ULONG;
begin
  Result := False;
  // make sure that target key exists first
  RtlInitUnicodeString(@KeyName, PWideChar(TargetKey));
  InitializeObjectAttributes(@ObjAttribs, @KeyName, OBJ_CASE_INSENSITIVE, 0, nil);
  if not NT_SUCCESS(NtCreateKey(@hKey, KEY_ALL_ACCESS, @ObjAttribs, 0, nil,
      REG_OPTION_NON_VOLATILE, @Disposition)) then
    Exit;
  // create link
  RtlInitUnicodeString(@KeyName, PWideChar(LinkKey));
  InitializeObjectAttributes(@ObjAttribs, @KeyName, OBJ_CASE_INSENSITIVE, 0, nil);
  if not NT_SUCCESS(NtCreateKey(@hKey, KEY_ALL_ACCESS, @ObjAttribs, 0, nil,
      REG_OPTION_NON_VOLATILE or REG_OPTION_CREATE_LINK, @Disposition)) then
    Exit;
  try
    RtlInitUnicodeString(@ValueName, REG_LINK_VALUE_NAME);
    Result := NT_SUCCESS(NtSetValueKey(hKey, @ValueName, 0, REG_LINK,
        PWideChar(TargetKey),
        // unlike for REG_XXX_SZ, *don't* include the terminating zeroes
        Length(TargetKey) * SizeOf(WideChar)));
  finally
    NtClose(hKey);
  end;
end;

function RemoveRegistryLink(const LinkKey: WideString): Boolean;
var
  hKey: THandle;
  KeyName: UNICODE_STRING;
  ObjAttribs: OBJECT_ATTRIBUTES;
begin
  Result := False;
  RtlInitUnicodeString(@KeyName, PWideChar(LinkKey));
  InitializeObjectAttributes(@ObjAttribs, @KeyName,
      OBJ_CASE_INSENSITIVE or REG_OPTION_OPEN_LINK, 0, nil);
  if not NT_SUCCESS(NtOpenKey(@hKey, KEY_ALL_ACCESS, @ObjAttribs)) then
    Exit;
  try
    Result := NT_SUCCESS(NtDeleteKey(hKey));
  finally
    NtClose(hKey);
  end;
end;

var
  xNtQueryObject: function(ObjectHandle: THandle;
      ObjectInformationClass: OBJECT_INFORMATION_CLASS;
      ObjectInformation: Pointer; ObjectInformationLength: ULONG;
      ReturnLength: PULONG): NTSTATUS; stdcall = nil;

function GetPathFromHandle(hObject: THandle; var Path: WideString): Boolean;
var
  Status: NTSTATUS;
  InitialBuffer: OBJECT_NAME_INFORMATION;
  NameInfo: POBJECT_NAME_INFORMATION;
  Size: Cardinal;
begin
  Result := False;
  Path := '';
  if not Assigned(@xNtQueryObject) then
    @xNtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
  if not Assigned(@xNtQueryObject) then
    Exit;
  FillChar(InitialBuffer, SizeOf(InitialBuffer), 0);
  NameInfo := @InitialBuffer;
  Size := SizeOf(InitialBuffer);
  // Query the name information a first time to get the size of the name.
  Status := xNtQueryObject(hObject, ObjectNameInformation, NameInfo, Size, @Size);
  if Size > 0 then begin
    GetMem(NameInfo, Size);
    try
      FillChar(NameInfo^, Size, 0);
      // Query the name information a second time to get the name of the
      // object referenced by the handle.
      Status := xNtQueryObject(hObject, ObjectNameInformation, NameInfo,
          Size, @Size);
      if NT_SUCCESS(Status) then begin
        Path := NameInfo.ObjectName.Buffer;
        SetLength(Path, NameInfo.ObjectName.Length div SizeOf(WideChar));
      end;
    finally
      FreeMem(NameInfo);
    end;
  end;
  Result := NT_SUCCESS(Status);
end;

function NormalizeNTUserRegPath(const RegPath: String;
    var NormalizedPath: String): Boolean;
const
  NTUsersRootKeyPath: PChar = '\REGISTRY\USER\';
var
  pNormalizedPath: PChar;
  pLastChar: PChar;
begin
  Result := False;
  if StrLIComp(PChar(RegPath), NTUsersRootKeyPath,
      Length(NTUsersRootKeyPath)) = 0 then begin
    NormalizedPath := Copy(RegPath, Length(NTUsersRootKeyPath) + 1, Length(RegPath));
    pNormalizedPath := StrScan(PChar(NormalizedPath), '\');
    if Assigned(pNormalizedPath) then begin
      Inc(pNormalizedPath);
      NormalizedPath := pNormalizedPath;
      pLastChar := AnsiLastChar(NormalizedPath);
      if Assigned(pLastChar) and (pLastChar^ <> '\') then
        NormalizedPath := NormalizedPath + '\';
      Result := True;
    end;
  end;
end;

procedure AlphaHighlight(hDC: Integer; R: TRect; AlphaBlend: TAlphaBlend);
var
  Bmp: TBitmap;
  Alpha: TBlendFunction;
  cx, cy: Integer;
  Rect: TRect;
begin
  if not Assigned(AlphaBlend) then
    Exit;
  cx := RectWidth(R);
  cy := RectHeight(R);
  with Rect do begin
    Left := 0;
    Top := 0;
    Right := cx;
    Bottom := cy;
  end;
  Bmp := TBitmap.Create;
  try
    with Bmp do begin
      Height := cy;
      Width := cx;
      Canvas.Brush.Style := bsSolid;
      Bmp.Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(Rect);
    end;
    with Alpha do begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 127;
      AlphaFormat := 0;
    end;
    AlphaBlend(hDC, R.Left, R.Top, cx, cy, Bmp.Canvas.Handle, 0, 0, cx, cy, Alpha);
  finally
    Bmp.Free;
  end;
end;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then
    InitCommonControls;
end;

function CheckCommonControl(CC: Integer): Boolean;
begin
  Result := InitCommonControl(CC);
    //raise EComponentError.Create(SInvalidComCtl32); // raises when designing
end;

// We can't simply subtract 2 tickcounts and expect an overflow to correctly
// handle a wrap just like in C - Delphi may throw an EIntOverflow exception.
function GetElapsedTicks(LastTick: Cardinal): Cardinal;
var
  CurrentTick : Cardinal;
begin
  CurrentTick := GetTickCount;
  if CurrentTick >= LastTick then
    Result := CurrentTick - LastTick
  else
    Result := (High(Cardinal) - LastTick) + CurrentTick;
end;

const
  MWFMO_WAITANY = 0;
  MWMO_INPUTAVAILABLE = $00000004;
  MSGF_WAIT_OBJ_TIMEOUT = $53F4; // arbitrary value for CallMsgFilter

// Based on the code in The Old New Thing - Pumping messages while waiting for a
// period of time
// http://blogs.msdn.com/b/oldnewthing/archive/2006/01/26/517849.aspx
function MsgWaitForObjectWithTimeout(hHandle: THandle;
    dwTimeout: Cardinal): DWORD;
var
  dwStart: Cardinal;
  dwElapsed: Cardinal;
  NumHandles: Cardinal;
  dwStatus: DWORD;
  Msg: TMsg;
begin
  dwStart := GetTickCount;
  dwElapsed := 0;
  if hHandle <> 0 then
    NumHandles := 1
  else
    NumHandles := 0;
  while dwElapsed < dwTimeout do begin
    dwStatus := MsgWaitForMultipleObjectsEx(NumHandles, hHandle,
        dwTimeout - dwElapsed,
        QS_ALLINPUT, MWFMO_WAITANY or MWMO_INPUTAVAILABLE);
    if dwStatus = WAIT_OBJECT_0 + NumHandles then begin
      while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
        if msg.message = WM_QUIT then begin
          PostQuitMessage(msg.wParam);
          Result := WAIT_FAILED; // abandoned due to WM_QUIT
          Exit;
        end;
        if not CallMsgFilter(msg, MSGF_WAIT_OBJ_TIMEOUT) then begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;
      end;
    end
    else if dwStatus = WAIT_TIMEOUT then
      // continue waiting
    else begin
      // object signaled
      Result := dwStatus;
      Exit;
    end;
    dwElapsed := GetElapsedTicks(dwStart);
  end;
  Result := WAIT_TIMEOUT; // timed out
end;

function IsWin32Success(ErrorCode: DWORD): Boolean;
begin
  Result := (ErrorCode = ERROR_SUCCESS);
end;

function RectWidth(const R: TRect): Integer;
begin
  if R.Right > R.Left then
    Result := R.Right - R.Left
  else
    Result := 0;
end;

function RectHeight(const R: TRect): Integer;
begin
  if R.Bottom > R.Top then
    Result := R.Bottom - R.Top
  else
    Result := 0;
end;

function IsValidPtr(Ptr: Pointer): Boolean;
begin
  // While setting up the process address space, the kernel reserves the bottom
  // 64KB of address space, so no valid objects will be allocated there. (Raymond
  // Chen)
  // So pointers are guaranteed to be always bigger than 64KB. This means that
  // there are 65536 possible pointer values which are no valid pointers, a
  // technique used by e.g. the MAKEINTRESOURCE macro. This function effectively
  // does the reverse of the buddy IS_INTRESOURCE macro.
  Result := HiWord(DWORD(Ptr)) <> 0;
end;

function ComparePointers(P1, P2: Pointer): Integer;
begin
  if Cardinal(P1) < Cardinal(P2) then
    Result := -1
  else if Cardinal(P1) > Cardinal(P2) then
    Result := 1
  else
    Result := 0;
end;

function InterlockedExchangePointer(var Target: Pointer;
    Value: Pointer): Pointer;
begin
  Result := Pointer(InterlockedExchange(Integer(Target), Integer(Value)));
end;

procedure FreeAndNilPIDL(var PIDL: PItemIDList);
var
  OldPIDL: PItemIDList;
begin
  OldPIDL := PIDL;
  PIDL := nil;
  ILFree(OldPIDL)
end;

function GetTaskbarWindow: HWND;
begin
  Result := FindWindow('Shell_TrayWnd', '');
end;

function RegisterTaskbarCreatedMsg: UINT;
begin
  Result := RegisterWindowMessage('TaskbarCreated');
end;

procedure GetFormatSettings2;
var
  DefaultLCID: LCID;
begin
  DefaultLCID := GetThreadLocale;
  WindowsTimeFormat := GetLocaleStr(DefaultLCID, LOCALE_STIMEFORMAT,
      LongTimeFormat);
end;

initialization
  IsWinNT := (Win32Platform = VER_PLATFORM_WIN32_NT);
  GetFormatSettings2; // extension of SysUtils.GetFormatSettings

end.
