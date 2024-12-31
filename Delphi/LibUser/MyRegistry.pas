(****************************************************************************
 *
 * Copyright 2016-2024 Tim De Baets
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
 * Enhanced TRegistry component and registry helper functions
 *
 ****************************************************************************)

unit MyRegistry;

{$R-}

interface

uses Windows, Registry, Classes, SysUtils;

type
  TRegLoadMUIStringW = function(hKey: HKEY; pszValue: PWideChar;
      pszOutBuf: PWideChar; cbOutBuf: DWORD; pcbData: PDWORD; Flags: DWORD;
      pszDirectory: PWideChar): Longint; stdcall;

type
  TMyRegistry = class;
  
  TEnumValuesProc = function(Reg: TMyRegistry; const ValueName: String;
      Data: Pointer): Boolean of object;

  TMyRegistry = class(TRegistry)
  private
    fRegLoadMUIString: TRegLoadMUIStringW;
    function ClearValuesProc(Reg: TMyRegistry; const ValueName: String;
        Data: Pointer): Boolean;
  public
    constructor Create;
    
    function OpenKeyReadOnly(const Key: String): Boolean;
    
    function ReadIntegerDef(const Name: String; default: Integer): Integer;
    function ReadBoolDef(const Name: String; default: Boolean): Boolean;
    function ReadStringDef(const Name: String; default: String): String;
    function ReadMUIStringDef(const Name: WideString;
        default: WideString): WideString;

    function ReadIntegerSafe(const Name: String; default : Integer): Integer;
    function ReadStringSafe(const Name, default: String): String;
    function ReadExpandStringSafe(const Name, default: String): String;
    procedure WriteIntegerSafe(const Name: String; Value: Integer);
    procedure WriteStringSafe(const Name, Value: String);
    procedure WriteIntegerForce(const Name: String; Value: Integer);
    procedure WriteStringForce(const Name, Value: String);

    procedure EnumValues(EnumProc: TEnumValuesProc; Data: Pointer);
    procedure ClearValues;
  end;

function GetKeyValue(Root: Cardinal; const Key: String; const Value: String;
    const Default: String): String;
function SetKeyValue(Root: Cardinal; const Key: String; const Value: String;
    const Data: String): Boolean;
function GetKeyBool(Root: Cardinal; const Key: String; const Value: String;
    Default: Boolean): Boolean;
procedure SetKeyBool(Root: Cardinal; const Key: String; const Value: String;
    Data: Boolean);
function GetKeyInt(Root: LongInt; const Key: String; const Value: String;
    Default: Integer): Integer;
procedure SetKeyInt(Root: LongInt; const Key: String; const Value: String;
    Data: Integer);
function CreateKey(Root: Longint; const Key: String): Boolean;
function ClearSubKeys(Root: Cardinal; const Key: String): Boolean;

implementation

uses Consts;

procedure ReadError(const Name: String);
begin
  raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function IsRelative(const Value: String): boolean;
begin
  Result := not ((Value <> '') and (Value[1] = '\'));
end;

constructor TMyRegistry.Create;
begin
  // Must use the Unicode version of RegLoadMUIString here - the ANSI version
  // always returns ERROR_CALL_NOT_IMPLEMENTED.
  fRegLoadMUIString := GetProcAddress(GetModuleHandle(advapi32),
      'RegLoadMUIStringW');
  inherited;
end;

function TMyRegistry.OpenKeyReadOnly(const Key: String): boolean;
var
  TempKey : HKey;
  S       : String;
  Relative: boolean;
begin
  S := Key;
  Relative := IsRelative(S);
  if not Relative then
    Delete(S, 1, 1);
  TempKey := 0;
  Result := RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0,
      KEY_READ, TempKey) = ERROR_SUCCESS;
  if Result then begin
    if (CurrentKey <> 0) and Relative then
      S := CurrentPath + '\' + S;
    ChangeKey(TempKey, S);
  end;
end;

function TMyRegistry.ReadIntegerDef(const Name: String; default: Integer): Integer;
begin
  try
    Result := ReadInteger(Name);
  except
    on ERegistryException do
      Result := default;
  end;
end;

function TMyRegistry.ReadBoolDef(const Name: String; default: Boolean): Boolean;
begin
  try
    Result := ReadInteger(Name) <> 0;
  except
    on ERegistryException do
      Result := default;
  end;
end;

function TMyRegistry.ReadStringDef(const Name: String; default: String): String;
var
  Len: Integer;
  RegData: TRegDataType;
begin
  Len := GetDataSize(Name);
  if Len > 0 then begin
    try
      SetString(Result, nil, Len);
      GetData(Name, PChar(Result), Len, RegData);
      if (RegData = rdString) or (RegData = rdExpandString) then
        SetLength(Result, StrLen(PChar(Result)))
      else
        ReadError(Name);
    except
      on ERegistryException do
        Result := default;
    end;
  end
  else
    Result := default;
end;

function TMyRegistry.ReadMUIStringDef(const Name: WideString;
    default: WideString): WideString;
var
  Len: Integer;
  Res: Longint;
begin
  Result := default;
  Len := 0;
  if Assigned(fRegLoadMUIString) then begin
    fRegLoadMUIString(CurrentKey, PWideChar(Name), nil, 0, @Len, 0, nil);
    if Len <= 0 then
      Exit;
    SetString(Result, nil, Len div SizeOf(WideChar));
    Res := fRegLoadMUIString(CurrentKey, PWideChar(Name), PWideChar(Result), Len,
        @Len, 0, nil);
    if Res <> ERROR_SUCCESS then begin
      Result := default;
      Exit;
    end;
    SetLength(Result, lstrlenW(PWideChar(Result)));
  end
  else
    Result := ReadStringDef(Name, default);
end;

function TMyRegistry.ReadIntegerSafe(const Name: String;
    default: Integer): Integer;
var
  rd  : TRegDataType;
  siz : Integer;
begin
  Result := Default;
  if not ValueExists(Name) then
    Exit;
  Siz := GetData(Name, nil, 0, rd);
  case rd of
    // String? Convert to Integer
    rdString,
    rdExpandString : Result := StrToIntDef(ReadString(Name), Default);
    // Integer? Just use it
    rdInteger      : Result := ReadInteger(Name);
    // Other? If it's 4 bytes in size, copy the data
    rdUnknown,
    rdBinary       : if Siz = 4 then GetData(Name, @Result, 4, rd);
  end;
end;

function TMyRegistry.ReadStringSafe(const Name, default: String): String;
var
  rd : TRegDataType;
  siz : Integer;
begin
  Result := Default;
  if not ValueExists(Name) then
    Exit;
  Siz := GetData(Name, nil, 0, rd);
  case rd of
    // String? Just read it
    rdString,
    rdExpandString : Result := ReadString(Name);
    // Integer? Convert to string
    rdInteger      : Result := IntToStr(ReadInteger(Name));
    // Other? Treat it as a string
    rdUnknown,
    rdBinary : begin
      SetLength(Result, Siz);
      GetData(Name, PChar(Result), Siz, rd);
      Result := StrPas(PChar(Result));
    end;
  end;
end;

function TMyRegistry.ReadExpandStringSafe(const Name, default: String): String;
var
  rd : TRegDataType;
  siz : Integer;

  function EE(const S: String): String;
  // Return S with any environment strings expanded
  var
    L : Integer;
  begin
    if Pos('%', S) = 0 then
      Result := S
    else begin
      // Don't use nil for 2nd argument to ExpandEnvironmentStrings,
      // as it fails in Win95
      SetLength(Result, 1);
      L := ExpandEnvironmentStrings(PChar(S), PChar(Result), 0) + 1;
      SetLength(Result, L);
      ExpandEnvironmentStrings(PChar(S), PChar(Result), L);
      // Next line required to get rid of extra #0s at end
      Result := StrPas(PChar(Result));
      if Result = '' then
        Result := S;
    end;
  end;

begin
  Result := Default;
  if not ValueExists(Name) then
    Exit;
  Siz := GetData(Name, nil, 0, rd);
  case rd of
    // String? Just read it
    rdString : Result := ReadString(Name);
    rdExpandString : Result := EE(ReadString(Name));
    // Integer? Convert to string
    rdInteger      : Result := IntToStr(ReadInteger(Name));
    // Other? Treat it as a string
    rdUnknown,
    rdBinary : begin
      SetLength(Result, Siz);
      GetData(Name, PChar(Result), Siz, rd);
      Result := StrPas(PChar(Result));
    end;
  end;
end;

procedure TMyRegistry.WriteIntegerSafe(const Name: String;
    Value: Integer);
var
  DType : DWord;
  DSize : DWord;
begin
  if RegQueryValueEx(CurrentKey, PChar(Name), nil, @DType, nil,
    @DSize) <> ERROR_SUCCESS then
    WriteInteger(Name, Value)
  else begin
    case DType of
      REG_SZ         : WriteString(Name, IntToStr(Value));
      REG_EXPAND_SZ  : WriteExpandString(Name, IntToStr(Value));
      REG_DWORD_BIG_ENDIAN :
                       WriteInteger(Name, MakeLong(Swap(HiWord(Value)),
                         Swap(LoWOrd(Value))));
      REG_DWORD      : WriteInteger(Name, Value);
      else
        RegSetValueEx(CurrentKey, PChar(Name), 0, DType,
          @Value, 4);
    end;
  end;
end;

procedure TMyRegistry.WriteStringSafe(const Name, Value: String);
var
  DType : DWord;
  DSize : DWord;
  I     : Integer;
begin
  if RegQueryValueEx(CurrentKey, PChar(Name), nil, @DType, nil,
    @DSize) <> ERROR_SUCCESS then
    WriteString(Name, Value)
  else begin
    I := StrToIntDef(Value, 0);
    case DType of
      REG_SZ         : WriteString(Name, Value);
      REG_EXPAND_SZ  : WriteExpandString(Name, Value);
      REG_DWORD_BIG_ENDIAN :
                       WriteInteger(Name, MakeLong(Swap(HiWord(I)),
                         Swap(LoWOrd(I))));
      REG_DWORD      : WriteInteger(Name, I);
      else
        RegSetValueEx(CurrentKey, PChar(Name), 0, DType,
          PChar(Value), Length(Value)+1);
    end;
  end;
end;

procedure TMyRegistry.WriteIntegerForce(const Name: String;
  Value: Integer);
begin
  DeleteValue(Name);
  WriteInteger(Name, Value);
end;

procedure TMyRegistry.WriteStringForce(const Name, Value: String);
begin
  DeleteValue(Name);
  WriteString(Name, Value);
end;

procedure TMyRegistry.EnumValues(EnumProc: TEnumValuesProc; Data: Pointer);
var
  Values: TStringList;
  i: Integer;
begin
  Values := TStringList.Create;
  try
    GetValueNames(Values);
    for i := 0 to Values.Count - 1 do
      EnumProc(Self, Values[i], Data);
  finally
    Values.Free;
  end;
end;

function TMyRegistry.ClearValuesProc(Reg: TMyRegistry; const ValueName: String;
    Data: Pointer): Boolean;
begin
  Result := True;
  Reg.DeleteValue(ValueName);
end;

procedure TMyRegistry.ClearValues;
begin
  EnumValues(ClearValuesProc, nil);
end;

function GetKeyValue(Root: Cardinal; const Key: String; const Value: String;
    const Default: String): String;
begin
  Result := Default;
  with TMyRegistry.Create do
  try
    RootKey := Root;
    if OpenKeyReadOnly(Key) then begin
      if ValueExists(Value) then
        Result := ReadStringSafe(Value, Default);
      CloseKey;
    end;
  finally
   Free;
  end;
end;

function SetKeyValue(Root: Cardinal; const Key: String;
    const Value, Data: String): Boolean;
begin
  Result := True;
  with TMyRegistry.Create do
  try
    RootKey := Root;
    if OpenKey(Key, False) then
    try
      WriteStringSafe(Value, Data);
      CloseKey;
    except
      on ERegistryException do
        Result := False;
    end
    else
      Result := False;
  finally
    Free;
  end;
end;

function GetKeyBool(Root: Cardinal; const Key: String; const Value: String;
    Default: Boolean): Boolean;
begin
  Result := Default;
  with TMyRegistry.Create do
  try
    RootKey := Root;
    if OpenKeyReadOnly(Key) then begin
      if ValueExists(Value) then
        Result := ReadBoolDef(Value, Default);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure SetKeyBool(Root: Cardinal; const Key: String; const Value: String;
    Data: Boolean);
begin
  with TRegistry.Create do
  try
    RootKey := Root;
    if OpenKey(Key, False) then begin
      WriteBool(Value, Data);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

function GetKeyInt(Root: LongInt; const Key: String; const Value: String;
    Default: Integer): Integer;
begin
  Result := Default;
  with TMyRegistry.Create do
  try
    RootKey := Root;
    if OpenKeyReadOnly(Key) then begin
      if ValueExists(Value) then
        Result := ReadIntegerSafe(Value, Default);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure SetKeyInt(Root: LongInt; const Key: String; const Value: String;
    Data: Integer);
begin
  with TMyRegistry.Create do
  try
    RootKey := Root;
    if OpenKey(Key, False) then begin
      WriteIntegerSafe(Value, Data);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

function CreateKey(Root: Longint; const Key: String): Boolean;
begin
  with TRegistry.Create do
  try
    RootKey := Root;
    Result := OpenKey(Key, True);
  finally
    Free;
  end;
end;

function ClearSubKeys(Root: Cardinal; const Key: String): Boolean;
var
  l: TStringList;
  i: Integer;
begin
  Result := True;
  with TRegistry.Create do
  try
    RootKey := Root;
    if OpenKey(Key, False) then begin
      l := TStringList.Create;
      try
        GetKeyNames(l);
        CloseKey;
        for i := 0 to l.Count - 1 do
          DeleteKey(Key + '\' + l[i]);
      finally
        l.Free;
      end;
    end
    else
      Result := False;
  finally
    Free;
  end;
end;

end.


