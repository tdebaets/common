(****************************************************************************
 *
 * Copyright 2017-2018 Tim De Baets
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
 * Windows Media Player WSZ skin format
 *
 ****************************************************************************)

unit WMPWSZFormat;

interface

uses Windows, Classes, SysUtils, Common2, StreamUtil, ActiveX, ComObj, PathFunc,
    CmnFunc2, MyRegistry, IntToStrList, EZDslHsh, EZStrHsh, EZDslSup;

const
  WSZ_ELEMENTID_NAMED       = $00;
  WSZ_ELEMENTID_BUTTON      = $03;
  WSZ_ELEMENTID_THEME       = $0B;
  WSZ_ELEMENTID_VIEW        = $0C;
  WSZ_ELEMENTID_SUBVIEW     = $0D;
  WSZ_ELEMENTID_CLSID_MASK  = $80;

const
  WSZ_ATTRIBUTE_TYPE_NAMED          = $00;
  WSZ_ATTRIBUTE_TYPE_WORDBOOL       = $01;
  WSZ_ATTRIBUTE_TYPE_INTEGER        = $04;
  WSZ_ATTRIBUTE_TYPE_STRING         = $08;
  WSZ_ATTRIBUTE_TYPE_SYSINT         = $0D;
  WSZ_ATTRIBUTE_TYPE_RESSTRING      = $18;
  WSZ_ATTRIBUTE_TYPE_WMPENABLED     = $28;
  WSZ_ATTRIBUTE_TYPE_NAMED_WMPPROP  = $40;
  WSZ_ATTRIBUTE_TYPE_WMPPROP        = $48;
  WSZ_ATTRIBUTE_TYPE_WMPPROP2       = $C8;
  WSZ_ATTRIBUTE_TYPE_NAMED_EVENT    = $E0;
  WSZ_ATTRIBUTE_TYPE_NAMED_JSCRIPT  = $80;
  WSZ_ATTRIBUTE_TYPE_JSCRIPT        = $88;

type
  TWMPWSZAttributeType = (wszatNamed, wszatWordBool, wszatInteger, wszatString,
      wszatSysInt, wszatResString, wszatWmpEnabled, wszatNamedWmpProp,
      wszatWmpProp, wszatNamedEvent, wszatNamedJscript, wszatJscript);

type
  EWMPWSZParseError = class(Exception)
  private
    fPosition: Longint;
  public
    constructor CreateFmt(Position: Longint; const Msg: String;
        const Args: array of const);
    property Position: Longint read fPosition;
  end;

type
  // info and warning only; errors are raised as an exception instead
  TWMPWSZLogLevel = (wszllInfo, wszllWarning);
  TWMPWSZLogProc = procedure(Sender: TObject; Level: TWMPWSZLogLevel;
      const Msg: String) of object;

type
  TWMPWSZParser = class
  private
    fLogProc: TWMPWSZLogProc;
    fWMPTypeLib: ITypeLib;
    fWMPObjCLSIDs: TStringHashTable; // key: CLSID as string; value: object name
    fWMPDispIDs: TIntToStrList; // int: disp ID; string: property name
    procedure Log(Level: TWMPWSZLogLevel; const Msg: String;
        const Args: array of const);
    procedure LogInfo(const Msg: String; const Args: array of const);
    procedure LogWarning(const Msg: String; const Args: array of const);
    procedure ParseAttrib(Stream: TSafeStream; EndOfElement: Longint);
    procedure ParseElement(Stream: TSafeStream; Level: Cardinal);
    procedure LoadWMPTypeLib;
    function ResolveWMPObjCLSIDFromTypeLib(const CLSID: TCLSID;
        var Name: String): Boolean;
    function ResolveWMPObjCLSID(const CLSID: TCLSID; var Name: String): Boolean;
    function ResolveWMPDispID(ID: Integer): String;
    procedure LoadWMPDispIDsForType(TypInfo: ITypeInfo);
    procedure LoadWMPDispIDs;
    procedure LoadWMPObjsFromRegistry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Stream: TStream);
    property LogProc: TWMPWSZLogProc read fLogProc write fLogProc;
  end;

function IsButtonElementName(const Name: String): Boolean;
function ParseWSZAttributeType(WSZType: Byte;
    out Typ: TWMPWSZAttributeType): Boolean;
function FormatWmpPropValue(const Value: String; Addend: Integer): String;

implementation

uses WMPUtil;

{ EWMPWSZParseError }

constructor EWMPWSZParseError.CreateFmt(Position: Longint; const Msg: String;
    const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  fPosition := Position;
end;

{ TWMPWSZParser }

constructor TWMPWSZParser.Create;
begin
  inherited;
  fWMPObjCLSIDs := TStringHashTable.Create;
  fWMPObjCLSIDs.HashFunction := HashELF; // default hash causes integer overflows
  fWMPObjCLSIDs.IgnoreCase := True;
  fWMPDispIDs := TIntToStrList.Create;
  fWMPDispIDs.Sorted := True; // for faster lookup
  fWMPDispIDs.Duplicates := dupIgnore;
  LoadWMPTypeLib;
  LoadWMPDispIDs;
  LoadWMPObjsFromRegistry;
end;

destructor TWMPWSZParser.Destroy;
begin
  fWMPTypeLib := nil;
  FreeAndNil(fWMPDispIDs);
  FreeAndNil(fWMPObjCLSIDs);
  inherited;
end;

procedure TWMPWSZParser.Log(Level: TWMPWSZLogLevel; const Msg: String;
    const Args: array of const);
begin
  if Assigned(fLogProc) then
    fLogProc(Self, Level, Format(Msg, Args));
end;

procedure TWMPWSZParser.LogInfo(const Msg: String; const Args: array of const);
begin
  Log(wszllInfo, Msg, Args);
end;

procedure TWMPWSZParser.LogWarning(const Msg: String; const Args: array of const);
begin
  Log(wszllWarning, Msg, Args);
end;

procedure TWMPWSZParser.ParseAttrib(Stream: TSafeStream; EndOfElement: Longint);
var
  NextAttribOffset: Word;
  EndOfAttrib: Longint;
  AttribType: Byte;
  Typ: TWMPWSZAttributeType;
  AttribName: WideString;
  AttribValue: Variant;
  AttrDispId: Word;
  Addend: Integer;
  Unknown: Word;
begin
  EndOfAttrib := 0;
  NextAttribOffset := Stream.ReadWord(0);
  if NextAttribOffset <> 0 then begin
    // only if the attribute isn't the last attribute of the element
    EndOfAttrib := Stream.Position - SizeOf(NextAttribOffset) + NextAttribOffset
  end
  else if EndOfElement <> 0 then
    EndOfAttrib := EndOfElement;
  AttribType := Stream.ReadByte(EndOfAttrib);
  LogInfo('Attribute found: type=0x%.02x, next=%u',
      [AttribType, NextAttribOffset]);
  if not ParseWSZAttributeType(AttribType, Typ) then begin
    raise EWMPWSZParseError.CreateFmt(Stream.Position,
        'Unrecognized attribute type: 0x%.02x', [AttribType]);
  end;
  case Typ of
    wszatNamed,
    wszatNamedEvent: begin
      AttribName := Stream.ReadWideString(EndOfAttrib);
      AttribValue := Stream.ReadWideString(EndOfAttrib);
      LogInfo('  %s=%s', [AttribName, AttribValue]);
    end;
    wszatNamedJscript: begin
      Unknown := Stream.ReadWord(EndOfAttrib);
      // TODO: try to find out its meaning (WMP12 skins only, always seems to be $78)
      // suggestion: try to load a skin with different value, and check if and
      // where in WMP's code the loading fails
      LogInfo('  unknown=%d', [Unknown]);
      AttribName := Stream.ReadWideString(EndOfAttrib);
      AttribValue := Stream.ReadWideString(EndOfAttrib);
      LogInfo('  %s=%s', [AttribName, AttribValue]);
    end;
    wszatNamedWmpProp: begin
      Addend := Stream.ReadInteger(EndOfAttrib);
      AttribName := Stream.ReadWideString(EndOfAttrib);
      AttribValue := Stream.ReadWideString(EndOfAttrib);
      LogInfo('  %s=%s', [AttribName, FormatWmpPropValue(AttribValue, Addend)]);
    end;
    wszatWmpProp: begin
      Addend := Stream.ReadInteger(EndOfAttrib);
      AttrDispId := Stream.ReadWord(EndOfAttrib);
      Stream.SkipPaddingWord(EndOfAttrib);
      AttribValue := Stream.ReadWideString(EndOfAttrib);
      LogInfo('  dispid %u %s=%s',
          [AttrDispId, ResolveWMPDispID(AttrDispId),
           FormatWmpPropValue(AttribValue, Addend)]);
    end;
    else begin
      AttrDispId := Stream.ReadWord(EndOfAttrib);
      Stream.SkipPaddingWord(EndOfAttrib);
      case Typ of
        wszatWordBool:
          AttribValue := Stream.ReadWord(EndOfAttrib);
        wszatInteger,
        wszAtResString:
          AttribValue := Stream.ReadInteger(EndOfAttrib);
        wszatString,
        wszatWmpEnabled,
        wszatSysInt,
        wszatJscript:
          AttribValue := Stream.ReadWideString(EndOfAttrib);
        else begin
          raise EWMPWSZParseError.CreateFmt(Stream.Position,
              'Unexpected attribute type: %u', [Cardinal(Typ)]);
        end;
      end;
      LogInfo('  dispid %u %s=%s',
          [AttrDispId, ResolveWMPDispID(AttrDispId), String(AttribValue)]);
    end;
  end;
end;

procedure TWMPWSZParser.ParseElement(Stream: TSafeStream; Level: Cardinal);
var
  IsRootElement: Boolean;
  NextSiblingOffset, FirstChildOffset: Word;
  EndOfElement: Longint;
  ElementType: Byte;
  NumChildren, NumAttribs: Byte;
  ElementName: WideString;
  ObjName: String;
  CLSID: TGUID;
  i: Integer;
begin
  EndOfElement := 0;
  IsRootElement := (Level = 0);
  NextSiblingOffset := Stream.ReadWord(0);
  FirstChildOffset := Stream.ReadWord(0);
  if FirstChildOffset <> 0 then begin
    // only if the element contains children
    EndOfElement := Stream.Position - SizeOf(FirstChildOffset) + FirstChildOffset;
  end
  else if NextSiblingOffset <> 0 then begin
    // only if the element isn't the last element in its parent
    EndOfElement := Stream.Position - SizeOf(FirstChildOffset) -
        SizeOf(NextSiblingOffset) + NextSiblingOffset;
  end;
  ElementType := Stream.ReadByte(EndOfElement);
  LogInfo('Element found: type=0x%.02x, level=%d, next=%u, first=%u',
      [ElementType, Level, NextSiblingOffset, FirstChildOffset]);
  if IsRootElement and (ElementType <> WSZ_ELEMENTID_THEME) then begin
    raise EWMPWSZParseError.CreateFmt(Stream.Position,
        'Root element isn''t a THEME element, type: %u', [ElementType]);
  end
  else if not IsRootElement and (ElementType = WSZ_ELEMENTID_THEME) then begin
    raise EWMPWSZParseError.CreateFmt(Stream.Position,
        'Found a THEME element that isn''t the root element', []);
  end;
  NumChildren := Stream.ReadByte(EndOfElement);
  NumAttribs := Stream.ReadByte(EndOfElement);
  case ElementType of
    WSZ_ELEMENTID_NAMED,
    WSZ_ELEMENTID_BUTTON: begin
      // Named element
      ElementName := Stream.ReadWideString(EndOfElement);
      if (ElementType = WSZ_ELEMENTID_NAMED)
          and IsButtonElementName(ElementName) then begin
        raise EWMPWSZParseError.CreateFmt(Stream.Position,
            'Unexpected element ID for BUTTONGROUP element', []);
      end
      else if (ElementType = WSZ_ELEMENTID_BUTTON)
          and not IsButtonElementName(ElementName) then begin
        raise EWMPWSZParseError.CreateFmt(Stream.Position,
            'Unexpected element ID for %s element', [ElementName]);
      end;
      LogInfo('  name=%s', [ElementName]);
    end;
    WSZ_ELEMENTID_THEME,
    WSZ_ELEMENTID_VIEW,
    WSZ_ELEMENTID_SUBVIEW: begin
      // Predefined element
      Stream.SkipPaddingWord(EndOfElement);
    end
    else if (ElementType and WSZ_ELEMENTID_CLSID_MASK) <> 0 then begin
      // CLSID element
      Stream.SkipPaddingWord(EndOfElement);
      CLSID := Stream.ReadGUID(EndOfElement);
      ResolveWMPObjCLSID(CLSID, ObjName);
      LogInfo('  object=%s', [ObjName]);
    end
    else begin
      raise EWMPWSZParseError.CreateFmt(Stream.Position,
          'Unrecognized element ID: 0x%.02x', [ElementType]);
    end;
  end;
  for i := 0 to NumAttribs - 1 do
    ParseAttrib(Stream, EndOfElement);
  for i := 0 to NumChildren - 1 do
    ParseElement(Stream, Level + 1);
end;

procedure TWMPWSZParser.Parse(Stream: TStream);
const
  RootElementLevel = 0;
var
  SafeStream: TSafeStream;
begin
  Stream.Seek(0, soFromBeginning);
  SafeStream := TSafeStream.Create(Stream);
  try
    ParseElement(SafeStream, RootElementLevel);
  finally
    FreeAndNil(SafeStream);
  end;
end;

procedure TWMPWSZParser.LoadWMPTypeLib;
var
  WMPDLLPath: WideString;
begin
  WMPDLLPath := AddBackslash(GetSystemDir) + WMPDll;
  if not Succeeded(LoadTypeLib(PWideChar(WMPDLLPath), fWMPTypeLib)) then
    LogWarning('Failed to load wmp.dll type library', []);  
end;

function TWMPWSZParser.ResolveWMPObjCLSIDFromTypeLib(const CLSID: TCLSID;
    var Name: String): Boolean;
var
  TypInfo: ITypeInfo;
  WideName: WideString;
begin
  Result := False;
  if not Assigned(fWMPTypeLib) then
    Exit;
  if not Succeeded(fWMPTypeLib.GetTypeInfoOfGuid(CLSID, TypInfo)) then
    Exit;
  if not Succeeded(TypInfo.GetDocumentation(MEMBERID_NIL, @WideName, nil, nil,
      nil)) then
    Exit;
  if Length(WideName) > 0 then begin
    Name := WideName;
    Result := True;
  end;
end;

function TWMPWSZParser.ResolveWMPObjCLSID(const CLSID: TCLSID;
    var Name: String): Boolean;
var
  ObjName: String;
begin
  Name := GUIDToString(CLSID);
  Result := ResolveWMPObjCLSIDFromTypeLib(CLSID, ObjName);
  if not Result then begin
    // Fallback for internal objects not in the typelib, e.g. taskcenter -
    // {395BF287-6477-495F-8427-2C09A23C3248} (ITaskCntrCtrl)
    Result := fWMPObjCLSIDs.Search(Name, ObjName);
  end;
  if Result then
    Name := String(ObjName) + ' ' + Name
  else
    LogWarning('Unknown object for CLSID %s', [GUIDToString(CLSID)]);
end;

function TWMPWSZParser.ResolveWMPDispID(ID: Integer): String;
var
  Idx: Integer;
begin
  // NOTE: when parsing a WMP12 WSZ skin on a system with WMP11, some dispIDs
  // new to WMP12 won't be recognized. These will be shown as warnings in the
  // parsing output.
  if fWMPDispIDs.Find(ID, Idx) then
    Result := fWMPDispIDs.Strings[Idx]
  else begin
    Result := '<unknown property>';
    if Assigned(fWMPTypeLib) then
      LogWarning('Unknown property for dispid %u', [ID]);
  end;
end;

procedure TWMPWSZParser.LoadWMPDispIDsForType(TypInfo: ITypeInfo);
var
  pTypAttr: PTypeAttr;
  pFunDesc: PFuncDesc;
  i: Integer;
  FuncName: WideString;
begin
  if not Assigned(TypInfo) then
    Exit;
  pTypAttr := nil;
  if not Succeeded(TypInfo.GetTypeAttr(pTypAttr)) or not Assigned(pTypAttr) then
    Exit;
  try
    for i := 0 to pTypAttr.cFuncs - 1 do begin
      pFunDesc := nil;
      if not Succeeded(TypInfo.GetFuncDesc(i, pFunDesc))
          or not Assigned(pFunDesc) then
        Continue;
      try
        // Ignore non-property functions as regular functions may have the same
        // dispid as properties. Check for 'property-get' because read-only
        // properties (eg. ID) can also be used in the skin.
        if pFunDesc.invkind <> INVOKE_PROPERTYGET then
          Continue;
        if not Succeeded(TypInfo.GetDocumentation(pFunDesc.memid,
            @FuncName, nil, nil, nil)) then
          Continue;
        try
          if Length(FuncName) > 0 then
            fWMPDispIDs.AddString(pFunDesc.memid, FuncName);
        finally
          FuncName := ''; // required to free the string before it gets reassigned
        end;
      finally
        TypInfo.ReleaseFuncDesc(pFunDesc);
      end;
    end;
  finally
    TypInfo.ReleaseTypeAttr(pTypAttr);
  end;
end;

procedure TWMPWSZParser.LoadWMPDispIDs;
var
  Count, i: Integer;
  TypInfo: ITypeInfo;
  TypeKind: TTypeKind;
begin
  if not Assigned(fWMPTypeLib) then
    Exit;
  Count := fWMPTypeLib.GetTypeInfoCount;
  for i := 0 to Count - 1 do begin
    TypInfo := nil;
    if not Succeeded(fWMPTypeLib.GetTypeInfoType(i, TypeKind)) then
      Continue;
    if TypeKind <> TKIND_DISPATCH then
      Continue;
    if not Succeeded(fWMPTypeLib.GetTypeInfo(i, TypInfo)) then
      Continue;
    LoadWMPDispIDsForType(TypInfo);
  end;
end;

procedure TWMPWSZParser.LoadWMPObjsFromRegistry;
const
  CLSIDValueName = 'classid';
  CLSIDPrefix = 'clsid:';
var
  Keys: TStringList;
  i: Integer;
  ObjName: String;
  CLSIDStr: String;
begin
  Keys := TStringList.Create;
  try
    with TMyRegistry.Create do try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly(WMPObjectsRegKey) then
        Exit;
      GetKeyNames(Keys);
      CloseKey;
      for i := 0 to Keys.Count - 1 do begin
        ObjName := Keys[i];
        if OpenKeyReadOnly(WMPObjectsRegKey + '\' + ObjName) then try
          // Value has the form "clsid:<CLSID without curly braces>"
          CLSIDStr := ReadStringSafe(CLSIDValueName, '');
          if not IsPrefix(CLSIDStr, CLSIDPrefix) then
            Continue;
          Delete(CLSIDStr, 1, Length(CLSIDPrefix));
          CLSIDStr := '{' + CLSIDStr + '}';
          try
            fWMPObjCLSIDs.Insert(CLSIDStr, ObjName);
          except
            on EEZContainerError do
              // duplicate CLSID; ignore
          end;
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  finally
    FreeAndNil(Keys);
  end;
end;

function IsButtonElementName(const Name: String): Boolean;
const
  ButtonElementName = 'buttonelement';
begin
  Result := (CompareText(Name, ButtonElementName) = 0);
end;

function ParseWSZAttributeType(WSZType: Byte;
    out Typ: TWMPWSZAttributeType): Boolean;
begin
  Result := True;
  case WSZType of
    WSZ_ATTRIBUTE_TYPE_NAMED:         Typ := wszatNamed;
    WSZ_ATTRIBUTE_TYPE_WORDBOOL:      Typ := wszatWordBool;
    WSZ_ATTRIBUTE_TYPE_INTEGER:       Typ := wszatInteger;
    WSZ_ATTRIBUTE_TYPE_STRING:        Typ := wszatString;
    WSZ_ATTRIBUTE_TYPE_SYSINT:        Typ := wszatSysInt;
    WSZ_ATTRIBUTE_TYPE_RESSTRING:     Typ := wszAtResString;
    WSZ_ATTRIBUTE_TYPE_WMPENABLED:    Typ := wszatWmpEnabled;
    WSZ_ATTRIBUTE_TYPE_NAMED_WMPPROP: Typ := wszatNamedWmpProp;
    WSZ_ATTRIBUTE_TYPE_WMPPROP:       Typ := wszatWmpProp;
    WSZ_ATTRIBUTE_TYPE_WMPPROP2:      Typ := wszatWmpProp;
    WSZ_ATTRIBUTE_TYPE_NAMED_EVENT:   Typ := wszatNamedEvent;
    WSZ_ATTRIBUTE_TYPE_NAMED_JSCRIPT: Typ := wszatNamedJscript;
    WSZ_ATTRIBUTE_TYPE_JSCRIPT:       Typ := wszatJscript;
    else
      Result := False;
  end;
end;

function FormatWmpPropValue(const Value: String; Addend: Integer): String;
begin
  Result := Value;
  if Addend > 0 then
    Result := Result + '+' + IntToStr(Addend)
  else if Addend < 0 then
    Result := Result + IntToStr(Addend);
end;

end.
