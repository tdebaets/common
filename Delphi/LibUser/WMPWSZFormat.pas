(****************************************************************************
 *
 * Copyright 2017 Tim De Baets
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

uses Windows, Classes, SysUtils, Common2, StreamUtil, ActiveX, ComObj, EZDslHsh,
    PathFunc, CmnFunc2, UIntList;

const
  WSZ_ELEMENTID_THEME       = $0B;
  WSZ_ELEMENTID_VIEW        = $0C;
  WSZ_ELEMENTID_SUBVIEW     = $0D;
  WSZ_ELEMENTID_CLSID_MASK  = $80;

const
  WSZ_ATTRIBUTE_TYPE_NAMED        = $00;
  WSZ_ATTRIBUTE_TYPE_WORDBOOL     = $01;
  WSZ_ATTRIBUTE_TYPE_INTEGER      = $04;
  WSZ_ATTRIBUTE_TYPE_STRING       = $08;
  WSZ_ATTRIBUTE_TYPE_SYSINT       = $0D;
  WSZ_ATTRIBUTE_TYPE_RESSTRING    = $18;
  WSZ_ATTRIBUTE_TYPE_WMPENABLED   = $28;
  WSZ_ATTRIBUTE_TYPE_WMPPROP      = $48;
  WSZ_ATTRIBUTE_TYPE_NAMED_EVENT  = $E0;
  WSZ_ATTRIBUTE_TYPE_GLOBAL_VAR   = $88;

type
  TWMPWSZAttributeType = (wszatNamed, wszatWordBool, wszatInteger, wszatString,
      wszatSysInt, wszatResString, wszatWmpEnabled, wszatWmpProp,
      wszatNamedEvent, wszatGlobalVar);

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
  TWMPWSZParseFlag = (wszpfIsRootElement);
  TWMPWSZParseFlags = set of TWMPWSZParseFlag;      

type
  TWMPWSZParser = class
  private
    fLogProc: TWMPWSZLogProc;
    fWMPTypeLib: ITypeLib;
    fWMPDispIDs: TIntList; // int: disp ID; object: property name (referenced string)
    procedure Log(Level: TWMPWSZLogLevel; const Msg: String;
        const Args: array of const);
    procedure LogInfo(const Msg: String; const Args: array of const);
    procedure LogWarning(const Msg: String; const Args: array of const);
    procedure ParseAttrib(Stream: TSafeStream; EndOfElement: Longint);
    procedure ParseElement(Stream: TSafeStream; Flags: TWMPWSZParseFlags);
    procedure LoadWMPTypeLib;
    function ResolveWMPObjectCLSID(const CLSID: TCLSID; var Name: String): Boolean;
    function ResolveWMPDispID(ID: Integer): String;
    procedure LoadWMPDispIDsForType(TypInfo: ITypeInfo);
    procedure LoadWMPDispIDs;
    procedure FreeWMPDispIDs;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Stream: TStream);
    property LogProc: TWMPWSZLogProc read fLogProc write fLogProc;
  end;

function ParseWSZAttributeType(WSZType: Byte;
    out Typ: TWMPWSZAttributeType): Boolean;

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
  fWMPDispIDs := TIntList.Create;
  fWMPDispIDs.Sorted := True; // for faster lookup
  fWMPDispIDs.Duplicates := dupError;
  LoadWMPTypeLib;
  LoadWMPDispIDs;
end;

destructor TWMPWSZParser.Destroy;
begin
  FreeWMPDispIDs;
  fWMPTypeLib := nil;
  FreeAndNil(fWMPDispIDs);
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
    wszatWmpProp: begin
      Stream.SkipPaddingInteger(EndOfAttrib);
      AttrDispId := Stream.ReadWord(EndOfAttrib);
      Stream.SkipPaddingWord(EndOfAttrib);
      AttribValue := Stream.ReadWideString(EndOfAttrib);
      LogInfo('  dispid %u %s=%s',
          [AttrDispId, ResolveWMPDispID(AttrDispId), String(AttribValue)]);
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
        wszatGlobalVar:
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

procedure TWMPWSZParser.ParseElement(Stream: TSafeStream;
    Flags: TWMPWSZParseFlags);
var
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
  LogInfo('Element found: type=0x%.02x, next=%u, first=%u',
      [ElementType, NextSiblingOffset, FirstChildOffset]);
  if (wszpfIsRootElement in Flags)
      and (ElementType <> WSZ_ELEMENTID_THEME) then begin
    raise EWMPWSZParseError.CreateFmt(Stream.Position,
        'Root element isn''t a THEME element, type: %u', [ElementType]);
  end
  else if not (wszpfIsRootElement in Flags)
      and (ElementType = WSZ_ELEMENTID_THEME) then begin
    raise EWMPWSZParseError.CreateFmt(Stream.Position,
        'Found a THEME element that isn''t the root element', []);
  end;
  NumChildren := Stream.ReadByte(EndOfElement);
  NumAttribs := Stream.ReadByte(EndOfElement);
  case ElementType of
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
      ResolveWMPObjectCLSID(CLSID, ObjName);
      LogInfo('  object=%s', [ObjName]);
    end
    else begin
      // Named element
      ElementName := Stream.ReadWideString(EndOfElement);
      LogInfo('  name=%s', [ElementName]);
    end;
  end;
  for i := 0 to NumAttribs - 1 do
    ParseAttrib(Stream, EndOfElement);
  for i := 0 to NumChildren - 1 do
    ParseElement(Stream, []);
end;

procedure TWMPWSZParser.Parse(Stream: TStream);
var
  SafeStream: TSafeStream;
begin
  Stream.Seek(0, soFromBeginning);
  SafeStream := TSafeStream.Create(Stream);
  try
    ParseElement(SafeStream, [wszpfIsRootElement]);
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

function TWMPWSZParser.ResolveWMPObjectCLSID(const CLSID: TCLSID;
    var Name: String): Boolean;
var
  TypInfo: ITypeInfo;
  ObjName: WideString;
begin
  Result := False;
  Name := GUIDToString(CLSID);
  if not Assigned(fWMPTypeLib) then
    Exit;
  try
    if not Succeeded(fWMPTypeLib.GetTypeInfoOfGuid(CLSID, TypInfo)) then
      Exit;
    if not Succeeded(TypInfo.GetDocumentation(MEMBERID_NIL,
        @ObjName, nil, nil, nil)) then
      Exit;
    if Length(ObjName) > 0 then begin
      Result := True;
      Name := String(ObjName) + ' ' + Name;
    end;
  finally
    if not Result then
      LogWarning('Unknown object for CLSID %s', [GUIDToString(CLSID)]);
  end;
end;

function TWMPWSZParser.ResolveWMPDispID(ID: Integer): String;
var
  Idx: Integer;
begin
  if fWMPDispIDs.Find(ID, Idx) then
    Result := String(fWMPDispIDs.Objects[Idx])
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
  pFuncName: Pointer;
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
          pFuncName := nil;
          if Length(FuncName) > 0 then try
            pFuncName := RefString(FuncName);
            fWMPDispIDs.AddObject(pFunDesc.memid, pFuncName);
          except
            on EStringListError do begin
              ReleaseString(pFuncName);
              Continue;
            end;
          end;
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

procedure TWMPWSZParser.FreeWMPDispIDs;
begin
  while fWMPDispIDs.Count > 0 do begin
    ReleaseString(fWMPDispIDs.Objects[0]);
    fWMPDispIDs.Objects[0] := nil;
    fWMPDispIDs.Delete(0);
  end;
end;

function ParseWSZAttributeType(WSZType: Byte;
    out Typ: TWMPWSZAttributeType): Boolean;
begin
  Result := True;
  case WSZType of
    WSZ_ATTRIBUTE_TYPE_NAMED:       Typ := wszatNamed;
    WSZ_ATTRIBUTE_TYPE_WORDBOOL:    Typ := wszatWordBool;
    WSZ_ATTRIBUTE_TYPE_INTEGER:     Typ := wszatInteger;
    WSZ_ATTRIBUTE_TYPE_STRING:      Typ := wszatString;
    WSZ_ATTRIBUTE_TYPE_SYSINT:      Typ := wszatSysInt;
    WSZ_ATTRIBUTE_TYPE_RESSTRING:   Typ := wszAtResString;
    WSZ_ATTRIBUTE_TYPE_WMPENABLED:  Typ := wszatWmpEnabled;
    WSZ_ATTRIBUTE_TYPE_WMPPROP:     Typ := wszatWmpProp;
    WSZ_ATTRIBUTE_TYPE_NAMED_EVENT: Typ := wszatNamedEvent;
    WSZ_ATTRIBUTE_TYPE_GLOBAL_VAR:  Typ := wszatGlobalVar;
    else
      Result := False;
  end;
end;

end.
