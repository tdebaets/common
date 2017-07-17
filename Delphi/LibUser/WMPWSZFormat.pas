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

uses Windows, Classes, SysUtils, Common2, StreamUtil, ComObj;

const
  WSZ_ELEMENTID_THEME   = $0B;
  WSZ_ELEMENTID_VIEW    = $0C;
  WSZ_ELEMENTID_SUBVIEW = $0D;
  WSZ_ELEMENTID_CLSID   = $82;
  WSZ_ELEMENTID_CLSID_2 = $87;
  WSZ_ELEMENTID_CLSID_3 = $88;

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
    procedure Log(Level: TWMPWSZLogLevel; const Msg: String;
        const Args: array of const);
    procedure LogInfo(const Msg: String; const Args: array of const);
    procedure LogWarning(const Msg: String; const Args: array of const);
    procedure ParseAttrib(Stream: TSafeStream; EndOfElement: Longint);
    procedure ParseElement(Stream: TSafeStream; Flags: TWMPWSZParseFlags);
  public
    procedure Parse(Stream: TStream);
    property LogProc: TWMPWSZLogProc read fLogProc write fLogProc;
  end;

function ParseWSZAttributeType(WSZType: Byte;
    out Typ: TWMPWSZAttributeType): Boolean;

implementation

constructor EWMPWSZParseError.CreateFmt(Position: Longint; const Msg: String;
    const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  fPosition := Position;
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
      LogInfo('  dispid %u=%s', [AttrDispId, AttribValue]);
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
      LogInfo('  dispid %u=%s', [AttrDispId, String(AttribValue)]);
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
    end;
    WSZ_ELEMENTID_CLSID,
    WSZ_ELEMENTID_CLSID_2,
    WSZ_ELEMENTID_CLSID_3: begin
      // CLSID element
      Stream.SkipPaddingWord(EndOfElement);
      CLSID := Stream.ReadGUID(EndOfElement);
      LogInfo('  clsid=%s', [GUIDToString(CLSID)]);
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
