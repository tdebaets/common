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
 * TStream-related utility code
 *
 ****************************************************************************)

unit StreamUtil;

interface

uses Windows, Classes, SysUtils, Common2;

type
  TBundledStream = class
  private
    fhModule: HMODULE;
    fStream: TStream;
  public
    constructor CreateFromResource(const ResFilePath: String;
        const ResName: String; pResType: PChar);
    destructor Destroy; override;
    property Stream: TStream read fStream;
  end;

type
  ESafeStreamError = class(Exception)
  private
    fPosition: Longint;
  public
    constructor CreateFmt(Position: Longint; const Msg: String;
        const Args: array of const);
    property Position: Longint read fPosition;
  end;

type
  TSafeStream = class
  private
    fStream: TStream;
    function GetPosition: Longint;
    procedure CheckSize(Size: Longint; EndOfRegion: Longint);
  public
    constructor Create(Stream: TStream);
    procedure SkipPaddingWord(EndOfRegion: Longint);
    procedure SkipPaddingInteger(EndOfRegion: Longint);
    function ReadByte(EndOfRegion: Longint): Byte;
    function ReadWord(EndOfRegion: Longint): Word;
    function ReadInteger(EndOfRegion: Longint): Integer;
    function ReadGUID(EndOfRegion: Longint): TGUID;
    function ReadWideString(EndOfRegion: Longint): WideString;
    property Position: Longint read GetPosition;
  end;

implementation

constructor TBundledStream.CreateFromResource(const ResFilePath: String;
    const ResName: String; pResType: PChar);
begin
  inherited Create;
  fhModule := LoadResourceDll(ResFilePath);
  if fhModule = 0 then
    raise Exception.CreateFmt('Failed to load resource file %s', [ResFilePath]);
  fStream := TResourceStream.Create(fhModule, ResName, pResType);
end;

destructor TBundledStream.Destroy;
begin
  if Assigned(fStream) then
    FreeAndNil(fStream);
  if fhModule <> 0 then
    FreeLibrary(fhModule);
  fhModule := 0;
  inherited;
end;

constructor ESafeStreamError.CreateFmt(Position: Longint; const Msg: String;
    const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  fPosition := Position;
end;

constructor TSafeStream.Create(Stream: TStream);
begin
  fStream := Stream;
end;

function TSafeStream.GetPosition: Longint;
begin
  Result := fStream.Position;
end;

procedure TSafeStream.CheckSize(Size: Longint; EndOfRegion: Longint);
begin
  if EndOfRegion <> 0 then begin
    if fStream.Position + Size > EndOfRegion then begin
      raise ESafeStreamError.CreateFmt(fStream.Position,
          'Read past end of safe region: %u > %u',
          [fStream.Position + Size, EndOfRegion]);
    end;
  end;
end;

procedure TSafeStream.SkipPaddingWord(EndOfRegion: Longint);
var
  Padding: Word;
begin
  Padding := ReadWord(EndOfRegion);
  if Padding <> 0 then begin
    raise ESafeStreamError.CreateFmt(fStream.Position,
        'Padding isn''t all zeroes: 0x%.04x', [Padding]);
  end;
end;

procedure TSafeStream.SkipPaddingInteger(EndOfRegion: Longint);
var
  Padding: Integer;
begin
  Padding := ReadInteger(EndOfRegion);
  if Padding <> 0 then begin
    raise ESafeStreamError.CreateFmt(fStream.Position,
        'Padding isn''t all zeroes: 0x%.08x', [Padding]);
  end;
end;

function TSafeStream.ReadByte(EndOfRegion: Longint): Byte;
begin
  CheckSize(SizeOf(Result), EndOfRegion);
  Result := 0;
  fStream.ReadBuffer(Result, SizeOf(Result));
end;

function TSafeStream.ReadWord(EndOfRegion: Longint): Word;
begin
  CheckSize(SizeOf(Result), EndOfRegion);
  Result := 0;
  fStream.ReadBuffer(Result, SizeOf(Result));
end;

function TSafeStream.ReadInteger(EndOfRegion: Longint): Integer;
begin
  CheckSize(SizeOf(Result), EndOfRegion);
  Result := 0;
  fStream.ReadBuffer(Result, SizeOf(Result));
end;

function TSafeStream.ReadGUID(EndOfRegion: Longint): TGUID;
begin
  CheckSize(SizeOf(Result), EndOfRegion);
  FillChar(Result, SizeOf(Result), 0);
  fStream.ReadBuffer(Result, SizeOf(Result));
end;

// TODO: optimize by reading into single buffer
function TSafeStream.ReadWideString(EndOfRegion: Longint): WideString;
var
  Ch: WideChar;
begin
  Result := '';
  while True do begin
    CheckSize(SizeOf(Ch), EndOfRegion);
    fStream.ReadBuffer(Ch, SizeOf(Ch));
    if Ch = WideChar(0) then
      Break;
    Result := Result + Ch;
  end
end;

end.
