(****************************************************************************
 *
 * Copyright 2018 Tim De Baets
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
 * Unicode-aware utility code
 *
 ****************************************************************************)

unit CommonUnicode;

interface

uses Windows, Common2, SysUtils, TntSysUtils, TntClasses, PathFuncWide,
    JclWideFormat;

function RealWideFormat(const Format: WideString;
    const Args: array of const): WideString;
procedure WideSplit(const S: WideString; Separator: WideChar;
    MyStrings: TTntStrings);
function WideStringChangeEx(var S: WideString;
    const FromStr, ToStr: WideString): Integer;
function WideDupeString(const AText: WideString; ACount: Integer): WideString;
function WideEscapeChars(const S: WideString; const CharsToEscapeSet: TSysCharSet;
    EscapeChar: WideChar): WideString;
function WideNewPathExtractExt(const Filename: WideString): WideString;
function WideGetParentDirectory(const Path: WideString): WideString;
function WideIsFileOrDirReadOnly(const FileName: WideString;
    var ReadOnly: Boolean): Boolean;
function QuoteArgvForCmdLine(const Argv: WideString): WideString;

implementation

function RealWideFormat(const Format: WideString;
    const Args: array of const): WideString;
begin
  // WideFormat in TntSysUtils doesn't really support Unicode, this one does
  Result := JclWideFormat.WideFormat(Format, Args);
end;

function GetNextToken(const S: WideString; Separator: WideChar;
    var StartPos: Integer): WideString;
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

procedure WideSplit(const S: WideString; Separator: WideChar;
    MyStrings: TTntStrings);
var
  Start: Integer;
begin
  Start := 1;
  while Start <= Length(S) do
    MyStrings.Add(GetNextToken(S, Separator, Start));
end;

function WideStringChangeEx(var S: WideString;
    const FromStr, ToStr: WideString): Integer;
{ Changes all occurrences in S of FromStr to ToStr. If SupportDBCS is True
  (recommended), double-byte character sequences in S are recognized and
  handled properly. Otherwise, the function behaves in a binary-safe manner.
  Returns the number of times FromStr was matched and changed. }
var
  FromStrLen, I, EndPos, J: Integer;
  IsMatch: Boolean;
label 1;
begin
  Result := 0;
  if FromStr = '' then Exit;
  FromStrLen := Length(FromStr);
  I := 1;
1:EndPos := Length(S) - FromStrLen + 1;
  while I <= EndPos do begin
    IsMatch := True;
    J := 0;
    while J < FromStrLen do begin
      if S[J+I] <> FromStr[J+1] then begin
        IsMatch := False;
        Break;
      end;
      Inc(J);
    end;
    if IsMatch then begin
      Inc(Result);
      Delete(S, I, FromStrLen);
      Insert(ToStr, S, I);
      Inc(I, Length(ToStr));
      goto 1;
    end;
    Inc(I);
  end;
end;

function WideDupeString(const AText: WideString; ACount: Integer): WideString;
var
  i: Integer;
begin
  Result := '';
  if ACount < 1 then
    Exit;
  for i := 1 to ACount do
    Result := Result + AText;
end;

function WideEscapeChars(const S: WideString; const CharsToEscapeSet: TSysCharSet;
    EscapeChar: WideChar): WideString;
  // The following subprocedure was copied from TntSysUtils.WideWrapText
  function WideCharIn(C: WideChar; const SysCharSet: TSysCharSet): Boolean;
  begin
    Result := (C <= High(AnsiChar)) and (AnsiChar(C) in SysCharSet);
  end;
  procedure AppendResultChar(Char: WideChar);
  begin
    Result := Result + Char;
  end;
var
  pS: PWideChar;
begin
  Result := '';
  pS := PWideChar(S);
  while pS^ <> #0 do begin
    if WideCharIn(pS^, CharsToEscapeSet) then
      AppendResultChar(EscapeChar);
    AppendResultChar(pS^);
    Inc(pS);
  end;
end;

// Like WidePathExtractExt, but the result doesn't include the separating period.
function WideNewPathExtractExt(const Filename: WideString): WideString;
begin
  Result := WidePathExtractExt(Filename);
  if Length(Result) > 0 then
    Delete(Result, 1, 1);
end;

// Returns the parent directory for the provided "path" (file or directory)
function WideGetParentDirectory(const Path: WideString): WideString;
begin
  Result := WidePathExpand(WideAddBackSlash(Path) + '..');
end;

function WideIsFileOrDirReadOnly(const FileName: WideString;
    var ReadOnly: Boolean): Boolean;
var
  Attr: Cardinal;
begin
  Attr := WideFileGetAttr(FileName);
  Result := (Attr <> INVALID_FILE_ATTRIBUTES);
  if Result then
    ReadOnly := (Attr and FILE_ATTRIBUTE_READONLY <> 0);
end;

// This function quotes an argument for inclusion in a command line. It
// effectively encodes an argument such that the CommandLineToArgvW API function
// should decode it to its original string value.
// See http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx
// See http://blogs.msdn.com/b/oldnewthing/archive/2010/09/17/10063629.aspx
function QuoteArgvForCmdLine(const Argv: WideString): WideString;
  procedure AppendResultStr(const Str: WideString);
  begin
    Result := Result + Str;
  end;
  procedure AppendResultChar(Char: WideChar);
  begin
    Result := Result + Char;
  end;
var
  pArgv: PWideChar;
  NumBackslashes: Integer;
begin
  Result := '"';
  pArgv := PWideChar(Argv);
  while pArgv^ <> #0 do begin
    NumBackslashes := 0;
    while pArgv^ = '\' do begin
      Inc(NumBackslashes);
      Inc(pArgv);
    end;
    if pArgv^ = #0 then begin
      // Escape all backslashes, but let the terminating double quotation mark
      // we add below be interpreted as a metacharacter.
      AppendResultStr(WideDupeString('\', NumBackslashes * 2));
      Break;
    end
    else if pArgv^ = '"' then begin
      // Escape all backslashes and the following double quotation mark.
      AppendResultStr(WideDupeString('\', NumBackslashes * 2 + 1));
      AppendResultChar(pArgv^);
    end
    else begin
      // Backslashes aren't special here.
      AppendResultStr(WideDupeString('\', NumBackslashes));
      AppendResultChar(pArgv^);
    end;
    Inc(pArgv);
  end;
  AppendResultChar('"');
end;

end.
