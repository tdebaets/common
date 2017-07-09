(****************************************************************************
 *
 * Originally based on PathFunc.pas from Inno Setup
 * Copyright (C) 1997-2010 Jordan Russell
 * Portions by Martijn Laan
 * Modifications are Copyright 2017 Tim De Baets
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
 * Unicode version of PathFunc.pas
 *
 ****************************************************************************)

unit PathFuncWide;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related, MBCS-aware functions.

  These functions should always be used in lieu of their SysUtils counterparts
  since they aren't MBCS-aware on Delphi 2, and sometimes not MBCS-aware on
  Delphi 6 and 7 either (see QC#5096).

  $jrsoftware: issrc/Components/PathFunc.pas,v 1.43 2010/04/19 21:43:01 jr Exp $
}

{$DEFINE UNICODE}

interface

uses TntSysUtils, TntWideStrUtils, cUnicode;

function WideAddBackslash(const S: WideString): WideString;
function WidePathChangeExt(const Filename, Extension: WideString): WideString;
function WidePathWideCharCompare(const S1, S2: PWideChar): Boolean;
function WidePathCharIsSlash(const C: WideChar): Boolean;
function WidePathCharIsTrailByte(const S: WideString; const Index: Integer): Boolean;
function WidePathCharLength(const S: WideString; const Index: Integer): Integer;
function WidePathCombine(const Dir, Filename: WideString): WideString;
function WidePathCompare(const S1, S2: WideString): Integer;
function WidePathDrivePartLength(const Filename: WideString): Integer;
function WidePathDrivePartLengthEx(const Filename: WideString;
  const IncludeSignificantSlash: Boolean): Integer;
function WidePathExpand(const Filename: WideString): WideString;
function WidePathExtensionPos(const Filename: WideString): Integer;
function WidePathExtractDir(const Filename: WideString): WideString;
function WidePathExtractDrive(const Filename: WideString): WideString;
function WidePathExtractExt(const Filename: WideString): WideString;
function WidePathExtractName(const Filename: WideString): WideString;
function WidePathExtractPath(const Filename: WideString): WideString;
function WidePathIsRooted(const Filename: WideString): Boolean;
function WidePathLastChar(const S: WideString): PWideChar;
function WidePathLastDelimiter(const Delimiters, S: WideString): Integer;
function WidePathLowercase(const S: WideString): WideString;
function WidePathNormalizeSlashes(const S: WideString): WideString;
function WidePathPathPartLength(const Filename: WideString;
  const IncludeSlashesAfterPath: Boolean): Integer;
function WidePathPos(Ch: WideChar; const S: WideString): Integer;
function WidePathStartsWith(const S, AStartsWith: WideString): Boolean;
function WidePathStrNextChar(const S: PWideChar): PWideChar;
function WidePathStrPrevChar(const Start, Current: PWideChar): PWideChar;
function WidePathStrScan(const S: PWideChar; const C: WideChar): PWideChar;
function WideRemoveBackslash(const S: WideString): WideString;
function WideRemoveBackslashUnlessRoot(const S: WideString): WideString;

implementation

uses
  Windows, SysUtils;

function WideAddBackslash(const S: WideString): WideString;
{ Returns S plus a trailing backslash, unless S is an empty WideString or already
  ends in a backslash/slash. }
begin
  if (S <> '') and not WidePathCharIsSlash(WidePathLastChar(S)^) then
    Result := S + '\'
  else
    Result := S;
end;

function WidePathCharLength(const S: WideString; const Index: Integer): Integer;
{ Returns the length in bytes of the Character at Index in S.
  Notes:
  1. If Index specifies the last Character in S, 1 will always be returned,
     even if the last Character is a lead byte.
  2. If a lead byte is followed by a null Character (e.g. #131#0), 2 will be
     returned. This mimics the behavior of MultiByteToWideWideChar and WideCharPrev,
     but not WideCharNext(P)-P, which would stop on the null. }
begin
  {$IFNDEF UNICODE}
  if IsDBCSLeadByte(Ord(S[Index])) and (Index < Length(S)) then
    Result := 2
  else
  {$ENDIF}
    Result := 1;
end;

function WidePathCharIsSlash(const C: WideChar): Boolean;
{ Returns True if C is a backslash or slash. }
begin
  Result := (C = '\') or (C = '/');
end;

function WidePathCharIsTrailByte(const S: WideString; const Index: Integer): Boolean;
{ Returns False if S[Index] is a single byte Character or a lead byte.
  Returns True otherwise (i.e. it must be a trail byte). }
var
  I: Integer;
begin
  I := 1;
  while I <= Index do begin
    if I = Index then begin
      Result := False;
      Exit;
    end;
    Inc(I, WidePathCharLength(S, I));
  end;
  Result := True;
end;

function WidePathWideCharCompare(const S1, S2: PWideChar): Boolean;
{ Compares two first Characters, and returns True if they are equal. }
var
  N, I: Integer;
begin
  N := WidePathStrNextChar(S1) - S1;
  if N = WidePathStrNextChar(S2) - S2 then begin
    for I := 0 to N-1 do begin
      if S1[I] <> S2[I] then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end else
    Result := False;
end;

function WidePathChangeExt(const Filename, Extension: WideString): WideString;
{ Takes Filename, removes any existing extension, then adds the extension
  specified by Extension and returns the resulting WideString. }
var
  I: Integer;
begin
  I := WidePathExtensionPos(Filename);
  if I = 0 then
    Result := Filename + Extension
  else
    Result := Copy(Filename, 1, I - 1) + Extension;
end;

function WidePathCombine(const Dir, Filename: WideString): WideString;
{ Combines a directory and filename into a path.
  If Dir is empty, it just returns Filename.
  If Filename is empty, it returns an empty WideString (ignoring Dir).
  If Filename begins with a drive letter or slash, it returns Filename
  (ignoring Dir).
  If Dir specifies only a drive letter and colon ('c:'), it returns
  Dir + Filename.
  Otherwise, it returns the equivalent of AddBackslash(Dir) + Filename. }
var
  I: Integer;
begin
  if (Dir = '') or (Filename = '') or WidePathIsRooted(Filename) then
    Result := Filename
  else begin
    I := WidePathCharLength(Dir, 1) + 1;
    if ((I = Length(Dir)) and (Dir[I] = ':')) or
       WidePathCharIsSlash(WidePathLastChar(Dir)^) then
      Result := Dir + Filename
    else
      Result := Dir + '\' + Filename;
  end;
end;

function WidePathCompare(const S1, S2: WideString): Integer;
{ Compares two filenames, and returns 0 if they are equal. }
begin
  Result := WideCompareStr(Tnt_WideLowerCase(S1), Tnt_WideLowerCase(S2));
end;

function WidePathDrivePartLength(const Filename: WideString): Integer;
begin
  Result := WidePathDrivePartLengthEx(Filename, False);
end;

function WidePathDrivePartLengthEx(const Filename: WideString;
  const IncludeSignificantSlash: Boolean): Integer;
{ Returns length of the drive portion of Filename, or 0 if there is no drive
  portion.
  If IncludeSignificantSlash is True, the drive portion can include a trailing
  slash if it is significant to the meaning of the path (i.e. 'x:' and 'x:\'
  are not equivalent, nor are '\' and '').
  If IncludeSignificantSlash is False, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 2  ('x:')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 0  ('')
  If IncludeSignificantSlash is True, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 3  ('x:\')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 1  ('\')
  Note: This is MBCS-safe, unlike the Delphi's ExtractFileDrive function.
  (Computer and share names can include multi-byte Characters!) }
var
  Len, I, C: Integer;
begin
  Len := Length(Filename);

  { \\server\share }
  if (Len >= 2) and WidePathCharIsSlash(Filename[1]) and WidePathCharIsSlash(Filename[2]) then begin
    I := 3;
    C := 0;
    while I <= Len do begin
      if WidePathCharIsSlash(Filename[I]) then begin
        Inc(C);
        if C >= 2 then
          Break;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        until (I > Len) or not WidePathCharIsSlash(Filename[I]);
      end
      else
        Inc(I, WidePathCharLength(Filename, I));
    end;
    Result := I - 1;
    Exit;
  end;

  { \ }
  { Note: Test this before 'x:' since '\:stream' means access stream 'stream'
    on the root directory of the current drive, not access drive '\:' }
  if (Len >= 1) and WidePathCharIsSlash(Filename[1]) then begin
    if IncludeSignificantSlash then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  { x: }
  if Len > 0 then begin
    I := WidePathCharLength(Filename, 1) + 1;
    if (I <= Len) and (Filename[I] = ':') then begin
      if IncludeSignificantSlash and (I < Len) and WidePathCharIsSlash(Filename[I+1]) then
        Result := I+1
      else
        Result := I;
      Exit;
    end;
  end;

  Result := 0;
end;

function WidePathIsRooted(const Filename: WideString): Boolean;
{ Returns True if Filename begins with a slash or drive ('x:').
  Equivalent to: PathDrivePartLengthEx(Filename, True) <> 0 }
var
  Len, I: Integer;
begin
  Result := False;
  Len := Length(Filename);
  if Len > 0 then begin
    { \ or \\ }
    if WidePathCharIsSlash(Filename[1]) then
      Result := True
    else begin
      { x: }
      I := WidePathCharLength(Filename, 1) + 1;
      if (I <= Len) and (Filename[I] = ':') then
        Result := True;
    end;
  end;
end;

function WidePathPathPartLength(const Filename: WideString;
  const IncludeSlashesAfterPath: Boolean): Integer;
{ Returns length of the path portion of Filename, or 0 if there is no path
  portion.
  Note these differences from Delphi's ExtractFilePath function:
  - The result will never be less than what PathDrivePartLength returns.
    If you pass a UNC root path, e.g. '\\server\share', it will return the
    length of the entire WideString, NOT the length of '\\server\'.
  - If you pass in a filename with a reference to an NTFS alternate data
    stream, e.g. 'abc:def', it will return the length of the entire WideString,
    NOT the length of 'abc:'. }
var
  LastWideCharToKeep, Len, I: Integer;
begin
  Result := WidePathDrivePartLengthEx(Filename, True);
  LastWideCharToKeep := Result;
  Len := Length(Filename);
  I := Result + 1;
  while I <= Len do begin
    if WidePathCharIsSlash(Filename[I]) then begin
      if IncludeSlashesAfterPath then
        Result := I
      else
        Result := LastWideCharToKeep;
      Inc(I);
    end
    else begin
      Inc(I, WidePathCharLength(Filename, I));
      LastWideCharToKeep := I-1;
    end;
  end;
end;

function WidePathExpand(const Filename: WideString): WideString;
{ Like Delphi's ExpandFileName, but does proper error checking. }
var
  Res: Integer;
  FilePart: PWideChar;
  Buf: array[0..4095] of WideChar;
begin
  DWORD(Res) := GetFullPathNameW(PWideChar(Filename), SizeOf(Buf) div SizeOf(Buf[0]),
    Buf, FilePart);
  if (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0])) then
    SetString(Result, Buf, Res)
  else
    Result := Filename;
end;

function WidePathExtensionPos(const Filename: WideString): Integer;
{ Returns index of the last '.' Character in the filename portion of Filename,
  or 0 if there is no '.' in the filename portion.
  Note: Filename is assumed to NOT include an NTFS alternate data stream name
  (i.e. 'filename:stream'). }
var
  Len, I: Integer;
begin
  Result := 0;
  Len := Length(Filename);
  I := WidePathPathPartLength(Filename, True) + 1;
  while I <= Len do begin
    if Filename[I] = '.' then begin
      Result := I;
      Inc(I);
    end
    else
      Inc(I, WidePathCharLength(Filename, I));
  end;
end;

function WidePathExtractDir(const Filename: WideString): WideString;
{ Like PathExtractPath, but strips any trailing slashes, unless the resulting
  path is the root directory of a drive (i.e. 'C:\' or '\'). }
var
  I: Integer;
begin
  I := WidePathPathPartLength(Filename, False);
  Result := Copy(Filename, 1, I);
end;

function WidePathExtractDrive(const Filename: WideString): WideString;
{ Returns the drive portion of Filename (either 'x:' or '\\server\share'),
  or an empty WideString if there is no drive portion. }
var
  L: Integer;
begin
  L := WidePathDrivePartLength(Filename);
  if L = 0 then
    Result := ''
  else
    Result := Copy(Filename, 1, L);
end;

function WidePathExtractExt(const Filename: WideString): WideString;
{ Returns the extension portion of the last component of Filename (e.g. '.txt')
  or an empty WideString if there is no extension. }
var
  I: Integer;
begin
  I := WidePathExtensionPos(Filename);
  if I = 0 then
    Result := ''
  else
    Result := Copy(Filename, I, Maxint);
end;

function WidePathExtractName(const Filename: WideString): WideString;
{ Returns the filename portion of Filename (e.g. 'filename.txt'). If Filename
  ends in a slash or consists only of a drive part, the result will be an empty
  WideString.
  This function is essentially the opposite of PathExtractPath. }
var
  I: Integer;
begin
  I := WidePathPathPartLength(Filename, True);
  Result := Copy(Filename, I + 1, Maxint);
end;

function WidePathExtractPath(const Filename: WideString): WideString;
{ Returns the path portion of Filename (e.g. 'c:\dir\'). If Filename contains
  no drive part or slash, the result will be an empty WideString.
  This function is essentially the opposite of PathExtractName. }
var
  I: Integer;
begin
  I := WidePathPathPartLength(Filename, True);
  Result := Copy(Filename, 1, I);
end;

function WidePathLastChar(const S: WideString): PWideChar;
{ Returns pointer to last Character in the WideString. Is MBCS-aware. Returns nil
  if the WideString is empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := WidePathStrPrevChar(Pointer(S), @S[Length(S)+1]);
end;

function WidePathLastDelimiter(const Delimiters, S: WideString): Integer;
{ Returns the index of the last occurrence in S of one of the Characters in
  Delimiters, or 0 if none were found.
  Note: S is allowed to contain null Characters. }
var
  P, E: PWideChar;
begin
  Result := 0;
  if (S = '') or (Delimiters = '') then
    Exit;
  P := Pointer(S);
  E := @P[Length(S)];
  while P < E do begin
    if P^ <> #0 then begin
      if WStrScan(PWideChar(Pointer(Delimiters)), P^) <> nil then
        Result := (P - PWideChar(Pointer(S))) + 1;
      P := WidePathStrNextChar(P);
    end
    else
      Inc(P);
  end;
end;

function WidePathLowercase(const S: WideString): WideString;
{ Converts the specified path name to lowercase }
{$IFNDEF UNICODE}
var
  I, L: Integer;
{$ENDIF}
begin
  {$IFNDEF UNICODE}
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) and
     (GetSystemMetrics(SM_DBCSENABLED) <> 0) then begin
    { Japanese Windows 98's handling of double-byte Roman Characters in
      filenames is case sensitive, so we can't change the case of double-byte
      Characters. (Japanese Windows NT/2000 is case insensitive, on both FAT
      and NTFS, in my tests.) Based on code from AnsiLowerCaseFileName. }
    Result := S;
    L := Length(Result);
    I := 1;
    while I <= L do begin
      if Result[I] in ['A'..'Z'] then begin
        Inc(Byte(Result[I]), 32);
        Inc(I);
      end
      else
        Inc(I, WidePathWideCharLength(Result, I));
    end;
  end
  else
  {$ENDIF}
    Result := WideLowerCase(S);
end;

function WidePathPos(Ch: WideChar; const S: WideString): Integer;
{ This is an MBCS-aware Pos function. }
var
  Len, I: Integer;
begin
  Len := Length(S);
  I := 1;
  while I <= Len do begin
    if S[I] = Ch then begin
      Result := I;
      Exit;
    end;
    Inc(I, WidePathCharLength(S, I));
  end;
  Result := 0;
end;

function WidePathNormalizeSlashes(const S: WideString): WideString;
{ Returns S minus any superfluous slashes, and with any forward slashes
  converted to backslashes. For example, if S is 'C:\\\some//path', it returns
  'C:\some\path'. Does not remove a double backslash at the beginning of the
  WideString, since that signifies a UNC path. }
var
  Len, I: Integer;
begin
  Result := S;
  Len := Length(Result);
  I := 1;
  while I <= Len do begin
    if Result[I] = '/' then
      Result[I] := '\';
    Inc(I, WidePathCharLength(Result, I));
  end;
  I := 1;
  while I < Length(Result) do begin
    if (Result[I] = '\') and (Result[I+1] = '\') and (I > 1) then
      Delete(Result, I+1, 1)
    else
      Inc(I, WidePathCharLength(Result, I));
  end;
end;

function WidePathStartsWith(const S, AStartsWith: WideString): Boolean;
{ Returns True if S starts with (or is equal to) AStartsWith. Uses path casing
  rules, and is MBCS-aware. }
var
  AStartsWithLen: Integer;
begin
  AStartsWithLen := Length(AStartsWith);
  if Length(S) = AStartsWithLen then
    Result := (WidePathCompare(S, AStartsWith) = 0)
  else if (Length(S) > AStartsWithLen) and not WidePathCharIsTrailByte(S, AStartsWithLen+1) then
    Result := (WidePathCompare(Copy(S, 1, AStartsWithLen), AStartsWith) = 0)
  else
    Result := False;
end;

function WidePathStrNextChar(const S: PWideChar): PWideChar;
{ Returns pointer to the Character after S, unless S points to a null (#0).
  Is MBCS-aware. }
begin
  {$IFNDEF UNICODE}
  Result := WideCharNext(S);
  {$ELSE}
  Result := S;
  if Result^ <> #0 then
    Inc(Result);
  {$ENDIF}
end;

function WidePathStrPrevChar(const Start, Current: PWideChar): PWideChar;
{ Returns pointer to the Character before Current, unless Current = Start.
  Is MBCS-aware. }
begin
  {$IFNDEF UNICODE}
  Result := WideCharPrev(Start, Current);
  {$ELSE}
  Result := Current;
  if Result > Start then
    Dec(Result);
  {$ENDIF}
end;

function WidePathStrScan(const S: PWideChar; const C: WideChar): PWideChar;
{ Returns pointer to first occurrence of C in S, or nil if there are no
  occurrences. Like StrScan, but MBCS-aware.
  Note: As with StrScan, specifying #0 for the search Character is legal. }
begin
  Result := S;
  while Result^ <> C do begin
    if Result^ = #0 then begin
      Result := nil;
      Break;
    end;
    Result := WidePathStrNextChar(Result);
  end;
end;

function WideRemoveBackslash(const S: WideString): WideString;
{ Returns S minus any trailing slashes. Use of this function is discouraged;
  use RemoveBackslashUnlessRoot instead when working with file system paths. }
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and WidePathCharIsSlash(WidePathStrPrevChar(Pointer(S), @S[I+1])^) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

function WideRemoveBackslashUnlessRoot(const S: WideString): WideString;
{ Returns S minus any trailing slashes, unless S specifies the root directory
  of a drive (i.e. 'C:\' or '\'), in which case it leaves 1 slash. }
var
  DrivePartLen, I: Integer;
begin
  DrivePartLen := WidePathDrivePartLengthEx(S, True);
  I := Length(S);
  while (I > DrivePartLen) and WidePathCharIsSlash(WidePathStrPrevChar(Pointer(S), @S[I+1])^) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

end.
