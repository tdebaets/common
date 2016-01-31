unit MBCSUtil;

{
  Inno Setup
  Copyright (C) 1998-2001 Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some MBCS-aware string functions for Delphi 2.

  IMPORTANT: MBCSUtil must be always specified in the 'uses' clause AFTER
  SysUtils for it to override the original functions.

  $Id: MBCSUtil.pas,v 1.4 2001/03/30 18:11:46 jr Exp $
}

interface

{$IFNDEF VER80}
{$IFNDEF VER90}
{$IFNDEF VER93}
  {$DEFINE Delphi3orHigher}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF Delphi3orHigher}
function AnsiCompareFileName (const S1, S2: String): Integer;
function AnsiLastChar (const S: String): PChar;
function AnsiLowerCaseFileName (const S: String): String;
function AnsiUpperCaseFileName (const S: String): String;
function AnsiPos (Ch: Char; const S: String): Integer;
function AnsiStrScan (Str: PChar; Ch: Char): PChar;
function ChangeFileExt (const Filename, Extension: String): String;
function ExtractFileExt (const Filename: String): String;
function ExtractFileName (const Filename: String): String;
function ExtractFilePath (const Filename: String): String;
function LastDelimiter (const Delimiters, S: String): Integer;
{$ENDIF}
function IsLeadByte (Ch: Char): Boolean;
function IsMBCS: Boolean;

{$IFNDEF Delphi3orHigher}
var
  LeadBytes: set of Char;
{$ENDIF}

{
  Notes:
  - It seems there is no need for an MBCS 'ExtractFileDrive' function. There
    is no MBCS-related code in the ExtractFileDrive functions of the latest
    Delphi versions (3.0-5.01).
  - This unit does not provide an MBCS 'ExtractFileDir' function, as I have
    no need for one. (Like the other Extract* functions, ExtractFileDir is
    NOT MBSC-aware in Delphi 2.)
}

implementation

uses
  Windows, SysUtils;

{$IFNDEF Delphi3orHigher}
var
  UsingMBCS: Boolean;

procedure InitMBCS;
var
  AnsiCPInfo: TCPInfo;
  I: Integer;
  J: Byte;
begin
  UsingMBCS := GetSystemMetrics(SM_DBCSENABLED) <> 0;
  if UsingMBCS then begin
    GetCPInfo (CP_ACP, AnsiCPInfo);
    with AnsiCPInfo do begin
      I := 0;
      while (I < MAX_LEADBYTES) and ((LeadByte[I] or LeadByte[I+1]) <> 0) do begin
        for J := LeadByte[I] to LeadByte[I+1] do
          Include (LeadBytes, Char(J));
        Inc (I, 2);
      end;
    end;
  end;
end;

function AnsiLastChar (const S: String): PChar;
{ This is an MBCS-aware function to obtain the last character in a string.
  Should be functionally identical to the Delphi 3+ AnsiLastChar function.
  Returns nil if the string is empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := CharPrev(Pointer(S), @S[Length(S)+1]);
end;

function AnsiStrScan (Str: PChar; Ch: Char): PChar;
{ This is an MBCS-aware StrScan function.
  Should be functionally identical to the Delphi 3+ AnsiStrScan function. }
begin
  if not UsingMBCS then begin
    Result := StrScan(Str, Ch);
    Exit;
  end;
  while True do begin
    if Str^ = Ch then begin
      Result := Str;
      Exit;
    end;
    if Str^ = #0 then  { check for #0 second, since Ch is allowed to be #0 }
      Break;
    Str := CharNext(Str);
  end;
  Result := nil;
end;

function AnsiPos (Ch: Char; const S: String): Integer;
{ This is an MBCS-aware Pos function.
  Should be functionally identical to the Delphi 3+ AnsiPos function, with the
  exception that this function can only search for a single character.
  Like the Delphi 3+ function, this function cannot search for or past null
  characters. }
var
  P: PChar;
begin
  Result := 0;
  if (Ch <> #0) and (S <> '') then begin
    P := AnsiStrScan(Pointer(S), Ch);
    if P <> nil then
      Result := (P - Pointer(S)) + 1;
  end;
end;

function LastDelimiter (const Delimiters, S: string): Integer;
{ This should be functionally identical to the Delphi 3+ LastDelimiter
  function. }
var
  P, E: PChar;
begin
  Result := 0;
  if (S = '') or (Delimiters = '') then
    Exit;
  P := Pointer(S);
  E := @P[Length(S)];
  while P < E do begin
    if P^ <> #0 then begin
      if StrScan(Pointer(Delimiters), P^) <> nil then
        Result := (P - Pointer(S)) + 1;
      P := CharNext(P);
    end
    else
      Inc (P);
  end;
end;

function ExtractFilePath (const Filename: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('\:', Filename);
  Result := Copy(Filename, 1, I);
end;

function ExtractFileName (const Filename: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('\:', Filename);
  Result := Copy(Filename, I + 1, Maxint);
end;

function ExtractFileExt (const Filename: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', Filename);
  if (I > 0) and (Filename[I] = '.') then
    Result := Copy(Filename, I, Maxint)
  else
    Result := '';
end;

function ChangeFileExt (const Filename, Extension: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', Filename);
  if (I = 0) or (Filename[I] <> '.') then
    I := Maxint;
  Result := Copy(Filename, 1, I - 1) + Extension;
end;

function AnsiCompareFileName(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(AnsiLowerCaseFileName(S1), AnsiLowerCaseFileName(S2));
end;

function AnsiLowerCaseFileName(const S: string): string;
var
  I,L: Integer;
begin
  if UsingMBCS then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['A'..'Z'] then Inc(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;

function AnsiUpperCaseFileName(const S: string): string;
var
  I,L: Integer;
begin
  if UsingMBCS then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['a'..'z'] then Dec(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiUpperCase(S);
end;
{$ENDIF}

function IsMBCS: Boolean;
begin
  {$IFNDEF Delphi3orHigher}
  Result := UsingMBCS;
  {$ELSE}
  Result := SysLocale.FarEast;
  {$ENDIF}
end;

function IsLeadByte (Ch: Char): Boolean;
begin
  Result := Ch in LeadBytes;
end;

{$IFNDEF Delphi3orHigher}
initialization
  InitMBCS;
{$ENDIF}
end.
