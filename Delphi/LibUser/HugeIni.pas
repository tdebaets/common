(****************************************************************************
 *
 * Copyright 2016 Tim De Baets
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
 * THugeIni Component
 * Taken from aoMisc.pas by Jason Swager
 * Contact Information:
 * Author: Jason Swager
 * Email: jswager@alohaoi.com
 * Web: http://www.alohaoi.com
 *
 ****************************************************************************)

unit HugeIni;

interface

uses
  SysUtils, MBCSUtil, Classes;

type

  {The standard Delphi TINIFile component has a 64K limitation. Further
  investigation showed that this was not due to Delphi, but to the WinAPI. This
  problem is apparent in both 16bit and 32bit Windows platforms. This is the
  replacement to the TINIFile component that solves this problem. This
  component is not as fast as the WinAPI calls - especially when the INI file
  is over 64K - but it will always work. For best results, set Delay = True and
  call WriteFile when you have finished manipulating the file. The methods
  and properties of this object correspond to the TINIFile object.}
  THugeINI = class(TObject)
  private
    FDelay   : Boolean;
    FFileName: String;
    FKeys    : TList;
    FSections: TStringList;
    FValues  : TList;
  protected
    function  AddKey(Section: Integer; Key: String): Integer; virtual;
    function  AddSection(Section: String): Integer; virtual;
    procedure ExtractKeyValue(Line: String; var Key: String; var Value: String); virtual;
    function  ExtractSection(Line: String): String; virtual;
    function  GetGenericByIndex(Section: Integer; IndexType: Integer): TStringList; virtual;
    function  GetGeneric(Section: String; IndexType: Integer): TStringList; virtual;
    function  GetLocation(Section, Key: String; var S, K: Integer; Create: Boolean): Boolean; virtual;
    function  GetSectionCount: Integer; virtual;
    function  GetValue(Section, Key: Integer): String; virtual;
    function  KeyIndex(Section: Integer; Key: String): Integer; virtual;
    procedure ReadFile; virtual;
    function  SectionIndex(Section: String): Integer; virtual;
    procedure SetValue(Section, Key: Integer; Value: String); virtual;
    {Number of sections in the INI file.}
    property SectionCount: Integer read GetSectionCount;
    {Returns the set of keys associated with the specified section.}
    property Keys[Section: Integer]: TStringList index 0 read GetGenericByIndex;
    {Returns or sets a single string value of the specified section and key.}
    property Value[Section, Key: Integer]: String read GetValue write SetValue;
    {Returns the set of values associated with the specified section.}
    property Values[Section: Integer]: TStringList index 1 read GetGenericByIndex;
  public
    constructor Create(FileName: String); virtual;
    destructor Destroy; override;
    procedure EraseSection(Section: String); virtual;
    function  ReadBool(Section, Key: String; Default: Boolean): Boolean; virtual;
    function  ReadInteger(Section, Key: String; Default: LongInt): LongInt; virtual;
    procedure ReadSection(Section: String; Target: TStrings); virtual;
    procedure ReadSections(Target: TStrings); virtual;
    procedure ReadSectionValues(Section: String; Target: TStrings); virtual;
    function  ReadString(Section, Key: String; Default: String): String; virtual;
    procedure RemoveKey(Section, Key: String); virtual;
    procedure WriteBool(Section, Key: String; Default: Boolean); virtual;
    procedure WriteFile; virtual;
    function WriteToString: string; virtual;
    procedure WriteInteger(Section, Key: String; Default: LongInt); virtual;
    procedure WriteString(Section, Key: String; Default: String); virtual;
    {Delay writing the INI file until it is closed or manually written. This
    can increase performance since the INI file is usually written everytime
    it is modified.}
    property Delay: Boolean read FDelay write FDelay;
  end;

implementation

{Create the object.}
constructor THugeINI.Create(FileName: String);
begin
  inherited Create;
  {Create dynamic stuff.}
  FKeys:= TList.Create;
  FSections:= TStringList.Create;
  FValues:= TList.Create;
  {Setup the vars.}
  FDelay:= False;
  FFileName:= FileName;
  ReadFile;
end;

{Destroy the object.}
destructor THugeINI.Destroy;
var
  i: Integer;
begin
  WriteFile;
  {Free dynamic stuff.}
  for i:= 0 to FKeys.Count - 1 do TStringList(FKeys[i]).Free;
  FKeys.Free;
  FSections.Free;
  for i:= 0 to FValues.Count - 1 do TStringList(FValues[i]).Free;
  FValues.Free;
  inherited Destroy;
end;

{Add a key and return its index.}
function THugeINI.AddKey(Section: Integer; Key: String): Integer;
begin
  Result:= KeyIndex(Section, Key);
  if Result = -1 then
    begin
      Result:= Keys[Section].Add(Key);
      Values[Section].Add('');
    end;
end;

{Add a section and return its index.}
function THugeINI.AddSection(Section: String): Integer;
var
  L: TStrings;
begin
  Result:= SectionIndex(Section);
  if Result = -1 then
    begin
      Result:= FSections.Add(Section);
      L:= TStringList.Create;
      FKeys.Add(L);
      L:= TStringList.Create;
      FValues.Add(L);
    end;
end;

{Erase an entire section.}
procedure THugeINI.EraseSection(Section: String);
var
  i: Integer;
begin
  i:= SectionIndex(Section);
  if i <> -1 then
    begin
      Keys[i].Free;
      FKeys.Delete(i);
      Values[i].Free;
      FValues.Delete(i);
      FSections.Delete(i);
    end;
end;

{See if the line contains a section value.}
procedure THugeINI.ExtractKeyValue(Line: String; var Key: String; var Value: String);
var
  i: Integer;
begin
  Key:= '';
  Value:= '';
  i:= Pos('=', Line);
  if i > 0 then
    begin
      Key:= Copy(Line, 1, i - 1);
      Value:= Copy(Line, i + 1, Length(Line));
    end;
end;

{See if the line contains a section.}
function THugeINI.ExtractSection(Line: String): String;
var
  i: Integer;
begin
  Result:= '';
  if Length(Line) = 0 then EXIT;
  if Line[1] = '[' then
    begin
      i:= Pos(']', Line);
      if i > 2 then
        begin
          Delete(Line, 1, 1);
          Result:= Copy(Line, 1, i - 2);
          while (Length(Result) > 0) and (Result[1] = ' ') do Delete(Result, 1, 1);
        end;
    end;
end;

{Read method for some TStringList properties.}
function THugeINI.GetGenericByIndex(Section: Integer; IndexType: Integer): TStringList;
begin
  Result:= NIL;
  case IndexType of
    0: Result:= TStringList(FKeys[Section]);
    1: Result:= TStringList(FValues[Section]);
  end;
end;

{Read method for some TStringList properties.}
function THugeINI.GetGeneric(Section: String; IndexType: Integer): TStringList;
var
  i: Integer;
begin
  i:= SectionIndex(Section);
  Result:= GetGenericByIndex(i, IndexType);
end;

{See if the index can be found.}
function THugeINI.GetLocation(Section, Key: String; var S, K: Integer; Create: Boolean): Boolean;
begin
  Result:= False;
  S:= SectionIndex(Section);
  if (S = -1) and Create then S:= AddSection(Section);
  if S <> -1 then
    begin
      K:= KeyIndex(S, Key);
      if (K = -1) and Create then K:= AddKey(S, Key);
      if K <> -1 then Result:= True;
    end;
end;

{Read method for the Value property.}
function THugeINI.GetValue(Section, Key: Integer): String;
var
  L: TStrings;
begin
  L:= Values[Section];
  Result:= L[Key];
end;

{Read method for the SectionCount property.}
function THugeINI.GetSectionCount: Integer;
begin
  Result:= FSections.Count;
end;

{Get the index of the key.}
function THugeINI.KeyIndex(Section: Integer; Key: String): Integer;
var
  i: Integer;
  L: TStrings;
  S: String;
begin
  Result:= -1;
  L:= Keys[Section];
  S:= UpperCase(Key);
  if L <> NIL then
    for i:= 0 to L.Count - 1 do
      if UpperCase(L[i]) = S then
        begin
          Result:= i;
          BREAK;
        end;
end;

{Read a boolean value.}
function THugeINI.ReadBool(Section, Key: String; Default: Boolean): Boolean;
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then Result:= not(Value[S, K] = '0')
  else Result:= Default;
end;

{Read the INI file.}
procedure THugeINI.ReadFile;
var
  Key: Integer;
  S: String;
  Section: Integer;
  T: TextFile;
  TempKey: String;
  TempSection: String;
  TempValue: String;
begin
  if FileExists(FFileName) then
    begin
      AssignFile(T, FFileName);
      Reset(T);
      Section:= -1;
      while not(EOF(T)) do
        begin
          ReadLn(T, S);
          TempSection:= ExtractSection(S);
          if TempSection <> '' then
            begin
              Section:= AddSection(TempSection);
            end
          else
            begin
              ExtractKeyValue(S, TempKey, TempValue);
              if TempKey <> '' then
                begin
                  Key:= AddKey(Section, TempKey);
                  Value[Section, Key]:= TempValue;
                end;
            end;
        end;
      CloseFile(T);
    end;
end;

{Read a integer value.}
function THugeINI.ReadInteger(Section, Key: String; Default: LongInt): LongInt;
var
  K, S: Integer;
begin
  try
    if GetLocation(Section, Key, S, K, False) then Result:= StrToInt(Value[S, K])
    else Result:= Default;
  except
    on EConvertError do Result:= Default;
  end;
end;

{Read a section.}
procedure THugeINI.ReadSection(Section: String; Target: TStrings);
var
  i: Integer;
begin
  Target.Clear;
  i:= SectionIndex(Section);
  if i <> -1 then Target.Assign(Keys[i]);
end;

{Returns a list of all section names.}
procedure THugeINI.ReadSections(Target: TStrings);
begin
  Target.Clear;
  Target.Assign(FSections);
end;

{Read section values.}
procedure THugeINI.ReadSectionValues(Section: String; Target: TStrings);
var
  i, j: Integer;
  K, V: TStrings;
begin
  Target.Clear;
  i:= SectionIndex(Section);
  if i <> -1 then
    begin
      K:= Keys[i];
      V:= Values[i];
      for j:= 0 to K.Count - 1 do Target.Add(K[j] + '=' + V[j]);
    end;
end;

{Read a string value.}
function THugeINI.ReadString(Section, Key: String; Default: String): String;
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then Result:= Value[S, K]
  else Result:= Default;
end;

{This will remove a key.}
procedure THugeINI.RemoveKey(Section, Key: String);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then
    begin
      Keys[S].Delete(K);
      Values[S].Delete(K);
      if not(FDelay) then WriteFile;
    end;
end;

{Get the index of the section.}
function THugeINI.SectionIndex(Section: String): Integer;
var
  i: Integer;
  S: String;
begin
  Result:= -1;
  S:= UpperCase(Section);
  for i:= 0 to SectionCount - 1 do
    if UpperCase(FSections[i]) = S then
      begin
        Result:= i;
        BREAK;
      end;
end;

{Write method for the Value property.}
procedure THugeINI.SetValue(Section, Key: Integer; Value: String);
var
  L: TStrings;
begin
  L:= Values[Section];
  L[Key]:= Value;
end;

{Write a boolean value.}
procedure THugeINI.WriteBool(Section, Key: String; Default: Boolean);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      if Default then Value[S, K]:= '1'
      else Value[S, K]:= '0';
      if not(FDelay) then WriteFile;
    end;
end;

{Write the INI file.}
procedure THugeINI.WriteFile;
var
  i, j: Integer;
  K   : TStrings;
  T   : TextFile;
  V   : TStrings;
begin
  if FFileName = '' then Exit;
  AssignFile(T, FFileName);
  Rewrite(T);
  for i:= 0 to SectionCount - 1 do
    begin
      WriteLn(T, '[' + FSections[i] + ']');
      K:= Keys[i];
      V:= Values[i];
      for j:= 0 to K.Count - 1 do  WriteLn(T, K[j] + '=' + V[j]);
      WriteLn(T);
    end;
  CloseFile(T);
end;

function THugeINI.WriteToString: string;
var
  i, j: Integer;
  K   : TStrings;
  V   : TStrings;
begin
  Result := '';
  for i:= 0 to SectionCount - 1 do
    begin
      Result:= Result + '[' + FSections[i] + ']' + #13#10;
      K:= Keys[i];
      V:= Values[i];
      for j:= 0 to K.Count - 1 do Result := Result + K[j] + '=' + V[j] + #13#10;
      Result:= Result + #13#10;
    end;
end;

{Write an integer into the INI file.}
procedure THugeINI.WriteInteger(Section, Key: String; Default: LongInt);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      Value[S, K]:= IntToStr(Default);
      if not(FDelay) then WriteFile;
    end;
end;

{Write a string into the INI file.}
procedure THugeINI.WriteString(Section, Key: String; Default: String);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      Value[S, K]:= Default;
      if not(FDelay) then WriteFile;
    end;
end;

end.
