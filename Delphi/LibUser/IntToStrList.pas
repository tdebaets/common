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
 * TIntToStrList class for mapping integers to strings
 *
 ****************************************************************************)

unit IntToStrList;

interface

uses Classes, UIntList, Common2;

type
  TIntToStrList = class(TIntList)
  protected
    procedure InvalidMethodError(const MethodName: String);
    procedure ClearStrings;
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
    function GetString(Index: Integer): String;
    procedure PutString(Index: Integer; const Str: String);
  public
    destructor Destroy; override;
    function AddObject(const S: int64; AObject: TObject): Integer; override;
    function AddString(const S: int64; const Str: String): Integer; virtual;
    procedure Clear;
    procedure Delete(Index: Integer);
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: String read GetString write PutString;
  end;

implementation

destructor TIntToStrList.Destroy;
begin
  ClearStrings;
  inherited;
end;

procedure TIntToStrList.InvalidMethodError(const MethodName: String);
  function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;
begin
  raise EStringListError.CreateFmt('Not allowed to use method %s on class %s',
      [MethodName, ClassName]) at ReturnAddr;
end;

procedure TIntToStrList.ClearStrings;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    PutString(i, '');
end;

function TIntToStrList.GetObject(Index: Integer): TObject;
begin
  InvalidMethodError('GetObject');
  Result := nil;
end;

procedure TIntToStrList.PutObject(Index: Integer; AObject: TObject);
begin
  InvalidMethodError('PutObject');
end;

function TIntToStrList.AddObject(const S: int64; AObject: TObject): Integer;
begin
  InvalidMethodError('AddObject');
  Result := -1;
end;

function TIntToStrList.GetString(Index: Integer): String;
var
  pStrRec: Pointer;
begin
  pStrRec := inherited Objects[Index];
  if Assigned(pStrRec) then
    Result := Common2.GetString(pStrRec)
  else
    Result := '';
end;

procedure TIntToStrList.PutString(Index: Integer; const Str: String);
var
  pStrRec: Pointer;
begin
  pStrRec := inherited Objects[Index];
  if Assigned(pStrRec) then
    FreeStringRec(pStrRec);
  if Str <> '' then
    inherited Objects[Index] := AllocStringRec(Str)
  else
    inherited Objects[Index] := nil;
end;

function TIntToStrList.AddString(const S: int64; const Str: String): Integer;
begin
  Result := Add(S);
  PutString(Result, Str);
end;

procedure TIntToStrList.Clear;
begin
  ClearStrings;
end;

procedure TIntToStrList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < Count) then
    PutString(Index, '');
  inherited;
end;

end.
