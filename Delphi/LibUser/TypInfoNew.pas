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
 * New declarations that aren't present yet in Delphi 4's TypInfo.pas
 *
 ****************************************************************************)

unit TypInfoNew;

interface

uses TypInfo;

function GetPropInfo(Instance: TObject; const PropName: string;
    AKinds: TTypeKinds = []): PPropInfo; overload;
function GetPropInfo(AClass: TClass; const PropName: string;
    AKinds: TTypeKinds = []): PPropInfo; overload;
function GetPropInfo(TypeInfo: PTypeInfo;
    const PropName: string): PPropInfo; overload
function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string;
    AKinds: TTypeKinds): PPropInfo; overload;

implementation

function GetPropInfo(Instance: TObject; const PropName: string;
    AKinds: TTypeKinds): PPropInfo;
begin
  Result := GetPropInfo(Instance.ClassType, PropName, AKinds);
end;

function GetPropInfo(AClass: TClass; const PropName: string;
    AKinds: TTypeKinds): PPropInfo;
begin
  Result := GetPropInfo(PTypeInfo(AClass.ClassInfo), PropName, AKinds);
end;

function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string): PPropInfo;
begin
  Result := TypInfo.GetPropInfo(TypeInfo, PropName);
end;

function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string;
    AKinds: TTypeKinds): PPropInfo;
begin
 Result := GetPropInfo(TypeInfo, PropName);
 if (Result <> nil) and
     (AKinds <> []) and
     not (Result^.PropType^^.Kind in AKinds) then
    Result := nil;
end;

end.
 