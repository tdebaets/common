(****************************************************************************
 *
 * Originally based on AgOpenDialog.pas by Deepak Shenoy
 * Copyright (C) 2000 Agni Software Pvt. Ltd.
 * Modifications are Copyright 2018 Tim De Baets
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
 * Unicode version of TAgOpenDialog and TAgSaveDialog VCL components
 *
 ****************************************************************************)

unit AgOpenDialogW;

interface

uses Windows, Classes, CommDlg, TntDialogs, AgOpenDialog, Common2;

type
  TAgOpenDialogW = class(TTntOpenDialog)
  protected
    FShowPlacesBar: Boolean;      // shows the left bar
    FInterceptor: Pointer;        // pointer to intercepting function
    FUseInterceptor: Boolean;
    FOptionsEx: TOpenOptionsEx;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property ShowPlacesBar: Boolean read FShowPlacesBar write FShowPlacesBar;
    property OptionsEx: TOpenOptionsEx read FOptionsEx write FOptionsEx
        default [ofxEnableSizing];
  end;

  TAgSaveDialogW = class(TAgOpenDialogW)
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  end;

type
  TOpenFileNameExW = record
    lStructSize: DWORD; // Size of the structure in bytes.
    hWndOwner: HWND; // Handle that is the parent of the dialog.
    hInstance: HINST; // Application instance handle.
    lpstrFilter: PWideChar; // String containing filter information.
    lpstrCustomFilter: PWideChar; // Will hold the filter chosen by the user.
    nMaxCustFilter: DWORD; // Size of lpstrCustomFilter, in bytes.
    nFilterIndex: DWORD; // Index of the filter to be shown.
    lpstrFile: PWideChar; // File name to start with (and retrieve).
    nMaxFile: DWORD; // Size of lpstrFile, in bytes.
    lpstrFileTitle: PWideChar; // File name without path will be returned.
    nMaxFileTitle: DWORD; // Size of lpstrFileTitle, in bytes.
    lpstrInitialDir: PWideChar; // Starting directory.
    lpstrTitle: PWideChar; // Title of the open dialog.
    Flags: DWORD; // Controls user selection Options.
    nFileOffset: Word; // Offset of file name in filepath=lpstrFile.
    nFileExtension: Word; // Offset of extension in filepath=lpstrFile.
    lpstrDefExt: PWideChar; // Default extension if no extension typed.
    lCustData: LPARAM; // Custom data to be passed to hook.
    lpfnHook: function(Wnd: THandle; Msg: UINT; wParam: WPARAM;
      lParam: LPARAM): UINT stdcall; // Hook.
    lpTemplateName: PWideChar; // Template dialog, if applicable.
    // Extended structure starts here.
    pvReserved: Pointer; // Reserved, use nil.
    dwReserved: DWORD; // Reserved, use 0.
    FlagsEx: DWORD; // Extended Flags.
  end;

procedure Register;

function GetOpenFileNameExW(var OpenFile: TOpenFilenameExW): BOOL; stdcall;
function GetSaveFileNameExW(var OpenFile: TOpenFilenameExW): BOOL; stdcall;

implementation

var
  CurInstanceShowPlacesBar: Boolean;
  CurInstanceOptionsEx: TOpenOptionsEx;

function GetOpenFileNameExW; external 'comdlg32.dll' name 'GetOpenFileNameW';
function GetSaveFileNameExW; external 'comdlg32.dll' name 'GetSaveFileNameW';

procedure Register;
begin
  RegisterComponents('Samples', [TAgOpenDialogW, TAgSaveDialogW]);
end;

procedure AddOFNFlags(var Flags: Cardinal);
const
  OpenOptionsEx: array[TOpenOptionEx] of Cardinal = (
      OFN_ENABLESIZING, OFN_DONTADDTORECENT);
var
  Option: TOpenOptionEx;
begin
  for Option := Low(Option) to High(Option) do
    if Option in CurInstanceOptionsEx then
      Flags := Flags or OpenOptionsEx[Option];
end;

function NormalOpenInterceptor(var DialogData: TOpenFileNameW): BOOL; stdcall;
begin
  AddOFNFlags(DialogData.Flags);
  Result := GetOpenFileNameW(DialogData);
end;

function OpenInterceptor(var DialogData: TOpenFileNameW): BOOL; stdcall;
var
  DialogDataEx: TOpenFileNameExW;
begin
  FillChar(DialogDataEx, SizeOf(DialogDataEx), 0);
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  AddOFNFlags(DialogDataEx.Flags);
  if CurInstanceShowPlacesBar then
    DialogDataEx.FlagsEx := 0
  else
    DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
  DialogDataEx.lStructSize := SizeOf(DialogDataEx); // size of new structure
  Result := GetOpenFileNameExW(DialogDataEx);
  Move(DialogDataEx, DialogData, SizeOf(DialogData)); // 02.01.2001 returned structure needs to be filled in
end;

function NormalSaveInterceptor(var DialogData: TOpenFileNameW): BOOL; stdcall;
begin
  AddOFNFlags(DialogData.Flags);
  Result := GetSaveFileNameW(DialogData);
end;

function SaveInterceptor(var DialogData: TOpenFileNameW): BOOL; stdcall;
var
  DialogDataEx: TOpenFileNameExW;
begin
  FillChar(DialogDataEx, SizeOf(DialogDataEx), 0);
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  AddOFNFlags(DialogDataEx.Flags);
  if CurInstanceShowPlacesBar then
    DialogDataEx.FlagsEx := 0
  else
    DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
  DialogDataEx.lStructSize := SizeOf(DialogDataEx); // size of new structure
  Result := GetSaveFileNameExW(DialogDataEx);
  Move(DialogDataEx, DialogData, SizeOf(DialogData));// 02.01.2001 returned structure needs to be filled in
end;

{ TAgOpenDialogW }

constructor TAgOpenDialogW.Create(AOwner: TComponent);
begin
  inherited;
  FShowPlacesBar := True;
  FInterceptor := @OpenInterceptor;
  FUseInterceptor := IsWindows2kOrHigher or IsWindowsMeOrHigher;
  FOptionsEx := [ofxEnableSizing];
end;

function TAgOpenDialogW.Execute: Boolean;
begin
  CurInstanceOptionsEx := FOptionsEx;
  if FUseInterceptor then begin
     CurInstanceShowPlacesBar := FShowPlacesBar;
     Result := DoExecuteW(FInterceptor);
  end
  else
    Result := DoExecuteW(@NormalOpenInterceptor);
end;

{ TAgSaveDialogW }

constructor TAgSaveDialogW.Create(AOwner: TComponent);
begin
  inherited;
  FInterceptor := @SaveInterceptor;
end;

function TAgSaveDialogW.Execute: Boolean;
begin
  CurInstanceOptionsEx := FOptionsEx;
  if FUseInterceptor then
    Result := inherited Execute // TAgOpenDialogW has the functionality
  else
    Result := DoExecuteW(@NormalSaveInterceptor);
    //Result := DoExecuteW(@GetSaveFileNameW); // can't call inherited because
                                           // it will call opendialogs DoExecute 
end;

end.
