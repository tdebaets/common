(************************************************************
Author: Deepak Shenoy
        shenoy@agnisoft.com
Copyright (C) 2000 Agni Software Pvt. Ltd.
All Rights Reserved.
http://www.agnisoft.com

Change Log:
20.09.2000
  Fix for possible redefinitions of TOpenFileName, in which case the dialog will
  not appear in Windows 9x/NT4.
11.09.2000
  Now works for Windows ME too. Thanks to Petri Mustonen <weilo@hem.passagen.se>
  and Mark Carrington <mark@mutantpenguin.net> for their help and code.
06.03.2000
  Bug fix on suggestion from Jean-Fabien Connault <cycocrew@worldnet.fr> (Thanks)
  Save Dialog on Win9x/NT would show "Open" as the caption if no title was
  assigned. Added DoExecute Procedure.
22.01.2000
  IsWin2000 fix on suggestion from Jean-Fabien Connault <cycocrew@worldnet.fr>
  - Major Version check should be >= 5
02.01.2001
  FilterIndex was not reflecting the index chosen by the user. The problem was that
  the structure returned by GetOpenFileNameEx was not passed back to the VCL - Fixed.
*******************************************************)
unit AgOpenDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CommDlg;

const
  OFN_ENABLESIZING = $00800000;
  OFN_DONTADDTORECENT = $2000000;

type
  TOpenOptionEx = (ofxEnableSizing, ofxDontAddToRecent);
  TOpenOptionsEx = set of TOpenOptionEx;

  TAgOpenDialog = class(TOpenDialog)
  protected
    FShowPlacesBar : boolean; // shows the left bar
    FInterceptor : Pointer;   // pointer to intercepting function
    FOptionsEx: TOpenOptionsEx;
    function IsWin2000 : boolean;  // are we on Windows2000?
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property ShowPlacesBar : boolean read FShowPlacesBar write FShowPlacesBar;
    property OptionsEx: TOpenOptionsEx read FOptionsEx write FOptionsEx default [ofxEnableSizing];
  end;

  TAgSaveDialog = class(TAgOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  end;


procedure Register;

type
  TOpenFileNameEx = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PAnsiChar;
    lpstrCustomFilter: PAnsiChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PAnsiChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PAnsiChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PAnsiChar;
    lpstrTitle: PAnsiChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PAnsiChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PAnsiChar;
    pvReserved : Pointer;
    dwReserved : DWORD;
    FlagsEx : DWORD;
  end;

const OFN_EX_NOPLACESBAR = 1;

function GetOpenFileNameEx(var OpenFile: TOpenFilenameEx): Bool; stdcall;
function GetSaveFileNameEx(var OpenFile: TOpenFilenameEx): Bool; stdcall;

implementation

{$R *.RES}

function GetOpenFileNameEx;      external 'comdlg32.dll'  name 'GetOpenFileNameA';
function GetSaveFileNameEx;      external 'comdlg32.dll'  name 'GetSaveFileNameA';

procedure Register;
begin
  RegisterComponents('Samples', [TAgOpenDialog, TAgSaveDialog]);
end;

var
  CurInstanceShowPlacesBar : boolean;
  CurInstanceOptionsEx: TOpenOptionsEx;

procedure AddOFNFlags(var Flags: Cardinal);
const
  OpenOptionsEx: array [TOpenOptionEx] of Cardinal = (
    OFN_ENABLESIZING, OFN_DONTADDTORECENT);
var
  Option: TOpenOptionEx;
begin
  for Option := Low(Option) to High(Option) do
    if Option in CurInstanceOptionsEx then
      Flags := Flags or OpenOptionsEx[Option];
end;

function NormalOpenInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
begin
  AddOFNFlags(DialogData.Flags);
  Result := GetOpenFileName(DialogData);
end;

function OpenInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
var
  DialogDataEx : TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, sizeof(DialogDataEx)-sizeof(DWORD)*2-sizeof(Pointer));
  AddOFNFlags(DialogDataEx.Flags);
  if CurInstanceShowPlacesBar then
    DialogDataEx.FlagsEx := 0
  else
    DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
  DialogDataEx.lStructSize := sizeof(TOpenFileNameEx); // size of new structure
  Result := GetOpenFileNameEx( DialogDataEx );
  Move(DialogDataEx, DialogData, SizeOf(DialogData)); // 02.01.2001 returned structure needs to be filled in
end;

function NormalSaveInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
begin
  AddOFNFlags(DialogData.Flags);
  Result := GetSaveFileName(DialogData);
end;

function SaveInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
var
  DialogDataEx : TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, sizeof(DialogDataEx)-sizeof(DWORD)*2-sizeof(Pointer));
  AddOFNFlags(DialogDataEx.Flags);
  if CurInstanceShowPlacesBar then
    DialogDataEx.FlagsEx := 0
  else
    DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
  DialogDataEx.lStructSize := sizeof(TOpenFileNameEx); // size of new structure
  Result := GetSaveFileNameEx( DialogDataEx );
  Move(DialogDataEx, DialogData, SizeOf(DialogData));// 02.01.2001 returned structure needs to be filled in
end;

{ TAgOpenDialog }

constructor TAgOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FShowPlacesBar := TRUE;
  FInterceptor := @OpenInterceptor;
  FOptionsEx := [ofxEnableSizing];
end;

function TAgOpenDialog.Execute: Boolean;
begin
  CurInstanceOptionsEx := FOptionsEx;
  if IsWin2000 then
  begin
     CurInstanceShowPlacesBar := FShowPlacesBar;
     Result := DoExecute(FInterceptor);
  end
  else
    Result := DoExecute(@NormalOpenInterceptor);
    //Result := inherited Execute;
end;

// CHANGED - 11 Sep 2000
// Function returns true on Windows ME too. Should change the name.
function TAgOpenDialog.IsWin2000: boolean;
var
  ver : TOSVersionInfo;
begin
  Result := FALSE;
  ver.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if not GetVersionEx(ver ) then
    Exit;

  if ( ver.dwPlatformId=VER_PLATFORM_WIN32_NT) then
  begin // Detect Windows 2000
    if (ver.dwMajorVersion >= 5 ) then
    Result := TRUE;
  end
  else // Detect Windows ME
    if ((ver.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS) and
       (ver.dwMajorVersion >= 4) and
       (ver.dwMinorVersion >= 90)
      ) then
    Result := TRUE;
end;


{ TAgSaveDialog }

constructor TAgSaveDialog.Create(AOwner: TComponent);
begin
  inherited;
  FInterceptor := @SaveInterceptor;
end;

function TAgSaveDialog.Execute: Boolean;
begin
  CurInstanceOptionsEx := FOptionsEx;
  if IsWin2000 then
  begin
     Result := Inherited Execute; // TAgOpenDialog has the functionality
  end
  else
    Result := DoExecute(@NormalSaveInterceptor);
    //Result := DoExecute(@GetSaveFileName); // can't call inherited because
                                           // it will call opendialogs DoExecute 
end;

end.
