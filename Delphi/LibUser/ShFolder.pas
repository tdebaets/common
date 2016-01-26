{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Shell Folder interface unit                                }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: ShFolder.h released 27 May 1999.           }
{ The original Pascal code is: ShFolder.pas, released 26 Feb 2000. }
{                                                                  }
{ The initial developer of the Pascal code is Rudy Velthuis        }
{ (rvelthuis@gmx.de).                                              }
{                                                                  }
{ Portions created by Rudy Velthuis are                            }
{ Copyright (C) 1999,2000 Rudy Velthuis.                           }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

// functions to get shell special folders/
// shfolder.dll supports these on all platforms including Win95, Win98,
// NT4 and IE4 shell

// all CSIDL values referred to here are supported natively by shfolder.dll,
// that is they will work on all platforms.

unit ShFolder;

interface

uses Windows;

type
  {EXTERNALSYM SHGFP_TYPE}
  SHGFP_TYPE = Integer;

const
  {EXTERNALSYM SHGFP_TYPE_CURRENT}
  SHGFP_TYPE_CURRENT  = 0;   // current value for user, verify it exists
  {EXTERNALSYM SHGFP_TYPE_DEFAULT}
  SHGFP_TYPE_DEFAULT  = 1;   // default value, may not exist

  {EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL             = $0005;  // My Documents
  {EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA              = $001A;  // Application Data, new for NT4

  {EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA        = $001C;  // non roaming,
                                       // user\Local Settings\Application Data
  {EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE       = $0020;
  {EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES              = $0021;
  {EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY              = $0022;
  {EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA       = $0023;  // All Users\Application Data
  {EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_WINDOWS              = $0024;  // GetWindowsDirectory()
  {EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_SYSTEM               = $0025;  // GetSystemDirectory()
  {EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_PROGRAM_FILES        = $0026;  // C:\Program Files
  {EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_MYPICTURES           = $0027;  // My Pictures, new for Win2K
  {EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_PROGRAM_FILES_COMMON = $002B;  // C:\Program Files\Common
  {EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_DOCUMENTS     = $002E;  // All Users\Documents


  {EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE          = $8000;  // new for Win2K, or this in to force
                                       // creation of folder

  // All Users\Start Menu\Programs\Administrative Tools
  {EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_COMMON_ADMINTOOLS    = $002F;
  // <user name>\Start Menu\Programs\Administrative Tools
  {EXTERNALSYM CSIDL_ADMINTOOLS}
  CSIDL_ADMINTOOLS           = $0030;


{EXTERNALSYM SHGetFolderPathA}
function SHGetFolderPathA(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
{EXTERNALSYM SHGetFolderPathW}
function SHGetFolderPathW(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HResult; stdcall;
{EXTERNALSYM SHGetFolderPath}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall;

// protos so callers can GetProcAddress() from shfolder.dll

type
  {EXTERNALSYM PFNSHGetFolderPathA}
  PFNSHGetFolderPathA = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HResult stdcall;
  {EXTERNALSYM PFNSHGetFolderPathW}
  PFNSHGetFolderPathW = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HResult stdcall;
  {EXTERNALSYM PFNSHGetFolderPath}
  PFNSHGetFolderPath = PFNSHGetFolderPathA;


implementation

function SHGetFolderPathA; external 'shfolder.dll' name 'SHGetFolderPathA';
function SHGetFolderPathW; external 'shfolder.dll' name 'SHGetFolderPathW';
function SHGetFolderPath; external 'shfolder.dll' name 'SHGetFolderPathA';

end.
