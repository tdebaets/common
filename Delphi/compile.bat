@echo off

rem **************************************************************************
rem *
rem * Copyright 2016-2017 Tim De Baets
rem *
rem * Licensed under the Apache License, Version 2.0 (the "License");
rem * you may not use this file except in compliance with the License.
rem * You may obtain a copy of the License at
rem *
rem *     http://www.apache.org/licenses/LICENSE-2.0
rem *
rem * Unless required by applicable law or agreed to in writing, software
rem * distributed under the License is distributed on an "AS IS" BASIS,
rem * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
rem * See the License for the specific language governing permissions and
rem * limitations under the License.
rem *
rem **************************************************************************
rem *
rem * Delphi-specific compile script
rem *
rem **************************************************************************

setlocal

rem  Units in LibFixed to compile separately (keep alphabetical please)
set LIBFIXED_UNITS= ^
    Controls.pas ^
    ComCtrls.pas ^
    FormDefs.pas ^
    Forms.pas

rem  Units in Imports to compile separately (keep alphabetical please)
set IMPORTS_UNITS= ^
    Accessibility_TLB.pas ^
    SHDocVw_TLB.pas ^
    WMPLib_TLB.pas

rem Units in LibUser to compile separately (keep alphabetical please)
set LIBUSER_UNITS= ^
    cUnicode.pas ^
    cUnicodeChar.pas ^
    FastMM4\FastMM4.pas ^
    GIFImage.pas ^
    HugeIni.pas ^
    IntToStrList.pas ^
    JclResources.pas ^
    JclWideFormat.pas ^
    MBCSUtil.pas ^
    NewDialogs.pas ^
    PathFuncWide.pas ^
    PEStruct.pas ^
    PJMessageDialog ^
    Scanf.pas ^
    Scanf_c.pas ^
    ShellApi2.pas ^
    ShlObj2.pas ^
    StreamUtil.pas ^
    UIntList.pas ^
    uProcessMemMgr.pas ^
    WMPAttribs.pas ^
    WMPUndocumented.pas ^
    WMPUtil.pas ^
    WMPWSZFormat.pas

set CFGFILE=
set OLDCFGFILE=
set FULLBUILD=0

rem  Quiet compile / Build all / Output warnings
set DCC32OPTS=-Q -W
set CUSTOMARGS=%1

if "%1"=="/incr" (
    echo * Incremental compilation enabled ^(non-default^)
    set CUSTOMARGS=%2
) else (
    rem Build all (default)
    set DCC32OPTS=%DCC32OPTS% -B
    set FULLBUILD=1
)

rem Generate unique number for temporary file renames
set RND=%RANDOM%

rem  Retrieve user-specific settings from file
call ..\Scripts\getuserprefs.bat
if errorlevel 1 goto failed2

set COMMON_LIB_PATH=..\LibFixed;%DELPHIROOT%\lib
set DCU_PATH=DCU

if %FULLBUILD% equ 1 (
    if exist %DCU_PATH%\*.dcu (
        echo Removing previous output DCU files...
        del %DCU_PATH%\*.dcu
        if errorlevel 1 goto failed
    )
)

set DCU_PATH=..\%DCU_PATH%

rem -------------------------------------------------------------------------

rem  Compile each project separately because it seems Delphi carries some
rem  settings (e.g. $APPTYPE) between projects if multiple projects are
rem  specified on the command line.

rem  Always use 'master' .cfg file when compiling from the command line to
rem  prevent user options from hiding compile failures in official builds.
rem  Temporarily rename any user-generated .cfg file during compilation.

cd LibFixed
if errorlevel 1 goto failed

echo - LibFixed

"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"%COMMON_LIB_PATH%" ^
    -R"%DELPHIROOT%\lib" ^
    -N"%DCU_PATH%" ^
    %LIBFIXED_UNITS%
if errorlevel 1 goto failed

cd ..

cd Imports
if errorlevel 1 goto failed

echo - Imports

"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"%COMMON_LIB_PATH%" ^
    -R"%DELPHIROOT%\lib" ^
    -N"%DCU_PATH%" ^
    %IMPORTS_UNITS%
if errorlevel 1 goto failed

cd ..

cd LibUser
if errorlevel 1 goto failed

echo - LibUser

"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"%COMMON_LIB_PATH%;..\Imports;TntUnicodeControls" ^
    -R"%DELPHIROOT%\lib" ^
    -N"%DCU_PATH%" ^
    %LIBUSER_UNITS%
if errorlevel 1 goto failed

echo - tdebaets_comps.dpk

rem  Rename user-generated .cfg file if it exists
if not exist tdebaets_comps.cfg goto tdebaets_comps
ren tdebaets_comps.cfg tdebaets_comps.cfg.%RND%
if errorlevel 1 goto failed
set OLDCFGFILE=tdebaets_comps.cfg

:tdebaets_comps
ren tdebaets_comps.cfg.main tdebaets_comps.cfg
if errorlevel 1 goto failed
set CFGFILE=tdebaets_comps.cfg
"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"%COMMON_LIB_PATH%;..\Imports;Virtual Treeview\Source;Virtual Treeview\Design" ^
    -R"%DELPHIROOT%\lib" ^
    -N"%DCU_PATH%" ^
    tdebaets_comps.dpk
if errorlevel 1 goto failed
ren %CFGFILE% %CFGFILE%.main
set CFGFILE=
if not "%OLDCFGFILE%"=="" ren %OLDCFGFILE%.%RND% %OLDCFGFILE%
set OLDCFGFILE=

cd TntUnicodeControls\Packages
if errorlevel 1 goto failed

echo - TntUnicodeVcl_R40.dpk

:tntunicode
"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"..\..\..\LibFixed;%DELPHIROOT%\lib;..\.." ^
    -R"%DELPHIROOT%\lib" ^
    -N"..\..\%DCU_PATH%" ^
    TntUnicodeVcl_R40.dpk
if errorlevel 1 (
    cd ..\..
    goto failed
)

cd ..\..

echo - tdebaets_comps_unicode.dpk

rem  Rename user-generated .cfg file if it exists
if not exist tdebaets_comps_unicode.cfg goto tdebaets_comps_unicode
ren tdebaets_comps_unicode.cfg tdebaets_comps_unicode.cfg.%RND%
if errorlevel 1 goto failed
set OLDCFGFILE=tdebaets_comps_unicode.cfg

:tdebaets_comps_unicode
ren tdebaets_comps_unicode.cfg.main tdebaets_comps_unicode.cfg
if errorlevel 1 goto failed
set CFGFILE=tdebaets_comps_unicode.cfg
"%DELPHIROOT%\bin\dcc32.exe" %DCC32OPTS% %CUSTOMARGS% ^
    -U"%COMMON_LIB_PATH%;..\..\Output" ^
    -R"%DELPHIROOT%\lib" ^
    -N"%DCU_PATH%" ^
    tdebaets_comps_unicode.dpk
if errorlevel 1 goto failed
ren %CFGFILE% %CFGFILE%.main
set CFGFILE=
if not "%OLDCFGFILE%"=="" ren %OLDCFGFILE%.%RND% %OLDCFGFILE%
set OLDCFGFILE=

echo Success!
cd ..
goto exit

:failed
if not "%CFGFILE%"=="" ren %CFGFILE% %CFGFILE%.main
if not "%OLDCFGFILE%"=="" ren %OLDCFGFILE%.%RND% %OLDCFGFILE%
echo *** FAILED ***
cd ..
:failed2
exit /b 1

:exit
