@echo off

rem **************************************************************************
rem *
rem * Copyright 2017 Tim De Baets
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
rem * Script to get user-specific compiler settings
rem *
rem **************************************************************************

rem Intentionally commented out!
rem setlocal

set DELPHIROOT=
set MSBUILD_BIN_PATH=

set PREFS_PATH=%~dp0\..\userprefs.bat
if exist %PREFS_PATH% goto userprefsfound

echo userprefs.bat not found in repository root - looking in parent folder

set PREFS_PATH=%~dp0\..\..\userprefs.bat
if exist %PREFS_PATH% (
  echo userprefs.bat found in parent folder
  goto userprefsfound
)

:userprefserror
echo userprefs.bat (in the root of the repository or its parent) is missing or
echo incomplete.
echo It needs to be created with at least one of the following lines, adjusted
echo for your system:
echo:
echo   set DELPHIROOT=c:\delphi4                [Path to Delphi 4 (or later)]
echo   set MSBUILD_BIN_PATH=C:\...\MSBuild\Current\Bin  [Path to MSBuild.exe]
goto failed

:userprefsfound
call %PREFS_PATH%

if not "%DELPHIROOT%"=="" goto exit
if not "%MSBUILD_BIN_PATH%"=="" goto exit

goto userprefserror

goto exit

:failed

exit /b 1

:exit
