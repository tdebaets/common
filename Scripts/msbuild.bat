@echo off

rem **************************************************************************
rem *
rem * Copyright 2023 Tim De Baets
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
rem * Common MSBuild compile script
rem *
rem **************************************************************************

setlocal

set SCRIPTPATH=%~dp0
set PROJECTFILE=%1
set CONFIGURATION=%2
set PLATFORM=%3

rem Retrieve user-specific settings from file
call "%SCRIPTPATH%\getuserprefs.bat"
if errorlevel 1 goto failed

echo:
echo - %PROJECTFILE% - %CONFIGURATION% - %PLATFORM%
echo:

"%MSBUILD_BIN_PATH%\MSBuild.exe" %PROJECTFILE% ^
    -target:Rebuild ^
    -property:Configuration=%CONFIGURATION%;Platform=%PLATFORM%
if errorlevel 1 goto failed

goto exit

:failed
exit /b 1

:exit
