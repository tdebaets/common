@echo off

rem **************************************************************************
rem *
rem * Copyright 2016-2023 Tim De Baets
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
rem * Global compile script
rem *
rem **************************************************************************

setlocal

rem Retrieve user-specific settings from file
call Scripts\getuserprefs.bat
if errorlevel 1 goto failed

:delphi

if "%DELPHIROOT%"=="" goto libcommon

cd Delphi
if errorlevel 1 goto failed

call compile.bat %*
if errorlevel 1 goto failed

cd ..

:libcommon

if "%MSBUILD_BIN_PATH%"=="" goto next

cd C\libcommon\VS2019
if errorlevel 1 goto failed

call compile.bat %*
if errorlevel 1 goto failed

cd ..\..\..

:next

goto exit

:failed
cd ..
:failed2
exit /b 1

:exit
