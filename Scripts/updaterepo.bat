@echo off

rem **************************************************************************
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
rem * Script to update a repository to its latest changes
rem *
rem **************************************************************************

rem NOTE: any other script in the repository that calls this script needs to
rem take precautions to prevent itself from being updated while it's being
rem executed.

setlocal enabledelayedexpansion

rem Strange effects can occur if this script is updated while it is being
rem executed. Therefore, we first create a temporary copy of ourselves and
rem then transfer execution to that copy.
set BATCHNAME=%~nx0
set BATCHSUFFIX=%BATCHNAME:~-8%
set BATCHTMPNAME=
if not "%BATCHSUFFIX%"==".tmp.bat" (
    set BATCHTMPNAME=%~dpn0.tmp.bat
    call "%~dp0\mycopy.bat" "%~f0" "!BATCHTMPNAME%!"
    if errorlevel 1 goto failed
    rem Transfer execution to temporary copy
    "!BATCHTMPNAME!"
    if errorlevel 1 goto failed
)

echo Checking repository...

git status --porcelain >NUL
if errorlevel 1 (
    echo git status FAILED
    goto failed
)

for /f %%i in ('git status --porcelain') do (
    echo Uncommitted local changes found; cannot continue
    goto failed2
)

echo Checking submodules...

call "%~dp0\checksubmodchanges.bat"
if errorlevel 1 goto failed2

echo Pulling...

git pull %*
if errorlevel 1 (
    echo git pull failed; checking for conflicts
    call "%~dp0\checkconflicts.bat"
    if errorlevel 1 (
        echo Conflicts found
        echo Resolve the conflicts, then run these commands to complete the update:
        echo - git add ^<file^>...
        echo - git rebase --continue
        echo - git submodule update
        goto failed2
    ) else (
        echo git pull FAILED for unknown reason!
        goto failed
    )
)

git submodule update
if errorlevel 1 goto failed

if exist Scripts\postupdate.bat (
    call .\Scripts\postupdate.bat
    if errorlevel 1 goto failed
)

echo Success^^!
goto exit

:failed
echo *** FAILED ***
:failed2
set ERRCODE=1
if "%BATCHSUFFIX%"==".tmp.bat" "%~dp0\deleteselfandexit.bat" "%~f0" %ERRCODE%
exit /b %ERRCODE%

:exit
if "%BATCHSUFFIX%"==".tmp.bat" "%~dp0\deleteselfandexit.bat" "%~f0"
