@echo off

rem **************************************************************************
rem *
rem * Copyright 2016 Tim De Baets
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
rem * Script to auto-update the common submodule before pushing
rem *
rem **************************************************************************

rem NOTE: any other script in common that calls this script needs to take
rem precautions to prevent itself from being updated while it's being executed.

setlocal enabledelayedexpansion

rem Strange effects can occur if this script is updated while it is being
rem executed. Therefore, we first create a temporary copy of ourselves and
rem then transfer execution to that copy.
set BATCHNAME=%~nx0
set BATCHSUFFIX=%BATCHNAME:~-8%
set BATCHTMPNAME=
if not "%BATCHSUFFIX%"==".tmp.bat" (
    set BATCHTMPNAME=%~dpn0.tmp.bat
    call %~dp0\mycopy.bat "%~f0" "!BATCHTMPNAME%!"
    if errorlevel 1 goto failed
    rem Transfer execution to temporary copy
    !BATCHTMPNAME!
    if errorlevel 1 goto failed
)

rem Bail out if the common submodule isn't present
if not exist common goto exit

rem Check if the common submodule isn't empty
for /F %%i in ('dir /b /a "common\*" 2^>NUL') do (
    rem common submodule folder not empty, ok
    goto common_ok
)

echo The common subdirectory is still empty; did you run postclone.bat yet?
goto failed

:common_ok

rem Check if auto-update is enabled in user prefs
call .\userprefs.bat
if not "%AUTO_UPD_COMMON%"=="1" goto exit

rem Check if on master branch
for /F %%i in ('git rev-parse --abbrev-ref HEAD 2^>NUL') do (
    if "%%i"=="master" (
        goto branch_ok
    ) else (
        goto exit
    )
)

echo Failed to get current branch name
goto failed

:branch_ok

for /F %%i in ('git status --porcelain') do (
    echo Uncommitted local changes found; cannot continue
    goto failed
)

echo Checking if common submodule it still up-to-date...

git submodule update --remote common
if errorlevel 1 goto failed

for /F %%i in ('git diff common 2^>NUL') do (
    echo common was updated, committing...
    goto common_updated
)

echo common is still up-to-date^^!
goto exit

:common_updated

cd common
if errorlevel 1 goto failed

set LATEST_REV_SHA=
for /F %%i in ('git rev-parse HEAD 2^>NUL') do (
    set LATEST_REV_SHA=%%i
)

cd ..
if errorlevel 1 goto failed

if "%LATEST_REV_SHA%"=="" (
    echo Failed to get latest commit in common
    goto failed
)

set MESSAGE="Update of submodule 'common' by %~nx0 to %LATEST_REV_SHA%"
git commit -m %MESSAGE% common
if errorlevel 1 goto failed

echo common was updated and a new commit has been created.
echo You should at least recompile and possibly also retest the changes.
echo Then try pushing again.
echo *** push ABORTED ***

rem Fall through to return exit code and abort the git push

:failed
set ERRCODE=1
if "%BATCHSUFFIX%"==".tmp.bat" %~dp0\deleteselfandexit.bat "%~f0" %ERRCODE%
exit /b %ERRCODE%

:exit
if "%BATCHSUFFIX%"==".tmp.bat" %~dp0\deleteselfandexit.bat "%~f0"
