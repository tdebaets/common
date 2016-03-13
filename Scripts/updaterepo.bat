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

setlocal

if exist common (
    set SCRIPTPATH=.\common\Scripts
) else (
    set SCRIPTPATH=.\Scripts
)

echo Checking repository...

git status --porcelain > NUL
if errorlevel 1 (
    echo git status FAILED
    goto failed
)

for /F %%i in ('git status --porcelain') do (
    echo Uncommitted local changes found; cannot continue
    goto failed2
)

echo Checking submodules...

call %SCRIPTPATH%\checksubmodchanges.bat
if errorlevel 1 goto failed2

git pull %*
if errorlevel 1 (
    echo git pull failed; checking for conflicts
    call %SCRIPTPATH%\checkconflicts.bat
    if errorlevel 1 (
        echo Conflicts found
        echo Resolve the conflicts, then run these commands to complete the update:
        echo - git add <file>...
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

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
