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
rem * Script to initially set up a repository
rem *
rem **************************************************************************

setlocal enabledelayedexpansion

set SCRIPTPATH=%~dp0
set REPONAME=
set LF=^


rem Two empty lines above are required

rem TODO remove?
if exist common (
    set COMMONPATH=common\
) else (
    set COMMONPATH=
)

call "%SCRIPTPATH%\checkdir.bat" ".git"
if errorlevel 1 (
    rem A subdirectory called '.git' doesn't exist - assume that we're being
    rem called by a parent repository that includes common as a submodule to
    rem set up the 'common' repository (see below)
    
    if exist common (
        echo ERROR: not expecting a 'common' subdirectory to exist
        goto failed
    )
    
    call "%SCRIPTPATH%\checkdir.bat" "..\.git\modules\common\hooks"
    if errorlevel 1 (
        echo ERROR: failed to determine path to git hooks
        goto failed
    )
    
    set GITHOOKPATH=..\.git\modules\common\hooks
) else (
    set GITHOOKPATH=.git\hooks
)

for /f %%i in ('%COMMONPATH%Scripts\getreponame.bat') do set REPONAME=%%i

echo Setting up repository '%REPONAME%'...

git config pull.rebase preserve
if errorlevel 1 goto failed

rem Check that all submodule commits to push are available on a remote
git config push.recurseSubmodules check
if errorlevel 1 goto failed

echo %REPONAME%: installing hooks...

for /f %%i in ("post-checkout!LF!pre-push!LF!post-rewrite!LF!post-merge") do (
    rem Create backup of possible existing hook
    if exist %GITHOOKPATH%\%%i (
        call "%SCRIPTPATH%\mymove.bat" ^
            "%GITHOOKPATH%\%%i" "%GITHOOKPATH%\%%i.bak"
        if errorlevel 1 goto failed
    )
    
    rem Generate the new hook file
    rem Double slash is to prevent MSYS from applying automatic Posix path
    rem conversion, otherwise "/c" would turn into "C:\"
    rem NOTE: any changes being made here won't have effect on already checked-
    rem out repositories! Modify the .bat hook scripts in \Scripts instead!
    set HOOKFILE=^
#^^!/bin/sh!LF!!LF!^
# Hook file generated by %COMMONPATH%Scripts\%~nx0!LF!!LF!^
cmd.exe //c "%COMMONPATH%Scripts\%%i.bat $@"

    rem "|| rem" is required to set errorlevel here
    echo !HOOKFILE! > "%GITHOOKPATH%\%%i" || rem do_not_remove
    if errorlevel 1 goto failed
)

rem If we are setting up a repository that includes common as a submodule, also
rem call this script to set up the 'common' repository too.
if exist common (
    cd common
    
    call Scripts\setuprepo.bat %*
    if errorlevel 1 goto failed_common
    
    cd ..
) else (
    echo %REPONAME%: creating directories...

    call "%SCRIPTPATH%\createdir.bat" "Output"
    if errorlevel 1 goto failed

    call "%SCRIPTPATH%\createdir.bat" "Delphi\DCU"
    if errorlevel 1 goto failed
)

goto exit

:failed_common
cd ..

:failed
exit /b 1

:exit
