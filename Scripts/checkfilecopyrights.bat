@echo off

rem **************************************************************************
rem *
rem * Copyright 2018 Tim De Baets
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
rem * Script that retrieves all files that were modified in a given year, and
rem * checks if their copyright is already up-to-date
rem *
rem **************************************************************************

setlocal enabledelayedexpansion

set SCRIPTPATH=%~dp0
set YEAR=%1

if [%YEAR%]==[] (
    echo Usage: %~nx0 ^<year^>
    exit /b 1
)

set /a YEARPLUS1=%YEAR%+1

for /f tokens^=*^ delims^=^ eol^= %%g in ('git log --pretty^=format: --name-only --since^="1/1/%YEAR%" --until^="1/1/%YEARPLUS1%" ^| sort -u') do (
    call %SCRIPTPATH%\checkdir.bat "%%g"
    if errorlevel 2 (
        echo %%g: no such file; skipping
    ) else if errorlevel 1 (
        set COPYRIGHT_FOUND=0
        rem Only search the first 7 lines of the file for the year (should be
        rem enough to get the copyright)
        rem Using sed seems to be much faster than 'head -7 | grep'
        for /f %%h in ('sed -n "/%YEAR%/p;7q" "%%g"') do (
            set COPYRIGHT_FOUND=1
        )
        if !COPYRIGHT_FOUND! equ 0 (
            echo %%g
        )
    ) else (
        rem %%g points to a directory (probably a submodule): skip it
    )
)
