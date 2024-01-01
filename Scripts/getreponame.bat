@echo off

rem **************************************************************************
rem *
rem * Copyright 2020-2023 Tim De Baets
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
rem * Script to get the name of a Git repository
rem *
rem **************************************************************************

setlocal enabledelayedexpansion

set ORIGINURL=

for /f %%i in ('git config --get remote.origin.url') do (
    set ORIGINURL=%%i
)

set TMP=%ORIGINURL%
set NUMSEGMENTS=0
set LASTSEGMENT=

:loopprocess
for /f "tokens=1* delims=/\" %%i in ("%TMP%") do (
    set /a NUMSEGMENTS+=1
    set TMP=%%j
    goto loopprocess
)

for /f "tokens=%NUMSEGMENTS% delims=/\" %%i in ("%ORIGINURL%") do (
    set LASTSEGMENT=%%i
)

rem Now we just need to strip the trailing '.git' from the last URL segment
echo %LASTSEGMENT:.git=%
