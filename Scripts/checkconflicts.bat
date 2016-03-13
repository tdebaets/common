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
rem * Script to check for conflicts in a Git repository
rem *
rem **************************************************************************

setlocal enabledelayedexpansion

for /f %%i in ('git status --porcelain') do (
    set status=%%i
    rem There are conflicts if there are unmerged changes by us (U*), by
    rem them (*U), or by both (UU).
    rem So check the 1st and 2nd character of the status
    if "!status:~0,1!"=="U" goto conflicts
    if "!status:~1,1!"=="U" goto conflicts
)

goto exit

:conflicts

exit /b 1

:exit
