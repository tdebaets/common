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

setlocal

if exist common (
    rem We're in another repository including common as a submodule
    set COMMONDIR=common
) else (
    rem We're in the common repository itself
    set COMMONDIR=.
)

echo Setting up repository...

git config pull.rebase preserve
if errorlevel 1 goto failed

echo Installing hooks...

rem No permission for regular users in Win7 to create symbolic links, so create
rem hardlinks instead

if not exist .git\hooks\post-checkout (
    mklink /H .git\hooks\post-checkout %COMMONDIR%\Scripts\post-checkout
    if errorlevel 1 goto failed
)

if not exist .git\hooks\pre-push (
    mklink /H .git\hooks\pre-push %COMMONDIR%\Scripts\pre-push
    if errorlevel 1 goto failed
)

goto exit

:failed
exit /b 1

:exit
