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
rem * Git pre-push hook
rem *
rem **************************************************************************

setlocal

rem Run project-specific hook if it exists
if exist Hooks\pre-push.bat call .\Hooks\pre-push.bat
if errorlevel 1 goto failed

goto exit

:failed
echo *** pre-push FAILED ***
exit /b 1

:exit
