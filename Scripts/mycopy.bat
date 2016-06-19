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
rem * Silent wrapper script around 'copy', but still showing any errors
rem *
rem **************************************************************************

setlocal

rem Arguments are assumed to be already quoted
copy /y %1 %2 >NUL
if errorlevel 1 (
    rem 'copy' sends all errors to stdout so echo something ourselves
    echo Failed to copy %1 to %2
    goto failed
)

goto exit

:failed
exit /b 1

:exit
