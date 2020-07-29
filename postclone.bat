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
rem * One-time script to run after cloning the repository
rem *
rem **************************************************************************

setlocal

call .\Scripts\setuprepo.bat %*
if errorlevel 1 goto failed

rem NOTE: any additional steps that are added here won't be executed when common
rem is included in another repository as a submodule. Therefore, it is
rem recommended to add these steps in Scripts\setuprepo.bat instead (see for
rem example the creation of the output directories for 'common').

echo Success!
goto exit

:failed
echo *** FAILED ***
:failed2
exit /b 1

:exit
