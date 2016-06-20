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
rem * Script to update the repository to the latest changes
rem *
rem **************************************************************************

setlocal

rem Intentionally not using 'call' here because this script should stop
rem executing. Otherwise, strange effects can occur if this script gets updated
rem while it is being executed!
.\Scripts\updaterepo.bat %*

rem If any processing is needed after the update, it should be added to the
rem Scripts\postupdate.bat file

echo ERROR: We should never get here!
goto failed

:failed
exit /b 1

:exit
