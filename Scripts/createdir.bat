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
rem * Script to create a directory if it doesn't exist
rem * Based on code by dbenham at
rem * http://stackoverflow.com/questions/8666225/how-to-test-if-a-path-is-a-file-or-directory-in-windows-batch-file/8669636#8669636
rem *
rem **************************************************************************

setlocal

for /f "tokens=1,2 delims=d" %%A in ("-%~a1") do if "%%B" neq "" (
  rem %1 is a folder
  goto exit
) else if "%%A" neq "-" (
  echo %1 is a file; cannot create directory with same name
  goto failed
) else (
  rem %1 does not exist
)

mkdir %1
if errorlevel 1 goto failed

goto exit

:failed
exit /b 1

:exit
