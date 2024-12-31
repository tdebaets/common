/****************************************************************************
 *
 * Copyright 2022-2024 Tim De Baets
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ****************************************************************************
 *
 * Wrapper class for a Win32 module (DLL) handle
 *
 ****************************************************************************/

#pragma once

#include <exception>
#include <string>
#include <Windows.h>

using namespace std;

class CModuleHandleError : public exception
{
private:

    DWORD   m_dwErrorCode;
    string  m_message;

public:

    CModuleHandleError(LPCWSTR lpModuleName, DWORD dwErrorCode);
    virtual const char *what() const;
};

class CModuleHandle
{
protected:

    CModuleHandle();

    HMODULE m_hMod;

public:

    CModuleHandle(LPCWSTR lpModuleName);
    ~CModuleHandle();

    HMODULE get_hMod();
};

class CModuleHandleLoad : public CModuleHandle
{
public:

    CModuleHandleLoad(LPCWSTR lpModuleName);
};
