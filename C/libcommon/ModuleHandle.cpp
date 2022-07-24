/****************************************************************************
 *
 * Copyright 2022 Tim De Baets
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

#include "ModuleHandle.h"
#include <sstream>

CModuleHandleError::CModuleHandleError(LPCSTR lpModuleName, DWORD dwErrorCode)
:   m_dwErrorCode(dwErrorCode)
{
    ostringstream oss;
    oss << "Failed to load module " << lpModuleName << ": " << dwErrorCode;
    m_message = string(oss.str());
}

const char *CModuleHandleError::what() const
{
    return m_message.c_str();
}

CModuleHandle::CModuleHandle(LPCSTR lpModuleName)
:   m_hMod(NULL)
{
    if (!GetModuleHandleExA(0, lpModuleName, &m_hMod))
    {
        throw CModuleHandleError(lpModuleName, GetLastError());
    }
}

CModuleHandle::~CModuleHandle()
{
    if (m_hMod)
    {
        FreeLibrary(m_hMod);
        m_hMod = NULL;
    }
}

HMODULE CModuleHandle::get_hMod()
{
    return m_hMod;
}