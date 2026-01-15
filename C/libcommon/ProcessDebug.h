/****************************************************************************
 *
 * Copyright 2025 Tim De Baets
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
 * Base helper class for debugging a process
 *
 ****************************************************************************/

#pragma once

#define WIN32_NO_STATUS
#include <Windows.h>
#undef WIN32_NO_STATUS
#include <ntstatus.h>

using namespace std;

class CProcessDebug
{
public:

    void Run();

protected:

    virtual bool OnProcessCreate(DWORD                      dwProcessID,
                                 DWORD                      dwThreadID,
                                 CREATE_PROCESS_DEBUG_INFO *pInfo);

    virtual void OnProcessExit(DWORD                        dwProcessID,
                               EXIT_PROCESS_DEBUG_INFO     *pInfo);

    virtual void OnException(DWORD                          dwProcessID,
                             DWORD                          dwThreadID,
                             EXCEPTION_DEBUG_INFO          *pInfo,
                             bool                          *pbExceptionHandled);

    virtual void OnDebugString(DWORD                        dwProcessID,
                               DWORD                        dwThreadID,
                               OUTPUT_DEBUG_STRING_INFO    *pInfo);

    /*
     * Method that should be used by this class and all non-leaf subclasses to generate debugging
     * output. Intentionally using LPCSTR for szFormat and not LPCWSTR to allow passing format
     * strings without having to use the Unicode literal string modifier (L"").
     */
    void OnDbgOut(LPCSTR szFormat, ...);

    /*
     * Method that can be used by leaf subclasses to consume all debugging output from superclasses
     * and do something useful with it.
     */
    virtual void OnDbgOut(LPCWSTR wszFormat, va_list argList);

};
