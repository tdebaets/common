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
 * Test application for the CProcessDebug class
 *
 ****************************************************************************/

#include <ProcessDebug.h>
#include <ProcessDLLInject.h>

#include "framework.h"
#include "ProcesDebugTest.h"

class CMyProcessDebug : public CProcessDebug
{
protected:

    virtual bool OnProcessCreate(DWORD                      dwProcessID,
                                 DWORD                      dwThreadID,
                                 CREATE_PROCESS_DEBUG_INFO *pInfo)
    {
        DbgOut("Process created, PID: %u", dwProcessID);
        return true;
    }

    virtual void OnProcessExit(DWORD                        dwProcessID,
                               EXIT_PROCESS_DEBUG_INFO     *pInfo)
    {
        DbgOut("Process exited, PID: %u", dwProcessID);
    }

    virtual void OnException(DWORD                          dwProcessID,
                             DWORD                          dwThreadID,
                             EXCEPTION_DEBUG_INFO          *pInfo,
                             bool                          *pbExceptionHandled)
    {
        DbgOut("Process exception");
    }

    virtual void OnDebugString(DWORD                        dwProcessID,
                               DWORD                        dwThreadID,
                               OUTPUT_DEBUG_STRING_INFO    *pInfo)
    {
        DbgOut("Process debug string");
    }

    virtual void OnDbgOut(LPCTSTR message)
    {
        DbgOut("%s", message);
    }
};

int APIENTRY wWinMain(_In_ HINSTANCE        hInstance,
                      _In_opt_ HINSTANCE    hPrevInstance,
                      _In_ LPWSTR           lpCmdLine,
                      _In_ int              nCmdShow)
{
    STARTUPINFO         startupInfo = {};
    PROCESS_INFORMATION procInfo    = {};
    //CMyProcessDebug     processDebug;
    CProcessDLLInject processDebug(L"version.dll", L"version.dll");

    startupInfo.cb = sizeof(startupInfo);

    if (lpCmdLine[0] == '\0')
        return 1;

    if (!CreateProcess(NULL,
                       lpCmdLine,
                       NULL, NULL,
                       TRUE, /* bInheritHandles */
                       DEBUG_PROCESS | DEBUG_ONLY_THIS_PROCESS,
                       NULL, NULL,
                       &startupInfo,
                       &procInfo))
    {
        DbgOut("CreateProcess failed (%u)", GetLastError());
        return 2;
    }

    CloseHandleSafe(&procInfo.hProcess);
    CloseHandleSafe(&procInfo.hThread);

    if (!DebugSetProcessKillOnExit(FALSE))
    {
        DbgOut("DebugSetProcessKillOnExit failed! (%u)", GetLastError());
    }

    processDebug.Run();

    DbgOut("Done");

    return 0;
}
