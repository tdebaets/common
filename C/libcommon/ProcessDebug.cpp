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

#include "ProcessDebug.h"

#include <Utils.h>

CProcessDebug::CProcessDebug() :
    m_dwExitCode(0)
{

}

void CProcessDebug::Run()
{
    DEBUG_EVENT dbgEvent    = {};
    bool        bRunning    = true;

    while (bRunning)
    {
        bool bExceptionHandled = false;

        if (!WaitForDebugEventEx(&dbgEvent, INFINITE))
            break;

        switch (dbgEvent.dwDebugEventCode)
        {
        case CREATE_PROCESS_DEBUG_EVENT:
            if (!OnProcessCreate(dbgEvent.dwProcessId,
                                 dbgEvent.dwThreadId,
                                 &dbgEvent.u.CreateProcessInfo))
            {
                return;
            }
            break;
        case EXIT_PROCESS_DEBUG_EVENT:
            OnProcessExit(dbgEvent.dwProcessId, &dbgEvent.u.ExitProcess);
            bRunning = false;
            break;
        case EXCEPTION_DEBUG_EVENT:
            OnException(dbgEvent.dwProcessId,
                        dbgEvent.dwThreadId,
                        &dbgEvent.u.Exception,
                        &bExceptionHandled);
            switch (dbgEvent.u.Exception.ExceptionRecord.ExceptionCode)
            {
            // A 64-bit debugger debugging a Wow64 process will see both types of breakpoint
            // exceptions
            case EXCEPTION_BREAKPOINT:
#if defined(_WIN64)
            case STATUS_WX86_BREAKPOINT:
#endif
                OnDbgOut(TEXT("Breakpoint"));
                break;
            default:
                ContinueDebugEvent(dbgEvent.dwProcessId,
                                   dbgEvent.dwThreadId,
                                   bExceptionHandled ? DBG_CONTINUE : DBG_EXCEPTION_NOT_HANDLED);
                continue; // We just called ContinueDebugEvent(), so skip the call below
                break;
            }
            break;
        case CREATE_THREAD_DEBUG_EVENT:
            CloseHandleSafe(&dbgEvent.u.CreateThread.hThread);
            break;
        case LOAD_DLL_DEBUG_EVENT:
            CloseHandleSafe(&dbgEvent.u.LoadDll.hFile);
            break;
        case OUTPUT_DEBUG_STRING_EVENT:
            OnDebugString(dbgEvent.dwProcessId, dbgEvent.dwThreadId, &dbgEvent.u.DebugString);
            break;
        }

        // Must use DBG_CONTINUE here, otherwise exceptions with OUTPUT_DEBUG_STRING_EVENT on NT!!!!
        if (!ContinueDebugEvent(dbgEvent.dwProcessId, dbgEvent.dwThreadId, DBG_CONTINUE))
            break;
    }
}

// TODO: move?
DWORD CProcessDebug::GetExitCode()
{
    return m_dwExitCode;
}

bool CProcessDebug::OnProcessCreate(DWORD                       dwProcessID,
                                    DWORD                       dwThreadID,
                                    CREATE_PROCESS_DEBUG_INFO  *pInfo)
{
    return true;
}

void CProcessDebug::OnProcessExit(DWORD                         dwProcessID,
                                  EXIT_PROCESS_DEBUG_INFO      *pInfo)
{

}

void CProcessDebug::OnException(DWORD                           dwProcessID,
                                DWORD                           dwThreadID,
                                EXCEPTION_DEBUG_INFO           *pInfo,
                                bool                           *pbExceptionHandled)
{

}

void CProcessDebug::OnDebugString(DWORD                         dwProcessID,
                                  DWORD                         dwThreadID,
                                  OUTPUT_DEBUG_STRING_INFO     *pInfo)
{

}

void CProcessDebug::OnDbgOut(LPCTSTR message)
{

}
