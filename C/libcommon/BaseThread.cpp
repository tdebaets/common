/****************************************************************************
 *
 * Copyright 2024 Tim De Baets
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
 * Base helper class for implementing a thread
 *
 ****************************************************************************/

#include <Utils.h>

#include "BaseThread.h"

CBaseThreadError::CBaseThreadError(const char *message)
    : m_message(message)
{
    m_dwErrorCode = GetLastError();
}

const char *CBaseThreadError::what() const
{
    return m_message.c_str();
}

CBaseThread::CBaseThread() :
    m_hThread(NULL),
    m_dwThreadID(0)
{

}

CBaseThread::~CBaseThread()
{
    DWORD dwExitCode;

    if (m_hThread)
    {
        if (!GetExitCodeThread(m_hThread, &dwExitCode))
            throw CBaseThreadError("GetExitCodeThread() failed");

        if (dwExitCode == STILL_ACTIVE)
            throw CBaseThreadError("Thread still running");

        CloseHandle(m_hThread);
        m_hThread = NULL;
    }
}

void CBaseThread::Start()
{
    m_hThread = CreateThread(NULL, 0, CBaseThread::ThreadProc, this, 0, &m_dwThreadID);

    if (!m_hThread)
        throw CBaseThreadError("CreateThread() failed");
}

void CBaseThread::WaitFor()
{
    if (!m_hThread || !m_dwThreadID)
        throw CBaseThreadError("Thread not started");

    if (WaitForSingleObject(m_hThread, INFINITE) != WAIT_OBJECT_0)
        throw CBaseThreadError("WaitForSingleObject() failed");
}

DWORD WINAPI CBaseThread::ThreadProc(LPVOID lpParameter)
{
    CBaseThread *pThis      = reinterpret_cast<CBaseThread *>(lpParameter);
    DWORD        dwExitCode = 0;

    if (!pThis->OnThreadBegin())
        return 1;

    dwExitCode = pThis->Run();

    pThis->OnThreadEnd();

    return dwExitCode;
}

bool CBaseThread::OnThreadBegin()
{
    return true;
}

void CBaseThread::OnThreadEnd()
{
}

DWORD CBaseThread::GetThreadID()
{
    return m_dwThreadID;
}
