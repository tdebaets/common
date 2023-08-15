/****************************************************************************
 *
 * Copyright 2023 Tim De Baets
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
 * Helper class that implements a thread running a Windows message loop
 *
 ****************************************************************************/

#include <Utils.h>

#include "MessageLoopThread.h"

CMessageLoopThreadError::CMessageLoopThreadError(const char *message)
    : m_message(message)
{
    m_dwErrorCode = GetLastError();
}

const char *CMessageLoopThreadError::what() const
{
    return m_message.c_str();
}

CMessageLoopThread::CMessageLoopThread() :
    m_hThread(NULL),
    m_dwThreadID(0)
{

}

CMessageLoopThread::~CMessageLoopThread()
{
    DWORD dwExitCode;

    if (m_hThread)
    {
        if (!GetExitCodeThread(m_hThread, &dwExitCode))
            throw CMessageLoopThreadError("GetExitCodeThread() failed");

        if (dwExitCode == STILL_ACTIVE)
            throw CMessageLoopThreadError("Message loop thread still running");

        CloseHandle(m_hThread);
        m_hThread = NULL;
    }
}

void CMessageLoopThread::Start()
{
    m_hThread = CreateThread(NULL, 0, CMessageLoopThread::ThreadProc, this, 0, &m_dwThreadID);

    if (!m_hThread)
        throw CMessageLoopThreadError("CreateThread() failed");
}

void CMessageLoopThread::Stop()
{
    if (!m_hThread || !m_dwThreadID)
        throw CMessageLoopThreadError("Thread not started");

    PostThreadMessage(m_dwThreadID, WM_QUIT, 0, 0);

    if (WaitForSingleObject(m_hThread, INFINITE) != WAIT_OBJECT_0)
        throw CMessageLoopThreadError("WaitForSingleObject() failed");
}

DWORD WINAPI CMessageLoopThread::ThreadProc(LPVOID lpParameter)
{
    CMessageLoopThread *pThis = reinterpret_cast<CMessageLoopThread *>(lpParameter);
    MSG                 msg;
    BOOL                fRet;

    if (!pThis->OnThreadBegin())
        return 1;

    while ((fRet = GetMessage(&msg, NULL, 0, 0)) != 0)
    {
        if (fRet == -1)
            throw GetLastError();

        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    pThis->OnThreadEnd();

    return (DWORD)msg.wParam;
}

bool CMessageLoopThread::OnThreadBegin()
{
    return true;
}

void CMessageLoopThread::OnThreadEnd()
{
}
