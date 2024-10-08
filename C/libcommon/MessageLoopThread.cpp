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

void CMessageLoopThread::Stop()
{
    PostThreadMessage(GetThreadID(), WM_QUIT, 0, 0);

    WaitFor();
}

DWORD CMessageLoopThread::Run()
{
    MSG     msg;
    BOOL    fRet;

    while ((fRet = GetMessage(&msg, NULL, 0, 0)) != 0)
    {
        if (fRet == -1)
            throw GetLastError();

        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (DWORD)msg.wParam;
}
