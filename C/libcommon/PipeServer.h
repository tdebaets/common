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
 * Helper classes for implementing a named pipe server
 *
 ****************************************************************************/

#pragma once

#include <BaseThread.h>

class CPipeServerThread : public CBaseThread
{
public:

    CPipeServerThread(const wstring strPipeName, size_t szMsgSize);

    virtual void Stop();

    virtual void OnError(DWORD dwErrorCode) = 0;

protected: 

    virtual DWORD Run();

private:

    wstring m_strPipeName;
    size_t  m_szMsgSize;

};

class CPipeInstanceThread : public CBaseThread
{
public:

    CPipeInstanceThread(HANDLE hPipe);

    virtual void Stop();

protected:

    virtual DWORD Run();

private:

    HANDLE m_hPipe;

};