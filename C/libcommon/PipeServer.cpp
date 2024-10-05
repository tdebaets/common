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

#include <strsafe.h>
#include <Utils.h>

#include "PipeServer.h"

// TODO
#define BUFSIZE 4096

// TODO
VOID GetAnswerToRequest(LPTSTR chRequest, 
   LPTSTR chReply, LPDWORD pchBytes)
{
   DbgOut(L"%s", chRequest);
   StringCchCopy(chReply, BUFSIZE, TEXT("Default answer from server"));
   *pchBytes = (lstrlen(chReply)+1)*sizeof(TCHAR);
}

CPipeServerThread::CPipeServerThread(const wstring strPipeName, size_t szMsgSize)
    : m_strPipeName(strPipeName),
      m_szMsgSize(szMsgSize)
{

}

void CPipeServerThread::Stop()
{
    // TODO
}

DWORD CPipeServerThread::Run()
{
    SECURITY_DESCRIPTOR secDesc     = {};
    SECURITY_ATTRIBUTES secAttrs    = {};

    if (!InitializeSecurityDescriptor(&secDesc, SECURITY_DESCRIPTOR_REVISION1)
        || !SetSecurityDescriptorDacl(&secDesc,
                                      TRUE /* bDaclPresent */,
                                      NULL,
                                      FALSE /* bDaclDefaulted */))
    {
        OnError(4); // TODO
        return 1;
    }

    secAttrs.nLength                = sizeof(secAttrs);
    secAttrs.lpSecurityDescriptor   = &secDesc;
    secAttrs.bInheritHandle         = FALSE;

    while (true) // TODO: check stop condition
    {
        HANDLE hPipe = CreateNamedPipe(m_strPipeName.c_str(),       // pipe name
                                       PIPE_ACCESS_DUPLEX,          // read/write access
                                       PIPE_TYPE_MESSAGE |          // message type pipe
                                       PIPE_READMODE_MESSAGE |      // message-read mode
                                       PIPE_WAIT,                   // blocking mode
                                       PIPE_UNLIMITED_INSTANCES,    // max. instances
                                       BUFSIZE,                     // output buffer size
                                       BUFSIZE,                     // input buffer size
                                       NMPWAIT_USE_DEFAULT_WAIT,    // client time-out
                                       &secAttrs);                  // default security attribute

        if (hPipe == INVALID_HANDLE_VALUE)
        {
            OnError(1); // TODO
            return 1;
        }

        /*
         * Wait for the client to connect; if it succeeds, the function returns a nonzero value.
         * If the function returns zero, GetLastError returns ERROR_PIPE_CONNECTED.
         */

        if (ConnectNamedPipe(hPipe, NULL) || (GetLastError() == ERROR_PIPE_CONNECTED))
        {
            /* Create a thread for this client */
            // TODO: use 'new' instead and keep in list
            CPipeInstanceThread *pThread = new CPipeInstanceThread(hPipe);
            pThread->Start();
        }
        else
        {
            /* The client could not connect, so close the pipe */
            CloseHandleSafe(&hPipe);
        }
    }

    return 0;
}

CPipeInstanceThread::CPipeInstanceThread(HANDLE hPipe)
    : m_hPipe(hPipe)
{

}

void CPipeInstanceThread::Stop()
{
    throw CBaseThreadError("Not implemented");
}

DWORD CPipeInstanceThread::Run()
{
    TCHAR   chRequest[BUFSIZE]; 
    TCHAR   chReply[BUFSIZE]; 
    DWORD   cbBytesRead, cbReplyBytes, cbWritten; 
    BOOL    fSuccess; 
 
    while (1) 
    { 
        // Read client requests from the pipe. 
        fSuccess = ReadFile(m_hPipe,        // handle to pipe 
                            chRequest,    // buffer to receive data 
                            BUFSIZE * sizeof(TCHAR), // size of buffer 
                            &cbBytesRead, // number of bytes read 
                            NULL);        // not overlapped I/O 
        if (!fSuccess || cbBytesRead == 0) 
            break;

        GetAnswerToRequest(chRequest, chReply, &cbReplyBytes); 
 
        // Write the reply to the pipe. 
        fSuccess = WriteFile(m_hPipe,        // handle to pipe
                             chReply,      // buffer to write from 
                             cbReplyBytes, // number of bytes to write 
                             &cbWritten,   // number of bytes written 
                             NULL);        // not overlapped I/O 
        if (!fSuccess || cbReplyBytes != cbWritten)
            break; 
    } 
 
    // Flush the pipe to allow the client to read the pipe's contents 
    // before disconnecting. Then disconnect the pipe, and close the 
    // handle to this pipe instance. 
 
    FlushFileBuffers(m_hPipe); 
    DisconnectNamedPipe(m_hPipe); 
    CloseHandleSafe(&m_hPipe); 

    return 0;
}
