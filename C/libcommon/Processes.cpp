/****************************************************************************
 *
 * Copyright 2020 Tim De Baets
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
 * Process-related utility functions
 *
 ****************************************************************************/

#include <ntstatus.h>

#define WIN32_NO_STATUS
#include "Processes.h"
#undef WIN32_NO_STATUS

bool GetProcessSidToken(HANDLE hProc, SidWrapper& refSid)
{
    bool        bSuccess    = false;
    HANDLE      hToken      = NULL;
    DWORD       dwLength    = 0;
    PSID        pSid        = NULL;
    ByteVector  tokenUserVector;

    if (!OpenProcessToken(hProc, TOKEN_READ | TOKEN_QUERY, &hToken))
        goto exit;

    if (!GetTokenInformation(hToken, TokenUser, NULL, 0, &dwLength))
    {
        if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
            goto exit;

        tokenUserVector.resize(dwLength, 0);
    }

    if (!GetTokenInformation(
            hToken,
            TokenUser,
            &tokenUserVector[0], tokenUserVector.size(),
            &dwLength))
    {
        goto exit;
    }

    pSid = ((PTOKEN_USER)&tokenUserVector[0])->User.Sid;

    if (!IsValidSid(pSid))
        goto exit;

    refSid.resize(GetLengthSid(pSid), 0);

    if (CopySid(refSid.size(), (PSID)&refSid[0], pSid))
        bSuccess = true;

exit:

    if (hToken)
    {
        CloseHandle(hToken);
    }

    return bSuccess;
}

static SID_IDENTIFIER_AUTHORITY ntAuthority = SECURITY_NT_AUTHORITY;

bool GetProcessSid(DWORD dwProcId, HANDLE hProc, SidWrapper& refSid)
{
    bool bSuccess       = false;
    bool bProcOpened    = false;
    PSID pSystemSid     = NULL;

    if (!hProc)
    {
        hProc = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, dwProcId);
        bProcOpened = true;
    }

    if (!hProc)
        goto exit;

    // TODO: call WinStaGetProcessSid first (see Delphi version of this function)
    if (GetProcessSidToken(hProc, refSid))
    {
        bSuccess = true;
    }
    else if (dwProcId == 4 || dwProcId == 8)
    {
        // both functions can fail for System (procid 4 or 8), so create the SID ourselves
        if (AllocateAndInitializeSid(
                &ntAuthority,
                1,
                SECURITY_LOCAL_SYSTEM_RID,
                0, 0, 0, 0, 0, 0, 0,
                &pSystemSid))
        {
            refSid.resize(GetLengthSid(pSystemSid), 0);

            if (CopySid(refSid.size(), (PSID)&refSid[0], pSystemSid))
            {
                bSuccess = true;
            }

            FreeSid(pSystemSid);
        }
    }

exit:

    if (bProcOpened && hProc)
    {
        CloseHandle(hProc);
    }

    return bSuccess;
}

BOOL SidEqualsKnownSid(PSID pSid, DWORD dwSubAuthority)
{
    BOOL    bEqual      = FALSE;
    PSID    pKnownSid   = NULL;

    if (AllocateAndInitializeSid(
            &ntAuthority,
            1,
            dwSubAuthority,
            0, 0, 0, 0, 0, 0, 0,
            &pKnownSid))
    {
        bEqual = EqualSid(pSid, pKnownSid);

        FreeSid(pKnownSid);
    }

    return bEqual;
}

bool IsSystemSid(PSID pSid)
{
    return SidEqualsKnownSid(pSid, SECURITY_LOCAL_SYSTEM_RID) ||
        SidEqualsKnownSid(pSid, SECURITY_LOCAL_SERVICE_RID) ||
        SidEqualsKnownSid(pSid, SECURITY_LOCAL_SERVICE_RID);
}

/**
 * Enumerates all open handles.
 *
 * \param ProcessHandle A handle to the process. The handle must have PROCESS_QUERY_INFORMATION access.
 * \param pHandles A variable which receives a pointer to a structure containing information about
 * handles opened by the process. You must free the structure using delete when you no longer need it.
 *
 * \retval STATUS_INSUFFICIENT_RESOURCES The handle information returned by the kernel is too large.
 *
 * \remarks This function is only available starting with Windows 8.
 */
NTSTATUS PhEnumHandlesEx2(_In_  HANDLE                                  ProcessHandle,
                          _Out_ PPROCESS_HANDLE_SNAPSHOT_INFORMATION   *pHandles)
{
    NTSTATUS                                status;
    PPROCESS_HANDLE_SNAPSHOT_INFORMATION    pHandleInfo     = NULL;
    ULONG                                   bufferSize      = 0;
    ULONG                                   returnLength    = 0;
    ULONG                                   attempts        = 0;

    bufferSize = 0x8000;
    pHandleInfo = reinterpret_cast<PPROCESS_HANDLE_SNAPSHOT_INFORMATION>(new BYTE[bufferSize]);
    pHandleInfo->NumberOfHandles = 0;

    status = NtQueryInformationProcess(ProcessHandle,
                                       ProcessHandleInformation,
                                       pHandleInfo, bufferSize,
                                       &returnLength);

    while (status == STATUS_INFO_LENGTH_MISMATCH && attempts < 8)
    {
        delete[] pHandleInfo;
        bufferSize = returnLength;
        pHandleInfo = reinterpret_cast<PPROCESS_HANDLE_SNAPSHOT_INFORMATION>(new BYTE[bufferSize]);
        pHandleInfo->NumberOfHandles = 0;

        status = NtQueryInformationProcess(ProcessHandle,
                                           ProcessHandleInformation,
                                           pHandleInfo, bufferSize,
                                           &returnLength);

        attempts++;
    }

    if (NT_SUCCESS(status))
    {
        // NOTE: This is needed to workaround minimal processes on Windows 10
        // returning STATUS_SUCCESS with invalid handle data. (dmex)
        // NOTE: 21H1 and above no longer set NumberOfHandles to zero before returning
        // STATUS_SUCCESS so we first zero the entire buffer using PhAllocateZero. (dmex)
        if (pHandleInfo->NumberOfHandles == 0)
        {
            status = STATUS_UNSUCCESSFUL;
            delete[] pHandleInfo;
        }
        else
        {
            *pHandles = pHandleInfo;
        }
    }
    else
    {
        delete[] pHandleInfo;
    }

    return status;
}
