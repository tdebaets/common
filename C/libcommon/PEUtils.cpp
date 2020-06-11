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
 * Utility functions related to the Portable Executable format
 *
 ****************************************************************************/

#include "PEUtils.h"

#include <DbgHelp.h>
#include <NativeApi.h>
#include <Utils.h>

PVOID RVAToAbsolute(PVOID pBase, DWORD_PTR dwRVA)
{
    return (PVOID)((uintptr_t)pBase + (uintptr_t)dwRVA);
}

bool PatchImport(PIMAGE_THUNK_DATA    pAddrThunk,
                 PVOID                pNewProc,
                 PVOID               *ppPrevProc)
{
    PVOID   lpAddress   = &pAddrThunk->u1.AddressOfData;
    DWORD   oldProtect  = 0;
    bool    result      = false;

    if (!pAddrThunk || !pNewProc)
    {
        SetLastError(ERROR_INVALID_PARAMETER);
        goto exit;
    }

    if (!VirtualProtect(lpAddress, sizeof(lpAddress),
                        PAGE_EXECUTE_READWRITE,
                        &oldProtect))
    {
        // last error already set
        goto exit;
    }

    if ((PVOID)pAddrThunk->u1.AddressOfData == pNewProc)
    {
        // Already hooked
        SetLastError(ERROR_ALREADY_EXISTS);
        goto exit;
    }

    if (ppPrevProc)
    {
        *ppPrevProc = (PVOID)pAddrThunk->u1.AddressOfData;
    }

    pAddrThunk->u1.AddressOfData = (ULONGLONG)pNewProc;

    result = true;

exit:

    if (oldProtect)
    {
        if (!VirtualProtect(lpAddress, sizeof(lpAddress),
                            oldProtect,
                            &oldProtect))
        {
            DbgOut(L"Failed to restore original protection of address thunk");
        }
    }

    return result;
}

INT LookupExport(PVOID pBaseAddress, PDWORD pNames, DWORD nNames, LPCSTR lpProcName)
{
    INT     iStart  = 0;
    INT     iIndex  = -1;
    INT     iMiddle = -1;
    INT     iEnd    = nNames - 1;
    INT     iCmp    = 0;
    CHAR   *name    = NULL;

    // Do a binary search on the name pointer table

    while (iStart <= iEnd && iIndex == -1)
    {
        iMiddle = (iStart + iEnd) >> 1;
        name = RtlOffsetToPointer(pBaseAddress, pNames[iMiddle]);

        iCmp = strcmp(name, lpProcName);
        if (iCmp == 0)
        {
            iIndex = iMiddle;
        }
        else if (iCmp < 0)
        {
            iStart = iMiddle + 1;
        }
        else
        {
            iEnd = iMiddle;
        }
    }

    return iIndex;
}

DWORD GetProcRVA(PVOID pBaseAddress, LPCSTR lpProcName, LPCSTR *ppForward)
{
    PIMAGE_EXPORT_DIRECTORY pExportDir      = NULL;
    ULONG                   ulExportDirSize = 0;
    DWORD                   dwExportDirRVA  = 0;
    PDWORD                  pFunctions      = NULL;
    PDWORD                  pNames          = NULL;
    PWORD                   pNameOrdinals   = NULL;
    DWORD                   dwProcRVA       = 0;
    INT                     iIndex          = 0;
    INT                     iDelta          = 0;
    BOOL                    bOrdinalSearch  = (HIWORD(lpProcName) == 0);
    WORD                    wOrdinal;

    if (ppForward)
    {
        *ppForward = NULL;
    }

    pExportDir = (PIMAGE_EXPORT_DIRECTORY)ImageDirectoryEntryToDataEx(pBaseAddress,
                        TRUE /* MappedAsImage */,
                        IMAGE_DIRECTORY_ENTRY_EXPORT,
                        &ulExportDirSize,
                        NULL);
    if (!pExportDir || ulExportDirSize == 0)
        return 0;

    dwExportDirRVA = RtlPointerToOffset(pBaseAddress, pExportDir);

    if (bOrdinalSearch)
    {
        wOrdinal = LOWORD(lpProcName);
    }
    else
    {
        pNames          = (PDWORD)RtlOffsetToPointer(pBaseAddress, pExportDir->AddressOfNames);
        pNameOrdinals   = (PWORD)RtlOffsetToPointer(pBaseAddress, pExportDir->AddressOfNameOrdinals);

        iIndex = LookupExport(pBaseAddress, pNames, pExportDir->NumberOfNames, lpProcName);
        if (iIndex == -1)
            return 0;

        wOrdinal = pNameOrdinals[iIndex];
    }

    pFunctions  = (PDWORD)RtlOffsetToPointer(pBaseAddress, pExportDir->AddressOfFunctions);
    iDelta      = pExportDir->Base - 1;

    dwProcRVA = pFunctions[wOrdinal - iDelta];

    // Check whether forwarded
    if (dwProcRVA >= dwExportDirRVA && dwProcRVA < dwExportDirRVA + ulExportDirSize)
    {
        if (!ppForward)
            return 0;

        *ppForward = (LPCSTR)RtlOffsetToPointer(pBaseAddress, dwProcRVA);

        return 0;
    }

    return dwProcRVA;
}
