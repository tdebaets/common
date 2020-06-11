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
 * WOW64-related utility functions
 *
 ****************************************************************************/

#include "Wow64Utils.h"

#include <DbgHelp.h>
#include <NativeApi.h>
#include <PEUtils.h>
#include <strsafe.h>

FARPROC Wow64GetKnownDllProcAddress(LPCSTR lpModuleName, LPCSTR lpProcName)
{
    WCHAR                       sectionName[64];
    UNICODE_STRING              usSectionName       = {};
    SECTION_IMAGE_INFORMATION   sectionImgInfo      = {};
    OBJECT_ATTRIBUTES           objAttrs            = {};
    PIMAGE_NT_HEADERS32         pNtHeaders          = NULL;
    HMODULE                     hNtDll              = 0;
    HANDLE                      hSection            = 0;
    PVOID                       pBaseAddress        = NULL;
    PVOID                       pWow64BaseAddress   = NULL;
    FARPROC                     pResult             = NULL;
    DWORD                       dwProcRVA           = 0;
    SIZE_T                      viewSize            = 0;
    SIZE_T                      length              = 0;
    NTSTATUS                    status              = 0;

    InitializeObjectAttributes(&objAttrs, NULL, 0, NULL, NULL);

    if (!LoadNativeApi())
        goto exit;

    if (!SUCCEEDED(StringCbPrintfW(sectionName, sizeof(sectionName),
                                   L"\\KnownDlls32\\%hs",
                                   lpModuleName)))
    {
        goto exit;
    }

    RtlInitUnicodeString(&usSectionName, sectionName);

    objAttrs.ObjectName = &usSectionName;

    status = NtOpenSection(&hSection, SECTION_QUERY | SECTION_MAP_READ, &objAttrs);
    if (!NT_SUCCESS(status))
        goto exit;

    status = NtQuerySection(hSection,
                            SectionImageInformation,
                            &sectionImgInfo, sizeof(sectionImgInfo),
                            &length);
    if (!NT_SUCCESS(status))
        goto exit;

    if (!sectionImgInfo.EntryPoint)
        goto exit;

    status = NtMapViewOfSection(hSection,
                                GetCurrentProcess(),
                                &pBaseAddress,
                                0, 0, NULL,
                                &viewSize,
                                ViewUnmap,
                                0,
                                PAGE_READONLY);
    if (!NT_SUCCESS(status))
        goto exit;

    pNtHeaders = (PIMAGE_NT_HEADERS32)ImageNtHeader(pBaseAddress);
    if (!pNtHeaders)
        goto exit;

    pWow64BaseAddress = (PBYTE)sectionImgInfo.EntryPoint -
        pNtHeaders->OptionalHeader.AddressOfEntryPoint;

    dwProcRVA = GetProcRVA(pBaseAddress, lpProcName, NULL);
    if (dwProcRVA == 0)
        goto exit;

    pResult = (FARPROC)RtlOffsetToPointer(pWow64BaseAddress, dwProcRVA);

exit:

    if (pBaseAddress)
    {
        NtUnmapViewOfSection(GetCurrentProcess(), pBaseAddress);
    }

    if (hSection)
    {
        NtClose(hSection);
    }

    return pResult;
}
