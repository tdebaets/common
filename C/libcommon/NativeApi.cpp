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
 * Native NT API utility functions
 *
 ****************************************************************************/

#include "NativeApi.h"

NTSTATUS (NTAPI *NtOpenSection)(
    OUT PHANDLE             SectionHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes
);

NTSTATUS (NTAPI *NtQuerySection)(
    IN HANDLE                       SectionHandle,
    IN SECTION_INFORMATION_CLASS    InformationClass,
    OUT PVOID                       InformationBuffer,
    IN SIZE_T                       InformationBufferSize,
    OUT PSIZE_T                     ResultLength OPTIONAL
);

NTSTATUS (NTAPI *NtMapViewOfSection)(
    IN HANDLE                       SectionHandle,
    IN HANDLE                       ProcessHandle,
    OUT PVOID                      *BaseAddress,
    IN ULONG_PTR                    ZeroBits,
    IN SIZE_T                       CommitSize,
    IN OUT OPTIONAL PLARGE_INTEGER  SectionOffset,
    IN OUT PSIZE_T                  ViewSize,
    IN SECTION_INHERIT              InheritDisposition,
    IN ULONG                        AllocationType,
    IN ULONG                        Win32Protect
);

NTSTATUS (NTAPI *NtUnmapViewOfSection)(
    IN HANDLE           ProcessHandle,
    IN OPTIONAL PVOID   BaseAddress
);

bool bLoaded = false;

bool LoadNativeApi()
{
    HMODULE hNtDll = 0;

    if (!bLoaded)
    {
        bLoaded = true;

        hNtDll = GetModuleHandle(TEXT("ntdll.dll"));
        if (!hNtDll)
            return false;

        *(FARPROC *)&NtOpenSection          = GetProcAddress(hNtDll, "NtOpenSection");
        *(FARPROC *)&NtQuerySection         = GetProcAddress(hNtDll, "NtQuerySection");
        *(FARPROC *)&NtMapViewOfSection     = GetProcAddress(hNtDll, "NtMapViewOfSection");
        *(FARPROC *)&NtUnmapViewOfSection   = GetProcAddress(hNtDll, "NtUnmapViewOfSection");
    }

    return NtOpenSection && NtQuerySection && NtMapViewOfSection && NtUnmapViewOfSection;
}
