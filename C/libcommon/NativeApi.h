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
 * Native NT API definitions
 *
 ****************************************************************************/

#pragma once

#include <windows.h>
#include <winternl.h>

#define ProcessHandleInformation (PROCESSINFOCLASS)51 // q: PROCESS_HANDLE_SNAPSHOT_INFORMATION // since WIN8

typedef struct _PROCESS_HANDLE_TABLE_ENTRY_INFO
{
    HANDLE      HandleValue;
    ULONG_PTR   HandleCount;
    ULONG_PTR   PointerCount;
    ULONG       GrantedAccess;
    ULONG       ObjectTypeIndex;
    ULONG       HandleAttributes;
    ULONG       Reserved;
} PROCESS_HANDLE_TABLE_ENTRY_INFO, *PPROCESS_HANDLE_TABLE_ENTRY_INFO;

typedef struct _PROCESS_HANDLE_SNAPSHOT_INFORMATION
{
    ULONG_PTR NumberOfHandles;
    ULONG_PTR Reserved;
    _Field_size_(NumberOfHandles) PROCESS_HANDLE_TABLE_ENTRY_INFO Handles[1];
} PROCESS_HANDLE_SNAPSHOT_INFORMATION, *PPROCESS_HANDLE_SNAPSHOT_INFORMATION;

#define ObjectNameInformation (OBJECT_INFORMATION_CLASS)1

typedef struct _OBJECT_NAME_INFORMATION
{
     UNICODE_STRING Name;
} OBJECT_NAME_INFORMATION, *POBJECT_NAME_INFORMATION;

typedef enum _SECTION_INFORMATION_CLASS
{
    SectionBasicInformation,
    SectionImageInformation
} SECTION_INFORMATION_CLASS, *PSECTION_INFORMATION_CLASS;

typedef struct _SECTION_IMAGE_INFORMATION
{
    PVOID                   EntryPoint;
    ULONG                   StackZeroBits;
    SIZE_T                  StackReserved;
    SIZE_T                  StackCommit;
    ULONG                   ImageSubsystem;
    WORD                    SubSystemVersionLow;
    WORD                    SubSystemVersionHigh;
    ULONG                   Unknown1;
    ULONG                   ImageCharacteristics;
    ULONG                   ImageMachineType;
    ULONG                   Unknown2[3];
} SECTION_IMAGE_INFORMATION, *PSECTION_IMAGE_INFORMATION;

extern NTSTATUS (NTAPI *NtOpenSection)(
    OUT PHANDLE             SectionHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes
);

extern NTSTATUS (NTAPI *NtQuerySection)(
    IN HANDLE                       SectionHandle,
    IN SECTION_INFORMATION_CLASS    InformationClass,
    OUT PVOID                       InformationBuffer,
    IN SIZE_T                       InformationBufferSize,
    OUT PSIZE_T                     ResultLength OPTIONAL
);

typedef enum _SECTION_INHERIT {
    ViewShare = 1,
    ViewUnmap = 2
} SECTION_INHERIT;

extern NTSTATUS (NTAPI *NtMapViewOfSection)(
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

extern NTSTATUS (NTAPI *NtUnmapViewOfSection)(
    IN HANDLE           ProcessHandle,
    IN OPTIONAL PVOID   BaseAddress
);

//++
//
// PCHAR
// RtlOffsetToPointer (
//     PVOID Base,
//     ULONG Offset
//     )
//
// Routine Description:
//
// This macro generates a pointer which points to the byte that is 'Offset'
// bytes beyond 'Base'. This is useful for referencing fields within
// self-relative data structures.
//
// Arguments:
//
//     Base - The address of the base of the structure.
//
//     Offset - An unsigned integer offset of the byte whose address is to
//         be generated.
//
// Return Value:
//
//     A PCHAR pointer to the byte that is 'Offset' bytes beyond 'Base'.
//
//
//--

#define RtlOffsetToPointer(B,O)  ((PCHAR)( ((PCHAR)(B)) + ((ULONG_PTR)(O))  ))


//++
//
// ULONG
// RtlPointerToOffset (
//     PVOID Base,
//     PVOID Pointer
//     )
//
// Routine Description:
//
// This macro calculates the offset from Base to Pointer.  This is useful
// for producing self-relative offsets for structures.
//
// Arguments:
//
//     Base - The address of the base of the structure.
//
//     Pointer - A pointer to a field, presumably within the structure
//         pointed to by Base.  This value must be larger than that specified
//         for Base.
//
// Return Value:
//
//     A ULONG offset from Base to Pointer.
//
//
//--

#define RtlPointerToOffset(B,P)  ((ULONG)( ((PCHAR)(P)) - ((PCHAR)(B))  ))


extern bool LoadNativeApi();
