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
 * Helper class to walk the modules referenced by a PE module
 *
 ****************************************************************************/

#include "PEModuleWalker.h"
#include <assert.h>
#include <iomanip>
#include <sstream>

#include "PEUtils.h"
#include "Utils.h"

CPEModuleWalkerError::CPEModuleWalkerError(HMODULE hMod, unsigned int errorCode)
:   m_errorCode(errorCode)
{
    ostringstream oss;
    oss << "Failed to walk PE imports of module 0x"
        << hex << setw(sizeof(hMod) * 2) << setfill('0')
        << hMod << ": " << errorCode;
    m_message = string(oss.str());
}

const char *CPEModuleWalkerError::what() const
{
    return m_message.c_str();
}

CPEModuleWalker::CPEModuleWalker(HMODULE hMod)
:   m_hMod(hMod),
    m_pFirstImpDesc(NULL)
{
    ParsePEHeader();
}

void CPEModuleWalker::ThrowError(unsigned int ErrorCode)
{
    throw CPEModuleWalkerError(m_hMod, ErrorCode);
}

void CPEModuleWalker::ParsePEHeader()
{
    PIMAGE_DOS_HEADER   pDosHeader      = NULL;
    PIMAGE_NT_HEADERS   pNtHeaders      = NULL;
    DWORD               dwRVAImportDir  = 0;

    if (!m_hMod)
    {
        ThrowError(PE_MOD_WALKER_INVALID_HMOD);
    }

    // Don't hook ourselves
    if (m_hMod == HINST_THISCOMPONENT)
    {
        ThrowError(PE_MOD_WALKER_OWN_HMOD);
    }

    // Only required on Win9x
    /*if (m_hMod <= ((HMODULE)0xFFFFFFFF)
    {
        ThrowError(PE_MOD_WALKER_);
    }*/

    pDosHeader = (PIMAGE_DOS_HEADER)m_hMod;
    if (pDosHeader->e_magic != IMAGE_DOS_SIGNATURE)
    {
        ThrowError(PE_MOD_WALKER_INVALID_DOS_SIGNATURE);
    }

    if (!pDosHeader->e_lfanew)
    {
        ThrowError(PE_MOD_WALKER_NO_EXE_HDR);
    }

    pNtHeaders = (PIMAGE_NT_HEADERS)RVAToAbsolute(pDosHeader->e_lfanew);

    if (pNtHeaders->Signature != IMAGE_NT_SIGNATURE)
    {
        ThrowError(PE_MOD_WALKER_INVALID_NT_SIGNATURE);
    }

    if (!pNtHeaders->FileHeader.SizeOfOptionalHeader)
    {
        ThrowError(PE_MOD_WALKER_NO_OPTIONAL_HDR);
    }

    dwRVAImportDir =
        pNtHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress;
    if (!dwRVAImportDir)
    {
        ThrowError(PE_MOD_WALKER_NO_IMPORT_DIR);
    }

    m_pFirstImpDesc = (PIMAGE_IMPORT_DESCRIPTOR)RVAToAbsolute(dwRVAImportDir);
}

bool CPEModuleWalker::WalkImportModules()
{
    PIMAGE_IMPORT_DESCRIPTOR pImpDesc = m_pFirstImpDesc;

    assert(m_pFirstImpDesc);

    while (pImpDesc->FirstThunk)
    {
        if (pImpDesc->Name)
        {
            const char *name = (const char *)RVAToAbsolute(pImpDesc->Name);

            if (!ImportModuleProc(pImpDesc, name))
                return false;
        }

        pImpDesc++;
    }

    return true;
}

PVOID CPEModuleWalker::RVAToAbsolute(DWORD_PTR dwRVA)
{
    return ::RVAToAbsolute((PVOID)m_hMod, dwRVA);
}
