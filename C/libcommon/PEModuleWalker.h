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

#pragma once

#include <exception>
#include <string>
#include <Windows.h>

using namespace std;

#define PE_MOD_WALKER_INVALID_HMOD              1
#define PE_MOD_WALKER_OWN_HMOD                  2
#define PE_MOD_WALKER_INVALID_DOS_SIGNATURE     3
#define PE_MOD_WALKER_NO_EXE_HDR                4
#define PE_MOD_WALKER_INVALID_NT_SIGNATURE      5
#define PE_MOD_WALKER_NO_OPTIONAL_HDR           6
#define PE_MOD_WALKER_NO_IMPORT_DIR             7
//#define PE_MOD_WALKER_IMPORTS_SECTION_NOT_FOUND 8

class CPEModuleWalkerError : public exception
{
private:

    unsigned int    m_errorCode;
    string          m_message;

public:

    CPEModuleWalkerError(HMODULE hMod, unsigned int errorCode);
    virtual const char *what() const;
};

class CPEModuleWalker
{
private:

    HMODULE                     m_hMod;
    PIMAGE_IMPORT_DESCRIPTOR    m_pFirstImpDesc;

private:

    void ThrowError(unsigned int ErrorCode);

protected:

    virtual bool ImportModuleProc(PIMAGE_IMPORT_DESCRIPTOR  pImpDesc,
                                  const char               *name) = 0;
    void ParsePEHeader();
    bool WalkImportModules();
    PVOID RVAToAbsolute(DWORD_PTR dwRVA);

public:

    CPEModuleWalker(HMODULE hMod);
};
