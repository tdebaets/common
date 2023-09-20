/****************************************************************************
 *
 * Copyright 2020-2022 Tim De Baets
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
 * Misc. utility definitions
 *
 ****************************************************************************/

#pragma once

#include <string>
#include <windows.h>
#include <winternl.h>   // NTSTATUS

using namespace std;

EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_THISCOMPONENT ((HINSTANCE)&__ImageBase)

extern const LPCSTR g_szKernel32;
extern const LPCSTR g_szUser32;

NTSTATUS GetPathFromHandle(HANDLE hObject, wstring &refStrPath);
BOOL CloseHandleSafe(PHANDLE phObject);

wstring GetModuleName(HMODULE hModule);

bool Is64BitWindows();
bool Is64BitExplorerRunning(bool & bIs64Bit);

bool LoadResString(HINSTANCE hInstance, UINT uID, wstring & refString);

HRESULT PatchCOMMethod(PVOID    pObj,
                       WORD     wMethodIndex,
                       PVOID    pNewAddress,
                       PVOID   *ppOldAddress);

/*
 * Defining DbgOut as a macro to allow projects to override this with their own
 * specific define (e.g. to prepend a project-specific prefix).
 */
#define DbgOut(kwszDebugFormatString, ...) \
    _DbgOut(kwszDebugFormatString, __VA_ARGS__)

void _DbgOut(LPCWSTR kwszDebugFormatString, ...);

#ifdef _DEBUG
#define DBGPRINT(kwszDebugFormatString, ...) \
    _DBGPRINT(__FUNCTIONW__, __LINE__, kwszDebugFormatString, __VA_ARGS__)

void _DBGPRINT(LPCWSTR  kwszFunction,
               INT      iLineNumber,
               LPCWSTR  kwszDebugFormatString,
               ...);
#else
#define DBGPRINT(kwszDebugFormatString, ...) ;;
#endif
