/****************************************************************************
 *
 * Copyright 2020-2023 Tim De Baets
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
 * Misc. utility functions
 *
 ****************************************************************************/

#include "Utils.h"

#include <strsafe.h>
#include <vector>

#include "NativeApi.h"
#include "WinNTStatus.h"

static const LPCSTR g_szKernel32    = "kernel32.dll";
static const LPCSTR g_szUser32      = "user32.dll";

NTSTATUS GetPathFromHandle(HANDLE hObject, wstring &refStrPath)
{
    ULONG           ulSize = 0;
    NTSTATUS        status;
    vector<BYTE>    vecBuffer;

    refStrPath.resize(0);

    // Query the name information a first time to get the size of the name.
    status = NtQueryObject(hObject, ObjectNameInformation, NULL, 0, &ulSize);
    
    if (ulSize > 0)
    {
        vecBuffer.resize(ulSize);

        POBJECT_NAME_INFORMATION pNameInfo = reinterpret_cast<POBJECT_NAME_INFORMATION>(&vecBuffer[0]);
        
        // Query the name information a second time to get the name of the object referenced by the
        // handle.
        status = NtQueryObject(hObject, ObjectNameInformation, pNameInfo, ulSize, &ulSize);

        if (NT_SUCCESS(status))
        {
            refStrPath.resize(pNameInfo->Name.Length / sizeof(WCHAR));

            if (pNameInfo->Name.Buffer && pNameInfo->Name.Length > 0)
            {
                wcsncpy_s(&refStrPath[0], refStrPath.capacity(),
                          pNameInfo->Name.Buffer,
                          refStrPath.capacity());
            }
        }
    }

    return status;
}

BOOL CloseHandleSafe(PHANDLE phObject)
{
    BOOL bResult = FALSE;

    if (!phObject)
    {
        SetLastError(ERROR_INVALID_PARAMETER);
        return FALSE;
    }

    if (*phObject == INVALID_HANDLE_VALUE)
    {
        SetLastError(ERROR_INVALID_PARAMETER);
        return FALSE;
    }

    bResult = CloseHandle(*phObject);

    *phObject = INVALID_HANDLE_VALUE;

    return bResult;
}

wstring GetModuleName(HMODULE hModule)
{
    TCHAR buffer[MAX_PATH];

    GetModuleFileName(hModule, buffer, ARRAYSIZE(buffer));

    return wstring(buffer);
}

bool Is64BitWindows()
{
#if defined(_WIN64)
    return TRUE;  // 64-bit programs run only on Win64
#elif defined(_WIN32)
    // 32-bit programs run on both 32-bit and 64-bit Windows
    // so must sniff
    BOOL f64 = FALSE;
    return IsWow64Process(GetCurrentProcess(), &f64) && f64;
#else
    return FALSE; // Win64 does not support Win16
#endif
}

bool Is64BitExplorerRunning(bool & bIs64Bit)
{
    HWND    hWndShell   = 0;
    DWORD   dwProcID    = 0;
    HANDLE  hProc       = 0;
    BOOL    bIsWow64    = FALSE;

    if (!Is64BitWindows())
    {
        bIs64Bit = false;
        return true;
    }

    hWndShell = GetShellWindow();
    if (!hWndShell)
        goto error_exit1;

    if (!GetWindowThreadProcessId(hWndShell, &dwProcID))
        goto error_exit1;

    hProc = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, dwProcID);
    if (!hProc)
        goto error_exit1;

    if (!IsWow64Process(hProc, &bIsWow64))
        goto error_exit2;

    bIs64Bit = !bIsWow64;

    CloseHandleSafe(&hProc);

    return TRUE;

error_exit2:

    CloseHandleSafe(&hProc);

error_exit1:

    return FALSE;
}

bool LoadResString(HINSTANCE hInstance, UINT uID, wstring & refString)
{
    TCHAR buffer[1024];

    if (LoadString(hInstance, uID, buffer, ARRAYSIZE(buffer)) == 0)
        return false;

    refString = wstring(buffer);

    return true;
}

// TODO: automatically determine method index by macro?
//      see "How do I convert a method name to a method index for the purpose of INTERFACEINFO?"
//      https://devblogs.microsoft.com/oldnewthing/20130329-00/?p=4813
HRESULT PatchCOMMethod(PVOID    pObj,
                       WORD     wMethodIndex,
                       PVOID    pNewAddress,
                       PVOID   *ppOldAddress)
{
    PVOID   pVFuncTable     = *((PVOID *)pObj);
    PVOID  *ppTableEntry    = (PVOID *)pVFuncTable; // first method
    PVOID   pOrigAddress    = NULL;
    DWORD   dwOldProtect    = 0;

    ppTableEntry += wMethodIndex;

    pOrigAddress = *ppTableEntry;

    /* Don't hook twice (prevent endless loop) */
    if (pOrigAddress == pNewAddress)
        return HRESULT_FROM_WIN32(ERROR_ALREADY_EXISTS);

    /*
     * For multithreaded COM objects, important to write old function address *before* patching the
     * function table!
     * Also using InterlockedExchangePointer to be really sure that other threads 'see' the old
     * address before we patch.
     */
    if (ppOldAddress)
    {
        InterlockedExchangePointer(ppOldAddress, pOrigAddress);
    }

    /*
     * Finally, patch the function table.
     * Using InterlockedExchangePointer() because WriteProcessMemory() isn't atomic (see "Is
     * WriteProcessMemory atomic?" - https://devblogs.microsoft.com/oldnewthing/20140515-00/?p=983)
     * The drawback of InterlockedExchangePointer() is that we have to mark the memory as writeable
     * first ourselves (WriteProcessMemory() already does this for us).
     */

    if (!VirtualProtect(ppTableEntry, sizeof(ppTableEntry), PAGE_EXECUTE_READWRITE, &dwOldProtect))
        return HRESULT_FROM_WIN32(GetLastError());

    InterlockedExchangePointer(ppTableEntry, pNewAddress);

    if (!VirtualProtect(ppTableEntry, sizeof(ppTableEntry), dwOldProtect, &dwOldProtect))
    {
        DbgOut(TEXT("Failed to restore original protection of virtual function table entry"));
    }

    return S_OK;
}

void _DbgOut(LPCWSTR kwszDebugFormatString, ...)
{
    INT     cbFormatString = 0;
    PWCHAR  wszDebugString = NULL;
    va_list args;

    va_start(args, kwszDebugFormatString);

    cbFormatString = _vscwprintf(kwszDebugFormatString, args) * sizeof(WCHAR) + 2;

    if (cbFormatString < 1 || cbFormatString > STRSAFE_MAX_CCH * sizeof(WCHAR))
        goto exit;

    /* Depending on the size of the format string, allocate space on the stack or the heap. */
    wszDebugString = (PWCHAR)_malloca(cbFormatString);
    if (!wszDebugString)
        goto exit;

    /* Populate the buffer with the contents of the format string. */
    StringCbVPrintfW(wszDebugString, cbFormatString, kwszDebugFormatString, args);

    OutputDebugStringW(wszDebugString);

    _freea(wszDebugString);

exit:
    va_end(args);
}

void _DBGPRINT(LPCWSTR  kwszFunction,
               INT      iLineNumber,
               LPCWSTR  kwszDebugFormatString,
               ...)
{
    INT     cbFormatString  = 0;
    PWCHAR  wszDebugString  = NULL;
    size_t  st_Offset       = 0;
    va_list args;

    va_start(args, kwszDebugFormatString);

    cbFormatString = _scwprintf(L"[%s:%d] ", kwszFunction, iLineNumber) * sizeof(WCHAR);
    cbFormatString += _vscwprintf(kwszDebugFormatString, args) * sizeof(WCHAR) + 2;

    if (cbFormatString < 1 || cbFormatString > STRSAFE_MAX_CCH * sizeof(WCHAR))
        goto exit;

    /* Depending on the size of the format string, allocate space on the stack or the heap. */
    wszDebugString = (PWCHAR)_malloca(cbFormatString);
    if (!wszDebugString)
        goto exit;

    /* Populate the buffer with the contents of the format string. */
    StringCbPrintfW(wszDebugString, cbFormatString, L"[%s:%d] ", kwszFunction, iLineNumber);

    if (!SUCCEEDED(StringCbLengthW(wszDebugString, cbFormatString, &st_Offset)))
        goto exit;

    StringCbVPrintfW(&wszDebugString[st_Offset / sizeof(WCHAR)],
                     cbFormatString - st_Offset,
                     kwszDebugFormatString,
                     args);

    OutputDebugStringW(wszDebugString);

    _freea(wszDebugString);

exit:
    va_end(args);
}
