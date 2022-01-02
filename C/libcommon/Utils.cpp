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
 * Misc. utility functions
 *
 ****************************************************************************/

#include "Utils.h"

#include <strsafe.h>

static const LPCSTR g_szKernel32    = "kernel32.dll";
static const LPCSTR g_szUser32      = "user32.dll";

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

bool LoadResString(HINSTANCE hInstance, UINT uID, wstring & refString)
{
    TCHAR buffer[1024];

    if (LoadString(hInstance, uID, buffer, ARRAYSIZE(buffer)) == 0)
        return false;

    refString = wstring(buffer);

    return true;
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
