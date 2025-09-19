/****************************************************************************
 *
 * Copyright 2025 Tim De Baets
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
 * Helper class for injecting a DLL into a process
 *
 ****************************************************************************/

#include "ProcessDLLInject.h"

#include <AtlBase.h>
#include <atlconv.h>
#include <intsafe.h>
#include <strsafe.h>

#include <Utils.h>
#include <Wow64Utils.h>

#define OPCODE_INT3         ((BYTE)0xCC)
#define OPCODE_REXW_PREFIX  ((BYTE)0x48)
#define OPCODE_SUB_ESP      ((WORD)0xEC83) // reverse byte order due to endianness
#define OPCODE_MOV_ECX      ((BYTE)0xB9)
#define OPCODE_MOV_EAX      ((BYTE)0xB8)
#define OPCODE_CALL_EAX     ((WORD)0xD0FF) // reverse byte order due to endianness
#define OPCODE_PUSH         ((BYTE)0x68)

// Use this to be able to attach a 'real' debugger to the child process
//#define SUSPEND_CHILD_PROCESS

#if defined(_WIN64)

#define CONTEXT_IP(pContext) (pContext)->Rip
#define CONTEXT_AX(pContext) (pContext)->Rax

// Native debugging when not a Wow64 process
#define IS_NATIVE_DEBUGGEE_PROCESS(bIsWow64Process) \
    (!bIsWow64Process)

#elif defined(_WIN32)

#define CONTEXT_IP(pContext) (pContext)->Eip
#define CONTEXT_AX(pContext) (pContext)->Eax

// Native debugging when on 32-bit Windows *or* a Wow64 process
#define IS_NATIVE_DEBUGGEE_PROCESS(bIsWow64Process) \
    (!m_bIs64BitWindows || bIsWow64Process)

#else
#error "Unsupported architecture"
#endif

bool ReadTargetMemory(HANDLE hProc, PVOID pAddress, PVOID pBuf, SIZE_T bufSize)
{
    SIZE_T cbRead = 0;

    return ReadProcessMemory(hProc, pAddress, pBuf, bufSize, &cbRead) && (cbRead == bufSize);
}

bool ReadProcessString(HANDLE   hProc,
                       bool     bUnicode,
                       PVOID    pAddress,
                       DWORD    dwLength,
                       wstring &refString)
{
    string  ansiString;
    PVOID   pBuf    = NULL;
    SIZE_T  bufSize = 0;

    refString.clear();

    if (!pAddress || dwLength == 0)
    {
        SetLastError(ERROR_INVALID_PARAMETER);
        return false;
    }

    if (bUnicode)
    {
        refString.resize(dwLength);
        pBuf    = &refString[0];
        bufSize = refString.size() * sizeof(refString[0]);
    }
    else
    {
        ansiString.resize(dwLength);
        pBuf    = &ansiString[0];
        bufSize = ansiString.size() * sizeof(ansiString[0]);
    }

    if (!ReadTargetMemory(hProc, pAddress, pBuf, bufSize))
        return false;

    if (!bUnicode)
    {
        CA2W ca2w(ansiString.c_str());
        refString = ca2w;
    }

    return true;
}

bool WriteTargetMemory(HANDLE hProc, PVOID pAddress, PVOID pBuf, SIZE_T bufSize)
{
    SIZE_T cbWritten = 0;

    return WriteProcessMemory(hProc, pAddress, pBuf, bufSize, &cbWritten) && (cbWritten == bufSize);
}

bool WriteTargetByte(HANDLE hProc, PVOID pAddress, BYTE byte)
{
    return WriteTargetMemory(hProc, pAddress, &byte, sizeof(byte));
}

CProcessDLLInject::CProcessDLLInject(const wstring &strDLLFilename32, const wstring &strDLLFilename64) :
    m_strDLLFilename32(strDLLFilename32),
    m_strDLLFilename64(strDLLFilename64)
{
    m_bIs64BitWindows = Is64BitWindows();

    if (!GetNativeLoadLibraryAddress())
    {
        DbgOut(L"Failed to get native LoadLibrary address");
        // TODO: throw exception
    }
}

bool CProcessDLLInject::InitLoadLibraryStub32(tLoadLibraryStub32  *pStub,              /* OUT */
                                              PVOID                pStubInTarget,
                                              const WCHAR         *dllName,
                                              FARPROC              pLoadLibraryAddr,
                                              PVOID               *ppStubInTargetBP    /* OUT */)
{
    if (!pStub || !pStubInTarget || !dllName || !pLoadLibraryAddr || !ppStubInTargetBP)
        return false;

    if (HIDWORD((DWORD64)pStubInTarget) != 0)
    {
        DbgOut(L"Upper 32 bits of stub address in target process are nonzero (0x%p)", pStubInTarget);
        return false;
    }

    if (HIDWORD((DWORD64)pLoadLibraryAddr) != 0)
    {
        DbgOut(L"Upper 32 bits of LoadLibraryW address are nonzero (0x%p)", pLoadLibraryAddr);
        return false;
    }

    pStub->instr_PUSH           = OPCODE_PUSH;
    pStub->operand_PUSH_value   = LODWORD((DWORD64)pStubInTarget) +
        offsetof(tLoadLibraryStub32, data_DllName);
    pStub->instr_MOV_EAX        = OPCODE_MOV_EAX;
    pStub->operand_MOV_EAX      = LODWORD((DWORD64)pLoadLibraryAddr);
    pStub->instr_CALL_EAX       = OPCODE_CALL_EAX;
    pStub->instr_INT_3          = OPCODE_INT3;

    if (!SUCCEEDED(StringCchCopyW(pStub->data_DllName, ARRAYSIZE(pStub->data_DllName),
                                  dllName)))
    {
        return false;
    }

    *ppStubInTargetBP = (PBYTE)pStubInTarget + offsetof(tLoadLibraryStub32, instr_INT_3);

    return true;
}

bool CProcessDLLInject::InitLoadLibraryStub64(tLoadLibraryStub64  *pStub,              /* OUT */
                                              PVOID                pStubInTarget,
                                              const WCHAR         *dllName,
                                              FARPROC              pLoadLibraryAddr,
                                              PVOID               *ppStubInTargetBP    /* OUT */)
{
    if (!pStub || !pStubInTarget || !dllName || !pLoadLibraryAddr || !ppStubInTargetBP)
        return false;

    pStub->prefix_SUB_RSP   = OPCODE_REXW_PREFIX;
    pStub->instr_SUB_RSP    = OPCODE_SUB_ESP;
    pStub->operand_SUB_RSP  = 0x8; // TODO: allocate more than 8 for home space?
    pStub->prefix_MOV_RCX   = OPCODE_REXW_PREFIX;
    pStub->instr_MOV_RCX    = OPCODE_MOV_ECX;
    pStub->operand_MOV_RCX  = (PBYTE)pStubInTarget +
        offsetof(tLoadLibraryStub64, data_DllName);
    pStub->prefix_MOV_RAX   = OPCODE_REXW_PREFIX;
    pStub->instr_MOV_RAX    = OPCODE_MOV_EAX;
    pStub->operand_MOV_RAX  = pLoadLibraryAddr;
    pStub->instr_CALL_RAX   = OPCODE_CALL_EAX;
    pStub->instr_INT_3      = OPCODE_INT3;

    if (!SUCCEEDED(StringCchCopyW(pStub->data_DllName, ARRAYSIZE(pStub->data_DllName),
                                  dllName)))
    {
        return false;
    }

    *ppStubInTargetBP = (PBYTE)pStubInTarget + offsetof(tLoadLibraryStub64, instr_INT_3);

    return true;
}

bool CProcessDLLInject::GetNativeLoadLibraryAddress()
{
    HMODULE hKernel32 = GetModuleHandleA(g_szKernel32);

    if (!hKernel32)
        return false;

    /*
     * Get the address of the native LoadLibraryW function in kernel32.dll. This address can only
     * be used when our architecture matches the architecture of the debuggee. In the other case
     * (64-bit debugger debugging a Wow64 process), we need another way to get the correct
     * LoadLibraryW address, see the Wow64GetKnownDllProcAddress() call in OnProcessCreate().
     */
    m_pNativeLoadLibraryW = GetProcAddress(hKernel32, "LoadLibraryW");

    return (m_pNativeLoadLibraryW != NULL);
}

bool CProcessDLLInject::SetEntryPointBP(tProcInfo *pProcInfo)
{
    if (!ReadTargetMemory(pProcInfo->createInfo.hProcess,
                          pProcInfo->createInfo.lpStartAddress,
                          &pProcInfo->byOriginalEntryPointOpcode,
                          sizeof(pProcInfo->byOriginalEntryPointOpcode)))
    {
        DbgOut(L"Failed to read original entry-point opcode from target process (%u)", GetLastError());
        return false;
    }

    if (!WriteTargetByte(pProcInfo->createInfo.hProcess,
                         pProcInfo->createInfo.lpStartAddress,
                         OPCODE_INT3))
    {
        DbgOut(L"Failed to write INT3 entry-point opcode to target process (%u)", GetLastError());
        return false;
    }

    return true;
}

bool CProcessDLLInject::RemoveEntryPointBP(tProcInfo *pProcInfo)
{
    return WriteTargetMemory(pProcInfo->createInfo.hProcess,
                             pProcInfo->createInfo.lpStartAddress,
                             &pProcInfo->byOriginalEntryPointOpcode,
                             sizeof(pProcInfo->byOriginalEntryPointOpcode));
}

bool CProcessDLLInject::SaveEntryPointContext(tProcInfo *pProcInfo)
{
    if (pProcInfo->bIsNative)
    {
        CONTEXT *pContext = &pProcInfo->origThreadContext;

        pContext->ContextFlags = CONTEXT_FULL;

        if (!GetThreadContext(pProcInfo->createInfo.hThread, pContext))
        {
            DbgOut(L"GetThreadContext failed (%u)", GetLastError());
            return false;
        }

        // The EIP/RIP in the context structure points past the BP, so decrement EIP/RIP to point
        // at the original instruction.
        CONTEXT_IP(pContext) -= sizeof(OPCODE_INT3);
    }
    else
    {
        WOW64_CONTEXT *pContext = &pProcInfo->origThreadWow64Context;

        pContext->ContextFlags = CONTEXT_FULL;

        if (!Wow64GetThreadContext(pProcInfo->createInfo.hThread, pContext))
        {
            DbgOut(L"Wow64GetThreadContext failed (%u)", GetLastError());
            return false;
        }

        // Same comment as above
        pContext->Eip -= sizeof(OPCODE_INT3);
    }

    return true;
}

bool CProcessDLLInject::SaveBaseAddressAndRestoreEntryPointContext(tProcInfo *pProcInfo)
{
    /*
     * Save the base address of the injected DLL by reading the return value of the LoadLibrary()
     * call from EAX/RAX and set the registers back to what they were before we redirected them to
     * the LoadLibrary stub.
     */

    if (pProcInfo->bIsNative)
    {
        CONTEXT context = {};

        context.ContextFlags = CONTEXT_FULL;

        if (GetThreadContext(pProcInfo->createInfo.hThread, &context))
        {
            pProcInfo->pInjectedDllBaseInTarget = (PVOID)CONTEXT_AX(&context);
        }
        else
        {
            DbgOut(L"GetThreadContext failed (%u)", GetLastError());
        }

        if (!SetThreadContext(pProcInfo->createInfo.hThread,
                              &pProcInfo->origThreadContext))
        {
            DbgOut(L"SetThreadContext failed (%u)", GetLastError());
            return false;
        }
    }
    else
    {
        WOW64_CONTEXT context = {};

        context.ContextFlags = CONTEXT_FULL;

        if (Wow64GetThreadContext(pProcInfo->createInfo.hThread, &context))
        {
            pProcInfo->pInjectedDllBaseInTarget = (PVOID)(DWORD_PTR)context.Eax;
        }
        else
        {
            DbgOut(L"GetThreadContext failed (%u)", GetLastError());
        }

        if (!Wow64SetThreadContext(pProcInfo->createInfo.hThread,
                                   &pProcInfo->origThreadWow64Context))
        {
            DbgOut(L"Wow64SetThreadContext failed (%u)", GetLastError());
            return false;
        }
    }

    DbgOut(L"Base address of injected DLL: 0x%p", pProcInfo->pInjectedDllBaseInTarget);

    return true;
}

bool CProcessDLLInject::FreeStub(tProcInfo *pProcInfo)
{
    if (!pProcInfo->pStubInTarget)
        return true;

    if (!VirtualFreeEx(pProcInfo->createInfo.hProcess, pProcInfo->pStubInTarget, 0, MEM_RELEASE))
    {
        DbgOut(L"VirtualFreeEx failed (%u)", GetLastError());
        return false;
    }

    pProcInfo->pStubInTarget = NULL;
    
    return true;
}

bool CProcessDLLInject::InjectCode(tProcInfo *pProcInfo)
{
    tLoadLibraryStub32  stub32              = {};
    tLoadLibraryStub64  stub64              = {};
    size_t              stubSize            = 0;
    PVOID               pStub               = NULL;
    PVOID               pStubInTarget       = NULL;
    PVOID               pStubInTargetBP     = NULL;
    CONTEXT             stubContext         = pProcInfo->origThreadContext;
    WOW64_CONTEXT       stubWow64Context    = pProcInfo->origThreadWow64Context;
    bool                bResult             = false;

    if (pProcInfo->bIs32Bit)
    {
        pStub       = &stub32;
        stubSize    = sizeof(stub32);
    }
    else
    {
        pStub       = &stub64;
        stubSize    = sizeof(stub64);
    }

    // Locate where the stub will be in the target process
    pStubInTarget = VirtualAllocEx(pProcInfo->createInfo.hProcess,
                                   NULL,
                                   stubSize,
                                   MEM_COMMIT,
                                   PAGE_EXECUTE_READWRITE);
    if (!pStubInTarget)
    {
        DbgOut(L"VirtualAllocEx failed (%u)", GetLastError());
        goto exit;
    }

    // Initialize the stub
    if (pProcInfo->bIs32Bit)
    {
        if (!InitLoadLibraryStub32(&stub32,
                                   pStubInTarget,
                                   m_strDLLFilename32.c_str(),
                                   pProcInfo->bIsNative ? m_pNativeLoadLibraryW :
                                                          m_pWow64LoadLibraryW,
                                   &pStubInTargetBP))
        {
            DbgOut(L"Failed to initialize 32-bit inject code");
            goto exit;
        }
    }
    else
    {
        if (!InitLoadLibraryStub64(&stub64,
                                   pStubInTarget,
                                   m_strDLLFilename64.c_str(),
                                   m_pNativeLoadLibraryW,
                                   &pStubInTargetBP))
        {
            DbgOut(L"Failed to initialize 64-bit inject code");
            goto exit;
        }
    }

    // Copy the stub into the target process
    if (!WriteTargetMemory(pProcInfo->createInfo.hProcess, pStubInTarget, pStub, stubSize))
    {
        DbgOut(L"Failed to write inject code to target process (%u)", GetLastError());
        goto exit;
    }

    // Change the EIP/RIP register in the target thread to point at the stub we just copied in
    if (pProcInfo->bIsNative)
    {
        CONTEXT_IP(&stubContext) = (DWORD64)pStubInTarget;

        if (!SetThreadContext(pProcInfo->createInfo.hThread, &stubContext))
        {
            DbgOut(L"Failed to modify thread context to point to injected code (%u)",
                   GetLastError());
            goto exit;
        }
    }
    else
    {
        if (HIDWORD((DWORD64)pStubInTarget) != 0)
        {
            DbgOut(L"Upper 32 bits of stub address in target process are nonzero (0x%p)",
                   pStubInTarget);
            goto exit;
        }

        stubWow64Context.Eip = LODWORD((DWORD64)pStubInTarget);

        if (!Wow64SetThreadContext(pProcInfo->createInfo.hThread, &stubWow64Context))
        {
            DbgOut(L"Failed to modify Wow64 thread context to point to injected code (%u)",
                   GetLastError());
            goto exit;
        }
    }

    pProcInfo->pStubInTarget    = pStubInTarget;
    pProcInfo->pStubInTargetBP  = pStubInTargetBP;
    pStubInTarget = NULL; // will be freed later in FreeStub()

    bResult = true;

exit:

    if (pStubInTarget)
    {
        VirtualFreeEx(pProcInfo->createInfo.hProcess, pStubInTarget, 0, MEM_RELEASE);
        pStubInTarget = NULL;
    }

    return bResult;
}

bool CProcessDLLInject::OnProcessCreate(DWORD                       dwProcessID,
                                        DWORD                       dwThreadID,
                                        CREATE_PROCESS_DEBUG_INFO  *pInfo)
{
    m_procInfo.createInfo = *pInfo;

    if (m_bIs64BitWindows)
    {
        BOOL bIsWow64 = FALSE;

        IsWow64Process(pInfo->hProcess, &bIsWow64);

        m_procInfo.bIs32Bit     = bIsWow64;
        m_procInfo.bIsNative    = IS_NATIVE_DEBUGGEE_PROCESS(bIsWow64);

        /* 
         * Prevent debugging a 64-bit process by a 32-bit debugger. This should be already blocked
         * by CreateProcess (ERROR_NOT_SUPPORTED), but checking here anyway just to be sure.
         */
#if !defined(_WIN64)
        if (!m_procInfo.bIs32Bit)
        {
            DbgOut(L"Cannot debug a 64-bit process with a 32-bit debugger");
            return false;
        }
#endif

        if (!m_procInfo.bIsNative && !m_pWow64LoadLibraryW)
        {
            m_pWow64LoadLibraryW = Wow64GetKnownDllProcAddress(g_szKernel32, "LoadLibraryW");
            DbgOut(L"pWow64LoadLibraryW: 0x%p", m_pWow64LoadLibraryW); // TODO: remove
            if (!m_pWow64LoadLibraryW)
            {
                DbgOut(L"Failed to get Wow64 LoadLibraryW address");
                return false;
            }
        }
    }
    else
    {
        // 32-bit Windows, no other possibility than 32-bit process/native debugging
        m_procInfo.bIs32Bit     = true;
        m_procInfo.bIsNative    = true;
    }

    DbgOut(L"Debugging process with ID %u (%hs, %hs bitness)",
           dwProcessID,
           m_procInfo.bIs32Bit ? "32-bit" : "64-bit",
           m_procInfo.bIsNative ? "native" : "non-native");

    return true;
}

void CProcessDLLInject::OnException(DWORD                           dwProcessID,
                                    DWORD                           dwThreadID,
                                    EXCEPTION_DEBUG_INFO           *pInfo,
                                    bool                           *pbExceptionHandled)
{
    *pbExceptionHandled = false;

    // When checking for breakpoint exceptions, we need to be careful which type to handle
    // depending on native/non-native bitness
    if (m_procInfo.bIsNative)
    {
        if (pInfo->ExceptionRecord.ExceptionCode == EXCEPTION_BREAKPOINT)
        {
            OnBreakpoint(dwProcessID, dwThreadID, pInfo);
            return;
        }
    }
    else
    {
        if (pInfo->ExceptionRecord.ExceptionCode == STATUS_WX86_BREAKPOINT)
        {
            OnBreakpoint(dwProcessID, dwThreadID, pInfo);
            return;
        }
    }
}

void CProcessDLLInject::OnBreakpoint(DWORD                          dwProcessID,
                                     DWORD                          dwThreadID,
                                     EXCEPTION_DEBUG_INFO          *pInfo)
{
    if (!m_procInfo.bInjected)
    {
        if (!m_procInfo.bFirstBPHit)
        {
            m_procInfo.bFirstBPHit = true;

            if (!SetEntryPointBP(&m_procInfo))
            {
                DbgOut(L"Failed to initialize hook");
            }
        }
        else if (pInfo->ExceptionRecord.ExceptionAddress == m_procInfo.createInfo.lpStartAddress)
        {
            DbgOut(L"Process entry point hit");

            if (!RemoveEntryPointBP(&m_procInfo))
            {
                DbgOut(L"Failed to remove entry point breakpoint");
                return;
            }

            if (!SaveEntryPointContext(&m_procInfo))
            {
                DbgOut(L"Failed to save entry point context");
                return;
            }

            if (!InjectCode(&m_procInfo))
            {
                DbgOut(L"Failed to inject code");
                return;
            }

#if defined(SUSPEND_CHILD_PROCESS)
            SuspendThread(pProcInfo->createInfo.hThread);
#endif
        }
        else if (pInfo->ExceptionRecord.ExceptionAddress == m_procInfo.pStubInTargetBP)
        {
            DbgOut(L"Stub breakpoint hit");

            if (!SaveBaseAddressAndRestoreEntryPointContext(&m_procInfo))
            {
                DbgOut(L"Failed to restore entry point context");
            }

            FreeStub(&m_procInfo);

            m_procInfo.bInjected = true;
        }
    }
    else
    {
        DbgOut(L"Breakpoint during regular execution at address 0x%p",
               pInfo->ExceptionRecord.ExceptionAddress);

        // Ignore
    }
}
