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

#pragma once

#include "ProcessDebug.h"

#include <stdexcept>
#include <string>

using namespace std;

typedef struct tProcInfo
{
    DWORD                       dwProcessID;
    bool                        bIs32Bit;
    bool                        bIsNative; // false when we're 64-bit and debugging a WOW64 process
    bool                        bInjected;
    bool                        bFirstBPHit;
    CREATE_PROCESS_DEBUG_INFO   createInfo;
    BYTE                        byOriginalEntryPointOpcode;
    CONTEXT                     origThreadContext;      // used when bIsNative == true
    WOW64_CONTEXT               origThreadWow64Context; // used when bIsNative == false
    PVOID                       pStubInTarget;
    PVOID                       pStubInTargetBP;
    PVOID                       pInjectedDllBaseInTarget;
} tProcInfo;

/*
 * 32-bit and 64-bit assembly stubs for injecting our DLL into WerFault.exe.
 * Note about the padding fields: LdrLoadDll() internally calls LdrpLogInternal(..., "%wZ\n",
 * DllName) which in its turn calls _vsnprintf()->_vsnprintf_l()->output_l(). The last function
 * expects DllName to be 2-byte aligned and calls the C Runtime Library invalid parameter handler
 * (_invalid_parameter) if it isn't. So in both these stubs, the data_DllName field must be 2-byte
 * aligned.
 */

#pragma pack(push, 1)
typedef struct tLoadLibraryStub32
{
    BYTE    instr_PUSH;
    DWORD   operand_PUSH_value;

    BYTE    instr_MOV_EAX;
    DWORD   operand_MOV_EAX;

    WORD    instr_CALL_EAX;

    BYTE    instr_INT_3;

    BYTE    padding;

    WCHAR   data_DllName[512];
} tLoadLibraryStub32;

typedef struct tLoadLibraryStub64
{
    BYTE    prefix_SUB_RSP;
    WORD    instr_SUB_RSP;
    BYTE    operand_SUB_RSP;

    BYTE    prefix_MOV_RCX;
    BYTE    instr_MOV_RCX;
    PVOID64 operand_MOV_RCX;

    BYTE    prefix_MOV_RAX;
    BYTE    instr_MOV_RAX;
    PVOID64 operand_MOV_RAX;
    
    WORD    instr_CALL_RAX;

    BYTE    instr_INT_3;

    BYTE    padding;

    WCHAR   data_DllName[512];
} tLoadLibraryStub64;
#pragma pack(pop)

bool ReadTargetMemory(HANDLE hProc, PVOID pAddress, PVOID pBuf, SIZE_T bufSize);
bool ReadProcessString(HANDLE   hProc,
                       bool     bUnicode,
                       PVOID    pAddress,
                       DWORD    dwLength,
                       wstring &refString);
bool WriteTargetMemory(HANDLE hProc, PVOID pAddress, PVOID pBuf, SIZE_T bufSize);
bool WriteTargetByte(HANDLE hProc, PVOID pAddress, BYTE byte);

class CProcessDLLInjectError : public runtime_error
{
public:
    CProcessDLLInjectError(const char* _Message) : runtime_error(_Message) {}
};

class CProcessDLLInject : public CProcessDebug
{
public:

    CProcessDLLInject(const wstring &strDLLFilename32, const wstring &strDLLFilename64);

protected:

    /* CProcessDebug */

    virtual bool OnProcessCreate(DWORD                      dwProcessID,
                                 DWORD                      dwThreadID,
                                 CREATE_PROCESS_DEBUG_INFO *pInfo);

    virtual void OnException(DWORD                          dwProcessID,
                             DWORD                          dwThreadID,
                             EXCEPTION_DEBUG_INFO          *pInfo,
                             bool                          *pbExceptionHandled);

    virtual void OnDebugString(DWORD                        dwProcessID,
                               DWORD                        dwThreadID,
                               OUTPUT_DEBUG_STRING_INFO    *pInfo);

    /* CProcessDLLInject */

    virtual void OnBreakpoint(const tProcInfo              *pProcInfo,
                              DWORD                         dwThreadID,
                              const EXCEPTION_DEBUG_INFO   *pInfo);

    virtual void OnException2(const tProcInfo              *pProcInfo,
                              DWORD                         dwThreadID,
                              EXCEPTION_DEBUG_INFO         *pInfo,
                              bool                         *pbExceptionHandled);

    virtual void OnDebugString2(const tProcInfo            *pProcInfo,
                                DWORD                       dwThreadID,
                                OUTPUT_DEBUG_STRING_INFO   *pInfo,
                                LPCWSTR                     wszDebugString);

private:

    bool InitLoadLibraryStub32(tLoadLibraryStub32  *pStub,              /* OUT */
                               PVOID                pStubInTarget,
                               const WCHAR         *dllName,
                               FARPROC              pLoadLibraryAddr,
                               PVOID               *ppStubInTargetBP    /* OUT */);

    bool InitLoadLibraryStub64(tLoadLibraryStub64  *pStub,              /* OUT */
                               PVOID                pStubInTarget,
                               const WCHAR         *dllName,
                               FARPROC              pLoadLibraryAddr,
                               PVOID               *ppStubInTargetBP    /* OUT */);

    bool GetNativeLoadLibraryAddress();

    bool SetEntryPointBP(tProcInfo *pProcInfo);

    bool RemoveEntryPointBP(tProcInfo *pProcInfo);

    bool SaveEntryPointContext(tProcInfo *pProcInfo);

    bool SaveBaseAddressAndRestoreEntryPointContext(tProcInfo *pProcInfo);

    bool FreeStub(tProcInfo *pProcInfo);

    bool InjectCode(tProcInfo *pProcInfo);

private:

    wstring     m_strDLLFilename32;
    wstring     m_strDLLFilename64;
    tProcInfo   m_procInfo              = {};
    FARPROC     m_pNativeLoadLibraryW   = NULL;
    FARPROC     m_pWow64LoadLibraryW    = NULL;
    bool        m_bIs64BitWindows       = false;

};
