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
 * Utility definitions related to delay-loaded DLLs
 *
 ****************************************************************************/

#pragma once

#include <Windows.h>
#include <delayloadhandler.h>

typedef PVOID (WINAPI *PRESOLVE_DELAY_LOADED_API) (
        PVOID                             ParentModuleBase,
        PCIMAGE_DELAYLOAD_DESCRIPTOR      DelayloadDescriptor,
        PDELAYLOAD_FAILURE_DLL_CALLBACK   FailureDllHook,
        PVOID                             FailureSystemHook,
        PIMAGE_THUNK_DATA                 ThunkAddress,
        ULONG                             Flags);

extern const LPCSTR g_szDelayLoadApiSetName;
extern const LPCSTR g_szResolveDelayLoadedAPIName;

bool CheckResolveDelayLoadedAPIResult(PVOID                             ParentModuleBase,
                                      PCIMAGE_DELAYLOAD_DESCRIPTOR      DelayloadDescriptor,
                                      PIMAGE_THUNK_DATA                 ThunkAddress);
