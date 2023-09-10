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
 * Process-related utility definitions
 *
 ****************************************************************************/

#pragma once

#include <vector>
#include <Utils.h>
#include <Windows.h>

#include "NativeApi.h"

typedef std::vector<BYTE> ByteVector;

// TODO: use a strong typedef instead and define operators for casting to and assigning from PSID
typedef ByteVector SidWrapper;

bool GetProcessSidToken(HANDLE hProc, SidWrapper& refSid);
bool GetProcessSid(DWORD dwProcId, HANDLE hProc, SidWrapper& refSid);
bool IsSystemSid(PSID pSid);

NTSTATUS PhEnumHandlesEx2(_In_  HANDLE                                  ProcessHandle,
                          _Out_ PPROCESS_HANDLE_SNAPSHOT_INFORMATION   *pHandles);
