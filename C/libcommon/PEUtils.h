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
 * Utility definitions related to the Portable Executable format
 *
 ****************************************************************************/

#pragma once

#include <Windows.h>

bool PatchImport(PIMAGE_THUNK_DATA  pAddrThunk,
                 PVOID              pNewProc,
                 PVOID             *ppPrevProc);
PVOID RVAToAbsolute(PVOID pBase, DWORD_PTR dwRVA);
DWORD GetProcRVA(PVOID pBaseAddress, LPCSTR lpProcName, LPCSTR *ppForward);
