/****************************************************************************
 *
 * Copyright 2022-2023 Tim De Baets
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
 * Windows Media Player utility definitions
 *
 ****************************************************************************/

#pragma once

#include <string>
#include <windows.h>

#include "atlbase.h" // CComBSTR

using namespace std;

#define WMP_VERSION_11  (11)
#define WMP_VERSION_12  (12)

extern const LPCWSTR g_wszWMPDLL;

BYTE GetWMPMainVersion(CComBSTR &pbstrVersionInfo);
