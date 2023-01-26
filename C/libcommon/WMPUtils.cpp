/****************************************************************************
 *
 * Copyright 2022 Tim De Baets
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
 * Windows Media Player utility code
 *
 ****************************************************************************/

#include "WMPUtils.h"

static const LPCSTR g_szWMPDLL = "wmp.dll";

BYTE GetWMPMainVersion(CComBSTR &pbstrVersionInfo)
{
    wstring str(pbstrVersionInfo);
    size_t  dotIdx;

    dotIdx = str.find('.');

    if (dotIdx == wstring::npos)
        return 0;

    str.erase(dotIdx, wstring::npos);

    return _wtoi(str.c_str());
}