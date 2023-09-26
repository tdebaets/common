/****************************************************************************
 *
 * Copyright 2023 Tim De Baets
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
 * Windows NT status definitions
 *
 * These definitions are missing from the Windows SDK (at least from version
 * 6.1 of the SDK), see
 * http://rogerkar.blogspot.com/2009/06/nterror-ntsuccess-identifier-not-found.html
 *
 ****************************************************************************/

#pragma once

/**
* @author Roger Karlsson
* @since 2009-03-13
*/

#ifndef NT_SUCCESS
#define NT_SUCCESS(Status) ((NTSTATUS)(Status) >= 0)
#endif

#ifndef NT_ERROR
#define NT_ERROR(Status) ((ULONG)(Status) >> 30 == 3)
#endif
