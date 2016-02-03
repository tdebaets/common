(****************************************************************************
 *
 * Copyright 2016 Tim De Baets
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
 * Portable Executable format structure declarations
 *
 ****************************************************************************)

unit PEStruct;

interface

uses Windows, TlHelp32;

const
  IMAGE_DIRECTORY_ENTRY_EXPORT = 0; { Export Directory }
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1; { Import Directory }
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2; { Resource Directory }
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; { Exception Directory }
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4; { Security Directory }
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; { Base Relocation Table }
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6; { Debug Directory }
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7; { Description String }
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8; { Machine Value (MIPS GP) }
  IMAGE_DIRECTORY_ENTRY_TLS   = 9; { TLS Directory }
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10; { Load Configuration Directory }
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11; { Bound Import Directory in headers }
  IMAGE_DIRECTORY_ENTRY_IAT   = 12; { Import Address Table }

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

const
  IMAGE_ORDINAL_FLAG = DWORD($80000000);

type
  { Image format }
  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record
    e_magic: Word; // Magic number
    e_cblp: Word; // Bytes on last page of file
    e_cp: Word; // Pages in file
    e_crlc: Word; // Relocations
    e_cparhdr: Word; // Size of header in paragraphs
    e_minalloc: Word; // Minimum extra paragraphs needed
    e_maxalloc: Word; // Maximum extra paragraphs needed
    e_ss: Word; // Initial (relative) SS value
    e_sp: Word; // Initial SP value
    e_csum: Word; // Checksum
    e_ip: Word; // Initial IP value
    e_cs: Word; // Initial (relative) CS value
    e_lfarlc: Word; // File address of relocation table
    e_ovno: Word; // Overlay number
    e_res: array[0..3] of Word; // Reserved words
    e_oemid: Word; // OEM identifier (for e_oeminfo)
    e_oeminfo: Word; // OEM information; e_oemid specific
    e_res2: array[0..9] of Word; // Reserved words
    e_lfanew: Longint; // File address of new exe header
  end;

  PImageImportByName = ^TImageImportByName;
  TImageImportByName = packed record
    Hint: Word;
    Name: array[0..0] of Char;
  end;

  PImageThunkData = ^TImageThunkData;
  TImageThunkData = packed record
    case Integer of
      0: (ForwarderString: PByte);
      1: (_Function: PDWORD);
      2: (Ordinal: DWORD);
      3: (AddressOfData: PImageImportByName);
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    Union: record
      case Integer of
        0: (
             Characteristics: DWORD; // 0 for terminating null import descriptor
           );
        1: (
             OriginalFirstThunk: PImageThunkData; // RVA to original unbound IAT
           );
    end;

    TimeDateStamp: DWORD; // 0 if not bound,
                          // -1 if bound, and real date\time stamp
                          //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                          // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWORD; // -1 if no forwarders
    Name: Pointer;
    FirstThunk: PImageThunkData; // RVA to IAT (if bound this IAT has actual addresses)
  end;

  PImageDelayLoadDescriptor = ^TImageDelayLoadDescriptor;
  TImageDelayLoadDescriptor = packed record
    Attributes: record
      case Integer of
        0: (
          AllAttributes: DWORD;
        );
        {1: (
          DWORD RvaBased : 1;             // Delay load version 2
          DWORD ReservedAttributes : 31;
        );}
    end;

    DllNameRVA: Pointer;                  // RVA to the name of the target library (NULL-terminate ASCII string)
    ModuleHandleRVA: Pointer;             // RVA to the HMODULE caching location (PHMODULE)
    ImportAddressTableRVA: Pointer;       // RVA to the start of the IAT (PIMAGE_THUNK_DATA)
    ImportNameTableRVA: Pointer;          // RVA to the start of the name table (PIMAGE_THUNK_DATA::AddressOfData)
    BoundImportAddressTableRVA: Pointer;  // RVA to an optional bound IAT
    UnloadInformationTableRVA: Pointer;   // RVA to an optional unload info table
    TimeDateStamp: Pointer;               // 0 if not bound,
                                          // Otherwise, date/time of the target DLL
  end;

function RVAToAbsolute(BaseAddress, Address: Pointer): Pointer;

implementation

function RVAToAbsolute(BaseAddress, Address: Pointer): Pointer;
begin
  Result := Pointer(Cardinal(BaseAddress) + Cardinal(Address));
end;

end.
 