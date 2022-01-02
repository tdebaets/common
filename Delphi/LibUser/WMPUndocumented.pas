(****************************************************************************
 *
 * Copyright 2017-2018 Tim De Baets
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
 * Undocumented Windows Media Player interfaces
 * Most of these were collected from OllyDbg's disassembly of wmp.dll,
 * together with Microsoft's public debugging symbols.
 *
 ****************************************************************************)

unit WMPUndocumented;

interface

uses Windows, ActiveX, WMPLib_TLB;

const
  WMPServiceType_RemoteDeskband = 'RemoteDeskband';

type
  IWMPItemData = interface(IUnknown)
    ['{6eca1ace-b012-4aa0-83a1-c7fae0c3d1be}']
    function GetAttributeFlags: HResult; stdcall; // prototype not known - do not use
    function GetCountForAttribute: HResult; stdcall; // prototype not known - do not use
    function GetAttribute(dwFlags: Integer; const bstrItemName: WideString;
        const bstrLanguage: WideString; dwIndex: Integer;
        var pvarValue: OleVariant): HResult; stdcall;
    function GetAttributeByAtom: HResult; stdcall; // prototype not known - do not use
    function SetAttribute(dwFlags: Integer; const bstrItemName: WideString;
        const bstrLanguage: WideString; dwIndex: Integer;
        const pvarValue: OleVariant): HResult; stdcall;
    function DeleteAttribute(dwFlags: Integer; const bstrItemName: WideString;
        const bstrLanguage: WideString; dwIndex: Integer): HResult; stdcall;
    function InsertAttribute(dwFlags: Integer; const bstrItemName: WideString;
        const bstrLanguage: WideString; dwIndex: Integer;
        const pvarValue: OleVariant): HResult; stdcall;
    function GetAttributeNameCount: HResult; stdcall; // prototype not known - do not use
    function GetAttributeNameByIndex: HResult; stdcall; // prototype not known - do not use
    function Flush: HResult; stdcall;
  end;

function WMPSetItemInfoByIndex(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer;
    const pvarValue: OleVariant): HResult;
function WMPInsertAttribute(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer;
    const pvarValue: OleVariant): HResult;
function WMPDeleteItemInfoByIndex(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer; SaveToFile: Boolean): HResult;

type
  IWMPSDKItemData = interface(IUnknown)
    ['{b253eed3-397b-4394-8f64-bbb4eced8295}']

  end;

type
  TFakeWString = packed record
    Str: PWideChar;
    Unknown1: Integer;
    AllocationType: Byte;
    Unknown2: array[0..14] of Byte;
  end;
  PFakeWString = ^TFakeWString;

type
  IWMPPlaylistInternal = interface(IUnknown)
    ['{6ab1544b-45df-11d3-b155-00c04f79faa6}']
    function GetType(pType: PInteger): HResult; stdcall;
    function CoerceType(Typ: Integer): HResult; stdcall;
    function CanChangeToBaseType: HResult; stdcall; // prototype not known - do not use
    function CanSaveToLibrary: LongBool; stdcall;
    function GetIdentifier: HResult; stdcall; // prototype not known - do not use
    function FindChildMediaByIdentifier: HResult; stdcall; // prototype not known - do not use
    function SetItemInfoInternal: HResult; stdcall; // prototype not known - do not use
    function ClearInternal: HResult; stdcall; // prototype not known - do not use
    function AppendItemInternal: HResult; stdcall; // prototype not known - do not use
    function RemoveItemInternal: HResult; stdcall; // prototype not known - do not use
    function GetURL(URL: PFakeWString): HResult; stdcall;
    function SetURL: HResult; stdcall; // prototype not known - do not use
    function ExportToFile: HResult; stdcall; // prototype not known - do not use
    function SetDirty(Dirty: LongBool): HResult; stdcall;
    function GetSortOrder: HResult; stdcall; // prototype not known - do not use
    function SetSortOrder: HResult; stdcall; // prototype not known - do not use
    function SortPlaylist(AttributeName: PWideChar; Flags: Integer): HResult; stdcall;
    function SetPlaylistCache: HResult; stdcall; // prototype not known - do not use
    function SetEventProvider: HResult; stdcall; // prototype not known - do not use
    function ExternalMediaChangeNotification: HResult; stdcall; // prototype not known - do not use
    function IOleObject_Close: HResult; stdcall; // prototype not known - do not use
    function IOleObject_Close2: HResult; stdcall; // prototype not known - do not use
    function InsertMediaSource: HResult; stdcall; // prototype not known - do not use
    function AppendMediaSource: HResult; stdcall; // prototype not known - do not use
    function ItemWasCreated: HResult; stdcall; // prototype not known - do not use
    function Advise: HResult; stdcall; // prototype not known - do not use
    function UnAdvise: HResult; stdcall; // prototype not known - do not use
    function FreezeEvents: HResult; stdcall; // prototype not known - do not use
    function UnFreezeEvents: HResult; stdcall; // prototype not known - do not use
    function UnFreezeEventsWithoutNotifications: HResult; stdcall; // prototype not known - do not use
    function FirePlaylistChanged: HResult; stdcall; // prototype not known - do not use
    function GetCopy: HResult; stdcall; // prototype not known - do not use
    function GetAlternateHandlerCopy: HResult; stdcall; // prototype not known - do not use
    function GetParentMedia: HResult; stdcall; // prototype not known - do not use
    function SetParentMedia: HResult; stdcall; // prototype not known - do not use
    function GetParentPlaylist: HResult; stdcall; // prototype not known - do not use
    function GetRootPlaylist: HResult; stdcall; // prototype not known - do not use
    function GetReservedItem: HResult; stdcall; // prototype not known - do not use
    function SetReservedItem: HResult; stdcall; // prototype not known - do not use
    function GetEventPlaylistCount: HResult; stdcall; // prototype not known - do not use
    function CreateEventPlaylist: HResult; stdcall; // prototype not known - do not use
    function CanMorphIntoEventPlaylist: HResult; stdcall; // prototype not known - do not use
    function MorphIntoEventPlaylist: HResult; stdcall; // prototype not known - do not use
    function RestoreMorph: HResult; stdcall; // prototype not known - do not use
    function GetDuration: HResult; stdcall; // prototype not known - do not use
    function GetRepeatStartEndMedia: HResult; stdcall; // prototype not known - do not use
    function SetRepeatStartEndMedia: HResult; stdcall; // prototype not known - do not use
    function GetBindInfo: HResult; stdcall; // prototype not known - do not use
    function GetBindInfo2: HResult; stdcall; // prototype not known - do not use
    function GetLoopsRemain: HResult; stdcall; // prototype not known - do not use
    function SetLoopsRemain: HResult; stdcall; // prototype not known - do not use
    function DecrementLoopsRemain: HResult; stdcall; // prototype not known - do not use
    function ResetLoopsRemain: HResult; stdcall; // prototype not known - do not use
    function GetRepeatCount: HResult; stdcall; // prototype not known - do not use
    function SetRepeatCount: HResult; stdcall; // prototype not known - do not use
    function GetDownloadItemCount: HResult; stdcall; // prototype not known - do not use
    function SetDownloadItemCount: HResult; stdcall; // prototype not known - do not use
    function GetCurrentMediaResumePosAfterUnMorph: HResult; stdcall; // prototype not known - do not use
    function SetCurrentMediaResumePosAfterUnMorph: HResult; stdcall; // prototype not known - do not use
    function GetResumeMediaAfterUnMorph: HResult; stdcall; // prototype not known - do not use
    function SetResumeMediaAfterUnMorph: HResult; stdcall; // prototype not known - do not use
    function putDocumentLocator: HResult; stdcall; // prototype not known - do not use
    function Cap_IsIntrinsicDelete: HResult; stdcall; // prototype not known - do not use
    function Cap_CanMove: HResult; stdcall; // prototype not known - do not use
    function Cap_CanDetach: HResult; stdcall; // prototype not known - do not use
    function Cap_CanDelete: HResult; stdcall; // prototype not known - do not use
    function Cap_CanAccept: HResult; stdcall; // prototype not known - do not use
    function IOleObject_Close3: HResult; stdcall; // prototype not known - do not use
    function Cap_CanDelete2: HResult; stdcall; // prototype not known - do not use
    function Cap_CanEdit: HResult; stdcall; // prototype not known - do not use
    function Cap_CanShuffle: LongBool; stdcall;
    {CWMPPlaylist::Cap_CanShowExpanded
    CWMPPlaylist::FindMedia
    CWMPPlaylist::ClearMediaErrors
    CWMPPlaylist::ClearMediaErrorsWithEvent
    CWMPPlaylist::IncrementErrorCount
    CWMPPlaylist::DecrementErrorCount
    CWMPPlaylist::GetMediaNode
    CHMEProvider::SetAttribute
    CWMPPlaylist::FireDelayedMorphEvent
    CWMPPlaylist::RebuildFromPlaylist
    CWMPPlaylist::SetSavedPlaylistCopy
    CWMPPlaylist::GetSavedPlaylistCopy
    CWMPPlaylist::UpdatePlayCount
    CWMPPlaylist::OnDelayedAddToLibrary
    CWMPPlaylist::IsDeviceBusyPlaylist
    CWMPPlaylist::GetFriendlyName
    CWMPPlaylist::DelayCreateRemove
    CWMPPlaylist::DelayCreateInsert
    CWMPPlaylist::DelayCreateMove
    CWMPPlaylist::DelayCreateMetadataChange
    CWMPPlaylist::DelayCreateBeginBulkEvents
    CWMPPlaylist::DelayCreateEndBulkEvents
    CWMPPlaylist::GetList
    CWMPPlaylist::MediaChangeNotify
    ATL::CComObject<CWMPDvdChapterPlaylist::QueryInterface
    ATL::CComObject<CWMPDvdChapterPlaylist::AddRef
    ATL::CComObject<CWMPVideoCDPlaylist::Release
    CWMPDvdMedia::GetPreferredRuntime
    CWMPPlaylist::CoerceType
    CWMPPlaylist::CanChangeToBaseType
    CWMPPlaylist::CanSaveToLibrary
    CWMPPlaylist::GetIdentifier
    CWMPPlaylist::FindChildMediaByIdentifier
    CWMPPlaylist::SetItemInfoInternal
    CWMPPlaylist::ClearInternal
    CWMPPlaylist::AppendItemInternal
    CWMPPlaylist::RemoveItemInternal
    CWMPPlaylist::GetURL
    CWMPPlaylist::SetURL
    CWMPPlaylist::ExportToFile
    CWMPPlaylist::SetDirty
    CWMPPlaylist::GetSortOrder
    CWMPPlaylist::SetSortOrder
    CWMPPlaylist::SortPlaylist
    CWMPPlaylist::SetPlaylistCache
    CWMPPlaylist::SetEventProvider
    CWMPPlaylist::ExternalMediaChangeNotification
    CWMPOleSupport::IOleObject_Close
    CWMPOleSupport::IOleObject_Close
    CWMPPlaylist::InsertMediaSource
    CWMPPlaylist::AppendMediaSource
    CWMPPlaylist::ItemWasCreated
    CWMPPlaylist::Advise
    CWMPPlaylist::UnAdvise
    CWMPPlaylist::FreezeEvents
    CWMPPlaylist::UnFreezeEvents
    CWMPPlaylist::UnFreezeEventsWithoutNotifications
    CWMPPlaylist::FirePlaylistChanged
    CWMPPlaylist::GetCopy
    CWMPPlaylist::GetAlternateHandlerCopy
    CWMPPlaylist::GetParentMedia
    CWMPPlaylist::SetParentMedia
    CWMPPlaylist::GetParentPlaylist
    CWMPPlaylist::GetRootPlaylist
    CWMPPlaylist::GetReservedItem
    CWMPPlaylist::SetReservedItem
    CWMPPlaylist::GetEventPlaylistCount
    CWMPPlaylist::CreateEventPlaylist
    CWMPPlaylist::CanMorphIntoEventPlaylist
    CWMPPlaylist::MorphIntoEventPlaylist
    CWMPPlaylist::RestoreMorph
    CWMPPlaylist::GetDuration
    CWMPPlaylist::GetRepeatStartEndMedia
    CWMPPlaylist::SetRepeatStartEndMedia
    COCXMoniker<COCXApplication__X_MPlayer2&CLSID_APPLICATION__X_M
    COCXMoniker<COCXApplication__X_MPlayer2&CLSID_APPLICATION__X_M
    CWMPPlaylist::GetLoopsRemain
    CWMPPlaylist::SetLoopsRemain
    CWMPPlaylist::DecrementLoopsRemain
    CWMPPlaylist::ResetLoopsRemain
    CWMPPlaylist::GetRepeatCount
    CWMPPlaylist::SetRepeatCount
    CWMPPlaylist::GetDownloadItemCount
    CWMPPlaylist::SetDownloadItemCount
    CWMPPlaylist::GetCurrentMediaResumePosAfterUnMorph
    CWMPPlaylist::SetCurrentMediaResumePosAfterUnMorph
    CWMPPlaylist::GetResumeMediaAfterUnMorph
    CWMPPlaylist::SetResumeMediaAfterUnMorph
    CWMPSAXContentHandler::putDocumentLocator
    CWMPPlaylist::Cap_IsIntrinsicDelete
    CWMPPlaylist::Cap_CanMove
    CWMPPlaylist::Cap_CanDetach
    CWMPPlaylist::Cap_CanDelete
    CWMPPlaylist::Cap_CanAccept
    CWMPOleSupport::IOleObject_Close
    CWMPPlaylist::Cap_CanDelete
    CWMPDvdChapterPlaylist::Cap_CanEdit
    CWMPPlaylist::Cap_CanShuffle
    CWMPPlaylist::Cap_CanShowExpanded
    CWMPPlaylist::FindMedia
    CWMPPlaylist::ClearMediaErrors
    CWMPPlaylist::ClearMediaErrorsWithEvent
    CWMPPlaylist::IncrementErrorCount
    CWMPPlaylist::DecrementErrorCount
    CWMPPlaylist::GetMediaNode
    CHMEProvider::SetAttribute
    CWMPPlaylist::FireDelayedMorphEvent
    CWMPPlaylist::RebuildFromPlaylist
    CWMPPlaylist::SetSavedPlaylistCopy
    CWMPPlaylist::GetSavedPlaylistCopy
    CWMPPlaylist::UpdatePlayCount
    CWMPPlaylist::OnDelayedAddToLibrary
    CWMPPlaylist::IsDeviceBusyPlaylist
    CWMPPlaylist::GetFriendlyName
    CWMPPlaylist::DelayCreateRemove
    CWMPPlaylist::DelayCreateInsert
    CWMPPlaylist::DelayCreateMove
    CWMPPlaylist::DelayCreateMetadataChange
    CWMPPlaylist::DelayCreateBeginBulkEvents
    CWMPPlaylist::DelayCreateEndBulkEvents
    CWMPPlaylist::GetList
    CWMPPlaylist::MediaChangeNotify
    ATL::CComObject<CWMPCdMediaStorePlaylist::QueryInterface
    ATL::CComObject<CWMPDvdChapterPlaylist::AddRef
    ATL::CComObject<CWMPVideoCDPlaylist::Release
    CWMPCdMediaStorePlaylist::GetType
    CWMPPlaylist::CoerceType
    CWMPPlaylist::CanChangeToBaseType
    CWMPPlaylist::CanSaveToLibrary
    CWMPCdMediaStorePlaylist::GetIdentifier
    CWMPPlaylist::FindChildMediaByIdentifier
    CWMPCdMediaStorePlaylist::SetItemInfoInternal
    CWMPPlaylist::ClearInternal
    CWMPPlaylist::AppendItemInternal
    CWMPPlaylist::RemoveItemInternal
    CWMPPlaylist::GetURL
    CWMPPlaylist::SetURL
    CWMPPlaylist::ExportToFile
    CWMPPlaylist::SetDirty
    CWMPPlaylist::GetSortOrder
    CWMPPlaylist::SetSortOrder
    CWMPPlaylist::SortPlaylist
    CWMPPlaylist::SetPlaylistCache
    CWMPPlaylist::SetEventProvider
    CWMPPlaylist::ExternalMediaChangeNotification
    CWMPCdMediaStorePlaylist::ExternalLoadNotification
    CWMPCdMediaStorePlaylist::ExternalPlayNotification
    CWMPPlaylist::InsertMediaSource
    CWMPPlaylist::AppendMediaSource
    CWMPPlaylist::ItemWasCreated
    CWMPPlaylist::Advise
    CWMPCdMediaStorePlaylist::UnAdvise
    CWMPPlaylist::FreezeEvents
    CWMPPlaylist::UnFreezeEvents
    CWMPPlaylist::UnFreezeEventsWithoutNotifications
    CWMPPlaylist::FirePlaylistChanged
    CWMPPlaylist::GetCopy
    CWMPPlaylist::GetAlternateHandlerCopy
    CWMPPlaylist::GetParentMedia
    CWMPPlaylist::SetParentMedia
    CWMPPlaylist::GetParentPlaylist
    CWMPPlaylist::GetRootPlaylist
    CWMPPlaylist::GetReservedItem
    CWMPPlaylist::SetReservedItem
    CWMPPlaylist::GetEventPlaylistCount
    CWMPPlaylist::CreateEventPlaylist
    CWMPPlaylist::CanMorphIntoEventPlaylist
    CWMPPlaylist::MorphIntoEventPlaylist
    CWMPPlaylist::RestoreMorph
    CWMPPlaylist::GetDuration
    CWMPPlaylist::GetRepeatStartEndMedia
    CWMPPlaylist::SetRepeatStartEndMedia
    COCXMoniker<COCXApplication__X_MPlayer2&CLSID_APPLICATION__X_M
    COCXMoniker<COCXApplication__X_MPlayer2&CLSID_APPLICATION__X_M
    CWMPPlaylist::GetLoopsRemain
    CWMPPlaylist::SetLoopsRemain
    CWMPPlaylist::DecrementLoopsRemain
    CWMPPlaylist::ResetLoopsRemain
    CWMPPlaylist::GetRepeatCount
    CWMPPlaylist::SetRepeatCount
    CWMPPlaylist::GetDownloadItemCount
    CWMPPlaylist::SetDownloadItemCount
    CWMPPlaylist::GetCurrentMediaResumePosAfterUnMorph
    CWMPPlaylist::SetCurrentMediaResumePosAfterUnMorph
    CWMPPlaylist::GetResumeMediaAfterUnMorph
    CWMPPlaylist::SetResumeMediaAfterUnMorph
    CWMPSAXContentHandler::putDocumentLocator
    CWMPPlaylist::Cap_IsIntrinsicDelete
    CWMPDvdChapterPlaylist::Cap_CanEdit
    CWMPOleSupport::IOleObject_Close
    CWMPOleSupport::IOleObject_Close
    CWMPOleSupport::IOleObject_Close
    CWMPDvdChapterPlaylist::Cap_CanEdit
    CWMPPlaylist::Cap_CanDelete
    CWMPDvdChapterPlaylist::Cap_CanEdit
    CWMPPlaylist::Cap_CanShuffle
    CWMPPlaylist::Cap_CanShowExpanded
    CWMPPlaylist::FindMedia
    CWMPPlaylist::ClearMediaErrors
    CWMPPlaylist::ClearMediaErrorsWithEvent
    CWMPPlaylist::IncrementErrorCount
    CWMPPlaylist::DecrementErrorCount
    CWMPCdMediaStorePlaylist::GetMediaNode
    CWMPCdMediaStorePlaylist::SetMediaNode
    CWMPPlaylist::FireDelayedMorphEvent
    CWMPPlaylist::RebuildFromPlaylist
    CWMPPlaylist::SetSavedPlaylistCopy
    CWMPPlaylist::GetSavedPlaylistCopy
    CWMPPlaylist::UpdatePlayCount
    CWMPPlaylist::OnDelayedAddToLibrary
    CWMPPlaylist::IsDeviceBusyPlaylist
    CWMPPlaylist::GetFriendlyName
    CWMPPlaylist::DelayCreateRemove
    CWMPPlaylist::DelayCreateInsert
    CWMPPlaylist::DelayCreateMove
    CWMPPlaylist::DelayCreateMetadataChange
    CWMPPlaylist::DelayCreateBeginBulkEvents
    CWMPPlaylist::DelayCreateEndBulkEvents
    CWMPPlaylist::GetList
    CWMPPlaylist::MediaChangeNotify}
  end;

  IWMPMediaInternal = interface(IUnknown)
    ['{967b4a95-7f50-11d3-b15e-00c04f79faa6}']
    function GetIndexInParentPlaylist: HResult; stdcall; // prototype not known - do not use
    function SetIndexInParentPlaylist: HResult; stdcall; // prototype not known - do not use
    function GetIndexInPlaylist: HResult; stdcall; // prototype not known - do not use
    function getItemInfoInternal: HResult; stdcall; // prototype not known - do not use
    function getItemInfoByAtomInternal: HResult; stdcall; // prototype not known - do not use
    function getAttributeCountByTypeInternal: HResult; stdcall; // prototype not known - do not use
    function getItemInfoByTypeInternal: HResult; stdcall; // prototype not known - do not use
    function get_durationInternal: HResult; stdcall; // prototype not known - do not use
    function get_nameInternal: HResult; stdcall; // prototype not known - do not use
    function InitFromURL: HResult; stdcall; // prototype not known - do not use
    function InitFromGraph: HResult; stdcall; // prototype not known - do not use
    function CloseFromGraph: HResult; stdcall; // prototype not known - do not use
    function ExternalMediaChangeNotification: HResult; stdcall; // prototype not known - do not use
    function ExternalDeleteNotification: HResult; stdcall; // prototype not known - do not use
    function ExternalInsertNotification: HResult; stdcall; // prototype not known - do not use
    function ExternalLoadNotification: HResult; stdcall; // prototype not known - do not use
    function ExternalPlayNotification: HResult; stdcall; // prototype not known - do not use
    function Advise(const pIID: TGUID; WMPPlaylistMgr: IUnknown): HResult; stdcall;
    function UnAdvise: HResult; stdcall; // prototype not known - do not use
    {wmp_1.CWMPMedia::FreezeEvents
    wmp_1.CWMPMedia::UnFreezeEvents
    wmp_1.CWMPMedia::UnFreezeEventsWithoutNotifications
    wmp_1.CWMPMedia::GetParentPlaylist
    wmp_1.CWMPMedia::SetParentPlaylist
    wmp_1.CWMPMedia::HasChildPlaylist
    wmp_1.CWMPMedia::GetChildPlaylist
    wmp_1.CWMPMedia::SetChildPlaylist
    wmp_1.CWMPMedia::OpenChildPlaylist
    wmp_1.CWMPMedia::GetType
    wmp_1.CWMPMedia::GetIdentifier
    wmp_1.CWMPMedia::GetCachedIdentifier
    wmp_1.CWMPMedia::CreateAlternateMedia
    wmp_1.CWMPMedia::MorphIntoNextAlternateMedia
    wmp_1.CWMPMedia::RestoreMorph
    wmp_1.CWMPMedia::GetCurrentURL
    wmp_1.CWMPMedia::GetCopy
    wmp_1.CWMPMedia::GetReservedItem
    wmp_1.CWMPMedia::SetReservedItem
    wmp_1.CWMPMedia::SetSAMIName
    wmp_1.CWMPMedia::GetSAMIName
    wmp_1.CWMPMedia::GetLoggingInfo
    wmp_1.CWMPSAXContentHandler::putDocumentLocator
    wmp_1.CWMPMedia::GetPreferredRuntime
    wmp_1.CWMPMedia::SetPreferredRuntime
    wmp_1.CWMPMedia::SetWMPInternetManager
    wmp_1.CWMPMedia::IsAvailable
    wmp_1.CWMPMedia::IsLocal
    wmp_1.CWMPMedia::GetAudioCodecName
    wmp_1.CWMPMedia::GetVideoCodecName
    wmp_1.CWMPMedia::GetSaveAsInfo
    wmp_1.CWMPMedia::ClearGraphInfo
    wmp_1.CWMPMedia::IsMorphed
    wmp_1.CWMPMedia::GetHandledScriptEventCount
    wmp_1.CWMPMedia::GetHandledScriptEventTime
    wmp_1.CWMPMedia::GetHandledScriptEventInfo
    wmp_1.CWMPMedia::AddHandledScriptEvent
    wmp_1.CWMPMedia::SetMediaSpecificError
    wmp_1.CWMPMedia::SetMediaSpecificErrorWithEvent
    wmp_1.CWMPMedia::SetItemInfoInternal
    wmp_1.CWMPPlaylist::GetMediaNode
    wmp_1.CHMEProvider::SetAttribute
    wmp_1.CWMPMedia::UpdatePlayCount
    wmp_1.CWMPMedia::InitIcons
    wmp_1.CWMPMedia::get_iconCount
    wmp_1.CWMPMedia::NextIcon
    wmp_1.CWMPMedia::DeleteCacheFile
    wmp_1.CWMPMedia::IsPhysical
    wmp_1.CWMPMedia::SetActualStartTime
    wmp_1.CWMPMedia::SetActualStartMinusStopTime
    wmp_1.CWMPMedia::AddMediaToLibrary
    wmp_1.CWMPMedia::SetValidateContent
    wmp_1.CWMPMedia::GetValidateContent
    wmp_1.CWMPMedia::CacheHeaderScriptEvents
    wmp_1.CWMPMedia::GetList}
  end;

  IWMPMediaCollectionInternal = interface(IUnknown)
    ['{07fe0dbd-f957-11d3-b3b3-00c04f68574b}']

  end;

  IWMPCoreInternal = interface(IUnknown)
    ['{f58425dd-aeb9-11d3-b16c-00c04f79faa6}']
    function RestorePreviousPlaylist: HResult; stdcall; // prototype not known - do not use
    function PreviousPlaylistAvailable: HResult; stdcall; // prototype not known - do not use
    function KillPreviousPlaylistAdvertising: HResult; stdcall; // prototype not known - do not use
    function LockOnDeckPlaylist: HResult; stdcall;
    {CWMPCore::RestoreOnDeckPlaylist
    CWMPCore::GetOnDeckPlaylist
    CWMPCore::GetRootPlaylist
    CWMPCore::createPlaylist
    CWMPCore::createBaseMedia
    CWMPCore::CreatePlaylistFromWmxURL
    CWMPCore::CreateCDPlaylistFromTOC
    CWMPCore::CreateMediaObjectFromURL
    CWMPCore::SetHostInfo
    CWMPCore::GetHostInfo
    CWMPCore::GetSubscriptionServicesManager
    CWMPCore::IsMediaPlaybackInActive
    CWMPCore::UpdateMetadataCDBurn
    CWMPCore::UpdateMetadata
    CWMPCore::UpdateMetadata
    CWMPCore::UpdateMetadata
    CWMPCore::GetAndProcessMetadataForIdentifier
    CWMPCore::ProcessMetadataAsIfDownloaded
    CWMPCore::NotifyCdromMediaChange
    CWMPCore::SetDocument
    CWMPCore::QueryAppExit
    CWMPOleSupport::IOleObject_Close
    CWMPOleSupport::IOleObject_Close
    CWMPCore::GetContainerInfo
    CWMPCore::ShowFilterPropertyPage
    CWMPCore::GetDetailedAspectRatio
    CWMPCore::SetupPlaylistAdvisory
    CWMPCore::SetupMediaAdvisory
    CWMPCore::FireMediaErrorEvent
    CWMPCore::CreatePlaylistInternalProxy
    CWMPCore::Capture
    CWMPCore::CanCapture
    CWMPCore::put_CDAutoStart
    CWMPCore::PostMessageToCore
    CWMPCore::GetMediaStore
    CWMPCore::GetMediaStoreIfCreated
    CWMPCore::GetPlaylistFromMediaStore
    CWMPCore::GetPlaylistFromSearchString
    CWMPCore::GetPlaylistFromWMPlaylist
    CWMPCore::QuerySleep
    CWMPCore::PrepareForSleep
    CWMPCore::GetCachedLCID
    CWMPVMRNode::SetRenderer
    CWMPCore::NotifyDownloadStateChangeInternal
    CWMPCore::ShowDvdPropertyPage
    CWMPCore::SetMDQByRequestID
    CWMPCore::GetMDQByRequestID
    CWMPCore::RenameMediaFileFromInternalAttributes
    CWMPCore::DeleteEmptyMusicFolder
    CWMPCore::IsEmbeddedinIE
    CWMPCore::IsEmbedded
    CWMPCore::SetEventListener
    CWMPCore::SetSite
    CWMPCore::GetAbsoluteURL
    CWMPCore::GetHTMLView
    CWMPCore::GetImagesForMedia
    CWMPCore::GetCurrentScriptCommand
    CWMPCore::EnterModalDialog
    CWMPCore::ExitModalDialog
    CWMPCore::IsModalDialogUp
    CWMPCore::SetInShim
    CWMPCore::GetInShim
    CWMPCore::ResetCurrentItem
    CWMPCore::SetTempStatus
    CWMPCore::GetWebService
    CWMPCore::GetMessenger
    CWMPCore::GetWMDMProvider
    CWMPCore::GetCDBurnProvider
    CWMPCore::ShouldSuppressDialogs
    CWMPCore::NotifyDeviceArrivalRemoval
    CWMPCore::NotifyDeviceStatusChange
    CWMPCore::NotifyDeviceSyncStateChange
    CWMPCore::NotifyDeviceSyncError
    CWMPCore::NotifyCreatePartnershipComplete
    CWMPCore::NotifyDeviceFriendlyNameChange
    CWMPCore::NotifyCdromBurnStateChange
    CWMPCore::NotifyCdromBurnMediaError
    CWMPCore::NotifyCdromBurnError
    CWMPCore::NotifyMediaProviderAdded
    CWMPCore::NotifyMediaProviderRemoved
    CWMPCore::NotifyNeedsIndividualization
    CWMPCore::NotifyNeedsWMGraphIndividualization
    CWMPCore::SetNonDefaultDataBaseDirectory
    CWMPCore::GetNonDefaultDataBaseDirectory
    CWMPCore::SetHostUPnPDevice
    CWMPCore::GetHostUPnPDevice
    CWMPCore::GetPlaylistFromList
    CWMPCore::InsertListInPlaylist
    CWMPCore::AdviseInternalEvents
    CWMPCore::UnadviseInternalEvents
    CWMPCore::HandleConvertPluginError
    CWMPCore::HasBurnSessionInProgress
    CWMPCore::SetThreadReqs
    CWMPCore::DisableRadioSkipping
    CWMPCore::PlayCoreCurrentPlaylist
    CWMPCore::launchURL
    CWMStoreTaskBase::QueryInterface
    CWMSchemaObj::AddRef
    CWMStoreTaskBase::Release
    CWMMetadataTask::GetFlags
    CWMMetadataTask::GetTaskIDFlag
    CWMPPlaylist::GetDuration
    CWMMetadataTask::ExecuteTask
    CWMMetadataTask::Reset
    CWMPOleSupport::IOleObject_Close
    CWMMetadataTask::`scalar deleting destructor'
    CWMStoreTaskBase::QueryInterface
    CWMSchemaObj::AddRef
    CWMStoreTaskBase::Release
    CWMMetadataTask::GetFlags
    CWMOfflineDownloadTask::GetTaskIDFlag
    CWMPPlaylist::GetDuration
    CWMOfflineDownloadTask::ExecuteTask
    CWMOfflineDownloadTask::Reset
    CWMPOleSupport::IOleObject_Close
    CWMOfflineDownloadTask::`scalar deleting destructor'
    CWMPOleSupport::IOleObject_Close
    CWMStoreTaskBase::QueryInterface
    CWMSchemaObj::AddRef
    CWMStoreTaskBase::Release
    CWMMetadataTask::GetFlags
    CWMAddMediaStoreTask::GetTaskIDFlag
    CWMPPlaylist::GetDuration
    CWMAddMediaStoreTask::ExecuteTask
    CHMEProvider::SetAttribute
    CWMAddMediaStoreTask::Shutdown
    CWMAddMediaStoreTask::`vector deleting destructor'
    CWMStoreTaskBase::QueryInterface
    CWMSchemaObj::AddRef
    CWMStoreTaskBase::Release
    CWMFileSyncSingleStoreTask::GetFlags
    CWMFileSyncSingleStoreTask::GetTaskIDFlag
    CWMPPlaylist::GetDuration
    CWMFileSyncSingleStoreTask::ExecuteTask
    CHMEProvider::SetAttribute
    CWMFileSyncSingleStoreTask::Shutdown
    CWMFileSyncSingleStoreTask::`vector deleting destructo>
    CWMStoreTaskBase::QueryInterface
    CWMSchemaObj::AddRef
    CWMStoreTaskBase::Release
    CWMMetadataTask::GetFlags
    CWMEffectiveRatingStoreTask::GetTaskIDFlag
    CWMPPlaylist::GetDuration
    CWMEffectiveRatingStoreTask::ExecuteTask
    CHMEProvider::SetAttribute
    CWMEffectiveRatingStoreTask::Shutdown
    CWMEffectiveRatingStoreTask::`vector deleting destruct>
    ATL::CComObject<CWMPGraphGenerator>::QueryInterface
    CWMSchemaObj::AddRef
    ATL::CComObject<CWMPGraphGenerator>::Release
    CWMPErrorBase::SetErrorManager
    CWMPErrorBase::GetErrorManager
    ATL::CComObject<CWMPGraphGenerator>::`vector deleting >
    ATL::CComObject<CWMStore>::QueryInterface
    ATL::CComObject<CWMStore>::AddRef
    ATL::CComObject<CWMStore>::Release
    CWMStore::Init
    CWMStore::Copy
    CWMStore::Insert
    CWMStore::Delete
    CWMStore::Update
    CWMStore::Query
    CWMStore::CreateEmptyResultSet
    CWMStore::Shutdown
    CWMDRMRSA::CWMDRMRSA
    ATL::CComObject<CWMStore>::`vector deleting destructor>
    CMainFrame::PreTranslateMessage
    CMainFrame::OnIdle}
  end;

  IWMPCoreExternal = interface(IUnknown)
    ['{5e83400b-7b6a-4cae-855e-cb5af5329a24}']
    function GetPlayerApplicationObject(out Obj: IUnknown): HResult; stdcall;
    function GetRootPlaylistMedia: HResult; stdcall; // prototype not known - do not use
    function AdjustMBRLevel: HResult; stdcall; // prototype not known - do not use
    function GetMediaStorePlaylist: HResult; stdcall; // prototype not known - do not use
    function ShowGraphPluginPropertyPage: HResult; stdcall; // prototype not known - do not use
    function SetGraphPluginEnable: HResult; stdcall; // prototype not known - do not use
    function GetGraphPluginInGraph: HResult; stdcall; // prototype not known - do not use
    function CreateRadioPlaylist: HResult; stdcall; // prototype not known - do not use
    function GetCDIcon: HResult; stdcall; // prototype not known - do not use
    function GetCurrentHTMLView: HResult; stdcall; // prototype not known - do not use
    function GetSyncServices: HResult; stdcall; // prototype not known - do not use
    function GetCountByType: HResult; stdcall; // prototype not known - do not use
    function GetLibraryByType: HResult; stdcall; // prototype not known - do not use
    function GetForegroundGroveler(out Groveler: IUnknown): HResult; stdcall;
    {AddMonitoredFolder
    RemoveMonitoredFolder
    IsFolderAnF3Default
    GetVideoInterface
    ShowLibrarySharing
    ShowHMEDeviceDialog
    ExportToXML}
  end;

  IWMPPluginMgr = interface(IDispatch)
    ['{d658392c-e872-11d2-83c2-00c04f8edcc4}']
    function add: HResult; stdcall; // prototype not known - do not use
    function remove: HResult; stdcall; // prototype not known - do not use
    function removeAll: HResult; stdcall; // prototype not known - do not use
    function registerRemoteLocation: HResult; stdcall; // prototype not known - do not use
    function unregisterRemoteLocation: HResult; stdcall; // prototype not known - do not use
    function setCurrentLocation: HResult; stdcall; // prototype not known - do not use
    function hasDisplay: HResult; stdcall; // prototype not known - do not use
    function finishPendingWork: HResult; stdcall; // prototype not known - do not use
    function getRemoteLocationsCount(out Count: Integer): HResult; stdcall;
    function getRemoteLocationInfo(Index: Integer; out RemoteObj: IUnknown;
        out RequestType: Integer): HResult; stdcall;
    {CPluginMgr::getActiveVideoControl
    CPluginMgr::notifyDeviceArrivalRemoval
    CPluginMgr::notifyDeviceStatusChange
    CPluginMgr::notifyDeviceSyncStateChange
    CPluginMgr::notifyDeviceSyncError
    CPluginMgr::notifyCreatePartnershipComplete
    CPluginMgr::notifyCdromRipStateChange
    CPluginMgr::notifyCdromRipMediaError
    CPluginMgr::notifyCdromBurnStateChange
    CPluginMgr::notifyCdromBurnMediaError
    CPluginMgr::notifyCdromBurnError
    CPluginMgr::notifyGrovelStateChange
    CPluginMgr::notifyLibraryConnect
    CPluginMgr::notifyLibraryDisconnect
    CPluginMgr::InstallPlugins
    CPluginMgr::notifyDeviceEstimationComplete}
  end;

  IWMPPlaylistMgr = interface(IUnknown)
    ['{68087ca1-72da-11d3-b15e-00c04f79faa6}']
    function Init: HResult; stdcall; // prototype not known - do not use
    function CreatePlaylistFromMedia: HResult; stdcall; // prototype not known - do not use
    function CreatePlaylistFromURL: HResult; stdcall; // prototype not known - do not use
    function CreateMediaObjectFromURL : HResult; stdcall; // prototype not known - do not use
    function Abort: HResult; stdcall; // prototype not known - do not use
    function Next: HResult; stdcall;
    function Previous: HResult; stdcall; // prototype not known - do not use
    function put_ResumePlayOnMediaFailure: HResult; stdcall; // prototype not known - do not use
    function get_ResumePlayOnMediaFailure: HResult; stdcall; // prototype not known - do not use
    function get_Mode: HResult; stdcall; // prototype not known - do not use
    function put_Mode: HResult; stdcall; // prototype not known - do not use
    function get_CurrentItem: HResult; stdcall; // prototype not known - do not use
    function put_CurrentItem: HResult; stdcall; // prototype not known - do not use
    function GetCurrentPlaylist: HResult; stdcall; // prototype not known - do not use
    function PutCurrentPlaylist: HResult; stdcall; // prototype not known - do not use
    function Close: HResult; stdcall; // prototype not known - do not use
    function GetEffectiveCurrentPlaylistObjects: HResult; stdcall; // prototype not known - do not use
    function GetCurrentPlaylistPlayableCount: HResult; stdcall; // prototype not known - do not use
    function NotifyMediaStateChange: HResult; stdcall; // prototype not known - do not use
    function NotifyPlayStateChange: HResult; stdcall; // prototype not known - do not use
    function NotifyPlaybackEnd: HResult; stdcall; // prototype not known - do not use
    function NotifyMediaError: HResult; stdcall; // prototype not known - do not use
    function NotifyNonPlaybackMediaError: HResult; stdcall; // prototype not known - do not use
    function NotifyScriptCommandWMXEvent: HResult; stdcall; // prototype not known - do not use
    function NotifyEarlyScriptCommandWMXEvent: HResult; stdcall; // prototype not known - do not use
    function LoadMedia: HResult; stdcall; // prototype not known - do not use
    function PrerollNext: HResult; stdcall;
    {function SetBaseURL
    function GetBaseURL
    function OpenMediaChildPlaylist
    function OpenDRMReader
    function CloseDRMReader
    function SetUpPlaylistAdvisory
    function GetImagesForMedia
    function IsMinimumLibraryRightsRead
    function CanAddToLibrary
    function InitWMDRMQuery}
  end;

  IWMPPlaylistEventHandler = interface(IUnknown)
    ['{68087ca2-72da-11d3-b15e-00c04f79faa6}']
    function NotifyAboutToClearPlaylist: HResult; stdcall; // prototype not known - do not use
    function NotifyAboutToRemoveItem: HResult; stdcall; // prototype not known - do not use
    function NotifyPlaylistChanged(Playlist: IUnknown {IWMPPlaylist};
        Unknown: Integer; change: WMPPlaylistChangeEventType): HResult; stdcall;
  end;

  IWMPListControlViewHandler = interface(IUnknown)
    ['{864ed541-6436-442f-b344-05af62eb2901}']

  end;

  IWMPMediaEventHandler = interface(IUnknown)
    ['{e84edd83-82b1-11d3-b15f-00c04f79faa6}']

  end;

  IAppDispatchWMP11 = interface(IDispatch)
    ['{E41C88DD-2364-4FF7-A0F5-CA9859AF783F}']
    function get_titlebarVisible(out Value: WordBool): HResult; stdcall;
    function put_titlebarVisible(Value: WordBool): HResult; stdcall;
    function get_titlebarAutoHide(out Value: WordBool): HResult; stdcall;
    function put_titlebarAutoHide(Value: WordBool): HResult; stdcall;
    function get_currentTask(out Value: WideString): HResult; stdcall;
    function put_currentTask(const Value: WideString): HResult; stdcall;
    function get_settingsVisible(out Value: WordBool): HResult; stdcall;
    function put_settingsVisible(Value: WordBool): HResult; stdcall;
    function get_playlistVisible(out Value: WordBool): HResult; stdcall;
    function put_playlistVisible(Value: WordBool): HResult; stdcall;
    function gotoSkinMode: HResult; stdcall;
    function navigatePrevious: HResult; stdcall;
    function navigateNext: HResult; stdcall;
    function goFullScreen: HResult; stdcall; // prototype not known - do not use
    function get_fullScreenEnabled: HResult; stdcall; // prototype not known - do not use
    function get_serviceLoginVisible: HResult; stdcall; // prototype not known - do not use
    function get_serviceLoginSignedIn: HResult; stdcall; // prototype not known - do not use
    function serviceLogin: HResult; stdcall; // prototype not known - do not use
    function serviceLogout: HResult; stdcall; // prototype not known - do not use
    function get_serviceGetInfo: HResult; stdcall; // prototype not known - do not use
    function get_navigatePreviousEnabled: HResult; stdcall; // prototype not known - do not use
    function get_navigateNextEnabled: HResult; stdcall; // prototype not known - do not use
    function navigateToAddress(const Address: WideString): HResult; stdcall;
    {wmp_1]E.CMainFrame::get_glassEnabled
    wmp_1]E.CWMPNowPlayingHelper::get_inVistaPlus
    wmp_1]E.CMainFrame::adjustLeft
    wmp_1]E.CWMPCdMediaStoreMedia::IsLocal
    wmp_1]E.CWMPSAXContentHandler::putDocumentLocator
    wmp_1]E.CMainFrame::get_DPI
    wmp_1]E.CMainFrame::get_previousEnabled
    wmp_1]E.CMainFrame::get_playLibraryItemEnabled
    wmp_1]E.CMainFrame::previous
    wmp_1]E.CMainFrame::get_titlebarCurrentlyVisible
    wmp_1]E.CMainFrame::get_menubarCurrentlyVisible
    wmp_1]E.CMainFrame::get_bgPluginRunning
    wmp_1]E.CMainFrame::configurePlugins
    wmp_1]E.CMainFrame::getTimeString
    wmp_1]E.CMainFrame::get_maximized
    wmp_1]E.CMainFrame::get_top
    wmp_1]E.CMainFrame::put_top
    wmp_1]E.CMainFrame::get_left
    wmp_1]E.CMainFrame::put_left
    wmp_1]E.CMainFrame::get_width
    wmp_1]E.CMainFrame::put_width
    wmp_1]E.CMainFrame::get_height
    wmp_1]E.CMainFrame::put_height
    wmp_1]E.CMainFrame::setWindowPos
    wmp_1]E.CMainFrame::logData
    wmp_1]E.CMainFrame::get_powerPersonality
    wmp_1]E.CMainFrame::navigateToAddress
    wmp_1]E.CMainFrame::get_exclusiveService
    wmp_1]E.CMainFrame::put_windowText}
  end;

  IMediaViewHandler = interface(IUnknown)
    {wmp_1]E.CMediaViewHandler::WeakAdvise
    wmp_1]E.CMediaViewHandler::Unadvise
    wmp_1]E.CMediaViewHandler::SetAttribute
    wmp_1]E.CMediaViewHandler::GetPlaylistFromDataObject
    wmp_1]E.CMediaViewHandler::SetList
    wmp_1]E.CMediaViewHandler::Shutdown
    wmp_1]E.CMediaViewHandler::SetFilterString
    wmp_1]E.CMediaViewHandler::FilteredItemVisible
    wmp_1]E.CMediaViewHandler::GetViewMode
    wmp_1]E.CMediaViewHandler::ViewModeChanged
    wmp_1]E.CMediaViewHandler::ArtSizeChanged
    wmp_1]E.CMediaViewHandler::GetColumns
    wmp_1]E.CMediaViewHandler::ColumnsUpdated
    wmp_1]E.CMediaViewHandler::GetColumnName
    wmp_1]E.CMediaViewHandler::NewColumnsNeeded
    wmp_1]E.CMediaViewHandler::CanRestoreColumns
    wmp_1]E.CMediaViewHandler::GetProgress
    wmp_1]E.CMediaViewHandler::GetItemColors
    wmp_1]E.CMediaViewHandler::ItemPostPaint
    wmp_1]E.CMediaViewHandler::GetIconImages
    wmp_1]E.CMediaViewHandler::GetItemIconFast
    wmp_1]E.CMediaViewHandler::GetItemIconFull
    wmp_1]E.CMediaViewHandler::GetItemIconTooltip
    wmp_1]E.CMediaViewHandler::IconClick
    wmp_1]E.CMediaViewHandler::GetItemAttributeString
    wmp_1]E.CMediaViewHandler::CanShowGroups
    wmp_1]E.CMediaViewHandler::GetGroupField
    wmp_1]E.CMediaViewHandler::GetSubgroupField
    wmp_1]E.CMediaViewHandler::PropertiesGrouped
    wmp_1]E.CMediaViewHandler::GetGroupFriendlyName
    wmp_1]E.CMediaViewHandler::GetGroupArt
    wmp_1]E.CMediaViewHandler::GetUnknownArt
    wmp_1]E.CMediaViewHandler::GetArtRequestToClear
    wmp_1]E.CMediaViewHandler::OnCustomDrawColumnItem
    wmp_1]E.CMediaViewHandler::OnCustomDrawHoverItemChange
    wmp_1]E.CMediaViewHandler::OnCustomDrawMouseMessage
    wmp_1]E.CMediaViewHandler::DoDefaultAction
    wmp_1]E.CMediaViewHandler::DoKey
    wmp_1]E.CMediaViewHandler::DoDelete
    wmp_1]E.CMediaViewHandler::DoContextMenu
    wmp_1]E.CMediaViewHandler::DoContextMenuAction
    wmp_1]E.CMediaViewHandler::CanGotoMetadataField
    wmp_1]E.CMediaViewHandler::GotoMetadataField
    wmp_1]E.CMediaViewHandler::GetSortedColumnID
    wmp_1]E.CMediaViewHandler::CanDoColumnClick
    wmp_1]E.CMediaViewHandler::DoColumnClick
    wmp_1]E.CMediaViewHandler::DragSupported
    wmp_1]E.CMediaViewHandler::DropSupported
    wmp_1]E.CMediaViewHandler::DoDrop
    wmp_1]E.CMediaViewHandler::CanEditColumn
    wmp_1]E.CMediaViewHandler::SaveColumnEdit
    wmp_1]E.CMediaViewHandler::GetLastEditedItem
    wmp_1]E.CMediaViewHandler::GetAccelerators
    wmp_1]E.CMediaViewHandler::DoAccelerator
    wmp_1]E.CMediaViewHandler::SetWeakListControl
    wmp_1]E.CMediaViewHandler::OnKillFocus
    wmp_1]E.CMediaViewHandler::OnMetadataChangeUpdate}
  end;

  TIUnknown = record
    QueryInterface: Pointer;
    AddRef: Pointer;
    Release: Pointer;
  end;
  
  TIWMPList = record
    Unknown: TIUnknown;
    WeakAdvise: function: HResult; stdcall; // prototype not known - do not use
    Unadvise: function: HResult; stdcall; // prototype not known - do not use
    GetSortOrder: function: HResult; stdcall; // prototype not known - do not use
    Sort: function(Order1, Order2: Integer): HResult; stdcall; // thiscall
    GetItemCount: function: Integer; stdcall; // thiscall
    CapabilitySupported: function: HResult; stdcall; // prototype not known - do not use
    ItemCapabilitySupported: function: HResult; stdcall; // prototype not known - do not use
    GetAttributeCount: function(lAtom: Integer): Integer; stdcall; // thiscall
    GetAttribute: function: HResult; stdcall; // prototype not known - do not use
    SetAttribute: function: HResult; stdcall; // prototype not known - do not use
    GetItemAttributeCount: function: HResult; stdcall; // prototype not known - do not use
    GetItemAttribute: function(lItemlParam: LPARAM; lAtom: Integer; lIndex: Integer;
        pvarValue: PVariantArg): HResult; stdcall; // thiscall
    SetItemAttribute: function(lItemlParam: LPARAM; lAtom: Integer; lIndex: Integer;
        pvarValue: PVariantArg): HResult; stdcall; // prototype not tested yet
    {wmp_1.CMediaList::GetItemAttributeString
    wmp_1.CMediaList::FireMetadataChange}
  end;
  PIWMPList = ^TIWMPList;

  IWMPList = interface(IUnknown)
    function WeakAdvise: HResult; stdcall; // prototype not known - do not use
    function Unadvise: HResult; stdcall; // prototype not known - do not use
    function GetSortOrder: HResult; stdcall; // prototype not known - do not use
    function Sort(Order1, Order2: Integer): HResult; stdcall; // thiscall
    function GetItemCount: Integer; stdcall; // thiscall
    function CapabilitySupported: HResult; stdcall; // prototype not known - do not use
    function ItemCapabilitySupported: HResult; stdcall; // prototype not known - do not use
    function GetAttributeCount(lAtom: Integer): Integer; stdcall; // thiscall
    function GetAttribute: HResult; stdcall; // prototype not known - do not use
    function SetAttribute: HResult; stdcall; // prototype not known - do not use
    function GetItemAttributeCount: HResult; stdcall; // prototype not known - do not use
    function GetItemAttribute: HResult; stdcall; // prototype not known - do not use
    function SetItemAttribute(Unknown: Integer; lAtom: Integer; lIndex: Integer;
        pvarValue: PVariantArg): HResult; stdcall; // prototype not tested yet
    {wmp_1.CMediaList::GetItemAttributeString
    wmp_1.CMediaList::FireMetadataChange}
  end;

  IWMPListFilter = interface(IUnknown)
    ['{6aa76d70-eb09-4e0b-97ba-5e5a61490b8a}']
  end;

  IWMPSyncEventsInternal = interface(IUnknown)
    ['{f6468267-bb10-4d69-aa08-a4fc5b6e5670}']
  end;

  ISyncEngine = interface(IUnknown)
    ['{696514d2-2f9c-43db-93ce-ddf1962462b9}']
  end;

  TIMediaCache = record
    //['{bd75c8da-67d8-435e-8205-aa57d1edc898}']
    Unknown: TIUnknown;
    GetAttribute: function: HResult; stdcall; // prototype not known - do not use
    ItemCapabilitySupported: function: HResult; stdcall; // prototype not known - do not use
    CapabilitySupported: function: HResult; stdcall; // prototype not known - do not use
    GetItemAttributeCount: function: HResult; stdcall; // prototype not known - do not use
    GetItemAttribute: function(Unknown, lAtom: Integer; lIndex: Integer;
        var pvarValue: OleVariant): HResult; stdcall;
    {wmp_1.CMediaCache::SetItemAttribute
    wmp_1.CMediaCache::GetItemAttributeString
    wmp_1.CMediaCache::GetItemStringReference
    wmp_1.CMediaCache::CalculateCumulativeProperties
    wmp_1.CMediaCache::ExternalLock
    wmp_1.CMediaCache::ExternalUnlock
    wmp_1.CMediaCache::ListRelease
    wmp_1.CMediaCache::GetMessenger}
  end;
  PIMediaCache = ^TIMediaCache;

type
  TWMPGrovelState = type Integer;
const
  wmpgsUnknown = 0;
  wmpgsSearchingFiles = 1;
  wmpgsInitializing = 2;
  wmpgsAddingFiles = 3;
  wmpgsFinished = 4;

type
  IWMForegroundGroveler = interface(IUnknown)
    ['{cd00015a-9436-46ce-9aff-920d476f54c2}']
    function BeginThreadGrovel: HResult; stdcall;
    function EndThreadGrovel(WaitWithMessageLoop: WordBool): HResult; stdcall;
    function GetGrovelState(out State: TWMPGrovelState): HResult; stdcall;
    function GetCompletionPercent(out Percent: Integer): HResult; stdcall;
    function GetNumberFilesScanned(out NumFiles: Integer): HResult; stdcall;
    function GetNumberFilesAdded(out NumFiles: Integer): HResult; stdcall;
    function GetCurrentDirectoryW(out Path: WideString): HResult; stdcall;
    function GetCurrentFile: HResult; stdcall; // prototype not known - do not use
  end;

  IWMPPlayerAppInternal = interface(IUnknown)
    ['{57d1e476-d663-4438-ac28-9b25eb230f58}']
    function AddRemoteApplication: HResult; stdcall; // prototype not known - do not use
    function RemoveRemoteApplication: HResult; stdcall; // prototype not known - do not use
    function GetRemoteAppCount(out Count: Integer): HResult; stdcall;
    function GetRemoteAppByIndex(Index: Integer;
        out App: Pointer {IWMPRemoteApp}): HResult; stdcall;
    function SwitchToControl: HResult; stdcall; // prototype not known - do not use
    function ActivateUIPlugin(const bstrPlugin: WideString): HResult; stdcall;
    {SetTaskPane
    SetTaskPaneURL
    SetBackgroundProcessingPriority
    SetCurrentRemoteService
    RemoveRemoteService
    SettingMediaOnCore
    SetExclusiveService}
  end;

  IWMPRemoteApp = interface(IUnknown)
    ['{04806107-3c85-4851-9bf1-d50231e6cadc}']
    function Initialize: HResult; stdcall; // prototype not known - do not use
    function GetRequestType: HResult; stdcall; // prototype not known - do not use
    function GetName(out Name: WideString): HResult; stdcall;
  end;

implementation

const
  SetAttributeFlagsWMPSDK = $8000000; // flags used by IWMPMedia::setItemInfo
  SetAttributeFlagsATE = $04000088; // flags used by ATE

function WMPSetItemInfoByIndex(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer;
    const pvarValue: OleVariant): HResult;
begin
  if not Assigned(ItemData) then begin
    Result := E_INVALIDARG;
    Exit;
  end;
  Result := ItemData.SetAttribute(SetAttributeFlagsATE, bstrItemName, '', Index,
      pvarValue);
//  if Succeeded(Result) then
//    ItemData.Flush; // doesn't seem to be necessary, but do it anyway
end;

function WMPInsertAttribute(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer;
    const pvarValue: OleVariant): HResult;
begin
  if not Assigned(ItemData) then begin
    Result := E_INVALIDARG;
    Exit;
  end;
  Result := ItemData.InsertAttribute(SetAttributeFlagsATE, bstrItemName, '',
      Index, pvarValue);
end;

function WMPDeleteItemInfoByIndex(ItemData: IWMPItemData;
    const bstrItemName: WideString; Index: Integer; SaveToFile: Boolean): HResult;
var
  Flags: Integer;
begin
  if not Assigned(ItemData) then begin
    Result := E_INVALIDARG;
    Exit;
  end;
  if SaveToFile then
    Flags := SetAttributeFlagsATE
  else
    Flags := SetAttributeFlagsWMPSDK;
  Result := ItemData.DeleteAttribute(Flags, bstrItemName, '', Index);
//  if Succeeded(Result) then
//    ItemData.Flush; // doesn't seem to be necessary, but do it anyway
end;

end.
