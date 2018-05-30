(****************************************************************************
 *
 * Copyright 2017 Tim De Baets
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
 * Windows Media Player attributes, as they are documented in the Windows SDK
 *
 ****************************************************************************)

unit WMPAttribs;

interface

uses Windows;

type
  TWMPMediaType = (wmpmtAudio, wmpmtCDPlaylist, wmpmtCDTrack, wmpmtDVD,
      wmpmtMusic, wmpmtOther, wmpmtPhoto, wmpmtPlaylist, wmpmtRadio, wmpmtVideo,
      wmpmtUnknown);
  TWMPMediaTypes = set of TWMPMediaType;

const
  WMPMediaTypes: array[TWMPMediaType] of PWideChar = (
      'audio', '', '', '',
      '', 'other', 'photo', 'playlist', 'radio', 'video',
      '');
  WMPAudioMediaTypes: TWMPMediaTypes = [wmpmtAudio, wmpmtCDTrack, wmpmtMusic];

type
  TWMPAttributeDataType = (wmpadtString, wmpadtInt, wmpadtDate);

  TWMPAttributeStoreType = (wmpastLibrary, wmpastFile);
  TWMPAttributeStoreTypes = set of TWMPAttributeStoreType;

  TWMPAttributeInfo = record
    Name: WideString;
    Typ: TWMPAttributeDataType;
    MultipleValues: Boolean;
    StoreTypes: TWMPAttributeStoreTypes;
    AppliesTo: TWMPMediaTypes;
  end;
  PWMPAttributeInfo = ^TWMPAttributeInfo;

  // mostly audio item attributes - other ones may get added when needed
  TWMPAttribute = (
    wmpaAcquisitionTime,
    wmpaAlbumID,
    wmpaAlbumIDAlbumArtist,
    wmpaAuthor,
    wmpaAverageLevel,
    wmpaBitrate,
    wmpaBuyNow,
    wmpaBuyTickets,
    wmpaCDTrackEnabled,
    wmpaChannels,
    wmpaCopyright,
    wmpaCurrentBitrate,
    wmpaDescription,
    wmpaDisplayArtist,
    wmpaDuration,
    wmpaFileSize,
    wmpaFileType,
    wmpaIs_Protected,
    wmpaIsVBR,
    wmpaMediaType,
    wmpaMoreInfo,
    wmpaPeakValue,
    wmpaPlaylistIndex,
    wmpaProvider,
    wmpaProviderLogoURL,
    wmpaProviderURL,
    wmpaRecordingTime,
    wmpaRecordingTimeDay,
    wmpaRecordingTimeMonth,
    wmpaRecordingTimeYear,
    wmpaRecordingTimeYearMonth,
    wmpaRecordingTimeYearMonthDay,
    wmpaReleaseDate,
    wmpaReleaseDateDay,
    wmpaReleaseDateMonth,
    wmpaReleaseDateYear,
    wmpaReleaseDateYearMonth,
    wmpaReleaseDateYearMonthDay,
    wmpaRequestState,
    wmpaShadowFilePath,
    wmpaSourceURL,
    wmpaSyncState,
    wmpaTitle,
    wmpaTrackingID,
    wmpaUserCustom1,
    wmpaUserCustom2,
    wmpaUserEffectiveRating,
    wmpaUserLastPlayedTime,
    wmpaUserPlayCount,
    wmpaUserPlaycountAfternoon,
    wmpaUserPlaycountEvening,
    wmpaUserPlaycountMorning,
    wmpaUserPlaycountNight,
    wmpaUserPlaycountWeekday,
    wmpaUserPlaycountWeekend,
    wmpaUserRating,
    wmpaUserServiceRating,
    wmpaWMAlbumArtist,
    wmpaWMAlbumTitle,
    wmpaWMBeatsPerMinute,
    wmpaWMCategory,
    wmpaWMComposer,
    wmpaWMConductor,
    wmpaWMContentDistributor,
    wmpaWMContentGroupDescription,
    wmpaWMEncodingTime,
    wmpaWMGenre,
    wmpaWMGenreID,
    wmpaWMInitialKey,
    wmpaWMISRC,
    wmpaWMLanguage,
    wmpaWMLyrics,
    wmpaWMMCDI,
    wmpaWMMediaClassPrimaryID,
    wmpaWMMediaClassSecondaryID,
    wmpaWMMood,
    wmpaWMOriginalAlbumTitle,
    wmpaWMParentalRating,
    wmpaPartOfSet,
    wmpaWMPeriod,
    wmpaWMProtectionType,
    wmpaWMProvider,
    wmpaWMProviderRating,
    wmpaWMProviderStyle,
    wmpaWMPublisher,
    wmpaWMSubscriptionContentID,
    wmpaWMSubTitle,
    wmpaWMTrackNumber,
    wmpaWMUniqueFileIdentifier,
    wmpaWMWMCollectionGroupID,
    wmpaWMWMCollectionID,
    wmpaWMWMContentID,
    wmpaWMWriter,
    wmpaWMYear
  );

  TWMPAttributes = set of TWMPAttribute;

const
  WMPAttributes: array[TWMPAttribute] of TWMPAttributeInfo = (
    (Name: 'AcquisitionTime'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'AlbumID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'AlbumIDAlbumArtist'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Author'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'AverageLevel'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Bitrate'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'BuyNow'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'BuyTickets'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'CDTrackEnabled'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtCDTrack]),
    (Name: 'Channels'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary {?}, wmpastFile {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Copyright'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'CurrentBitrate'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary {?}, wmpastFile {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Description'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtMusic]),
    (Name: 'DisplayArtist'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Duration'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'FileSize'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'FileType'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Is_Protected'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'IsVBR'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'MediaType'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'MoreInfo'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'PeakValue'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'PlaylistIndex'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Provider'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtCDPlaylist]),
    (Name: 'ProviderLogoURL'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ProviderURL'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTime'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTimeDay'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTimeMonth'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTimeYear'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTimeYearMonth'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RecordingTimeYearMonthDay'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDate'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary]; // year part is saved to file as WM/Year
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDateDay'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDateMonth'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDateYear'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDateYearMonth'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ReleaseDateYearMonthDay'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'RequestState'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'ShadowFilePath'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'SourceURL'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'SyncState'; Typ: wmpadtInt {?}; MultipleValues: False;
        StoreTypes: [wmpastLibrary {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'Title'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'TrackingID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserCustom1'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserCustom2'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserEffectiveRating'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserLastPlayedTime'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlayCount'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountAfternoon'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountEvening'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountMorning'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountNight'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountWeekday'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserPlaycountWeekend'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserRating'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'UserServiceRating'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/AlbumArtist'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/AlbumTitle'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/BeatsPerMinute'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtMusic]),
    (Name: 'WM/Category'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Composer'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Conductor'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/ContentDistributor'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/ContentGroupDescription'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/EncodingTime'; Typ: wmpadtDate; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Genre'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/GenreID'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/InitialKey'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    // TODO: does WM/ISRC actually work? (not documented)
    (Name: 'WM/ISRC'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtMusic]),
    (Name: 'WM/Language'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Lyrics'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/MCDI'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/MediaClassPrimaryID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/MediaClassSecondaryID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Mood'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/OriginalAlbumTitle'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtMusic]),
    (Name: 'WM/ParentalRating'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    // The SDK only lists PartOfSet (without WM/) for audio items, but the
    // correct name still seems to include WM/ even for audio items
    (Name: 'WM/PartOfSet'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile {?}];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Period'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/ProtectionType'; Typ: wmpadtString {?}; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Provider'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/ProviderRating'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/ProviderStyle'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Publisher'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/SubscriptionContentID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/SubTitle'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/TrackNumber'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/UniqueFileIdentifier'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/WMCollectionGroupID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/WMCollectionID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/WMContentID'; Typ: wmpadtString; MultipleValues: False;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Writer'; Typ: wmpadtString; MultipleValues: True;
        StoreTypes: [wmpastLibrary, wmpastFile];
        AppliesTo: [wmpmtAudio]),
    (Name: 'WM/Year'; Typ: wmpadtInt; MultipleValues: False;
        StoreTypes: [wmpastFile];
        AppliesTo: [wmpmtAudio])
  );

function FindWMPAttribute(const Name: WideString;
    var Attribute: TWMPAttribute): Boolean;
function IsWMPUserRatingAttribute(Attribute: TWMPAttribute): Boolean;

implementation

function FindWMPAttribute(const Name: WideString;
    var Attribute: TWMPAttribute): Boolean;
var
  WMPAttribute: TWMPAttribute;
begin
  Result := False;
  for WMPAttribute := Low(TWMPAttribute) to High(TWMPAttribute) do begin
    if lstrcmpiW(PWideChar(Name),
        PWideChar(WMPAttributes[WMPAttribute].Name)) = 0 then begin
      Attribute := WMPAttribute;
      Result := True;
      Exit;
    end;
  end;
end;

function IsWMPUserRatingAttribute(Attribute: TWMPAttribute): Boolean;
begin
  Result := Attribute in [wmpaUserEffectiveRating, wmpaUserRating,
      wmpaUserServiceRating];
end;

end.
