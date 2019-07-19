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
 * Windows Media Player utility code
 *
 ****************************************************************************)

unit WMPUtil;

interface

uses Windows, Messages, SysUtils, ComObj, WMPLib_TLB, Common2, CommCtrl,
    cUnicode, CommonUnicode, TntSysUtils, ActiveX, WMPAttribs, MyRegistry,
    WMPUndocumented;

const
  WMPBaseRegKey = 'Software\Microsoft\MediaPlayer';
  WMPPreferencesRegKey = WMPBaseRegKey + '\Preferences';
  WMPObjectsRegKey = WMPBaseRegKey + '\Objects';

// Resources - verified in WMP 10-12
const
  WMPDll: PChar = 'wmp.dll';
  WMPLocDll: PChar = 'wmploc.dll';
  WMErrorDll: PChar = 'wmerror.dll';
  WMPLocSkinResType = 256;
  WMPUnknownArtistID = 916;
  WMPPopupMenusID = 7228;
  WMPPlayPauseID = 18808;
  WMPStopID = 18809;
  WMPFindAlbumInfoID: Word = 18906;
  WMPOpenContainingFolderID = 7243;
  WMPShuffleID = 18842;
  WMPRepeatID = 18843;
  WMPOptionsID = 18825;
  WMPMainMenuID = 616;
  WMPErrorDialogID = 2001;
  WMPErrorIconID = 705;
  WMPAdvancedTagEditorDialogID = 21400;
  WMPDefaultKeysIDStart = 20789;
  WMPDefaultKeysIDEnd = 20831;
  WMPDefaultMoodsIDStart = 21474;
  WMPDefaultMoodsIDEnd = 21485;
  WMPSaveNowPlayingListID = 19013;
  WMPDeleteID = 7240;
  WMPPropertiesID = 7261;
  WMPPlayItemID = 7230;
  WMPNormalPlaybackSpeedID = 18835;
  WMPShowMenuBarID = 20432;

const
  WMPFindInLibraryID = 7668;

// WMP 11 or lower only
const
  WMPAdvancedEditorItemID = 7304;

// Window class names and captions
const
  WMPlayerAppClass: PChar = 'WMPlayerApp';
  WMPPlaylistClass: PChar = 'WMPPlaylist';
  WMPAppHostClass: PChar = 'WMPAppHost';
  WMPSkinHostClass: PChar = 'WMP Skin Host';
  LibraryContainerCaption: PChar = 'LibraryContainer';
  BasketListViewCaption: PChar = 'BasketListView';
  PrimaryListViewCaption: PChar = 'PrimaryListView';
  WMPPluginHostClass: PChar = 'WMP Plugin UI Host';
  ATLListViewClass: PChar = 'ATL:SysListView32';
  WMPFullScreenClass: PChar = 'WMPTransition';
  WMPControlContainerClass: PChar = 'CWmpControlCntr';
  RebarWindowClass: PChar = 'ReBarWindow32';

  // WMP 11
  LibraryTreeViewCaption11: PChar = 'LibraryTree';

  // WMP 12
  LibraryTreeViewCaption12: PChar = 'Library Treeview';
  WMPListTitleCaptionBeta: PChar = 'BasketPlaylistToolbar'; // Windows 7 Beta
  WMPPlayHistoryBackCaption: PChar = 'PlayHistory Back Toolbar';
  WMPPlayHistoryForwardCaption: PChar = 'PlayHistory Forward Toolbar';
  WMPNowPlayingWindowCaption: PChar = 'Now Playing Basket Window';

// Localized window captions - WMP 12 only
type
  TWMPLocalizedCaption = (wmplcListTitle, wmplcNowPlayingList);
  TWMPLocalizedCaptions = array[TWMPLocalizedCaption] of String;

const
  WMPLocalizedCaptionIDs: array[TWMPLocalizedCaption] of Word = (
    7530, 5406
  );
  WMPDefaultLocalizedCaptions: TWMPLocalizedCaptions = (
    'List Title', 'Now Playing list'
  );

const
  MediaTypeAudio: PWideChar = 'audio';
  MediaTypeVideo: PWideChar = 'video';
  MediaTypeOther: PWideChar = 'other';
  MediaTypePlaylist: PWideChar = 'playlist';
  MediaTypeRadio: PWideChar = 'radio';

const
  MediaClassSecondaryID_StaticPlaylist: TGUID =
      '{D0E20D5C-CAD6-4F66-9FA1-6018830F1DCC}';
  MediaClassSecondaryID_AutoPlaylist: TGUID =
      '{EB0BAFB6-3C4F-4C31-AA39-95C7B8D7831D}';

type
  TWMPPane = (wmppLibrary, wmppList);

const
  LastPlayedFilename = 'lastplayed.wpl';

const
  PLUGIN_TYPE_MASK = $000000FF;

function GetWMPlayerAppWindow: HWND;
function IsMainWMPWindow(hWnd: HWND; pClassName: PChar): Boolean;
function GetWMPNowPlayingWindow: HWND;
function IsWMPNowPlayingWindow(hWnd: HWND; pClassName: PChar): Boolean;
function GetWMPModalhWndParent(WMPlayerAppWnd, WMPNowPlayingWnd: HWND): HWND;
function GetWMPLibraryContainer(WMPlayerAppWnd: HWND): HWND;
function GetWMPPrimaryListView(WMPlayerAppWnd: HWND): HWND;
function GetWMPBasketListView(WMPlayerAppWnd: HWND): HWND;
function IsWMPPrimaryListView(hWnd: HWND): Boolean;
function IsWMPBasketListView(hWnd: HWND): Boolean;
function IsWMPMainMenuVisible(WMPlayerAppWnd: HWND): Boolean;
function IsEditControl(hWnd: HWND): Boolean;
function IsMainWMPWindowActive(WMPlayerAppWnd: HWND;
    var FullScreen: Boolean): Boolean;
procedure CloseWMP(WMPlayerAppWnd: HWND);
function WMPRatingToStars(WMPRating: Integer): Byte;
function StarsToWMPRating(Stars: Byte): Byte;
procedure SplitPartOfSet(const PartOfSet: String; var Disc, TotalDiscs: Integer);
function GetWMPMainVersion(const Version: WideString): Byte;
function GetWMPLibraryPath: String;
function WMPGetItemInfo(Media: IWMPMedia;
    const bstrItemName: WideString): WideString;
function FindAtlWindow(hwndParent: HWND; const WindowTitle: String): Integer;
procedure GetWMPLocalizedCaptions(hWMPLoc: HMODULE;
    var Captions: TWMPLocalizedCaptions);

type
  // On WMP 12, there's a single 'playlists' location for all media types. On WMP
  // 11 and earlier, there's a separate 'playlists' location for each media type.
  TWMPLibraryCategory = (
    wmplcPlaylists, wmplcAllTracks, wmplcArtist, wmplcContributingArtist,
    wmplcAlbum, wmplcGenre, wmplcYear, wmplcComposer, wmplcRating,
    wmplcParentalRating, wmplcOnlineStores, wmplcFolder);

const
  // tested on WMP 11 and 12
  WMPLibraryCategoryStrs: array[TWMPLibraryCategory] of WideString = (
    'Playlists', 'AllTracks', 'DisplayArtist', 'Author',
    'AlbumID', 'WM/Genre', 'ReleaseDateYear', 'WM/Composer', 'UserRatingStars',
    'WM/ParentalRating', 'WM/ContentDistributor', 'Folder00');

type
  EWMPNavigateException = class(Exception);

function WMPNavigateToAddress(WMPAppDispatch: IDispatch;
    const Address: WideString): Boolean;
function WMPNavigateToLibraryCategory(WMPAppDispatch: IDispatch; WMPVersion: Byte;
    MediaType: TWMPMediaType; Category: TWMPLibraryCategory;
    const CategoryValue: WideString; const WordWheelText: WideString = ''): Boolean;

type
  TWMPPlayState = (
      wmppsUndefined,
      wmppsStopped,
      wmppsPaused,
      wmppsPlaying,
      wmppsScanForward,
      wmppsScanReverse,
      wmppsBuffering,
      wmppsWaiting,
      wmppsMediaEnded,
      wmppsTransitioning,
      wmppsReady,
      wmppsReconnecting,
      wmppsLast
  );

type
// *********************************************************************//
// Interface: IWMPEvents
// Flags:     (0)
// GUID:      {19A6627B-DA9E-47C1-BB23-00B5E668236A}
// *********************************************************************//
  IWMPEventsNew = interface(IUnknown)
    ['{19A6627B-DA9E-47C1-BB23-00B5E668236A}']
    procedure OpenStateChange(NewState: Integer); stdcall;
    procedure PlayStateChange(NewState: TWMPPlayState); stdcall;
    procedure AudioLanguageChange(LangID: Integer); stdcall;
    procedure StatusChange; stdcall;
    procedure ScriptCommand(const scType: WideString; const Param: WideString); stdcall;
    procedure NewStream; stdcall;
    procedure Disconnect(Result_: Integer); stdcall;
    procedure Buffering(Start: WordBool); stdcall;
    procedure Error; stdcall;
    procedure Warning(WarningType: Integer; Param: Integer; const Description: WideString); stdcall;
    procedure EndOfStream(Result_: Integer); stdcall;
    procedure PositionChange(oldPosition: Double; newPosition: Double); stdcall;
    procedure MarkerHit(MarkerNum: Integer); stdcall;
    procedure DurationUnitChange(NewDurationUnit: Integer); stdcall;
    procedure CdromMediaChange(CdromNum: Integer); stdcall;
    procedure PlaylistChange(Playlist: IDispatch; change: WMPPlaylistChangeEventType); stdcall;
    procedure CurrentPlaylistChange(change: WMPPlaylistChangeEventType); stdcall;
    procedure CurrentPlaylistItemAvailable(const bstrItemName: WideString); stdcall;
    procedure MediaChange(Item: IDispatch); stdcall;
    procedure CurrentMediaItemAvailable(const bstrItemName: WideString); stdcall;
    procedure CurrentItemChange(pdispMedia: IDispatch); stdcall;
    procedure MediaCollectionChange; stdcall;
    procedure MediaCollectionAttributeStringAdded(const bstrAttribName: WideString; 
                                                  const bstrAttribVal: WideString); stdcall;
    procedure MediaCollectionAttributeStringRemoved(const bstrAttribName: WideString; 
                                                    const bstrAttribVal: WideString); stdcall;
    procedure MediaCollectionAttributeStringChanged(const bstrAttribName: WideString; 
                                                    const bstrOldAttribVal: WideString; 
                                                    const bstrNewAttribVal: WideString); stdcall;
    procedure PlaylistCollectionChange; stdcall;
    procedure PlaylistCollectionPlaylistAdded(const bstrPlaylistName: WideString); stdcall;
    procedure PlaylistCollectionPlaylistRemoved(const bstrPlaylistName: WideString); stdcall;
    procedure PlaylistCollectionPlaylistSetAsDeleted(const bstrPlaylistName: WideString; 
                                                     varfIsDeleted: WordBool); stdcall;
    procedure ModeChange(const ModeName: WideString; NewValue: WordBool); stdcall;
    procedure MediaError(pMediaObject: IDispatch); stdcall;
    procedure OpenPlaylistSwitch(pItem: IDispatch); stdcall;
    procedure DomainChange(const strDomain: WideString); stdcall;
    procedure SwitchedToPlayerApplication; stdcall;
    procedure SwitchedToControl; stdcall;
    procedure PlayerDockedStateChange; stdcall;
    procedure PlayerReconnect; stdcall;
    procedure Click(nButton: Smallint; nShiftState: Smallint; fX: Integer; fY: Integer); stdcall;
    procedure DoubleClick(nButton: Smallint; nShiftState: Smallint; fX: Integer; fY: Integer); stdcall;
    procedure KeyDown(nKeyCode: Smallint; nShiftState: Smallint); stdcall;
    procedure KeyPress(nKeyAscii: Smallint); stdcall;
    procedure KeyUp(nKeyCode: Smallint; nShiftState: Smallint); stdcall;
    procedure MouseDown(nButton: Smallint; nShiftState: Smallint; fX: Integer; fY: Integer); stdcall;
    procedure MouseMove(nButton: Smallint; nShiftState: Smallint; fX: Integer; fY: Integer); stdcall;
    procedure MouseUp(nButton: Smallint; nShiftState: Smallint; fX: Integer; fY: Integer); stdcall;
  end;

type
  TWMPEvents = class(TNewInterfacedObject, IWMPEvents)
    procedure OpenStateChange(NewState: Integer); virtual; stdcall;
    procedure PlayStateChange(NewState: Integer); virtual; stdcall;
    procedure AudioLanguageChange(LangID: Integer); virtual; stdcall;
    procedure StatusChange; virtual; stdcall;
    procedure ScriptCommand(const scType, Param: WideString); virtual; stdcall;
    procedure NewStream; virtual; stdcall;
    procedure Disconnect(Result: Integer); virtual; stdcall;
    procedure Buffering(Start: WordBool); virtual; stdcall;
    procedure Error; virtual; stdcall;
    procedure Warning(WarningType, Param: Integer;
        const Description: WideString); virtual; stdcall;
    procedure EndOfStream(Result: Integer); virtual; stdcall;
    procedure PositionChange(oldPosition, newPosition: Double); virtual; stdcall;
    procedure MarkerHit(MarkerNum: Integer); virtual; stdcall;
    procedure DurationUnitChange(NewDurationUnit: Integer); virtual; stdcall;
    procedure CdromMediaChange(CdromNum: Integer); virtual; stdcall;
    procedure PlaylistChange(Playlist: IDispatch;
        change: WMPPlaylistChangeEventType); virtual; stdcall;
    procedure CurrentPlaylistChange(change: WMPPlaylistChangeEventType);
        virtual; stdcall;
    procedure CurrentPlaylistItemAvailable(const bstrItemName: WideString);
        virtual; stdcall;
    procedure MediaChange(Item: IDispatch); virtual; stdcall;
    procedure CurrentMediaItemAvailable(const bstrItemName: WideString);
        virtual; stdcall;
    procedure CurrentItemChange(pdispMedia: IDispatch); virtual; stdcall;
    procedure MediaCollectionChange; virtual; stdcall;
    procedure MediaCollectionAttributeStringAdded(const bstrAttribName,
        bstrAttribVal: WideString); virtual; stdcall;
    procedure MediaCollectionAttributeStringRemoved(const bstrAttribName,
        bstrAttribVal: WideString); virtual; stdcall;
    procedure MediaCollectionAttributeStringChanged(const bstrAttribName,
        bstrOldAttribVal, bstrNewAttribVal: WideString); virtual; stdcall;
    procedure PlaylistCollectionChange; virtual; stdcall;
    procedure PlaylistCollectionPlaylistAdded(const bstrPlaylistName: WideString);
        virtual; stdcall;
    procedure PlaylistCollectionPlaylistRemoved(const bstrPlaylistName: WideString);
        virtual; stdcall;
    procedure PlaylistCollectionPlaylistSetAsDeleted(const bstrPlaylistName: WideString;
        varfIsDeleted: WordBool); virtual; stdcall;
    procedure ModeChange(const ModeName: WideString;
        NewValue: WordBool); virtual; stdcall;
    procedure MediaError(pMediaObject: IDispatch); virtual; stdcall;
    procedure OpenPlaylistSwitch(pItem: IDispatch); virtual; stdcall;
    procedure DomainChange(const strDomain: WideString); virtual; stdcall;
    procedure SwitchedToPlayerApplication; virtual; stdcall;
    procedure SwitchedToControl; virtual; stdcall;
    procedure PlayerDockedStateChange; virtual; stdcall;
    procedure PlayerReconnect; virtual; stdcall;
    procedure Click(nButton, nShiftState: Smallint;
        fX, fY: Integer); virtual; stdcall;
    procedure DoubleClick(nButton, nShiftState: Smallint;
        fX, fY: Integer); virtual; stdcall;
    procedure KeyDown(nKeyCode, nShiftState: Smallint); virtual; stdcall;
    procedure KeyPress(nKeyAscii: Smallint); virtual; stdcall;
    procedure KeyUp(nKeyCode, nShiftState: Smallint); virtual; stdcall;
    procedure MouseDown(nButton, nShiftState: Smallint;
        fX, fY: Integer); virtual; stdcall;
    procedure MouseMove(nButton, nShiftState: Smallint;
        fX, fY: Integer); virtual; stdcall;
    procedure MouseUp(nButton, nShiftState: Smallint;
        fX, fY: Integer); virtual; stdcall;
  end;

{ IWMPMedia: Public interface. }

  IWMPMediaSafe = interface(IDispatch)
    ['{94D55E95-3FAC-11D3-B155-00C04F79FAA6}']
    function Get_isIdentical(const pIWMPMedia: IWMPMediaSafe;
        out pvbool: WordBool): HResult; stdcall;
    function Get_sourceURL: WideString; safecall;
    function Get_name: WideString; safecall;
    procedure Set_name(const Value: WideString); safecall;
    function Get_imageSourceWidth: Integer; safecall;
    function Get_imageSourceHeight: Integer; safecall;
    function Get_markerCount: Integer; safecall;
    function getMarkerTime(MarkerNum: Integer): Double; safecall;
    function getMarkerName(MarkerNum: Integer): WideString; safecall;
    function Get_duration: Double; safecall;
    function Get_durationString: WideString; safecall;
    function Get_attributeCount(var plCount: Integer): HResult; stdcall;
    function getAttributeName(lIndex: Integer;
        var pbstrItemName: WideString): HResult; stdcall;
    function getItemInfo(const bstrItemName: WideString;
        var pbstrVal: WideString): HResult; stdcall;
    function setItemInfo(const bstrItemName, bstrVal: WideString): HResult;
        stdcall;
    function getItemInfoByAtom(lAtom: Integer; var pbstrVal: WideString): HResult;
        stdcall;
    function isMemberOf(const pPlaylist: IWMPPlaylist;
        var pvarfIsMemberOf: WordBool): HResult; stdcall;
    function isReadOnlyItem(const bstrItemName: WideString;
        var pvarfIsReadOnly: WordBool): HResult; stdcall;
    property sourceURL: WideString read Get_sourceURL;
    property name: WideString read Get_name write Set_name;
    property imageSourceWidth: Integer read Get_imageSourceWidth;
    property imageSourceHeight: Integer read Get_imageSourceHeight;
    property markerCount: Integer read Get_markerCount;
    property duration: Double read Get_duration;
    property durationString: WideString read Get_durationString;
  end;

{ IWMPMedia2: Public interface. }

  IWMPMedia2Safe = interface(IWMPMediaSafe)
    ['{AB7C88BB-143E-4EA4-ACC3-E4350B2106C3}']
    function Get_Error: IWMPErrorItem; safecall;
    property Error: IWMPErrorItem read Get_Error;
  end;

{ IWMPMedia3: Public interface. }

  IWMPMedia3Safe = interface(IWMPMedia2Safe)
    ['{F118EFC7-F03A-4FB4-99C9-1C02A5C1065B}']
    function getAttributeCountByType(const bstrType, bstrLanguage: WideString;
        var plCount: Integer): HResult; stdcall;
    function getItemInfoByType(const bstrType, bstrLanguage: WideString;
        lIndex: Integer; var pvarValue: OleVariant): HResult; stdcall;
  end;

{ IWMPPlaylist: Public interface. }

  IWMPPlaylistSafe = interface(IDispatch)
    ['{D5F0F4F1-130C-11D3-B14E-00C04F79FAA6}']
    function Get_count: Integer; safecall;
    function Get_name: WideString; safecall;
    procedure Set_name(const Value: WideString); safecall;
    function Get_attributeCount: Integer; safecall;
    function Get_attributeName(lIndex: Integer): WideString; safecall;
    function Get_Item(lIndex: Integer; out ppIWMPMedia: IWMPMediaSafe): HResult;
        stdcall;
    function getItemInfo(const bstrName: WideString;
        var pbstrVal: WideString): HResult; stdcall;
    function setItemInfo(const bstrName, bstrValue: WideString): HResult; stdcall;
    function Get_isIdentical(const pIWMPPlaylist: IWMPPlaylistSafe;
        out pvbool: WordBool): HResult; stdcall;
    procedure clear; safecall;
    procedure insertItem(lIndex: Integer; const pIWMPMedia: IWMPMedia); safecall;
    function appendItem(const pIWMPMedia: IWMPMediaSafe): HResult; stdcall;
    function removeItem(const pIWMPMedia: IWMPMediaSafe): HResult; stdcall;
    procedure moveItem(lIndexOld, lIndexNew: Integer); safecall;
    property count: Integer read Get_count;
    property name: WideString read Get_name write Set_name;
    property attributeCount: Integer read Get_attributeCount;
    property attributeName[lIndex: Integer]: WideString read Get_attributeName;
  end;

{ IWMPPlaylistArray: Public interface. }

  IWMPPlaylistArraySafe = interface(IDispatch)
    ['{679409C0-99F7-11D3-9FB7-00105AA620BB}']
    function Get_count: Integer; safecall;
    function Item(lIndex: Integer; out ppItem: IWMPPlaylistSafe): HResult;
        stdcall;
    property count: Integer read Get_count;
  end;

{ IWMPPlaylistCollection: Public interface. }

  IWMPPlaylistCollectionSafe = interface(IDispatch)
    ['{10A13217-23A7-439B-B1C0-D847C79B7774}']
    function newPlaylist(const bstrName: WideString;
        out ppItem: IWMPPlaylistSafe): HResult; stdcall;
    function getAll(out ppPlaylistArray: IWMPPlaylistArraySafe): HResult; stdcall;
    function getByName(const bstrName: WideString;
        out ppPlaylistArray: IWMPPlaylistArraySafe): HResult; stdcall;
    procedure remove(const pItem: IWMPPlaylistSafe); safecall;
    procedure setDeleted(const pItem: IWMPPlaylistSafe; varfIsDeleted: WordBool);
        safecall;
    function isDeleted(const pItem: IWMPPlaylistSafe): WordBool; safecall;
    function importPlaylist(const pItem: IWMPPlaylistSafe;
        out ppImportedItem: IWMPPlaylistSafe): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IWMPControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {74C09E02-F828-11D2-A74B-00A0C905F36E}
// *********************************************************************//
  IWMPControlsSafe = interface(IDispatch)
    ['{74C09E02-F828-11D2-A74B-00A0C905F36E}']
    function Get_isAvailable(const bstrItem: WideString): WordBool; safecall;
    function play: HResult; stdcall;
    function stop: HResult; stdcall;
    function pause: HResult; stdcall;
    function fastForward: HResult; stdcall;
    function fastReverse: HResult; stdcall;
    function Get_currentPosition: Double; safecall;
    function Set_currentPosition(pdCurrentPosition: Double): HResult; stdcall;
    function Get_currentPositionString: WideString; safecall;
    function next: HResult; stdcall;
    function previous: HResult; stdcall;
    function Get_currentItem: IWMPMediaSafe; safecall;
    function Set_currentItem(const ppIWMPMedia: IWMPMediaSafe): HResult; stdcall;
    function Get_currentMarker: Integer; safecall;
    procedure Set_currentMarker(plMarker: Integer); safecall;
    function playItem(const pIWMPMedia: IWMPMedia): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IWMPControls2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6F030D25-0890-480F-9775-1F7E40AB5B8E}
// *********************************************************************//
  IWMPControls2Safe = interface(IWMPControlsSafe)
    ['{6F030D25-0890-480F-9775-1F7E40AB5B8E}']
    function step(lStep: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IWMPSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9104D1AB-80C9-4FED-ABF0-2E6417A6DF14}
// *********************************************************************//
  IWMPSettingsSafe = interface(IDispatch)
    ['{9104D1AB-80C9-4FED-ABF0-2E6417A6DF14}']
    function Get_isAvailable(const bstrItem: WideString): WordBool; safecall;
    function Get_autoStart: WordBool; safecall;
    function Set_autoStart(pfAutoStart: WordBool): HResult; stdcall;
    function Get_baseURL: WideString; safecall;
    procedure Set_baseURL(const pbstrBaseURL: WideString); safecall;
    function Get_defaultFrame: WideString; safecall;
    procedure Set_defaultFrame(const pbstrDefaultFrame: WideString); safecall;
    function Get_invokeURLs: WordBool; safecall;
    procedure Set_invokeURLs(pfInvokeURLs: WordBool); safecall;
    function Get_mute(out pfMute: WordBool): HResult; stdcall;
    function Set_mute(pfMute: WordBool): HResult; stdcall;
    function Get_playCount: Integer; safecall;
    procedure Set_playCount(plCount: Integer); safecall;
    function Get_rate(out pdRate: Double): HResult; stdcall;
    function Set_rate(dRate: Double): HResult; stdcall;
    function Get_balance: Integer; safecall;
    procedure Set_balance(plBalance: Integer); safecall;
    function Get_volume(out plVolume: Integer): HResult; stdcall;
    function Set_volume(lVolume: Integer): HResult; stdcall;
    function getMode(const bstrMode: WideString;
        out pvarfMode: WordBool): HResult; stdcall;
    function setMode(const bstrMode: WideString; varfMode: WordBool): HResult;
        stdcall;
    function Get_enableErrorDialogs: WordBool; safecall;
    procedure Set_enableErrorDialogs(pfEnableErrorDialogs: WordBool); safecall;
  end;

{ IWMPMediaCollection: Public interface. }

  IWMPMediaCollectionSafe = interface(IDispatch)
    ['{8363BC22-B4B4-4B19-989D-1CD765749DD1}']
    function add(const bstrURL: WideString; out ppItem: IWMPMediaSafe): HResult;
        stdcall;
    function getAll: IWMPPlaylistSafe; safecall;
    function getByName(const bstrName: WideString): IWMPPlaylist; safecall;
    function getByGenre(const bstrGenre: WideString): IWMPPlaylist; safecall;
    function getByAuthor(const bstrAuthor: WideString): IWMPPlaylist; safecall;
    function getByAlbum(const bstrAlbum: WideString): IWMPPlaylist; safecall;
    function getByAttribute(const bstrAttribute, bstrValue: WideString;
        out ppMediaTiems: IWMPPlaylistSafe): HResult; stdcall;
    function remove(const pItem: IWMPMediaSafe;
        varfDeleteFile: WordBool): HResult; stdcall;
    function getAttributeStringCollection(const bstrAttribute,
        bstrMediaType: WideString): IWMPStringCollection; safecall;
    function getMediaAtom(const bstrItemName: WideString): Integer; safecall;
    procedure setDeleted(const pItem: IWMPMedia; varfIsDeleted: WordBool);
        safecall;
    function isDeleted(const pItem: IWMPMedia): WordBool; safecall;
  end;

  IWMPCdromSafe = interface(IDispatch)
    ['{CFAB6E98-8730-11D3-B388-00C04F68574B}']
    function Get_driveSpecifier: WideString; safecall;
    function Get_Playlist(out ppPlaylist: IWMPPlaylistSafe): HResult; stdcall;
    procedure eject; safecall;
  end;

  IWMPCdromCollectionSafe = interface(IDispatch)
    ['{EE4C8FE2-34B2-11D3-A3BF-006097C9B344}']
    function Get_count: Integer; safecall;
    function Item(lIndex: Integer; out ppItem: IWMPCdromSafe): HResult; stdcall;
    function getByDriveSpecifier(const bstrDriveSpecifier: WideString): IWMPCdrom;
        safecall;
    property count: Integer read Get_count;
  end;

{ IWMPCore: Public interface. }

  IWMPCoreSafe = interface(IDispatch)
    ['{D84CCA99-CCE2-11D2-9ECC-0000F8085981}']
    procedure close; safecall;
    function Get_URL: WideString; safecall;
    procedure Set_URL(const Value: WideString); safecall;
    function Get_openState: WMPOpenState; safecall;
    function Get_playState: TWMPPlayState; safecall;
    function Get_controls: IWMPControlsSafe; safecall;
    function Get_settings: IWMPSettingsSafe; safecall;
    function Get_currentMedia: IWMPMediaSafe; safecall;
    procedure Set_currentMedia(const Value: IWMPMediaSafe); safecall;
    function Get_mediaCollection: IWMPMediaCollectionSafe; safecall;
    function Get_playlistCollection: IWMPPlaylistCollectionSafe; safecall;
    function Get_versionInfo: WideString; safecall;
    procedure launchURL(const bstrURL: WideString); safecall;
    function Get_network: IWMPNetwork; safecall;
    function get_currentPlaylist(out ppPL: IWMPPlaylistSafe): HResult; stdcall;
    function Set_currentPlaylist(const Value: IWMPPlaylistSafe): HResult; stdcall;
    function Get_cdromCollection(out ppCdromCollection: IWMPCdromCollectionSafe): HResult;
        stdcall;
    function Get_closedCaption: IWMPClosedCaption; safecall;
    function Get_isOnline: WordBool; safecall;
    function Get_Error: IWMPError; safecall;
    function Get_status: WideString; safecall;
  end;

{ IWMPCore3: Public interface. }

  IWMPCore3Safe = interface(IWMPCore2)
    ['{7587C667-628F-499F-88E7-6A6F4E888464}']
    function newPlaylist(const bstrName, bstrURL: WideString;
        out ppPlaylist: IWMPPlaylistSafe): HResult; stdcall;
    function newMedia(const bstrURL: WideString;
        out ppMedia: IWMPMediaSafe): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IWMPPlayerApplication
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {40897764-CEAB-47BE-AD4A-8E28537F9BBF}
// *********************************************************************//
  IWMPPlayerApplicationSafe = interface(IDispatch)
    ['{40897764-CEAB-47BE-AD4A-8E28537F9BBF}']
    procedure switchToPlayerApplication; safecall;
    procedure switchToControl; safecall;
    function Get_playerDocked(out pbPlayerDocked: WordBool): HResult; stdcall;
    function Get_hasDisplay: WordBool; safecall;
  end;

function GetLastPlayedPlaylistFilename: String;
function GetLastPlayedPlaylist(Core: IWMPCore3): IWMPPlaylistSafe;
function GetMediaTitle(Media: IWMPMediaSafe): WideString;
function GetWMPPlaylistFilename(Playlist: IWMPPlaylistSafe;
    var Filename: WideString): Boolean;

type
  TWMPPlaylistArray = array of IWMPPlaylistSafe;

function FindWMPPlaylist(Core: IWMPCoreSafe; const Name, Filename: WideString;
    DoSlowSearch: Boolean): TWMPPlaylistArray;

function IsAutoPlaylist(Playlist: IWMPMediaSafe): Boolean;

type
  TWMPMediaArray = array of IWMPMediaSafe;

function GetWMPMetaInt(Media: IWMPMedia3Safe; const AttrName: String;
    var Value: Integer): Boolean;

function GetWMPMediaType(Item: IWMPMediaSafe): TWMPMediaType;
function CompareWMPMediaURLs(Media1, Media2: IWMPMediaSafe): Integer;
function GetWMPItemsByMediaType(MediaCollection: IWMPMediaCollectionSafe;
    MediaType: TWMPMediaType): IWMPPlaylistSafe;
function GetWMPItemByFilename(MediaCollection: IWMPMediaCollectionSafe;
    const Filename: WideString): IWMPMediaSafe;
function GetWMPItemByTrackingID(MediaCollection: IWMPMediaCollectionSafe;
    const TrackingID: WideString): IWMPMediaSafe;
function WMPMediaItemsAreIdentical(Item: IDispatch;
    Media2: IWMPMediaSafe): Boolean;

function WMErrorMessage(ErrorCode: Cardinal): String;

type
  TWMPBoolSetting = (
    wmpbsFlushMetadata, // introduced in Win10
    wmpbsFlushRating
  );

function GetWMPBoolSetting(Setting: TWMPBoolSetting): Boolean;
procedure SetWMPBoolSetting(Setting: TWMPBoolSetting; Value: Boolean);

function IsWMPCDUrl(const URL: String): Boolean;
function IsWMPDVDUrl(const URL: String): Boolean;
function ParseWMPCDUrl(const URL: String;
    var DriveIdx, TrackNum: Integer): Boolean;

procedure WMPPlayOrPause(Core: IWMPCoreSafe);

function GetWMPAppInternal(Core: IWMPCoreSafe): IWMPPlayerAppInternal;
function GetWMPRemoteApps(Core: IWMPCoreSafe; var Count: Integer;
    pNames: PStringArray): Boolean;

function IsWMPBurning(Core: IWMPCoreSafe): Boolean;
function IsWMPRipping(Core: IWMPCoreSafe): Boolean;

function WMPNavigateToSingleMedia(WMPAppDispatch: IDispatch; WMPVersion: Byte;
    Media: IWMPMedia3Safe): Boolean;
  
implementation

uses CmnFunc2, PathFunc, PathFuncWide, Scanf;

type
  TFindWindowInfo = record
    ClassName: PChar;
    WindowTitle: PChar;
    ExStyle: Integer;
    Result: HWND;
  end;
  PFindWindowInfo = ^TFindWindowInfo;

function FindWinInCurrentProcessProc(hWnd: HWND;
    pInfo: PFindWindowInfo): BOOL; stdcall;
var
  Buffer: array[0..MAX_PATH] of Char;
  ExStyle: Integer;
  ProcID: DWORD;
begin
  Result := True;
  GetClassName(hWnd, Buffer, SizeOf(Buffer));
  if lstrcmpi(Buffer, pInfo.ClassName) = 0 then begin
    if pInfo.ExStyle <> 0 then begin
      ExStyle := GetWindowLong(hWnd, GWL_EXSTYLE);
      if (ExStyle and pInfo.ExStyle) = 0 then
        Exit;
    end;
    GetWindowThreadProcessId(hwnd, @ProcID);
    if ProcID = GetCurrentProcessId then begin
      pInfo.Result := hWnd;
      Result := False;
    end;
  end;
end;

function FindWindowInCurrentProcess(const ClassName: String;
    ExStyle: Integer): HWND;
var
  Info: TFindWindowInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.ClassName := PChar(ClassName);
  Info.ExStyle := ExStyle;
  EnumWindows(@FindWinInCurrentProcessProc, Integer(@Info));
  Result := Info.Result;
end;

function GetWMPlayerAppWindow: HWND;
begin
  Result := FindWindowInCurrentProcess(WMPlayerAppClass, 0);
end;

function IsMainWMPWindow(hWnd: HWND; pClassName: PChar): Boolean;
var
  ClassName: array[0..MAX_PATH] of Char;
begin
  if not Assigned(pClassName) then begin
    GetClassName(hWnd, ClassName, SizeOf(ClassName));
    pClassName := @ClassName;
  end;
  Result := lstrcmpi(pClassName, WMPlayerAppClass) = 0;
end;

function GetWMPNowPlayingWindow: HWND;
begin
  // this function should be kept in sync with IsWMPNowPlayingWindow!
  // there can be multiple WMP Skin Host windows, only get the one that is shown
  // in the taskbar, i.e. with WS_EX_APPWINDOW set
  Result := FindWindowInCurrentProcess(WMPSkinHostClass, WS_EX_APPWINDOW);
end;

function IsWMPNowPlayingWindow(hWnd: HWND; pClassName: PChar): Boolean;
var
  ClassName: array[0..MAX_PATH] of Char;
  WndStyle, WndExStyle: Integer;
begin
  // this function should be kept in sync with GetWMPNowPlayingWindow!
  Result := False;
  if not Assigned(pClassName) then begin
    GetClassName(hWnd, ClassName, SizeOf(ClassName));
    pClassName := @ClassName;
  end;
  if lstrcmpi(pClassName, WMPSkinHostClass) = 0 then begin
    WndStyle := GetWindowLong(hWnd, GWL_STYLE);
    WndExStyle := GetWindowLong(hWnd, GWL_EXSTYLE);
    // there can be multiple WMP Skin Host windows, only get the one that is
    // shown in the taskbar, i.e. with WS_EX_APPWINDOW set
    Result := ((WndStyle and WS_CHILD) = 0)
        and ((WndExStyle and WS_EX_APPWINDOW) <> 0);
  end;
end;

function GetWMPModalhWndParent(WMPlayerAppWnd, WMPNowPlayingWnd: HWND): HWND;
begin
  if (WMPNowPlayingWnd <> 0) and IsWindow(WMPNowPlayingWnd)
      and IsWindowVisible(WMPNowPlayingWnd) then
    Result := WMPNowPlayingWnd
  else
    Result := WMPlayerAppWnd;
end;

const
  AtlClassName = 'ATL:';

function FindAtlWindowProc(hWnd: HWND; pInfo: PFindWindowInfo): BOOL; stdcall;
var
  ClassName: array[0..Length(AtlClassName)] of Char;
  WinText: String;
begin
  Result := True;
  GetClassName(hWnd, ClassName, SizeOf(ClassName));
  if lstrcmpi(ClassName, AtlClassName) = 0 then begin
    WinText := WindowText(hWnd);
    if lstrcmpi(PChar(WinText), pInfo.WindowTitle) = 0 then begin
      pInfo.Result := hWnd;
      Result := False;
    end;
  end;
end;

function FindAtlWindow(hwndParent: HWND; const WindowTitle: String): Integer;
var
  Info: TFindWindowInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.WindowTitle := PChar(WindowTitle);
  EnumChildWindows(hwndParent, @FindAtlWindowProc, Integer(@Info));
  Result := Info.Result;
end;

procedure GetWMPLocalizedCaptions(hWMPLoc: HMODULE;
    var Captions: TWMPLocalizedCaptions);
var
  Caption: TWMPLocalizedCaption;
begin
  for Caption := Low(Captions) to High(Captions) do begin
    if hWMPLoc <> 0 then
      Captions[Caption] := LoadResString(hWMPLoc, WMPLocalizedCaptionIDs[Caption]);
    if (hWMPLoc = 0) or (Trim(Captions[Caption]) = '') then
      Captions[Caption] := WMPDefaultLocalizedCaptions[Caption];
  end;
end;

function GetWMPLibraryContainer(WMPlayerAppWnd: HWND): HWND;
begin
  Result := FindWindowEx(WMPlayerAppWnd, 0, WMPAppHostClass, nil);
  if Result = 0 then
    Exit;
  Result := FindWindowEx(Result, 0, WMPSkinHostClass, nil);
  if Result = 0 then
    Exit;
  Result := FindWindowEx(Result, 0, nil, LibraryContainerCaption);
end;

function GetWMPPrimaryListView(WMPlayerAppWnd: HWND): HWND;
begin
  Result := GetWMPLibraryContainer(WMPlayerAppWnd);
  if Result = 0 then
    Exit;
  Result := FindWindowEx(Result, 0, WC_LISTVIEW, PrimaryListViewCaption);
end;

function GetWMPBasketListView(WMPlayerAppWnd: HWND): HWND;
begin
  Result := GetWMPLibraryContainer(WMPlayerAppWnd);
  if Result = 0 then
    Exit;
  Result := FindWindowEx(Result, 0, WC_LISTVIEW, BasketListViewCaption);
end;

function IsWMPPrimaryListView(hWnd: HWND): Boolean;
var
  Text, ClassName: String;
begin
  WindowTextAndClassName(hWnd, Text, ClassName);
  Result := (lstrcmpi(PChar(Text), PrimaryListViewCaption) = 0) and // WMP 11
      (lstrcmpi(PChar(ClassName), WC_LISTVIEW) = 0);
end;

function IsWMPBasketListView(hWnd: HWND): Boolean;
var
  Text, ClassName: String;
begin
  WindowTextAndClassName(hWnd, Text, ClassName);
  Result := (lstrcmpi(PChar(Text), BasketListViewCaption) = 0) and // WMP 11
      (lstrcmpi(PChar(ClassName), WC_LISTVIEW) = 0);
end;

function IsWMPMainMenuVisible(WMPlayerAppWnd: HWND): Boolean;
var
  hRebarWindow: HWND;
begin
  Result := False;
  hRebarWindow := FindWindowEx(WMPlayerAppWnd, 0, RebarWindowClass, nil);
  if hRebarWindow = 0 then
    Exit;
  Result := IsWindowVisible(hRebarWindow);
end;

function IsMainWMPWindowActive(WMPlayerAppWnd: HWND;
    var FullScreen: Boolean): Boolean;
var
  hActiveWnd: HWND;
  ClassName: String;
begin
  hActiveWnd := GetActiveWindow;
  FullScreen := False;
  if hActiveWnd = WMPlayerAppWnd then
    Result := True
  else begin
    ClassName := WindowClassName(hActiveWnd);
    FullScreen := (CompareText(ClassName, WMPFullScreenClass) = 0)
        // the following is true in WMP 12 when the full-screen controls are focused
        or (CompareText(ClassName, WMPControlContainerClass) = 0);
    // don't just check for fPlugin.WMPNowPlayingWnd - in WMP 12, the play controls
    // in Now Playing mode seem to be in a different window
    Result := FullScreen or (CompareText(ClassName, WMPSkinHostClass) = 0);
  end;
end;

procedure CloseWMP(WMPlayerAppWnd: HWND);
begin
  PostMessage(WMPlayerAppWnd, WM_CLOSE, 0, 0);
end;

function IsEditControl(hWnd: HWND): Boolean;
const
  AtlEditClass = 'Atl:Edit';
  InternetExplorerServerClass = 'Internet Explorer_Server';
var
  ClassName: array[0..MAX_PATH] of Char;
begin
  // RealGetWindowClass doesn't seem to return anything different than
  // GetClassName, but use it anyway
  RealGetWindowClass(hWnd, ClassName, Length(ClassName));
  Result := (lstrcmpi(ClassName, WC_EDIT) = 0)
      or (lstrcmpi(ClassName, AtlEditClass) = 0)
      // ignore keypresses in Media Guide
      or (lstrcmpi(ClassName, InternetExplorerServerClass) = 0);
end;

function WMPRatingToStars(WMPRating: Integer): Byte;
begin
  case WMPRating of
    1..12:  Result := 1;
    13..37: Result := 2;
    38..62: Result := 3;
    63..86: Result := 4;
    87..99: Result := 5;
    else
      Result := 0;
  end;
end;

function StarsToWMPRating(Stars: Byte): Byte;
const
  RatingValues: array[0..5] of Byte =
      (0, 1, 25, 50, 75, 99);
begin
  if Stars <= High(RatingValues) then
    Result := RatingValues[Stars]
  else
    Result := 0;
end;

procedure SplitPartOfSet(const PartOfSet: String; var Disc, TotalDiscs: Integer);
var
  IndexPart, TotalPart: String;
  DividerIdx: Integer;
  Temp: Integer;
begin
  if Trim(PartOfSet) = '' then
    Exit;
  DividerIdx := Pos('/', PartOfSet);
  if DividerIdx = 0 then
    DividerIdx := Length(PartOfSet) + 1;
  IndexPart := Copy(PartOfSet, 1, DividerIdx - 1);
  Temp := StrToIntDef(IndexPart, -1);
  if Temp >= 0 then
    Disc := Temp;
  if DividerIdx < Length(PartOfSet) then begin
    TotalPart := PartOfSet;
    Delete(TotalPart, 1, DividerIdx);
    Temp := StrToIntDef(TotalPart, -1);
    if Temp >= 0 then
      TotalDiscs := Temp;
  end;
end;

function WMPNavigateToAddress(WMPAppDispatch: IDispatch;
    const Address: WideString): Boolean;
var
  VarWMPAppDispatch: OleVariant;
begin
  // interfaces are different between WMP versions, so call navigateToAddress
  // through IDispatch as OleVariant
  VarWMPAppDispatch := WMPAppDispatch;
  // navigateToAddress may sometimes throw an EOleException, such as being called
  // when closing WMP. Make sure that such exceptions are handled somewhere,
  // otherwise WMP could crash!
  VarWMPAppDispatch.navigateToAddress(Address);
  Result := True;
end;

// In the following constants, %s is always a placeholder for the media type
// (e.g. "Music")
const
  WMPLocalLibraryAddress = '\MediaLibrary\LocalLibrary\%s';
  WMPRootLibraryAddress11 = '\MediaLibrary\%s';
  WMPLocalLibraryAddress11 = WMPRootLibraryAddress11 + '\LocalLibrary';
  WMPPlaylistsLibraryAddress11 = WMPRootLibraryAddress11 + '\Playlists';
  WMPLibraryAddressLibraryTypes: array[TWMPMediaType] of WideString = (
    'Music', '', '', '', '', 'Other', 'Pictures', 'Playlists', '', 'Video', '');
  WMPLibraryWordWheelGUID = '95ca482e-7442-463e-8df7-fb71f3607ff7';

function WMPNavigateToLibraryCategory(WMPAppDispatch: IDispatch; WMPVersion: Byte;
    MediaType: TWMPMediaType; Category: TWMPLibraryCategory;
    const CategoryValue: WideString; const WordWheelText: WideString = ''): Boolean;
var
  LocalLibraryAddress, Address: WideString;
begin
  if (MediaType = wmpmtPlaylist)
      or (WMPLibraryAddressLibraryTypes[MediaType] = '') then begin
    raise EWMPNavigateException.CreateFmt('Invalid media type: %u',
        [Cardinal(MediaType)]);
  end;
  if WMPVersion > 11 then
    LocalLibraryAddress := WMPLocalLibraryAddress
  else
    LocalLibraryAddress := WMPLocalLibraryAddress11;
  if Category = wmplcPlaylists then begin
    if WMPVersion > 11 then begin
      // Here, there's a single 'playlists' location for all media types, so we
      // ignore MediaType
      Address := RealWideFormat(LocalLibraryAddress,
          [WMPLibraryAddressLibraryTypes[wmpmtPlaylist]]);
    end
    else begin
      Address := RealWideFormat(WMPPlaylistsLibraryAddress11,
          [WMPLibraryAddressLibraryTypes[MediaType]]);
    end;
  end
  else begin
    Address := RealWideFormat(LocalLibraryAddress + '\%s',
        [WMPLibraryAddressLibraryTypes[MediaType],
         WMPLibraryCategoryStrs[Category]]);
  end;
  // ignore CategoryValue when navigating to 'all tracks'
  if (Category <> wmplcAllTracks) and (CategoryValue <> '') then begin
    // escape backslashes in CategoryValue
    Address := Address + '\' + WideEscapeChars(CategoryValue, ['\'], '\');
  end;
  if WordWheelText <> '' then begin
    Address := Address + RealWideFormat('\%s!%s',
        [WMPLibraryWordWheelGUID, WordWheelText]);
  end;
  Result := WMPNavigateToAddress(WMPAppDispatch, Address);
end;

function WMPNavigateToSingleMedia(WMPAppDispatch: IDispatch; WMPVersion: Byte;
    Media: IWMPMedia3Safe): Boolean;
var
  TrackingID: WideString;
begin
  Result := False;
  if not Succeeded(Media.getItemInfo(WMPAttributes[wmpaTrackingID].Name,
      TrackingID)) then
    Exit;
  Result := WMPNavigateToLibraryCategory(WMPAppDispatch, WMPVersion,
      GetWMPMediaType(Media), wmplcAllTracks, '', 'trackingid:' + TrackingID);
end;

function WMPGetItemInfo(Media: IWMPMedia;
    const bstrItemName: WideString): WideString;
begin
  Result := '';
  try
    Result := Media.getItemInfo(bstrItemName);
  except
  end;
end;

function GetWMPMetaInt(Media: IWMPMedia3Safe; const AttrName: String;
    var Value: Integer): Boolean;
var
  OleValue: OleVariant;
begin
  if Succeeded(Media.getItemInfoByType(AttrName, '', 0, OleValue)) then try
    Value := OleValue;
    Result := True;
    Exit;
  except
    on EVariantError do // nothing
  end;
  Result := False;
end;

function GetWMPMediaType(Item: IWMPMediaSafe): TWMPMediaType;
var
  MediaTypeStr: WideString;
  MediaType: TWMPMediaType;
  SourceURL: String;
begin
  Result := wmpmtUnknown;
  // check for a WMPCD/WMPDVD sourceURL first - the media type of these media
  // items is sometimes empty, sometimes 'audio' (CD), or sometimes 'photo' (DVD)
  SourceURL := Item.Get_sourceURL;
  if IsWMPCDUrl(SourceURL) then
    Result := wmpmtCDTrack
  else if IsWMPDVDUrl(SourceURL) then
    Result := wmpmtDVD
  else begin
    if Succeeded(Item.getItemInfo(WMPAttributes[wmpaMediaType].Name,
        MediaTypeStr))
        and (MediaTypeStr <> '') then begin
      for MediaType := Low(TWMPMediaType) to High(TWMPMediaType) do begin
        if lstrcmpiW(PWideChar(MediaTypeStr),
            WMPMediaTypes[MediaType]) = 0 then begin
          Result := MediaType;
          Break;
        end;
      end;
    end;
  end;
end;

function CompareWMPMediaURLs(Media1, Media2: IWMPMediaSafe): Integer;
begin
  if not Assigned(Media1) and not Assigned(Media2) then
    Result := 0
  else if not Assigned(Media1) then
    Result := -1
  else if not Assigned(Media2) then
    Result := 1
  else begin
    // comparing the raw pointers doesn't work, so compare the filenames instead
    Result := WidePathCompare(Media1.sourceURL, Media2.sourceURL);
  end;
end;

function GetWMPItemsByMediaType(MediaCollection: IWMPMediaCollectionSafe;
    MediaType: TWMPMediaType): IWMPPlaylistSafe;
var
  MediaTypeStr: PWideChar;
begin
  MediaTypeStr := WMPMediaTypes[MediaType];
  if MediaTypeStr = '' then
    raise Exception.Create('Invalid media type');
  if not Succeeded(MediaCollection.getByAttribute(WMPAttributes[wmpaMediaType].Name,
      MediaTypeStr, Result)) then
    Result := nil;
end;

function GetWMPItemByFilename(MediaCollection: IWMPMediaCollectionSafe;
    const Filename: WideString): IWMPMediaSafe;
var
  hr: HResult;
  Playlist: IWMPPlaylistSafe;
begin
  Result := nil;
  hr := MediaCollection.getByAttribute(WMPAttributes[wmpaSourceURL].Name,
      Filename, Playlist);
  if Succeeded(hr) and (Playlist.count > 0) then
    Playlist.Get_Item(0, Result);
end;

function GetWMPItemByTrackingID(MediaCollection: IWMPMediaCollectionSafe;
    const TrackingID: WideString): IWMPMediaSafe;
var
  Playlist: IWMPPlaylistSafe;
begin
  Result := nil;
  if Succeeded(MediaCollection.getByAttribute(WMPAttributes[wmpaTrackingID].Name,
      TrackingID, Playlist)) and (Playlist.count > 0) then
    Playlist.Get_Item(0, Result);
end;

function WMPMediaItemsAreIdentical(Item: IDispatch;
    Media2: IWMPMediaSafe): Boolean;
var
  Media: IWMPMediaSafe;
  IsIdentical: WordBool;
begin
  if Assigned(Item) then begin
    Media := Item as IWMPMediaSafe;
    Result := Succeeded(Media.Get_isIdentical(Media2, IsIdentical))
        and IsIdentical;
  end
  else
    Result := False;
end;

function GetWMPMainVersion(const Version: WideString): Byte;
var
  Idx: Integer;
begin
  Idx := Pos('.', Version);
  if Idx > 0 then
    Result := StrToIntDef(Copy(Version, 0, Idx - 1), 0)
  else
    Result := 0;
end;

const
  LibraryPath = 'Microsoft\Media Player';

function GetWMPLibraryPath: String;
var
  LocalAppDataPath: String;
begin
  LocalAppDataPath := GetShellFolderByCSIDL(CSIDL_LOCAL_APPDATA, True);
  Result := AddBackSlash(LocalAppDataPath) + LibraryPath;
  if not DirExists(Result) then
    Result := ''
  else
    Result := RemoveBackslashUnlessRoot(Result);
end;

function GetLastPlayedPlaylistFilename: String;
begin
  Result := AddBackSlash(GetWMPLibraryPath) + LastPlayedFilename;
end;

function GetLastPlayedPlaylist(Core: IWMPCore3): IWMPPlaylistSafe;
begin
  try
    Result := Core.newPlaylist('', GetLastPlayedPlaylistFilename)
        as IWMPPlaylistSafe;
  except
    on E: EOleError do
      Result := nil;
  end;
end;

function GetMediaTitle(Media: IWMPMediaSafe): WideString;
var
  Author: WideString;  
begin
  Result := Media.name;
  if Succeeded(Media.getItemInfo(WMPAttributes[wmpaAuthor].Name, Author))
      and (WideTrim(Author) <> '') then
    Result := Author + ' - ' + Result;
end;

procedure InitFakeWString(var WString: TFakeWString);
begin
  FillChar(WString, SizeOf(WString), 0);
  WString.AllocationType := 4; // tell WMP to use SysAllocString
end;

procedure FreeFakeWString(var WString: TFakeWString);
begin
  if Assigned(WString.Str) then
    SysFreeString(WString.Str);
  WString.Str := nil;
end;

function GetWMPPlaylistFilename(Playlist: IWMPPlaylistSafe;
    var Filename: WideString): Boolean;
var
  PlaylistInternal: IWMPPlaylistInternal;
  URL: TFakeWString;
begin
  Result := False;
  Filename := '';
  if Succeeded(Playlist.QueryInterface(IWMPPlaylistInternal,
      PlaylistInternal)) then begin
    // IWMPPlaylistInternal::GetURL expects a pointer to an internal WString
    // object, which consists of 3 successive stack locations (with the first
    // containing the actual PWideChar).
    // So pass a record, with enough buffer space to prevent GetURL from reading
    // bogus stack data.
    InitFakeWString(URL);
    try
      Result := Succeeded(PlaylistInternal.GetURL(@URL));
      if Result and Assigned(URL.Str) then
        SetString(Filename, URL.Str, StrLenW(URL.Str));
    finally
      FreeFakeWString(URL);
    end;
  end;
end;

function FindWMPPlaylist(Core: IWMPCoreSafe; const Name, Filename: WideString;
    DoSlowSearch: Boolean): TWMPPlaylistArray;

  function FindPlaylistByFilename(PLArray: IWMPPlaylistArraySafe;
      const Filename: WideString; var Idx: Integer): Boolean;
  var
    i: Integer;
    CurPlaylist: IWMPPlaylistSafe;
    CurFilename: WideString;
  begin
    Result := True;
    for i := 0 to PLArray.count - 1 do begin
      if not Succeeded(PLArray.Item(i, CurPlaylist)) then
        Continue;
      if not GetWMPPlaylistFilename(CurPlaylist, CurFilename) then begin
        if i = 0 then begin
          Result := False;
          Exit;
        end;
      end
      else if WidePathCompare(CurFilename, Filename) = 0 then begin
        Idx := i;
        Exit;
      end;
    end;
  end;

  function FindPlaylistByFilenameAsMedia(const Filename: WideString;
      var Media: IWMPMediaSafe): Boolean;
  var
    Results: IWMPPlaylistSafe;
    i: Integer;
    CurMedia: IWMPMediaSafe;
  begin
    Result := False;
    if not Succeeded(Core.Get_mediaCollection.getByAttribute(WMPAttributes[wmpaSourceURL].Name,
        Filename, Results)) then
      Exit;
    for i := 0 to Results.count - 1 do begin
      if not Succeeded(Results.Get_Item(i, CurMedia)) then
        Continue;
      if GetWMPMediaType(CurMedia) = wmpmtPlaylist then begin
        Media := CurMedia;
        Result := True;
        Exit;
      end;
    end;
  end;

var
  hr: HResult;
  Playlists: IWMPPlaylistArraySafe;
  Playlist: IWMPPlaylistSafe;
  i: Integer;
  CorrectPlaylistIdx: Integer;
  PlaylistAsMedia: IWMPMediaSafe;
begin
  SetLength(Result, 0);
  // getAll and getByName can fail, e.g. when WMP can't create the library
  // database folder in appdata
  if Name = '' then
    hr := Core.Get_playlistCollection.getAll(Playlists)
  else
    hr := Core.Get_playlistCollection.getByName(Name, Playlists);
  if not Succeeded(hr) then
    Exit;
  CorrectPlaylistIdx := -1;
  if Filename <> '' then begin
    // optimization: search list of exact name matches first
    FindPlaylistByFilename(Playlists, Filename, CorrectPlaylistIdx);
    if DoSlowSearch
        and (CorrectPlaylistIdx = -1) // no playlist with matching filename found yet
        and (Name <> '') then begin // if Name is empty, we already searched all playlists
      // optimization: search IWMPMediaCollection first (much faster), and use
      // corrected playlist name
      if FindPlaylistByFilenameAsMedia(Filename, PlaylistAsMedia) then begin
        Result := FindWMPPlaylist(Core, PlaylistAsMedia.name,
            PlaylistAsMedia.sourceURL, False);
        Exit;
      end;
    end;
  end;
  if CorrectPlaylistIdx > -1 then begin
    if Succeeded(Playlists.Item(CorrectPlaylistIdx, Playlist)) then begin
      SetLength(Result, 1);
      Result[0] := Playlist;
    end;
  end
  else if not DoSlowSearch or (Filename = '') then begin
    for i := 0 to Playlists.count - 1 do begin
      if Succeeded(Playlists.Item(i, Playlist)) then begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Playlist;
      end;
    end;
  end
end;

function IsAutoPlaylist(Playlist: IWMPMediaSafe): Boolean;
var
  SecondaryID, AutoPlaylistGUID: WideString;
begin
  Result := False;
  if Succeeded(Playlist.getItemInfo(WMPAttributes[wmpaWMMediaClassSecondaryID].Name,
      SecondaryID)) then begin
    AutoPlaylistGUID := GUIDToString(MediaClassSecondaryID_AutoPlaylist);
    Result := lstrcmpiW(PWideChar(SecondaryID), PWideChar(AutoPlaylistGUID)) = 0;
  end;
end;

function WMErrorMessage(ErrorCode: Cardinal): String;
const
  UseWMErrorFlag: array[Boolean] of Cardinal =
      (0, FORMAT_MESSAGE_FROM_HMODULE);
var
  hWMError: HMODULE;
  WMErrorLoaded: Boolean;
  Buffer: PChar;
  Res: Integer;
begin
  Result := '';
  hWMError := Common2.SafeLoadLibrary(WMErrorDll);
  WMErrorLoaded := (hWMError <> 0);
  try
    Buffer := nil;
    Res := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER
        or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY
        or UseWMErrorFlag[WMErrorLoaded],
        Pointer(hWMError), ErrorCode, 0, @Buffer, 0, nil);
    if Res <> 0 then try
      Result := Buffer;
      SetLength(Result, Res);
    finally
      LocalFree(Integer(Buffer));
    end;
  finally
    if WMErrorLoaded then
      FreeLibrary(hWMError);
  end;
end;

const
  WMPBoolSettingValues: array[TWMPBoolSetting] of String = (
    'FlushMetadataToFiles', 'FlushRatingsToFiles');
  WMPBoolSettingDefaults: array[TWMPBoolSetting] of Boolean = (
    False, True);

function GetWMPBoolSetting(Setting: TWMPBoolSetting): Boolean;
begin
  with TMyRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(WMPPreferencesRegKey) then begin
      Result := ReadBoolDef(WMPBoolSettingValues[Setting],
          WMPBoolSettingDefaults[Setting])
    end
    else
      Result := WMPBoolSettingDefaults[Setting];
  finally
    Free;
  end;
end;

procedure SetWMPBoolSetting(Setting: TWMPBoolSetting; Value: Boolean);
begin
  with TMyRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(WMPPreferencesRegKey, True) then
      WriteBool(WMPBoolSettingValues[Setting], Value)
  finally
    Free;
  end;
end;

const
  WMPCDPrefix = 'wmpcd://';
  WMPDVDPrefix = 'wmpdvd://';
  WMPCDFormat = 'wmpcd://%d/%d';

function IsWMPCDUrl(const URL: String): Boolean;
begin
  Result := IsPrefix(URL, WMPCDPrefix);
end;

function IsWMPDVDUrl(const URL: String): Boolean;
begin
  Result := IsPrefix(URL, WMPDVDPrefix);
end;

function ParseWMPCDUrl(const URL: String;
    var DriveIdx, TrackNum: Integer): Boolean;
begin
  Result := (DeFormat(URL, WMPCDFormat, [@DriveIdx, @TrackNum]) = 2);
end;

procedure WMPPlayOrPause(Core: IWMPCoreSafe);
begin
  if Core.Get_playState = wmppsPlaying then
    Core.Get_controls.pause
  else
    Core.Get_controls.play;
end;

function GetWMPAppInternal(Core: IWMPCoreSafe): IWMPPlayerAppInternal;
var
  CoreExternal: IWMPCoreExternal;
  Player: IUnknown;
begin
  Result := nil;
  if not Succeeded(Core.QueryInterface(IWMPCoreExternal, CoreExternal)) then
    Exit;
  if not Succeeded(CoreExternal.GetPlayerApplicationObject(Player)) then
    Exit;
  if not Assigned(Player) then
    Exit;
  if not Succeeded(Player.QueryInterface(IWMPPlayerAppInternal, Result)) then
    Exit;
end;

function GetWMPRemoteApps(Core: IWMPCoreSafe; var Count: Integer;
    pNames: PStringArray): Boolean;
var
  AppInternal: IWMPPlayerAppInternal;
  i: Integer;
  pRemoteApp: Pointer;
  RemoteApp: IWMPRemoteApp;
  Name: WideString;
begin
  Result := False;
  if Assigned(pNames) then
    SetLength(pNames^, 0);
  Count := 0;
  AppInternal := GetWMPAppInternal(Core);
  if not Assigned(AppInternal) then
    Exit;
  if not Succeeded(AppInternal.GetRemoteAppCount(Count)) then
    Exit;
  if not Assigned(pNames) then begin
    Result := True;
    Exit;
  end;
  for i := 0 to Count - 1 do begin
    // contrary to COM rules, GetRemoteAppByIndex doesn't call AddRef on the
    // returned interface pointer, hence the pointer type and cast to IUnknown
    // (to bypass Delphi's automatic reference counting)
    if not Succeeded(AppInternal.GetRemoteAppByIndex(i, pRemoteApp)) then
      Continue;
    if not Assigned(pRemoteApp) then
      Continue;
    if not Succeeded(IUnknown(pRemoteApp).QueryInterface(IWMPRemoteApp,
        RemoteApp)) then
      Continue;
    if not Succeeded(RemoteApp.GetName(Name)) then
      Continue;
    AddToStringArray(pNames^, Name);
  end;
  Result := True;
end;

function IsWMPBurning(Core: IWMPCoreSafe): Boolean;
const
  ActiveBurnStates =
      [wmpbsPreparingToBurn, wmpbsBurning, wmpbsErasing];
var
  CdromCollection: IWMPCdromCollectionSafe;
  CdromCount: Integer;
  i: Integer;
  Cdrom: IWMPCdromSafe;
  CdromBurn: IWMPCdromBurn;
  BurnState: TOleEnum;
begin
  Result := False;
  if not Succeeded(Core.Get_cdromCollection(CdromCollection))
      or not Assigned(CdromCollection) then
    Exit;
  CdromCount := CdromCollection.count;
  for i := 0 to CdromCount - 1 do begin
    if not Succeeded(CdromCollection.Item(i, Cdrom)) or not Assigned(Cdrom) then
      Continue;
    if Succeeded(Cdrom.QueryInterface(IWMPCdromBurn, CdromBurn)) then begin
      if Succeeded(CdromBurn.Get_burnState(BurnState))
          and (BurnState in ActiveBurnStates) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function IsWMPRipping(Core: IWMPCoreSafe): Boolean;
var
  CdromCollection: IWMPCdromCollectionSafe;
  CdromCount: Integer;
  i: Integer;
  Cdrom: IWMPCdromSafe;
  CdromRip: IWMPCdromRip;
  RipState: TOleEnum;
begin
  Result := False;
  if not Succeeded(Core.Get_cdromCollection(CdromCollection))
      or not Assigned(CdromCollection) then
    Exit;
  CdromCount := CdromCollection.count;
  for i := 0 to CdromCount - 1 do begin
    if not Succeeded(CdromCollection.Item(i, Cdrom)) or not Assigned(Cdrom) then
      Continue;
    if Succeeded(Cdrom.QueryInterface(IWMPCdromRip, CdromRip)) then begin
      if Succeeded(CdromRip.Get_ripState(RipState))
          and (RipState = wmprsRipping) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{ TWMPEvents }

procedure TWMPEvents.OpenStateChange(NewState: Integer); stdcall;
begin
end;

procedure TWMPEvents.PlayStateChange(NewState: Integer); stdcall;
begin
end;

procedure TWMPEvents.AudioLanguageChange(LangID: Integer); stdcall;
begin
end;

procedure TWMPEvents.StatusChange; stdcall;
begin
end;

procedure TWMPEvents.ScriptCommand(const scType, Param: WideString); stdcall;
begin
end;

procedure TWMPEvents.NewStream; stdcall;
begin
end;

procedure TWMPEvents.Disconnect(Result: Integer); stdcall;
begin
end;

procedure TWMPEvents.Buffering(Start: WordBool); stdcall;
begin
end;

procedure TWMPEvents.Error; stdcall;
begin
end;

procedure TWMPEvents.Warning(WarningType, Param: Integer;
    const Description: WideString); stdcall;
begin
end;

procedure TWMPEvents.EndOfStream(Result: Integer); stdcall;
begin
end;

procedure TWMPEvents.PositionChange(oldPosition, newPosition: Double); stdcall;
begin
end;

procedure TWMPEvents.MarkerHit(MarkerNum: Integer); stdcall;
begin
end;

procedure TWMPEvents.DurationUnitChange(NewDurationUnit: Integer); stdcall;
begin
end;

procedure TWMPEvents.CdromMediaChange(CdromNum: Integer); stdcall;
begin
end;

procedure TWMPEvents.PlaylistChange(Playlist: IDispatch;
    change: WMPPlaylistChangeEventType); stdcall;
begin
end;

procedure TWMPEvents.CurrentPlaylistChange(change: WMPPlaylistChangeEventType);
    stdcall;
begin
end;

procedure TWMPEvents.CurrentPlaylistItemAvailable(const bstrItemName: WideString);
    stdcall;
begin
end;

procedure TWMPEvents.MediaChange(Item: IDispatch); stdcall;
begin
end;

procedure TWMPEvents.CurrentMediaItemAvailable(const bstrItemName: WideString);
    stdcall;
begin
end;

procedure TWMPEvents.CurrentItemChange(pdispMedia: IDispatch); stdcall;
begin
end;

procedure TWMPEvents.MediaCollectionChange; stdcall;
begin
end;

procedure TWMPEvents.MediaCollectionAttributeStringAdded(const bstrAttribName,
    bstrAttribVal: WideString); stdcall;
begin
end;

procedure TWMPEvents.MediaCollectionAttributeStringRemoved(const bstrAttribName,
    bstrAttribVal: WideString); stdcall;
begin
end;

procedure TWMPEvents.MediaCollectionAttributeStringChanged(const bstrAttribName,
    bstrOldAttribVal, bstrNewAttribVal: WideString); stdcall;
begin
end;

procedure TWMPEvents.PlaylistCollectionChange; stdcall;
begin
end;

procedure TWMPEvents.PlaylistCollectionPlaylistAdded(const bstrPlaylistName: WideString);
    stdcall;
begin
end;

procedure TWMPEvents.PlaylistCollectionPlaylistRemoved(const bstrPlaylistName: WideString);
    stdcall;
begin
end;

procedure TWMPEvents.PlaylistCollectionPlaylistSetAsDeleted(const bstrPlaylistName: WideString;
    varfIsDeleted: WordBool); stdcall;
begin
end;

procedure TWMPEvents.ModeChange(const ModeName: WideString;
    NewValue: WordBool); stdcall;
begin
end;

procedure TWMPEvents.MediaError(pMediaObject: IDispatch); stdcall;
begin
end;

procedure TWMPEvents.OpenPlaylistSwitch(pItem: IDispatch); stdcall;
begin
end;

procedure TWMPEvents.DomainChange(const strDomain: WideString); stdcall;
begin
end;

procedure TWMPEvents.SwitchedToPlayerApplication; stdcall;
begin
end;

procedure TWMPEvents.SwitchedToControl; stdcall;
begin
end;

procedure TWMPEvents.PlayerDockedStateChange; stdcall;
begin
end;

procedure TWMPEvents.PlayerReconnect; stdcall;
begin
end;

procedure TWMPEvents.Click(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPEvents.DoubleClick(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPEvents.KeyDown(nKeyCode, nShiftState: Smallint); stdcall;
begin
end;

procedure TWMPEvents.KeyPress(nKeyAscii: Smallint); stdcall;
begin
end;

procedure TWMPEvents.KeyUp(nKeyCode, nShiftState: Smallint); stdcall;
begin
end;

procedure TWMPEvents.MouseDown(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPEvents.MouseMove(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

procedure TWMPEvents.MouseUp(nButton, nShiftState: Smallint;
    fX, fY: Integer); stdcall;
begin
end;

end.
