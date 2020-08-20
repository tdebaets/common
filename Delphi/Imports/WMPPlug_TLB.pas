(****************************************************************************
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
 * Interface implemented by Windows Media Player UI Plugins.
 * This unit is a port of the wmpplug.h header.
 *
 ****************************************************************************)

unit WMPPlug_TLB;

interface

uses Windows, ActiveX, WMPLib_TLB;

const
  PLUGIN_INSTALLREGKEY                      = 'Software\Microsoft\MediaPlayer\UIPlugins';
  PLUGIN_INSTALLREGKEYW: PWideChar          = 'Software\Microsoft\MediaPlayer\UIPlugins';
  PLUGIN_INSTALLREGKEY_FRIENDLYNAME: PChar  = 'FriendlyName';
  PLUGIN_INSTALLREGKEY_DESCRIPTION: PChar   = 'Description';
  PLUGIN_INSTALLREGKEY_CAPABILITIES: PChar  = 'Capabilities';
  PLUGIN_INSTALLREGKEY_UNINSTALL: PChar     = 'UninstallPath';

const
  PLUGIN_TYPE_BACKGROUND          = $1;
  PLUGIN_TYPE_SEPARATEWINDOW      = $2;
  PLUGIN_TYPE_DISPLAYAREA         = $3;
  PLUGIN_TYPE_SETTINGSAREA        = $4;
  PLUGIN_TYPE_METADATAAREA        = $5;
  PLUGIN_FLAGS_HASPROPERTYPAGE    = $80000000;
  PLUGIN_FLAGS_INSTALLAUTORUN     = $40000000;
  PLUGIN_FLAGS_LAUNCHPROPERTYPAGE = $20000000;
  PLUGIN_FLAGS_ACCEPTSMEDIA       = $10000000;
  PLUGIN_FLAGS_ACCEPTSPLAYLISTS   = $8000000;
  PLUGIN_FLAGS_HASPRESETS         = $4000000;
  PLUGIN_FLAGS_HIDDEN             = $2000000;

const
  PLUGIN_MISC_PRESETCOUNT: PWideChar              = 'PresetCount';
  PLUGIN_MISC_PRESETNAMES: PWideChar              = 'PresetNames';
  PLUGIN_MISC_CURRENTPRESET: PWideChar            = 'CurrentPreset';
  PLUGIN_SEPARATEWINDOW_RESIZABLE: PWideChar      = 'Resizable';
  PLUGIN_SEPARATEWINDOW_DEFAULTWIDTH: PWideChar   = 'DefaultWidth';
  PLUGIN_SEPARATEWINDOW_DEFAULTHEIGHT: PWideChar  = 'DefaultHeight';
  PLUGIN_SEPARATEWINDOW_MINWIDTH: PWideChar       = 'MinWidth';
  PLUGIN_SEPARATEWINDOW_MINHEIGHT: PWideChar      = 'MinHeight';
  PLUGIN_SEPARATEWINDOW_MAXWIDTH: PWideChar       = 'MaxWidth';
  PLUGIN_SEPARATEWINDOW_MAXHEIGHT: PWideChar      = 'MaxHeight';
  PLUGIN_MISC_QUERYDESTROY: PWideChar             = 'QueryDestroy';
  PLUGIN_ALL_MEDIASENDTO: PWideChar               = 'MediaSendTo';
  PLUGIN_ALL_PLAYLISTSENDTO: PWideChar            = 'PlaylistSendTo';

const
  PluginAddRemoveMessage: PChar = 'WMPlayer_PluginAddRemove';

type

{ Forward declarations: Interfaces }
  IWMPPluginUI = interface;

  {__MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  wireHWND = _RemotableHandle;}

  UINT_PTR = UINT;

  LONG_PTR = Integer;

  tagPOINT = record
    x: Integer;
    y: Integer;
  end;

  tagMSG = record
    hwnd: HWND;
    message: SYSUINT;
    wParam: UINT_PTR;
    lParam: LONG_PTR;
    time: UINT;
    pt: tagPOINT;
  end;

{ IWMPPluginUI: Public interface for Windows Media Player SDK. }

  IWMPPluginUI = interface(IUnknown)
    ['{4C5E8F9F-AD3E-4BF9-9753-FCD30D6D38DD}']
    function SetCore(const pCore: IWMPCore): HResult; stdcall;
    function CreateProc(hwndParent: HWND; out phwndWindow: HWND): HResult; stdcall;
    function DestroyProc: HResult; stdcall;
    function DisplayPropertyPage(hwndParent: HWND): HResult; stdcall;
    function GetProperty(pwszName: PWideChar; out pvarProperty: OleVariant): HResult; stdcall;
    function SetProperty(pwszName: PWideChar; var pvarProperty: OleVariant): HResult; stdcall;
    function TranslateAccelerator(var lpmsg: tagMSG): HResult; stdcall;
  end;


function WMPNotifyPluginAddRemove: Boolean;

implementation

function WMPNotifyPluginAddRemove: Boolean;
begin
  Result := PostMessage(HWND_BROADCAST,
      RegisterWindowMessage(PluginAddRemoveMessage), 0, 0);
end;

end.
