(****************************************************************************
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
 * TWMPLibraryHook class
 *
 ****************************************************************************)

unit WMPLibraryHook;

interface

uses Windows, Common2, WMPUndocumented, WMPUtil, WMPPlug;

type
  TWMPLibraryHook = class(TDummyInterfacedObject)
  private

    fPluginLoaded: Boolean;

    fWMPPlaylistEventHandler: IUnknown;
    fMediaHooked: Boolean;

    fStub_IWMPMediaInternal_Advise: Pointer;
    fPrev_IWMPMediaInternal_Advise: function(This: Pointer; pIID: PGUID;
        WMPPlaylistEventHandler: Pointer): HResult; stdcall;

    function NewIWMPMediaInternal_Advise(This: Pointer; pIID: PGUID;
        WMPPlaylistEventHandler: Pointer): HResult; stdcall;

  protected

    function OnMediaInternalAdvise(This: Pointer; pIID: PGUID;
        WMPPlaylistEventHandler: Pointer): HResult; virtual;

  public

    constructor Create;
    destructor Destroy; override;

    function HookMedia(Core: IWMPCoreSafe; Media: IWMPMediaSafe): Boolean; virtual;
    function UnhookMedia(Media: IWMPMediaSafe): Boolean;
    function GetFlushableWMPItemData(Media: IWMPMediaSafe): IWMPItemData;

    procedure OnPluginLoad(Plugin: TWMPPlug); virtual;
    procedure OnPluginUnload; virtual;

    property WMPPlaylistEventHandler: IUnknown read fWMPPlaylistEventHandler;
    
  end;

implementation

uses ClassCallback, WMPAttribs;

constructor TWMPLibraryHook.Create;
begin
  inherited Create;
  fPluginLoaded := False;
  fWMPPlaylistEventHandler := nil;
  fMediaHooked := False;
  fStub_IWMPMediaInternal_Advise := CreateStub(Self,
      @TWMPLibraryHook.NewIWMPMediaInternal_Advise);
end;

destructor TWMPLibraryHook.Destroy;
begin
  DisposeStub(fStub_IWMPMediaInternal_Advise);
  fWMPPlaylistEventHandler := nil;
  inherited;
end;

function TWMPLibraryHook.OnMediaInternalAdvise(This: Pointer; pIID: PGUID;
    WMPPlaylistEventHandler: Pointer): HResult;
begin
  Result := fPrev_IWMPMediaInternal_Advise(This, pIID, WMPPlaylistEventHandler);
  // On WMP exit, we still get here after our plugin has been unloaded
  // so make sure that we don't keep the event handler then (and leak memory)
  if Succeeded(Result) and fPluginLoaded then
    fWMPPlaylistEventHandler := IUnknown(WMPPlaylistEventHandler);
end;

function TWMPLibraryHook.NewIWMPMediaInternal_Advise(This: Pointer; pIID: PGUID;
    WMPPlaylistEventHandler: Pointer): HResult; stdcall;
begin
  // Call a separate method to allow subclassing
  // (making this method virtual doesn't work because of the stub use)
  Result := OnMediaInternalAdvise(This, pIID, WMPPlaylistEventHandler);
end;

function TWMPLibraryHook.HookMedia(Core: IWMPCoreSafe;
    Media: IWMPMediaSafe): Boolean;
var
  MediaInternal: IWMPMediaInternal;
begin
  if not Assigned(fWMPPlaylistEventHandler)
      // CD and DVD media items are different from usual media items (different
      // vtable, different class). So don't hook these. The usual method of
      // getting WMPPlaylistEventHandler doesn't work for these items either.
      // TODO: but we still need to prevent preroll of CD tracks for 'close after current'
      //  -> create dummy media item and hook that one?
      and not (GetWMPMediaType(Media) in [wmpmtCDTrack, wmpmtDVD,
          wmpmtPhoto, // DVD media items sometimes get identified as photo
          wmpmtUnknown]) then begin
    if not fMediaHooked then begin
      if Succeeded(Media.QueryInterface(IWMPMediaInternal, MediaInternal)) then begin
        fMediaHooked := True;
        HookComMethod(Pointer(MediaInternal), 20, fStub_IWMPMediaInternal_Advise,
            @@fPrev_IWMPMediaInternal_Advise);
      end;
    end;
    // We just hooked IWMPMediaInternal::Advise, trigger another call so we can
    // get the WMPPlaylistEventHandler from it.
    GetWMPItemByFilename(Core.Get_mediaCollection, Media.sourceURL);
  end;
  Result := Assigned(fWMPPlaylistEventHandler);
end;

function TWMPLibraryHook.UnhookMedia(Media: IWMPMediaSafe): Boolean;
var
  MediaInternal: IWMPMediaInternal;
begin
  if fMediaHooked and Assigned(fPrev_IWMPMediaInternal_Advise)
      and Succeeded(Media.QueryInterface(IWMPMediaInternal, MediaInternal)) then begin
    fMediaHooked := False;
    HookComMethod(Pointer(MediaInternal), 20, @fPrev_IWMPMediaInternal_Advise, nil);
    Result := True;
  end
  else
    Result := False;
end;

function TWMPLibraryHook.GetFlushableWMPItemData(Media: IWMPMediaSafe): IWMPItemData;
var
  MediaInternal: IWMPMediaInternal;
begin
  Result := nil;
  if Assigned(fWMPPlaylistEventHandler) then begin
    if Succeeded(Media.QueryInterface(IWMPMediaInternal, MediaInternal)) then begin
      if Succeeded(MediaInternal.Advise(IWMPMediaEventHandler,
          fWMPPlaylistEventHandler)) then begin
        if not Succeeded(MediaInternal.QueryInterface(IWMPItemData, Result)) then
          Result := nil;
      end;
    end;
  end;
end;

procedure TWMPLibraryHook.OnPluginLoad(Plugin: TWMPPlug);
begin
  fPluginLoaded := True;
end;

procedure TWMPLibraryHook.OnPluginUnload;
begin
  fWMPPlaylistEventHandler := nil;
  fPluginLoaded := False;
end;

end.
