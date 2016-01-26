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
 * TProcessListView VCL component
 *
 ****************************************************************************)

unit ProcessListView;

interface

uses Messages, Classes, SysUtils, Graphics, Controls, ComCtrls, CmnFunc2,
    NewCommCtrl, ExtChkListView, ExtListView, EnhListView, EZDslHsh, EZStrHsh,
    EZDslBse, Windows, TimerThread, UpdateThread, ShellApi, ShlWapi;

// TODO: fix: "list index out of bounds" when repeatedly toggling system processes
// TODO: fails on Windows XP in Virtual PC
// TODO: test on vista/w7/w8
// TODO: fix high CPU usage on Vista x64 and Win7 x64
// TODO: flicker when checking/unchecking and adding items
// TODO: add option to toggle background (invisible) processes
// TODO; add unicode support

type

  TProcessListItem = class(TChkListItem)
  private
    //FProcess: TProcess;
    FProcessID: Integer;
    FProcessHandle: Integer;
    FImageIcon: Integer;
    FUpdated: Boolean;
  protected
    constructor Create(AOwner: TListItems);
    //procedure Assign(Source: TPersistent); override;
  public
    //procedure Delete;
    //destructor Destroy; override;
  published
    //property Process: TProcess read FProcess;
    property ProcessID: Integer read FProcessID;
    property ImageIcon: Integer read FImageIcon;
  end;

  TExeImageInfo = record
    FileIconIndex: Integer;
    Description: String;
    Company: String;
  end;
  PExeImageInfo = ^TExeImageInfo;

  TTempProcess = record
    ProcessID: Integer;
    //ProcessHandle: Integer; // was only used for adding creation time to hash
    //HandleNeeded: Boolean;
    //IsProtected: Boolean;
    Path: String;
  end;
  PTempProcess = ^TTempProcess;

  TCustomProcessListView = class;

  TProcess = class(TObject)
  private
    FProcessID: Integer;
    FProcessHandle: Integer;
    FName: String;
    FPath: String;
    FUser: String;
    FDomain: String;
    FSystemProcess: Boolean;
    FSystem: Boolean;
    FCurrentUser: Boolean;
    FIsProtected: Boolean;
    FItem: TProcessListItem;
    FImageInfo: TExeImageInfo;
    FOwner: TCustomProcessListView;
  private
    function GetDisplayPath: String;
    procedure SetPath(const Path: String);
    procedure SetItem(Item: TProcessListItem);
    procedure SetImageInfo(const ImageInfo: TExeImageInfo);
    function UpdatePath: Boolean;
    function GetHash: String;
  public
    constructor Create(Owner: TCustomProcessListView;
        ProcessID, ProcessHandle: Integer; IsProtected: Boolean);
    destructor Destroy; override;
    function GetUserName: String;
  published
    property ProcessID: Integer read FProcessID;
    property ProcessHandle: Integer read FProcessHandle;
    property Name: String read FName;
    property Path: String read FPath;
    property User: String read FUser;
    property Domain: String read FDomain;
    property SystemProcess: Boolean read FSystemProcess;
    property System: Boolean read FSystem;
    property CurrentUser: Boolean read FCurrentUser;
    property IsProtected: Boolean read FIsProtected;
    property Item: TProcessListItem read FItem;
  end;

  TExeImage = class(TObject)
  private
    FImageInfo: TExeImageInfo;
    FInfoLoaded: Boolean;
    FProcesses: TList;
    FOwner: TCustomProcessListView;
  protected
    constructor Create(Owner: TCustomProcessListView);
    destructor Destroy; override;
    procedure AddProcess(Process: TProcess);
    procedure RemoveProcess(Process: TProcess);
    procedure SetImageInfo(const ImageInfo: TExeImageInfo);
  end;

  TProcessEvent = procedure(Sender: TObject; Process: TProcess) of object;
  TShowProcessEvent = function(Sender: TObject;
      var Show: Boolean): Boolean of object;
  TProcessItemEvent = procedure(Sender: TObject; Process: TProcess;
      Item: TProcessListItem) of Object;

  TCustomProcessListView = class(TCustomExtChkListView)
  private
    FProcesses: THashTable; // key: hash (proc id), value: TProcess object
    FProcessPaths: TStringHashTable; // key: hash (proc id), value: path string
    FProcessesTemp: THashTable; // key: hash (proc id), value: TTempProcess pointer
    FImages: TThreadSafeHashTable; // key: filename (ignore case), value: TExeImage object
    FExitedProcesses: TStringList;
    FNewProcesses: TStringList;
    FProcIDColumnIdx: Integer;
    FNameColumnIdx: Integer;
    FPathColumnIdx: Integer;
    FUserColumnIdx: Integer;
    FDescColumnIdx: Integer;
    FCompanyColumnIdx: Integer;
    FAutoRefresh: Boolean;
    FRefreshInterval: Cardinal;
    FRefreshOnLoad: Boolean;
    FShowSystemProcess: Boolean;
    FShowSystem: Boolean;
    FShowOtherUser: Boolean;
    FShowCheckAll: Boolean;
    FCheckAllCaption: String;
//    FShowIcons: Boolean;
    FTimerThread: TTimerThread;
    FUpdateThread: TUpdateThread;
    FUpdateThreadFsRedirDisabled: Boolean; // TODO: remove
//    FImageList: TImageList;
    FhSIL: Integer; // handle of system image list
    FDefaultExeIconIdx: Integer;
    FNewProcessFilenameFuncAvailable: Boolean;
    FRedraw: Boolean;
    FCheckAllItem: TChkListItem;
    FProcessItemChecked: Integer;
    FAddingItem: Integer;
    FOnProcessAdded: TProcessEvent;
    FOnBeforeProcessRemove: TProcessEvent;
    FOnShowProcess: TShowProcessEvent;
    FOnItemAdded: TProcessItemEvent;
    FOnBeforeItemDelete: TProcessItemEvent;
    procedure SetColumnIdx(Index: Integer; ColumnIdx: Integer);
    procedure SetAutoRefresh(AutoRefresh: Boolean);
    procedure SetRefreshInterval(RefreshInterval: Cardinal);
    procedure SetShowOption(Index: Integer; Show: Boolean);
    function GetShowCheckAll: Boolean;
    procedure SetShowCheckAll(Value: Boolean);
    procedure SetCheckAllCaption(Value: String);
//    procedure SetShowIcons(Show: Boolean);
//    procedure SetSmallImages(Value: TImageList);
    procedure InitializeImageInfo(var ImageInfo: TExeImageInfo);
    function OpenProc(ProcessId: Integer; var IsProtected: Boolean): Integer;
    procedure RefreshProcesses(Startup, Full, RemoveOnly: Boolean);
    procedure RefreshImageInfo;
    function AddProcess(Process: PTempProcess): TProcess;
    procedure RemoveProcess(Process: TProcess);
    procedure AddProcessToImage(Path: String; Process: TProcess;
      RefreshImageInfo: Boolean);
    procedure RemoveProcessFromImage(Path: String; Process: TProcess);
    function ShowProcess(Process: TProcess): Boolean;
    procedure RefreshItem(Item: TProcessListItem; Process: TProcess;
      Full: Boolean);
    function RefreshProcess(Process: TProcess; Full: Boolean): TProcessListItem;
    procedure RefreshItems(Subitems: Boolean);
    procedure UpdateImage(Sender: TObject; const Item: String;
        var Data: Pointer);
    procedure ImageUpdated(Sender: TObject; const Item: String;
        var Data: Pointer);
    procedure UpdateDone(Sender: TObject);
    procedure StartTimer;
    procedure StopTimer;
    procedure OnTimer(Sender: TObject);
    function GetTotalProcessCount: Integer;
    procedure UpdateAllChecked;
    procedure LVMInsertItem(var Message: TMessage); message LVM_INSERTITEM;
  protected
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function CreateListItem: TListItem; override;
    procedure Loaded; override;
    procedure LVItemPostPaint(const NMLVCD: TNMLVCustomDraw;
        var DrawIcon: Boolean); override;
    function ItemChecking(Item: TListItem): Boolean; override;
    procedure ItemChecked(ItemIndex: integer; Checked: boolean); override;
    function CustomSortProc(Item1, Item2: TListItem;
        var CompResult: integer): Boolean; override;
    property ProcIDColumnIdx: Integer index 0
      read FProcIDColumnIdx write SetColumnIdx;
    property NameColumnIdx: Integer index 1
      read FNameColumnIdx write SetColumnIdx;
    property PathColumnIdx: Integer index 2
      read FPathColumnIdx write SetColumnIdx;
    property UserColumnIdx: Integer index 3
      read FUserColumnIdx write SetColumnIdx;
    property DescriptionColumnIdx: Integer index 4
      read FDescColumnIdx write SetColumnIdx;
    property CompanyColumnIdx: Integer index 5
      read FCompanyColumnIdx write SetColumnIdx;
    property ShowSystemProcess: Boolean index 0
      read FShowSystemProcess write SetShowOption;
    property ShowSystem: Boolean index 1
      read FShowSystem write SetShowOption;
    property ShowOtherUser: Boolean index 2
      read FShowOtherUser write SetShowOption;
    property ShowCheckAll: Boolean read GetShowCheckAll write SetShowCheckAll;
    property CheckAllCaption: String
      read FCheckAllCaption write SetCheckAllCaption;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh;
    property AutoRefreshInterval: Cardinal read FRefreshInterval
      write SetRefreshInterval;
    property RefreshOnLoad: Boolean read FRefreshOnLoad write FRefreshOnLoad;
//    property ShowIcons: Boolean read FShowIcons write SetShowIcons;
//    property SmallImages: TImageList read GetSmallImages write SetSmallImages;
    //property Items: TListItems read GetItems stored False;
    property OnProcessAdded: TProcessEvent read FOnProcessAdded
        write FOnProcessAdded;
    property OnBeforeProcessRemove: TProcessEvent read FOnBeforeProcessRemove
        write FOnBeforeProcessRemove;
    property OnShowProcess: TShowProcessEvent read FOnShowProcess write
        FOnShowProcess;
    property OnItemAdded: TProcessItemEvent read FOnItemAdded
        write FOnItemAdded;
    property OnBeforeItemDelete: TProcessItemEvent read FOnBeforeItemDelete
        write FOnBeforeItemDelete;
    property TotalProcessCount: Integer read GetTotalProcessCount;
  public
    procedure Refresh(Full: Boolean);
//    function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
  end;

  TProcessListView = class(TCustomProcessListView)
  public
    property ItemEnabled;

    property LastColumnClicked;
    property CurrentColumnWidth;
    property HeaderHandle;
    property SubItem_BoundsRect;
    property SubItem_IconRect;
    property SubItem_LabelRect;
    property SubItem_SelectBoundsRect;
    property HotItem;
    property HotCursor;
    property WorkArea;
    property IsChecked;
    property SubItem_ImageIndex;
    property SelectionMark;
    property ItemIndent;
    property CurrentSortAscending;
  published
    //NEW
    property ProcIDColumnIdx;
    property NameColumnIdx;
    property PathColumnIdx;
    property UserColumnIdx;
    property DescriptionColumnIdx;
    property CompanyColumnIdx;
    property AutoRefresh;
    property AutoRefreshInterval;
    property RefreshOnLoad;
    property ShowSystemProcess;
    property ShowSystem;
    property ShowOtherUser;
    property ShowCheckAll;
    property CheckAllCaption;
//    property ShowIcons;

    property OnProcessAdded;
    property OnBeforeProcessRemove;
    property OnShowProcess;
    property OnBeforeItemDelete;
    property OnItemAdded;

    property TotalProcessCount;
    
    property CheckBoxOptions;
    property OnItemPrePaint;
    property OnItemChecking;
    property DisabledColor;

    property Columns;
    property ColumnSearch;
    property HideSelection;
    property ExtendedStyles;
    property HoverTime;
    property RequireComCtlUpdate;
{$IFDEF BACKGROUND_FIXED}
    property BackgroundImage;
{$ENDIF}
    property NoColumnResize;
    property SaveSettings;
    property ColumnsFormat;
    // New Events
    property OnItemChecked;
    property OnMarqueeBegin;
    property OnItemActivate;
    property OnHotTrack;
    property OnInfoTip;
    property OnVMGetItemInfo;
    property OnVMCacheHint;
    property OnVMFindItem;
    property OnVMStateChanged;
    property OnVMCaptionEdited;
    property ShowSortArrows;

    // Publish inherited protected properties
    property AutoColumnSort;
    property AutoSortStyle;
    property AutoResort;
    property AutoSortAscending;
    property ReverseSortArrows;
    property Style;

    property OnDrawHeader;
    property OnMeasureItem;
    property OnDrawItem;
    property OnDrawSubItem;
    property OnAfterDefaultDrawItem;
    property OnSortItems;
    property OnSortBegin;
    property OnSortFinished;

    property Align;
{$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode;
{$ENDIF}
    property BorderStyle;
{$IFDEF DFS_COMPILER_4_UP}
    property BorderWidth;
{$ENDIF}
    property Color;
    property ColumnClick;
{$IFDEF DFS_COMPILER_4_UP}
    property Constraints;
{$ENDIF}
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
{$IFDEF DFS_COMPILER_4_UP}
    property DragKind;
{$ENDIF}
    property Enabled;
    property Font;
    property IconOptions;
    property AllocBy;
    property MultiSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnDeletion;
{$IFDEF DFS_COMPILER_4_UP}
    property OnEndDock;
{$ENDIF}
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
//    property OnGetImageIndex;
{$IFDEF DFS_COMPILER_5_UP}
    property OnGetSubItemImage;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF DFS_COMPILER_4_UP}
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
{$ENDIF}
    property ParentColor
       default False;
    property ParentFont;
    property ParentShowHint;
{$IFDEF DFS_COMPILER_4_UP}
    property ParentBiDiMode;
{$ENDIF}
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop
       default True;
    property ViewStyle;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property LargeImages;
    property SmallImages;
    property StateImages;

    //property Items;

    property DoubleBuffered;
  end;

  {TListColumnProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;}

procedure Register;

implementation

{$R ProcessListView.res}

uses Common2, CtrlsCommon, VersionInfo, Processes, Wow64;

const
  InitialTableSize = 150;

procedure DisposeTempProcessProc(aData: Pointer);
var
  TempProcess: PTempProcess;
begin
  TempProcess := PTempProcess(aData);
  if Assigned(TempProcess) then begin
    {if not TempProcess.HandleNeeded and (TempProcess.ProcessHandle <> 0) then
      CloseHandle(TempProcess.ProcessHandle);}
    TempProcess.Path := ''; // IMPORTANT: prevents memory leak!
    Dispose(TempProcess);
  end;
end;

procedure DisposeProcessProc(aData: Pointer);
begin
  if Assigned(aData) then
    TProcess(aData).Free;
end;

procedure DisposeExeImageProc(aData: Pointer);
begin
  if Assigned(aData) then
    TExeImage(aData).Free;
end;

function GetProcessHash(ProcID{, ProcHandle}: Integer): String;
{const
  CreationTimeFormat = 'ssnnhhddmmyyyy';
var
  CreationTime: TFileTime;
  CreationTimeSys: TSystemTime;}
begin
  Result := IntToStr(ProcID);
  {if IsWinNT and (ProcHandle <> 0) then begin
    if GetProcessCreationTime(ProcHandle, CreationTime) then begin
      if FileTimeToSystemTime(CreationTime, CreationTimeSys) then
        Result := Result + '-' + FormatDateTime(CreationTimeFormat,
            SystemTimeToDateTime(CreationTimeSys));
    end
    else
      outputdebugstring(pchar('getprocesstime failed for ' + inttostr(procid)));
  end;}
end;

{TProcess}

constructor TProcess.Create(Owner: TCustomProcessListView;
    ProcessID, ProcessHandle: Integer; IsProtected: Boolean);
var
  Sid: PSID;
begin
  inherited Create;
  FOwner := Owner;
  FProcessID := ProcessID;
  FProcessHandle := ProcessHandle;
  {if FProcessHandle = 0 then
    FProcessHandle := OpenProc(ProcessID);}
  FSystemProcess := False;
  FSystem := False;
  FCurrentUser := True;
  FUser := '';
  FDomain := '';
  FIsProtected := IsProtected;
  if IsWinNT and (FProcessHandle <> 0) then begin
    Sid := nil;
    if GetProcessSid(ProcessID, FProcessHandle, Sid) then try
      GetUserAndDomainFromSid(Sid, FUser, FDomain);
      CheckSid(Sid, FSystem, FCurrentUser);
    finally
      FreeMem(Sid);
    end;
  end;
  Owner.InitializeImageInfo(FImageInfo);
end;

destructor TProcess.Destroy;
begin
  if FProcessHandle <> 0 then
    CloseHandle(FProcessHandle);
  SetItem(nil);
  inherited;
end;

function TProcess.GetUserName: String;
begin
  Result := '';
  if FDomain <> '' then
    Result := FDomain + '\';
  Result := Result + FUser;
end;

function TProcess.GetDisplayPath: String;
begin
  Result := FPath;
  GetLongFilename(Result);
end;

procedure TProcess.SetPath(const Path: String);
begin
  if Path <> FPath then begin
    FPath := Path;
    // base the displayname on the displaypath
    FName := GetExeDisplayName(GetDisplayPath);
    // System's procid is always 4 or 8, but this isn't documented anywhere, so
    // compare path instead
    FSystemProcess := FSystem and (AnsiCompareFileName(FPath, 'system') = 0);
  end;
end;

procedure TProcess.SetItem(Item: TProcessListItem);
begin
  if Item <> FItem then begin
    if Assigned(FItem) then begin
      // item can already be deleted (or is being deleted) when component is
      // being destroyed
      // so don't delete it ourselves to prevent access violations
      // TODO: remove try-except
      if not (csDestroying in FOwner.ComponentState) then try
        FItem.Delete;
      except
        // FItem can already be freed
      end;
    end;
    FItem := Item;
    if Assigned(FItem) then begin
      FItem.FProcessID := ProcessID;
      FItem.FProcessHandle := FProcessHandle;
      FItem.Caption := FName;
      //FItem.ImageIndex := -1; // this causes a listview autosize bug with theming!!!!
      //FItem.FImageIcon := -1;
      SetImageInfo(FImageInfo); // initializes FImageIcon
    end;
  end;
end;

procedure TProcess.SetImageInfo(const ImageInfo: TExeImageInfo);
begin
  FImageInfo := ImageInfo;
  if Assigned(FItem) then
    FItem.FImageIcon := FImageInfo.FileIconIndex;
end;

function TProcess.UpdatePath: Boolean;
var
  NewPath: String;
begin
  Result := False;
  if FProcessHandle <> 0 then begin
    GetProcessFilename(FProcessID, FProcessHandle, NewPath);
    if (NewPath <> '') and (AnsiCompareFileName(NewPath, FPath) <> 0) then begin
      SetPath(NewPath);
      Result := True;
    end;
  end;
end;

function TProcess.GetHash: String;
begin
  Result := GetProcessHash(FProcessID{, FProcessHandle});
end;

{TProcessListItem}

constructor TProcessListItem.Create(AOwner: TListItems);
begin
  inherited Create(AOwner);
  FProcessID := -1;
  FUpdated := False;
end;

//destructor TProcessListItem.Destroy;
//begin
//  {if Assigned(FProcess) then
//    FreeAndNil(FProcess);}
//  inherited;
//end;

{procedure TProcessListItem.Delete;
begin
  outputdebugstring('item delete');
  inherited;
end;}

{procedure TProcessListItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TProcessListItem then
    with Source as TProcessListItem do begin
      Self.FProcessID := ProcessID;
      Self.FProcessHandle := ProcessHandle;
      Self.FName := Name;
      Self.FPath := Path;
    end;
end;}

{TExeImage}

constructor TExeImage.Create(Owner: TCustomProcessListView);
begin
  inherited Create;
  FOwner := Owner;
  FInfoLoaded := False;
  FProcesses := TList.Create;
  Owner.InitializeImageInfo(FImageInfo);
end;

destructor TExeImage.Destroy;
begin
  FProcesses.Free;
end;

procedure TExeImage.AddProcess(Process: TProcess);
var
  i: Integer;
begin
  for i := 0 to FProcesses.Count - 1 do begin
    if FProcesses[i] = Process then
      Exit;
  end;
  FProcesses.Add(Process);
  Process.SetImageInfo(FImageInfo);
end;

procedure TExeImage.RemoveProcess(Process: TProcess);
var
  i: Integer;
begin
  // TODO: we should be looping backwards here
  for i := 0 to FProcesses.Count - 1 do begin
    if FProcesses[i] = Process then begin
      FProcesses.Delete(i);
      Break;
    end;
  end;
end;

procedure TExeImage.SetImageInfo(const ImageInfo: TExeImageInfo);
var
  i: Integer;
  Process: TProcess;
begin
  FImageInfo := ImageInfo;
  FInfoLoaded := True;
  for i := 0 to FProcesses.Count - 1 do begin
    Process := TProcess(FProcesses[i]);
    Process.SetImageInfo(ImageInfo);
    if Assigned(Process.Item) then begin
      Process.Item.FUpdated := True;
      Process.FOwner.RefreshItem(Process.Item, Process, False);
    end;
  end;
end;

{TCustomProcessListView}

constructor TCustomProcessListView.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
begin
  inherited Create(AOwner);
  ReadOnly := True;
  FProcesses := THashTable.Create(True);
  FProcesses.DisposeData := DisposeProcessProc;
  FProcesses.TableSize := InitialTableSize;
  FProcesses.HashFunction := HashELF;
  FProcessPaths := TStringHashTable.Create;
  FProcessPaths.TableSize := InitialTableSize;
  FProcessPaths.HashFunction := HashELF;
  FProcessesTemp := THashTable.Create(True);
  FProcessesTemp.DisposeData := DisposeTempProcessProc;
  FProcessesTemp.TableSize := InitialTableSize;
  FProcessesTemp.HashFunction := HashELF;
  FImages := TThreadSafeHashTable.Create(True);
  with FImages.AcquireAccess do try
    DisposeData := DisposeExeImageProc;
    HashFunction := HashELF; // default hash causes integer overflows
    IgnoreCase := True;
  finally
    FImages.ReleaseAccess;
  end;
  FExitedProcesses := TStringList.Create;
  FNewProcesses := TStringList.Create;
  FProcIDColumnIdx := -1;
  FNameColumnIdx := 0;
  FPathColumnIdx := -1;
  FUserColumnIdx := -1;
  FDescColumnIdx := -1;
  FCompanyColumnIdx := -1;
  FAutoRefresh := True;
  FRefreshInterval := 1000;
  FRefreshOnLoad := True;
  FShowSystemProcess := True;
  FShowSystem := True;
  FShowOtherUser := True;
  FRedraw := True;
  FShowCheckAll := False;
  FCheckAllItem := nil;
  FCheckAllCaption := '(All programs)';
  FProcessItemChecked := 0;
  FAddingItem := 0;
//  FShowIcons := True;
//  FImageList := TImageList.Create(Self);
  //FImageList.Masked := True;
//  ConvertTo32BitImageList(FImageList);
//  Ico := TIcon.Create;
//  try
//    Ico.Handle := LoadIcon(hInstance, 'PROCLVPROGRAM');
//    FImageList.AddIcon(Ico);
//  finally
//    Ico.Free;
//  end;
  //SmallImages := FImageList;
  FhSIL := SHGetFileInfo( '.exe', FILE_ATTRIBUTE_NORMAL, FileInfo,
      SizeOf(FileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
  FDefaultExeIconIdx := FileInfo.iIcon;
  FNewProcessFilenameFuncAvailable := NewProcessFilenameFuncAvailable;
  if IsWinNT then
    EnableDebugPrivilege;
  FUpdateThreadFsRedirDisabled := False;
  if not (csDesigning in ComponentState) then
    FUpdateThread := TUpdateThread.Create(False, UpdateImage, ImageUpdated,
        UpdateDone)
  else
    FUpdateThread := nil;
end;

destructor TCustomProcessListView.Destroy;
begin
  Items.Clear;
  StopTimer;
  // TODO: free update thread (after it has stopped!) or use FreeOnTerminate
  if Assigned(FUpdateThread) then
    FUpdateThread.Terminate;
  // TODO: use FreeAndNil
//  FImageList.Free;
  FImages.Free;
  FProcesses.Free;
  FProcessPaths.Free;
  FProcessesTemp.Free;
  FExitedProcesses.Free;
  FNewProcesses.Free;
  if IsWinNT then
    UnloadWinSta;
  inherited Destroy;
end;

procedure TCustomProcessListView.CreateWnd;
begin
  inherited CreateWnd;
//  if SmallImages = nil then
//    SetSmallImages(nil);
  //RefreshProcesses(True, True, False); // calling this here causes the listview autosize bug with theming
end;

procedure TCustomProcessListView.DestroyWnd;
begin
  RefreshProcesses(False, False, True);
  if Assigned(FCheckAllItem) then begin
    FCheckAllItem.Delete;
    // TODO: free item?
    FCheckAllItem := nil;
  end;
  inherited DestroyWnd;
end;

function TCustomProcessListView.CreateListItem: TListItem;
begin
  Result := TProcessListItem.Create(Items);
end;

procedure TCustomProcessListView.Loaded;
begin
//  RefreshItems(True); // access violation
  //Items.Clear;
  if FRefreshOnLoad then
    RefreshProcesses(True, True, False);
  // doing RefreshProcesses first fixes csLoading bug with MultiSelect = True
  // and at least 1 column!
  inherited Loaded;
  if FRefreshOnLoad then
    RefreshImageInfo;
  if FAutoRefresh then
    StartTimer;
end;

procedure TCustomProcessListView.LVItemPostPaint(const NMLVCD: TNMLVCustomDraw;
    var DrawIcon: Boolean);
var
  Item: TProcessListItem;
  hImageList: Integer;
  ImageIndex: Integer;
  R: TRect;
begin
  if TObject(NMLVCD.nmcd.lItemlParam) is TProcessListItem then begin
  Item := TProcessListItem(NMLVCD.nmcd.lItemlParam);
    if (Item <> FCheckAllItem) and (Item.ImageIcon > -1)
        and Assigned(SmallImages) then begin
      DrawIcon := False;
      hImageList := FhSIL;
      ImageIndex := Item.ImageIcon;
      if ListView_GetItemRect(Handle, NMLVCD.nmcd.dwItemSpec, R,
          LVIR_ICON) then begin
        // prevent drawing on the column header
        if (R.Top < 5) and (ViewStyle = vsReport) and ShowColumnHeaders then
          Exit;
        if not FRedraw then
          Exit;
        FillRect(NMLVCD.nmcd.hdc, R, ColorToRgb(Color));
        if Item.Selected
            {and (NMLVCD.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED)} then begin
          ImageList_DrawEx(hImageList, ImageIndex, NMLVCD.nmcd.hdc,
            R.Left, R.Top, 0, 0, CLR_DEFAULT, CLR_DEFAULT, ILD_BLEND50);
        end
        else if Item.Enabled or not CheckBoxOptions.GrayedImages then begin
          ImageList_DrawEx(hImageList, ImageIndex, NMLVCD.nmcd.hdc,
            R.Left, R.Top, 0, 0, CLR_DEFAULT, CLR_DEFAULT, ILD_NORMAL);
        end
        else begin
          ImageList_DrawEx(hImageList, ImageIndex, NMLVCD.nmcd.hdc,
            R.Left, R.Top, 0, 0, CLR_DEFAULT, ColorToRgb(clGrayText),
            ILD_BLEND50);
        end;
      end;
    end;
  end;
end;

procedure TCustomProcessListView.UpdateAllChecked;
var
  i: Integer;
  Item: TChkListItem;
  Grayed, PrevChecked: Boolean;
  IsFirst: Boolean;
begin
  if not Assigned(FCheckAllItem) then
    Exit;
  PrevChecked := False;
  Grayed := False;
  IsFirst := True;
  for i := 0 to Items.Count - 1 do begin
    Item := TChkListItem(Items[i]);
    if (Item <> FCheckAllItem) and Item.Enabled then begin
      if not IsFirst then begin
        if PrevChecked <> Item.Checked then begin
          Grayed := True;
          Break;
        end;
      end;
      PrevChecked := Item.Checked;
      IsFirst := False;
    end;
  end;
  Inc(FProcessItemChecked);
  try
    FCheckAllItem.Checked := PrevChecked;
    FCheckAllItem.Grayed := Grayed;
  finally
    Dec(FProcessItemChecked);
  end;
end;

function TCustomProcessListView.ItemChecking(Item: TListItem): Boolean;
var
  SelItem: TListItem;
  Check: Boolean;
begin
  Result := inherited ItemChecking(Item);
  if not Result then
    Exit;
  if Item.Selected and (SelCount > 1) then begin
    SelItem := Selected;
    if Assigned(SelItem) then begin // should always be true, but check anyway
      //SelItem := GetNextItem(nil, sdAll, [isSelected]);
      Check := not Item.Checked;
      while Assigned(SelItem) do begin
        //if (SelItem <> Item) then
          if ItemEnabled[SelItem.Index] then
            SelItem.Checked := Check;
        SelItem := GetNextItem(SelItem, sdAll, [isSelected]);
      end;
      Result := False;
    end;
  end;
end;

procedure TCustomProcessListView.ItemChecked(ItemIndex: Integer;
    Checked: Boolean);
var
  i: Integer;
  Item: TChkListItem;
begin
  inherited;
  if FAddingItem > 0 then
    Exit; // we don't want to update FCheckAllItem on every item add
  if FProcessItemChecked > 0 then
    Exit; // setting Item.Checked will call this method again!
  Inc(FProcessItemChecked);
  try
    if Assigned(FCheckAllItem) then begin
      if ItemIndex = FCheckAllItem.Index then begin
        for i := 0 to Items.Count - 1 do begin
          Item := TChkListItem(Items[i]);
          if (Item <> FCheckAllItem) and ItemEnabled[Item.Index] then
            Item.Checked := Checked;
        end;
      end
      else begin
        Item := TChkListItem(Items[ItemIndex]);
        if Item.Enabled then begin
          if FCheckAllItem.Grayed then
            UpdateAllChecked
          else if FCheckAllItem.Checked <> Checked then
            FCheckAllItem.Grayed := True;
        end;
      end;
    end;
  finally
    Dec(FProcessItemChecked);
  end;
end;

function TCustomProcessListView.ShowProcess(Process: TProcess): Boolean;
begin
  Result := (Process.ProcessHandle <> 0) and (Process.Path <> '');
  if Result then begin
    if Assigned(FOnShowProcess) then begin
      if not FOnShowProcess(Self, Result) then
        Exit;
    end;
    if Process.System then
      Result := FShowSystem
    else if not Process.CurrentUser then
      Result := FShowOtherUser;
    if Process.SystemProcess then
      Result := FShowSystemProcess;
  end;
end;

procedure TCustomProcessListView.RefreshItem(Item: TProcessListItem;
    Process: TProcess; Full: Boolean);
var
  i: Integer;
  Text: String;
begin
  {if Full then
    Item.SubItems.Clear;}
  if not Assigned(Item.SubItems) then
    Exit;
  while Item.SubItems.Count < Columns.Count - 1 do
    Item.SubItems.Add('');
  for i := 0 to Columns.Count - 1 do begin
    if i = FProcIDColumnIdx then
      Text := ProcessIDToStr(Process.ProcessID)
    else if i = FNameColumnIdx then
      Text := Process.Name
    else if i = FPathColumnIdx then
      Text := Process.GetDisplayPath
    else if i = FUserColumnIdx then begin
      Text := Process.GetUserName;
      if Process.IsProtected then
        Text := Text + ' (protected)';
    end
    else if i = FDescColumnIdx then
      Text := Process.FImageInfo.Description
    else if i = FCompanyColumnIdx then
      Text := Process.FImageInfo.Company
    else
      Text := '';
    if i = 0 then
      Item.Caption := Text
    else if Text <> '' then
      Item.SubItems[i - 1] := Text
    else if Full then
      Item.SubItems[i - 1] := '';
    {else if Full then
      Item.SubItems.Add(Text)
    else if i - 1 < Item.SubItems.Count then
      Item.SubItems[i - 1] := Text;}
  end;
  // resorting here can cause a lot of flicker!
  {if AutoResort then
    Resort; // resort for changes in subitems (such as path)}
  UpdateItems(Item.Index, Item.Index);
end;

function TCustomProcessListView.RefreshProcess(Process: TProcess;
    Full: Boolean): TProcessListItem;
var
  Item: TProcessListItem;
  Show: Boolean;
begin
  Item := Process.Item;
  Show := ShowProcess(Process);
  if not Assigned(Item) and Show then begin
    Process.SetItem(TProcessListItem(Items.Add));
    Item := Process.Item;
    if Assigned(FOnItemAdded) then
      FOnItemAdded(Self, Process, Item);
    // new items don't get checked, regardless of 'check all', to prevent
    // selecting unwanted processes
    {if Assigned(FCheckAllItem) and Item.Enabled then begin
      if not FCheckAllItem.Grayed then
        Item.Checked := FCheckAllItem.Checked;
    end;}
  end
  else if Assigned(Item) and not Show then begin
    if Assigned(FOnBeforeItemDelete) then
      FOnBeforeItemDelete(Self, Process, Item);
    Process.SetItem(nil);
    Item := nil;
  end;
  Result := Item;
  if Assigned(Item) then
    RefreshItem(Item, Process, Full);
end;

function RefreshProcessesProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
begin
  Result := True;
  TCustomProcessListView(ExtraData).RefreshProcess(TProcess(aData), False);
end;

function RefreshProcessesFullProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
begin
  Result := True;
  TCustomProcessListView(ExtraData).RefreshProcess(TProcess(aData), True);
end;

procedure TCustomProcessListView.RefreshItems(Subitems: Boolean);
begin
  BeginUpdate;
  try
    if Subitems then
      FProcesses.Iterate2(RefreshProcessesFullProc, False, Self)
    else
      FProcesses.Iterate2(RefreshProcessesProc, False, Self);
    UpdateAllChecked; // deleted items can change 'all checked'-state
  finally
    EndUpdate;
  end;
end;

procedure TCustomProcessListView.SetColumnIdx(Index: Integer;
    ColumnIdx: Integer);
var
  PColumnIdx: PInteger;
begin
  case Index of
    0: PColumnIdx := @FProcIDColumnIdx;
    1: PColumnIdx := @FNameColumnIdx;
    2: PColumnIdx := @FPathColumnIdx;
    3: PColumnIdx := @FUserColumnIdx;
    4: PColumnIdx := @FDescColumnIdx;
    5: PColumnIdx := @FCompanyColumnIdx;
    else
      Exit;
  end;
  if ColumnIdx <> PColumnIdx^ then begin
    PColumnIdx^ := ColumnIdx;
    if not (csLoading in ComponentState) then
      RefreshItems(True);
  end;
end;

procedure TCustomProcessListView.SetShowOption(Index: Integer; Show: Boolean);
var
  PShowOption: ^Boolean;
begin
  case Index of
    0: PShowOption := @FShowSystemProcess;
    1: PShowOption := @FShowSystem;
    2: PShowOption := @FShowOtherUser;
    else
      Exit;
  end;
  if Show <> PShowOption^ then begin
    PShowOption^ := Show;
    if not (csLoading in ComponentState) then begin
      RefreshItems(True);
      RefreshImageInfo; // refresh images that weren't previously shown
    end;
  end;
end;

procedure TCustomProcessListView.SetAutoRefresh(AutoRefresh: Boolean);
begin
  if AutoRefresh <> FAutoRefresh then begin
    FAutoRefresh := AutoRefresh;
    if FAutoRefresh then
      StartTimer
    else
      StopTimer;
  end;
end;

procedure TCustomProcessListView.SetRefreshInterval(RefreshInterval: Cardinal);
begin
  if RefreshInterval <> FRefreshInterval then begin
    FRefreshInterval := RefreshInterval;
    if FRefreshInterval = 0 then
      FRefreshInterval := 100;
    if FTimerThread <> nil then
      FTimerThread.Interval := FRefreshInterval;
  end;
end;

function TCustomProcessListView.GetShowCheckAll: Boolean;
begin
  //Result := Assigned(FCheckAllItem);
  Result := FShowCheckAll;
end;

procedure TCustomProcessListView.SetShowCheckAll(Value: Boolean);
begin
  if Value <> Assigned(FCheckAllItem) then begin
    FShowCheckAll := Value;
    if Value then begin
      FCheckAllItem := TChkListItem(Items.Add);
      FCheckAllItem.Caption := FCheckAllCaption;
      FCheckAllItem.ImageIndex := -1;
      FCheckAllItem.Enabled := True;
      UpdateAllChecked;
      // TODO: resort here
    end
    else begin
      FCheckAllItem.Delete;
      // TODO: free item?
      FCheckAllItem := nil;
    end;
  end;
end;

procedure TCustomProcessListView.SetCheckAllCaption(Value: String);
begin
  FCheckAllCaption := Value;
  if Assigned(FCheckAllItem) then
    FCheckAllItem.Caption := Value;
end;

procedure TCustomProcessListView.LVMInsertItem(var Message: TMessage);
begin
  Inc(FAddingItem);
  try
    inherited;
  finally
    Dec(FAddingItem);
  end;
end;

//procedure TCustomProcessListView.SetShowIcons(Show: Boolean);
//begin
//  if Show <> FShowIcons then begin
//    FShowIcons := Show;
//    if SmallImages = nil then
////      SetSmallImages(nil);
//    if not (csLoading in ComponentState) then
//      //RefreshItems(True);
//      RefreshProcesses(False, True, False);
//  end;
//end;

//procedure TCustomProcessListView.SetSmallImages(Value: TImageList);
//begin
//  inherited SmallImages := Value;
//  if HandleAllocated and (Value = nil) then begin
//    if FShowIcons then
//      ListView_SetImageList(Handle, FImageList.Handle, LVSIL_SMALL)
//    else begin
//      ListView_SetImageList(Handle, 0, LVSIL_SMALL);
//    end;
//  end;
//end;

procedure TCustomProcessListView.InitializeImageInfo(var ImageInfo: TExeImageInfo);
begin
  ImageInfo.FileIconIndex := FDefaultExeIconIdx;
  ImageInfo.Description := '';
  ImageInfo.Company := '';
end;

function TCustomProcessListView.OpenProc(ProcessId: Integer;
    var IsProtected: Boolean): Integer;
var
  Access: Integer;
begin
  IsProtected := False;
  Access := PROCESS_QUERY_INFORMATION;
  if not FNewProcessFilenameFuncAvailable then
    Access := Access or PROCESS_VM_READ; // GetModuleFileNameEx needs this
  Result := OpenProcess(Access, False, ProcessID);
  if Result = 0 then begin
    IsProtected := True;
    Result := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, ProcessID);
  end;
end;

function EnumProcessesProc(ProcID: Integer; const Filename: String;
    ExtraData: Pointer): Boolean;
var
  TempProcess: PTempProcess;
begin
  Result := True;
  with TCustomProcessListView(ExtraData) do begin
    New(TempProcess);
    TempProcess.ProcessID := ProcID;
    {TempProcess.ProcessHandle := OpenProc(ProcID, TempProcess.IsProtected);
    TempProcess.HandleNeeded := False;}
    TempProcess.Path := Filename;
    FProcessesTemp.Insert(GetProcessHash(ProcID{, TempProcess.ProcessHandle}),
        TempProcess);
  end;
end;

function RemoveAllProcessesProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
begin
  Result := True;
  TCustomProcessListView(ExtraData).FExitedProcesses.AddObject(Key, aData);
end;

function FindExitedProcessesProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
var
  Data: Pointer;
begin
  Result := True;
  with TCustomProcessListView(ExtraData) do begin
    if not FProcessesTemp.Search(Key, Data) then
      FExitedProcesses.AddObject(Key, aData);
  end;
end;

function AddNewProcessesProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
var
  Data: Pointer;
begin
  Result := True;
  with TCustomProcessListView(ExtraData) do begin
    if not FProcesses.Search(Key, Data) then
      FNewProcesses.AddObject(Key, aData);
  end;
end;

function UpdatePathsProc(Table: THashTable; Key: String; aData: Pointer;
    ExtraData: Pointer): Boolean;
var
  ListView: TCustomProcessListView;
  Process: TProcess;
  OldPath: String;
begin
  Result := True;
  ListView := TCustomProcessListView(ExtraData);
  Process := TProcess(aData);
  //outputdebugstring(Pchar(Process.Path));
  OldPath := Process.Path;
  if Process.UpdatePath then begin
    ListView.RemoveProcessFromImage(OldPath, Process);
    ListView.AddProcessToImage(Process.Path, Process, True);
    ListView.RefreshProcess(Process, False);
    if ListView.AutoResort then
      ListView.Resort;
  end;
end;

// TODO; use set of enum flags here
procedure TCustomProcessListView.RefreshProcesses(Startup, Full,
    RemoveOnly: Boolean);
var
  i: Integer;
  Update: Boolean;
  TempProcess: PTempProcess;
  Process: TProcess;
begin
  //outputdebugstring('start');
  FRedraw := False;
  try
    FExitedProcesses.Clear;
    FNewProcesses.Clear;
    FProcessesTemp.Empty;
    EnumProcessesAll(EnumProcessesProc, Pointer(Self));
    if Full then begin
      {if Assigned(FUpdateThread) then
        FUpdateThread.ClearItems;} // not stable, generally isn't needed anyway
      // prevents weird things when the listview window is recreated (shouldnt
      // happen at runtime)
      // TODO: bad practice? remove?
      LockWindowUpdate(Handle);
      BeginUpdate;
    end;
    try
      if Full or RemoveOnly then
        FProcesses.Iterate2(RemoveAllProcessesProc, False, Pointer(Self))
      else
        FProcesses.Iterate2(FindExitedProcessesProc, False, Pointer(Self));
      if not RemoveOnly then
        FProcessesTemp.Iterate2(AddNewProcessesProc, False, Pointer(Self));
      Update := (FExitedProcesses.Count > 0) or (FNewProcesses.Count > 0);
      if Update then
        BeginUpdate;
      try
          for i := 0 to FExitedProcesses.Count - 1 do begin
            Process := TProcess(FExitedProcesses.Objects[i]);
            if Assigned(FOnBeforeProcessRemove) then
              FOnBeforeProcessRemove(Self, Process);
            RemoveProcess(Process);
            FProcesses.Erase(FExitedProcesses[i]);
          end;
          // only required with GetModuleFileNameEx
          if not FNewProcessFilenameFuncAvailable
              and not Full and not RemoveOnly then
            FProcesses.Iterate2(UpdatePathsProc, False, Self);
          for i := 0 to FNewProcesses.Count - 1 do begin
            TempProcess := PTempProcess(FNewProcesses.Objects[i]);
            Process := AddProcess(TempProcess);
            FProcesses.Insert(FNewProcesses[i], Process);
            if Assigned(FOnProcessAdded) then
              FOnProcessAdded(Self, Process);
          end;
      finally
        if Update then begin
          UpdateAllChecked; // added or deleted items can change 'all checked'-state
          EndUpdate;
        end;
      end;
    finally
      if Full then begin
        EndUpdate;
        // TODO: bad practice? remove?
        LockWindowUpdate(0);
      end;
    end;
    if (Full or Update) and not Startup then
      RefreshImageInfo;
  finally
    FRedraw := True;
  end;
  //outputdebugstring('stop');
end;

procedure TCustomProcessListView.RefreshImageInfo;
var
  i: Integer;
  Item: TProcessListItem;
  Path: String;
begin
  for i := 0 to Items.Count - 1 do begin
    Item := TProcessListItem(Items[i]);
    if not Item.FUpdated then begin
      if FProcessPaths.Search(GetProcessHash(Item.ProcessID{,
          Item.FProcessHandle}), Path) and Assigned(FUpdateThread) then
        FUpdateThread.EnqueueItem(Path, False);
    end;
  end;
end;

function TCustomProcessListView.AddProcess(Process: PTempProcess): TProcess;
var
  hProc: Integer;
  IsProtected: Boolean;
begin
  //Process.HandleNeeded := True;
  hProc := OpenProc(Process.ProcessID, IsProtected);
  Result := TProcess.Create(Self, Process.ProcessID, hProc, IsProtected);
  // TODO: remove?
  if Result.ProcessHandle = 0 then
    outputdebugstring(pchar('openprocess failed for ' + inttostr(Process.ProcessID)));
  Result.SetPath(Process.Path);
  Result.UpdatePath;
  RefreshProcess(Result, True);
  AddProcessToImage(Result.Path, Result, False);
  //outputdebugstring(pchar('new: ' + inttostr(procid) + ' ' + path));
end;

procedure TCustomProcessListView.RemoveProcess(Process: TProcess);
begin
  Process.SetItem(nil);
  RemoveProcessFromImage(Process.Path, Process);
  //outputdebugstring(pchar('exit: ' + inttostr(item.ProcessID) + ' ' + item.path));
end;

procedure TCustomProcessListView.AddProcessToImage(Path: String;
    Process: TProcess; RefreshImageInfo: Boolean);
var
  Hash: String;
  Image: TExeImage;
  Data: Pointer;
begin
  if IsWin64 then begin
    if not NewFileExistsRedir(True, Path) then // Wow64 FS redirection aware
      Exit;
  end
  else begin
    if not NewFileExists(Path) then
      Exit;
  end;
  Hash := Process.GetHash;
  // TODO: remove try-except or specify exception
  try
    FProcessPaths.Erase(Hash);
  except
  end;
  FProcessPaths.Insert(Hash, Path);
  with FImages.AcquireAccess do try
    Data := nil;
    if Search(Path, Data) then
      Image := TExeImage(Data)
    else begin
      Image := TExeImage.Create(Self);
      Insert(Path, Image);
      if RefreshImageInfo and Assigned(FUpdateThread) then
        FUpdateThread.EnqueueItem(Path, False);
    end;
    Image.AddProcess(Process);
  finally
    FImages.ReleaseAccess;
  end;
end;

procedure TCustomProcessListView.RemoveProcessFromImage(Path: String;
    Process: TProcess);
var
  Image: TExeImage;
  Data: Pointer;
begin
  // TODO: remove try-except or specify exception
  try
    FProcessPaths.Erase(Process.GetHash);
  except
  end;
  with FImages.AcquireAccess do try
    if Search(Path, Data) then begin
      Image := TExeImage(Data);
      Image.RemoveProcess(Process);
      if Image.FProcesses.Count = 0 then
        Erase(Path);
    end;
  finally
    FImages.ReleaseAccess;
  end;
end;

procedure TCustomProcessListView.Refresh(Full: Boolean);
begin
  if Assigned(FTimerThread) then
    FTimerThread.Reset;
  RefreshProcesses(False, Full, False);
end;

procedure TCustomProcessListView.StartTimer;
begin
  if (not (csDesigning in ComponentState)) and not Assigned(FTimerThread) then
    FTimerThread := TTimerThread.Create(OnTimer, FRefreshInterval);
end;

procedure TCustomProcessListView.StopTimer;
begin
  if Assigned(FTimerThread) then begin
    FTimerThread.Stop;
    // TODO: free timer?
    FTimerThread := nil;
  end;
end;

procedure TCustomProcessListView.OnTimer(Sender: TObject);
begin
  RefreshProcesses(False, False, False);
end;

procedure TCustomProcessListView.UpdateImage(Sender: TObject;
    const Item: String; var Data: Pointer);
var
  FileInfo: TSHFileInfo;
  {Data: Pointer;
  Image: TExeImage;
  ImageInfo: TExeImageInfo;}
  ImageInfo: PExeImageInfo;
  VersionInfo: TFileVersionInfo;
  PrevRedirState: TPreviousFsRedirectionState;
begin
  {with FImages.AcquireAccess do try
    if Search(Item, Data) then begin
      Image := TExeImage(Data);
      if Image.FInfoLoaded then
        Exit;
    end
    else
      Exit;
  finally
    FImages.ReleaseAccess;
  end;}
  New(ImageInfo);
  InitializeImageInfo(ImageInfo^);
  if IsWin64 then
    DisableFsRedirectionIf(True, PrevRedirState);
  try
    // TODO: check if this works without SHGFI_SYSICONINDEX for UNC paths
    // possible solution: one imagelist for each UNC image
    if not PathIsUNC(PChar(Item)) then begin
      FillChar(FileInfo, SizeOf(FileInfo), 0);
      FileInfo.iIcon := -1;
      SHGetFileInfo(PChar(Item), 0, FileInfo, SizeOf(FileInfo),
          SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
      if FileInfo.iIcon > -1 then
        ImageInfo.FileIconIndex := FileInfo.iIcon
    end;
    VersionInfo := FileVersionInfo(Item);
    ImageInfo.Description := VersionInfo.FileDescription;
    ImageInfo.Company := VersionInfo.CompanyName;
  finally
    if IsWin64 then
      RestoreFsRedirection(PrevRedirState);
  end;
  Data := ImageInfo;
  {with FImages.AcquireAccess do try
    if Search(Item, Data) then
      TExeImage(Data).SetImageInfo(ImageInfo);
  finally
    FImages.ReleaseAccess;
  end;}
end;

procedure TCustomProcessListView.ImageUpdated(Sender: TObject;
    const Item: String; var Data: Pointer);
var
  ImageInfo: PExeImageInfo;
  pImage: Pointer;
  Image: TExeImage;
  {Process: TProcess;
  i: Integer;}
begin
  ImageInfo := PExeImageInfo(Data);
  try
    with FImages.AcquireAccess do try
      if Search(Item, pImage) then begin
        Image := TExeImage(pImage);
        Image.SetImageInfo(PExeImageInfo(Data)^);
        {for i := 0 to Image.FProcesses.Count - 1 do begin
          Process := TProcess(Image.FProcesses[i]);
          Process.SetImageInfo(Image.FImageInfo);
          if Assigned(Process.Item) then begin
            Process.Item.FUpdated := True;
            RefreshItem(Process.Item, Process, False);
          end;
        end;}
      end;
    finally
      FImages.ReleaseAccess;
    end;
  finally
    ImageInfo.Description := ''; // IMPORTANT: prevents memory leak!
    ImageInfo.Company := '';
    Dispose(ImageInfo);
    Data := nil;
  end;
end;

procedure TCustomProcessListView.UpdateDone(Sender: TObject);
begin
  if ((LastColumnClicked = FDescColumnIdx) or
      (LastColumnClicked = FCompanyColumnIdx))
      and AutoResort then
    Resort;
end;

{type
  TSortData = record
    ListView: TCustomProcessListView;
    OldSortProc: TTVCompare;
    OldData: Integer;
  end;
  PSortData = ^TSortData;

function CustomSortProc(Item1, Item2: TListItem;
    SortData: PSortData): Integer stdcall;
const
  GreaterSmaller: array[Boolean, Boolean] of Integer =
    ((1, -1), (-1, 1));
begin
  if Item1 = SortData.ListView.FCheckAllItem then
    Result := GreaterSmaller[SortData.ListView.CurrentSortAscending][True]
  else if Item2 = SortData.ListView.FCheckAllItem then
    Result := GreaterSmaller[SortData.ListView.CurrentSortAscending][False]
  else
    Result := SortData.OldSortProc(Integer(Item1), Integer(Item2),
        SortData.OldData);
end;


function TCustomProcessListView.CustomSort(SortProc: TTVCompare;
    Data: Longint): Boolean;
var
  SortData: TSortData;
begin
  SortData.ListView := Self;
  SortData.OldSortProc := SortProc;
  SortData.OldData := Data;
  Result := inherited CustomSort(@CustomSortProc, Integer(@SortData));
end;}

function TCustomProcessListView.CustomSortProc(Item1, Item2: TListItem;
    var CompResult: integer): Boolean;
begin
  Result := False;
  if Item1 = FCheckAllItem then
    CompResult := -1
  else if Item2 = FCheckAllItem then
    CompResult := 1
  else
    Result := True;
end;

function TCustomProcessListView.GetTotalProcessCount: Integer;
begin
  Result := FProcesses.Count;
end;

{ TListColumnProperty }

{function TListColumnProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TListColumnProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

function TListColumnProperty.GetValue: string;
begin
  if GetOrdValue = 0 then
    Result := ''
  else
    Result := TListColumn(GetOrdValue).DisplayName;
end;

procedure TListColumnProperty.GetValues(Proc: TGetStrProc);
var
  HostListView: TCustomProcessListView;
  i: Integer;
begin
  HostListView := TCustomProcessListView(GetComponent(0));
  if HostListView <> nil then begin
    for i := 0 to HostListView.Columns.Count - 1 do
      Proc(HostListView.Columns[i].DisplayName);
  end;
end;

procedure TListColumnProperty.SetValue(const Value: string);
var
  ListColumn: TListColumn;
  HostListView: TCustomProcessListView;
  i: Integer;
begin
  ListColumn := nil;
  if Value <> '' then begin
      HostListView := TCustomProcessListView(GetComponent(0));
      if HostListView <> nil then begin
        for i := 0 to HostListView.Columns.Count - 1 do begin
          if HostListView.Columns[i].DisplayName = Value then begin
            ListColumn := HostListView.Columns[i];
            Break;
          end;
        end;
      end;
  end;
  SetOrdValue(Longint(ListColumn));
end;}

procedure Register;
begin
  {RegisterPropertyEditor(TypeInfo(TListColumn), TCustomProcessListView, '',
      TListColumnProperty);}
  RegisterComponents('BM', [TProcessListView]);
end;

end.
