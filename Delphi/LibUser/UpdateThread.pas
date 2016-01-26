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
 * TThread descendant for processing asynchronous updates
 *
 ****************************************************************************)

unit UpdateThread;

interface

uses Windows, Classes, SyncObjs, Common2;

type
  TItemEvent = procedure(Sender: TObject; const Item: String;
      var Data: Pointer) of object;

type
  TUpdateThread = class(TThread)
  private
    FOnUpdate: TItemEvent;
    FOnUpdated: TItemEvent;
    FOnDone: TNotifyEvent;
    FList: TStringList;
    FTerminateEvent: TEvent;
    FItemAddedEvent: TEvent;
    FDoneEvent: TEvent;
    FItemBusy: Boolean;
    FCallEvents: Boolean;
    FClear: Boolean;
    FUpdating: Boolean;
    procedure SyncUpdated;
    procedure SyncDone;
    procedure Enqueue(Item: String; NewIdx: Integer);
  public
    CurrentItem: String;
    CurrentData: Pointer;
    Busy: Boolean;
    constructor Create(CreateSuspended: Boolean;
        OnUpdate, OnUpdated: TItemEvent; OnDone: TNotifyEvent);
    destructor Destroy; override;
    procedure Execute; override;
    procedure EnqueueItem(Item: String; First: Boolean);
    procedure EnqueueItems(Items: TStringList; First: Boolean);
    procedure ClearItems;
    function ContainsItem(Item: String): Boolean;
    procedure Terminate;
  end;

implementation

constructor TUpdateThread.Create(CreateSuspended: Boolean;
    OnUpdate, OnUpdated: TItemEvent; OnDone: TNotifyEvent);
begin
  FreeOnTerminate := True;
  Priority := tpLowest;
  FList := TStringList.Create;
  FList.Sorted := False;
  FTerminateEvent := TEvent.Create(nil, False, False, '');
  FItemAddedEvent := TEvent.Create(nil, True, False, '');
  FDoneEvent := TEvent.Create(nil, True, False, '');
  Busy := False;
  FItemBusy := False;
  FClear := False;
  FUpdating := False;
  FOnUpdate := OnUpdate;
  FOnUpdated := OnUpdated;
  FOnDone := OnDone;
  CurrentItem := '';
  CurrentData := nil;
  inherited Create(CreateSuspended);
end;

destructor TUpdateThread.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FTerminateEvent);
  FreeAndNil(FItemAddedEvent);
  FreeAndNil(FDoneEvent);
end;

procedure TUpdateThread.Execute;

  procedure WalkList(List: TStringList);
  begin
    FItemBusy := True;
    try
      while (List.Count > 0) do begin
        if FClear then
          Exit;
        CurrentItem := List[0];
        List.Delete(0);
        if FClear then
          Exit;
        if FCallEvents and Assigned(FOnUpdate) then
          FOnUpdate(Self, CurrentItem, CurrentData);
        FUpdating := True;
        try
          Synchronize(SyncUpdated);
        finally
          FUpdating := False;
          CurrentData := nil;
        end;
        if FClear or Terminated then
          Exit;
      end;
      CurrentItem := '';
    finally
      FItemBusy := False;
    end;
  end;
  
var
  Handles: array[0..1] of THandle;
begin
  Handles[0] := FItemAddedEvent.Handle;
  Handles[1] := FTerminateEvent.Handle;
  FDoneEvent.ResetEvent;
  repeat
    Busy := True;
    FCallEvents := True;
    try
      WalkList(FList);
    finally
      if FCallEvents then
        Synchronize(SyncDone);
      FDoneEvent.SetEvent;
      Busy := False;
    end;
    if Terminated then
      Exit;
    if WaitForMultipleObjects(2, @Handles[0], False,
        INFINITE) = WAIT_OBJECT_0 then
      FItemAddedEvent.ResetEvent;
  until Terminated;
end;

procedure TUpdateThread.Enqueue(Item: String; NewIdx: Integer);
var
  Idx: Integer;
begin
  Idx := FList.IndexOf(Item);
  if Idx > -1 then begin
    if (NewIdx > -1) and (Idx <> NewIdx) then
      FList.Delete(Idx)
    else
      Exit;
  end {else begin
    if Assigned(ItemAddedEvent) then
      ItemAddedEvent(Self, Item, CurrentData)
  end;};
  if NewIdx > -1 then
    FList.Insert(NewIdx, Item)
  else
    FList.Add(Item);
end;

procedure TUpdateThread.EnqueueItem(Item: String; First: Boolean);
begin
  if First then
    Enqueue(Item, 0)
  else
    Enqueue(Item, -1);
  FItemAddedEvent.SetEvent;
end;

procedure TUpdateThread.EnqueueItems(Items: TStringList; First: Boolean);
var
  i: Integer;
begin
  if not Assigned(Items) then
    Exit;
  for i :=  0 to Items.Count - 1 do begin
    if First then
      Enqueue(Items[i], i)
    else
      Enqueue(Items[i], -1);
  end;
  FItemAddedEvent.SetEvent;
end;

procedure TUpdateThread.SyncUpdated;
begin
  if Assigned(FOnUpdated) then
    FOnUpdated(Self, CurrentItem, CurrentData);
end;

procedure TUpdateThread.SyncDone;
begin
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

procedure TUpdateThread.ClearItems;
var
  i: Integer;
begin
  FClear := True;
  try
    FCallEvents := False;
    for i := FList.Count - 1 downto 0 do begin
      FList.Delete(i);
    end;
  finally
    FClear := False;
  end;
end;

function TUpdateThread.ContainsItem(Item: String): Boolean;
begin
  Result := (Item = CurrentItem) or (FList.IndexOf(Item) > -1);
end;

procedure TUpdateThread.Terminate;
begin
  inherited;
  FTerminateEvent.SetEvent;
end;

end.
