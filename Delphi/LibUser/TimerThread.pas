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
 * TThread descendant for generating timed events
 *
 ****************************************************************************)

unit TimerThread;

interface

uses Windows, Classes;

type
  TTimerThread = class(TThread)
  private
    FSignalEvent: Integer;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
  private
    procedure SetInterval(Interval: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create(OnTimer: TNotifyEvent; Interval: Cardinal);
    destructor Destroy; override;
    procedure Stop;
    procedure Reset;
    procedure Timer;
    property Interval: Cardinal read FInterval write SetInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

constructor TTimerThread.Create(OnTimer: TNotifyEvent; Interval: Cardinal);
begin
  FSignalEvent := CreateEvent(nil, False, False, nil);
  FreeOnTerminate := True;
  SetInterval(Interval);
  FOnTimer := OnTimer;
  inherited Create(False);
end;

destructor TTimerThread.Destroy;
begin
  CloseHandle(FSignalEvent);
  inherited;
end;

procedure TTimerThread.Stop;
begin
  Terminate;
  SetEvent(FSignalEvent);
end;

procedure TTimerThread.Reset;
begin
  SetEvent(FSignalEvent);
end;

procedure TTimerThread.Execute;
begin
  while not Terminated do begin
    if WaitForSingleObject(FSignalEvent, FInterval) = WAIT_TIMEOUT then
      Synchronize(Timer);
  end;
end;

procedure TTimerThread.SetInterval(Interval: Cardinal);
begin
  if (Interval <> FInterval) and (Interval > 0) then
    FInterval := Interval;
end;

procedure TTimerThread.Timer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

end.
 