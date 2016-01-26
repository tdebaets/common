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
 * Multi-monitor helper functions
 *
 ****************************************************************************)

unit MonitorFunc;

interface

uses Windows, Classes, Math, MultiMon;

function GetDisplayMonitors(Monitors: TList): Boolean;
function GetDisplayWorkArea(hWnd: HWND): TRect; overload;
function GetDisplayWorkArea(Pt: TPoint): TRect; overload;

const
  MONITOR_CENTER    = $0001;        // center rect to monitor
  MONITOR_CLIP      = $0000;        // clip rect to monitor
  MONITOR_WORKAREA  = $0002;        // use monitor work area
  MONITOR_AREA      = $0000;        // use monitor entire area

procedure ClipOrCenterRectToMonitor(var prc: TRect; flags: UINT);
procedure ClipOrCenterWindowToMonitor(hwnd: HWND; flags: UINT);

function CenterWindow(hwndChild, hwndParent: HWND): Boolean;

implementation

function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect;
    Monitors: TList): Boolean; stdcall;
begin
  Result := True;
  if Assigned(Monitors) then
    Monitors.Add(Pointer(hm));
end;

function GetDisplayMonitors(Monitors: TList): Boolean;
begin
  Result := False;
  if not Assigned(Monitors) then
    Exit;
  Monitors.Clear;
  Result := EnumDisplayMonitors(0, nil, @MonitorEnumProc, Integer(Monitors));
end;

function GetDisplayWorkArea(hWnd: HWND): TRect;
var
  hMon: HMONITOR;
  mi: MONITORINFO;
begin
  hMon := MonitorFromWindow(hWnd, MONITOR_DEFAULTTONEAREST);
  if hMon <> 0 then begin
    FillChar(mi, SizeOf(mi), 0);
    mi.cbSize := SizeOf(mi);
    if GetMonitorInfo(hMon, @mi) then
      Result := mi.rcWork;
  end;
end;

function GetDisplayWorkArea(Pt: TPoint): TRect;
var
  hMon: HMONITOR;
  mi: MONITORINFO;
begin
  hMon := MonitorFromPoint(Pt, MONITOR_DEFAULTTONEAREST);
  if hMon <> 0 then begin
    FillChar(mi, SizeOf(mi), 0);
    mi.cbSize := SizeOf(mi);
    if GetMonitorInfo(hMon, @mi) then
      Result := mi.rcWork;
  end;
end;

//
//  ClipOrCenterRectToMonitor
//
//  The most common problem apps have when running on a
//  multimonitor system is that they "clip" or "pin" windows
//  based on the SM_CXSCREEN and SM_CYSCREEN system metrics.
//  Because of app compatibility reasons these system metrics
//  return the size of the primary monitor.
//
//  This shows how you use the multi-monitor functions
//  to do the same thing.
//
procedure ClipOrCenterRectToMonitor(var prc: TRect; flags: UINT);
var
  hMon: HMONITOR;
  mi: MONITORINFO;
  rc: TRect;
  w, h: Integer;
begin
  w := prc.right - prc.left;
  h := prc.bottom - prc.top;
  //
  // get the nearest monitor to the passed rect.
  //
  hMon := MonitorFromRect(@prc, MONITOR_DEFAULTTONEAREST);
  //
  // get the work area or entire monitor rect.
  //
  mi.cbSize := SizeOf(mi);
  GetMonitorInfo(hMon, @mi);

  if flags and MONITOR_WORKAREA <> 0 then
    rc := mi.rcWork
  else
    rc := mi.rcMonitor;

  //
  // center or clip the passed rect to the monitor rect
  //
  if flags and MONITOR_CENTER <> 0 then begin
    prc.left   := rc.left + (rc.right  - rc.left - w) div 2;
    prc.top    := rc.top  + (rc.bottom - rc.top  - h) div 2;
    prc.right  := prc.left + w;
    prc.bottom := prc.top  + h;
  end
  else begin
    prc.left   := MaxIntValue([rc.left, MinIntValue([rc.right-w,  prc.left])]);
    prc.top    := MaxIntValue([rc.top,  MinIntValue([rc.bottom-h, prc.top])]);
    prc.right  := prc.left + w;
    prc.bottom := prc.top  + h;
  end;
end;

procedure ClipOrCenterWindowToMonitor(hwnd: HWND; flags: UINT);
var
  rc: TRect;
begin
  GetWindowRect(hwnd, rc);
  ClipOrCenterRectToMonitor(rc, flags);
  SetWindowPos(hwnd, 0, rc.left, rc.top, 0, 0,
      SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

// Center with respect to another window.
// Specifying NULL for hwndParent centers hwndChild relative to the
// screen.
function CenterWindow(hwndChild, hwndParent: HWND): Boolean;
var
  rcChild, rcParent, rcScreen: TRect;
  cxChild, cyChild, cxParent, cyParent: Integer;
  xNew, yNew: Integer;
begin

  // Get the Height and Width of the child window.
  GetWindowRect(hwndChild, rcChild);
  cxChild := rcChild.right - rcChild.left;
  cyChild := rcChild.bottom - rcChild.top;

  rcScreen := GetDisplayWorkArea(hWndChild);

  if hwndParent <> 0 then begin
    // Get the Height and Width of the parent window.
    GetWindowRect(hwndParent, rcParent);
  end
  else
    rcParent := rcScreen;

  cxParent := rcParent.right - rcParent.left;
  cyParent := rcParent.bottom - rcParent.top;

  // Calculate new X position, then adjust for screen.
  xNew := rcParent.left + ((cxParent - cxChild) div 2);
  if xNew < rcScreen.Left then
    xNew := rcScreen.Left
  else if (xNew + cxChild) > rcScreen.Right then
    xNew := rcScreen.Right - cxChild;

  // Calculate new Y position, then adjust for screen.
  yNew := rcParent.top  + ((cyParent - cyChild) div 2);
  if yNew < rcScreen.Top then
    yNew := rcScreen.Top
  else if (yNew + cyChild) > rcScreen.Bottom then
    yNew := rcScreen.Bottom - cyChild;

  // Set it, and return.
  Result := SetWindowPos(hwndChild, 0, xNew, yNew, 0, 0,
      SWP_NOSIZE or SWP_NOZORDER);
end;


end.
