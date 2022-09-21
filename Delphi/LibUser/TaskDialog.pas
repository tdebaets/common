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
 * Windows Task Dialog API declarations
 *
 ****************************************************************************)

unit TaskDialog;

interface

uses Windows, Messages;

// *** The Task Dialog declarations require Windows >= Vista ***

type
  { $EXTERNALSYM PFTASKDIALOGCALLBACK}
  PFTASKDIALOGCALLBACK = function(hwnd: HWND; msg: UINT; wParam: WPARAM;
    lParam: LPARAM; lpRefData: Pointer): HResult; stdcall;
  TFTaskDialogCallback = PFTASKDIALOGCALLBACK;

const
  { Task Dialog Flags }

  TDF_ENABLE_HYPERLINKS               = $0001;
  TDF_USE_HICON_MAIN                  = $0002;
  TDF_USE_HICON_FOOTER                = $0004;
  TDF_ALLOW_DIALOG_CANCELLATION       = $0008;
  TDF_USE_COMMAND_LINKS               = $0010;
  TDF_USE_COMMAND_LINKS_NO_ICON       = $0020;
  TDF_EXPAND_FOOTER_AREA              = $0040;
  TDF_EXPANDED_BY_DEFAULT             = $0080;
  TDF_VERIFICATION_FLAG_CHECKED       = $0100;
  TDF_SHOW_PROGRESS_BAR               = $0200;
  TDF_SHOW_MARQUEE_PROGRESS_BAR       = $0400;
  TDF_CALLBACK_TIMER                  = $0800;
  TDF_POSITION_RELATIVE_TO_WINDOW     = $1000;
  TDF_RTL_LAYOUT                      = $2000;
  TDF_NO_DEFAULT_RADIO_BUTTON         = $4000;
  TDF_CAN_BE_MINIMIZED                = $8000;

  { Task Dialog Messages }

  TDM_NAVIGATE_PAGE                   = WM_USER+101;
  TDM_CLICK_BUTTON                    = WM_USER+102; // wParam = Button ID
  TDM_SET_MARQUEE_PROGRESS_BAR        = WM_USER+103; // wParam = 0 (nonMarque) wParam != 0 (Marquee)
  TDM_SET_PROGRESS_BAR_STATE          = WM_USER+104; // wParam = new progress state
  TDM_SET_PROGRESS_BAR_RANGE          = WM_USER+105; // lParam = MAKELPARAM(nMinRange, nMaxRange)
  TDM_SET_PROGRESS_BAR_POS            = WM_USER+106; // wParam = new position
  TDM_SET_PROGRESS_BAR_MARQUEE        = WM_USER+107; // wParam = 0 (stop marquee), wParam != 0 (start marquee), lparam = speed (milliseconds between repaints)
  TDM_SET_ELEMENT_TEXT                = WM_USER+108; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
  TDM_CLICK_RADIO_BUTTON              = WM_USER+110; // wParam = Radio Button ID
  TDM_ENABLE_BUTTON                   = WM_USER+111; // lParam = 0 (disable), lParam != 0 (enable), wParam = Button ID
  TDM_ENABLE_RADIO_BUTTON             = WM_USER+112; // lParam = 0 (disable), lParam != 0 (enable), wParam = Radio Button ID
  TDM_CLICK_VERIFICATION              = WM_USER+113; // wParam = 0 (unchecked), 1 (checked), lParam = 1 (set key focus)
  TDM_UPDATE_ELEMENT_TEXT             = WM_USER+114; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
  TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER+115; // wParam = Button ID, lParam = 0 (elevation not required), lParam != 0 (elevation required)
  TDM_UPDATE_ICON                     = WM_USER+116; // wParam = icon element (TASKDIALOG_ICON_ELEMENTS), lParam = new icon (hIcon if TDF_USE_HICON_* was set, PCWSTR otherwise)

  { Task Dialog Notifications }

  TDN_CREATED                = 0;
  TDN_NAVIGATED              = 1;
  TDN_BUTTON_CLICKED         = 2;            // wParam = Button ID
  TDN_HYPERLINK_CLICKED      = 3;            // lParam = (LPCWSTR)pszHREF
  TDN_TIMER                  = 4;            // wParam = Milliseconds since dialog created or timer reset
  TDN_DESTROYED              = 5;
  TDN_RADIO_BUTTON_CLICKED   = 6;            // wParam = Radio Button ID
  TDN_DIALOG_CONSTRUCTED     = 7;
  TDN_VERIFICATION_CLICKED   = 8;            // wParam = 1 if checkbox checked, 0 if not, lParam is unused and always 0
  TDN_HELP                   = 9;
  TDN_EXPANDO_BUTTON_CLICKED = 10;           // wParam = 0 (dialog is now collapsed), wParam != 0 (dialog is now expanded)

type
  { $EXTERNALSYM TASKDIALOG_BUTTON}
  TASKDIALOG_BUTTON = packed record
    nButtonID: Integer;
    pszButtonText: LPCWSTR;
  end;
  { $EXTERNALSYM _TASKDIALOG_BUTTON}
  _TASKDIALOG_BUTTON = TASKDIALOG_BUTTON;
  PTaskDialogButton = ^TTaskDialogButton;
  TTaskDialogButton = TASKDIALOG_BUTTON;

const
  { Task Dialog Elements }

  TDE_CONTENT              = 0;
  TDE_EXPANDED_INFORMATION = 1;
  TDE_FOOTER               = 2;
  TDE_MAIN_INSTRUCTION     = 3;

  { Task Dialog Icon Elements }

  TDIE_ICON_MAIN           = 0;
  TDIE_ICON_FOOTER         = 1;

  { Task Dialog Common Icons }

  TD_WARNING_ICON         = MAKEINTRESOURCEW(Word(-1));
  TD_ERROR_ICON           = MAKEINTRESOURCEW(Word(-2));
  TD_INFORMATION_ICON     = MAKEINTRESOURCEW(Word(-3));
  TD_SHIELD_ICON          = MAKEINTRESOURCEW(Word(-4));

  { Task Dialog Button Flags }

  TDCBF_OK_BUTTON            = $0001;  // selected control return value IDOK
  TDCBF_YES_BUTTON           = $0002;  // selected control return value IDYES
  TDCBF_NO_BUTTON            = $0004;  // selected control return value IDNO
  TDCBF_CANCEL_BUTTON        = $0008;  // selected control return value IDCANCEL
  TDCBF_RETRY_BUTTON         = $0010;  // selected control return value IDRETRY
  TDCBF_CLOSE_BUTTON         = $0020;  // selected control return value IDCLOSE

type
  { $EXTERNALSYM TASKDIALOGCONFIG}
  TASKDIALOGCONFIG = packed record
    cbSize: UINT;
    hwndParent: HWND;
    hInstance: HINST;                     // used for MAKEINTRESOURCE() strings
    dwFlags: DWORD;                       // TASKDIALOG_FLAGS (TDF_XXX) flags
    dwCommonButtons: DWORD;               // TASKDIALOG_COMMON_BUTTON (TDCBF_XXX) flags
    pszWindowTitle: LPCWSTR;              // string or MAKEINTRESOURCE()
    case Integer of
      0: (hMainIcon: HICON);
      1: (pszMainIcon: LPCWSTR;
          pszMainInstruction: LPCWSTR;
          pszContent: LPCWSTR;
          cButtons: UINT;
          pButtons: PTaskDialogButton;
          nDefaultButton: Integer;
          cRadioButtons: UINT;
          pRadioButtons: PTaskDialogButton;
          nDefaultRadioButton: Integer;
          pszVerificationText: LPCWSTR;
          pszExpandedInformation: LPCWSTR;
          pszExpandedControlText: LPCWSTR;
          pszCollapsedControlText: LPCWSTR;
          case Integer of
            0: (hFooterIcon: HICON);
            1: (pszFooterIcon: LPCWSTR;
                pszFooter: LPCWSTR;
                pfCallback: TFTaskDialogCallback;
                lpCallbackData: Pointer;
                cxWidth: UINT  // width of the Task Dialog's client area in DLU's.
                               // If 0, Task Dialog will calculate the ideal width.
              );
          );
  end;
  _TASKDIALOGCONFIG = TASKDIALOGCONFIG;
  PTaskDialogConfig = ^TTaskDialogConfig;
  TTaskDialogConfig = TASKDIALOGCONFIG;

type
  TTaskDialogIndirect = function(const pTaskConfig: TTaskDialogConfig;
    pnButton: PInteger; pnRadioButton: PInteger;
    pfVerificationFlagChecked: PBOOL): HRESULT; stdcall;

  TTaskDialog = function(hwndParent: HWND; hInstance: HINST; pszWindowTitle,
    pszMainInstruction, pszContent: LPCWSTR; dwCommonButtons: DWORD;
    pszIcon: LPCWSTR; pnButton: PInteger): HRESULT; stdcall;

implementation

end.
 