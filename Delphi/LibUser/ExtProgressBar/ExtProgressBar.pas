{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsExtProgressBar v2.06                                                     }
{------------------------------------------------------------------------------}
{ A progress bar control that enables access to the new style types and large  }
{ range values provided by the updated progress bar control.                   }
{                                                                              }
{ Copyright 2000-2001, Brad Stowers.  All Rights Reserved.                     }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TDFSColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See ExtProgressBar.txt for notes, known issues, and revision history.        }
{ -----------------------------------------------------------------------------}
{ Date last modified:  June 28, 2001                                           }
{ -----------------------------------------------------------------------------}

unit ExtProgressBar;

{$IFNDEF DFS_WIN32}
  ERROR!  This unit only available for Delphi 2.0 and above!!!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CommCtrl, ComCtrls;


const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsExtProgressBar v2.06';

{ I can't get PBM_SETBKCOLOR (the BkColor property) to work at all.  If you want
  to have a go at it, enable this define. }

{.$DEFINE DFS_TRY_BKCOLOR}


{$IFDEF DFS_COMPILER_2}
// Internal use types and constants.  These are converted from the new COMMCTRL.H file.
type
  PPBRange = ^TPBRange;
  TPBRange = record
    iLow:  integer;
    iHigh: integer;
  end;
{$ENDIF}


{$IFDEF DFS_COMPILER_2}
const
  PBM_SETRANGE32     = WM_USER+6;      // lParam = high, wParam = low
  PBM_GETRANGE       = WM_USER+7;      // wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
  PBM_GETPOS         = WM_USER+8;
{$ENDIF}

{ C++Builder 3 and Delphi 4 define these in COMMCTRL.PAS, but no others do }
{$IFNDEF DFS_DELPHI_4_UP}
{$IFNDEF DFS_CPPB_3_UP}
const
  CCM_FIRST          = $2000;          // Common control shared messages
  CCM_SETBKCOLOR     = CCM_FIRST + 1;  // lParam is bkColor

  PBM_SETBARCOLOR    = WM_USER+9;      // lParam = bar color
  PBM_SETBKCOLOR     = CCM_SETBKCOLOR; // lParam = bkColor

  PBS_SMOOTH         = $01;
  PBS_VERTICAL       = $04;
{$ENDIF}
{$ENDIF}



const
  DEF_COLOR     = clBtnFace;
  DEF_SEL_COLOR = clHighlight;


type
  {$IFNDEF DFS_COMPILER_4_UP}
  TProgressBarOrientation = (pbHorizontal, pbVertical);
  {$ENDIF}

  // The new class
  TdfsExtProgressBar = class(TProgressBar)
  private
    // Internal property variables
    {$IFNDEF DFS_COMPILER_4_UP}
    FPosition: integer;
    FMin: integer;
    FMax: integer;
    FOrientation: TProgressBarOrientation;
    FSmooth: boolean;
    {$ENDIF}
    FColor: TColor;
    FSelectionColor: TColor;

    {$IFNDEF DFS_TRY_BKCOLOR}
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    {$ENDIF}

    // Property methods
    {$IFNDEF DFS_COMPILER_4_UP}
    procedure SetMin(Val: integer);
    procedure SetMax(Val: integer);
    procedure SetParams(AMin, AMax: integer);
    procedure SetPosition(Val: integer);
    function GetPosition: integer;
    procedure SetSmooth(const Value: boolean);
    {$ENDIF}
    function GetOrientation: TProgressBarOrientation;
    procedure SetOrientation(const Value: TProgressBarOrientation);
    procedure SetColor(Val: TColor);
    procedure SetSelectionColor(val: TColor);
    function GetVersion: string;
    procedure SetVersion(const Val: string);
  protected
    // Overriden methods
    procedure CreateWnd; override;
    {$IFDEF DFS_COMPILER_4_UP}
    procedure DestroyWnd; override;
    {$ENDIF}
    {$IFNDEF DFS_COMPILER_4_UP}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;
    property SelectionColor: TColor
       read FSelectionColor
       write SetSelectionColor
       default DEF_SEL_COLOR;
    property Color: TColor
       read FColor
       write SetColor
       default DEF_COLOR;
    property Orientation: TProgressBarOrientation
       read GetOrientation
       write SetOrientation
       default pbHorizontal;

    {$IFNDEF DFS_COMPILER_4_UP}
    // Properties overriden from the ancestor.
    property Smooth: boolean
       read FSmooth
       write SetSmooth
       default FALSE;
    property Min: integer
       read FMin
       write SetMin;
    property Max: integer
       read FMax
       write SetMax;
    property Position: integer
       read GetPosition
       write SetPosition
       default 0;
    {$ENDIF}
  end;


implementation

uses
  Consts;

constructor TdfsExtProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Zero out the internal variables.
  {$IFNDEF DFS_COMPILER_4_UP}
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FSmooth := FALSE;
  FOrientation := pbHorizontal;
  {$ENDIF}
  FColor := DEF_COLOR;
  FSelectionColor := DEF_SEL_COLOR;
end;

// CreateWnd is responsible for actually creating the window (value of Handle).
// As soon as the window is created, we need to set it to our values.
procedure TdfsExtProgressBar.CreateWnd;
begin
  inherited CreateWnd;

  {$IFNDEF DFS_COMPILER_4_UP}
  // Set the 32-bit min and max range.
  SendMessage(Handle, PBM_SETRANGE32, FMin, FMax);
  // Set the 32-bit position value.
  SendMessage(Handle, PBM_SETPOS, FPosition, 0);
  {$ENDIF}
  // Set the colors
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FSelectionColor));
{$IFDEF DFS_TRY_BKCOLOR}
  SendMessage(Handle, PBM_SETBKCOLOR, 0, ColorToRGB(FColor));
{$ENDIF}
end;

{$IFDEF DFS_COMPILER_4_UP}
// Delphi 4 loses the position on window recreate usually.
procedure TdfsExtProgressBar.DestroyWnd;
var
  TempPos: integer;
begin
  // Get current value
  TempPos := Position;
  // Kill the window handle
  inherited DestroyWnd;
  // Put the position value into TProgressBar's memory variable so it will be
  // reset in inherited CreateWnd
  Position := TempPos;
end;
{$ENDIF}

// CreateParams is responsible for providing all the parameters for describing the
// window to create.  The new vertical and smooth styles are window sytle flags, so
// we need to supply them here.

{$IFNDEF DFS_COMPILER_4_UP}
procedure TdfsExtProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    if FOrientation = pbVertical then Style := Style or PBS_VERTICAL;
    if FSmooth then Style := Style or PBS_SMOOTH;
  end;
end;
{$ENDIF}

// Loaded is called immediately after a component has been loaded from a stream, i.e
// a form (.DFM) file.
procedure TdfsExtProgressBar.Loaded;
var
  Temp: integer;
begin
  inherited Loaded;
  // If it's the new vertical style, and we are in the form designer (IDE), we have
  // to swap the width and height.
  if (csDesigning in ComponentState) and (Orientation = pbVertical) then
  begin
    Temp := Width;
    Width := Height;
    Height := Temp;
  end;
end;


// Utility function used by both SetMin and SetMax methods.

{$IFNDEF DFS_COMPILER_4_UP}
procedure TdfsExtProgressBar.SetParams(AMin, AMax: integer);
begin
  // Maximum can not be less than the minimum.
  if AMax < AMin then
    {$IFDEF DFS_COMPILER_2}
    raise EInvalidOperation.CreateResFmt(SPropertyOutOfRange, [Self.Classname]);
    {$ELSE}
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    {$ENDIF}
  // If neither value has changed, there's nothing to do.
  if (FMin <> AMin) or (FMax <> AMax) then begin
    // We can only send window messages if the window has been created (CreateWnd).
    if HandleAllocated then begin
      SendMessage(Handle, PBM_SETRANGE32, AMin, AMax);
      if FMin > AMin then // since Windows sets Position when increase Min..
        SendMessage(Handle, PBM_SETPOS, AMin, 0); // set it back if decrease
    end;
    FMin := AMin;
    FMax := AMax;
  end;
end;

// Update the Min property.
procedure TdfsExtProgressBar.SetMin(Val: integer);
begin
  SetParams(Val, FMax);
end;

// Update the Max property.
procedure TdfsExtProgressBar.SetMax(Val: integer);
begin
  SetParams(FMin, Val);
end;

// Read the current position of the progress bar.
function TdfsExtProgressBar.GetPosition: integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, PBM_GETPOS, 0, 0)
  else
    Result := FPosition;
end;

// Set the current position of the progress bar.
procedure TdfsExtProgressBar.SetPosition(Val: integer);
begin
  if HandleAllocated then
    SendMessage(Handle, PBM_SETPOS, Val, 0);
  FPosition := Val;
end;

procedure TdfsExtProgressBar.SetSmooth(const Value: boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    RecreateWnd;
  end;
end;

{$ENDIF}

procedure TdfsExtProgressBar.SetOrientation(const Value: TProgressBarOrientation);
begin
  if Orientation <> Value then
  begin
    // Swap width and height if orientation is changing in design mode
    if (csDesigning in ComponentState) then
      SetBounds(Left, Top, Height, Width);

    {$IFDEF DFS_COMPILER_4_UP}
    inherited Orientation := Value;
    {$ELSE}
    FOrientation := Value;
    RecreateWnd;
    {$ENDIF}
  end;
end;

function TdfsExtProgressBar.GetOrientation: TProgressBarOrientation;
begin
  {$IFDEF DFS_COMPILER_4_UP}
  Result := inherited Orientation;
  {$ELSE}
  Result := FOrientation;
  {$ENDIF}
end;


// Set the bar background color.
procedure TdfsExtProgressBar.SetSelectionColor(Val: TColor);
begin
  if HandleAllocated then
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(Val));
  FSelectionColor := Val;
end;


// Set the bar background color.
procedure TdfsExtProgressBar.SetColor(val: TColor);
begin
{$IFDEF DFS_TRY_BKCOLOR}
  if HandleAllocated then
    SendMessage(Handle, PBM_SETBKCOLOR, 0, ColorToRGB(Val));
{$ELSE}
  Invalidate;
{$ENDIF}
  FColor := Val;
end;

{$IFNDEF DFS_TRY_BKCOLOR}
procedure TdfsExtProgressBar.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
var
  Br: HBRUSH;
begin
  Msg.Result := 1;
  Br := CreateSolidBrush(ColorToRGB(FColor));
  try
    FillRect(Msg.DC, ClientRect, Br);
  finally
    DeleteObject(Br);
  end;
end;
{$ENDIF}

function TdfsExtProgressBar.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsExtProgressBar.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;


end.

