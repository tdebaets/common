{
  Inno Setup
  Copyright (C) 1997-2013 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

{ Determine Delphi/C++Builder version }
{$IFNDEF VER80}  { if it's not Delphi 1.0 }
  {$IFNDEF VER90}  { if it's not Delphi 2.0 }
    {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
      {$DEFINE IS_D3}  { then it must be at least Delphi 3 or C++Builder 3 }
      {$DEFINE Delphi3orHigher}
      {$IFNDEF VER100}  { if it's not Delphi 3.0 }
        {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
          {$DEFINE IS_D4}  { then it must be at least Delphi 4 or C++Builder 4 }
          {$IFNDEF VER120} {$IFNDEF VER125}  { if it's not Delphi 4 or C++Builder 4 }
            {$DEFINE IS_D5}  { then it must be at least Delphi 5 or C++Builder 5 }
            {$IFNDEF VER130}  { if it's not Delphi 5 or C++Builder 5 }
              {$DEFINE IS_D6}  { then it must be at least Delphi 6 or C++Builder 6 }
            {$ENDIF}
          {$ENDIF} {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{ Check for Delphi 7 and its WideString bug }
{$IFDEF IS_D6}
  {$IF Trunc(RTLVersion) = 15}
    { Delphi 7's WideString type is fundamentally flawed and should not be
      used; see QC #8565. If possible, you should compile with another version
      of Delphi (< 7 or 9).
      The bug is that it handles ANSI->Wide and Wide->ANSI conversions
      incorrectly; it uses the thread code page (CP_THREAD_ACP) instead of the
      system code page (CP_ACP). This is wrong because all of the "A" Windows
      APIs expect strings to use the system code page.
      In Inno Setup, this can result in shortcuts being created with incorrect
      names, since it converts ANSI->Wide when calling IPersistFile::Save.
      If you *really* want to compile under Delphi 7, you can remove this
      check. But expect breakage. }
    {$MESSAGE ERROR 'Delphi 7''s WideString type is broken. See comments in source code.'}
  {$IFEND}
{$ENDIF}

{ Check for Delphi 2007 and its "Halt in except block" bug }
{$IFDEF VER185}
{ On Delphi 2007, if Halt is called inside an 'except' or 'finally' block, the
  current exception's message is forcibly displayed on the screen (even if
  it's an EAbort exception), and the process returns with an exit code of 1
  instead of the exit code specified in the Halt call. See QC #51427.
  There are many places in Inno Setup that are affected by this issue (see
  the Halt calls in Setup.dpr for example). }
{$MESSAGE ERROR 'Delphi 2007''s Halt is broken. See comments in source code.'}
{$ENDIF}
