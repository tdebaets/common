
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://tnt.ccci.org/delphi_unicode_controls/                           }
{        Version: 2.1.11                                                      }
{                                                                             }
{    Copyright (c) 2002-2004, Troy Wolbrink (troy.wolbrink@ccci.org)          }
{                                                                             }
{*****************************************************************************}

unit TntUnicodeVcl_Register;

{$INCLUDE ..\TntCompilers.inc}

interface

procedure Register;

implementation

uses
  Classes, {DB,} TntForms, TntMenus, TntStdCtrls, {TntGrids,} TntExtCtrls, {TntComCtrls,}
  TntButtons, {TntDB, TntDBCtrls, TntDBGrids,} TntActnList, TntDialogs, TntExtDlgs;

const
  TNT_STANDARD      = 'Tnt Standard';
  TNT_ADDITIONAL    = 'Tnt Additional';
  TNT_WIN32         = 'Tnt Win32';
  TNT_DATA_CONTROLS = 'Tnt Data Controls';
  TNT_DIALOGS       = 'Tnt Dialogs';

procedure Register;
begin
  // ------- Standard -------
  RegisterComponents(TNT_STANDARD, [TTntMainMenu]);
  {$IFDEF COMPILER_5_UP}
  RegisterComponents(TNT_STANDARD, [TTntPopupMenu]);
  {$ENDIF COMPILER_5_UP}
  RegisterComponents(TNT_STANDARD, [TTntLabel]);
  RegisterComponents(TNT_STANDARD, [TTntEdit]);
  RegisterComponents(TNT_STANDARD, [TTntMemo]);
  RegisterComponents(TNT_STANDARD, [TTntButton]);
  RegisterComponents(TNT_STANDARD, [TTntCheckBox]);
  RegisterComponents(TNT_STANDARD, [TTntRadioButton]);
  RegisterComponents(TNT_STANDARD, [TTntListBox]);
  RegisterComponents(TNT_STANDARD, [TTntComboBox]);
  RegisterComponents(TNT_STANDARD, [TTntScrollBar]);
  RegisterComponents(TNT_STANDARD, [TTntGroupBox]);
  RegisterComponents(TNT_STANDARD, [TTntRadioGroup]);
  RegisterComponents(TNT_STANDARD, [TTntPanel]);
  RegisterComponents(TNT_STANDARD, [TTntActionList]);

  // ------- Additional -------
  RegisterComponents(TNT_ADDITIONAL, [TTntBitBtn]);
  RegisterComponents(TNT_ADDITIONAL, [TTntSpeedButton]);
  { -- TTntMaskEdit goes here -- }
  {$IFDEF TNT_FULL_BUILD}
  RegisterComponents(TNT_ADDITIONAL, [TTntStringGrid]);
  RegisterComponents(TNT_ADDITIONAL, [TTntDrawGrid]);
  {$ENDIF TNT_FULL_BUILD}
  RegisterComponents(TNT_ADDITIONAL, [TTntImage]);
  RegisterComponents(TNT_ADDITIONAL, [TTntShape]);
  RegisterComponents(TNT_ADDITIONAL, [TTntBevel]);
  RegisterComponents(TNT_ADDITIONAL, [TTntScrollBox]);
  {$IFDEF TNT_FULL_BUILD}
  RegisterComponents(TNT_ADDITIONAL, [TTntCheckListBox]);
  {$ENDIF TNT_FULL_BUILD}
  RegisterComponents(TNT_ADDITIONAL, [TTntSplitter]);
  RegisterComponents(TNT_ADDITIONAL, [TTntStaticText]);
  RegisterComponents(TNT_ADDITIONAL, [TTntControlBar]);

  // ------- Win32 -------
  {$IFDEF TNT_FULL_BUILD}
  RegisterComponents(TNT_WIN32, [TTntTabControl]);
  RegisterComponents(TNT_WIN32, [TTntPageControl]);
  RegisterComponents(TNT_WIN32, [TTntRichEdit]);
  RegisterComponents(TNT_WIN32, [TTntTrackBar]);
  RegisterComponents(TNT_WIN32, [TTntProgressBar]);
  RegisterComponents(TNT_WIN32, [TTntUpDown]);
  { -- TTntHotKey goes here -- }
  { -- TTntAnimate goes here -- }
  RegisterComponents(TNT_WIN32, [TTntDateTimePicker]);
  RegisterComponents(TNT_WIN32, [TTntMonthCalendar]);
  RegisterComponents(TNT_WIN32, [TTntTreeView]);
  RegisterComponents(TNT_WIN32, [TTntListView]);
  { -- TTntHeader goes here -- }
  RegisterComponents(TNT_WIN32, [TTntStatusBar]);
  { -- TTntToolBar goes here -- }
  { -- TTntCoolBar goes here -- }
  RegisterComponents(TNT_WIN32, [TTntPageScroller]);
  { -- TTntComboBoxEx goes here -- }
  {$ENDIF TNT_FULL_BUILD}

  // ------- System -------
  RegisterComponents(TNT_ADDITIONAL, [TTntPaintBox]);
  { -- TTntMediaPlayer goes here -- }
  { -- TTntOleContainer goes here -- }

  // ------- Data Controls -------
  {$IFDEF TNT_FULL_BUILD}
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBGrid]);
  { -- TTntDBNavigator goes here -- }
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBText]);
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBEdit]);
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBMemo]);
  { -- TTntDBImage goes here -- }
  { -- TTntDBListBox goes here -- }
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBComboBox]);
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBCheckBox]);
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBRadioGroup]);
  { -- TTntDBLookupListBox goes here -- }
  { -- TTntDBLookupComboBox goes here -- }
  RegisterComponents(TNT_DATA_CONTROLS, [TTntDBRichEdit]);
  { -- TTntDBCtrlGrid here -- }
  { -- TTntDBLookupListBox goes here -- }
  { -- TTntDBChart goes here -- }
  {$ENDIF TNT_FULL_BUILD}

  // ------- Dialogs -------
  RegisterComponents(TNT_DIALOGS, [TTntOpenDialog]);
  RegisterComponents(TNT_DIALOGS, [TTntSaveDialog]);
  RegisterComponents(TNT_DIALOGS, [TTntOpenPictureDialog]);
  RegisterComponents(TNT_DIALOGS, [TTntSavePictureDialog]);

  // --------- Fields --------------
  {$IFDEF TNT_FULL_BUILD}
  RegisterTntFields;
  {$ENDIF TNT_FULL_BUILD}

  // --------- Classes --------------
  RegisterClass(TTntMenuItem);
  {$IFDEF TNT_FULL_BUILD}
  RegisterClass(TTntTabSheet);
  {$ENDIF TNT_FULL_BUILD}
end;

end.
