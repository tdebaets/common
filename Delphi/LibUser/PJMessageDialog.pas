{ ##
  @PROJECT_NAME             Message Dialog Component
  @PROJECT_DESC             A component that provides a customisable Windows
                            message dialog box. It wraps the Windows
                            MessageBoxIndirect API call.
  @FILE                     PJMessageDialog.pas
  @COMMENTS                 Component source code.
  @LEGAL_NOTICE             The source code and any help files can be freely
                            distributed on a not-for-profit basis providing
                            that:
                            + the source code is not altered and
                            + these comments are not removed from the source
                              file.\
                            By not-for-profit I mean that you may recover out of
                            pocket expenses incurred in distributing the code,
                            but should not make a profit from this. If you
                            discover any bugs in this implementation, or if you
                            have any update suggestions, please contact me at
                            peter.johnson@openlink.org Please do modify the code
                            for you own use. I'd like to see any changes you
                            make - I could incorporate them into future
                            versions. Please notify me of changes on at the
                            above e-mail address. This software is provided as
                            is - no warranty is given as to its suitability for
                            any purposes to which you may wish to put it.
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.pjsoft.contactbox.co.uk/
  @AUTHOR                   Peter D Johnson, Llanarth, Ceredigion, Wales, UK.
  @COPYRIGHT                Copyright © 2001, P.D.Johnson, Llanarth, Ceredigion,
                            Wales UK.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 26/03/2001
      @COMMENTS             Original version
    )
  )
}


unit PJMessageDialog;

interface

uses
  // Delphi
  Windows, Classes, Controls, Forms;

type
  {The kinds of icons that can be displayed in TPJMessageDialog dialog boxes}
  TPJMsgDlgIconKind = (miWarning, miInfo, miQuestion, miError, miUser, miNone);

  {The various groups of buttons that can be displayed in TPJMessageDialog
  dialog boxes}
  TPJMsgDlgButtonGroup = (bgAbortRetryIgnore, bgOK, bgOKCancel, bgRetryCancel,
    bgYesNo, bgYesNoCancel);

  TPJMsgDlgDefButton = (dbDefButton1, dbDefButton2, dbDefButton3, dbDefButton4);

  {Component that wraps the windows MessageBoxIndirect API call and displays
  dialog boxes customised per properties}
  TPJMessageDialog = class(TComponent)
  private
    fText: string;
    fTitle: TCaption;
    fHelpContext: Integer;
    fIconResource: string;
    fButtonGroup: TPJMsgDlgButtonGroup;
    fIconKind: TPJMsgDlgIconKind;
    fDefButton: TPJMsgDlgDefButton;
    fMakeSound: Boolean;
  protected
    function GetDefaultTitle: string;
      {Returns default title for window based on kind of icon}
    function GetHWND: THandle;
      {Returns the window handle of the form (if any) that owns this component}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Integer;
  published
    property Text: string
      read fText write fText;
    property Title: TCaption
      read fTitle write fTitle;
    property HelpContext: Integer
      read fHelpContext write fHelpContext;
    property IconKind: TPJMsgDlgIconKind
      read fIconKind write fIconKind default miInfo;
    property IconResource: string
      read fIconResource write fIconResource;
    property ButtonGroup: TPJMsgDlgButtonGroup
      read fButtonGroup write fButtonGroup default bgOK;
    property DefaultButton: TPJMsgDlgDefButton
      read fDefButton write fDefButton default dbDefButton1;
    property MakeSound: Boolean
      read fMakeSound write fMakeSound default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PJ Stuff', [TPJMessageDialog]);
end;

{ TPJMessageDialog }



procedure HelpCallback(var HelpInfo: THelpInfo); stdcall;
  {Callback procedure for Execute method procedure. Starts win help with help
  context passed in HelpInfo param}
begin
  Application.HelpContext(HelpInfo.dwContextId);
end;

constructor TPJMessageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fButtonGroup := bgOK;
  fIconKind := miInfo;
  fDefButton := dbDefButton1;
  fMakeSound := False;
end;

destructor TPJMessageDialog.Destroy;
begin
  inherited Destroy;
end;

function TPJMessageDialog.Execute: Integer;
const
  cButtonFlags: array[TPJMsgDlgButtonGroup] of Integer = (
    MB_ABORTRETRYIGNORE, MB_OK, MB_OKCANCEL, MB_RETRYCANCEL,
    MB_YESNO, MB_YESNOCANCEL);
  cIconFlags: array[TPJMsgDlgIconKind] of Integer = (
    MB_ICONWARNING, MB_ICONINFORMATION, MB_ICONQUESTION, MB_ICONERROR,
    MB_USERICON, 0);
  cSounds: array[TPJMsgDlgIconKind] of Integer = (
    MB_ICONEXCLAMATION, MB_ICONASTERISK, MB_ICONQUESTION, MB_ICONHAND,
    MB_OK, MB_OK);
  cDefButtonFlags: array[TPJMsgDlgDefButton] of Integer = (
    MB_DEFBUTTON1, MB_DEFBUTTON2, MB_DEFBUTTON3, MB_DEFBUTTON4);
var
  MsgBoxParams: TMsgBoxParams;  // params passed to MessageBoxIndirect fn
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  // Set up TMsgBoxParams structure
  FillChar(MsgBoxParams, SizeOf(MsgBoxParams), 0);
  with MsgBoxParams do
  begin
    cbSize := SizeOf(TMsgBoxParams);
    hwndOwner := GetHWND;
    {$IFDEF VER90}  // Delphi 2
    hInstance := System.HInstance;
    {$ELSE}         // Delphi 3...
    hInstance := SysInit.HInstance;
    {$ENDIF}
    lpszText := PChar(fText);
    if fTitle <> '' then
      lpszCaption := PChar(fTitle)
    else
      lpszCaption := PChar(GetDefaultTitle);
    dwStyle := cButtonFlags[fButtonGroup] or cIconFlags[fIconKind]
      or cDefButtonFlags[fDefButton];
    if HelpContext <> 0 then
    begin
      // we're displaying help button with a callback procedure
      dwStyle := dwStyle + MB_HELP;
      dwContextHelpId := fHelpContext;
      lpfnMsgBoxCallback := @HelpCallback;
    end;
    if fIconKind = miUser then
      lpszIcon := PChar(fIconResource);
  end;
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    // Make sound if required
    if fMakeSound then
      MessageBeep(cSounds[fIconKind]);
    // Display dlg
    Result := Integer(MessageBoxIndirect(MsgBoxParams));
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function TPJMessageDialog.GetDefaultTitle: string;
  {Returns default title for window based on kind of icon}
const
  cDefTitles: array[TPJMsgDlgIconKind] of string = (      // default titles
    'Warning', 'Information', 'Confirm', 'Error', '', '');
begin
  Result := cDefTitles[fIconKind];
  if Result = '' then
    Result := Application.Title;    // use application title when miUser
end;

function TPJMessageDialog.GetHWND: THandle;
  {Returns the window handle of the form (if any) that owns this component}
begin
  if (Owner <> nil) and (Owner is TWinControl) then
    Result := (Owner as TWinControl).Handle
  else
    Result := Application.Handle;
end;

end.
