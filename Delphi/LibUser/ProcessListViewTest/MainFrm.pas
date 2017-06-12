(****************************************************************************
 *
 * Copyright 2017 Tim De Baets
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
 * TProcessListView Test main form
 *
 ****************************************************************************)

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, EnhListView, ExtListView, ExtChkListView, ProcessListView,
  StdCtrls, TimerThread, XPTheme, CtrlsCommon, ImgList, ExtCtrls;

type
  TMainForm = class(TForm)
    ProcListView: TProcessListView;
    ImageList: TImageList;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    btnStartTimer: TButton;
    btnResetTimer: TButton;
    btnStopTimer: TButton;
    GroupBox2: TGroupBox;
    chkHideSelection: TCheckBox;
    chkColumnClick: TCheckBox;
    chkShowIcons: TCheckBox;
    chkSortArrows: TCheckBox;
    GroupBox3: TGroupBox;
    chkShowCheckAll: TCheckBox;
    chkShowSystemProcess: TCheckBox;
    chkShowSystem: TCheckBox;
    chkShowOtherUser: TCheckBox;
    btnRefresh: TButton;
    Label1: TLabel;
    lblProcCount: TLabel;
    txtLog: TMemo;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnStartTimerClick(Sender: TObject);
    procedure btnResetTimerClick(Sender: TObject);
    procedure btnStopTimerClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure ProcListViewDblClick(Sender: TObject);
    procedure chkHideSelectionClick(Sender: TObject);
    procedure chkColumnClickClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkShowIconsClick(Sender: TObject);
    procedure ProcListViewMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chkShowCheckAllClick(Sender: TObject);
    procedure chkShowSystemProcessClick(Sender: TObject);
    procedure chkShowSystemClick(Sender: TObject);
    procedure chkShowOtherUserClick(Sender: TObject);
    procedure ProcListViewProcessAdded(Sender: TObject;
      Process: TProcess);
    procedure ProcListViewBeforeProcessRemove(Sender: TObject;
      Process: TProcess);
    procedure chkSortArrowsClick(Sender: TObject);
  private
    FTimerThread: TTimerThread;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  ProcListView.Refresh(True);
end;

procedure TMainForm.btnStartTimerClick(Sender: TObject);
begin
  FTimerThread := TTimerThread.Create(OnTimer, 1000);
end;

procedure TMainForm.btnResetTimerClick(Sender: TObject);
begin
  FTimerThread.Reset;
end;

procedure TMainForm.btnStopTimerClick(Sender: TObject);
begin
  FTimerThread.Stop;
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  OutputDebugString('timer');
end;

procedure TMainForm.ProcListViewDblClick(Sender: TObject);
begin
  if Assigned(ProcListView.Selected) then begin
    TListItem(ProcListView.Selected).Checked :=
        not TListItem(ProcListView.Selected).Checked
  end;
end;

procedure TMainForm.chkHideSelectionClick(Sender: TObject);
begin
  ProcListView.HideSelection := chkHideSelection.Checked;
end;

procedure TMainForm.chkColumnClickClick(Sender: TObject);
begin
  ProcListView.ColumnClick := chkColumnClick.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ConvertTo32BitImageList(ImageList);
  with ProcListView do begin
    chkHideSelection.Checked := HideSelection;
    chkColumnClick.Checked := ColumnClick;
    chkShowIcons.Checked := Assigned(SmallImages);
    chkShowCheckAll.Checked := ShowCheckAll;
    chkShowSystem.Checked := ShowSystem;
    chkShowSystemProcess.Checked := ShowSystemProcess;
    chkShowOtherUser.Checked := ShowOtherUser;
    chkSortArrows.Checked := ShowSortArrows;
  end;
end;

procedure TMainForm.chkShowIconsClick(Sender: TObject);
begin
  if chkShowIcons.Checked then
    ProcListView.SmallImages := ImageList
  else
    ProcListView.SmallImages := nil
end;

procedure TMainForm.ProcListViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and Assigned(ProcListView.Selected) then begin
    TProcessListItem(ProcListView.Selected).Enabled :=
      not TProcessListItem(ProcListView.Selected).Enabled;
  end;
end;

procedure TMainForm.chkShowCheckAllClick(Sender: TObject);
begin
  ProcListView.ShowCheckAll := chkShowCheckAll.Checked;
end;

procedure TMainForm.chkShowSystemProcessClick(Sender: TObject);
begin
  ProcListView.ShowSystemProcess := chkShowSystemProcess.Checked;
end;

procedure TMainForm.chkShowSystemClick(Sender: TObject);
begin
  ProcListView.ShowSystem := chkShowSystem.Checked;
end;

procedure TMainForm.chkShowOtherUserClick(Sender: TObject);
begin
  ProcListView.ShowOtherUser := chkShowOtherUser.Checked;
end;

procedure TMainForm.chkSortArrowsClick(Sender: TObject);
begin
  ProcListView.ShowSortArrows := chkSortArrows.Checked;
end;

procedure TMainForm.ProcListViewProcessAdded(Sender: TObject;
  Process: TProcess);
begin
  lblProcCount.Caption := IntToStr(ProcListView.TotalProcessCount);
  txtLog.Lines.Add(Format( 'Started: %s (%d)', [Process.Name, Process.ProcessID]));
end;

procedure TMainForm.ProcListViewBeforeProcessRemove(Sender: TObject;
  Process: TProcess);
begin
  // - 1 because exiting process is still counted
  lblProcCount.Caption := IntToStr(ProcListView.TotalProcessCount - 1);
  txtLog.Lines.Add(Format( 'Exited: %s (%d)', [Process.Name, Process.ProcessID]));
end;

end.
