{$I DFS.INC}

unit ExtProgressBarReg;

interface

procedure Register;

implementation

uses
  {$IFDEF DFS_NO_DSGNINTF}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ExtProgressBar, DFSAbout, Classes;


procedure Register;
begin
  RegisterComponents('DFS', [TdfsExtProgressBar]);
  RegisterPropertyEditor(TypeInfo(string), TdfsExtProgressBar, 'Version',
     TDFSVersionProperty);
end;


end.
