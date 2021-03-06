program dfm2text;
{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes, CmnFunc2;

const
  Version = 'dfm2text v1.0 by Jordan Russell, 2001-01-05';

function StreamsIdentical (const S1, S2: TStream): Boolean;
var
  BufOld, BufNew: array[0..4095] of Byte;
  Total, Count: Integer;
begin
  Result := False;
  Total := S1.Size;
  if Total <> S2.Size then
    Exit;
  S1.Seek (0, soFromBeginning);
  S2.Seek (0, soFromBeginning);
  while Total > 0 do begin
    Count := Total;
    if Count > SizeOf(BufOld) then
      Count := SizeOf(BufOld);
    S1.ReadBuffer (BufOld, Count);
    S2.ReadBuffer (BufNew, Count);
    if not CompareMem(@BufOld, @BufNew, Count) then
      Exit;
    Dec (Total, Count);
  end;
  Result := True;
end;

procedure Convert (const Filename: String);
var
  OutFilename: String;
  InStream, OutStream: TFileStream;
  TmpStream: TMemoryStream;
  Existed: Boolean;
begin
  Write ('- ', Filename, ': ');
  OutFilename := ChangeFileExt(Filename, '.txt');
  //OutFilename := Filename + '.txt';
  OutStream := nil;
  TmpStream := nil;
  InStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    TmpStream := TMemoryStream.Create;
    ObjectResourceToText (InStream, TmpStream);
    Existed := NewFileExists(OutFilename);
    if not Existed then
      OutStream := TFileStream.Create(OutFilename, fmCreate)
    else begin
      OutStream := TFileStream.Create(OutFilename, fmOpenReadWrite or fmShareExclusive);
      if StreamsIdentical(TmpStream, OutStream) then begin
        Writeln ('not modified');
        Exit;
      end;
      OutStream.Seek (0, soFromBeginning);
    end;
    OutStream.Size := 0;
    TmpStream.SaveToStream (OutStream);
    // Synchronize the dates of the DFM and TXT
    FileSetDate(OutStream.Handle, FileGetDate(InStream.Handle));
  finally
    OutStream.Free;
    TmpStream.Free;
    InStream.Free;
  end;
  Writeln ('OK');
end;

var
  P, I: Integer;
  S: String;
  FilesList: TStringList;
  SR: TSearchRec;
  NumFiles: Integer = 0;
begin
  Writeln (Version);
  if (ParamCount = 0) or (ParamStr(1) = '/?') then begin
    Writeln ('usage:  dfm2text file.dfm [file2.dfm ...]');
    Halt (1);
  end;

  for P := 1 to ParamCount do begin
    S := ParamStr(P);
    FilesList := TStringList.Create;
    try
      FilesList.Sorted := True;
      if FindFirst(S, 0, SR) <> 0 then begin
        Writeln ('No files matching "', S, '" found.');
        Continue;
      end;
      repeat
        if CompareText(ExtractFileExt(SR.Name), '.bak') <> 0 then
          FilesList.Add (ExtractFilePath(S) + SR.Name);
      until FindNext(SR) <> 0;
      FindClose (SR);
      for I := 0 to FilesList.Count-1 do
        Convert (FilesList[I]);
      Inc (NumFiles);
    finally
      FilesList.Free;
    end;
  end;
  if NumFiles = 0 then
    Halt (2);
end.

