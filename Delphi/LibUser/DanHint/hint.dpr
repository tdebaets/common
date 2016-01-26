program hint;

uses
  Forms,
  danunit in 'danunit.pas' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
