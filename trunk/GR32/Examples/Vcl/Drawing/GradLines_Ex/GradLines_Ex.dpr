program GradLines_Ex;

uses
  ExceptionLog,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
