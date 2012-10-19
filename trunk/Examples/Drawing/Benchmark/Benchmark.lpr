program Benchmark;

{$R Media.rc}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {Form1};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
