program Benchmark;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
