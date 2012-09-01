program GradFills_Ex;

{$I GR32.inc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
