program Visualization_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
