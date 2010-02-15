program Visualization_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Title:='Visualization Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
