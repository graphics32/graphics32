program Visualization_Ex;

{$R 'Visualization_Ex.res' 'Visualization_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
