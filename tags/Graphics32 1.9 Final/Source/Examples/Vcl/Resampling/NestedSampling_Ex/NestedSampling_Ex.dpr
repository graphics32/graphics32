program NestedSampling_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  SimplePropEdit in 'SimplePropEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
