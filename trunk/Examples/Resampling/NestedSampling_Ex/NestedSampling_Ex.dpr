program NestedSampling_Ex;

uses
  Forms,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {MainForm},
  SimplePropEdit in 'SimplePropEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
