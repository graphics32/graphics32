program NestedSampling_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {Form1},
  SimplePropEdit in 'SimplePropEdit.pas';

begin
  Application.Title:='Nested Sampling';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
