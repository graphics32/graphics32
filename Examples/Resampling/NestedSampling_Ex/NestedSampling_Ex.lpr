program NestedSampling_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1},
  SimplePropEdit in 'SimplePropEdit.pas';

{$R *.res}

begin
  Application.Title := 'Nested Sampling';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
