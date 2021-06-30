program NestedSampling;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas',
  SimplePropEdit in 'SimplePropEdit.pas';

{$R *.res}

begin
  Application.Title := 'Nested Sampling';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
