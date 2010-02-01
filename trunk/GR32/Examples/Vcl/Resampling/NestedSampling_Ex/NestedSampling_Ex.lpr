program NestedSampling_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1},
  SimplePropEdit in 'SimplePropEdit.pas';

begin
  Application.Title:='Nested Sampling';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
