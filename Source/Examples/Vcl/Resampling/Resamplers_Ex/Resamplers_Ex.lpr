program Resamplers_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

begin
  Application.Title:='Resamplers Example';
  Application.Initialize;
  Application.CreateForm(TfmResamplersExample, fmResamplersExample);
  Application.Run;
end.
