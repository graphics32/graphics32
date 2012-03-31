program Resamplers_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title:='Resamplers Example';
  Application.Initialize;
  Application.CreateForm(TfmResamplersExample, fmResamplersExample);
  Application.Run;
end.
