program Resamplers_Ex;

uses
  Forms,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {fmResamplersExample};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmResamplersExample, fmResamplersExample);
  Application.Run;
end.
