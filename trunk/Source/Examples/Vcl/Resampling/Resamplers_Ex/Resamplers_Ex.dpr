program Resamplers_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {fmResamplersExample};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmResamplersExample, fmResamplersExample);
  Application.Run;
end.
