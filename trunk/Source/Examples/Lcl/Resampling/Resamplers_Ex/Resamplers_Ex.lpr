program Resamplers_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L, ImagesForLazarus;

{$IFDEF Windows}
{.$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TfmResamplersExample, fmResamplersExample);
  Application.Run;
end.
