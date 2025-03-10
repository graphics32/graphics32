program GammaCorrection;

{$R 'Media.rc'}

uses
  Forms,
  Interfaces,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaCorrection, FrmGammaCorrection);
  Application.Run;
end.
