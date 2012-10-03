program GammaCorrection;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaCorrection, FrmGammaCorrection);
  Application.Run;
end.

