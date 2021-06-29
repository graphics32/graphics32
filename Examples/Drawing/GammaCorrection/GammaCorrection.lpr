program GammaCorrection;

{$R 'Media.rc'}

uses
  Interfaces,
  Forms, imagesforlazarus,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaCorrection, FrmGammaCorrection);
  Application.Run;
end.
