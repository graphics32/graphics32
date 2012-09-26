program GradSamplerEx;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmGradientSampler};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmGradientSampler, FrmGradientSampler);
  Application.Run;
end.

