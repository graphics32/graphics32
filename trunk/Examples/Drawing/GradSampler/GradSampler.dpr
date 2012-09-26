program GradSampler;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmGradientSampler};

begin
  Application.Initialize;
  Application.CreateForm(TFrmGradientSampler, FrmGradientSampler);
  Application.Run;
end.
