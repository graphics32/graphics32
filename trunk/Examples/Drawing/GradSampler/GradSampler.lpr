program GradSampler;

{$R 'Media.rc'}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmGradientSampler};

begin
  Application.Title:='Gradient Sampler Example';
  Application.Initialize;
  Application.CreateForm(TFrmGradientSampler, FrmGradientSampler);
  Application.Run;
end.

