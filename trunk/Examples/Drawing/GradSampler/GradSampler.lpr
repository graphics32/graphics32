program GradSampler;

{$R 'Media.rc'}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmGradientSampler};

begin
  Application.Initialize;
  Application.CreateForm(TFrmGradientSampler, FrmGradientSampler);
  Application.Run;
end.

