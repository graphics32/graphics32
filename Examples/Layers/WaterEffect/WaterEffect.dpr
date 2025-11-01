program WaterEffect;

{$R 'Media.res' 'Media.rc'}

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FormMain},
  GR32.Layers.WaterEffect in 'GR32.Layers.WaterEffect.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
