program AntiAliasing;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmAntiAliasingTest, FrmAntiAliasingTest);
  Application.Run;
end.
