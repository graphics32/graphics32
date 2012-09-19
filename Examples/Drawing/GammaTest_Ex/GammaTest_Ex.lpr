program GammaTest_Ex;

{$R 'Media.rc'}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaTest, FrmGammaTest);
  Application.Run;
end.
