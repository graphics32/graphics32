program GammaTest_Ex;

{$R 'GammaTest_Ex.res' 'GammaTest_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaTest, FrmGammaTest);
  Application.Run;
end.

