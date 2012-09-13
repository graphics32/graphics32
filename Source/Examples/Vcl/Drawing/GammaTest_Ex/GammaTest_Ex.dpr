program GammaTest_Ex;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FrmGammaTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmGammaTest, FrmGammaTest);
  Application.Run;
end.

