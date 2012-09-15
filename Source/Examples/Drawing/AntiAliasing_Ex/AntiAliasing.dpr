program AntiAliasing;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmAntiAliasingTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmAntiAliasingTest, FrmAntiAliasingTest);
  Application.Run;
end.
