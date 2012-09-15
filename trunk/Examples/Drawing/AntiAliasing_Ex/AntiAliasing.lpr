program AntiAliasing;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmAntiAliasingTest};

{$R *.res}

begin
  Application.Title:='Anti-Aliasing Test';
  Application.Initialize;
  Application.CreateForm(TFrmAntiAliasingTest, FrmAntiAliasingTest);
  Application.Run;
end.
