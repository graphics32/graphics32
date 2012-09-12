program AntiAliasing;

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FrmAntiAliasingTest};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmAntiAliasingTest, FrmAntiAliasingTest);
  Application.Run;
end.
