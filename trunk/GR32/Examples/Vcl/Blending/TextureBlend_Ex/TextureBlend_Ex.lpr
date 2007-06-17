program TextureBlend_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm}, GR32_L, JPEGForLazarus;

{$IFDEF Windows}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
