program TextureBlend_Ex;

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm}, GR32_L, ImagesForLazarus;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
