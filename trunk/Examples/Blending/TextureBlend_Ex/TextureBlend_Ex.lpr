program TextureBlend_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms, ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Title := 'GR32 Texture Blend Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
