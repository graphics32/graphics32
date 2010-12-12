program TextureBlend_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms, ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

begin
    Application.Title:='GR32 Texture Blend Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
