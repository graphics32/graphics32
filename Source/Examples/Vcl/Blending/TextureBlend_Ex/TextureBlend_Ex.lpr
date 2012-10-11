program TextureBlend_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms, ImagesForLazarus,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
    Application.Title:='GR32 Texture Blend Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
