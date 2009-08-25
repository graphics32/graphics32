program TextureBlend_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms, ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
