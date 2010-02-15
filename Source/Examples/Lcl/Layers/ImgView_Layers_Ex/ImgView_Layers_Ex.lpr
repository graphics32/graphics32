program ImgView_Layers_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  NewImageUnit in 'NewImageUnit.pas' {NewImageForm},
  RGBALoaderUnit in 'RGBALoaderUnit.pas' {RGBALoaderForm}, GR32_L,
  ImagesForLazarus;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNewImageForm, NewImageForm);
  Application.CreateForm(TRGBALoaderForm, RGBALoaderForm);
  Application.Run;
end.
