program ImgView_Layers;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  RGBALoaderUnit in 'RGBALoaderUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFrmNewImage, FrmNewImage);
  Application.CreateForm(TRGBALoaderForm, RGBALoaderForm);
  Application.Run;
end.
