program ImgView_Layers_Ex;

{$R 'ImgView_Layers_Ex.res' 'ImgView_Layers_Ex.rc'}

uses
  Forms,
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
