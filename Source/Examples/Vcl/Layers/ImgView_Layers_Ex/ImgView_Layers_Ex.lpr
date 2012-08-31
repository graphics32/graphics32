program ImgView_Layers_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {MainForm},
  NewImageUnit in 'NewImageUnit.pas' {FrmNewImage},
  RGBALoaderUnit in 'RGBALoaderUnit.pas' {RGBALoaderForm};

{$R *.res}

begin
  Application.Title:='Image View Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFrmNewImage, FrmNewImage);
  Application.CreateForm(TRGBALoaderForm, RGBALoaderForm);
  Application.Run;
end.
