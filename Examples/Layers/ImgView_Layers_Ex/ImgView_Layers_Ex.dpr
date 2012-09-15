program ImgView_Layers_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  NewImageUnit in 'NewImageUnit.pas' {FrmNewImage},
  RGBALoaderUnit in 'RGBALoaderUnit.pas' {RGBALoaderForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFrmNewImage, FrmNewImage);
  Application.CreateForm(TRGBALoaderForm, RGBALoaderForm);
  Application.Run;
end.
