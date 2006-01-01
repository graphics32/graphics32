{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program ImgView_Layers_Ex;

uses
  QForms,
  MainUnit in 'MainUnit.pas' {MainForm},
  NewImageUnit in 'NewImageUnit.pas' {NewImageForm},
  RGBALoaderUnit in 'RGBALoaderUnit.pas' {RGBALoaderForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNewImageForm, NewImageForm);
  Application.CreateForm(TRGBALoaderForm, RGBALoaderForm);
  Application.Run;
end.
