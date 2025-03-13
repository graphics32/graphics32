program SvgPath;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmSvgPathRenderer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmSvgPathRenderer, FrmSvgPathRenderer);
  Application.Run;
end.
