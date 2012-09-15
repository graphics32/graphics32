program GR32_Clipper_Demo;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmClipper},
  GR32_Clipper in '..\..\..\Source\GR32_Clipper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmClipper, FrmClipper);
  Application.Run;
end.
