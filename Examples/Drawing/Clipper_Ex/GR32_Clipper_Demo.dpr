program GR32_Clipper_Demo;

{$R 'GR32_Clipper_Demo.res' 'GR32_Clipper_Demo.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmClipper, FrmClipper);
  Application.Run;
end.
