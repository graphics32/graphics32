program PNG_GR32_Demo;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmPngDemo},
  GR32_Png in '..\GR32_Png.pas',
  GR32_PortableNetworkGraphic in '..\GR32_PortableNetworkGraphic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPngDemo, FmPngDemo);
  Application.Run;
end.
