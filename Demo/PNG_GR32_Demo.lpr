program PNG_GR32_Demo;

{$R *.res}

uses
  Interfaces,
  Forms,
  GR32_Lazarus,
  GR32_PortableNetworkGraphic,
  GR32_Png,
  MainUnit;

begin
  Application.Title:='GR32 PNG Demo';
  Application.Initialize;
  Application.CreateForm(TFmPngDemo, FmPngDemo);
  Application.Run;
end.
