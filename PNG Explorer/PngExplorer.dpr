program PNGExplorer;

uses
  FastMM4,
  FastMove,
  Forms,
  PNGExplorerMain in 'PngExplorerMain.pas' {FmPngExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPngExplorer, FmPngExplorer);
  Application.Run;
end.
