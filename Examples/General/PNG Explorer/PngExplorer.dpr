program PNGExplorer;

{$EXCESSPRECISION OFF}

uses
  Forms,
  PNGExplorerMain in 'PngExplorerMain.pas' {FmPngExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPngExplorer, FmPngExplorer);
  Application.Run;
end.
