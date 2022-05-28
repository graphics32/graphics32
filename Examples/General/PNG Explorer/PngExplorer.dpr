program PNGExplorer;

{$EXCESSPRECISION OFF}

uses
  FastMM4,
  {$IFNDEF CPUX64}
  FastMove,
  {$ENDIF}
  Forms,
  PNGExplorerMain in 'PngExplorerMain.pas' {FmPngExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPngExplorer, FmPngExplorer);
  Application.Run;
end.
