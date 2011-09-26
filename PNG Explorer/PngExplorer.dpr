program PNGExplorer;

uses
  FastMM4,
  FastMove,
  Forms,
  PNGExplorerMain in 'PNGExplorerMain.pas' {FmTTF};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmTTF, FmTTF);
  Application.Run;
end.
