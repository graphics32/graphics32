program PNGExplorer;

{$EXCESSPRECISION OFF}

uses
  Forms,
{$ifdef FPC}
  Interfaces,
{$endif}
  PngExplorerMain in 'PngExplorerMain.pas' {FmPngExplorer};

{$R *.res}

begin
{$ifdef FPC}
  Application.Scaled:=True;
{$endif}
  Application.Initialize;
  Application.CreateForm(TFmPngExplorer, FmPngExplorer);
  Application.Run;
end.
