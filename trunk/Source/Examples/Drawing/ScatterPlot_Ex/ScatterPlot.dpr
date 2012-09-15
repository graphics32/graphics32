program ScatterPlot;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmScatterPlot};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmScatterPlot, FmScatterPlot);
  Application.Run;
end.

