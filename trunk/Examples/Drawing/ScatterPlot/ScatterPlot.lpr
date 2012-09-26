program ScatterPlot;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmScatterPlot, FmScatterPlot);
  Application.Run;
end.

