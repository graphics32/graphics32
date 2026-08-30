program ImageDisplayAndInteractiveLayersWithTImage32;

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormInteractiveLayers, FormInteractiveLayers);
  Application.Run;
end.
