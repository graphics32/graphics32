program RotLayer;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormRotLayer, FormRotLayer);
  Application.Run;
end.
