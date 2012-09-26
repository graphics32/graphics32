program RotLayer;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormRotLayer, FormRotLayer);
  Application.Run;
end.
