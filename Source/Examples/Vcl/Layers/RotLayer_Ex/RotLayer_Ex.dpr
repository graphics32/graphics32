program RotLayer_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormRotLayer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRotLayer, FormRotLayer);
  Application.Run;
end.
