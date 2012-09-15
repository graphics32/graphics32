program RotLayer_Ex;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormRotLayer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRotLayer, FormRotLayer);
  Application.Run;
end.
