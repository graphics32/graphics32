program Polygons_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormPolygons};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
