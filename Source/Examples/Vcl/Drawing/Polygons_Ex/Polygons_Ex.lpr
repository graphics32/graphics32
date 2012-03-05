program Polygons_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title:='GR32 Polygons Example';
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
