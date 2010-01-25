program Polygons_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_L,
  MainUnit in 'MainUnit.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
