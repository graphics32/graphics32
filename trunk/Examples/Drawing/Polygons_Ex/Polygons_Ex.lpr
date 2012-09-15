program Polygons_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title := 'GR32 Polygons Example';
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
