program MeshGradients;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmTriangulationDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTriangulationDemo, FrmTriangulationDemo);
  Application.Run;
end.

