program MeshGradients;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmMeshGradients};

begin
  Application.Initialize;
  Application.CreateForm(TFrmMeshGradients, FrmMeshGradients);
  Application.Run;
end.

