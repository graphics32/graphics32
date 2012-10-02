program MeshGradients;

{$R 'Media.rc'}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FrmMeshGradients};

begin
  Application.Initialize;
  Application.CreateForm(TFrmMeshGradients, FrmMeshGradients);
  Application.Run;
end.

