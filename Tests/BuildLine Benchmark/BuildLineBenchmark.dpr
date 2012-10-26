program BuildLineBenchmark;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FrmBuildLineBenchmark};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmBuildLineBenchmark, FrmBuildLineBenchmark);
  Application.Run;
end.

