program Resamplers_Ex;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmResamplersExample};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
