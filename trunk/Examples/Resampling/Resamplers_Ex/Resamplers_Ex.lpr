program Resamplers_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {FrmResamplersExample};

{$R *.res}

begin
  Application.Title := 'Resamplers Example';
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
