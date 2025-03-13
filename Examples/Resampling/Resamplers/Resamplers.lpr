program Resamplers;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
