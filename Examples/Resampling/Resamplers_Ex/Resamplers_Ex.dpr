program Resamplers_Ex;

{$R 'Resamplers_Ex.res' 'Resamplers_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
