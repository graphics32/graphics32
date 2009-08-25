program GradLines_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormGradientLines};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
