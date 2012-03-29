program GradLines_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L;

{$R *.res}

begin
  Application.Title:='GR32 Gradient Lines Example';
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
