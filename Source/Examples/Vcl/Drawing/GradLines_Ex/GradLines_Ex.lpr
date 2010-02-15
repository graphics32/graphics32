program GradLines_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L;

begin
  Application.Title:='Gradient Lines';
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
