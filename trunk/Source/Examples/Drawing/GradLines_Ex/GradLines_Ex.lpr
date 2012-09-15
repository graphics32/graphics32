program GradLines_Ex;

{$I GR32.inc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title:='GR32 Gradient Lines Example';
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
