program GradLines;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Title:='GR32 Gradient Lines Example';
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
