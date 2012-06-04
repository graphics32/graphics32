program GR32_Clipper_Demo;

uses
  Forms,
  main in 'main.pas' {Form1},
  clipper in 'clipper.pas',
  GR32_Clipper in 'GR32_Clipper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
