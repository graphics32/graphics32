program Mandelbrot_Ex;

uses
  Forms,
  MandelUnit in 'MandelUnit.pas' {Form1},
  GR32_ExtImage in '..\..\GR32\GR32_ExtImage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
