program Mandelbrot_Ex;

{$R 'Mandelbrot_Ex.res' 'Mandelbrot_Ex.rc'}

uses
  Forms,
  MandelUnit in 'MandelUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
