program Mandelbrot;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MandelUnit in 'MandelUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
