program Mandelbrot_Ex;

uses
  Forms,
  MandelUnit in 'MandelUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
