program Mandelbrot_Ex;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms,
  MandelUnit in 'MandelUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Title:='Mandelbrot Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
