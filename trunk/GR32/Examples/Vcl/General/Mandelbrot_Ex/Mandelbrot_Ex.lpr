program Mandelbrot_Ex;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms,
  MandelUnit in 'MandelUnit.pas' {Form1};

begin
  Application.Title:='Mandelbrot Example';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
