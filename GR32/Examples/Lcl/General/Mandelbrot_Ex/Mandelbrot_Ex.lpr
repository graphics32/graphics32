program Mandelbrot_Ex;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms,
  MandelUnit in 'MandelUnit.pas' {Form1}, GR32_L;

{$IFDEF Windows}
{.$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
