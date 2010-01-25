program Image32_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_L,
  MainUnit in 'MainUnit.pas' {Form1};

{$IFDEF Windows}
{.$R *.RES}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
