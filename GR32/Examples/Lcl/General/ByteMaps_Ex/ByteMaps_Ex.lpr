program ByteMaps_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L, ImagesForLazarus;

{$IFDEF Windows}
{.$R *.RES}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
