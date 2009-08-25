program PixelCombine_Ex;

{$MODE Delphi}

uses
  Interfaces,
  Forms, ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1}, GR32_L;

{$IFDEF Windows}
{$R *.RES}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
