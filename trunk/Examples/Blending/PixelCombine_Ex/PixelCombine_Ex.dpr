program PixelCombine_Ex;

{$R 'PixelCombine_Ex.res' 'PixelCombine_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
