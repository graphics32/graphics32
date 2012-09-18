program PixelCombine_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
