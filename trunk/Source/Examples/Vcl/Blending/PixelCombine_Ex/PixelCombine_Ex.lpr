program PixelCombine_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  GR32_L,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title:='GR32 Pixel Combine Example';
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
