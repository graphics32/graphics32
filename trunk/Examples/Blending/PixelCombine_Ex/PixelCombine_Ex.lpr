program PixelCombine_Ex;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Title := 'GR32 Pixel Combine Example';
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
