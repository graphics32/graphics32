program PixelCombine_Ex;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  GR32_MediaPathLocator in '..\..\GR32_MediaPathLocator.pas',
  MainUnit in 'MainUnit.pas' {FormPixelCombine};

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
