program PixelCombine_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormPixelCombine};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
