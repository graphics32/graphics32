program PNG_GR32_Demo;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmPngDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPngDemo, FmPngDemo);
  Application.Run;
end.
