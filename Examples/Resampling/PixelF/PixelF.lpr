program PixelF;

{$R Media.rc}

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas';

{$R *.res}

begin
  Application.Title := 'PixelF Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
