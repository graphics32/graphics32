program ByteMaps_Ex;

uses
  Interfaces,
  Forms,
  ImagesForLazarus,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
