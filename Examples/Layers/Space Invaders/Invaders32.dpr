program Invaders32;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

begin
  Application.Initialize;
  Application.Title := 'Space Invaders - Invaders32';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
