program GradFills;

{$R 'Media.res' 'Media.rc'}
{$R ..\..\manifest.res}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
