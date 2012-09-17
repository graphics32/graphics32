program ByteMaps_Ex;

{$R 'ByteMaps_Ex.res' 'ByteMaps_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
