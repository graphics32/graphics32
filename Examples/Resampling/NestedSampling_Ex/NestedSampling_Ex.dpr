program NestedSampling_Ex;

{$R 'NestedSampling_Ex.res' 'NestedSampling_Ex.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas',
  SimplePropEdit in 'SimplePropEdit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
