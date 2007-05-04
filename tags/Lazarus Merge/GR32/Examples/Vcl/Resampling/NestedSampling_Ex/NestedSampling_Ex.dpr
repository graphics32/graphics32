program NestedSampling_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  SimplePropEdit in 'SimplePropEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
