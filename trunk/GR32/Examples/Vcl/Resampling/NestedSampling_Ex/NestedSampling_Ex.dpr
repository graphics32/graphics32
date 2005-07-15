program NestedSampling_Ex;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  SimplePropEdit in 'SimplePropEdit.pas',
  GR32_IntegerMaps in '..\..\GR32\GR32_IntegerMaps.pas',
  GR32_Math in '..\..\GR32\GR32_Math.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
