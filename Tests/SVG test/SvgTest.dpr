program SvgTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Forms,
  GR32.Tests.SVG.Xml in 'GR32.Tests.SVG.Xml.pas',
  GR32.Tests.SVG.Types in 'GR32.Tests.SVG.Types.pas',
  GR32.Tests.SVG.Path in 'GR32.Tests.SVG.Path.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.
