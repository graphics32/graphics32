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
  GR32.Tests.SVG.Path in 'GR32.Tests.SVG.Path.pas',
  GR32.Tests.SVG.Tree in 'GR32.Tests.SVG.Tree.pas',
  GR32.Tests.SVG.Css in 'GR32.Tests.SVG.Css.pas',
  GR32.Tests.SVG.Gradients in 'GR32.Tests.SVG.Gradients.pas',
  GR32.Tests.SVG.Renderer in 'GR32.Tests.SVG.Renderer.pas',
  GR32.Tests.SVG.Facade in 'GR32.Tests.SVG.Facade.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.
