program GR32_FiltersTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Forms,
  GR32_Filters in '..\..\Source\GR32_Filters.pas',
  TestCheckParams in 'TestCheckParams.pas',
  TestCopyComponents in 'TestCopyComponents.pas',
  TestAlphaToGrayscale in 'TestAlphaToGrayscale.pas',
  TestColorToGrayscale in 'TestColorToGrayscale.pas',
  TestIntensityToAlpha in 'TestIntensityToAlpha.pas',
  TestInvert in 'TestInvert.pas',
  TestInvertRGB in 'TestInvertRGB.pas',
  TestApplyLUT in 'TestApplyLUT.pas',
  TestChromaKey in 'TestChromaKey.pas',
  TestCreateBitmask in 'TestCreateBitmask.pas',
  TestApplyBitmask in 'TestApplyBitmask.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.
