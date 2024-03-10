program BlendTest;

{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthlt das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners fgen Sie den konditinalen Definitionen
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardmig
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Forms,
  TestGR32Blend in 'TestGR32Blend.pas',
  GR32_Blend in '..\..\Source\GR32_Blend.pas',
  GR32_BlendReference in 'GR32_BlendReference.pas';

{ *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.
