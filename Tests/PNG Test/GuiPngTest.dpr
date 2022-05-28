program GuiPngTest;

{$I GR32.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  GR32_TestGuiPng in 'GR32_TestGuiPng.pas',
  GR32_TestGuiPngDisplay in 'GR32_TestGuiPngDisplay.pas' {FmDisplay};

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

