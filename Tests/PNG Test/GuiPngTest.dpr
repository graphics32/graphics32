program GuiPngTest;

{$I GR32.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
{$IFNDEF COMPILERXE2_UP}
  FastMM4,
  FastMove,
{$ENDIF}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  GR32_Png in '..\GR32_Png.pas',
  GR32_TestGuiPng in 'GR32_TestGuiPng.pas',
  GR32_TestGuiPngDisplay in 'GR32_TestGuiPngDisplay.pas' {FmDisplay},
  GR32_PortableNetworkGraphic in '..\GR32_PortableNetworkGraphic.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

