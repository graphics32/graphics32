program GuiPngTest;

{$I GR32.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Interfaces,
  Forms,
  GUITestRunner, fpcunittestrunner, imagesforlazarus, fpcunitconsolerunner,
  GR32_Png in '..\GR32_Png.pas',
  GR32_TestGuiPng in 'GR32_TestGuiPng.pas',
  GR32_TestGuiPngDisplay in 'GR32_TestGuiPngDisplay.pas' {FmDisplay},
  GR32_PortableNetworkGraphic in '..\GR32_PortableNetworkGraphic.pas';

{$R *.res}

begin
  Application.Title := 'PNG Test';
  Application.Initialize;
  InitializeGR32PngTests;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

