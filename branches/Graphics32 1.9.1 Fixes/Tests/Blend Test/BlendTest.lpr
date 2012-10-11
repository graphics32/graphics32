program BlendTest;

{$I GR32.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Interfaces,
  Forms,
  fpcunittestrunner,
  {$IFDEF CONSOLE_TESTRUNNER}
  fpcunitconsolerunner,
  {$ELSE}
  GUITestRunner,
  {$ENDIF}
  TestGR32Blend in 'TestGR32Blend.pas',
  GR32_Blend in '..\..\Source\GR32_Blend.pas',
  GR32_BlendReference in 'GR32_BlendReference.pas';

{ *.RES}

begin
  Application.Title := 'PNG Test';
  Application.Initialize;
  {$IFNDEF CONSOLE_TESTRUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$ENDIF}
  Application.Run;
end.
