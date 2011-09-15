program LowLevelTest;

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
  TestGR32LowLevel in 'TestGR32LowLevel.pas',
  GR32_LowLevel in '..\..\Source\GR32_LowLevel.pas;

{ *.RES}

begin
  Application.Title := 'PNG Test';
  Application.Initialize;
  {$IFNDEF CONSOLE_TESTRUNNER}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$ENDIF}
  Application.Run;
end.
