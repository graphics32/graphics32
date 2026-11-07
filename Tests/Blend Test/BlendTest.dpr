program BlendTest;

{

  Delphi DUnitX Test Project
  ---------------------------

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  DUnitX.TestFramework,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Loggers.Console,
  GR32.DUnitx in '..\Tools\GR32.DUnitx.pas',
  TestGR32Blend in 'TestGR32Blend.pas',
  TestGR32BlendModes in 'TestGR32BlendModes.pas',
  TestGR32Premultiply in 'TestGR32Premultiply.pas',
  GR32_Blend in '..\..\Source\GR32_Blend.pas',
  GR32_BlendReference in 'GR32_BlendReference.pas';

{$R *.res}

begin
  Application.Initialize;
  if IsConsole then
  begin
    var Runner := TDUnitX.CreateRunner;
    var ConsoleLogger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(ConsoleLogger);
    var Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := 1;
  end
  else
    DUnitX.Loggers.GUI.VCL.Run;
end.
