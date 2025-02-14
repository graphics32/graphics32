program Benchmark.LowLevel.FastTrunc;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  System.Types,
  Spring.Benchmark,
  GR32,
  GR32_Bindings,
  GR32_System,
  GR32_LowLevel;

procedure BM_FastTrunc(const state: TState);
begin
  var FastTruncProc: TFastRoundSingleProc := TFastRoundSingleProc(state[0]);

  for var _ in state do
  begin
    RandSeed := 0;

    for var i := 1 to 1000*1000*1000 do
    begin
      FastTruncProc(Random(i) / i);
    end;
  end;
end;

procedure Main;

  procedure DoTest(const Binding: IBindingInfo);
  begin
    Assert(Binding <> nil);

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_FastTrunc, Implement.Name).Args([Int64(Implement.Proc)]);
      bm.TimeUnit(kMillisecond);
    end;
  end;

begin
  Spring.Benchmark.benchmark_format_args := False;

  SetPerformanceAffinityMask;

  var Binding := LowLevelRegistry.FindBinding('FastTrunc');

  DoTest(Binding);

  Binding := LowLevelRegistry.FindBinding('FastRound');
  DoTest(Binding);

  Binding := LowLevelRegistry.FindBinding('FastFloorSingle');
  DoTest(Binding);

  Spring.Benchmark.Benchmark_Main;
end;

begin
  try
    Main;
    WriteLn('Done');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

