program Benchmark.Math.CumSum;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  System.SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_Math;

const
  Sizes: array of integer = [256, 512, 1024, 2048, 4096, 8192];

procedure BenchmarkCumSum(const state: TState);
begin
  var CumSumProc := TCumSumProc(state[0]);
  var Size := state[1];

  var Values: TArray<Single>;
  SetLength(Values, Size);


  for var _ in state do
  begin
    // Different offsets so we also benchmark alignment
    for var Offset := 0 to 255 do
    begin

      state.PauseTiming;
      for var i := 0 to High(Values) do
        Values[i] := i;
      state.ResumeTiming;

      CumSumProc(PSingleArray(@Values[Offset]), Length(Values)-Offset);
    end;
  end;

  state.Counters['Rate'] := Counter(Size, [kIsRate, kIsIterationInvariant]);
end;

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  var Binding := MathRegistry.FindBinding('CumSum');
  Assert(Binding <> nil);

  for var Size in Sizes do
  begin
    for var CumSum in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BenchmarkCumSum, CumSum.Name + '/Size:' + Size.ToString).Args([Int64(CumSum.Proc), Size]);
      bm.Iterations(1000);
    end;
  end;

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
