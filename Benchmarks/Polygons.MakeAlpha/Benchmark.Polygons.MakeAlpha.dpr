program Benchmark.Polygons.MakeAlpha;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  System.SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_Math,
  GR32_Polygons;

const
  Sizes: array of integer = [256, 512, 1024, 2048, 4096, 8192];

procedure BenchmarkMakeAlpha(const state: TState);
begin
  var Proc := TFillProc(state[0]);
  var Size := state[1];

  // TFillProc = procedure(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);
  var Values: TArray<Single>;
  var Colors: TArray<TColor32>;
  SetLength(Values, Size);
  SetLength(Colors, Size);



  for var _ in state do
  begin
    // Different offsets so we also benchmark alignment
    for var Offset := 0 to 255 do
    begin
      state.PauseTiming;

      var Color: TColor32;
      Color := SetAlpha(clRed32, Offset);

      // Create array of float values [-1..+1] with some streams of consecutive duplicates
      // in case the algorithm is able to take advantage of those.
      for var i := 0 to High(Values) do
        Values[i] := ((i div 4) mod 21) / 10 - 1;

      state.ResumeTiming;

      Proc(PSingleArray(@Values[Offset]), PColor32Array(@Colors[0]), Length(Values)-Offset, Color);
    end;
  end;

end;

procedure Main;
begin

  var Binding := PolygonsRegistry.FindBinding('MakeAlphaNonZeroUP');
  Assert(Binding <> nil);

  for var Size in Sizes do
  begin
    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BenchmarkMakeAlpha, Implement.Name + '/Size:' + Size.ToString).Args([Int64(Implement.Proc), Size]);
      bm.Iterations(1000);
    end;
  end;

  Spring.Benchmark.Benchmark_Main;
end;

begin
  try
    Main;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
