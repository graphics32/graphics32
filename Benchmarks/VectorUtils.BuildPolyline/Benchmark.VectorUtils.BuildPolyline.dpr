program Benchmark.VectorUtils.BuildPolyline;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  System.Types,
  Spring.Benchmark,
  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_VectorUtils.Reference,
  GR32_VectorUtils.Angus,
  GR32_VectorUtils.Clipper2;

procedure BM_BuildPolyLine(const state: TState);
const
  Count = 5000;
begin
  var PolylineBuilder := TPolylineBuilderClass(state[0]);
  var JoinStyle := jsRound;

  for var _ in state do
  begin
    state.PauseTiming;

    RandSeed := 0;

    var Polyline: TArrayOfFloatPoint;
    SetLength(Polyline, Count);

    for var i := 0 to High(Polyline) do
      Polyline[i] := FloatPoint(Random(1000), Random(1000));

    state.ResumeTiming;

    PolylineBuilder.BuildPolyLine(Polyline, 2.0, JoinStyle, esButt);
  end;
end;

procedure BM_BuildPolyPolyLine(const state: TState);
const
  CountMajor = 5;
  CountMinor = 1000;
begin
  var PolylineBuilder := TPolylineBuilderClass(state[0]);
  var JoinStyle := jsRound;

  for var _ in state do
  begin
    state.PauseTiming;

    RandSeed := 0;

    var PolyPolyline: TArrayOfArrayOfFloatPoint;
    SetLength(PolyPolyline, CountMajor);

    for var i := 0 to High(PolyPolyline) do
    begin
      SetLength(PolyPolyline[i], CountMinor);
      for var j := 0 to High(PolyPolyline[i]) do
        PolyPolyline[i, j] := FloatPoint(Random(1000), Random(1000));
    end;

    state.ResumeTiming;

    PolylineBuilder.BuildPolyPolyLine(PolyPolyline, True, 2.0, JoinStyle, esButt);
  end;
end;


procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  for var PolyLineBuilderClass in [PolyLineBuilderReference, PolyLineBuilderAngus, PolyLineBuilderClipper] do
    Spring.Benchmark.Benchmark(BM_BuildPolyline, 'BuildPolyLine/'+PolyLineBuilderClass.ClassName).Arg(Int64(PolyLineBuilderClass)).TimeUnit(kMillisecond);

  for var PolyLineBuilderClass in [PolyLineBuilderReference, PolyLineBuilderAngus, PolyLineBuilderClipper] do
    Spring.Benchmark.Benchmark(BM_BuildPolyPolyline, 'BuildPolyPolyLine/'+PolyLineBuilderClass.ClassName).Arg(Int64(PolyLineBuilderClass)).TimeUnit(kMillisecond);


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

