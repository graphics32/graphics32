program Benchmark.Polygons.VPR;

{$APPTYPE CONSOLE}

{$I GR32.inc}

{-$define TEST_CLIPPING}

uses
  System.SysUtils,
  System.Types,

  Spring.Benchmark,

  GR32,
  GR32_Math,
  GR32_VectorUtils,
  GR32_VPR;

type
  TPolygonCase = record
    Name: string;
    Points: TArrayOfArrayOfFloatPoint;
  end;

  TClipRectCase = record
    Name: string;
    Rect: TFloatRect;
  end;

var
  PolygonCases: array of TPolygonCase;
  ClipRectCases: array of TClipRectCase;

procedure DummyRenderProc(Data: Pointer; const Span: TValueSpan; DstY: Integer);
begin
  // do-nothing render proc to benchmark only the VPR rasterization logic
end;

procedure InitializeCases;
begin
  SetLength(PolygonCases, 10);

  // 1. Star 10 vertices (5 points star)
  PolygonCases[0].Name := 'Star_10_Verts';
  PolygonCases[0].Points := PolyPolygon(Star(200.0, 200.0, 50.0, 150.0, 5));

  // 2. Star 40 vertices (20 points star)
  PolygonCases[1].Name := 'Star_40_Verts';
  PolygonCases[1].Points := PolyPolygon(Star(200.0, 200.0, 50.0, 150.0, 20));

  // 3. Star 100 vertices (50 points star)
  PolygonCases[2].Name := 'Star_100_Verts';
  PolygonCases[2].Points := PolyPolygon(Star(200.0, 200.0, 50.0, 150.0, 50));

  // 4. Circle 10 vertices
  PolygonCases[3].Name := 'Circle_10_Verts';
  PolygonCases[3].Points := PolyPolygon(Circle(200, 200, 150, 10));

  // 5. Circle 32 vertices
  PolygonCases[4].Name := 'Circle_32_Verts';
  PolygonCases[4].Points := PolyPolygon(Circle(200, 200, 150, 32));

  // 6. Circle 100 vertices
  PolygonCases[5].Name := 'Circle_100_Verts';
  PolygonCases[5].Points := PolyPolygon(Circle(200, 200, 150, 100));

  // 7. Ellipse 20 vertices
  PolygonCases[6].Name := 'Ellipse_20_Verts';
  PolygonCases[6].Points := PolyPolygon(Ellipse(200, 200, 150, 100, 20));

  // 8. Ellipse 60 vertices
  PolygonCases[7].Name := 'Ellipse_60_Verts';
  PolygonCases[7].Points := PolyPolygon(Ellipse(200, 200, 150, 100, 60));

  // 9. Nested Stars (two concentric stars, total 100 vertices)
  PolygonCases[8].Name := 'Nested_Stars_100_Verts';
  SetLength(PolygonCases[8].Points, 2);
  PolygonCases[8].Points[0] := Star(200.0, 200.0, 20.0, 60.0, 25); // 50 vertices
  PolygonCases[8].Points[1] := Star(200.0, 200.0, 80.0, 150.0, 25); // 50 vertices

  // 10. Nested Circles (two concentric circles, total 100 vertices)
  PolygonCases[9].Name := 'Nested_Circles_100_Verts';
  SetLength(PolygonCases[9].Points, 2);
  PolygonCases[9].Points[0] := Circle(200, 200, 75, 50); // 50 vertices
  PolygonCases[9].Points[1] := Circle(200, 200, 150, 50); // 50 vertices

{$ifdef TEST_CLIPPING}
  SetLength(ClipRectCases, 3);
{$else}
  SetLength(ClipRectCases, 1);
{$endif}

  // 1. No Clipping
  ClipRectCases[0].Name := 'NoClipping';
  ClipRectCases[0].Rect := FloatRect(0, 0, 1000, 1000);

{$ifdef TEST_CLIPPING}
  // 2. Partial Clipping
  ClipRectCases[1].Name := 'PartialClipping';
  ClipRectCases[1].Rect := FloatRect(100, 100, 300, 300);

  // 3. Severe Clipping
  ClipRectCases[2].Name := 'SevereClipping';
  ClipRectCases[2].Rect := FloatRect(180, 180, 220, 220);
{$endif}

(*
  // 4. Complete Clipping (no overlap)
  ClipRectCases[3].Name := 'CompleteClipping';
  ClipRectCases[3].Rect := FloatRect(500, 500, 600, 600);
*)
end;

procedure BenchmarkVPR(const state: TState);
begin
  var PolyIdx := state[0];
  var ClipIdx := state[1];

  var Points := PolygonCases[PolyIdx].Points;
  var ClipRect := ClipRectCases[ClipIdx].Rect;

  for var _ in state do
  begin
    RenderPolyPolygon(Points, ClipRect, DummyRenderProc);
  end;
end;

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  InitializeCases;

  for var PolyIdx := 0 to High(PolygonCases) do
  begin
    for var ClipIdx := 0 to High(ClipRectCases) do
    begin
      var CaseName := PolygonCases[PolyIdx].Name;
{$ifdef TEST_CLIPPING}
      CaseName := CaseName + '/' + ClipRectCases[ClipIdx].Name;
{$endif}
      var bm := Spring.Benchmark.Benchmark(BenchmarkVPR, CaseName).Args([PolyIdx, ClipIdx]);
      bm.MinTime(5); // seconds
      bm.TimeUnit(kMicrosecond);
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
