unit TestPolygonIssues;

interface

uses
  Classes, Types,
  TestFramework;

type
  TTestPolygonIssues = class(TTestCase)
  private
  public
  published
    procedure Issue272_PolygonFS;
  end;

implementation

uses
  SysUtils,
  GR32,
  GR32_Polygons;

{$RANGECHECKS OFF}

procedure TTestPolygonIssues.Issue272_PolygonFS;
const
  PolyX = 4;
  PolyY = 3;
  Expected: Double = (PolyX * PolyY) / 2;

  procedure DoTest(const p: TArrayOfFloatPoint; MaxDelta: Double);
  var
    Bitmap: TBitmap32;
    i: integer;
    Actual: Double;
  begin
    Bitmap := TBitmap32.Create;
    try
      Bitmap.SetSize(5, 5);
      Bitmap.Clear(0);
      GR32_Polygons.PolygonFS(Bitmap, p, clBlack32);

      Actual := 0;

      // Sum actual polygon coverage
      for i := 0 to (Bitmap.Width * Bitmap.Height)-1 do
        Actual := Actual + TColor32Entry(Bitmap.Bits[i]).A / 255;

    finally
      Bitmap.Free;
    end;

    CheckEquals(Expected, Actual, MaxDelta);
  end;

var
  p: TArrayOfFloatPoint;
  i: integer;
begin

  SetLength(p, 3);

  p[0] := FloatPoint(0, 0);
  p[1] := FloatPoint(PolyX, 0);
  p[2] := FloatPoint(0, PolyY);

  DoTest(p, 0.01);

  // Now translate the polygon 0,5 pixels down and to the right and repeat the test
  for i := 0 to High(p) do
  begin
    p[i].X := p[i].X + 0.5;
    p[i].Y := p[i].Y + 0.5;
  end;

  DoTest(p, 0.1);
end;


initialization
  RegisterTest(TTestPolygonIssues.Suite);
end.


