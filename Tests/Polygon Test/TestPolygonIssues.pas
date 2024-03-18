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
    procedure RoundRect_MissingPoint;
    procedure RoundRect_NoArc;
  end;

implementation

uses
  SysUtils,
  Math,
  GR32,
  GR32_VectorUtils,
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


procedure TTestPolygonIssues.RoundRect_MissingPoint;
var
  i: integer;
  Found: boolean;
const
  Epsilon = 0.001;
begin
  var Rect := FloatRect(10, 10, 30, 20);
  var RoundedRectangle := RoundRect(Rect, 2.0);

  // The straight rectangle contains 4 vertices.
  // The rounded rectangle should at least contain 8 vertices (choppy arc; only start
  // and end point in the arc) and preferably 12 vertices (3 or more vertice in the
  // arc).
  Check(Length(RoundedRectangle) >= 12);


  //
  // Verify that RoundRect produces a polygon aligned with the X and Y axis
  //

  // Look for horizontal segment going from (x1,10) to (x2,10)
  Found := False;
  for i := 0 to High(RoundedRectangle) do
    if (RoundedRectangle[i].Y = Rect.Top) then
    begin
      Found := SameValue(Rect.Top, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].Y, Epsilon);
      if (Found) then
      begin
        CheckNotEquals(RoundedRectangle[i].X, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].X, Epsilon);
        break;
      end;
    end;
  Check(Found);

  // Look for horizontal segment going from (x1,20) to (x2,20)
  Found := False;
  for i := 0 to High(RoundedRectangle) do
    if (RoundedRectangle[i].Y = Rect.Bottom) then
    begin
      Found := SameValue(Rect.Bottom, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].Y, Epsilon);
      if (Found) then
      begin
        CheckNotEquals(RoundedRectangle[i].X, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].X, Epsilon);
        break;
      end;
    end;
  Check(Found);

  // Look for vertical segment going from (10,y1) to (10,y2)
  Found := False;
  for i := 0 to High(RoundedRectangle) do
    if (RoundedRectangle[i].X = Rect.Left) then
    begin
      Found := SameValue(Rect.Left, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].X, Epsilon);
      if (Found) then
      begin
        CheckNotEquals(RoundedRectangle[i].Y, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].Y, Epsilon);
        break;
      end;
    end;
  Check(Found);

  // Look for vertical segment going from (30,y1) to (30,y2)
  Found := False;
  for i := 0 to High(RoundedRectangle) do
    if (RoundedRectangle[i].X = Rect.Right) then
    begin
      Found := SameValue(Rect.Right, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].X, 0.001);
      if (Found) then
      begin
        CheckNotEquals(RoundedRectangle[i].Y, RoundedRectangle[(i+1) mod Length(RoundedRectangle)].Y, Epsilon);
        break;
      end;
    end;
  Check(Found);

end;

procedure TTestPolygonIssues.RoundRect_NoArc;
begin
  var Rect := FloatRect(10, 10, 30, 20);
  var RoundedRectangle := RoundRect(Rect, 0.0);

  // Round with zero arc radius should just produce a straight rectangle with 4 vertices.
  Check(Length(RoundedRectangle) = 4);

  var Bounds := PolygonBounds(RoundedRectangle);
  Check(Rect = Bounds);
end;

initialization
  RegisterTest(TTestPolygonIssues.Suite);
end.


