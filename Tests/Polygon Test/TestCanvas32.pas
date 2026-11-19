unit TestCanvas32;

interface

uses
  Classes, Types,
  TestFramework;

type
  TTestCanvas32 = class(TTestCase)
  published
    procedure EllipticalArc_HTML5;
    procedure EllipticalArc_SVG;
    procedure EllipticalArc_SVG_OffCenter;
    procedure CubicBezier_SmallCoordinates;
  end;


implementation

uses
  SysUtils,
  Math,

  GR32,
  GR32_Paths,
  GR32_VectorUtils;

{$RANGECHECKS OFF}

procedure TTestCanvas32.EllipticalArc_HTML5;
var
  Path: TFlattenedPath;
  Points: TArrayOfFloatPoint;
begin
  Path := TFlattenedPath.Create;
  try
    // Draw 90 degree arc on circle/ellipse centered at (100, 100)
    Path.EllipticalArc(FloatPoint(100, 100), 50, 50, 0, 0, Pi * 0.5, False);
    Path.EndPath;

    Check(Length(Path.Path) > 0, 'Path should not be empty');
    Points := Path.Path[0];
    Check(Length(Points) >= 6, 'Arc should flatten into multiple points');

    // First point should be near (150, 100) [center + (rx * cos(0), ry * sin(0))]
    CheckEquals(150.0, Points[0].X, 0.1);
    CheckEquals(100.0, Points[0].Y, 0.1);

    // Last point should be near (100, 150) [center + (rx * cos(pi/2), ry * sin(pi/2))]
    CheckEquals(100.0, Points[High(Points)].X, 0.1);
    CheckEquals(150.0, Points[High(Points)].Y, 0.1);
  finally
    Path.Free;
  end;
end;

procedure TTestCanvas32.EllipticalArc_SVG_OffCenter;
var
  Path: TFlattenedPath;
  Points: TArrayOfFloatPoint;
  i: Integer;
  dist: Double;
begin
  Path := TFlattenedPath.Create;
  try
    // Start at (150, 200), arc to (300, 50) with rx=150, ry=150, apMajor, adNegative
    Path.MoveTo(150, 200);
    Path.EllipticalArc(FloatPoint(300, 50), 150, 150, 0, apMajor, adNegative);
    Path.EndPath;

    Check(Length(Path.Path) > 0, 'Path should not be empty');
    Points := Path.Path[0];
    Check(Length(Points) >= 6, 'Arc should flatten into multiple points');

    // Start point
    CheckEquals(150.0, Points[0].X, 0.1);
    CheckEquals(200.0, Points[0].Y, 0.1);

    // End point
    CheckEquals(300.0, Points[High(Points)].X, 0.1);
    CheckEquals(50.0, Points[High(Points)].Y, 0.1);

    // All points must lie on circle of radius 150 centered at (300, 200)
    for i := 0 to High(Points) do
    begin
      dist := Sqrt(Sqr(Points[i].X - 300.0) + Sqr(Points[i].Y - 200.0));
      CheckEquals(150.0, dist, 1.0);
    end;
  finally
    Path.Free;
  end;
end;

procedure TTestCanvas32.CubicBezier_SmallCoordinates;
var
  Path: TFlattenedPath;
  Points: TArrayOfFloatPoint;
begin
  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(0.0, 0.0);
    Path.CurveTo(FloatPoint(0.201843, 0.201843), FloatPoint(0.403515, 0.403513), FloatPoint(0.609067, 0.572102));
    Path.EndPath;

    Check(Length(Path.Path) > 0, 'Path should not be empty');
    Points := Path.Path[0];
    Check(Length(Points) >= 6, 'Cubic bezier in small coordinates should flatten smoothly into multiple points');
  finally
    Path.Free;
  end;
end;

procedure TTestCanvas32.EllipticalArc_SVG;
var
  Path: TFlattenedPath;
  Points: TArrayOfFloatPoint;
begin
  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(100, 100);
    // Draw SVG arc from (100, 100) to (200, 100) with rx=50, ry=50
    Path.EllipticalArc(FloatPoint(200, 100), 50, 50, 0, apMinor, adPositive);
    Path.EndPath;

    Check(Length(Path.Path) > 0, 'Path should not be empty');
    Points := Path.Path[0];
    Check(Length(Points) >= 6, 'Arc should flatten into multiple points');

    // Start point
    CheckEquals(100.0, Points[0].X, 0.1);
    CheckEquals(100.0, Points[0].Y, 0.1);

    // End point
    CheckEquals(200.0, Points[High(Points)].X, 0.1);
    CheckEquals(100.0, Points[High(Points)].Y, 0.1);
  finally
    Path.Free;
  end;
end;

procedure InitializeTestSuites;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create('GR32_Paths');
  RegisterTest(TestSuite);

  TestSuite.AddTests(TTestCanvas32);
end;

initialization
  InitializeTestSuites;
end.


