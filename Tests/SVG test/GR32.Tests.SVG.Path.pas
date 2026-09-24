unit GR32.Tests.SVG.Path;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  SysUtils, Classes,
  GR32, GR32_Paths, GR32.SVG.Path;

type
  TTestSvgPath = class(TTestCase)
  published
    procedure TestSimpleLines;
    procedure TestRelativeMovement;
    procedure TestBeziers;
    procedure TestArc;
    procedure TestPieSliceArc;
    procedure TestSmallCoordinateCubicBezier;
    procedure TestClosePath;
    procedure TestMultipleSubpaths;
    procedure TestCircleWithTwoArcs;
  end;

implementation

{ TTestSvgPath }

procedure TTestSvgPath.TestSimpleLines;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  pts := SvgPathDataToPoints('M 10 20 L 30 40 H 50 V 60');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) >= 4);
  CheckEquals(10.0, pts[0][0].X, 1E-3);
  CheckEquals(20.0, pts[0][0].Y, 1E-3);
  CheckEquals(30.0, pts[0][1].X, 1E-3);
  CheckEquals(40.0, pts[0][1].Y, 1E-3);
  CheckEquals(50.0, pts[0][2].X, 1E-3);
  CheckEquals(40.0, pts[0][2].Y, 1E-3);
  CheckEquals(50.0, pts[0][3].X, 1E-3);
  CheckEquals(60.0, pts[0][3].Y, 1E-3);
end;

procedure TTestSvgPath.TestRelativeMovement;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  pts := SvgPathDataToPoints('m 10 10 l 20 20 h 10 v -5');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) >= 4);
  CheckEquals(10.0, pts[0][0].X, 1E-3);
  CheckEquals(10.0, pts[0][0].Y, 1E-3);
  CheckEquals(30.0, pts[0][1].X, 1E-3);
  CheckEquals(30.0, pts[0][1].Y, 1E-3);
  CheckEquals(40.0, pts[0][2].X, 1E-3);
  CheckEquals(30.0, pts[0][2].Y, 1E-3);
  CheckEquals(40.0, pts[0][3].X, 1E-3);
  CheckEquals(25.0, pts[0][3].Y, 1E-3);
end;

procedure TTestSvgPath.TestBeziers;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  pts := SvgPathDataToPoints('M 0 0 C 10 20 30 40 50 50 S 90 80 100 100');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) > 2);
  CheckEquals(0.0, pts[0][0].X, 1E-3);
  CheckEquals(0.0, pts[0][0].Y, 1E-3);
  CheckEquals(100.0, pts[0][High(pts[0])].X, 1E-3);
  CheckEquals(100.0, pts[0][High(pts[0])].Y, 1E-3);
end;

procedure TTestSvgPath.TestArc;
var
  pts: TArrayOfArrayOfFloatPoint;
  midIdx: Integer;
begin
  pts := SvgPathDataToPoints('M 10 10 A 25 25 0 0 0 35 35');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) > 2);
  CheckEquals(10.0, pts[0][0].X, 1E-3);
  CheckEquals(10.0, pts[0][0].Y, 1E-3);
  CheckEquals(35.0, pts[0][High(pts[0])].X, 1E-3);
  CheckEquals(35.0, pts[0][High(pts[0])].Y, 1E-3);

  // Center of circle passing through (10,10) and (35,35) with r=25, fA=0, fS=0 is (35,10)
  // Mid-point of curve is at angle 135 degrees from center (35,10): (35 + 25*cos(135°), 10 + 25*sin(135°)) = (17.322, 27.678)
  midIdx := Length(pts[0]) div 2;
  CheckEquals(17.322, pts[0][midIdx].X, 0.5);
  CheckEquals(27.678, pts[0][midIdx].Y, 0.5);
end;

procedure TTestSvgPath.TestSmallCoordinateCubicBezier;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  // Cubic bezier with small coordinates (span ~0.8 units)
  pts := SvgPathDataToPoints('M 0.0,0.0 C 0.201843,0.201843 0.403515,0.403513 0.609067,0.572102');
  CheckEquals(1, Length(pts));

  // Should generate multiple flattened points (subdivided curve), not just start and end points
  Check(Length(pts[0]) >= 6, 'Small coordinate cubic bezier should subdivide smoothly into multiple points');
  CheckEquals(0.0, pts[0][0].X, 1E-3);
  CheckEquals(0.0, pts[0][0].Y, 1E-3);
  CheckEquals(0.609067, pts[0][High(pts[0])].X, 1E-3);
  CheckEquals(0.572102, pts[0][High(pts[0])].Y, 1E-3);
end;

procedure TTestSvgPath.TestPieSliceArc;
var
  pts: TArrayOfArrayOfFloatPoint;
  i, arcStartIdx, arcEndIdx, midIdx: Integer;
  dist, centerX, centerY, radius: Double;
begin
  // W3C SVG 1.1 spec 25% pie slice example path
  pts := SvgPathDataToPoints('M300,200 h-150 a150,150 0 1,0 150,-150 z');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) >= 6);

  // Start point (MoveTo)
  CheckEquals(300.0, pts[0][0].X, 1E-3);
  CheckEquals(200.0, pts[0][0].Y, 1E-3);

  // LineTo (300-150, 200) = (150, 200)
  CheckEquals(150.0, pts[0][1].X, 1E-3);
  CheckEquals(200.0, pts[0][1].Y, 1E-3);

  // End of arc point (300, 50) before ClosePath
  arcStartIdx := 1;
  arcEndIdx := High(pts[0]) - 1; // Last point before closed start point duplicate
  CheckEquals(300.0, pts[0][arcEndIdx].X, 1E-3);
  CheckEquals(50.0, pts[0][arcEndIdx].Y, 1E-3);

  // Verify all arc points have distance 150 from center (300, 200)
  centerX := 300.0;
  centerY := 200.0;
  radius := 150.0;
  for i := arcStartIdx to arcEndIdx do
  begin
    dist := Sqrt(Sqr(pts[0][i].X - centerX) + Sqr(pts[0][i].Y - centerY));
    CheckEquals(radius, dist, 1.0);
  end;

  // Mid-point of 270-degree arc should be at (300 + 150*cos(45°), 200 + 150*sin(45°)) = (406.066, 306.066)
  midIdx := (arcStartIdx + arcEndIdx) div 2;
  CheckEquals(406.066, pts[0][midIdx].X, 2.0);
  CheckEquals(306.066, pts[0][midIdx].Y, 2.0);
end;

procedure TTestSvgPath.TestClosePath;
var
  path: TFlattenedPath;
begin
  path := SvgPathDataToPath('M 10 10 L 20 10 L 20 20 Z');
  try
    CheckEquals(1, Length(path.Path));
    Check(Length(path.Path[0]) >= 3);
  finally
    path.Free;
  end;
end;

procedure TTestSvgPath.TestCircleWithTwoArcs;
var
  pts: TArrayOfArrayOfFloatPoint;
  i: Integer;
  dist, cx, cy, r: Double;
begin
  // Standard comma-separated arc path
  pts := SvgPathDataToPoints('M32,3 A29,29,0,1,0,61,32 29,29,0,0,0,32,3 Z');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) >= 12, 'Multi-arc path should flatten into multiple points');

  cx := 32.0;
  cy := 32.0;
  r := 29.0;

  for i := 0 to High(pts[0]) do
  begin
    dist := Sqrt(Sqr(pts[0][i].X - cx) + Sqr(pts[0][i].Y - cy));
    CheckEquals(r, dist, 0.5);
  end;

  // Condensed flag-concatenated arc path without commas between rotation and flags (e.g. 010 and 000)
  pts := SvgPathDataToPoints('M32,3 A29,29,01061,32 29,29,00032,3 Z');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) >= 12, 'Condensed multi-arc path should parse all arc segments');

  for i := 0 to High(pts[0]) do
  begin
    dist := Sqrt(Sqr(pts[0][i].X - cx) + Sqr(pts[0][i].Y - cy));
    CheckEquals(r, dist, 0.5);
  end;
end;

procedure TTestSvgPath.TestMultipleSubpaths;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  pts := SvgPathDataToPoints('M 10 10 L 20 20 M 30 30 L 40 40');
  CheckEquals(2, Length(pts));
  CheckEquals(10.0, pts[0][0].X, 1E-3);
  CheckEquals(30.0, pts[1][0].X, 1E-3);
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgPath.Suite);
{$ELSE}
  RegisterTest(TTestSvgPath.Suite);
{$ENDIF}

end.
