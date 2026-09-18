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
    procedure TestClosePath;
    procedure TestMultipleSubpaths;
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
begin
  pts := SvgPathDataToPoints('M 10 10 A 25 25 0 0 0 35 35');
  CheckEquals(1, Length(pts));
  Check(Length(pts[0]) > 2);
  CheckEquals(10.0, pts[0][0].X, 1E-3);
  CheckEquals(10.0, pts[0][0].Y, 1E-3);
  CheckEquals(35.0, pts[0][High(pts[0])].X, 1E-3);
  CheckEquals(35.0, pts[0][High(pts[0])].Y, 1E-3);

  // Mid-point of curve
  CheckEquals(17.32, pts[0][Length(pts[0]) div 2].X, 1E-3);
  CheckEquals(27.68, pts[0][Length(pts[0]) div 2].Y, 1E-3);
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
