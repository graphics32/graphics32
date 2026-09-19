unit GR32.Tests.SVG.Types;

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
  SysUtils, Classes, Math,
  GR32, GR32_Transforms, GR32.SVG.Types;

type
  TTestSvgTypes = class(TTestCase)
  published
    procedure TestSvgLengthParseAndToPixels;
    procedure TestSvgColorParse;
    procedure TestSvgPreserveAspectRatioParse;
    procedure TestSvgViewBoxParseAndTransform;
    procedure TestFloatMatrixHelper;
    procedure TestHslToRgb;
  end;

implementation

uses
  Types;

{ TTestSvgTypes }

procedure TTestSvgTypes.TestSvgLengthParseAndToPixels;
var
  len: TSvgLength;
begin
  len := TSvgLength.Parse('100px');
  CheckEquals(100.0, len.Value, 1E-4);
  CheckEquals(Ord(suPx), Ord(len.UnitType));
  CheckEquals(100.0, len.ToPixels, 1E-4);

  len := TSvgLength.Parse('2in');
  CheckEquals(2.0, len.Value, 1E-4);
  CheckEquals(Ord(suIn), Ord(len.UnitType));
  CheckEquals(192.0, len.ToPixels(0, 96.0), 1E-4);

  len := TSvgLength.Parse('50%');
  CheckEquals(50.0, len.Value, 1E-4);
  CheckEquals(Ord(suPercent), Ord(len.UnitType));
  CheckEquals(250.0, len.ToPixels(500.0), 1E-4);

  len := TSvgLength.Parse('1.5em');
  CheckEquals(1.5, len.Value, 1E-4);
  CheckEquals(Ord(suEm), Ord(len.UnitType));
  CheckEquals(24.0, len.ToPixels(0, 96.0, 16.0), 1E-4);

  len := TSvgLength.Parse('10ex');
  CheckEquals(10.0, len.Value, 1E-4);
  CheckEquals(Ord(suEx), Ord(len.UnitType));
  CheckEquals(80.0, len.ToPixels(0, 96.0, 16.0), 1E-4);

  len := TSvgLength.Parse('1.5e2px');
  CheckEquals(150.0, len.Value, 1E-4);
  CheckEquals(Ord(suPx), Ord(len.UnitType));
  CheckEquals(150.0, len.ToPixels, 1E-4);
end;

procedure TTestSvgTypes.TestSvgColorParse;
var
  c: TSvgColor;
begin
  c := TSvgColor.Parse('none');
  Check(c.IsNone);

  c := TSvgColor.Parse('currentColor');
  Check(c.IsCurrentColor);

  c := TSvgColor.Parse('#ff0000');
  CheckEquals(clRed32, c.Color);

  c := TSvgColor.Parse('#0f0');
  CheckEquals(clLime32, c.Color);

  c := TSvgColor.Parse('rgb(0, 0, 255)');
  CheckEquals(clBlue32, c.Color);

  c := TSvgColor.Parse('rgba(255, 255, 0, 0.5)');
  CheckEquals($80FFFF00, c.Color);

  c := TSvgColor.Parse('red');
  CheckEquals(clRed32, c.Color);

  c := TSvgColor.Parse('cyan');
  CheckEquals(clAqua32, c.Color);

  c := TSvgColor.Parse('fuchsia');
  CheckEquals(clFuchsia32, c.Color);

  c := TSvgColor.Parse('forestgreen');
  CheckEquals(clForestGreen32, c.Color);

  c := TSvgColor.Parse('pink');
  CheckEquals(clPink32, c.Color);

  c := TSvgColor.Parse('orange');
  CheckEquals(clOrange32, c.Color);

  c := TSvgColor.Parse('coral');
  CheckEquals(clCoral32, c.Color);

  c := TSvgColor.Parse('gold');
  CheckEquals(clGold32, c.Color);

  c := TSvgColor.Parse('transparent');
  CheckEquals(clNone32, c.Color);

  c := TSvgColor.Parse('cornflowerblue');
  CheckEquals(clCornFlowerBlue32, c.Color);

  c := TSvgColor.Parse('aliceblue');
  CheckEquals(clAliceBlue32, c.Color);

  c := TSvgColor.Parse('lightgoldenrodyellow');
  CheckEquals(clLightGoldenRodYellow32, c.Color);

  c := TSvgColor.Parse('mediumspringgreen');
  CheckEquals(clMediumSpringGreen32, c.Color);
end;

procedure TTestSvgTypes.TestSvgPreserveAspectRatioParse;
var
  ar: TSvgPreserveAspectRatio;
begin
  ar := TSvgPreserveAspectRatio.Parse('xMidYMid meet');
  CheckEquals(Ord(saXMidYMid), Ord(ar.Align));
  CheckEquals(Ord(msMeet), Ord(ar.MeetOrSlice));

  ar := TSvgPreserveAspectRatio.Parse('xMinYMax slice');
  CheckEquals(Ord(saXMinYMax), Ord(ar.Align));
  CheckEquals(Ord(msSlice), Ord(ar.MeetOrSlice));

  ar := TSvgPreserveAspectRatio.Parse('none');
  CheckEquals(Ord(saNone), Ord(ar.Align));
end;

procedure TTestSvgTypes.TestSvgViewBoxParseAndTransform;
var
  vb: TSvgViewBox;
  targetRect: TFloatRect;
  aspect: TSvgPreserveAspectRatio;
  m: TFloatMatrix;
  helper: TFloatMatrixHelper;
  pIn, pOut: TFloatPoint;
begin
  vb := TSvgViewBox.Parse('0 0 100 200');
  Check(vb.IsValid);
  CheckEquals(0.0, vb.X, 1E-4);
  CheckEquals(0.0, vb.Y, 1E-4);
  CheckEquals(100.0, vb.Width, 1E-4);
  CheckEquals(200.0, vb.Height, 1E-4);

  targetRect := FloatRect(0, 0, 200, 400);
  aspect := TSvgPreserveAspectRatio.Parse('none');
  m := vb.GetTransform(targetRect, aspect);

  pIn := FloatPoint(50, 50);
  helper.Matrix := m;
  pOut := helper.TransformPoint(pIn);
  CheckEquals(100.0, pOut.X, 1E-4);
  CheckEquals(100.0, pOut.Y, 1E-4);
end;

procedure TTestSvgTypes.TestFloatMatrixHelper;
var
  helper: TFloatMatrixHelper;
  pIn, pOut: TFloatPoint;
begin
  helper.Matrix := IdentityMatrix;
  helper.Translate(10, 20);
  pIn := FloatPoint(5, 5);
  pOut := helper.TransformPoint(pIn);
  CheckEquals(15.0, pOut.X, 1E-4);
  CheckEquals(25.0, pOut.Y, 1E-4);

  helper.Matrix := IdentityMatrix;
  helper.Scale(2, 3);
  pOut := helper.TransformPoint(pIn);
  CheckEquals(10.0, pOut.X, 1E-4);
  CheckEquals(15.0, pOut.Y, 1E-4);
end;

procedure TTestSvgTypes.TestHslToRgb;
var
  color: TColor32;
begin
  color := GR32.SVG.Types.HSLtoRGB(0.0, 1.0, 0.5, 1.0);
  CheckEquals(clRed32, color);
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgTypes.Suite);
{$ELSE}
  RegisterTest(TTestSvgTypes.Suite);
{$ENDIF}

end.
