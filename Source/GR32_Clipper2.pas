unit GR32_Clipper2;

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
 * The Original Code is GR32_Clipper
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson
 *
 * Portions created by the Initial Developer are Copyright (C) 2012-2022
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Gr32, Gr32_Polygons,
  Clipper, Clipper.Core, Clipper.Engine, Clipper.Offset;

(*
** TFixed variants
*)

function Gr32BoolOp(clipType: TClipType; fillMode: TPolyFillMode;
  const subject, clip: Gr32.TArrayOfArrayOfFixedPoint):
  Gr32.TArrayOfArrayOfFixedPoint; overload;

function Gr32_Intersect(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint; overload;
function Gr32_Union(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint; overload;
function Gr32_Difference(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint; overload;
function Gr32_XOR(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint; overload;

function Gr32_Inflate(const paths: Gr32.TArrayOfArrayOfFixedPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double = 2): Gr32.TArrayOfArrayOfFixedPoint; overload;


(*
** TFloat variants
*)

function Gr32BoolOp(clipType: TClipType; fillMode: TPolyFillMode;
  const subject, clip: Gr32.TArrayOfArrayOfFloatPoint):
  Gr32.TArrayOfArrayOfFloatPoint; overload;

function Gr32_Intersect(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint; overload;
function Gr32_Union(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint; overload;
function Gr32_Difference(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint; overload;
function Gr32_XOR(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint; overload;

function Gr32_Inflate(const paths: Gr32.TArrayOfArrayOfFloatPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double = 2): Gr32.TArrayOfArrayOfFloatPoint; overload;


function FixedPointsToPath64(const pathFixed: Gr32.TArrayOfFixedPoint): Clipper.TPath64;
function FloatPointsToPath64(const pathFloat: Gr32.TArrayOfFloatPoint): Clipper.TPath64;
function FixedPointsToPaths64(const pathsFixed: Gr32.TArrayOfArrayOfFixedPoint): Clipper.TPaths64;
function FloatPointsToPaths64(const pathsFloat: Gr32.TArrayOfArrayOfFloatPoint): Clipper.TPaths64;

function Paths64ToFixedPoints(const paths: Clipper.TPaths64): Gr32.TArrayOfArrayOfFixedPoint;
function Paths64ToFloatPoints(const paths: Clipper.TPaths64): Gr32.TArrayOfArrayOfFloatPoint;

function FloatRect(const r: TRect64): GR32.TFloatRect;

type
  TClipper    = Clipper.Engine.TClipper64;
  TClipper64  = Clipper.Engine.TClipper64;
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath64     = Clipper.Core.TPath64;
  TPaths64    = Clipper.Core.TPaths64;
  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;
  TFillRule   = Clipper.Core.TFillRule;
  TPolyTree64 = Clipper.Engine.TPolyTree64;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;
  TJoinType   = Clipper.Offset.TJoinType;
  TEndType    = Clipper.Offset.TEndType;
  TClipType   = Clipper.Core.TClipType;
const
  frEvenOdd   = Clipper.Core.frEvenOdd;
  frNonZero   = Clipper.Core.frNonZero;
  frPositive  = Clipper.Core.frPositive;
  frNegative  = Clipper.Core.frNegative;
  jtSquare    = Clipper.Offset.jtSquare;
  jtRound     = Clipper.Offset.jtRound;
  jtRoundEx   = Clipper.Offset.jtRound; // Not implemented in Clipper2
  jtMiter     = Clipper.Offset.jtMiter;
  etPolygon   = Clipper.Offset.etPolygon;
  etJoined    = Clipper.Offset.etJoined;
  etButt      = Clipper.Offset.etButt;
  etSquare    = Clipper.Offset.etSquare;
  etRound     = Clipper.Offset.etRound;

  ctNone          = Clipper.Core.ctNone;
  ctIntersection  = Clipper.Core.ctIntersection;
  ctUnion         = Clipper.Core.ctUnion;
  ctDifference    = Clipper.Core.ctDifference;
  ctXor           = Clipper.Core.ctXor;

function ClipperFloatScale: Double; {$IFDEF USEINLINING} inline; {$ENDIF}
function SetClipperFloatScale(Value: Double): Double; {$IFDEF USEINLINING} inline; {$ENDIF}

implementation

var
  FClipperFloatScale: Double;
  FClipperInvFloatScale: Double;

function ClipperFloatScale: Double;
begin
  Result := FClipperFloatScale;
end;

function SetClipperFloatScale(Value: Double): Double;
begin
  Result := FClipperFloatScale;
  if (Value <> 0) then
  begin
    FClipperFloatScale := Value;
    FClipperInvFloatScale := 1/FClipperFloatScale;
  end;
end;

function DoubleToInt64(val: double): Int64; {$IFDEF INLINE} inline; {$ENDIF}
var
  exp: integer;
  i64: UInt64 absolute val;
begin
  //https://en.wikipedia.org/wiki/Double-precision_floating-point_format
  Result := 0;
  if i64 = 0 then
    Exit;
  exp := Integer(Cardinal(i64 shr 52) and $7FF) - 1023;
  //nb: when exp == 1024 then val == INF or NAN.
  if exp < 0 then
    Exit;
  Result := ((i64 and $1FFFFFFFFFFFFF) shr (52 - exp)) or (UInt64(1) shl exp);
  if val < 0 then
    Result := -Result;
end;

function FloatRect(const r: TRect64): GR32.TFloatRect;
begin
  Result.Left := DoubleToInt64(r.Left * FClipperFloatScale);
  Result.Top := DoubleToInt64(r.Top * FClipperFloatScale);
  Result.Right := DoubleToInt64(r.Right * FClipperFloatScale);
  Result.Bottom  := DoubleToInt64(r.Bottom * FClipperFloatScale);
end;

function FixedPointsToPath64(const pathFixed: Gr32.TArrayOfFixedPoint): Clipper.TPath64;
var
  i, len: integer;
begin
  len := Length(pathFixed);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := pathFixed[i].X;
    Result[i].Y := pathFixed[i].Y;
  end;
end;

function FloatPointsToPath64(const pathFloat: Gr32.TArrayOfFloatPoint): Clipper.TPath64;
var
  i, len: integer;
begin
  len := Length(pathFloat);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := DoubleToInt64(pathFloat[i].X * FClipperFloatScale);
    Result[i].Y := DoubleToInt64(pathFloat[i].Y * FClipperFloatScale);
  end;
end;

function FixedPointsToPaths64(const pathsFixed: Gr32.TArrayOfArrayOfFixedPoint): Clipper.TPaths64;
var
  i, len: integer;
begin
  len := Length(pathsFixed);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FixedPointsToPath64(pathsFixed[i]);
end;

function FloatPointsToPaths64(const pathsFloat: Gr32.TArrayOfArrayOfFloatPoint): Clipper.TPaths64;
var
  i, len: integer;
begin
  len := Length(pathsFloat);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatPointsToPath64(pathsFloat[i]);
end;

function Path64ToFixedPoints(const path: Clipper.TPath64): Gr32.TArrayOfFixedPoint;
var
  i, len: integer;
begin
  len := Length(path);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := TFixed(path[i].X);
    Result[i].Y := TFixed(path[i].Y);
  end;
end;

function Path64ToFloatPoints(const path: Clipper.TPath64): Gr32.TArrayOfFloatPoint;
var
  i, len: integer;
begin
  len := Length(path);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := path[i].X * FClipperInvFloatScale;
    Result[i].Y := path[i].Y * FClipperInvFloatScale;
  end;
end;

function Paths64ToFixedPoints(const paths: Clipper.TPaths64): Gr32.TArrayOfArrayOfFixedPoint;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64ToFixedPoints(paths[i]);
end;

function Paths64ToFloatPoints(const paths: Clipper.TPaths64): Gr32.TArrayOfArrayOfFloatPoint;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64ToFloatPoints(paths[i]);
end;

function Gr32BoolOp(clipType: TClipType; fillMode: TPolyFillMode;
  const subject, clip: Gr32.TArrayOfArrayOfFixedPoint):
  Gr32.TArrayOfArrayOfFixedPoint;
var
  sub, clp, sol: TPaths64;
begin
  sub := FixedPointsToPaths64(subject);
  clp := FixedPointsToPaths64(clip);
  sol := Clipper.BooleanOp(clipType, sub, clp, TFillRule(fillMode));
  Result := Paths64ToFixedPoints(sol);
end;

function Gr32BoolOp(clipType: TClipType; fillMode: TPolyFillMode;
  const subject, clip: Gr32.TArrayOfArrayOfFloatPoint):
  Gr32.TArrayOfArrayOfFloatPoint;
var
  sub, clp, sol: TPaths64;
begin
  sub := FloatPointsToPaths64(subject);
  clp := FloatPointsToPaths64(clip);
  sol := Clipper.BooleanOp(clipType, sub, clp, TFillRule(fillMode));
  Result := Paths64ToFloatPoints(sol);
end;

function Gr32_Intersect(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint;
begin
  Result := Gr32BoolOp(ctIntersection, fillMode, subject, clip);
end;

function Gr32_Union(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint;
begin
  Result := Gr32BoolOp(ctUnion, fillMode, subject, clip);
end;

function Gr32_Difference(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint;
begin
  Result := Gr32BoolOp(ctDifference, fillMode, subject, clip);
end;

function Gr32_XOR(const subject, clip: Gr32.TArrayOfArrayOfFixedPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFixedPoint;
begin
  Result := Gr32BoolOp(ctXor, fillMode, subject, clip);
end;

function Gr32_Intersect(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint;
begin
  Result := Gr32BoolOp(ctIntersection, fillMode, subject, clip);
end;

function Gr32_Union(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint;
begin
  Result := Gr32BoolOp(ctUnion, fillMode, subject, clip);
end;

function Gr32_Difference(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint;
begin
  Result := Gr32BoolOp(ctDifference, fillMode, subject, clip);
end;


function Gr32_XOR(const subject, clip: Gr32.TArrayOfArrayOfFloatPoint;
  fillMode: TPolyFillMode): Gr32.TArrayOfArrayOfFloatPoint;
begin
  Result := Gr32BoolOp(ctXor, fillMode, subject, clip);
end;


function Gr32_Inflate(const paths: Gr32.TArrayOfArrayOfFixedPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double): Gr32.TArrayOfArrayOfFixedPoint;
var
  sub, sol: TPaths64;
begin
  sub := FixedPointsToPaths64(paths);
  sol := Clipper.InflatePaths(sub, delta * FixedOne * 0.5,
    jointType, endType, miterLimit);
  sol := RamerDouglasPeucker(sol, 1);
  Result := Paths64ToFixedPoints(sol);
end;

function Gr32_Inflate(const paths: Gr32.TArrayOfArrayOfFloatPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double = 2): Gr32.TArrayOfArrayOfFloatPoint;
var
  sub, sol: TPaths64;
begin
  sub := FloatPointsToPaths64(paths);
  sol := Clipper.InflatePaths(sub, delta * FClipperFloatScale * 0.5,
    jointType, endType, miterLimit);
  sol := RamerDouglasPeucker(sol, 10);
  Result := Paths64ToFloatPoints(sol);
end;

initialization
  SetClipperFloatScale(100);

  // Guard against breaking change in Clipper TFillRule order
  Assert(Ord(TPolyFillMode.pfAlternate) = Ord(TFillRule.frEvenOdd));
  Assert(Ord(TPolyFillMode.pfWinding) = Ord(TFillRule.frNonZero));
  Assert(Ord(TPolyFillMode.pfEvenOdd) = Ord(TFillRule.frEvenOdd));
  Assert(Ord(TPolyFillMode.pfNonZero) = Ord(TFillRule.frNonZero));
end.

