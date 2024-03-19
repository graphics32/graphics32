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
  GR32, GR32_Polygons,
  Clipper, Clipper.Core, Clipper.Engine, Clipper.Offset;

(*
** TFixed variants
*)

function GR32BoolOp(ClipType: TClipType; FillMode: TPolyFillMode; const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint): GR32.TArrayOfArrayOfFixedPoint; overload;

function GR32_Intersect(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint; overload;
function GR32_Union(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint; overload;
function GR32_Difference(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint; overload;
function GR32_XOR(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint; overload;

function GR32_Inflate(const Paths: GR32.TArrayOfArrayOfFixedPoint; Delta: double; JoinType: TJoinType; EndType: TEndType; MiterLimit: double = 2): GR32.TArrayOfArrayOfFixedPoint; overload;


(*
** TFloat variants
*)

function GR32BoolOp(ClipType: TClipType; FillMode: TPolyFillMode; const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint): GR32.TArrayOfArrayOfFloatPoint; overload;

function GR32_Intersect(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint; overload;
function GR32_Union(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint; overload;
function GR32_Difference(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint; overload;
function GR32_XOR(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint; FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint; overload;

function GR32_Inflate(const Paths: GR32.TArrayOfArrayOfFloatPoint; Delta: double; JoinType: TJoinType; EndType: TEndType; MiterLimit: double = 2): GR32.TArrayOfArrayOfFloatPoint; overload;


(*
** TPath64 conversion
*)

function FixedPointsToPath64(const PathFixed: GR32.TArrayOfFixedPoint): Clipper.TPath64;
function FloatPointsToPath64(const PathFloat: GR32.TArrayOfFloatPoint): Clipper.TPath64;
function FixedPointsToPaths64(const PathsFixed: GR32.TArrayOfArrayOfFixedPoint): Clipper.TPaths64;
function FloatPointsToPaths64(const PathsFloat: GR32.TArrayOfArrayOfFloatPoint): Clipper.TPaths64;

function Paths64ToFixedPoints(const Paths: Clipper.TPaths64): GR32.TArrayOfArrayOfFixedPoint;
function Paths64ToFloatPoints(const Paths: Clipper.TPaths64): GR32.TArrayOfArrayOfFloatPoint;

function FloatRect(const r: TRect64): GR32.TFloatRect;


(*
** Convenience redeclarations
*)

type
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


(*
** Ditto consts
*)

const
  frEvenOdd   = Clipper.Core.frEvenOdd;
  frNonZero   = Clipper.Core.frNonZero;
  frPositive  = Clipper.Core.frPositive;
  frNegative  = Clipper.Core.frNegative;

  jtMiter     = Clipper.Offset.jtMiter;
  jtBevel     = Clipper.Offset.jtBevel;
  jtRound     = Clipper.Offset.jtRound;
  jtSquare    = Clipper.Offset.jtSquare;
  jtRoundEx   = Clipper.Offset.jtRound; // Not implemented in Clipper2

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


(*
** Clipper float scale factor
*)

type
  ClipperFloat = {$ifdef RECORD_CLASS_VAR}record{$else}object{$endif}
  private
    class var
      FScale: Double;
      FInvScale: Double;
      FGrowScale: Double;
      FFixedGrowScale: Double;

    class procedure SetScale(Value: Double); static;
  public
    class property Scale: Double read FScale write SetScale;
    class property InvScale: Double read FInvScale;             // 1 / Scale
    class property GrowScale: Double read FGrowScale;           // Scale * OffsetFactor
    class property FixedGrowScale: Double read FFixedGrowScale; // Note: Does not apply scale since fixed values aren't scaled

  const
    // OffsetFactor specifies the ratio between Graphic32's traditional measure
    // of stroke width (or growth delta) and the measure Clipper2 uses.
    //
    // For example, in Graphics32, when we draw a line with a stroke width of 10,
    // we expect a line that is approximately 10 pixel wide. Prior to using
    // Clipper2 to offset polylines into polygons, we would simply inflate the
    // polyline with a growth delta of 10. With the current Clipper2 implementation
    // we instead has to inflate the polyline with a growth delta of 5.
    //
    // In some older versions of Clipper2, OffsetFactor was 1.
    OffsetFactor = 0.5;
  end;

implementation

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
  Result.Left := DoubleToInt64(r.Left * ClipperFloat.Scale);
  Result.Top := DoubleToInt64(r.Top * ClipperFloat.Scale);
  Result.Right := DoubleToInt64(r.Right * ClipperFloat.Scale);
  Result.Bottom  := DoubleToInt64(r.Bottom * ClipperFloat.Scale);
end;

function FixedPointsToPath64(const PathFixed: GR32.TArrayOfFixedPoint): Clipper.TPath64;
var
  i, len: integer;
begin
  len := Length(PathFixed);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := PathFixed[i].X;
    Result[i].Y := PathFixed[i].Y;
  end;
end;

function FloatPointsToPath64(const PathFloat: GR32.TArrayOfFloatPoint): Clipper.TPath64;
var
  i, len: integer;
begin
  len := Length(PathFloat);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := DoubleToInt64(PathFloat[i].X * ClipperFloat.Scale);
    Result[i].Y := DoubleToInt64(PathFloat[i].Y * ClipperFloat.Scale);
  end;
end;

function FixedPointsToPaths64(const PathsFixed: GR32.TArrayOfArrayOfFixedPoint): Clipper.TPaths64;
var
  i, len: integer;
begin
  len := Length(PathsFixed);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FixedPointsToPath64(PathsFixed[i]);
end;

function FloatPointsToPaths64(const PathsFloat: GR32.TArrayOfArrayOfFloatPoint): Clipper.TPaths64;
var
  i, len: integer;
begin
  len := Length(PathsFloat);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatPointsToPath64(PathsFloat[i]);
end;

function Path64ToFixedPoints(const Path: Clipper.TPath64): GR32.TArrayOfFixedPoint;
var
  i, len: integer;
begin
  len := Length(Path);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := TFixed(Path[i].X);
    Result[i].Y := TFixed(Path[i].Y);
  end;
end;

function Path64ToFloatPoints(const Path: Clipper.TPath64): GR32.TArrayOfFloatPoint;
var
  i, len: integer;
begin
  len := Length(Path);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := Path[i].X * ClipperFloat.InvScale;
    Result[i].Y := Path[i].Y * ClipperFloat.InvScale;
  end;
end;

function Paths64ToFixedPoints(const Paths: Clipper.TPaths64): GR32.TArrayOfArrayOfFixedPoint;
var
  i, len: integer;
begin
  len := Length(Paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64ToFixedPoints(Paths[i]);
end;

function Paths64ToFloatPoints(const Paths: Clipper.TPaths64): GR32.TArrayOfArrayOfFloatPoint;
var
  i, len: integer;
begin
  len := Length(Paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64ToFloatPoints(Paths[i]);
end;

function GR32BoolOp(ClipType: TClipType; FillMode: TPolyFillMode;
  const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint):
  GR32.TArrayOfArrayOfFixedPoint;
var
  sub, clp, sol: TPaths64;
begin
  sub := FixedPointsToPaths64(Subject);
  clp := FixedPointsToPaths64(Clip);
  sol := Clipper.BooleanOp(ClipType, sub, clp, TFillRule(FillMode));
  Result := Paths64ToFixedPoints(sol);
end;

function GR32BoolOp(ClipType: TClipType; FillMode: TPolyFillMode;
  const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint):
  GR32.TArrayOfArrayOfFloatPoint;
var
  sub, clp, sol: TPaths64;
begin
  sub := FloatPointsToPaths64(Subject);
  clp := FloatPointsToPaths64(Clip);
  sol := Clipper.BooleanOp(ClipType, sub, clp, TFillRule(FillMode));
  Result := Paths64ToFloatPoints(sol);
end;

function GR32_Intersect(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint;
begin
  Result := GR32BoolOp(ctIntersection, FillMode, Subject, Clip);
end;

function GR32_Union(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint;
begin
  Result := GR32BoolOp(ctUnion, FillMode, Subject, Clip);
end;

function GR32_Difference(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint;
begin
  Result := GR32BoolOp(ctDifference, FillMode, Subject, Clip);
end;

function GR32_XOR(const Subject, Clip: GR32.TArrayOfArrayOfFixedPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFixedPoint;
begin
  Result := GR32BoolOp(ctXor, FillMode, Subject, Clip);
end;

function GR32_Intersect(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint;
begin
  Result := GR32BoolOp(ctIntersection, FillMode, Subject, Clip);
end;

function GR32_Union(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint;
begin
  Result := GR32BoolOp(ctUnion, FillMode, Subject, Clip);
end;

function GR32_Difference(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint;
begin
  Result := GR32BoolOp(ctDifference, FillMode, Subject, Clip);
end;


function GR32_XOR(const Subject, Clip: GR32.TArrayOfArrayOfFloatPoint;
  FillMode: TPolyFillMode): GR32.TArrayOfArrayOfFloatPoint;
begin
  Result := GR32BoolOp(ctXor, FillMode, Subject, Clip);
end;


function GR32_Inflate(const Paths: GR32.TArrayOfArrayOfFixedPoint;
  Delta: double; JoinType: TJoinType; EndType: TEndType;
  MiterLimit: double): GR32.TArrayOfArrayOfFixedPoint;
var
  sub, sol: TPaths64;
begin
  sub := FixedPointsToPaths64(Paths);
  sol := Clipper.InflatePaths(sub, Delta * ClipperFloat.FixedGrowScale, JoinType, EndType, MiterLimit);
  sol := RamerDouglasPeucker(sol, 1);
  Result := Paths64ToFixedPoints(sol);
end;

function GR32_Inflate(const Paths: GR32.TArrayOfArrayOfFloatPoint;
  Delta: double; JoinType: TJoinType; EndType: TEndType;
  MiterLimit: double = 2): GR32.TArrayOfArrayOfFloatPoint;
var
  sub, sol: TPaths64;
begin
  sub := FloatPointsToPaths64(Paths);
  sol := Clipper.InflatePaths(sub, Delta * ClipperFloat.GrowScale, JoinType, EndType, MiterLimit);
  sol := RamerDouglasPeucker(sol, 10);
  Result := Paths64ToFloatPoints(sol);
end;

{ ClipperFloat }

class procedure ClipperFloat.SetScale(Value: Double);
begin
  if (Value <> 0) then
  begin
    FScale := Value;
    FInvScale := 1 / FScale;
    FGrowScale := FScale * OffsetFactor;
    FFixedGrowScale := OffsetFactor * FixedToFloat;
  end;
end;

initialization
  ClipperFloat.Scale := 100;

  // Guard against breaking change in Clipper TFillRule order
  Assert(Ord(TPolyFillMode.pfAlternate) = Ord(TFillRule.frEvenOdd));
  Assert(Ord(TPolyFillMode.pfWinding) = Ord(TFillRule.frNonZero));
  Assert(Ord(TPolyFillMode.pfEvenOdd) = Ord(TFillRule.frEvenOdd));
  Assert(Ord(TPolyFillMode.pfNonZero) = Ord(TFillRule.frNonZero));
end.

