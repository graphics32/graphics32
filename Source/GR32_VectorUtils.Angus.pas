unit GR32_VectorUtils.Angus;

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
 * The Original Code is Vector drawing for TImage32
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson (http://www.angusj.com)
 *
 * Portions created by the Initial Developer are Copyright (C) 2019-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$BOOLEVAL OFF}

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_Geometry; // In interface section for inlining

//------------------------------------------------------------------------------
//
//      Grow and BuildPoly*line replacements adapted from Image32
//
//------------------------------------------------------------------------------
// Note: Does not currently support JoinStyle=jsSquare; jsBevel is used instead.
//------------------------------------------------------------------------------
// The CalcRoundingSteps function has been rewritten to use the same algorithm
// as the Arc function.
//------------------------------------------------------------------------------


type
  PolyLineBuilderAngus = class(TPolyLineBuilder)
  protected
    // Float
    class function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;
  public
    class function SupportedJoinStyles: TJoinStyles; override;
    class function SupportedEndStyles: TEndStyles; override;

    // Float
    class function BuildPolyLine(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;
    class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload; override;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,

  GR32_Math;

const
  GrowScale = 1.0;

//------------------------------------------------------------------------------

class function PolyLineBuilderAngus.SupportedEndStyles: TEndStyles;
begin
  Result := [esButt, esSquare, esRound];
end;

class function PolyLineBuilderAngus.SupportedJoinStyles: TJoinStyles;
begin
  Result := [jsMiter, jsBevel, jsRound];
end;

//------------------------------------------------------------------------------
//
//      Type and value mappings
//
//------------------------------------------------------------------------------
type
  TJoinStyle  = (jsAuto, jsSquare, jsMiter, jsRound);
  TEndStyle   = (esPolygon = 0, esClosed = 0, esButt, esSquare, esRound);

const
  // Note: We map Graphics32's jsBevel to jsSquare, and jsRoundEx to jsRound
  JoinStyleMap: array[GR32_Polygons.TJoinStyle] of TJoinStyle = (jsMiter, jsSquare, jsRound, jsRound, jsSquare);
  EndStyleMap: array[GR32_Polygons.TEndStyle] of TEndStyle = (esButt, esSquare, esRound);

type
  TPathD = TArrayOfFloatPoint;
  TPathsD = TArrayOfArrayOfFloatPoint;
  TPointD = TFloatPoint;
  TRectD = TFloatRect;


//------------------------------------------------------------------------------
//
//      Grow types
//
//------------------------------------------------------------------------------
const
  InvalidPointD : TPointD = (X: -Infinity; Y: -Infinity);


//------------------------------------------------------------------------------
//
//      Grow global config
//
//------------------------------------------------------------------------------
var
  //AutoWidthThreshold: When JoinStyle = jsAuto, this is the threshold at
  //which line joins will be rounded instead of squared. With wider strokes,
  //rounded joins generally look better, but as rounding is more complex it
  //also requries more processing and hence is slower to execute.
  AutoWidthThreshold: double = 5.0;
  //When lines are too narrow, they become too faint to sensibly draw
  MinStrokeWidth: double = 0.5;
  //Miter limit avoids excessive spikes when line offsetting
  DefaultMiterLimit: double = 4.0;


//------------------------------------------------------------------------------
//
//      Function redirects to Graphics32 equivalents
//
//------------------------------------------------------------------------------
function PointD(const X, Y: Double): TPointD; inline;
begin
  Result := FloatPoint(X, Y);
end;

function RectD(const L, T, R, B: TFloat): TRectD; inline;
begin
  Result := FloatRect(L, T, R, B);
end;

function GetBoundsD(const path: TPathD): TRectD; inline;
begin
  Result := PolygonBounds(path);
end;

function ReversePath(const path: TPathD): TPathD; inline;
begin
  Result := ReversePolygon(path);
end;

function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD; out ip: TPointD): Boolean; overload;
var
  m1,b1,m2,b2: double;
begin
   // Note: Returns the intersection between the two lines.
   // Unlike the Graphics32 Intersect function it does not test for
   // intersection between the line segments.

  result := False;
  //see http://paulbourke.net/geometry/pointlineplane/
  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; //parallel lines
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    ip.X := ln1A.X;
    ip.Y := m2*ln1A.X + b2;
    Result := True;
  end
  else if (ln2B.X = ln2A.X) then
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    ip.X := ln2A.X;
    ip.Y := m1*ln2A.X + b1;
    Result := True;
  end else
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    if m1 = m2 then exit; //parallel lines
    ip.X := (b2 - b1)/(m1 - m2);
    ip.Y := m1 * ip.X + b1;
    Result := True;
  end;
end;

function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;  overload; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if (not IntersectPoint(ln1a, ln1b, ln2a, ln2b, Result)) then
    Result := InvalidPointD;
end;

function PointsNearEqual(const pt1, pt2: TPointD; distSqrd: double): Boolean; inline;
begin
  Result := SamePoint(pt1, pt2, distSqrd);
end;

function PointsEqual(const pt1, pt2: TPointD): Boolean; inline;
begin
  result := (pt1 = pt2);
end;

function DotProduct(const vector1, vector2: TPointD): double; inline;
begin
  Result := Dot(vector1, vector2);
end;

//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function NormalizeVector(const vec: TPointD): TPointD; inline;
var
  h, inverseHypot: Double;
begin
  h := GR32_Math.Hypot(vec.X, vec.Y);
  if IsZero(h, 0.001) then
  begin
    Result := Default(TPointD);
    Exit;
  end;
  inverseHypot := 1 / h;
  Result.X := vec.X * inverseHypot;
  Result.Y := vec.Y * inverseHypot;
end;

//------------------------------------------------------------------------------

function ReflectPoint(const pt, pivot: TPointD): TPointD; inline;
begin
  Result.X := pivot.X + (pivot.X - pt.X);
  Result.Y := pivot.Y + (pivot.Y - pt.Y);
end;

//------------------------------------------------------------------------------

function GetNormals(const path: TPathD): TPathD;
var
  i, highI: integer;
  last: TPointD;
begin
  highI := High(path);
  setLength(result, highI+1);
  if highI < 0 then Exit;

  last := Default(TPointD);
  for i := 0 to highI -1 do
  begin
    if (not SamePoint(path[i], path[i+1], 0.001)) then
      GetUnitNormal(path[i], path[i+1], last);

    result[i] := last;
(*
    if GetUnitNormal(path[i], path[i+1], result[i]) then
      last := result[i] else
      result[i] := last;
*)
  end;

  if (not SamePoint(path[highI], path[0], 0.001)) then
  begin
    GetUnitNormal(path[highI], path[0], result[highI]);
    last := result[highI];
  end;

  for i := 0 to highI do
  begin
    if (result[i].X <> 0) or (result[i].Y <> 0) then Break;
    result[i] := last;
  end;

end;

//------------------------------------------------------------------------------

function ApplyNormal(const pt, norm: TPointD; delta: double): TPointD; inline;
begin
  result := PointD(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
end;

//------------------------------------------------------------------------------

function StripNearDuplicates(const path: TPathD;
  minDist: double; isClosedPath: Boolean): TPathD;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := path[0];
  j := 0;
  minDist := minDist * minDist;
  for i := 1 to len -1 do
    if not PointsNearEqual(Result[j], path[i], minDist) then
    begin
      inc(j);
      Result[j] := path[i];
    end;
  if isClosedPath and
    PointsNearEqual(Result[j], Result[0], minDist) then dec(j);
  SetLength(Result, j +1);
end;

//------------------------------------------------------------------------------

procedure AppendToPath(var path: TPathD; const pt: TPointD);
var
  len: integer;
begin
  len := length(path);
  if (len > 0) and PointsEqual(pt, path[len -1]) then Exit;
  setLength(path, len + 1);
  path[len] := pt;
end;

//------------------------------------------------------------------------------

procedure AppendPath(var path1: TPathD; const path2: TPathD); overload;
var
  len1, len2: integer;
begin
  len1 := length(path1);
  len2 := length(path2);
  if len2 = 0 then Exit;
  if (len1 > 0) and (path2[0] = path1[len1 -1]) then dec(len1);
  setLength(path1, len1 + len2);
  Move(path2[0], path1[len1], len2 * SizeOf(TPointD));
end;

procedure AppendPath(var paths: TPathsD; const extra: TPathD); overload;
var
  len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + 1);
  paths[len1] := Copy(extra, 0, len2);
end;

procedure AppendPath(var paths: TPathsD; const extra: TPathsD); overload;
var
  i, len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1+i] := Copy(extra[i], 0, length(extra[i]));
end;


//------------------------------------------------------------------------------
//
//      Grow internals
//
//------------------------------------------------------------------------------

function CalcRoundingStepsOld(radius: double): double;
begin
  //the results of this function have been derived empirically
  //and may need further adjustment
  if radius < 0.55 then result := 4
  else result := Pi * Sqrt(radius);
end;

function CalcRoundingSteps(radius: double): double;
const
  MINSTEPS = 6;
  SQUAREDMINSTEPS = Sqr(MINSTEPS);
var
  Temp: TFloat;
begin
  Temp := Abs(Radius) * Sqr(TWOPI);
  if Temp < SQUAREDMINSTEPS then
    Result := 6
  else
    Result := Round(Sqrt(Temp));
end;

//------------------------------------------------------------------------------
//
//      Grow
//
//------------------------------------------------------------------------------
function Grow(const path, normals: TPathD; delta: double;
  joinStyle: TJoinStyle; miterLim: double; isOpen: Boolean): TPathD;
var
  resCnt, resCap    : integer;
  norms             : TPathD;
  stepsPerRadian    : double;
  stepSin, stepCos  : double;
  asin, acos        : double;

  procedure AddPoint(const pt: TPointD);
  begin
    if resCnt >= resCap then
    begin
      inc(resCap, 64);
      setLength(result, resCap);
    end;
    result[resCnt] := pt;
    inc(resCnt);
  end;

  procedure DoMiter(j, k: Integer; cosA: Double);
  var
    q: Double;
  begin
    q := delta / (cosA +1);
    AddPoint(PointD(
      path[j].X + (norms[k].X + norms[j].X) *q,
      path[j].Y + (norms[k].Y + norms[j].Y) *q));
  end;

  procedure DoBevel(j, k: Integer);
  var
    absDelta: double;
  begin
    if k = j then
    begin
      absDelta := Abs(delta);
      AddPoint(PointD(
        path[j].x - absDelta * norms[j].x,
        path[j].y - absDelta * norms[j].y));
      AddPoint(PointD(
        path[j].x + absDelta * norms[j].x,
        path[j].y + absDelta * norms[j].y));
    end else
    begin
      AddPoint(PointD(
        path[j].x + delta * norms[k].x,
        path[j].y + delta * norms[k].y));
      AddPoint(PointD(
        path[j].x + delta * norms[j].x,
        path[j].y + delta * norms[j].y));
    end;
  end;

  procedure DoRound(j, k: Integer);
  var
    i, steps: Integer;
    pt: TPointD;
    dx, dy, oldDx: double;
    angle: double;
  begin
    // nb: angles may be negative but this will always be a convex join
    pt := path[j];
    if j = k then
    begin
      dx := -norms[k].X * delta;
      dy := -norms[k].Y * delta;
    end else
    begin
      dx := norms[k].X * delta;
      dy := norms[k].Y * delta;
    end;
    AddPoint(PointD(pt.X + dx, pt.Y + dy));

    angle := ArcTan2(asin, acos);
    steps := Ceil(stepsPerRadian * abs(angle));

    for i := 2 to steps do
    begin
      oldDx := dx;
      dx := oldDx * stepCos - stepSin * dy;
      dy := oldDx * stepSin + stepCos * dy;
      AddPoint(PointD(pt.X + dx, pt.Y + dy));
    end;
    AddPoint(PointD(
      pt.X + norms[j].X * delta,
      pt.Y + norms[j].Y * delta));
  end;

var
  j, k      : cardinal;
  len       : cardinal;
  steps     : double;
  highI     : cardinal;
  iLo,iHi   : cardinal;
  absDelta  : double;
begin
  Result := nil;
  if not Assigned(path) then exit;
  len := Length(path);
  if not isOpen then
    while (len > 2) and
      PointsNearEqual(path[len -1], path[0], 0.001) do
        dec(len);
  if len < 2 then Exit;

  absDelta := Abs(delta);
  if absDelta < MinStrokeWidth/2 then
  begin
    if delta < 0 then
      delta := -MinStrokeWidth/2 else
      delta := MinStrokeWidth/2;
  end;
  if absDelta < 1 then
    joinStyle := jsSquare
  else if joinStyle = jsAuto then
  begin
    if delta < AutoWidthThreshold / 2 then
      joinStyle := jsSquare else
      joinStyle := jsRound;
  end;

  if assigned(normals) then
    norms := normals else
    norms := GetNormals(path);

  highI := len -1;

  stepsPerRadian := 0;
  if joinStyle = jsRound then
  begin
    steps := CalcRoundingSteps(delta);
//    // avoid excessive precision // todo - recheck if needed
//		if (steps > absDelta * Pi) then
//			steps := absDelta * Pi;
    stepSin := sin(TwoPi/steps);
    stepCos := cos(TwoPi/steps);
		if (delta < 0) then stepSin := -stepSin;
    stepsPerRadian := steps / TwoPi;
  end;

  if miterLim <= 0 then miterLim := DefaultMiterLimit
  else if miterLim < 2 then miterLim := 2;
  miterLim := 2 /(sqr(miterLim));

  resCnt := 0;
  resCap := 0;

  if isOpen then
  begin
    iLo := 1; iHi := highI -1;
    k := 0;
    AddPoint(PointD(
     path[0].X + norms[0].X * delta,
     path[0].Y + norms[0].Y * delta));
  end else
  begin
    iLo := 0; iHi := highI;
    k := highI;
  end;

  for j := iLo to iHi do
  begin

    if PointsNearEqual(path[j], path[k], 0.01) then
    begin
       k := j; // todo - check if needed
       Continue;
    end;

    asin := CrossProduct(norms[k], norms[j]);
    if (asin > 1.0) then asin := 1.0
    else if (asin < -1.0) then asin := -1.0;
    acos := DotProduct(norms[k], norms[j]);

    if (acos > -0.999) and (asin * delta < 0) then
    begin
      // is concave
      AddPoint(PointD(
        path[j].X + norms[k].X * delta, path[j].Y + norms[k].Y * delta));
      AddPoint(path[j]);
      AddPoint(PointD(
        path[j].X + norms[j].X * delta, path[j].Y + norms[j].Y * delta));
    end
    else if (acos > 0.999) and (joinStyle <> jsRound) then
    begin
      // almost straight - less than 2.5 degree, so miter
      DoMiter(j, k, acos);
    end
    else if (joinStyle = jsMiter) then
    begin
      if (1 + acos > miterLim) then
        DoMiter(j, k, acos) else
        DoBevel(j, k);
    end
    else if (joinStyle = jsRound) then
    begin
      DoRound(j, k);
    end
    else
      DoBevel(j, k);
    k := j;
  end;

  if isOpen then
    AddPoint(PointD(
     path[highI].X + norms[highI].X * delta,  //todo - check this !!!
     path[highI].Y + norms[highI].Y * delta));

  SetLength(Result, resCnt);
end;

//------------------------------------------------------------------------------
//
//      RoughOutline internals
//
//------------------------------------------------------------------------------
function GetAvgUnitVector(const vec1, vec2: TPointD): TPointD; inline;
begin
  Result := NormalizeVector(PointD(vec1.X + vec2.X, vec1.Y + vec2.Y));
end;


//------------------------------------------------------------------------------
//
//      RoughOutline
//
//------------------------------------------------------------------------------
function GrowOpenLine(const line: TPathD; delta: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLim: double): TPathD;
var
  len               : integer;
  resCnt, resCap    : integer;
  asin, acos        : double;
  stepSin, stepCos  : double;
  stepsPerRadian    : double;
  path, norms       : TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if resCnt >= resCap then
    begin
      inc(resCap, 64);
      setLength(result, resCap);
    end;
    result[resCnt] := pt;
    inc(resCnt);
  end;

  procedure DoMiter(j, k: Integer; cosA: Double);
  var
    q: Double;
  begin
    q := delta / (cosA +1);
    AddPoint(PointD(
      path[j].X + (norms[k].X + norms[j].X) *q,
      path[j].Y + (norms[k].Y + norms[j].Y) *q));
  end;

  procedure DoBevel(j, k: Integer);
  var
    absDelta: double;
  begin
    if k = j then
    begin
      absDelta := Abs(delta);
      AddPoint(PointD(
        path[j].x - absDelta * norms[j].x,
        path[j].y - absDelta * norms[j].y));
      AddPoint(PointD(
        path[j].x + absDelta * norms[j].x,
        path[j].y + absDelta * norms[j].y));
    end else
    begin
      AddPoint(PointD(
        path[j].x + delta * norms[k].x,
        path[j].y + delta * norms[k].y));
      AddPoint(PointD(
        path[j].x + delta * norms[j].x,
        path[j].y + delta * norms[j].y));
    end;
  end;

  procedure DoSquare(j, k: Integer);
  var
    vec, ptQ, ptR, ptS, ptT, ptU, ip: TPointD;
    absDelta: double;
  begin
    if k = j then
    begin
      vec.X := norms[j].Y;     //squaring a line end
      vec.Y := -norms[j].X;
    end else
    begin
      // using the reciprocal of unit normals (as unit vectors)
      // get the average unit vector ...
      vec := GetAvgUnitVector(
        PointD(-norms[k].Y, norms[k].X),
        PointD(norms[j].Y, -norms[j].X));
    end;

    absDelta := Abs(delta);
    ptQ := PointD(path[j].X + absDelta * vec.X, path[j].Y + absDelta * vec.Y);

    ptR := PointD(ptQ.X + delta * vec.Y, ptQ.Y + delta * -vec.X);
    ptS := ReflectPoint(ptR, ptQ);

    // get 2 vertices along one edge offset
    ptT := PointD(
      path[k].X + norms[k].X * delta,
      path[k].Y + norms[k].Y * delta);

    if (j = k) then
    begin
      ptU.X := ptT.X + vec.X * delta;
      ptU.Y := ptT.Y + vec.Y * delta;
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ReflectPoint(ip, ptQ));
      AddPoint(ip);
    end else
    begin
      ptU := PointD(
        path[j].X + norms[k].X * delta,
        path[j].Y + norms[k].Y * delta);
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ip);
      AddPoint(ReflectPoint(ip, ptQ));
    end;
  end;

  procedure DoRound(j, k: Integer);
  var
    i, steps: Integer;
    pt: TPointD;
    dx, dy, oldDx: double;
    angle: double;
  begin
    // nb: angles may be negative but this will always be a convex join
    pt := path[j];
    if j = k then
    begin
      dx := -norms[k].X * delta;
      dy := -norms[k].Y * delta;
      angle := PI;
    end else
    begin
      dx := norms[k].X * delta;
      dy := norms[k].Y * delta;
      angle := ArcTan2(asin, acos);
    end;
    AddPoint(PointD(pt.X + dx, pt.Y + dy));

    steps := Ceil(stepsPerRadian * abs(angle));
    for i := 2 to steps do
    begin
      oldDx := dx;
      dx := oldDx * stepCos - stepSin * dy;
      dy := oldDx * stepSin + stepCos * dy;
      AddPoint(PointD(pt.X + dx, pt.Y + dy));
    end;
    AddPoint(PointD(
      pt.X + norms[j].X * delta,
      pt.Y + norms[j].Y * delta));
  end;

  procedure DoPoint(j: Cardinal; var k: Cardinal);
  begin
    asin := CrossProduct(norms[k], norms[j]);
    if (asin > 1.0) then asin := 1.0
    else if (asin < -1.0) then asin := -1.0;
    acos := DotProduct(norms[k], norms[j]);

    if (acos > -0.999) and (asin * delta < 0) then
    begin
      // is concave
      AddPoint(PointD(
        path[j].X + norms[k].X * delta, path[j].Y + norms[k].Y * delta));
      AddPoint(path[j]);
      AddPoint(PointD(
        path[j].X + norms[j].X * delta, path[j].Y + norms[j].Y * delta));
    end
    else if (acos > 0.999) and (joinStyle <> jsRound) then
      // almost straight - less than 2.5 degree, so miter
      DoMiter(j, k, acos)
    else if (joinStyle = jsMiter) then
    begin
      if (1 + acos > miterLim) then
        DoMiter(j, k, acos) else
        DoBevel(j, k);
    end
    else if (joinStyle = jsRound) then
      DoRound(j, k)
    else
      DoBevel(j, k);
    k := j;
  end;

var
  highJ : cardinal;
  j, k  : cardinal;
  steps : double;
begin
  Result := nil;
  path := StripNearDuplicates(line, 0.5, false);
  len := length(path);
  if len = 0 then Exit;
  if delta < MinStrokeWidth then
    delta := MinStrokeWidth;
  delta := delta * 0.5;

  if len = 1 then
  begin
    with path[0] do
      result := Ellipse(RectD(x-delta, y-delta, x+delta, y+delta));
    Exit;
  end;

  Assert(endStyle <> esClosed);

  //with very narrow lines, don't get fancy with joins and line ends
  if (delta <= 1) then
  begin
    joinStyle := jsSquare;
    if endStyle = esRound then endStyle := esSquare;
  end
  else if joinStyle = jsAuto then
  begin
    if (endStyle = esRound) and
      (delta >= AutoWidthThreshold) then
      joinStyle := jsRound
    else
      joinStyle := jsSquare;
  end;

  stepsPerRadian := 0;
  if (joinStyle = jsRound) or (endStyle = esRound) then
  begin
    steps := CalcRoundingSteps(delta);
//		if (steps > absDelta * Pi) then // todo - recheck if needed
//			steps := absDelta * Pi;
    stepSin := sin(TwoPi/steps);
    stepCos := cos(TwoPi/steps);
		if (delta < 0) then stepSin := -stepSin;
    stepsPerRadian := steps / TwoPi;
  end;

  if miterLim <= 0 then miterLim := DefaultMiterLimit
  else if miterLim < 2 then miterLim := 2;
  miterLim := 2 /(sqr(miterLim));

  norms := GetNormals(path);
  resCnt := 0; resCap := 0;

  case endStyle of
    esButt: DoBevel(0,0);
    esRound: DoRound(0,0);
    else DoSquare(0, 0);
  end;

  // offset the left side going **forward**
  k := 0;
  highJ := len -1;
  for j := 1 to highJ -1 do DoPoint(j,k);

  // reverse the normals ...
  for j := highJ downto 1 do
  begin
    norms[j].X := -norms[j-1].X;
    norms[j].Y := -norms[j-1].Y;
  end;
  norms[0] := norms[len -1];

  case endStyle of
    esButt: DoBevel(highJ,highJ);
    esRound: DoRound(highJ,highJ);
    else DoSquare(highJ,highJ);
  end;

  // offset the left side going **backward**
  k := highJ;
  for j := highJ -1 downto 1 do
    DoPoint(j, k);

  SetLength(Result, resCnt);
end;

//------------------------------------------------------------------------------

function GrowClosedLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; miterLimOrRndScale: double): TPathsD;
var
  norms: TPathD;
  rec: TRectD;
  skipHole: Boolean;
begin
  rec := GetBoundsD(line);
  skipHole := (rec.Width <= width) or (rec.Height <= width);
  if skipHole then
  begin
    SetLength(Result, 1);
    norms := GetNormals(line);
    Result[0] := Grow(line, norms, width/2, joinStyle, miterLimOrRndScale, false);
  end else
  begin
    SetLength(Result, 2);
    norms := GetNormals(line);
    Result[0] := Grow(line, norms, width/2, joinStyle, miterLimOrRndScale, false);
    Result[1] := ReversePath(
      Grow(line, norms, -width/2, joinStyle, miterLimOrRndScale, false));
  end;
end;

//------------------------------------------------------------------------------

function RoughOutline(const line: TPathD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimOrRndScale: double): TPathsD; overload;
begin
  if not assigned(line) then
    Result := nil
  else if endStyle = esClosed then
    result := GrowClosedLine(line,
      lineWidth, joinStyle, miterLimOrRndScale)
  else
  begin
    SetLength(Result,1);
    result[0] := GrowOpenLine(line, lineWidth,
      joinStyle, endStyle, miterLimOrRndScale);
  end;
end;

//------------------------------------------------------------------------------

function RoughOutline(const lines: TPathsD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimOrRndScale: double): TPathsD; overload;
var
  i: integer;
  lwDiv2: double;
  p: TPathD;
begin
  result := nil;
  if not assigned(lines) then exit;
  if joinStyle = jsAuto then
  begin
    if endStyle in [esPolygon, esRound] then
      joinStyle := jsRound else
      joinStyle := jsSquare;
  end;
  if endStyle = esPolygon then
  begin
    for i := 0 to high(lines) do
    begin
      if Length(lines[i]) = 1 then
      begin
        lwDiv2 := lineWidth/2;
        with lines[i][0] do
          AppendPath(Result,
            Ellipse(RectD(x-lwDiv2, y-lwDiv2, x+lwDiv2, y+lwDiv2)));
      end else
      begin
        p := StripNearDuplicates(lines[i], 0.25, true);
        if Length(p) = 2 then AppendToPath(p, p[0]);
        AppendPath(Result,
          GrowClosedLine(p, lineWidth, joinStyle, miterLimOrRndScale));
      end;
    end;
  end
  else
    for i := 0 to high(lines) do
      AppendPath(Result, GrowOpenLine(lines[i], lineWidth,
        joinStyle, endStyle, miterLimOrRndScale));
end;


//------------------------------------------------------------------------------
//
//      PolyLineBuilderAngus
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Grow
//------------------------------------------------------------------------------
class function PolyLineBuilderAngus.Grow(const Points, Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint;
begin
  Result := GR32_VectorUtils.Angus.Grow(Points, Normals, Delta * GrowScale, JoinStyleMap[JoinStyle], MiterLimit, not Closed);
end;

//------------------------------------------------------------------------------
// BuildPoly*line
//------------------------------------------------------------------------------
class function PolyLineBuilderAngus.BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; EndStyle: GR32_Polygons.TEndStyle; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  Res: TArrayOfArrayOfFloatPoint;
begin
  Res := RoughOutline(Points, StrokeWidth * GrowScale, JoinStyleMap[JoinStyle], EndStyleMap[EndStyle], MiterLimit);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;

class function PolyLineBuilderAngus.BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; EndStyle: GR32_Polygons.TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  OutlineEndStyle: TEndStyle;
begin
  if (Closed) then
    OutlineEndStyle := esPolygon
  else
    OutlineEndStyle := EndStyleMap[EndStyle];

  Result := RoughOutline(Points, StrokeWidth * GrowScale, JoinStyleMap[JoinStyle], OutlineEndStyle, MiterLimit);
end;

//------------------------------------------------------------------------------

end.
