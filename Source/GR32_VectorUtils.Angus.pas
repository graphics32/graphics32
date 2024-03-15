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
  GR32_VectorUtils;

//------------------------------------------------------------------------------
//
//      Grow and BuildPoly*line replacements adapted from Image32
//
//------------------------------------------------------------------------------


type
  BuildPolylineAngus = class(TBuildPolyline)
  protected
    // Float
    class function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;
  public
    // Float
    class function BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;
    class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload; override;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,

  GR32_Math,
  GR32_Geometry;

const
  GrowScale = 1.0;

//------------------------------------------------------------------------------
//
//      Type and value mappings
//
//------------------------------------------------------------------------------
type
  TJoinStyle  = (jsAuto, jsSquare, jsMiter, jsRound);
  TEndStyle   = (esPolygon = 0, esClosed = 0, esButt, esSquare, esRound);

const
  // Note: We map Graphics32's jsBevel to jsSquare
  JoinStyleMap: array[GR32_Polygons.TJoinStyle] of TJoinStyle = (jsMiter, jsSquare, jsRound, jsSquare);
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
type
  TGrowRec = record
    StepsPerRad : double;
    StepSin     : double;
    StepCos     : double;
    Radius      : double;
    aSin        : double;
    aCos        : double;
    pt          : TPointD;
    norm1       : TPointD;
    norm2       : TPointD;
  end;

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
function PointD(const X, Y: Double): TPointD; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := FloatPoint(X, Y);
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

function PointsNearEqual(const pt1, pt2: TPointD; distSqrd: double): Boolean;
begin
  Result := SamePoint(pt1, pt2, distSqrd);
end;

function DotProduct(const vector1, vector2: TPointD): double;
begin
  Result := Dot(vector1, vector2);
end;

//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function NormalizeVector(const vec: TPointD): TPointD;
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

function ReflectPoint(const pt, pivot: TPointD): TPointD;
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

function ApplyNormal(const pt, norm: TPointD; delta: double): TPointD; {$IFDEF INLINE} inline; {$ENDIF}
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
function GetParallelOffests(const path, norms: TPathD; // TODO : Typo
  delta: double): TPathD;
var
  i, highI, len: integer;
begin
  len := Length(path);
  highI := len -1;
  SetLength(Result, len *2);
  Result[0]  := ApplyNormal(path[0], norms[0], delta);
  for i := 1 to highI do
  begin
    Result[i*2-1] := ApplyNormal(path[i], norms[i-1], delta);
    Result[i*2]   := ApplyNormal(path[i], norms[i], delta);
  end;
  Result[highI*2+1] := ApplyNormal(path[0], norms[highI], delta);
end;

//------------------------------------------------------------------------------

function CalcRoundingSteps(radius: double): double;
begin
  //the results of this function have been derived empirically
  //and may need further adjustment
  if radius < 0.55 then result := 4
  else result := Pi * Sqrt(radius);
end;

//------------------------------------------------------------------------------

procedure GetSinCos(angle: double; out sinA, cosA: double);
{$IFDEF INLINE} inline; {$ENDIF}
{$IFNDEF FPC}
var s, c: extended;
{$ENDIF}
begin
{$IFDEF FPC}
  Math.SinCos(angle, sinA, cosA);
{$ELSE}
  Math.SinCos(angle, s, c);
  sinA := s; cosA := c;
{$ENDIF}
end;

//------------------------------------------------------------------------------

function DoRound(const growRec: TGrowRec): TPathD;
var
  i, steps: Integer;
  a: Double;
  pt2: TPointD;
begin
  with growRec do
  begin
    a := ArcTan2(aSin, aCos);
    steps := Round(StepsPerRad * Abs(a));
    SetLength(Result, steps + 2);
    pt2 := PointD(norm1.x * Radius, norm1.y * Radius);
    Result[0] := PointD(pt.x + pt2.x, pt.y + pt2.y);
    for i := 1 to steps do
    begin
      pt2 := PointD(pt2.X * StepCos - StepSin * pt2.Y,
        pt2.X * StepSin + pt2.Y * StepCos);
      Result[i] := PointD(pt.X + pt2.X, pt.Y + pt2.Y);
    end;
    pt2 := PointD(norm2.x * Radius, norm2.y * Radius);
    Result[steps+1] := PointD(pt.x + pt2.x, pt.y + pt2.y);
  end;
end;

//------------------------------------------------------------------------------
//
//      Grow
//
//------------------------------------------------------------------------------
function Grow(const path, normals: TPathD; delta: double; joinStyle: TJoinStyle; miterLim: double; isOpen: Boolean = False): TPathD;
var
  resCnt, resCap: integer;
  norms : TPathD;
  parallelOffsets : TPathD;

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

  procedure DoMiter(const growRec: TGrowRec);
  var
    a: double;
  begin
    with growRec do
    begin
      a := delta / (1 + aCos); //see offset_triginometry4.svg
      AddPoint(PointD(pt.X + (norm2.X + norm1.X) * a,
            pt.Y + (norm2.Y + norm1.Y) * a));
    end;
  end;

  procedure DoSquare(const growRec: TGrowRec; const po1, po2: TPointD);
  var
    pt1, pt2: TPointD;
    ip, ptQ : TPointD;
    vec     : TPointD;
  begin
    with growRec do
    begin
      // using the reciprocal of unit normals (as unit vectors)
      // get the average unit vector ...
      //vec := GetAvgUnitVector(PointD(-norm1.Y, norm1.X),PointD(norm2.Y,-norm2.X));
      vec := NormalizeVector(PointD(norm2.Y - norm1.Y, norm1.X - norm2.X));
      // now offset the original vertex delta units along unit vector
      ptQ := OffsetPoint(pt, delta * vec.X, delta * vec.Y);

      // get perpendicular vertices
      pt1 := OffsetPoint(ptQ, delta * vec.Y, delta * -vec.X);
      pt2 := OffsetPoint(ptQ, delta * -vec.Y, delta * vec.X);
      // using 2 vertices along one edge offset (po1 & po2)
      IntersectPoint(pt1,pt2,po1,po2, ip);
      AddPoint(ip);
      //get the second intersect point through reflecion
      ip := ReflectPoint(ip, ptQ);
      AddPoint(ip);
    end;
  end;

  procedure AppendPath(const path: TPathD);
  var
    len: integer;
  begin
    len := Length(path);
    if resCnt + len > resCap then
    begin
      inc(resCap, len);
      setLength(result, resCap);
    end;
    Move(path[0], result[resCnt], len * SizeOf(TPointD));
    inc(resCnt, len);
  end;

var
  i       : cardinal;
  prevI   : cardinal;
  len     : cardinal;
  highI   : cardinal;
  iLo,iHi : cardinal;
  growRec   : TGrowRec;
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
  parallelOffsets := GetParallelOffests(path, norms, delta);

  if joinStyle = jsRound then
  begin
    growRec.Radius := delta;
    growRec.StepsPerRad := CalcRoundingSteps(growRec.Radius)/(Pi *2);
    if delta < 0 then
      GetSinCos(-1/growRec.StepsPerRad, growRec.StepSin, growRec.StepCos) else
      GetSinCos(1/growRec.StepsPerRad, growRec.StepSin, growRec.StepCos);
  end else
  begin
    if miterLim <= 0 then miterLim := DefaultMiterLimit
    else if miterLim < 2 then miterLim := 2;
    miterLim := 2 /(sqr(miterLim));
    growRec.StepsPerRad := 0; //stop compiler warning.
  end;

  resCnt := 0; resCap := 0;

  if isOpen then
  begin
    iLo := 1; iHi := highI -1;
    prevI := 0;
    AddPoint(parallelOffsets[0]);
  end else
  begin
    iLo := 0; iHi := highI;
    prevI := highI;
  end;

  for i := iLo to iHi do
  begin

    if PointsNearEqual(path[i], path[prevI], 0.01) then
    begin
       prevI := i;
       Continue;
    end;

    growRec.aSin := CrossProduct(norms[prevI], norms[i]);
    growRec.aCos := DotProduct(norms[prevI], norms[i]);
    if (growRec.aSin > 1.0) then growRec.aSin := 1.0
    else if (growRec.aSin < -1.0) then growRec.aSin := -1.0;

    growRec.pt := path[i];
    growRec.norm1 := norms[prevI];
    growRec.norm2 := norms[i];

    if (growRec.aCos > 0.99) then // almost straight - less than 8 degrees
    begin
      AddPoint(parallelOffsets[prevI*2+1]);
      if (growRec.aCos < 0.9998) then // greater than 1 degree
        AddPoint(parallelOffsets[i*2]);
    end
    else if (growRec.aCos > -0.99) and (growRec.aSin * delta < 0) then
    begin //ie is concave
      AddPoint(parallelOffsets[prevI*2+1]);
      AddPoint(path[i]);
      AddPoint(parallelOffsets[i*2]);
    end
    else if (joinStyle = jsRound) then
    begin
      AppendPath(DoRound(growRec));
    end
    else if (joinStyle = jsMiter) then // nb: miterLim <= 2
    begin
      if (1 + growRec.aCos > miterLim) then //within miter range
        DoMiter(growRec) else
        DoSquare(growRec,
          parallelOffsets[prevI*2], parallelOffsets[prevI*2 +1]);
    end
    // don't bother squaring angles that deviate < ~20 deg. because squaring
    // will be indistinguishable from mitering and just be a lot slower
    else if (growRec.aCos > 0.9) then
      DoMiter(growRec)
    else
      DoSquare(growRec, parallelOffsets[prevI*2], parallelOffsets[prevI*2 +1]);

    prevI := i;
  end;
  if isOpen then AddPoint(parallelOffsets[highI*2-1]);
  SetLength(Result, resCnt);
end;


//------------------------------------------------------------------------------
//
//      Outline internals
//
//------------------------------------------------------------------------------
function ReverseNormals(const norms: TPathD): TPathD;
var
  i, highI: integer;
begin
  highI := high(norms);
  setLength(result, highI +1);
  for i := 1 to highI  do
  begin
    result[i -1].X := -norms[highI -i].X;
    result[i -1].Y := -norms[highI -i].Y;
  end;
  result[highI].X := -norms[highI].X;
  result[highI].Y := -norms[highI].Y;
end;

procedure AdjustPoint(var pt: TPointD; const referencePt: TPointD; delta: double);
var
  vec: TPointD;
begin
  //Positive delta moves pt away from referencePt, and
  //negative delta moves pt toward referencePt.
  vec := GetUnitVector(referencePt, pt);
  pt.X := pt.X + (vec.X * delta);
  pt.Y := pt.Y + (vec.Y * delta);
end;

//------------------------------------------------------------------------------
//
//      Outline
//
//------------------------------------------------------------------------------
function GrowOpenLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimOrRndScale: double): TPathD;
var
  len, x,y: integer;
  segLen, halfWidth, rndScale: double;
  normals, line2, lineL, lineR, arc: TPathD;
  invNorm: TPointD;
  growRec: TGrowRec;
begin
  Result := nil;
  line2 := StripNearDuplicates(line, 0.5, false);
  len := length(line2);
  if len = 0 then Exit;
  if width < MinStrokeWidth then
    width := MinStrokeWidth;
  halfWidth := width * 0.5;
  if len = 1 then
  begin
    x := Round(line2[0].X);
    y := Round(line2[0].Y);
    SetLength(result, 1);
    result := Ellipse(FloatRect(x -halfWidth, y -halfWidth,
      x +halfWidth, y +halfWidth));
    Exit;
  end;

  if endStyle = esPolygon then
  begin
    case joinStyle of
      jsSquare, jsMiter : endStyle := esSquare;
      else                endStyle := esRound;
    end;
  end;

  //with very narrow lines, don't get fancy with joins and line ends
  if (width <= 2) then
  begin
    joinStyle := jsSquare;
    if endStyle = esRound then endStyle := esSquare;
  end
  else if joinStyle = jsAuto then
  begin
    if (endStyle = esRound) and
      (width >= AutoWidthThreshold) then
      joinStyle := jsRound
    else
      joinStyle := jsSquare;
  end;

  normals := GetNormals(line2);
  if endStyle = esRound then
  begin
    //grow the line's left side of the line => line1
    lineL := Grow(line2, normals,
      halfWidth, joinStyle, miterLimOrRndScale, true);
    //build the rounding at the start => result
    invNorm.X := -normals[0].X;
    invNorm.Y := -normals[0].Y;
    //get the rounding parameters
    if miterLimOrRndScale > 0 then
      rndScale := miterLimOrRndScale else
      rndScale := 1;
    growRec.StepsPerRad :=
      CalcRoundingSteps(halfWidth * rndScale)/(Pi*2);
    GetSinCos(1/growRec.StepsPerRad, growRec.StepSin, growRec.StepCos);
    growRec.aSin := invNorm.X * normals[0].Y - invNorm.Y * normals[0].X;
    growRec.aCos := invNorm.X * normals[0].X + invNorm.Y * normals[0].Y;
    growRec.Radius := halfWidth;
    growRec.pt := line2[0];
    growRec.norm1 := invNorm;
    growRec.norm2 := normals[0];
    Result := DoRound(growRec);
    //join line1 into result
    AppendPath(Result, lineL);
    //reverse the normals and build the end arc => arc
    normals := ReverseNormals(normals);
    invNorm.X := -normals[0].X; invNorm.Y := -normals[0].Y;
    growRec.aSin := invNorm.X * normals[0].Y - invNorm.Y * normals[0].X;
    growRec.aCos := invNorm.X * normals[0].X + invNorm.Y * normals[0].Y;
    growRec.pt := line2[High(line2)];
    growRec.norm1 := invNorm;
    growRec.norm2 := normals[0];
    arc := DoRound(growRec);
    //grow the line's right side of the line
    lineR := Grow(ReversePolygon(line2), normals,
      halfWidth, joinStyle, rndScale, true);
    //join arc and line2 into result
    AppendPath(Result, arc);
    AppendPath(Result, lineR);
  end else
  begin
    lineL := Copy(line2, 0, len);
    if endStyle = esSquare then
    begin
      // esSquare => extends both line ends by 1/2 lineWidth
      AdjustPoint(lineL[0], lineL[1], width * 0.5);
      AdjustPoint(lineL[len-1], lineL[len-2], width * 0.5);
    end else
    begin
      //esButt -> extend only very short end segments
      segLen := Distance(lineL[0], lineL[1]);
      if segLen < width * 0.5 then
        AdjustPoint(lineL[0], lineL[1], width * 0.5 - segLen);
      segLen := Distance(lineL[len-1], lineL[len-2]);
      if segLen < width * 0.5 then
        AdjustPoint(lineL[len-1], lineL[len-2], width * 0.5 - segLen);
    end;
    //first grow the left side of the line => Result
    Result := Grow(lineL, normals,
      halfWidth, joinStyle, miterLimOrRndScale, true);
    //reverse normals and path and grow the right side => lineR
    normals := ReverseNormals(normals);
    lineR := Grow(ReversePolygon(lineL), normals,
      halfWidth, joinStyle, miterLimOrRndScale, true);
    //join both sides
    AppendPath(Result, lineR);
  end;
end;
//------------------------------------------------------------------------------

function GrowClosedLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; miterLimOrRndScale: double): TPathsD;
var
  line2, norms: TPathD;
  rec: TRectD;
  skipHole: Boolean;
begin
  line2 := StripNearDuplicates(line, 0.5, true);
  rec := PolygonBounds(line2);
  skipHole := (rec.Width <= width) or (rec.Height <= width);
  if skipHole then
  begin
    SetLength(Result, 1);
    norms := GetNormals(line2);
    Result[0] := Grow(line2, norms, width/2, joinStyle, miterLimOrRndScale);
  end else
  begin
    SetLength(Result, 2);
    norms := GetNormals(line2);
    Result[0] := Grow(line2, norms, width/2, joinStyle, miterLimOrRndScale);
    line2 := ReversePolygon(line2);
    norms := ReverseNormals(norms);
    Result[1] := Grow(line2, norms, width/2, joinStyle, miterLimOrRndScale);
  end;
end;
//------------------------------------------------------------------------------

function Outline(const line: TPathD; lineWidth: double;
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

function Outline(const lines: TPathsD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimOrRndScale: double): TPathsD; overload;
var
  i: integer;
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
      if Length(lines[i]) > 2 then
        AppendPath(Result, GrowClosedLine(lines[i],
          lineWidth, joinStyle, miterLimOrRndScale))
      else
        AppendPath(Result, GrowOpenLine(lines[i], lineWidth,
          joinStyle, endStyle, miterLimOrRndScale));
  end
  else
    for i := 0 to high(lines) do
      AppendPath(Result, GrowOpenLine(lines[i], lineWidth,
        joinStyle, endStyle, miterLimOrRndScale));
end;

//------------------------------------------------------------------------------
//
//      BuildPolylineAngus
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Grow
//------------------------------------------------------------------------------
class function BuildPolylineAngus.Grow(const Points, Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint;
begin
  Result := GR32_VectorUtils.Angus.Grow(Points, Normals, Delta * GrowScale, JoinStyleMap[JoinStyle], MiterLimit, not Closed);
end;

//------------------------------------------------------------------------------
// BuildPoly*line
//------------------------------------------------------------------------------
class function BuildPolylineAngus.BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; EndStyle: GR32_Polygons.TEndStyle; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  Res: TArrayOfArrayOfFloatPoint;
begin
  Res := Outline(Points, StrokeWidth * GrowScale, JoinStyleMap[JoinStyle], EndStyleMap[EndStyle], MiterLimit);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;

class function BuildPolylineAngus.BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: GR32_Polygons.TJoinStyle; EndStyle: GR32_Polygons.TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  OutlineEndStyle: TEndStyle;
begin
  if (Closed) then
    OutlineEndStyle := esPolygon
  else
    OutlineEndStyle := EndStyleMap[EndStyle];

  Result := Outline(Points, StrokeWidth * GrowScale, JoinStyleMap[JoinStyle], OutlineEndStyle, MiterLimit);
end;

//------------------------------------------------------------------------------

end.
