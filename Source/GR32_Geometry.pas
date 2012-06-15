unit GR32_Geometry;

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
 * The Original Code is Additional Math Routines for Graphics32
 *
 * The Initial Developers of the Original Code are
 * Mattias Andersson <mattias@centaurix.com>
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developers are Copyright (C) 2005-2012
 * the Initial Developers. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Math, GR32, GR32_Math;

type
  TLinePos = (lpStart, lpEnd, lpBoth, lpNeither);

function Average(const V1, V2: TFloatPoint): TFloatPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Dot(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function GetPointAtAngleFromPoint(const pt: TFloatPoint; const dist, radians: single): TFloatPoint; overload;
function GetAngleOfPt2FromPt1(const pt1, pt2: TFloatPoint): single; overload;
function GetUnitNormal(const pt1, pt2: TFloatPoint): TFloatPoint; overload;
function GetUnitVector(const pt1, pt2: TFloatPoint): TFloatPoint; overload;
function OffsetPoint(const pt: TFloatPoint; dx, dy: TFloat): TFloatPoint; overload;
function Shorten(const pts: TArrayOfFloatPoint;
  delta: TFloat; linePos: TLinePos): TArrayOfFloatPoint; overload;
function PointInPolygon(const pt: TFloatPoint; const Pts: TArrayOfFloatPoint): boolean; overload;
function SegmentIntersect(const p1, p2, p3, p4: TFloatPoint;
  out IntersectPoint: TFloatPoint): boolean; overload;

// Fixed Overloads
function Average(const V1, V2: TFixedPoint): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Dot(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function GetPointAtAngleFromPoint(const pt: TFixedPoint; const dist, radians: single): TFixedPoint; overload;
function GetAngleOfPt2FromPt1(pt1, pt2: TFixedPoint): single; overload;
function GetUnitVector(const pt1, pt2: TFixedPoint): TFloatPoint; overload;
function GetUnitNormal(const pt1, pt2: TFixedPoint): TFloatPoint; overload;
function OffsetPoint(const pt: TFixedPoint; dx, dy: TFloat): TFixedPoint; overload;
function Shorten(const pts: TArrayOfFixedPoint;
  delta: TFloat; linePos: TLinePos): TArrayOfFixedPoint; overload;
function PointInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean; overload;
function SegmentIntersect(const p1, p2, p3, p4: TFixedPoint;
  out IntersectPoint: TFixedPoint): boolean; overload;

const
  rad01 = pi / 180;
  rad30 = pi / 6;
  rad45 = pi / 4;
  rad60 = pi / 3;
  rad90 = pi / 2;
  rad180 = pi;
  rad270 = rad90 * 3;
  rad360 = rad180 * 2;
  DegToRad = pi/180;
  RadToDeg = 180/pi;

implementation

function Average(const V1, V2: TFloatPoint): TFloatPoint;
begin
  Result.X := (V1.X + V2.X) * 0.5;
  Result.Y := (V1.Y + V2.Y) * 0.5;
end;

function Dot(const V1, V2: TFloatPoint): TFloat;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y;
end;

function Distance(const V1, V2: TFloatPoint): TFloat;
begin
  Result := Hypot(V2.X - V1.X, V2.Y - V1.Y);
end;

function SqrDistance(const V1, V2: TFloatPoint): TFloat;
begin
  Result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y);
end;

function GetPointAtAngleFromPoint(const pt: TFloatPoint;
  const dist, radians: TFloat): TFloatPoint; overload;
var
  sinAng, cosAng: TFloat;
begin
  GR32_Math.SinCos(radians, sinAng, cosAng);
  result.X := dist * cosAng + pt.X;
  result.Y := -dist * sinAng + pt.Y; //Y axis is positive down
end;

function GetAngleOfPt2FromPt1(const pt1, pt2: TFloatPoint): single;
var
  x, y: single;
begin
  x := pt2.X - pt1.X; y := pt2.Y - pt1.Y;
  if x = 0 then
  begin
    if y > 0 then result := rad270 else result := rad90;
  end else
  begin
    result := arctan2(-y, x);
    if result < 0 then result := result + rad360;
  end;
end;

function GetUnitVector(const pt1, pt2: TFloatPoint): TFloatPoint;
var
  dx, dy, f: TFloat;
begin
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  if (dx = 0) and (dy = 0) then
  begin
    result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    Result.X := dx * f;
    Result.Y := dy * f;
  end;
end;

function GetUnitNormal(const pt1, pt2: TFloatPoint): TFloatPoint;
var
  dx, dy, f: single;
begin
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);

  if (dx = 0) and (dy = 0) then
  begin
    result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;  //ie perpendicular to
  Result.Y := -dx; //the unit vector
end;

function OffsetPoint(const pt: TFloatPoint; dx, dy: TFloat): TFloatPoint;
begin
  result.X := pt.X + dx;
  result.Y := pt.Y + dy;
end;

function Shorten(const pts: TArrayOfFloatPoint;
  delta: TFloat; linePos: TLinePos): TArrayOfFloatPoint;
var
  i, highI: integer;
  dist, deltaSqr: TFloat;
  unitVec: TFloatPoint;

  procedure FixStart;
  begin
    i := 1;
    while (i < highI) and (SqrDistance(pts[i],pts[0]) < deltaSqr) do inc(i);
    unitVec := GetUnitVector(pts[i], pts[0]);
    dist := Distance(pts[i],pts[0]) - delta;
    if i > 1 then
    begin
      move(result[i], result[1], sizeof(TFloatPoint) * (highI -i +1));
      setlength(result, highI - i +2);
    end;
    result[0] := OffsetPoint(result[1], unitVec.X * dist, unitVec.Y * dist);
  end;

  procedure FixEnd;
  begin
    i := highI -1;
    while (i > 0) and (SqrDistance(pts[i],pts[highI]) < deltaSqr) do dec(i);
    unitVec := GetUnitVector(pts[i],pts[highI]);
    dist := Distance(pts[i],pts[highI]) - delta;
    if i +1 < highI then setlength(result, i+2);
    result[i+1] := OffsetPoint(result[i], unitVec.X * dist, unitVec.Y * dist);
  end;

begin
  result := pts;
  highI := high(pts);
  deltaSqr := delta * delta;
  if highI < 1 then exit;

  case linePos of
    lpStart: FixStart;
    lpEnd  : FixEnd;
    lpBoth : begin FixStart; FixEnd; end;
  end;
end;

function PointInPolygon(const pt: TFloatPoint; const Pts: TArrayOfFloatPoint): boolean;
var
  I: Integer;
  iPt, jPt: PFloatPoint;
begin
  Result := False;
  iPt := @Pts[0];
  jPt := @Pts[High(Pts)];
  for I := 0 to High(Pts) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      ((Pt.X - iPt.X) < ((jPt.X - iPt.X) * (Pt.Y -iPt.Y) / (jPt.Y - iPt.Y))));
    jPt := iPt;
    Inc(iPt);
  end;
end;

function SegmentIntersect(const p1, p2, p3, p4: TFloatPoint;
  out IntersectPoint: TFloatPoint): boolean;
var
  m1,b1,m2,b2: TFloat;
begin
  //see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  result := false;
  if (p2.X = p1.X) then
  begin
    if (p4.X = p3.X) then exit; //parallel lines
    m2 := (p4.Y - p3.Y)/(p4.X - p3.X);
    b2 := p3.Y - m2 * p3.X;
    IntersectPoint.X := p1.X;
    IntersectPoint.Y := m2*p1.X + b2;
    result := (IntersectPoint.Y < p2.Y) = (IntersectPoint.Y > p1.Y);
  end
  else if (p4.X = p3.X) then
  begin
    m1 := (p2.Y - p1.Y)/(p2.X - p1.X);
    b1 := p1.Y - m1 * p1.X;
    IntersectPoint.X := p3.X;
    IntersectPoint.Y := m1*p3.X + b1;
    result := (IntersectPoint.Y < p3.Y) = (IntersectPoint.Y > p4.Y);
  end else
  begin
    m1 := (p2.Y - p1.Y)/(p2.X - p1.X);
    b1 := p1.Y - m1 * p1.X;
    m2 := (p4.Y - p3.Y)/(p4.X - p3.X);
    b2 := p3.Y - m2 * p3.X;
    if m1 = m2 then exit; //parallel lines
    IntersectPoint.X := (b2 - b1)/(m1 - m2);
    IntersectPoint.Y := m1 * IntersectPoint.X + b1;
    result := ((IntersectPoint.X < p2.X) = (IntersectPoint.X > p1.X));
  end;
end;

// Fixed overloads

function Average(const V1, V2: TFixedPoint): TFixedPoint;
begin
  Result.X := (V1.X + V2.X) div 2;
  Result.Y := (V1.Y + V2.Y) div 2;
end;

function Dot(const V1, V2: TFixedPoint): TFixed;
begin
  Result := FixedMul(V1.X, V2.X) + FixedMul(V1.Y, V2.Y);
end;

function Distance(const V1, V2: TFixedPoint): TFixed;
begin
  Result :=
    Fixed(Hypot((V2.X - V1.X) * FixedToFloat, (V2.Y - V1.Y) * FixedToFloat));
end;

function SqrDistance(const V1, V2: TFixedPoint): TFixed;
begin
  Result := FixedSqr(V2.X - V1.X) + FixedSqr(V2.Y - V1.Y);
end;

function GetPointAtAngleFromPoint(const pt: TFixedPoint;
  const dist, radians: TFloat): TFixedPoint;
var
  sinAng, cosAng: TFloat;
begin
  GR32_Math.SinCos(radians, sinAng, cosAng);
  result.X := round(dist * cosAng *FixedOne) + pt.X;
  result.Y := -round(dist * sinAng *FixedOne) + pt.Y; //Y axis is positive down
end;

function GetAngleOfPt2FromPt1(pt1, pt2: TFixedPoint): single;
begin
  with pt2 do
  begin
    X := X - pt1.X; Y := Y - pt1.Y;
    if X = 0 then
    begin
     if Y > 0 then result := rad270 else result := rad90;
    end else
    begin
      result := arctan2(-Y,X);
      if result < 0 then result := result + rad360;
    end;
  end;
end;

function GetUnitVector(const pt1, pt2: TFixedPoint): TFloatPoint;
var
  dx, dy, f: single;
begin
  dx := (pt2.X - pt1.X)*FixedToFloat;
  dy := (pt2.Y - pt1.Y)*FixedToFloat;
  if (dx = 0) and (dy = 0) then
  begin
    result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    Result.X := dx * f;
    Result.Y := dy * f;
  end;
end;

function GetUnitNormal(const pt1, pt2: TFixedPoint): TFloatPoint;
var
  dx, dy, f: single;
begin
  dx := (pt2.X - pt1.X)*FixedToFloat;
  dy := (pt2.Y - pt1.Y)*FixedToFloat;

  if (dx = 0) and (dy = 0) then
  begin
    result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;  //ie perpendicular to
  Result.Y := -dx; //the unit vector
end;

function OffsetPoint(const pt: TFixedPoint; dx, dy: TFloat): TFixedPoint;
begin
  result.X := pt.X + Fixed(dx);
  result.Y := pt.Y + Fixed(dy);
end;

function Shorten(const pts: TArrayOfFixedPoint;
  delta: TFloat; linePos: TLinePos): TArrayOfFixedPoint;
var
  i, highI: integer;
  dist, deltaSqr: TFloat;
  unitVec: TFloatPoint;

  procedure FixStart;
  begin
    i := 1;
    while (i < highI) and (SqrDistance(pts[i],pts[0]) < deltaSqr) do inc(i);
    unitVec := GetUnitVector(pts[i], pts[0]);
    dist := Distance(pts[i],pts[0]) - delta;
    if i > 1 then
    begin
      move(result[i], result[1], sizeof(TFloatPoint) * (highI -i +1));
      setlength(result, highI - i +2);
    end;
    result[0] := OffsetPoint(result[1], unitVec.X * dist, unitVec.Y * dist);
  end;

  procedure FixEnd;
  begin
    i := highI -1;
    while (i > 0) and (SqrDistance(pts[i],pts[highI]) < deltaSqr) do dec(i);
    unitVec := GetUnitVector(pts[i],pts[highI]);
    dist := Distance(pts[i],pts[highI]) - delta;
    if i +1 < highI then setlength(result, i+2);
    result[i+1] := OffsetPoint(result[i], unitVec.X * dist, unitVec.Y * dist);
  end;

begin
  result := pts;
  highI := high(pts);
  deltaSqr := delta * delta;
  if highI < 1 then exit;

  case linePos of
    lpStart: FixStart;
    lpEnd  : FixEnd;
    lpBoth : begin FixStart; FixEnd; end;
  end;
end;

function PointInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean;
var
  I: Integer;
  iPt, jPt: PFixedPoint;
begin
  Result := False;
  iPt := @Pts[0];
  jPt := @Pts[High(Pts)];
  for I := 0 to High(Pts) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      (Pt.X - iPt.X < MulDiv(jPt.X - iPt.X, Pt.Y - iPt.Y, jPt.Y - iPt.Y)));
    jPt := iPt;
    Inc(iPt);
  end;
end;

function SegmentIntersect(const p1, p2, p3, p4: TFixedPoint;
  out IntersectPoint: TFixedPoint): boolean;
var
  m1,b1,m2,b2: TFloat;
begin
  result := false;
  if (p2.X = p1.X) then
  begin
    if (p4.X = p3.X) then exit; //parallel lines
    m2 := (p4.Y - p3.Y)/(p4.X - p3.X);
    b2 := p3.Y - m2 * p3.X;
    IntersectPoint.X := p1.X;
    IntersectPoint.Y := round(m2*p1.X + b2);
    result := (IntersectPoint.Y < p2.Y) = (IntersectPoint.Y > p1.Y);
  end
  else if (p4.X = p3.X) then
  begin
    m1 := (p2.Y - p1.Y)/(p2.X - p1.X);
    b1 := p1.Y - m1 * p1.X;
    IntersectPoint.X := p3.X;
    IntersectPoint.Y := round(m1*p3.X + b1);
    result := (IntersectPoint.Y < p3.Y) = (IntersectPoint.Y > p4.Y);
  end else
  begin
    m1 := (p2.Y - p1.Y)/(p2.X - p1.X);
    b1 := p1.Y - m1 * p1.X;
    m2 := (p4.Y - p3.Y)/(p4.X - p3.X);
    b2 := p3.Y - m2 * p3.X;
    if m1 = m2 then exit; //parallel lines
    IntersectPoint.X := round((b2 - b1)/(m1 - m2));
    IntersectPoint.Y := round(m1 * IntersectPoint.X + b1);
    result := ((IntersectPoint.X < p2.X) = (IntersectPoint.X > p1.X));
  end;
end;

end.
