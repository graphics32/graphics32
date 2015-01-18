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
  Math, Types, GR32;

type
  TLinePos = (lpStart, lpEnd, lpBoth, lpNeither);

// TFloat Overloads
function Average(const V1, V2: TFloatPoint): TFloatPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function CrossProduct(V1, V2: TFloatPoint): TFloat; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Dot(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFloatPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function GetPointAtAngleFromPoint(const Pt: TFloatPoint; const Dist, Radians: Single): TFloatPoint; overload;
function GetAngleOfPt2FromPt1(const Pt1, Pt2: TFloatPoint): Single; overload;
function GetUnitNormal(const Pt1, Pt2: TFloatPoint): TFloatPoint; overload;
function GetUnitVector(const Pt1, Pt2: TFloatPoint): TFloatPoint; overload;
function OffsetPoint(const Pt: TFloatPoint; DeltaX, DeltaY: TFloat): TFloatPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt, Delta: TFloatPoint): TFloatPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFloatRect; const DeltaX, DeltaY: TFloat): TFloatRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFloatRect; const Delta: TFloatPoint): TFloatRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Shorten(const Pts: TArrayOfFloatPoint;
  Delta: TFloat; LinePos: TLinePos): TArrayOfFloatPoint; overload;
function PointInPolygon(const Pt: TFloatPoint; const Pts: TArrayOfFloatPoint): Boolean; overload;
function SegmentIntersect(const P1, P2, P3, P4: TFloatPoint;
  out IntersectPoint: TFloatPoint): Boolean; overload;
function PerpendicularDistance(const P, P1, P2: TFloatPoint): TFloat; overload;


// TFixed Overloads
function Average(const V1, V2: TFixedPoint): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function CrossProduct(V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Dot(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFixedPoint): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function GetPointAtAngleFromPoint(const Pt: TFixedPoint; const Dist, Radians: Single): TFixedPoint; overload;
function GetAngleOfPt2FromPt1(Pt1, Pt2: TFixedPoint): Single; overload;
function GetUnitVector(const Pt1, Pt2: TFixedPoint): TFloatPoint; overload;
function GetUnitNormal(const Pt1, Pt2: TFixedPoint): TFloatPoint; overload;
function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFixed): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFloat): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt: TFixedPoint; const Delta: TFixedPoint): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt: TFixedPoint; const Delta: TFloatPoint): TFixedPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFixed): TFixedRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFloat): TFixedRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFixedRect; const Delta: TFixedPoint): TFixedRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetRect(const Rct: TFixedRect; const Delta: TFloatPoint): TFixedRect; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Shorten(const Pts: TArrayOfFixedPoint;
  Delta: TFloat; LinePos: TLinePos): TArrayOfFixedPoint; overload;
function PointInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean; overload;
function SegmentIntersect(const P1, P2, P3, P4: TFixedPoint;
  out IntersectPoint: TFixedPoint): Boolean; overload;
function PerpendicularDistance(const P, P1, P2: TFixedPoint): TFixed; overload;

// Integer Overloads
function Average(const V1, V2: TPoint): TPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function CrossProduct(V1, V2: TPoint): Integer; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Dot(const V1, V2: TPoint): Integer; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TPoint): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TPoint): Integer; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt: TPoint; DeltaX, DeltaY: Integer): TPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function OffsetPoint(const Pt, Delta: TPoint): TPoint; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function PerpendicularDistance(const P, P1, P2: TPoint): TFloat; overload;

const
  CRad01 = Pi / 180;
  CRad30 = Pi / 6;
  CRad45 = Pi / 4;
  CRad60 = Pi / 3;
  CRad90 = Pi / 2;
  CRad180 = Pi;
  CRad270 = CRad90 * 3;
  CRad360 = CRad180 * 2;
  CDegToRad = Pi / 180;
  CRadToDeg = 180 / Pi;

implementation

uses
  GR32_Math;

function Average(const V1, V2: TFloatPoint): TFloatPoint;
begin
  Result.X := (V1.X + V2.X) * 0.5;
  Result.Y := (V1.Y + V2.Y) * 0.5;
end;

function CrossProduct(V1, V2: TFloatPoint): TFloat;
begin
  Result := V1.X * V2.Y - V1.Y * V2.X;
end;

function Dot(const V1, V2: TFloatPoint): TFloat;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y;
end;

function Distance(const V1, V2: TFloatPoint): TFloat;
begin
  Result := GR32_Math.Hypot(V2.X - V1.X, V2.Y - V1.Y);
end;

function SqrDistance(const V1, V2: TFloatPoint): TFloat;
begin
  Result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y);
end;

function GetPointAtAngleFromPoint(const Pt: TFloatPoint;
  const Dist, Radians: TFloat): TFloatPoint; overload;
var
  SinAng, CosAng: TFloat;
begin
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  Result.X :=  Dist * CosAng + Pt.X;
  Result.Y := -Dist * SinAng + Pt.Y; // Y axis is positive down
end;

function GetAngleOfPt2FromPt1(const Pt1, Pt2: TFloatPoint): Single;
var
  X, Y: TFloat;
begin
  X := Pt2.X - Pt1.X;
  Y := Pt2.Y - Pt1.Y;
  if X = 0 then
  begin
    if Y > 0 then Result := CRad270 else Result := CRad90;
  end else
  begin
    Result := ArcTan2(-Y, X);
    if Result < 0 then Result := Result + CRad360;
  end;
end;

function GetUnitVector(const Pt1, Pt2: TFloatPoint): TFloatPoint;
var
  Delta: TFloatPoint;
  Temp: TFloat;
begin
  Delta.X := (Pt2.X - Pt1.X);
  Delta.Y := (Pt2.Y - Pt1.Y);
  if (Delta.X = 0) and (Delta.Y = 0) then
    Result := FloatPoint(0, 0)
  else
  begin
    Temp := 1 / GR32_Math.Hypot(Delta.X, Delta.Y);
    Result.X := Delta.X * Temp;
    Result.Y := Delta.Y * Temp;
  end;
end;

function GetUnitNormal(const Pt1, Pt2: TFloatPoint): TFloatPoint;
var
  Delta: TFloatPoint;
  Temp: TFloat;
begin
  Delta.X := (Pt2.X - Pt1.X);
  Delta.Y := (Pt2.Y - Pt1.Y);

  if (Delta.X = 0) and (Delta.Y = 0) then
    Result := FloatPoint(0, 0)
  else
  begin
    Temp := 1 / GR32_Math.Hypot(Delta.X, Delta.Y);
    Delta.X := Delta.X * Temp;
    Delta.Y := Delta.Y * Temp;
  end;
  Result.X :=  Delta.Y; // ie perpendicular to
  Result.Y := -Delta.X; // the unit vector
end;

function OffsetPoint(const Pt: TFloatPoint; DeltaX, DeltaY: TFloat): TFloatPoint;
begin
  Result.X := Pt.X + DeltaX;
  Result.Y := Pt.Y + DeltaY;
end;

function OffsetPoint(const Pt, Delta: TFloatPoint): TFloatPoint;
begin
  Result.X := Pt.X + Delta.X;
  Result.Y := Pt.Y + Delta.Y;
end;

function OffsetRect(const Rct: TFloatRect; const DeltaX, DeltaY: TFloat): TFloatRect;
begin
  Result.TopLeft := OffsetPoint(Rct.TopLeft, DeltaX, DeltaY);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, DeltaX, DeltaY);
end;

function OffsetRect(const Rct: TFloatRect; const Delta: TFloatPoint): TFloatRect;
begin
  Result.TopLeft := OffsetPoint(Rct.TopLeft, Delta);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, Delta);
end;


function Shorten(const Pts: TArrayOfFloatPoint;
  Delta: TFloat; LinePos: TLinePos): TArrayOfFloatPoint;
var
  Index, HighI: integer;
  Dist, DeltaSqr: TFloat;
  UnitVec: TFloatPoint;

  procedure FixStart;
  begin
    Index := 1;
    while (Index < HighI) and (SqrDistance(Pts[Index], Pts[0]) < DeltaSqr) do
      Inc(Index);
    UnitVec := GetUnitVector(Pts[Index], Pts[0]);
    Dist := Distance(Pts[Index], Pts[0]) - Delta;
    if Index > 1 then
    begin
      HighI := HighI - Index + 1;
      Move(Result[Index], Result[1], SizeOf(TFloatPoint) * HighI);
      SetLength(Result, HighI + 1);
    end;
    Result[0] := OffsetPoint(Result[1], UnitVec.X * Dist, UnitVec.Y * Dist);
  end;

  procedure FixEnd;
  begin
    Index := HighI - 1;
    while (Index > 0) and (SqrDistance(Pts[Index],Pts[HighI]) < DeltaSqr) do
      Dec(Index);
    UnitVec := GetUnitVector(Pts[Index],Pts[HighI]);
    Dist := Distance(Pts[Index], Pts[HighI]) - Delta;
    if Index + 1 < HighI then SetLength(Result, Index + 2);
    Result[Index + 1] := OffsetPoint(Result[Index], UnitVec.X * Dist, UnitVec.Y * Dist);
  end;

begin
  Result := Pts;
  HighI := High(Pts);
  DeltaSqr := Delta * Delta;
  if HighI < 1 then Exit;

  case LinePos of
    lpStart: FixStart;
    lpEnd  : FixEnd;
    lpBoth : begin FixStart; FixEnd; end;
  end;
end;

function PointInPolygon(const Pt: TFloatPoint; const Pts: TArrayOfFloatPoint): Boolean;
var
  Index: Integer;
  iPt, jPt: PFloatPoint;
begin
  Result := False;
  iPt := @Pts[0];
  jPt := @Pts[High(Pts)];
  for Index := 0 to High(Pts) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      ((Pt.X - iPt.X) < ((jPt.X - iPt.X) * (Pt.Y -iPt.Y) / (jPt.Y - iPt.Y))));
    jPt := iPt;
    Inc(iPt);
  end;
end;

function SegmentIntersect(const P1, P2, P3, P4: TFloatPoint;
  out IntersectPoint: TFloatPoint): Boolean;
var
  m1, b1, m2, b2: TFloat;
begin
  // see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  Result := False;
  if (P2.X = P1.X) then
  begin
    if (P4.X = P3.X) then Exit; // parallel lines
    m2 := (P4.Y - P3.Y) / (P4.X - P3.X);
    b2 := P3.Y - m2 * P3.X;
    IntersectPoint.X := P1.X;
    IntersectPoint.Y := m2 * P1.X + b2;
    Result := (((IntersectPoint.Y < P2.Y) = (IntersectPoint.Y > P1.Y)) or
      (IntersectPoint.Y = P2.Y) or (IntersectPoint.Y = P1.Y)) and
      (((IntersectPoint.Y < P3.Y) = (IntersectPoint.Y > P4.Y)) or
      (IntersectPoint.Y = P3.Y) or (IntersectPoint.Y = P4.Y));
  end
  else if (P4.X = P3.X) then
  begin
    m1 := (P2.Y - P1.Y) / (P2.X - P1.X);
    b1 := P1.Y - m1 * P1.X;
    IntersectPoint.X := P3.X;
    IntersectPoint.Y := m1 * P3.X + b1;
    Result := (((IntersectPoint.Y < P2.Y) = (IntersectPoint.Y > P1.Y)) or
      (IntersectPoint.Y = P2.Y) or (IntersectPoint.Y = P1.Y)) and
      (((IntersectPoint.Y < P3.Y) = (IntersectPoint.Y > P4.Y)) or
      (IntersectPoint.Y = P3.Y) or (IntersectPoint.Y = P4.Y));
  end else
  begin
    m1 := (P2.Y - P1.Y) / (P2.X - P1.X);
    b1 := P1.Y - m1 * P1.X;
    m2 := (P4.Y - P3.Y) / (P4.X - P3.X);
    b2 := P3.Y - m2 * P3.X;
    if m1 = m2 then Exit; // parallel lines
    IntersectPoint.X := (b2 - b1) / (m1 - m2);
    IntersectPoint.Y := m1 * IntersectPoint.X + b1;
    Result := (((IntersectPoint.X < P2.X) = (IntersectPoint.X > P1.X)) or
      (IntersectPoint.X = P2.X) or (IntersectPoint.X = P1.X)) and
      (((IntersectPoint.X < P3.X) = (IntersectPoint.X > P4.X)) or
      (IntersectPoint.X = P3.X) or (IntersectPoint.X = P4.X));
  end;
end;

function PerpendicularDistance(const P, P1, P2: TFloatPoint): TFloat;
begin
  Result := Abs((P.x - P2.x) * (P1.y - P2.y) - (P.y - P2.y) * (P1.x - P2.x)) /
    GR32_Math.Hypot(P1.x - P2.x, P1.y - P2.y);
end;


// Fixed overloads

function Average(const V1, V2: TFixedPoint): TFixedPoint;
begin
  Result.X := (V1.X + V2.X) div 2;
  Result.Y := (V1.Y + V2.Y) div 2;
end;

function CrossProduct(V1, V2: TFixedPoint): TFixed;
begin
  Result := FixedMul(V1.X, V2.Y) - FixedMul(V1.Y, V2.X);
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

function GetPointAtAngleFromPoint(const Pt: TFixedPoint;
  const Dist, Radians: TFloat): TFixedPoint;
var
  SinAng, CosAng: TFloat;
begin
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  Result.X := Round(Dist * CosAng * FixedOne) + Pt.X;
  Result.Y := -Round(Dist * SinAng * FixedOne) + Pt.Y; // Y axis is positive down
end;

function GetAngleOfPt2FromPt1(Pt1, Pt2: TFixedPoint): Single;
begin
  with Pt2 do
  begin
    X := X - Pt1.X;
    Y := Y - Pt1.Y;
    if X = 0 then
    begin
     if Y > 0 then Result := CRad270 else Result := CRad90;
    end else
    begin
      Result := ArcTan2(-Y,X);
      if Result < 0 then Result := Result + CRad360;
    end;
  end;
end;

function GetUnitVector(const Pt1, Pt2: TFixedPoint): TFloatPoint;
var
  Delta: TFloatPoint;
  Temp: Single;
begin
  Delta.X := (Pt2.X - Pt1.X) * FixedToFloat;
  Delta.Y := (Pt2.Y - Pt1.Y) * FixedToFloat;
  if (Delta.X = 0) and (Delta.Y = 0) then
  begin
    Result := FloatPoint(0,0);
  end else
  begin
    Temp := 1 / GR32_Math.Hypot(Delta.X, Delta.Y);
    Result.X := Delta.X * Temp;
    Result.Y := Delta.Y * Temp;
  end;
end;

function GetUnitNormal(const Pt1, Pt2: TFixedPoint): TFloatPoint;
var
  Delta: TFloatPoint;
  Temp: Single;
begin
  Delta.X := (Pt2.X - Pt1.X) * FixedToFloat;
  Delta.Y := (Pt2.Y - Pt1.Y) * FixedToFloat;

  if (Delta.X = 0) and (Delta.Y = 0) then
  begin
    Result := FloatPoint(0,0);
  end else
  begin
    Temp := 1 / GR32_Math.Hypot(Delta.X, Delta.Y);
    Delta.X := Delta.X * Temp;
    Delta.Y := Delta.Y * Temp;
  end;
  Result.X :=  Delta.Y; // ie perpendicular to
  Result.Y := -Delta.X; // the unit vector
end;

function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFixed): TFixedPoint;
begin
  Result.X := Pt.X + DeltaX;
  Result.Y := Pt.Y + DeltaY;
end;

function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFloat): TFixedPoint;
begin
  Result.X := Pt.X + Fixed(DeltaX);
  Result.Y := Pt.Y + Fixed(DeltaY);
end;

function OffsetPoint(const Pt: TFixedPoint; const Delta: TFixedPoint): TFixedPoint;
begin
  Result.X := Pt.X + Delta.X;
  Result.Y := Pt.Y + Delta.Y;
end;

function OffsetPoint(const Pt: TFixedPoint; const Delta: TFloatPoint): TFixedPoint;
begin
  Result.X := Pt.X + Fixed(Delta.X);
  Result.Y := Pt.Y + Fixed(Delta.Y);
end;

function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFixed): TFixedRect;
begin
  Result.TopLeft := OffsetPoint(Rct.TopLeft, DeltaX, DeltaY);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, DeltaX, DeltaY);
end;

function OffsetRect(const Rct: TFixedRect; const Delta: TFixedPoint): TFixedRect;
begin
  Result.TopLeft := OffsetPoint(Rct.TopLeft, Delta);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, Delta);
end;

function OffsetRect(const Rct: TFixedRect; const DeltaX, DeltaY: TFloat): TFixedRect;
var
  DX, DY: TFixed;
begin
  DX := Fixed(DeltaX);
  DY := Fixed(DeltaY);
  Result.TopLeft := OffsetPoint(Rct.TopLeft, DX, DY);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, DX, DY);
end;

function OffsetRect(const Rct: TFixedRect; const Delta: TFloatPoint): TFixedRect;
var
  DX, DY: TFixed;
begin
  DX := Fixed(Delta.X);
  DY := Fixed(Delta.Y);
  Result.TopLeft := OffsetPoint(Rct.TopLeft, DX, DY);
  Result.BottomRight := OffsetPoint(Rct.BottomRight, DX, DY);
end;

function Shorten(const Pts: TArrayOfFixedPoint;
  Delta: TFloat; LinePos: TLinePos): TArrayOfFixedPoint;
var
  Index, HighI: integer;
  Dist, DeltaSqr: TFloat;
  UnitVec: TFloatPoint;

  procedure FixStart;
  begin
    Index := 1;
    while (Index < HighI) and (SqrDistance(Pts[Index],Pts[0]) < DeltaSqr) do Inc(Index);
    UnitVec := GetUnitVector(Pts[Index], Pts[0]);
    Dist := Distance(Pts[Index],Pts[0]) - Delta;
    if Index > 1 then
    begin
      Move(Result[Index], Result[1], SizeOf(TFloatPoint) * (HighI - Index + 1));
      SetLength(Result, HighI - Index + 2);
      HighI := HighI - Index + 1;
    end;
    Result[0] := OffsetPoint(Result[1], UnitVec.X * Dist, UnitVec.Y * Dist);
  end;

  procedure FixEnd;
  begin
    Index := HighI -1;
    while (Index > 0) and (SqrDistance(Pts[Index],Pts[HighI]) < DeltaSqr) do Dec(Index);
    UnitVec := GetUnitVector(Pts[Index],Pts[HighI]);
    Dist := Distance(Pts[Index],Pts[HighI]) - Delta;
    if Index + 1 < HighI then SetLength(Result, Index + 2);
    Result[Index + 1] := OffsetPoint(Result[Index], UnitVec.X * Dist, UnitVec.Y * Dist);
  end;

begin
  Result := Pts;
  HighI := High(Pts);
  DeltaSqr := Delta * Delta;
  if HighI < 1 then Exit;

  case LinePos of
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

function SegmentIntersect(const P1, P2, P3, P4: TFixedPoint;
  out IntersectPoint: TFixedPoint): Boolean;
var
  m1,b1,m2,b2: TFloat;
begin
  Result := False;
  if (P2.X = P1.X) then
  begin
    if (P4.X = P3.X) then Exit; // parallel lines
    m2 := (P4.Y - P3.Y) / (P4.X - P3.X);
    b2 := P3.Y - m2 * P3.X;
    IntersectPoint.X := P1.X;
    IntersectPoint.Y := Round(m2 * P1.X + b2);
    Result := (IntersectPoint.Y < P2.Y) = (IntersectPoint.Y > P1.Y);
  end
  else if (P4.X = P3.X) then
  begin
    m1 := (P2.Y - P1.Y) / (P2.X - P1.X);
    b1 := P1.Y - m1 * P1.X;
    IntersectPoint.X := P3.X;
    IntersectPoint.Y := Round(m1 * P3.X + b1);
    Result := (IntersectPoint.Y < P3.Y) = (IntersectPoint.Y > P4.Y);
  end else
  begin
    m1 := (P2.Y - P1.Y) / (P2.X - P1.X);
    b1 := P1.Y - m1 * P1.X;
    m2 := (P4.Y - P3.Y) / (P4.X - P3.X);
    b2 := P3.Y - m2 * P3.X;
    if m1 = m2 then Exit; // parallel lines
    IntersectPoint.X := Round((b2 - b1) / (m1 - m2));
    IntersectPoint.Y := Round(m1 * IntersectPoint.X + b1);
    Result := ((IntersectPoint.X < P2.X) = (IntersectPoint.X > P1.X));
  end;
end;

function PerpendicularDistance(const P, P1, P2: TFixedPoint): TFixed;
begin
  Result := Fixed(Abs((P.x - P2.x) * (P1.y - P2.y) - (P.y - P2.y) *
    (P1.x - P2.x)) * FixedToFloat / Hypot((P1.x - P2.x) * FixedToFloat,
    (P1.y - P2.y) * FixedToFloat));
end;


// Integer overloads

function Average(const V1, V2: TPoint): TPoint;
begin
  Result.X := (V1.X + V2.X) div 2;
  Result.Y := (V1.Y + V2.Y) div 2;
end;

function CrossProduct(V1, V2: TPoint): Integer;
begin
  Result := V1.X * V2.Y - V1.Y * V2.X;
end;

function Dot(const V1, V2: TPoint): Integer;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y;
end;

function Distance(const V1, V2: TPoint): TFloat;
begin
  Result := Hypot(Integer(V2.X - V1.X), Integer(V2.Y - V1.Y));
end;

function SqrDistance(const V1, V2: TPoint): Integer;
begin
  Result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y);
end;

function OffsetPoint(const Pt: TPoint; DeltaX, DeltaY: Integer): TPoint;
begin
  Result.X := Pt.X + DeltaX;
  Result.Y := Pt.Y + DeltaY;
end;

function OffsetPoint(const Pt, Delta: TPoint): TPoint;
begin
  Result.X := Pt.X + Delta.X;
  Result.Y := Pt.Y + Delta.Y;
end;

function PerpendicularDistance(const P, P1, P2: TPoint): TFloat;
begin
  Result := Abs((P.x - P2.x) * (P1.y - P2.y) - (P.y - P2.y) * (P1.x - P2.x)) /
    Math.Hypot(P1.x - P2.x, P1.y - P2.y);
end;

end.
