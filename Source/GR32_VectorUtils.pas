unit GR32_VectorUtils;

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
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$BOOLEVAL OFF}

uses
  GR32, GR32_Transforms, GR32_Polygons;

const
  DEFAULT_MITER_LIMIT = 4.0;
  TWOPI = 2 * Pi;

function InSignedRange(const X, X1, X2: TFloat): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
function OverlapExclusive(const X1, X2, Y1, Y2: TFloat): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
function OverlapInclusive(const X1, X2, Y1, Y2: TFloat): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
function Intersect(const A1, A2, B1, B2: TFloatPoint; out P: TFloatPoint): Boolean;

function ClosePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;

function ClipLine(var X1, Y1, X2, Y2: Integer; MinX, MinY, MaxX, MaxY: Integer): Boolean; overload;
function ClipLine(var X1, Y1, X2, Y2: TFloat; MinX, MinY, MaxX, MaxY: TFloat): Boolean; overload;
function ClipLine(var P1, P2: TFloatPoint; const ClipRect: TFloatRect): Boolean; overload;

procedure Extract(Src: TArrayOfFloat; Indexes: TArrayOfInteger; out Dst: TArrayOfFloat);
procedure FastMergeSort(const Values: TArrayOfFloat; out Indexes: TArrayOfInteger);

function BuildNormals(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter;
  Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;
function Grow(const Points: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter;
  Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;
function ReversePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;

function BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint;
function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint;
  Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint;
function BuildDashedLine(const Points: TArrayOfFloatPoint;
  const DashArray: TArrayOfFloat; DashOffset: TFloat = 0;
  Closed: Boolean = False): TArrayOfArrayOfFloatPoint;

function ClipPolygon(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect): TArrayOfFloatPoint;
function CatPolygon(const P1, P2: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;

function CalculateCircleSteps(Radius: TFloat): Cardinal; {$IFDEF USEINLINING} inline; {$ENDIF}
function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;
function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFloatPoint; overload;
function Circle(const P: TFloatPoint; const Radius: TFloat; Steps: Integer = 100): TArrayOfFloatPoint; overload;
function Circle(const X, Y, Radius: TFloat; Steps: Integer = 100): TArrayOfFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Ellipse(const P, R: TFloatPoint; Steps: Integer = 100): TArrayOfFloatPoint; overload;
function Ellipse(const X, Y, Rx, Ry: TFloat; Steps: Integer = 100): TArrayOfFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Rectangle(const R: TFloatRect): TArrayOfFloatPoint; {$IFDEF USEINLINING} inline; {$ENDIF}
function RoundRect(const R: TFloatRect; const Radius: TFloat): TArrayOfFloatPoint; {$IFDEF USEINLINING} inline; {$ENDIF}

function PolygonBounds(const Points: TArrayOfFloatPoint): TFloatRect;

function ScalePolygon(const Src: TArrayOfFloatPoint; ScaleX, ScaleY: TFloat): TArrayOfFloatPoint;
function ScalePolyPolygon(const Src: TArrayOfArrayOfFloatPoint; ScaleX, ScaleY: TFloat): TArrayOfArrayOfFloatPoint;

function TransformPolygon(const Points: TArrayOfFloatPoint; Transformation: TTransformation): TArrayOfFloatPoint;
function TransformPolyPolygon(const Points: TArrayOfArrayOfFloatPoint; Transformation: TTransformation): TArrayOfArrayOfFloatPoint;

function BuildPolygon(const Data: array of TFloat): TArrayOfFloatPoint;
function TranslatePolygon(const Points: TArrayOfFloatPoint; Dx, Dy: TFloat): TArrayOfFloatPoint;
function PolyPolygon(const Points: TArrayOfFloatPoint): TArrayOfArrayOfFloatPoint; {$IFDEF USEINLINING}inline;{$ENDIF}
function FixedPointToFloatPoint(const Points: TArrayOfFixedPoint): TArrayOfFloatPoint; overload; {$IFDEF USEINLINING}inline;{$ENDIF}
function FixedPointToFloatPoint(const Points: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint; overload; {$IFDEF USEINLINING}inline;{$ENDIF}

implementation

uses
  Math, SysUtils, GR32_Math, GR32_Paths, GR32_Geometry, GR32_LowLevel;

type
  TTransformationAccess = class(TTransformation);

// Returns True if Min(X1, X2) <= X < Max(X1, X2)
function InSignedRange(const X, X1, X2: TFloat): Boolean;
begin
  Result := (X < X1) xor (X < X2);
end;

// Returns True if the intervals (X1,X2) and (Y1,Y2) overlap
function OverlapExclusive(const X1, X2, Y1, Y2: TFloat): Boolean;
begin
  Result := Abs((X1 + X2) - (Y1 + Y2)) < Abs(X1 - X2) + Abs(Y1 - Y2);
end;

// Returns True if the intervals [X1,X2] and [Y1,Y2] overlap
function OverlapInclusive(const X1, X2, Y1, Y2: TFloat): Boolean;
begin
  Result := Abs((X1 + X2) - (Y1 + Y2)) <= Abs(X1 - X2) + Abs(Y1 - Y2);
end;

// Returns True if the line segments (A1, A2) and (B1, B2) intersects
// P is the point of intersection
function Intersect(const A1, A2, B1, B2: TFloatPoint; out P: TFloatPoint): Boolean;
var
  Adx, Ady, Bdx, Bdy, ABy, ABx: TFloat;
  t, ta, tb: TFloat;
begin
  Result := False;
  Adx := A2.X - A1.X;
  Ady := A2.Y - A1.Y;
  Bdx := B2.X - B1.X;
  Bdy := B2.Y - B1.Y;
  t := (Bdy * Adx) - (Bdx * Ady);

  if t = 0 then Exit; // lines are parallell

  ABx := A1.X - B1.X;
  ABy := A1.Y - B1.Y;
  ta := Bdx * ABy - Bdy * ABx;
  tb := Adx * ABy - Ady * ABx;
  if InSignedRange(ta, 0, t) and InSignedRange(tb, 0, t) then
  begin
    Result := True;
    ta := ta / t;
    P.X := A1.X + ta * Adx;
    P.Y := A1.Y + ta * Ady;
  end;
end;

function ClosePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  L: Integer;
  P1, P2: TFloatPoint;
begin
  L := Length(Points);
  Result := Points;
  if L <= 1 then
    Exit;

  P1 := Result[0];
  P2 := Result[L - 1];
  if (P1.X = P2.X) and (P1.Y = P2.Y) then
    Exit;

  SetLength(Result, L+1);
  Move(Result[0], Points[0], L * SizeOf(TFloatPoint));
  Result[L] := P1;
end;

function ClipLine(var X1, Y1, X2, Y2: Integer; MinX, MinY, MaxX, MaxY: Integer): Boolean;
var
  C1, C2: Integer;
  V: Integer;
begin
  { Get edge codes }
  C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1 + Ord(Y1 < MinY) shl 2 + Ord(Y1 > MaxY) shl 3;
  C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1 + Ord(Y2 < MinY) shl 2 + Ord(Y2 > MaxY) shl 3;

  if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
  begin
    if (C1 and 12) <> 0 then
    begin
      if C1 < 8 then V := MinY else V := MaxY;
      Inc(X1, MulDiv(V - Y1, X2 - X1, Y2 - Y1));
      Y1 := V;
      C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1;
    end;

    if (C2 and 12) <> 0 then
    begin
      if C2 < 8 then V := MinY else V := MaxY;
      Inc(X2, MulDiv(V - Y2, X2 - X1, Y2 - Y1));
      Y2 := V;
      C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1;
    end;

    if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
    begin
      if C1 <> 0 then
      begin
        if C1 = 1 then V := MinX else V := MaxX;
        Inc(Y1, MulDiv(V - X1, Y2 - Y1, X2 - X1));
        X1 := V;
        C1 := 0;
      end;

      if C2 <> 0 then
      begin
        if C2 = 1 then V := MinX else V := MaxX;
        Inc(Y2, MulDiv(V - X2, Y2 - Y1, X2 - X1));
        X2 := V;
        C2 := 0;
      end;
    end;
  end;

  Result := (C1 or C2) = 0;
end;

function ClipLine(var X1, Y1, X2, Y2: TFloat; MinX, MinY, MaxX, MaxY: TFloat): Boolean;
var
  C1, C2: Integer;
  V: TFloat;
begin
  { Get edge codes }
  C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1 + Ord(Y1 < MinY) shl 2 + Ord(Y1 > MaxY) shl 3;
  C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1 + Ord(Y2 < MinY) shl 2 + Ord(Y2 > MaxY) shl 3;

  if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
  begin
    if (C1 and 12) <> 0 then
    begin
      if C1 < 8 then V := MinY else V := MaxY;
      X1 := X1 + (V - Y1) * (X2 - X1) / (Y2 - Y1);
      Y1 := V;
      C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1;
    end;

    if (C2 and 12) <> 0 then
    begin
      if C2 < 8 then V := MinY else V := MaxY;
      X2 := X2 + (V - Y2) * (X2 - X1) / (Y2 - Y1);
      Y2 := V;
      C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1;
    end;

    if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
    begin
      if C1 <> 0 then
      begin
        if C1 = 1 then V := MinX else V := MaxX;
        Y1 := Y1 + (V - X1) * (Y2 - Y1) / (X2 - X1);
        X1 := V;
        C1 := 0;
      end;

      if C2 <> 0 then
      begin
        if C2 = 1 then V := MinX else V := MaxX;
        Y2 := Y2 + (V - X2) * (Y2 - Y1) / (X2 - X1);
        X2 := V;
        C2 := 0;
      end;
    end;
  end;

  Result := (C1 or C2) = 0;
end;

function ClipLine(var P1, P2: TFloatPoint; const ClipRect: TFloatRect): Boolean;
begin
  Result := ClipLine(P1.X, P1.Y, P2.X, P2.Y, ClipRect.Left, ClipRect.Top,
    ClipRect.Right, ClipRect.Bottom);
end;

procedure Extract(Src: TArrayOfFloat; Indexes: TArrayOfInteger; out Dst: TArrayOfFloat);
var
  I: Integer;
begin
  SetLength(Dst, Length(Indexes));
  for I := 0 to High(Indexes) do
    Dst[I] := Src[Indexes[I]];
end;

// A modified implementation of merge sort
// - returns the indexes of the sorted elements
// - use Extract(Indexes, Output) to return the sorted values
// - complexity when input is already sorted: O(n)
// - worst case complexity: O(n log n)
procedure FastMergeSort(const Values: TArrayOfFloat; out Indexes: TArrayOfInteger);
var
  Temp: TArrayOfInteger;

  procedure Merge(I1, I2, J1, J2: Integer);
  var
    I, J, K: Integer;
  begin
    if Values[Indexes[I2]] < Values[Indexes[J1]] then Exit;
    I := I1;
    J := J1;
    K := 0;
    repeat
      if Values[Indexes[I]] < Values[Indexes[J]] then
      begin
        Temp[K] := Indexes[I];
        Inc(I);
      end
      else
      begin
        Temp[K] := Indexes[J];
        Inc(J);
      end;
      Inc(K);
    until (I > I2) or (J > J2);

    while I <= I2 do
    begin
      Temp[K] := Indexes[I];
      Inc(I); Inc(K);
    end;
    while J <= J2 do
    begin
      Temp[K] := Indexes[J];
      Inc(J); Inc(K);
    end;
    for I := 0 to K - 1 do
    begin
      Indexes[I + I1] := Temp[I];
    end;
  end;

  procedure Recurse(I1, I2: Integer);
  var
    I, IX: Integer;
  begin
    if I1 = I2 then
      Indexes[I1] := I1
    else if Indexes[I1] = Indexes[I2] then
    begin
      if Values[I1] <= Values[I2] then
      begin
        for I := I1 to I2 do Indexes[I] := I;
      end
      else
      begin
        IX := I1 + I2;
        for I := I1 to I2 do Indexes[I] := IX - I;
      end;
    end
    else
    begin
      IX := (I1 + I2) div 2;
      Recurse(I1, IX);
      Recurse(IX + 1, I2);
      Merge(I1, IX, IX + 1, I2);
    end;
  end;

var
  I, Index, S: Integer;
begin
  SetLength(Temp, Length(Values));
  SetLength(Indexes, Length(Values));

  Index := 0;
  S := Math.Sign(Values[1] - Values[0]);
  if S = 0 then S := 1;

  Indexes[0] := 0;
  for I := 1 to High(Values) do
  begin
    if Math.Sign(Values[I] - Values[I - 1]) = -S then
    begin
      S := -S;
      Inc(Index);
    end;
    Indexes[I] := Index;
  end;

  Recurse(0, High(Values));
end;

function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat;
  Steps: Integer): TArrayOfFloatPoint;
var
  I, N: Integer;
  Angle, DeltaAngle: TFloat;
  Delta: TFloatPoint;
begin
  SetLength(Result, Steps);
  N := Steps - 1;
  DeltaAngle := (EndAngle - StartAngle) / N;
  Angle := StartAngle;
  for I := 0 to N do
  begin
    SinCos(Angle, Radius, Delta.Y, Delta.X);
    Result[I].X := P.X + Delta.X;
    Result[I].Y := P.Y + Delta.Y;
    Angle := Angle + DeltaAngle;
  end;
end;

function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFloatPoint;
const
  MINSTEPS = 6;
var
  Steps: Integer;
begin
  Steps := Max(MINSTEPS, System.Round(Sqrt(Abs(Radius)) * Abs(EndAngle - StartAngle)));
  Result := BuildArc(P, StartAngle, EndAngle, Radius, Steps);
end;

function CalculateCircleSteps(Radius: TFloat): Cardinal;
var
  AbsRadius: TFloat;
begin
  AbsRadius := Abs(Radius);
  Result := Trunc(Pi / (ArcCos(AbsRadius / (AbsRadius + 0.125))));
end;

function Circle(const P: TFloatPoint; const Radius: TFloat;
  Steps: Integer = 100): TArrayOfFloatPoint;
var
  I: Integer;
  M: TFloat;
  C, D: TFloatPoint;
begin
  if Steps <= 0 then
    Steps := CalculateCircleSteps(Radius);

  SetLength(Result, Steps);
  M := 2 * System.Pi / Steps;

  // first item
  Result[0].X := Radius + P.X;
  Result[0].Y := P.Y;

  // calculate complex offset
  GR32_Math.SinCos(M, C.Y, C.X);
  D := C;

  // second item
  Result[1].X := Radius * D.X + P.X;
  Result[1].Y := Radius * D.Y + P.Y;

  // other items
  for I := 2 to Steps - 1 do
  begin
    D := FloatPoint(D.X * C.X - D.Y * C.Y, D.Y * C.X + D.X * C.Y);

    Result[I].X := Radius * D.X + P.X;
    Result[I].Y := Radius * D.Y + P.Y;
  end;
end;

function Circle(const X, Y, Radius: TFloat; Steps: Integer = 100): TArrayOfFloatPoint;
begin
  Result := Circle(FloatPoint(X, Y), Radius, Steps);
end;

function Ellipse(const X, Y, Rx, Ry: TFloat; Steps: Integer): TArrayOfFloatPoint;
begin
  Result := Ellipse(FloatPoint(X, Y), FloatPoint(Rx, Ry), Steps);
end;

function Ellipse(const P, R: TFloatPoint; Steps: Integer): TArrayOfFloatPoint;
var
  I: Integer;
  M: TFloat;
  C, D: TFloatPoint;
begin
  if Steps <= 0 then
    Steps := CalculateCircleSteps(Min(R.X, R.Y));

  SetLength(Result, Steps);
  M := 2 * System.Pi / Steps;

  // first item
  Result[0].X := R.X + P.X;
  Result[0].Y := P.Y;

  // calculate complex offset
  GR32_Math.SinCos(M, C.Y, C.X);
  D := C;

  // second item
  Result[1].X := R.X * D.X + P.X;
  Result[1].Y := R.Y * D.Y + P.Y;

  // other items
  for I := 2 to Steps - 1 do
  begin
    D := FloatPoint(D.X * C.X - D.Y * C.Y, D.Y * C.X + D.X * C.Y);

    Result[I].X := R.X * D.X + P.X;
    Result[I].Y := R.Y * D.Y + P.Y;
  end;
end;

function Rectangle(const R: TFloatRect): TArrayOfFloatPoint;
begin
  SetLength(Result, 4);
  Result[0] := R.TopLeft;
  Result[1] := FloatPoint(R.Right, R.Top);
  Result[2] := R.BottomRight;
  Result[3] := FloatPoint(R.Left, R.Bottom);
end;

function RoundRect(const R: TFloatRect; const Radius: TFloat): TArrayOfFloatPoint;
var
  R2: TFloatRect;
begin
  R2 := R;
  InflateRect(R2, -Radius, -Radius);
  Result := Grow(Rectangle(R2), Radius, jsRound, True);
end;

function BuildNormals(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
const
  EPSILON = 1E-4;
var
  I, Count, NextI: Integer;
  dx, dy, f: Double;
begin
  Count := Length(Points);
  SetLength(Result, Count);

  I := 0;
  NextI := 1;

  while I < Count do
  begin
    if NextI >= Count then NextI := 0;

    dx := Points[NextI].X - Points[I].X;
    dy := Points[NextI].Y - Points[I].Y;
    if Abs(dx) <= EPSILON then dx := 0;
    if Abs(dy) <= EPSILON then dy := 0;
    if (dx <> 0) or (dy <> 0) then
    begin
      f := 1 / GR32_Math.Hypot(dx, dy);
      dx := dx * f;
      dy := dy * f;
    end;

    Result[I].X := dy;
    Result[I].Y := -dx;

    Inc(I);
    Inc(NextI);
  end;
end;

function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint; overload;
const
  BUFFSIZEINCREMENT = 128;
  EPSILON = 0.001;
var
  I, L, H: Integer;
  ResSize, BuffSize: integer;
  PX, PY, D, RMin: TFloat;
  A, B: TFloatPoint;

  procedure AddPoint(const LongDeltaX, LongDeltaY: TFloat);
  begin
    if ResSize = BuffSize then
    begin
      inc(BuffSize, BUFFSIZEINCREMENT);
      SetLength(Result, BuffSize);
    end;
    with Result[ResSize] do
    begin
      X := PX + LongDeltaX;
      Y := PY + LongDeltaY;
    end;
    inc(resSize);
  end;

  procedure AddMitered(const X1, Y1, X2, Y2: TFloat);
  var
    R, CX, CY: TFloat;
  begin
    CX := X1 + X2;
    CY := Y1 + Y2;

    R := X1 * CX + Y1 * CY; //(1 - cos(ß))  (range: 0 <= R <= 2)
    if R < RMin then
    begin
      AddPoint(D * X1, D * Y1);
      AddPoint(D * X2, D * Y2);
    end
    else
    begin
      R := D / R;
      AddPoint(CX * R, CY * R)
    end;
  end;

  procedure AddBevelled(const X1, Y1, X2, Y2: TFloat);
  var
    R: TFloat;
  begin
    R := X1 * Y2 - X2 * Y1; //cross product
    if R * D <= 0 then      //ie angle is concave
    begin
      AddMitered(X1, Y1, X2, Y2);
    end
    else
    begin
      AddPoint(D * X1, D * Y1);
      AddPoint(D * X2, D * Y2);
    end;
  end;

  procedure AddRoundedJoin(const X1, Y1, X2, Y2: TFloat);
  var
    R, a1, a2, da: TFloat;
    Arc: TArrayOfFloatPoint;
    arcLen: integer;
  begin
    R := X1 * Y2 - X2 * Y1;
    if R * D <= 0 then
    begin
      AddMitered(X1, Y1, X2, Y2);
    end
    else
    begin
      a1 := ArcTan2(Y1, X1);
      a2 := ArcTan2(Y2, X2);
      da := a2 - a1;
      if da > Pi then
        a2 := a2 - TWOPI
      else if da < -Pi then
        a2 := a2 + TWOPI;
      Arc := BuildArc(FloatPoint(PX, PY), a1, a2, D);

      arcLen := Length(Arc);
      if resSize + arcLen >= BuffSize then
      begin
        inc(BuffSize, arcLen);
        SetLength(Result, BuffSize);
      end;
      Move(Arc[0], Result[resSize], Length(Arc) * SizeOf(TFloatPoint));
      inc(resSize, arcLen);
    end;
  end;

  procedure AddJoin(const X, Y, X1, Y1, X2, Y2: TFloat);
  begin
    PX := X;
    PY := Y;
    case JoinStyle of
      jsMiter: AddMitered(A.X, A.Y, B.X, B.Y);
      jsBevel: AddBevelled(A.X, A.Y, B.X, B.Y);
      jsRound: AddRoundedJoin(A.X, A.Y, B.X, B.Y);
    end;
  end;

begin
  Result := nil;

  if Length(Points) <= 1 then Exit;

  D := Delta;

  //MiterLimit = Sqrt(2/(1 - cos(ß)))
  //Sqr(MiterLimit) = 2/(1 - cos(ß))
  //1 - cos(ß) = 2/Sqr(MiterLimit) = RMin;
  RMin := 2 / Sqr(MiterLimit);

  H := High(Points) - Ord(not Closed);
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);

{** all normals zeroed => Exit }
  if H < 0 then Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  if Closed then
    A := Normals[H]
  else
    A := Normals[L];

  ResSize := 0;
  BuffSize := BUFFSIZEINCREMENT;
  SetLength(Result, BuffSize);

  for I := L to H do
  begin
    B := Normals[I];
    if (B.X = 0) and (B.Y = 0) then Continue;

    with Points[I] do AddJoin(X, Y, A.X, A.Y, B.X, B.Y);
    A := B;
  end;
  if not Closed then
    with Points[High(Points)] do AddJoin(X, Y, A.X, A.Y, A.X, A.Y);

  SetLength(Result, resSize);
end;

function Grow(const Points: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean;
  MiterLimit: TFloat): TArrayOfFloatPoint; overload;
var
  Normals: TArrayOfFloatPoint;
begin
  Normals := BuildNormals(Points);
  Result := Grow(Points, Normals, Delta, JoinStyle, Closed, MiterLimit);
end;

function ReversePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  Dec(L);
  for I := 0 to L do
    Result[I] := Points[L - I];
end;

function BuildLineEnd(const P, N: TFloatPoint; const W: TFloat; EndStyle: TEndStyle): TArrayOfFloatPoint;
var
  a1, a2: TFloat;
begin
  case EndStyle of
    esButt:
      begin
        Result := nil;
      end;
    esSquare:
      begin
        SetLength(Result, 2);
        Result[0].X := P.X + (N.X - N.Y) * W;
        Result[0].Y := P.Y + (N.Y + N.X) * W;
        Result[1].X := P.X - (N.X + N.Y) * W;
        Result[1].Y := P.Y - (N.Y - N.X) * W;
      end;
    esRound:
      begin
        a1 := ArcTan2(N.Y, N.X);
        a2 := ArcTan2(-N.Y, -N.X);
        if a2 < a1 then a2 := a2 + TWOPI;
        Result := BuildArc(P, a1, a2, W);
      end;
  end;
end;

function BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  L, H: Integer;
  Normals: TArrayOfFloatPoint;
  P1, P2, E1, E2: TArrayOfFloatPoint;
  V: TFloat;
  P: PFloatPoint;
begin
  V := StrokeWidth * 0.5;
  Normals := BuildNormals(Points);

  H := High(Points) - 1;
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);
  if H < 0 then Exit;
  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  P1 := Grow(Points, Normals, V, JoinStyle, False, MiterLimit);
  P2 := ReversePolygon(Grow(Points, Normals, -V, JoinStyle, False, MiterLimit));

  E1 := BuildLineEnd(Points[0], Normals[L], -V, EndStyle);
  E2 := BuildLineEnd(Points[High(Points)], Normals[H], V, EndStyle);

  SetLength(Result, Length(P1) + Length(P2) + Length(E1) + Length(E2));
  P := @Result[0];
  Move(E1[0], P^, Length(E1) * SizeOf(TFloatPoint)); Inc(P, Length(E1));
  Move(P1[0], P^, Length(P1) * SizeOf(TFloatPoint)); Inc(P, Length(P1));
  Move(E2[0], P^, Length(E2) * SizeOf(TFloatPoint)); Inc(P, Length(E2));
  Move(P2[0], P^, Length(P2) * SizeOf(TFloatPoint));
end;

function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint;
  Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
  P1, P2: TArrayOfFloatPoint;
  Dst: TArrayOfArrayOfFloatPoint;
  Normals: TArrayOfFloatPoint;
begin
  if Closed then
  begin
    SetLength(Dst, Length(Points) * 2);
    for I := 0 to High(Points) do
    begin
      Normals := BuildNormals(Points[I]);
      P1 := Grow(Points[I], Normals, StrokeWidth * 0.5, JoinStyle, True, MiterLimit);
      P2 := Grow(Points[I], Normals, -StrokeWidth * 0.5, JoinStyle, True, MiterLimit);
      Dst[I * 2] := P1;
      Dst[I * 2 + 1] := ReversePolygon(P2);
    end;
  end
  else
  begin
    SetLength(Dst, Length(Points));
    for I := 0 to High(Points) do
      Dst[I] := BuildPolyline(Points[I], StrokeWidth, JoinStyle, EndStyle);
  end;
  Result := Dst;
end;

function InterpolateX(X: TFloat; const P1, P2: TFloatPoint): TFloatPoint;
var
  W: Double;
begin
  W := (X - P1.X) / (P2.X - P1.X);
  Result.X := X;
  Result.Y := P1.Y + W * (P2.Y - P1.Y);
end;

function InterpolateY(Y: TFloat; const P1, P2: TFloatPoint): TFloatPoint;
var
  W: Double;
begin
  W := (Y - P1.Y) / (P2.Y - P1.Y);
  Result.Y := Y;
  Result.X := P1.X + W * (P2.X - P1.X);
end;

function GetCode(const P: TFloatPoint; const R: TFloatRect): Integer; {$IFDEF USEINLINING}inline;{$ENDIF}
begin
  Result := Ord(P.X >= R.Left) or
    (Ord(P.X <= R.Right) shl 1) or
    (Ord(P.Y >= R.Top) shl 2) or
    (Ord(P.Y <= R.Bottom) shl 3);
end;

function ClipPolygon(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect): TArrayOfFloatPoint;
type
  TInterpolateProc = function(X: TFloat; const P1, P2: TFloatPoint): TFloatPoint;
const
  SAFEOVERSIZE = 5;
  POPCOUNT: array [0..15] of Integer =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
var
  I, J, K, L, N: Integer;
  X, Y, Z, Code, Count: Integer;
  Codes: PByteArray;
  NextIndex: PIntegerArray;
  Temp: PFloatPointArray;
label
  ExitProc;

  procedure AddPoint(Index: Integer; const P: TFloatPoint);
  begin
    Temp[K] := P;
    Codes[K] := GetCode(P, ClipRect);
    Inc(K);
    Inc(Count);
  end;

  function ClipEdges(Mask: Integer; V: TFloat; Interpolate: TInterpolateProc): Boolean;
  var
    I, NextI, StopIndex: Integer;
  begin
    I := 0;
    while (I < K) and (Codes[I] and Mask = 0) do Inc(I);

    Result := I = K;
    if Result then { all points outside }
    begin
      ClipPolygon := nil;
      Result := True;
      Exit;
    end;

    StopIndex := I;
    repeat
      NextI := NextIndex[I];

      if Codes[NextI] and Mask = 0 then  { inside -> outside }
      begin
        NextIndex[I] := K;
        NextIndex[K] := K + 1;
        AddPoint(I, Interpolate(V, Temp[I], Temp[NextI]));

        while Codes[NextI] and Mask = 0 do
        begin
          Dec(Count);
          Codes[NextI] := 0;
          I := NextI;
          NextI := NextIndex[I];
        end;
        { outside -> inside }
        NextIndex[I] := K;
        NextIndex[K] := NextI;
        AddPoint(I, Interpolate(V, Temp[I], Temp[NextI]));
      end;

      I := NextI;
    until I = StopIndex;
  end;

begin
  N := Length(Points);
{$IFDEF USESTACKALLOC}
  Codes := StackAlloc(N * SAFEOVERSIZE * SizeOf(Byte));
{$ELSE}
  GetMem(Codes, N * SAFEOVERSIZE * SizeOf(Byte));
{$ENDIF}
  X := 15;
  Y := 0;
  for I := 0 to N - 1 do
  begin
    Code := GetCode(Points[I], ClipRect);
    Codes[I] := Code;
    X := X and Code;
    Y := Y or Code;
  end;
  if X = 15 then { all points inside }
  begin
    Result := Points;
  end
  else if Y <> 15 then { all points outside }
  begin
    Result := nil;
  end
  else
  begin
    Count := N;
    Z := Codes[N - 1];
    for I := 0 to N - 1 do
    begin
      Code := Codes[I];
      Inc(Count, POPCOUNT[Z xor Code]);
      Z := Code;
    end;
{$IFDEF USESTACKALLOC}
    Temp := StackAlloc(Count * SizeOf(TFloatPoint));
    NextIndex := StackAlloc(Count * SizeOf(TFloatPoint));
{$ELSE}
    GetMem(Temp, Count * SizeOf(TFloatPoint));
    GetMem(NextIndex, Count * SizeOf(TFloatPoint));
{$ENDIF}

    Move(Points[0], Temp[0], N * SizeOf(TFloatPoint));
    for I := 0 to N - 2 do NextIndex[I] := I + 1;
    NextIndex[N - 1] := 0;

    Count := N;
    K := N;
    if X and 1 = 0 then if ClipEdges(1, ClipRect.Left, InterpolateX) then goto ExitProc;
    if X and 2 = 0 then if ClipEdges(2, ClipRect.Right, InterpolateX) then goto ExitProc;
    if X and 4 = 0 then if ClipEdges(4, ClipRect.Top, InterpolateY) then goto ExitProc;
    if X and 8 = 0 then if ClipEdges(8, ClipRect.Bottom, InterpolateY) then goto ExitProc;

    SetLength(Result, Count);

    { start with first point inside the clipping rectangle }
    I := 0;
    while Codes[I] = 0 do
      I := NextIndex[I];

    J := I;
    L := 0;
    repeat
      Result[L] := Temp[I];
      Inc(L);
      I := NextIndex[I];
    until I = J;

ExitProc:
{$IFDEF USESTACKALLOC}
    StackFree(NextIndex);
    StackFree(Temp);
{$ELSE}
    FreeMem(NextIndex);
    FreeMem(Temp);
{$ENDIF}
  end;
{$IFDEF USESTACKALLOC}
  StackFree(Codes);
{$ELSE}
  FreeMem(Codes);
{$ENDIF}
end;

function BuildDashedLine(const Points: TArrayOfFloatPoint;
  const DashArray: TArrayOfFloat; DashOffset: TFloat = 0;
  Closed: Boolean = False): TArrayOfArrayOfFloatPoint;
var
  I, J, DashIndex, len1, len2: Integer;
  Offset, Dist, v: TFloat;
  Delta: TFloatPoint;

  procedure AddPoint(X, Y: TFloat);
  var
    K: Integer;
  begin
    K := Length(Result[J]);
    SetLength(Result[J], K + 1);
    Result[J][K].X := X;
    Result[J][K].Y := Y;
  end;

  procedure AddDash(I: Integer);
  begin
    if i = 0 then
    begin
      Delta.X := Points[0].X - Points[High(Points)].X;
      Delta.Y := Points[0].Y - Points[High(Points)].Y;
    end else
    begin
      Delta.X := Points[I].X - Points[I - 1].X;
      Delta.Y := Points[I].Y - Points[I - 1].Y;
    end;
    Dist := GR32_Math.Hypot(Delta.X, Delta.Y);
    if Dist = 0 then Exit;
    Offset := Offset + Dist;
    Dist := 1 / Dist;
    Delta.X := Delta.X * Dist;
    Delta.Y := Delta.Y * Dist;
    while Offset > DashOffset do
    begin
      v := Offset - DashOffset;
      AddPoint(Points[I].X - v * Delta.X, Points[I].Y - v * Delta.Y);
      DashIndex := (DashIndex + 1) mod Length(DashArray);
      DashOffset := DashOffset + DashArray[DashIndex];
      if Odd(DashIndex) then
      begin
        Inc(J);
        SetLength(Result, J + 1);
      end;
    end;
    if not Odd(DashIndex) then
      AddPoint(Points[I].X, Points[I].Y);
  end;

begin
  if Length(Points) <= 0 then Exit;
  DashIndex := -1;
  Offset := 0;

  V := 0;
  for I := 0 to High(DashArray) do
    V := V + DashArray[I];
  DashOffset := Wrap(DashOffset, V);

  DashOffset := DashOffset - V;
  while DashOffset < 0 do
  begin
    Inc(DashIndex);
    DashOffset := DashOffset + DashArray[DashIndex];
  end;

  J := 0;
  // note to self: second dimension might not be zero by default!
  SetLength(Result, 1, 0);

  if not Odd(DashIndex) then
    AddPoint(Points[0].X, Points[0].Y);
  for I := 1 to High(Points) do
    AddDash(I);

  if Closed then
  begin
    AddDash(0);
    len1 := Length(Result[0]);
    len2 := Length(Result[J]);
    if (len1 > 0) and (len2 > 0) then
    begin
      SetLength(Result[0], len1 + len2 -1);
      Move(Result[0][0], Result[0][len2 - 1], SizeOf(TFloatPoint) * len1);
      Move(Result[J][0], Result[0][0], SizeOf(TFloatPoint) * len2);
      SetLength(Result, J);
      Dec(J);
    end;
  end;

  if Length(Result[J]) = 0 then SetLength(Result, J);
end;

function CatPolygon(const P1, P2: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
var
  L1, L2: Integer;
begin
  L1 := Length(P1);
  L2 := Length(P2);
  SetLength(Result, L1 + L2);
  Move(P1[0], Result[0], L1 * SizeOf(TFloatPoint));
  Move(P2[0], Result[L1], L2 * SizeOf(TFloatPoint));
end;

function PolygonBounds(const Points: TArrayOfFloatPoint): TFloatRect;
var
  I: Integer;
begin
  Result.Left := Points[0].X;
  Result.Top := Points[0].Y;
  Result.Right := Points[0].X;
  Result.Bottom := Points[0].Y;
  for I := 1 to High(Points) do
  begin
    Result.Left := Min(Result.Left, Points[I].X);
    Result.Right := Max(Result.Right, Points[I].X);
    Result.Top := Min(Result.Top, Points[I].Y);
    Result.Bottom := Max(Result.Bottom, Points[I].Y);
  end;
end;

// Scales to a polygon (TArrayOfFloatPoint)
function ScalePolygon(const Src: TArrayOfFloatPoint; ScaleX, ScaleY: TFloat): TArrayOfFloatPoint;
var
  I, L: Integer;
begin
  L := Length(Src);
  SetLength(Result, L);
  for I := 0 to L - 1 do
  begin
    Result[I].X := Src[I].X * ScaleX;
    Result[I].Y := Src[I].Y * ScaleY;
  end;
end;

// Scales all sub polygons in a complex polygon (TArrayOfArrayOfFloatPoint)
function ScalePolyPolygon(const Src: TArrayOfArrayOfFloatPoint;
  ScaleX, ScaleY: TFloat): TArrayOfArrayOfFloatPoint;
var
  I, L: Integer;
begin
  L := Length(Src);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := ScalePolygon(Src[I], ScaleX, ScaleY);
end;

// Applies transformation to a polygon (TArrayOfFloatPoint)
function TransformPolygon(const Points: TArrayOfFloatPoint;
  Transformation: TTransformation): TArrayOfFloatPoint;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := 0 to High(Result) do
    TTransformationAccess(Transformation).TransformFloat(Points[I].X,
      Points[I].Y, Result[I].X, Result[I].Y);
end;

// Applies transformation to all sub polygons in a complex polygon
function TransformPolyPolygon(const Points: TArrayOfArrayOfFloatPoint;
  Transformation: TTransformation): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  TTransformationAccess(Transformation).PrepareTransform;

  for I := 0 to High(Result) do
    Result[I] := TransformPolygon(Points[I], Transformation);
end;

function BuildPolygon(const Data: array of TFloat): TArrayOfFloatPoint;
var
  Index, Count: Integer;
begin
  Count := Length(Data) div 2;
  SetLength(Result, Count);
  if Count = 0 then Exit;
  for Index := 0 to Count - 1 do
  begin
    Result[Index].X := Data[Index * 2];
    Result[Index].Y := Data[Index * 2 + 1];
  end;
end;

function TranslatePolygon(const Points: TArrayOfFloatPoint;
  Dx, Dy: TFloat): TArrayOfFloatPoint;
var
  I, Len: Integer;
begin
  Len := Length(Points);
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
  begin
    Result[I].X := Points[I].X + Dx;
    Result[I].Y := Points[I].Y + Dy;
  end;
end;

// Copy data from Polygon to simple PolyPolygon (using 1 sub polygon only)
function PolyPolygon(const Points: TArrayOfFloatPoint)
  : TArrayOfArrayOfFloatPoint;
begin
  SetLength(Result, 1);
  Result[0] := Points;
end;

// Converts an array of points in TFixed format to an array of points in TFloat format
function FixedPointToFloatPoint(const Points: TArrayOfFixedPoint)
  : TArrayOfFloatPoint;
var
  Index: Integer;
begin
  if Length(Points) > 0 then
  begin
    SetLength(Result, Length(Points));
    for Index := 0 to Length(Points) - 1 do
    begin
      Result[Index].X := Points[Index].X * FixedToFloat;
      Result[Index].Y := Points[Index].Y * FixedToFloat;
    end;
  end;
end;

// Converts an array of array of points in TFixed format to an array of array of points in TFloat format
function FixedPointToFloatPoint(const Points: TArrayOfArrayOfFixedPoint)
  : TArrayOfArrayOfFloatPoint;
var
  Index, PointIndex: Integer;
begin
  if Length(Points) > 0 then
  begin
    SetLength(Result, Length(Points));
    for Index := 0 to Length(Points) - 1 do
    begin
      SetLength(Result[Index], Length(Points[Index]));
      for PointIndex := 0 to Length(Points[Index]) - 1 do
      begin
        Result[Index, PointIndex].X := Points[Index, PointIndex].X * FixedToFloat;
        Result[Index, PointIndex].Y := Points[Index, PointIndex].Y * FixedToFloat;
      end;
    end;
  end;
end;

end.
