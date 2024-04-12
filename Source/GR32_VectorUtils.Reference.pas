unit GR32_VectorUtils.Reference;

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
 * The Original Code is Polyline builder for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 * Angus Johnson (http://www.angusj.com)
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$BOOLEVAL OFF}


uses
  GR32,
  GR32_VectorUtils,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
//      PolyLineBuilderReference
//
//------------------------------------------------------------------------------
// Old implementation of the Grow and BuildPoly*line functions.
//------------------------------------------------------------------------------
// Note: Does not currently support JoinStyle=jsSquare; jsBevel is used instead.
//------------------------------------------------------------------------------

type
  PolyLineBuilderReference = class(TPolyLineBuilder)
  private
    class function BuildLineEnd(const P, N: TFloatPoint; const W: TFloat; EndStyle: TEndStyle): TArrayOfFloatPoint; overload; static;
    class function BuildLineEnd(const P, N: TFixedPoint; const W: TFixed; EndStyle: TEndStyle): TArrayOfFixedPoint; overload; static;
  public
    class function SupportedJoinStyles: TJoinStyles; override;
    class function SupportedEndStyles: TEndStyles; override;

    // Float
    class function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;

    // Float
    class function BuildPolyLine(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload; override;
    class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload; override;
    // Fixed
    class function BuildPolyLine(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload; override;
    class function BuildPolyPolyLine(const Points: TArrayOfArrayOfFixedPoint; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfArrayOfFixedPoint; overload; override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  Types,

  GR32_Math,
  GR32_Geometry,
  GR32_LowLevel;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.SupportedEndStyles: TEndStyles;
begin
  Result := [esButt, esSquare, esRound];
end;

class function PolyLineBuilderReference.SupportedJoinStyles: TJoinStyles;
begin
  Result := [jsMiter, jsBevel, jsRound, jsRoundEx];
end;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint;
const
  BUFFSIZEINCREMENT = 128;
  MINDISTPIXEL = 1.414; // just a little bit smaller than sqrt(2),
  // -> set to about 2.5 for a similar output with the previous version
var
  I, L, H: Integer;
  ResultSize, BufferSize: Integer;
  PX, PY: TFloat;
  AngleInv, RMin: TFloat;
  A, B, Dm: TFloatPoint;
  InvMiterLimit: TFloat;

  procedure AddPoint(const DeltaX, DeltaY: TFloat); overload;
  begin
    if (ResultSize = BufferSize) then
    begin
      Inc(BufferSize, BUFFSIZEINCREMENT);
      SetLength(Result, BufferSize);
    end;
    Result[ResultSize] := FloatPoint(PX + DeltaX, PY + DeltaY);
    Inc(ResultSize);
  end;

  procedure AddPoint(const Delta: TFloatPoint); overload;
  begin
    if (ResultSize = BufferSize) then
    begin
      Inc(BufferSize, BUFFSIZEINCREMENT);
      SetLength(Result, BufferSize);
    end;
    Result[ResultSize] := FloatPoint(PX + Delta.X, PY + Delta.Y);
    Inc(ResultSize);
  end;

  procedure AddMitered(const P1, P2: TFloatPoint);
  var
    R, CX, CY: TFloat;
  begin
    CX := P1.X + P2.X;
    CY := P1.Y + P2.Y;

    R := P1.X * CX + P1.Y * CY; //(1 - cos(?))  (range: 0 <= R <= 2)

    if (R < RMin) then
    begin
      AddPoint(Delta * P1.X, Delta * P1.Y);
      AddPoint(Delta * P2.X, Delta * P2.Y);
    end else
    begin
      R := Delta / R;
      AddPoint(CX * R, CY * R)
    end;
  end;

  procedure AddBevelled(const P1, P2: TFloatPoint);
  var
    R: TFloat;
  begin
    R := CrossProduct(P1, P2);

    if (R * Delta <= 0) then  // ie angle is concave
      AddMitered(P1, P2)
    else
    begin
      AddPoint(Delta * P1.X, Delta * P1.Y);
      AddPoint(Delta * P2.X, Delta * P2.Y);
    end;
  end;

  procedure AddRoundedJoin(const P1, P2: TFloatPoint);
  var
    SinA, CosA, A: TFloat;
    Steps: Integer;
    ii: Integer;
    C: TFloatPoint;
    d: TFloat;
    m,n: Integer;
    C2, C3: TFloatPoint;
    SignA: Cardinal absolute Delta;
    SignB: Cardinal absolute SinA;
  begin
    SinA := CrossProduct(P1, P2);
    CosA := Dot(P1, P2);
    A := ArcTan2(SinA, CosA);
    Steps := Round(Abs(A * AngleInv));

    if (SinA < 0) then
      Dm.Y := -Abs(Dm.Y)
    else
      Dm.Y := Abs(Dm.Y);

    //
    // (SignA and $80000000) <> (SignB and $80000000) is a fast test for (SinA * Delta < 0).
    // It tests if the sign-bit of the two 32-bit Single values are the same.
    //
    //   (SinA * Delta < 0)              =>
    //   Sign(SinA) <> Sign(Delta)
    //
    if ((SignA and $80000000) <> (SignB and $80000000)) and ((SignB and (not $80000000)) <> 0) then
    begin // ie angle is concave

      // Compute the inner arc for the jsRoundEx join style (both inner
      // and outer vertex rounded).

      A := Delta / (CosA +1);
      // C = offset pt of concave vertex ...
      C.X := PX + (P1.X + P2.X) * A;
      C.Y := PY + (P1.Y + P2.Y) * A;

      if (I = 0) then
        m := H
      else
        m := I-1;

      if (I = H) then
        n := 0
      else
        n := I+1;

      A := Min(SqrDistance(Points[m], Points[I]), SqrDistance(Points[n], Points[I]));

      if (SqrDistance(C, Points[I]) > A) then
      begin
        // There's no room to draw anything ...

        // This will create a self-intersection but it also ensures that
        // the offset will be maintained beyond this intersection ...
        AddPoint(Delta * P1.X, Delta * P1.Y);
        AddPoint(Delta * P2.X, Delta * P2.Y);
        Exit;
      end;
      A := Sqrt(A);

      // Get the points on both edges that's same distance from
      // the concave vertex as its closest adjacent vertex.
      // nb: using unit normals as unit vectors here ...
      C2.X := PX + P1.Y * A;
      C2.Y := PY - P1.X * A;
      C3.X := PX - P2.Y * A;
      C3.Y := PY + P2.X * A;

      // Now Delta offset these points ...
      C2.X := C2.X + P1.X * Delta;
      C2.Y := C2.Y + P1.Y * Delta;
      C3.X := C3.X + P2.X * Delta;
      C3.Y := C3.Y + P2.Y * Delta;

      // This will do Delta/MiterLimit radius rounding of concavities ...
      A := SqrDistance(C2, C3);
      d := Delta * InvMiterLimit;
      if (A < Sqr(d * 3)) then
        d := -Sqrt(A) / 3;

      // Move point(PX,PY) across the offset path so the
      // rounding path will curve around this new point ...
      A := (d + Delta) / (CosA +1);
      PX := PX + (P1.X + P2.X) * A;
      PY := PY + (P1.Y + P2.Y) * A;

      // Start of arc
      C.X := -P1.X * d;
      C.Y := -P1.Y * d;
      AddPoint(C);

      // Arc
      for ii := 1 to Steps -1 do
      begin
        C := FloatPoint(
          C.X * Dm.X - C.Y * Dm.Y,
          C.X * Dm.Y + C.Y * Dm.X);
        AddPoint(C);
      end;

      // End of arc
      if (P1 <> P2) then
      begin
        C.X := -P2.X * d;
        C.Y := -P2.Y * d;
        AddPoint(C);
      end;

    end else
    begin

      // Start of arc
      C.X := P1.X * Delta;
      C.Y := P1.Y * Delta;
      AddPoint(C);

      // Arc
      for ii := 1 to Steps - 1 do
      begin
        C := FloatPoint(
          C.X * Dm.X - C.Y * Dm.Y,
          C.Y * Dm.X + C.X * Dm.Y);
        AddPoint(C);
      end;

      // End of arc
      if (P1 <> P2) then
      begin
        C.X := P2.X * Delta;
        C.Y := P2.Y * Delta;
        AddPoint(C);
      end;
    end;
  end;

  procedure AddJoin(const P, PA, PB: TFloatPoint);
  begin
    PX := P.X;
    PY := P.Y;

    if (JoinStyle <> jsRoundEx) and (CrossProduct(PA, PB) * Delta < 0)  then
    begin
      // Concave angle
      AddPoint(Delta * PA.X, Delta * PA.Y);
      AddPoint(0, 0); // Current corner. See #106, #185
      AddPoint(Delta * PB.X, Delta * PB.Y);
    end else
      case JoinStyle of
        jsMiter: AddMitered(A, B);

        jsSquare,
        jsBevel: AddBevelled(A, B);

        jsRoundEx,
        jsRound: AddRoundedJoin(A, B);
      end;
  end;

begin
  Result := nil;

  if (Length(Points) <= 1) then
    Exit;

  if (IsZero(Delta)) then
    Exit(Points);

  if (MiterLimit > 0) then
    InvMiterLimit := 1 / MiterLimit
  else
    InvMiterLimit := 0;

  RMin := 2 * Sqr(InvMiterLimit);

  H := High(Points) - Ord(not Closed);
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do
    Dec(H);
  // All normals zeroed => Exit
  if H < 0 then
    Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do
    Inc(L);

  ResultSize := 0;
  BufferSize := BUFFSIZEINCREMENT;
  SetLength(Result, BufferSize);

  // prepare
  if (JoinStyle in [jsRound, jsRoundEx]) then
  begin
    Dm.X := 1 - 0.5 * Min(3, Sqr(MINDISTPIXEL / Abs(Delta)));
    Dm.Y := Sqrt(1 - Sqr(Dm.X));
    AngleInv := 1 / ArcCos(Dm.X);
  end;

  if Closed then
    A := Normals[H]
  else
    A := Normals[L];

  for I := L to H do
  begin
    B := Normals[I];
    if (B.X = 0) and (B.Y = 0) then
      Continue;

    AddJoin(Points[I], A, B);
    A := B;
  end;

  if (not Closed) then
    AddJoin(Points[High(Points)], A, A);

  SetLength(Result, ResultSize);
end;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.BuildLineEnd(const P, N: TFloatPoint; const W: TFloat; EndStyle: TEndStyle): TArrayOfFloatPoint;
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
        if a2 < a1 then
          a2 := a2 + TWOPI;
        Result := BuildArc(P, a1, a2, W);
      end;
  end;
end;

class function PolyLineBuilderReference.BuildLineEnd(const P, N: TFixedPoint; const W: TFixed; EndStyle: TEndStyle): TArrayOfFixedPoint;
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
        if a2 < a1 then
          a2 := a2 + TWOPI;
        Result := BuildArc(P, a1, a2, W);
      end;
  end;
end;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.BuildPolyLine(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  L, H: Integer;
  Normals: TArrayOfFloatPoint;
  P1, P2, E1, E2: TArrayOfFloatPoint;
  V: TFloat;
  P: PFloatPoint;
begin
  Result := nil;
  V := StrokeWidth * 0.5;
  Normals := BuildNormals(Points);

  H := High(Points) - 1;
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do
    Dec(H);
  // All normals zeroed => Exit
  if H < 0 then
    Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do
    Inc(L);

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

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint;
  Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
  P1, P2: TArrayOfFloatPoint;
  Dst: TArrayOfArrayOfFloatPoint;
  Normals: TArrayOfFloatPoint;
  HalfStrokeWidth: TFloat;
begin
  if Closed then
  begin

    SetLength(Dst, Length(Points) * 2);
    HalfStrokeWidth := StrokeWidth * 0.5;

    for I := 0 to High(Points) do
    begin
      Normals := BuildNormals(Points[I]);
      P1 := Grow(Points[I], Normals, HalfStrokeWidth, JoinStyle, True, MiterLimit);
      P2 := Grow(Points[I], Normals, -HalfStrokeWidth, JoinStyle, True, MiterLimit);
      Dst[I * 2] := P1;
      Dst[I * 2 + 1] := ReversePolygon(P2);
    end;

  end else
  begin

    SetLength(Dst, Length(Points));
    for I := 0 to High(Points) do
      Dst[I] := BuildPolyLine(Points[I], StrokeWidth, JoinStyle, EndStyle, MiterLimit);

  end;
  Result := Dst;
end;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.BuildPolyLine(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFixed): TArrayOfFixedPoint;
var
  L, H: Integer;
  Normals: TArrayOfFixedPoint;
  P1, P2, E1, E2: TArrayOfFixedPoint;
  V: TFixed;
  P: PFixedPoint;
begin
  Result := nil;
  V := StrokeWidth shr 1;
  Normals := BuildNormals(Points);

  H := High(Points) - 1;
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do
    Dec(H);
  if H < 0 then
    Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do
    Inc(L);

  P1 := Grow(Points, Normals, V, JoinStyle, False, MiterLimit);
  P2 := ReversePolygon(Grow(Points, Normals, -V, JoinStyle, False, MiterLimit));

  E1 := BuildLineEnd(Points[0], Normals[L], -V, EndStyle);
  E2 := BuildLineEnd(Points[High(Points)], Normals[H], V, EndStyle);

  SetLength(Result, Length(P1) + Length(P2) + Length(E1) + Length(E2));
  P := @Result[0];
  Move(E1[0], P^, Length(E1) * SizeOf(TFixedPoint)); Inc(P, Length(E1));
  Move(P1[0], P^, Length(P1) * SizeOf(TFixedPoint)); Inc(P, Length(P1));
  Move(E2[0], P^, Length(E2) * SizeOf(TFixedPoint)); Inc(P, Length(E2));
  Move(P2[0], P^, Length(P2) * SizeOf(TFixedPoint));
end;

//------------------------------------------------------------------------------

class function PolyLineBuilderReference.BuildPolyPolyLine(const Points: TArrayOfArrayOfFixedPoint;
  Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFixed): TArrayOfArrayOfFixedPoint;
var
  I: Integer;
  P1, P2: TArrayOfFixedPoint;
  Dst: TArrayOfArrayOfFixedPoint;
  Normals: TArrayOfFixedPoint;
  HalfStrokeWidth: TFixed;
begin
  if Closed then
  begin

    SetLength(Dst, Length(Points) * 2);
    HalfStrokeWidth := StrokeWidth shr 1;

    for I := 0 to High(Points) do
    begin
      Normals := BuildNormals(Points[I]);
      P1 := Grow(Points[I], Normals, HalfStrokeWidth, JoinStyle, True, MiterLimit);
      P2 := Grow(Points[I], Normals, -HalfStrokeWidth, JoinStyle, True, MiterLimit);
      Dst[I * 2] := P1;
      Dst[I * 2 + 1] := ReversePolygon(P2);
    end;

  end else
  begin

    SetLength(Dst, Length(Points));
    for I := 0 to High(Points) do
      Dst[I] := BuildPolyLine(Points[I], StrokeWidth, JoinStyle, EndStyle);

  end;
  Result := Dst;
end;

//------------------------------------------------------------------------------

end.
