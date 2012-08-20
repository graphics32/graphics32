unit GR32_Clipper;

(* BEGIN LICENSE BLOCK *********************************************************
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
 * The Original Code is GR32_Clipper.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2012 the Initial Developer. All Rights Reserved.
 *
 * Version 1.00 (5-June-2012)
 *
 * END LICENSE BLOCK **********************************************************)

interface

uses
  Classes, GR32, Clipper;

type
  TClipper32 = class(TClipper)
    function AddArrayOfFixedPoint(
      const Afp: TArrayOfFixedPoint;
      PolyType: TPolyType): Boolean;
    function AddArrayOfArrayOfFixedPoint(
      const Aafp: TArrayOfArrayOfFixedPoint;
      PolyType: TPolyType): Boolean;

    function AddArrayOfFloatPoint(
      const Afp: TArrayOfFloatPoint;
      PolyType: TPolyType): Boolean;
    function AddArrayOfArrayOfFloatPoint(
      const Aafp: TArrayOfArrayOfFloatPoint;
      PolyType: TPolyType): Boolean;

    function Execute(
      ClipType: TClipType;
      out Solution: TArrayOfArrayOfFixedPoint;
      SubjFillType: TPolyFillType = pftEvenOdd;
      ClipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;
    function Execute(
      ClipType: TClipType;
      out Solution: TArrayOfArrayOfFloatPoint;
      SubjFillType: TPolyFillType = pftEvenOdd;
      ClipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;
  end;

  function OffsetPolygons32(const Aafp: TArrayOfArrayOfFixedPoint;
    const Delta: Double; JoinType: TJoinType = jtSquare;
    MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint; overload;

  function OffsetPolygons32(const Aafp: TArrayOfArrayOfFloatPoint;
    const Delta: Double; JoinType: TJoinType = jtSquare;
    MiterLimit: Double = 2): TArrayOfArrayOfFloatPoint; overload;

  function ArrayOfFixedPointToPolygon(
    const ArrFxPt: TArrayOfFixedPoint): TPolygon;
  function ArrayOfArrayOfFixedPointToPolygons(
    const ArrArrFxPt: TArrayOfArrayOfFixedPoint): TPolygons;
  function PolygonToArrayOfFixedPoint(
    const Poly: TPolygon): TArrayOfFixedPoint;
  function PolygonsToArrayOfArrayOfFixedPoint(
    const Polys: TPolygons): TArrayOfArrayOfFixedPoint;

  function ArrayOfFloatPointToPolygon(
    const ArrFltPt: TArrayOfFloatPoint): TPolygon;
  function ArrayOfArrayOfFloatPointToPolygons(
    const ArrArrFltPt: TArrayOfArrayOfFloatPoint): TPolygons;
  function PolygonToArrayOfFloatPoint(
    const Poly: TPolygon): TArrayOfFloatPoint;
  function PolygonsToArrayOfArrayOfFloatPoint(
    const Polys: TPolygons): TArrayOfArrayOfFloatPoint;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfFixedPoint(const Afp: TArrayOfFixedPoint;
  PolyType: TPolyType): Boolean;
var
  Poly: TPolygon;
begin
  Poly := ArrayOfFixedPointToPolygon(Afp);
  Result := AddPolygon(Poly, PolyType);
end;
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfArrayOfFixedPoint(
  const Aafp: TArrayOfArrayOfFixedPoint; PolyType: TPolyType): Boolean;
var
  Index: Integer;
  Poly: TPolygon;
begin
  Result := False;
  for Index := 0 to High(Aafp) do
  begin
    Poly := ArrayOfFixedPointToPolygon(Aafp[Index]);
    if AddPolygon(Poly, PolyType) then Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfFloatPoint(
  const Afp: TArrayOfFloatPoint;
  PolyType: TPolyType): Boolean;
var
  Poly: TPolygon;
begin
  Poly := ArrayOfFloatPointToPolygon(Afp);
  Result := AddPolygon(Poly, PolyType);
end;
//------------------------------------------------------------------------------


function TClipper32.AddArrayOfArrayOfFloatPoint(
  const Aafp: TArrayOfArrayOfFloatPoint;
  PolyType: TPolyType): Boolean;
var
  Index: Integer;
  Poly: TPolygon;
begin
  Result := False;
  for Index := 0 to High(Aafp) do
  begin
    Poly := ArrayOfFloatPointToPolygon(Aafp[Index]);
    if AddPolygon(Poly, PolyType) then Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper32.Execute(ClipType: TClipType;
  out Solution: TArrayOfArrayOfFixedPoint;
  SubjFillType: TPolyFillType = pftEvenOdd;
  ClipFillType: TPolyFillType = pftEvenOdd): Boolean;
var
  Polys: TPolygons;
begin
  Result := Execute(ClipType, Polys, SubjFillType, ClipFillType);
  Solution := PolygonsToArrayOfArrayOfFixedPoint(Polys);
end;
//------------------------------------------------------------------------------

function TClipper32.Execute(
  ClipType: TClipType;
  out Solution: TArrayOfArrayOfFloatPoint;
  SubjFillType: TPolyFillType = pftEvenOdd;
  ClipFillType: TPolyFillType = pftEvenOdd): Boolean;
var
  Polys: TPolygons;
begin
  Result := Execute(ClipType, Polys, SubjFillType, ClipFillType);
  Solution := PolygonsToArrayOfArrayOfFloatPoint(Polys);
end;
//------------------------------------------------------------------------------

function OffsetPolygons32(const Aafp: TArrayOfArrayOfFixedPoint;
  const Delta: Double; JoinType: TJoinType = jtSquare;
  MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint;
var
  Polys: TPolygons;
begin
  Polys := ArrayOfArrayOfFixedPointToPolygons(Aafp);
  Polys := OffsetPolygons(Polys, Delta * 65536, JoinType, MiterLimit);
  Result := PolygonsToArrayOfArrayOfFixedPoint(Polys);
end;
//------------------------------------------------------------------------------

function OffsetPolygons32(const Aafp: TArrayOfArrayOfFloatPoint;
  const Delta: Double; JoinType: TJoinType = jtSquare;
  MiterLimit: Double = 2): TArrayOfArrayOfFloatPoint; overload;
var
  Polys: TPolygons;
begin
  Polys := ArrayOfArrayOfFloatPointToPolygons(Aafp);
  Polys := OffsetPolygons(Polys, Delta * 65536, JoinType, MiterLimit);
  Result := PolygonsToArrayOfArrayOfFloatPoint(Polys);
end;
//------------------------------------------------------------------------------

function ArrayOfFixedPointToPolygon(const ArrFxPt: TArrayOfFixedPoint): TPolygon;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(ArrFxPt);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
  begin
    Result[Index].X := ArrFxPt[Index].X;
    Result[Index].Y := ArrFxPt[Index].Y;
  end;
end;
//------------------------------------------------------------------------------

function PolygonToArrayOfFixedPoint(const Poly: TPolygon): TArrayOfFixedPoint;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(Poly);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
  begin
    Result[Index].X := Poly[Index].X;
    Result[Index].Y := Poly[Index].Y;
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfArrayOfFixedPointToPolygons(const ArrArrFxPt:
  TArrayOfArrayOfFixedPoint): TPolygons;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(ArrArrFxPt);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
    Result[Index] := ArrayOfFixedPointToPolygon(ArrArrFxPt[Index]);
end;
//------------------------------------------------------------------------------

function PolygonsToArrayOfArrayOfFixedPoint(
  const Polys: TPolygons): TArrayOfArrayOfFixedPoint;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(Polys);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
    Result[Index] := PolygonToArrayOfFixedPoint(Polys[Index]);
end;
//------------------------------------------------------------------------------

function ArrayOfFloatPointToPolygon(
  const ArrFltPt: TArrayOfFloatPoint): TPolygon;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(ArrFltPt);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
  begin
    Result[Index].X := Round(ArrFltPt[Index].X * 65536);
    Result[Index].Y := Round(ArrFltPt[Index].Y * 65536);
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfArrayOfFloatPointToPolygons(
  const ArrArrFltPt: TArrayOfArrayOfFloatPoint): TPolygons;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(ArrArrFltPt);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
    Result[Index] := ArrayOfFloatPointToPolygon(ArrArrFltPt[Index]);
end;
//------------------------------------------------------------------------------

function PolygonToArrayOfFloatPoint(
  const Poly: TPolygon): TArrayOfFloatPoint;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(Poly);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
  begin
    Result[Index].X := Poly[Index].X / 65536;
    Result[Index].Y := Poly[Index].Y / 65536;
  end;
end;
//------------------------------------------------------------------------------

function PolygonsToArrayOfArrayOfFloatPoint(
  const Polys: TPolygons): TArrayOfArrayOfFloatPoint;
var
  Index, VertexCount: Integer;
begin
  VertexCount := Length(Polys);
  SetLength(Result, VertexCount);
  for Index := 0 to VertexCount - 1 do
    Result[Index] := PolygonToArrayOfFloatPoint(Polys[Index]);
end;
//------------------------------------------------------------------------------

end.
