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
  Classes, Gr32, Clipper;

type
  TClipper32 = class(TClipper)
    function AddArrayOfFixedPoint(
      const afp: TArrayOfFixedPoint;
      polyType: TPolyType): boolean;
    function AddArrayOfArrayOfFixedPoint(
      const aafp: TArrayOfArrayOfFixedPoint;
      polyType: TPolyType): boolean;

    function AddArrayOfFloatPoint(
      const afp: TArrayOfFloatPoint;
      polyType: TPolyType): boolean;
    function AddArrayOfArrayOfFloatPoint(
      const aafp: TArrayOfArrayOfFloatPoint;
      polyType: TPolyType): boolean;

    function Execute(
      clipType: TClipType;
      out solution: TArrayOfArrayOfFixedPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): boolean; overload;
    function Execute(
      clipType: TClipType;
      out solution: TArrayOfArrayOfFloatPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): boolean; overload;
  end;

  function OffsetPolygons32(const aafp: TArrayOfArrayOfFixedPoint;
    const delta: double; JoinType: TJoinType = jtSquare;
    MiterLimit: double = 2): TArrayOfArrayOfFixedPoint; overload;

  function OffsetPolygons32(const aafp: TArrayOfArrayOfFloatPoint;
    const delta: double; JoinType: TJoinType = jtSquare;
    MiterLimit: double = 2): TArrayOfArrayOfFloatPoint; overload;

  function ArrayOfFixedPointToPolygon(
    const ArrFxPt: TArrayOfFixedPoint): TPolygon;
  function ArrayOfArrayOfFixedPointToPolygons(
    const ArrArrFxPt: TArrayOfArrayOfFixedPoint): TPolygons;
  function PolygonToArrayOfFixedPoint(
    const poly: TPolygon): TArrayOfFixedPoint;
  function PolygonsToArrayOfArrayOfFixedPoint(
    const polys: TPolygons): TArrayOfArrayOfFixedPoint;

  function ArrayOfFloatPointToPolygon(
    const ArrFltPt: TArrayOfFloatPoint): TPolygon;
  function ArrayOfArrayOfFloatPointToPolygons(
    const ArrArrFltPt: TArrayOfArrayOfFloatPoint): TPolygons;
  function PolygonToArrayOfFloatPoint(
    const poly: TPolygon): TArrayOfFloatPoint;
  function PolygonsToArrayOfArrayOfFloatPoint(
    const polys: TPolygons): TArrayOfArrayOfFloatPoint;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfFixedPoint(const afp: TArrayOfFixedPoint;
  polyType: TPolyType): boolean;
var
  poly: TPolygon;
begin
  poly := ArrayOfFixedPointToPolygon(afp);
  result := AddPolygon(poly, polyType);
end;
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfArrayOfFixedPoint(
  const aafp: TArrayOfArrayOfFixedPoint; polyType: TPolyType): boolean;
var
  i: integer;
  poly: TPolygon;
begin
  result := false;
  for i := 0 to high(aafp) do
  begin
    poly := ArrayOfFixedPointToPolygon(aafp[i]);
    if AddPolygon(poly, polyType) then result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper32.AddArrayOfFloatPoint(
  const afp: TArrayOfFloatPoint;
  polyType: TPolyType): boolean;
var
  poly: TPolygon;
begin
  poly := ArrayOfFloatPointToPolygon(afp);
  result := AddPolygon(poly, polyType);
end;
//------------------------------------------------------------------------------


function TClipper32.AddArrayOfArrayOfFloatPoint(
  const aafp: TArrayOfArrayOfFloatPoint;
  polyType: TPolyType): boolean;
var
  i: integer;
  poly: TPolygon;
begin
  result := false;
  for i := 0 to high(aafp) do
  begin
    poly := ArrayOfFloatPointToPolygon(aafp[i]);
    if AddPolygon(poly, polyType) then result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper32.Execute(clipType: TClipType;
  out solution: TArrayOfArrayOfFixedPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): boolean;
var
  polys: TPolygons;
begin
  result := Execute(clipType, polys, subjFillType, clipFillType);
  solution := PolygonsToArrayOfArrayOfFixedPoint(polys);
end;
//------------------------------------------------------------------------------

function TClipper32.Execute(
  clipType: TClipType;
  out solution: TArrayOfArrayOfFloatPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): boolean;
var
  polys: TPolygons;
begin
  result := Execute(clipType, polys, subjFillType, clipFillType);
  solution := PolygonsToArrayOfArrayOfFloatPoint(polys);
end;
//------------------------------------------------------------------------------

function OffsetPolygons32(const aafp: TArrayOfArrayOfFixedPoint;
  const delta: double; JoinType: TJoinType = jtSquare;
  MiterLimit: double = 2): TArrayOfArrayOfFixedPoint;
var
  polys: TPolygons;
begin
  polys := ArrayOfArrayOfFixedPointToPolygons(aafp);
  polys := OffsetPolygons(polys, delta * 65536, JoinType, MiterLimit);
  result := PolygonsToArrayOfArrayOfFixedPoint(polys);
end;
//------------------------------------------------------------------------------

function OffsetPolygons32(const aafp: TArrayOfArrayOfFloatPoint;
  const delta: double; JoinType: TJoinType = jtSquare;
  MiterLimit: double = 2): TArrayOfArrayOfFloatPoint; overload;
var
  polys: TPolygons;
begin
  polys := ArrayOfArrayOfFloatPointToPolygons(aafp);
  polys := OffsetPolygons(polys, delta * 65536, JoinType, MiterLimit);
  result := PolygonsToArrayOfArrayOfFloatPoint(polys);
end;
//------------------------------------------------------------------------------

function ArrayOfFixedPointToPolygon(const ArrFxPt: TArrayOfFixedPoint): TPolygon;
var
  i,len: integer;
begin
  len := length(ArrFxPt);
  setlength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ArrFxPt[i].X;
    Result[i].Y := ArrFxPt[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function PolygonToArrayOfFixedPoint(const poly: TPolygon): TArrayOfFixedPoint;
var
  i,len: integer;
begin
  len := length(poly);
  setlength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := poly[i].X;
    Result[i].Y := poly[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfArrayOfFixedPointToPolygons(const ArrArrFxPt:
  TArrayOfArrayOfFixedPoint): TPolygons;
var
  i,len: integer;
begin
  len := length(ArrArrFxPt);
  setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := ArrayOfFixedPointToPolygon(ArrArrFxPt[i]);
end;
//------------------------------------------------------------------------------

function PolygonsToArrayOfArrayOfFixedPoint(
  const polys: TPolygons): TArrayOfArrayOfFixedPoint;
var
  i,len: integer;
begin
  len := length(polys);
  setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := PolygonToArrayOfFixedPoint(polys[i]);
end;
//------------------------------------------------------------------------------

function ArrayOfFloatPointToPolygon(
  const ArrFltPt: TArrayOfFloatPoint): TPolygon;
var
  i,len: integer;
begin
  len := length(ArrFltPt);
  setlength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := round(ArrFltPt[i].X * 65536);
    Result[i].Y := round(ArrFltPt[i].Y * 65536);
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfArrayOfFloatPointToPolygons(
  const ArrArrFltPt: TArrayOfArrayOfFloatPoint): TPolygons;
var
  i,len: integer;
begin
  len := length(ArrArrFltPt);
  setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := ArrayOfFloatPointToPolygon(ArrArrFltPt[i]);
end;
//------------------------------------------------------------------------------

function PolygonToArrayOfFloatPoint(
  const poly: TPolygon): TArrayOfFloatPoint;
var
  i,len: integer;
begin
  len := length(poly);
  setlength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := poly[i].X / 65536;
    Result[i].Y := poly[i].Y / 65536;
  end;
end;
//------------------------------------------------------------------------------

function PolygonsToArrayOfArrayOfFloatPoint(
  const polys: TPolygons): TArrayOfArrayOfFloatPoint;
var
  i,len: integer;
begin
  len := length(polys);
  setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := PolygonToArrayOfFloatPoint(polys[i]);
end;
//------------------------------------------------------------------------------

end.
