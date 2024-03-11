unit GR32_VectorUtils.Clipper2;

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
  Math, {$IFDEF FPC}Types, {$ENDIF} {$IFDEF COMPILERXE2_UP}Types, {$ENDIF}
  GR32,
  GR32_VectorUtils,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
//      Grow and BuildPoly*line replacements using Clipper2
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//
//      Grow
//
//------------------------------------------------------------------------------
function GrowClipper(const Points: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;
function GrowClipper(const Points: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle = jsMiter; Closed: Boolean = True; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload;

//------------------------------------------------------------------------------
//
//      BuildPoly*line
//
//------------------------------------------------------------------------------
function BuildPolylineClipper(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfFloatPoint; overload;
function BuildPolyPolyLineClipper(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = DEFAULT_MITER_LIMIT): TArrayOfArrayOfFloatPoint; overload;
function BuildPolylineClipper(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfFixedPoint; overload;
function BuildPolyPolyLineClipper(const Points: TArrayOfArrayOfFixedPoint; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = DEFAULT_MITER_LIMIT_FIXED): TArrayOfArrayOfFixedPoint; overload;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils,

  Clipper,
  Clipper.Core,
  Clipper.Offset,
  GR32_Clipper2,

  GR32_Math,
  GR32_Geometry,
  GR32_LowLevel;

const
  JoinStyleToJoinType: array[TJoinStyle] of TJoinType = (jtMiter, jtBevel, jtRound, jtSquare);
  EndStyleToEndType: array[TEndStyle] of TEndType = (etButt, etSquare, etRound);


//------------------------------------------------------------------------------
//
//      Grow
//
//------------------------------------------------------------------------------
function GrowClipper(const Points: TPaths64; const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TPaths64; overload;
var
  EndType: TEndType;
begin
  if (Closed) then
    EndType := etPolygon
  else
    EndType := etJoined;

  Result := Clipper.InflatePaths(Points, Delta, JoinStyleToJoinType[JoinStyle], EndType, MiterLimit);

  Result := Clipper.Core.RamerDouglasPeucker(Result, 1);
end;

function GrowClipper(const Points: TArrayOfFloatPoint; const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  Points64, Result64: TPaths64;
  Res: TArrayOfArrayOfFloatPoint;
begin
  Points64 := [GR32_Clipper2.FloatPointsToPath64(Points)];

  Result64 := GrowClipper(Points64, Delta * GR32_Clipper2.ClipperFloat.GrowScale, JoinStyle, Closed, MiterLimit);

  Res := GR32_Clipper2.Paths64ToFloatPoints(Result64);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;

function GrowClipper(const Points: TArrayOfFixedPoint; const Delta: TFixed; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFixed): TArrayOfFixedPoint;
var
  Points64, Result64: TPaths64;
  Res: TArrayOfArrayOfFixedPoint;
begin
  Points64 := [GR32_Clipper2.FixedPointsToPath64(Points)];

  Result64 := GrowClipper(Points64, Delta * FixedToFloat, JoinStyle, Closed, MiterLimit);

  Res := GR32_Clipper2.Paths64ToFixedPoints(Result64);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;


//------------------------------------------------------------------------------
//
//      BuildPoly*line
//
//------------------------------------------------------------------------------
function BuildPolylineClipper(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat; JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfFloatPoint;
var
  Paths64: TPaths64;
  EndType: TEndType;
  Result64: TPaths64;
  Res: TArrayOfArrayOfFloatPoint;
begin
  Paths64 := [GR32_Clipper2.FloatPointsToPath64(Points)];

  EndType := EndStyleToEndType[EndStyle];

  Result64 := Clipper.InflatePaths(Paths64, StrokeWidth * GR32_Clipper2.ClipperFloat.GrowScale, JoinStyleToJoinType[JoinStyle], EndType, MiterLimit);
  Result64 := Clipper.Core.RamerDouglasPeucker(Result64, 1);

  Res := GR32_Clipper2.Paths64ToFloatPoints(Result64);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;

function BuildPolyPolyLineClipper(const Points: TArrayOfArrayOfFloatPoint; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  Paths64: TPaths64;
  EndType: TEndType;
  Result64: TPaths64;
begin
  Paths64 := GR32_Clipper2.FloatPointsToPaths64(Points);

  if (Closed) then
    EndType := etJoined
  else
    EndType := EndStyleToEndType[EndStyle];

  Result64 := Clipper.InflatePaths(Paths64, StrokeWidth * GR32_Clipper2.ClipperFloat.GrowScale, JoinStyleToJoinType[JoinStyle], EndType, MiterLimit);

  Result64 := Clipper.Core.RamerDouglasPeucker(Result64, 1);

  Result := GR32_Clipper2.Paths64ToFloatPoints(Result64);
end;

function BuildPolylineClipper(const Points: TArrayOfFixedPoint; StrokeWidth: TFixed; JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFixed): TArrayOfFixedPoint;
var
  Paths64: TPaths64;
  Result64: TPaths64;
  Res: TArrayOfArrayOfFixedPoint;
begin
  Paths64 := [GR32_Clipper2.FixedPointsToPath64(Points)];

  Result64 := Clipper.InflatePaths(Paths64, StrokeWidth * GR32_Clipper2.ClipperFloat.GrowScale, JoinStyleToJoinType[JoinStyle], EndStyleToEndType[EndStyle], MiterLimit);
  Result64 := Clipper.Core.RamerDouglasPeucker(Result64, 1);

  Res := GR32_Clipper2.Paths64ToFixedPoints(Result64);

  if (Length(Res) > 0) then
    Result := Res[0]
  else
    SetLength(Result, 0);
end;

function BuildPolyPolyLineClipper(const Points: TArrayOfArrayOfFixedPoint; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFixed): TArrayOfArrayOfFixedPoint;
var
  Paths64: TPaths64;
  EndType: TEndType;
  Result64: TPaths64;
begin
  Paths64 := GR32_Clipper2.FixedPointsToPaths64(Points);

  if (Closed) then
    EndType := etJoined
  else
    EndType := EndStyleToEndType[EndStyle];

  Result64 := Clipper.InflatePaths(Paths64, StrokeWidth * GR32_Clipper2.ClipperFloat.GrowScale, JoinStyleToJoinType[JoinStyle], EndType, MiterLimit);
  Result64 := Clipper.Core.RamerDouglasPeucker(Result64, 1);

  Result := GR32_Clipper2.Paths64ToFixedPoints(Result64);
end;

end.
