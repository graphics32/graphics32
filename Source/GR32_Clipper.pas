unit GR32_Clipper;

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

{$I GR32.inc}

uses
  Classes,
  SysUtils,
  Math,
  GR32_Clipper2,
  GR32_Polygons,
  Clipper.Core,
  Clipper.Engine,
  Clipper.Offset,
  GR32;

const
  // TPathType
  ptSubject     = Clipper.Engine.ptSubject;
  ptClip        = Clipper.Engine.ptClip;

const
  // TClipType
  ctNone        = Clipper.Core.ctNone;
  ctIntersection= Clipper.Core.ctIntersection;
  ctUnion       = Clipper.Core.ctUnion;
  ctDifference  = Clipper.Core.ctDifference;
  ctXor         = Clipper.Core.ctXor;

const
  // TFillRule
  frEvenOdd     = Clipper.Core.frEvenOdd;
  frNonZero     = Clipper.Core.frNonZero;
  frPositive    = Clipper.Core.frPositive;
  frNegative    = Clipper.Core.frNegative;

type
  TPathType = Clipper.Engine.TPathType;
  TClipType = Clipper.Core.TClipType;
  TFillRule = Clipper.Core.TFillRule;

type
  TClipper = class(TClipper64)
  private
  protected
  public
    function GetBounds: TFloatRect;

    // ADDPATH & ADDPATHS METHODS ...
    procedure AddPath(const Path: TArrayOfFloatPoint; PolyType: TPathType = ptSubject; IsOpen: Boolean = False); overload;
    procedure AddPath(const Path: TArrayOfFixedPoint; PolyType: TPathType = ptSubject; IsOpen: Boolean = False); overload;
    procedure AddPaths(const Paths: TArrayOfArrayOfFloatPoint; PolyType: TPathType = ptSubject; IsOpen: Boolean = False); overload;
    procedure AddPaths(const Paths: TArrayOfArrayOfFixedPoint; PolyType: TPathType = ptSubject; IsOpen: Boolean = False); overload;

    // EXECUTE METHODS ...
    function Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
    function Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths, OpenPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths, OpenPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
  end;

  TClipper32 = TClipper; // Can be used to avoid ambiguity with Clipper's own TClipper class.


const
  // TJoinType
  jtBevel       = Clipper.Offset.jtBevel;
  jtSquare      = Clipper.Offset.jtSquare;
  jtRound       = Clipper.Offset.jtRound;
  jtRoundEx     = Clipper.Offset.jtRound; // Not implemented in Clipper2
  jtMiter       = Clipper.Offset.jtMiter;

const
  // TEndType
  etPolygon     = Clipper.Offset.etPolygon;
  etOpenJoined  = Clipper.Offset.etJoined;
  etOpenButt    = Clipper.Offset.etButt;
  etOpenSquare  = Clipper.Offset.etSquare;
  etOpenRound   = Clipper.Offset.etRound;

type
  TJoinType = Clipper.Offset.TJoinType;
  TEndType = Clipper.Offset.TEndType;

type
  TClipperOffset = class(Clipper.Offset.TClipperOffset)
  private
  public
    procedure AddPath(const Path: TArrayOfFloatPoint); overload; deprecated 'Use AddPath(path, joinType, endType)';
    procedure AddPath(const Path: TArrayOfFloatPoint; JoinType: TJoinType; EndType: TEndType); overload;

    procedure AddPaths(const Paths: TArrayOfArrayOfFloatPoint); overload; deprecated 'Use AddPaths(paths, joinType, endType)';
    procedure AddPaths(const Paths: TArrayOfArrayOfFloatPoint; JoinType: TJoinType; EndType: TEndType); overload;

    procedure Execute(Delta: Double; jt: TJoinType; et: TEndType; out Solution: TArrayOfArrayOfFloatPoint); overload; deprecated 'Use Execute(delta)';
    function Execute(Delta: Double): TArrayOfArrayOfFloatPoint; overload;
  end;

function InflatePaths(const Paths: GR32.TArrayOfArrayOfFixedPoint;
  Delta: double; jointType: TJoinType; EndType: TEndType;
  MiterLimit: double = 2): GR32.TArrayOfArrayOfFixedPoint; overload;

function InflatePaths(const Paths: GR32.TArrayOfArrayOfFloatPoint;
  Delta: double; jointType: TJoinType; EndType: TEndType;
  MiterLimit: double = 2): GR32.TArrayOfArrayOfFloatPoint; overload;

implementation

uses
  Clipper;

//------------------------------------------------------------------------------

function InflatePaths(const Paths: GR32.TArrayOfArrayOfFixedPoint;
  Delta: double; jointType: TJoinType; EndType: TEndType;
  MiterLimit: double): GR32.TArrayOfArrayOfFixedPoint;
var
  sub, sol: TPaths64;
begin
  sub := GR32_Clipper2.FixedPointsToPaths64(Paths);
  sol := Clipper.InflatePaths(sub, Delta * ClipperFloat.FixedGrowScale, jointType, EndType, MiterLimit);
  sol := Clipper.Core.RamerDouglasPeucker(sol, 10);
  Result := GR32_Clipper2.Paths64ToFixedPoints(sol);
end;

function InflatePaths(const Paths: GR32.TArrayOfArrayOfFloatPoint;
  Delta: double; jointType: TJoinType; EndType: TEndType;
  MiterLimit: double): GR32.TArrayOfArrayOfFloatPoint;
var
  sub, sol: TPaths64;
begin
  sub := GR32_Clipper2.FloatPointsToPaths64(Paths);
  sol := Clipper.InflatePaths(sub, Delta * ClipperFloat.GrowScale, jointType, EndType, MiterLimit);
  sol := Clipper.Core.RamerDouglasPeucker(sol, 10);
  Result := GR32_Clipper2.Paths64ToFloatPoints(sol);
end;



//------------------------------------------------------------------------------
//  TClipper methods ...
//------------------------------------------------------------------------------
procedure TClipper.AddPath(const Path: TArrayOfFloatPoint; PolyType: TPathType; IsOpen: Boolean);
var
  Path64: TPath64;
begin
  Path64 := GR32_Clipper2.FloatPointsToPath64(Path);
  inherited AddPath(Path64, PolyType, IsOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPath(const Path: TArrayOfFixedPoint; PolyType: TPathType; IsOpen: Boolean);
var
  Path64: TPath64;
begin
  Path64 := GR32_Clipper2.FixedPointsToPath64(Path);
  inherited AddPath(Path64, PolyType, IsOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const Paths: TArrayOfArrayOfFloatPoint; PolyType: TPathType; IsOpen: Boolean);
var
  Paths64: TPaths64;
begin
  Paths64 := GR32_Clipper2.FloatPointsToPaths64(Paths);
  inherited AddPaths(Paths64, PolyType, IsOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const Paths: TArrayOfArrayOfFixedPoint; PolyType: TPathType; IsOpen: Boolean);
var
  Paths64: TPaths64;
begin
  Paths64 := GR32_Clipper2.FixedPointsToPaths64(Paths);
  inherited AddPaths(Paths64, PolyType, IsOpen);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  ClosedSolutions: TPaths64;
begin
  Result := inherited Execute(ClipType, FillRule, ClosedSolutions);
  if (Result) then
    ClosedPaths := GR32_Clipper2.Paths64ToFloatPoints(ClosedSolutions)
  else
    SetLength(ClosedPaths, 0);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  ClosedSolutions: TPaths64;
begin
  Result := inherited Execute(ClipType, FillRule, ClosedSolutions);
  if (Result) then
    ClosedPaths := GR32_Clipper2.Paths64ToFixedPoints(ClosedSolutions)
  else
    SetLength(ClosedPaths, 0);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths, OpenPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  ClosedSolutions, OpenSolutions: TPaths64;
begin
  Result := inherited Execute(ClipType, FillRule, ClosedSolutions, OpenSolutions);
  if (Result) then
  begin
    ClosedPaths := GR32_Clipper2.Paths64ToFloatPoints(ClosedSolutions);
    OpenPaths := GR32_Clipper2.Paths64ToFloatPoints(OpenSolutions);
  end else
  begin
    SetLength(ClosedPaths, 0);
    SetLength(OpenPaths, 0);
  end;
end;

//------------------------------------------------------------------------------

function TClipper.Execute(ClipType: TClipType; FillRule: TFillRule; out ClosedPaths, OpenPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  ClosedSolutions, OpenSolutions: TPaths64;
begin
  Result := inherited Execute(ClipType, FillRule, ClosedSolutions, OpenSolutions);
  if (Result) then
  begin
    ClosedPaths := GR32_Clipper2.Paths64ToFixedPoints(ClosedSolutions);
    OpenPaths := GR32_Clipper2.Paths64ToFixedPoints(OpenSolutions);
  end else
  begin
    SetLength(ClosedPaths, 0);
    SetLength(OpenPaths, 0);
  end;
end;

//------------------------------------------------------------------------------

function TClipper.GetBounds: TFloatRect;
begin
  Result := GR32_Clipper2.FloatRect(inherited GetBounds);
end;


//------------------------------------------------------------------------------
//  TClipperOffset methods ...
//------------------------------------------------------------------------------
procedure TClipperOffset.AddPath(const Path: TArrayOfFloatPoint; JoinType: TJoinType; EndType: TEndType);
var
  Path64: TPath64;
begin
  Path64 := GR32_Clipper2.FloatPointsToPath64(Path);
  inherited AddPath(Path64, JoinType, EndType);
end;

procedure TClipperOffset.AddPath(const Path: TArrayOfFloatPoint);
begin
  AddPath(Path, jtRound, etPolygon);
end;

//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const Paths: TArrayOfArrayOfFloatPoint; JoinType: TJoinType; EndType: TEndType);
var
  Paths64: TPaths64;
begin
  Paths64 := GR32_Clipper2.FloatPointsToPaths64(Paths);
  inherited AddPaths(Paths64, JoinType, EndType);
end;

procedure TClipperOffset.AddPaths(const Paths: TArrayOfArrayOfFloatPoint);
begin
  AddPaths(Paths, jtRound, etPolygon);
end;

//------------------------------------------------------------------------------

function TClipperOffset.Execute(Delta: Double): TArrayOfArrayOfFloatPoint;
var
  Paths64: TPaths64;
begin
  inherited Execute(Delta, Paths64);
  Result := GR32_Clipper2.Paths64ToFloatPoints(Paths64);
end;

procedure TClipperOffset.Execute(Delta: Double; jt: TJoinType; et: TEndType; out Solution: TArrayOfArrayOfFloatPoint);
begin
  Solution := Execute(Delta);
end;

//------------------------------------------------------------------------------

end.

