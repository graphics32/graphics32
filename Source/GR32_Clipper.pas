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
    procedure AddPath(const path: TArrayOfFloatPoint; polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;
    procedure AddPath(const path: TArrayOfFixedPoint; polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;
    procedure AddPaths(const paths: TArrayOfArrayOfFloatPoint; polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;
    procedure AddPaths(const paths: TArrayOfArrayOfFixedPoint; polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;

    // EXECUTE METHODS ...
    function Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths, openPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths, openPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
  end;


const
  // TJoinType
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
    procedure AddPath(const path: TArrayOfFloatPoint); overload; deprecated 'Use AddPath(path, joinType, endType)';
    procedure AddPath(const path: TArrayOfFloatPoint; joinType: TJoinType; endType: TEndType); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure AddPaths(const paths: TArrayOfArrayOfFloatPoint); overload; deprecated 'Use AddPaths(paths, joinType, endType)';
    procedure AddPaths(const paths: TArrayOfArrayOfFloatPoint; joinType: TJoinType; endType: TEndType); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure Execute(delta: Double; jt: TJoinType; et: TEndType; out solution: TArrayOfArrayOfFloatPoint); overload; deprecated 'Use Execute(delta)';
    function Execute(delta: Double): TArrayOfArrayOfFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  end;

function InflatePaths(const paths: Gr32.TArrayOfArrayOfFixedPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double = 2): Gr32.TArrayOfArrayOfFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

function InflatePaths(const paths: Gr32.TArrayOfArrayOfFloatPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double = 2): Gr32.TArrayOfArrayOfFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

implementation

uses
  Clipper;

//------------------------------------------------------------------------------

function InflatePaths(const paths: Gr32.TArrayOfArrayOfFixedPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double): Gr32.TArrayOfArrayOfFixedPoint;
var
  sub, sol: TPaths64;
begin
  sub := GR32_Clipper2.FixedPointsToPaths64(paths);
  sol := Clipper.InflatePaths(sub, delta * FixedOne * 0.5, jointType, endType, miterLimit);
  sol := Clipper.Core.RamerDouglasPeucker(sol, 10);
  Result := GR32_Clipper2.Paths64ToFixedPoints(sol);
end;

function InflatePaths(const paths: Gr32.TArrayOfArrayOfFloatPoint;
  delta: double; jointType: TJoinType; endType: TEndType;
  miterLimit: double): Gr32.TArrayOfArrayOfFloatPoint;
var
  sub, sol: TPaths64;
begin
  sub := GR32_Clipper2.FloatPointsToPaths64(paths);
  sol := Clipper.InflatePaths(sub, delta * ClipperFloatScale * 0.5, jointType, endType, miterLimit);
  sol := Clipper.Core.RamerDouglasPeucker(sol, 10);
  Result := GR32_Clipper2.Paths64ToFloatPoints(sol);
end;



//------------------------------------------------------------------------------
//  TClipper methods ...
//------------------------------------------------------------------------------
procedure TClipper.AddPath(const path: TArrayOfFloatPoint; polyType: TPathType; isOpen: Boolean);
var
  path64: TPath64;
begin
  path64 := GR32_Clipper2.FloatPointsToPath64(path);
  inherited AddPath(path64, polyType, isOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPath(const path: TArrayOfFixedPoint; polyType: TPathType; isOpen: Boolean);
var
  path64: TPath64;
begin
  path64 := GR32_Clipper2.FixedPointsToPath64(path);
  inherited AddPath(path64, polyType, isOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths: TArrayOfArrayOfFloatPoint; polyType: TPathType; isOpen: Boolean);
var
  paths64: TPaths64;
begin
  paths64 := GR32_Clipper2.FloatPointsToPaths64(paths);
  inherited AddPaths(paths64, polyType, isOpen);
end;

//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths: TArrayOfArrayOfFixedPoint; polyType: TPathType; isOpen: Boolean);
var
  paths64: TPaths64;
begin
  paths64 := GR32_Clipper2.FixedPointsToPaths64(paths);
  inherited AddPaths(paths64, polyType, isOpen);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  closedSolutions: TPaths64;
begin
  Result := inherited Execute(clipType, fillRule, closedSolutions);
  if (Result) then
    closedPaths := GR32_Clipper2.Paths64ToFloatPoints(closedSolutions)
  else
    SetLength(closedPaths, 0);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  closedSolutions: TPaths64;
begin
  Result := inherited Execute(clipType, fillRule, closedSolutions);
  if (Result) then
    closedPaths := GR32_Clipper2.Paths64ToFixedPoints(closedSolutions)
  else
    SetLength(closedPaths, 0);
end;

//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths, openPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  closedSolutions, openSolutions: TPaths64;
begin
  Result := inherited Execute(clipType, fillRule, closedSolutions, openSolutions);
  if (Result) then
  begin
    closedPaths := GR32_Clipper2.Paths64ToFloatPoints(closedSolutions);
    openPaths := GR32_Clipper2.Paths64ToFloatPoints(openSolutions);
  end else
  begin
    SetLength(closedPaths, 0);
    SetLength(openPaths, 0);
  end;
end;

//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule; out closedPaths, openPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  closedSolutions, openSolutions: TPaths64;
begin
  Result := inherited Execute(clipType, fillRule, closedSolutions, openSolutions);
  if (Result) then
  begin
    closedPaths := GR32_Clipper2.Paths64ToFixedPoints(closedSolutions);
    openPaths := GR32_Clipper2.Paths64ToFixedPoints(openSolutions);
  end else
  begin
    SetLength(closedPaths, 0);
    SetLength(openPaths, 0);
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
procedure TClipperOffset.AddPath(const path: TArrayOfFloatPoint; joinType: TJoinType; endType: TEndType);
var
  path64: TPath64;
begin
  path64 := GR32_Clipper2.FloatPointsToPath64(path);
  inherited AddPath(path64, joinType, endType);
end;

procedure TClipperOffset.AddPath(const path: TArrayOfFloatPoint);
begin
  AddPath(path, jtRound, etPolygon);
end;

//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const paths: TArrayOfArrayOfFloatPoint; joinType: TJoinType; endType: TEndType);
var
  paths64: TPaths64;
begin
  paths64 := GR32_Clipper2.FloatPointsToPaths64(paths);
  inherited AddPaths(paths64, joinType, endType);
end;

procedure TClipperOffset.AddPaths(const paths: TArrayOfArrayOfFloatPoint);
begin
  AddPaths(paths, jtRound, etPolygon);
end;

//------------------------------------------------------------------------------

function TClipperOffset.Execute(delta: Double): TArrayOfArrayOfFloatPoint;
var
  paths64: TPaths64;
begin
  inherited Execute(delta, paths64);
  Result := GR32_Clipper2.Paths64ToFloatPoints(paths64);
end;

procedure TClipperOffset.Execute(delta: Double; jt: TJoinType; et: TEndType; out solution: TArrayOfArrayOfFloatPoint);
begin
  solution := Execute(delta);
end;

//------------------------------------------------------------------------------

end.

