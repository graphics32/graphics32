unit GR32_ArrowHeads;

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
 * Angus Johnson < http://www.angusj.com >
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  SysUtils, GR32, GR32_Polygons, GR32_VectorUtils, GR32_Geometry;

type
  TArrowHeadAbstract = class
  private
    FSize: TFloat;
    FTipPoint: TFloatPoint;
    FBasePoint: TFloatPoint;
  protected
    function GetPointsInternal: TArrayOfFloatPoint; virtual; abstract;
  public
    constructor Create(size: TFloat); virtual;
    function GetPoints(const Line: TArrayOfFloatPoint; AtEnd: Boolean): TArrayOfFloatPoint;
    //Size: distance between arrow tip and arrow base
    property Size: TFloat read FSize write FSize;
  end;

  TArrowHeadSimple = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;

  TArrowHeadFourPt = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;

  TArrowHeadCircle = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;

  TArrowHeadDiamond = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;


resourcestring
  RCStrInsufficientPointsInArray = 'Insufficient points in array';

implementation

constructor TArrowHeadAbstract.Create(Size: TFloat);
begin
  FSize := Size;
end;
//------------------------------------------------------------------------------

function TArrowHeadAbstract.GetPoints(const Line: TArrayOfFloatPoint;
  AtEnd: Boolean): TArrayOfFloatPoint;
var
  HighI: Integer;
  UnitVec: TFloatPoint;
begin
  HighI := high(Line);
  if HighI < 1 then
    raise exception.create(RCStrInsufficientPointsInArray);

  if AtEnd then
  begin
    FBasePoint := Line[HighI];
    UnitVec := GetUnitVector(Line[HighI -1], Line[HighI]);
  end else
  begin
    FBasePoint := Line[0];
    UnitVec := GetUnitVector(Line[1], Line[0]);
  end;
  FTipPoint := OffsetPoint(FBasePoint, UnitVec.X * FSize, UnitVec.Y * FSize);
  Result := GetPointsInternal;
end;
//------------------------------------------------------------------------------

function TArrowHeadSimple.GetPointsInternal: TArrayOfFloatPoint;
var
  UnitNorm: TFloatPoint;
  Sz: Single;
begin
  SetLength(Result, 3);
  UnitNorm := GetUnitNormal(FTipPoint, FBasePoint);
  Sz := FSize * 0.5;
  Result[0] := FTipPoint;
  Result[1] := OffsetPoint(FBasePoint, UnitNorm.X *Sz, UnitNorm.Y *Sz);
  Result[2] := OffsetPoint(FBasePoint, -UnitNorm.X *Sz, -UnitNorm.Y *Sz);
end;
//------------------------------------------------------------------------------

function TArrowHeadFourPt.GetPointsInternal: TArrayOfFloatPoint;
var
  Angle: Double;
begin
  SetLength(Result, 4);
  Result[0] := FTipPoint;
  Angle := GetAngleOfPt2FromPt1(FTipPoint, FBasePoint);
  Result[1] := GetPointAtAngleFromPoint(FBasePoint, FSize * 0.5, Angle + CRad60);
  Result[2] := FBasePoint;
  Result[3] := GetPointAtAngleFromPoint(FBasePoint, FSize * 0.5, Angle - CRad60);
end;
//------------------------------------------------------------------------------

function TArrowHeadCircle.GetPointsInternal: TArrayOfFloatPoint;
var
  MidPt: TFloatPoint;
begin
  MidPt := Average(FTipPoint, FBasePoint);
  Result := Circle(MidPt.X, MidPt.Y, FSize * 0.5, Round(FSize));
end;
//------------------------------------------------------------------------------

function TArrowHeadDiamond.GetPointsInternal: TArrayOfFloatPoint;
var
  MidPt, UnitNorm: TFloatPoint;
  Sz: Single;
begin
  MidPt := Average(FTipPoint, FBasePoint);
  UnitNorm := GetUnitNormal(FTipPoint, FBasePoint);
  Sz := FSize / 3;
  SetLength(Result, 4);
  Result[0] := FTipPoint;
  Result[1] := OffsetPoint(MidPt, UnitNorm.X * Sz, UnitNorm.Y * Sz);
  Result[2] := FBasePoint;
  Result[3] := OffsetPoint(MidPt, -UnitNorm.X * Sz, -UnitNorm.Y * Sz);
end;
//------------------------------------------------------------------------------

end.
