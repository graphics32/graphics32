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
  GR32, GR32_Polygons, GR32_VectorUtils, GR32_Geometry;

type
  TArrowHeadAbstract = class
  private
    fSize: TFloat;
    fTipPoint: TFloatPoint;
    fBasePoint: TFloatPoint;
  protected
    function GetPointsInternal: TArrayOfFloatPoint; virtual; abstract;
  public
    constructor Create(size: TFloat); virtual;
    function GetPoints(const line: TArrayOfFloatPoint; AtEnd: boolean): TArrayOfFloatPoint;
    //Size: distance between arrow tip and arrow base
    property Size: TFloat read fSize write fSize;
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


implementation


constructor TArrowHeadAbstract.Create(size: TFloat);
begin
  fSize := size;
end;
//------------------------------------------------------------------------------

function TArrowHeadAbstract.GetPoints(const line: TArrayOfFloatPoint;
  AtEnd: boolean): TArrayOfFloatPoint;
var
  highI: integer;
  angle: double;
begin
  highI := high(line);
  //nb: the line must contain at least 2 points to reposition ...
  if highI < 1 then exit;

  if AtEnd then
  begin
    fBasePoint := line[highI];
    angle := GetAngleOfPt2FromPt1(line[highI -1], line[highI]);
  end else
  begin
    fBasePoint := line[0];
    angle := GetAngleOfPt2FromPt1(line[1], line[0]);
  end;
  fTipPoint := GetPointAtAngleFromPoint(fBasePoint, fSize, angle);
  result := GetPointsInternal;
end;
//------------------------------------------------------------------------------

function TArrowHeadSimple.GetPointsInternal: TArrayOfFloatPoint;
var
  midPt, unitNorm: TFloatPoint;
  sz: single;
begin
  setlength(result, 3);
  unitNorm := GetUnitNormal(fTipPoint, fBasePoint);
  sz := fSize / 2;
  result[0] := fTipPoint;
  result[1] := OffsetPoint(fBasePoint, unitNorm.X *sz, unitNorm.Y *sz);
  result[2] := OffsetPoint(fBasePoint, -unitNorm.X *sz, -unitNorm.Y *sz);
end;
//------------------------------------------------------------------------------

function TArrowHeadFourPt.GetPointsInternal: TArrayOfFloatPoint;
var
  angle: double;
begin
  setlength(result, 4);
  result[0] := fTipPoint;
  angle := GetAngleOfPt2FromPt1(fTipPoint, fBasePoint);
  result[1] := GetPointAtAngleFromPoint(fBasePoint, fSize /2, angle + rad60);
  result[2] := fBasePoint;
  result[3] := GetPointAtAngleFromPoint(fBasePoint, fSize /2, angle - rad60);
end;
//------------------------------------------------------------------------------

function TArrowHeadCircle.GetPointsInternal: TArrayOfFloatPoint;
var
  midPt: TFloatPoint;
begin
  midPt := Average(fTipPoint, fBasePoint);
  result := Ellipse(midPt.X, midPt.Y, fSize /2, fSize /2, round(fSize));
end;
//------------------------------------------------------------------------------

function TArrowHeadDiamond.GetPointsInternal: TArrayOfFloatPoint;
var
  midPt, unitNorm: TFloatPoint;
  sz: single;
begin
  midPt := Average(fTipPoint, fBasePoint);
  unitNorm := GetUnitNormal(fTipPoint, fBasePoint);
  sz := fSize /3;
  setlength(result, 4);
  result[0] := fTipPoint;
  result[1] := OffsetPoint(midPt, unitNorm.X *sz, unitNorm.Y *sz);
  result[2] := fBasePoint;
  result[3] := OffsetPoint(midPt, -unitNorm.X *sz, -unitNorm.Y *sz);
end;
//------------------------------------------------------------------------------

end.
