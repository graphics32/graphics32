unit GR32_ColorGradients_draft;

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
 * The Original Code is Color Gradients for Graphics32
 *
 * The Initial Developer of the Original Code is Angus Johnson
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Types, SysUtils, Math, GR32, GR32_Math, GR32_Blend, GR32_Polygons;

const
  LUTSize = 512;
  LUTSizeMin1 = LUTSize -1;
  
type
  TColor32Gradient = record
    color32: TColor32;
    offset: single; //expected range between 0.0 and 1.0
  end;
  TArrayOfColor32Gradient = array of TColor32Gradient;

  TLinearGradientType = (lgVertical, lgHorizontal, lgAngled);

  TCustomGradientPolygonFiller = class(TCustomPolygonFiller)
  protected
    FGradientLUT: array [0..LUTSizeMin1] of TColor32;
    FGradientColors: TArrayOfColor32Gradient;
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32); virtual; abstract;
    procedure InitGradientLUT(const gradColors: array of TColor32Gradient);
  public
  end;

  TLinearGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FStartPoint: TPoint;
    FEndPoint: TPoint;
    FDx: TFloat;
    FDy: TFloat;
    FDistance: TFloat;
    FDistanceSqrd: TFloat;
    FLinearGradType: TLinearGradientType;
    function ClosestPointOnLine(const pt: TPoint): TPoint;
  protected
    procedure FillLine(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32); override;
  public
    procedure InitGradient(const StartPoint, EndPoint: TFloatPoint;
      const gradColors: array of TColor32Gradient);
  end;

  TRadialGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FCenterI: TPoint;
    FRadiusI: TPoint;
    FColorBuffer: TArrayOfColor32;
  protected
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32); override;
  public
    procedure InitGradient(const Center: TFloatPoint; RadiusX, RadiusY: TFloat;
      const gradColors: array of TColor32Gradient);
  end;

procedure AddGradientColor(Color: TColor32; Offset: single;
  var Cga: TArrayOfColor32Gradient);

implementation

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

procedure AddGradientColor(Color: TColor32; Offset: single;
  var Cga: TArrayOfColor32Gradient);
var
  i, len: integer;
begin
  if Offset <= 0 then Offset := 0
  else if Offset >= 1 then Offset := 1;

  len := length(Cga);
  i := 0;
  while (i < len) and (Offset > Cga[i].Offset) do inc(i);
  if (i = len) then
  begin
    setlength(Cga, len +1);
    Cga[len].Offset := Offset;
    Cga[len].color32 := Color;
  end else
  begin
    if (Offset <> Cga[i].Offset) then
    begin
      setlength(Cga, len +1);
      move(Cga[i].color32, Cga[i+1].color32, sizeof(TColor32Gradient) * (len - i));
    end;
    Cga[i].Offset := Offset;
    Cga[i].color32 := Color;
  end;
end;
//------------------------------------------------------------------------------

procedure InitColorLUT(var ColorLUT: array of TColor32; Cga: array of TColor32Gradient);
var
  i, j, len, highLUT: integer;
  Fraction: TFloat;
  cLUT, c1, c2: PColor32Entry;
begin
  len := length(Cga);
  highLUT := high(ColorLUT);
  if len = 0 then
    for i := 0 to highLUT do ColorLUT[i] := 0
  else if len = 1 then
    for i := 0 to highLUT do ColorLUT[i] := Cga[0].color32
  else
  begin
    ColorLUT[0] := Cga[0].color32;
    ColorLUT[highLUT] := Cga[len-1].color32;
    j := 0;
    for i := 1 to highLUT -1 do
    begin
      Fraction := i / highLUT;
      while (j < len) and (Fraction > Cga[j].offset) do inc(j);
      if j = 0 then
        ColorLUT[i] := ColorLUT[0]
      else if j = len then
      begin
        for j := i to highLUT -1 do ColorLUT[j] := ColorLUT[highLUT];
        break;
      end;
      Fraction := (Fraction - Cga[j-1].offset)/(Cga[j].offset - Cga[j-1].offset);
      if Fraction <= 0 then ColorLUT[i] := Cga[j-1].color32
      else if Fraction >= 1 then ColorLUT[i] := Cga[j].color32
      else
      begin
        c1 := @Cga[j-1].color32;
        c2 := @Cga[j].color32;
        cLUT := @ColorLUT[i];
        cLUT.B := trunc(c2.B * Fraction + c1.B * (1-Fraction));
        cLUT.G := trunc(c2.G * Fraction + c1.G * (1-Fraction));
        cLUT.R := trunc(c2.R * Fraction + c1.R * (1-Fraction));
        cLUT.A := trunc(c2.A * Fraction + c1.A * (1-Fraction));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TCustomGradientPolygonFiller
//------------------------------------------------------------------------------

function TCustomGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLine;
end;
//------------------------------------------------------------------------------

procedure TCustomGradientPolygonFiller.InitGradientLUT(
  const gradColors: array of TColor32Gradient);
var
  I, ColorCount: integer;
begin
  ColorCount := length(gradColors);
  setlength(FGradientColors, ColorCount);
  for I := 0 to ColorCount -1 do FGradientColors[I] := gradColors[I];
  InitColorLUT(FGradientLUT, FGradientColors);
end;
//------------------------------------------------------------------------------
// TLinearGradientPolygonFiller
//------------------------------------------------------------------------------

function TLinearGradientPolygonFiller.ClosestPointOnLine(const pt: TPoint): TPoint;
var
  q: TFloat;
begin
  if (FDistanceSqrd > 0) then
  begin
    q := ((pt.X-FStartPoint.X) *fDx + (pt.Y-FStartPoint.Y) *fDy) / FDistanceSqrd;
    if q < 0 then q := 0 else if q > 1 then q := 1;
    Result.X := Round((1-q)*FStartPoint.X + q*fEndPoint.X);
    Result.Y := Round((1-q)*FStartPoint.Y + q*fEndPoint.Y);
  end else
    Result := FStartPoint;

end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLine(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  pt: TPoint;
  dist: TFloat;
  distance: integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  case FLinearGradType of
    lgVertical:
      begin
        distance := round(FDistance);
        if (DstY > FStartPoint.Y) = (DstY < FEndPoint.Y) then
          Color32 := FGradientLUT[abs(DstY - FStartPoint.Y) * LUTSizeMin1 div distance]
        else if (DstY > FStartPoint.Y) <> (FEndPoint.Y > FStartPoint.Y) then
          Color32 := FGradientLUT[0]
        else
          Color32 := FGradientLUT[LUTSizeMin1];
        for X := DstX to DstX + Length - 1 do
        begin
          BlendMemEx(Color32, Dst^, AlphaValues^);
          Inc(Dst);
          Inc(AlphaValues);
        end;
        EMMS;
      end;
    lgHorizontal:
      begin
        distance := round(FDistance);
        for X := DstX to DstX + Length - 1 do
        begin
          if (X > FStartPoint.X) = (X < FEndPoint.X) then
            Color32 := FGradientLUT[abs(X - FStartPoint.X) * LUTSizeMin1 div distance]
          else if (X > FStartPoint.X) <> (FEndPoint.X > FStartPoint.X) then
            Color32 := FGradientLUT[0]
          else
            Color32 := FGradientLUT[LUTSizeMin1];
          BlendMemEx(Color32, Dst^, AlphaValues^);
          Inc(Dst);
          Inc(AlphaValues);
        end;
        EMMS;
      end;
    else
      begin
        for X := DstX to DstX + Length - 1 do
        begin
          pt := ClosestPointOnLine(Point(X,DstY));
          dist := Hypot(pt.X - FStartPoint.X, pt.Y - FStartPoint.Y);
          if dist > FDistance then
            Color32 := FGradientLUT[LUTSizeMin1] else
            Color32 := FGradientLUT[trunc(dist * LUTSizeMin1 / FDistance)];
          BlendMemEx(Color32, Dst^, AlphaValues^);
          EMMS;
          Inc(Dst);
          Inc(AlphaValues);
        end;
      end;
  end; //case
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.InitGradient(
  const StartPoint, EndPoint: TFloatPoint; const gradColors: array of TColor32Gradient);
begin
  InitGradientLUT(gradColors);
  FStartPoint := Point(StartPoint);
  FEndPoint := Point(EndPoint);
  FDx := EndPoint.X - StartPoint.X;
  FDy := EndPoint.Y - StartPoint.Y;
  FDistanceSqrd := FDx * FDx + FDy * FDy;
  FDistance := Sqrt(FDistanceSqrd);
  if abs(FStartPoint.Y - FEndPoint.Y) < 1 then FLinearGradType := lgHorizontal
  else if abs(FStartPoint.X - FEndPoint.X) < 1 then FLinearGradType := lgVertical
  else FLinearGradType := lgAngled;
end;

//------------------------------------------------------------------------------
// TRadialGradientPolygonFiller
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.FillLine(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, dx, dy: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  dy := abs(DstY - FCenterI.Y);
  for X := DstX to DstX + Length - 1 do
  begin
    dx := abs(X - FCenterI.X);
    if (dx >= FRadiusI.X) or (dy >= FRadiusI.Y) then
      Color32 := FGradientLUT[LUTSizeMin1] else
      Color32 := FColorBuffer[dy*FRadiusI.X +dx];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.InitGradient(const Center: TFloatPoint;
  RadiusX, RadiusY: TFloat; const gradColors: array of TColor32Gradient);
var
  I,J, rX, rY: integer;
  RadiusXDivRadiusY, rad, rad2, x,y: TFloat;
begin
  InitGradientLUT(gradColors);
  rX := ceil(RadiusX);
  rY := ceil(RadiusY);
  setlength(FColorBuffer, rX * rY);

  FCenterI := Point(Center);
  FRadiusI := Point(round(RadiusX), round(RadiusY));

  //fill the color buffer using GradientLUT ...
  if abs(rX - rY) = 0 then
  begin
    for I := 0 to rX -1 do
      for J := 0 to rY -1 do
      begin
        rad := hypot(I, J);
        if rad >= RadiusX then
          FColorBuffer[(j*rX) +i] := FGradientLUT[LUTSizeMin1] else
          FColorBuffer[(j*rX) +i] := FGradientLUT[trunc(rad * LUTSizeMin1 / RadiusX)];
      end;
  end else
  begin
    RadiusXDivRadiusY := RadiusX/RadiusY;
    for I := 0 to rX -1 do
      for J := 0 to rY -1 do
      begin
        rad := hypot(I, J);
        if I = 0 then rad2 := RadiusY
        else if J = 0 then rad2 := RadiusX
        else
        begin
          SinCos(ArcTan(RadiusXDivRadiusY * j/i), y, x);
          rad2 := hypot(x* RadiusX, y* RadiusY);
        end;
        if rad >= rad2 then
          FColorBuffer[(j*rX) +i] := FGradientLUT[LUTSizeMin1] else
          FColorBuffer[(j*rX) +i] := FGradientLUT[trunc(rad * LUTSizeMin1 / rad2)];
      end;
  end;
end;
//------------------------------------------------------------------------------

end.
