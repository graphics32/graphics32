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
  Types, SysUtils, Math, GR32, GR32_Polygons;

const
  LUTSize = 512;
  LUTSizeMin1 = LUTSize -1;

type
  TColor32Gradient = record
    Offset: TFloat; //expected range between 0.0 and 1.0
    Color32: TColor32;
  end;
  TArrayOfColor32Gradient = array of TColor32Gradient;

  TLinearGradientType = (lgVertical, lgHorizontal, lgAngled);

  TCustomGradientSampler = class(TCustomSampler)
  private
    FCenterColor: TColor32;
    procedure SetCenterColor(const Value: TColor32);
  protected
    FGradientColors: array of TColor32Gradient;
    procedure GradientColorsChanged; virtual; abstract;
  public
    procedure AddColorStop(Offset: TFloat; Color: TColor32); virtual;
    procedure ClearColors;

    property CenterColor: TColor32 read FCenterColor write SetCenterColor;
  end;

  TRadialGradientSampler = class(TCustomGradientSampler)
  private
    FCenterX: TFloat;
    FCenterY: TFloat;
    FGradientLUT: array [Byte] of TColor32;
    FRadius: TFloat;
    FSqrInvRadius: TFloat;
    procedure SetRadius(const Value: TFloat);
    procedure PrepareGradientLUT;
  protected
    procedure GradientColorsChanged; override;
  public
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Radius: TFloat read FRadius write SetRadius;
    property CenterX: TFloat read FCenterX write FCenterX;
    property CenterY: TFloat read FCenterY write FCenterY;
  end;

  TCustomGradientPolygonFiller = class(TCustomPolygonFiller)
  protected
    FGradientLUT: array [0..LUTSizeMin1] of TColor32;
    FGradientColors: TArrayOfColor32Gradient;
(*
    procedure FillLine(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32); virtual; abstract;
*)
    procedure InitGradientLUT(const GradColors: array of TColor32Gradient);
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
    function ClosestPointOnLine(const Pt: TPoint): TPoint;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineVertical(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontal(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineAngle(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
  public
    procedure InitGradient(const StartPoint, EndPoint: TFloatPoint;
      const GradColors: array of TColor32Gradient);
  end;

  TRadialGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FCenterI: TPoint;
    FRadiusI: TPoint;
    FColorBuffer: TArrayOfColor32;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  public
    procedure InitGradient(const Center: TFloatPoint; RadiusX, RadiusY: TFloat;
      const GradColors: array of TColor32Gradient);
  end;

procedure AddGradientColor(Offset: TFloat; Color: TColor32;
  var Cga: TArrayOfColor32Gradient);

implementation

uses
  GR32_Math, GR32_Blend;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

procedure AddGradientColor(Offset: TFloat; Color: TColor32;
  var Cga: TArrayOfColor32Gradient);
var
  i, Len: Integer;
begin
  if Offset <= 0 then Offset := 0
  else if Offset >= 1 then Offset := 1;

  Len := Length(Cga);
  i := 0;
  while (i < Len) and (Offset > Cga[i].Offset) do inc(i);
  if (i = Len) then
  begin
    SetLength(Cga, Len +1);
    Cga[Len].Offset := Offset;
    Cga[Len].Color32 := Color;
  end else
  begin
    if (Offset <> Cga[i].Offset) then
    begin
      SetLength(Cga, Len +1);
      Move(Cga[i].Color32, Cga[i + 1].Color32, sizeof(TColor32Gradient) * (Len - i));
    end;
    Cga[i].Offset := Offset;
    Cga[i].Color32 := Color;
  end;
end;
//------------------------------------------------------------------------------

procedure InitColorLUT(var ColorLUT: array of TColor32; Cga: array of TColor32Gradient);
var
  i, j, Len, HighLUT: Integer;
  Fraction: TFloat;
  ClrLUT, c1, c2: PColor32Entry;
begin
  Len := Length(Cga);
  HighLUT := High(ColorLUT);
  if Len = 0 then
    for i := 0 to HighLUT do ColorLUT[i] := 0
  else if Len = 1 then
    for i := 0 to HighLUT do ColorLUT[i] := Cga[0].Color32
  else
  begin
    ColorLUT[0] := Cga[0].Color32;
    ColorLUT[HighLUT] := Cga[Len - 1].Color32;
    j := 0;
    for i := 1 to HighLUT - 1 do
    begin
      Fraction := i / HighLUT;
      while (j < Len) and (Fraction > Cga[j].Offset) do
        Inc(j);

      if j = 0 then
      begin
        ColorLUT[i] := ColorLUT[0];
        Continue;
      end
      else if j = Len then
      begin
        for j := i to HighLUT -1 do
          ColorLUT[j] := ColorLUT[HighLUT];
        Break;
      end;
      Fraction := (Fraction - Cga[j - 1].Offset) / (Cga[j].Offset - Cga[j-1].Offset);
      if Fraction <= 0 then ColorLUT[i] := Cga[j-1].Color32
      else if Fraction >= 1 then ColorLUT[i] := Cga[j].Color32
      else
      begin
        ColorLUT[i] := CombineReg(Cga[j].Color32, Cga[j - 1].Color32,
          Round($FF * Fraction));
      end;
    end;
  end;
end;


{ TCustomGradientSampler }

procedure TCustomGradientSampler.AddColorStop(Offset: TFloat; Color: TColor32);
var
  Index, OldCount: Integer;
begin
  if Offset <= 0 then
    FCenterColor := Color
  else
  begin
    OldCount := Length(FGradientColors);
    SetLength(FGradientColors, OldCount + 1);
    Index := 0;
    while Index < OldCount do
    begin
      if Offset < FGradientColors[Index].Offset then
        Break;
      Inc(Index);
    end;

    if (OldCount - Index) > 0 then
      Move(FGradientColors[Index], FGradientColors[Index + 1],
        (OldCount - Index) * SizeOf(TColor32Gradient));

    FGradientColors[Index].Offset := Offset;
    FGradientColors[Index].Color32 := Color;
  end;
  GradientColorsChanged;
end;

procedure TCustomGradientSampler.ClearColors;
begin
  SetLength(FGradientColors, 0);
  GradientColorsChanged;
end;

procedure TCustomGradientSampler.SetCenterColor(const Value: TColor32);
begin
  if FCenterColor <> Value then
  begin
    FCenterColor := Value;
    GradientColorsChanged;
  end;
end;


{ TRadialGradientSampler }

function TRadialGradientSampler.GetSampleFixed(X, Y: TFixed): TColor32;
var
  RelativeRadius: TFloat;
begin
  RelativeRadius := Sqrt((Sqr(FixedToFloat * X - FCenterX) +
    Sqr(FixedToFloat * Y - FCenterY)) * FSqrInvRadius);
  Result := FGradientLUT[EnsureRange(Round($FF * RelativeRadius), 0, $FF)];
end;

function TRadialGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  RelativeRadius: TFloat;
begin
  RelativeRadius := Sqrt((Sqr(X - FCenterX) + Sqr(Y - FCenterY)) * FSqrInvRadius);
  Result := FGradientLUT[EnsureRange(Round($FF * RelativeRadius), 0, $FF)];
end;

function TRadialGradientSampler.GetSampleInt(X, Y: Integer): TColor32;
var
  RelativeRadius: TFloat;
begin
  RelativeRadius := Sqrt((Sqr(X - FCenterX) + Sqr(Y - FCenterY)) * FSqrInvRadius);
  Result := FGradientLUT[EnsureRange(Round($FF * RelativeRadius), 0, $FF)];
end;

procedure TRadialGradientSampler.GradientColorsChanged;
begin
  inherited;
  PrepareGradientLUT;
end;

procedure TRadialGradientSampler.PrepareGradientLUT;
const
  DIV255 = 1/255;
var
  Color: TColor32;
  Current, OldOffset, RangeInv: TFloat;
  Index, GradIndex: Integer;
begin
  Color := FCenterColor;
  if Length(FGradientColors) = 0 then
    for Index := 0 to 255 do
      FGradientLUT[Index] := Color
  else
  begin
    OldOffset := 0;
    GradIndex := 0;
    RangeInv := 1 / (FGradientColors[0].Offset - OldOffset);
    for Index := 0 to 255 do
    begin
      Current := Index * DIV255;
      FGradientLUT[Index] := CombineReg(FGradientColors[GradIndex].Color32, Color,
        EnsureRange(Round($FF * (Current - OldOffset) * RangeInv), 0, $FF));
      if Current > FGradientColors[GradIndex].Offset then
      begin
        Color := FGradientColors[GradIndex].Color32;
        OldOffset := FGradientColors[GradIndex].Offset;
        Inc(GradIndex);
        if GradIndex < Length(FGradientColors) then
          RangeInv := 1 / (FGradientColors[GradIndex].Offset - OldOffset);
      end;
    end;
  end;
end;

procedure TRadialGradientSampler.SetRadius(const Value: TFloat);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FSqrInvRadius := 1 / Sqr(FRadius);
  end;
end;


//------------------------------------------------------------------------------
// TCustomGradientPolygonFiller
//------------------------------------------------------------------------------

procedure TCustomGradientPolygonFiller.InitGradientLUT(
  const GradColors: array of TColor32Gradient);
var
  I, ColorCount: Integer;
begin
  ColorCount := Length(GradColors);
  SetLength(FGradientColors, ColorCount);
  for I := 0 to ColorCount - 1 do
    FGradientColors[I] := GradColors[I];
  InitColorLUT(FGradientLUT, FGradientColors);
end;
//------------------------------------------------------------------------------
// TLinearGradientPolygonFiller
//------------------------------------------------------------------------------

function TLinearGradientPolygonFiller.ClosestPointOnLine(const Pt: TPoint): TPoint;
var
  Q: TFloat;
begin
  if (FDistanceSqrd > 0) then
  begin
    Q := ((Pt.X - FStartPoint.X) * fDx + (Pt.Y - FStartPoint.Y) * fDy) / FDistanceSqrd;
    if Q < 0 then Q := 0 else if Q > 1 then Q := 1;
    Result.X := Round((1 - Q) * FStartPoint.X + Q * FEndPoint.X);
    Result.Y := Round((1 - Q) * FStartPoint.Y + Q * FEndPoint.Y);
  end else
    Result := FStartPoint;
end;
//------------------------------------------------------------------------------

function TLinearGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FLinearGradType of
    lgVertical:
      Result := FillLineVertical;
    lgHorizontal:
      Result := FillLineHorizontal;
    else
      Result := FillLineAngle;
  end; //case
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLineAngle(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Pt: TPoint;
  Dist: TFloat;
  Distance: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  for X := DstX to DstX + Length - 1 do
  begin
    Pt := ClosestPointOnLine(GR32.Point(X,DstY));
    Dist := Hypot(Pt.X - FStartPoint.X, Pt.Y - FStartPoint.Y);
    if Dist > FDistance then
      Color32 := FGradientLUT[LUTSizeMin1] else
      Color32 := FGradientLUT[Trunc(Dist * LUTSizeMin1 / FDistance)];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLineHorizontal(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Pt: TPoint;
  Dist: TFloat;
  Distance: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  Distance := Round(FDistance);
  for X := DstX to DstX + Length - 1 do
  begin
    if (X > FStartPoint.X) = (X < FEndPoint.X) then
      Color32 := FGradientLUT[Abs(X - FStartPoint.X) * LUTSizeMin1 div Distance]
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
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLineVertical(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Pt: TPoint;
  Dist: TFloat;
  Distance: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  Distance := Round(FDistance);
  if (DstY > FStartPoint.Y) = (DstY < FEndPoint.Y) then
    Color32 := FGradientLUT[Abs(DstY - FStartPoint.Y) * LUTSizeMin1 div Distance]
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
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.InitGradient(
  const StartPoint, EndPoint: TFloatPoint; const GradColors: array of TColor32Gradient);
begin
  InitGradientLUT(GradColors);
  FStartPoint := Point(StartPoint);
  FEndPoint := Point(EndPoint);
  FDx := EndPoint.X - StartPoint.X;
  FDy := EndPoint.Y - StartPoint.Y;
  FDistanceSqrd := FDx * FDx + FDy * FDy;
  FDistance := Sqrt(FDistanceSqrd);

  if Abs(FStartPoint.Y - FEndPoint.Y) < 1 then
    FLinearGradType := lgHorizontal
  else
  if Abs(FStartPoint.X - FEndPoint.X) < 1 then
    FLinearGradType := lgVertical
  else
    FLinearGradType := lgAngled;
end;

//------------------------------------------------------------------------------
// TRadialGradientPolygonFiller
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.FillLineEllipse(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, dx, dy: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  dy := Abs(DstY - FCenterI.Y);
  for X := DstX to DstX + Length - 1 do
  begin
    dx := Abs(X - FCenterI.X);
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

function TRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineEllipse;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.InitGradient(const Center: TFloatPoint;
  RadiusX, RadiusY: TFloat; const GradColors: array of TColor32Gradient);
var
  I,J, rX, rY: Integer;
  RadiusXDivRadiusY, Rad, rad2, x, y: TFloat;
begin
  InitGradientLUT(GradColors);
  rX := Ceil(RadiusX);
  rY := Ceil(RadiusY);
  SetLength(FColorBuffer, rX * rY);

  FCenterI := Point(Center);
  FRadiusI := GR32.Point(Round(RadiusX), Round(RadiusY));

  //fill the color buffer using GradientLUT ...
  if Abs(rX - rY) = 0 then
  begin
    for I := 0 to rX - 1 do
      for J := 0 to rY - 1 do
      begin
        Rad := Hypot(I, J);
        if Rad >= RadiusX then
          FColorBuffer[(j * rX) + i] := FGradientLUT[LUTSizeMin1] else
          FColorBuffer[(j * rX) + i] := FGradientLUT[Trunc(Rad * LUTSizeMin1 / RadiusX)];
      end;
  end else
  begin
    RadiusXDivRadiusY := RadiusX / RadiusY;
    for I := 0 to rX -1 do
      for J := 0 to rY -1 do
      begin
        Rad := Hypot(I, J);
        if I = 0 then rad2 := RadiusY
        else if J = 0 then rad2 := RadiusX
        else
        begin
          GR32_Math.SinCos(ArcTan(RadiusXDivRadiusY * j / i), y, x);
          rad2 := Hypot(x * RadiusX, y * RadiusY);
        end;
        if Rad >= rad2 then
          FColorBuffer[(j * rX) + i] := FGradientLUT[LUTSizeMin1] else
          FColorBuffer[(j * rX) + i] := FGradientLUT[Trunc(Rad * LUTSizeMin1 / rad2)];
      end;
  end;
end;
//------------------------------------------------------------------------------

end.
