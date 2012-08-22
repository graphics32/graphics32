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
  Types, Classes, SysUtils, Math, GR32, GR32_Polygons;

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

  TGradient32 = class
  private
    FStartColor: TColor32;
    FOnGradientColorsChanged: TNotifyEvent;
    function GetGradientEntry(Index: Integer): TColor32Gradient;
    procedure SetStartColor(const Value: TColor32);
    function GetGradientCount: Integer;
  protected
    FGradientColors: array of TColor32Gradient;
    procedure GradientColorsChanged;
    procedure StartColorChanged;
  public
    constructor Create(StartColor: TColor32 = clBlack32);
    procedure AddColorStop(Offset: TFloat; Color: TColor32); virtual;
    procedure ClearColors;
    procedure SetColors(GradientColors: array of TColor32Gradient);

    property StartColor: TColor32 read FStartColor write SetStartColor;
    property GradientEntry[Index: Integer]: TColor32Gradient read GetGradientEntry;
    property GradientCount: Integer read GetGradientCount;
    property OnGradientColorsChanged: TNotifyEvent read FOnGradientColorsChanged write FOnGradientColorsChanged;
  end;

  TCustomGradientSampler = class(TCustomSampler)
  private
    FGradient: TGradient32;
    procedure SetGradient(const Value: TGradient32);
  protected
    procedure GradientChangedHandler(Sender: TObject);
    procedure GradientChanged;
    procedure GradientColorsChanged; virtual; abstract;
  public
    property Gradient: TGradient32 read FGradient write SetGradient;
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
  private
    procedure SetGradient(const Value: TGradient32);
  protected
    FGradientLUT: array [0..LUTSizeMin1] of TColor32;
    FGradient: TGradient32;
    FGradientColors: TArrayOfColor32Gradient;

    procedure GradientChangedHandler(Sender: TObject);
    procedure GradientChanged;
    procedure GradientColorsChanged; virtual;
  public
    property Gradient: TGradient32 read FGradient write SetGradient;
  end;

  TLinearGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FUsingHorzAxis: boolean;
    FStart: TFloat;
    FEnd: TFloat;
    FTanAngle: TFloat;
    FLength: TFloat;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineVertical(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontal(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
  public
    procedure InitGradient(const StartPoint, EndPoint: TFloatPoint);
  end;

  TRadialGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FColorBuffer: TArrayOfColor32;
    FCenterX: Integer;
    FCenterY: Integer;
    FRadiusX: Integer;
    FRadiusY: Integer;
    procedure SetCenterX(const Value: Integer);
    procedure SetCenterY(const Value: Integer);
    procedure SetRadiusX(const Value: Integer);
    procedure SetRadiusY(const Value: Integer);
    procedure UpdateColorBuffer;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure CenterXChanged;
    procedure CenterYChanged;
    procedure RadiusXChanged;
    procedure RadiusYChanged;
    procedure FillLineCircle(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  public
    property CenterX: Integer read FCenterX write SetCenterX;
    property CenterY: Integer read FCenterY write SetCenterY;
    property RadiusX: Integer read FRadiusX write SetRadiusX;
    property RadiusY: Integer read FRadiusY write SetRadiusY;
  end;

implementation

uses
  GR32_Math, GR32_Blend;


{ TGradient32 }

constructor TGradient32.Create(StartColor: TColor32 = clBlack32);
begin
  inherited Create;
  FStartColor := StartColor;
end;

procedure TGradient32.ClearColors;
begin
  SetLength(FGradientColors, 0);
  GradientColorsChanged;
end;

procedure TGradient32.SetColors(GradientColors: array of TColor32Gradient);
var
  Index: Integer;
begin
  if Length(GradientColors) = 0 then
  begin
    // no further colors specified -> clear colors
    ClearColors;
    Exit;
  end;

  if GradientColors[0].Offset = 0 then
  begin
    // copy start color
    FStartColor := GradientColors[0].Color32;

    SetLength(FGradientColors, Length(GradientColors) - 1);
    for Index := 1 to Length(GradientColors) - 1 do
      FGradientColors[Index - 1] := GradientColors[Index];
  end
  else
  begin
    // keep start color (only copy color stops)
    SetLength(FGradientColors, Length(GradientColors));
    for Index := 0 to Length(GradientColors) - 1 do
      FGradientColors[Index] := GradientColors[Index];
  end;
end;

procedure TGradient32.SetStartColor(const Value: TColor32);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    StartColorChanged;
  end;
end;

procedure TGradient32.StartColorChanged;
begin
  GradientColorsChanged;
end;

function TGradient32.GetGradientCount: Integer;
begin
  Result := Length(FGradientColors) + 1;
end;

function TGradient32.GetGradientEntry(Index: Integer): TColor32Gradient;
begin
  if Index > Length(FGradientColors) then
    raise Exception.CreateFmt('Index out of bounds (%d)', [Index])
  else
  if Index = 0 then
  begin
    Result.Offset := 0;
    Result.Color32 := FStartColor;
  end
  else
    Result := FGradientColors[Index - 1];
end;

procedure TGradient32.GradientColorsChanged;
begin
  if Assigned(FOnGradientColorsChanged) then
    FOnGradientColorsChanged(Self);
end;

procedure TGradient32.AddColorStop(Offset: TFloat; Color: TColor32);
var
  Index, OldCount: Integer;
begin
  if Offset <= 0 then
    FStartColor := Color
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


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetAngleOfParam2FromParam1(const Point1, Point2: TFloatPoint): single;
var
  x,y: TFloat;
const
  rad90 = pi / 2;
  rad270 = rad90 * 3;
  rad360 = pi * 2;
begin
  x := Point2.X - Point1.X;
  y := Point2.Y - Point1.Y;
  if x = 0 then
  begin
    if y > 0 then result := rad270 else result := rad90;
  end else
  begin
    result := arctan2(-y,x);
    if result < 0 then result := result + rad360;
  end;
end;
//------------------------------------------------------------------------------

procedure InitColorLUT(var ColorLUT: array of TColor32; Cga: array of TColor32Gradient);
var
  i, j, Len, HighLUT: Integer;
  Fraction: TFloat;
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
        EMMS;
      end;
    end;
  end;
end;


{ TCustomGradientSampler }

procedure TCustomGradientSampler.SetGradient(const Value: TGradient32);
begin
  if FGradient <> Value then
  begin
    FGradient := Value;
    GradientChanged;
  end;
end;

procedure TCustomGradientSampler.GradientChangedHandler(Sender: TObject);
begin
  GradientChanged;
end;

procedure TCustomGradientSampler.GradientChanged;
begin
  FGradient.OnGradientColorsChanged := GradientChangedHandler;
  GradientColorsChanged;
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
  Color := FGradient.StartColor;
  if Length(FGradient.FGradientColors) = 0 then
    for Index := 0 to 255 do
      FGradientLUT[Index] := Color
  else
  begin
    OldOffset := 0;
    GradIndex := 0;
    RangeInv := 1 / (FGradient.FGradientColors[0].Offset - OldOffset);
    for Index := 0 to 255 do
    begin
      Current := Index * DIV255;
      FGradientLUT[Index] := CombineReg(FGradient.FGradientColors[GradIndex].Color32, Color,
        EnsureRange(Round($FF * (Current - OldOffset) * RangeInv), 0, $FF));
      if Current > FGradient.FGradientColors[GradIndex].Offset then
      begin
        Color := FGradient.FGradientColors[GradIndex].Color32;
        OldOffset := FGradient.FGradientColors[GradIndex].Offset;
        Inc(GradIndex);
        if GradIndex < Length(FGradient.FGradientColors) then
          RangeInv := 1 / (FGradient.FGradientColors[GradIndex].Offset - OldOffset);
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

procedure TCustomGradientPolygonFiller.GradientChanged;
begin
  FGradient.OnGradientColorsChanged := GradientChangedHandler;
  GradientColorsChanged;
end;

procedure TCustomGradientPolygonFiller.GradientChangedHandler(Sender: TObject);
begin
  GradientChanged;
end;

procedure TCustomGradientPolygonFiller.GradientColorsChanged;
var
  I, ColorCount: Integer;
begin
  ColorCount := FGradient.GradientCount;
  SetLength(FGradientColors, ColorCount);
  for I := 0 to ColorCount - 1 do
    FGradientColors[I] := FGradient.GradientEntry[I];
  InitColorLUT(FGradientLUT, FGradientColors);
end;

procedure TCustomGradientPolygonFiller.SetGradient(const Value: TGradient32);
begin
  if FGradient <> Value then
  begin
    FGradient := Value;
    GradientChanged;
  end;
end;


//------------------------------------------------------------------------------
// TLinearGradientPolygonFiller
//------------------------------------------------------------------------------

function TLinearGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if FUsingHorzAxis then
    Result := FillLineHorizontal else
    Result := FillLineVertical;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLineHorizontal(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  AxisPt: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  for X := DstX to DstX + Length - 1 do
  begin
    AxisPt := X + FTanAngle * DstY;
    if (AxisPt > FStart) = (AxisPt < FEnd) then
      Color32 := FGradientLUT[Round((AxisPt - FStart) * LUTSizeMin1 / FLength)]
    else if (FLength > 0) <> (AxisPt > FEnd) then
      Color32 := FGradientLUT[0]
    else
      Color32 := FGradientLUT[LUTSizeMin1];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.FillLineVertical(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  AxisPt: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  for X := DstX to DstX + Length - 1 do
  begin
    AxisPt := DstY + FTanAngle * X;
    if (AxisPt > FStart) = (AxisPt < FEnd) then
      Color32 := FGradientLUT[Round((AxisPt - FStart) * LUTSizeMin1 / FLength)]
    else if (FLength > 0) <> (AxisPt > FEnd) then
      Color32 := FGradientLUT[0]
    else
      Color32 := FGradientLUT[LUTSizeMin1];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientPolygonFiller.InitGradient(
  const StartPoint, EndPoint: TFloatPoint);
var
  a: TFloat;
begin
  FUsingHorzAxis :=
    Abs(StartPoint.X - EndPoint.X) > Abs(StartPoint.Y - EndPoint.Y);
  if FUsingHorzAxis then
  begin
    a := pi*2 - GetAngleOfParam2FromParam1(StartPoint, EndPoint);
    FTanAngle := Tan(a);
    FStart := StartPoint.X + FTanAngle * StartPoint.Y;
    FEnd := EndPoint.X + FTanAngle * EndPoint.Y;
  end else
  begin
    a := pi*3/2 + GetAngleOfParam2FromParam1(StartPoint, EndPoint);
    FTanAngle := Tan(a);
    FStart := StartPoint.Y + FTanAngle * StartPoint.X;
    FEnd := EndPoint.Y + FTanAngle * EndPoint.X;
  end;
  FLength := FEnd - FStart;
end;

//------------------------------------------------------------------------------
// TRadialGradientPolygonFiller
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.FillLineCircle(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  RelativeRadius: TFloat;
  SqrInvRadius: TFloat;
  YDist: TFloat;
  Index: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  SqrInvRadius := Sqr(1 / FRadiusX);
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  YDist := Sqr(DstY - FCenterY);
  for X := DstX to DstX + Length - 1 do
  begin
    RelativeRadius := Sqrt((Sqr(X - FCenterX) + YDist) * SqrInvRadius);
    Index := EnsureRange(Round($1FF * RelativeRadius), 0, $1FF);
    Color32 := FGradientLUT[Index];
    BlendMemEx(Color32 and $00FFFFFF or $FF000000, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.FillLineEllipse(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, dx, dy: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  dy := Abs(DstY - FCenterY);
  for X := DstX to DstX + Length - 1 do
  begin
    dx := Abs(X - FCenterX);
    if (dx >= FRadiusX) or (dy >= FRadiusY) then
      Color32 := FGradientLUT[LUTSizeMin1] else
      Color32 := FColorBuffer[dy * FRadiusX + dx];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;
//------------------------------------------------------------------------------

function TRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if FRadiusX = FRadiusY then
    Result := FillLineCircle
  else
    Result := FillLineEllipse;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.UpdateColorBuffer;
var
  I,J, rX, rY: Integer;
  RadiusXDivRadiusY, Rad, Rad2, x, y: TFloat;
begin
  if (RadiusX = 0) or (RadiusY = 0) then
    Exit;

  rX := Ceil(RadiusX);
  rY := Ceil(RadiusY);
  SetLength(FColorBuffer, rX * rY);

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
    for I := 0 to rX - 1 do
      for J := 0 to rY - 1 do
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

procedure TRadialGradientPolygonFiller.RadiusXChanged;
begin
  UpdateColorBuffer;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.RadiusYChanged;
begin
  UpdateColorBuffer;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.CenterXChanged;
begin
  // do nothing yet
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.CenterYChanged;
begin
  // do nothing yet
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.SetCenterX(const Value: Integer);
begin
  if FCenterX <> Value then
  begin
    FCenterX := Value;
    CenterXChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.SetCenterY(const Value: Integer);
begin
  if FCenterY <> Value then
  begin
    FCenterY := Value;
    CenterYChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.SetRadiusX(const Value: Integer);
begin
  if FRadiusX <> Value then
  begin
    FRadiusX := Value;
    RadiusXChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientPolygonFiller.SetRadiusY(const Value: Integer);
begin
  if FRadiusY <> Value then
  begin
    FRadiusY := Value;
    RadiusYChanged;
  end;
end;
//------------------------------------------------------------------------------

end.
