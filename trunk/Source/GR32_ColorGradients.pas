unit GR32_ColorGradients;

(* ***** BEGIN LICENSE BLOCK ***************************************************
* Version: MPL 1.1 or LGPL 2.1 with linking exception                          *
*                                                                              *
* The contents of this file are subject to the Mozilla Public License Version  *
* 1.1 (the "License"); you may not use this file except in compliance with     *
* the License. You may obtain a copy of the License at                         *
* http://www.mozilla.org/MPL/                                                  *
*                                                                              *
* Software distributed under the License is distributed on an "AS IS" basis,   *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License     *
* for the specific language governing rights and limitations under the         *
* License.                                                                     *
*                                                                              *
* Alternatively, the contents of this file may be used under the terms of the  *
* Free Pascal modified version of the GNU Lesser General Public License        *
* Version 2.1 (the "FPC modified LGPL License"), in which case the provisions  *
* of this license are applicable instead of those above.                       *
* Please see the file LICENSE.txt for additional information concerning this   *
* license.                                                                     *
*                                                                              *
* The Original Code is Color Gradients for Graphics32                          *
*                                                                              *
* The Initial Developer of the Original Code is Angus Johnson                  *
*                                                                              *
* Portions created by the Initial Developer are Copyright (C) 2008-2012        *
* the Initial Developer. All Rights Reserved.                                  *
*                                                                              *
* Contributor(s): Christian Budde <Christian@aixcoustic.com>                   *
*                                                                              *
* ***** END LICENSE BLOCK *****************************************************)

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

  TGradient32 = class(TInterfacedPersistent, IStreamPersist)
  private
    FGradientColors: array of TColor32Gradient;
    FOnGradientColorsChanged: TNotifyEvent;
    function GetGradientEntry(Index: Integer): TColor32Gradient;
    function GetGradientCount: Integer; {$IFDEF USEINLINING}inline;{$ENDIF}
    function GetStartColor: TColor32;
    function GetEndColor: TColor32;
  protected
    procedure GradientColorsChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Color: TColor32); overload;
    constructor Create(StartColor, EndColor: TColor32); overload;
    constructor Create(const GradientColors: array of TColor32Gradient); overload;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure ClearColors;
    procedure AddColorStop(Offset: TFloat; Color: TColor32); virtual;
    procedure SetColors(const GradientColors: array of TColor32Gradient);
    function GetColorAt(Fraction: TFloat): TColor32;
    procedure FillColorLookUpTable(var ColorLUT: array of TColor32);

    property GradientEntry[Index: Integer]: TColor32Gradient read GetGradientEntry;
    property GradientCount: Integer read GetGradientCount;
    property StartColor: TColor32 read GetStartColor;
    property EndColor: TColor32 read GetEndColor;
    property OnGradientColorsChanged: TNotifyEvent
      read FOnGradientColorsChanged write FOnGradientColorsChanged;
  end;

  TCustomGradientSampler = class(TCustomSampler)
  private
    FGradient: TGradient32;
    FInitialized: Boolean;
    procedure SetGradient(const Value: TGradient32);
  protected
    procedure GradientChangedHandler(Sender: TObject);
    procedure GradientSamplerChanged; //de-initializes sampler
    property Initialized: Boolean read FInitialized;
  public
    procedure PrepareSampling; override;
    constructor Create;
    destructor Destroy; override;
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
    function GetCenter: TFloatPoint;
    procedure SetCenter(const Value: TFloatPoint);
  public
    procedure PrepareSampling; override;

    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Radius: TFloat read FRadius write SetRadius;
    property Center: TFloatPoint read GetCenter write SetCenter;
  end;

  TColorGradientSpread = (gsPad, gsReflect, gsRepeat);

  TCustomGradientPolygonFiller = class(TCustomPolygonFiller)
  private
    FGradient: TGradient32;
    FOwnsGradient: Boolean;
    FSpread: TColorGradientSpread;
  protected
    procedure FillLineNone(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineSolid(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure GradientFillerChanged; virtual;
  public
    constructor Create; overload;
    constructor Create(ColorGradient: TGradient32); overload; virtual;
    destructor Destroy; override;

    property Gradient: TGradient32 read FGradient;
    property Spread: TColorGradientSpread read FSpread write FSpread;
  end;

  TCustomLinearGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FIncline: TFloat;
    FStartPoint: TFloatPoint;
    FEndPoint: TFloatPoint;
    procedure SetStartPoint(const Value: TFloatPoint);
    procedure SetEndPoint(const Value: TFloatPoint);

    procedure UpdateIncline;
  protected
    procedure EndPointChanged;
    procedure StartPointChanged;
  public
    property StartPoint: TFloatPoint read FStartPoint write SetStartPoint;
    property EndPoint: TFloatPoint read FEndPoint write SetEndPoint;
  end;

  TLinearGradientPolygonFiller = class(TCustomLinearGradientPolygonFiller)
  private
    function ColorStopToScanLine(Index: Integer; Y: Integer): TFloat;
  protected
    function GetFillLine: TFillLineEvent; override;

    procedure FillLineNegative(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLinePositive(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineVertical(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalExtreme(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
  end;

  TLinearGradientLookupTablePolygonFiller = class(TCustomLinearGradientPolygonFiller)
  private
    FInitialized: Boolean;
    FGradientLUT: array [0..LUTSizeMin1] of TColor32;
  protected
    procedure OnBeginRendering; override; //flags initialized
    procedure GradientColorsChangedHandler(Sender: TObject);
    procedure GradientFillerChanged; override;

    function GetFillLine: TFillLineEvent; override;

    procedure FillLineVerticalPad(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalPadExtreme(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalRepeat(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalReflect(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalPadPos(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalPadNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalMirrorNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalMirrorPos(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalRepeatNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalRepeatPos(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);

    property Initialized: Boolean read FInitialized;
   public
     constructor Create(ColorGradient: TGradient32); override;
  end;

  TCustomRadialGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  protected
    FGradientLUT: array [0..LUTSizeMin1] of TColor32;
    FInitialized: Boolean;

    property Initialized: Boolean read FInitialized;
  end;

  TRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)
  private
    FCenter: TFloatPoint;
    FRadius: TFloatPoint;
    FRadScale: TFloat;
    FRadXInv: TFloat;

    FEllipseBounds: TFloatRect;
    procedure SetEllipseBounds(const Value: TFloatRect);
    procedure SetCenter(const Value: TFloatPoint);
    procedure SetRadius(const Value: TFloatPoint);
    procedure UpdateEllipseBounds;
    procedure UpdateRadiusScale;
  protected
    procedure OnBeginRendering; override;
    function GetFillLine: TFillLineEvent; override;
    procedure FillLinePad(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineRepeat(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineReflect(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  public
    property EllipseBounds: TFloatRect read FEllipseBounds write SetEllipseBounds;
    property Radius: TFloatPoint read FRadius write SetRadius;
    property Center: TFloatPoint read FCenter write SetCenter;
  end;

  TSVGRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)
  private
    FColorBuffer: TArrayOfColor32;

    FOffset: TPoint;
    FRadius: TPoint;
    FFocalPt: TFloatPoint;

    FEllipseBounds: TFloatRect;
    FFocalPointNative: TFloatPoint;

    procedure SetEllipseBounds(const Value: TFloatRect);
    procedure SetFocalPoint(const Value: TFloatPoint);
    procedure InitMembers;
    procedure InitColorBuffer;
  protected
    procedure OnBeginRendering; override;
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  public
    property EllipseBounds: TFloatRect read FEllipseBounds write SetEllipseBounds;
    property FocalPoint: TFloatPoint read FFocalPointNative write SetFocalPoint;
  end;

implementation

uses
  GR32_LowLevel, GR32_Math, GR32_Blend, GR32_Geometry;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrWrongFormat = 'Wrong format';

const
  FloatTolerance = 0.001;
  clNone32: TColor32 = $00000000;

{ TGradient32 }

constructor TGradient32.Create(Color: TColor32);
begin
  Create(Color, Color);
end;

constructor TGradient32.Create(StartColor, EndColor: TColor32);
var
  Temp: array of TColor32Gradient;
begin
  SetLength(Temp, 2);
  Temp[0].Offset := 0;
  Temp[0].Color32 := StartColor;
  Temp[1].Offset := 1;
  Temp[1].Color32 := EndColor;
  Create(Temp);
end;

constructor TGradient32.Create(const GradientColors: array of TColor32Gradient);
begin
  inherited Create;
  SetColors(GradientColors);
end;

procedure TGradient32.AssignTo(Dest: TPersistent);
begin
  if Dest is TGradient32 then
    TGradient32(Dest).SetColors(self.FGradientColors) else
    inherited;
end;

procedure TGradient32.ClearColors;
begin
  SetLength(FGradientColors, 0);
  GradientColorsChanged;
end;

procedure TGradient32.SetColors(const GradientColors: array of TColor32Gradient);
var
  Index: Integer;
begin
  if Length(GradientColors) = 0 then
  begin
    if Length(FGradientColors) > 0 then
      ClearColors;
  end else
  begin
    SetLength(FGradientColors, Length(GradientColors));
    for Index := 0 to Length(GradientColors) - 1 do
      FGradientColors[Index] := GradientColors[Index];
    GradientColorsChanged;
  end;
end;

function TGradient32.GetGradientCount: Integer;
begin
  Result := Length(FGradientColors);
end;

function TGradient32.GetGradientEntry(Index: Integer): TColor32Gradient;
begin
  if Index > Length(FGradientColors) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    Result := FGradientColors[Index];
end;

function TGradient32.GetStartColor: TColor32;
begin
  if Length(FGradientColors) = 0 then
    Result := clNone32
  else
    Result := FGradientColors[0].Color32;
end;

function TGradient32.GetEndColor: TColor32;
var
  Count: Integer;
begin
  Count := Length(FGradientColors);
  if Count = 0 then
    Result := clNone32
  else
    Result := FGradientColors[Count - 1].Color32;
end;

function TGradient32.GetColorAt(Fraction: TFloat): TColor32;
var
  I, Count: Integer;
begin
  Count := GradientCount;
  if (Count = 0) or (Fraction <= FGradientColors[0].Offset) then
    Result := StartColor
  else if (Fraction >= FGradientColors[Count - 1].Offset) then
    Result := EndColor
  else
  begin
    I := 1;
    while (I < Count) and (Fraction > FGradientColors[I].Offset) do
      Inc(I);
    Fraction := (Fraction - FGradientColors[I - 1].Offset) /
      (FGradientColors[I].Offset - FGradientColors[I - 1].Offset);
    if Fraction <= 0 then
      Result := FGradientColors[I - 1].Color32
    else if Fraction >= 1 then
      Result := FGradientColors[I].Color32
    else
    begin
      Result := CombineReg(FGradientColors[I].Color32,
        FGradientColors[I - 1].Color32, Round($FF * Fraction));
      EMMS;
    end;
  end;
end;

procedure TGradient32.FillColorLookUpTable(var ColorLUT: array of TColor32);
var
  LutIndex, StopIndex, Count, HighLUT: Integer;
  RecalculateScale: Boolean;
  Fraction, LocalFraction, Delta, Scale: TFloat;
begin
  HighLUT := High(ColorLUT);
  Count := GradientCount;

  //check trivial case
  if (Count < 2) or (HighLUT < 2) then
  begin
    for LutIndex := 0 to HighLUT do
      ColorLUT[LutIndex] := StartColor;
    Exit;
  end;

  ColorLUT[0] := StartColor;
  ColorLUT[HighLUT] := EndColor;
  Delta := 1 / HighLUT;
  Fraction := Delta;

  LutIndex := 1;
  while Fraction <= FGradientColors[0].Offset do
  begin
    ColorLUT[LutIndex] := ColorLUT[0];
    Fraction := Fraction + Delta;
    Inc(LutIndex);
  end;

  Scale := 1;
  StopIndex := 1;
  RecalculateScale := True;
  for LutIndex := LutIndex to HighLUT - 1 do
  begin
    // eventually search next stop
    while (Fraction > FGradientColors[StopIndex].Offset) do
    begin
      Inc(StopIndex);
      if (StopIndex >= Count) then
        Break;
      RecalculateScale := True;
    end;

    // eventually fill remaining LUT
    if StopIndex = Count then
    begin
      for StopIndex := LutIndex to HighLUT - 1 do
        ColorLUT[StopIndex] := ColorLUT[HighLUT];
      Break;
    end;

    // eventually recalculate scale
    if RecalculateScale then
      Scale := 1 / (FGradientColors[StopIndex].Offset -
        FGradientColors[StopIndex - 1].Offset);

    // calculate current color
    LocalFraction := (Fraction - FGradientColors[StopIndex - 1].Offset) * Scale;
    if LocalFraction <= 0 then
      ColorLUT[LutIndex] := FGradientColors[StopIndex - 1].Color32
    else if LocalFraction >= 1 then
      ColorLUT[LutIndex] := FGradientColors[StopIndex].Color32
    else
    begin
      ColorLUT[LutIndex] := CombineReg(FGradientColors[StopIndex].Color32,
        FGradientColors[StopIndex - 1].Color32, Round($FF * LocalFraction));
      EMMS;
    end;
    Fraction := Fraction + Delta;
  end;
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
  OldCount := Length(FGradientColors);
  Index := 0;
  while (Index < OldCount) and (Offset >= FGradientColors[Index].Offset) do
    Inc(Index);
  SetLength(FGradientColors, OldCount + 1);
  if (Index < OldCount) then
    Move(FGradientColors[Index], FGradientColors[Index + 1],
      (OldCount - Index) * SizeOf(TColor32Gradient));
  FGradientColors[Index].Offset := Offset;
  FGradientColors[Index].Color32 := Color;
  GradientColorsChanged;
end;

procedure TGradient32.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ChunkName: array [0..3] of AnsiChar;
  ValueInt: Integer;
  ValueFloat: Single;
begin
  // read simple header
  Stream.Read(ChunkName, 4);
  if ChunkName <> 'Grad' then
    raise Exception.Create(RCStrWrongFormat);
  Stream.Read(ValueInt, 4);
  SetLength(FGradientColors, ValueInt);

  // read data
  for Index := 0 to Length(FGradientColors) - 1 do
  begin
    ValueFloat := FGradientColors[Index].Offset;
    Stream.Read(ValueFloat, 4);
    ValueInt := FGradientColors[Index].Color32;
    Stream.Read(ValueInt, 4);
  end;

  GradientColorsChanged;
end;

procedure TGradient32.SaveToStream(Stream: TStream);
var
  Index: Integer;
  ChunkName: array [0..3] of AnsiChar;
  ValueInt: Integer;
  ValueFloat: Single;
begin
  // write simple header
  ChunkName := 'Grad';
  Stream.Write(ChunkName, 4);
  ValueInt := Length(FGradientColors);
  Stream.Write(ValueInt, 4);

  // write data
  for Index := 0 to Length(FGradientColors) - 1 do
  begin
    ValueFloat := FGradientColors[Index].Offset;
    Stream.Write(ValueFloat, 4);
    ValueInt := FGradientColors[Index].Color32;
    Stream.Write(ValueInt, 4);
  end;
end;

{ TCustomGradientSampler }

constructor TCustomGradientSampler.Create;
begin
  inherited;
  FGradient := TGradient32.Create;
end;

destructor TCustomGradientSampler.Destroy;
begin
  FGradient.Free;
  inherited;
end;

procedure TCustomGradientSampler.SetGradient(const Value: TGradient32);
begin
  if not assigned(Value) then
    FGradient.ClearColors else
    Value.AssignTo(self);
  GradientSamplerChanged;
end;

procedure TCustomGradientSampler.GradientChangedHandler(Sender: TObject);
begin
  GradientSamplerChanged;
end;

procedure TCustomGradientSampler.GradientSamplerChanged;
begin
  FInitialized := False;
end;

procedure TCustomGradientSampler.PrepareSampling;
begin
  inherited;
  FInitialized := True;
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

procedure TRadialGradientSampler.PrepareSampling;
begin
  if not Initialized then
  begin
    FGradient.FillColorLookUpTable(FGradientLUT);
    inherited;
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

function TRadialGradientSampler.GetCenter: TFloatPoint;
begin
  Result.X := FCenterX;
  Result.Y := FCenterY;
end;

procedure TRadialGradientSampler.SetCenter(const Value: TFloatPoint);
begin
  FCenterX := Value.X;
  FCenterY := Value.Y;
end;


procedure FillLineAlpha(var Dst: PColor32; var AlphaValues: PColor32;
  Count: Integer; Color: TColor32); {$IFDEF USEINLINING}inline;{$ENDIF}
var
  X: Integer;
begin
  for X := 0 to Count - 1 do
  begin
    BlendMemEx(Color, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

{TCustomGradientPolygonFiller}

constructor TCustomGradientPolygonFiller.Create;
begin
  Create(TGradient32.Create);
  FOwnsGradient := True;
end;

constructor TCustomGradientPolygonFiller.Create(ColorGradient: TGradient32);
begin
  FOwnsGradient := False;
  FGradient := ColorGradient;
  inherited Create;
end;

destructor TCustomGradientPolygonFiller.Destroy;
begin
  if FOwnsGradient then
    FGradient.Free
  else
    FGradient.OnGradientColorsChanged := nil;
  inherited;
end;

procedure TCustomGradientPolygonFiller.FillLineNone(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
begin
  // do nothing!
end;

procedure TCustomGradientPolygonFiller.FillLineSolid(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
begin
  FillLineAlpha(Dst, AlphaValues, Length, FGradient.StartColor);
end;

procedure TCustomGradientPolygonFiller.GradientFillerChanged;
begin
  // do nothing
end;


{ TCustomLinearGradientPolygonFiller }

procedure TCustomLinearGradientPolygonFiller.SetStartPoint(const Value: TFloatPoint);
begin
  if (FStartPoint.X <> Value.X) or (FStartPoint.Y <> Value.Y) then
  begin
    FStartPoint := Value;
    StartPointChanged;
  end;
end;

procedure TCustomLinearGradientPolygonFiller.SetEndPoint(const Value: TFloatPoint);
begin
  if (FEndPoint.X <> Value.X) or (FEndPoint.Y <> Value.Y) then
  begin
    FEndPoint := Value;
    EndPointChanged;
  end;
end;

procedure TCustomLinearGradientPolygonFiller.StartPointChanged;
begin
  GradientFillerChanged;
  UpdateIncline;
end;

procedure TCustomLinearGradientPolygonFiller.EndPointChanged;
begin
  GradientFillerChanged;
  UpdateIncline;
end;

procedure TCustomLinearGradientPolygonFiller.UpdateIncline;
begin
  if (FEndPoint.X - FStartPoint.X) <> 0 then
    FIncline := (FEndPoint.Y - FStartPoint.Y) / (FEndPoint.X - FStartPoint.X)
  else
  if (FEndPoint.Y - FStartPoint.Y) <> 0 then
    FIncline := 1 / (FEndPoint.Y - FStartPoint.Y);
end;


{ TLinearGradientPolygonFiller }

function TLinearGradientPolygonFiller.ColorStopToScanLine(Index: Integer;
  Y: Integer): TFloat;
var
  Offset: TFloat;
begin
  Offset := FGradient.FGradientColors[Index].Offset;
  Result := (1 - Offset) * FStartPoint.X + Offset * FEndPoint.X +
    ((1 - Offset) * (FStartPoint.Y - Y) + Offset * (FEndPoint.Y - Y)) * FIncline;
end;

function TLinearGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FGradient.GradientCount of
    0: Result := FillLineNone;
    1: Result := FillLineSolid;
    else
      if FStartPoint.X = FEndPoint.X then
        if FStartPoint.Y = FEndPoint.Y then
          Result := FillLineVerticalExtreme
        else
          Result := FillLineVertical
      else
      if FStartPoint.X < FEndPoint.X then
        Result := FillLinePositive
      else
        Result := FillLineNegative;
  end;
end;

procedure TLinearGradientPolygonFiller.FillLineVertical(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  Color32 := FGradient.GetColorAt((DstY - FStartPoint.Y) * FIncline);

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLineVerticalExtreme(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  if DstY < FStartPoint.Y then
    Color32 := FGradient.StartColor
  else
    Color32 := FGradient.EndColor;

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLinePositive(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index: Integer;
  IntScale, IntValue: Integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of Integer;
begin
  // set first offset/position
  XOffset[0] := ColorStopToScanLine(0, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineSolid(Dst, DstX, DstY, Length, AlphaValues);
    Exit;
  end;

  // set start color
  Colors[0] := FGradient.FGradientColors[0].Color32;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0]);

  Index := 1;
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := FGradient.FGradientColors[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        BlendMemEx(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Inc(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index = FGradient.GradientCount);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0]);
end;


procedure TLinearGradientPolygonFiller.FillLineNegative(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index: Integer;
  IntScale, IntValue: Integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of Integer;
begin
  Index := FGradient.GradientCount - 1;

  // set first offset/position
  XOffset[0] := ColorStopToScanLine(Index, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // set start color
  Colors[0] := FGradient.FGradientColors[Index].Color32;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, Colors[0]);
    Exit;
  end;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0]);

  Dec(Index);
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := FGradient.FGradientColors[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        BlendMemEx(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Dec(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index < 0);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0]);
end;


{ TLinearGradientLookupTablePolygonFiller }

constructor TLinearGradientLookupTablePolygonFiller.Create(
  ColorGradient: TGradient32);
begin
  inherited;
  FGradient.OnGradientColorsChanged := GradientColorsChangedHandler;
end;

procedure TLinearGradientLookupTablePolygonFiller.GradientColorsChangedHandler(Sender: TObject);
begin
  GradientFillerChanged;
end;

procedure TLinearGradientLookupTablePolygonFiller.GradientFillerChanged;
begin
end;

function TLinearGradientLookupTablePolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FGradient.GradientCount of
    0: Result := FillLineNone;
    1: Result := FillLineSolid;
    else
      case FSpread of
        gsPad:
          if FStartPoint.X = FEndPoint.X then
            if FStartPoint.Y = FEndPoint.Y then
              Result := FillLineVerticalPadExtreme
            else
              Result := FillLineVerticalPad
          else
          if FStartPoint.X < FEndPoint.X then
            Result := FillLineHorizontalPadPos
          else
            Result := FillLineHorizontalPadNeg;
        gsReflect:
          if FStartPoint.X = FEndPoint.X then
            Result := FillLineVerticalReflect
          else
          if FStartPoint.X < FEndPoint.X then
            Result := FillLineHorizontalMirrorPos
          else
            Result := FillLineHorizontalMirrorNeg;
        gsRepeat:
          if FStartPoint.X = FEndPoint.X then
            Result := FillLineVerticalRepeat
          else
          if FStartPoint.X < FEndPoint.X then
            Result := FillLineHorizontalRepeatPos
          else
            Result := FillLineHorizontalRepeatNeg;
      end;
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalPad(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  Color32 := FGradientLUT[EnsureRange(Round(LUTSizeMin1 *
    (DstY - FStartPoint.Y) * FIncline), 0, LUTSizeMin1)];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalPadExtreme(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  if DstY < FStartPoint.Y then
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

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalReflect(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  Color32 := FGradientLUT[Mirror(Round(LUTSizeMin1 *
    (DstY - FStartPoint.Y) * FIncline), LUTSizeMin1)];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalRepeat(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  X := Round(LUTSizeMin1 * (DstY - FStartPoint.Y) * FIncline);
  Color32 := FGradientLUT[Wrap(X, LUTSizeMin1)];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalPadPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, XPos, Count: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, FGradientLUT[0]);
    Exit;
  end;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, FGradientLUT[LUTSizeMin1]);
    Exit;
  end;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGradientLUT[EnsureRange(Round((X - XOffset[0]) * Scale), 0,
      LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalPadNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, XPos, Count: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, FGradientLUT[LUTSizeMin1]);
    Exit;
  end;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, FGradientLUT[0]);
    Exit;
  end;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGradientLUT[EnsureRange(Round((XOffset[1] - X) * Scale), 0,
      LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalMirrorPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGradientLUT[Mirror(Round((X - XOffset[0]) * Scale),
      LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalMirrorNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGradientLUT[Mirror(Round((XOffset[1] - X) * Scale),
      LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalRepeatPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, ScaledX: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    ScaledX := Round((X - XOffset[0]) * Scale);
    BlendMemEx(FGradientLUT[Wrap(ScaledX, LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalRepeatNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, ScaledX: Integer;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;

  Scale := LUTSizeMin1 / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    ScaledX := Round((XOffset[1] - X) * Scale);
    BlendMemEx(FGradientLUT[Wrap(ScaledX, LUTSizeMin1)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.OnBeginRendering;
begin
  if not Initialized then
  begin
    FGradient.FillColorLookUpTable(FGradientLUT);
    inherited; //sets initialized = true
  end;
end;


{ TRadialGradientPolygonFiller }

procedure TRadialGradientPolygonFiller.SetEllipseBounds(const Value: TFloatRect);
begin
  with Value do
  begin
    FCenter := FloatPoint((Left + Right) * 0.5, (Top + Bottom) * 0.5);
    FRadius.X := Round((Right - Left) * 0.5);
    FRadius.Y := Round((Bottom - Top) * 0.5);
  end;

  UpdateRadiusScale;
  FEllipseBounds := Value;
end;

procedure TRadialGradientPolygonFiller.SetCenter(const Value: TFloatPoint);
begin
  if (FCenter.X <> Value.X) or (FCenter.Y <> Value.Y) then
  begin
    FCenter := Value;
    UpdateEllipseBounds;
  end;
end;

procedure TRadialGradientPolygonFiller.SetRadius(const Value: TFloatPoint);
begin
  if (FRadius.X <> Value.X) or (FRadius.Y <> Value.Y) then
  begin
    FRadius := Value;
    UpdateRadiusScale;
    UpdateEllipseBounds;
  end;
end;

procedure TRadialGradientPolygonFiller.UpdateEllipseBounds;
begin
  with FEllipseBounds do
  begin
    Left := FCenter.X - FRadius.X;
    Top := FCenter.X + FRadius.X;
    Right := FCenter.Y - FRadius.Y;
    Bottom := FCenter.Y + FRadius.Y;
  end;
end;

procedure TRadialGradientPolygonFiller.UpdateRadiusScale;
begin
  FRadScale := FRadius.X / FRadius.Y;
  FRadXInv := 1 / FRadius.X;
end;

procedure TRadialGradientPolygonFiller.OnBeginRendering;
begin
  if not Initialized then
  begin
    FGradient.FillColorLookUpTable(FGradientLUT);
    FInitialized := True;
  end;
end;

function TRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FSpread of
    gsPad :
      Result := FillLinePad;
    gsReflect :
      Result := FillLineReflect;
    gsRepeat:
      Result := FillLineRepeat;
  end;
end;

procedure TRadialGradientPolygonFiller.FillLinePad(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, Count: Integer;
  SqrRelRad, RadMax: TFloat;
  YDist, SqrInvRadius: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;

  // small optimization
  Index := Ceil(FCenter.X - FRadius.X);
  if Index > DstX then
  begin
    Count := Min((Index - DstX), Length);
    FillLineAlpha(Dst, AlphaValues, Count, FGradientLUT[LUTSizeMin1]);
    Length := Length - Count;
    if Length = 0 then
      Exit;
    DstX := Index;
  end;

  // further optimization
  if Abs(DstY - FCenter.Y) > FRadius.Y then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, FGradientLUT[LUTSizeMin1]);
    Exit;
  end;

  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  RadMax := (Sqr(FRadius.X) + YDist) * SqrInvRadius;

  for X := DstX to DstX + Length - 1 do
  begin
    SqrRelRad := (Sqr(X - FCenter.X) + YDist) * SqrInvRadius;
    if SqrRelRad > RadMax then
      Index := LUTSizeMin1
    else
      Index := Min(Round(LUTSizeMin1 * FastSqrt(SqrRelRad)), LUTSizeMin1);

    Color32 := FGradientLUT[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineReflect(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, DivResult: Integer;
  SqrInvRadius: TFloat;
  YDist: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  SqrInvRadius := Sqr(FRadXInv);
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round(LUTSizeMin1 * FastSqrt((Sqr(X - FCenter.X) + YDist)
      * SqrInvRadius));
    DivResult := DivMod(Index, LUTSize, Index);
    if Odd(DivResult) then
      Index := LUTSizeMin1 - Index;
    Color32 := FGradientLUT[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineRepeat(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  YDist, SqrInvRadius: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  SqrInvRadius := Sqr(FRadXInv);
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  for X := DstX to DstX + Length - 1 do
  begin
    Color32 := FGradientLUT[Round(LUTSizeMin1 *
      FastSqrt((Sqr(X - FCenter.X) + YDist) * SqrInvRadius)) mod LUTSize];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

(*
procedure TRadialGradientPolygonFiller.FillLineEllipsePad(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Delta: TFloatPoint;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  Delta.Y := Abs(DstY - FCenter.Y);
  for X := DstX to DstX + Length - 1 do
  begin
    Delta.X := Abs(X - FCenter.X);
    if (Delta.X >= FRadius.X) or (Delta.Y >= FRadiusY) then
      Color32 := FGradientLUT[LUTSizeMin1] else
      Color32 := FColorBuffer[Trunc(Delta.Y * FRadius.X + Delta.X)];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;
*)

{ TSVGRadialGradientPolygonFiller }

procedure TSVGRadialGradientPolygonFiller.SetEllipseBounds(const Value: TFloatRect);
begin
  FEllipseBounds := Value;
  GradientFillerChanged;
end;

procedure TSVGRadialGradientPolygonFiller.SetFocalPoint(const Value: TFloatPoint);
begin
  FFocalPointNative := Value;
  GradientFillerChanged;
end;

procedure TSVGRadialGradientPolygonFiller.InitMembers;
var
  X, Y: TFloat;
  Temp: TFloat;
begin
  FRadius.X := Round((FEllipseBounds.Right - FEllipseBounds.Left) * 0.5);
  FRadius.Y := Round((FEllipseBounds.Bottom - FEllipseBounds.Top) * 0.5);
  FOffset.X := -Round(FEllipseBounds.Left);
  FOffset.Y := -Round(FEllipseBounds.Top);
  //make FFocalPoint relative to the ellipse midpoint ...
  FFocalPt.X :=
    FFocalPointNative.X - (FEllipseBounds.Right + FEllipseBounds.Left) * 0.5;
  FFocalPt.Y :=
    FFocalPointNative.Y - (FEllipseBounds.Bottom + FEllipseBounds.Top) * 0.5;

  //make sure the focal point stays within the bounding ellipse ...
  if Abs(FFocalPt.X) < FloatTolerance then
  begin
    X := 0;
    if FFocalPt.Y < 0 then
      Y := -1 else
      Y := 1;
  end else
    begin
      Temp := FRadius.X * FFocalPt.Y / (FRadius.Y * FFocalPt.X);
      X := 1 / Sqrt(1 + Sqr(Temp));
      Y := Temp * X;
    end;
  if FFocalPt.X < 0 then
  begin
    X := -X;
    Y := -Y;
  end;
  X := X * FRadius.X;
  Y := Y * FRadius.Y;
  if (Y * Y + X * X) < (Sqr(FFocalPt.X) + Sqr(FFocalPt.Y)) then
  begin
    FFocalPt.X := Trunc(X);
    FFocalPt.Y := Trunc(Y);
  end;
end;

procedure TSVGRadialGradientPolygonFiller.InitColorBuffer;
var
  I, J, FocalPointX, RadXSqrd, RadYSqrd, DiamX, DiamY: Integer;
  Rad, Rad2, X2, Y2, DistYDown, DistYUp: TFloat;
  m, b, Qa, Qb, Qc, Qz: Double;
begin
  if (FRadius.X = 0) or (FRadius.Y = 0) then
    Exit;

  FGradient.FillColorLookUpTable(FGradientLUT);

  RadXSqrd := FRadius.X * FRadius.X;
  RadYSqrd := FRadius.Y * FRadius.Y;

  DiamX := (FRadius.X * 2) + 1;
  DiamY := (FRadius.Y * 2) + 1;
  FocalPointX := Round(FFocalPt.X);

  //Because the slope of vertical lines is infinite, we need to find where a
  //vertical line through the FocalPoint intersects with the Ellipse, and
  //store the distances from the focal point to these 2 intersections points ...
  Y2 := Sqrt(RadYSqrd - Sqr(FFocalPt.X) * RadYSqrd / RadXSqrd);
  DistYDown := Abs(Y2 - FFocalPt.Y);
  DistYUp := Abs(-Y2 - FFocalPt.Y);

  SetLength(FColorBuffer, DiamX * DiamY);
  for I := -FRadius.X to FRadius.X do
    for J := -FRadius.Y to FRadius.Y do
    begin
      Rad := Hypot(I - FFocalPt.X , J - FFocalPt.Y);
      if I = FocalPointX then //ie on the vertical line (see above)
      begin
        if I * I <= RadXSqrd then
        begin
          if J < FFocalPt.Y then
            Rad2 := DistYUp else
            Rad2 := DistYDown;
          if Rad >= Rad2 then
            FColorBuffer[((J + FRadius.Y) * DiamX) + I + FRadius.X] :=
              FGradientLUT[LUTSizeMin1]
          else
            FColorBuffer[((J + FRadius.Y) * DiamX) + i + FRadius.X] :=
              FGradientLUT[Trunc(Rad * LUTSizeMin1 / Rad2)];
        end else
          FColorBuffer[((J + FRadius.Y) * DiamX) + I + FRadius.X] :=
            FGradientLUT[LUTSizeMin1];
        Continue;
      end;

      //equation of line: y = mx + b
      m := (J - FFocalPt.Y) / (I - FFocalPt.X);
      b := FFocalPt.Y - m * FFocalPt.X;
      //equation of ellipse ...
      //  x^2/RadXSqrd + y^2/RadYSqrd = 1
      //  x^2*RadYSqrd/RadXSqrd + y^2 = RadYSqrd
      //  RadYSqrd(x^2) + RadXSqrd(y^2) = RadXSqrd*RadYSqrd
      //simultaneous equations ...
      //  RadYSqrd(x^2) + RadXSqrd(mx + b)^2 = RadXSqrd*RadYSqrd
      //  (RadYSqrd + RadXSqrd*m^2)(x^2) +
      //    RadXSqrd*2mb(x) + RadXSqrd*b^2 - RadXSqrd*RadYSqrd = 0

      //apply quadratic equation ...
      Qa := (RadYSqrd + RadXSqrd * m * m);
      Qb := RadXSqrd * 2 * m * b;
      Qc := RadXSqrd * b * b - RadXSqrd * RadYSqrd;
      Qz := Qb * Qb - 4 * Qa * Qc;
      if Qz >= 0 then
      begin
        Qz := Sqrt(Qz);
        X2 := (-Qb + Qz) / (2 * Qa);
        if (FFocalPt.X > X2) = (I > FFocalPt.X) then
          X2 := (-Qb - Qz) / (2 * Qa);
        Y2 := m * X2 + b;
        Rad2 := Hypot(X2 - FFocalPt.X, Y2 - FFocalPt.Y);
      end else
        Rad2 := Rad;

      if Rad >= Rad2 then
        FColorBuffer[((J + FRadius.Y) * DiamX) + I + FRadius.X] :=
          FGradientLUT[LUTSizeMin1]
      else
        FColorBuffer[((J + FRadius.Y) * DiamX) + I + FRadius.X] :=
          FGradientLUT[Trunc(Rad * LUTSizeMin1 / Rad2)];
    end;
end;

procedure TSVGRadialGradientPolygonFiller.OnBeginRendering;
begin
  if not Initialized then
  begin
    InitMembers;
    InitColorBuffer;
    inherited; //sets initialized = True
  end;
end;

function TSVGRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineEllipse;
end;

procedure TSVGRadialGradientPolygonFiller.FillLineEllipse(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, DiamX, DiamY: Integer;
  Delta: TPoint;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  Delta.Y := DstY + FOffset.Y;
  DiamX := FRadius.X * 2 + 1;
  DiamY := FRadius.Y * 2 + 1;
  for X := DstX to DstX + Length - 1 do
  begin
    Delta.X := X + FOffset.X;
    if (Delta.X < 0) or (Delta.Y < 0) or (Delta.X >= DiamX) or (Delta.Y >= DiamY) then
      Color32 := FGradientLUT[LUTSizeMin1] else
      Color32 := FColorBuffer[Delta.Y * DiamX + Delta.X];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

end.
