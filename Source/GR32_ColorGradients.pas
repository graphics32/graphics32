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

type
  TColor32Gradient = record
    Offset: TFloat; //expected range between 0.0 and 1.0
    Color32: TColor32;
  end;
  TArrayOfColor32Gradient = array of TColor32Gradient;

  TLinearGradientType = (lgVertical, lgHorizontal, lgAngled);

  TColor32LookupTable = class(TPersistent)
  private
    FGradientLUT: PColor32Array;
    FOrder: Byte;
    FMask: Cardinal;
    FSize: Cardinal;
    FOnOrderChanged: TNotifyEvent;
    procedure SetOrder(const Value: Byte);
    function GetColor32(Index: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetColor32(Index: Integer; const Value: TColor32);
  protected
    procedure OrderChanged;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Order: Byte = 9); virtual;
    destructor Destroy; override;

    property Order: Byte read FOrder write SetOrder;
    property Size: Cardinal read FSize;
    property Mask: Cardinal read FMask;
    property Color32[Index: Integer]: TColor32 read GetColor32 write SetColor32;
    property Color32Ptr: PColor32Array read FGradientLUT;

    property OnOrderChanged: TNotifyEvent read FOnOrderChanged write FOnOrderChanged;
  end;

  TGradient32 = class(TInterfacedPersistent, IStreamPersist)
  private
    FGradientColors: array of TColor32Gradient;
    FOnGradientColorsChanged: TNotifyEvent;
    function GetGradientEntry(Index: Integer): TColor32Gradient;
    function GetGradientCount: Integer; {$IFDEF USEINLINING}inline;{$ENDIF}
    function GetStartColor: TColor32;
    function GetEndColor: TColor32;
    procedure SetEndColor(const Value: TColor32);
    procedure SetStartColor(const Value: TColor32);
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
    procedure FillColorLookUpTable(var ColorLUT: array of TColor32); overload;
    procedure FillColorLookUpTable(ColorLUT: PColor32Array; Count: Integer); overload;
    procedure FillColorLookUpTable(ColorLUT: TColor32LookupTable); overload;

    property GradientEntry[Index: Integer]: TColor32Gradient read GetGradientEntry;
    property GradientCount: Integer read GetGradientCount;
    property StartColor: TColor32 read GetStartColor write SetStartColor;
    property EndColor: TColor32 read GetEndColor write SetEndColor;
    property OnGradientColorsChanged: TNotifyEvent
      read FOnGradientColorsChanged write FOnGradientColorsChanged;
  end;

  TTriangularGradientSampler = class(TCustomSampler)
  private
    function GetColor(Index: Integer): TColor32;
    function GetPoint(Index: Integer): TFloatPoint;
    procedure SetColor(Index: Integer; const Value: TColor32);
    procedure SetPoint(Index: Integer; const Value: TFloatPoint);
  protected
    FTriangle: array [0 .. 2] of TFloatPoint;
    FColors: array [0 .. 2] of TColor32;
    FNormScale: TFloat;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure PrepareSampling; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Color[Index: Integer]: TColor32 read GetColor write SetColor;
    property Point[Index: Integer]: TFloatPoint read GetPoint write SetPoint;
  end;

  TCustomGradientSampler = class(TCustomSampler)
  private
    FGradient: TGradient32;
    FWrapMode: TWrapMode;
    procedure SetGradient(const Value: TGradient32);
    procedure SetWrapMode(const Value: TWrapMode);
  protected
    FInitialized: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
    procedure GradientChangedHandler(Sender: TObject);
    procedure GradientSamplerChanged; //de-initializes sampler
    procedure WrapModeChanged; virtual;
    procedure UpdateInternals; virtual; abstract;

    property Initialized: Boolean read FInitialized;
  public
    constructor Create; overload; virtual;
    constructor Create(ColorGradient: TGradient32); overload; virtual;
    destructor Destroy; override;

    procedure PrepareSampling; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;

    property Gradient: TGradient32 read FGradient write SetGradient;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
  end;

  TCustomLookUpTableGradientSampler = class(TCustomGradientSampler)
  private
    FGradientLUT: TColor32LookupTable;
    FLutPtr: PColor32Array;
    FLutMask: Integer;
    FWrapProc: TWrapProc;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure WrapModeChanged; override;
    procedure UpdateInternals; override;

    property LutPtr: PColor32Array read FLutPtr;
    property LutMask: Integer read FLutMask;
    property WrapProc: TWrapProc read FWrapProc;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCustomCenterLutGradientSampler = class(TCustomLookUpTableGradientSampler)
  private
    FCenter: TFloatPoint;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Transform(var X, Y: TFloat); virtual;
  public
    constructor Create; override;

    property Center: TFloatPoint read FCenter write FCenter;
  end;

  TConicGradientSampler = class(TCustomCenterLutGradientSampler)
  private
    FScale: TFloat;
    FAngle: TFloat;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Angle: TFloat read FAngle write FAngle;
  end;

  TCustomCenterRadiusLutGradientSampler = class(TCustomCenterLutGradientSampler)
  private
    FRadius: TFloat;
    procedure SetRadius(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: TFloat read FRadius write SetRadius;
  end;

  TRadialGradientSampler = class(TCustomCenterRadiusLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomCenterRadiusAngleLutGradientSampler = class(TCustomCenterRadiusLutGradientSampler)
  private
    FAngle: TFloat;
    FSinCos: TFloatPoint;
    procedure SetAngle(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AngleChanged; virtual;
    procedure RadiusChanged; override;
    procedure Transform(var X, Y: TFloat); override;
  public
    constructor Create; override;

    property Angle: TFloat read FAngle write SetAngle;
  end;

  TDiamondGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TXGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TXYGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TXYSqrtGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomGradientPolygonFiller = class(TCustomPolygonFiller)
  private
    FGradient: TGradient32;
    FOwnsGradient: Boolean;
    FWrapMode: TWrapMode;
    FWrapProc: TWrapProc;
    procedure SetWrapMode(const Value: TWrapMode);
  protected
    procedure FillLineNone(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure FillLineSolid(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
    procedure GradientFillerChanged; virtual;
    procedure WrapModeChanged;
  public
    constructor Create; overload;
    constructor Create(ColorGradient: TGradient32); overload; virtual;
    destructor Destroy; override;

    property Gradient: TGradient32 read FGradient;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
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
    procedure SimpleGradient(StartPoint: TFloatPoint; StartColor: TColor32;
      EndPoint: TFloatPoint; EndColor: TColor32); virtual;
    procedure SetPoints(StartPoint, EndPoint: TFloatPoint); virtual;

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
    FOwnsLUT: Boolean;
    FGradientLUT: TColor32LookupTable;
  protected
    procedure BeginRendering; override; //flags initialized
    procedure GradientColorsChangedHandler(Sender: TObject);
    procedure GradientFillerChanged; override;

    function GetFillLine: TFillLineEvent; override;

    procedure FillLineVerticalPad(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalPadExtreme(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineVerticalWrap(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalPadPos(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalPadNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalWrapNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);
    procedure FillLineHorizontalWrapPos(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32);

    property Initialized: Boolean read FInitialized;
  public
    constructor Create(ColorGradient: TGradient32); overload; override;
    constructor Create(LookupTable: TColor32LookupTable); overload; virtual;
    destructor Destroy; override;

    property GradientLUT: TColor32LookupTable read FGradientLUT;
  end;

  TCustomRadialGradientPolygonFiller = class(TCustomGradientPolygonFiller)
  protected
    FInitialized: Boolean;
    FOwnsLUT: Boolean;
    FGradientLUT: TColor32LookupTable;

    property Initialized: Boolean read FInitialized;
    procedure LUTChangedHandler(Sender: TObject);
  public
    constructor Create(ColorGradient: TGradient32); overload; override;
    constructor Create(LookupTable: TColor32LookupTable); overload; virtual;
    destructor Destroy; override;

    property GradientLUT: TColor32LookupTable read FGradientLUT;
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
    procedure BeginRendering; override;
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
    FOffset: TFloatPoint;
    FRadius: TFloatPoint;
    FCenter: TFloatPoint;
    FFocalPt: TFloatPoint;
    FVertDist: TFloat;

    FEllipseBounds: TFloatRect;
    FFocalPointNative: TFloatPoint;

    procedure SetEllipseBounds(const Value: TFloatRect);
    procedure SetFocalPoint(const Value: TFloatPoint);
    procedure InitMembers;
  protected
    procedure BeginRendering; override;
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  public
    property EllipseBounds: TFloatRect read FEllipseBounds write SetEllipseBounds;
    property FocalPoint: TFloatPoint read FFocalPointNative write SetFocalPoint;
  end;

implementation

uses
  GR32_LowLevel, GR32_System, GR32_Math, GR32_Blend, GR32_Geometry;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrWrongFormat = 'Wrong format';

const
  CFloatTolerance = 0.001;
  clNone32: TColor32 = $00000000;

procedure FillLineAlpha(var Dst, AlphaValues: PColor32; Count: Integer;
  Color: TColor32); {$IFDEF USEINLINING}inline;{$ENDIF}
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
    TGradient32(Dest).SetColors(self.FGradientColors)
  else
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

procedure TGradient32.SetStartColor(const Value: TColor32);
begin
  if Length(FGradientColors) = 0 then
    SetLength(FGradientColors, 1);
  FGradientColors[0].Offset := 0;
  FGradientColors[0].Color32 := Value;
  GradientColorsChanged;
end;

procedure TGradient32.SetEndColor(const Value: TColor32);
begin
  if Length(FGradientColors) = 1 then
    SetLength(FGradientColors, 2);
  FGradientColors[High(FGradientColors)].Color32 := Value;
  FGradientColors[High(FGradientColors)].Offset := 1;
  GradientColorsChanged;
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

procedure TGradient32.FillColorLookUpTable(ColorLUT: TColor32LookupTable);
begin
  FillColorLookUpTable(ColorLUT.Color32Ptr, ColorLUT.Size);
end;

procedure TGradient32.FillColorLookUpTable(var ColorLUT: array of TColor32);
begin
{$WARNINGS OFF}
  FillColorLookUpTable(@ColorLUT[0], Length(ColorLUT));
{$WARNINGS ON}
end;

procedure TGradient32.FillColorLookUpTable(ColorLUT: PColor32Array;
  Count: Integer);
var
  LutIndex, StopIndex, GradCount: Integer;
  RecalculateScale: Boolean;
  Fraction, LocalFraction, Delta, Scale: TFloat;
begin
  GradCount := GradientCount;

  //check trivial case
  if (GradCount < 2) or (Count < 2) then
  begin
    for LutIndex := 0 to Count - 1 do
      ColorLUT^[LutIndex] := StartColor;
    Exit;
  end;

  ColorLUT^[0] := StartColor;
  ColorLUT^[Count - 1] := EndColor;
  Delta := 1 / Count;
  Fraction := Delta;

  LutIndex := 1;
  while Fraction <= FGradientColors[0].Offset do
  begin
    ColorLUT^[LutIndex] := ColorLUT^[0];
    Fraction := Fraction + Delta;
    Inc(LutIndex);
  end;

  Scale := 1;
  StopIndex := 1;
  RecalculateScale := True;
  for LutIndex := LutIndex to Count - 2 do
  begin
    // eventually search next stop
    while (Fraction > FGradientColors[StopIndex].Offset) do
    begin
      Inc(StopIndex);
      if (StopIndex >= GradCount) then
        Break;
      RecalculateScale := True;
    end;

    // eventually fill remaining LUT
    if StopIndex = GradCount then
    begin
      for StopIndex := LutIndex to Count - 2 do
        ColorLUT^[StopIndex] := ColorLUT^[Count];
      Break;
    end;

    // eventually recalculate scale
    if RecalculateScale then
      Scale := 1 / (FGradientColors[StopIndex].Offset -
        FGradientColors[StopIndex - 1].Offset);

    // calculate current color
    LocalFraction := (Fraction - FGradientColors[StopIndex - 1].Offset) * Scale;
    if LocalFraction <= 0 then
      ColorLUT^[LutIndex] := FGradientColors[StopIndex - 1].Color32
    else if LocalFraction >= 1 then
      ColorLUT^[LutIndex] := FGradientColors[StopIndex].Color32
    else
    begin
      ColorLUT^[LutIndex] := CombineReg(FGradientColors[StopIndex].Color32,
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


{ TColor32LookupTable }

procedure TColor32LookupTable.AssignTo(Dest: TPersistent);
begin
  if Dest is TColor32LookupTable then
    with TColor32LookupTable(Dest) do
    begin
      FOrder := Self.FOrder;
      OrderChanged;
      Move(Self.FGradientLUT^, FGradientLUT^, FSize * SizeOf(TColor32));
    end
  else
    inherited;
end;

constructor TColor32LookupTable.Create(Order: Byte);
begin
  inherited Create;
  FOrder := Order;
  OrderChanged;
end;

destructor TColor32LookupTable.Destroy;
begin
{$WARNINGS OFF}
  FreeMem(FGradientLUT);
{$WARNINGS ON}
  inherited;
end;

function TColor32LookupTable.GetColor32(Index: Integer): TColor32;
begin
  Result := FGradientLUT^[Index and FMask];
end;

procedure TColor32LookupTable.OrderChanged;
begin
  FSize := 1 shl FOrder;
  FMask := FSize - 1;
{$WARNINGS OFF}
  GetMem(FGradientLUT, FSize * SizeOf(TColor32));
{$WARNINGS ON}
  if Assigned(FOnOrderChanged) then
    FOnOrderChanged(Self);
end;

procedure TColor32LookupTable.SetColor32(Index: Integer; const Value: TColor32);
begin
  if (Index < 0) or (Index > Integer(FMask)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    FGradientLUT^[Index] := Value;
end;

procedure TColor32LookupTable.SetOrder(const Value: Byte);
begin
  if FOrder <> Value then
  begin
    FOrder := Value;
    OrderChanged;
  end;
end;


{ TTriangularGradientSampler }

procedure TTriangularGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

function TTriangularGradientSampler.GetColor(Index: Integer): TColor32;
begin
  if Index in [0..2] then
    Result := FColors[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TTriangularGradientSampler.GetPoint(Index: Integer): TFloatPoint;
begin
  if Index in [0..2] then
    Result := FTriangle[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TTriangularGradientSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

type
  TTrilinearInterpolation = function (A, B, C: TColor32; WA, WB, WC: Single): TColor32;

var
  TrilinearInterpolation: TTrilinearInterpolation;

function TrilinearInterpolation_Native(A, B, C: TColor32; WA, WB, WC: Single): TColor32;
var
  Clr: TColor32Entry absolute Result;
begin
  Clr.B := Clamp(Round(
    WA * TColor32Entry(A).B +
    WB * TColor32Entry(B).B +
    WC * TColor32Entry(C).B));
  Clr.G := Clamp(Round(
    WA * TColor32Entry(A).G +
    WB * TColor32Entry(B).G +
    WC * TColor32Entry(C).G));
  Clr.R := Clamp(Round(
    WA * TColor32Entry(A).R +
    WB * TColor32Entry(B).R +
    WC * TColor32Entry(C).R));
  Clr.A := Clamp(Round(
    WA * TColor32Entry(A).R +
    WB * TColor32Entry(B).R +
    WC * TColor32Entry(C).R));
end;

function TrilinearInterpolation_SSE2(A, B, C: TColor32; WA, WB, WC: Single): TColor32;
asm
        PXOR      XMM3,XMM3
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM3
        PUNPCKLWD XMM0,XMM3
        CVTDQ2PS  XMM0, XMM0
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM3
        PUNPCKLWD XMM1,XMM3
        CVTDQ2PS  XMM1, XMM1
        MOVD      XMM2,ECX
        PUNPCKLBW XMM2,XMM3
        PUNPCKLWD XMM2,XMM3
        CVTDQ2PS  XMM2, XMM2

        MOV       EAX, WA
        MOV       EDX, WB
        MOV       ECX, WC
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MOVD      XMM6,ECX
        SHUFPS    XMM6,XMM6,0

        MULPS     XMM0,XMM4
        MULPS     XMM1,XMM5
        MULPS     XMM2,XMM6
        ADDPS     XMM0,XMM1
        ADDPS     XMM0,XMM2
        CVTPS2DQ  XMM0,XMM0
        PACKSSDW  XMM0,XMM3
        PACKUSWB  XMM0,XMM3
        MOVD      EAX,XMM0
end;

function TTriangularGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  Temp: TFloatPoint;
  Barycentric: array [0..1] of TFloat;
begin
  Temp.X := (X - FTriangle[2].X);
  Temp.Y := (Y - FTriangle[2].Y);
  Barycentric[0] := FNormScale * ((FTriangle[1].Y - FTriangle[2].Y) * Temp.X +
    (FTriangle[2].X - FTriangle[1].X) * Temp.Y);
  Barycentric[1] := FNormScale * ((FTriangle[2].Y - FTriangle[0].Y) * Temp.X +
    (FTriangle[0].X - FTriangle[2].X) * Temp.Y);

  Result := TrilinearInterpolation(FColors[0], FColors[1], FColors[2],
    Barycentric[0], Barycentric[1], 1 - Barycentric[1] - Barycentric[0]);
end;

function TTriangularGradientSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFloat(X, Y);
end;

procedure TTriangularGradientSampler.PrepareSampling;
begin
  FNormScale := 1 / ((FTriangle[1].Y - FTriangle[2].Y) *
    (FTriangle[0].X - FTriangle[2].X) + (FTriangle[2].X - FTriangle[1].X) *
    (FTriangle[0].Y - FTriangle[2].Y));
end;

procedure TTriangularGradientSampler.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if Index in [0..2] then
    FColors[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TTriangularGradientSampler.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if Index in [0..2] then
    FTriangle[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;


{ TCustomGradientSampler }

constructor TCustomGradientSampler.Create;
begin
  inherited;
  FGradient := TGradient32.Create(clNone32);
  FWrapMode := wmMirror;
  WrapModeChanged;
end;

procedure TCustomGradientSampler.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomGradientSampler then
    with TCustomGradientSampler(Dest) do
    begin
      FGradient.Assign(Self.FGradient);
      FInitialized := False;
      FWrapMode := Self.WrapMode;
    end
  else
    inherited;
end;

constructor TCustomGradientSampler.Create(ColorGradient: TGradient32);
begin
  Create;

  if Assigned(ColorGradient) then
    FGradient.Assign(ColorGradient);
end;

destructor TCustomGradientSampler.Destroy;
begin
  inherited;
end;

procedure TCustomGradientSampler.SetGradient(const Value: TGradient32);
begin
  if not Assigned(Value) then
    FGradient.ClearColors
  else
    Value.AssignTo(Self);
  GradientSamplerChanged;
end;

procedure TCustomGradientSampler.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapModeChanged;
  end;
end;

procedure TCustomGradientSampler.WrapModeChanged;
begin
end;

function TCustomGradientSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

function TCustomGradientSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFloat(X, Y);
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

  if not FInitialized then
  begin
    UpdateInternals;
    FInitialized := True;
  end;
end;


{ TCustomLookUpTableGradientSampler }

procedure TCustomLookUpTableGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomLookUpTableGradientSampler then
    with TCustomLookUpTableGradientSampler(Dest) do
    begin
      FGradientLUT.Assign(Self.FGradientLUT);
      FWrapProc := Self.FWrapProc;
    end
end;

constructor TCustomLookUpTableGradientSampler.Create;
begin
  FGradientLUT := TColor32LookupTable.Create;
  inherited;
end;

destructor TCustomLookUpTableGradientSampler.Destroy;
begin
  FGradientLUT.Free;
  inherited;
end;

procedure TCustomLookUpTableGradientSampler.UpdateInternals;
begin
  FGradient.FillColorLookUpTable(FGradientLUT);
  FLutPtr := FGradientLUT.Color32Ptr;
  FLutMask := FGradientLUT.Mask;
  FWrapProc := GetWrapProc(WrapMode, FGradientLUT.FSize);
end;

procedure TCustomLookUpTableGradientSampler.WrapModeChanged;
begin
  inherited;
  FWrapProc := GetWrapProc(WrapMode);
end;


{ TCustomCenterLutGradientSampler }

procedure TCustomCenterLutGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterLutGradientSampler then
    TCustomCenterLutGradientSampler(Dest).FCenter := Self.FCenter;
end;

constructor TCustomCenterLutGradientSampler.Create;
begin
  inherited;
end;


procedure TCustomCenterLutGradientSampler.Transform(var X, Y: TFloat);
begin
  X := X - FCenter.X;
  Y := Y - FCenter.Y;
  inherited;
end;

{ TConicGradientSampler }

procedure TConicGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TConicGradientSampler then
    TConicGradientSampler(Dest).FAngle := Self.FAngle;
end;

function TConicGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(FScale * Abs(FAngle + ArcTan2(Y, X))),
    FLutMask)];
end;

procedure TConicGradientSampler.UpdateInternals;
begin
  inherited;
  FLutMask := FGradientLUT.Mask;
  FScale := FLutMask / Pi;
end;


{ TCustomCenterRadiusLutGradientSampler }

constructor TCustomCenterRadiusLutGradientSampler.Create;
begin
  inherited;
  FRadius := 1;
  RadiusChanged;
end;

procedure TCustomCenterRadiusLutGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterRadiusLutGradientSampler then
    TCustomCenterRadiusLutGradientSampler(Dest).FRadius := Self.FRadius;
end;

procedure TCustomCenterRadiusLutGradientSampler.RadiusChanged;
begin
  FInitialized := False;
end;

procedure TCustomCenterRadiusLutGradientSampler.SetRadius(
  const Value: TFloat);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    RadiusChanged;
  end;
end;


{ TRadialGradientSampler }

function TRadialGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FGradientLUT.Color32Ptr^[FWrapProc(Round(Sqrt(Sqr(X) + Sqr(Y)) * FScale),
    FLutMask)];
end;

procedure TRadialGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TCustomCenterRadiusAngleLutGradientSampler }

constructor TCustomCenterRadiusAngleLutGradientSampler.Create;
begin
  inherited;
  FAngle := 0;
  FSinCos.X := 1;
  FSinCos.Y := 0;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.AssignTo(
  Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterRadiusAngleLutGradientSampler then
    with TCustomCenterRadiusAngleLutGradientSampler(Dest) do
    begin
      FAngle := Self.FAngle;
      FSinCos := Self.FSinCos;
    end;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.RadiusChanged;
begin
  inherited;
  FInitialized := False;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.AngleChanged;
begin
  GR32_Math.SinCos(FAngle, FSinCos.X, FSinCos.Y);
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.SetAngle(
  const Value: TFloat);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    AngleChanged;
  end;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.Transform(var X,
  Y: TFloat);
var
  Temp: TFloat;
begin
  X := X - FCenter.X;
  Y := Y - FCenter.Y;

  Temp := X * FSinCos.X + Y * FSinCos.Y;
  Y := X * FSinCos.Y - Y * FSinCos.X;
  X := Temp;
end;


{ TDiamondGradientSampler }

function TDiamondGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(Max(Abs(X), Abs(Y)) * FScale), FLutMask)];
end;

procedure TDiamondGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TXGradientSampler }

function TXGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(X * FScale), FLutMask)];
end;

procedure TXGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TXYGradientSampler }

function TXYGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round((Abs(X) * Abs(Y)) * FScale), FLutMask)];
end;

procedure TXYGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / Sqr(FRadius);
end;


{ TXYSqrtGradientSampler }

function TXYSqrtGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(Sqrt(Abs(X) * Abs(Y)) * FScale), FLutMask)];
end;

procedure TXYSqrtGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{TCustomGradientPolygonFiller}

constructor TCustomGradientPolygonFiller.Create;
begin
  Create(TGradient32.Create(clNone32));
  FOwnsGradient := True;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

constructor TCustomGradientPolygonFiller.Create(ColorGradient: TGradient32);
begin
  FOwnsGradient := False;
  FGradient := ColorGradient;
  inherited Create;
end;

destructor TCustomGradientPolygonFiller.Destroy;
begin
  if Assigned(FGradient) then
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

procedure TCustomGradientPolygonFiller.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapModeChanged;
  end;
end;

procedure TCustomGradientPolygonFiller.WrapModeChanged;
begin
  FWrapProc := GetWrapProc(FWrapMode);
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

procedure TCustomLinearGradientPolygonFiller.SimpleGradient(
  StartPoint: TFloatPoint; StartColor: TColor32; EndPoint: TFloatPoint;
  EndColor: TColor32);
begin
  SetPoints(StartPoint, EndPoint);
  if Assigned(FGradient) then
  begin
    FGradient.ClearColors;
    FGradient.StartColor := StartColor;
    FGradient.EndColor := EndColor;
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

procedure TCustomLinearGradientPolygonFiller.SetPoints(StartPoint,
  EndPoint: TFloatPoint);
begin
  FStartPoint := StartPoint;
  FEndPoint := EndPoint;
  GradientFillerChanged;
  UpdateIncline;
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
  FGradientLUT := TColor32LookupTable.Create;
  FOwnsLUT := True;
  inherited Create(ColorGradient);
  FGradient.OnGradientColorsChanged := GradientColorsChangedHandler;
  FGradientLUT.OnOrderChanged := GradientColorsChangedHandler;
end;

constructor TLinearGradientLookupTablePolygonFiller.Create(
  LookupTable: TColor32LookupTable);
begin
  FGradientLUT := LookupTable;
  FOwnsLUT := False;
  FGradient := nil;
  FOwnsGradient := False;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

destructor TLinearGradientLookupTablePolygonFiller.Destroy;
begin
  if FOwnsLUT then
    FGradientLUT.Free;
  inherited;
end;

procedure TLinearGradientLookupTablePolygonFiller.GradientColorsChangedHandler(Sender: TObject);
begin
  GradientFillerChanged;
end;

procedure TLinearGradientLookupTablePolygonFiller.GradientFillerChanged;
begin
  FInitialized := False;
end;

function TLinearGradientLookupTablePolygonFiller.GetFillLine: TFillLineEvent;
var
  GradientCount: Integer;
begin
  if Assigned(FGradient) then
    GradientCount := FGradient.GradientCount
  else
    GradientCount := FGradientLUT.Size;

  case GradientCount of
    0: Result := FillLineNone;
    1: Result := FillLineSolid;
    else
      case FWrapMode of
        wmClamp:
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
        wmMirror, wmRepeat:
          if FStartPoint.X = FEndPoint.X then
            Result := FillLineVerticalWrap
          else
          if FStartPoint.X < FEndPoint.X then
            Result := FillLineHorizontalWrapPos
          else
            Result := FillLineHorizontalWrapNeg;
      end;
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalPad(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  Color32 := FGradientLUT.Color32Ptr^[FWrapProc(Round(FGradientLUT.Mask *
    (DstY - FStartPoint.Y) * FIncline), FGradientLUT.Mask)];

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
    Color32 := FGradientLUT.Color32Ptr^[0]
  else
    Color32 := FGradientLUT.Color32Ptr^[FGradientLUT.Mask];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineVerticalWrap(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
begin
  X := Round(FGradientLUT.Mask * (DstY - FStartPoint.Y) * FIncline);
  Color32 := FGradientLUT.Color32Ptr^[FWrapProc(X, Integer(FGradientLUT.Mask))];

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
  X, XPos, Count, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;
  ColorLUT := FGradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0]);
    Exit;
  end;

  Mask := FGradientLUT.Mask;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(ColorLUT^[FWrapProc(Round((X - XOffset[0]) * Scale), Mask)], Dst^,
      AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalPadNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, XPos, Count, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;

  Mask := FGradientLUT.Mask;
  ColorLUT := FGradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0]);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(ColorLUT^[FWrapProc(Round((XOffset[1] - X) * Scale), Mask)], Dst^,
      AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalWrapPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((X - XOffset[0]) * Scale);
    BlendMemEx(ColorLUT^[FWrapProc(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.FillLineHorizontalWrapNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
begin
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((XOffset[1] - X) * Scale);
    BlendMemEx(ColorLUT^[FWrapProc(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientLookupTablePolygonFiller.BeginRendering;
begin
  if not Initialized then
  begin
    if Assigned(FGradient) then
      FGradient.FillColorLookUpTable(FGradientLUT);
    inherited; //sets initialized = true
  end;
end;


{ TCustomRadialGradientPolygonFiller }

constructor TCustomRadialGradientPolygonFiller.Create(
  ColorGradient: TGradient32);
begin
  inherited;
  FGradientLUT := TColor32LookupTable.Create;
  FGradientLUT.OnOrderChanged := LUTChangedHandler;
end;

constructor TCustomRadialGradientPolygonFiller.Create(
  LookupTable: TColor32LookupTable);
begin
  FGradientLUT := LookupTable;
  FOwnsLUT := False;
  FGradient := nil;
  FOwnsGradient := False;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

destructor TCustomRadialGradientPolygonFiller.Destroy;
begin
  if FOwnsLUT then
    FGradientLUT.Free;
  inherited;
end;

procedure TCustomRadialGradientPolygonFiller.LUTChangedHandler(Sender: TObject);
begin
  FInitialized := False;
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

procedure TRadialGradientPolygonFiller.BeginRendering;
begin
  if not Initialized then
  begin
    if Assigned(FGradient) then
      FGradient.FillColorLookUpTable(FGradientLUT);
    FInitialized := True;
  end;
end;

function TRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FWrapMode of
    wmClamp:
      Result := FillLinePad;
    wmMirror:
      Result := FillLineReflect;
    wmRepeat:
      Result := FillLineRepeat;
  end;
end;

procedure TRadialGradientPolygonFiller.FillLinePad(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, Count, Mask: Integer;
  SqrRelRad, RadMax: TFloat;
  ColorLUT: PColor32Array;
  YDist, SqrInvRadius: TFloat;
  Color32: TColor32;
begin
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  // small optimization
  Index := Ceil(FCenter.X - FRadius.X);
  if Index > DstX then
  begin
    Count := Min((Index - DstX), Length);
    FillLineAlpha(Dst, AlphaValues, Count, ColorLUT^[Mask]);
    Length := Length - Count;
    if Length = 0 then
      Exit;
    DstX := Index;
  end;

  // further optimization
  if Abs(DstY - FCenter.Y) > FRadius.Y then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  RadMax := (Sqr(FRadius.X) + YDist) * SqrInvRadius;

  for X := DstX to DstX + Length - 1 do
  begin
    SqrRelRad := (Sqr(X - FCenter.X) + YDist) * SqrInvRadius;
    if SqrRelRad > RadMax then
      Index := Mask
    else
      Index := Min(Round(Mask * FastSqrt(SqrRelRad)), Mask);

    Color32 := ColorLUT^[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineReflect(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Index, Mask, DivResult: Integer;
  SqrInvRadius: TFloat;
  YDist: TFloat;
  ColorLUT: PColor32Array;
  Color32: TColor32;
begin
  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round(Mask * FastSqrt((Sqr(X - FCenter.X) + YDist)
      * SqrInvRadius));
    DivResult := DivMod(Index, FGradientLUT.Size, Index);
    if Odd(DivResult) then
      Index := Mask - Index;
    Color32 := ColorLUT^[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineRepeat(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  X, Mask: Integer;
  YDist, SqrInvRadius: TFloat;
  ColorLUT: PColor32Array;
  Color32: TColor32;
begin
  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;
  for X := DstX to DstX + Length - 1 do
  begin
    Color32 := ColorLUT^[Round(Mask * FastSqrt((Sqr(X - FCenter.X) + YDist) *
      SqrInvRadius)) mod FGradientLUT.Size];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

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
  FRadius.X := (FEllipseBounds.Right - FEllipseBounds.Left) * 0.5;
  FRadius.Y := (FEllipseBounds.Bottom - FEllipseBounds.Top) * 0.5;
  FCenter.X := (FEllipseBounds.Right + FEllipseBounds.Left) * 0.5;
  FCenter.Y := (FEllipseBounds.Bottom + FEllipseBounds.Top) * 0.5;
  FOffset.X := FEllipseBounds.Left;
  FOffset.Y := FEllipseBounds.Top;
  //make FFocalPoint relative to the ellipse midpoint ...
  FFocalPt.X := FFocalPointNative.X - FCenter.X;
  FFocalPt.Y := FFocalPointNative.Y - FCenter.Y;

  //make sure the focal point stays within the bounding ellipse ...
  if Abs(FFocalPt.X) < CFloatTolerance then
  begin
    X := 0;
    if FFocalPt.Y < 0 then
      Y := -1
    else
      Y := 1;
  end
  else
  begin
    Temp := FRadius.X * FFocalPt.Y / (FRadius.Y * FFocalPt.X);
    X := 1 / FastSqrtBab1(1 + Sqr(Temp));
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
    FFocalPt.X := 0.999 * X;
    FFocalPt.Y := 0.999 * Y;
  end;

  // Because the slope of vertical lines is infinite, we need to find where a
  // vertical line through the FocalPoint intersects with the Ellipse, and
  // store the distances from the focal point to these 2 intersections points
  FVertDist := FRadius.Y * FastSqrtBab1(1 - Sqr(FFocalPt.X) / Sqr(FRadius.X));
end;

procedure TSVGRadialGradientPolygonFiller.BeginRendering;
begin
  if not Initialized then
  begin
    InitMembers;
    if Assigned(FGradient) then
      FGradient.FillColorLookUpTable(FGradientLUT);
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
  X, Mask: Integer;
  ColorLUT: PColor32Array;
  Rad, Rad2, X2, Y2: TFloat;
  m, b, Qa, Qb, Qc, Qz: Double;
  RelPos: TFloatPoint;
  Color32: TColor32;
begin
  if (FRadius.X = 0) or (FRadius.Y = 0) then
    Exit;

  ColorLUT := FGradientLUT.Color32Ptr;

  RelPos.Y := DstY - FCenter.Y - FFocalPt.Y;
  Mask := Integer(FGradientLUT.Mask);

  // check if out of bounds (vertically)
  if (DstY < FOffset.Y) or (DstY >= (FRadius.Y * 2) + 1 + FOffset.Y) then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask]);
    Exit;
  end;

  for X := DstX to DstX + Length - 1 do
  begin
    // check if out of bounds (horizontally)
    if (X < FOffset.X) or (X >= (FRadius.X * 2) + 1 + FOffset.X) then
      Color32 := ColorLUT^[Mask]
    else
    begin
      RelPos.X := X - FCenter.X - FFocalPt.X;

      if Abs(RelPos.X) < CFloatTolerance then //ie on the vertical line (see above)
      begin
        Assert(Abs(X - FCenter.X) <= FRadius.X);

        Rad := Abs(RelPos.Y);
        if Abs(Abs(X - FCenter.X)) <= FRadius.X then
        begin
          if RelPos.Y < 0 then
            Rad2 := Abs(-FVertDist - FFocalPt.Y)
          else
            Rad2 := Abs( FVertDist - FFocalPt.Y);
          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * Rad / Rad2)];
        end else
          Color32 := ColorLUT^[Mask];
      end
      else
      begin
        m := RelPos.Y / RelPos.X;
        b := FFocalPt.Y - m * FFocalPt.X;

        //apply quadratic equation ...
        Qa := 2 * (Sqr(FRadius.Y) + Sqr(FRadius.X) * m * m);
        Qb := Sqr(FRadius.X) * 2 * m * b;
        Qc := Sqr(FRadius.X) * (b * b - Sqr(FRadius.Y));
        Qz := Qb * Qb - 2 * Qa * Qc;

        if Qz >= 0 then
        begin
          Qz := FastSqrtBab2(Qz);
          Qa := 1 / Qa;
          X2 := (-Qb + Qz) * Qa;
          if (FFocalPt.X > X2) = (RelPos.X > 0) then
            X2 := (-Qb - Qz) * Qa;
          Y2 := m * X2 + b;
          Rad := Sqr(RelPos.X) + Sqr(RelPos.Y);
          Rad2 := Sqr(X2 - FFocalPt.X) + Sqr(Y2 - FFocalPt.Y);

          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * FastSqrtBab1(Rad / Rad2))];
        end else
          Color32 := ColorLUT^[Mask]
      end;
    end;

    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

initialization
{$IFNDEF PUREPASCAL}
  if ciSSE2 in CPUFeatures then
    TrilinearInterpolation := TrilinearInterpolation_SSE2
  else
{$ENDIF}
    TrilinearInterpolation := TrilinearInterpolation_Native;

end.
