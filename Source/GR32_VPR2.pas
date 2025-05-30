unit GR32_VPR2;

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
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32, GR32_Polygons, GR32_OrdinalMaps;

type
  PIntSpan = ^TIntSpan;
  TIntSpan = record
    Min, Max: Integer;
  end;

const
  STARTSPAN: TIntSpan = (Min: MAXINT; Max: 0);

type
  TPolygonRenderer32VPR2 = class(TPolygonRenderer32)
  private
    FOpacityMap: TFloatMap;
    FXSpan: array of TIntSpan;
    FYSpan: TIntSpan;
    procedure AddLineSegment(X1, Y1, X2, Y2: TFloat); overload;
    procedure DrawBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

  { TPolygonRenderer32VPR2X }

  TPolygonRenderer32VPR2X = class(TPolygonRenderer32)
  private
    FOpacityMap: TIntegerMap;
    FXSpan: array of TIntSpan;
    FYSpan: TIntSpan;
    procedure AddLineSegment(X1, Y1, X2, Y2: TFixed); overload;
    procedure DrawBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

implementation

uses
  Math,
  Types,
  GR32_VectorUtils,
  GR32_Math,
  GR32_LowLevel,
  GR32_Blend;

// FastFloor is slow on x86 due to call overhead
{$if (not defined(PUREPASCAL)) and defined(CPUx86_64)}
// Use of FastFloor currently corrupts the memory manager of FPC
// so temporarily disabled there.
  {$if (not defined(FPC))}
    {$define USE_POLYFLOOR}
  {$ifend}
{$ifend}

function PolyFloor(Value: Single): integer; overload; inline;
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastFloorSingle(Value);
{$else}
  Result := Round(Value);
{$ifend}
end;

function PolyFloor(Value: Double): integer; overload; inline;
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastFloorDouble(Value);
{$else}
  Result := Round(Value);
{$ifend}
end;

{ TPolygonRenderer32VPR2 }

procedure UpdateSpan(var Span: TIntSpan; Value: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if Value < Span.Min then
    Span.Min := Value;

  if Value > Span.Max then
    Span.Max := Value;
end;

procedure TPolygonRenderer32VPR2.AddLineSegment(X1, Y1, X2, Y2: TFloat);
type
  PFloatArray = ^TFloatArray;
  TFloatArray = array [0..1] of TFloat;

const
  SGN: array [0..1] of Integer = (1, -1);
  EPSILON: TFloat = 0.0001;
  EPSILON_Dx: Double = 1 / 255;

var
  P: PFloatArray;

  procedure AddSegment(X1, Y1, X2, Y2: TFloat);
  var
    dX, dY: TFloat;
  begin
    dX := (X1 + X2) * 0.5;
    dX := dX - PolyFloor(dX);
    dY := Y2 - Y1;
    dX := dX * dY;
    P[0] := P[0] + dY - dX;
    P[1] := P[1] + dX;
  end;

var
  dX, dY, dYdX, dXdY: Double;
  PrevX, PrevY, NextX, NextY: Double;
  Delta, DeltaX, DeltaY: Double;
  X, Y, StepX, StepY: Integer;
  MaxDelta: Double;
begin
{$ifndef FPC}
  // Same as (Y2 = Y1)
  if (PCardinal(@Y2)^ = PCardinal(@Y1)^) then
    Exit;
{$else} // Above optimization fails on FPC
  if (Y2 = Y1) then
    Exit;
{$endif}

{$if defined(FloatCast)}
  dY := Double(Y2) - Double(Y1);
  dX := Double(X2) - Double(X1);
{$else}
  dY := Y2 - Y1;
  dX := X2 - X1;
{$ifend}

  X := PolyFloor(X1);
  Y := PolyFloor(Y1);

  UpdateSpan(FYSpan, Y);

  StepX := Ord(dX < 0);
  StepY := Ord(dY < 0);

  X1 := X1 - StepX;
  Y1 := Y1 - StepY;
  X2 := X2 - StepX;
  Y2 := Y2 - StepY;

  StepX := SGN[StepX];
  StepY := SGN[StepY];

  if (Abs(dX) <= EPSILON_Dx) then
  begin

    MaxDelta := Abs(dY) - EPSILON;
    NextY := Y1;

    repeat

      UpdateSpan(FXSpan[Y], X);

      P := PFloatArray(FOpacityMap.ValPtr[X, Y]);

      PrevY := NextY;
      Inc(Y, StepY);
      NextY := Y;

      AddSegment(X1, PrevY, X1, NextY);

    until (Abs(Y1 - NextY) >= MaxDelta);

    AddSegment(X1, NextY, X1, Y2);

  end else
  begin

    dYdX := dY/dX;
    dXdY := dX/dY;

    DeltaX := X + StepX - X1;
    DeltaY := (Y + StepY - Y1) * dXdY;

    MaxDelta := Abs(dX) - EPSILON;
    NextX := X1;
    NextY := Y1;

    repeat
      PrevX := NextX;
      PrevY := NextY;

      UpdateSpan(FXSpan[Y], X);

      P := PFloatArray(FOpacityMap.ValPtr[X, Y]);

      if (Abs(DeltaX) <= Abs(DeltaY)) then
      begin

        Inc(X, StepX);
        Delta := DeltaX;
        DeltaX := DeltaX + StepX;

      end else
      begin

        Inc(Y, StepY);
        Delta := DeltaY;
        DeltaY := DeltaY + StepY * dXdY;

      end;

      NextX := X1 + Delta;
      NextY := Y1 + Delta * dYdX;

      AddSegment(PrevX, PrevY, NextX, NextY);

    until (Abs(Delta) >= MaxDelta);

    AddSegment(NextX, NextY, X2, Y2);

  end;
end;


constructor TPolygonRenderer32VPR2.Create;
begin
  inherited Create;
  FOpacityMap := TFloatMap.Create;
end;

destructor TPolygonRenderer32VPR2.Destroy;
begin
  FOpacityMap.Free;
  inherited;
end;


procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(PolyFloor(Last * $10000)); // TODO : Is Floor the correct operator here?
      if V > $10000 then V := $10000;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

procedure MakeAlphaEvenOddUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(PolyFloor(Coverage[I] * $10000)); // TODO : Is Floor the correct operator here?
      V := V and $01ffff;
      if V >= $10000 then V := V xor $1ffff;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

{$IFDEF UseStackAlloc}{$W+}{$ENDIF}
procedure TPolygonRenderer32VPR2.DrawBitmap;
const
  FillProcs: array [TPolyFillMode] of TFillProc = (MakeAlphaEvenOddUP, MakeAlphaNonZeroUP);
var
  I, N: Integer;
  Dst: PColor32Array;
  Src: PFloatArray;
  P: PIntSpan;
  FillProc: TFillProc;
  FG: PColor32Array;
begin
  {$IFDEF UseStackAlloc}
  FG := StackAlloc(Bitmap.Width * SizeOf(TColor32));
  {$ELSE}
  GetMem(FG, Bitmap.Width * SizeOf(TColor32));
  {$ENDIF}

  FillProc := FillProcs[FillMode];
  FYSpan.Max := Min(FYSpan.Max, Bitmap.Height - 1);
  Assert(FYSpan.Min >= 0);
  Assert(FYSpan.Max < Bitmap.Height);
  for I := FYSpan.Min to FYSpan.Max do
  begin
    P := @FXSpan[I];
    P.Max := Min(P.Max + 1, Bitmap.Width - 1);
    if P.Max < P.Min then Continue;

    N := P.Max - P.Min + 1;
    Dst := Bitmap.Scanline[I];
    Src := PFloatArray(FOpacityMap.ValPtr[0, I]);

    // 1. Cumulative sum
    CumSum(@Src[P.Min], N);

    // 2. Convert opacity to colors
    FillProc(@Src[P.Min], @FG[P.Min], N, Color);

    // 3. Blend colors
    BlendLine(@FG[P.Min], @Dst[P.Min], N);

    // 4. Clear opacity map
    FillLongWord(Src[P.Min], N, 0);
  end;

  {$IFDEF UseStackAlloc}
  StackFree(FG);
  {$ELSE}
  FreeMem(FG);
  {$ENDIF}
end;
{$IFDEF UseStackAlloc}{$W-}{$ENDIF}

{$ifdef FPC}
type
  TRoundingMode = Math.TFPURoundingMode;
{$endif}

procedure TPolygonRenderer32VPR2.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  APoints: TArrayOfFloatPoint;
  I, J, H: Integer;
  R: TFloatRect;
{$if not defined(USE_POLYFLOOR)}
  SavedRoundingMode: TRoundingMode;
{$ifend}
begin
  FYSpan := STARTSPAN;

{$if not defined(USE_POLYFLOOR)}
  SavedRoundingMode := SetRoundMode(rmDown);
  try
{$ifend}

  FOpacityMap.SetSize(Bitmap.Width + 1, Bitmap.Height);

  // temporary fix for floating point rounding errors
  R := ClipRect;
  R.Right := R.Right - 0.0001;
  R.Bottom := R.Bottom - 0.0001;

  SetLength(FXSpan, Bitmap.Height);
  for I := 0 to High(FXSpan) do
    FXSpan[I] := STARTSPAN;

  for I := 0 to High(Points) do
  begin
    APoints := ClipPolygon(Points[I], R);
    H := High(APoints);
    if H <= 0 then Continue;

    for J := 0 to H - 1 do
      AddLineSegment(APoints[J].X, APoints[J].Y, APoints[J + 1].X, APoints[J + 1].Y);
    AddLineSegment(APoints[H].X, APoints[H].Y, APoints[0].X, APoints[0].Y);
  end;

  DrawBitmap;

{$if not defined(USE_POLYFLOOR)}
  finally
    SetRoundMode(SavedRoundingMode);
  end
{$ifend}
end;

//============================================================================//
procedure TPolygonRenderer32VPR2X.AddLineSegment(X1, Y1, X2, Y2: TFixed);
type
  PFixedArray = ^TFixedArray;
  TFixedArray = array [0..1] of TFixed;
const
  SGN: array [0..1] of Integer = (1, -1);
var
  Dx, Dy, DyDx, DxDy, t, tX, tY, Xm, Ym, Xn, Yn: TFixed;
  X, Y, StepX, StepY: Integer;
  P: PFixedArray;

  procedure AddSegment(X1, Y1, X2, Y2: TFixed);
  var
    Dx, Dy: TFixed;
  begin
    Dx := (X1 + X2) shr 1;
    Dx := Dx and $ffff;
    Dy := Y2 - Y1;
    Dx := FixedMul(Dx, Dy);
    P[0] := P[0] + Dy - Dx;
    P[1] := P[1] + Dx;
  end;

begin
  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dy = 0 then Exit;

  X := FixedFloor(X1);
  Y := FixedFloor(Y1);

  UpdateSpan(FYSpan, Y);

  StepX := Ord(Dx < 0);
  StepY := Ord(Dy < 0);

  X1 := X1 - StepX * FixedOne;
  Y1 := Y1 - StepY * FixedOne;
  X2 := X2 - StepX * FixedOne;
  Y2 := Y2 - StepY * FixedOne;

  StepX := SGN[StepX];
  StepY := SGN[StepY];

  if Dx = 0 then
  begin
    Yn := Y1;
    repeat
      UpdateSpan(FXSpan[Y], X);
      P := PFixedArray(FOpacityMap.ValPtr[X, Y]);
      Ym := Yn;
      Inc(Y, StepY);
      Yn := Y * FixedOne;
      AddSegment(X1, Ym, X1, Yn);
    until Abs(Y1 - Yn) >= Abs(Dy);
    AddSegment(X1, Yn, X1, Y2);
  end
  else
  begin
    DyDx := FixedDiv(Dy, Dx);
    DxDy := FixedDiv(Dx, Dy);

    tX := (X + StepX) * FixedOne - X1;
    tY := FixedMul((Y + StepY) * FixedOne - Y1, DxDy);

    Xn := X1;
    Yn := Y1;

    repeat
      Xm := Xn;
      Ym := Yn;

      UpdateSpan(FXSpan[Y], X);
      P := PFixedArray(FOpacityMap.ValPtr[X, Y]);
      if Abs(tX) <= Abs(tY) then
      begin
        Inc(X, StepX);
        t := tX;
        tX := tX + StepX*FixedOne;
      end
      else
      begin
        Inc(Y, StepY);
        t := tY;
        tY := tY + StepY * DxDy;
      end;
      Xn := X1 + t;
      Yn := Y1 + FixedMul(t, DyDx);
      AddSegment(Xm, Ym, Xn, Yn);
    until Abs(t) >= Abs(Dx);
    AddSegment(Xn, Yn, X2, Y2);
  end;
end;

procedure CumSumX(PSrc: PFixedArray; N: Integer);
var
  I: Integer;
begin
  for I := 1 to N - 1 do
    Inc(PSrc[I], PSrc[I - 1]);
end;

procedure MakeAlphaNonZeroUPX(Coverage: PFixedArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I, V, M, Last: Integer;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := MaxInt;
  for I := 0 to Count - 1 do
  begin
    if Last <> Coverage[I] then
    begin
      V := Abs(Coverage[I]);
      if V > $ffff then V := $ffff;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

procedure MakeAlphaEvenOddUPX(Coverage: PFixedArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I, V, M, Last: Integer;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := MaxInt;
  for I := 0 to Count - 1 do
  begin
    if Last <> Coverage[I] then
    begin
      V := Abs(Coverage[I]);
      V := V and $01ffff;
      if V >= $10000 then V := V xor $1ffff;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

{$IFDEF UseStackAlloc}{$W+}{$ENDIF}
procedure TPolygonRenderer32VPR2X.DrawBitmap;
type
  TFillProcX = procedure(Coverage: PFixedArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);
const
  FillProcs: array [TPolyFillMode] of TFillProcX = (MakeAlphaEvenOddUPX, MakeAlphaNonZeroUPX);
var
  I, N: Integer;
  Dst: PColor32Array;
  Src: PFixedArray;
  P: PIntSpan;
  FillProc: TFillProcX;
  FG: PColor32Array;
begin
  {$IFDEF UseStackAlloc}
  FG := StackAlloc(Bitmap.Width * SizeOf(TColor32));
  {$ELSE}
  GetMem(FG, Bitmap.Width * SizeOf(TColor32));
  {$ENDIF}

  FillProc := FillProcs[FillMode];
  FYSpan.Max := Min(FYSpan.Max, Bitmap.Height - 1);
  Assert(FYSpan.Min >= 0);
  Assert(FYSpan.Max < Bitmap.Height);
  for I := FYSpan.Min to FYSpan.Max do
  begin
    P := @FXSpan[I];
    P.Max := Min(P.Max + 1, Bitmap.Width - 1);
    if P.Max < P.Min then Continue;

    N := P.Max - P.Min + 1;
    Dst := Bitmap.Scanline[I];
    Src := PFixedArray(FOpacityMap.ValPtr[0, I]);

    // 1. Cumulative sum
    CumSumX(@Src[P.Min], N);

    // 2. Convert opacity to colors
    FillProc(@Src[P.Min], @FG[P.Min], N, Color);

    // 3. Blend colors
    BlendLine(@FG[P.Min], @Dst[P.Min], N);

    // 4. Clear opacity map
    FillLongWord(Src[P.Min], N, 0);
  end;

  {$IFDEF UseStackAlloc}
  StackFree(FG);
  {$ELSE}
  FreeMem(FG);
  {$ENDIF}
end;
{$IFDEF UseStackAlloc}{$W-}{$ENDIF}

procedure TPolygonRenderer32VPR2X.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  APoints: TArrayOfFloatPoint;
  I, J, H: Integer;
  R: TFloatRect;
begin
  FYSpan := STARTSPAN;

  FOpacityMap.SetSize(Bitmap.Width + 1, Bitmap.Height);

  // temporary fix for floating point rounding errors
  R := ClipRect;
{$ifndef FPC}
  R.Inflate(-0.05, -0.05);
{$else}
  GR32.InflateRect(R, -0.05, -0.05);
{$endif}

  SetLength(FXSpan, Bitmap.Height);
  for I := 0 to High(FXSpan) do
    FXSpan[I] := STARTSPAN;

  for I := 0 to High(Points) do
  begin
    APoints := ClipPolygon(Points[I], R);
    H := High(APoints);
    if H <= 0 then Continue;

    for J := 0 to H - 1 do
      AddLineSegment(Fixed(APoints[J].X), Fixed(APoints[J].Y), Fixed(APoints[J + 1].X), Fixed(APoints[J + 1].Y));
    AddLineSegment(Fixed(APoints[H].X), Fixed(APoints[H].Y), Fixed(APoints[0].X), Fixed(APoints[0].Y));
  end;

  DrawBitmap;
end;

constructor TPolygonRenderer32VPR2X.Create;
begin
  inherited Create;
  FOpacityMap := TIntegerMap.Create;
end;

destructor TPolygonRenderer32VPR2X.Destroy;
begin
  FOpacityMap.Free;
  inherited Destroy;
end;

initialization
  RegisterPolygonRenderer(TPolygonRenderer32VPR2);

  // TPolygonRenderer32VPR2X has been disabled as it's incomplete.
  // It causes AVs - and always have.
  // RegisterPolygonRenderer(TPolygonRenderer32VPR2X);
end.
