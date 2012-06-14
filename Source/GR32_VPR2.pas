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

{$I GR32.inc}

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
    procedure AddLineSegment(X1, Y1, X2, Y2: TFloat);
    procedure DrawBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

implementation

uses
  Math, GR32_VectorUtils, GR32_Math, GR32_LowLevel, GR32_Blend;

{ TPolygonRenderer32VPR2 }

procedure UpdateSpan(var Span: TIntSpan; Value: Integer); inline;
begin
  if Value < Span.Min then Span.Min := Value;
  if Value > Span.Max then Span.Max := Value;
end;

procedure TPolygonRenderer32VPR2.AddLineSegment(X1, Y1, X2, Y2: TFloat);
type
  PFloatArray = ^TFloatArray;
  TFloatArray = array [0..1] of TFloat;
const
  SGN: array [0..1] of Integer = (1, -1);
  EPSILON: TFloat = 0.0001;
var
  Dx, Dy, DyDx, DxDy, Xm, Ym, Xn, Yn, t, tX, tY: Double;
  X, Y, StepX, StepY: Integer;
  P: PFloatArray;

  procedure AddSegment(X1, Y1, X2, Y2: TFloat);
  var
    Dx, Dy: TFloat;
  begin
    Dx := (X1 + X2) * 0.5;
    Dx := Dx - Round(Dx);
    Dy := Y2 - Y1;
    Dx := Dx * Dy;
    P[0] := P[0] + Dy - Dx;
    P[1] := P[1] + Dx;
  end;

begin
  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dy = 0 then Exit;

  X := Round(X1);
  Y := Round(Y1);

  UpdateSpan(FYSpan, Y);

  StepX := Ord(Dx < 0);
  StepY := Ord(Dy < 0);

  X1 := X1 - StepX;
  Y1 := Y1 - StepY;
  X2 := X2 - StepX;
  Y2 := Y2 - StepY;

  StepX := SGN[StepX];
  StepY := SGN[StepY];

  if Dx = 0 then
  begin
    Yn := Y1;
    repeat
      UpdateSpan(FXSpan[Y], X);
      P := PFloatArray(FOpacityMap.ValPtr[X, Y]);
      Ym := Yn;
      Inc(Y, StepY);
      Yn := Y;
      AddSegment(X1, Ym, X1, Yn);
    until Abs(Y1 - Yn) + EPSILON >= Abs(Dy);
    AddSegment(X1, Yn, X1, Y2);
  end
  else
  begin
    DyDx := Dy/Dx;
    DxDy := Dx/Dy;

    tX := X + StepX - X1;
    tY := (Y + StepY - Y1) * DxDy;

    Xn := X1;
    Yn := Y1;

    repeat
      Xm := Xn;
      Ym := Yn;

      UpdateSpan(FXSpan[Y], X);
      P := PFloatArray(FOpacityMap.ValPtr[X, Y]);
      if Abs(tX) <= Abs(tY) then
      begin
        Inc(X, StepX);
        t := tX;
        tX := tX + StepX;
      end
      else
      begin
        Inc(Y, StepY);
        t := tY;
        tY := tY + StepY * DxDy;
      end;
      Xn := X1 + t;
      Yn := Y1 + t * DyDx;
      AddSegment(Xm, Ym, Xn, Yn);
    until Abs(t) + EPSILON >= Abs(Dx);
    AddSegment(Xn, Yn, X2, Y2);
  end;
end;

constructor TPolygonRenderer32VPR2.Create;
begin
  inherited;
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
      V := Abs(Round(Last * $10000));
      if V > $10000 then V := $10000;
      V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
      V := GAMMA_TABLE[V];
{$ENDIF}
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
      V := Abs(Round(Coverage[I] * $10000));
      V := V and $01ffff;
      if V >= $10000 then V := V xor $1ffff;
      V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
      V := GAMMA_TABLE[V];
{$ENDIF}
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

procedure TPolygonRenderer32VPR2.DrawBitmap;
const
  FillProcs: array [TPolyFillMode] of TFillProc = (MakeAlphaEvenOddUP, MakeAlphaNonZeroUP);
var
  I, J, N: Integer;
  Dst: PColor32Array;
  Src: PFloatArray;
  P: PIntSpan;
  FillProc: TFillProc;
  FG: PColor32Array;
begin
  FG := StackAlloc(Bitmap.Width * SizeOf(TColor32));
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
  StackFree(FG);
end;

procedure TPolygonRenderer32VPR2.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  APoints: TArrayOfFloatPoint;
  I, J, H: Integer;
  SavedRoundMode: TFPURoundingMode;
  P: PIntSpan;
begin
  FYSpan := STARTSPAN;
  SavedRoundMode := SetRoundMode(rmDown);
  try
    FOpacityMap.SetSize(Bitmap.Width + 1, Bitmap.Height);

    SetLength(FXSpan, Bitmap.Height);
    for I := 0 to High(FXSpan) do
      FXSpan[I] := STARTSPAN;

    for I := 0 to High(Points) do
    begin
      APoints := ClipPolygon(Points[I], ClipRect);
      H := High(APoints);
      if H <= 0 then Continue;

      for J := 0 to H - 1 do
        AddLineSegment(APoints[J].X, APoints[J].Y, APoints[J + 1].X, APoints[J + 1].Y);
      AddLineSegment(APoints[H].X, APoints[H].Y, APoints[0].X, APoints[0].Y);
    end;

    DrawBitmap;
  finally
    SetRoundMode(SavedRoundMode);
  end;
end;

initialization
  RegisterPolygonRenderer(TPolygonRenderer32VPR2);

end.
