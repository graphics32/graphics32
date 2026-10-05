unit GR32.Noise.Perlin;

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
 * The Original Code is Perlin Noise for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

//------------------------------------------------------------------------------
//
//      Perlin Noise
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//
//      TPerlinNoise
//
//------------------------------------------------------------------------------
type
  TPerlinNoise = class
  const
    BSize = $100;
    BM = $FF;
    PerlinN = $1000;
  public type
    TStitchInfo = record
      Width: Integer;   // How much to subtract to wrap for stitching
      Height: Integer;
      WrapX: Integer;   // Minimum value to wrap
      WrapY: Integer;
    end;
    PStitchInfo = ^TStitchInfo;

  private
    FSeed: Double;
    FLatticeSelector: array[0..BSize + BSize + 1] of Integer;
    FGradient: array[0..3, 0..BSize + BSize + 1, 0..1] of Double;
    procedure InitWithSeed(ASeed: Double);
  public
    constructor Create(const ASeed: Double = 0.0);
    function Noise2(Channel: Integer; const X, Y: Double; Stitch: PStitchInfo = nil): Double;
    function Turbulence(Channel: Integer; X, Y: Double; BaseFreqX, BaseFreqY: Double;
      NumOctaves: Integer; BFractalSum, BDoStitching: Boolean; TileX, TileY, TileWidth, TileHeight: Double): Double;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils, Math,
  GR32,
  GR32_Math;

{ TPerlinNoise }

const
  RAND_m = 2147483647;
  RAND_a = 16807;
  RAND_q = 127773;
  RAND_r = 2836;

function SetupSeed(LSeed: LongInt): LongInt;
begin
  if LSeed <= 0 then
    LSeed := -(LSeed mod (RAND_m - 1)) + 1;
  if LSeed > RAND_m - 1 then
    LSeed := RAND_m - 1;
  Result := LSeed;
end;

function RandomLCG(LSeed: LongInt): LongInt;
var
  Res: LongInt;
begin
  Res := RAND_a * (LSeed mod RAND_q) - RAND_r * (LSeed div RAND_q);
  if Res <= 0 then
    Res := Res + RAND_m;
  Result := Res;
end;

function SCurve(const T: Double): Double; inline;
begin
  Result := T * T * (3.0 - 2.0 * T);
end;

function Lerp(const T, A, B: Double): Double; inline;
begin
  Result := A + T * (B - A);
end;

//------------------------------------------------------------------------------
//
//      TPerlinNoise
//
//------------------------------------------------------------------------------
constructor TPerlinNoise.Create(const ASeed: Double);
begin
  inherited Create;
  InitWithSeed(ASeed);
end;

procedure TPerlinNoise.InitWithSeed(ASeed: Double);
var
  LSeed: LongInt;
  I, J, K: Integer;
  S: Double;
begin
  FSeed := ASeed;
  LSeed := SetupSeed(Trunc(ASeed));

  for K := 0 to 3 do
  begin
    for I := 0 to BSize - 1 do
    begin
      FLatticeSelector[I] := I;
      for J := 0 to 1 do
      begin
        LSeed := RandomLCG(LSeed);
        FGradient[K, I, J] := ((LSeed mod (BSize + BSize)) - BSize) / BSize;
      end;

      S := GR32_Math.Hypot(FGradient[K, I, 0], FGradient[K, I, 1]);
      if S <> 0 then
      begin
        S := 1 / S;
        FGradient[K, I, 0] := FGradient[K, I, 0] * S;
        FGradient[K, I, 1] := FGradient[K, I, 1] * S;
      end;
    end;
  end;

  I := BSize;
  while True do
  begin
    Dec(I);
    if I <= 0 then
      Break;

    K := FLatticeSelector[I];
    LSeed := RandomLCG(LSeed);
    J := LSeed mod BSize;

    FLatticeSelector[I] := FLatticeSelector[J];
    FLatticeSelector[J] := K;
  end;

  for I := 0 to BSize + 1 do
  begin
    FLatticeSelector[BSize + I] := FLatticeSelector[I];
    for K := 0 to 3 do
      for J := 0 to 1 do
        FGradient[K, BSize + I, J] := FGradient[K, I, J];
  end;
end;

function TPerlinNoise.Noise2(Channel: Integer; const X, Y: Double; Stitch: PStitchInfo): Double;
var
  Bx0, Bx1, By0, By1: Integer;
  B00, B10, B01, B11: Integer;
  Rx0, Rx1, Ry0, Ry1: Double;
  Sx, Sy, A, B, T, U, V: Double;
  I, J: Integer;
begin
  T := X + PerlinN;
  Bx0 := Trunc(T);
  Bx1 := Bx0 + 1;
  Rx0 := T - Trunc(T);
  Rx1 := Rx0 - 1.0;

  T := Y + PerlinN;
  By0 := Trunc(T);
  By1 := By0 + 1;
  Ry0 := T - Trunc(T);
  Ry1 := Ry0 - 1.0;

  if Stitch <> nil then
  begin
    if Bx0 >= Stitch.WrapX then
      Dec(Bx0, Stitch.Width);
    if Bx1 >= Stitch.WrapX then
      Dec(Bx1, Stitch.Width);
    if By0 >= Stitch.WrapY then
      Dec(By0, Stitch.Height);
    if By1 >= Stitch.WrapY then
      Dec(By1, Stitch.Height);
  end;

  Bx0 := Bx0 and BM;
  Bx1 := Bx1 and BM;
  By0 := By0 and BM;
  By1 := By1 and BM;

  I := FLatticeSelector[Bx0];
  J := FLatticeSelector[Bx1];

  B00 := FLatticeSelector[I + By0];
  B10 := FLatticeSelector[J + By0];
  B01 := FLatticeSelector[I + By1];
  B11 := FLatticeSelector[J + By1];

  Sx := SCurve(Rx0);
  Sy := SCurve(Ry0);

  U := Rx0 * FGradient[Channel, B00, 0] + Ry0 * FGradient[Channel, B00, 1];
  V := Rx1 * FGradient[Channel, B10, 0] + Ry0 * FGradient[Channel, B10, 1];
  A := Lerp(Sx, U, V);

  U := Rx0 * FGradient[Channel, B01, 0] + Ry1 * FGradient[Channel, B01, 1];
  V := Rx1 * FGradient[Channel, B11, 0] + Ry1 * FGradient[Channel, B11, 1];
  B := Lerp(Sx, U, V);

  Result := Lerp(Sy, A, B);
end;

function TPerlinNoise.Turbulence(Channel: Integer; X, Y: Double; BaseFreqX, BaseFreqY: Double;
  NumOctaves: Integer; BFractalSum, BDoStitching: Boolean; TileX, TileY, TileWidth, TileHeight: Double): Double;
var
  Stitch: TStitchInfo;
  pStitch: PStitchInfo;
  LoFreq, HiFreq: Double;
  Ratio: Double;
  Octave: Integer;
begin

  if BDoStitching then
  begin
    if BaseFreqX <> 0.0 then
    begin
      LoFreq := Floor(TileWidth * BaseFreqX) / TileWidth;
      HiFreq := Ceil(TileWidth * BaseFreqX) / TileWidth;
      if (LoFreq > 0) and (BaseFreqX / LoFreq < HiFreq / BaseFreqX) then
        BaseFreqX := LoFreq
      else
        BaseFreqX := HiFreq;
    end;

    if BaseFreqY <> 0.0 then
    begin
      LoFreq := Floor(TileHeight * BaseFreqY) / TileHeight;
      HiFreq := Ceil(TileHeight * BaseFreqY) / TileHeight;
      if (LoFreq > 0) and (BaseFreqY / LoFreq < HiFreq / BaseFreqY) then
        BaseFreqY := LoFreq
      else
        BaseFreqY := HiFreq;
    end;

    Stitch.Width := Round(TileWidth * BaseFreqX);
    Stitch.WrapX := Round(TileX * BaseFreqX) + PerlinN + Stitch.Width;
    Stitch.Height := Round(TileHeight * BaseFreqY);
    Stitch.WrapY := Round(TileY * BaseFreqY) + PerlinN + Stitch.Height;

    pStitch := @Stitch;
  end else
    pStitch := nil;

  Result := 0.0;
  // Scale initial X and Y coordinates by their respective base frequencies
  X := X * BaseFreqX;
  Y := Y * BaseFreqY;
  Ratio := 1.0;

  for Octave := 0 to NumOctaves - 1 do
  begin
    if BFractalSum then
      Result := Result + (Noise2(Channel, X, Y, pStitch) * Ratio)
    else
      Result := Result + (Abs(Noise2(Channel, X, Y, pStitch)) * Ratio);

    X := X * 2.0;
    Y := Y * 2.0;
    Ratio := Ratio / 2.0;

    if pStitch <> nil then
    begin
      Stitch.Width := Stitch.Width * 2;
      Stitch.WrapX := 2 * Stitch.WrapX - PerlinN;
      Stitch.Height := Stitch.Height * 2;
      Stitch.WrapY := 2 * Stitch.WrapY - PerlinN;
    end;
  end;
end;

end.
