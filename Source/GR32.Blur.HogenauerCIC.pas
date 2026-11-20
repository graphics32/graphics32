unit GR32.Blur.HogenauerCIC;

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
 * The Original Code is Hogenauer Blur for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32,
  GR32_OrdinalMaps;

//------------------------------------------------------------------------------
//
//      Hogenauer / Cascaded Integrator-Comb (CIC) Gaussian Blur
//
//------------------------------------------------------------------------------
// HogenauerBlur32, HogenauerAlphaBlur32, and HogenauerBlur8 implement a 3-stage
// Hogenauer / Cascaded Integrator-Comb (CIC) filter pipeline with exact phase
// centering and zero per-scanline heap allocation.
//
// References:
//
// [1] An economical class of digital filters for decimation and interpolation
//     Eugene B. Hogenauer
//     IEEE Transactions on Acoustics, Speech, and Signal Processing 29(2):155-162, May 1981
//
// [2] Cascaded Integrator-Comb (CIC) Filter Single-Pass B-Spline Blur
//     Kragen Sitaker, Hacker News, March 2021
//     https://news.ycombinator.com/item?id=26547871
//------------------------------------------------------------------------------

procedure HogenauerBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure HogenauerBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure HogenauerHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure HogenauerHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure HogenauerAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure HogenauerAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure HogenauerHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure HogenauerHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure HogenauerBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure HogenauerBlur8(Bitmap: TByteMap; Radius: TFloat); overload;

procedure HogenauerHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure HogenauerHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat); overload;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,
  GR32_Blend,
  GR32.Blur,
  GR32.Transpose,
  GR32_Bindings;

type
  TQuadInt64 = array[0..3] of Int64;
  PQuadInt64Array = ^TQuadInt64Array;
  TQuadInt64Array = array[0..0] of TQuadInt64;
  PInt64Array = ^TInt64Array;
  TInt64Array = array[0..0] of Int64;

//------------------------------------------------------------------------------
// Convert Sigma to CIC Filter Lag (Radius)
//------------------------------------------------------------------------------
function SigmaToLag(Sigma: TFloat): Integer;
begin
  if Sigma < 0.1 then
    Result := 0
  else
    Result := Max(1, Round(Sigma * 1.732050807568877)); // sqrt(3) * sigma
end;

//------------------------------------------------------------------------------
// Single-Pass 3-Stage Hogenauer / CIC Filter for 32-bit ARGB Row
//------------------------------------------------------------------------------
procedure HogenauerBlurRow32(pIn, pOut: PColor32EntryArray; Width, Lag: Integer; HistI3, HistC1, HistC2: PQuadInt64Array);
var
  i, c, totalLen, rightEdge, phaseOffset, outIdx, outVal: Integer;
  inVal: array[0..3] of Integer;
  invScale: Double;
  i1, i2, i3: TQuadInt64;
  c1, c2, c3: TQuadInt64;
begin
  if (Lag <= 0) or (Width <= 1) then
  begin
    if pIn <> pOut then
      Move(pIn^, pOut^, Width * SizeOf(TColor32Entry));
    Exit;
  end;

  totalLen := Width + 3 * Lag;
  phaseOffset := (3 * Lag - 3) div 2;
  invScale := 1.0 / (Double(Lag) * Double(Lag) * Double(Lag));
  rightEdge := Width - 1;

  FillChar(HistI3^, totalLen * SizeOf(TQuadInt64), 0);
  FillChar(HistC1^, totalLen * SizeOf(TQuadInt64), 0);
  FillChar(HistC2^, totalLen * SizeOf(TQuadInt64), 0);

  i1 := Default(TQuadInt64);
  i2 := Default(TQuadInt64);
  i3 := Default(TQuadInt64);

  for i := 0 to totalLen - 1 do
  begin
    for c := 0 to 3 do
    begin
      if i < Width then
        inVal[c] := pIn[i].Planes[c]
      else
        inVal[c] := pIn[rightEdge].Planes[c];

      Inc(i1[c], inVal[c]);
      Inc(i2[c], i1[c]);
      Inc(i3[c], i2[c]);

      HistI3[i][c] := i3[c];

      if i >= Lag then
        c1[c] := i3[c] - HistI3[i - Lag][c]
      else
        c1[c] := i3[c];
      HistC1[i][c] := c1[c];

      if i >= Lag then
        c2[c] := c1[c] - HistC1[i - Lag][c]
      else
        c2[c] := c1[c];
      HistC2[i][c] := c2[c];

      if i >= Lag then
        c3[c] := c2[c] - HistC2[i - Lag][c]
      else
        c3[c] := c2[c];

      outIdx := i - phaseOffset;
      if (outIdx >= 0) and (outIdx < Width) then
      begin
        outVal := Round(c3[c] * invScale);
        if outVal < 0 then
          outVal := 0
        else
        if outVal > 255 then
          outVal := 255;
        pOut[outIdx].Planes[c] := outVal;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Single-Pass 3-Stage Hogenauer / CIC Filter for 8-bit Byte Row
//------------------------------------------------------------------------------
procedure HogenauerBlurRow8(pIn, pOut: PByteArray; Width, Lag: Integer; HistI3, HistC1, HistC2: PInt64Array);
var
  i, totalLen, rightEdge, phaseOffset, outIdx, outVal: Integer;
  inVal: Int64;
  invScale: Double;
  i1, i2, i3: Int64;
  c1, c2, c3: Int64;
begin
  if (Lag <= 0) or (Width <= 1) then
  begin
    if pIn <> pOut then
      Move(pIn^, pOut^, Width);
    Exit;
  end;

  totalLen := Width + 3 * Lag;
  phaseOffset := (3 * Lag - 3) div 2;
  invScale := 1.0 / (Double(Lag) * Double(Lag) * Double(Lag));
  rightEdge := Width - 1;

  FillChar(HistI3^, totalLen * SizeOf(Int64), 0);
  FillChar(HistC1^, totalLen * SizeOf(Int64), 0);
  FillChar(HistC2^, totalLen * SizeOf(Int64), 0);

  i1 := 0; i2 := 0; i3 := 0;

  for i := 0 to totalLen - 1 do
  begin
    if i < Width then
      inVal := pIn[i]
    else
      inVal := pIn[rightEdge];

    Inc(i1, inVal);
    Inc(i2, i1);
    Inc(i3, i2);

    HistI3[i] := i3;

    if i >= Lag then
      c1 := i3 - HistI3[i - Lag]
    else
      c1 := i3;
    HistC1[i] := c1;

    if i >= Lag then
      c2 := c1 - HistC1[i - Lag]
    else
      c2 := c1;
    HistC2[i] := c2;

    if i >= Lag then
      c3 := c2 - HistC2[i - Lag]
    else
      c3 := c2;

    outIdx := i - phaseOffset;
    if (outIdx >= 0) and (outIdx < Width) then
    begin
      outVal := Round(c3 * invScale);
      if outVal < 0 then
        outVal := 0
      else
      if outVal > 255 then
        outVal := 255;
      pOut[outIdx] := outVal;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Internal 32-bit ARGB Hogenauer Blur Core
//------------------------------------------------------------------------------
procedure InternalHogenauerBlur32(Src, Dst: TCustomBitmap32; Sigma: TFloat; TwoDimensional: Boolean);
var
  Lag, RowIndex, MaxLen, TotalLen: Integer;
  HistI3, HistC1, HistC2: PQuadInt64Array;
  TransposedBuffer: PColor32EntryArray;
  pInRow, pOutRow: PColor32EntryArray;
begin
  if (Sigma < GaussianRadiusToSigma) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Src.CopyMapTo(Dst);
    Exit;
  end;

  if Src <> Dst then
    Src.CopyMapTo(Dst);

  Lag := SigmaToLag(Sigma);
  if Lag <= 0 then
    Exit;

  MaxLen := Max(Src.Height, Src.Width);
  TotalLen := MaxLen + 3 * Lag;

  GetMem(HistI3, TotalLen * SizeOf(TQuadInt64));
  GetMem(HistC1, TotalLen * SizeOf(TQuadInt64));
  GetMem(HistC2, TotalLen * SizeOf(TQuadInt64));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.ByteCount);
      try
        // Horizontal pass in-place on Dst
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
          pOutRow := pInRow;
          HogenauerBlurRow32(pInRow, pOutRow, Src.Width, Lag, HistI3, HistC1, HistC2);
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose32(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // Vertical pass on Transposed Buffer
        for RowIndex := 0 to Src.Width - 1 do
        begin
          pInRow := PColor32EntryArray(@TransposedBuffer[RowIndex * Src.Height]);
          HogenauerBlurRow32(pInRow, pInRow, Src.Height, Lag, HistI3, HistC1, HistC2);
        end;

        // Transpose back: TransposedBuffer (H x W) -> Dst (W x H)
        Transpose32(TransposedBuffer, Dst.Bits, Src.Height, Src.Width);
      finally
        FreeMem(TransposedBuffer);
      end;
    end
    else
    begin
      // Horizontal pass only
      for RowIndex := 0 to Src.Height - 1 do
      begin
        pInRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
        pOutRow := pInRow;
        HogenauerBlurRow32(pInRow, pOutRow, Src.Width, Lag, HistI3, HistC1, HistC2);
      end;
    end;
  finally
    FreeMem(HistC2);
    FreeMem(HistC1);
    FreeMem(HistI3);
  end;
end;

//------------------------------------------------------------------------------
// Internal 8-bit ByteMap Hogenauer Blur Core
//------------------------------------------------------------------------------
procedure InternalHogenauerBlur8(Src, Dst: TByteMap; Sigma: TFloat; TwoDimensional: Boolean);
var
  Lag, RowIndex, MaxLen, TotalLen: Integer;
  HistI3, HistC1, HistC2: PInt64Array;
  TransposedBuffer: PByteArray;
  pInRow, pOutRow: PByteArray;
begin
  if (Sigma < GaussianRadiusToSigma) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Dst.Assign(Src);
    Exit;
  end;

  if Src <> Dst then
    Dst.Assign(Src);

  Lag := SigmaToLag(Sigma);
  if Lag <= 0 then
    Exit;

  MaxLen := Max(Src.Height, Src.Width);
  TotalLen := MaxLen + 3 * Lag;

  GetMem(HistI3, TotalLen * SizeOf(Int64));
  GetMem(HistC1, TotalLen * SizeOf(Int64));
  GetMem(HistC2, TotalLen * SizeOf(Int64));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.ByteCount);
      try
        // Horizontal pass in-place on Dst
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PByteArray(Dst.Scanline[RowIndex]);
          pOutRow := pInRow;
          HogenauerBlurRow8(pInRow, pOutRow, Src.Width, Lag, HistI3, HistC1, HistC2);
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose8(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // Vertical pass on Transposed Buffer
        for RowIndex := 0 to Src.Width - 1 do
        begin
          pInRow := PByteArray(@TransposedBuffer[RowIndex * Src.Height]);
          HogenauerBlurRow8(pInRow, pInRow, Src.Height, Lag, HistI3, HistC1, HistC2);
        end;

        // Transpose back: TransposedBuffer (H x W) -> Dst (W x H)
        Transpose8(TransposedBuffer, Dst.Bits, Src.Height, Src.Width);
      finally
        FreeMem(TransposedBuffer);
      end;
    end
    else
    begin
      // Horizontal pass only
      for RowIndex := 0 to Src.Height - 1 do
      begin
        pInRow := PByteArray(Dst.Scanline[RowIndex]);
        pOutRow := pInRow;
        HogenauerBlurRow8(pInRow, pOutRow, Src.Width, Lag, HistI3, HistC1, HistC2);
      end;
    end;
  finally
    FreeMem(HistC2);
    FreeMem(HistC1);
    FreeMem(HistI3);
  end;
end;

//------------------------------------------------------------------------------
// HogenauerBlur32 Public API
//------------------------------------------------------------------------------
procedure HogenauerBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalHogenauerBlur32(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure HogenauerBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalHogenauerBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure HogenauerHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalHogenauerBlur32(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure HogenauerHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalHogenauerBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

//------------------------------------------------------------------------------
// HogenauerAlphaBlur32 Public API (with alpha pre- and unpremultiplication)
//------------------------------------------------------------------------------
procedure HogenauerAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Src.CopyMapTo(Dst);
    Exit;
  end;

  if Src <> Dst then
    Src.CopyMapTo(Dst);

  Premultiply32(Dst);
  HogenauerBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure HogenauerAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  HogenauerBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

procedure HogenauerHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Src.CopyMapTo(Dst);
    Exit;
  end;

  if Src <> Dst then
    Src.CopyMapTo(Dst);

  Premultiply32(Dst);
  HogenauerHorizontalBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure HogenauerHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  HogenauerHorizontalBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

//------------------------------------------------------------------------------
// HogenauerBlur8 Public API
//------------------------------------------------------------------------------
procedure HogenauerBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalHogenauerBlur8(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure HogenauerBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalHogenauerBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure HogenauerHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalHogenauerBlur8(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure HogenauerHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalHogenauerBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

end.
