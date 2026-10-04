unit GR32.Blur.DraftBlur;

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
 * The Original Code is Draft Blur for Graphics32
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
  GR32.Blur,
  GR32_OrdinalMaps;

//------------------------------------------------------------------------------
//
//      Fast Draft Gaussian Blur (Alvarez & Mazorra 2nd-Order Recursive Filter)
//
//------------------------------------------------------------------------------
// Draft Blur provides high-performance 2nd-order recursive Gaussian blur
// implementations utilizing Alvarez & Mazorra's algorithm, prioritizing
// performance over quality.
//
// Adaptive Q-scaling maintains full fixed-point precision for large blur radii
// (sigma > 5..100+) in pure 32-bit integer arithmetic with strict unit DC gain
// normalization.
//
// DraftBlur32:         Performs blur without taking the alpha channel into
//                      account.
//                      It should only be used on opaque bitmaps or when
//                      transparent edge color bleeding is acceptable.
//
// DraftAlphaBlur32:    Handles alpha pre- and unpremultiplication to prevent
//                      edge color bleeding on transparent pixels.
//
// DraftBlur8:          Performs blur on an 8-bit TByteMap.
//
// The *Horizontal* variants performs blur in the horizontal plane only. They
// are typically used to implement anisotropic 2D blur.
//
//------------------------------------------------------------------------------

procedure DraftBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure DraftBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure DraftHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure DraftHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure DraftAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure DraftAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure DraftHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure DraftHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure DraftBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure DraftBlur8(Bitmap: TByteMap; Radius: TFloat); overload;

procedure DraftHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure DraftHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat); overload;


//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,
  GR32_Blend,
  GR32.Transpose,
  GR32_Bindings;

type
  TQuadInt = array[0..3] of Integer;
  PQuadIntArray = ^TQuadIntArray;
  TQuadIntArray = array[0..0] of TQuadInt;
  PIntArray = ^TIntArray;
  TIntArray = array[0..0] of Integer;

type
  TDraftBlurRow32 = procedure(pIn, pOut: PColor32EntryArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PQuadIntArray);
  TDraftBlurRow8 = procedure(pIn, pOut: PByteArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PIntArray);

var
  DraftBlurRow32: TDraftBlurRow32;
  DraftBlurRow8: TDraftBlurRow8;

//------------------------------------------------------------------------------
// Calculate Alvarez & Mazorra 2nd-order coefficients with adaptive Q-scaling
// and strict unit DC gain normalization (iK + i2Q - iQ2 = QScale).
//------------------------------------------------------------------------------
procedure ComputeCoefficientsAlvarezMazorra(Sigma: TFloat; var iK, i2Q, iQ2, QScale: Integer);
var
  Gamma, q, k: Double;
begin
  // Scale Sigma by 1/sqrt(2) so that 2nd-order causal + 2nd-order anti-causal pass
  // produces exact variance sigma^2 and matches the requested blur radius
  Sigma := Sigma * 0.7071067811865475244;
  if Sigma < 0.01 then
    Sigma := 0.01;

  Gamma := Sqr(Sigma) * 0.5; // Gamma = Sigma^2 / 2
  q := 1.0 + (1.0 / Gamma) - Sqrt(Sqr(1.0 / Gamma) + (2.0 / Gamma));
  k := Sqr(1.0 - q);

  // Select optimal fixed-point precision based on k
  if k >= 0.1 then
    QScale := 256
  else
  if k >= 0.01 then
    QScale := 1024
  else
    QScale := 4096;

  i2Q := Round(2.0 * q * QScale);
  iQ2 := Round(Sqr(q) * QScale);
  iK := QScale - i2Q + iQ2;
end;

//------------------------------------------------------------------------------
// Internal 4-channel 32-bit row blur using Alvarez & Mazorra 2nd-order IIR
// in pure 32-bit Integer arithmetic (zero Int64, zero _llmul/_lldiv)
//------------------------------------------------------------------------------
procedure DraftBlurRow32_Pas(pIn, pOut: PColor32EntryArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PQuadIntArray);
var
  i, c, term0, term12, outVal, valVal: Integer;
  v1, v2, v0: TQuadInt;
begin
  // Initialize forward pass boundary values in Q8 fixed-point (shl 8) from first pixel
  for c := 0 to 3 do
  begin
    v1[c] := pIn[0].Planes[c] shl 8;
    v2[c] := v1[c];
  end;

  // Causal 2nd-order forward pass across row for all 4 channels
  for i := 0 to Width - 1 do
  begin
    for c := 0 to 3 do
    begin
      term0 := (pIn[i].Planes[c] * iK * 256) div QScale;
      term12 := (v1[c] * i2Q - v2[c] * iQ2) div QScale;
      v0[c] := term0 + term12;
      v2[c] := v1[c];
      v1[c] := v0[c];
      RowBuffer[i][c] := v0[c];
    end;
  end;

  // Initialize backward pass boundary values in Q8 fixed-point from last pixel
  for c := 0 to 3 do
  begin
    v1[c] := RowBuffer[Width - 1][c];
    v2[c] := v1[c];
  end;

  // Anti-causal 2nd-order backward pass across row for all 4 channels
  for i := Width - 1 downto 0 do
  begin
    for c := 0 to 3 do
    begin
      term0 := (RowBuffer[i][c] * iK) div QScale;
      term12 := (v1[c] * i2Q - v2[c] * iQ2) div QScale;
      v0[c] := term0 + term12;
      v2[c] := v1[c];
      v1[c] := v0[c];

      valVal := v0[c] + 128;
      if valVal < 0 then
        outVal := 0
      else
      begin
        outVal := valVal div 256;
        if outVal > 255 then
          outVal := 255;
      end;
      pOut[i].Planes[c] := outVal;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Internal 1-channel 8-bit row blur using Alvarez & Mazorra 2nd-order IIR
// in pure 32-bit Integer arithmetic (zero Int64, zero _llmul/_lldiv)
//------------------------------------------------------------------------------
procedure DraftBlurRow8_Pas(pIn, pOut: PByteArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PIntArray);
var
  i, term0, term12, outVal, valVal, v1, v2, v0: Integer;
begin
  // Initialize forward pass boundary value in Q8 fixed-point (shl 8) from first byte
  v1 := pIn[0] shl 8;
  v2 := v1;

  // Causal 2nd-order forward pass across row
  for i := 0 to Width - 1 do
  begin
    term0 := (pIn[i] * iK * 256) div QScale;
    term12 := (v1 * i2Q - v2 * iQ2) div QScale;
    v0 := term0 + term12;
    v2 := v1;
    v1 := v0;
    RowBuffer[i] := v0;
  end;

  // Initialize backward pass boundary value from last byte
  v1 := RowBuffer[Width - 1];
  v2 := v1;

  // Anti-causal 2nd-order backward pass across row
  for i := Width - 1 downto 0 do
  begin
    term0 := (RowBuffer[i] * iK) div QScale;
    term12 := (v1 * i2Q - v2 * iQ2) div QScale;
    v0 := term0 + term12;
    v2 := v1;
    v1 := v0;

    valVal := v0 + 128;
    if valVal < 0 then
      outVal := 0
    else
    begin
      outVal := valVal div 256;
      if outVal > 255 then
        outVal := 255;
    end;
    pOut[i] := outVal;
  end;
end;

//------------------------------------------------------------------------------
// Internal 32-bit ARGB Draft Blur Core
//------------------------------------------------------------------------------
procedure InternalDraftBlur32(Src, Dst: TCustomBitmap32; Sigma: TFloat; TwoDimensional: Boolean);
var
  iK, i2Q, iQ2, QScale: Integer;
  MaxRows, RowIndex: Integer;
  RowBuffer: PQuadIntArray;
  TransposedBuffer: PColor32EntryArray;
  pInRow, pOutRow: PColor32EntryArray;
begin
  if (Sigma < GaussianRadiusToSigma) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Src.CopyMapTo(Dst);
    Exit;
  end;

  Dst.SetSize(Src.Width, Src.Height, False);
  ComputeCoefficientsAlvarezMazorra(Sigma, iK, i2Q, iQ2, QScale);

  MaxRows := Max(Src.Height, Src.Width);
  GetMem(RowBuffer, MaxRows * SizeOf(TQuadInt));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.Width * Src.Height * SizeOf(TColor32Entry));
      try
        // Horizontal pass: Src -> Dst
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PColor32EntryArray(Src.Scanline[RowIndex]);
          pOutRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
          DraftBlurRow32(pInRow, pOutRow, Src.Width, iK, i2Q, iQ2, QScale, RowBuffer);
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose32(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // Vertical pass: TransposedBuffer -> TransposedBuffer
        for RowIndex := 0 to Src.Width - 1 do
        begin
          pInRow := PColor32EntryArray(@TransposedBuffer[RowIndex * Src.Height]);
          DraftBlurRow32(pInRow, pInRow, Src.Height, iK, i2Q, iQ2, QScale, RowBuffer);
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
        pInRow := PColor32EntryArray(Src.Scanline[RowIndex]);
        pOutRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
        DraftBlurRow32(pInRow, pOutRow, Src.Width, iK, i2Q, iQ2, QScale, RowBuffer);
      end;
    end;
  finally
    FreeMem(RowBuffer);
  end;
end;

//------------------------------------------------------------------------------
// Internal 8-bit ByteMap Draft Blur Core
//------------------------------------------------------------------------------
procedure InternalDraftBlur8(Src, Dst: TByteMap; Sigma: TFloat; TwoDimensional: Boolean);
var
  iK, i2Q, iQ2, QScale: Integer;
  MaxRows, RowIndex: Integer;
  RowBuffer: PIntArray;
  TransposedBuffer: PByteArray;
  pInRow, pOutRow: PByteArray;
begin
  if (Sigma < GaussianRadiusToSigma) or (Src.Width <= 1) or (Src.Height <= 1) then
  begin
    if Src <> Dst then
      Dst.Assign(Src);
    Exit;
  end;

  Dst.SetSize(Src.Width, Src.Height);
  ComputeCoefficientsAlvarezMazorra(Sigma, iK, i2Q, iQ2, QScale);

  MaxRows := Max(Src.Height, Src.Width);
  GetMem(RowBuffer, MaxRows * SizeOf(Integer));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.Width * Src.Height);
      try
        // Horizontal pass: Src -> Dst
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PByteArray(Src.Scanline[RowIndex]);
          pOutRow := PByteArray(Dst.Scanline[RowIndex]);
          DraftBlurRow8(pInRow, pOutRow, Src.Width, iK, i2Q, iQ2, QScale, RowBuffer);
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose8(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // Vertical pass: TransposedBuffer -> TransposedBuffer
        for RowIndex := 0 to Src.Width - 1 do
        begin
          pInRow := PByteArray(@TransposedBuffer[RowIndex * Src.Height]);
          DraftBlurRow8(pInRow, pInRow, Src.Height, iK, i2Q, iQ2, QScale, RowBuffer);
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
        pInRow := PByteArray(Src.Scanline[RowIndex]);
        pOutRow := PByteArray(Dst.Scanline[RowIndex]);
        DraftBlurRow8(pInRow, pOutRow, Src.Width, iK, i2Q, iQ2, QScale, RowBuffer);
      end;
    end;
  finally
    FreeMem(RowBuffer);
  end;
end;

//------------------------------------------------------------------------------
// DraftBlur32 Public API
//------------------------------------------------------------------------------
procedure DraftBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalDraftBlur32(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure DraftBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalDraftBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure DraftHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalDraftBlur32(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure DraftHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalDraftBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

//------------------------------------------------------------------------------
// DraftAlphaBlur32 Public API (with alpha pre- and unpremultiplication)
//------------------------------------------------------------------------------
procedure DraftAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
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
  DraftBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure DraftAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  DraftBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

procedure DraftHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
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
  DraftHorizontalBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure DraftHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  DraftHorizontalBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

//------------------------------------------------------------------------------
// DraftBlur8 Public API
//------------------------------------------------------------------------------
procedure DraftBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalDraftBlur8(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure DraftBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalDraftBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure DraftHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalDraftBlur8(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure DraftHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalDraftBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

procedure RegisterBindings;
begin
  BlurRegistry.RegisterBinding(@@DraftBlurRow32, 'DraftBlurRow32');
  BlurRegistry.RegisterBinding(@@DraftBlurRow8, 'DraftBlurRow8');

  BlurRegistry[@@DraftBlurRow32].Add(@DraftBlurRow32_Pas, [isPascal]).Name := 'DraftBlurRow32_Pas';
  BlurRegistry[@@DraftBlurRow8].Add(@DraftBlurRow8_Pas, [isPascal]).Name := 'DraftBlurRow8_Pas';
end;

initialization
  RegisterBindings;
end.
