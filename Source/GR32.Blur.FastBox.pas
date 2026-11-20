unit GR32.Blur.FastBox experimental;

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
 * The Original Code is Fast Box Blur for Graphics32
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
//      Fast Box Blur (W3C SVG 1.1 3-Pass Sliding Accumulator)
//
//------------------------------------------------------------------------------
// Implement W3C SVG 1.1 compliant 3-pass sliding accumulator box blurs. Time
// complexity is O(1) per pixel independent of radius.
//------------------------------------------------------------------------------

procedure FastBoxBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure FastBoxBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure FastBoxHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure FastBoxHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure FastBoxAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure FastBoxAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure FastBoxHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat); overload;
procedure FastBoxHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat); overload;

procedure FastBoxBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure FastBoxBlur8(Bitmap: TByteMap; Radius: TFloat); overload;

procedure FastBoxHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat); overload;
procedure FastBoxHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat); overload;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,
  GR32_Blend,
  GR32.Blur,
  GR32.Transpose;

type
  TQuadInt = array[0..3] of Integer;
  PQuadIntArray = ^TQuadIntArray;
  TQuadIntArray = array[0..0] of TQuadInt;
  PIntArray = ^TIntArray;
  TIntArray = array[0..0] of Integer;

//------------------------------------------------------------------------------
// Calculate W3C SVG 1.1 box blur radii (R1, R2, R3) for a given Sigma
// Formula: d = floor(sigma * 3 * sqrt(2 * pi) / 4 + 0.5)
//------------------------------------------------------------------------------
procedure ComputeW3CBoxRadii(Sigma: TFloat; out R1, R2, R3: Integer);
var
  d, r: Integer;
begin
  if Sigma < 0.1 then
  begin
    R1 := 0; R2 := 0; R3 := 0;
    Exit;
  end;

  d := Floor(Sigma * 1.879971205974764 + 0.5);
  if d < 1 then
    d := 1;

  r := (d - 1) div 2;
  if (d and 1) = 0 then
  begin
    R1 := r;
    R2 := r;
    R3 := r + 1;
  end else
  begin
    R1 := r;
    R2 := r;
    R3 := r;
  end;
end;

//------------------------------------------------------------------------------
// 1D Sliding Accumulator Box Blur for 32-bit ARGB pixels
//------------------------------------------------------------------------------
procedure BoxBlur1D32(pIn, pOut: PColor32EntryArray; Width, Radius: Integer; Buffer: PQuadIntArray);
var
  i, c, invDiv, windowSize, leftIdx, rightIdx: Integer;
  sum: TQuadInt;
begin
  if (Radius <= 0) or (Width <= 1) then
  begin
    if pIn <> pOut then
      Move(pIn^, pOut^, Width * SizeOf(TColor32Entry));
    Exit;
  end;

  windowSize := 2 * Radius + 1;
  invDiv := (65536 + windowSize div 2) div windowSize;

  for c := 0 to 3 do
  begin
    sum[c] := pIn[0].Planes[c] * (Radius + 1);
    for i := 1 to Radius do
    begin
      if i < Width then
        Inc(sum[c], pIn[i].Planes[c])
      else
        Inc(sum[c], pIn[Width - 1].Planes[c]);
    end;
  end;

  for i := 0 to Width - 1 do
  begin
    for c := 0 to 3 do
    begin
      Buffer[i][c] := (sum[c] * invDiv + 32768) shr 16;

      rightIdx := i + Radius + 1;
      if rightIdx >= Width then
        rightIdx := Width - 1;

      leftIdx := i - Radius;
      if leftIdx < 0 then
        leftIdx := 0;

      Inc(sum[c], pIn[rightIdx].Planes[c] - pIn[leftIdx].Planes[c]);
    end;
  end;

  for i := 0 to Width - 1 do
  begin
    for c := 0 to 3 do
    begin
      if Buffer[i][c] < 0 then pOut[i].Planes[c] := 0
      else if Buffer[i][c] > 255 then pOut[i].Planes[c] := 255
      else pOut[i].Planes[c] := Buffer[i][c];
    end;
  end;
end;

//------------------------------------------------------------------------------
// 1D Sliding Accumulator Box Blur for 8-bit bytes
//------------------------------------------------------------------------------
procedure BoxBlur1D8(pIn, pOut: PByteArray; Width, Radius: Integer; Buffer: PIntArray);
var
  i, sum, invDiv, windowSize, leftIdx, rightIdx: Integer;
begin
  if (Radius <= 0) or (Width <= 1) then
  begin
    if pIn <> pOut then
      Move(pIn^, pOut^, Width);
    Exit;
  end;

  windowSize := 2 * Radius + 1;
  invDiv := (65536 + windowSize div 2) div windowSize;

  sum := pIn[0] * (Radius + 1);
  for i := 1 to Radius do
  begin
    if i < Width then
      Inc(sum, pIn[i])
    else
      Inc(sum, pIn[Width - 1]);
  end;

  for i := 0 to Width - 1 do
  begin
    Buffer[i] := (sum * invDiv + 32768) shr 16;

    rightIdx := i + Radius + 1;
    if rightIdx >= Width then
      rightIdx := Width - 1;

    leftIdx := i - Radius;
    if leftIdx < 0 then
      leftIdx := 0;

    Inc(sum, pIn[rightIdx] - pIn[leftIdx]);
  end;

  for i := 0 to Width - 1 do
  begin
    if Buffer[i] < 0 then pOut[i] := 0
    else if Buffer[i] > 255 then pOut[i] := 255
    else pOut[i] := Buffer[i];
  end;
end;

//------------------------------------------------------------------------------
// Internal 32-bit ARGB Box Blur Core
//------------------------------------------------------------------------------
procedure InternalFastBoxBlur32(Src, Dst: TCustomBitmap32; Sigma: TFloat; TwoDimensional: Boolean);
var
  R1, R2, R3, Pass, MaxRows, RowIndex: Integer;
  Radii: array[1..3] of Integer;
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

  if Src <> Dst then
    Src.CopyMapTo(Dst);

  ComputeW3CBoxRadii(Sigma, R1, R2, R3);
  Radii[1] := R1; Radii[2] := R2; Radii[3] := R3;

  MaxRows := Max(Src.Height, Src.Width);
  GetMem(RowBuffer, MaxRows * SizeOf(TQuadInt));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.ByteCount);
      try
        // 3-Pass Horizontal Blur
        for Pass := 1 to 3 do
        begin
          if Radii[Pass] <= 0 then Continue;
          for RowIndex := 0 to Src.Height - 1 do
          begin
            pInRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
            pOutRow := pInRow;
            BoxBlur1D32(pInRow, pOutRow, Src.Width, Radii[Pass], RowBuffer);
          end;
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose32(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // 3-Pass Vertical Blur on Transposed Buffer
        for Pass := 1 to 3 do
        begin
          if Radii[Pass] <= 0 then Continue;
          for RowIndex := 0 to Src.Width - 1 do
          begin
            pInRow := PColor32EntryArray(@TransposedBuffer[RowIndex * Src.Height]);
            BoxBlur1D32(pInRow, pInRow, Src.Height, Radii[Pass], RowBuffer);
          end;
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
      for Pass := 1 to 3 do
      begin
        if Radii[Pass] <= 0 then Continue;
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PColor32EntryArray(Dst.Scanline[RowIndex]);
          pOutRow := pInRow;
          BoxBlur1D32(pInRow, pOutRow, Src.Width, Radii[Pass], RowBuffer);
        end;
      end;
    end;
  finally
    FreeMem(RowBuffer);
  end;
end;

//------------------------------------------------------------------------------
// Internal 8-bit ByteMap Box Blur Core
//------------------------------------------------------------------------------
procedure InternalFastBoxBlur8(Src, Dst: TByteMap; Sigma: TFloat; TwoDimensional: Boolean);
var
  R1, R2, R3, Pass, MaxRows, RowIndex: Integer;
  Radii: array[1..3] of Integer;
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

  if Src <> Dst then
    Dst.Assign(Src);

  ComputeW3CBoxRadii(Sigma, R1, R2, R3);
  Radii[1] := R1; Radii[2] := R2; Radii[3] := R3;

  MaxRows := Max(Src.Height, Src.Width);
  GetMem(RowBuffer, MaxRows * SizeOf(Integer));
  try
    if TwoDimensional then
    begin
      GetMem(TransposedBuffer, Src.ByteCount);
      try
        // 3-Pass Horizontal Blur
        for Pass := 1 to 3 do
        begin
          if Radii[Pass] <= 0 then Continue;
          for RowIndex := 0 to Src.Height - 1 do
          begin
            pInRow := PByteArray(Dst.Scanline[RowIndex]);
            pOutRow := pInRow;
            BoxBlur1D8(pInRow, pOutRow, Src.Width, Radii[Pass], RowBuffer);
          end;
        end;

        // Transpose: Dst (W x H) -> TransposedBuffer (H x W)
        Transpose8(Dst.Bits, TransposedBuffer, Src.Width, Src.Height);

        // 3-Pass Vertical Blur on Transposed Buffer
        for Pass := 1 to 3 do
        begin
          if Radii[Pass] <= 0 then Continue;
          for RowIndex := 0 to Src.Width - 1 do
          begin
            pInRow := PByteArray(@TransposedBuffer[RowIndex * Src.Height]);
            BoxBlur1D8(pInRow, pInRow, Src.Height, Radii[Pass], RowBuffer);
          end;
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
      for Pass := 1 to 3 do
      begin
        if Radii[Pass] <= 0 then Continue;
        for RowIndex := 0 to Src.Height - 1 do
        begin
          pInRow := PByteArray(Dst.Scanline[RowIndex]);
          pOutRow := pInRow;
          BoxBlur1D8(pInRow, pOutRow, Src.Width, Radii[Pass], RowBuffer);
        end;
      end;
    end;
  finally
    FreeMem(RowBuffer);
  end;
end;

//------------------------------------------------------------------------------
// FastBoxBlur32 Public API
//------------------------------------------------------------------------------
procedure FastBoxBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalFastBoxBlur32(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure FastBoxBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalFastBoxBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure FastBoxHorizontalBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
begin
  InternalFastBoxBlur32(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure FastBoxHorizontalBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  InternalFastBoxBlur32(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

//------------------------------------------------------------------------------
// FastBoxAlphaBlur32 Public API (with alpha pre- and unpremultiplication)
//------------------------------------------------------------------------------
procedure FastBoxAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
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
  FastBoxBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure FastBoxAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  FastBoxBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

procedure FastBoxHorizontalAlphaBlur32(Src, Dst: TCustomBitmap32; Radius: TFloat);
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
  FastBoxHorizontalBlur32(Dst, Dst, Radius);
  Unpremultiply32(Dst);
end;

procedure FastBoxHorizontalAlphaBlur32(Bitmap: TCustomBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) or (Bitmap.Width <= 1) or (Bitmap.Height <= 1) then
    Exit;

  Premultiply32(Bitmap);
  FastBoxHorizontalBlur32(Bitmap, Bitmap, Radius);
  Unpremultiply32(Bitmap);
end;

//------------------------------------------------------------------------------
// FastBoxBlur8 Public API
//------------------------------------------------------------------------------
procedure FastBoxBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalFastBoxBlur8(Src, Dst, Radius * GaussianRadiusToSigma, True);
end;

procedure FastBoxBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalFastBoxBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, True);
end;

procedure FastBoxHorizontalBlur8(Src, Dst: TByteMap; Radius: TFloat);
begin
  InternalFastBoxBlur8(Src, Dst, Radius * GaussianRadiusToSigma, False);
end;

procedure FastBoxHorizontalBlur8(Bitmap: TByteMap; Radius: TFloat);
begin
  InternalFastBoxBlur8(Bitmap, Bitmap, Radius * GaussianRadiusToSigma, False);
end;

end.
