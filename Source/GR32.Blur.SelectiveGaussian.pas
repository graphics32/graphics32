unit GR32.Blur.SelectiveGaussian;

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
 * The Original Code is Selective Gaussian Blur for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32;


//------------------------------------------------------------------------------
//
//      Selective Gaussian Blur
//
//------------------------------------------------------------------------------
// Can be used as ordinary Gaussian Blur by specifying Delta >= 255
//------------------------------------------------------------------------------
// Radius appears to be specified in 1/5 of a pixel.
// I.e. Radius 1 = 0.2 pixel, Radius 5 = 1 pixel, Radius 10 = 2 pixels, etc.
// This has been verified by comparing the output against that of PhotoShop.
//------------------------------------------------------------------------------
// Note that the current selective blur implementations doesn't fully handle
// alpha insofar as they don't blur on alpha premultipled colors.
// They also do not perform gamma correction.
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Selective Gaussian Blur by Mattias Andersson, 2005
// Based on the source of "Selective gaussian blur filter for the GIMP".
//------------------------------------------------------------------------------
procedure SelectiveGaussianGimp(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);


//------------------------------------------------------------------------------
// Selective Gaussian Blur by Mattias Andersson, 2005
// http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.language.basm&messageid=42c91608$1@newsgroups.borland.com
// https://borland.public.delphi.language.basm.narkive.com/XiSH6pUn/anyone-up-for-a-selective-gaussian-optimization
//------------------------------------------------------------------------------
procedure SelectiveGaussianNew(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianHorzVert(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer); deprecated 'Destroys source bitmap';


//------------------------------------------------------------------------------
// Selective Gaussian Blur by Mattias Andersson, 2005
// MMX optimized versions
//------------------------------------------------------------------------------
{$ifndef OMIT_MMX}
procedure SelectiveGaussianMMX1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer); deprecated 'SelectiveGaussianMMX1 does not appear to work'; // TODO : Why?
procedure SelectiveGaussianMMX2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
{$endif}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32.Blur,
  GR32_Bindings,
  GR32_LowLevel,
  GR32_Blend, // EMMS
  GR32_System,
  GR32_OrdinalMaps;

//------------------------------------------------------------------------------
//
// Selective Gaussian Blur by Mattias Andersson
//
//------------------------------------------------------------------------------
type
  TSingleDynArray = array of Single;

function GaussianKernel(Radius: Single): TSingleDynArray;
var
  I, R: Integer;
  Sd, C1, C2: Single;
begin
  R := Ceil(Radius);
  SetLength(Result, R + 1);
  Sd := Radius * GaussianRadiusToSigma;
  C1 := 1.0 / Sqrt(2.0 * Pi * Sd);
  C2 := -0.5 / Sqr(Sd);
  for I := 0 to R do
    Result[I] := C1 * Exp(Sqr(I) * C2);
end;


//------------------------------------------------------------------------------
//
// The original implementation of selective gaussian blur (similar to the
// one in GIMP).
//
//------------------------------------------------------------------------------
procedure SelectiveGaussianGimp(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  I, J, Plane, X, Y, V, R, SrcR, SrcB: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, W: Single;
  RefColor: TColor32;
  RefValue: Integer;
  PDstLine, PSrcLine: PColor32Array;
begin
  ASSERT(Src <> Dst);

  R := Ceil(Radius);
  SrcR := Src.Width - 1;
  SrcB := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  Kernel := GaussianKernel(Radius);
  try
    for J := 0 to SrcB do
    begin
      PDstLine := Dst.ScanLine[J];
      for I := 0 to SrcR do
      begin
        RefColor := Src[I, J];
        for Plane := 0 to 3 do
        begin
          Sum := 0;
          Fact := 0;
          // TODO : Premultiply and gamma (sRGB->LinearRGB)
          RefValue := TColor32Entry(RefColor).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;
          for Y := -R to R do
          begin
            if Y + J < 0 then
              Continue;
            if Y + J > SrcB then
              Break;

            PSrcLine := Src.ScanLine[Y + J];
            for X := -R to R do
            begin
              if X + I < 0 then
                Continue;
              if X + I > SrcR then
                Break;

              V := TColor32Entry(PSrcLine[X + I]).Planes[Plane];
              if (V >= MinValue) and (V <= MaxValue) then
              begin
                W := Kernel[Abs(X)] * Kernel[Abs(Y)];
                Sum := Sum + V * W;
                Fact := Fact + W;
              end;
            end;
          end;
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          TColor32Entry(PDstLine[I]).Planes[Plane] := Round(Sum / Fact);
        end;
      end;
    end;
  finally
    Kernel := nil;
  end;
end;


//------------------------------------------------------------------------------
//
// Optimized algorithm that performs horizontal and vertical blurring only
// once for each reference color at a certain position (x, y). A table is
// used for cacheing convolution sum of the reference color values that have
// already been visited. Vertical blurring can then be  performed in a
// single pass by looking up already cached entries for a given reference
// color (and we can thus take advantage of the fact the gaussian is
// separable). In order to minimize required memory, the horizontal pass is
// performed on one column at a time. This requires Src.Height * 2^BitDepth
// bytes of memory, so it's a problem if we want to support 16-bit images.
// Another problem with images with higher bit-depth is that there is a
// smaller probability that adjacent colors have the same pixel values.
//
//------------------------------------------------------------------------------
procedure SelectiveGaussianNew(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
type
  PCacheEntry = ^TCacheEntry;
  TCacheEntry = record
    Sum: Single;  // intermediate sum of convolution
    Fact: Single; // intermediate sum of kernel weights
  end;
var
  I, J, Plane, X, Y, LoY, V, R, SrcR, SrcB: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, W: Single;
  RefValue: Integer;
  PSrcLine: PColor32Array;
  PEntry: PCacheEntry;

  SumCache: array of array [Byte] of TCacheEntry;
  LastPos: array [Byte] of Integer;
begin
  ASSERT(Src <> Dst);

  R := Ceil(Radius);
  SrcR := Src.Width - 1;
  SrcB := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);
  Kernel := GaussianKernel(Radius);
  try
    for I := 0 to SrcR do
    begin
      for Plane := 0 to 3 do
      begin
        FillLongword(LastPos[0], 256, Cardinal(Low(Integer)));
        for J := 0 to SrcB do
        begin
          // TODO : Premultiply and gamma (sRGB->LinearRGB)
          RefValue := TColor32Entry(Src[I, J]).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;

          if LastPos[RefValue] < J - R then
            LoY := J - R
          else
            LoY := LastPos[RefValue] + 1;

          for Y := LoY to J + R do
          begin
            if Y < 0 then
              Continue;
            if Y > SrcB then
              Break;

            Sum := 0;
            Fact := 0;
            PSrcLine := Src.Scanline[Y];
            for X := -R to R do
            begin
              if X + I < 0 then
                Continue;
              if X + I > SrcR then
                Break;

              V := TColor32Entry(PSrcLine[X + I]).Planes[Plane];
              if (V >= MinValue) and (V <= MaxValue) then
              begin
                W := Kernel[Abs(X)];
                Sum := Sum + V * W;
                Fact := Fact + W;
              end;
            end;
            PEntry := @SumCache[Y][RefValue];
            PEntry.Sum := Sum;
            PEntry.Fact := Fact;
          end;
          LastPos[RefValue] := J + R;
        end;

        for J := 0 to SrcB do
        begin
          RefValue := TColor32Entry(Src[I, J]).Planes[Plane];
          Sum := 0;
          Fact := 0;
          for Y := -R to R do
          begin
            if Y + J < 0 then Continue;
            if Y + J > SrcB then Break;
            W := Kernel[Abs(Y)];
            PEntry := @SumCache[Y + J][RefValue];
            Sum := Sum + PEntry.Sum * W;
            Fact := Fact + PEntry.Fact * W;
          end;
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          PColor32Entry(Dst.PixelPtr[I, J]).Planes[Plane] := Round(Sum / Fact);
        end;
      end;
    end;
  finally
    SumCache := nil;
    Kernel := nil;
  end;
end;


//------------------------------------------------------------------------------
//
// This algorithm performs selective blurring first horizontally (storing
// the intermediate result in a bitmap), and then vertically. The output
// is slightly different from ordinary gaussian selective blurring, but
// the speed-up is significant.
//
//------------------------------------------------------------------------------
procedure SelectiveGaussianHorzVert(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  I, J, Plane, {LoU,} X, Y, V, R, SrcR, SrcB: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, W: Single;
  RefColor: TColor32;
  RefValue: Integer;
  PDstLine, PSrcLine: PColor32Array;
begin
  ASSERT(Src <> Dst);

  R := Ceil(Radius);
  SrcR := Src.Width - 1;
  SrcB := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  Kernel := GaussianKernel(Radius);
  try
    for J := 0 to SrcB do
    begin
      PDstLine := Dst.ScanLine[J];
      PSrcLine := Src.ScanLine[J];
      for I := 0 to SrcR do
      begin
        RefColor := Src[I, J];
        for Plane := 0 to 3 do
        begin
          Sum := 0;
          Fact := 0;
          // TODO : Premultiply and gamma (sRGB->LinearRGB)
          RefValue := TColor32Entry(RefColor).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;
          for X := -R to R do
          begin
            if X + I < 0 then
              Continue;
            if X + I > SrcR then
              Break;

            V := TColor32Entry(PSrcLine[X + I]).Planes[Plane];
            if (V >= MinValue) and (V <= MaxValue) then
            begin
              W := Kernel[Abs(X)];
              Sum := Sum + V * W;
              Fact := Fact + W;
            end;
          end;
          TColor32Entry(PDstLine[I]).Planes[Plane] := Round(Sum / Fact);
        end;
      end;
    end;

    for J := 0 to SrcB do
    begin
      // TODO : This is using the source bitmap as a temporary buffer
      // thus destroying the source bitmap!
      PDstLine := Src.ScanLine[J];
      for I := 0 to SrcR do
      begin
        RefColor := Dst[I, J];
        for Plane := 0 to 3 do
        begin
          Sum := 0;
          Fact := 0;
          RefValue := TColor32Entry(RefColor).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;
          for Y := -R to R do
          begin
            if Y + J < 0 then
              Continue;
            if Y + J > SrcB then
              Break;

            V := TColor32Entry(Dst[I, Y + J]).Planes[Plane];
            if (V >= MinValue) and (V <= MaxValue) then
            begin
              W := Kernel[Abs(Y)];
              Sum := Sum + V * W;
              Fact := Fact + W;
            end;
          end;
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          TColor32Entry(PDstLine[I]).Planes[Plane] := Round(Sum / Fact);
        end;
      end;
    end;
    Dst.Assign(Src);
  finally
    Kernel := nil;
  end;
end;



{$ifndef OMIT_MMX}
//------------------------------------------------------------------------------
//
// MMX optimized Selective Gaussian Blur from http://developer.centaurix.com/pub/
// by Mattias Andersson
//
//------------------------------------------------------------------------------
// Modified to also blur alpha channel.
// TODO : This is probably not a good idea since we're not operating on
// premultiplied alpha.
//------------------------------------------------------------------------------
type
  PCacheEntry = ^TCacheEntry;
  TCacheEntry = record
    Sum: Integer;  // intermediate sum of convolution
    Fact: Integer; // intermediate sum of kernel weights
  end;

  PRangeEntry = ^TRangeEntry;
  TRangeEntry = packed record
    Min: Byte;
    Max: Byte;
    Sum: Integer;
  end;

function GaussianKernelInt(Radius: Single): TArrayOfWord;
var
  I, R: Integer;
  C: Single;
begin
  R := Ceil(Radius);
  SetLength(Result, R * 2 + 1 + 4);
  C := -0.5 / Sqr(Radius * GaussianRadiusToSigma);
  for I := -R to R do
    Result[I + R] := Round(255 * Exp(Sqr(I) * C));
end;

procedure AccumulateMMX(PSrc: Pointer; PFact: Pointer; Count, Min, Max: Integer; out Sum, FactSum: Integer);
const
  Mask: UInt64 = $FF00FF00FF00FF00;
asm
  // MM0 <- Min
  // MM1 <- Max
  // MM2 <- PSrc
  // MM3 <- PFact
  // MM5 <- Sum
  // MM6 <- FactSum

  // initialize
        MOVD        MM0,Min
        PUNPCKLWD   MM0,MM0
        PUNPCKLDQ   MM0,MM0
        MOVD        MM1,Max
        PUNPCKLWD   MM1,MM1
        PUNPCKLDQ   MM1,MM1
        PXOR        MM5,MM5
        PXOR        MM6,MM6
        PXOR        MM7,MM7

  // loop start
@1:
        MOVD        MM2,[EAX+ECX*4]
        PUNPCKLBW   MM2,MM7
        MOVQ        MM3,[EDX+ECX*8]

  // store threshold mask in MM4
        MOVQ        MM4,MM2
        PCMPGTW     MM4,MM0
        PAND        MM4,MM1
        PCMPGTW     MM4,MM2

  // mask colors and weights
        PAND        MM2,MM4
        PAND        MM3,MM4

  // multiply colors and weights
        PMULLW      MM2,MM3
        PAND        MM2,Mask

  // perform accumulation
        PSADBW      MM2,MM7
        PADDD       MM5,MM2

        PSADBW      MM3,MM7
        PADDD       MM6,MM3

  // loop end
        DEC         ECX
        JGE         @1

        MOV         EAX,Sum
        MOVD        [EAX],MM5
        MOV         EAX,FactSum
        MOVD        [EAX],MM6
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussianMMX1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
const
  Planes: array[0..3] of TConversionType = (ctBlue, ctGreen, ctRed, ctAlpha);
var
  I, J, Plane, LoU, X, Y, LoY, V, VMin, VMax, R, SrcR, SrcB: Integer;
  XCount, ColCount: Integer;
  MinValue, MaxValue, MinX: Integer;
  Kernel: TArrayOfWord;
  PKernel: PWordArray;
  VSum, VFact, Sum: Integer;
  W, Fact, CFact: Integer;
  RefValue: Integer;
  PSrcLine: PByteArray;
  CacheEntry: PCacheEntry;

  SumCache: array of array [Byte] of TCacheEntry;
  LastPos: array [Byte] of Integer;

  Map: array[Low(Planes)..High(Planes)] of TByteMap;
  PMap: PByteArray;
begin
  ASSERT(Src <> Dst);

  R := Ceil(Radius);
  SrcR := Src.Width - 1;
  SrcB := Src.Height - 1;
  ColCount := Src.Width;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);

  Kernel := GaussianKernelInt(Radius);
  PKernel := PWordArray(@Kernel[R]); // anme

  for Plane := Low(Planes) to High(Planes) do
    Map[Plane] := TByteMap.Create;
  try
    for Plane := Low(Planes) to High(Planes) do
      // TODO : Premultiply and gamma (sRGB->LinearRGB)
      Map[Plane].ReadFrom(Src, Planes[Plane]);

    CFact := 0;
    for X := -R to R do
      CFact := CFact + PKernel[X];

    for I := 0 to SrcR do
    begin
      MinX := Max(-R, -I);
      XCount := (Min(R, SrcR - I) - MinX) shr 2;

      for Plane := 0 to 3 do
      begin
        PMap := PByteArray(Map[Plane].ValPtr[I + MinX, 0]);

        FillLongword(LastPos[0], 256, Cardinal(Low(Integer)));
        for J := 0 to SrcB do
        begin
          RefValue := PMap[J * ColCount - MinX];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;

          if LastPos[RefValue] < J - R then
          begin
            if J < R then
              LoY := 0 else LoY := J - R
          end
          else
            LoY := LastPos[RefValue] + 1;

          VSum := 0;
          VFact := 0;
          for Y := J - R to LoY - 1 do
          begin
            if Y < 0 then
              Continue;
            W := PKernel[Y - J];
            CacheEntry := @SumCache[Y][RefValue];
            VSum := VSum + CacheEntry.Sum * W;
            VFact := VFact + CacheEntry.Fact * W;
          end;

          for Y := LoY to J + R do
          begin
            if Y > SrcB then
              Break;
            AccumulateMMX(@PMap[Y * ColCount], @PKernel[MinX], XCount, MinValue, MaxValue, Sum, Fact);

            CacheEntry := @SumCache[Y][RefValue];
            CacheEntry.Sum := Sum;
            CacheEntry.Fact := Fact;

            W := PKernel[Y - J];
            VSum := VSum + Sum * W;
            VFact := VFact + Fact * W;
          end;
          LastPos[RefValue] := Min(J + R, SrcB);
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          PColor32Entry(Dst.PixelPtr[I, J]).Planes[Plane] := VSum div (VFact shr 8);
        end;
      end;
    end;
  finally
    EMMS;
    for Plane := Low(Planes) to High(Planes) do
      Map[Plane].Free;
    SumCache := nil;
    Kernel := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussianMMX2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  I, J, Plane, LoU, X, Y, LoY, V, VMin, VMax, R, SrcR, SrcB: Integer;
  MinValue, MaxValue, MinX, MaxX, ColCount: Integer;
  Kernel: TArrayOfWord;
  PKernel: PWordArray;
  VSum, VFact, Sum: Integer;
  W, Fact, CFact: Integer;
  RefValue: Integer;
  PSrcLine: PByteArray;
  CacheEntry: PCacheEntry;
  RangeEntry: PRangeEntry;

  SumCache: array of array [Byte] of TCacheEntry;
  RangeCache: array of TRangeEntry;
  LastPos: array [Byte] of Integer;

  Map: array[0..3] of TByteMap;
  PMap: PByteArray;
const
  ConvType: array[0..3] of TConversionType = (ctBlue, ctGreen, ctRed, ctAlpha);
begin
  ASSERT(Src <> Dst);

  R := Ceil(Radius);
  SrcR := Src.Width - 1;
  SrcB := Src.Height - 1;
  ColCount := Src.Width;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);
  SetLength(RangeCache, Src.Height);

  Kernel := GaussianKernelInt(Radius);
  PKernel := PWordArray(@Kernel[R]);

  for I := 0 to 3 do
    Map[I] := TByteMap.Create;
  try
    for I := 0 to 3 do
      // TODO : Premultiply and gamma (sRGB->LinearRGB)
      Map[I].ReadFrom(Src, ConvType[I]);

    CFact := 0;
    for X := -R to R do
      CFact := CFact + PKernel[X];

    for I := 0 to SrcR do
    begin
      MinX := Max(-R, -I);
      MaxX := Min(R, SrcR - I);

      for Plane := 0 to 3 do
      begin
        PMap := PByteArray(Map[Plane].ValPtr[I, 0]);

        // compute range entries

        for J := 0 to SrcB do
        begin
          PSrcLine := PByteArray(Map[Plane].ValPtr[I, J]); //@Src.Scanline[J][I];
          Sum := 0;
          VMin := 255;
          VMax := 0;
          for X := MinX to MaxX do
          begin
            V := PSrcLine[X]; //TColor32Entry(PSrcLine[X]).Planes[Plane];
            Sum := Sum + PKernel[X] * V;
            if V < VMin then VMin := V;
            if V > VMax then VMax := V;
          end;
          RangeEntry := @RangeCache[J];
          RangeEntry.Min := VMin;
          RangeEntry.Max := VMax;
          RangeEntry.Sum := Sum shr 8;
        end;

        FillLongword(LastPos[0], 256, Cardinal(Low(Integer)));
        for J := 0 to SrcB do
        begin
          RefValue := PMap[J * ColCount];

          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;

          if LastPos[RefValue] < J - R then
          begin
            if J < R then
              LoY := 0 else LoY := J - R
          end
          else
            LoY := LastPos[RefValue] + 1;

          VSum := 0;
          VFact := 0;
          for Y := J - R to LoY - 1 do
          begin
            if Y < 0 then
              Continue;
            W := PKernel[Y - J];
            CacheEntry := @SumCache[Y][RefValue];
            VSum := VSum + CacheEntry.Sum * W;
            VFact := VFact + CacheEntry.Fact * W;
          end;

          for Y := LoY to J + R do
          begin
            if Y > SrcB then
              Break;
            RangeEntry := @RangeCache[Y];

            if not ((RangeEntry.Min >= MinValue) and (RangeEntry.Max <= MaxValue)) then
            begin
              AccumulateMMX(@PMap[Y * ColCount + MinX], @PKernel[MinX], (MaxX - MinX) div 4, MinValue, MaxValue, Sum, Fact);
            end
            else
            begin
              Sum := RangeEntry.Sum;
              Fact := CFact;
              if I - R < 0 then
                for X := I to R - 1 do
                  Fact := Fact - PKernel[X + 1]
              else
              if I + R > SrcR then
                for X := SrcR - I to R - 1 do
                  Fact := Fact - PKernel[X + 1];
            end;

            CacheEntry := @SumCache[Y][RefValue];
            CacheEntry.Sum := Sum;
            CacheEntry.Fact := Fact;

            W := PKernel[Y - J];
            VSum := VSum + Sum * W;
            VFact := VFact + Fact * W;
          end;
          LastPos[RefValue] := Min(J + R, SrcB);
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          PColor32Entry(Dst.PixelPtr[I, J]).Planes[Plane] := VSum div (VFact shr 8);
        end;
      end;
    end;
  finally
    EMMS;
    for I := 0 to 3 do
      Map[I].Free;
    SumCache := nil;
    RangeCache := nil;
    Kernel := nil;
  end;
end;
{$endif}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  // Order by performance:
  // 1. SelectiveGaussianMMX1 (broken)
  // 2. SelectiveGaussianMMX2
  // 3. SelectiveGaussianHorzVert (destructive)
  // 4. SelectiveGaussianNew
  // 5. SelectiveGaussianGimp

  BlurRegistry.Add(@@SelectiveGaussianBlur32, @SelectiveGaussianGimp,           [isPascal], 1024);
  BlurRegistry.Add(@@SelectiveGaussianBlur32, @SelectiveGaussianNew,            [isPascal], 768);
//  BlurRegistry.Add(@@SelectiveGaussianBlur32, @SelectiveGaussianHorzVert,       [isPascal], 512);

{$ifndef PUREPASCAL}
{$ifndef OMIT_MMX}
  BlurRegistry.Add(@@SelectiveGaussianBlur32, @SelectiveGaussianMMX2,           [isMMX], 0);
//  BlurRegistry.Add(@@SelectiveGaussianBlur32, @SelectiveGaussianMMX1,           [isMMX], -256);
{$endif}
{$endif}
end;

initialization
  RegisterBindings;
end.
