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
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
{$if not defined(FPC)}
  System.SysUtils, // Must be before GR32 so we get the correct PByteArray
{$else}
  SysUtils,
{$ifend}
  GR32;


//------------------------------------------------------------------------------
//
//      Selective Gaussian Blur
//
//------------------------------------------------------------------------------
// Definition of Selective Gaussian Blur from the GIMP User Manual:
//
// The Selective Gaussian Blur filter performs a mathematical region-based
// selection of the image in small chunks, and determines the level of detail
// within that chunk. After this it applies a Gaussian-based blur to it.
// Selective Gaussian Blur can be very processor intensive, but produces very
// controlled blurring.
//
// The Blur Radius setting affects the maximum number of pixels considered for
// blurring. The higher the setting, the higher the number of pixels that will
// be included in the region analysis. Be aware that a higher setting will take
// considerably longer to compute.
//
// The Delta affects the level of detail that will be blurred. A higher setting
// here will produce more smoothing of the pixels in the radius.
//
// A common use for the Selective Gaussian Blur filter is smoothing areas
// affected by populations of JPEG artifacts, or bad pixelization distortions.
//------------------------------------------------------------------------------
// Can, in theory, be used as (a bad) ordinary Gaussian Blur by specifying
// Delta >= 255.
//
// Note that the selective blur, by design, does not blur the alpha channel.
//------------------------------------------------------------------------------
type
  TSelectiveGaussian32Proc = procedure(ASource, ADest: TBitmap32; Radius: TFloat; Delta: Integer);

var
  SelectiveGaussianBlur32: TSelectiveGaussian32Proc;
  GammaSelectiveGaussianBlur32: TSelectiveGaussian32Proc;


//------------------------------------------------------------------------------
// Selective Gaussian Blur
// Mattias Andersson, 2005
//------------------------------------------------------------------------------
// SIMD optimized versions
//------------------------------------------------------------------------------
// The performance of SelectiveGaussian1 is generally better than
// SelectiveGaussian2 (25% faster on some images) but it also has a slightly
// higher signal loss. For example:
// - SelectiveGaussian1 on a solid color (value=255), yields value=253.
// - SelectiveGaussian2 on a solid color (value=255), yields value=254.
// This is generally not a problem since such a small difference isn't visible.
//------------------------------------------------------------------------------
procedure SelectiveGaussian1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianGamma1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussian2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianGamma2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);

type
  TSelectiveGaussianAccumulateProc = procedure(PSrc: PByteArray; PFact: PWordArray; Count, Min, Max: Integer; out Sum, FactSum: Cardinal);

var
  SelectiveGaussianAccumulate: TSelectiveGaussianAccumulateProc;


//------------------------------------------------------------------------------
// Selective Gaussian Blur
// Eric Grange, 2005
//------------------------------------------------------------------------------
// https://borland.public.delphi.language.basm.narkive.com/XiSH6pUn/anyone-up-for-a-selective-gaussian-optimization
// https://web.archive.org/web/20240914225741/https://borland.public.delphi.language.basm.narkive.com/XiSH6pUn/anyone-up-for-a-selective-gaussian-optimization
//------------------------------------------------------------------------------
// Unoptimized reference implemention.
//------------------------------------------------------------------------------
procedure SelectiveGaussianGimp(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianGimpGamma(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);


//------------------------------------------------------------------------------
// Various Selective Gaussian Blur variations
// Mattias Andersson, 2005
//------------------------------------------------------------------------------
// http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.language.basm&messageid=42c91608$1@newsgroups.borland.com (link dead)
// https://groups.google.com/g/borland.public.delphi.language.basm/c/QXxiJZnIOa8/m/YMID8XaqzdsJ
// https://web.archive.org/web/20240914232817/https://groups.google.com/g/borland.public.delphi.language.basm/c/QXxiJZnIOa8/m/YMID8XaqzdsJ
//------------------------------------------------------------------------------
// Unoptimized reference implementions.
//------------------------------------------------------------------------------
procedure SelectiveGaussianNew(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianNewGamma(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
procedure SelectiveGaussianHorzVert(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer); deprecated 'Destroys source pixels';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$if not defined(FPC)}
  System.Math,
  System.SyncObjs, // TCriticalSection
{$else}
  Math,
  SyncObjs, // TCriticalSection
{$ifend}
  GR32_Gamma,
  GR32.Blur,
  GR32_Bindings,
  GR32_LowLevel,
  GR32_System,
  GR32_OrdinalMaps,
  GR32.Types.SIMD;

// Ensure that we use the GR32.TFloat and not FPC's Math.TFloat (which is an alias for Double!)
type
  TFloat = GR32.TFloat;
  PFloat = ^TFloat;

//------------------------------------------------------------------------------
//
//      PremultiplyLUT
//
//------------------------------------------------------------------------------
// Lookup tables for alpha premultiplication.
//
//   MulDiv255[a,b] = a * b / 255           Used for premultiplication
//   Mul255Div[a,b] = Round(a * 255 / b)    Used for unpremultiplication
//
// where
//
//   a: Color value
//   b: Alpha value
//
// PremultiplyLUT is used for pre- and unpremultiplication.
// GammaPremultiplyLUT rolls the gamma correction and pre-/unpremultiplication
// operations into one for a significant gain in precision at no extra cost in
// performance.
//
//------------------------------------------------------------------------------
type
  PPremultiplyLUT = ^TPremultiplyLUT;

  TPremultiplyLUT = record
  strict private
    class constructor Create;
    class destructor Destroy;
    const OneOver255 = 1 / 255;
  strict private class var
    FLock: TCriticalSection;
    FPremultiplyLUT: PPremultiplyLUT;
    FGammaPremultiplyLUT: PPremultiplyLUT;
  strict private
    FsRGB: boolean;
    FGamma: Double;
    FGammaInv: Double;
    procedure SetGamma(const GammaValue: Double; sRGB: boolean);
    procedure GammaChangedHandler;
  public type
    TLUT88 = array[byte, byte] of byte;
  public
    Mul255Div: TLUT88;
    MulDiv255: TLUT88;
  public
    class function PremultiplyLUT: PPremultiplyLUT; static;
    class function GammaPremultiplyLUT: PPremultiplyLUT; static;
    class procedure Apply(const LUT: TLUT88; Values, Alpha: PByteArray; Count: integer); static;

    property sRGB: boolean read FsRGB;
    property Gamma: Double read FGamma;
  end;

//------------------------------------------------------------------------------

class constructor TPremultiplyLUT.Create;
begin
  FLock := TCriticalSection.Create;
  FPremultiplyLUT := nil;
  FGammaPremultiplyLUT := nil;
end;

class destructor TPremultiplyLUT.Destroy;
begin
  FLock.Free;

  if (FPremultiplyLUT <> nil) then
    Dispose(FPremultiplyLUT);

  if (FGammaPremultiplyLUT <> nil) then
  begin
    UnregisterGammaChangeNotification(FGammaPremultiplyLUT.GammaChangedHandler);
    Dispose(FGammaPremultiplyLUT);
  end;
end;

//------------------------------------------------------------------------------

class procedure TPremultiplyLUT.Apply(const LUT: TLUT88; Values, Alpha: PByteArray; Count: integer);
begin
  while (Count > 0) do
  begin
    PByte(Values)^ := LUT[PByte(Values)^, PByte(Alpha)^];
    Inc(PByte(Values));
    Inc(PByte(Alpha));
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

class function TPremultiplyLUT.PremultiplyLUT: PPremultiplyLUT;
var
  AlphaValue, ColorValue: Integer;
begin
  if (FPremultiplyLUT = nil) then
  begin
    FLock.Acquire;

    if (FPremultiplyLUT = nil) then
    begin
      New(FPremultiplyLUT);

      for ColorValue := 0 to 255 do
      begin
        FPremultiplyLUT.Mul255Div[ColorValue, 0] := 0;
        FPremultiplyLUT.MulDiv255[ColorValue, 0] := 0;

        for AlphaValue := 1 to 255 do
        begin
          FPremultiplyLUT.Mul255Div[ColorValue, AlphaValue] := Clamp(Round(ColorValue * 255 / AlphaValue));
          FPremultiplyLUT.MulDiv255[ColorValue, AlphaValue] := Round(ColorValue * AlphaValue * OneOver255);
        end;
      end;
    end;

    FLock.Release;
  end;

  Result := FPremultiplyLUT;
end;

//------------------------------------------------------------------------------

class function TPremultiplyLUT.GammaPremultiplyLUT: PPremultiplyLUT;
begin
  if (FGammaPremultiplyLUT = nil) then
  begin
    FLock.Acquire;

    if (FGammaPremultiplyLUT = nil) then
    begin
      New(FGammaPremultiplyLUT);
      RegisterGammaChangeNotification(FGammaPremultiplyLUT.GammaChangedHandler);
      FGammaPremultiplyLUT.SetGamma(GAMMA_VALUE, GAMMA_IS_SRGB);
    end;

    FLock.Release;
  end;

  Result := FGammaPremultiplyLUT;
end;

procedure TPremultiplyLUT.GammaChangedHandler;
begin
  SetGamma(GAMMA_VALUE, GAMMA_IS_SRGB);
end;

procedure TPremultiplyLUT.SetGamma(const GammaValue: Double; sRGB: boolean);
var
  AlphaValue, ColorValue: Integer;
  n: Single;
  ColorLinear, ColorRGB: TFloat;
begin
  if (FsRGB = sRGB) and ((FsRGB) or (GammaValue = FGamma)) then
    exit;

  FsRGB := sRGB;

  if (not FsRGB) then
  begin
    FGamma := GammaValue;
    FGammaInv := 1 / FGamma;
  end;

  for ColorValue := 0 to 255 do
  begin
    Mul255Div[ColorValue, 0] := 0;
    MulDiv255[ColorValue, 0] := 0;

    // sRGB -> Linear RGB / 255
    ColorLinear := ColorValue * OneOver255;
    if (FsRGB) then
    begin
      if (ColorLinear >= 0.04045) then
        ColorLinear := Power((ColorLinear + 0.055) * (1 / 1.055), 2.4)
      else
        ColorLinear := ColorLinear * (1 / 12.92);

    end else
      ColorLinear := Power(ColorLinear, FGamma);

    // ColorValue: Color, AlphaValue: Alpha
    for AlphaValue := 1 to 255 do
    begin
      // Linear RGB -> Premultiplied, Linear RGB
      n := ColorLinear * AlphaValue;
      MulDiv255[ColorValue, AlphaValue] := Round(n);

      // Premultiplied, Linear RGB -> Unpremultiplied, Linear RGB
      n := ColorValue / AlphaValue;

      // Linear RGB -> sRGB / 255
      if (FsRGB) then
      begin
        if (n >= 0.0031308) then
          ColorRGB := 1.055 * Power(n, 1 / 2.4) - 0.055
        else
          ColorRGB := n * 12.92;
      end else
        ColorRGB := Power(n, FGammaInv);

      Mul255Div[ColorValue, AlphaValue] := Clamp(Round(ColorRGB * 255));
    end;
  end;
end;

//------------------------------------------------------------------------------

type
  TSingleDynArray = array of Single;

function GaussianKernel(Radius: Single): TSingleDynArray;
var
  i, R: Integer;
  StdDev, C1, C2: Single;
begin
  R := Ceil(Radius);
  SetLength(Result, R + 1);
  StdDev := Radius * GaussianRadiusToSigma;
  C1 := 1.0 / Sqrt(2.0 * Pi * StdDev);
  C2 := -0.5 / Sqr(StdDev);
  for i := 0 to R do
    Result[i] := C1 * Exp(Sqr(i) * C2);
end;


//------------------------------------------------------------------------------
//
// The original implementation of selective gaussian blur (similar to the
// one in GIMP).
//
//------------------------------------------------------------------------------
// Adapted from Eric Grange's version which in turn was based on the source
// of "Selective gaussian blur filter for the GIMP".
//------------------------------------------------------------------------------
procedure InternalSelectiveGaussianGimp(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer; const PremultiplyLUT: TPremultiplyLUT);
var
  X, Y, Plane, WindowX, WindowY, R, MaxX, MaxY: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, Weight: Single;
  RefColor: TColor32Entry;
  RefValue: Integer;
  SampleColor: TColor32Entry;
  SampleValue: Integer;
  DstValue: Byte;
  pDestColor: PColor32Entry;
  pDstLine, pSrcLine: PColor32Array;
begin
  ASSERT(Src <> Dst);

  if (Radius < GaussianRadiusToSigma) or (Delta <= 0) or (Src.Empty) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  R := Ceil(Radius);
  MaxX := Src.Width - 1;
  MaxY := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  Kernel := GaussianKernel(Radius);
  try

    for Y := 0 to MaxY do
    begin
      pDstLine := Dst.ScanLine[Y];

      for X := 0 to MaxX do
      begin
        RefColor := TColor32Entry(Src[X, Y]);
        pDestColor := @(pDstLine[X]);

        // Process each of the RGB channels in turn
        for Plane := 0 to 2 do
        begin
          Sum := 0;
          Fact := 0;

          RefValue := RefColor.Planes[Plane];
          // Premultiply and gamma (sRGB->LinearRGB)
          RefValue := PremultiplyLUT.MulDiv255[RefValue, RefColor.A];

          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;

          for WindowY := -R to R do
          begin
            if WindowY + Y < 0 then
              Continue;
            if WindowY + Y > MaxY then
              Break;

            pSrcLine := Src.ScanLine[WindowY + Y];
            for WindowX := -R to R do
            begin
              if WindowX + X < 0 then
                Continue;
              if WindowX + X > MaxX then
                Break;

              SampleColor := TColor32Entry(pSrcLine[WindowX + X]);
              SampleValue := SampleColor.Planes[Plane];
              // Premultiply and gamma (sRGB->LinearRGB)
              SampleValue := PremultiplyLUT.MulDiv255[SampleValue, SampleColor.A];

              if (SampleValue >= MinValue) and (SampleValue <= MaxValue) then
              begin
                Weight := Kernel[Abs(WindowX)] * Kernel[Abs(WindowY)];
                Sum := Sum + SampleValue * Weight;
                Fact := Fact + Weight;
              end;
            end;
          end;

          DstValue := FastRound(Sum / Fact); // TODO : Need to Clamp. Rounding errors can cause values to grow beyond 255
          // Unpremultiply and gamma (LinearRGB->sRGB)
          DstValue := PremultiplyLUT.Mul255Div[DstValue, RefColor.A];

          pDestColor.Planes[Plane] := DstValue;
        end;

        // Copy alpha
        pDestColor.A := RefColor.A;
      end;
    end;
  finally
    Kernel := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussianGimp(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.PremultiplyLUT;
  InternalSelectiveGaussianGimp(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;

procedure SelectiveGaussianGimpGamma(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.GammaPremultiplyLUT;
  InternalSelectiveGaussianGimp(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;



//------------------------------------------------------------------------------
//
// Optimized algorithm that performs horizontal and vertical blurring only
// once for each reference color at a certain position (x, y). A table is
// used for cacheing convolution sum of the reference color values that have
// already been visited. Vertical blurring can then be performed in a
// single pass by looking up already cached entries for a given reference
// color (and we can thus take advantage of the fact the gaussian is
// separable). In order to minimize required memory, the horizontal pass is
// performed on one column at a time. This requires Src.Height * 2^BitDepth
// bytes of memory, so it's a problem if we want to support 16-bit images.
// Another problem with images with higher bit-depth is that there is a
// smaller probability that adjacent colors have the same pixel values.
//
//------------------------------------------------------------------------------
procedure InternalSelectiveGaussianNew(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer; const PremultiplyLUT: TPremultiplyLUT);
type
  PCacheEntry = ^TCacheEntry;
  TCacheEntry = record
    Sum: Single;  // intermediate sum of convolution
    Fact: Single; // intermediate sum of kernel weights
  end;
var
  X, Y, Plane, WindowX, WindowY, LoY, SampleValue, R, MaxX, MaxY: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, Weight: Single;
  Color: TColor32Entry;
  pColor: PColor32Entry;
  RefValue: Integer;
  PSrcLine: PColor32Array;
  PEntry: PCacheEntry;
  SumCache: array of array [Byte] of TCacheEntry;
  LastPos: array [Byte] of Integer;
  Value: Byte;
begin
  ASSERT(Src <> Dst);

  if (Radius < GaussianRadiusToSigma) or (Delta <= 0) or (Src.Empty) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  R := Ceil(Radius);
  MaxX := Src.Width - 1;
  MaxY := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);
  Kernel := GaussianKernel(Radius);

  for X := 0 to MaxX do
  begin

    // Process each of the RGB channels in turn
    for Plane := 0 to 2 do
    begin
      FillLongword(LastPos[0], Length(LastPos), Cardinal(Low(Integer)));

      for Y := 0 to MaxY do
      begin

        Color := TColor32Entry(Src[X, Y]);
        RefValue := Color.Planes[Plane];

        // Premultiply and gamma (sRGB->LinearRGB)
        RefValue := PremultiplyLUT.MulDiv255[RefValue, Color.A];

        MinValue := RefValue - Delta;
        MaxValue := RefValue + Delta;

        if LastPos[RefValue] < Y - R then
          LoY := Y - R
        else
          LoY := LastPos[RefValue] + 1;

        for WindowY := LoY to Y + R do
        begin
          if WindowY < 0 then
            Continue;
          if WindowY > MaxY then
            Break;

          Sum := 0;
          Fact := 0;
          PSrcLine := Src.Scanline[WindowY];

          for WindowX := -R to R do
          begin
            if WindowX + X < 0 then
              Continue;
            if WindowX + X > MaxX then
              Break;

            Color := TColor32Entry(PSrcLine[WindowX + X]);
            SampleValue := Color.Planes[Plane];
            // Premultiply and gamma (sRGB->LinearRGB)
            SampleValue := PremultiplyLUT.MulDiv255[SampleValue, Color.A];

            if (SampleValue >= MinValue) and (SampleValue <= MaxValue) then
            begin
              Weight := Kernel[Abs(WindowX)];
              Sum := Sum + SampleValue * Weight;
              Fact := Fact + Weight;
            end;
          end;

          PEntry := @SumCache[WindowY][RefValue];
          PEntry.Sum := Sum;
          PEntry.Fact := Fact;
        end;

        LastPos[RefValue] := Y + R;
      end;

      for Y := 0 to MaxY do
      begin
        Color := TColor32Entry(Src[X, Y]);
        RefValue := Color.Planes[Plane];
        // Premultiply and gamma (sRGB->LinearRGB)
        RefValue := PremultiplyLUT.MulDiv255[RefValue, Color.A];

        Sum := 0;
        Fact := 0;
        for WindowY := -R to R do
        begin
          if WindowY + Y < 0 then
            Continue;
          if WindowY + Y > MaxY then
            Break;
          Weight := Kernel[Abs(WindowY)];
          PEntry := @SumCache[WindowY + Y][RefValue];
          Sum := Sum + PEntry.Sum * Weight;
          Fact := Fact + PEntry.Fact * Weight;
        end;

        pColor := PColor32Entry(Dst.PixelPtr[X, Y]);

        Value := Round(Sum / Fact);
        // Unpremultiply and gamma (LinearRGB->sRGB)
        Value := PremultiplyLUT.Mul255Div[Value, pColor.A];

        pColor.Planes[Plane] := Value;
      end;

      // Copy alpha
      for Y := 0 to MaxY do
        PColor32Entry(Dst.PixelPtr[X, Y]).A := TColor32Entry(Src[X, Y]).A;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussianNew(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.PremultiplyLUT;
  InternalSelectiveGaussianNew(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;

procedure SelectiveGaussianNewGamma(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.GammaPremultiplyLUT;
  InternalSelectiveGaussianNew(Src, Dst, Radius, Delta, PremultiplyLUT^);
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
  X, Y, Plane, {LoU,} WindowX, WindowY, SampleValue, R, MaxX, MaxY: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TSingleDynArray;
  Sum, Fact, Weight: Single;
  RefColor: TColor32;
  RefValue: Integer;
  PDstLine, PSrcLine: PColor32Array;
begin
  ASSERT(Src <> Dst);

  if (Radius < GaussianRadiusToSigma) or (Delta <= 0) or (Src.Empty) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  R := Ceil(Radius);
  MaxX := Src.Width - 1;
  MaxY := Src.Height - 1;

  Dst.SetSizeFrom(Src);
  Kernel := GaussianKernel(Radius);
  try
    for Y := 0 to MaxY do
    begin
      PDstLine := Dst.ScanLine[Y];
      PSrcLine := Src.ScanLine[Y];
      for X := 0 to MaxX do
      begin
        RefColor := Src[X, Y];
        for Plane := 0 to 2 do
        begin
          Sum := 0;
          Fact := 0;
          // TODO : Premultiply and gamma (sRGB->LinearRGB)
          RefValue := TColor32Entry(RefColor).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;
          for WindowX := -R to R do
          begin
            if WindowX + X < 0 then
              Continue;
            if WindowX + X > MaxX then
              Break;

            SampleValue := TColor32Entry(PSrcLine[WindowX + X]).Planes[Plane];
            if (SampleValue >= MinValue) and (SampleValue <= MaxValue) then
            begin
              Weight := Kernel[Abs(WindowX)];
              Sum := Sum + SampleValue * Weight;
              Fact := Fact + Weight;
            end;
          end;
          TColor32Entry(PDstLine[X]).Planes[Plane] := Round(Sum / Fact);
        end;
      end;
    end;

    for Y := 0 to MaxY do
    begin
      // TODO : This is using the source bitmap as a temporary buffer
      // thus destroying the source bitmap!
      PDstLine := Src.ScanLine[Y];
      for X := 0 to MaxX do
      begin
        RefColor := Dst[X, Y];
        for Plane := 0 to 2 do
        begin
          Sum := 0;
          Fact := 0;
          RefValue := TColor32Entry(RefColor).Planes[Plane];
          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;
          for WindowY := -R to R do
          begin
            if WindowY + Y < 0 then
              Continue;
            if WindowY + Y > MaxY then
              Break;

            SampleValue := TColor32Entry(Dst[X, WindowY + Y]).Planes[Plane];
            if (SampleValue >= MinValue) and (SampleValue <= MaxValue) then
            begin
              Weight := Kernel[Abs(WindowY)];
              Sum := Sum + SampleValue * Weight;
              Fact := Fact + Weight;
            end;
          end;
          // TODO : Unpremultiply and gamma (LinearRGB->sRGB)
          TColor32Entry(PDstLine[X]).Planes[Plane] := Round(Sum / Fact);
        end;
        TColor32Entry(PDstLine[X]).A := TColor32Entry(RefColor).A;
      end;
    end;
    Dst.Assign(Src);
  finally
    Kernel := nil;
  end;
end;



//------------------------------------------------------------------------------
//
// SIMD optimized Selective Gaussian Blur
// Originally by Mattias Andersson
//
//------------------------------------------------------------------------------
// Modified to also blur alpha channel.
// Various fixes for size not mod 4 and buffer overflows.
// Replaced MMX version with SSE2 version.
//------------------------------------------------------------------------------
type
  PCacheEntry = ^TCacheEntry;
  TCacheEntry = record
    Sum: Cardinal;  // intermediate sum of convolution
    Fact: Cardinal; // intermediate sum of kernel weights
  end;

  PRangeEntry = ^TRangeEntry;
  TRangeEntry = packed record
    Min: Byte;
    Max: Byte;
    Sum: Cardinal;
  end;

//------------------------------------------------------------------------------

function GaussianKernelInt(Radius: Single): TArrayOfWord;
var
  i, R: Integer;
  C: Single;
begin
  R := Ceil(Radius);
  SetLength(Result, R * 2 + 1);
  C := -0.5 / Sqr(Radius * GaussianRadiusToSigma);
//  for i := -R to R do
  for i := 0 to R do
  begin
//    Result[R+i] := Round(255 * Exp(Sqr(i) * C));
    Result[R+i] := Round(255 * Exp(Sqr(i) * C));
    Result[R-i] := Result[R+i];
  end;
end;

//------------------------------------------------------------------------------

procedure Accumulate_Pas(pSrc: PByteArray; pFact: PWordArray; Count, Min, Max: Integer; out Sum, FactSum: Cardinal);
begin
  Sum := 0;
  FactSum := 0;
  while (Count > 0) do
  begin
    Dec(Count);
    if (pSrc[Count] > Min) and (pSrc[Count] < Max) then
    begin
      Sum := Sum + ((pSrc[Count] * pFact[Count]) shr 8);
      FactSum := FactSum + pFact[Count];
    end;
  end;
end;

{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}

procedure Accumulate_SSE2(pSrc: Pointer; pFact: Pointer; Count, Min, Max: Integer; out Sum, FactSum: Cardinal); //{$IFDEF FPC} assembler; {$ENDIF}
  // Parameters (x86):
  //   EAX <- pSrc
  //   EDX <- pFact
  //   ECX <- Count
  //   Stack[0] <- Min
  //   Stack[1] <- Max
  //   Stack[2] <- @Sum
  //   Stack[3] <- @FactSum
  //
  // Parameters (x64):
  //   RCX <- pSrc
  //   RDX <- pFact
  //   R8 <- Count
  //   R9 <- Min
  //   Stack[0] <- Max
  //   Stack[1] <- @Sum
  //   Stack[2] <- @FactSum

  // SSE register usage:
  //   XMM0: Min | Min | Min | Min
  //   XMM1: Max | Max | Max | Max
  //   XMM2: Four pSrc bytes
  //   XMM3: Four pFact words
  //   XMM4: "Misc"
  //   XMM5: Sum
  //   XMM6: FactSum
  //   XMM7: "Zero"
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x64)}
{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
  .SAVENV XMM7
{$ENDIF}
{$elseif defined(TARGET_x86)}
  // nothing
{$else}
{$message fatal 'Unsupported target'}
{$ifend}
{$IFDEF FPC}
{$define RETARD_COMPILER} // Just to make it clear what I think of FPC's assembler
{$ENDIF}

  // initialize
        // M0 := Min;
        MOVD        XMM0, Min
        PUNPCKLWD   XMM0, XMM0  // Unpack Low Data ([ab][cd] -> [acbd])
        PUNPCKLDQ   XMM0, XMM0
        // M1 := Max;
        MOVD        XMM1, Max
        PUNPCKLWD   XMM1, XMM1
        PUNPCKLDQ   XMM1, XMM1

        PXOR        XMM5, XMM5
        PXOR        XMM6, XMM6
        PXOR        XMM7, XMM7

        // Negative offset "trick"
{$if defined(TARGET_x86)}
        LEA         pSrc, [pSrc+Count]
        LEA         pFact, [pFact+Count*2]
        NEG         Count
{$elseif defined(TARGET_x64)}
{$IFNDEF RETARD_COMPILER}
        LEA         pSrc, [pSrc+R8]
        LEA         pFact, [pFact+R8*2]
{$ELSE}
        LEA         ECX, [RCX+R8]
        LEA         EDX, [RDX+R8*2]
{$ENDIF}
        NEG         R8
{$ifend}

        // if (Count mod 4 = 0) then goto :ProcessFours
{$if defined(TARGET_x86)}
        TEST        Count, $0003
{$elseif defined(TARGET_x64)}
        TEST        R8, $0003
{$ifend}
        JZ          @ProcessFours

        // Process Count/4 remainders
{$if defined(TARGET_x86)}
        PUSH        EBX
        PUSH        EDI
{$ifend}
@NextOne:


{$if defined(TARGET_x86)}
        // if (pSrc[Count] <= Min) or (pSrc[Count] >= Max) then goto :SkipOne
        MOVZX       EBX, BYTE PTR[pSrc+Count] // Load single byte

        CMP         Min, EBX
        JGE         @SkipOne
        CMP         EBX, Max
        JGE         @SkipOne

        // Sum := Sum + ((pSrc[Count] * pFact[Count]) shr 8);
        MOVZX       EDI, WORD PTR[pFact+Count*2] // Load single word

        IMUL        EBX, EDI
        SHR         EBX, 8
        MOVD        XMM2, EBX
        PADDD       XMM5, XMM2

        // FactSum := FactSum + pFact[Count];
        MOVD        XMM3, EDI
        PADDD       XMM6, XMM3
{$elseif defined(TARGET_x64)}
        // if (pSrc[Count] <= Min) or (pSrc[Count] >= Max) then goto :SkipOne
{$IFNDEF RETARD_COMPILER}
        MOVZX       R10D, BYTE PTR[pSrc+R8] // Load single byte
{$ELSE}
        MOVZX       R10D, BYTE PTR[RCX+R8] // Load single byte
{$ENDIF}

{$IFNDEF RETARD_COMPILER}
        CMP         Min, R10D
{$ELSE}
        CMP         R9D, R10D
{$ENDIF}
        JGE         @SkipOne
        CMP         R10D, Max
        JGE         @SkipOne

        // Sum := Sum + ((pSrc[Count] * pFact[Count]) shr 8);
{$IFNDEF RETARD_COMPILER}
        MOVZX       R11D, WORD PTR[pFact+R8*2] // Load single word
{$ELSE}
        MOVZX       R11D, WORD PTR[RDX+R8*2] // Load single word
{$ENDIF}

        IMUL        R10D, R11D
        SHR         R10D, 8
        MOVD        XMM2, R10D
        PADDD       XMM5, XMM2

        // FactSum := FactSum + pFact[Count];
        MOVD        XMM3, R11D
        PADDD       XMM6, XMM3
{$ifend}

@SkipOne:
{$if defined(TARGET_x86)}
        INC         Count
{$elseif defined(TARGET_x64)}
        INC         R8
{$ifend}
        // if (Count mod 4 <> 0) then goto :NextOne
{$if defined(TARGET_x86)}
        TEST        Count, $0003
{$elseif defined(TARGET_x64)}
        TEST        R8, $0003
{$ifend}
        JNZ         @NextOne

{$if defined(TARGET_x86)}
        POP         EDI
        POP         EBX
{$ifend}

@ProcessFours:
        // Count := Count div 4
        // if (Count = 0) then goto :Done
{$if defined(TARGET_x86)}
        SAR         Count, 2
        JCXZ        @Done
{$elseif defined(TARGET_x64)}
        SAR         R8, 2
        JZ          @Done
{$ifend}


  // loop start
@Loop:
        // if (pSrc[Count] > Min) and (pSrc[Count] < Max) then
        // begin
        //   Sum := Sum + pSrc[Count] * pFact[Count];
        //   FactSum := FactSum + pFact[Count];
        // end;

        // M2 := pSrc[Count];
{$if defined(TARGET_x86)}
        MOVD        XMM2, DWORD PTR [pSrc+Count*4] // Load four bytes
{$elseif defined(TARGET_x64)}
{$IFNDEF RETARD_COMPILER}
        MOVD        XMM2, DWORD PTR [pSrc+R8*4] // Load four bytes
{$ELSE}
        MOVD        XMM2, DWORD PTR [RCX+R8*4] // Load four bytes
{$ENDIF}
{$ifend}
        PUNPCKLBW   XMM2, XMM7
        // M3 := pFact[Count];
{$if defined(TARGET_x86)}
        MOVQ        XMM3, QWORD PTR [pFact+Count*8] // Load four words
{$elseif defined(TARGET_x64)}
{$IFNDEF RETARD_COMPILER}
        MOVQ        XMM3, QWORD PTR [pFact+R8*8] // Load four words
{$ELSE}
        MOVQ        XMM3, QWORD PTR [RDX+R8*8] // Load four words
{$ENDIF}
{$ifend}

  // store threshold mask in MM4
        // M4 := M2;
        MOVQ        XMM4, XMM2
        // if (M4 > Min) then M4 := $FF else M4 := $00;
        PCMPGTW     XMM4, XMM0  // Compare Packed Signed Integers for Greater Than
        // M4 := M4 and Max;
        PAND        XMM4, XMM1
        // if (M4 > M2) then M4 := $FF else M4 := $00;
        PCMPGTW     XMM4, XMM2

  // mask colors and weights
        // M2 := M2 and M4;
        PAND        XMM2, XMM4
        // M3 := M3 and M4;
        PAND        XMM3, XMM4

  // multiply colors and weights
        // M2 := M2 * M3;
        PMULLW      XMM2, XMM3
        // Clear lower byte of four words
{$if (not defined(FPC)) or (not defined(TARGET_X64))}
        PAND        XMM2, DQWORD PTR [SSE_FF00FF00_ALIGNED]
{$else}
        PAND        XMM2, DQWORD PTR [rip+SSE_FF00FF00_ALIGNED]
{$ifend}

  // perform accumulation
        // M5 := M5 + M2;
        PSADBW      XMM2, XMM7  // Compute Sum of Absolute Differences
                                // This sums the four ((M2 * M3) shl 8).
        PADDD       XMM5, XMM2

        // M6 := M6 + M3;
        PSADBW      XMM3, XMM7
        PADDD       XMM6, XMM3

  // loop end
{$if defined(TARGET_x86)}
        INC         Count
{$elseif defined(TARGET_x64)}
        INC         R8
{$ifend}
        JNZ         @Loop

@Done:

{$if defined(TARGET_x86)}
        MOV         EAX, Sum
        MOVD        DWORD PTR [EAX], XMM5
        MOV         EAX, FactSum
        MOVD        DWORD PTR [EAX], XMM6
{$elseif defined(TARGET_x64)}
        MOV         RAX, Sum
        MOVD        DWORD PTR [RAX], XMM5
        MOV         RAX, FactSum
        MOVD        DWORD PTR [RAX], XMM6
{$ifend}

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4', 'XMM5', 'XMM6', 'XMM7'];{$ifend}
end;
{$ifend}

//------------------------------------------------------------------------------

procedure InternalSelectiveGaussian1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer; const PremultiplyLUT: TPremultiplyLUT);
const
  SourcePlanes: array[0..2] of TConversionType = (ctBlue, ctGreen, ctRed);
var
  X, Y, WindowY, MinX: NativeInt; // NativeInt required on FPC to support negative array offset
  Plane, LoY, R, MaxX, MaxY: Integer;
  XCount, ColCount: Integer;
  MinValue, MaxValue: Integer;
  Kernel: TArrayOfWord;
  PKernel: PWordArray;
  VSum, VFact, Sum: Cardinal;
  Weight, Fact, CFact: Cardinal;
  RefValue: Cardinal;
  CacheEntry: PCacheEntry;
  Value: Cardinal;
  pColor: PColor32Entry;

  SumCache: array of array [Byte] of TCacheEntry;
  LastPos: array [Byte] of Integer;

  Map: array[Low(SourcePlanes)..High(SourcePlanes)] of TByteMap;
  pMap: PByteArray;
begin
  ASSERT(Src <> Dst);

  if (Radius < GaussianRadiusToSigma) or (Delta <= 0) or (Src.Empty) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  R := Ceil(Radius);
  MaxX := Src.Width - 1;
  MaxY := Src.Height - 1;
  ColCount := Src.Width;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);

  Kernel := GaussianKernelInt(Radius);
  PKernel := PWordArray(@Kernel[R]); // Note: Pointer to midpoint

  for Plane := Low(Map) to High(Map) do
    Map[Plane] := TByteMap.Create;
  try

    // Load RGB into separate maps
    for Plane := Low(Map) to High(Map) do
    begin
      Map[Plane].ReadFrom(Src, SourcePlanes[Plane]);

      // Premultiply and gamma (sRGB->LinearRGB)
      pMap := Map[Plane].Bits;
      for X := 0 to Src.Width*Src.Height-1 do
      begin
        TColor32Entry(Dst.Bits[X]).A := TColor32Entry(Src.Bits[X]).A;
        pMap[X] := PremultiplyLUT.MulDiv255[pMap[X], TColor32Entry(Src.Bits[X]).A];
      end;
    end;

    CFact := 0;
    for X := -R to R do
      CFact := CFact + PKernel[X];

    for X := 0 to MaxX do
    begin
      MinX := Max(-R, -X);
      XCount := Min(R, MaxX - X) - MinX;

      // Process each channel in turn
      for Plane := Low(Map) to High(Map) do
      begin
        pMap := PByteArray(Map[Plane].ValPtr[X + MinX, 0]);

        FillLongword(LastPos[0], Length(LastPos), Cardinal(Low(Integer)));
        for Y := 0 to MaxY do
        begin
          RefValue := pMap[Y * ColCount - MinX];

          MinValue := integer(RefValue) - Delta;
          MaxValue := integer(RefValue) + Delta;

          if LastPos[RefValue] < Y - R then
          begin
            if Y < R then
              LoY := 0
            else
              LoY := Y - R
          end else
            LoY := LastPos[RefValue] + 1;

          VSum := 0;
          VFact := 0;
          for WindowY := Y - R to LoY - 1 do
          begin
            if WindowY < 0 then
              Continue;
            Weight := PKernel[WindowY - Y];
            CacheEntry := @SumCache[WindowY][RefValue];
            VSum := VSum + CacheEntry.Sum * Weight;
            VFact := VFact + CacheEntry.Fact * Weight;
          end;

          for WindowY := LoY to Y + R do
          begin
            if WindowY > MaxY then
              Break;

            SelectiveGaussianAccumulate(@pMap[WindowY * ColCount], @PKernel[MinX], XCount, MinValue, MaxValue, Sum, Fact);

            CacheEntry := @SumCache[WindowY][RefValue];
            CacheEntry.Sum := Sum;
            CacheEntry.Fact := Fact;

            Weight := PKernel[WindowY - Y];
            VSum := VSum + Sum * Weight;
            VFact := VFact + Fact * Weight;
          end;
          LastPos[RefValue] := Min(Y + R, MaxY);

          Value := 0;
          pColor := PColor32Entry(Dst.PixelPtr[X, Y]);

          // Note:
          // It's tempting to lessen the rounding error by doing a
          // "VSum shl 8" below instead of a "VFact shr 8" here.
          // Unfortunately that can cause an overflow because
          // "VSum shl 8" overflows 31 bits and turn the result
          // negative.
          // In order to avoid this overflow we need the Sum variables
          // to be unsigned so we can use all 32 bits.
          // Old:
          //   VFact := VFact shr 8;
          //   Value := VSum div (VFact shr 8);
          // New:
          //   Value := (VSum shl 8) div VFact;

          if (VSum <> 0) and (VFact <> 0) then
          begin

            // We could improve the precision and lessen the signal loss by
            // doing a Round instead of a Div here, but it only improves
            // the loss slightly and it absolutely kills the performance.
            // Value := Round((VSum shl 8) / VFact);

            Value := (VSum shl 8) div VFact;

            // Unpremultiply and gamma (LinearRGB->sRGB)
            Value := PremultiplyLUT.Mul255Div[Value, pColor.A];

          end;

          pColor.Planes[Plane] := Value;

        end;
      end;
    end;

  finally
    for Plane := Low(Map) to High(Map) do
      Map[Plane].Free;
  end;
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussian1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.PremultiplyLUT;
  InternalSelectiveGaussian1(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;

procedure SelectiveGaussianGamma1(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.GammaPremultiplyLUT;
  InternalSelectiveGaussian1(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;

//------------------------------------------------------------------------------

procedure InternalSelectiveGaussian2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer; const PremultiplyLUT: TPremultiplyLUT);
const
  SourcePlanes: array[0..2] of TConversionType = (ctBlue, ctGreen, ctRed);
var
  KernelMinX, KernelMaxX, WindowX, WindowY: NativeInt;
  Plane, LoY, R: Integer;
  X, Y, MaxX, MaxY: integer;
  MinValue, MaxValue: integer;
  ColCount: integer;
  Kernel: TArrayOfWord;
  PKernel: PWordArray;
  VSum, VFact, Sum: Cardinal;
  Weight, Fact, CFact: Cardinal;
  RefValue: integer;
  PSrcLine: PByteArray;
  CacheEntry: PCacheEntry;
  RangeEntry: PRangeEntry;
  pColor: PColor32Entry;
  SampleValue: integer;
  Value: Cardinal;

  SumCache: array of array [Byte] of TCacheEntry;
  RangeCache: array of TRangeEntry;
  LastPos: array [Byte] of Integer;

  Map: array[Low(SourcePlanes)..High(SourcePlanes)] of TByteMap;
  PMap: PByteArray;
begin
  ASSERT(Src <> Dst);

  if (Radius < GaussianRadiusToSigma) or (Delta <= 0) or (Src.Empty) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  R := Ceil(Radius);
  MaxX := Src.Width - 1;
  MaxY := Src.Height - 1;
  ColCount := Src.Width;

  Dst.SetSizeFrom(Src);
  SetLength(SumCache, Src.Height);
  SetLength(RangeCache, Src.Height);

  Kernel := GaussianKernelInt(Radius);
  PKernel := PWordArray(@Kernel[R]);

  for Plane := Low(Map) to High(Map) do
    Map[Plane] := TByteMap.Create;
  try

    // Load RGB into separate maps
    for Plane := Low(Map) to High(Map) do
    begin
      Map[Plane].ReadFrom(Src, SourcePlanes[Plane]);

      // Premultiply and gamma (sRGB->LinearRGB)
      pMap := Map[Plane].Bits;
      for X := 0 to Src.Width*Src.Height-1 do
      begin
        TColor32Entry(Dst.Bits[X]).A := TColor32Entry(Src.Bits[X]).A;
        pMap[X] := PremultiplyLUT.MulDiv255[pMap[X], TColor32Entry(Src.Bits[X]).A];
      end;
    end;

    CFact := 0;
    for WindowX := -R to R do
      CFact := CFact + PKernel[WindowX];

    for X := 0 to MaxX do
    begin
      KernelMinX := Max(-R, -X);
      KernelMaxX := Min(R, MaxX - X);

      // Process each channel in turn
      for Plane := Low(Map) to High(Map) do
      begin
        PMap := PByteArray(Map[Plane].ValPtr[X, 0]);

        // compute range entries

        for Y := 0 to MaxY do
        begin
          PSrcLine := PByteArray(Map[Plane].ValPtr[X, Y]);

          Sum := 0;
          MinValue := 255;
          MaxValue := 0;

          for WindowX := KernelMinX to KernelMaxX do
          begin
            SampleValue := PSrcLine[WindowX];
            Sum := Sum + Cardinal(PKernel[WindowX] * SampleValue);

            if SampleValue < MinValue then
              MinValue := SampleValue;
            if SampleValue > MaxValue then
              MaxValue := SampleValue;
          end;

          RangeEntry := @RangeCache[Y];
          RangeEntry.Min := MinValue;
          RangeEntry.Max := MaxValue;
          RangeEntry.Sum := Sum shr 8;
        end;

        FillLongword(LastPos[0], Length(LastPos), Cardinal(Low(Integer)));
        for Y := 0 to MaxY do
        begin
          RefValue := PMap[Y * ColCount];

          MinValue := RefValue - Delta;
          MaxValue := RefValue + Delta;

          if LastPos[RefValue] < Y - R then
          begin
            if Y < R then
              LoY := 0
            else
              LoY := Y - R
          end else
            LoY := LastPos[RefValue] + 1;

          VSum := 0;
          VFact := 0;
          for WindowY := Y - R to LoY - 1 do
          begin
            if WindowY < 0 then
              Continue;
            Weight := PKernel[WindowY - Y];
            CacheEntry := @SumCache[WindowY][RefValue];
            VSum := VSum + CacheEntry.Sum * Weight;
            VFact := VFact + CacheEntry.Fact * Weight;
          end;

          for WindowY := LoY to Y + R do
          begin
            if WindowY > MaxY then
              break;

            RangeEntry := @RangeCache[WindowY];

            if (RangeEntry.Min < MinValue) or (RangeEntry.Max > MaxValue) then
            begin

              SelectiveGaussianAccumulate(@PMap[WindowY * ColCount + KernelMinX], @PKernel[KernelMinX], KernelMaxX - KernelMinX, MinValue, MaxValue, Sum, Fact);

            end else
            begin
              Sum := RangeEntry.Sum;
              Fact := CFact;
              if X - R < 0 then
                for WindowX := X to R - 1 do
                  Fact := Fact - PKernel[WindowX + 1]
              else
              if X + R > MaxX then
                for WindowX := MaxX - X to R - 1 do
                  Fact := Fact - PKernel[WindowX + 1];
            end;

            CacheEntry := @SumCache[WindowY][RefValue];
            CacheEntry.Sum := Sum;
            CacheEntry.Fact := Fact;

            Weight := PKernel[WindowY - Y];
            VSum := VSum + Sum * Weight;
            VFact := VFact + Fact * Weight;
          end;

          LastPos[RefValue] := Min(Y + R, MaxY);

          Value := 0;
          pColor := PColor32Entry(Dst.PixelPtr[X, Y]);

          // Disabled: We do a "VSum shl 8" below instead.
          // See comment in InternalSelectiveGaussian2.
          // VFact := VFact shr 8;

          if (VSum <> 0) and (VFact <> 0) then
          begin

            Value := (VSum shl 8) div VFact;

            // Unpremultiply and gamma (LinearRGB->sRGB)
            Value := PremultiplyLUT.Mul255Div[Value, pColor.A];

          end;

          pColor.Planes[Plane] := Value;

        end;
      end;
    end;

  finally
    for Plane := Low(Map) to High(Map) do
      Map[Plane].Free;
  end;
end;

//------------------------------------------------------------------------------

procedure SelectiveGaussian2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.PremultiplyLUT;
  InternalSelectiveGaussian2(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;

procedure SelectiveGaussianGamma2(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
var
  PremultiplyLUT: PPremultiplyLUT;
begin
  PremultiplyLUT := TPremultiplyLUT.GammaPremultiplyLUT;
  InternalSelectiveGaussian2(Src, Dst, Radius, Delta, PremultiplyLUT^);
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure SelectiveGaussian32NotImplemented(ASource, ADest: TBitmap32; Radius: TFloat; Delta: Integer);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

procedure RegisterBindings;
begin
  BlurRegistry.RegisterBinding(@@SelectiveGaussianBlur32, 'SelectiveGaussianBlur32');
  BlurRegistry.RegisterBinding(@@GammaSelectiveGaussianBlur32, 'GammaSelectiveGaussianBlur32');
  BlurRegistry.RegisterBinding(@@SelectiveGaussianAccumulate, 'SelectiveGaussianAccumulate');


  (*
  ** SelectiveGaussianAccumulate
  *)
  BlurRegistry[@@SelectiveGaussianAccumulate].Add(@Accumulate_Pas,              [isPascal]).Name := 'Accumulate_Pas';
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  BlurRegistry[@@SelectiveGaussianAccumulate].Add(@Accumulate_SSE2,             [isSSE2]).Name := 'Accumulate_SSE2';
{$ifend}


  // Implementation ordered by performance:
  // 1. SelectiveGaussian1
  // 2. SelectiveGaussian2
  // 3. SelectiveGaussianHorzVert (destructive, disqualified)
  // 4. SelectiveGaussianNew
  // 5. SelectiveGaussianGimp

  (*
  ** SelectiveGaussianBlur32
  *)
  BlurRegistry[@@SelectiveGaussianBlur32].Add(@SelectiveGaussianGimp,         [isPascal], 1024).Name := 'SelectiveGaussianGimp';
  BlurRegistry[@@SelectiveGaussianBlur32].Add(@SelectiveGaussianNew,          [isPascal], 768).Name := 'SelectiveGaussianNew';
  BlurRegistry[@@SelectiveGaussianBlur32].Add(@SelectiveGaussian2,            [isPascal], 0).Name := 'SelectiveGaussian2';
  BlurRegistry[@@SelectiveGaussianBlur32].Add(@SelectiveGaussian1,            [isPascal], -256).Name := 'SelectiveGaussian1';

  (*
  ** GammaSelectiveGaussianBlur32
  *)
  BlurRegistry[@@GammaSelectiveGaussianBlur32].Add(@SelectiveGaussianGimpGamma,    [isPascal], 1024).Name := 'SelectiveGaussianGimpGamma';
  BlurRegistry[@@GammaSelectiveGaussianBlur32].Add(@SelectiveGaussianNewGamma,     [isPascal], 768).Name := 'SelectiveGaussianNewGamma';
  BlurRegistry[@@GammaSelectiveGaussianBlur32].Add(@SelectiveGaussianGamma2,       [isPascal], 0).Name := 'SelectiveGaussianGamma2';
  BlurRegistry[@@GammaSelectiveGaussianBlur32].Add(@SelectiveGaussianGamma1,       [isPascal], -256).Name := 'SelectiveGaussianGamma1';


  (*
  ** Rebind the above bindings
  *)
  BlurRegistry[@@SelectiveGaussianBlur32].Rebind;
  BlurRegistry[@@GammaSelectiveGaussianBlur32].Rebind;
  BlurRegistry[@@SelectiveGaussianAccumulate].Rebind;
end;

initialization
  SelectiveGaussianBlur32 := SelectiveGaussian32NotImplemented;
  GammaSelectiveGaussianBlur32 := SelectiveGaussian32NotImplemented;
  RegisterBindings;
end.
