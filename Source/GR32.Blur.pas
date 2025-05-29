unit GR32.Blur;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes,
  GR32_Bindings,
  GR32;

//------------------------------------------------------------------------------
// Note that all blur functions operate on all channels (R, G, B, and A).
// If you don't want the Alpha channel blurred, reset/restore the Alpha of the
// result bitmap after it has been blurred.
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
//
//      Gaussian Blur
//
//------------------------------------------------------------------------------
// Note that although it is common for Gaussian blurs to specify the amount of
// blur as the Gaussian standard deviation parameter (most often referred to
// as "Sigma"), most users would probably prefer to specify the blur amount as
// the radius of the blur.
//
// The problem with that is that the Gaussian curve has an infinite radius
// regardless of the value of Sigma. A common solution, and the one we use here,
// is to simply clip the Gaussian curve to ignore the part that has values
// smaller than one pixel.
//
// So, even though the blur function internally works with Sigma values the
// public wrapper function requires the blur amount to be specified as a pixel
// radius.
// If you need to specify the blur as Sigma, simply convert between Sigma and
// pixels with the GaussianSigmaToRadius and GaussianRadiusToSigma constants:
//
//   Radius = Sigma * GaussianSigmaToRadius
//   Sigma = Radius * GaussianRadiusToSigma
//
//------------------------------------------------------------------------------
procedure Blur32(ASource, ADest: TBitmap32; Radius: TFloat); overload;
procedure Blur32(Bitmap: TBitmap32; Radius: TFloat); overload;
procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect); overload;
procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint); overload;

// Variants that take Gamma into acount
procedure GammaBlur32(ASource, ADest: TBitmap32; Radius: TFloat); overload;
procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat); overload;
procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect); overload;
procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint); overload;


const
  // Ratio between Radius and Sigma.
  GaussianRadiusToSigma = 0.300386630413846; // See TGaussianKernel for the rationale behind this value
  GaussianSigmaToRadius = 1 / GaussianRadiusToSigma;

var
  Blur32MinRadius: TFloat = 0.5;

// Bindings
type
  TBlur32Proc = procedure(ASource, ADest: TBitmap32; Radius: TFloat);
  TBlurInplace32Proc = procedure(Bitmap: TBitmap32; Radius: TFloat);

var
  Blur32Proc: TBlur32Proc;
  BlurInplace32Proc: TBlurInplace32Proc;
  GammaBlur32Proc: TBlur32Proc;
  GammaBlurInplace32Proc: TBlurInplace32Proc;


//------------------------------------------------------------------------------
//
//      Horizontal Blur
//
//------------------------------------------------------------------------------
// Blurs in the horizontal direction only.
// Can be used to implement effects such as motion blur.
//------------------------------------------------------------------------------
var
  HorizontalBlur32: TBlur32Proc;
  GammaHorizontalBlur32: TBlur32Proc;


//------------------------------------------------------------------------------
//
//      Box Blur
//
//------------------------------------------------------------------------------
//
// One way to reduce the cost of a gaussian blur is to use a three-pass box blur
// approach. This means that you convolve the image with a box filter three
// times in a row. The width of the box filter should be the same in each pass.
// This will correspond to convolving the image with a second-order B-spline
// filter, which is very similar to a Gaussian filter.
//
//------------------------------------------------------------------------------
//
// A fast algorithm for performing box-blur is to compute the cumulative sum of
// each scanline and then to determine the convolved pixel value by computing
//
//  (CSum[i + r] - CSum[i - r]) / (2*r + 1)
//
// where CSum is the cumulative sum.
//
//------------------------------------------------------------------------------
// When approximating a gaussian blur with a three-pass box blur, be aware that:
//   - The cost of a box blur grows exponentially with the blur radius.
//   - The current gaussian blur implementation is most likely faster than the
//     box blur implementation.
//------------------------------------------------------------------------------
(* Since our default Gaussian blur currently outperforms all known box blur
** implementations (including variants such as stackblur), the box blur
** implementations has not been included and the bindings are not made available.

type
  TBoxBlur32Proc = procedure(ASource, ADest: TBitmap32; Radius: integer);
  TBoxBlurDiscrete32Proc = procedure(ASource, ADest: TBitmap32; Radius: integer; Passes: integer = 3);

var
  BoxBlur32: TBoxBlur32Proc deprecated;
  BoxBlurDiscrete32: TBoxBlurDiscrete32Proc deprecated;
*)


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
function BlurRegistry: TFunctionRegistry;



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types,
  SysUtils,
  GR32_Backends_Generic,
  GR32_Blend,
  GR32_Resamplers,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32.Blur.RecursiveGaussian;


//------------------------------------------------------------------------------
//
//      Gaussian Blur
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Pixel combiner for use by the Bitmap polygon filler
//------------------------------------------------------------------------------
type
  TBlurCombiner = class
  public
    class procedure PixelCombineHandler(F: TColor32; var B: TColor32; M: Cardinal);
  end;

class procedure TBlurCombiner.PixelCombineHandler(F: TColor32; var B: TColor32; M: Cardinal);
begin
  CombineMem(F, B, M);
end;

//------------------------------------------------------------------------------
// Abstract blur of region.
// Handles both with and without gamma via delegates.
//------------------------------------------------------------------------------
procedure BlurRegion32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint; BlurDelegate: TBlur32Proc; BlurInplaceDelegate: TBlurInplace32Proc);
var
  Bounds: TRect;
  BlurBlock: boolean;
  Dest: TBitmap32;
  Points: TArrayOfArrayOfFloatPoint;
  Filler: TBitmapPolygonFiller;
begin
  Bounds := MakeRect(PolygonBounds(Region), rrOutside);

  // If we are blurring less than 75% of the bitmap, do it via a temporary bitmap
  BlurBlock := (Bitmap.Width*Bitmap.Height * 0.75 > Bounds.Width * Bounds.Height);

  Dest := TBitmap32.Create(TMemoryBackend);
  try
    Dest.DrawMode := dmCustom;
    Dest.OnPixelCombine := TBlurCombiner.PixelCombineHandler;

    if (BlurBlock) then
    begin
      // The temporary bitmap contains just the area to be blurred
      Dest.SetSize(Bounds.Width, Bounds.Height);

      // Copy the target area
      BlockTransfer(Dest, 0, 0, Dest.BoundsRect, Bitmap, Bounds, dmOpaque);

      // Blur just the target area
      BlurInplaceDelegate(Dest, Radius);

      // Use a polygon filler to transfer the pixels covered by the region back
      // into the target bitmap
      Filler := TBitmapPolygonFiller.Create;
      try
        Filler.Pattern := Dest;
        Filler.OffsetX := Bounds.Left;
        Filler.OffsetY := Bounds.Top;

{$if defined(DynArrayOps)} // XE7
        Points := [Region];
{$else}
        Points := PolyPolygon(Region);
{$ifend}
        PolyPolygonFS(Bitmap, Points, Filler);
      finally
        Filler.Free;
      end;
    end else
    begin
      // Blur the whole source bitmap into the temporary bitmap
      BlurDelegate(Bitmap, Dest, Radius);

      // Use a polygon filler to transfer the pixels covered by the region
      // back into the target bitmap
      Filler := TBitmapPolygonFiller.Create;
      try
        Filler.Pattern := Dest;

{$if defined(DynArrayOps)} // XE7
        Points := [Region];
{$else}
        Points := PolyPolygon(Region);
{$ifend}
        PolyPolygonFS(Bitmap, Points, Filler);
      finally
        Filler.Free;
      end;
    end;

  finally
    Dest.Free;
  end;
end;

//------------------------------------------------------------------------------
// Abstract blur of rectagular area.
// Handles both with and without gamma via delegates.
//------------------------------------------------------------------------------
procedure BlurRect32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect; BlurDelegate: TBlur32Proc; BlurInplaceDelegate: TBlurInplace32Proc);
var
  Dest: TBitmap32;
  Points: TArrayOfFloatPoint;
begin
  // If we are blurring less than 75% of the bitmap, do it via a temporary bitmap
  if (Bitmap.Width*Bitmap.Height * 0.75 > Bounds.Width * Bounds.Height) then
  begin
    // Create a temporary bitmap containing just the area to be blurred
    Dest := TBitmap32.Create(TMemoryBackend);
    try
      Dest.SetSize(Bounds.Width, Bounds.Height);

      // Copy the target area
      BlockTransfer(Dest, 0, 0, Dest.BoundsRect, Bitmap, Bounds, dmOpaque);

      // Blur just the target area
      BlurInplaceDelegate(Dest, Radius);

      // Copy the blurred area back into the source bitmap
      BlockTransfer(Bitmap, Bounds.Left, Bounds.Top, Bounds, Dest, Dest.BoundsRect, dmOpaque);
    finally
      Dest.Free;
    end;
  end else
  begin
    // Masked blur via polygon filler
    Points := Rectangle(Bounds);
    BlurRegion32(Bitmap, Radius, Points, BlurDelegate, BlurInplaceDelegate);
  end;
end;

//------------------------------------------------------------------------------
// Blur32 API
//------------------------------------------------------------------------------
procedure Blur32(ASource, ADest: TBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) then
  begin
    ASource.CopyMapTo(ADest);
    exit;
  end;

  if (Assigned(Blur32Proc)) then
    Blur32Proc(ASource, ADest, Radius)
  else
  if (Assigned(BlurInplace32Proc)) then
  begin
    ASource.CopyMapTo(ADest);
    BlurInplace32Proc(ADest, Radius);
  end else
    raise Exception.Create('Missing Blur32 implementation');
end;

procedure Blur32(Bitmap: TBitmap32; Radius: TFloat);
var
  Dest: TBitmap32;
begin
  if (Radius < Blur32MinRadius) then
    exit;

  if (Assigned(BlurInplace32Proc)) then
    BlurInplace32Proc(Bitmap, Radius)
  else
  if (Assigned(Blur32Proc)) then
  begin
    Dest := TBitmap32.Create(TMemoryBackend);
    try
      Blur32Proc(Bitmap, Dest, Radius);
      Dest.CopyMapTo(Bitmap);
    finally
      Dest.Free;
    end;
  end else
    raise Exception.Create('Missing Blur32 implementation');
end;

procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect);
begin
  if (Radius < Blur32MinRadius) then
    exit;

  BlurRect32(Bitmap, Radius, Bounds, Blur32, Blur32);
end;


procedure Blur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint);
begin
  if (Radius < Blur32MinRadius) then
    exit;

  BlurRegion32(Bitmap, Radius, Region, Blur32, Blur32);
end;

//------------------------------------------------------------------------------

procedure GammaBlur32(ASource, ADest: TBitmap32; Radius: TFloat);
begin
  if (Radius < Blur32MinRadius) then
  begin
    ASource.CopyMapTo(ADest);
    exit;
  end;

  if (Assigned(GammaBlur32Proc)) then
    GammaBlur32Proc(ASource, ADest, Radius)
  else
  if (Assigned(GammaBlurInplace32Proc)) then
  begin
    ASource.CopyMapTo(ADest);
    GammaBlurInplace32Proc(ADest, Radius);
  end else
    raise Exception.Create('Missing GammaBlur32 implementation');
end;

procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat);
var
  Dest: TBitmap32;
begin
  if (Radius < Blur32MinRadius) then
    exit;

  if (Assigned(GammaBlurInplace32Proc)) then
    GammaBlurInplace32Proc(Bitmap, Radius)
  else
  if (Assigned(GammaBlur32Proc)) then
  begin
    Dest := TBitmap32.Create(TMemoryBackend);
    try
      GammaBlur32Proc(Bitmap, Dest, Radius);
      Dest.CopyMapTo(Bitmap);
    finally
      Dest.Free;
    end;
  end else
    raise Exception.Create('Missing GammaBlur32 implementation');
end;

procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Bounds: TRect);
begin
  if (Radius < Blur32MinRadius) then
    exit;

  BlurRect32(Bitmap, Radius, Bounds, GammaBlur32, GammaBlur32);
end;

procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat; const Region: TArrayOfFloatPoint);
begin
  if (Radius < Blur32MinRadius) then
    exit;

  BlurRegion32(Bitmap, Radius, Region, GammaBlur32, GammaBlur32);
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure Blur32NotImplemented(ASource, ADest: TBitmap32; Radius: TFloat);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

procedure BlurInplace32NotImplemented(Bitmap: TBitmap32; Radius: TFloat);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

//------------------------------------------------------------------------------

var
  FBlurRegistry: TFunctionRegistry;

procedure RegisterBindings;
begin
  FBlurRegistry.RegisterBinding(@@Blur32Proc, 'Blur32Proc');
  FBlurRegistry.RegisterBinding(@@BlurInplace32Proc, 'BlurInplace32Proc');

  FBlurRegistry.RegisterBinding(@@GammaBlur32Proc, 'GammaBlur32Proc');
  FBlurRegistry.RegisterBinding(@@GammaBlurInplace32Proc, 'GammaBlurInplace32Proc');

  FBlurRegistry.RegisterBinding(@@HorizontalBlur32, 'HorizontalBlur32');
  FBlurRegistry.RegisterBinding(@@GammaHorizontalBlur32, 'GammaHorizontalBlur32');

  // Default fallback stubs for unimplemented functions
  FBlurRegistry[@@HorizontalBlur32].Add(@Blur32NotImplemented, [isPascal], FBlurRegistry.WORST_PRIORITY);
  FBlurRegistry[@@GammaHorizontalBlur32].Add(@Blur32NotImplemented, [isPascal], FBlurRegistry.WORST_PRIORITY);
end;

function BlurRegistry: TFunctionRegistry;
begin
  if (FBlurRegistry = nil) then
  begin
    FBlurRegistry := NewRegistry('GR32.Blur bindings');
    RegisterBindings;
  end;

  Result := FBlurRegistry;
end;

initialization
  BlurRegistry.RebindAll;
end.
