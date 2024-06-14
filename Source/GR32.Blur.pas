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
  Windows,
  GR32_Bindings,
  GR32;


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
procedure Blur32(Src, Dst: TBitmap32; Radius: TFloat); overload;
procedure Blur32(Bitmap: TBitmap32; Radius: TFloat); overload;

// Variants that take Gamma into acount
procedure GammaBlur32(Src, Dst: TBitmap32; Radius: TFloat); overload;
procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat); overload;


const
  // Ratio between Radius and Sigma.
  GaussianRadiusToSigma = 0.300386630413846; // See TGaussianKernel for the rationale behind this value
  GaussianSigmaToRadius = 1 / GaussianRadiusToSigma;


// Bindings
type
  TBlur32Proc = procedure(Src, Dst: TBitmap32; Radius: TFloat);
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
// Can be used as ordinary Gaussian Blur by specifying Delta >= 255
//------------------------------------------------------------------------------
type
  TSelectiveGaussian32Proc = procedure(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);

var
  SelectiveGaussianBlur32: TSelectiveGaussian32Proc;


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
  TBoxBlur32Proc = procedure(Src, Dst: TBitmap32; Radius: integer);
  TBoxBlurDiscrete32Proc = procedure(Src, Dst: TBitmap32; Radius: integer; Passes: integer = 3);

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
  SysUtils,
  GR32_Backends_Generic,
  GR32.Blur.RecursiveGaussian,
  GR32.Blur.SelectiveGaussian;


//------------------------------------------------------------------------------
//
//      Gaussian Blur
//
//------------------------------------------------------------------------------
type
  TBitmap32Cracker = class(TCustomBitmap32);

procedure Blur32(Src, Dst: TBitmap32; Radius: TFloat);
begin
  if (Assigned(Blur32Proc)) then
    Blur32Proc(Src, Dst, Radius)
  else
  if (Assigned(BlurInplace32Proc)) then
  begin
    TBitmap32Cracker(Src).CopyMapTo(Dst);
    BlurInplace32Proc(Dst, Radius);
  end else
    raise Exception.Create('Missing Blur32 implementation');
end;

procedure Blur32(Bitmap: TBitmap32; Radius: TFloat);
var
  Dest: TBitmap32;
begin
  if (Assigned(BlurInplace32Proc)) then
    BlurInplace32Proc(Bitmap, Radius)
  else
  if (Assigned(Blur32Proc)) then
  begin
    Dest := TBitmap32.Create(TMemoryBackend);
    try
      Blur32Proc(Bitmap, Dest, Radius);
      TBitmap32Cracker(Dest).CopyMapTo(Bitmap);
    finally
      Dest.Free;
    end;
  end else
    raise Exception.Create('Missing Blur32 implementation');
end;

procedure GammaBlur32(Src, Dst: TBitmap32; Radius: TFloat);
begin
  if (Assigned(GammaBlur32Proc)) then
    GammaBlur32Proc(Src, Dst, Radius)
  else
  if (Assigned(GammaBlurInplace32Proc)) then
  begin
    TBitmap32Cracker(Src).CopyMapTo(Dst);
    GammaBlurInplace32Proc(Dst, Radius);
  end else
    raise Exception.Create('Missing GammaBlur32 implementation');
end;

procedure GammaBlur32(Bitmap: TBitmap32; Radius: TFloat);
var
  Dest: TBitmap32;
begin
  if (Assigned(GammaBlurInplace32Proc)) then
    GammaBlurInplace32Proc(Bitmap, Radius)
  else
  if (Assigned(GammaBlur32Proc)) then
  begin
    Dest := TBitmap32.Create(TMemoryBackend);
    try
      GammaBlur32Proc(Bitmap, Dest, Radius);
      TBitmap32Cracker(Dest).CopyMapTo(Bitmap);
    finally
      Dest.Free;
    end;
  end else
    raise Exception.Create('Missing GammaBlur32 implementation');
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure Blur32NotImplemented(Src, Dst: TBitmap32; Radius: TFloat);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

procedure BlurInplace32NotImplemented(Bitmap: TBitmap32; Radius: TFloat);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

procedure SelectiveGaussian32NotImplemented(Src, Dst: TBitmap32; Radius: TFloat; Delta: Integer);
begin
  raise Exception.Create('This blur function has not been implemented');
end;

//------------------------------------------------------------------------------

var
  FBlurRegistry: TFunctionRegistry;

procedure RegisterBindings;
begin
  FBlurRegistry.RegisterBinding(@@Blur32Proc);
  FBlurRegistry.RegisterBinding(@@BlurInplace32Proc);

  FBlurRegistry.RegisterBinding(@@GammaBlur32Proc);
  FBlurRegistry.RegisterBinding(@@GammaBlurInplace32Proc);

  FBlurRegistry.RegisterBinding(@@HorizontalBlur32);
  FBlurRegistry.RegisterBinding(@@GammaHorizontalBlur32);

  FBlurRegistry.RegisterBinding(@@SelectiveGaussianBlur32);

  // Default fallback stubs for unimplemented functions
  FBlurRegistry.Add(@@HorizontalBlur32,         @Blur32NotImplemented,          [isPascal], FBlurRegistry.WORST_PRIORITY);
  FBlurRegistry.Add(@@GammaHorizontalBlur32,    @BlurInplace32NotImplemented,   [isPascal], FBlurRegistry.WORST_PRIORITY);
  FBlurRegistry.Add(@@SelectiveGaussianBlur32,  @SelectiveGaussian32NotImplemented, [isPascal], FBlurRegistry.WORST_PRIORITY);
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
