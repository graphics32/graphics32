unit GR32.Blur.RecursiveGaussian;

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
 * The Original Code is Recursive Gaussian Blur for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-2018
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

// IIR = Infinite Input Response
{$define IIR_BLUR_DEFAULT}              // Register as default Blur32 implementation.
{$define IIR_BLUR_SIMD}                 // Enable forward/backward filter implemented using Streaming SIMD Extensions (SSE).
{$define IIR_BLUR_EDGE_CORRECTION}      // Apply the boundary corrections from [3]
{$define IIR_BLUR_EDGE_CORRECTION_SIMD} // Enable SIMD implementation of [3] - significantly faster than the Pascal version but unfortunately not a hotspot
{.$define IIR_BLUR_INKSCAPE_COEFFICIENTS}// Enable use of InkScape [7] coefficient calculation
{.$define IIR_BLUR_GABOR_COEFFICIENTS}  // Enable use of Gabor [2] coefficient calculation
{$define IIR_USE_HADDPS}                // Use the HADDPS SSE3 instruction instead of two ADDPS SSE instructions. See comment in code.
{$define IIR_USE_DPPS}                  // Use the DPPS SSE4.1 instruction instead of a MULPS and two ADDPS SSE instructions. See comment in code.
{$define IIR_BLUR_ALIGN_BUFFERS}        // Use aligned buffers (note only start of buffer is aligned; Individual rows are not)

{$define IIR_BLUR_DIV_LUT}              // Use a Look Up Table for alpha pre- and unpremultiplication and gamma correction. Slightly faster and much more precise.
                                        // Note that GammaBlur32 requires that IIR_BLUR_DIV_LUT is defined.

uses
  GR32;


//------------------------------------------------------------------------------
//
//      Recursive Gaussian Blur
//
//------------------------------------------------------------------------------
// Anders Melander, August 2010-September 2018
//------------------------------------------------------------------------------
// References:
//
// [1] Recursive implementation of the Gaussian filter
//     Ian T. Young, Lucas J. van Vliet
//     Signal Processing, Volume 44, Number 2, June 1995, pp. 139-151(13)
//
// [2] Recursive Gabor Filtering
//     Ian T. Young, Lucas J. van Vliet, and Michael van Ginkel
//     IEEE Transactions on Signal Processing, Volume 50, Number 11, Nov 2002, pp. 2799–2805
//
// [3] Boundary Conditions for Young - van Vliet Recursive Filtering
//     Bill Triggs, Michael Sdika
//     IEEE Transactions on Signal Processing, Volume 54, Number 6, June 2006, pp. 2365-2367
//
// [4] Recursive Gaussian Derivative Filters
//     L. J. Van Vliet, I. T. Young, and P. W. Verbeek
//     Fourteenth International Conference on Pattern Recognition, 1998. Brisbane.
//     Proceedings, Volume 1, pp. 509-514
//
// [5] Performance of three recursive algorithms for fast space-variant Gaussian filtering
//     Sovira Tan, Jason L. Dale, Alan Johnston
//     Real-Time Imaging, Number 9, 2003, pp 215–228
//
// [6] Gimp, Retinex plug-in
//     Fabien Pelisson
//
// [7] Inkscape, Gaussian blur renderer
//     Niko Kiirala, bulia byak, Jasper van de Gronde
//------------------------------------------------------------------------------
//
// From [1]:
//
//   This implementation yields an infinite impulse response filter that has six
//   MADDs (Multiply/Add) per dimension independent of the value of sigma in the
//   Gaussian kernel. In contrast to the Deriche implementation (1987), the
//   coefficients of our recursive filter have a simple, closed-form solution for a
//   desired value of the Gaussian sigma. Our implementation is, in general, faster
//   than:
//
//     (1) an implementation based upon direct convolution with samples of a Gaussian,
//     (2) repeated convolutions with a kernel such as the uniform filter, and
//     (3) an FFT implementation of a Gaussian filter.
//
//------------------------------------------------------------------------------

procedure RecursiveGaussianBlur(Src, Dst: TBitmap32; Sigma: TFloat);
procedure RecursiveGaussianBlurRadius(Src, Dst: TBitmap32; Radius: TFloat);
procedure RecursiveGaussianBlurGamma(Src, Dst: TBitmap32; Sigma: TFloat);
procedure RecursiveGaussianBlurRadiusGamma(Src, Dst: TBitmap32; Radius: TFloat);

procedure RecursiveGaussianBlurHorizontalRadius(Src, Dst: TBitmap32; Radius: TFloat);
procedure RecursiveGaussianBlurHorizontalRadiusGamma(Src, Dst: TBitmap32; Radius: TFloat);


//------------------------------------------------------------------------------
//
//      Recursive Gaussian Blur internal API types
//
//------------------------------------------------------------------------------
// For now this implementation only works when TFloat=Single since the SSE
// and transpose code has been written for 32-bit values.
//------------------------------------------------------------------------------
type
  TQuadFloatRec = packed record
    v0: TFloat;
    v1: TFloat;
    v2: TFloat;
    v3: TFloat;
  end;

  TQuadFloat = array[0..3] of TFloat;
  PQuadFloat = ^TQuadFloat;
  TNineFloats = array[0..8] of TFloat;


//------------------------------------------------------------------------------
//
//      Recursive Gaussian Blur internal bindings
//
//------------------------------------------------------------------------------
type
  TIIR_BlurFilterBackwardProc = procedure (pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; var v: TQuadFloat);
  TIIR_BlurFilterForwardProc = procedure (pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; var v: TQuadFloat);
  TIIR_BlurApplyEdgeCorrection = procedure (iPlus: PFloat; const B: TQuadFloat; var v: TQuadFloat; const M: TNineFloats);

var
  IIR_BlurFilterBackward: TIIR_BlurFilterBackwardProc = nil;
  IIR_BlurFilterForward: TIIR_BlurFilterForwardProc = nil;
  IIR_BlurApplyEdgeCorrection: TIIR_BlurApplyEdgeCorrection = nil;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SyncObjs, // TCriticalSection
  Math,
  GR32.Blur,
  GR32_Bindings,
  GR32.Transpose,
  GR32.Math.Complex,
  GR32_OrdinalMaps,
  GR32_Blend,
  GR32_LowLevel,
  GR32_Math,
  GR32_System,
  GR32_Gamma;

// Ensure that we use the GR32.TFloat and not FPC's Math.TFloat (which is an alias for Double!)
type
  TFloat = GR32.TFloat;
  PFloat = ^TFloat;

const
  OneOver255: TFloat = 1/255;

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
{$ifdef IIR_BLUR_DIV_LUT}
type
  PPremultiplyLUT = ^TPremultiplyLUT;

  TPremultiplyLUT = record
  strict private
    class constructor Create;
    class destructor Destroy;
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
  public
    Mul255Div: array[byte, byte] of byte;
    MulDiv255: array[byte, byte] of TFloat;
  public class var
  public
    class function PremultiplyLUT: PPremultiplyLUT; static;
    class function GammaPremultiplyLUT: PPremultiplyLUT; static;

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
        FPremultiplyLUT.MulDiv255[ColorValue, 0] := 0.0;

        for AlphaValue := 1 to 255 do
        begin
          FPremultiplyLUT.Mul255Div[ColorValue, AlphaValue] := Clamp(Round(ColorValue * 255 / AlphaValue));
          FPremultiplyLUT.MulDiv255[ColorValue, AlphaValue] := ColorValue * AlphaValue * OneOver255;;
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
    MulDiv255[ColorValue, 0] := 0.0;

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
      MulDiv255[ColorValue, AlphaValue] := n;

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


{$endif IIR_BLUR_DIV_LUT}

//------------------------------------------------------------------------------
//
//      Recursive Gaussian Blur
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Low level, SIMD implementations
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure BlurFilterForward_SSE41(pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; var v: TQuadFloat); {$IFDEF FPC}assembler;{$ENDIF}
  // Parameters (x86):
  //   EAX <- pIn
  //   EDX <- pOut
  //   ECX <- B
  //   Stack[0] <- Width
  //   Stack[1] <- v
  //   Preserves: (none)
  //
  // Parameters (x64):
  //   RCX <- pIn
  //   RDX <- pOut
  //   R8 <- B
  //   R9 <- Width
  //   Stack[0] <- v
  //   Preserves: (none)
  //
  // SSE register usage:
  //   XMM0: B3 | B2 | B1 | B0
  //   XMM1:  * | v1 | v2 | v3
  //   XMM2, XMM3: misc use
asm
{$if defined(TARGET_x64)}
{$IFNDEF FPC}
  // nothing
{$ELSE}
  // nothing
{$ENDIF}
{$elseif defined(TARGET_x86)}
  // nothing
{$else}
{$message fatal 'Unsupported target'}
{$ifend}
  MOVUPS        XMM0, TQuadFloatRec PTR [B]             // XMM0 <-  B3 | B2 | B1 | B0

  (*
  ** Initialization of forward pass
  **
  ** v[1] := pIn^ / (B[0] + B[1] + B[2] + B[3]);
  ** v[2] := v[1];
  ** v[3] := v[1];
  *)

  MOVAPS        XMM2, XMM0                              // XMM2 <- B3 | B2 | B1 | B0

  // XMM2[0] <- (B + B1 + B2 + B3)
  // TODO : HADDPS is slow according to this:
  //   https://stackoverflow.com/questions/6996764/fastest-way-to-do-horizontal-sse-vector-sum-or-other-reduction/35270026#35270026
  // However, my own benchmarks indicate that the alternative is slower...
{$ifdef IIR_USE_HADDPS}
  HADDPS        XMM2, XMM2                              // XMM2[0..3] <- XMM2[0] + XMM2[1] + XMM2[2] + XMM2[3]
  HADDPS        XMM2, XMM2
{$else IIR_USE_HADDPS}
  MOVHLPS       XMM3, XMM2                              // XMM3[0,1] <- XMM2[2,3]
  ADDPS         XMM2, XMM3                              // XMM2[0] <- XMM2[0] + XMM2[1] + XMM2[2] + XMM2[3]
  PSHUFD        XMM3, XMM2, $01
  ADDSS         XMM2, XMM3
{$endif IIR_USE_HADDPS}

  MOVSS         XMM1, [pIn]                             // XMM1[0] <- pIn^
  DIVSS         XMM1, XMM2                              // XMM1[0] <- XMM1[0] / XMM2[0]

  SHUFPS        XMM1, XMM1, $00                         // XMM1[0..3] <-  XMM1[0]

  // Iterate over all cols using the "negative offset technique"
{$if defined(TARGET_x86)}
  // We're now done with the value in the "B" register so it's repurposed for the "Width" counter.
  MOV           ECX, Width

  LEA           pIn, [pIn+ECX*4]
  LEA           pOut, [pOut+ECX*4]
  NEG           ECX
{$elseif defined(TARGET_x64)}
  LEA           pIn, [pIn+R9*4]
  LEA           pOut, [pOut+R9*4]
  NEG           R9
{$ifend}

@LOOP:

  (*
  **   v0 := (pIn^ * B0) + (v1 * B1) + (v2 * B2) + (v3 * B3);
  *)

{$if defined(TARGET_x86)}
  MOVSS         XMM2, [pIn+ECX*4]                       // XMM2[0] <- pIn
{$elseif defined(TARGET_x64)}
  MOVSS         XMM2, TFloat PTR [pIn+R9*4]             // XMM2[0] <- pIn
{$ifend}
  MOVSS         XMM1, XMM2                              // XMM1[0] <- XMM2[0]
  MOVAPS        XMM2, XMM1                              // XMM2 <- XMM1

  // TODO : DPPS might be slow on some CPUs:
  //   https://stackoverflow.com/questions/37879678/dot-product-performance-with-sse-instructions-is-dpps-worth-using
  // On my system it's significantly faster than the alternative
{$ifdef IIR_USE_DPPS}
  DPPS          XMM2, XMM0, $FF
{$else IIR_USE_DPPS}
  // Multiply B[] with v[]
  MULPS         XMM2, XMM0                              // XMM2 <- XMM2 * XMM0

  // Add the four results of the previous vectorized multiplication
  MOVHLPS       XMM3, XMM2                              // XMM2[0] <- XMM2[0] + XMM2[1] + XMM2[2] + XMM2[3]
  ADDPS         XMM2, XMM3
  PSHUFD        XMM3, XMM2, $01
  ADDSS         XMM2, XMM3
{$endif IIR_USE_DPPS}

  MOVSS         XMM1, XMM2                              // XMM1[0] <- v0

  // pOut^ := v0;
{$if defined(TARGET_x86)}
  MOVSS         [pOut+ECX*4], XMM2
{$elseif defined(TARGET_x64)}
  MOVSS         TFloat PTR [pOut+R9*4], XMM2
{$ifend}

  (*
  ** Shift cached output values
  ** v3 := v2;
  ** v2 := v1;
  ** v1 := v0;
  **)
  SHUFPS        XMM1, XMM1, $93                         // XMM1[0,1,2,3] <- XMM1[3,0,1,2]

  // Loop to next col
{$if defined(TARGET_x86)}
  INC           ECX
{$elseif defined(TARGET_x64)}
  INC           R9
{$ifend}
  JNZ           @LOOP

  // Update v1, v2, v3 for backward filter
{$if defined(TARGET_x86)}
  MOV           EDX, v
  MOVUPS        [EDX], XMM1
{$elseif defined(TARGET_x64)}
// TODO : For some reason this doesn't work ("v" isn't updated):
//  MOVUPS        [TQuadFloat PTR v], XMM1
  MOV           RDX, v
  MOVUPS        [RDX], XMM1
{$ifend}

{$if defined(TARGET_x64)}
{$IFDEF FPC}
  // nothing
{$ENDIF}
{$elseif defined(TARGET_x86)}
  // nothing
{$ifend}
end;

//------------------------------------------------------------------------------

procedure BlurFilterBackward_SSE41(pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; const v: TQuadFloat); {$IFDEF FPC}assembler;{$ENDIF}
  // Parameters (x86):
  //   EAX <- pIn
  //   EDX <- pOut
  //   ECX <- B
  //   Stack[0] <- Width
  //   Stack[1] <- v
  //   Preserves: (none)
  //
  // Parameters (x64):
  //   RCX <- pIn
  //   RDX <- pOut
  //   R8 <- B
  //   R9 <- Width
  //   Stack[0] <- v
  //   Preserves: (none)
  //
  // SSE register usage:
  //   XMM0: B3 | B2 | B1 | B0
  //   XMM1:  * | v1 | v2 | v3
  //   XMM2, XMM3: misc use
asm
  MOVUPS        XMM0, TQuadFloatRec PTR [B]             // XMM0 <-  B3 | B2 | B1 | B0

{$if defined(TARGET_x86)}
  MOV           ECX, v
  MOVUPS        XMM1, [ECX]                             // XMM1 <-  v3 | v2 | v1 | *
{$elseif defined(TARGET_x64)}
// TODO : For some reason this doesn't work ("v" isn't loaded):
//  MOVUPS        XMM1, [TQuadFloat PTR v]                // XMM1 <-  v3 | v2 | v1 | *
  MOV           R8, QWORD PTR v
  MOVUPS        XMM1, [R8]                              // XMM1 <-  v3 | v2 | v1 | *
{$ifend}

  // Iterate over all cols
  // Note: We're iterating backwards from the last to the first col.
  // On entry, pIn and pOut points to the last col. Here we adjust them to point to the first col
  // so we can use the col counter as an offset.
{$if defined(TARGET_x86)}
  // We're now done with the value in the "B" register so it's repurposed for the "Width" counter.
  MOV           ECX, Width

  // Although the compiler accepts [RegA-RegB*4] it assembles it to [RegA+RegB*4] so we need
  // to temporarily negate the offset value
  NEG ECX
  LEA           pIn, [pIn+ECX*4]
  LEA           pOut, [pOut+ECX*4]
  NEG ECX
{$elseif defined(TARGET_x64)}
  NEG R9
  LEA           pIn, [pIn+R9*4]
  LEA           pOut, [pOut+R9*4]
  NEG R9
{$ifend}

@LOOP:

  (*
  **   v0 := (B0 * pIn^) + (v1 * B1) + (v2 * B2) + (v3 * B3);
  *)

  // Multiply B[] with v[]
{$if defined(TARGET_x86)}
  INSERTPS      XMM1, [pIn+ECX*4], $00                  // XMM1[0] <- pIn
{$elseif defined(TARGET_x64)}
  INSERTPS      XMM1, TFloat PTR [pIn+R9*4], $00        // XMM1[0] <- pIn
{$ifend}
  MOVAPS        XMM2, XMM1                              // XMM2 <- XMM1

{$ifdef IIR_USE_DPPS}
  DPPS          XMM2, XMM0, $FF                         // XMM2 <- XMM2 dot XMM1
{$else IIR_USE_DPPS}
  MULPS         XMM2, XMM0                              // XMM2 <- XMM2 * XMM0

  // Add the four results of the previous vectorized multiplication
  MOVHLPS       XMM3, XMM2                              // XMM2[0] <- XMM2[0] + XMM2[1] + XMM2[2] + XMM2[3]
  ADDPS         XMM2, XMM3
  PSHUFD        XMM3, XMM2, $01
  ADDSS         XMM2, XMM3
{$endif IIR_USE_DPPS}
  MOVSS         XMM1, XMM2                              // XMM1[0] <- v0

  // pOut^ := v0;
{$if defined(TARGET_x86)}
  MOVSS         [pOut+ECX*4], XMM2
{$elseif defined(TARGET_x64)}
  MOVSS         TFloat PTR [pOut+R9*4], XMM2
{$ifend}

  (*
  ** Shift cached output values
  ** v3 := v2;
  ** v2 := v1;
  ** v1 := v0;
  **)
  SHUFPS        XMM1, XMM1, $93                         // XMM0 <-  v2 | v1 | v0 | v3

  // Loop to next (previous actually) col
{$if defined(TARGET_x86)}
  DEC           ECX
{$elseif defined(TARGET_x64)}
  DEC           R9
{$ifend}
  JNZ           @LOOP
end;

//------------------------------------------------------------------------------

procedure BlurApplyEdgeCorrection_SSE41(iPlus: PFloat; const B: TQuadFloat; var v: TQuadFloat; const M: TNineFloats); // {$IFDEF FPC}assembler;{$ENDIF}
  // Parameters (x86):
  //   EAX <- iPlus
  //   EDX <- B0
  //   ECX <- v
  //   Stack[0] <- M
  //   Preserves: (none)
  //
  // Parameters (x64):
  //   RCX <- iPlus
  //   RDX <- B0
  //   R8 <- v
  //   R9 <- M
  //   Preserves: XMM4
  //
  // SSE register usage:
  //   XMM0: B3 | B2 | B1 | B0
  //   XMM1:  * | v1 | v2 | v3
  //   XMM2, XMM3, XMM4: misc use
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x64)}
{$IFNDEF FPC}
  .SAVENV XMM4
{$ENDIF}
{$elseif defined(TARGET_x86)}
  // nothing
{$else}
{$message fatal 'Unsupported target'}
{$ifend}

  (*
  ** unp0 := v[1] - uPlus;
  ** unp1 := v[2] - uPlus;
  ** unp2 := v[3] - uPlus;
  *)
  // XMM1 = [v3 | v2 | v1 | 0]
  MOVUPS        XMM1, TQuadFloat PTR [v] // XMM1 <-  v3 | v2 | v1 | v0
  // Save v0
  MOVAPS        XMM5, XMM1
  // Now get rid of XMM1[0]
  SHUFPS        XMM1, XMM1, $39 // XMM1 <-  v0 | v3 | v2 | v1
  INSERTPS      XMM1, XMM1, $F8 // XMM1 <-   0 | v3 | v2 | v1

  // XMM0 = [0 | u+ | u+ | u+]
  MOVSS         XMM0, [iPlus]   // XMM0[0] <- u+
  SHUFPS        XMM0, XMM0, $00 // XMM0 <-  u+ | u+ | u+ | u+

  // XMM1 = XMM1*XMM0 = [0, unp2, unp1, unp0]
  SUBPS         XMM1, XMM0      // XMM1 <-  XMM0  - u+
                                // XMM1 = [0, unp2,unp1,unp0]
  INSERTPS      XMM1, XMM1, $F8 // XMM1[3] <- 0

  (*
  ** v[1] := ( (M0 * unp0) + (M1 * unp1) + (M2 * unp2) ) * B0 + vPlus;
  ** v[2] := ( (M3 * unp0) + (M4 * unp1) + (M5 * unp2) ) * B0 + vPlus;
  ** v[3] := ( (M6 * unp0) + (M7 * unp1) + (M8 * unp2) ) * B0 + vPlus;
  *)
  // XMM2 = [0 | M2 | M1 | M0]
  // XMM3 = [0 | M5 | M4 | M3]
  // XMM4 = [0 | M8 | M7 | M6]
{$if defined(TARGET_x86)}
  MOV           EAX, M
  MOVUPS        XMM2, [EAX]     // XMM1 <-   * | M2 | M1 | M0
{$elseif defined(TARGET_x64)}
  MOVUPS        XMM2, TQuadFloat PTR [M] // XMM1 <-   * | M2 | M1 | M0
{$ifend}
  INSERTPS      XMM2, XMM2, $F8 // XMM2 <-   0 | M2 | M1 | M0
{$if defined(TARGET_x86)}
  ADD           EAX, 12//SizeOf(TFloat)*3
  MOVUPS        XMM3, [EAX]     // XMM3 <-   * | M5 | M4 | M3
{$elseif defined(TARGET_x64)}
  ADD           M, 12//SizeOf(TFloat)*3
  MOVUPS        XMM3, TQuadFloat PTR [M] // XMM3 <-   * | M5 | M4 | M3
{$ifend}
  INSERTPS      XMM3, XMM3, $F8 // XMM3 <-   0 | M5 | M4 | M3
{$if defined(TARGET_x86)}
  ADD           EAX, 12//SizeOf(TFloat)*3
  MOVUPS        XMM4, [EAX]     // XMM4 <-   * | M8 | M7 | M6
{$elseif defined(TARGET_x64)}
  ADD           M, 12//SizeOf(TFloat)*3
  MOVUPS        XMM4, TQuadFloat PTR [M] // XMM4 <-   * | M8 | M7 | M6
{$ifend}
  INSERTPS      XMM4, XMM4, $F8 // XMM4 <-   0 | M8 | M7 | M6

  // XMM2[1] = [M2, M1, M0]*[unp2, unp1, unp0] = (M0 * unp0) + (M1 * unp1) + (M2 * unp2)
  // TODO : {$ifdef IIR_USE_DPPS}
  DPPS          XMM2, XMM1, $72 // XMM2[1] <-  [M2, M1, M0]*[unp2, unp1, unp0]

  // XMM3[2] = [M5, M4, M3]*[unp2, unp1, unp0] = (M3 * unp0) + (M4 * unp1) + (M5 * unp2)
  // TODO : {$ifdef IIR_USE_DPPS}
  DPPS          XMM3, XMM1, $74 // XMM3[2] <-  [M5, M4, M3]*[unp2, unp1, unp0]

  // XMM4[3] = [M8, M7, M6]*[unp2, unp1, unp0] = (M6 * unp0) + (M7 * unp1) + (M8 * unp2)
  // TODO : {$ifdef IIR_USE_DPPS}
  DPPS          XMM4, XMM1, $78 // XMM4[3] <-  [M8, M7, M6]*[unp2, unp1, unp0]

  // XMM2[1..3] = [ XMM2[1] | XMM3[2] | XMM4[3] ]
  INSERTPS      XMM2, XMM3, $A0 // XMM2[2] <-  XMM3[2]
  INSERTPS      XMM2, XMM4, $F0 // XMM2[3] <-  XMM4[3]

  // XMM1 = B[0]
{$IFNDEF FPC}
  MOVSS         XMM1, TFloat PTR [B] // XMM1[1] <- B0
{$ELSE}
{$if defined(TARGET_x64)}
  MOVSS         XMM1, TFloat PTR [RDX] // XMM1[1] <- B0
{$else}
  MOVSS         XMM1, TFloat PTR [EDX] // XMM1[1] <- B0
{$ifend}
{$ENDIF}
  SHUFPS        XMM1, XMM1, 0   // XMM1 <-  B0 | B0 | B0 | B0

  // XMM2 = XMM2 * B0
  MULPS         XMM2, XMM1      // XMM2 <-  XMM2 * B0

  // XMM2 = XMM2 + v+
  ADDPS         XMM2, XMM0      // XMM2 <-  XMM2 + v+

  INSERTPS      XMM2, XMM5, $00 // XMM2[0] <-  XMM5[0]

  MOVUPS        TQuadFloat PTR [v], XMM2     // XMM2 -> v

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4'];{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------
// Low level, Pascal implementations
//------------------------------------------------------------------------------
procedure BlurFilterForward_Pas(pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; var vv: TQuadFloat); inline;
var
  i: integer;
  v: TQuadFloat;
begin
  // Initialization of forward pass
  // [2] Equation (20)
  //
  //                            in[1]
  // w[1] = w[2] = w[3] = ──────────────────
  //                      1+ fd1 + fd2 + fd3
  //
  v[1] := pIn[0] / (B[0] + B[1] + B[2] + B[3]);
  v[2] := v[1];
  v[3] := v[1];

  // Forward pass
  // [1] Equation (9a)
  //
  //                    B[1]*v[n-1] + B[2]*v[n-2] + B[3]*v[n-3]           ∑ B[1..3]
  // w[n] = B * in[n] + ─────────────────────────────────────── , B = 1 - ───────── ([1] equation 10)
  //                                      B[0]                               B[0]
  //
  for i := 0 to Width-1 do
  begin
    // Note: In this implementation B[1..3] has already been scaled by 1/b0 and B[0] instead contains B.
    v[0] := (pIn[i] * B[0]) + (v[1] * B[1]) + (v[2] * B[2]) + (v[3] * B[3]);
    pOut[i] := v[0];

    // Shift cached output values
    v[3] := v[2];
    v[2] := v[1];
    v[1] := v[0];
  end;
  vv := v;
end;

//------------------------------------------------------------------------------

procedure BlurFilterBackward_Pas(pIn, pOut: PFloatArray; const B: TQuadFloat; Width: Cardinal; var v: TQuadFloat); inline;
var
  i: integer;
begin
  Dec(PFloat(pIn), Width-1);
  Dec(PFloat(pOut), Width-1);

  // Backward pass
  // [1] Equation (9b)
  //
  //                     B[1]*out[n+1] + B[2]*out[n+2] + B[3]*out[n+3]           ∑ B[1..3]
  // out[n] = B * w[n] + ───────────────────────────────────────────── , B = 1 - ─────────  ([1] equation 10)
  //                                      B[0]                                      B[0]
  //
  for i := Width-1 downto 0 do // Using negative array index works too but will give the purists a heart attack
  begin
    v[0] := (B[0] * pIn[i]) + (v[1] * B[1]) + (v[2] * B[2]) + (v[3] * B[3]);
    pOut[i] := v[0];

    // Shift cached output values
    v[3] := v[2];
    v[2] := v[1];
    v[1] := v[0];
  end;
end;

//------------------------------------------------------------------------------

procedure BlurApplyEdgeCorrection_Pas(iPlus: PFloat; const B: TQuadFloat; var v: TQuadFloat; const M: TNineFloats);
var
  uPlus, vPlus: TFloat;
  unp0, unp1, unp2: TFloat;
begin
  // [3] Equation (15)
  // iPlus = infinite stream of input values = value of rightmost pixel in our case

  // [3] Equation (15a)
  //
  //            i+          i+
  // u+ = ────────────── = ────
  //       1 - ∑ b1..b3     B
  //
  // Note: B = B[0] = 1-(B[1]+B[2]+B[3]),   [1] Equation (10)
  //       See calculation of B[0] in ComputeCoefficients*
  uPlus := iPlus^; // We have already scaled by B

  // [3] Equation (15b)
  //
  //            u+          u+
  // v+ = ────────────── = ────
  //       1 - ∑ b1..b3     B
  //
  vPlus := uPlus; // We have already scaled by B

  // [3] Equation (14)
  //
  //             ┌         ┐   ┌    ┐
  //             │v[2] - u+│   │ v+ │
  // v[] = M[] * │v[1] - u+│ + │ v+ │
  //             │v[0] - u+│   │ v+ │
  //             └         ┘   └    ┘
  //
  unp0 := v[1] - uPlus;
  unp1 := v[2] - uPlus;
  unp2 := v[3] - uPlus;

  v[1] := ( (M[0] * unp0) + (M[1] * unp1) + (M[2] * unp2) ) * B[0] + vPlus;
  v[2] := ( (M[3] * unp0) + (M[4] * unp1) + (M[5] * unp2) ) * B[0] + vPlus;
  v[3] := ( (M[6] * unp0) + (M[7] * unp1) + (M[8] * unp2) ) * B[0] + vPlus;
end;


//------------------------------------------------------------------------------
// Coefficient calculation
//------------------------------------------------------------------------------

// Uses algorithm from Inkscape to calculate coefficients (Adapted from [7]. Based on [4]).
// (Iterative and supposedly even better approximation than [2]).
// Note: My (anme) own tests shows that [2] produces a much more precise approximation
// of the gaussian curve.
procedure ComputeCoefficientsInkscape(Sigma: TFloat; var B: TQuadFloat);
var
  d1_org: TComplex;
  d3_org: Double;
  qLow, qHigh: Double;
  SigmaSqr: Double;
  s, q: Double;
  d1: TComplex;
  d3: Double;
  sSqr: Double;
  AbsD1Sqr: Double;
  re2d1: Double;
  B0: Double;
  Limit: Double;
begin
(*
  The use of ComputeCoefficientsInkscape is currently disabled by default because:

    - ComputeCoefficientsInkscape is only valid for Sigma >= 20 and the transition
      between the default coefficient and ComputeCoefficientsInkscape isn't smooth.

*)

  // [4] Table 1, 3rd order filter
  d1_org := TComplex.From(1.40098,  1.00236);
  d3_org := 1.85132;

  SigmaSqr := Sqr(Sigma);

  // [4] suggests using "a few iterations of a linear extrapolation scheme" to find
  // the value of q that yields a filter h[n] with the specified variance.
  // Here we use a binary search instead.
  qLow := 1; // Don't go lower than sigma=2 (we'd probably want a normal convolution in that case anyway)
  qHigh := 2 * Sigma;
{$if (SizeOf(TFloat) > SizeOf(Single))}
    Limit := Sigma / (1 shl 30)
{$else}
    Limit := Sigma / (1 shl 20);
{$ifend}
  repeat
    q := (qLow + qHigh) / 2;
    // Compute scaled filter coefficients
    // [4] Equation (20)
    //                    1     jθ(d)         1      jθ(d)
    //                   ───    ─────        ───     ─────  -2
    // var(h[n]) = ∑ 2|d| q  * e  q   * ( |d| q   - e  q   )
    //
    d1 := TComplex.Power(d1_org, 1 / q);
    d3 := Power(d3_org, 1 / q);

    // Compute actual sigma^2
    // [4] Equation (10)
    //
    //                  2d
    // ∑ n²*h[n] = ∑ ────────
    //               (d - 1)²
    //
    sSqr := 2 * (2 * (d1 / TComplex.Sqr(d1 - 1)).Real + d3 / Sqr(d3 - 1));

    if (sSqr < SigmaSqr) then
      qLow := q
    else
      qHigh := q;

    s := qHigh - qLow;
  until (s <= Limit);
  q := (qLow + qHigh) / 2;

  // Compute z-poles
  // [4] Equation (20)
  d1 := TComplex.Power(d1_org, 1 / q);
  d3 := Power(d3_org, 1 / q);
  AbsD1Sqr := TComplex.AbsSqr(d1); // d1*d2 = d1*conj(d1) = |d1|^2 = std::norm(d1)
  re2d1 := 2 * d1.Real; // d1+d2 = d1+conj(d1) = 2*real(d1)

  // Compute filter coefficients
  // [4] Equation (13)

  // Note: For performance b1..b3 are divided b0 to avoid doing so in the inner loop.
  //       Additionally B is stored in B[0] since we no longer need b0.
  B0   := 1 / (AbsD1Sqr * d3);
  B[1] := (AbsD1Sqr + d3 * re2d1) * B0;
  B[2] := -(d3 + re2d1) * B0;
  B[3] :=  B0;

  // B: Normalization constant
  // [1] Equation (10)
  B[0] := 1.0 - (B[1] + B[2] + B[3]);
end;

//------------------------------------------------------------------------------
// Uses algorithm from [2] to calculate coefficients (more precise approximation).
// Not stable for sigma >= 20 (Radius >= 50) - produces "crazy" results.
(*
  The use of ComputeCoefficientsGabor is currently disabled by default because:

    a) ComputeCoefficientsGaussianDerivative produces almost the same values for
       Radius=1 as ComputeCoefficientsGabor does for Radius=2 which makes no
       sense to the user.

    b) ComputeCoefficientsGabor doesn't support Sigma < 0.5 so we can't just use
       it instead of ComputeCoefficientsGaussianDerivative.

  * Radius: 1; Sigma: 0.300386630413846
    ComputeCoefficientsGaussianDerivative
    B[0.840293169021606, 0.170402005314827, -0.0110350390896201, 0.00033983588218689]

  * Radius: 2; Sigma: 0.300386630413846) 0.600773260827692
    ComputeCoefficientsGabor
    B[0.845094859600067, 0.164933204650879, -0.0103367269039154, 0.000308647722704336]
*)
procedure ComputeCoefficientsGabor(Sigma: TFloat; var B: TQuadFloat);
var
  q, qq, qqq: Double;
  B0: Double;
const
  // [2] Equation (7)
  m0 = 1.16680;
  m1 = 1.10783;
  m2 = 1.40586;
  m1m1 = m1*m1;
  m2m2 = m2*m2;
begin
  // Compute q from Standard Deviation σ
  // [2] Equation (16)
  //
  //     ┌
  //     │  0.9804(σ -3.556) + 2.5091     for σ >= 3.556
  // q = ┤
  //     │  0.0561σ² +0.5784σ - 0.2568    for 0.5 <= σ <= 3.556
  //     └
  // Note: Doesn't work well for values smaller than 0.5
  if (Sigma < 3.556) then
    q := (0.0561 * Sigma + 0.5784) * Sigma - 0.2568 // = 0.0561*Sqr(Sigma) + 0.5784*Sigma - 0.2568
  else
    q := 0.9804 * (Sigma - 3.556) + 2.5091;

  qq := Sqr(q);
  qqq := qq * q;

  // Compute filter coefficients
  // [2] Equation (15)
  //
  // b0 = 1 [*] scale = (m0 + q)(m1² + m2² + 2m1q + q²)           [*] Something is missing here in the original paper
  // b1 = -q(2m0m1 + m1² + m2² + (2m0 + 4m1)q + 3q²) / scale
  // b2 = q²(m0 + 2m1 + 3q) / scale
  // b3 = -q³ / scale
  //
  // Note: We have reversed the sign of b1, b2 & b3 from [2]
  // Note: For performance we scale B[1..3] by 1/b0 to avoid doing so in the inner loop.
  //       Additionally we store B in B[0].

  B0   := 1 / ((m0 + q) * (m1m1 + m2m2 + 2 * m1 * q + qq));
  B[1] := (q * (2 * m0 * m1 + m1m1 + m2m2 + (2 * m0 + 4 * m1) * q + 3 * qq)) * B0;
  B[2] := (-qq * (m0 + 2 * m1 + 3 * q)) * B0;
  B[3] := (qqq) * B0;

  // [2] Equation (19)
  //
  // B = (m0(m1² + m2²)/scale)²
  //
  B[0] := (m0 * (m1m1 + m2m2)) * B0;
end;

//------------------------------------------------------------------------------
// Uses algorithm from [4] to calculate coefficients
procedure ComputeCoefficientsGaussianDerivative(Sigma: TFloat; var B: TQuadFloat);
var
  q, qq, qqq: Double;
  B0: Double;
//  Sigma2, Sigma3, Sigma4: Double;
begin
  // Compute q from Standard Deviation
(*
  if (Sigma <= 4) and (Sigma >= 1) then
  begin
    // [5] Equation (22)
    // Valid for the range σ = [1,4]
    Sigma2 := Sqr(Sigma);
    Sigma3 := Sigma2 * Sigma;
    Sigma4 := Sigma3 * Sigma;
    q := 0.0001 * Sigma4 - 0.0021 * Sigma3 + 0.0207 * Sigma2 + 0.3797 * Sigma + 0.1763;
  end else
  // Well, this sucks: The transition between the case above (Radius=13, Sigma=3.9) and the one
  // below (Radius=14, Sigma=4.2) isn't smooth; There's a visible "jump" in the amount of blur
  // when we transition between them.
*)
  if (Sigma >= 2.5) then
    // [1] Equation (11b)
    q := 0.98711 * Sigma - 0.96330
  else
  if (Sigma >= 0.5) then
    // [1] Equation (11b)
    q := 3.97156 - 4.14554 * Sqrt(1.0 - 0.26891 * Sigma)
  else
    // [6]
    q := 0.1147705018520355224609375;

  qq := Sqr(q);
  qqq := qq * q;

  // Compute filter coefficients
  // [1] Equation (8c)
  B0   :=  1 / (1.57825 + (2.44413 * q) + (1.4281  * qq) + (0.422205 * qqq));
  B[1] := (               (2.44413 * q) + (2.85619 * qq) + (1.26661  * qqq))  * B0;
  B[2] := (-(                             (1.4281  * qq) + (1.26661  * qqq))) * B0;
  B[3] := (                                                 0.422205 * qqq)   * B0;

  // B: Normalization constant
  // [1] Equation (10)
  B[0] := 1.0 - (B[1] + B[2] + B[3]);
end;


//------------------------------------------------------------------------------
// Edge correction matrix [3]
//------------------------------------------------------------------------------
procedure ComputeBoundaryCorrectionMatrix(const B: TQuadFloat; var M: TNineFloats);
var
  Scale: Double;
  B3B3, B2B2: Double;
  B3B1, B3B2: Double;
begin
  // Compute edge correction matrix M[3,3]
  // [3] Equation (5)
  //
  //
  //                           1
  // Scale = ────────────────────────────────────────
  //         (1+a1−a2+a3)(1−a1−a2−a3)(1+a2+(a1−a3)a3)
  //
  //             ┌                                                                   ┐
  //             │ −a3a1+1−a3²-a2       (a3+a1)(a2+a3a1)              a3(a1+a3a2)    │
  // M = Scale * │     a1+a3a2          −(a2−1)(a2+a3a1)          −(a3a1+a3²+a2−1)a3 │
  //             │ a3a1+a2+a1²−a2²  a1a2+a3a2²−a1a3²−a3³−a3a2+a3      a3(a1+a3a2)    │
  //             └                                                                   ┘
  //
  Scale := 1 / ((1 + B[1] - B[2] + B[3]) * (1 - B[1] - B[2] - B[3]) * (1 + B[2] + (B[1] - B[3]) * B[3]));

  B3B3 := Sqr(B[3]);
  B2B2 := Sqr(B[2]);
  B3B1 := B[3] * B[1];
  B3B2 := B[3] * B[2];

  M[0] :=  Scale * (-B3B1 + 1 - B3B3 - B[2]);                                          // [0,0]
  M[1] :=  Scale * (B[3] + B[1]) * (B[2] + B3B1);                                      // [0,1]
  M[3] :=  Scale * (B[1] + B3B2);                                                      // [1,0]
  M[2] :=  M[3] * B[3];                                                                // [0,2]
  M[4] := -Scale * (B[2] - 1) * (B[2] + B3B1);                                         // [1,1]
  M[5] := -Scale * B[3] * (B3B1 + B3B3 + B[2] - 1);                                    // [1,2]
  M[6] :=  Scale * (B3B1 + B[2] + Sqr(B[1]) - B2B2);                                   // [2,0]
  M[7] :=  Scale * (B[1] * B[2] + B[3] * B2B2 - B3B3 * (B[1]+B[3]) - B3B2 + B[3]);     // [2,1]
  M[8] :=  Scale * B[3] * (B[1] + B3B2);                                               // [2,2]
end;


//------------------------------------------------------------------------------
// Single channel buffer I/O
//------------------------------------------------------------------------------
// Note: Although the layout of the Graphics32 TColor32Entry type depends on the
// RGBA_FORMAT define, the position of the Alpha channel in the record is
// constant which is what is important for us here.
//------------------------------------------------------------------------------

procedure LoadChannel(Src: TBitmap32; Channel: integer; Buffer: PFloatArray
  {$ifdef IIR_BLUR_DIV_LUT}; const PremultiplyLUT: TPremultiplyLUT{$endif});
var
  DestValues: PFloatArray;
  SourceValues: PColor32EntryArray;
  AlphaValues: PColor32EntryArray;
  Count: integer;
  i: integer;
  Alpha: Byte;
begin
  Count := Src.Height * Src.Width;
  DestValues := Buffer;
  SourceValues := PColor32EntryArray(@(PByte(Src.Bits)[Channel]));

  if (Channel = Ord(ccAlpha)) then
  begin
    for i := 0 to Count-1 do
      DestValues[i] := SourceValues[i].Planes[0];
  end else
  begin
    AlphaValues := PColor32EntryArray(@(PColor32Entry(Src.Bits).A));

    for i := 0 to Count-1 do
    begin
      Alpha := AlphaValues[i].Planes[0];

      // Important:
      // 1) First convert to linear color space.
      // 2) Then alpha premultiply.

      if (Alpha = 255) then // Most common case first
{$ifdef IIR_BLUR_DIV_LUT}

        DestValues[i] := PremultiplyLUT.MulDiv255[SourceValues[i].Planes[0], 255]

{$else IIR_BLUR_DIV_LUT}

        DestValues[i] := GAMMA_DECODING_TABLE[SourceValues[i].Planes[0]]

{$endif IIR_BLUR_DIV_LUT}
      else
      if (Alpha = 0) then
        DestValues[i] := 0
      else
        // Premultiply: ColorPremult := Color * Alpha / 255
{$ifdef IIR_BLUR_DIV_LUT}

        DestValues[i] := PremultiplyLUT.MulDiv255[SourceValues[i].Planes[0], Alpha];

{$else IIR_BLUR_DIV_LUT}

        DestValues[i] := GAMMA_DECODING_TABLE[SourceValues[i].Planes[0]] * Alpha * OneOver255;

{$endif IIR_BLUR_DIV_LUT}
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure SaveChannel(Dst: TBitmap32; Channel: integer; Buffer: PFloatArray
  {$ifdef IIR_BLUR_DIV_LUT}; const PremultiplyLUT: TPremultiplyLUT{$endif});
var
  SourceValues: PFloatArray;
  DestValues: PColor32EntryArray;
  AlphaValues: PColor32EntryArray;
  Count: integer;
  i: integer;
  n: TFloat;
  Color, Alpha: Byte;
begin
  Count := Dst.Height * Dst.Width;
  SourceValues := Buffer;
  DestValues := PColor32EntryArray(@(PByte(Dst.Bits)[Channel]));

  if (Channel = Ord(ccAlpha)) then
  begin
    for i := 0 to Count-1 do
    begin
      Alpha := Clamp(FastRound(SourceValues[i]));
      DestValues[i].Planes[0] := Alpha;
    end;
  end else
  begin
    AlphaValues := PColor32EntryArray(@(PColor32Entry(Dst.Bits).A));

    for i := 0 to Count-1 do
    begin
      Alpha := AlphaValues[i].Planes[0];

      // Important:
      // 1) First alpha unpremultiply.
      // 2) Then convert to gamma/sRGB color space.

      if (Alpha = 255) then // Most common case first
      begin
{$ifdef IIR_BLUR_DIV_LUT}

        n := SourceValues[i];
        Color := Clamp(FastRound(n));
        Color := PremultiplyLUT.Mul255Div[Color, 255];

{$else IIR_BLUR_DIV_LUT}

        Color := FastRound(SourceValues[i]);
        Color := GAMMA_ENCODING_TABLE[Color];

{$endif IIR_BLUR_DIV_LUT}
      end else
      if (Alpha = 0) then
        Color := 0
      else
      begin
        // Unpremultiply: Color := ColorPremult * 255 / AlphaValues
{$ifdef IIR_BLUR_DIV_LUT}

        n := SourceValues[i];
        Color := Clamp(FastRound(n)); // Warning: Loss of precision due to rounding before division
        Color := PremultiplyLUT.Mul255Div[Color, Alpha];

{$else IIR_BLUR_DIV_LUT}

        // Note: We're doing a double Round() here because SourceValues[i]/Alpha can be greater than 1
        // since we're dividing an unrounded value by a rounded value.
        // For example if both values are equal (e.g. source was $FFFFFFFF) and the 'alpha' value was rounded
        // down, then we will be dividing a larger value (e.g. 25.4) with a smaller (e.g. 25).
        // To avoid this rounding error we would have to keep a copy of the unrounded alpha values and
        // divide with those.
        Color := FastRound(FastRound(SourceValues[i]) / Alpha * 255 );
        Color := GAMMA_ENCODING_TABLE[Color];

{$endif IIR_BLUR_DIV_LUT}
      end;

      DestValues[i].Planes[0] := Color;
    end;
  end;
end;


//------------------------------------------------------------------------------
// Buffer management
//------------------------------------------------------------------------------
function AllocateChannelBuffer(out Buffer: pointer; Size: integer): PFloatArray;
const
  Alignment: NativeUInt = 64*1024;
begin
{$ifdef IIR_BLUR_ALIGN_BUFFERS}
  // Allocate a cache-friendly channel buffer
  // TODO : Although this gives us a small performance improvement it is most likely not worth the effort

  Inc(Size, 2 * Alignment);

  GetMem(Buffer, Size);
  Result := Buffer;

  Inc(PByte(Result), Alignment);

  // Ensure data alignment by X bytes
  Result := Pointer(NativeUInt(Result) and (NativeUInt(-1) xor (Alignment - 1)));

  // Make sure data is not aligned by 2*A bytes
  NativeUInt(Result) := NativeUInt(Result) or Alignment;
{$else IIR_BLUR_ALIGN_BUFFERS}
  GetMem(Buffer, Size);
  Result := Buffer;
{$endif IIR_BLUR_ALIGN_BUFFERS}
end;


//------------------------------------------------------------------------------
// Core function
//------------------------------------------------------------------------------
procedure InternalRecursiveGaussianBlur(Src, Dst: TBitmap32; Sigma: TFloat; TwoDimensional: boolean
  {$ifdef IIR_BLUR_DIV_LUT}; const PremultiplyLUT: TPremultiplyLUT{$endif});
var
  B: TQuadFloat;
  M: TNineFloats; // [3] Fix boundary
  Buffer, TransposedBuffer: PFloatArray;
  RowBuffer: PFloatArray;

  procedure BlurRow(Row: integer; Width, Height: integer; InBuffer, OutBuffer: PFloatArray);
  var
    InOffset, OutOffset: integer;
    pIn, pOut: PFloatArray;
    v: TQuadFloat;
  begin
    (*
    ** Causal filter (forward)
    *)
    InOffset := Row * Width;
    pIn := PFloatArray(@InBuffer[InOffset]);
    pOut := RowBuffer;

    IIR_BlurFilterForward(pIn, pOut, B, Width, v);

    (*
    ** Anti-causal filter (backward)
    *)
    OutOffset := InOffset + Width-1;

    pIn := @RowBuffer[Width-1];
    pOut := @OutBuffer[OutOffset];

{$ifdef IIR_BLUR_EDGE_CORRECTION}

    (*
    ** [3] Apply Triggs/Sdika edge correction
    *)
    begin
      IIR_BlurApplyEdgeCorrection(@InBuffer[InOffset + Width-1], B, v, M);

      pOut[0] := v[1];

      Dec(PFloat(pOut));
      Dec(PFloat(pIn));

      Dec(Width); // Adjust backward loop count
    end;

{$else IIR_BLUR_EDGE_CORRECTION}

    (*
    ** Regular initialization of backward pass. Not used with [3]
    *)
    // [2] Equation (21)
    v[1] := pIn[0] / (B[0] + B[1] + B[2] + B[3]);
    v[2] := v[1];
    v[3] := v[1];

{$endif IIR_BLUR_EDGE_CORRECTION}

    IIR_BlurFilterBackward(pIn, pOut, B, Width, v);
  end;

  procedure BlurBuffer;
  var
    i: integer;
  begin
    (*
    ** A note about transpose:
    ** An earlier version performed the transpose operation between the horizontal and vertical pass, as part of the
    ** backward (anti-causal) row filter.
    ** Tests has shown that this is at least 25% slower than performing the transpose in one go, after the complete
    ** horizontal and vertical passes, using a blocked transpose algorithm.
    *)

    for i := 0 to Src.Height-1 do
      BlurRow(i, Src.Width, Src.Height, Buffer, Buffer);

    Transpose32(@Buffer[0], @TransposedBuffer[0], Src.Width, Src.Height);

    for i := 0 to Src.Width-1 do
      BlurRow(i, Src.Height, Src.Width, TransposedBuffer, TransposedBuffer);

    Transpose32(@TransposedBuffer[0], @Buffer[0], Src.Height, Src.Width);
  end;

  procedure BlurHorizontal;
  var
    i: integer;
  begin
    for i := 0 to Src.Height-1 do
      BlurRow(i, Src.Width, Src.Height, Buffer, Buffer);
  end;

var
  Channel: integer;
  MaxRows: integer;
  UnAlignRowBuffer: pointer;
  UnAlignBuffer: pointer;
  UnAlignTransposedBuffer: pointer;
begin
  if (Sigma < GaussianRadiusToSigma) then
  begin
    Src.CopyMapTo(Dst);
    exit;
  end;

  Dst.SetSize(Src.Width, Src.Height);

{$ifdef IIR_BLUR_INKSCAPE_COEFFICIENTS}
  if (Sigma >= 20) then
    ComputeCoefficientsInkscape(Sigma, B)
  else
{$endif IIR_BLUR_INKSCAPE_COEFFICIENTS}
{$ifdef IIR_BLUR_GABOR_COEFFICIENTS}
  if (Sigma >= 0.5) then
    ComputeCoefficientsGabor(Sigma, B)
  else
{$endif IIR_BLUR_GABOR_COEFFICIENTS}
    ComputeCoefficientsGaussianDerivative(Sigma, B);

  // [3] Fix boundary
  ComputeBoundaryCorrectionMatrix(B, M);

  MaxRows := Max(Src.Height, Src.Width);

  UnAlignBuffer := nil;
  UnAlignTransposedBuffer := nil;
  UnAlignRowBuffer := nil;
  try
    Buffer := AllocateChannelBuffer(UnAlignBuffer, Src.Width * Src.Height * SizeOf(TFloat));
    if (TwoDimensional) then
      TransposedBuffer := AllocateChannelBuffer(UnAlignTransposedBuffer, Src.Width * Src.Height * SizeOf(TFloat));
    RowBuffer := AllocateChannelBuffer(UnAlignRowBuffer, MaxRows * SizeOf(TFloat));

    // Start with A channel so unpremult of RGB channels uses correct value
    for Channel := 3 downto 0 do
    begin
      LoadChannel(Src, Channel, Buffer
        {$ifdef IIR_BLUR_DIV_LUT}, PremultiplyLUT{$endif});

      if (TwoDimensional) then
        BlurBuffer
      else
        BlurHorizontal;

      SaveChannel(Dst, Channel, Buffer
        {$ifdef IIR_BLUR_DIV_LUT}, PremultiplyLUT{$endif});
    end;
  finally
    Freemem(UnAlignRowBuffer);
    Freemem(UnAlignTransposedBuffer);
    Freemem(UnAlignBuffer);
  end;
end;

//------------------------------------------------------------------------------

procedure RecursiveGaussianBlur(Src, Dst: TBitmap32; Sigma: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Sigma, True
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.PremultiplyLUT^{$endif});
end;

procedure RecursiveGaussianBlurRadius(Src, Dst: TBitmap32; Radius: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Radius * GaussianRadiusToSigma, True
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.PremultiplyLUT^{$endif});
end;

procedure RecursiveGaussianBlurGamma(Src, Dst: TBitmap32; Sigma: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Sigma, True
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.GammaPremultiplyLUT^{$endif});
end;

procedure RecursiveGaussianBlurRadiusGamma(Src, Dst: TBitmap32; Radius: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Radius * GaussianRadiusToSigma, True
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.GammaPremultiplyLUT^{$endif});
end;

//------------------------------------------------------------------------------

procedure RecursiveGaussianBlurHorizontalRadius(Src, Dst: TBitmap32; Radius: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Radius * GaussianRadiusToSigma, False
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.PremultiplyLUT^{$endif});
end;

procedure RecursiveGaussianBlurHorizontalRadiusGamma(Src, Dst: TBitmap32; Radius: TFloat);
begin
  InternalRecursiveGaussianBlur(Src, Dst, Radius * GaussianRadiusToSigma, False
    {$ifdef IIR_BLUR_DIV_LUT}, TPremultiplyLUT.GammaPremultiplyLUT^{$endif});
end;


//------------------------------------------------------------------------------
//
//      Registration of internal bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  BlurRegistry.RegisterBinding(@@IIR_BlurFilterForward, 'IIR_BlurFilterForward');
  BlurRegistry.RegisterBinding(@@IIR_BlurFilterBackward, 'IIR_BlurFilterBackward');
  BlurRegistry.RegisterBinding(@@IIR_BlurApplyEdgeCorrection, 'IIR_BlurApplyEdgeCorrection');

{$ifdef IIR_BLUR_DEFAULT}
  // Register as default Blur32 implementation
  BlurRegistry[@@Blur32Proc].Add(               @RecursiveGaussianBlurRadius,         [isPascal]).Name := 'RecursiveGaussianBlurRadius';
  BlurRegistry[@@GammaBlur32Proc].Add(          @RecursiveGaussianBlurRadiusGamma,    [isPascal]).Name := 'RecursiveGaussianBlurRadiusGamma';
{$endif IIR_BLUR_DEFAULT}

  BlurRegistry[@@HorizontalBlur32].Add(         @RecursiveGaussianBlurHorizontalRadius, [isPascal]).Name := 'RecursiveGaussianBlurHorizontalRadius';
  BlurRegistry[@@GammaHorizontalBlur32].Add(    @RecursiveGaussianBlurHorizontalRadiusGamma, [isPascal]).Name := 'RecursiveGaussianBlurHorizontalRadiusGamma';

  BlurRegistry[@@IIR_BlurFilterForward].Add(    @BlurFilterForward_Pas,               [isPascal]).Name := 'BlurFilterForward_Pas';
  BlurRegistry[@@IIR_BlurFilterBackward].Add(   @BlurFilterBackward_Pas,              [isPascal]).Name := 'BlurFilterBackward_Pas';
  BlurRegistry[@@IIR_BlurApplyEdgeCorrection].Add(@BlurApplyEdgeCorrection_Pas,         [isPascal]).Name := 'BlurApplyEdgeCorrection_Pas';

{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
{$ifdef IIR_BLUR_SIMD}
  BlurRegistry[@@IIR_BlurFilterForward].Add(    @BlurFilterForward_SSE41,
  {$if defined(IIR_USE_DPPS)}
                                                                                        [isSSE41]).Name := 'BlurFilterForward_SSE41';
  {$elseif defined(IIR_USE_HADDPS)}
                                                                                        [isSSE3]).Name := 'BlurFilterForward_SSE41';
  {$else}
                                                                                        [isSSE2]).Name := 'BlurFilterForward_SSE41';
  {$ifend}

  BlurRegistry[@@IIR_BlurFilterBackward].Add(   @BlurFilterBackward_SSE41,            [isSSE41]).Name := 'BlurFilterBackward_SSE41';

{$endif IIR_BLUR_SIMD}

{$ifdef IIR_BLUR_EDGE_CORRECTION_SIMD}
  BlurRegistry[@@IIR_BlurApplyEdgeCorrection].Add(@BlurApplyEdgeCorrection_SSE41,       [isSSE41]).Name := 'BlurApplyEdgeCorrection_SSE41';
{$endif IIR_BLUR_EDGE_CORRECTION_SIMD}

  BlendRegistry.RebindAll;

{$ifend}
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  RegisterBindings;
end.
