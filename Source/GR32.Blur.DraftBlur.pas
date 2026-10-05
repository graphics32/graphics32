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
  GR32_Bindings,
  GR32.Types.SIMD;

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

{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}

//------------------------------------------------------------------------------
// SIMD 4-channel 32-bit row blur using Alvarez & Mazorra 2nd-order IIR
// Vectorized across all 4 channels (B, G, R, A) simultaneously in 128-bit XMM
// registers using SSE4.1 32-bit integer SIMD arithmetic.
//
// QScale division is converted into an exact arithmetic right shift (PSRAD)
// using ShiftCount = BSR(QScale) (8 for QScale=256, 10 for 1024, 12 for 4096).
// Numeric overflow is prevented because intermediate products (max 255*256*iK
// ~ 2.67e8 or v1*i2Q ~ 5.34e8) easily fit within 32-bit signed integers
// (max 2.14e9). Final byte packing uses saturated conversion (PACKUSDW +
// PACKUSWB).
//------------------------------------------------------------------------------
procedure DraftBlurRow32_SSE41(pIn, pOut: PColor32EntryArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PQuadIntArray);
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x86)}
  // Parameters (x86):
  //   pIn       : EAX
  //   pOut      : EDX
  //   Width     : ECX
  //   iK        : [ESP + 4]
  //   i2Q       : [ESP + 8]
  //   iQ2       : [ESP + 12]
  //   QScale    : [ESP + 16]
  //   RowBuffer : [ESP + 20]

  TEST      ECX, ECX
  JLE       @Exit

  PUSH      EBX
  PUSH      ESI
  PUSH      EDI

  MOV       ESI, pIn                  // ESI = pIn
  MOV       EDI, pOut                 // EDI = pOut
  MOV       EBX, RowBuffer            // EBX = RowBuffer

  // Calculate shift count for division by QScale (QScale = 256, 1024, or 4096)
  BSR       EAX, QScale
  MOVD      XMM3, EAX                 // XMM3 = ShiftCount in low 32 bits

  // Broadcast coefficients iK, i2Q, iQ2 to all 4 dwords
  MOVD      XMM4, iK
  PSHUFD    XMM4, XMM4, 0             // XMM4 = [iK, iK, iK, iK]
  MOVD      XMM5, i2Q
  PSHUFD    XMM5, XMM5, 0             // XMM5 = [i2Q, i2Q, i2Q, i2Q]
  MOVD      XMM6, iQ2
  PSHUFD    XMM6, XMM6, 0             // XMM6 = [iQ2, iQ2, iQ2, iQ2]

  // Initialize forward pass boundary values (v1 = v2 = pIn[0] * 256)
  PMOVZXBD  XMM1, [ESI]               // Zero-extend 4 bytes to 4 dwords
  PSLLD     XMM1, 8                   // v1 = pIn[0] * 256
  MOVDQA    XMM2, XMM1                // v2 = v1

  MOV       EAX, ECX                  // Loop counter = Width

@ForwardLoop:
  // term12 = (v1 * i2Q - v2 * iQ2) div QScale
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  // term0 = (pIn[i] * 256 * iK) div QScale
  PMOVZXBD  XMM0, [ESI]
  PSLLD     XMM0, 8
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  // v0 = term0 + term12
  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  MOVDQU    [EBX], XMM0               // Store v0 to RowBuffer[i] (MOVDQU for unaligned GetMem buffer safety)

  ADD       ESI, 4
  ADD       EBX, 16
  DEC       EAX
  JNZ       @ForwardLoop

  // Backward pass initialization
  SUB       EBX, 16                   // Pointer to RowBuffer[Width - 1]
  LEA       EDI, [EDI + ECX * 4 - 4]  // Pointer to pOut[Width - 1]

  MOVDQU    XMM1, [EBX]               // v1 = RowBuffer[Width - 1]
  MOVDQA    XMM2, XMM1                // v2 = v1

  MOV       EAX, ECX                  // Loop counter = Width

@BackwardLoop:
  // term12 = (v1 * i2Q - v2 * iQ2) div QScale
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  // term0 = (RowBuffer[i] * iK) div QScale
  MOVDQU    XMM0, [EBX]
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  // v0 = term0 + term12
  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  // Construct [128, 128, 128, 128] constant in XMM7
  PCMPEQD   XMM7, XMM7
  PSRLD     XMM7, 31
  PSLLD     XMM7, 7                   // XMM7 = [128, 128, 128, 128]

  // Output pixel conversion: valVal = v0 + 128, div 256, saturate to [0..255]
  PADDD     XMM0, XMM7
  PSRAD     XMM0, 8
  PACKUSDW  XMM0, XMM0                // Saturate 32-bit dwords to 16-bit unsigned words
  PACKUSWB  XMM0, XMM0                // Saturate 16-bit words to 8-bit unsigned bytes
  MOVD      [EDI], XMM0               // Store 4 bytes to pOut[i]

  SUB       EBX, 16
  SUB       EDI, 4
  DEC       EAX
  JNZ       @BackwardLoop

  POP       EDI
  POP       ESI
  POP       EBX

@Exit:

{$elseif defined(TARGET_x64)}
  // Parameters (x64):
  //   pIn       : RCX
  //   pOut      : RDX
  //   Width     : R8D
  //   iK        : R9D
  //   i2Q       : [RSP + 40]
  //   iQ2       : [RSP + 48]
  //   QScale    : [RSP + 56]
  //   RowBuffer : [RSP + 64]

  TEST      R8D, R8D
  JLE       @Exit

{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
  .SAVENV XMM7
{$ENDIF}

  MOV       R10, RowBuffer            // R10 = RowBuffer
  MOV       EAX, QScale
  BSR       EAX, EAX
  MOVD      XMM3, EAX                 // XMM3 = ShiftCount

  MOVD      XMM4, iK
  PSHUFD    XMM4, XMM4, 0             // XMM4 = [iK, iK, iK, iK]
  MOVD      XMM5, i2Q
  PSHUFD    XMM5, XMM5, 0             // XMM5 = [i2Q, i2Q, i2Q, i2Q]
  MOVD      XMM6, iQ2
  PSHUFD    XMM6, XMM6, 0             // XMM6 = [iQ2, iQ2, iQ2, iQ2]

  PMOVZXBD  XMM1, [RCX]
  PSLLD     XMM1, 8                   // v1 = pIn[0] * 256
  MOVDQA    XMM2, XMM1                // v2 = v1

  MOV       R11D, R8D                 // Loop counter = Width

@ForwardLoop64:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  PMOVZXBD  XMM0, [RCX]
  PSLLD     XMM0, 8
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  MOVDQU    [R10], XMM0

  ADD       RCX, 4
  ADD       R10, 16
  DEC       R11D
  JNZ       @ForwardLoop64

  // Backward pass initialization
  SUB       R10, 16
  MOVSXD    RAX, R8D
  LEA       RDX, [RDX + RAX * 4 - 4]

  MOVDQU    XMM1, [R10]
  MOVDQA    XMM2, XMM1

  MOV       R11D, R8D

@BackwardLoop64:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  MOVDQU    XMM0, [R10]
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  PCMPEQD   XMM7, XMM7
  PSRLD     XMM7, 31
  PSLLD     XMM7, 7                   // XMM7 = [128, 128, 128, 128]

  PADDD     XMM0, XMM7
  PSRAD     XMM0, 8
  PACKUSDW  XMM0, XMM0
  PACKUSWB  XMM0, XMM0
  MOVD      [RDX], XMM0

  SUB       R10, 16
  SUB       RDX, 4
  DEC       R11D
  JNZ       @BackwardLoop64

@Exit:

{$else}
{$message fatal 'Unsupported target'}
{$ifend}

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4', 'XMM5', 'XMM6', 'XMM7'];{$ifend}
end;

//------------------------------------------------------------------------------
// SIMD 1-channel 8-bit row blur using Alvarez & Mazorra 2nd-order IIR
// using SSE4.1 32-bit integer SIMD arithmetic in XMM registers.
//------------------------------------------------------------------------------
procedure DraftBlurRow8_SSE41(pIn, pOut: PByteArray; Width: Integer; iK, i2Q, iQ2, QScale: Integer; RowBuffer: PIntArray);
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x86)}
  TEST      ECX, ECX
  JLE       @Exit

  PUSH      EBX
  PUSH      ESI
  PUSH      EDI

  MOV       ESI, pIn
  MOV       EDI, pOut
  MOV       EBX, RowBuffer

  BSR       EAX, QScale
  MOVD      XMM3, EAX

  MOVD      XMM4, iK
  MOVD      XMM5, i2Q
  MOVD      XMM6, iQ2

  MOVZX     EAX, BYTE PTR [ESI]
  MOVD      XMM1, EAX
  PSLLD     XMM1, 8                   // v1 = pIn[0] * 256
  MOVDQA    XMM2, XMM1                // v2 = v1

  MOV       EAX, ECX

@ForwardLoop:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  MOVZX     EDX, BYTE PTR [ESI]
  MOVD      XMM0, EDX
  PSLLD     XMM0, 8
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  MOVD      [EBX], XMM0               // Store v0 to RowBuffer[i]

  INC       ESI
  ADD       EBX, 4
  DEC       EAX
  JNZ       @ForwardLoop

  // Backward pass
  SUB       EBX, 4
  LEA       EDI, [EDI + ECX - 1]

  MOVD      XMM1, [EBX]
  MOVDQA    XMM2, XMM1

  MOV       EAX, ECX

@BackwardLoop:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  MOVD      XMM0, [EBX]
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  PCMPEQD   XMM7, XMM7
  PSRLD     XMM7, 31
  PSLLD     XMM7, 7                   // XMM7 = [128, 128, 128, 128]

  PADDD     XMM0, XMM7
  PSRAD     XMM0, 8
  PACKUSDW  XMM0, XMM0
  PACKUSWB  XMM0, XMM0
  MOVD      EDX, XMM0
  MOV       [EDI], DL                 // Store 1 byte to pOut[i]

  SUB       EBX, 4
  DEC       EDI
  DEC       EAX
  JNZ       @BackwardLoop

  POP       EDI
  POP       ESI
  POP       EBX

@Exit:

{$elseif defined(TARGET_x64)}

  TEST      R8D, R8D
  JLE       @Exit

{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
  .SAVENV XMM7
{$ENDIF}

  MOV       R10, RowBuffer
  MOV       EAX, QScale
  BSR       EAX, EAX
  MOVD      XMM3, EAX

  MOVD      XMM4, iK
  MOVD      XMM5, i2Q
  MOVD      XMM6, iQ2

  MOVZX     EAX, BYTE PTR [RCX]
  MOVD      XMM1, EAX
  PSLLD     XMM1, 8                   // v1 = pIn[0] * 256
  MOVDQA    XMM2, XMM1                // v2 = v1

  MOV       R11D, R8D

@ForwardLoop64:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  MOVZX     EAX, BYTE PTR [RCX]
  MOVD      XMM0, EAX
  PSLLD     XMM0, 8
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  MOVD      [R10], XMM0

  INC       RCX
  ADD       R10, 4
  DEC       R11D
  JNZ       @ForwardLoop64

  // Backward pass
  SUB       R10, 4
  MOVSXD    RAX, R8D
  LEA       RDX, [RDX + RAX - 1]

  MOVD      XMM1, [R10]
  MOVDQA    XMM2, XMM1

  MOV       R11D, R8D

@BackwardLoop64:
  MOVDQA    XMM7, XMM1
  PMULLD    XMM7, XMM5
  MOVDQA    XMM0, XMM2
  PMULLD    XMM0, XMM6
  PSUBD     XMM7, XMM0
  PSRAD     XMM7, XMM3

  MOVD      XMM0, [R10]
  PMULLD    XMM0, XMM4
  PSRAD     XMM0, XMM3

  PADDD     XMM0, XMM7

  MOVDQA    XMM2, XMM1
  MOVDQA    XMM1, XMM0

  PCMPEQD   XMM7, XMM7
  PSRLD     XMM7, 31
  PSLLD     XMM7, 7                   // XMM7 = [128, 128, 128, 128]

  PADDD     XMM0, XMM7
  PSRAD     XMM0, 8
  PACKUSDW  XMM0, XMM0
  PACKUSWB  XMM0, XMM0
  MOVD      EAX, XMM0
  MOV       [RDX], AL

  SUB       R10, 4
  DEC       RDX
  DEC       R11D
  JNZ       @BackwardLoop64

@Exit:

{$else}
{$message fatal 'Unsupported target'}
{$ifend}

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4', 'XMM5', 'XMM6', 'XMM7'];{$ifend}
end;

{$ifend}

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

  {$IF (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  BlurRegistry[@@DraftBlurRow32].Add(@DraftBlurRow32_SSE41, [isSSE41]).Name := 'DraftBlurRow32_SSE41';
  BlurRegistry[@@DraftBlurRow8].Add(@DraftBlurRow8_SSE41, [isSSE41]).Name := 'DraftBlurRow8_SSE41';
  {$IFEND}
end;

initialization
  RegisterBindings;
end.
