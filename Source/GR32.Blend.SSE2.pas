unit GR32.Blend.SSE2;

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
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

// Define GR32_SCALEMEMS_FAST to use the faster, but not very precise version of ScaleMems.
// The fast version uses a "shr 8" as a substitute for "div 255" which is also what
// ColorScale_Pas does.
{$define GR32_SCALEMEMS_FAST}

uses
  GR32;

{$if not defined(PUREPASCAL)}

//------------------------------------------------------------------------------
//
//      SSE SIMD blend implementations
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Blend
//------------------------------------------------------------------------------
function BlendReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function BlendReg_SSE41(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMem_SSE2(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMems_SSE2(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegEx_SSE2(F, B: TColor32; M: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegRGB_SSE2(F, B: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}

procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}
{$if not defined(FPC)}
procedure BlendLine_SSE41(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}
{$ifend}
procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}


//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
function MergeReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}

{$if not defined(FPC)}
function MergeReg_SSE41(F, B: TColor32): TColor32;
procedure MergeMem_SSE41(F: TColor32; var B: TColor32);
procedure MergeLine_SSE41(Src, Dst: PColor32; Count: Integer);
{$ifend}


//------------------------------------------------------------------------------
// Combine
//------------------------------------------------------------------------------
function CombineReg_SSE2(X, Y: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}

procedure CombineMem_SSE2_Table(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}
procedure CombineMem_SSE2_128(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}
procedure CombineMem_SSE41_8081(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}
procedure CombineMem_SSE41_Kadaif(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}

procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}


//------------------------------------------------------------------------------
// Color algebra
//------------------------------------------------------------------------------
function ColorAdd_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorSub_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorModulate_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorMax_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorMin_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorDifference_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorExclusion_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorScale_SSE2(C: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}


//------------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------------
function LightenReg_SSE2(C: TColor32; Amount: Integer): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure ScaleMems_SSE41(Dst: PColor32; Count: Integer; Weight: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}
procedure FastScaleMems_SSE41(Dst: PColor32; Count: Integer; Weight: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}


//------------------------------------------------------------------------------
// Premultiply/Unpremultiply
//------------------------------------------------------------------------------
procedure PremultiplyMem_SSE41(Pixels: PColor32Entry; Count: Integer);
procedure UnpremultiplyMem_SSE41(Pixels: PColor32Entry; Count: Integer);


{$ifend}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$if not defined(PUREPASCAL)}

uses
  GR32_Blend,
  GR32_LowLevel,
  GR32_Bindings,
  GR32.Types.SIMD;

//------------------------------------------------------------------------------
//
//      Blend
//
//------------------------------------------------------------------------------
// Blend foreground color (F) with a background color (B), using alpha channel
// value of F.
//
// Result := F.a * (F.argb - B.argb) + B.argb
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// BlendReg
//------------------------------------------------------------------------------
function BlendReg_SSE41(F, B: TColor32): TColor32;
(*
        BlendReg_SSE41_Sanyin_257
        Contributed by: Sanyin

        Errors: 0

        Calculates "x div 255" as:

          x div 255 = ((x + 128) * 257) >> 16
*)
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0, EAX
        MOVD      XMM1, EDX
{$elseif defined(TARGET_x64)}
        MOVD      XMM0, ECX
        MOVD      XMM1, EDX
{$ifend}

        PMOVZXBW  XMM0, XMM0
        PMOVZXBW  XMM1, XMM1
        PSHUFLW   XMM2, XMM0, $FF
        MOVDQA    XMM3, DQWORD PTR [SSE_00FF00FF_ALIGNED]
        PSUBW     XMM3, XMM2
        PMULLW    XMM0, XMM2
        PMULLW    XMM1, XMM3
        PADDW     XMM0, XMM1
        PADDW     XMM0, DQWORD PTR [SSE_00800080_ALIGNED]
        PMULHUW   XMM0, DQWORD PTR [SSE_01010101_ALIGNED]
        PACKUSWB  XMM0, XMM0
        MOVD      EAX, XMM0
        OR        EAX, $FF000000
end;

//------------------------------------------------------------------------------

function BlendReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
(*
        BlendReg_SSE2_Sanyin_257
        Contributed by: Sanyin

        Errors: 0

        Calculates "x div 255" as:

          x div 255 = ((x + 128) * 257) >> 16
*)
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x64) and not defined(FPC)}
        .SAVENV XMM4
{$ifend}

{$if defined(TARGET_x86)}
        MOVD      XMM0, EAX
        MOVD      XMM1, EDX
{$elseif defined(TARGET_x64)}
        MOVD      XMM0, ECX
        MOVD      XMM1, EDX
{$ifend}

        PXOR      XMM4, XMM4
        PUNPCKLBW XMM0, XMM4          // Components of F (word)
        PUNPCKLBW XMM1, XMM4          // Components of B (word)
        PSHUFLW   XMM2, XMM0, $FF     // Broadcast Fa into words
        MOVDQA    XMM3, DQWORD PTR [SSE_00FF00FF_ALIGNED]
        PSUBW     XMM3, XMM2
        PMULLW    XMM0, XMM2
        PMULLW    XMM1, XMM3
        PADDW     XMM0, XMM1
        PADDW     XMM0, DQWORD PTR [SSE_00800080_ALIGNED]
        PMULHUW   XMM0, DQWORD PTR [SSE_01010101_ALIGNED]
        PACKUSWB  XMM0, XMM0
        MOVD      EAX, XMM0
        OR        EAX, $FF000000

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4'];{$ifend}
end;


//------------------------------------------------------------------------------
// BlendMem
//------------------------------------------------------------------------------
procedure BlendMem_SSE2(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        PXOR      XMM3,XMM3
        MOVD      XMM0,EAX
        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM0,XMM3
        MOV       ECX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PSHUFLW   XMM1,XMM1,$FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[ECX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [EDX],XMM2

@1:     RET
@2:     MOV       [EDX], EAX
{$elseif defined(TARGET_x64)}
  // ECX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      ECX,$FF000000
        JZ        @1
        CMP       ECX,$FF000000
        JNC       @2

        PXOR      XMM3,XMM3
        MOVD      XMM0,ECX
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM0,XMM3
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PSHUFLW   XMM1,XMM1,$FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[RAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [RDX],XMM2

@1:     RET
@2:     MOV       [RDX], ECX
{$ifend}
end;


//------------------------------------------------------------------------------
// BlendRegEx
//------------------------------------------------------------------------------
// Result := M * F.a * (F.argb - B.argb) + B.argb
//------------------------------------------------------------------------------
function BlendRegEx_SSE2(F, B: TColor32; M: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX <- F
  // EDX <- B
  // ECX <- M
        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,EAX
        SHL       ECX,4
        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       ECX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8
        MOV       ECX,bias_ptr
        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1

        POP       EBX
        RET

@1:     MOV       EAX,EDX
        POP       EBX

{$elseif defined(TARGET_x64)}

  // ECX <- F
  // EDX <- B
  // R8D <- M

        MOV       EAX,ECX
        SHR       EAX,24
        INC       R8D             // 255:256 range bias
        IMUL      R8D,EAX
        SHR       R8D,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,ECX
        SHL       R8D,4
        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8
{$IFNDEF FPC}
        MOV       R8,bias_ptr
{$ELSE}
        MOV       R8,[RIP+bias_ptr]
{$ENDIF}
        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
        RET

@1:     MOV       EAX,EDX

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendMemEx
//------------------------------------------------------------------------------
// B.argb := M * F.a * (F.argb - B.argb) + B.argb
//------------------------------------------------------------------------------
procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm

{$if defined(TARGET_x86)}
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Fargb - Bargb) + Bargb
        TEST      EAX,$FF000000
        JZ        @2

        PUSH      EBX
        MOV       EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        SHR       EBX,24          // EBX  <-  00 00 00 Fa
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX         // ECX  <-  00 00  W **
        SHR       ECX,8           // ECX  <-  00 00 00  W
        JZ        @1

        PXOR      XMM0,XMM0       // XMM0 <-  00 00 00 00 00 00 00 00
        MOVD      XMM1,EAX        // XMM1 <-  00 00 00 00 Fa Fr Fg Fb
        SHL       ECX,4
        MOVD      XMM2,[EDX]      // XMM2 <-  00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       ECX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8
        MOV       ECX,bias_ptr
        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [EDX],XMM1

@1:
        POP       EBX

@2:

{$elseif defined(TARGET_x64)}

  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // RCX <- F
  // [RDX] <- B
  // R8 <- M
  // Result := M * Fa * (Fargb - Bargb) + Bargb

        TEST      ECX,$FF000000
        JZ        @1

        MOV       R9D,ECX
        SHR       R9D,24
        INC       R8D            // 255:256 range bias
        IMUL      R8D,R9D
        SHR       R8D,8
        JZ        @1

        PXOR      XMM0,XMM0
        MOVD      XMM1,ECX
        SHL       R8D,4
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8
{$IFNDEF FPC}
        MOV       R8,bias_ptr
{$ELSE}
        MOV       R8,[RIP+bias_ptr]
{$ENDIF}
        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      DWORD PTR [RDX],XMM1
@1:

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendRegRGB
//------------------------------------------------------------------------------
function BlendRegRGB_SSE2(F, B: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        BSWAP     ECX
        PSUBW     XMM0,XMM1
        MOVD      XMM3,ECX
        PUNPCKLBW XMM3,XMM2
        PMULLW    XMM0,XMM3
        MOV       EAX,bias_ptr
        PSLLW     XMM1,8
        PADDW     XMM1,[EAX]
        PADDW     XMM1,XMM0
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM2
        MOVD      EAX,XMM1

{$elseif defined(TARGET_x64)}

        PXOR      XMM2,XMM2
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        BSWAP     R8D
        PSUBW     XMM0,XMM1
        MOVD      XMM3,R8D
        PUNPCKLBW XMM3,XMM2
        PMULLW    XMM0,XMM3
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PSLLW     XMM1,8
        PADDW     XMM1,[RAX]
        PADDW     XMM1,XMM0
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM2
        MOVD      EAX,XMM1

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendMemRGB
//------------------------------------------------------------------------------
procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,[EDX]
        PUNPCKLBW XMM1,XMM2
        BSWAP     ECX
        PSUBW     XMM0,XMM1
        MOVD      XMM3,ECX
        PUNPCKLBW XMM3,XMM2
        PMULLW    XMM0,XMM3
        MOV       EAX,bias_ptr
        PSLLW     XMM1,8
        PADDW     XMM1,[EAX]
        PADDW     XMM1,XMM0
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM2
        MOVD      [EDX],XMM1

{$elseif defined(TARGET_x64)}

        MOVD      XMM1,R8D

        PXOR      XMM4,XMM4
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        MOVQ      XMM5,[RAX]
        MOVD      XMM0,ECX
        MOVD      XMM2,[RDX]

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM1,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM1,$1B

  // C = wA  B - wB
        PMULLW    XMM0,XMM1
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8

        PADDW     XMM0,XMM2

        PMULLW    XMM2,XMM1
        PADDW     XMM2,XMM5
        PSRLW     XMM2,8

        PSUBW     XMM0,XMM2

        PACKUSWB  XMM0,XMM4

        MOVD      [RDX],XMM0

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendMemRGB128
//------------------------------------------------------------------------------
{$IFDEF TEST_BLENDMEMRGB128SSE4}
procedure BlendMemRGB128_SSE4(F: TColor32; var B: TColor32; W: UInt64); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        MOVQ      XMM1,W

        PXOR      XMM4,XMM4
        MOV       ECX,[bias_ptr]
        MOVDQA    XMM5,[ECX]

        MOVD      XMM0,EAX
        PINSRD    XMM0,EAX,1
        MOVQ      XMM2,[EDX].QWORD

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM1,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM1,$1B
        PSHUFHW   XMM1,XMM1,$1B

  // C = wA  B - wB
        PMULLW    XMM0,XMM1
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8

        PADDW     XMM0,XMM2

        PMULLW    XMM2,XMM1
        PADDW     XMM2,XMM5
        PSRLW     XMM2,8

        PSUBW     XMM0,XMM2

        PACKUSWB  XMM0,XMM4

        MOVQ      [EDX].QWORD,XMM0

{$elseif defined(TARGET_x64)}

        MOVQ      XMM1,R8

        PXOR      XMM4,XMM4
        MOV       RAX,[RIP+bias_ptr]
        MOVDQA    XMM5,[RAX]

        MOVD      XMM0,ECX
        PINSRD    XMM0,ECX,1
        MOVQ      XMM2,[RDX].QWORD

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM1,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM1,$1B
        PSHUFHW   XMM1,XMM1,$1B

  // C = wA  B - wB
        PMULLW    XMM0,XMM1
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8

        PADDW     XMM0,XMM2

        PMULLW    XMM2,XMM1
        PADDW     XMM2,XMM5
        PSRLW     XMM2,8

        PSUBW     XMM0,XMM2

        PACKUSWB  XMM0,XMM4

        MOVQ      [RDX].QWORD,XMM0

{$ifend}
end;
{$ENDIF}


//------------------------------------------------------------------------------
// BlendLine
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// BlendLine_SSE41
//
// - Estimates x * alpha / 255 as ((x * alpha * 0x8081) + bias) >> 23.
//
// - Uses PTEST for 4-pixel early exit (skipping entirely transparent blocks or
//   copying entirely opaque blocks).
//
// - Uses PMOVZXBW for efficient unpacking of byte components to words.
//
// - Unrolls the main loop to process 4 pixels per iteration.
//
// - FPC is not supported.
//------------------------------------------------------------------------------
{$if not defined(FPC)}
procedure BlendLine_SSE41(Src, Dst: PColor32; Count: Integer);
asm
{$if defined(TARGET_X86)}
  //
  // Parameters (x86):
  //   EAX <- Src
  //   EDX <- Dst
  //   ECX <- Count
  //
  // SSE register usage:
  //   XMM0: 4 pixels
  //   XMM1: Misc.
  //   XMM2: Misc.
  //   XMM3: Misc.
  //   XMM4: Zero
  //   XMM5: Bias
  //   XMM6: Alpha Mask
  //   XMM7: Misc.
  //

        TEST      ECX,ECX
        JLE       @Done

        PUSH      EBX
        PUSH      ESI

        MOV       ESI, [bias_ptr]
        MOVDQA    XMM5, [ESI]      // XMM5 = Bias constant ($0080 per word) for (x + 128) div 256

        PXOR      XMM4, XMM4       // XMM4 = Zero
        PCMPEQD   XMM6, XMM6
        PSLLD     XMM6, 24         // XMM6 = Alpha Mask ($FF000000 per dword)

        SUB       ECX, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [EAX]      // Load 4 source pixels: [s4 s3 s2 s1]
        // Early exit: If all pixels in the block are fully transparent (Alpha=0), skip them.
        PTEST     XMM0, XMM6
        JZ        @Next4           // ZF=1: All alphas are 0

        MOVDQU    XMM1, [EDX]      // Load 4 destination pixels: [d4 d3 d2 d1]
        // Early exit: If all pixels in the block are fully opaque (Alpha=255), direct copy.
        // PTEST sets CF if (NOT XMM0 AND XMM6) == 0. With XMM6 as alpha mask,
        // this means all alpha bits in XMM0 are set.
        JC        @Opaque4         // CF=1: All alphas are 255

        // --- Pixels 1 & 2 ---
        // Unpack pixels 1 and 2 to 16-bit words using SSE4.1 zero-extension
        PMOVZXBW  XMM2, XMM0       // XMM2 = [00sa 00sr 00sg 00sb | 00sa 00sr 00sg 00sb] (pixels 1 & 2)
        PMOVZXBW  XMM3, XMM1       // XMM3 = [00da 00dr 00dg 00db | 00da 00dr 00dg 00db] (pixels 1 & 2)

        // Broadcast alpha for pixels 1 and 2
        PSHUFLW   XMM7, XMM2, $FF
        PSHUFHW   XMM7, XMM7, $FF  // XMM7 = [sa sa sa sa | sa sa sa sa] (broadcasted)

        // Src pre-multiplication: Src_pre = (Src * Alpha + 128) >> 8
        MOVDQA    XMM4, XMM7
        PSRLQ     XMM4, 16         // Shift multipliers to clear Alpha channel: [00 sa sa sa]
        PMULLW    XMM2, XMM4       // Multiply RGB components by alpha
        PADDW     XMM2, XMM5       // Add rounding bias ($0080 per word)
        PSRLW     XMM2, 8          // Divide by 256
        PSLLQ     XMM4, 48         // Restore alpha component: [sa 00 00 00]
        POR       XMM2, XMM4       // XMM2 = [sa sr' sg' sb'] (Source pre-multiplied)

        // Blend: Result = Src_pre + Dst - (Dst * Alpha + 128) >> 8
        // This is equivalent to Porter-Duff 'Over': Src + Dst * (1 - Alpha)
        MOVDQA    XMM4, XMM3       // Keep copy of unpacked Destination
        PMULLW    XMM3, XMM7       // Dst * Alpha (including Alpha channel)
        PADDW     XMM3, XMM5       // Add bias
        PSRLW     XMM3, 8          // Divide by 256
        PADDW     XMM2, XMM4       // Add Src_pre and Dst
        PSUBW     XMM2, XMM3       // Subtract (Dst * Alpha) part

        // --- Pixels 3 & 4 ---
        // Access high 64-bits of previously loaded pixels using shuffles
        PSHUFD    XMM3, XMM0, $EE  // Src pixels 3 & 4
        PSHUFD    XMM4, XMM1, $EE  // Dst pixels 3 & 4
        PMOVZXBW  XMM3, XMM3       // Unpack to words
        PMOVZXBW  XMM4, XMM4

        PSHUFLW   XMM7, XMM3, $FF
        PSHUFHW   XMM7, XMM7, $FF  // Alpha multipliers 3 & 4

        // Src pre-multiplication 3 & 4
        MOVDQA    XMM1, XMM7
        PSRLQ     XMM1, 16
        PMULLW    XMM3, XMM1
        PADDW     XMM3, XMM5
        PSRLW     XMM3, 8
        PSLLQ     XMM1, 48
        POR       XMM3, XMM1       // XMM3 = Src_pre 3 & 4

        // Result 3 & 4
        MOVDQA    XMM1, XMM4       // Copy Dst
        PMULLW    XMM4, XMM7       // Dst * Alpha
        PADDW     XMM4, XMM5
        PSRLW     XMM4, 8
        PADDW     XMM3, XMM1
        PSUBW     XMM3, XMM4       // XMM3 = Result 3 & 4

        // Pack 4 pixels (from words back to bytes) and store
        PACKUSWB  XMM2, XMM3       // Pack XMM2 (1 & 2) and XMM3 (3 & 4) into XMM2
        MOVDQU    [EDX], XMM2
        PXOR      XMM4,XMM4        // Re-zero XMM4 for tail or next iteration

@Next4:
        ADD       EAX, 16
        ADD       EDX, 16
        SUB       ECX, 4
        JGE       @Loop4

@Tail:
        ADD       ECX, 4
        JZ        @Exit

@TailLoop:
        // Scalar fallback for remaining 1-3 pixels
        MOV       EBX, [EAX]       // EBX = Current Source pixel
        TEST      EBX, $FF000000   // Check alpha
        JZ        @TailNext        // Skip if fully transparent
        CMP       EBX, $FF000000
        JNC       @TailOpaque      // Direct copy if fully opaque

        // Blend individual pixel
        MOVD      XMM0, EBX        // XMM0 = Src
        MOVD      XMM1, [EDX]      // XMM1 = Dst
        PUNPCKLBW XMM0, XMM4       // Unpack Src to words
        PUNPCKLBW XMM1, XMM4       // Unpack Dst to words
        PSHUFLW   XMM2, XMM0, $FF  // Broadcast Alpha multiplier

        // Pre-multiply Source: Src_pre = (Src * Alpha + 128) >> 8
        MOVDQA    XMM3, XMM2
        PSRLQ     XMM3, 16         // RGB multiplier
        PMULLW    XMM0, XMM3
        PADDW     XMM0, XMM5
        PSRLW     XMM0, 8
        PSLLQ     XMM3, 48         // Alpha for restore
        POR       XMM0, XMM3       // XMM0 = Src_pre

        // Blend: Result = Src_pre + Dst - (Dst * Alpha + 128) >> 8
        PMULLW    XMM2, XMM1
        PADDW     XMM2, XMM5
        PSRLW     XMM2, 8
        PADDW     XMM0, XMM1
        PSUBW     XMM0, XMM2
        PACKUSWB  XMM0, XMM4       // Pack result word -> byte
        MOVD      [EDX], XMM0
        JMP       @TailNext

@Opaque4:
        MOVDQU    [EDX], XMM0      // All opaque, direct copy source to destination
        JMP       @Next4

@TailOpaque:
        MOV       [EDX], EBX       // All opaque, direct copy

@TailNext:
        ADD       EAX, 4
        ADD       EDX, 4
        DEC       ECX
        JNZ       @TailLoop
        // Fall through to @Exit

@Exit:
        POP       ESI
        POP       EBX

@Done:

{$elseif defined(TARGET_X64)}

  //
  // Parameters (x64):
  //   RCX <- Src
  //   RDX <- Dst
  //   R8D <- Count
  //
  // SSE register usage:
  //   XMM0: 4 pixels
  //   XMM1: Misc.
  //   XMM2: Misc.
  //   XMM3: Misc.
  //   XMM4: Zero
  //   XMM5: Bias
  //   XMM6: Alpha Mask
  //   XMM7: Misc.
  //   XMM8: Misc.
  //   XMM9: Misc.
  //   XMM10: Misc.
  //

        TEST      R8D,R8D
        JLE       @Done

        .SAVENV XMM4
        .SAVENV XMM5
        .SAVENV XMM6
        .SAVENV XMM7
        .SAVENV XMM8
        .SAVENV XMM9
        .SAVENV XMM10

        PXOR      XMM4, XMM4       // XMM4 = Zero
        MOV       RAX, bias_ptr
        MOVDQA    XMM5, [RAX]      // XMM5 = Bias constant ($0080 per word)

        PCMPEQD   XMM6, XMM6
        PSLLD     XMM6, 24         // XMM6 = Alpha Mask ($FF000000 per dword)

        SUB       R8D, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [RCX]      // Load 4 source pixels
        // Early exit: If all pixels in the block are fully transparent (Alpha=0), skip them.
        PTEST     XMM0, XMM6
        JZ        @Next4           // All transparent

        MOVDQU    XMM1, [RDX]      // Load 4 destination pixels
        // Early exit: If all pixels in the block are fully opaque (Alpha=255), direct copy.
        JC        @Opaque4         // All opaque

        // --- Pixels 1 & 2 ---
        // Unpack pixels to words using SSE4.1 zero-extension
        PMOVZXBW  XMM2, XMM0       // XMM2 = Src 1 & 2 unpacked
        PMOVZXBW  XMM3, XMM1       // XMM3 = Dst 1 & 2 unpacked
        PSHUFLW   XMM7, XMM2, $FF
        PSHUFHW   XMM7, XMM7, $FF  // Alpha multipliers 1 & 2

        // Src pre-multiplication 1 & 2
        MOVDQA    XMM8, XMM7
        PSRLQ     XMM8, 16
        PMULLW    XMM2, XMM8
        PADDW     XMM2, XMM5
        PSRLW     XMM2, 8
        PSLLQ     XMM8, 48
        POR       XMM2, XMM8       // XMM2 = Src_pre 1 & 2

        // Blend: Result = Src_pre + Dst - (Dst * Alpha + 128) >> 8
        MOVDQA    XMM8, XMM3       // Dst 1 & 2 copy
        PMULLW    XMM3, XMM7       // Dst * Alpha
        PADDW     XMM3, XMM5
        PSRLW     XMM3, 8
        PADDW     XMM2, XMM8
        PSUBW     XMM2, XMM3       // XMM2 = Result 1 & 2

        // --- Pixels 3 & 4 ---
        // Access high 64-bits of previously loaded pixels
        PSHUFD    XMM8, XMM0, $EE  // Src 3 & 4
        PSHUFD    XMM9, XMM1, $EE  // Dst 3 & 4
        PMOVZXBW  XMM8, XMM8
        PMOVZXBW  XMM9, XMM9
        PSHUFLW   XMM7, XMM8, $FF
        PSHUFHW   XMM7, XMM7, $FF  // Alpha multipliers 3 & 4

        // Src pre-multiplication 3 & 4
        MOVDQA    XMM10, XMM7
        PSRLQ     XMM10, 16
        PMULLW    XMM8, XMM10
        PADDW     XMM8, XMM5
        PSRLW     XMM8, 8
        PSLLQ     XMM10, 48
        POR       XMM8, XMM10      // XMM8 = Src_pre 3 & 4

        // Result 3 & 4
        MOVDQA    XMM10, XMM9      // Dst 3 & 4 copy
        PMULLW    XMM9, XMM7       // Dst * Alpha
        PADDW     XMM9, XMM5
        PSRLW     XMM9, 8
        PADDW     XMM8, XMM10
        PSUBW     XMM8, XMM9       // XMM8 = Result 3 & 4

        // Pack 4 pixels (from words back to bytes) and store
        PACKUSWB  XMM2, XMM8       // Pack XMM2 (1 & 2) and XMM8 (3 & 4) into XMM2
        MOVDQU    [RDX], XMM2

@Next4:
        ADD       RCX, 16
        ADD       RDX, 16
        SUB       R8D, 4
        JGE       @Loop4

@Tail:
        ADD       R8D, 4
        JZ        @Done

        PXOR      XMM4,XMM4        // Re-zero XMM4 for tail fallback

@TailLoop:
        // Scalar fallback for remaining pixels
        MOV       EAX, [RCX]       // EAX = Current Source pixel
        TEST      EAX, $FF000000   // Check alpha
        JZ        @TailNext        // Skip if fully transparent
        CMP       EAX, $FF000000
        JNC       @TailOpaque      // Direct copy if fully opaque

        // Blend individual pixel
        MOVD      XMM0, EAX        // XMM0 = Src
        MOVD      XMM1, [RDX]      // XMM1 = Dst
        PUNPCKLBW XMM0, XMM4       // Unpack Src to words
        PUNPCKLBW XMM1, XMM4       // Unpack Dst to words
        PSHUFLW   XMM2, XMM0, $FF  // Broadcast Alpha multiplier

        // Pre-multiply Source: Src_pre = (Src * Alpha + 128) >> 8
        MOVDQA    XMM3, XMM2
        PSRLQ     XMM3, 16         // RGB multiplier
        PMULLW    XMM0, XMM3
        PADDW     XMM0, XMM5
        PSRLW     XMM0, 8
        PSLLQ     XMM3, 48         // Alpha for restore
        POR       XMM0, XMM3       // XMM0 = Src_pre

        // Blend: Result = Src_pre + Dst - (Dst * Alpha + 128) >> 8
        PMULLW    XMM2, XMM1
        PADDW     XMM2, XMM5
        PSRLW     XMM2, 8
        PADDW     XMM0, XMM1
        PSUBW     XMM0, XMM2
        PACKUSWB  XMM0, XMM4       // Pack result word -> byte
        MOVD      [RDX], XMM0
        JMP       @TailNext

@Opaque4:
        MOVDQU    [RDX], XMM0      // All opaque, direct copy source to destination
        JMP       @Next4

@TailOpaque:
        MOV       [RDX], EAX       // All opaque, direct copy

@TailNext:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @TailLoop
        // Fall through to @Done
        // JMP       @Done

@Done:

{$else}
{$message fatal 'Unsupported target'}
{$ifend}

end;
{$ifend}

//------------------------------------------------------------------------------
// BlendLine_SSE2
//------------------------------------------------------------------------------
procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
{$IFDEF FPC}
const
  COpaque: QWORD = QWORD($FF000000FF000000);
{$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

        TEST      ECX,ECX
        JLE       @3

        PUSH      EBX
        PXOR      XMM4,XMM4
        MOV       EBX,[bias_ptr]
        MOVDQA    XMM5,[EBX]
        POP       EBX

        TEST      ECX, 1
        JZ        @2
        MOVD      XMM0,[EAX]
        MOVD      XMM2,[EDX]

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM0,$FF

  // premultiply source pixel by its alpha
        MOVQ      XMM3,XMM1
        PSRLQ     XMM3,16
        PMULLW    XMM0,XMM3
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8
        PSLLQ     XMM3,48
        POR       XMM0,XMM3

  // C' = A'  B' - aB'
        PMULLW    XMM1,XMM2
        PADDW     XMM1,XMM5
        PSRLW     XMM1,8
        PADDW     XMM0,XMM2
        PSUBW     XMM0,XMM1

        PACKUSWB  XMM0,XMM4
        MOVD      [EDX], XMM0

@2:
        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]

        SHR       ECX,1
        JZ        @3
        NEG       ECX

@1:
        MOVQ      XMM0,[EAX + ECX * 8].QWORD
        MOVQ      XMM2,[EDX + ECX * 8].QWORD

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM0,$FF
        PSHUFHW   XMM1,XMM1,$FF

  // premultiply source pixel by its alpha
        MOVDQA    XMM3,XMM1
        PSRLQ     XMM3,16
        PMULLW    XMM0,XMM3
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8
        PSLLQ     XMM3,48
        POR       XMM0,XMM3

  // C' = A' + B' - aB'
        PMULLW    XMM1,XMM2
        PADDW     XMM1,XMM5
        PSRLW     XMM1,8
        PADDW     XMM0,XMM2
        PSUBW     XMM0,XMM1

        PACKUSWB  XMM0,XMM4
        MOVQ      [EDX + ECX * 8].QWORD,XMM0

        ADD       ECX,1
        JS        @1
@3:

{$elseif defined(TARGET_x64)}

        TEST      R8D,R8D
        JLE       @3

        PXOR      XMM4,XMM4
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        MOVDQA    XMM5,[RAX]

        MOV       R9D, R8D
        SHR       R9D, 1
        TEST      R9D, R9D
        JZ        @2

@1:
        MOVQ      XMM0,[RCX].QWORD
        MOVQ      RAX,XMM0
{$IFDEF FPC}
        AND       RAX,[RIP+COpaque]
        JZ        @1b
        CMP       RAX,[RIP+COpaque]
        JZ        @1a
{$ENDIF}

        MOVQ      XMM2,[RDX].QWORD

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM0,$FF
        PSHUFHW   XMM1,XMM1,$FF

  // premultiply source pixel by its alpha
        MOVDQA    XMM3,XMM1
        PSRLQ     XMM3,16
        PMULLW    XMM0,XMM3
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8
        PSLLQ     XMM3,48
        POR       XMM0,XMM3

  // C' = A' + B' - aB'
        PMULLW    XMM1,XMM2
        PADDW     XMM1,XMM5
        PSRLW     XMM1,8
        PADDW     XMM0,XMM2
        PSUBW     XMM0,XMM1

        PACKUSWB  XMM0,XMM4
@1a:    MOVQ      [RDX].QWORD,XMM0

@1b:    ADD       RCX,8
        ADD       RDX,8

        SUB       R9D,1
        JNZ       @1

@2:
        AND       R8D, 1
        JZ        @3

        MOVD      XMM0,[RCX]
        MOVD      XMM2,[RDX]

        PUNPCKLBW XMM0,XMM4
        PUNPCKLBW XMM2,XMM4

        PSHUFLW   XMM1,XMM0,$FF

  // premultiply source pixel by its alpha
        MOVQ      XMM3,XMM1
        PSRLQ     XMM3,16
        PMULLW    XMM0,XMM3
        PADDW     XMM0,XMM5
        PSRLW     XMM0,8
        PSLLQ     XMM3,48
        POR       XMM0,XMM3

  // C' = A'  B' - aB'
        PMULLW    XMM1,XMM2
        PADDW     XMM1,XMM5
        PSRLW     XMM1,8
        PADDW     XMM0,XMM2
        PSUBW     XMM0,XMM1

        PACKUSWB  XMM0,XMM4
        MOVD      [RDX], XMM0
@3:

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendMems
// Like BlendLine except the Src parameter is static.
//------------------------------------------------------------------------------
procedure BlendMems_SSE2(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}
asm
  //
  // Result Z = Fa * (Fargb - Bargb) + Bargb
  //          = Fa * Fargb - Fa * Bargb + Bargb
  //
  // For Fa * Fargb, ((a*x) div 255) is approximated as ((((a * $101) shr 16) * x + 128) div 256)
  // For Fa * Bargb, (x div 255) is approximated as ((x + 128) div 256)
  //
{$if defined(TARGET_x86)}

  // EAX <- Src: TColor32
  // EDX <- Dst: PColor32
  // ECX <- Count

  // Test the counter for zero or negativity
        // JCXZ      @Done
        TEST      ECX, ECX
        JLE       @Done

  // Test if source if fully transparent
        TEST      EAX, $FF000000
        JZ        @Done

  // Setup division by 255 bias
        PUSH      EBX
        PXOR      XMM4, XMM4
        MOV       EBX, [bias_ptr]
        MOVDQA    XMM5, [EBX]
        POP       EBX

  // Load source
        MOVD      XMM0, EAX                     // XMM0 <- 00 00 00 00 Fa Fr Fg Fb

  // Get source alpha and test if fully opaque
        SHR       EAX, 24
        CMP       EAX, $FF
        JZ        @FillWithSource

        PSHUFD    XMM0, XMM0, 0                 // XMM0[0..3] <- XMM0[0][0..3]
        PUNPCKLBW XMM0, XMM4                    // XMM0 <- 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1, XMM0, $FF               // XMM1 <- 00 Fa 00 Fa 00 Fa 00 Fa
        PSHUFHW   XMM1, XMM1, $FF

  // Premultiply source pixel by its alpha: Fa * Fargb
        MOVDQA    XMM3, XMM1                    // XMM3 <- 2*QWord(XMM1)
        PSRLQ     XMM3, 16                      // XMM3 <- 00 00 00 Fa 00 Fa 00 Fa
        PMULLW    XMM0, XMM3                    // XMM0 <- Frgb * Fa
        PADDW     XMM0, XMM5                    // XMM0 <- Frgb * Fa + Bias
        PSRLW     XMM0, 8                       // XMM0 <- (Frgb * Fa + Bias) div 256
        PSLLQ     XMM3, 48                      // XMM3 <- 00 Fa 00 00 00 00 00 00
        POR       XMM0, XMM3                    // XMM0 <- 00 Fa 00 FR 00 FG 00 FB

  // Save alpha multiplier
        MOVDQA    XMM3, XMM1

  // Test for odd/even count
        TEST      ECX, 1
        JZ        @Even

  // We have an odd number of pixels.
  // Blend a single pixel so the remaining count is even.

  // Load dest
        MOVD      XMM2, DWORD PTR [EDX]         // XMM2 <- 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM2, XMM4                    // XMM2 <- 00 Ba 00 Br 00 Bg 00 Bb

  // Blend: C' = A'  B' - aB'
        PMULLW    XMM1, XMM2                    // Z1 = Fa * Brgba
        PADDW     XMM1, XMM5                    // Z1 = Fa * Brgba + Bias
        PSRLW     XMM1, 8                       // Z1 = (Fa * Bargb + Bias) div 256
        PADDW     XMM2, XMM0                    // Z2 = Brgba + FaRGB
        PSUBW     XMM2, XMM1                    // Z2 = Z2 - Z1

        PACKUSWB  XMM2, XMM4
        MOVD      [EDX], XMM2

@Even:
        LEA       EDX, [EDX + ECX * 4]          // Get address of last pixel

        SHR       ECX,1                         // Number of QWORDs
        JZ        @Done
        NEG       ECX                           // Negate count so we can use it as an offset to move forward

@Loop:
  // Blend two pixels at a time

  // Restore alpha multiplier
        MOVDQA    XMM1, XMM3

  // Load dest
        MOVQ      XMM2, [EDX + ECX * 8].QWORD   // XMM2 <- Ba Br Bg Bb Ba Br Bg Bb
        PUNPCKLBW XMM2, XMM4                    // XMM2 <- 00 Ba 00 Br 00 Bg 00 Bb

  // Blend: C' = A' + B' - aB'
        PMULLW    XMM1, XMM2
        PADDW     XMM1, XMM5
        PSRLW     XMM1, 8
        PADDW     XMM2, XMM0
        PSUBW     XMM2, XMM1

        PACKUSWB  XMM2, XMM4
        MOVQ      [EDX + ECX * 8].QWORD, XMM2

        ADD       ECX, 1
        JS        @Loop

@Done:
        RET

@FillWithSource:
  // Shuffle registers for FillLongword
        MOV       EAX, EDX
        MOV       EDX, ECX
        MOVD      ECX, XMM0

        CALL      FillLongword // EAX:Dest, EDX:Count, ECX:Value

{$elseif defined(TARGET_x64)}

  // ECX <- Src: TColor32
  // RDX <- Dst: PColor32
  // R8D <- Count

  // Test the counter for zero or negativity
        TEST      R8D, R8D
        JLE       @Done

  // Test if source if fully transparent
        TEST      ECX, $FF000000
        JZ        @Done

  // Get source alpha
        MOV       EAX, ECX
        SHR       EAX, 24

  // Test if source is fully opaque
        CMP       EAX, $FF
        JZ        @FillWithSource

  // Setup division by 255 bias
        PXOR      XMM4, XMM4
{$IFNDEF FPC}
        MOV       RAX, bias_ptr
{$ELSE}
        MOV       RAX, [RIP+bias_ptr]
{$ENDIF}
        MOVDQA    XMM5, [RAX]

  // Load source
        MOVQ      XMM0, RCX                     // XMM0 <- 00 00 00 00 Fa Fr Fg Fb
        PSHUFD    XMM0, XMM0, 0                 // XMM0[0..3] <- XMM0[0][0..3]
        PUNPCKLBW XMM0, XMM4                    // XMM0 <- 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1, XMM0, $FF               // XMM1 <- 00 Fa 00 Fa 00 Fa 00 Fa
        PSHUFHW   XMM1, XMM1, $FF

  // Premultiply source pixel by its alpha
        MOVDQA    XMM3, XMM1                    // XMM3 <- 2*QWord(XMM1)
        PSRLQ     XMM3, 16                      // XMM3 <- 00 00 00 Fa 00 Fa 00 Fa
        PMULLW    XMM0, XMM3                    // XMM0 <- Frgb * Fa
        PADDW     XMM0, XMM5                    // XMM0 <- Frgb * Fa + Bias
        PSRLW     XMM0, 8                       // XMM0 <- (Frgb * Fa + Bias) shr 8
        PSLLQ     XMM3, 48                      // XMM3 <- 00 Fa 00 00 00 00 00 00
        POR       XMM0, XMM3                    // XMM0 <- 00 Fa 00 FR 00 FG 00 FB

  // Save alpha multiplier
        MOVDQA    XMM3, XMM1

  // Test for odd/even count
        MOV       R9D, R8D
        SHR       R9D, 1                        // Get number of double pixels
        TEST      R9D, R9D
        JZ        @SinglePixel                  // None; We only have a single pixel

@Loop:
  // Blend two pixels at a time

  // Load dest
        MOVQ      XMM2, [RDX].QWORD
        PUNPCKLBW XMM2, XMM4

  // Blend: C' = A' + B' - aB'
        PMULLW    XMM1, XMM2
        PADDW     XMM1, XMM5
        PSRLW     XMM1, 8
        PADDW     XMM2, XMM0
        PSUBW     XMM2, XMM1

  // Restore alpha multiplier
        MOVDQA    XMM1, XMM3

  // Store dest
        PACKUSWB  XMM2, XMM4
        MOVQ      [RDX].QWORD, XMM2

        ADD       RDX, 8
        SUB       R9D, 1
        JNZ       @Loop

@SinglePixel:
        AND       R8D, 1
        JZ        @Done

  // Blend a single pixel

  // Load dest
        MOVD      XMM2, [RDX]
        PUNPCKLBW XMM2, XMM4

  // Blend: C' = A'  B' - aB'
        PMULLW    XMM1, XMM2
        PADDW     XMM1, XMM5
        PSRLW     XMM1, 8
        PADDW     XMM0, XMM2
        PSUBW     XMM0, XMM1

  // Store dest
        PACKUSWB  XMM0, XMM4
        MOVD      [RDX], XMM0

@Done:
        RET

@FillWithSource:
  // Shuffle registers for FillLongword
        MOV       EAX, ECX
        MOV       RCX, RDX
        MOV       EDX, R8D
        MOV       R8D, EAX

{$IFNDEF FPC}
        CALL      FillLongword // RCX:Dest, EDX:Count, R8D:Value
{$ELSE}
        CALL      [rip+FillLongword] // RCX:Dest, EDX:Count, R8D:Value
{$ENDIF}

{$ifend}
end;


//------------------------------------------------------------------------------
// BlendLineEx
//------------------------------------------------------------------------------
procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: Cardinal); {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX,ECX
        JLE       @4

        PUSH      ESI
        PUSH      EDI
        PUSH      EBX

        MOV       ESI,EAX         // ESI <- Src
        MOV       EDI,EDX         // EDI <- Dst
        MOV       EDX,M           // EDX <- Master Alpha

  // loop start
@1:     MOV       EAX,[ESI]
        TEST      EAX,$FF000000
        JZ        @3             // complete transparency, proceed to next point
        MOV       EBX,EAX
        SHR       EBX,24
        INC       EBX            // 255:256 range bias
        IMUL      EBX,EDX
        SHR       EBX,8
        JZ        @3             // complete transparency, proceed to next point

  // blend
        PXOR      XMM0,XMM0
        MOVD      XMM1,EAX
        SHL       EBX,4
        MOVD      XMM2,[EDI]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
        ADD       EBX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[EBX]
        PSLLW     XMM2,8
        MOV       EBX,bias_ptr
        PADDW     XMM2,[EBX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EBX
        POP       EDI
        POP       ESI
@4:

{$elseif defined(TARGET_x64)}

  // ECX <- Src
  // EDX <- Dst
  // R8D <- Count
  // R9D <- M

  // test the counter for zero or negativity
        TEST      R8D,R8D
        JLE        @4
        TEST      R9D,R9D
        JZ        @4

        MOV       R10,RCX         // ESI <- Src

  // loop start
@1:     MOV       ECX,[R10]
        TEST      ECX,$FF000000
        JZ        @3              // complete transparency, proceed to next point
        MOV       EAX,ECX
        SHR       EAX,24
        INC       EAX             // 255:256 range bias
        IMUL      EAX,R9D
        SHR       EAX,8
        JZ        @3              // complete transparency, proceed to next point

  // blend
        PXOR      XMM0,XMM0
        MOVD      XMM1,ECX
        SHL       EAX,4
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0
{$IFNDEF FPC}
        ADD       RAX,alpha_ptr
{$ELSE}
        ADD       RAX,[RIP+alpha_ptr]
{$ENDIF}
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[RAX]
        PSLLW     XMM2,8
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PADDW     XMM2,[RAX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      ECX,XMM1

@2:     MOV       [RDX],ECX

@3:     ADD       R10,4
        ADD       RDX,4

  // loop end
        DEC       R8D
        JNZ       @1
@4:

{$ifend}
end;


//------------------------------------------------------------------------------
//
//      Combine
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// CombineReg
//------------------------------------------------------------------------------
function CombineReg_SSE2(X, Y: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      XMM1,EAX
        PXOR      XMM0,XMM0
        SHL       ECX,4

        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        ADD       ECX,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[ECX]
        PSLLW     XMM2,8

        MOV       ECX,bias_ptr

        PADDW     XMM2,[ECX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1

{$elseif defined(TARGET_x64)}

  // ECX - Color X
  // EDX - Color Y
  // R8D - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      XMM1,ECX
        PXOR      XMM0,XMM0
        SHL       R8D,4

        MOVD      XMM2,EDX
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8

{$IFNDEF FPC}
        MOV       R8,bias_ptr
{$ELSE}
        MOV       R8,[RIP+bias_ptr]
{$ENDIF}

        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1

{$ifend}
end;


//------------------------------------------------------------------------------
// CombineMem
//------------------------------------------------------------------------------

procedure CombineMem_SSE2_Table(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
(*
Contributed by: Christian-W. Budde

TestCombineMem:
Errors: 39.082 = 29,8 % (Limit: -1)
Differences: 92.042
Average difference: -0,29
Max difference: 1 (Limit: 1)
*)
asm
  //
  // Result := W * (X - Y) + Y
  //
  // Approximates (x div 255) as ((x + 128) div 256)
  //

{$if defined(TARGET_x86)}

  // EAX        Color X
  // [EDX]      Color Y
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x

        // Load ColorX and ColorY
        MOVD      XMM1, EAX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [EDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)

        // Create a Zero for use in unpack
        PXOR      XMM0, XMM0                    // XMM0 <- 0

        SHL       ECX, 4                        // ECX <- Offset into AlphaTable

        // Unpack the ColorX and ColorY WORDs into DWORDs
        PUNPCKLBW XMM1, XMM0                    // XMM1.high <- 0       (00 Fa 00 Fr 00 Fg 00 Fb)
        PUNPCKLBW XMM2, XMM0                    // XMM2.high <- 0       (00 Ba 00 Br 00 Bg 00 Bb)

        ADD       ECX, alpha_ptr                // ECX <- &AlphaTable[Weight]

        // Lerp: Result = (Weight * (ColorX - ColorY) + 256 * ColorY) / 256
        PSUBW     XMM1, XMM2                    // XMM1 <- ColorX - ColorY
        PMULLW    XMM1, [ECX]                   // XMM1 <- (ColorX - ColorY) * AlphaTable[Weight]

        PSLLW     XMM2, 8                       // XMM2 <- ColorY * 256
        MOV       ECX, bias_ptr                 // ECX <- AlphaTable[128] (= $00800080 = 0.5)
        PADDW     XMM2, [ECX]                   // XMM2 <- (ColorY * 256) + 128

        PADDW     XMM1, XMM2                    // XMM1 <- (ColorX - ColorY) * Weight + ColorY
        PSRLW     XMM1, 8                       // XMM1 <- XMM1 div 256

        // Pack result back from word to byte components
        PACKUSWB  XMM1, XMM0                    // XMM1 <- XMM1.low     (Ra Rr Rg Rb)
        MOVD      [EDX], XMM1                   // ColorY <- XMM1

@exit:
        RET

@return_x:
        MOV       [EDX], EAX                    // ColorY <- ColorX

{$elseif defined(TARGET_x64)}

  // ECX - Color X
  // [RDX] - Color Y
  // R8D - Weight of X [0..255]

        TEST      R8D,R8D            // Set flags for R8
        JZ        @1                 // W = 0 ?  => Result := EDX
        CMP       R8D,$FF
        JZ        @2

        MOVD      XMM1,ECX
        PXOR      XMM0,XMM0

        SHL       R8D,4

        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8

{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}

        PADDW     XMM2,[RAX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [RDX],XMM1

@1:     RET

@2:     MOV       [RDX],ECX

{$ifend}
end;

//------------------------------------------------------------------------------

procedure CombineMem_SSE2_128(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
(*
Contributed by: Anders Melander

Basically the same as CombineMem_SSE2_Table but uses immediate loads instead of tables.

TestCombineMem:
Errors: 39.082 = 29,8 % (Limit: -1)
Differences: 92.042
Average difference: -0,29
Max difference: 1 (Limit: 1)
*)
asm
  //
  // Result := W * (X - Y) + Y
  //
  // Approximates (x div 255) as ((x + 128) div 256)
  //

{$if defined(TARGET_x86)}

  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$elseif defined(TARGET_x64)}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$ifend}

        // Load ColorX and ColorY
{$if defined(TARGET_x86)}
        MOVD      XMM1, EAX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [EDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$elseif defined(TARGET_x64)}
        MOVD      XMM1, ECX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [RDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ifend}

        // Duplicate weight into 4 words
{$if defined(TARGET_x86)}
        MOVD      XMM3, ECX                     // XMM3 <- Weight       (00 00 00 00 00 00 00 WW)
{$elseif defined(TARGET_x64)}
        MOVD      XMM3, R8D                     // XMM3 <- Weight       (00 00 00 00 00 00 00 WW)
{$ifend}
        PSHUFLW   XMM3, XMM3, 0                 //                      (00 WW 00 WW 00 WW 00 WW)

        // Duplicate 128 into 4 words for saturated biasing
        MOV       ECX, 128
        MOVD      XMM4, ECX                     // XMM4 <- 0            (00 00 00 00 00 00 00 80)
        PSHUFLW   XMM4, XMM4, 0                 //                      (00 80 00 80 00 80 00 80)

        // Create a Zero for use in unpack
        PXOR      XMM0, XMM0                    // XMM0 <- 0

        // Unpack the ColorX and ColorY byte components into words
        PUNPCKLBW XMM1, XMM0                    // XMM1.high <- 0       (00 Fa 00 Fr 00 Fg 00 Fb)
        PUNPCKLBW XMM2, XMM0                    // XMM2.high <- 0       (00 Ba 00 Br 00 Bg 00 Bb)

        // Save a copy of ColorY*256
        MOVQ      XMM0, XMM2
        PSLLW     XMM0, 8                       // XMM0 <-              (Ba 00 Br 00 Bg 00 Bb 00)

        // Lerp: Result = (weight * (ColorX - ColorY) + 256 * ColorY) / 256
        PSUBW     XMM1, XMM2                    // XMM1 <- ColorX - ColorY
        PMULLW    XMM1, XMM3                    // XMM1 <- Weight * (ColorX - ColorY)
        PADDW     XMM1, XMM0                    // XMM1 <- Weight * (ColorX - ColorY) + 256 * ColorY
        // Add 255:256 correction bias
        PADDW     XMM1, XMM4                    // XMM1 <- Weight * (ColorX - ColorY) + 256 * ColorY + 128
        PSRLW     XMM1, 8                       // XMM1 <- (Weight * (ColorX - ColorY) + 256 * ColorY) div 256

        // Pack result back from word to byte components
        PACKUSWB  XMM1, XMM1                    // XMM1 <- XMM1.low     (Ra Rr Rg Rb)
{$if defined(TARGET_x86)}
        MOVD      [EDX], XMM1                   // ColorY <- XMM1
{$elseif defined(TARGET_x64)}
        MOVD      [RDX], XMM1                   // ColorY <- XMM1
{$ifend}

@exit:
        RET

@return_x:
{$if defined(TARGET_x86)}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$elseif defined(TARGET_x64)}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ifend}
end;

//------------------------------------------------------------------------------

procedure CombineMem_SSE41_8081(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
(*
Contributed by: Anders Melander

Based on CombineMem_SSE41_Kadaif but uses immediate loads instead of tables.
Also uses a slight different bias value.
Also slower :-(

TestCombineMem:
Errors: 20 = 0,0 % (Limit: -1)
Differences: 38
Average difference: -0,05
Max difference: 1 (Limit: 1)
*)
asm
  //
  // Result := W * (X - Y) + Y
  //
  // Approximates Round(x / 255) as (((x + $7F) * $8081) shr 23) = ((x * $8081 + Bias) shr 23)
  //

{$if defined(TARGET_x86)}

  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$elseif defined(TARGET_x64)}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       R8D, $FF
        JZ        @return_x
{$ifend}

        // Load ColorX and ColorY
{$if defined(TARGET_x86)}
        MOVD      XMM1, EAX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [EDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$elseif defined(TARGET_x64)}
        MOVD      XMM1, ECX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [RDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ifend}

        // Duplicate weight*$8081 into 4 dwords
{$if defined(TARGET_x86)}
        IMUL      ECX, ECX, $8081
{$elseif defined(TARGET_x64)}
        IMUL      ECX, R8D, $8081
{$ifend}
        MOVD      XMM3, ECX                     // XMM3 <- Weight * $8081
        PSHUFD    XMM3, XMM3, 0                 // XMM3[0..3] <- XMM3[0][0..3]

        // Unpack the ColorX and ColorY byte components into dwords
        // PMOVZXBD is SSE4.1
        PMOVZXBD  XMM1, XMM1                    // XMM1[0..3] <- ColorX[0][0..3]
        PMOVZXBD  XMM0, XMM2                    // XMM0[0..3] <- ColorY[0][0..3]


        //
        // Lerp: Result = (weight * (ColorX - ColorY) + ColorY)
        //              = (($8081 * weight * (ColorX - ColorY)) shr 23 + ColorY)
        //
        PSUBD     XMM1, XMM0                    // XMM1 <- ColorX - ColorY
        PMULLD    XMM1, XMM3                    // XMM1 <- (ColorX - ColorY) * Weight * $8081

        // Duplicate bias (~$7F*$8081) into 4 dwords
        MOV       ECX, $003FFF0F
        MOVD      XMM3, ECX                     // XMM3 <- Bias
        PSHUFD    XMM3, XMM3, 0                 // XMM3[0..3] <- XMM3[0][0..3]
        // Add bias
        PADDD     XMM1, XMM3                    // XMM2 <- (ColorX - ColorY) * Weight * $8081 + Bias
        // Reduce 32-bits to 9-bits
        PSRLD     XMM1, 23                      // XMM2 <- ((ColorX - ColorY) * Weight * $8081 + Bias) shr 23

        // PACKUSDW is SSE4.1
        // Convert from dwords to words
        PACKUSDW  XMM1, XMM0                    // XMM1[0..1][0..1] <- XMM1[0..3]
        // Convert from words.lo to bytes
        PSLLW     XMM1, 8                       // Get rid of the high byte
        PSRLW     XMM1, 8
        PACKUSWB  XMM1, XMM0                    // XMM1[0][0..3] <- XMM1[0..1][0..1]

        // Result := Value + ColorY
        PADDB     XMM1, XMM2                    // XMM0 <- XMM2 + ColorY
{$if defined(TARGET_x86)}
        MOVD      [EDX], XMM1                   // ColorY <- XMM1
{$elseif defined(TARGET_x64)}
        MOVD      [RDX], XMM1                   // ColorY <- XMM1
{$ifend}

@exit:
        RET

@return_x:
{$if defined(TARGET_x86)}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$elseif defined(TARGET_x64)}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ifend}
end;

//------------------------------------------------------------------------------

procedure CombineMem_SSE41_Kadaif(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
(*
Contributed by: Kadaif

TestCombineMem:
Errors: 16 = 0,0 % (Limit: -1)
Differences: 30
Average difference: 0,20
Max difference: 1 (Limit: 1)
*)
asm
  //
  // Result := W * (X - Y) + Y
  //
  // Approximates Round(x / 255) as ((x * $8081 + Bias) shr 23)
  //

{$if defined(TARGET_x86)}
  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$elseif defined(TARGET_x64)}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       R8D, $FF
        JZ        @return_x
{$ifend}

        // Load ColorX and ColorY
{$if defined(TARGET_x86)}
        MOVD      XMM0, EAX                     // XMM0 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM1, [EDX]                   // XMM1 <- ColorY       (Ba Br Bg Bb)
{$elseif defined(TARGET_x64)}
        MOVD      XMM0, ECX                     // XMM0 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM1, [RDX]                   // XMM1 <- ColorY       (Ba Br Bg Bb)
{$ifend}

        // Weight = Weight * $8081
{$if defined(TARGET_x86)}
        IMUL      ECX, ECX, $8081
{$elseif defined(TARGET_x64)}
        IMUL      ECX, R8D, $8081
{$ifend}

        // Convert from bytes to integers
        // PMOVZXBD is SSE4.1
        PMOVZXBD  XMM2, XMM1                    // XMM2[0..3] <- ColorY[0][0..3]
        PMOVZXBD  XMM0, XMM0                    // XMM0[0..3] <- ColorX[0][0..3]

        //
        // Lerp: Result = (weight * (ColorX - ColorY) + ColorY)
        //              = (($8081 * weight * (ColorX - ColorY)) shr 23 + ColorY)
        //
        PSUBD     XMM0, XMM2                    // XMM0 <- ColorX - ColorY

        MOVD      XMM2, ECX                     // XMM2 <- Weight * $8081
        PSHUFD    XMM2, XMM2, 0                 // XMM2[0..3] <- XMM2[0][0..3]

        PMULLD    XMM2, XMM0                    // XMM2 <- (ColorX - ColorY) * Weight * $8081

        // Add bias (~$7F*$8081)
{$if (not defined(FPC)) or (not defined(TARGET_X64))}
        PADDD     XMM2, DQWORD PTR [SSE_003FFF7F_ALIGNED] // XMM2 <- ((ColorX - ColorY) * Weight * $8081) + Bias
{$else}
        PADDD     XMM2, DQWORD PTR [rip+SSE_003FFF7F_ALIGNED]
{$ifend}

        // Reduce 32-bits to 9-bits
        PSRLD     XMM2, 23                      // XMM2 <- (((ColorX - ColorY) * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
{$if (not defined(FPC)) or (not defined(TARGET_X64))}
        PSHUFB    XMM2, DQWORD PTR [SSE_0C080400_ALIGNED] // XMM2[0] <- XMM4[0..3][0]
{$else}
        PSHUFB    XMM2, DQWORD PTR [rip+SSE_0C080400_ALIGNED]
{$ifend}

        // Result := Value + ColorY
        PADDB     XMM2, XMM1                    // XMM2 <- XMM2 + ColorY
{$if defined(TARGET_x86)}
        MOVD      [EDX], XMM2                   // ColorY <- XMM2
{$elseif defined(TARGET_x64)}
        MOVD      [RDX], XMM2                   // ColorY <- XMM2
{$ifend}

@exit:
        RET

@return_x:
{$if defined(TARGET_x86)}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$elseif defined(TARGET_x64)}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ifend}
end;


//------------------------------------------------------------------------------
// CombineLine
//------------------------------------------------------------------------------
procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: Cardinal); {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // Result := W * (X - Y) + Y

        TEST      ECX,ECX
        JZ        @3

        PUSH      EBX
        MOV       EBX,W

        TEST      EBX,EBX
        JZ        @2

        CMP       EBX,$FF
        JZ        @4

        SHL       EBX,4
        ADD       EBX,alpha_ptr
        MOVQ      XMM3,[EBX]
        MOV       EBX,bias_ptr
        MOVQ      XMM4,[EBX]
        PXOR      XMM0,XMM0

@1:     MOVD      XMM1,[EAX]
        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,XMM3
        PSLLW     XMM2,8

        PADDW     XMM2,XMM4
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [EDX],XMM1

        ADD       EAX,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1

@2:     POP       EBX
        POP       EBP

@3:     RET       $0004

@4:     SHL       ECX,2
        CALL      Move
        POP       EBX

{$elseif defined(TARGET_x64)}

  // ECX <- Src
  // EDX <- Dst
  // R8D <- Count

  // Result := W * (X - Y) + Y

        TEST      R8D,R8D
        JZ        @2

        TEST      R9D,R9D
        JZ        @2

        CMP       R9D,$FF
        JZ        @3

        SHL       R9D,4
{$IFNDEF FPC}
        ADD       R9,alpha_ptr
{$ELSE}
        ADD       R9,[RIP+alpha_ptr]
{$ENDIF}
        MOVQ      XMM3,[R9]
{$IFNDEF FPC}
        MOV       R9,bias_ptr
{$ELSE}
        MOV       R9,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        MOVQ      XMM4,[R9]
        PXOR      XMM0,XMM0

@1:     MOVD      XMM1,[RCX]
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM1,XMM0
        PUNPCKLBW XMM2,XMM0

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,XMM3
        PSLLW     XMM2,8

        PADDW     XMM2,XMM4
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [RDX],XMM1

        ADD       RCX,4
        ADD       RDX,4

        DEC       R8D
        JNZ       @1

@2:     RET

@3:     SHL       R8D,2
        CALL      Move

{$ifend}
end;


//------------------------------------------------------------------------------
//
//      Merge
//
//------------------------------------------------------------------------------
(*
  This is an implementation of the merge formula, as described
    in a paper by Bruce Wallace in 1981. Merging is associative,
    that is, A over (B over C) = (A over B) over C. The formula is,

      Ra = Fa + Ba * (1 - Fa)
      Rc = (Fa * (Fc - Bc * Ba) + Bc * Ba) / Ra

    where

      Rc is the resultant color,
      Ra is the resultant alpha,
      Fc is the foreground color,
      Fa is the foreground alpha,
      Bc is the background color,
      Ba is the background alpha.

    Implementation:

      Ra := 1 - (1 - Fa) * (1 - Ba);
      Wa := Fa / Ra;
      Rc := Bc + Wa * (Fc - Bc);

      (1 - Fa) * (1 - Ba) = 1 - Fa - Ba + Fa * Ba = (1 - Ra)
*)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MergeReg
//------------------------------------------------------------------------------
function MergeReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        TEST      EAX,$FF000000  // foreground completely transparent =>
        JZ        @1             // result = background
        CMP       EAX,$FF000000  // foreground completely opaque =>
        JNC       @2             // result = foreground
        TEST      EDX,$FF000000  // background completely transparent =>
        JZ        @2             // result = foreground

        PXOR      XMM7,XMM7       // XMM7  <-  00
        MOVD      XMM0,EAX        // XMM0  <-  Fa Fr Fg Fb
        SHR       EAX,24          //  EAX  <-  Fa
        ROR       EDX,24
        MOVZX     ECX,DL          //  ECX  <-  Ba
        PUNPCKLBW XMM0,XMM7       // XMM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
        SUB       EAX,$FF         //  EAX  <-  (Fa - 1)
        XOR       ECX,$FF         //  ECX  <-  (1 - Ba)
        IMUL      ECX,EAX         //  ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
        IMUL      ECX,$8081       //  ECX  <-  Xa 00 00 00
        ADD       ECX,$8081*$FF*$FF
        SHR       ECX,15          //  ECX  <-  Ra
        MOV       DL,CH           //  EDX  <-  Br Bg Bb Ra
        ROR       EDX,8           //  EDX  <-  Ra Br Bg Bb
        MOVD      XMM1,EDX        // XMM1  <-  Ra Br Bg Bb
        PUNPCKLBW XMM1,XMM7       // XMM1  <-  00 Ra 00 Br 00 Bg 00 Bb
        SHL       EAX,20          //  EAX  <-  Fa 00 00
        PSUBW     XMM0,XMM1       // XMM0  <-  ** Da ** Dr ** Dg ** Db
        ADD       EAX,$0FF01000
        PSLLW     XMM0,4
        XOR       EDX,EDX         //  EDX  <-  00
        DIV       ECX             //  EAX  <-  Fa / Ra  =  Wa
        MOVD      XMM4,EAX        // XMM3  <-  Wa
        PSHUFLW   XMM4,XMM4,$C0   // XMM3  <-  00 00 ** Wa ** Wa ** Wa
        PMULHW    XMM0,XMM4       // XMM0  <-  00 00 ** Pr ** Pg ** Pb
        PADDW     XMM0,XMM1       // XMM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
        PACKUSWB  XMM0,XMM7       // XMM0  <-  Ra Rr Rg Rb
        MOVD      EAX,XMM0

        RET
@1:     MOV       EAX,EDX
@2:

{$elseif defined(TARGET_x64)}

        TEST      ECX,$FF000000   // foreground completely transparent =>
        JZ        @1              // result = background
        MOV       EAX,ECX         //  EAX  <-  Fa
        CMP       EAX,$FF000000   // foreground completely opaque =>
        JNC       @2              // result = foreground
        TEST      EDX,$FF000000   // background completely transparent =>
        JZ        @2              // result = foreground

        PXOR      XMM7,XMM7       // XMM7  <-  00
        MOVD      XMM0,EAX        // XMM0  <-  Fa Fr Fg Fb
        SHR       EAX,24          //  EAX  <-  Fa
        ROR       EDX,24
        MOVZX     ECX,DL          //  ECX  <-  Ba
        PUNPCKLBW XMM0,XMM7       // XMM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
        SUB       EAX,$FF         //  EAX  <-  (Fa - 1)
        XOR       ECX,$FF         //  ECX  <-  (1 - Ba)
        IMUL      ECX,EAX         //  ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
        IMUL      ECX,$8081       //  ECX  <-  Xa 00 00 00
        ADD       ECX,$8081*$FF*$FF
        SHR       ECX,15          //  ECX  <-  Ra
        MOV       DL,CH           //  EDX  <-  Br Bg Bb Ra
        ROR       EDX,8           //  EDX  <-  Ra Br Bg Bb
        MOVD      XMM1,EDX        // XMM1  <-  Ra Br Bg Bb
        PUNPCKLBW XMM1,XMM7       // XMM1  <-  00 Ra 00 Br 00 Bg 00 Bb
        SHL       EAX,20          //  EAX  <-  Fa 00 00
        PSUBW     XMM0,XMM1       // XMM0  <-  ** Da ** Dr ** Dg ** Db
        ADD       EAX,$0FF01000
        PSLLW     XMM0,4
        XOR       EDX,EDX         //  EDX  <-  00
        DIV       ECX             //  EAX  <-  Fa / Ra  =  Wa
        MOVD      XMM4,EAX        // XMM3  <-  Wa
        PSHUFLW   XMM4,XMM4,$C0   // XMM3  <-  00 00 ** Wa ** Wa ** Wa
        PMULHW    XMM0,XMM4       // XMM0  <-  00 00 ** Pr ** Pg ** Pb
        PADDW     XMM0,XMM1       // XMM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
        PACKUSWB  XMM0,XMM7       // XMM0  <-  Ra Rr Rg Rb
        MOVD      EAX,XMM0

        RET
@1:     MOV       EAX,EDX
@2:

{$ifend}
end;


//------------------------------------------------------------------------------
// MergeReg
//------------------------------------------------------------------------------
{$if not defined(FPC)}
function MergeReg_SSE41(F, B: TColor32): TColor32;
asm
{$if defined(TARGET_X86)}
  //
  // Parameters (x86):
  //   EAX <- F (Foreground)
  //   EDX <- B (Background)
  //
  // SSE register usage:
  //   XMM0:
  //   XMM1:
  //   XMM2:
  //   XMM3:
  //   XMM4:
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        TEST      EAX, $FF000000   // Foreground completely transparent?
        JZ        @ReturnB         // Yes, result = background
        CMP       EAX, $FF000000   // Foreground completely opaque?
        JNC       @Exit            // Yes, result = foreground (EAX)
        TEST      EDX, $FF000000   // Background completely transparent?
        JZ        @Exit            // Yes, result = foreground (EAX)

        PUSH      EBX
        PUSH      ESI

        // Load Bias ($0080 per word)
        MOV       ESI, bias_ptr
        MOVDQA    XMM5, [ESI]

        PXOR      XMM4, XMM4       // Zero
        MOVD      XMM0, EAX        // XMM0 <- [0 0 0 0 | Fa Fr Fg Fb]
        MOVD      XMM1, EDX        // XMM1 <- [0 0 0 0 | Ba Br Bg Bb]

        // Unpack pixels to 16-bit words using SSE4.1 zero-extension
        PMOVZXBW  XMM0, XMM0       // XMM0 <- [00Fa 00Fr 00Fg 00Fb]
        PMOVZXBW  XMM1, XMM1       // XMM1 <- [00Ba 00Br 00Bg 00Bb]

        // Calculate Result Alpha: Ra = Fa + Ba * (255 - Fa) / 255
        // Ra = 255 - Round((255 - Fa) * (255 - Ba) / 255)
        PEXTRW    EAX, XMM0, 3     // EAX <- Fa
        PEXTRW    ECX, XMM1, 3     // ECX <- Ba
        MOV       EBX, EAX         // EBX <- Fa (save)
        XOR       EAX, $FF         // EAX <- (255 - Fa)
        XOR       ECX, $FF         // ECX <- (255 - Ba)
        IMUL      EAX, ECX         // EAX <- (255 - Fa) * (255 - Ba) = y

        // div 255 trick: Ra = 255 - Round( y / 255 )
        ADD       EAX, 128
        MOV       ECX, EAX
        SHR       ECX, 8
        ADD       EAX, ECX
        SHR       EAX, 8           // EAX <- Round(y / 255)
        XOR       EAX, $FF         // EAX <- Ra (Result Alpha)

        // Calculate Weight: Wa = Round(Fa * 255 / Ra)
        SHL       EAX, 8
        LEA       ESI, [EAX + DivMul255Table]
        MOVZX     ESI, BYTE PTR [ESI + EBX] // ESI <- Wa
        SHR       EAX, 8           // Restore Ra

        // Result Color: Rc = Bc + Wa * (Fc - Bc) / 255
        // We use the approximation: ( (F - B) * Wa + B * 256 + 128 ) >> 8
        MOVD      XMM3, ESI
        PSHUFLW   XMM3, XMM3, $00  // XMM3 <- [Wa Wa Wa Wa]
        PSUBW     XMM0, XMM1       // XMM0 <- [Fa-Ba Fr-Br Fg-Bg Fb-Bb]
        PMULLW    XMM0, XMM3       // XMM0 <- Wa * (Fc - Bc)
        PSLLW     XMM1, 8          // XMM1 <- B * 256
        PADDW     XMM1, XMM5       // XMM1 <- B * 256 + 128
        PADDW     XMM0, XMM1       // XMM0 <- Wa * (Fc - Bc) + B * 256 + 128
        PSRLW     XMM0, 8

        // Correct the Alpha channel: Insert the pre-calculated Ra
        PINSRW    XMM0, EAX, 3     // XMM0.word[3] <- Ra

        PACKUSWB  XMM0, XMM4
        MOVD      EAX, XMM0        // Result color in EAX

        POP       ESI
        POP       EBX
        JMP       @Exit

@ReturnB:
        MOV       EAX, EDX
@Exit:

{$elseif defined(TARGET_X64)}
  //
  // Parameters (x64):
  //   RCX <- F (Foreground)
  //   RDX <- B (Background)
  //
  // SSE register usage:
  //   XMM0:
  //   XMM1:
  //   XMM2:
  //   XMM3:
  //   XMM4:
  //   XMM5:
  //

        TEST      ECX, $FF000000   // Foreground completely transparent?
        JZ        @ReturnB         // Yes, result = background
        CMP       ECX, $FF000000   // Foreground completely opaque?
        JNC       @ReturnF         // Yes, result = foreground
        TEST      EDX, $FF000000   // Background completely transparent?
        JZ        @ReturnF         // Yes, result = foreground

        MOVD      XMM0, ECX        // XMM0 <- [0 0 0 0 | Fa Fr Fg Fb]
        MOVD      XMM1, EDX        // XMM1 <- [0 0 0 0 | Ba Br Bg Bb]

        // Unpack pixels to 16-bit words using SSE4.1 zero-extension
        PMOVZXBW  XMM0, XMM0       // XMM0 <- [00Fa 00Fr 00Fg 00Fb]
        PMOVZXBW  XMM1, XMM1       // XMM1 <- [00Ba 00Br 00Bg 00Bb]

        // Calculate Result Alpha: Ra = 255 - Round((255 - Fa) * (255 - Ba) / 255)
        PEXTRW    EAX, XMM0, 3     // EAX <- Fa
        PEXTRW    R8D, XMM1, 3     // R8D <- Ba
        MOV       R9D, EAX         // R9D <- Fa (save)
        XOR       EAX, 255         // EAX <- (255 - Fa)
        XOR       R8D, 255         // R8D <- (255 - Ba)
        IMUL      EAX, R8D         // EAX <- (255 - Fa) * (255 - Ba) = y

        // div 255 trick: Ra = 255 - Round( y / 255 )
        ADD       EAX, 128
        MOV       R8D, EAX
        SHR       R8D, 8
        ADD       EAX, R8D
        SHR       EAX, 8           // EAX <- Round(y / 255)
        XOR       EAX, 255         // EAX <- Ra (Result Alpha)

        MOV       R11D, EAX        // R11D <- Ra (save)

        // Calculate Weight: Wa = Round(Fa * 255 / Ra)
        SHL       RAX, 8
        ADD       RAX, R9          // RAX = Ra * 256 + Fa
        LEA       R10, [DivMul255Table]
        MOVZX     R10D, BYTE PTR [R10 + RAX] // R10D <- Wa

        // Result Color: Rc = Bc + Wa * (Fc - Bc) / 255
        MOVD      XMM3, R10D
        PSHUFLW   XMM3, XMM3, $00  // XMM3 <- [Wa Wa Wa Wa]
        PSUBW     XMM0, XMM1       // XMM0 <- [Fa-Ba Fr-Br Fg-Bg Fb-Bb]
        PMULLW    XMM0, XMM3       // XMM0 <- Wa * (Fc - Bc)
        PSLLW     XMM1, 8          // XMM1 <- B * 256

        // Create 0x0080 in XMM2 for rounding (using XMM2 to avoid preservation)
        PCMPEQW   XMM2, XMM2
        PSRLW     XMM2, 15
        PSLLW     XMM2, 7

        PADDW     XMM1, XMM2       // XMM1 <- B * 256 + 128
        PADDW     XMM0, XMM1       // XMM0 <- Wa * (Fc - Bc) + B * 256 + 128
        PSRLW     XMM0, 8

        // Correct the Alpha channel: Insert the pre-calculated Ra
        PINSRW    XMM0, R11D, 3    // XMM0.word[3] <- Ra

        PXOR      XMM2, XMM2       // Zero
        PACKUSWB  XMM0, XMM2
        MOVD      EAX, XMM0        // Result color in EAX
        JMP       @Exit

@ReturnB:
        MOV       EAX, EDX
        JMP       @Exit

@ReturnF:
        MOV       EAX, ECX
@Exit:
{$ifend}

end;
{$ifend}

//------------------------------------------------------------------------------
// MergeMem
//------------------------------------------------------------------------------
{$if not defined(FPC)}
procedure MergeMem_SSE41(F: TColor32; var B: TColor32);
asm
{$if defined(TARGET_X86)}
  //
  // Parameters (x86):
  //   EAX <- F (Foreground)
  //   EDX <- B (Background pointer)
  //
  // SSE register usage:
  //   XMM0:
  //   XMM1:
  //   XMM2:
  //   XMM3:
  //   XMM4:
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        TEST      EAX, $FF000000   // Foreground completely transparent?
        JZ        @Exit            // Yes, result = background (no-op)
        CMP       EAX, $FF000000   // Foreground completely opaque?
        JNC       @Opaque          // Yes, result = foreground
        MOV       ECX, [EDX]       // ECX <- Background pixel
        TEST      ECX, $FF000000   // Background completely transparent?
        JZ        @Opaque          // Yes, result = foreground

        PUSH      EBX
        PUSH      ESI

        // Load Bias ($0080 per word)
        MOV       ESI, bias_ptr
        MOVDQA    XMM5, [ESI]

        PXOR      XMM4, XMM4       // Zero
        MOVD      XMM0, EAX        // XMM0 <- [0 0 0 0 | Fa Fr Fg Fb]
        MOVD      XMM1, ECX        // XMM1 <- [0 0 0 0 | Ba Br Bg Bb]

        // Unpack pixels to 16-bit words using SSE4.1 zero-extension
        PMOVZXBW  XMM0, XMM0       // XMM0 <- [00Fa 00Fr 00Fg 00Fb]
        PMOVZXBW  XMM1, XMM1       // XMM1 <- [00Ba 00Br 00Bg 00Bb]

        // Calculate Result Alpha: Ra = Fa + Ba * (255 - Fa) / 255
        // Ra = 255 - Round((255 - Fa) * (255 - Ba) / 255)
        PEXTRW    EAX, XMM0, 3     // EAX <- Fa
        PEXTRW    ECX, XMM1, 3     // ECX <- Ba
        MOV       EBX, EAX         // EBX <- Fa (save)
        XOR       EAX, $FF         // EAX <- (255 - Fa)
        XOR       ECX, $FF         // ECX <- (255 - Ba)
        IMUL      EAX, ECX         // EAX <- (255 - Fa) * (255 - Ba) = y

        // div 255 trick: Ra = 255 - Round( y / 255 )
        ADD       EAX, 128
        MOV       ECX, EAX
        SHR       ECX, 8
        ADD       EAX, ECX
        SHR       EAX, 8           // EAX <- Round(y / 255)
        XOR       EAX, $FF         // EAX <- Ra (Result Alpha)

        // Calculate Weight: Wa = Round(Fa * 255 / Ra)
        SHL       EAX, 8
        LEA       ESI, [EAX + DivMul255Table]
        MOVZX     ESI, BYTE PTR [ESI + EBX] // ESI <- DivMul255Table[Ra, Fa] = Wa
        SHR       EAX, 8           // Restore Ra

        // Result Color: Rc = Bc + Wa * (Fc - Bc) / 255
        // We use the approximation: ( (F - B) * Wa + B * 256 + 128 ) >> 8
        MOVD      XMM3, ESI
        PSHUFLW   XMM3, XMM3, $00  // XMM3 <- [Wa Wa Wa Wa]
        PSUBW     XMM0, XMM1       // XMM0 <- [Fa-Ba Fr-Br Fg-Bg Fb-Bb]
        PMULLW    XMM0, XMM3       // XMM0 <- Wa * (Fc - Bc) (low 16 bits)
        // PMULLW also multiplies the alpha channel: Wa * (Fa - Ba)
        // This is not needed because we overwrite alpha later, but it doesn't hurt.
        PSLLW     XMM1, 8          // XMM1 <- B * 256
        PADDW     XMM1, XMM5       // XMM1 <- B * 256 + 128
        PADDW     XMM0, XMM1       // XMM0 <- Wa * (Fc - Bc) + B * 256 + 128
        PSRLW     XMM0, 8

        // Correct the Alpha channel: Insert the pre-calculated Ra
        PINSRW    XMM0, EAX, 3     // XMM0.word[3] <- Ra

        PACKUSWB  XMM0, XMM4
        MOVD      [EDX], XMM0      // Store result

        POP       ESI
        POP       EBX
        JMP       @Exit

@Opaque:
        MOV       [EDX], EAX
@Exit:

{$elseif defined(TARGET_X64)}
  //
  // Parameters (x64):
  //   RCX <- F (Foreground)
  //   RDX <- B (Background pointer)
  //
  // SSE register usage:
  //   XMM0:
  //   XMM1:
  //   XMM2:
  //   XMM3:
  //   XMM4:
  //   XMM5:
  //

        TEST      ECX, $FF000000   // Foreground completely transparent?
        JZ        @Exit            // Yes, result = background (no-op)
        CMP       ECX, $FF000000   // Foreground completely opaque?
        JNC       @Opaque          // Yes, result = foreground
        MOV       EAX, [RDX]       // EAX <- Background pixel
        TEST      EAX, $FF000000   // Background completely transparent?
        JZ        @Opaque          // Yes, result = foreground

        MOVD      XMM0, ECX        // XMM0 <- [0 0 0 0 | Fa Fr Fg Fb]
        MOVD      XMM1, [RDX]      // XMM1 <- [0 0 0 0 | Ba Br Bg Bb]

        // Unpack pixels to 16-bit words using SSE4.1 zero-extension
        PMOVZXBW  XMM0, XMM0       // XMM0 <- [00Fa 00Fr 00Fg 00Fb]
        PMOVZXBW  XMM1, XMM1       // XMM1 <- [00Ba 00Br 00Bg 00Bb]

        // Calculate Result Alpha: Ra = 255 - Round((255 - Fa) * (255 - Ba) / 255)
        PEXTRW    EAX, XMM0, 3     // EAX <- Fa
        PEXTRW    R8D, XMM1, 3     // R8D <- Ba
        MOV       R9D, EAX         // R9D <- Fa (save)
        XOR       EAX, 255         // EAX <- (255 - Fa)
        XOR       R8D, 255         // R8D <- (255 - Ba)
        IMUL      EAX, R8D         // EAX <- (255 - Fa) * (255 - Ba) = y

        // div 255 trick: Ra = 255 - Round( y / 255 )
        ADD       EAX, 128
        MOV       R8D, EAX
        SHR       R8D, 8
        ADD       EAX, R8D
        SHR       EAX, 8           // EAX <- Round(y / 255)
        XOR       EAX, 255         // EAX <- Ra (Result Alpha)

        MOV       R11D, EAX        // R11D <- Ra (save)

        // Calculate Weight: Wa = Round(Fa * 255 / Ra)
        SHL       RAX, 8
        ADD       RAX, R9          // RAX = Ra * 256 + Fa
        LEA       R10, [DivMul255Table]
        MOVZX     R10D, BYTE PTR [R10 + RAX] // R10D <- Wa

        // Result Color: Rc = Bc + Wa * (Fc - Bc) / 255
        // We use the approximation: ( (F - B) * Wa + B * 256 + 128 ) >> 8
        MOVD      XMM3, R10D
        PSHUFLW   XMM3, XMM3, $00  // XMM3 <- [Wa Wa Wa Wa]
        PSUBW     XMM0, XMM1       // XMM0 <- [Fa-Ba Fr-Br Fg-Bg Fb-Bb]
        PMULLW    XMM0, XMM3       // XMM0 <- Wa * (Fc - Bc) (low 16 bits)
        // PMULLW also multiplies the alpha channel: Wa * (Fa - Ba)
        // This is not needed because we overwrite alpha later, but it doesn't hurt.
        PSLLW     XMM1, 8          // XMM1 <- B * 256

        // Create 0x0080 in XMM2 for rounding (using XMM2 to avoid preservation)
        PCMPEQW   XMM2, XMM2
        PSRLW     XMM2, 15
        PSLLW     XMM2, 7

        PADDW     XMM1, XMM2       // XMM1 <- B * 256 + 128
        PADDW     XMM0, XMM1       // XMM0 <- Wa * (Fc - Bc) + B * 256 + 128
        PSRLW     XMM0, 8

        // Correct the Alpha channel: Insert the pre-calculated Ra
        PINSRW    XMM0, R11D, 3    // XMM0.word[3] <- Ra

        PXOR      XMM2, XMM2       // Zero
        PACKUSWB  XMM0, XMM2
        MOVD      [RDX], XMM0      // Store result
        JMP       @Exit

@Opaque:
        MOV       [RDX], ECX
@Exit:
{$ifend}

end;
{$ifend}

//------------------------------------------------------------------------------
// MergeLine
//------------------------------------------------------------------------------
{$if not defined(FPC)}
//------------------------------------------------------------------------------
// MergeLine_SSE41
//
// - Uses Bruce Wallace's merge formula:
//   Ra = Fa + Ba * (255 - Fa) / 255
//   Wa = Fa * 255 / Ra
//   Rc = Bc + Wa * (Fc - Bc) / 255
//
// - Uses PTEST for 4-pixel early exit (skipping transparent blocks or
//   copying opaque blocks).
//
// - Uses PMOVZXBW for unpacking of byte components to words.
//
// - Unrolls the main loop to process 4 pixels per iteration.
//
// - FPC is not supported.
//------------------------------------------------------------------------------
procedure MergeLine_SSE41(Src, Dst: PColor32; Count: Integer);
asm
{$if defined(TARGET_X86)}
  //
  // Parameters (x86):
  //   EAX <- Src
  //   EDX <- Dst
  //   ECX <- Count
  //
  // SSE register usage:
  //   XMM0: Loaded Src 4 pixels / Temp
  //   XMM1: Loaded Dst 4 pixels / Temp
  //   XMM2: Unpacked pixels / Work
  //   XMM3: Unpacked pixels / Work
  //   XMM4: Multipliers / Work
  //   XMM5: Bias ($0080 per word)
  //   XMM6: Alpha Mask ($FF000000 per dword)
  //   XMM7: Wa / Ra storage (word[0]=Wa1, word[1]=Ra1, word[2]=Wa2, word[3]=Ra2, ...)
  //

        TEST      ECX, ECX
        JLE       @Done

        PUSH      EBX
        PUSH      ESI
        PUSH      EDI
        PUSH      EBP

        MOV       ESI, [bias_ptr]
        MOVDQA    XMM5, [ESI]

        PXOR      XMM4, XMM4
        PCMPEQD   XMM6, XMM6
        PSLLD     XMM6, 24

        SUB       ECX, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [EAX]      // Load 4 source pixels
        PTEST     XMM0, XMM6
        JZ        @Next4           // All source transparent

        MOVDQU    XMM1, [EDX]      // Load 4 destination pixels
        JC        @Opaque4         // All source opaque

        // Save pointers and count for scalar operations
        PUSH      EAX
        PUSH      EDX
        PUSH      ECX

        LEA       EBP, [DivMul255Table]

        // --- Calculate Ra/Wa for all 4 pixels ---
        // Pixel 1
        PMOVZXBW  XMM2, XMM0
        PMOVZXBW  XMM3, XMM1
        PEXTRW    EAX, XMM2, 3     // Fa1
        PEXTRW    EDX, XMM3, 3     // Ba1
        MOV       EBX, EAX         // Save Fa1
        XOR       EAX, $FF
        XOR       EDX, $FF
        IMUL      EAX, EDX
        ADD       EAX, 128
        MOV       EDX, EAX
        SHR       EDX, 8
        ADD       EAX, EDX
        SHR       EAX, 8
        XOR       EAX, $FF         // Ra1
        MOV       ESI, EAX         // Save Ra1
        SHL       EAX, 8
        ADD       EAX, EBX         // Index
        MOVZX     EAX, BYTE PTR [EBP + EAX] // Wa1
        PINSRW    XMM7, EAX, 0
        PINSRW    XMM7, ESI, 1

        // Pixel 2
        PEXTRW    EAX, XMM2, 7     // Fa2
        PEXTRW    EDX, XMM3, 7     // Ba2
        MOV       EBX, EAX
        XOR       EAX, $FF
        XOR       EDX, $FF
        IMUL      EAX, EDX
        ADD       EAX, 128
        MOV       EDX, EAX
        SHR       EDX, 8
        ADD       EAX, EDX
        SHR       EAX, 8
        XOR       EAX, $FF         // Ra2
        MOV       ESI, EAX
        SHL       EAX, 8
        ADD       EAX, EBX
        MOVZX     EAX, BYTE PTR [EBP + EAX] // Wa2
        PINSRW    XMM7, EAX, 2
        PINSRW    XMM7, ESI, 3

        // Pixels 3 & 4
        PSHUFD    XMM2, XMM0, $EE
        PMOVZXBW  XMM2, XMM2
        PSHUFD    XMM3, XMM1, $EE
        PMOVZXBW  XMM3, XMM3

        // Pixel 3
        PEXTRW    EAX, XMM2, 3
        PEXTRW    EDX, XMM3, 3
        MOV       EBX, EAX
        XOR       EAX, $FF
        XOR       EDX, $FF
        IMUL      EAX, EDX
        ADD       EAX, 128
        MOV       EDX, EAX
        SHR       EDX, 8
        ADD       EAX, EDX
        SHR       EAX, 8
        XOR       EAX, $FF         // Ra3
        MOV       ESI, EAX
        SHL       EAX, 8
        ADD       EAX, EBX
        MOVZX     EAX, BYTE PTR [EBP + EAX] // Wa3
        PINSRW    XMM7, EAX, 4
        PINSRW    XMM7, ESI, 5

        // Pixel 4
        PEXTRW    EAX, XMM2, 7
        PEXTRW    EDX, XMM3, 7
        MOV       EBX, EAX
        XOR       EAX, $FF
        XOR       EDX, $FF
        IMUL      EAX, EDX
        ADD       EAX, 128
        MOV       EDX, EAX
        SHR       EDX, 8
        ADD       EAX, EDX
        SHR       EAX, 8
        XOR       EAX, $FF         // Ra4
        MOV       ESI, EAX
        SHL       EAX, 8
        ADD       EAX, EBX
        MOVZX     EAX, BYTE PTR [EBP + EAX] // Wa4
        PINSRW    XMM7, EAX, 6
        PINSRW    XMM7, ESI, 7

        // Restore pointers and count
        POP       ECX
        POP       EDX
        POP       EAX

        // --- Merge step ---
        // 1 & 2
        PMOVZXBW  XMM2, XMM0
        PMOVZXBW  XMM3, XMM1
        PSHUFLW   XMM4, XMM7, $00
        MOVDQA    XMM0, XMM7
        PSHUFLW   XMM0, XMM0, $AA
        PUNPCKLQDQ XMM4, XMM0      // Wa 1&2 multipliers
        PSUBW     XMM2, XMM3
        PMULLW    XMM2, XMM4
        PSLLW     XMM3, 8
        PADDW     XMM3, XMM5
        PADDW     XMM2, XMM3
        PSRLW     XMM2, 8
        PEXTRW    ESI, XMM7, 1
        PINSRW    XMM2, ESI, 3
        PEXTRW    ESI, XMM7, 3
        PINSRW    XMM2, ESI, 7
        MOVDQA    XMM4, XMM2       // Result 1&2 in XMM4

        // 3 & 4
        MOVDQU    XMM0, [EAX]      // Reload Src for shuffles
        PSHUFD    XMM2, XMM0, $EE
        PMOVZXBW  XMM2, XMM2
        PSHUFD    XMM3, XMM1, $EE
        PMOVZXBW  XMM3, XMM3
        PSHUFHW   XMM0, XMM7, $00
        PSHUFHW   XMM1, XMM7, $AA
        PUNPCKHQDQ XMM0, XMM1      // Wa 3&4 multipliers
        PSUBW     XMM2, XMM3
        PMULLW    XMM2, XMM0
        PSLLW     XMM3, 8
        PADDW     XMM3, XMM5
        PADDW     XMM2, XMM3
        PSRLW     XMM2, 8
        PEXTRW    ESI, XMM7, 5
        PINSRW    XMM2, ESI, 3
        PEXTRW    ESI, XMM7, 7
        PINSRW    XMM2, ESI, 7

        PACKUSWB  XMM4, XMM2
        MOVDQU    [EDX], XMM4
        MOVDQU    XMM0, [EAX]      // Reset XMM0 for Next4 check

@Next4:
        ADD       EAX, 16
        ADD       EDX, 16
        SUB       ECX, 4
        JGE       @Loop4

@Tail:
        ADD       ECX, 4
        JZ        @Exit

@TailLoop:
        MOV       EBX, [EAX]
        TEST      EBX, $FF000000
        JZ        @TailNext
        CMP       EBX, $FF000000
        JNC       @TailOpaque

        MOVD      XMM0, EBX
        MOVD      XMM1, [EDX]
        PMOVZXBW  XMM0, XMM0
        PMOVZXBW  XMM1, XMM1
        PEXTRW    ESI, XMM0, 3     // Fa
        PEXTRW    EDI, XMM1, 3     // Ba
        MOV       EBX, ESI         // Save Fa
        XOR       ESI, $FF
        XOR       EDI, $FF
        IMUL      ESI, EDI
        ADD       ESI, 128
        MOV       EDI, ESI
        SHR       EDI, 8
        ADD       ESI, EDI
        SHR       ESI, 8
        XOR       ESI, $FF         // Ra
        MOV       EBP, ESI         // Save Ra
        SHL       ESI, 8
        ADD       ESI, EBX         // Ra*256 + Fa
        LEA       EDI, [DivMul255Table]
        MOVZX     ESI, BYTE PTR [EDI + ESI] // Wa
        MOVD      XMM2, ESI
        PSHUFLW   XMM2, XMM2, $00
        PMOVZXBW  XMM1, [EDX]      // Reload Dst unpacked
        PSUBW     XMM0, XMM1
        PMULLW    XMM0, XMM2
        PSLLW     XMM1, 8
        PADDW     XMM1, XMM5
        PADDW     XMM0, XMM1
        PSRLW     XMM0, 8
        PINSRW    XMM0, EBP, 3     // Insert Ra
        PXOR      XMM2, XMM2
        PACKUSWB  XMM0, XMM2
        MOVD      [EDX], XMM0
        JMP       @TailNext

@Opaque4:
        MOVDQU    [EDX], XMM0
        JMP       @Next4

@TailOpaque:
        MOV       [EDX], EBX

@TailNext:
        ADD       EAX, 4
        ADD       EDX, 4
        DEC       ECX
        JNZ       @TailLoop

@Exit:
        POP       EBP
        POP       EDI
        POP       ESI
        POP       EBX
@Done:

{$elseif defined(TARGET_X64)}
  //
  // Parameters (x64):
  //   RCX <- Src
  //   RDX <- Dst
  //   R8D <- Count
  //
  // SSE register usage:
  //   XMM0: Loaded Src / Temp / Merged Result
  //   XMM1: Loaded Dst / Temp
  //   XMM2: Work register
  //   XMM3: Work / Mask
  //   XMM4: Wa / Ra storage (8 words)
  //   XMM5: Bias ($0080 per word)
  //
  // Volatile GPRs: RAX, RCX, RDX, R8, R9, R10, R11
  //
{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
{$ENDIF}

        TEST      R8D, R8D
        JLE       @Done

        PXOR      XMM4, XMM4
        MOV       RAX, [bias_ptr]
        MOVDQA    XMM5, [RAX]

        SUB       R8D, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [RCX]      // Load 4 source pixels
        PCMPEQD   XMM3, XMM3
        PSLLD     XMM3, 24         // Alpha Mask = $FF000000
        PTEST     XMM0, XMM3
        JZ        @Next4           // All source transparent

        MOVDQU    XMM1, [RDX]      // Load 4 destination pixels
        JC        @Opaque4         // All source opaque

        // --- Calculate Ra/Wa for all 4 pixels ---
        LEA       R10, [DivMul255Table]

        // Pixel 1
        PEXTRB    EAX, XMM0, 3     // Fa1
        PEXTRB    R9D, XMM1, 3     // Ba1
        MOV       R11D, EAX        // Fa1 save
        XOR       EAX, 255
        XOR       R9D, 255
        IMUL      EAX, R9D
        ADD       EAX, 128
        MOV       R9D, EAX
        SHR       R9D, 8
        ADD       EAX, R9D
        SHR       EAX, 8
        XOR       EAX, 255         // Ra1
        MOV       R9D, EAX         // Ra1 save
        SHL       RAX, 8
        ADD       RAX, R11         // Ra1 * 256 + Fa1
        MOVZX     EAX, BYTE PTR [R10 + RAX] // Wa1
        PINSRW    XMM4, EAX, 0
        PINSRW    XMM4, R9D, 1

        // Pixel 2
        PEXTRB    EAX, XMM0, 7     // Fa2
        PEXTRB    R9D, XMM1, 7     // Ba2
        MOV       R11D, EAX        // Fa2 save
        XOR       EAX, 255
        XOR       R9D, 255
        IMUL      EAX, R9D
        ADD       EAX, 128
        MOV       R9D, EAX
        SHR       R9D, 8
        ADD       EAX, R9D
        SHR       EAX, 8
        XOR       EAX, 255         // Ra2
        MOV       R9D, EAX         // Ra2 save
        SHL       RAX, 8
        ADD       RAX, R11
        MOVZX     EAX, BYTE PTR [R10 + RAX] // Wa2
        PINSRW    XMM4, EAX, 2
        PINSRW    XMM4, R9D, 3

        // Pixel 3
        PEXTRB    EAX, XMM0, 11    // Fa3
        PEXTRB    R9D, XMM1, 11    // Ba3
        MOV       R11D, EAX
        XOR       EAX, 255
        XOR       R9D, 255
        IMUL      EAX, R9D
        ADD       EAX, 128
        MOV       R9D, EAX
        SHR       R9D, 8
        ADD       EAX, R9D
        SHR       EAX, 8
        XOR       EAX, 255         // Ra3
        MOV       R9D, EAX
        SHL       RAX, 8
        ADD       RAX, R11
        MOVZX     EAX, BYTE PTR [R10 + RAX] // Wa3
        PINSRW    XMM4, EAX, 4
        PINSRW    XMM4, R9D, 5

        // Pixel 4
        PEXTRB    EAX, XMM0, 15    // Fa4
        PEXTRB    R9D, XMM1, 15    // Ba4
        MOV       R11D, EAX
        XOR       EAX, 255
        XOR       R9D, 255
        IMUL      EAX, R9D
        ADD       EAX, 128
        MOV       R9D, EAX
        SHR       R9D, 8
        ADD       EAX, R9D
        SHR       EAX, 8
        XOR       EAX, 255         // Ra4
        MOV       R9D, EAX
        SHL       RAX, 8
        ADD       RAX, R11
        MOVZX     EAX, BYTE PTR [R10 + RAX] // Wa4
        PINSRW    XMM4, EAX, 6
        PINSRW    XMM4, R9D, 7

        // --- Merge step ---
        // Merge 1&2
        PMOVZXBW  XMM2, XMM0       // Unpack Src 1&2
        PMOVZXBW  XMM3, XMM1       // Unpack Dst 1&2
        PEXTRW    EAX, XMM4, 0     // Wa1
        MOVD      XMM0, EAX
        PSHUFLW   XMM0, XMM0, 0
        PEXTRW    EAX, XMM4, 2     // Wa2
        MOVD      XMM1, EAX
        PSHUFLW   XMM1, XMM1, 0
        PUNPCKLQDQ XMM0, XMM1      // Wa 1&2 multipliers in XMM0
        PSUBW     XMM2, XMM3
        PMULLW    XMM2, XMM0
        PSLLW     XMM3, 8
        PADDW     XMM3, XMM5
        PADDW     XMM2, XMM3
        PSRLW     XMM2, 8
        PEXTRW    EAX, XMM4, 1
        PINSRW    XMM2, EAX, 3
        PEXTRW    EAX, XMM4, 3
        PINSRW    XMM2, EAX, 7
        // Result 1&2 now in XMM2. Save it to XMM1.
        MOVDQA    XMM1, XMM2

        // Merge 3&4
        MOVDQU    XMM0, [RCX]
        PSHUFD    XMM0, XMM0, $EE
        PMOVZXBW  XMM0, XMM0       // Unpack Src 3&4
        MOVDQU    XMM2, [RDX]
        PSHUFD    XMM2, XMM2, $EE
        PMOVZXBW  XMM3, XMM2       // Unpack Dst 3&4
        PEXTRW    EAX, XMM4, 4     // Wa3
        MOVD      XMM2, EAX
        PSHUFLW   XMM2, XMM2, 0
        PEXTRW    EAX, XMM4, 6     // Wa4
        // Use R9D as temp
        MOV       R9D, EAX
        PEXTRW    R10D, XMM4, 5    // Ra3
        PEXTRW    R11D, XMM4, 7    // Ra4
        MOVD      XMM4, R9D
        PSHUFLW   XMM4, XMM4, 0
        PUNPCKLQDQ XMM2, XMM4      // Wa 3&4 multipliers in XMM2

        PSUBW     XMM0, XMM3
        PMULLW    XMM0, XMM2
        PSLLW     XMM3, 8
        PADDW     XMM3, XMM5
        PADDW     XMM0, XMM3
        PSRLW     XMM0, 8
        // Insert Ra3, Ra4
        PINSRW    XMM0, R10D, 3
        PINSRW    XMM0, R11D, 7
        // Result 3&4 in XMM0. Result 1&2 in XMM1.

        PACKUSWB  XMM1, XMM0
        MOVDQU    [RDX], XMM1

        PXOR      XMM4, XMM4       // Reset XMM4 for next iteration

@Next4:
        ADD       RCX, 16
        ADD       RDX, 16
        SUB       R8D, 4
        JGE       @Loop4

@Tail:
        ADD       R8D, 4
        JZ        @Done

@TailLoop:
        // Scalar fallback
        MOV       EAX, [RCX]
        TEST      EAX, $FF000000
        JZ        @TailNext
        CMP       EAX, $FF000000
        JNC       @TailOpaque

        MOVD      XMM0, EAX
        MOVD      XMM1, [RDX]
        PMOVZXBW  XMM0, XMM0
        PMOVZXBW  XMM1, XMM1
        PEXTRW    R10D, XMM0, 3    // Fa
        PEXTRW    R9D, XMM1, 3     // Ba
        MOV       R11D, R10D       // Fa
        XOR       R10D, 255
        XOR       R9D, 255
        IMUL      R10D, R9D
        ADD       R10D, 128
        MOV       R9D, R10D
        SHR       R9D, 8
        ADD       R10D, R9D
        SHR       R10D, 8
        XOR       R10D, 255        // Ra in R10D
        MOV       R9D, R10D        // Save Ra
        SHL       R10D, 8
        ADD       R10D, R11D       // Ra*256 + Fa
        LEA       RAX, [DivMul255Table]
        MOVZX     EAX, BYTE PTR [RAX + R10] // Wa
        MOVD      XMM2, EAX
        PSHUFLW   XMM2, XMM2, $00
        PMOVZXBW  XMM1, [RDX]      // Reload Dst unpacked
        PSUBW     XMM0, XMM1
        PMULLW    XMM0, XMM2
        PSLLW     XMM1, 8
        PADDW     XMM1, XMM5
        PADDW     XMM0, XMM1
        PSRLW     XMM0, 8
        PINSRW    XMM0, R9D, 3
        PXOR      XMM2, XMM2
        PACKUSWB  XMM0, XMM2
        MOVD      [RDX], XMM0
        JMP       @TailNext

@Opaque4:
        MOVDQU    [RDX], XMM0
        JMP       @Next4

@TailOpaque:
        MOV       [RDX], EAX

@TailNext:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @TailLoop

@Done:
{$else}
        RET
{$ifend}

end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Color algebra
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// ColorAdd
//------------------------------------------------------------------------------
function ColorAdd_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PADDUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PADDUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorSub
//------------------------------------------------------------------------------
function ColorSub_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PSUBUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PSUBUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorModulate
//------------------------------------------------------------------------------
function ColorModulate_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        PMULLW    XMM0,XMM1
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        PXOR      XMM2,XMM2
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        PMULLW    XMM0,XMM1
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorMax
//------------------------------------------------------------------------------
function ColorMax_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PMAXUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PMAXUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorMin
//------------------------------------------------------------------------------
function ColorMin_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PMINUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PMINUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorDifference
//------------------------------------------------------------------------------
function ColorDifference_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        MOVQ      XMM2,XMM0
        PSUBUSB   XMM0,XMM1
        PSUBUSB   XMM1,XMM2
        POR       XMM0,XMM1
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        MOVQ      XMM2,XMM0
        PSUBUSB   XMM0,XMM1
        PSUBUSB   XMM1,XMM2
        POR       XMM0,XMM1
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorExclusion
//------------------------------------------------------------------------------
function ColorExclusion_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        MOVQ      XMM3,XMM0
        PADDW     XMM0,XMM1
        PMULLW    XMM1,XMM3
        PSRLW     XMM1,7
        PSUBUSW   XMM0,XMM1
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        PXOR      XMM2,XMM2
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        MOVQ      XMM3,XMM0
        PADDW     XMM0,XMM1
        PMULLW    XMM1,XMM3
        PSRLW     XMM1,7
        PSUBUSW   XMM0,XMM1
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
// ColorScale
//------------------------------------------------------------------------------
function ColorScale_SSE2(C: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}
        PXOR      XMM2,XMM2
        SHL       EDX,4
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        ADD       EDX,alpha_ptr
        PMULLW    XMM0,[EDX]
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$elseif defined(TARGET_x64)}
        PXOR      XMM2,XMM2
        SHL       RDX,4
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
{$IFNDEF FPC}
        ADD       RDX,alpha_ptr
{$ELSE}
        ADD       RDX,[RIP+alpha_ptr]
{$ENDIF}
        PMULLW    XMM0,[RDX]
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ifend}
end;


//------------------------------------------------------------------------------
//
//      Misc
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// LightenReg
//------------------------------------------------------------------------------
function LightenReg_SSE2(C: TColor32; Amount: Integer): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

  // EAX <- C: TColor32
  // EDX <- Amount: integer
  // EAX -> Result
        MOVD    XMM0, EAX
        TEST    EDX, EDX
        JL      @1

  // Positive: Lighten
        IMUL    EDX, $010101
        MOVD    XMM1, EDX
        PADDUSB XMM0, XMM1
        MOVD    EAX, XMM0
        RET

  // Negative: Darken
@1:     NEG     EDX
        IMUL    EDX, $010101
        MOVD    XMM1, EDX
        PSUBUSB XMM0, XMM1
        MOVD    EAX, XMM0

{$elseif defined(TARGET_x64)}

  // ECX <- C: TColor32
  // EDX <- Amount: integer
  // EAX -> Result
        MOVD    XMM0, ECX
        TEST    EDX, EDX
        JL      @1

  // Positive: Lighten
        IMUL    EDX, $010101
        MOVD    XMM1, EDX
        PADDUSB XMM0, XMM1
        MOVD    EAX, XMM0
        RET

  // Negative: Darken
@1:     NEG     EDX
        IMUL    EDX, $010101
        MOVD    XMM1, EDX
        PSUBUSB XMM0, XMM1
        MOVD    EAX, XMM0

{$ifend}
end;


//------------------------------------------------------------------------------
// ScaleMems
//------------------------------------------------------------------------------
procedure ScaleMems_SSE41(Dst: PColor32; Count: Integer; Weight: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  //
  // Result Z = W * Bargb
  //
  // Approximates (x div 255) as ((x * $8081 + Bias) shr 23)
  //
{$if defined(TARGET_x86)}

  // EAX <- Dst: PColor32
  // EDX <- Count
  // ECX <- Weight: Byte

        // Test the counter for zero or negativity
        TEST      EDX, EDX
        JLE       @Done

        // Test if:
        // - Weight is 0 (i.e. clear RGB to zero)
        // - Weight is 255 (i.e. no scale)
        AND       ECX, $000000FF
        JZ        @Clear
        TEST      ECX, $000000FF
        JE        @Done

        // Weight = Weight * $8081
        IMUL      ECX, ECX, $8081
        MOVD      XMM0, ECX
        // 1*Byte -> 4*DWord
        PSHUFD    XMM0, XMM0, 0                 // XMM0[0..3] <- XMM0[0][0]

@Loop:
        // Load dest
        MOVD      XMM1, DWORD PTR [EAX]         // XMM1 <- 00 00 00 00 Ba Br Bg Bb
        // 4*Byte -> 4*DWord
        PMOVZXBD  XMM1, XMM1                    // XMM1[0..3] <- Color[0][0..3]

        //
        // Scale: Result = (Weight * Color)
        //               = (($8081 * Weight * Color) shr 23)
        //
        PMULLD    XMM1, XMM0                    // XMM1 <- Color * Weight * $8081

        // Add bias (~$7F*$8081)
        PADDD     XMM1, DQWORD PTR [SSE_003FFF7F_ALIGNED] // XMM1 <- (Color * Weight * $8081) + Bias

        // Reduce 32-bits to 9-bits
        PSRLD     XMM1, 23                      // XMM1 <- ((Color * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
        PSHUFB    XMM1, DQWORD PTR [SSE_0C080400_ALIGNED] // XMM1[0] <- XMM1[0..3][0]

        // Store dest
        MOVD      [EAX], XMM1

        ADD       EAX, 4
        DEC       EDX
        JNZ       @Loop

@Done:
        RET

@Clear:
  // Clear RGB, leave A as-is
        MOV       ECX, DWORD PTR [EAX]
        AND       ECX, $FF000000
        MOV       DWORD PTR [EAX], ECX
        ADD       EAX, 4
        DEC       EDX
        JNZ       @Clear

{$elseif defined(TARGET_x64)}

  // RCX <- Dst: PColor32
  // RDX <- Count
  // R8D <- Weight: Byte

        // Test the counter for zero or negativity
        TEST      EDX, EDX
        JLE       @Done

        // Test if:
        // - Weight is 0 (i.e. clear RGB to zero)
        // - Weight is 255 (i.e. no scale)
        AND       R8D, $000000FF
        JZ        @Clear
        TEST      R8D, $000000FF
        JE        @Done

        // Weight = Weight * $8081
        IMUL      R8D, R8D, $8081
        MOVD      XMM0, R8D                     // XMM0 <- Weight * $8081
        // 1*Byte -> 4*DWord
        PSHUFD    XMM0, XMM0, 0                 // XMM0[0..3] <- XMM0[0][0]

@Loop:
        // Load dest
        MOVD      XMM1, DWORD PTR [RCX]         // XMM1 <- 00 00 00 00 Ba Br Bg Bb
        // 4*Byte -> 4*DWord
        PMOVZXBD  XMM1, XMM1                    // XMM1[0..3] <- Color[0][0..3]

        //
        // Scale: Result = (Weight * Color)
        //               = (($8081 * Weight * Color) shr 23)
        //
        PMULLD    XMM1, XMM0                    // XMM1 <- Color * Weight * $8081

        // Add bias (~$7F*$8081)
{$if (not defined(FPC))}
        PADDD     XMM1, DQWORD PTR [SSE_003FFF7F_ALIGNED] // XMM1 <- (Color * Weight * $8081) + Bias
{$else}
        PADDD     XMM1, DQWORD PTR [rip+SSE_003FFF7F_ALIGNED]
{$ifend}

        // Reduce 32-bits to 9-bits
        PSRLD     XMM1, 23                      // XMM1 <- ((Color * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
{$if (not defined(FPC))}
        PSHUFB    XMM1, DQWORD PTR [SSE_0C080400_ALIGNED] // XMM1[0] <- XMM1[0..3][0]
{$else}
        PSHUFB    XMM1, DQWORD PTR [rip+SSE_0C080400_ALIGNED]
{$ifend}

        // Store dest
        MOVD      [RCX], XMM1

        ADD       RCX,4
        DEC       EDX
        JNZ       @Loop

@Done:
        RET

@Clear:
  // Clear RGB, leave A as-is
        MOV       EAX, DWORD PTR [RCX]
        AND       EAX, $FF000000
        MOV       DWORD PTR [RCX], EAX
        ADD       RCX, 4
        DEC       EDX
        JNZ       @Clear

{$ifend}
end;

procedure FastScaleMems_SSE41(Dst: PColor32; Count: Integer; Weight: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  //
  // Result Z = W * Bargb
  //
  // Approximates (x div 255) as (x shr 8); Same as ColorScale_Pas
  //
{$if defined(TARGET_x86)}

  // EAX <- Dst: PColor32
  // EDX <- Count
  // ECX <- Weight: Byte

        // Test the counter for zero or negativity
        TEST      EDX, EDX
        JLE       @Done

        // Test if:
        // - Weight is 0 (i.e. clear RGB to zero)
        // - Weight is 255 (i.e. no scale)
        AND       ECX, $000000FF
        JZ        @Clear
        TEST      ECX, $000000FF
        JE        @Done

        PXOR      XMM2, XMM2

        // Duplicate Weight into 8 words so we can process two pixels at a time
        MOVD      XMM0, ECX                     // XMM0 <- (00 00 00 00 00 00 00 WW)
        PSHUFLW   XMM0, XMM0, 0                 //         (00 WW 00 WW 00 WW 00 WW)
        PSHUFD    XMM0, XMM0, 0                 //         (00 WW 00 WW 00 WW 00 WW)*2

  // Test for odd/even count
        TEST      EDX, 1
        JZ        @Even

  // We have an odd number of pixels.
  // Process a single pixel so the remaining count is even.

  // Load dest
        MOVD      XMM1, DWORD PTR [EAX]         // XMM1 <- 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM1, XMM2                    // XMM1 <- 00 Ba 00 Br 00 Bg 00 Bb

        //
        // Scale: Result = (Weight * Color)
        //               = ((Weight * Color) shr 8)
        //
        PMULLW    XMM1, XMM0
        PSRLW     XMM1, 8

  // Store dest
        // Pack result back from word to byte components
        PACKUSWB  XMM1, XMM1
        MOVD      [EAX], XMM1

@Even:
        LEA       EAX, [EAX + EDX * 4]          // Get address of last pixel
        SHR       EDX, 1                        // Number of QWORDs
        JZ        @Done
        NEG       EDX                           // Negate count so we can use it as an offset to move forward

@Loop:
  // Load dest
        MOVQ      XMM1, [EAX + EDX * 8].QWORD   // XMM2 <- Ba Br Bg Bb Ba Br Bg Bb
{-$define FASTSCALEMEMS_SKIPWRITE}
{$ifdef FASTSCALEMEMS_SKIPWRITE}
        // Skip scale (and thus the relatively costly write) if the color is pure black
        PTEST     XMM1, XMM1
        JZ        @SkipWrite
{$endif FASTSCALEMEMS_SKIPWRITE}
        // 8*Byte -> 8*Word
        PUNPCKLBW XMM1, XMM2                    // XMM2 <- 00 Ba 00 Br 00 Bg 00 Bb

        //
        // Scale: Result = (Weight * Color)
        //               = ((Weight * Color) shr 8)
        //
        PMULLW    XMM1, XMM0
        PSRLW     XMM1, 8

  // Store dest
        PACKUSWB  XMM1, XMM2
        MOVQ      [EAX + EDX * 8].QWORD, XMM1

{$ifdef FASTSCALEMEMS_SKIPWRITE}
@SkipWrite:
{$endif FASTSCALEMEMS_SKIPWRITE}
        ADD       EDX, 1
        JS        @Loop

@Done:
        RET

@Clear:
  // Clear RGB, leave A as-is
        MOV       ECX, DWORD PTR [EAX]
        AND       ECX, $FF000000
        MOV       DWORD PTR [EAX], ECX
        ADD       EAX, 4
        DEC       EDX
        JNZ       @Clear

{$elseif defined(TARGET_x64)}

  // RCX <- Dst: PColor32
  // RDX <- Count
  // R8D <- Weight: Byte


        // Test the counter for zero or negativity
        TEST      RDX, RDX
        JLE       @Done

        // Test if:
        // - Weight is 0 (i.e. clear RGB to zero)
        // - Weight is 255 (i.e. no scale)
        AND       R8D, $000000FF
        JZ        @Clear
        TEST      R8D, $000000FF
        JE        @Done

        PXOR      XMM2, XMM2

        // Duplicate Weight into 8 words so we can process two pixels at a time
        MOVD      XMM0, R8D                     // XMM0 <- (00 00 00 00 00 00 00 WW)
        PSHUFLW   XMM0, XMM0, 0                 //         (00 WW 00 WW 00 WW 00 WW)
        PSHUFD    XMM0, XMM0, 0                 //         (00 WW 00 WW 00 WW 00 WW)*2

  // Test for odd/even count
        TEST      EDX, 1
        JZ        @Even

  // We have an odd number of pixels.
  // Process a single pixel so the remaining count is even.

  // Load dest
        MOVD      XMM1, DWORD PTR [RCX]         // XMM1 <- 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM1, XMM2                    // XMM1 <- 00 Ba 00 Br 00 Bg 00 Bb

        //
        // Scale: Result = (Weight * Color)
        //               = ((Weight * Color) shr 8)
        //
        PMULLW    XMM1, XMM0
        PSRLW     XMM1, 8

  // Store dest
        // Pack result back from word to byte components
        PACKUSWB  XMM1, XMM1
        MOVD      [RCX], XMM1

@Even:
        LEA       RCX, [RCX + RDX * 4]          // Get address of last pixel
        SHR       RDX, 1                        // Number of QWORDs
        JZ        @Done
        NEG       RDX                           // Negate count so we can use it as an offset to move forward

@Loop:
  // Load dest
        MOVQ      XMM1, [RCX + RDX * 8].QWORD   // XMM1 <- Ba Br Bg Bb Ba Br Bg Bb
// FASTSCALEMEMS_SKIPWRITE has been disabled as it doesn't give us enough and in some
// cases makes the loop slower. Probably due to branch misprediction.
{-$define FASTSCALEMEMS_SKIPWRITE}
{$ifdef FASTSCALEMEMS_SKIPWRITE}
        // Skip scale (and thus the relatively costly write) if the color is pure black
        PTEST     XMM1, XMM1
        JZ        @SkipWrite
{$endif FASTSCALEMEMS_SKIPWRITE}
        // 8*Byte -> 8*Word
        PUNPCKLBW XMM1, XMM2                    // XMM1 <- 00 Ba 00 Br 00 Bg 00 Bb

        //
        // Scale: Result = (Weight * Color)
        //               = ((Weight * Color) shr 8)
        //
        PMULLW    XMM1, XMM0
        PSRLW     XMM1, 8

  // Store dest
        PACKUSWB  XMM1, XMM2
        MOVQ      [RCX + RDX * 8].QWORD, XMM1

{$ifdef FASTSCALEMEMS_SKIPWRITE}
@SkipWrite:
{$endif FASTSCALEMEMS_SKIPWRITE}
        ADD       RDX, 1
        JS        @Loop

@Done:
        RET

@Clear:
  // Clear RGB, leave A as-is
        MOV       ECX, DWORD PTR [RCX]
        AND       ECX, $FF000000
        MOV       DWORD PTR [RCX], ECX
        ADD       RCX, 4
        DEC       RDX
        JNZ       @Clear

{$ifend}
end;


//------------------------------------------------------------------------------
// Premultiply/Unpremultiply
//------------------------------------------------------------------------------
procedure PremultiplyMem_SSE41(Pixels: PColor32Entry; Count: Integer);
//
// Uses PMULLD for 32-bit integer multiplication and the $8081 formula for
// accurate division:
//
//   Round(x/255) approx (x * $8081 + $400000) >> 23.
//
// This variant processes a single pixel at a time but is extremely accurate.
//
asm
{$IFDEF TARGET_x86}
  // Parameters (x86):
  //   EAX <- Pixels
  //   EDX <- Count
  //
  // Register usage:
  //   ECX: Misc., @SSE_0C080400_ALIGNED
  //
  // SSE register usage:
  //   XMM0: Pixel
  //   XMM1: Misc.
  //   XMM2: SSE_003FFF7F_ALIGNED
  //   XMM3: $8081
  //   XMM4: Zero
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        TEST      EDX, EDX              // Count=0 -> Exit
        JZ        @Exit

        PXOR      XMM4, XMM4
        MOV       ECX, $8081
        MOVD      XMM3, ECX
        PSHUFD    XMM3, XMM3, 0         // Multiplier constant for division

        MOV       ECX, offset SSE_003FFF7F_ALIGNED
        MOVDQA    XMM2, [ECX]
        MOV       ECX, offset SSE_0C080400_ALIGNED // Table for PSHUFB byte extraction

@Loop:
        MOVD      XMM0, [EAX]           // Load pixel

        PMOVZXBD  XMM0, XMM0            // Zero-extend bytes to dwords: B G R A
        PSHUFD    XMM1, XMM0, $FF       // Broadcast Alpha to all slots
        PMULLD    XMM0, XMM1            // C * A
        PMULLD    XMM0, XMM3            // (C * A) * $8081
        PADDD     XMM0, XMM2            // Add bias ($400000) for rounding
        PSRLD     XMM0, 23              // Component result

        PINSRB    XMM0, [EAX+3], 12     // Restore original Alpha to byte 12 (component 3 low byte)

        PSHUFB    XMM0, DQWORD PTR [ECX]// Extract low bytes of each dword to pack
        MOVD      [EAX], XMM0

        ADD       EAX, 4                // Next pixel
        DEC       EDX
        JNZ       @Loop

@Exit:
{$ELSE}
  // Parameters (x64):
  //   RCX <- Pixels
  //   RDX <- Count
  //
  // Register usage:
  //   RAX: Misc., SSE_0C080400_ALIGNED
  //
  // SSE register usage:
  //   XMM0: Pixel
  //   XMM1: Misc.
  //   XMM2: SSE_003FFF7F_ALIGNED
  //   XMM3: $8081
  //   XMM4: Zero
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        TEST      EDX, EDX              // Count=0 -> Exit
        JZ        @Exit

        PXOR      XMM4, XMM4
        MOV       EAX, $8081
        MOVD      XMM3, EAX
        PSHUFD    XMM3, XMM3, 0
        LEA       RAX, [RIP+SSE_003FFF7F_ALIGNED]
        MOVDQA    XMM2, [RAX]
        LEA       RAX, [RIP+SSE_0C080400_ALIGNED]

@Loop:
        MOVD      XMM0, [RCX]           // Load pixel

        PMOVZXBD  XMM0, XMM0            // Zero-extend bytes to dwords: B G R A
        PSHUFD    XMM1, XMM0, $FF       // Broadcast Alpha to all slots
        PMULLD    XMM0, XMM1            // C * A
        PMULLD    XMM0, XMM3            // (C * A) * $8081
        PADDD     XMM0, XMM2            // Add bias ($400000) for rounding
        PSRLD     XMM0, 23              // Component result

        PINSRB    XMM0, [RCX+3], 12     // Restore original Alpha to byte 12 (component 3 low byte)

        PSHUFB    XMM0, [RAX]           // Extract low bytes of each dword to pack
        MOVD      [RCX], XMM0

        ADD       RCX, 4                // Next pixel
        DEC       EDX
        JNZ       @Loop

@Exit:
{$ENDIF}
end;

// Precomputed multipliers used to avoid costly integer division inside the
// unpremultiply loop.
// The table maps an alpha value (1..255) to a 16.16 fixed point multiplier:
//
//   UnpremultiplyTable[A] = Round((255 << 16) / A)
//
// and allows us to replace division by A with a 16.16 fixed-point
// multiplication.
var
  UnpremultiplyTable: array[0..255] of Cardinal;

procedure InitUnpremultiplyTable;
var
  i: integer;
begin
  UnpremultiplyTable[0] := 0;
  for i := 1 to 255 do
    UnpremultiplyTable[i] := (Cardinal(255) shl 16 + i div 2) div i;
end;

procedure UnpremultiplyMem_SSE41(Pixels: PColor32Entry; Count: Integer);
//
// Uses PMULLD and PSHUFB to improve the multiplier-based fixed-point logic.
//
asm
{$IFDEF TARGET_x86}
  // Parameters (x86):
  //   EAX <- Pixels
  //   EDX <- Count
  //
  // Register usage:
  //   EBX: Misc.
  //   ECX: Misc.
  //   ESI: @UnpremultiplyTable
  //
  // SSE register usage:
  //   XMM0: Pixel
  //   XMM1: Misc.
  //   XMM2: 32768 x 4
  //   XMM3: SSE_0C080400_ALIGNED
  //   XMM4:
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        PUSH      ESI
        PUSH      EBX

        MOV       ESI, offset SSE_0C080400_ALIGNED
        MOVDQA    XMM3, [ESI]
        LEA       ESI, [UnpremultiplyTable]
        // Generate 32768 bias for rounding: (x + 32768) >> 16
        PCMPEQW   XMM2, XMM2
        PSLLW     XMM2, 15              // words = $8000
        PSHUFD    XMM2, XMM2, 0         // dwords = 32768

@Loop:
        MOV       ECX, [EAX]            // Load pixel
        TEST      ECX, $FF000000        // Skip if Alpha=0
        JZ        @ZeroAlpha

// [1] --> To here --+
//                   |
//                   v
        MOVD      XMM0, ECX             // Save pixel in XMM0

        SHR       ECX, 24               // Extract Alpha
        CMP       CL, 255               // Skip if Alpha=255
        JZ        @Next

        MOV       EBX, [ESI + ECX * 4]  // Multiplier from precomputed table
        MOVD      XMM1, EBX
        PSHUFD    XMM1, XMM1, 0         // Broadcast multiplier to all slots

// [1] Moved from here --+
//                       |
//                       v
//        MOVD      XMM0, [EAX]
        PMOVZXBD  XMM0, XMM0            // Extend bytes to 4 DWords

        PMULLD    XMM0, XMM1            // component * multiplier

        PADDD     XMM0, XMM2            // Round: (x + 32768) >> 16
        PSRLD     XMM0, 16

        // Restore Alpha
        MOVZX     EBX, BYTE PTR [EAX+3]
        PINSRD    XMM0, EBX, 3          // Put original Alpha in dword 3

        PSHUFB    XMM0, XMM3            // Fast byte extraction
        MOVD      [EAX], XMM0

@Next:
        ADD       EAX, 4                // Next pixel
        DEC       EDX
        JNZ       @Loop

@Exit:
        POP       EBX
        POP       ESI
        RET

@ZeroAlpha:
        MOV       DWORD PTR [EAX], 0    // Clear pixel
        JMP       @Next

{$ELSE}
  // Parameters (x64):
  //   RCX <- Pixels
  //   RDX <- Count
  //
  // Register usage:
  //   RAX: Misc.
  //   R8:  Misc.
  //   R9:  @UnpremultiplyTable
  //
  // SSE register usage:
  //   XMM0: Pixel
  //   XMM1: Misc.
  //   XMM2: 32768 x 4
  //   XMM3: SSE_0C080400_ALIGNED
  //   XMM4:
  //   XMM5:
  //   XMM6:
  //   XMM7:
  //

        LEA       R9, [UnpremultiplyTable]
        LEA       RAX, [RIP+SSE_0C080400_ALIGNED]
        MOVDQA    XMM3, [RAX]
        // Generate 32768 bias for rounding: (x + 32768) >> 16
        PCMPEQW   XMM2, XMM2
        PSLLW     XMM2, 15              // words = $8000
        PSHUFD    XMM2, XMM2, 0         // dwords = 32768

@Loop:
        MOV       R8D, [RCX]            // Load pixel
        TEST      R8D, $FF000000        // Skip if Alpha=0
        JZ        @ZeroAlpha

// [1] --> To here --+
//                   |
//                   v
        MOVD      XMM0, R8D

        SHR       R8D, 24               // Extract Alpha
        CMP       R8B, 255              // Skip if Alpha=255
        JZ        @Next

        MOV       EAX, [R9 + R8 * 4]    // Multiplier from precomputed table
        MOVD      XMM1, EAX
        PSHUFD    XMM1, XMM1, 0         // Broadcast multiplier to all slots

// [1] Moved from here --+
//                       |
//                       v
//        MOVD      XMM0, [RCX]
        PMOVZXBD  XMM0, XMM0            // Extend bytes to 4 DWords

        PMULLD    XMM0, XMM1            // component * multiplier

        PADDD     XMM0, XMM2            // Round: (x + 32768) >> 16
        PSRLD     XMM0, 16

        MOVZX     EAX, BYTE PTR [RCX+3] // Put original Alpha in dword 3
        PINSRD    XMM0, EAX, 3

        PSHUFB    XMM0, XMM3            // Fast byte extraction
        MOVD      [RCX], XMM0

@Next:
        ADD       RCX, 4                // Next pixel
        DEC       EDX
        JNZ       @Loop

@Exit:
        RET

@ZeroAlpha:
        MOV       DWORD PTR [RCX], 0    // Clear pixel
        JMP       @Next
{$ENDIF}
end;



{$ifend}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindingFunctions;
begin
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}

  BlendRegistry[@@MergeReg].Add(       @MergeReg_SSE2,          [isSSE2]).Name := 'MergeReg_SSE2';
{$if not defined(FPC)}
  BlendRegistry[@@MergeReg].Add(       @MergeReg_SSE41,         [isSSE41]).Name := 'MergeReg_SSE41';
  BlendRegistry[@@MergeMem].Add(       @MergeMem_SSE41,         [isSSE41]).Name := 'MergeMem_SSE41';
  BlendRegistry[@@MergeLine].Add(      @MergeLine_SSE41,        [isSSE41]).Name := 'MergeLine_SSE41';
{$ifend}

  BlendRegistry[@@CombineReg].Add(     @CombineReg_SSE2,        [isSSE2]).Name := 'CombineReg_SSE2';
  BlendRegistry[@@CombineMem].Add(     @CombineMem_SSE2_128,    [isSSE2]).Name := 'CombineMem_SSE2_128';
  BlendRegistry[@@CombineMem].Add(     @CombineMem_SSE41_Kadaif,[isSSE41]).Name := 'CombineMem_SSE41_Kadaif';
{$if defined(BENCHMARK)} // TODO : Move these to the benchmark unit
  BlendRegistry[@@CombineMem].Add(     @CombineMem_SSE2_Table,  [isSSE2], BindingPriorityWorse).Name := 'CombineMem_SSE2_Table';
  BlendRegistry[@@CombineMem].Add(     @CombineMem_SSE41_8081,  [isSSE41], BindingPriorityWorse).Name := 'CombineMem_SSE41_8081';
{$ifend}
  BlendRegistry[@@CombineLine].Add(    @CombineLine_SSE2,       [isSSE2]).Name := 'CombineLine_SSE2';
  BlendRegistry[@@BlendReg].Add(       @BlendReg_SSE2,          [isSSE2]).Name := 'BlendReg_SSE2';
  BlendRegistry[@@BlendReg].Add(       @BlendReg_SSE41,         [isSSE41]).Name := 'BlendReg_SSE41';
  BlendRegistry[@@BlendMem].Add(       @BlendMem_SSE2,          [isSSE2]).Name := 'BlendMem_SSE2';
  BlendRegistry[@@BlendMems].Add(      @BlendMems_SSE2,         [isSSE2]).Name := 'BlendMems_SSE2';
  BlendRegistry[@@BlendMemEx].Add(     @BlendMemEx_SSE2,        [isSSE2]).Name := 'BlendMemEx_SSE2';
  BlendRegistry[@@BlendLine].Add(      @BlendLine_SSE2,         [isSSE2]).Name := 'BlendLine_SSE2';
{$if not defined(FPC)}
  BlendRegistry[@@BlendLine].Add(      @BlendLine_SSE41,        [isSSE41]).Name := 'BlendLine_SSE41';
{$ifend}
  BlendRegistry[@@BlendLineEx].Add(    @BlendLineEx_SSE2,       [isSSE2]).Name := 'BlendLineEx_SSE2';
  BlendRegistry[@@BlendRegEx].Add(     @BlendRegEx_SSE2,        [isSSE2]).Name := 'BlendRegEx_SSE2';
  BlendRegistry[@@ColorMax].Add(       @ColorMax_SSE2,          [isSSE2]).Name := 'ColorMax_SSE2';
  BlendRegistry[@@ColorMin].Add(       @ColorMin_SSE2,          [isSSE2]).Name := 'ColorMin_SSE2';
  BlendRegistry[@@ColorAdd].Add(       @ColorAdd_SSE2,          [isSSE2]).Name := 'ColorAdd_SSE2';
  BlendRegistry[@@ColorSub].Add(       @ColorSub_SSE2,          [isSSE2]).Name := 'ColorSub_SSE2';
  BlendRegistry[@@ColorModulate].Add(  @ColorModulate_SSE2,     [isSSE2]).Name := 'ColorModulate_SSE2';
  BlendRegistry[@@ColorDifference].Add(@ColorDifference_SSE2,   [isSSE2]).Name := 'ColorDifference_SSE2';
  BlendRegistry[@@ColorExclusion].Add( @ColorExclusion_SSE2,    [isSSE2]).Name := 'ColorExclusion_SSE2';
  BlendRegistry[@@ColorScale].Add(     @ColorScale_SSE2,        [isSSE2]).Name := 'ColorScale_SSE2';
  BlendRegistry[@@LightenReg].Add(     @LightenReg_SSE2,        [isSSE]).Name := 'LightenReg_SSE2';
  BlendRegistry[@@BlendRegRGB].Add(    @BlendRegRGB_SSE2,       [isSSE2]).Name := 'BlendRegRGB_SSE2';
  BlendRegistry[@@BlendMemRGB].Add(    @BlendMemRGB_SSE2,       [isSSE2]).Name := 'BlendMemRGB_SSE2';

{$if defined(GR32_SCALEMEMS_FAST) or defined(BENCHMARK)}
  BlendRegistry[@@ScaleMems].Add(      @FastScaleMems_SSE41,    [isSSE41]).Name := 'FastScaleMems_SSE41';
{$ifend}
{$if (not defined(GR32_SCALEMEMS_FAST)) or defined(BENCHMARK)}
  BlendRegistry[@@ScaleMems].Add(      @ScaleMems_SSE41,        [isSSE41]).Name := 'ScaleMems_SSE41';
{$ifend}

{$if defined(TEST_BLENDMEMRGB128SSE4)}
  BlendRegistry[@@BlendMemRGB128].Add( @BlendMemRGB128_SSE4,    [isSSE2]).Name := 'BlendMemRGB128_SSE4';
{$ifend}

  BlendRegistry[@@PremultiplyMem].Add(@PremultiplyMem_SSE41,    [isSSE41]).Name := 'PremultiplyMem_SSE41';
  BlendRegistry[@@UnpremultiplyMem].Add(@UnpremultiplyMem_SSE41,[isSSE41]).Name := 'UnpremultiplyMem_SSE41';
{$if declared(PremultiplyMem_SSE2)}
  BlendRegistry[@@PremultiplyMem].Add(@PremultiplyMem_SSE2,     [isSSE2]).Name := 'PremultiplyMem_SSE2';
{$ifend}
{$if declared(UnpremultiplyMem_SSE2)}
  BlendRegistry[@@UnpremultiplyMem].Add(@UnpremultiplyMem_SSE2, [isSSE2]).Name := 'UnpremultiplyMem_SSE2';
{$ifend}

{$ifend}
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  RegisterBindingFunctions;
{$if not defined(PUREPASCAL)}
  InitUnpremultiplyTable;
{$ifend}
end.
