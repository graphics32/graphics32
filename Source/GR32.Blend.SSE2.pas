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

//------------------------------------------------------------------------------
//
//      SSE SIMD blend implementations
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Blend
//------------------------------------------------------------------------------
function BlendReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMem_SSE2(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMems_SSE2(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegEx_SSE2(F, B: TColor32; M: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegRGB_SSE2(F, B: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}

procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: Cardinal); {$IFDEF FPC} assembler; {$ENDIF}


//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
function MergeReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}


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
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_Blend,
  GR32_LowLevel,
  GR32_Bindings;

//------------------------------------------------------------------------------
//
//      Blend
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// BlendReg
//------------------------------------------------------------------------------
function BlendReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb

{$IFDEF TARGET_x86}
        MOVD      XMM0,EAX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        MOV       ECX,bias_ptr       // ECX   <-  Pointer to Bias
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 Fa 00 Fa
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM0  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[ECX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
{$ENDIF}

{$IFDEF TARGET_x64}
        MOVD      XMM0,ECX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
{$IFNDEF FPC}
        MOV       RAX,bias_ptr       // RAX   <-  Pointer to Bias
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 ** 00 **
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM2  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[RAX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendMem
//------------------------------------------------------------------------------
procedure BlendMem_SSE2(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}

{$IFDEF TARGET_x64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendRegEx
//------------------------------------------------------------------------------
function BlendRegEx_SSE2(F, B: TColor32; M: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result := M * Fa * (Fargb - Bargb) + Bargb

{$IFDEF TARGET_x86}
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
{$ENDIF}

{$IFDEF TARGET_x64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendMemEx
//------------------------------------------------------------------------------
procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
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
{$ENDIF}

{$IFDEF TARGET_x64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendRegRGB
//------------------------------------------------------------------------------
function BlendRegRGB_SSE2(F, B: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}

{$IFDEF TARGET_x64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendMemRGB
//------------------------------------------------------------------------------
procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}
{$IFDEF TARGET_x64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendMemRGB128
//------------------------------------------------------------------------------
{$IFDEF TEST_BLENDMEMRGB128SSE4}
procedure BlendMemRGB128_SSE4(F: TColor32; var B: TColor32; W: UInt64); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}
{$IFDEF TARGET_x64}
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
{$ENDIF}
end;
{$ENDIF}


//------------------------------------------------------------------------------
// BlendLine
//------------------------------------------------------------------------------
procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
{$IFDEF FPC}
const
  COpaque: QWORD = QWORD($FF000000FF000000);
{$ENDIF}
asm
{$IFDEF TARGET_X86}
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

{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
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
{$IFDEF TARGET_X86}
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

{$ENDIF}

{$IFDEF TARGET_X64}
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

{$ENDIF}
end;


//------------------------------------------------------------------------------
// BlendLineEx
//------------------------------------------------------------------------------
procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: Cardinal); {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
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
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
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

{$IFDEF TARGET_X86}
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
{$ENDIF}
{$IFDEF TARGET_X64}
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
{$ENDIF}
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

{$IFDEF TARGET_X86}
  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$ELSE}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$ENDIF}

        // Load ColorX and ColorY
{$IFDEF TARGET_X86}
        MOVD      XMM1, EAX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [EDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ELSE}
        MOVD      XMM1, ECX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [RDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ENDIF}

        // Duplicate weight into 4 words
{$IFDEF TARGET_X86}
        MOVD      XMM3, ECX                     // XMM3 <- Weight       (00 00 00 00 00 00 00 WW)
{$ELSE}
        MOVD      XMM3, R8D                     // XMM3 <- Weight       (00 00 00 00 00 00 00 WW)
{$ENDIF}
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
{$IFDEF TARGET_X86}
        MOVD      [EDX], XMM1                   // ColorY <- XMM1
{$ELSE}
        MOVD      [RDX], XMM1                   // ColorY <- XMM1
{$ENDIF}

@exit:
        RET

@return_x:
{$IFDEF TARGET_X86}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$ELSE}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ENDIF}
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

{$IFDEF TARGET_X86}
  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$ELSE}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       R8D, $FF
        JZ        @return_x
{$ENDIF}

        // Load ColorX and ColorY
{$IFDEF TARGET_X86}
        MOVD      XMM1, EAX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [EDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ELSE}
        MOVD      XMM1, ECX                     // XMM1 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM2, [RDX]                   // XMM2 <- ColorY       (Ba Br Bg Bb)
{$ENDIF}

        // Duplicate weight*$8081 into 4 dwords
{$IFDEF TARGET_X86}
        IMUL      ECX, ECX, $8081
{$ELSE}
        IMUL      ECX, R8D, $8081
{$ENDIF}
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
{$IFDEF TARGET_X86}
        MOVD      [EDX], XMM1                   // ColorY <- XMM1
{$ELSE}
        MOVD      [RDX], XMM1                   // ColorY <- XMM1
{$ENDIF}

@exit:
        RET

@return_x:
{$IFDEF TARGET_X86}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$ELSE}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ENDIF}
end;

//------------------------------------------------------------------------------

// Aligned bias table
procedure SIMD_4x003FFF7F; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
end;

// Aligned pack table for PSHUFB: Picks low byte of 4 dwords
procedure SIMD_4x0C080400; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
end;

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

{$IFDEF TARGET_X86}
  // EAX        Color X (Foreground)
  // [EDX]      Color Y (Background)
  // ECX        Weight of X [0..255]

        // Return ColorY if weight=0
        JCXZ      @exit

        // Return ColorX if weight=255
        CMP       ECX, $FF
        JZ        @return_x
{$ELSE}
  // ECX        Color X (Foreground)
  // [RDX]      Color Y (Background)
  // R8D        Weight of X [0..255]

        // Return ColorY if weight=0
        TEST      R8D, R8D
        JZ        @exit

        // Return ColorX if weight=255
        CMP       R8D, $FF
        JZ        @return_x
{$ENDIF}

        // Load ColorX and ColorY
{$IFDEF TARGET_X86}
        MOVD      XMM0, EAX                     // XMM0 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM1, [EDX]                   // XMM1 <- ColorY       (Ba Br Bg Bb)
{$ELSE}
        MOVD      XMM0, ECX                     // XMM0 <- ColorX       (Fa Fr Fg Fb)
        MOVD      XMM1, [RDX]                   // XMM1 <- ColorY       (Ba Br Bg Bb)
{$ENDIF}

        // Weight = Weight * $8081
{$IFDEF TARGET_X86}
        IMUL      ECX, ECX, $8081
{$ELSE}
        IMUL      ECX, R8D, $8081
{$ENDIF}

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
        PADDD     XMM2, DQWORD PTR [SIMD_4x003FFF7F] // XMM2 <- ((ColorX - ColorY) * Weight * $8081) + Bias
{$else}
        PADDD     XMM2, DQWORD PTR [rip+SIMD_4x003FFF7F]
{$ifend}

        // Reduce 32-bits to 9-bits
        PSRLD     XMM2, 23                      // XMM2 <- (((ColorX - ColorY) * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
{$if (not defined(FPC)) or (not defined(TARGET_X64))}
        PSHUFB    XMM2, DQWORD PTR [SIMD_4x0C080400] // XMM2[0] <- XMM4[0..3][0]
{$else}
        PSHUFB    XMM2, DQWORD PTR [rip+SIMD_4x0C080400]
{$ifend}

        // Result := Value + ColorY
        PADDB     XMM2, XMM1                    // XMM2 <- XMM2 + ColorY
{$IFDEF TARGET_X86}
        MOVD      [EDX], XMM2                   // ColorY <- XMM2
{$ELSE}
        MOVD      [RDX], XMM2                   // ColorY <- XMM2
{$ENDIF}

@exit:
        RET

@return_x:
{$IFDEF TARGET_X86}
        MOV       [EDX], EAX                    // ColorY <- ColorX
{$ELSE}
        MOV       [RDX], ECX                    // ColorY <- ColorX
{$ENDIF}
end;


//------------------------------------------------------------------------------
// CombineLine
//------------------------------------------------------------------------------
procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: Cardinal); {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      Merge
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MergeReg
//------------------------------------------------------------------------------
function MergeReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  { This is an implementation of the merge formula, as described
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
  }

{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
end;


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
{$IFDEF TARGET_X86}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PADDUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PADDUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorSub
//------------------------------------------------------------------------------
function ColorSub_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PSUBUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PSUBUSB   XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorModulate
//------------------------------------------------------------------------------
function ColorModulate_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        PXOR      XMM2,XMM2
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        PMULLW    XMM0,XMM1
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        PXOR      XMM2,XMM2
        MOVD      XMM0,ECX
        PUNPCKLBW XMM0,XMM2
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM2
        PMULLW    XMM0,XMM1
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorMax
//------------------------------------------------------------------------------
function ColorMax_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PMAXUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PMAXUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorMin
//------------------------------------------------------------------------------
function ColorMin_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        PMINUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        PMINUB    XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorDifference
//------------------------------------------------------------------------------
function ColorDifference_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      XMM0,EAX
        MOVD      XMM1,EDX
        MOVQ      XMM2,XMM0
        PSUBUSB   XMM0,XMM1
        PSUBUSB   XMM1,XMM2
        POR       XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      XMM0,ECX
        MOVD      XMM1,EDX
        MOVQ      XMM2,XMM0
        PSUBUSB   XMM0,XMM1
        PSUBUSB   XMM1,XMM2
        POR       XMM0,XMM1
        MOVD      EAX,XMM0
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorExclusion
//------------------------------------------------------------------------------
function ColorExclusion_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
// ColorScale
//------------------------------------------------------------------------------
function ColorScale_SSE2(C: TColor32; W: Cardinal): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        PXOR      XMM2,XMM2
        SHL       EDX,4
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM2
        ADD       EDX,alpha_ptr
        PMULLW    XMM0,[EDX]
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
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
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
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
{$IFDEF TARGET_X86}
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
        PADDD     XMM1, DQWORD PTR [SIMD_4x003FFF7F] // XMM1 <- (Color * Weight * $8081) + Bias

        // Reduce 32-bits to 9-bits
        PSRLD     XMM1, 23                      // XMM1 <- ((Color * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
        PSHUFB    XMM1, DQWORD PTR [SIMD_4x0C080400] // XMM1[0] <- XMM1[0..3][0]

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
{$ENDIF}

{$IFDEF TARGET_X64}
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
        PADDD     XMM1, DQWORD PTR [SIMD_4x003FFF7F] // XMM1 <- (Color * Weight * $8081) + Bias
{$else}
        PADDD     XMM1, DQWORD PTR [rip+SIMD_4x003FFF7F]
{$ifend}

        // Reduce 32-bits to 9-bits
        PSRLD     XMM1, 23                      // XMM1 <- ((Color * Weight * $8081) + Bias) shr 23

        // Convert from dwords to bytes with truncation (losing the sign in the 9th bit)
{$if (not defined(FPC))}
        PSHUFB    XMM1, DQWORD PTR [SIMD_4x0C080400] // XMM1[0] <- XMM1[0..3][0]
{$else}
        PSHUFB    XMM1, DQWORD PTR [rip+SIMD_4x0C080400]
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
{$ENDIF}
end;

procedure FastScaleMems_SSE41(Dst: PColor32; Count: Integer; Weight: Cardinal); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  //
  // Result Z = W * Bargb
  //
  // Approximates (x div 255) as (x shr 8); Same as ColorScale_Pas
  //
{$IFDEF TARGET_X86}
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
{$ENDIF}

{$IFDEF TARGET_X64}
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
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
{$IFNDEF PUREPASCAL}
procedure RegisterBindingFunctions;
begin
{$IFNDEF OMIT_SSE2}

  BlendRegistry.BindingsByID[FID_MERGEREG].Add(      @MergeReg_SSE2,         [isSSE2]).Name := 'MergeReg_SSE2';
  BlendRegistry.BindingsByID[FID_COMBINEREG].Add(    @CombineReg_SSE2,       [isSSE2]).Name := 'CombineReg_SSE2';
  BlendRegistry.BindingsByID[FID_COMBINEMEM].Add(    @CombineMem_SSE2_128,   [isSSE2]).Name := 'CombineMem_SSE2_128';
{$ifndef FPC} // CombineMem_SSE41_Kadaif is currently broken on FPC
  BlendRegistry.BindingsByID[FID_COMBINEMEM].Add(    @CombineMem_SSE41_Kadaif, [isSSE41]).Name := 'CombineMem_SSE41_Kadaif';
{$else}
  BlendRegistry.BindingsByID[FID_COMBINEMEM].Add(    @CombineMem_SSE41_8081, [isSSE41]).Name := 'CombineMem_SSE41_8081';
{$endif}
{$ifdef BENCHMARK}
  BlendRegistry.BindingsByID[FID_COMBINEMEM].Add(    @CombineMem_SSE2_Table, [isSSE2], BindingPriorityWorse).Name := 'CombineMem_SSE2_Table';
{$ifndef FPC}
  BlendRegistry.BindingsByID[FID_COMBINEMEM].Add(    @CombineMem_SSE41_8081, [isSSE41], BindingPriorityWorse).Name := 'CombineMem_SSE41_8081';
{$endif}
{$endif}
  BlendRegistry.BindingsByID[FID_COMBINELINE].Add(   @CombineLine_SSE2,      [isSSE2]).Name := 'CombineLine_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDREG].Add(      @BlendReg_SSE2,         [isSSE2]).Name := 'BlendReg_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDMEM].Add(      @BlendMem_SSE2,         [isSSE2]).Name := 'BlendMem_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDMEMS].Add(     @BlendMems_SSE2,        [isSSE2]).Name := 'BlendMems_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDMEMEX].Add(    @BlendMemEx_SSE2,       [isSSE2]).Name := 'BlendMemEx_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDLINE].Add(     @BlendLine_SSE2,        [isSSE2]).Name := 'BlendLine_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDLINEEX].Add(   @BlendLineEx_SSE2,      [isSSE2]).Name := 'BlendLineEx_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDREGEX].Add(    @BlendRegEx_SSE2,       [isSSE2]).Name := 'BlendRegEx_SSE2';
  BlendRegistry.BindingsByID[FID_COLORMAX].Add(      @ColorMax_SSE2,         [isSSE2]).Name := 'ColorMax_SSE2';
  BlendRegistry.BindingsByID[FID_COLORMIN].Add(      @ColorMin_SSE2,         [isSSE2]).Name := 'ColorMin_SSE2';
  BlendRegistry.BindingsByID[FID_COLORADD].Add(      @ColorAdd_SSE2,         [isSSE2]).Name := 'ColorAdd_SSE2';
  BlendRegistry.BindingsByID[FID_COLORSUB].Add(      @ColorSub_SSE2,         [isSSE2]).Name := 'ColorSub_SSE2';
  BlendRegistry.BindingsByID[FID_COLORMODULATE].Add( @ColorModulate_SSE2,    [isSSE2]).Name := 'ColorModulate_SSE2';
  BlendRegistry.BindingsByID[FID_COLORDIFFERENCE].Add(@ColorDifference_SSE2,  [isSSE2]).Name := 'ColorDifference_SSE2';
  BlendRegistry.BindingsByID[FID_COLOREXCLUSION].Add(@ColorExclusion_SSE2,   [isSSE2]).Name := 'ColorExclusion_SSE2';
  BlendRegistry.BindingsByID[FID_COLORSCALE].Add(    @ColorScale_SSE2,       [isSSE2]).Name := 'ColorScale_SSE2';
  BlendRegistry.BindingsByID[FID_LIGHTEN].Add(       @LightenReg_SSE2,       [isSSE]).Name := 'LightenReg_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDREGRGB].Add(   @BlendRegRGB_SSE2,      [isSSE2]).Name := 'BlendRegRGB_SSE2';
  BlendRegistry.BindingsByID[FID_BLENDMEMRGB].Add(   @BlendMemRGB_SSE2,      [isSSE2]).Name := 'BlendMemRGB_SSE2';
{$ifdef GR32_SCALEMEMS_FAST}
  BlendRegistry.Bindings[@@ScaleMems].Add(           @FastScaleMems_SSE41,[isSSE41]).Name := 'FastScaleMems_SSE41';
{$else}
  BlendRegistry.Bindings[@@ScaleMems].Add(           @ScaleMems_SSE41,    [isSSE41]).Name := 'ScaleMems_SSE41';
{$endif}
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendRegistry.BindingsByID[FID_BLENDMEMRGB128].Add(@BlendMemRGB128_SSE4,   [isSSE2]).Name := 'BlendMemRGB128_SSE4';
{$ENDIF}

{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
{$IFNDEF PUREPASCAL}
  RegisterBindingFunctions;
{$ENDIF}
end.
