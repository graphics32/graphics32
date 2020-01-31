unit GR32_BlendMMX;

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
 * Contributor(s):
 *  Christian-W. Budde
 *      - 2019/04/01 - Refactoring
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  GR32;

{$IFNDEF OMIT_MMX}
function BlendReg_MMX(F, B: TColor32): TColor32;
procedure BlendMem_MMX(F: TColor32; var B: TColor32);

function BlendRegEx_MMX(F, B, M: TColor32): TColor32;
procedure BlendMemEx_MMX(F: TColor32; var B:TColor32; M: TColor32);

function BlendRegRGB_MMX(F, B, W: TColor32): TColor32;
procedure BlendMemRGB_MMX(F: TColor32; var B: TColor32; W: TColor32);

procedure BlendLine_MMX(Src, Dst: PColor32; Count: Integer);
procedure BlendLineEx_MMX(Src, Dst: PColor32; Count: Integer; M: TColor32);

function CombineReg_MMX(X, Y, W: TColor32): TColor32;
procedure CombineMem_MMX(F: TColor32; var B: TColor32; W: TColor32);
procedure CombineLine_MMX(Src, Dst: PColor32; Count: Integer; W: TColor32);

procedure EMMS_MMX;

function LightenReg_MMX(C: TColor32; Amount: Integer): TColor32;

function ColorAdd_MMX(C1, C2: TColor32): TColor32;
function ColorSub_MMX(C1, C2: TColor32): TColor32;
function ColorModulate_MMX(C1, C2: TColor32): TColor32;
function ColorMax_EMMX(C1, C2: TColor32): TColor32;
function ColorMin_EMMX(C1, C2: TColor32): TColor32;
function ColorDifference_MMX(C1, C2: TColor32): TColor32;
function ColorExclusion_MMX(C1, C2: TColor32): TColor32;
function ColorScale_MMX(C, W: TColor32): TColor32;
{$ENDIF}

implementation

uses
  GR32_Blend,
  GR32_LowLevel,
  GR32_System;

{ MMX versions }

function BlendReg_MMX(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
{$IFDEF TARGET_x86}
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb
        MOVD      MM0,EAX
        PXOR      MM3,MM3
        MOVD      MM2,EDX
        PUNPCKLBW MM0,MM3
        MOV       ECX,bias_ptr
        PUNPCKLBW MM2,MM3
        MOVQ      MM1,MM0
        PUNPCKHWD MM1,MM1
        PSUBW     MM0,MM2
        PUNPCKHDQ MM1,MM1
        PSLLW     MM2,8
        PMULLW    MM0,MM1
        PADDW     MM2,[ECX]
        PADDW     MM2,MM0
        PSRLW     MM2,8
        PACKUSWB  MM2,MM3
        MOVD      EAX,MM2
        OR        EAX,$FF000000   // EAX  <-  FF Zr Zg Zb
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb
        MOVD      MM0,ECX
        PXOR      MM3,MM3
        MOVD      MM2,EDX
        PUNPCKLBW MM0,MM3
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PUNPCKLBW MM2,MM3
        MOVQ      MM1,MM0
        PUNPCKHWD MM1,MM1
        PSUBW     MM0,MM2
        PUNPCKHDQ MM1,MM1
        PSLLW     MM2,8
        PMULLW    MM0,MM1
        PADDW     MM2,[RAX]
        PADDW     MM2,MM0
        PSRLW     MM2,8
        PACKUSWB  MM2,MM3
        MOVD      EAX,MM2
        OR        EAX,$FF000000   // EAX  <-  FF Zr Zg Zb
{$ENDIF}
end;

{$IFDEF TARGET_x86}

procedure BlendMem_MMX(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        PXOR      MM3,MM3
        MOVD      MM0,EAX
        MOVD      MM2,[EDX]
        PUNPCKLBW MM0,MM3
        MOV       ECX,bias_ptr
        PUNPCKLBW MM2,MM3
        MOVQ      MM1,MM0
        PUNPCKHWD MM1,MM1
        PSUBW     MM0,MM2
        PUNPCKHDQ MM1,MM1
        PSLLW     MM2,8
        PMULLW    MM0,MM1
        PADDW     MM2,[ECX]
        PADDW     MM2,MM0
        PSRLW     MM2,8
        PACKUSWB  MM2,MM3
        MOVD      [EDX],MM2
        OR        [EDX],$FF000000

@1:     RET
@2:     MOV       [EDX],EAX
end;

function BlendRegEx_MMX(F, B, M: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Fargb - Bargb) + Bargb
        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        PXOR      MM0,MM0
        MOVD      MM1,EAX
        SHL       ECX,4
        MOVD      MM2,EDX
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        PSUBW     MM1,MM2
        PMULLW    MM1,[ECX]
        PSLLW     MM2,8
        MOV       ECX,bias_ptr
        PADDW     MM2,[ECX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      EAX,MM1

        POP       EBX
        RET

@1:     MOV       EAX,EDX
        POP       EBX
end;

{$ENDIF}

procedure BlendMemEx_MMX(F: TColor32; var B:TColor32; M: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        PXOR      MM0,MM0
        MOVD      MM1,EAX
        SHL       ECX,4
        MOVD      MM2,[EDX]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        PSUBW     MM1,MM2
        PMULLW    MM1,[ECX]
        PSLLW     MM2,8
        MOV       ECX,bias_ptr
        PADDW     MM2,[ECX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [EDX],MM1

@1:     POP       EBX

@2:
{$ENDIF}

{$IFDEF TARGET_x64}
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // ECX <- F
  // [EDX] <- B
  // R8 <- M
  // Result := M * Fa * (Fargb - Bargb) + Bargb
        TEST      ECX,$FF000000
        JZ        @1

        MOV       EAX,ECX
        SHR       EAX,24
        INC       R8D             // 255:256 range bias
        IMUL      R8D,EAX
        SHR       R8D,8
        JZ        @1

        PXOR      MM0,MM0
        MOVD      MM1,ECX
        SHL       R8D,4
        MOVD      MM2,[RDX]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0
{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}
        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [RDX],MM1

@1:
{$ENDIF}
end;

function BlendRegRGB_MMX(F, B, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PXOR      MM2,MM2
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        BSWAP     ECX
        PSUBW     MM0,MM1
        MOVD      MM3,ECX
        PUNPCKLBW MM3,MM2
        PMULLW    MM0,MM3
        MOV       EAX,bias_ptr
        PSLLW     MM1,8
        PADDW     MM1,[EAX]
        PADDW     MM1,MM0
        PSRLW     MM1,8
        PACKUSWB  MM1,MM2
        MOVD      EAX,MM1
{$ENDIF}

{$IFDEF TARGET_x64}
        PXOR      MM2,MM2
        MOVD      MM0,ECX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        BSWAP     R8D
        PSUBW     MM0,MM1
        MOVD      MM3,R8D
        PUNPCKLBW MM3,MM2
        PMULLW    MM0,MM3
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PSLLW     MM1,8
        PADDW     MM1,[RAX]
        PADDW     MM1,MM0
        PSRLW     MM1,8
        PACKUSWB  MM1,MM2
        MOVD      EAX,MM1
{$ENDIF}
end;

procedure BlendMemRGB_MMX(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PXOR      MM2,MM2
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,[EDX]
        PUNPCKLBW MM1,MM2
        BSWAP     ECX
        PSUBW     MM0,MM1
        MOVD      MM3,ECX
        PUNPCKLBW MM3,MM2
        PMULLW    MM0,MM3
        MOV       EAX,bias_ptr
        PSLLW     MM1,8
        PADDW     MM1,[EAX]
        PADDW     MM1,MM0
        PSRLW     MM1,8
        PACKUSWB  MM1,MM2
        MOVD      [EDX],MM1
{$ENDIF}

{$IFDEF TARGET_x64}
        PXOR      MM2,MM2
        MOVD      MM0,ECX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,[EDX]
        PUNPCKLBW MM1,MM2
        BSWAP     R8D
        PSUBW     MM0,MM1
        MOVD      MM3,R8D
        PUNPCKLBW MM3,MM2
        PMULLW    MM0,MM3
{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PSLLW     MM1,8
        PADDW     MM1,[RAX]
        PADDW     MM1,MM0
        PSRLW     MM1,8
        PACKUSWB  MM1,MM2
        MOVD      [EDX],MM1
{$ENDIF}
end;


{$IFDEF TARGET_x86}
procedure BlendLine_MMX(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX,ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI

        MOV       ESI,EAX         // ESI <- Src
        MOV       EDI,EDX         // EDI <- Dst

  // loop start
@1:     MOV       EAX,[ESI]
        TEST      EAX,$FF000000
        JZ        @3              // complete transparency, proceed to next point
        CMP       EAX,$FF000000
        JNC       @2              // opaque pixel, copy without blending

  // blend
        MOVD      MM0,EAX         // MM0  <-  00 00 00 00 Fa Fr Fg Fb
        PXOR      MM3,MM3         // MM3  <-  00 00 00 00 00 00 00 00
        MOVD      MM2,[EDI]       // MM2  <-  00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW MM0,MM3         // MM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
        MOV       EAX,bias_ptr
        PUNPCKLBW MM2,MM3         // MM2  <-  00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      MM1,MM0         // MM1  <-  00 Fa 00 Fr 00 Fg 00 Fb
        PUNPCKHWD MM1,MM1         // MM1  <-  00 Fa 00 Fa 00 ** 00 **
        PSUBW     MM0,MM2         // MM0  <-  00 Da 00 Dr 00 Dg 00 Db
        PUNPCKHDQ MM1,MM1         // MM1  <-  00 Fa 00 Fa 00 Fa 00 Fa
        PSLLW     MM2,8           // MM2  <-  Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    MM0,MM1         // MM0  <-  Pa ** Pr ** Pg ** Pb **
        PADDW     MM2,[EAX]       // add bias
        PADDW     MM2,MM0         // MM2  <-  Qa ** Qr ** Qg ** Qb **
        PSRLW     MM2,8           // MM2  <-  00 Qa 00 Qr 00 Qg 00 Qb
        PACKUSWB  MM2,MM3         // MM2  <-  00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,MM2         // EAX  <-  Qa Qr Qg Qb
        OR        EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:
end;

procedure BlendLineEx_MMX(Src, Dst: PColor32; Count: Integer; M: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX,ECX
        JS        @4

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
        JZ        @3              // complete transparency, proceed to next point

  // blend
        PXOR      MM0,MM0
        MOVD      MM1,EAX
        SHL       EBX,4
        MOVD      MM2,[EDI]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0
        ADD       EBX,alpha_ptr
        PSUBW     MM1,MM2
        PMULLW    MM1,[EBX]
        PSLLW     MM2,8
        MOV       EBX,bias_ptr
        PADDW     MM2,[EBX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      EAX,MM1

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
end;

{$ENDIF}

function CombineReg_MMX(X, Y, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      MM1,EAX
        PXOR      MM0,MM0
        SHL       ECX,4

        MOVD      MM2,EDX
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0

        ADD       ECX,alpha_ptr

        PSUBW     MM1,MM2
        PMULLW    MM1,[ECX]
        PSLLW     MM2,8

        MOV       ECX,bias_ptr

        PADDW     MM2,[ECX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      EAX,MM1
{$ENDIF}

{$IFDEF TARGET_X64}
  // ECX - Color X
  // EDX - Color Y
  // R8 - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      MM1,ECX
        PXOR      MM0,MM0
        SHL       R8D,4

        MOVD      MM2,EDX
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0

{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}

        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8

{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}

        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      EAX,MM1
{$ENDIF}
end;

procedure CombineMem_MMX(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ      @1
        CMP       ECX,$FF
        JZ        @2

        MOVD      MM1,EAX
        PXOR      MM0,MM0

        SHL       ECX,4

        MOVD      MM2,[EDX]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0

        ADD       ECX,alpha_ptr

        PSUBW     MM1,MM2
        PMULLW    MM1,[ECX]
        PSLLW     MM2,8

        MOV       ECX,bias_ptr

        PADDW     MM2,[ECX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [EDX],MM1

@1:     RET

@2:     MOV       [EDX],EAX
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX - Color X
  // [RDX] - Color Y
  // R8 - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        TEST      R8D,R8D            // Set flags for R8
        JZ        @1                 // W = 0 ?  => Result := EDX
        CMP       R8D,$FF
        JZ        @2

        MOVD      MM1,ECX
        PXOR      MM0,MM0

        SHL       R8D,4

        MOVD      MM2,[RDX]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0

{$IFNDEF FPC}
        ADD       R8,alpha_ptr
{$ELSE}
        ADD       R8,[RIP+alpha_ptr]
{$ENDIF}

        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8

{$IFNDEF FPC}
        MOV       RAX,bias_ptr
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}

        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [RDX],MM1

@1:     RET

@2:     MOV       [RDX],RCX
{$ENDIF}
end;

{$IFDEF TARGET_x86}

procedure CombineLine_MMX(Src, Dst: PColor32; Count: Integer; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // Result := W * (X - Y) + Y

        TEST      ECX,ECX
        JS        @3

        PUSH      EBX
        MOV       EBX,W

        TEST      EBX,EBX
        JZ        @2              // weight is zero

        CMP       EBX,$FF
        JZ        @4              // weight = 255  =>  copy src to dst

        SHL       EBX,4
        ADD       EBX,alpha_ptr
        MOVQ      MM3,[EBX]
        MOV       EBX,bias_ptr
        MOVQ      MM4,[EBX]

   // loop start
@1:     MOVD      MM1,[EAX]
        PXOR      MM0,MM0
        MOVD      MM2,[EDX]
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0

        PSUBW     MM1,MM2
        PMULLW    MM1,MM3
        PSLLW     MM2,8

        PADDW     MM2,MM4
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [EDX],MM1

        ADD       EAX,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1
@2:     POP       EBX
        POP       EBP
@3:     RET       $0004

@4:     CALL      GR32_LowLevel.MoveLongword
        POP       EBX
end;

{$ENDIF}

procedure EMMS_MMX; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  EMMS
end;

function LightenReg_MMX(C: TColor32; Amount: Integer): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD    MM0,EAX
        TEST    EDX,EDX
        JL      @1
        IMUL    EDX,$010101
        MOVD    MM1,EDX
        PADDUSB MM0,MM1
        MOVD    EAX,MM0
        RET
@1:     NEG     EDX
        IMUL    EDX,$010101
        MOVD    MM1,EDX
        PSUBUSB MM0,MM1
        MOVD    EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD    MM0,ECX
        TEST    EDX,EDX
        JL      @1
        IMUL    EDX,$010101
        MOVD    MM1,EDX
        PADDUSB MM0,MM1
        MOVD    EAX,MM0
        RET
@1:     NEG     EDX
        IMUL    EDX,$010101
        MOVD    MM1,EDX
        PSUBUSB MM0,MM1
        MOVD    EAX,MM0
{$ENDIF}
end;

{ MMX Color algebra versions }

function ColorAdd_MMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PADDUSB   MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      MM0,ECX
        MOVD      MM1,EDX
        PADDUSB   MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorSub_MMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PSUBUSB   MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      MM0,ECX
        MOVD      MM1,EDX
        PSUBUSB   MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorModulate_MMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        PXOR      MM2,MM2
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        PMULLW    MM0,MM1
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        PXOR      MM2,MM2
        MOVD      MM0,ECX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        PMULLW    MM0,MM1
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorMax_EMMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PMAXUB    MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      MM0,ECX
        MOVD      MM1,EDX
        PMAXUB    MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorMin_EMMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PMINUB    MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      MM0,ECX
        MOVD      MM1,EDX
        PMINUB    MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorDifference_MMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        MOVQ      MM2,MM0
        PSUBUSB   MM0,MM1
        PSUBUSB   MM1,MM2
        POR       MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD      MM0,ECX
        MOVD      MM1,EDX
        MOVQ      MM2,MM0
        PSUBUSB   MM0,MM1
        PSUBUSB   MM1,MM2
        POR       MM0,MM1
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorExclusion_MMX(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        PXOR      MM2,MM2
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        MOVQ      MM3,MM0
        PADDW     MM0,MM1
        PMULLW    MM1,MM3
        PSRLW     MM1,7
        PSUBUSW   MM0,MM1
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        PXOR      MM2,MM2
        MOVD      MM0,ECX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        MOVQ      MM3,MM0
        PADDW     MM0,MM1
        PMULLW    MM1,MM3
        PSRLW     MM1,7
        PSUBUSW   MM0,MM1
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}
end;

function ColorScale_MMX(C, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        PXOR      MM2,MM2
        SHL       EDX,4
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        ADD       EDX,alpha_ptr
        PMULLW    MM0,[EDX]
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}

{$IFDEF TARGET_X64}
        PXOR      MM2,MM2
        SHL       RDX,4
        MOVD      MM0,ECX
        PUNPCKLBW MM0,MM2
{$IFNDEF FPC}
        ADD       RDX,alpha_ptr
{$ELSE}
        ADD       RDX,[RIP+alpha_ptr]
{$ENDIF}
        PMULLW    MM0,[RDX]
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}
end;

end.
