unit GR32_BlendSSE2;

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

function BlendReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMem_SSE2(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMems_SSE2(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegEx_SSE2(F, B, M: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: TColor32); {$IFDEF FPC} assembler; {$ENDIF}

function BlendRegRGB_SSE2(F, B, W: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; {$ENDIF}

procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; {$ENDIF}
procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: TColor32); {$IFDEF FPC} assembler; {$ENDIF}

function CombineReg_SSE2(X, Y, W: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
procedure CombineMem_SSE2(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; {$ENDIF}
procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: TColor32); {$IFDEF FPC} assembler; {$ENDIF}

function MergeReg_SSE2(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}

procedure EMMS_SSE2; {$IFDEF FPC} assembler; {$ENDIF}

function LightenReg_SSE2(C: TColor32; Amount: Integer): TColor32; {$IFDEF FPC} assembler; {$ENDIF}

function ColorAdd_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorSub_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorModulate_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorMax_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorMin_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorDifference_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorExclusion_SSE2(C1, C2: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}
function ColorScale_SSE2(C, W: TColor32): TColor32; {$IFDEF FPC} assembler; {$ENDIF}

implementation

uses
  GR32_Blend,
  GR32_LowLevel,
  GR32_System;

{ SSE2 versions }

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

procedure BlendMems_SSE2(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        TEST      ECX,ECX
        JZ        @2

        TEST      EAX,$FF000000
        JZ        @2

        PUSH      EBX

        MOV       EBX,EAX
        SHR       EBX,24

        CMP       EBX,$FF
        JZ        @3

        MOVD      XMM4,EAX
        PXOR      XMM3,XMM3
        PUNPCKLBW XMM4,XMM3
        MOV       EBX,bias_ptr

@1:
        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM4
        PUNPCKLBW XMM1,XMM3
        PUNPCKHWD XMM1,XMM1
        MOVQ      XMM0,XMM4
        PSUBW     XMM0,XMM2
        PUNPCKHDQ XMM1,XMM1
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[EBX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [EDX],XMM2

        ADD       EDX,4

        DEC       ECX
        JNZ       @1

        POP       EBX

@2:
        RET

@3:
        MOV       [EDX],EAX
        ADD       EDX,4

        DEC       ECX
        JNZ       @3

        POP       EBX
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST      R8D,R8D
        JZ        @2

        TEST      ECX,$FF000000
        JZ        @2

        MOV       RAX,RCX
        SHR       EAX,24

        CMP       EAX,$FF
        JZ        @3

        MOVD      XMM4,ECX
        PXOR      XMM3,XMM3
        PUNPCKLBW XMM4,XMM3
        MOV       RAX,bias_ptr

@1:
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM4
        PUNPCKLBW XMM1,XMM3
        PUNPCKHWD XMM1,XMM1
        MOVQ      XMM0,XMM4
        PSUBW     XMM0,XMM2
        PUNPCKHDQ XMM1,XMM1
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[RAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [RDX], XMM2

        ADD       RDX,4

        DEC       R8D
        JNZ       @1

@2:
        RET

@3:
        MOV       [RDX],ECX
        ADD       RDX,4

        DEC       R8D
        JNZ       @3
{$ENDIF}
end;


function BlendRegEx_SSE2(F, B, M: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

        PXOR      XMM0,XMM0
        MOVD      XMM1,EAX
        SHL       ECX,4
        MOVD      XMM2,[EDX]
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

function BlendRegRGB_SSE2(F, B, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

procedure BlendMemRGB_SSE2(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
{$IFDEF FPC}
const
  COpaque: QWORD = $FF000000FF000000;
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


procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
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
        JS        @4
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

function CombineReg_SSE2(X, Y, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

procedure CombineMem_SSE2(F: TColor32; var B: TColor32; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ    @1

        CMP       ECX,$FF
        JZ        @2

        MOVD      XMM1,EAX
        PXOR      XMM0,XMM0

        SHL       ECX,4

        MOVD      XMM2,[EDX]
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

@1:     RET

@2:     MOV       [EDX],EAX
{$ENDIF}

{$IFDEF TARGET_X64}
  // ECX - Color X
  // [RDX] - Color Y
  // R8D - Weight of X [0..255]
  // Result := W * (X - Y) + Y

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


procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

procedure EMMS_SSE2; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
end;


function LightenReg_SSE2(C: TColor32; Amount: Integer): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_X86}
        MOVD    XMM0,EAX
        TEST    EDX,EDX
        JL      @1
        IMUL    EDX,$010101
        MOVD    XMM1,EDX
        PADDUSB XMM0,XMM1
        MOVD    EAX,XMM0
        RET
@1:     NEG     EDX
        IMUL    EDX,$010101
        MOVD    XMM1,EDX
        PSUBUSB XMM0,XMM1
        MOVD    EAX,XMM0
{$ENDIF}

{$IFDEF TARGET_X64}
        MOVD    XMM0,ECX
        TEST    EDX,EDX
        JL      @1
        IMUL    EDX,$010101
        MOVD    XMM1,EDX
        PADDUSB XMM0,XMM1
        MOVD    EAX,XMM0
        RET
@1:     NEG     EDX
        IMUL    EDX,$010101
        MOVD    XMM1,EDX
        PSUBUSB XMM0,XMM1
        MOVD    EAX,XMM0
{$ENDIF}
end;


{ SSE2 Color algebra}

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

function ColorScale_SSE2(C, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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

end.
