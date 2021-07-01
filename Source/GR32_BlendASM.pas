unit GR32_BlendASM;

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

function BlendReg_ASM(F, B: TColor32): TColor32;
procedure BlendMem_ASM(F: TColor32; var B: TColor32);
procedure BlendMems_ASM(F: TColor32; B: PColor32; Count: Integer);

function BlendRegEx_ASM(F, B, M: TColor32): TColor32;
procedure BlendMemEx_ASM(F: TColor32; var B:TColor32; M: TColor32);

procedure BlendLine_ASM(Src, Dst: PColor32; Count: Integer);
procedure BlendLine1_ASM(Src: TColor32; Dst: PColor32; Count: Integer);

function CombineReg_ASM(X, Y, W: TColor32): TColor32;
procedure CombineMem_ASM(X: TColor32; var Y: TColor32; W: TColor32);

{$IFDEF TARGET_x86}
function MergeReg_ASM(F, B: TColor32): TColor32;
{$ENDIF}

procedure EMMS_ASM;

implementation

uses
  GR32_Blend,
  GR32_LowLevel,
  GR32_System;

{ ASM versions }

{ Assembler versions }

const
  bias = $00800080;


function BlendReg_ASM(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Fargb + (1 - Fa) * Bargb
  // Result Z =    P      +        Q

{$IFDEF TARGET_x86}
  // EAX <- F
  // EDX <- B

// Test Fa = 255 ?
        CMP     EAX,$FF000000   // Fa = 255 ? => Result = EAX
        JNC     @2

  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => Result = EDX
        JZ      @1

  // Get weight W = Fa
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

        PUSH    EBX

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr 00 Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX,EDX
@2:
{$ENDIF}

  // EAX <- F
  // EDX <- B
{$IFDEF TARGET_x64}
        MOV     RAX, RCX

  // Test Fa = 255 ?
        CMP     EAX,$FF000000   // Fa = 255 ? => Result = EAX
        JNC     @2

  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => Result = EDX
        JZ      @1

  // Get weight W = Fa
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // P = W * F
        MOV     R9D,EAX         // R9D  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     R9D,$FF00FF00   // R9D  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     R9D,8           // R9D  <-  00 Fa 00 Fg
        IMUL    R9D,ECX         // R9D  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Pa 00 Pg 00
        OR      EAX,R9D         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     R9D,EDX         // R9D  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     R9D,$FF00FF00   // R9D  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     R9D,8           // R9D  <-  00 Ba 00 Bg
        IMUL    R9D,ECX         // R9D  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr 00 Qb
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qa 00 Qg 00
        OR      R9D,EDX         // R9D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R9D         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb
        RET

@1:     MOV     EAX,EDX
@2:
{$ENDIF}
end;

procedure BlendMem_ASM(F: TColor32; var B: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- F
  // [EDX] <- B

  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => do not write
        JZ      @2

  // Get weight W = Fa
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @1

        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias        // add bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias        // add bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

        MOV     ESI,[EDX]

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX

  // Q = W * B
        MOV     EBX,ESI         // EBX  <-  Ba Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     ESI,bias        // add bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr 00 Qb
        ADD     EBX,bias        // add bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        MOV     [EDX],EAX
        POP     ESI
        POP     EBX
        RET

@1:     MOV     [EDX],EAX
@2:
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- F
  // [RDX] <- B

  // Test Fa = 0 ?
        TEST    ECX,$FF000000   // Fa = 0 ?   => do not write
        JZ      @2

        MOV     EAX, ECX        // EAX  <-  Fa Fr Fg Fb

  // Get weight W = Fa
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

        // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @1

  // P = W * F
        MOV     R8D,EAX         // R8D  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     R8D,$FF00FF00   // R8D  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     R8D,8           // R8D  <-  00 Fa 00 Fg
        IMUL    R8D,ECX         // R8D  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Pa 00 Pg 00
        OR      EAX,R8D         // EAX  <-  Pa Pr Pg Pb

        MOV     R9D,[RDX]

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     R8D,R9D         // R8D  <-  Ba Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     R8D,$FF00FF00   // R8D  <-  Ba 00 Bg 00
        IMUL    R9D,ECX         // R9D  <-  Qr ** Qb **
        SHR     R8D,8           // R8D  <-  00 Ba 00 Bg
        IMUL    R8D,ECX         // R8D  <-  Qa ** Qg **
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qr 00 Qb 00
        SHR     R9D,8           // R9D  <-  00 Qr 00 Qb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Qa 00 Qg 00
        OR      R8D,R9D         // R8D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R8D         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        MOV     [RDX],EAX
        RET

@1:     MOV     [RDX],EAX
@2:
{$ENDIF}
end;

procedure BlendMems_ASM(F: TColor32; B: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        TEST    ECX,ECX
        JZ      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

@1:
  // Test Fa = 0 ?
        MOV     EAX,[ESI]
        TEST    EAX,$FF000000
        JZ      @3

        PUSH    ECX

  // Get weight W = Fa
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @2

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias        // add bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias        // add bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

        MOV     EDX,[EDI]

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX

  // Q = W * B
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias        // add bias
        AND     EDX,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX,8           // ESI  <-  00 Qr 00 Qb
        ADD     EBX,bias        // add bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

@2:
        OR      EAX,$FF000000
        MOV     [EDI],EAX
        POP     ECX

@3:
        ADD     ESI,4
        ADD     EDI,4

        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:
        RET
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST    R8D,R8D
        JZ      @4

        PUSH    RDI

        MOV     R9,RCX
        MOV     RDI,RDX

@1:
        MOV     ECX,[RSI]
        TEST    ECX,$FF000000
        JZ      @3

        PUSH    R8

        MOV     R8D,ECX
        SHR     R8D,24

        CMP     R8D,$FF
        JZ      @2

        MOV     EAX,ECX
        AND     ECX,$00FF00FF
        AND     EAX,$FF00FF00
        IMUL    ECX,R8D
        SHR     EAX,8
        IMUL    EAX,R8D
        ADD     ECX,bias
        AND     ECX,$FF00FF00
        SHR     ECX,8
        ADD     EAX,bias
        AND     EAX,$FF00FF00
        OR      ECX,EAX

        MOV     EDX,[RDI]
        XOR     R8D,$000000FF
        MOV     EAX,EDX
        AND     EDX,$00FF00FF
        AND     EAX,$FF00FF00
        IMUL    EDX, R8D
        SHR     EAX,8
        IMUL    EAX,R8D
        ADD     EDX,bias
        AND     EDX,$FF00FF00
        SHR     EDX,8
        ADD     EAX,bias
        AND     EAX,$FF00FF00
        OR      EAX,EDX

        ADD     ECX,EAX
@2:
        OR      ECX,$FF000000
        MOV     [RDI],ECX
        POP     R8

@3:
        ADD     R9,4
        ADD     RDI,4

        DEC     R8D
        JNZ     @1

        POP     RDI

@4:
        RET
{$ENDIF}
end;

function BlendRegEx_ASM(F, B, M: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, in this case Graphics32 uses BlendReg
  // Result Z = Fa * M * Fargb + (1 - Fa * M) * Bargb
  // Result Z =      P        +        Q
  // EAX <- F
  // EDX <- B
  // ECX <- M

{$IFDEF TARGET_x86}

  // Check Fa > 0 ?
        TEST    EAX,$FF000000   // Fa = 0? => Result := EDX
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        INC     ECX             // 255:256 range bias
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        SHR     ECX,8           // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => Result := EDX

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,EDX         // EBX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  00 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr 00 Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        POP     EBX
        RET

@1:
        POP     EBX

@2:     MOV     EAX,EDX
{$ENDIF}

{$IFDEF TARGET_x64}
        MOV     EAX,ECX         // EAX  <-  Fa Fr Fg Fb
        TEST    EAX,$FF000000   // Fa = 0? => Result := EDX
        JZ      @1

  // Get weight W = Fa * M
        INC     R8D             // 255:256 range bias
        SHR     ECX,24          // ECX  <-  00 00 00 Fa
        IMUL    R8D,ECX         // R8D  <-  00 00  W **
        SHR     R8D,8           // R8D  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => Result := EDX

  // P = W * F
        MOV     ECX,EAX         // ECX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     ECX,$FF00FF00   // ECX  <-  Fa 00 Fg 00
        IMUL    EAX,R8D         // EAX  <-  Pr ** Pb **
        SHR     ECX,8           // ECX  <-  00 Fa 00 Fg
        IMUL    ECX,R8D         // ECX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pa 00 Pg 00
        OR      EAX,ECX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
  // Q = W * B
        MOV     ECX,EDX         // ECX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     ECX,$FF00FF00   // ECX  <-  Ba 00 Bg 00
        IMUL    EDX,R8D         // EDX  <-  Qr ** Qb **
        SHR     ECX,8           // ECX  <-  00 Ba 00 Bg
        IMUL    ECX,R8D         // ECX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Qa 00 Qg 00
        OR      ECX,EDX         // ECX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,ECX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        RET

@1:     MOV     EAX,EDX
{$ENDIF}
end;

procedure BlendMemEx_ASM(F: TColor32; var B: TColor32; M: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- F
  // [EDX] <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX,$FF000000   // Fa = 0? => write nothing
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        INC     ECX             // 255:256 range bias for M
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        ADD     ECX,bias
        SHR     ECX,8           // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W;
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,ESI         // EBX  <-  00 Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        MOV     [EDX],EAX
        POP     ESI

@1:     POP     EBX
@2:
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- F
  // [RDX] <- B
  // R8 <- M

  // ECX <- F
  // [EDX] <- B
  // R8 <- M

  // Check Fa > 0 ?
        TEST    ECX,$FF000000   // Fa = 0? => write nothing
        JZ      @1

  // Get weight W = Fa * M
        MOV     EAX,ECX         // EAX  <-  Fa Fr Fg Fb
        INC     R8D             // 255:256 range bias
        SHR     EAX,24          // EAX  <-  00 00 00 Fa
        IMUL    R8D,EAX         // R8D <-  00 00  W **
        ADD     R8D,bias
        SHR     R8D,8           // R8D <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

  // P = W * F
        MOV     EAX,ECX         // EAX  <-  ** Fr Fg Fb
        AND     ECX,$00FF00FF   // ECX  <-  00 Fr 00 Fb
        AND     EAX,$FF00FF00   // EAX  <-  Fa 00 Fg 00
        IMUL    ECX,R8D         // ECX  <-  Pr ** Pb **
        SHR     EAX,8           // EAX  <-  00 Fa 00 Fg
        IMUL    EAX,R8D         // EAX  <-  Pa 00 Pg **
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pr 00 Pb 00
        SHR     ECX,8           // ECX  <-  00 Pr 00 Pb
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pa 00 Pg 00
        OR      ECX,EAX         // ECX  <-  Pa Pr Pg Pb

  // W = 1 - W
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8
  // Q = W * B
        MOV     EAX,R9D         // EAX  <-  00 Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     EAX,$FF00FF00   // EAX  <-  Ba 00 Bg 00
        IMUL    R9D,R8D         // R9D  <-  Qr ** Qb **
        SHR     EAX,8           // EAX  <-  00 00 00 Bg
        IMUL    EAX,R8D         // EAX  <-  00 00 Qg **
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qr 00 Qb 00
        SHR     R9D,8           // R9D  <-  00 Qr ** Qb
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Qa 00 Qg 00
        OR      EAX,R9D         // EAX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     ECX,EAX         // ECX  <-  00 Zr Zg Zb

        MOV     [RDX],ECX

@1:
{$ENDIF}
end;

procedure BlendLine_ASM(Src, Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST    ECX,ECX
        JS      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX         // ESI <- Src
        MOV     EDI,EDX         // EDI <- Dst

  // loop start
@1:     MOV     EAX,[ESI]
        TEST    EAX,$FF000000
        JZ      @3              // complete transparency, proceed to next point

        PUSH    ECX             // store counter

  // Get weight W = Fa
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @2

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W;
        MOV     EDX,[EDI]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb
@2:
        MOV     [EDI],EAX

        POP     ECX             // restore counter

@3:
        ADD     ESI,4
        ADD     EDI,4

  // loop end
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:
{$ENDIF}

{$IFDEF TARGET_x64}
  // RCX <- Src
  // RDX <- Dst
  // R8 <- Count

  // test the counter for zero or negativity
        TEST    R8D,R8D
        JS      @4

        MOV     R10,RCX         // R10 <- Src
        MOV     R11,RDX         // R11 <- Dst
        MOV     ECX,R8D         // RCX <- Count

  // loop start
@1:
        MOV     EAX,[R10]
        TEST    EAX,$FF000000
        JZ      @3              // complete transparency, proceed to next point

  // Get weight W = Fa
        MOV     R9D,EAX        // R9D  <-  Fa Fr Fg Fb
        SHR     R9D,24         // R9D  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     R9D,$FF
        JZ      @2

  // P = W * F
        MOV     R8D,EAX         // R8D  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     R8D,$FF00FF00   // R8D  <-  Fa 00 Fg 00
        IMUL    EAX,R9D         // EAX  <-  Pr ** Pb **
        SHR     R8D,8           // R8D  <-  00 Fa 00 Fg
        IMUL    R8D,R9D         // R8D  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Pa 00 Pg 00
        OR      EAX,R8D         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W;
        MOV     EDX,[R11]
        XOR     R9D,$000000FF   // R9D  <-  1 - R9D
  // Q = W * B
        MOV     R8D,EDX         // R8D  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     R8D,$FF00FF00   // R8D  <-  Ba 00 Bg 00
        IMUL    EDX,R9D         // EDX  <-  Qr ** Qb **
        SHR     R8D,8           // R8D  <-  00 Ba 00 Bg
        IMUL    R8D,R9D         // R8D  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Qa 00 Qg 00
        OR      R8D,EDX         // R8D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R8D         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb
@2:
        MOV     [R11],EAX

@3:
        ADD     R10,4
        ADD     R11,4

  // loop end
        DEC     ECX
        JNZ     @1

@4:
{$ENDIF}
end;

procedure BlendLine1_ASM(Src: TColor32; Dst: PColor32; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST    ECX,ECX
        JS      @4

  // test if source if fully transparent
        TEST    EAX,$FF000000
        JZ      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX         // ESI <- Src
        MOV     EDI,EDX         // EDI <- Dst

  // Get weight W = Fa
        SHR     ESI, 24         // ESI <- W

  // test if source is fully opaque
        CMP     ESI,$FF
        JZ      @4

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ESI         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ESI         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb
        XOR     ESI,$000000FF   // ESI  <-  1 - Fa

  // loop start
@1:
        MOV     EDX,[EDI]
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ESI         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ESI         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EBX,EAX         // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000   // EAX  <-  FF Zr Zg Zb

        OR      EBX,$FF000000
        MOV     [EDI],EBX

        ADD     EDI,4

        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@3:
        RET

@4:
        MOV     [EDI],EAX
        ADD     EDI,4

        DEC     ECX
        JNZ     @4

        POP     EDI
        POP     ESI
        POP     EBX

{$ENDIF}

{$IFDEF TARGET_x64}
  // RCX <- Src
  // RDX <- Dst
  // R8 <- Count

  // test the counter for zero or negativity
        TEST    R8D,R8D          // R8D <- Count
        JZ      @2

  // test if source if fully transparent
        TEST    ECX,$FF000000
        JZ      @2

        PUSH    RDI

        MOV     RDI,RDX           // RDI <- Dst
        MOV     R9D,ECX           // R9D <- Src

  // Get weight W = Fa
        SHR     R9D,24            // R9D <- W

  // Test Fa = 255 ?
        CMP     R9D,$FF
        JZ      @3                // complete opaque,copy source

  // P = W * F
        MOV     EAX,ECX           // EAX  <-  Fa Fr Fg Fb
        AND     ECX,$00FF00FF     // ECX  <-  00 Fr 00 Fb
        AND     EAX,$FF00FF00     // EAX  <-  Fa 00 Fg 00
        IMUL    ECX,R9D           // ECX  <-  Pr ** Pb **
        SHR     EAX,8             // EAX  <-  00 Fa 00 Fg
        IMUL    EAX,R9D           // EAX  <-  Pa ** Pg **
        ADD     ECX,Bias
        AND     ECX,$FF00FF00     // ECX  <-  Pr 00 Pb 00
        SHR     ECX,8             // ECX  <-  00 Pr 00 Pb
        ADD     EAX,Bias
        AND     EAX,$FF00FF00     // EAX  <-  Pa 00 Pg 00
        OR      ECX,EAX           // ECX  <-  Pa Pr Pg Pb
        XOR     R9D,$000000FF     // R9D  <-  1 - Fa

  // loop start
@1:
        MOV     EDX,[RDI]
        MOV     EAX,EDX           // EAX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF     // EDX  <-  00 Br 00 Bb
        AND     EAX,$FF00FF00     // EAX  <-  Ba 00 Bg 00
        IMUL    EDX,R9D           // EDX  <-  Qr ** Qb **
        SHR     EAX,8             // EAX  <-  00 Ba 00 Bg
        IMUL    EAX,R9D           // EAX  <-  Qa ** Qg **
        ADD     EDX,Bias
        AND     EDX,$FF00FF00     // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8             // EDX  <-  00 Qr ** Qb
        ADD     EAX,Bias
        AND     EAX,$FF00FF00     // EAX  <-  Qa 00 Qg 00
        OR      EAX,EDX           // EAX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,ECX           // EAX  <-  Za Zr Zg Zb
        OR      EAX,$FF000000     // EAX  <-  FF Zr Zg Zb

        OR      EAX,$FF000000
        MOV     [RDI],EAX

        ADD     RDI,4

  // loop end
        DEC     R8D
        JNZ     @1

        POP     RDI

@2:
        RET

@3:
  // just copy source
        MOV     [RDI],ECX
        ADD     RDI,4

        DEC     R8D
        JNZ     @3

        POP     RDI
{$ENDIF}
end;

{$IFDEF TARGET_x86}

function MergeReg_ASM(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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
  }

  // EAX <- F
  // EDX <- B

  // if F.A = 0 then
        TEST    EAX,$FF000000
        JZ      @exit0

  // else if B.A = 255 then
        CMP     EDX,$FF000000
        JNC     @blend

  // else if F.A = 255 then
        CMP     EAX,$FF000000
        JNC     @Exit

  // else if B.A = 0 then
        TEST    EDX,$FF000000
        JZ      @Exit

@4:
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        ADD     ESP,-$0C
        MOV     [ESP+$04],EDX
        MOV     [ESP],EAX

  // AH <- F.A
  // DL, CL <- B.A
        SHR     EAX,16
        AND     EAX,$0000FF00
        SHR     EDX,24
        MOV     CL,DL
        NOP
        NOP
        NOP

  // EDI <- PF
  // EDX <- PB
  // ESI <- PR

  // PF := @DivTable[F.A];
        LEA     EDI,[EAX+DivTable]
  // PB := @DivTable[B.A];
        SHL     EDX,$08
        LEA     EDX,[EDX+DivTable]

  // Result.A := B.A + F.A - PB[F.A];
        SHR     EAX,8
        ADD     ECX,EAX
        SUB     ECX,[EDX+EAX]
        MOV     [ESP+$0B],CL
  // PR := @RcTable[Result.A];
        SHL     ECX,$08
        AND     ECX,$0000FFFF
        LEA     ESI,[ECX+RcTable]

  { Red component }

  // Result.R := PB[B.R];
        XOR     EAX,EAX
        MOV     AL,[ESP+$06]
        MOV     CL,[EDX+EAX]
        MOV     [ESP+$0a],CL
  // X := F.R - Result.R;
        MOV     AL,[ESP+$02]
        XOR     EBX,EBX
        MOV     BL,CL
        SUB     EAX,EBX
  // if X >= 0 then
        JL      @5
  // Result.R := PR[PF[X] + Result.R]
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        AND     ECX,$000000FF
        ADD     EAX,ECX
        MOV     AL,[ESI+EAX]
        MOV     [ESP+$0A],AL
        JMP     @6
@5:
  // Result.R := PR[Result.R - PF[-X]];
        NEG     EAX
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        XOR     ECX,ECX
        MOV     CL,[ESP+$0A]
        SUB     ECX,EAX
        MOV     AL,[ESI+ECX]
        MOV     [ESP+$0A],AL


  { Green component }

@6:
  // Result.G := PB[B.G];
        XOR     EAX,EAX
        MOV     AL,[ESP+$05]
        MOV     CL,[EDX+EAX]
        MOV     [ESP+$09],CL
  // X := F.G - Result.G;
        MOV     AL,[ESP+$01]
        XOR     EBX,EBX
        MOV     BL,CL
        SUB     EAX,EBX
  // if X >= 0 then
        JL      @7
  // Result.G := PR[PF[X] + Result.G]
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        AND     ECX,$000000FF
        ADD     EAX,ECX
        MOV     AL,[ESI+EAX]
        MOV     [ESP+$09],AL
        JMP     @8
@7:
  // Result.G := PR[Result.G - PF[-X]];
        NEG     EAX
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        XOR     ECX,ECX
        MOV     CL,[ESP+$09]
        SUB     ECX,EAX
        MOV     AL,[ESI+ECX]
        MOV     [ESP+$09],AL


  { Blue component }
@8:
  // Result.B := PB[B.B];
        XOR     EAX,EAX
        MOV     AL,[ESP+$04]
        MOV     CL,[EDX+EAX]
        MOV     [ESP+$08],CL
  // X := F.B - Result.B;
        MOV     AL,[ESP]
        XOR     EDX,EDX
        MOV     DL,CL
        SUB     EAX,EDX
  // if X >= 0 then
        JL      @9
  // Result.B := PR[PF[X] + Result.B]
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        XOR     EDX,EDX
        MOV     DL,CL
        ADD     EAX,EDX
        MOV     AL,[ESI+EAX]
        MOV     [ESP+$08],AL
        JMP     @10
@9:
  // Result.B := PR[Result.B - PF[-X]];
        NEG     EAX
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        XOR     EDX,EDX
        MOV     DL,CL
        SUB     EDX,EAX
        MOV     AL,[ESI+EDX]
        MOV     [ESP+$08],AL

@10:
  // EAX <- Result
        MOV     EAX,[ESP+$08]

  // end;
        ADD     ESP,$0C
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@blend:
        CALL    DWORD PTR [BlendReg]
        OR      EAX,$FF000000
        RET
@exit0:
        MOV     EAX,EDX
@Exit:
end;

{$ENDIF}

function CombineReg_ASM(X, Y, W: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
{$IFDEF TARGET_x86}
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
        JCXZ    @1              // CX = 0 ?  => Result := EDX
        CMP     ECX,$FF         // CX = $FF ?  => Result := EDX
        JE      @2

        PUSH    EBX

  // P = W * X
        MOV     EBX,EAX         // EBX  <-  Xa Xr Xg Xb
        AND     EAX,$00FF00FF   // EAX  <-  00 Xr 00 Xb
        AND     EBX,$FF00FF00   // EBX  <-  Xa 00 Xg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Xa 00 Xg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pa 00 Pg 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ya Yr Yg Yb
  // Q = W * Y
        AND     EDX,$00FF00FF   // EDX  <-  00 Yr 00 Yb
        AND     EBX,$FF00FF00   // EBX  <-  Ya 00 Yg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ya 00 Yg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX,EDX
@2:
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- X
  // EDX <- Y
  // R8D <- W

  // W = 0 or $FF?
        TEST    R8D,R8D
        JZ      @1              // W = 0 ?  => Result := EDX
        MOV     EAX,ECX         // EAX  <-  Xa Xr Xg Xb
        CMP     R8B,$FF         // W = $FF ?  => Result := EDX
        JE      @2

  // P = W * X
        AND     EAX,$00FF00FF   // EAX  <-  00 Xr 00 Xb
        AND     ECX,$FF00FF00   // ECX  <-  Xa 00 Xg 00
        IMUL    EAX,R8D         // EAX  <-  Pr ** Pb **
        SHR     ECX,8           // ECX  <-  00 Xa 00 Xg
        IMUL    ECX,R8D         // ECX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pa 00 Pg 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pa 00 Pg 00
        OR      EAX,ECX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
        MOV     ECX,EDX         // ECX  <-  Ya Yr Yg Yb
  // Q = W * Y
        AND     EDX,$00FF00FF   // EDX  <-  00 Yr 00 Yb
        AND     ECX,$FF00FF00   // ECX  <-  Ya 00 Yg 00
        IMUL    EDX,R8D         // EDX  <-  Qr ** Qb **
        SHR     ECX,8           // ECX  <-  00 Ya 00 Yg
        IMUL    ECX,R8D         // ECX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Qa 00 Qg 00
        OR      ECX,EDX         // ECX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,ECX         // EAX  <-  Za Zr Zg Zb

        RET

@1:     MOV     EAX,EDX
@2:
{$ENDIF}
end;

procedure CombineMem_ASM(X: TColor32; var Y: TColor32; W: TColor32); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
        JCXZ    @1              // W = 0 ?  => write nothing
        CMP     ECX,$FF         // W = 255? => write F
{$IFDEF FPC}
        DB      $74,$76         // Prob with FPC 2.2.2 and below
{$ELSE}
        JZ      @2
{$ENDIF}


        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,ESI         // EBX  <-  Ba Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb

        MOV     [EDX],EAX

        POP     ESI
        POP     EBX
@1:     RET

@2:     MOV     [EDX],EAX
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- F
  // [RDX] <- B
  // R8 <- W

  // Check W
        TEST    R8D,R8D         // Set flags for R8
        JZ      @2              // W = 0 ?  => Result := EDX
        MOV     EAX,ECX         // EAX  <-  ** Fr Fg Fb
        CMP     R8B,$FF         // W = 255? => write F
        JZ      @1

  // P = W * F
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     ECX,$FF00FF00   // ECX  <-  Fa 00 Fg 00
        IMUL    EAX,R8D         // EAX  <-  Pr ** Pb **
        SHR     ECX,8           // ECX  <-  00 Fa 00 Fg
        IMUL    ECX,R8D         // ECX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pa 00 Pg 00
        OR      EAX,ECX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
  // Q = W * B
        MOV     ECX,R9D         // ECX  <-  Ba Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     ECX,$FF00FF00   // ECX  <-  Ba 00 Bg 00
        IMUL    R9D,R8D         // R9D  <-  Qr ** Qb **
        SHR     ECX,8           // ECX  <-  00 Ba 00 Bg
        IMUL    ECX,R8D         // ECX  <-  Qa ** Qg **
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qr 00 Qb 00
        SHR     R9D,8           // R9D  <-  00 Qr ** Qb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Qa 00 Qg 00
        OR      ECX,R9D         // ECX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,ECX         // EAX  <-  00 Zr Zg Zb

@1:     MOV     [RDX],EAX
@2:

{$ENDIF}
end;

procedure EMMS_ASM; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
end;

end.
