unit GR32_Blend;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Mattias Andersson
 *      - 2004/07/07 - MMX Blendmodes
 *
 *  Michael Hansen <dyster_tid@hotmail.com>
 *      - 2004/07/07 - Pascal Blendmodes, function setup
 *
 * ***** END LICENSE BLOCK ***** *)
// $Id: GR32_Blend.pas,v 1.2 2004/07/07 11:39:58 abeckedorf Exp $

interface

{$I GR32.inc}

uses
  GR32;

var
  MMX_ACTIVE: Boolean;

type
{ Function Prototypes }
  TCombineReg  = function(X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure(F: TColor32; var B: TColor32; W: TColor32);
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);

procedure EMMS;

var
{ Function Variables }
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;

  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

{ Color algebra functions }
  ColorAdd: TBlendReg;
  ColorSub: TBlendReg;
  ColorModulate: TBlendReg;
  ColorMax: TBlendReg;
  ColorMin: TBlendReg;
  ColorDifference: TBlendReg;
  ColorExclusion: TBlendReg;

{ Special LUT pointers }
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;

{ Misc stuff }
function Lighten(C: TColor32; Amount: Integer): TColor32;




implementation

uses Math, GR32_System;

{ Non-MMX versions }

const bias = $00800080;

function _CombineReg(X, Y, W: TColor32): TColor32;
asm
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
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

  // W = 1 - W; Q = W * Y
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ya Yr Yg Yb
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
@2:     RET
end;

procedure _CombineMem(F: TColor32; var B: TColor32; W: TColor32);
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
        JCXZ    @1              // W = 0 ?  => write nothing
        CMP     ECX,$FF         // W = 255? => write F
        JZ      @2

        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  00 Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX],EAX

        POP     ESI
        POP     EBX
@1:     RET

@2:     MOV     [EDX],EAX
        RET
end;

function _BlendReg(F, B: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Frgb + (1 - Fa) * Brgb
  // EAX <- F
  // EDX <- B

  // Test Fa = 255 ?
        CMP     EAX,$FF000000   // Fa = 255 ? => Result = EAX
        JNC     @2

  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => Result = EDX
        JZ      @1

  // Get weight W = Fa * M
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
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
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

        POP     EBX
        RET

@1:     MOV     EAX,EDX
@2:     RET
end;

procedure _BlendMem(F: TColor32; var B: TColor32);
asm
  // EAX <- F
  // [EDX] <- B


  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => do not write
        JZ      @2

  // Get weight W = Fa * M
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @1

        PUSH EBX
        PUSH ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
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
        RET

@1:     MOV     [EDX],EAX
@2:     RET
end;

function _BlendRegEx(F, B, M: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, if this is the case when Graphics32 uses BlendReg
  // Result Z = Fa * M * Frgb + (1 - Fa * M) * Brgb
  // EAX <- F
  // EDX <- B
  // ECX <- M

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
        AND     EBX,$0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,EDX         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        POP     EBX
        RET
        
@1:     POP     EBX
@2:     MOV     EAX,EDX
        RET
end;

procedure _BlendMemEx(F: TColor32; var B: TColor32; M: TColor32);
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX,$FF000000   // Fa = 0? => write nothing
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        INC     ECX             // 255:256 range bias
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        SHR     ECX,8           // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  00 Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX],EAX
        POP     ESI

@1:     POP     EBX
@2:     RET
end;

procedure _BlendLine(Src, Dst: PColor32; Count: Integer);
asm
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

  // Get weight W = Fa * M
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
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     EDX,[EDI]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
@2:     MOV     [EDI],EAX

        POP     ECX             // restore counter

@3:     ADD     ESI,4
        ADD     EDI,4

  // loop end
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:     RET
end;

procedure _BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
begin
  while Count > 0 do
  begin
    _BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;


{ MMX versions }


procedure GenAlphaTable;
var
  I: Integer;
  L: Longword;
  P: ^Longword;
begin
  GetMem(AlphaTable, 257 * 8);
  alpha_ptr := Pointer(Integer(AlphaTable) and $FFFFFFF8);
  if Integer(alpha_ptr) < Integer(AlphaTable) then
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := Pointer(Integer(alpha_ptr) + $80 * 8);
end;

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
end;

procedure EMMS;
begin
  if MMX_ACTIVE then
  asm
    db $0F,$77               /// EMMS
  end;
end;

function M_CombineReg(X, Y, W: TColor32): TColor32;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        SHL       ECX,3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1
end;

procedure M_CombineMem(F: TColor32; var B: TColor32; W: TColor32);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ      @1
        CMP       ECX,$FF
        JZ        @2

        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        SHL       ECX,3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     RET

@2:     MOV       [EDX],EAX
end;

function M_BlendReg(F, B: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV     ECX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2
end;

procedure M_BlendMem(F: TColor32; var B: TColor32);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       ECX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$12           /// MOVD      [EDX],MM2
@1:     RET

@2:     MOV       [EDX],EAX
end;

function M_BlendRegEx(F, B, M: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX,3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

        POP       EBX
        RET

@1:     MOV       EAX,EDX
        POP       EBX
end;

procedure M_BlendMemEx(F: TColor32; var B:TColor32; M: TColor32);
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        TEST      EAX,$FF000000
        JZ        @2

        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX,3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     POP       EBX
@2:
end;

procedure M_BlendLine(Src, Dst: PColor32; Count: Integer);
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
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       EAX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$10           /// PADDW     MM2,[EAX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:     RET
end;

procedure M_BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
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
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       EBX,3
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       EBX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$0B           /// PMULLW    MM1,[EBX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       EBX,bias_ptr
        db $0F,$FD,$13           /// PADDW     MM2,[EBX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

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

{ Non-MMX Color algebra versions }

function _ColorAdd(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  a1 := a1 + a2;
  r1 := r1 + r2;
  g1 := g1 + g2;
  b1 := b1 + b2;

  if a1 > $FF then a1 := $FF;
  if r1 > $FF0000 then r1 := $FF0000;
  if g1 > $FF00 then g1 := $FF00;
  if b1 > $FF then b1 := $FF;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorSub(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 - a2;
  r1 := r1 - r2;
  g1 := g1 - g2;
  b1 := b1 - b2;

  if a1 < 0 then a1 := 0;
  if r1 < 0 then r1 := 0;
  if g1 < 0 then g1 := 0;
  if b1 < 0 then b1 := 0;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorModulate(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 * a2 shr 8;
  r1 := r1 * r2 shr 8;
  g1 := g1 * g2 shr 8;
  b1 := b1 * b2 shr 8;

  if a1 > 255 then a1 := 255;
  if r1 > 255 then r1 := 255;
  if g1 > 255 then g1 := 255;
  if b1 > 255 then b1 := 255;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorMax(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  if a2 > a1 then a1 := a2;
  if r2 > r1 then r1 := r2;
  if g2 > g1 then g1 := g2;
  if b2 > b1 then b1 := b2;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorMin(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  if a2 < a1 then a1 := a2;
  if r2 < r1 then r1 := r2;
  if g2 < g1 then g1 := g2;
  if b2 < b1 then b1 := b2;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorDifference(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := abs(a2 - a1);
  r1 := abs(r2 - r1);
  g1 := abs(g2 - g1);
  b1 := abs(b2 - b1);

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorExclusion(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 + a2 - (a1 * a2 shr 7);
  r1 := r1 + r2 - (r1 * r2 shr 7);
  g1 := g1 + g2 - (g1 * g2 shr 7);
  b1 := b1 + b2 - (b1 * b2 shr 7);

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

{ MMX Color algebra versions }

function M_ColorAdd(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DC,$C1           /// PADDUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorSub(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorModulate(C1, C2: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$60,$CA           /// PUNPCKLBW MM1,MM2
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$71,$D0,$08       /// PSRLW     MM0,8
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorMax(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DE,$C1           /// PMAXUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorMin(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DA,$C1           /// PMINUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;


function M_ColorDifference(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$6F,$D0           /// MOVQ      MM2,MM0
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$D8,$CA           /// PSUBUSB   MM1,MM2
        db $0F,$EB,$C1           /// POR       MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorExclusion(C1, C2: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$60,$CA           /// PUNPCKLBW MM1,MM2
        db $0F,$6F,$D8           /// MOVQ      MM3,MM0
        db $0F,$FD,$C1           /// PADDW     MM0,MM1
        db $0F,$D5,$CB           /// PMULLW    MM1,MM3
        db $0F,$71,$D1,$07       /// PSRLW     MM1,7
        db $0F,$D9,$C1           /// PSUBUSW   MM0,MM1
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

{ Misc stuff }

function Lighten(C: TColor32; Amount: Integer): TColor32;
var
  r, g, b, a: Integer;
begin
  a := C shr 24;
  r := C and $00FF0000;
  g := C and $0000FF00;
  b := C and $000000FF;

  r := r shr 16;
  g := g shr 8;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  Result := a shl 24 + r shl 16 + g shl 8 + b;
end;

{ MMX Detection and linking }

procedure SetupFunctions;
begin
  MMX_ACTIVE := HasMMX;
  if MMX_ACTIVE then
  begin
    // link MMX functions
    CombineReg := M_CombineReg;
    CombineMem := M_CombineMem;
    BlendReg := M_BlendReg;
    BlendMem := M_BlendMem;
    BlendRegEx := M_BlendRegEx;
    BlendMemEx := M_BlendMemEx;
    BlendLine := M_BlendLine;
    BlendLineEx := M_BlendLineEx;

    ColorAdd := M_ColorAdd;
    ColorSub := M_ColorSub;
    ColorModulate:= M_ColorModulate;
    ColorMax:= M_ColorMax;
    ColorMin:= M_ColorMin;
    ColorDifference:= M_ColorDifference;
    ColorExclusion:= M_ColorExclusion;
  end
  else
  begin
    // link non-MMX functions
    CombineReg := _CombineReg;
    CombineMem := _CombineMem;
    BlendReg := _BlendReg;
    BlendMem := _BlendMem;
    BlendRegEx := _BlendRegEx;
    BlendMemEx := _BlendMemEx;
    BlendLine := _BlendLine;
    BlendLineEx := _BlendLineEx;

    ColorAdd := _ColorAdd;
    ColorSub := _ColorSub;
    ColorModulate:= _ColorModulate;
    ColorMax:= _ColorMax;
    ColorMin:= _ColorMin;
    ColorDifference:= _ColorDifference;
    ColorExclusion:= _ColorExclusion;
  end;
end;

initialization
  SetupFunctions;
  if MMX_ACTIVE then GenAlphaTable;

finalization
  if MMX_ACTIVE then FreeAlphaTable;

end.



