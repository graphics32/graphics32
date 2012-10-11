unit GR32_Blend;

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
 *  Mattias Andersson
 *      - 2004/07/07 - MMX Blendmodes
 *      - 2004/12/10 - _MergeReg, M_MergeReg
 *
 *  Michael Hansen <dyster_tid@hotmail.com>
 *      - 2004/07/07 - Pascal Blendmodes, function setup
 *      - 2005/08/19 - New merge table concept and reference implementations
 *
 *  Bob Voigt
 *      - 2004/08/25 - ColorDiv
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  GR32, GR32_System, GR32_Bindings, SysUtils;

var
  MMX_ACTIVE: Boolean;

type
{ Function Prototypes }
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);
  TCombineReg  = function(X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure(X: TColor32; var Y: TColor32; W: TColor32);
  TCombineLine = procedure(Src, Dst: PColor32; Count: Integer; W: TColor32);
  TLightenReg  = function(C: TColor32; Amount: Integer): TColor32;

var
{$IFNDEF OMIT_MMX}
  EMMS: procedure;
{$ENDIF}

{ Function Variables }
  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

  CombineReg: TCombineReg;
  CombineMem: TCombineMem;
  CombineLine: TCombineLine;

  MergeReg: TBlendReg;
  MergeMem: TBlendMem;

  MergeRegEx: TBlendRegEx;
  MergeMemEx: TBlendMemEx;

  MergeLine: TBlendLine;
  MergeLineEx: TBlendLineEx;

{ Color algebra functions }
  ColorAdd: TBlendReg;
  ColorSub: TBlendReg;
  ColorDiv: TBlendReg;
  ColorModulate: TBlendReg;
  ColorMax: TBlendReg;
  ColorMin: TBlendReg;
  ColorDifference: TBlendReg;
  ColorAverage: TBlendReg;
  ColorExclusion: TBlendReg;
  ColorScale: TBlendReg;

{ Special LUT pointers }
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;


{ Misc stuff }
  LightenReg: TLightenReg;

function Lighten(C: TColor32; Amount: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Access to alpha composite functions corresponding to a combine mode }

const
  BLEND_REG: array[TCombineMode] of ^TBlendReg = ((@@BlendReg),(@@MergeReg));
  BLEND_MEM: array[TCombineMode] of ^TBlendMem = ((@@BlendMem),(@@MergeMem));
  BLEND_REG_EX: array[TCombineMode] of ^TBlendRegEx = ((@@BlendRegEx),(@@MergeRegEx));
  BLEND_MEM_EX: array[TCombineMode] of ^TBlendMemEx = ((@@BlendMemEx),(@@MergeMemEx));
  BLEND_LINE: array[TCombineMode] of ^TBlendLine = ((@@BlendLine),(@@MergeLine));
  BLEND_LINE_EX: array[TCombineMode] of ^TBlendLineEx = ((@@BlendLineEx),(@@MergeLineEx));

var
  BlendRegistry: TFunctionRegistry;

{$IFDEF OMIT_MMX}
procedure EMMS; {$IFDEF USEINLINING} inline; {$ENDIF}
{$ENDIF}

implementation

{$IFDEF TARGET_x86}
uses GR32_LowLevel;
{$ENDIF}

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;

{$IFDEF OMIT_MMX}
procedure EMMS;
begin
end;
{$ENDIF}

{ Pure Pascal }

function BlendReg_Pas(F, B: TColor32): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
  FA : Byte;
begin
  FA := FX.A;

  if FA = 0 then
  begin
    Result := B;
    Exit;
  end;

  if FA = $FF then
  begin
    Result := F;
    Exit;
  end;

  with BX do
  begin
    Af := @DivTable[FA];
    Ab := @DivTable[not FA];
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
  end;
  Result := B;
end;

procedure BlendMem_Pas(F: TColor32; var B: TColor32);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
  FA : Byte;
begin
  FA := FX.A;

  if FA = 0 then Exit;

  if FA = $FF then
  begin
    B := F;
    Exit;
  end;

  with BX do
  begin
    Af := @DivTable[FA];
    Ab := @DivTable[not FA];
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
  end;
end;

function BlendRegEx_Pas(F, B, M: TColor32): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];

  M := Af[FX.A];

  if M = 0 then
  begin
    Result := B;
    Exit;
  end;

  if M = $FF then
  begin
    Result := F;
    Exit;
  end;

  with BX do
  begin
    Af := @DivTable[M];
    Ab := @DivTable[255 - M];
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
  end;
  Result := B;
end;

procedure BlendMemEx_Pas(F: TColor32; var B: TColor32; M: TColor32);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];

  M := Af[FX.A];

  if M = 0 then
  begin
    Exit;
  end;

  if M = $FF then
  begin
    B := F;
    Exit;
  end;

  with BX do
  begin
    Af := @DivTable[M];
    Ab := @DivTable[255 - M];
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
  end;
end;

procedure BlendLine_Pas(Src, Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem(Src^, Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

procedure BlendLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: TColor32);
begin
  while Count > 0 do
  begin
    BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

function CombineReg_Pas(X, Y, W: TColor32): TColor32;
var
  Xe: TColor32Entry absolute X;
  Ye: TColor32Entry absolute Y;
  Af, Ab: PByteArray;
begin
  if W = 0 then
  begin
    Result := Y;
    Exit;
  end;

  if W >= $FF then
  begin
    Result := X;
    Exit;
  end;

  with Xe do
  begin
    Af := @DivTable[W];
    Ab := @DivTable[255 - W];
    R := Ab[Ye.R] + Af[R];
    G := Ab[Ye.G] + Af[G];
    B := Ab[Ye.B] + Af[B];
  end;
  Result := X;
end;

procedure CombineMem_Pas(X: TColor32; var Y: TColor32; W: TColor32);
var
  Xe: TColor32Entry absolute X;
  Ye: TColor32Entry absolute Y;
  Af, Ab: PByteArray;
begin
  if W = 0 then
  begin
    Exit;
  end;

  if W >= $FF then
  begin
    Y := X;
    Exit;
  end;

  with Xe do
  begin
    Af := @DivTable[W];
    Ab := @DivTable[255 - W];
    R := Ab[Ye.R] + Af[R];
    G := Ab[Ye.G] + Af[G];
    B := Ab[Ye.B] + Af[B];
  end;
  Y := X;
end;

procedure CombineLine_Pas(Src, Dst: PColor32; Count: Integer; W: TColor32);
begin
  while Count > 0 do
  begin
    CombineMem(Src^, Dst^, W);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

function MergeReg_Pas(F, B: TColor32): TColor32;
var
 Fa, Ba, Wa: TColor32;
 Fw, Bw: PByteArray;
 Fx: TColor32Entry absolute F;
 Bx: TColor32Entry absolute B;
 Rx: TColor32Entry absolute Result;
begin
 Fa := F shr 24;
 Ba := B shr 24;
 if Fa = $FF then
   Result := F
 else if Fa = $0 then
   Result := B
 else if Ba = $0 then
   Result := F
 else
 begin
   Rx.A := DivTable[Fa xor 255, Ba xor 255] xor 255;
   Wa := RcTable[Rx.A, Fa];
   Fw := @DivTable[Wa];
   Bw := @DivTable[Wa xor $ff];
   Rx.R := Fw[Fx.R] + Bw[Bx.R];
   Rx.G := Fw[Fx.G] + Bw[Bx.G];
   Rx.B := Fw[Fx.B] + Bw[Bx.B];
 end;
end;

function MergeRegEx_Pas(F, B, M: TColor32): TColor32;
begin
  Result := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;

procedure MergeMem_Pas(F: TColor32; var B: TColor32);
begin
  B := MergeReg(F, B);
end;

procedure MergeMemEx_Pas(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;

procedure MergeLine_Pas(Src, Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    Dst^ := MergeReg(Src^, Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

procedure MergeLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: TColor32);
var
  PM: PByteArray absolute M;
begin
  PM := @DivTable[M];
  while Count > 0 do
  begin
    Dst^ := MergeReg((PM[Src^ shr 24] shl 24) or (Src^ and $00FFFFFF), Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

procedure EMMS_Pas;
begin
//Dummy
end;

function LightenReg_Pas(C: TColor32; Amount: Integer): TColor32;
var
  r, g, b, a: Integer;
  CX: TColor32Entry absolute C;
  RX: TColor32Entry absolute Result;
begin
  a := CX.A;
  r := CX.R;
  g := CX.G;
  b := CX.B;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  RX.A := a;
  RX.R := r;
  RX.G := g;
  RX.B := b;
end;

{ Color algebra }

function ColorAdd_Pas(C1, C2: TColor32): TColor32;
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

function ColorSub_Pas(C1, C2: TColor32): TColor32;
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

function ColorDiv_Pas(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := (C1 and $00FF0000) shr 16;
  g1 := (C1 and $0000FF00) shr 8;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := (C2 and $00FF0000) shr 16;
  g2 := (C2 and $0000FF00) shr 8;
  b2 := C2 and $000000FF;

  if a1 = 0 then a1:=$FF
  else a1 := (a2 shl 8) div a1;
  if r1 = 0 then r1:=$FF
  else r1 := (r2 shl 8) div r1;
  if g1 = 0 then g1:=$FF
  else g1 := (g2 shl 8) div g1;
  if b1 = 0 then b1:=$FF
  else b1 := (b2 shl 8) div b1;

  if a1 > $FF then a1 := $FF;
  if r1 > $FF then r1 := $FF;
  if g1 > $FF then g1 := $FF;
  if b1 > $FF then b1 := $FF;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function ColorModulate_Pas(C1, C2: TColor32): TColor32;
var
  REnt: TColor32Entry absolute Result;
  C2Ent: TColor32Entry absolute C2;
begin
  Result := C1;
  REnt.A := (C2Ent.A * REnt.A) shr 8;
  REnt.R := (C2Ent.R * REnt.R) shr 8;
  REnt.G := (C2Ent.G * REnt.G) shr 8;
  REnt.B := (C2Ent.B * REnt.B) shr 8;
end;

function ColorMax_Pas(C1, C2: TColor32): TColor32;
var
  REnt: TColor32Entry absolute Result;
  C2Ent: TColor32Entry absolute C2;
begin
  Result := C1;
  with C2Ent do
  begin
    if A > REnt.A then REnt.A := A;
    if R > REnt.R then REnt.R := R;
    if G > REnt.G then REnt.G := G;
    if B > REnt.B then REnt.B := B;
  end;
end;

function ColorMin_Pas(C1, C2: TColor32): TColor32;
var
  REnt: TColor32Entry absolute Result;
  C2Ent: TColor32Entry absolute C2;
begin
  Result := C1;
  with C2Ent do
  begin
    if A < REnt.A then REnt.A := A;
    if R < REnt.R then REnt.R := R;
    if G < REnt.G then REnt.G := G;
    if B < REnt.B then REnt.B := B;
  end;
end;

function ColorDifference_Pas(C1, C2: TColor32): TColor32;
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

function ColorExclusion_Pas(C1, C2: TColor32): TColor32;
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

function ColorAverage_Pas(C1, C2: TColor32): TColor32;
//(A + B)/2 = (A and B) + (A xor B)/2
var
  C3 : TColor32;
begin
  C3 := C1;
  C1 := C1 xor C2;
  C1 := C1 shr 1;
  C1 := C1 and $7F7F7F7F;
  C3 := C3 and C2;
  Result := C3 + C1;
end;

function ColorScale_Pas(C, W: TColor32): TColor32;
var
  r1, g1, b1, a1: Cardinal;
begin
  a1 := C shr 24;
  r1 := C and $00FF0000;
  g1 := C and $0000FF00;
  b1 := C and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a1 := a1 * W shr 8;
  r1 := r1 * W shr 8;
  g1 := g1 * W shr 8;
  b1 := b1 * W shr 8;

  if a1 > 255 then a1 := 255;
  if r1 > 255 then r1 := 255;
  if g1 > 255 then g1 := 255;
  if b1 > 255 then b1 := 255;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

{$IFNDEF PUREPASCAL}

{ Assembler versions }

const
  bias = $00800080;


function BlendReg_ASM(F, B: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Frgb + (1 - Fa) * Brgb

{$IFDEF TARGET_x86}
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
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

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

  // Get weight W = Fa * M
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
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Pa 00 Pg 00
        OR      EAX,R9D         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     R9D,EDX         // R9D  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     R9D,$FF00FF00   // R9D  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     R9D,8           // R9D  <-  00 Ba 00 Bg
        IMUL    R9D,ECX         // R9D  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qa 00 Qg 00
        OR      R9D,EDX         // R9D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R9D         // EAX  <-  Za Zr Zg Zb
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV     EAX,EDX
@2:
{$ENDIF}
end;

procedure BlendMem_ASM(F: TColor32; var B: TColor32);
asm
{$IFDEF TARGET_x86}
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

        PUSH    EBX
        PUSH    ESI

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

        MOV     ESI,[EDX]

// W = 1 - W; Q = W * B
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
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

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

        // Get weight W = Fa * M
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
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Pa 00 Pg 00
        OR      EAX,R8D         // EAX  <-  Pa Pr Pg Pb

        MOV     R9D,[RDX]

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     R8D,R9D         // R8D  <-  Ba Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     R8D,$FF00FF00   // R8D  <-  Ba 00 Bg 00
        IMUL    R9D,ECX         // R9D  <-  Qr ** Qb **
        SHR     R8D,8           // R8D  <-  00 Ba 00 Bg
        IMUL    R8D,ECX         // R8D  <-  Qa ** Qg **
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qr 00 Qb 00
        SHR     R9D,8           // R9D  <-  00 Qr ** Qb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Qa 00 Qg 00
        OR      R8D,R9D         // R8D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R8D         // EAX  <-  Za Zr Zg Zb

        MOV     [RDX],EAX
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV     [RDX],EAX
@2:
{$ENDIF}
end;

function BlendRegEx_ASM(F, B, M: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, in this case Graphics32 uses BlendReg
  // Result Z = Fa * M * Frgb + (1 - Fa * M) * Brgb
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
{$IFDEF FPC}
        JMP @3
{$ELSE}
        RET
{$ENDIF}

@1:
        POP     EBX

@2:     MOV     EAX,EDX
@3:
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
        AND     ECX,$0000FF00   // ECX  <-  00 00 Fg 00
        IMUL    EAX,R8D         // EAX  <-  Pr ** Pb **
        SHR     ECX,8           // ECX  <-  00 00 00 Fg
        IMUL    ECX,R8D         // ECX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     ECX,bias
        AND     ECX,$0000FF00   // ECX  <-  00 00 Pg 00
        OR      EAX,ECX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
        MOV     ECX,EDX         // ECX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     ECX,$0000FF00   // ECX  <-  00 00 Bg 00
        IMUL    EDX,R8D         // EDX  <-  Qr ** Qb **
        SHR     ECX,8           // ECX  <-  00 00 00 Bg
        IMUL    ECX,R8D         // ECX  <-  00 00 Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     ECX,bias
        AND     ECX,$0000FF00   // ECX  <-  00 00 Qg 00
        OR      ECX,EDX         // ECX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,ECX         // EAX  <-  00 Zr Zg Zb

{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV     EAX,EDX
@2:
{$ENDIF}
end;

procedure BlendMemEx_ASM(F: TColor32; var B: TColor32; M: TColor32);
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
        SHR     R8D,8           // R8D <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

  // P = W * F
        MOV     EAX,ECX         // EAX  <-  ** Fr Fg Fb
        AND     ECX,$00FF00FF   // ECX  <-  00 Fr 00 Fb
        AND     EAX,$0000FF00   // EAX  <-  00 00 Fg 00
        IMUL    ECX,R8D         // ECX  <-  Pr ** Pb **
        SHR     EAX,8           // EAX  <-  00 00 00 Fg
        IMUL    EAX,R8D         // EAX  <-  00 00 Pg **
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pr 00 Pb 00
        SHR     ECX,8           // ECX  <-  00 Pr ** Pb
        ADD     EAX,bias
        AND     EAX,$0000FF00   // EAX  <-  00 00 Pg 00
        OR      ECX,EAX         // ECX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8
        MOV     EAX,R9D         // EAX  <-  00 Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     EAX,$0000FF00   // EAX  <-  00 00 Bg 00
        IMUL    R9D,R8D         // R9D  <-  Qr ** Qb **
        SHR     EAX,8           // EAX  <-  00 00 00 Bg
        IMUL    EAX,R8D         // EAX  <-  00 00 Qg **
        ADD     R9D,bias
        AND     R9D,$FF00FF00   // R9D  <-  Qr 00 Qb 00
        SHR     R9D,8           // R9D  <-  00 Qr ** Qb
        ADD     EAX,bias
        AND     EAX,$0000FF00   // EAX  <-  00 00 Qg 00
        OR      EAX,R9D         // EAX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     ECX,EAX         // ECX  <-  00 Zr Zg Zb

        MOV     [RDX],ECX

@1:
{$ENDIF}
end;

procedure BlendLine_ASM(Src, Dst: PColor32; Count: Integer);
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

  // Get weight W = Fa * M
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
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Pa 00 Pg 00
        OR      EAX,R8D         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     EDX,[R11]
        XOR     R9D,$000000FF   // R9D  <-  1 - R9D
        MOV     R8D,EDX         // R8D  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     R8D,$FF00FF00   // R8D  <-  Ba 00 Bg 00
        IMUL    EDX,R9D         // ESI  <-  Qr ** Qb **
        SHR     R8D,8           // R8D  <-  00 Ba 00 Bg
        IMUL    R8D,R9D         // R8D  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX,8           // ESI  <-  00 Qr ** Qb
        ADD     R8D,bias
        AND     R8D,$FF00FF00   // R8D  <-  Qa 00 Qg 00
        OR      R8D,EDX         // R8D  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,R8D         // EAX  <-  Za Zr Zg Zb
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

{$IFDEF TARGET_x86}

function MergeReg_ASM(F, B: TColor32): TColor32;
asm
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
        //ADD CL,al
        ADD     ECX,EAX
        //SUB CL,[EDX+EAX]
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
        MOV     [ESP+$0a],al
        JMP     @6
@5:
        // Result.R := PR[Result.R - PF[-X]];
        NEG     EAX
        MOVZX   EAX,BYTE PTR[EDI+EAX]
        XOR     ECX,ECX
        MOV     CL,[ESP+$0A]
        SUB     ECX,EAX
        MOV     AL,[ESI+ECX]
        MOV     [ESP+$0A],al


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
        MOV     [ESP+$08],al
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
{$IFDEF FPC}
        JMP @Exit
{$ELSE}
        RET
{$ENDIF}
@blend:
        CALL    DWORD PTR [BlendReg]
        OR      EAX,$FF000000
{$IFDEF FPC}
        JMP @Exit
{$ELSE}
        RET
{$ENDIF}
@exit0:
        MOV     EAX,EDX
@Exit:
end;

{$ENDIF}

function CombineReg_ASM(X, Y, W: TColor32): TColor32;
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
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

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

  // W = 1 - W; Q = W * Y
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
        MOV     ECX,EDX         // ECX  <-  Ya Yr Yg Yb
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

{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV     EAX,EDX
@2:
{$ENDIF}
end;

procedure CombineMem_ASM(X: TColor32; var Y: TColor32; W: TColor32);
asm
{$IFDEF TARGET_x86}
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
        JCXZ    @1              // W = 0 ?  => write nothing
        CMP     ECX,$FF         // W = 255? => write F
{$IFDEF FPC}
        DB      $74,$76         //Prob with FPC 2.2.2 and below
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
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  Ba Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa 00 Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX],EAX

        POP     ESI
        POP     EBX
{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV     [EDX],EAX
@3:
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
        IMUL    ECX,R8D         // ECX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     ECX,bias
        AND     ECX,$FF00FF00   // ECX  <-  Pa 00 Pg 00
        OR      EAX,ECX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
        MOV     ECX,R9D         // ECX  <-  Ba Br Bg Bb
        AND     R9D,$00FF00FF   // R9D  <-  00 Br 00 Bb
        AND     ECX,$FF00FF00   // ECX  <-  Ba 00 Bg 00
        IMUL    R9D,R8D         // R9D  <-  Qr ** Qb **
        SHR     ECX,8           // ECX  <-  00 Ba 00 Bg
        IMUL    ECX,R8D         // ECX  <-  Qa 00 Qg **
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

procedure EMMS_ASM;
asm
end;

procedure GenAlphaTable;
var
  I: Integer;
  L: LongWord;
  P: PLongWord;
begin
  GetMem(AlphaTable, 257 * 8 * SizeOf(Cardinal));
  {$IFDEF HAS_NATIVEINT}
  alpha_ptr := Pointer(NativeUInt(AlphaTable) and (not $F));
  if NativeUInt(alpha_ptr) < NativeUInt(AlphaTable) then
    alpha_ptr := Pointer(NativeUInt(alpha_ptr) + 16);
  {$ELSE}
  alpha_ptr := Pointer(Cardinal(AlphaTable) and (not $F));
  if Cardinal(alpha_ptr) < Cardinal(AlphaTable) then
    Inc(Cardinal(alpha_ptr), 16);
  {$ENDIF}
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := alpha_ptr;
  Inc(PLongWord(bias_ptr), 4 * $80);
end;

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
end;

{$IFNDEF OMIT_MMX}

{ MMX versions }

function BlendReg_MMX(F, B: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
{$IFDEF TARGET_x86}
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
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
{$ENDIF}

{$IFDEF TARGET_x64}
  // ECX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
        MOVD      MM0,ECX
        PXOR      MM3,MM3
        MOVD      MM2,EDX
        PUNPCKLBW MM0,MM3
        MOV       RAX,bias_ptr
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
{$ENDIF}
end;

{$IFDEF TARGET_x86}

procedure BlendMem_MMX(F: TColor32; var B: TColor32);
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

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [EDX],EAX
@3:
end;

function BlendRegEx_MMX(F, B, M: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
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
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV       EAX,EDX
        POP       EBX
@2:
end;

{$ENDIF}

procedure BlendMemEx_MMX(F: TColor32; var B:TColor32; M: TColor32);
asm
{$IFDEF TARGET_x86}
  // blend foreground color (F) to a background color (B),
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
  // Result := M * Fa * (Frgb - Brgb) + Brgb
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
        ADD       R8,alpha_ptr
        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8
        MOV       RAX,bias_ptr
        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [RDX],MM1

@1:
{$ENDIF}
end;

{$IFDEF TARGET_x86}
procedure BlendLine_MMX(Src, Dst: PColor32; Count: Integer);
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
        PMULLW    MM0,MM1         // MM2  <-  Pa ** Pr ** Pg ** Pb **
        PADDW     MM2,[EAX]       // add bias
        PADDW     MM2,MM0         // MM2  <-  Qa ** Qr ** Qg ** Qb **
        PSRLW     MM2,8           // MM2  <-  00 Qa 00 Qr 00 Qg 00 Qb
        PACKUSWB  MM2,MM3         // MM2  <-  00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,MM2

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

procedure BlendLineEx_MMX(Src, Dst: PColor32; Count: Integer; M: TColor32);
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

function CombineReg_MMX(X, Y, W: TColor32): TColor32;
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

        ADD       R8,alpha_ptr

        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8

        MOV       RAX,bias_ptr

        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      EAX,MM1
{$ENDIF}
end;

procedure CombineMem_MMX(F: TColor32; var B: TColor32; W: TColor32);
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

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [EDX],EAX
@3:
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

        ADD       R8,alpha_ptr

        PSUBW     MM1,MM2
        PMULLW    MM1,[R8]
        PSLLW     MM2,8

        MOV       RAX,bias_ptr

        PADDW     MM2,[RAX]
        PADDW     MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM0
        MOVD      [RDX],MM1

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [RDX],RCX
@3:
{$ENDIF}
end;

{$IFDEF TARGET_x86}

procedure CombineLine_MMX(Src, Dst: PColor32; Count: Integer; W: TColor32);
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

procedure EMMS_MMX;
asm
  EMMS
end;

function LightenReg_MMX(C: TColor32; Amount: Integer): TColor32;
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

function ColorAdd_MMX(C1, C2: TColor32): TColor32;
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

function ColorSub_MMX(C1, C2: TColor32): TColor32;
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

function ColorModulate_MMX(C1, C2: TColor32): TColor32;
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

function ColorMax_EMMX(C1, C2: TColor32): TColor32;
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

function ColorMin_EMMX(C1, C2: TColor32): TColor32;
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

function ColorDifference_MMX(C1, C2: TColor32): TColor32;
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

function ColorExclusion_MMX(C1, C2: TColor32): TColor32;
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

function ColorScale_MMX(C, W: TColor32): TColor32;
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
        ADD       RDX,alpha_ptr
        PMULLW    MM0,[RDX]
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
{$ENDIF}
end;
{$ENDIF}


{ SSE2 versions }

{$IFNDEF OMIT_SSE2}

function BlendReg_SSE2(F, B: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb

{$IFDEF TARGET_x86}
        MOVD      XMM0,EAX
        PXOR      XMM3,XMM3
        MOVD      XMM2,EDX
        PUNPCKLBW XMM0,XMM3
        MOV       ECX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PSHUFLW   XMM1,XMM1, $FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[ECX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      EAX,XMM2
{$ENDIF}

{$IFDEF TARGET_x64}
        MOVD      XMM0,ECX
        PXOR      XMM3,XMM3
        MOVD      XMM2,EDX
        PUNPCKLBW XMM0,XMM3
        MOV       RAX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PSHUFLW   XMM1,XMM1, $FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[RAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      EAX,XMM2
{$ENDIF}
end;

procedure BlendMem_SSE2(F: TColor32; var B: TColor32);
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
        PSHUFLW   XMM1,XMM1, $FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[ECX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [EDX],XMM2

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [EDX], EAX
@3:
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
        MOV       RAX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PSHUFLW   XMM1,XMM1, $FF
        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[RAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      [RDX],XMM2

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [RDX], ECX
@3:
{$ENDIF}
end;

function BlendRegEx_SSE2(F, B, M: TColor32): TColor32;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result := M * Fa * (Frgb - Brgb) + Brgb

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
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV       EAX,EDX
        POP       EBX
@2:
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
        ADD       R8,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8
        MOV       R8,bias_ptr
        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}

@1:     MOV       EAX,EDX
@2:
{$ENDIF}
end;

procedure BlendMemEx_SSE2(F: TColor32; var B:TColor32; M: TColor32);
asm
{$IFDEF TARGET_x86}
  // blend foreground color (F) to a background color (B),
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
  // Result := M * Fa * (Frgb - Brgb) + Brgb

        TEST      ECX, $FF000000
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
        ADD       R8,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8
        MOV       R8,bias_ptr
        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      DWORD PTR [RDX],XMM1
@1:
{$ENDIF}
end;

procedure BlendLine_SSE2(Src, Dst: PColor32; Count: Integer);
asm
{$IFDEF TARGET_X86}
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

        TEST      ECX,ECX
        JZ        @4

        PUSH      EBX

        MOV       EBX,EAX

@1:     MOV       EAX,[EBX]
        TEST      EAX,$FF000000
        JZ        @3
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX
        PXOR      XMM3,XMM3
        MOVD      XMM2,[EDX]
        PUNPCKLBW XMM0,XMM3
        MOV       EAX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PUNPCKLBW XMM1,XMM3
        PUNPCKHWD XMM1,XMM1
        PSUBW     XMM0,XMM2
        PUNPCKHDQ XMM1,XMM1
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[EAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      EAX, XMM2

@2:     MOV       [EDX],EAX

@3:     ADD       EBX,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1

        POP       EBX

@4:
{$ENDIF}

{$IFDEF TARGET_X64}
  // ECX <- Src
  // EDX <- Dst
  // R8D <- Count

        TEST      R8D,R8D
        JZ        @4

@1:     MOV       EAX,[RCX]
        TEST      EAX,$FF000000
        JZ        @3
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX
        PXOR      XMM3,XMM3
        MOVD      XMM2,[RDX]
        PUNPCKLBW XMM0,XMM3
        MOV       RAX,bias_ptr
        PUNPCKLBW XMM2,XMM3
        MOVQ      XMM1,XMM0
        PUNPCKLBW XMM1,XMM3
        PUNPCKHWD XMM1,XMM1
        PSUBW     XMM0,XMM2
        PUNPCKHDQ XMM1,XMM1
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1
        PADDW     XMM2,[RAX]
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8
        PACKUSWB  XMM2,XMM3
        MOVD      EAX, XMM2

@2:     MOV       [RDX],EAX

@3:     ADD       RCX,4
        ADD       RDX,4

        DEC       R8D
        JNZ       @1

@4:
{$ENDIF}
end;


procedure BlendLineEx_SSE2(Src, Dst: PColor32; Count: Integer; M: TColor32);
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
        ADD       RAX,alpha_ptr
        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[RAX]
        PSLLW     XMM2,8
        MOV       RAX,bias_ptr
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

function CombineReg_SSE2(X, Y, W: TColor32): TColor32;
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

        ADD       R8,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8

        MOV       R8,bias_ptr

        PADDW     XMM2,[R8]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      EAX,XMM1
{$ENDIF}
end;

procedure CombineMem_SSE2(F: TColor32; var B: TColor32; W: TColor32);
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

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [EDX],EAX
@3:
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

        ADD       R8,alpha_ptr

        PSUBW     XMM1,XMM2
        PMULLW    XMM1,[R8]
        PSLLW     XMM2,8

        MOV       RAX,bias_ptr

        PADDW     XMM2,[RAX]
        PADDW     XMM1,XMM2
        PSRLW     XMM1,8
        PACKUSWB  XMM1,XMM0
        MOVD      [RDX],XMM1

{$IFDEF FPC}
@1:     JMP @3
{$ELSE}
@1:     RET
{$ENDIF}

@2:     MOV       [RDX],ECX
@3:
{$ENDIF}
end;


procedure CombineLine_SSE2(Src, Dst: PColor32; Count: Integer; W: TColor32);
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

@1:     MOVD      XMM1,[EAX]
        PXOR      XMM0,XMM0
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
        ADD       R9,alpha_ptr
        MOVQ      XMM3,[R9]
        MOV       R9,bias_ptr
        MOVQ      XMM4,[R9]

@1:     MOVD      XMM1,[RCX]
        PXOR      XMM0,XMM0
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

{$IFDEF FPC}
@2:     JMP @4
{$ELSE}
@2:     RET
{$ENDIF}

@3:     SHL       R8D,2
        CALL      Move
@4:
{$ENDIF}
end;

function MergeReg_SSE2(F, B: TColor32): TColor32;
asm
  { This is an implementation of the merge formula, as described
    in a paper by Bruce Wallace in 1981. Merging is associative,
    that is, A over (B over C) = (A over B) over C. The formula is,

      Ra = Fa + Ba - Fa * Ba
      Rc = (Fa (Fc - Bc * Ba) + Bc * Ba) / Ra

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
      // Rc := Bc + Wa * (Fc - Bc)

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

{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}
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

{$IFDEF FPC}
        JMP @2
{$ELSE}
        RET
{$ENDIF}
@1:     MOV       EAX,EDX
@2:
{$ENDIF}
end;

procedure EMMS_SSE2;
asm
end;


function LightenReg_SSE2(C: TColor32; Amount: Integer): TColor32;
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

function ColorAdd_SSE2(C1, C2: TColor32): TColor32;
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

function ColorSub_SSE2(C1, C2: TColor32): TColor32;
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

function ColorModulate_SSE2(C1, C2: TColor32): TColor32;
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

function ColorMax_SSE2(C1, C2: TColor32): TColor32;
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

function ColorMin_SSE2(C1, C2: TColor32): TColor32;
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

function ColorDifference_SSE2(C1, C2: TColor32): TColor32;
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

function ColorExclusion_SSE2(C1, C2: TColor32): TColor32;
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

function ColorScale_SSE2(C, W: TColor32): TColor32;
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
        ADD       RDX,alpha_ptr
        PMULLW    XMM0,[RDX]
        PSRLW     XMM0,8
        PACKUSWB  XMM0,XMM2
        MOVD      EAX,XMM0
{$ENDIF}
end;

{$ENDIF}
{$ENDIF}

{ Misc stuff }

function Lighten(C: TColor32; Amount: Integer): TColor32;
begin
  Result := LightenReg(C, Amount);
end;

procedure MakeMergeTables;
var
  I, J: Integer;
const
  OneByteth : Double = 1 / 255;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      DivTable[I, J] := Round(I * J * OneByteth);
      if I > 0 then
        RcTable[I, J] := Round(J * 255 / I)
      else
        RcTable[I, J] := 0;
    end;
end;

const
  FID_EMMS = 0;
  FID_MERGEREG = 1;
  FID_MERGEMEM = 2;
  FID_MERGELINE = 3;
  FID_MERGEREGEX = 4;
  FID_MERGEMEMEX = 5;
  FID_MERGELINEEX = 6;
  FID_COMBINEREG = 7;
  FID_COMBINEMEM = 8;
  FID_COMBINELINE = 9;

  FID_BLENDREG = 10;
  FID_BLENDMEM = 11;
  FID_BLENDLINE = 12;
  FID_BLENDREGEX = 13;
  FID_BLENDMEMEX = 14;
  FID_BLENDLINEEX = 15;

  FID_COLORMAX = 16;
  FID_COLORMIN = 17;
  FID_COLORAVERAGE = 18;
  FID_COLORADD = 19;
  FID_COLORSUB = 20;
  FID_COLORDIV = 21;
  FID_COLORMODULATE = 22;
  FID_COLORDIFFERENCE = 23;
  FID_COLOREXCLUSION = 24;
  FID_COLORSCALE = 25;
  FID_Lighten = 26;

procedure RegisterBindings;
begin
  BlendRegistry := NewRegistry('GR32_Blend bindings');
{$IFNDEF OMIT_MMX}
  BlendRegistry.RegisterBinding(FID_EMMS, @@EMMS);
{$ENDIF}
  BlendRegistry.RegisterBinding(FID_MERGEREG, @@MergeReg);
  BlendRegistry.RegisterBinding(FID_MERGEMEM, @@MergeMem);
  BlendRegistry.RegisterBinding(FID_MERGELINE, @@MergeLine);
  BlendRegistry.RegisterBinding(FID_MERGEREGEX, @@MergeRegEx);
  BlendRegistry.RegisterBinding(FID_MERGEMEMEX, @@MergeMemEx);
  BlendRegistry.RegisterBinding(FID_MERGELINEEX, @@MergeLineEx);
  BlendRegistry.RegisterBinding(FID_COMBINEREG, @@CombineReg);
  BlendRegistry.RegisterBinding(FID_COMBINEMEM, @@CombineMem);
  BlendRegistry.RegisterBinding(FID_COMBINELINE, @@CombineLine);

  BlendRegistry.RegisterBinding(FID_BLENDREG, @@BlendReg);
  BlendRegistry.RegisterBinding(FID_BLENDMEM, @@BlendMem);
  BlendRegistry.RegisterBinding(FID_BLENDLINE, @@BlendLine);
  BlendRegistry.RegisterBinding(FID_BLENDREGEX, @@BlendRegEx);
  BlendRegistry.RegisterBinding(FID_BLENDMEMEX, @@BlendMemEx);
  BlendRegistry.RegisterBinding(FID_BLENDLINEEX, @@BlendLineEx);

  BlendRegistry.RegisterBinding(FID_COLORMAX, @@ColorMax);
  BlendRegistry.RegisterBinding(FID_COLORMIN, @@ColorMin);
  BlendRegistry.RegisterBinding(FID_COLORAVERAGE, @@ColorAverage);
  BlendRegistry.RegisterBinding(FID_COLORADD, @@ColorAdd);
  BlendRegistry.RegisterBinding(FID_COLORSUB, @@ColorSub);
  BlendRegistry.RegisterBinding(FID_COLORDIV, @@ColorDiv);
  BlendRegistry.RegisterBinding(FID_COLORMODULATE, @@ColorModulate);
  BlendRegistry.RegisterBinding(FID_COLORDIFFERENCE, @@ColorDifference);
  BlendRegistry.RegisterBinding(FID_COLOREXCLUSION, @@ColorExclusion);
  BlendRegistry.RegisterBinding(FID_COLORSCALE, @@ColorScale);

  BlendRegistry.RegisterBinding(FID_LIGHTEN, @@LightenReg);

  // pure pascal
  BlendRegistry.Add(FID_EMMS, @EMMS_Pas);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_Pas);
  BlendRegistry.Add(FID_MERGEMEM, @MergeMem_Pas);
  BlendRegistry.Add(FID_MERGEMEMEX, @MergeMemEx_Pas);
  BlendRegistry.Add(FID_MERGEREGEX, @MergeRegEx_Pas);
  BlendRegistry.Add(FID_MERGELINE, @MergeLine_Pas);
  BlendRegistry.Add(FID_MERGELINEEX, @MergeLineEx_Pas);
  BlendRegistry.Add(FID_COLORDIV, @ColorDiv_Pas);
  BlendRegistry.Add(FID_COLORAVERAGE, @ColorAverage_Pas);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_Pas);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_Pas);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_Pas);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_Pas);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_Pas);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_Pas);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_Pas);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_Pas);
  BlendRegistry.Add(FID_BLENDLINEEX, @BlendLineEx_Pas);
  BlendRegistry.Add(FID_COLORMAX, @ColorMax_Pas);
  BlendRegistry.Add(FID_COLORMIN, @ColorMin_Pas);
  BlendRegistry.Add(FID_COLORADD, @ColorAdd_Pas);
  BlendRegistry.Add(FID_COLORSUB, @ColorSub_Pas);
  BlendRegistry.Add(FID_COLORMODULATE, @ColorModulate_Pas);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_Pas);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_Pas);
  BlendRegistry.Add(FID_COLORSCALE, @ColorScale_Pas);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_Pas);

{$IFNDEF PUREPASCAL}
  BlendRegistry.Add(FID_EMMS, @EMMS_ASM, []);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_ASM, []);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_ASM, []);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_ASM, []);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_ASM, []);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_ASM, []);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_ASM, []);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_ASM, []);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_Pas, []);   // no ASM version available
{$IFNDEF OMIT_MMX}
  BlendRegistry.Add(FID_EMMS, @EMMS_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDLINEEX, @BlendLineEx_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLORMAX, @ColorMax_EMMX, [ciEMMX]);
  BlendRegistry.Add(FID_COLORMIN, @ColorMin_EMMX, [ciEMMX]);
  BlendRegistry.Add(FID_COLORADD, @ColorAdd_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLORSUB, @ColorSub_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLORMODULATE, @ColorModulate_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COLORSCALE, @ColorScale_MMX, [ciMMX]);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_MMX, [ciMMX]);
{$ENDIF}
{$IFNDEF OMIT_SSE2}
  BlendRegistry.Add(FID_EMMS, @EMMS_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDLINEEX, @BlendLineEx_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORMAX, @ColorMax_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORMIN, @ColorMin_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORADD, @ColorAdd_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORSUB, @ColorSub_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORMODULATE, @ColorModulate_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COLORSCALE, @ColorScale_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_SSE2, [ciSSE]);
{$ENDIF}
{$IFNDEF TARGET_x64}
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_ASM, []);
{$ENDIF}
{$ENDIF}

  BlendRegistry.RebindAll;
end;

initialization
  RegisterBindings;
  MakeMergeTables;

{$IFNDEF PUREPASCAL}
  MMX_ACTIVE := (ciMMX in CPUFeatures);
  if [ciMMX, ciSSE2] * CPUFeatures <> [] then
    GenAlphaTable;
{$ELSE}
  MMX_ACTIVE := False;
{$ENDIF}

finalization
{$IFNDEF PUREPASCAL}
{$IFNDEF OMIT_MMX}
  if (ciMMX in CPUFeatures) then FreeAlphaTable;
{$ENDIF}
{$ENDIF}

end.
