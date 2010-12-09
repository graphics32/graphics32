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
  TCombineReg  = function(X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure(X: TColor32; var Y: TColor32; W: TColor32);
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);
  TCombineLine = procedure(Src, Dst: PColor32; Count: Integer; W: TColor32);

var
  EMMS: procedure;
{ Function Variables }
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;

  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

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
function Lighten(C: TColor32; Amount: Integer): TColor32;

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

implementation

{$IFDEF TARGET_x86}
uses GR32_LowLevel;
{$ENDIF}

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;

{ Merge }

function MergeReg_Pas(F, B: TColor32): TColor32;
var
  PF, PB, PR: PByteArray;
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  RX: TColor32Entry absolute Result;
  X: Integer;
begin
  if FX.A = $FF then
    Result := F
  else if FX.A = $0 then
    Result := B
  else if BX.A = $0 then
    Result := F
  else if BX.A = $FF then
    Result := BlendReg(F,B)
  else
  begin
    PF := @DivTable[FX.A];
    PB := @DivTable[BX.A];
    RX.A := BX.A + FX.A - PB^[FX.A];
    PR := @RcTable[RX.A];

    // Red component
    RX.R := PB[BX.R];
    X := FX.R - RX.R;
    if X >= 0 then
      RX.R := PR[PF[X] + RX.R]
    else
      RX.R := PR[RX.R - PF[-X]];

    // Green component
    RX.G := PB[BX.G];
    X := FX.G - RX.G;
    if X >= 0 then RX.G := PR[PF[X] + RX.G]
    else RX.G := PR[RX.G - PF[-X]];

    // Blue component
    RX.B := PB[BX.B];
    X := FX.B - RX.B;
    if X >= 0 then RX.B := PR[PF[X] + RX.B]
    else RX.B := PR[RX.B - PF[-X]];
  end;
end;

{$IFDEF TARGET_x86}
{$IFNDEF PUREPASCAL}

function MergeReg_ASM(F, B: TColor32): TColor32;
asm
  // EAX <- F
  // EDX <- B

  // GR32_Blend.pas.156: if F.A = 0 then
    test eax,$ff000000
    jz   @exit0

  // GR32_Blend.pas.160: else if B.A = 255 then
    cmp     edx,$ff000000
    jnc     @blend

  // GR32_Blend.pas.158: else if F.A = 255 then
    cmp     eax,$ff000000
    jnc     @exit

  // else if B.A = 0 then
    test    edx,$ff000000
    jz      @exit

@4:
    push ebx
    push esi
    push edi
    add  esp,-$0c
    mov  [esp+$04],edx
    mov  [esp],eax

  // AH <- F.A
  // DL, CL <- B.A
    shr eax,16
    and eax,$0000ff00
    shr edx,24
    mov cl,dl
    nop
    nop
    nop

  // EDI <- PF
  // EDX <- PB
  // ESI <- PR

  // GR32_Blend.pas.164: PF := @DivTable[F.A];
    lea edi,[eax+DivTable]
  // GR32_Blend.pas.165: PB := @DivTable[B.A];
    shl edx,$08
    lea edx,[edx+DivTable]
  // GR32_Blend.pas.166: Result.A := B.A + F.A - PB[F.A];
    shr eax,8
    //add cl,al
    add ecx,eax
    //sub cl,[edx+eax]
    sub ecx,[edx+eax]
    mov [esp+$0b],cl
  // GR32_Blend.pas.167: PR := @RcTable[Result.A];
    shl ecx,$08
    and ecx,$0000ffff
    lea esi,[ecx+RcTable]

  { Red component }

  // GR32_Blend.pas.169: Result.R := PB[B.R];
    xor eax,eax
    mov al,[esp+$06]
    mov cl,[edx+eax]
    mov [esp+$0a],cl
  // GR32_Blend.pas.170: X := F.R - Result.R;
    mov al,[esp+$02]
    xor ebx,ebx
    mov bl,cl
    sub eax,ebx
  // GR32_Blend.pas.171: if X >= 0 then
    jl @5
  // GR32_Blend.pas.172: Result.R := PR[PF[X] + Result.R]
    movzx eax,byte ptr[edi+eax]
    and ecx,$000000ff
    add eax,ecx
    mov al,[esi+eax]
    mov [esp+$0a],al
    jmp @6
@5:
  // GR32_Blend.pas.252: Result.R := PR[Result.R - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor ecx,ecx
    mov cl,[esp+$0a]
    sub ecx,eax
    mov al,[esi+ecx]
    mov [esp+$0a],al


  { Green component }

@6:
  // GR32_Blend.pas.176: Result.G := PB[B.G];
    xor eax,eax
    mov al,[esp+$05]
    mov cl,[edx+eax]
    mov [esp+$09],cl
  // GR32_Blend.pas.177: X := F.G - Result.G;
    mov al,[esp+$01]
    xor ebx,ebx
    mov bl,cl
    sub eax,ebx
  // GR32_Blend.pas.178: if X >= 0 then
    jl @7
  // GR32_Blend.pas.179: Result.G := PR[PF[X] + Result.G]
    movzx eax,byte ptr[edi+eax]
    and ecx,$000000ff
    add eax,ecx
    mov al,[esi+eax]
    mov [esp+$09],al
    jmp @8
@7:
  // GR32_Blend.pas.259: Result.G := PR[Result.G - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor ecx,ecx
    mov cl,[esp+$09]
    sub ecx,eax
    mov al,[esi+ecx]
    mov [esp+$09],al


  { Blue component }

@8:
  // GR32_Blend.pas.183: Result.B := PB[B.B];
    xor eax,eax
    mov al,[esp+$04]
    mov cl,[edx+eax]
    mov [esp+$08],cl
  // GR32_Blend.pas.184: X := F.B - Result.B;
    mov al,[esp]
    xor edx,edx
    mov dl,cl
    sub eax,edx
  // GR32_Blend.pas.185: if X >= 0 then
    jl @9
  // GR32_Blend.pas.186: Result.B := PR[PF[X] + Result.B]
    movzx eax,byte ptr[edi+eax]
    xor edx,edx
    mov dl,cl
    add eax,edx
    mov al,[esi+eax]
    mov [esp+$08],al
    jmp @10
@9:
  // GR32_Blend.pas.266: Result.B := PR[Result.B - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor edx,edx
    mov dl,cl
    sub edx,eax
    mov al,[esi+edx]
    mov [esp+$08],al

@10:
  // EAX <- Result
    mov eax,[esp+$08]

  // GR32_Blend.pas.190: end;
    add esp,$0c
    pop edi
    pop esi
    pop ebx
    ret
@blend:
    call dword ptr [BlendReg]
    or   eax,$ff000000
    ret
@exit0:
    mov eax,edx
@exit:
end;

{$ENDIF}
{$ENDIF}

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
    Dst^ := MergeReg(PM[Src^ shr 24] shl 24 or Src^ and $00FFFFFF, Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

{ Non-MMX versions }

const bias = $00800080;

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

{$IFDEF TARGET_x86}

function CombineReg_ASM(X, Y, W: TColor32): TColor32;

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

{$ENDIF}

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

{$IFDEF TARGET_x86}

procedure CombineMem_ASM(X: TColor32; var Y: TColor32; W: TColor32);
asm
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
@1:     RET

@2:     MOV     [EDX],EAX
        RET
end;

{$ENDIF}

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

{$IFDEF TARGET_x86}

function BlendReg_ASM(F, B: TColor32): TColor32;
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
{$ENDIF}

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

{$IFDEF TARGET_x86}

procedure BlendMem_ASM(F: TColor32; var B: TColor32);
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

{$ENDIF}

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



{$IFDEF TARGET_x86}

function BlendRegEx_ASM(F, B, M: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, in this case Graphics32 uses BlendReg
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

{$ENDIF}

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

{$IFDEF TARGET_x86}

procedure BlendMemEx_ASM(F: TColor32; var B: TColor32; M: TColor32);
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

{$ENDIF}

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

{$IFDEF TARGET_x86}

procedure BlendLine_ASM(Src, Dst: PColor32; Count: Integer);
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

{$ENDIF}

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

{ MMX versions }

procedure EMMS_Pas;
begin
//Dummy
end;

{$IFDEF TARGET_x86}
{$IFNDEF PUREPASCAL}

procedure EMMS_MMX;
asm
  EMMS
end;

procedure GenAlphaTable;
var
  I: Integer;
  L: Longword;
  P: ^Longword;
begin
  GetMem(AlphaTable, 257 * 8);
  alpha_ptr := Pointer(Cardinal(AlphaTable) and $FFFFFFF8);
  if Cardinal(alpha_ptr) < Cardinal(AlphaTable) then
    {$IFDEF FPC}
    Inc(alpha_ptr, 8);
    {$ELSE}
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
    {$ENDIF}
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

function CombineReg_MMX(X, Y, W: TColor32): TColor32;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        MOVD      MM1,EAX
        PXOR      MM0,MM0
        SHL       ECX,3

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
end;

procedure CombineMem_MMX(F: TColor32; var B: TColor32; W: TColor32);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ      @1
        CMP       ECX,$FF
        JZ        @2

        MOVD      MM1,EAX
        PXOR      MM0,MM0

        SHL       ECX,3

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
end;

function BlendReg_MMX(F, B: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
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
end;

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

@1:     RET

@2:     MOV       [EDX],EAX
end;

function BlendRegEx_MMX(F, B, M: TColor32): TColor32;
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

        PXOR      MM0,MM0
        MOVD      MM1,EAX
        SHL       ECX,3
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

procedure BlendMemEx_MMX(F: TColor32; var B:TColor32; M: TColor32);
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

        PXOR      MM0,MM0
        MOVD      MM1,EAX
        SHL       ECX,3
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
end;

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

@4:     RET
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
        SHL       EBX,3
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

        SHL       EBX,3
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
{$ENDIF}

{ Non-MMX Color algebra versions }

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

{ MMX Color algebra versions }

{$IFDEF TARGET_x86}
{$IFNDEF PUREPASCAL}
function ColorAdd_MMX(C1, C2: TColor32): TColor32;
asm
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PADDUSB   MM0,MM1
        MOVD      EAX,MM0
end;

function ColorSub_MMX(C1, C2: TColor32): TColor32;
asm
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PSUBUSB   MM0,MM1
        MOVD      EAX,MM0
end;

function ColorModulate_MMX(C1, C2: TColor32): TColor32;
asm
        PXOR      MM2,MM2
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        MOVD      MM1,EDX
        PUNPCKLBW MM1,MM2
        PMULLW    MM0,MM1
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
end;

function ColorMax_EMMX(C1, C2: TColor32): TColor32;
asm
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PMAXUB    MM0,MM1
        MOVD      EAX,MM0
end;

function ColorMin_EMMX(C1, C2: TColor32): TColor32;
asm
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        PMINUB    MM0,MM1
        MOVD      EAX,MM0
end;

function ColorDifference_MMX(C1, C2: TColor32): TColor32;
asm
        MOVD      MM0,EAX
        MOVD      MM1,EDX
        MOVQ      MM2,MM0
        PSUBUSB   MM0,MM1
        PSUBUSB   MM1,MM2
        POR       MM0,MM1
        MOVD      EAX,MM0
end;

function ColorExclusion_MMX(C1, C2: TColor32): TColor32;
asm
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
end;

function ColorScale_MMX(C, W: TColor32): TColor32;
asm
        PXOR      MM2,MM2
        SHL       EDX,3
        MOVD      MM0,EAX
        PUNPCKLBW MM0,MM2
        ADD       EDX,alpha_ptr
        PMULLW    MM0,[EDX]
        PSRLW     MM0,8
        PACKUSWB  MM0,MM2
        MOVD      EAX,MM0
end;
{$ENDIF}
{$ENDIF}

{ Misc stuff }

function Lighten(C: TColor32; Amount: Integer): TColor32;
var
  r, g, b, a: Integer;
begin
  a := C shr 24;
  r := (C and $00FF0000) shr 16;
  g := (C and $0000FF00) shr 8;
  b := C and $000000FF;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  Result := a shl 24 + r shl 16 + g shl 8 + b;
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

procedure RegisterBindings;
begin
  BlendRegistry := NewRegistry('GR32_Blend bindings');
  BlendRegistry.RegisterBinding(FID_EMMS, @@EMMS);
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

{$IFDEF TARGET_x86}
{$IFNDEF PUREPASCAL}
  BlendRegistry.Add(FID_EMMS, @EMMS_MMX, [ciMMX]);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_ASM, []);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_ASM, []);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_ASM, []);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_MMX, [ciMMX]);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_ASM, []);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_ASM, []);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_ASM, []);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_ASM, []);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_ASM, []);
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
{$ENDIF}
{$ENDIF}

  BlendRegistry.RebindAll;
end;

initialization
  RegisterBindings;
  MakeMergeTables;

{$IFNDEF PUREPASCAL}
{$IFDEF TARGET_x86}
  if (ciMMX in CPUFeatures) then
  begin
    GenAlphaTable;
    MMX_ACTIVE := (ciMMX in CPUFeatures);
  end
  else
    MMX_ACTIVE := False;
{$ELSE}
  MMX_ACTIVE := False;
{$ENDIF}
{$ELSE}
  MMX_ACTIVE := False;
{$ENDIF}

finalization
{$IFDEF TARGET_x86}
{$IFNDEF PUREPASCAL}
  if (ciMMX in CPUFeatures) then FreeAlphaTable;
{$ENDIF}
{$ENDIF}

end.
