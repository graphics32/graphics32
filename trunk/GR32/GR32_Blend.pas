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
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
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
  GR32, GR32_System, SysUtils;

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


var
  GR32_Blend_FunctionTemplates : TTemplatesHandle;

{ Access to alpha composite functions corresponding to a combine mode }

const
  BLEND_REG: array[TCombineMode] of ^TBlendReg = ((@@BlendReg),(@@MergeReg));
  BLEND_MEM: array[TCombineMode] of ^TBlendMem = ((@@BlendMem),(@@MergeMem));
  BLEND_REG_EX: array[TCombineMode] of ^TBlendRegEx = ((@@BlendRegEx),(@@MergeRegEx));
  BLEND_MEM_EX: array[TCombineMode] of ^TBlendMemEx = ((@@BlendMemEx),(@@MergeMemEx));
  BLEND_LINE: array[TCombineMode] of ^TBlendLine = ((@@BlendLine),(@@MergeLine));
  BLEND_LINE_EX: array[TCombineMode] of ^TBlendLineEx = ((@@BlendLineEx),(@@MergeLineEx));

implementation

uses
  GR32_LowLevel;

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
        JZ      @2

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

procedure EMMS_MMX;
asm
  db $0F,$77               /// EMMS
end;

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

function CombineReg_MMX(X, Y, W: TColor32): TColor32;
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

procedure CombineMem_MMX(F: TColor32; var B: TColor32; W: TColor32);
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

function BlendReg_MMX(F, B: TColor32): TColor32;
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

procedure BlendMem_MMX(F: TColor32; var B: TColor32);
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

        CMP       EDX,$FF
        JZ        @4              // weight = 255  =>  copy src to dst

        SHL       EBX,3
        ADD       EBX,alpha_ptr
        db $0F,$6F,$1B           /// MOVQ      MM3,[EBX]
        MOV       EBX,bias_ptr
        db $0F,$6F,$23           /// MOVQ      MM4,[EBX]

   // loop start
@1:     db $0F,$6E,$08           /// MOVD      MM1,[EAX]
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0

        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$CB           /// PMULLW    MM1,MM3
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8

        db $0F,$FD,$D4           /// PADDW     MM2,MM4
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1

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
function ColorAdd_MMX(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DC,$C1           /// PADDUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function ColorSub_MMX(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function ColorModulate_MMX(C1, C2: TColor32): TColor32;
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

function ColorMax_EMMX(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DE,$C1           /// PMAXUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function ColorMin_EMMX(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DA,$C1           /// PMINUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function ColorDifference_MMX(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$6F,$D0           /// MOVQ      MM2,MM0
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$D8,$CA           /// PSUBUSB   MM1,MM2
        db $0F,$EB,$C1           /// POR       MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function ColorExclusion_MMX(C1, C2: TColor32): TColor32;
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

function ColorScale_MMX(C, W: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        SHL       EDX,3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        ADD       EDX,alpha_ptr
        db $0F,$D5,$02           /// PMULLW    MM0,[EDX]
        db $0F,$71,$D0,$08       /// PSRLW     MM0,8
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;
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
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      DivTable[I, J] := Round(I * J / 255);
      if I > 0 then
        RcTable[I, J] := Round(J * 255 / I)
      else
        RcTable[I, J] := 0;
    end;
end;

{CPU target and feature Function templates}

const

  MergeMemProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeMem_Pas; Requires: []));


  MergeMemExProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeMemEx_Pas; Requires: []));

  MergeRegExProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeRegEx_Pas; Requires: []));

  MergeLineProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeLine_Pas; Requires: []));

  MergeLineExProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeLineEx_Pas; Requires: []));

  ColorDivProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorDiv_Pas; Requires: []));

  ColorAverageProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorAverage_Pas; Requires: []));

{$IFDEF TARGET_x86}

  MergeRegProcs : array [0..1] of TFunctionInfo = (
    (Address : @MergeReg_Pas; Requires: []),
    (Address : @MergeReg_ASM; Requires: []));

  EMMSProcs : array [0..1] of TFunctionInfo = (
    (Address : @EMMS_Pas; Requires: []),
    (Address : @EMMS_MMX; Requires: [ciMMX]));

  CombineRegProcs : array [0..2] of TFunctionInfo = (
    (Address : @CombineReg_Pas; Requires: []),
    (Address : @CombineReg_ASM; Requires: []),
    (Address : @CombineReg_MMX; Requires: [ciMMX]));

  CombineMemProcs : array [0..2] of TFunctionInfo = (
    (Address : @CombineMem_Pas; Requires: []),
    (Address : @CombineMem_ASM; Requires: []),
    (Address : @CombineMem_MMX; Requires: [ciMMX]));

  CombineLineProcs : array [0..1] of TFunctionInfo = (
    (Address : @CombineLine_Pas; Requires: []),
    (Address : @CombineLine_MMX; Requires: [ciMMX]));

  BlendRegProcs : array [0..2] of TFunctionInfo = (
    (Address : @BlendReg_Pas; Requires: []),
    (Address : @BlendReg_ASM; Requires: []),
    (Address : @BlendReg_MMX; Requires: [ciMMX]));

  BlendMemProcs : array [0..2] of TFunctionInfo = (
    (Address : @BlendMem_Pas; Requires: []),
    (Address : @BlendMem_ASM; Requires: []),
    (Address : @BlendMem_MMX; Requires: [ciMMX]));

  BlendRegExProcs : array [0..2] of TFunctionInfo = (
    (Address : @BlendRegEx_Pas; Requires: []),
    (Address : @BlendRegEx_ASM; Requires: []),
    (Address : @BlendRegEx_MMX; Requires: [ciMMX]));

  BlendMemExProcs : array [0..2] of TFunctionInfo = (
    (Address : @BlendMemEx_Pas; Requires: []),
    (Address : @BlendMemEx_ASM; Requires: []),
    (Address : @BlendMemEx_MMX; Requires: [ciMMX]));

  BlendLineProcs : array [0..2] of TFunctionInfo = (
    (Address : @BlendLine_Pas; Requires: []),
    (Address : @BlendLine_ASM; Requires: []),
    (Address : @BlendLine_MMX; Requires: [ciMMX]));

  BlendLineExProcs : array [0..1] of TFunctionInfo = (
    (Address : @BlendLineEx_Pas; Requires: []),
    (Address : @BlendLineEx_MMX; Requires: [ciMMX]));



  ColorMaxProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorMax_Pas; Requires: []),
    (Address : @ColorMax_EMMX; Requires: [ciEMMX]));

  ColorMinProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorMin_Pas; Requires: []),
    (Address : @ColorMin_EMMX; Requires: [ciEMMX]));

  ColorAddProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorAdd_Pas; Requires: []),
    (Address : @ColorAdd_MMX; Requires: [ciMMX]));

  ColorSubProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorSub_Pas; Requires: []),
    (Address : @ColorSub_MMX; Requires: [ciMMX]));

  ColorModulateProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorModulate_Pas; Requires: []),
    (Address : @ColorModulate_MMX; Requires: [ciMMX]));

  ColorDifferenceProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorDifference_Pas; Requires: []),
    (Address : @ColorDifference_MMX; Requires: [ciMMX]));

  ColorExclusionProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorExclusion_Pas; Requires: []),
    (Address : @ColorExclusion_MMX; Requires: [ciMMX]));

  ColorScaleProcs : array [0..1] of TFunctionInfo = (
    (Address : @ColorScale_Pas; Requires: []),
    (Address : @ColorScale_MMX; Requires: [ciMMX]));

{$ELSE}

  MergeRegProcs : array [0..0] of TFunctionInfo = (
    (Address : @MergeReg_Pas; Requires: []));

  EMMSProcs : array [0..0] of TFunctionInfo = (
    (Address : @EMMS_Pas; Requires: []));

  CombineRegProcs : array [0..0] of TFunctionInfo = (
    (Address : @CombineReg_Pas; Requires: []));

  CombineMemProcs : array [0..0] of TFunctionInfo = (
    (Address : @CombineMem_Pas; Requires: []));

  CombineLineProcs : array [0..0] of TFunctionInfo = (
    (Address : @CombineLine_Pas; Requires: []));


  BlendRegProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendReg_Pas; Requires: []));

  BlendMemProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendMem_Pas; Requires: []));

  BlendLineProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendLine_Pas; Requires: []));


  BlendRegExProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendRegEx_Pas; Requires: []));

  BlendMemExProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendMemEx_Pas; Requires: []));

  BlendLineExProcs : array [0..0] of TFunctionInfo = (
    (Address : @BlendLineEx_Pas; Requires: []));



  ColorMaxProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorMax_Pas; Requires: []));

  ColorMinProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorMin_Pas; Requires: []));

  ColorAddProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorAdd_Pas; Requires: []));

  ColorSubProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorSub_Pas; Requires: []));

  ColorModulateProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorModulate_Pas; Requires: []));

  ColorDifferenceProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorDifference_Pas; Requires: []));

  ColorExclusionProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorExclusion_Pas; Requires: []));

  ColorScaleProcs : array [0..0] of TFunctionInfo = (
    (Address : @ColorScale_Pas; Requires: []));

{$ENDIF}

{Complete collection of unit templates}

var
  FunctionTemplates : array [0..25] of TFunctionTemplate = (
     (FunctionVar: @@EMMS; FunctionProcs : @EMMSProcs; Count: Length(EMMSProcs)),

     (FunctionVar: @@MergeReg; FunctionProcs : @MergeRegProcs; Count: Length(MergeRegProcs)),
     (FunctionVar: @@MergeMem; FunctionProcs : @MergeMemProcs; Count: Length(MergeMemProcs)),
     (FunctionVar: @@MergeLine; FunctionProcs : @MergeLineProcs; Count: Length(MergeLineProcs)),
     (FunctionVar: @@MergeRegEx; FunctionProcs : @MergeRegExProcs; Count: Length(MergeRegExProcs)),
     (FunctionVar: @@MergeMemEx; FunctionProcs : @MergeMemExProcs; Count: Length(MergeMemExProcs)),
     (FunctionVar: @@MergeLineEx; FunctionProcs : @MergeLineExProcs; Count: Length(MergeLineExProcs)),

     (FunctionVar: @@CombineReg; FunctionProcs : @CombineRegProcs; Count: Length(CombineRegProcs)),
     (FunctionVar: @@CombineMem; FunctionProcs : @CombineMemProcs; Count: Length(CombineMemProcs)),
     (FunctionVar: @@CombineLine; FunctionProcs : @CombineLineProcs; Count: Length(CombineLineProcs)),

     (FunctionVar: @@BlendReg; FunctionProcs : @BlendRegProcs; Count: Length(BlendRegProcs)),
     (FunctionVar: @@BlendMem; FunctionProcs : @BlendMemProcs; Count: Length(BlendMemProcs)),
     (FunctionVar: @@BlendLine; FunctionProcs : @BlendLineProcs; Count: Length(BlendLineProcs)),
     (FunctionVar: @@BlendRegEx; FunctionProcs : @BlendRegExProcs; Count: Length(BlendRegExProcs)),
     (FunctionVar: @@BlendMemEx; FunctionProcs : @BlendMemExProcs; Count: Length(BlendMemExProcs)),
     (FunctionVar: @@BlendLineEx; FunctionProcs : @BlendLineExProcs; Count: Length(BlendLineExProcs)),

     (FunctionVar: @@ColorMax; FunctionProcs : @ColorMaxProcs; Count: Length(ColorMaxProcs)),
     (FunctionVar: @@ColorMin; FunctionProcs : @ColorMinProcs; Count: Length(ColorMinProcs)),
     (FunctionVar: @@ColorAverage; FunctionProcs : @ColorAverageProcs; Count: Length(ColorAverageProcs)),
     (FunctionVar: @@ColorAdd; FunctionProcs : @ColorAddProcs; Count: Length(ColorAddProcs)),
     (FunctionVar: @@ColorSub; FunctionProcs : @ColorSubProcs; Count: Length(ColorSubProcs)),
     (FunctionVar: @@ColorDiv; FunctionProcs : @ColorDivProcs; Count: Length(ColorDivProcs)),
     (FunctionVar: @@ColorModulate; FunctionProcs : @ColorModulateProcs; Count: Length(ColorModulateProcs)),
     (FunctionVar: @@ColorDifference; FunctionProcs : @ColorDifferenceProcs; Count: Length(ColorDifferenceProcs)),
     (FunctionVar: @@ColorExclusion; FunctionProcs : @ColorExclusionProcs; Count: Length(ColorExclusionProcs)),
     (FunctionVar: @@ColorScale; FunctionProcs : @ColorScaleProcs; Count: Length(ColorScaleProcs))
   );

type
  TUnitAccess = class
  end;

initialization
  MakeMergeTables;
  GR32_Blend_FunctionTemplates := RegisterTemplates(FunctionTemplates,
    GetUnitName(TypeInfo(TUnitAccess)));

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

finalization
{$IFDEF TARGET_x86}
  if (ciMMX in CPUFeatures) then FreeAlphaTable;
{$ENDIF}

end.



