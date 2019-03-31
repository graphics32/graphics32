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
  GR32, GR32_Bindings, SysUtils;

var
  MMX_ACTIVE: Boolean;

type
{ Function Prototypes }
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendMems   = procedure(F: TColor32; B: PColor32; Count: Integer);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendRegRGB = function(F, B, W: TColor32): TColor32;
  TBlendMemRGB = procedure(F: TColor32; var B: TColor32; W: TColor32);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  TBlendMemRGB128 = procedure(F: TColor32; var B: TColor32; W: UInt64);
{$ENDIF}
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);
  TBlendLine1  = procedure(Src: TColor32; Dst: PColor32; Count: Integer);
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
  BlendMems: TBlendMems;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendRegRGB: TBlendRegRGB;
  BlendMemRGB: TBlendMemRGB;
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendMemRGB128: TBlendMemRGB128;
{$ENDIF}

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
  BlendLine1: TBlendLine1;

  CombineReg: TCombineReg;
  CombineMem: TCombineMem;
  CombineLine: TCombineLine;

  MergeReg: TBlendReg;
  MergeMem: TBlendMem;

  MergeRegEx: TBlendRegEx;
  MergeMemEx: TBlendMemEx;

  MergeLine: TBlendLine;
  MergeLineEx: TBlendLineEx;
  MergeLine1: TBlendLine1;

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
  ColorScreen: TBlendReg;
  ColorDodge: TBlendReg;
  ColorBurn: TBlendReg;

{ Blended color algebra functions }
  BlendColorAdd: TBlendReg;
  BlendColorModulate: TBlendReg;

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

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;

implementation

uses
  GR32_LowLevel,
  GR32_System;

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
    A := $FF;
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
    A := $FF;
  end;
end;

procedure BlendMems_Pas(F: TColor32; B: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem(F, B^);
    Inc(B);
    Dec(Count);
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
    A := Af[FX.A] + Ab[A];
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
    A := Af[FX.A] + Ab[A];
  end;
end;

function BlendRegRGB_Pas(F, B, W: TColor32): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  WX: TColor32Entry absolute W;
  RX: TColor32Entry absolute Result;
begin
  RX.R := (FX.R - BX.R) * WX.B div 255 + BX.R;
  RX.G := (FX.G - BX.G) * WX.G div 255 + BX.G;
  RX.B := (FX.B - BX.B) * WX.R div 255 + BX.B;
end;

procedure BlendMemRGB_Pas(F: TColor32; var B: TColor32; W: TColor32);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  WX: TColor32Entry absolute W;
begin
  BX.R := (FX.R - BX.R) * WX.B div 255 + BX.R;
  BX.G := (FX.G - BX.G) * WX.G div 255 + BX.G;
  BX.B := (FX.B - BX.B) * WX.R div 255 + BX.B;
end;

procedure BlendLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem(Src, Dst^);
    Inc(Dst);
    Dec(Count);
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
    A := Ab[Ye.A] + Af[A];
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
    A := Ab[Ye.A] + Af[A];
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
    Bw := @DivTable[Wa xor $FF];
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

procedure MergeLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    Dst^ := MergeReg(Src, Dst^);
    Inc(Dst);
    Dec(Count);
  end;
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
  // Dummy
end;

function LightenReg_Pas(C: TColor32; Amount: Integer): TColor32;
var
  r, g, b: Integer;
  CX: TColor32Entry absolute C;
  RX: TColor32Entry absolute Result;
begin
  r := CX.R;
  g := CX.G;
  b := CX.B;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  // preserve alpha
  RX.A := CX.A;
  RX.R := r;
  RX.G := g;
  RX.B := b;
end;

{ Color algebra }

function ColorAdd_Pas(C1, C2: TColor32): TColor32;
var
  Xe: TColor32Entry absolute C1;
  Ye: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
begin
  R.A := Clamp(Xe.A + Ye.A, 255);
  R.R := Clamp(Xe.R + Ye.R, 255);
  R.G := Clamp(Xe.G + Ye.G, 255);
  R.B := Clamp(Xe.B + Ye.B, 255);
end;

function ColorSub_Pas(C1, C2: TColor32): TColor32;
var
  Xe: TColor32Entry absolute C1;
  Ye: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
  Temp: SmallInt;
begin
  Temp := Xe.A - Ye.A;
  if Temp < 0 then
    R.A := 0
  else
    R.A := Temp;
  Temp := Xe.R - Ye.R;
  if Temp < 0 then
    R.R := 0
  else
    R.R := Temp;
  Temp := Xe.G - Ye.G;
  if Temp < 0 then
    R.G := 0
  else
    R.G := Temp;
  Temp := Xe.B - Ye.B;
  if Temp < 0 then
    R.B := 0
  else
    R.B := Temp;
end;

function ColorDiv_Pas(C1, C2: TColor32): TColor32;
var
  C1e: TColor32Entry absolute C1;
  C2e: TColor32Entry absolute C2;
  Re: TColor32Entry absolute Result;
  Temp: Word;
begin
  if C1e.A = 0 then
    Re.A := $FF
  else
  begin
    Temp := (C2e.A shl 8) div C1e.A;
    if Temp > $FF then
      Re.A := $FF
    else
      Re.A := Temp;
  end;

  if C1e.R = 0 then
    Re.R := $FF
  else
  begin
    Temp := (C2e.R shl 8) div C1e.R;
    if Temp > $FF then
      Re.R := $FF
    else
      Re.R := Temp;
  end;

  if C1e.G = 0 then
    Re.G := $FF
  else
  begin
    Temp := (C2e.G shl 8) div C1e.G;
    if Temp > $FF then
      Re.G := $FF
    else
      Re.G := Temp;
  end;

  if C1e.B = 0 then
    Re.B := $FF
  else
  begin
    Temp := (C2e.B shl 8) div C1e.B;
    if Temp > $FF then
      Re.B := $FF
    else
      Re.B := Temp;
  end;
end;

function ColorModulate_Pas(C1, C2: TColor32): TColor32;
var
  C1e: TColor32Entry absolute C2;
  C2e: TColor32Entry absolute C2;
  Re: TColor32Entry absolute Result;
begin
  Re.A := (C2e.A * C1e.A + $80) shr 8;
  Re.R := (C2e.R * C1e.R + $80) shr 8;
  Re.G := (C2e.G * C1e.G + $80) shr 8;
  Re.B := (C2e.B * C1e.B + $80) shr 8;
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
  Xe: TColor32Entry absolute C1;
  Ye: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
begin
  R.A := Abs(Xe.A - Ye.A);
  R.R := Abs(Xe.R - Ye.R);
  R.G := Abs(Xe.G - Ye.G);
  R.B := Abs(Xe.B - Ye.B);
end;

function ColorExclusion_Pas(C1, C2: TColor32): TColor32;
var
  Xe: TColor32Entry absolute C1;
  Ye: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
begin
  R.A := Xe.A + Ye.A - ((Xe.A * Ye.A) shl 7);
  R.R := Xe.R + Ye.R - ((Xe.R * Ye.R) shr 7);
  R.G := Xe.G + Ye.G - ((Xe.G * Ye.G) shr 7);
  R.B := Xe.B + Ye.B - ((Xe.B * Ye.B) shr 7);
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
  Ce: TColor32Entry absolute C;
var
  r1, g1, b1, a1: Cardinal;
begin
  a1 := Ce.A * W shr 8;
  r1 := Ce.R * W shr 8;
  g1 := Ce.G * W shr 8;
  b1 := Ce.B * W shr 8;

  if a1 > 255 then a1 := 255;
  if r1 > 255 then r1 := 255;
  if g1 > 255 then g1 := 255;
  if b1 > 255 then b1 := 255;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function ColorScreen_Pas(B, S: TColor32): TColor32;
var
  Be: TColor32Entry absolute B;
  Se: TColor32Entry absolute S;
  R: TColor32Entry absolute Result;
begin
  R.A := Be.A + Se.A - (Be.A * Se.A) div 255;
  R.R := Be.R + Se.R - (Be.R * Se.R) div 255;
  R.G := Be.G + Se.G - (Be.G * Se.G) div 255;
  R.B := Be.B + Se.B - (Be.B * Se.B) div 255;
end;

function ColorDodge_Pas(B, S: TColor32): TColor32;

  function Dodge(B, S: Byte): Byte;
  begin
    if B = 0 then
      Result := 0
    else
    if S = 255 then
      Result := 255
    else
      Result := Clamp((255 * B) div (255 - S), 255);
  end;

var
  Be: TColor32Entry absolute B;
  Se: TColor32Entry absolute S;
  R: TColor32Entry absolute Result;
begin
  R.A := Dodge(Be.A, Se.A);
  R.R := Dodge(Be.R, Se.R);
  R.G := Dodge(Be.G, Se.G);
  R.B := Dodge(Be.B, Se.B);
end;

function ColorBurn_Pas(B, S: TColor32): TColor32;

  function Burn(B, S: Byte): Byte;
  begin
    if B = 255 then
      Result := 255
    else
    if S = 0 then
      Result := 0
    else
      Result := 255 - Clamp(255 * (255 - B) div S, 255);
  end;

var
  Be: TColor32Entry absolute B;
  Se: TColor32Entry absolute S;
  R: TColor32Entry absolute Result;
begin
  R.A := Burn(Be.A, Se.A);
  R.R := Burn(Be.R, Se.R);
  R.G := Burn(Be.G, Se.G);
  R.B := Burn(Be.B, Se.B);
end;


{ Blended color algebra }

function BlendColorAdd_Pas(C1, C2: TColor32): TColor32;
var
  Xe: TColor32Entry absolute C1;
  Ye: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[Xe.A];
  Ab := @DivTable[not Xe.A];
  R.A := Af[Clamp(Xe.A + Ye.A, 255)] + Ab[Ye.A];
  R.R := Af[Clamp(Xe.R + Ye.R, 255)] + Ab[Ye.R];
  R.G := Af[Clamp(Xe.G + Ye.G, 255)] + Ab[Ye.G];
  R.B := Af[Clamp(Xe.B + Ye.B, 255)] + Ab[Ye.B];
end;

function BlendColorModulate_Pas(C1, C2: TColor32): TColor32;
var
  C1e: TColor32Entry absolute C1;
  C2e: TColor32Entry absolute C2;
  R: TColor32Entry absolute Result;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[C1e.A];
  Ab := @DivTable[not C1e.A];
  R.A := Af[(C2e.A * C1e.A + $80) shr 8] + Ab[C2e.A];
  R.R := Af[(C2e.R * C1e.R + $80) shr 8] + Ab[C2e.R];
  R.G := Af[(C2e.G * C1e.G + $80) shr 8] + Ab[C2e.G];
  R.B := Af[(C2e.B * C1e.B + $80) shr 8] + Ab[C2e.B];
end;

{$IFNDEF PUREPASCAL}

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
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
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
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr 00 Qb
        ADD     EBX,bias
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
        MOV     EAX,[ESI]
        TEST    EAX,$FF000000
        JZ      @3

        PUSH    ECX

        MOV     ECX,EAX
        SHR     ECX,24

        CMP     ECX,$FF
        JZ      @2

        MOV     EBX,EAX
        AND     EAX,$00FF00FF
        AND     EBX,$FF00FF00
        IMUL    EAX,ECX
        SHR     EBX,8
        IMUL    EBX,ECX
        ADD     EAX,bias
        AND     EAX,$FF00FF00
        SHR     EAX,8
        ADD     EBX,bias
        AND     EBX,$FF00FF00
        OR      EAX,EBX

        MOV     EDX,[EDI]
        XOR     ECX,$000000FF
        MOV     EBX,EDX
        AND     EDX,$00FF00FF
        AND     EBX,$FF00FF00
        IMUL    EDX,ECX
        SHR     EBX,8
        IMUL    EBX,ECX
        ADD     EDX,bias
        AND     EDX,$FF00FF00
        SHR     EDX,8
        ADD     EBX,bias
        AND     EBX,$FF00FF00
        OR      EBX,EDX

        ADD     EAX,EBX
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

  // W = 1 - W
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
        MOV     EBX,EDX         // EBX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr 00 Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,EDX         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

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
        AND     ECX,$0000FF00   // ECX  <-  00 00 Fg 00
        IMUL    EAX,R8D         // EAX  <-  Pr ** Pb **
        SHR     ECX,8           // ECX  <-  00 00 00 Fg
        IMUL    ECX,R8D         // ECX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     ECX,bias
        AND     ECX,$0000FF00   // ECX  <-  00 00 Pg 00
        OR      EAX,ECX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
  // Q = W * B
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
        INC     ECX             // 255:256 range bias
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        ADD     ECX,bias
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
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W;
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
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
        ADD     R8D,bias
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
        SHR     ECX,8           // ECX  <-  00 Pr 00 Pb
        ADD     EAX,bias
        AND     EAX,$0000FF00   // EAX  <-  00 00 Pg 00
        OR      ECX,EAX         // ECX  <-  00 Pr Pg Pb

  // W = 1 - W
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8
  // Q = W * B
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
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
  // Q = W * B
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

  // W = 1 - W
        MOV     R9D,[RDX]
        XOR     R8D,$000000FF   // R8D  <-  1 - R8D
  // Q = W * B
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

procedure EMMS_ASM; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
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
{$ENDIF}


{ SSE2 versions }

{$IFNDEF OMIT_SSE2}

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
  begin
    DivTable[0, J] := 0;
    RcTable[0, J] := 0;
  end;
  for J := 0 to 255 do
    for I := 1 to 255 do
    begin
      DivTable[I, J] := Round(I * J * OneByteth);
      RcTable[I, J] := Round(J * 255 / I)
    end;
end;

const
  FID_EMMS = 0;
  FID_MERGEREG = 1;
  FID_MERGEMEM = 2;
  FID_MERGELINE = 3;
  FID_MERGELINE1 = 4;
  FID_MERGEREGEX = 5;
  FID_MERGEMEMEX = 6;
  FID_MERGELINEEX = 7;
  FID_COMBINEREG = 8;
  FID_COMBINEMEM = 9;
  FID_COMBINELINE = 10;

  FID_BLENDREG = 11;
  FID_BLENDMEM = 12;
  FID_BLENDMEMS = 13;
  FID_BLENDLINE = 14;
  FID_BLENDREGEX = 15;
  FID_BLENDMEMEX = 16;
  FID_BLENDLINEEX = 17;
  FID_BLENDLINE1 = 18;

  FID_COLORMAX = 19;
  FID_COLORMIN = 20;
  FID_COLORAVERAGE = 21;
  FID_COLORADD = 22;
  FID_COLORSUB = 23;
  FID_COLORDIV = 24;
  FID_COLORMODULATE = 25;
  FID_COLORDIFFERENCE = 26;
  FID_COLOREXCLUSION = 27;
  FID_COLORSCALE = 28;
  FID_COLORSCREEN = 29;
  FID_COLORDODGE = 30;
  FID_COLORBURN = 31;
  FID_BLENDCOLORADD = 32;
  FID_BLENDCOLORMODULATE = 33;
  FID_LIGHTEN = 34;

  FID_BLENDREGRGB = 35;
  FID_BLENDMEMRGB = 36;
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  FID_BLENDMEMRGB128 = 37;
{$ENDIF}

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
  BlendRegistry.RegisterBinding(FID_BLENDMEMS, @@BlendMems);
  BlendRegistry.RegisterBinding(FID_BLENDLINE, @@BlendLine);
  BlendRegistry.RegisterBinding(FID_BLENDREGEX, @@BlendRegEx);
  BlendRegistry.RegisterBinding(FID_BLENDMEMEX, @@BlendMemEx);
  BlendRegistry.RegisterBinding(FID_BLENDLINEEX, @@BlendLineEx);
  BlendRegistry.RegisterBinding(FID_BLENDLINE1, @@BlendLine1);

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
  BlendRegistry.RegisterBinding(FID_COLORSCREEN, @@ColorScreen);
  BlendRegistry.RegisterBinding(FID_COLORDODGE, @@ColorDodge);
  BlendRegistry.RegisterBinding(FID_COLORBURN, @@ColorBurn);

  BlendRegistry.RegisterBinding(FID_BLENDCOLORADD, @@BlendColorAdd);
  BlendRegistry.RegisterBinding(FID_BLENDCOLORMODULATE, @@BlendColorModulate);

  BlendRegistry.RegisterBinding(FID_LIGHTEN, @@LightenReg);
  BlendRegistry.RegisterBinding(FID_BLENDREGRGB, @@BlendRegRGB);
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB, @@BlendMemRGB);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB128, @@BlendMemRGB128);
{$ENDIF}

  // pure pascal
  BlendRegistry.Add(FID_EMMS, @EMMS_Pas);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_Pas);
  BlendRegistry.Add(FID_MERGEMEM, @MergeMem_Pas);
  BlendRegistry.Add(FID_MERGEMEMEX, @MergeMemEx_Pas);
  BlendRegistry.Add(FID_MERGEREGEX, @MergeRegEx_Pas);
  BlendRegistry.Add(FID_MERGELINE, @MergeLine_Pas);
  BlendRegistry.Add(FID_MERGELINEEX, @MergeLineEx_Pas);
  BlendRegistry.Add(FID_MERGELINE1, @MergeLine1_Pas);
  BlendRegistry.Add(FID_COLORDIV, @ColorDiv_Pas);
  BlendRegistry.Add(FID_COLORAVERAGE, @ColorAverage_Pas);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_Pas);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_Pas);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_Pas);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_Pas);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_Pas);
  BlendRegistry.Add(FID_BLENDMEMS, @BlendMems_Pas);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_Pas);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_Pas);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_Pas);
  BlendRegistry.Add(FID_BLENDLINEEX, @BlendLineEx_Pas);
  BlendRegistry.Add(FID_BLENDLINE1, @BlendLine1_Pas);
  BlendRegistry.Add(FID_COLORMAX, @ColorMax_Pas);
  BlendRegistry.Add(FID_COLORMIN, @ColorMin_Pas);
  BlendRegistry.Add(FID_COLORADD, @ColorAdd_Pas);
  BlendRegistry.Add(FID_COLORSUB, @ColorSub_Pas);
  BlendRegistry.Add(FID_COLORMODULATE, @ColorModulate_Pas);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_Pas);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_Pas);
  BlendRegistry.Add(FID_COLORSCALE, @ColorScale_Pas);
  BlendRegistry.Add(FID_COLORSCREEN, @ColorScreen_Pas);
  BlendRegistry.Add(FID_COLORDODGE, @ColorDodge_Pas);
  BlendRegistry.Add(FID_COLORBURN, @ColorBurn_Pas);
  BlendRegistry.Add(FID_BLENDCOLORADD, @BlendColorAdd_Pas);
  BlendRegistry.Add(FID_BLENDCOLORMODULATE, @BlendColorModulate_Pas);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_Pas);
  BlendRegistry.Add(FID_BLENDREGRGB, @BlendRegRGB_Pas);
  BlendRegistry.Add(FID_BLENDMEMRGB, @BlendMemRGB_Pas);

{$IFNDEF PUREPASCAL}
  BlendRegistry.Add(FID_EMMS, @EMMS_ASM, []);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_ASM, []);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_ASM, []);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_ASM, []);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_ASM, []);
  BlendRegistry.Add(FID_BLENDMEMS, @BlendMems_ASM, []);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_ASM, []);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_ASM, []);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_ASM, []);
  BlendRegistry.Add(FID_BLENDLINE1, @BlendLine1_ASM, []);
{$IFNDEF TARGET_x64}
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_ASM, []);
{$ENDIF}
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
  BlendRegistry.Add(FID_BLENDREGRGB, @BlendRegRGB_MMX, [ciMMX]);
  BlendRegistry.Add(FID_BLENDMEMRGB, @BlendMemRGB_MMX, [ciMMX]);
{$ENDIF}
{$IFNDEF OMIT_SSE2}
  BlendRegistry.Add(FID_EMMS, @EMMS_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDMEMS, @BlendMems_SSE2, [ciSSE2]);
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
  BlendRegistry.Add(FID_BLENDREGRGB, @BlendRegRGB_SSE2, [ciSSE2]);
  BlendRegistry.Add(FID_BLENDMEMRGB, @BlendMemRGB_SSE2, [ciSSE2]);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendRegistry.Add(FID_BLENDMEMRGB128, @BlendMemRGB128_SSE4, [ciSSE2]);
{$ENDIF}
{$ENDIF}
{$ENDIF}

  BlendRegistry.RebindAll;
end;

initialization
  BlendColorAdd := BlendColorAdd_Pas;

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
  if [ciMMX, ciSSE2] * CPUFeatures <> [] then
    FreeAlphaTable;
{$ENDIF}

end.
