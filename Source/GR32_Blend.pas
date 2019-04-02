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
 *  Christian-W. Budde
 *      - 2019/04/01 - Refactoring
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

type
  PBlendReg = ^TBlendReg;
  PBlendMem = ^TBlendMem;
  PBlendRegEx = ^TBlendRegEx;
  PBlendMemEx = ^TBlendMemEx;
  PBlendLine = ^TBlendLine;
  PBlendLineEx = ^TBlendLineEx;

  TBlendRegCombineModeArray = array[TCombineMode] of PBlendReg;
  TBlendMemCombineModeArray = array[TCombineMode] of PBlendMem;
  TBlendRegExCombineModeArray = array[TCombineMode] of PBlendRegEx;
  TBlendMemExCombineModeArray = array[TCombineMode] of PBlendMemEx;
  TBlendLineCombineModeArray = array[TCombineMode] of PBlendLine;
  TBlendLineExCombineModeArray = array[TCombineMode] of PBlendLineEx;

const
  BLEND_REG: TBlendRegCombineModeArray = ((@@BlendReg),(@@MergeReg));
  BLEND_MEM: TBlendMemCombineModeArray = ((@@BlendMem),(@@MergeMem));
  BLEND_REG_EX: TBlendRegExCombineModeArray = ((@@BlendRegEx),(@@MergeRegEx));
  BLEND_MEM_EX: TBlendMemExCombineModeArray = ((@@BlendMemEx),(@@MergeMemEx));
  BLEND_LINE: TBlendLineCombineModeArray = ((@@BlendLine),(@@MergeLine));
  BLEND_LINE_EX: TBlendLineExCombineModeArray = ((@@BlendLineEx),(@@MergeLineEx));

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
{$IFNDEF PUREPASCAL}
  GR32_BlendASM,
{$IFNDEF OMIT_MMX}
  GR32_BlendMMX,
{$ENDIF}
{$IFNDEF OMIT_SSE2}
  GR32_BlendSSE2,
{$ENDIF}
{$ENDIF}
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

  Af := @DivTable[FA];
  Ab := @DivTable[not FA];
  with BX do
  begin
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

  Af := @DivTable[FA];
  Ab := @DivTable[not FA];
  with BX do
  begin
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

  Ab := @DivTable[255 - M];
  with BX do
  begin
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
    A := $FF;
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

  Ab := @DivTable[255 - M];
  with BX do
  begin
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
    A := $FF;
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

  Af := @DivTable[W];
  Ab := @DivTable[255 - W];
  with Xe do
  begin
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

  Af := @DivTable[W];
  Ab := @DivTable[255 - W];
  with Xe do
  begin
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
{$ENDIF}

{ Misc stuff }

function Lighten(C: TColor32; Amount: Integer): TColor32;
begin
  Result := LightenReg(C, Amount);
end;

procedure MakeMergeTables;
var
  I, J: Integer;
begin
  for J := 0 to 255 do
  begin
    DivTable[0, J] := 0;
    RcTable[0, J] := 0;
  end;
  for J := 0 to 255 do
    for I := 1 to 255 do
    begin
      DivTable[I, J] := Round(I * J * COne255th);
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

const
  BlendBindingFlagPascal = $0001;


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
  BlendRegistry.Add(FID_EMMS, @EMMS_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEREG, @MergeReg_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEMEM, @MergeMem_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEMEMEX, @MergeMemEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEREGEX, @MergeRegEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINE, @MergeLine_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINEEX, @MergeLineEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINE1, @MergeLine1_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORDIV, @ColorDiv_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORAVERAGE, @ColorAverage_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COMBINEREG, @CombineReg_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COMBINEMEM, @CombineMem_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COMBINELINE, @CombineLine_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDREG, @BlendReg_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEM, @BlendMem_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMS, @BlendMems_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINE, @BlendLine_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDREGEX, @BlendRegEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMEX, @BlendMemEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINEEX, @BlendLineEx_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINE1, @BlendLine1_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMAX, @ColorMax_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMIN, @ColorMin_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORADD, @ColorAdd_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSUB, @ColorSub_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMODULATE, @ColorModulate_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSCALE, @ColorScale_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSCREEN, @ColorScreen_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORDODGE, @ColorDodge_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORBURN, @ColorBurn_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDCOLORADD, @BlendColorAdd_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDCOLORMODULATE, @BlendColorModulate_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_LIGHTEN, @LightenReg_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDREGRGB, @BlendRegRGB_Pas, [], BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMRGB, @BlendMemRGB_Pas, [], BlendBindingFlagPascal);

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
