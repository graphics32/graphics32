unit GR32.Blend.Pascal;

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

{$I GR32.inc}

uses
  GR32;

//------------------------------------------------------------------------------
//
//      PUREPASCAL blend implementations
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Blend
//------------------------------------------------------------------------------
function BlendReg_Pas(F, B: TColor32): TColor32;
procedure BlendMem_Pas(F: TColor32; var B: TColor32);
procedure BlendMems_Pas(F: TColor32; B: PColor32; Count: Integer);
function BlendRegEx_Pas(F, B: TColor32; M: Cardinal): TColor32;
procedure BlendMemEx_Pas(F: TColor32; var B: TColor32; M: Cardinal);
function BlendRegRGB_Pas(F, B: TColor32; W: Cardinal): TColor32;
procedure BlendMemRGB_Pas(F: TColor32; var B: TColor32; W: Cardinal);
procedure BlendLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
procedure BlendLine_Pas(Src, Dst: PColor32; Count: Integer);
procedure BlendLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: Cardinal);


//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
function MergeReg_Pas(F, B: TColor32): TColor32;
function MergeRegEx_Pas(F, B: TColor32; M: Cardinal): TColor32;
procedure MergeMem_Pas(F: TColor32; var B: TColor32);
procedure MergeMemEx_Pas(F: TColor32; var B: TColor32; M: Cardinal);
procedure MergeLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
procedure MergeLine_Pas(Src, Dst: PColor32; Count: Integer);
procedure MergeLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: Cardinal);


//------------------------------------------------------------------------------
// Combine
//------------------------------------------------------------------------------
function CombineReg_Pas(X, Y: TColor32; W: Cardinal): TColor32;

procedure CombineMem_Pas_Table(X: TColor32; var Y: TColor32; W: Cardinal);
procedure CombineMem_Pas_Div255(X: TColor32; var Y: TColor32; W: Cardinal);
procedure CombineMem_Pas_Retro(X: TColor32; var Y: TColor32; W: Cardinal);

procedure CombineLine_Pas(Src, Dst: PColor32; Count: Integer; W: Cardinal);


//------------------------------------------------------------------------------
// Color algebra
//------------------------------------------------------------------------------
function ColorAdd_Pas(C1, C2: TColor32): TColor32;
function ColorSub_Pas(C1, C2: TColor32): TColor32;
function ColorDiv_Pas(C1, C2: TColor32): TColor32;
function ColorModulate_Pas(C1, C2: TColor32): TColor32;
function ColorMax_Pas(C1, C2: TColor32): TColor32;
function ColorMin_Pas(C1, C2: TColor32): TColor32;
function ColorDifference_Pas(C1, C2: TColor32): TColor32;
function ColorExclusion_Pas(C1, C2: TColor32): TColor32;
function ColorAverage_Pas(C1, C2: TColor32): TColor32;
function ColorScale_Pas(C: TColor32; W: Cardinal): TColor32;
function ColorScreen_Pas(B, S: TColor32): TColor32;
function ColorDodge_Pas(B, S: TColor32): TColor32;
function ColorBurn_Pas(B, S: TColor32): TColor32;


//------------------------------------------------------------------------------
// Blended color algebra
//------------------------------------------------------------------------------
function BlendColorAdd_Pas(C1, C2: TColor32): TColor32;
function BlendColorModulate_Pas(C1, C2: TColor32): TColor32;


//------------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------------
function LightenReg_Pas(C: TColor32; Amount: Integer): TColor32;


//------------------------------------------------------------------------------
// EMMS
//------------------------------------------------------------------------------
procedure EMMS_Pas;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_Blend,
  GR32_Bindings,
  GR32_LowLevel,
  SysUtils;

//------------------------------------------------------------------------------
//
//      Blend
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// BlendReg
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// BlendMem
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// BlendMems
//------------------------------------------------------------------------------
procedure BlendMems_Pas(F: TColor32; B: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem(F, B^);
    Inc(B);
    Dec(Count);
  end;
end;


//------------------------------------------------------------------------------
// BlendRegEx
//------------------------------------------------------------------------------
function BlendRegEx_Pas(F, B: TColor32; M: Cardinal): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];
  M := Af[FX.A];

  if (M = 0) then
  begin
    Result := B;
    Exit;
  end;

  if (M = $FF) then
  begin
    Result := F;
    Exit;
  end;

  Af := @DivTable[M];
  Ab := @DivTable[255 - M];

  TColor32Entry(Result).R := Af[FX.R] + Ab[BX.R];
  TColor32Entry(Result).G := Af[FX.G] + Ab[BX.G];
  TColor32Entry(Result).B := Af[FX.B] + Ab[BX.B];
  TColor32Entry(Result).A := $FF;
end;


//------------------------------------------------------------------------------
// BlendMemEx
//------------------------------------------------------------------------------
procedure BlendMemEx_Pas(F: TColor32; var B: TColor32; M: Cardinal);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];
  M := Af[FX.A]; // M = (M / 255) * (FX.A / 255)

  if (M = 0) then
    Exit;

  if (M = $FF) then
  begin
    B := F;
    Exit;
  end;

  Af := @DivTable[M];
  Ab := @DivTable[255 - M];

  BX.R := Af[FX.R] + Ab[BX.R];
  BX.G := Af[FX.G] + Ab[BX.G];
  BX.B := Af[FX.B] + Ab[BX.B];
  BX.A := $FF;
end;


//------------------------------------------------------------------------------
// BlendRegRGB
//------------------------------------------------------------------------------
function BlendRegRGB_Pas(F, B: TColor32; W: Cardinal): TColor32;
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  WX: TColor32Entry absolute W;
  RX: TColor32Entry absolute Result;
begin
  if (W = 0) then
    Result := B
  else
  if (W = $FF) then
    Result := F
  else
  begin
    RX.R := (FX.R - BX.R) * WX.B div 255 + BX.R;
    RX.G := (FX.G - BX.G) * WX.G div 255 + BX.G;
    RX.B := (FX.B - BX.B) * WX.R div 255 + BX.B;
  end;
end;


//------------------------------------------------------------------------------
// BlendMemRGB
//------------------------------------------------------------------------------
procedure BlendMemRGB_Pas(F: TColor32; var B: TColor32; W: Cardinal);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  WX: TColor32Entry absolute W;
begin
  if (W = 0) then
    exit;

  if ((W and $FFFFFF) = $FFFFFF) then
    B := F
  else
  begin
    BX.R := (FX.R - BX.R) * WX.B div 255 + BX.R;
    BX.G := (FX.G - BX.G) * WX.G div 255 + BX.G;
    BX.B := (FX.B - BX.B) * WX.R div 255 + BX.B;
  end;
end;


//------------------------------------------------------------------------------
// BlendMemRGB
//------------------------------------------------------------------------------
procedure BlendLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem(Src, Dst^);
    Inc(Dst);
    Dec(Count);
  end;
end;


//------------------------------------------------------------------------------
// BlendLine
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// BlendLineEx
//------------------------------------------------------------------------------
procedure BlendLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: Cardinal);
begin
  if (M = 0) then
    exit;

  while Count > 0 do
  begin
    BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;


//------------------------------------------------------------------------------
// BlendLineEx
//------------------------------------------------------------------------------
function CombineReg_Pas(X, Y: TColor32; W: Cardinal): TColor32;
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


//------------------------------------------------------------------------------
//
//      Combine
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// CombineMem
//------------------------------------------------------------------------------
procedure CombineMem_Pas_Table(X: TColor32; var Y: TColor32; W: Cardinal);
(*
TestCombineMem:
Errors: 32.364 = 24,7 % (Limit: -1)
Differences: 129.456
Average difference: 0,00
Max difference: 1 (Limit: 1)
*)
var
  Xe: TColor32Entry absolute X;
  Ye: TColor32Entry absolute Y;
  Af, Ab: PByteArray;
begin
  if W = 0 then
    Exit;

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

//------------------------------------------------------------------------------

procedure CombineMem_Pas_Div255(X: TColor32; var Y: TColor32; W: Cardinal);
(*
Contributed by: Anders Melander

TestCombineMem:
Errors: 56.170 (42,8 %)
Differences: 95.152
Average difference: -1,00
Max error:1
*)
var
  Xe: TColor32Entry absolute X;
  Ye: TColor32Entry absolute Y;
begin
  if W = 0 then
    Exit;

  if W >= $FF then
  begin
    Y := X;
    Exit;
  end;

  //
  // Magic number division using:
  //
  //   a*b/255 = (a * b * $8081) shr 23
  //
  // Applied to:
  //
  //   Result := W * (X - Y) + Y
  //

  // The Div255 function already uses the above method so
  // we can just use that directly:
  Ye.A := Div255(SmallInt(W) * (Xe.A - Ye.A)) + Ye.A;
  Ye.B := Div255(SmallInt(W) * (Xe.B - Ye.B)) + Ye.B;
  Ye.G := Div255(SmallInt(W) * (Xe.G - Ye.G)) + Ye.G;
  Ye.R := Div255(SmallInt(W) * (Xe.R - Ye.R)) + Ye.R;
end;

//------------------------------------------------------------------------------

procedure CombineMem_Pas_Retro(X: TColor32; var Y: TColor32; W: Cardinal);
(*
Contributed by: Anders Melander

Uses the "Double-blend" technique.
Much faster than CombineMem_Pas_Table but not as precise.

TestCombineMem:
Errors: 55.769 (42,5 %)
Differences: 95.884
Average difference: -1,00
Max error:1
*)
const
  MaskAG = $FF00FF00;
  MaskRB = $00FF00FF;
  FixedOne = 1 shl 8; // 1.0 in 1:8 fixed point = base 256
var
  FixedWeight: Word;
  Xag, Yag: TColor32;
  Xrb, Yrb: TColor32;
  ag, rb: TColor32;
begin
  if W = 0 then
    Exit;

  if W >= $FF then
  begin
    Y := X;
    Exit;
  end;


  // [0..255] -> [0..256]
  // FixedWeight := Round(W * FixedOne / 255);
  FixedWeight := Div255Round(W * FixedOne);

  Xag := (X and MaskAG) shr 8;
  Yag := (Y and MaskAG) shr 8;
  Xrb := (X and MaskRB);
  Yrb := (Y and MaskRB);

  // Unsigned multiplication of signed value. Works out because of 2's complement. No worries.
  ag := Cardinal(Integer(Xag - Yag) * FixedWeight) shr 8;
  rb := Cardinal(Integer(Xrb - Yrb) * FixedWeight) shr 8;

  ag := ((ag + Yag) shl 8) and MaskAG;
  rb := ((rb + Yrb)      ) and MaskRB;

  Y := (ag or rb);
end;


//------------------------------------------------------------------------------
// CombineLine
//------------------------------------------------------------------------------
procedure CombineLine_Pas(Src, Dst: PColor32; Count: Integer; W: Cardinal);
begin
  if W = 0 then
    Exit;

  if W >= $FF then
  begin
    MoveLongword(Src^, Dst^, Count);
    Exit;
  end;

  while Count > 0 do
  begin
    CombineMem(Src^, Dst^, W);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;


//------------------------------------------------------------------------------
//
//      Merge
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MergeReg
//------------------------------------------------------------------------------
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
    Rx.A := not DivTable[Fa xor 255, Ba xor 255]; // "xor 255" is faster than "not" for the indices because the asm is shorter
    Wa := RcTable[Rx.A, Fa];
    Fw := @DivTable[Wa];
    Bw := @DivTable[Wa xor $FF];
    Rx.R := Fw[Fx.R] + Bw[Bx.R];
    Rx.G := Fw[Fx.G] + Bw[Bx.G];
    Rx.B := Fw[Fx.B] + Bw[Bx.B];
  end;
end;


//------------------------------------------------------------------------------
// MergeRegEx
//------------------------------------------------------------------------------
function MergeRegEx_Pas(F, B: TColor32; M: Cardinal): TColor32;
begin
  Result := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;


//------------------------------------------------------------------------------
// MergeMem
//------------------------------------------------------------------------------
procedure MergeMem_Pas(F: TColor32; var B: TColor32);
begin
  B := MergeReg(F, B);
end;


//------------------------------------------------------------------------------
// MergeMemEx
//------------------------------------------------------------------------------
procedure MergeMemEx_Pas(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;


//------------------------------------------------------------------------------
// MergeLine1
//------------------------------------------------------------------------------
procedure MergeLine1_Pas(Src: TColor32; Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    Dst^ := MergeReg(Src, Dst^);
    Inc(Dst);
    Dec(Count);
  end;
end;


//------------------------------------------------------------------------------
// MergeLine
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// MergeLineEx
//------------------------------------------------------------------------------
procedure MergeLineEx_Pas(Src, Dst: PColor32; Count: Integer; M: Cardinal);
var
  PM: PByteArray;
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


//------------------------------------------------------------------------------
//
//      Color algebra
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// ColorAdd
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorSub
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorDiv
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorModulate
//------------------------------------------------------------------------------
function ColorModulate_Pas(C1, C2: TColor32): TColor32;
var
  C1e: TColor32Entry absolute C1;
  C2e: TColor32Entry absolute C2;
  Re: TColor32Entry absolute Result;
begin
  Re.A := (C2e.A * C1e.A + $80) shr 8;
  Re.R := (C2e.R * C1e.R + $80) shr 8;
  Re.G := (C2e.G * C1e.G + $80) shr 8;
  Re.B := (C2e.B * C1e.B + $80) shr 8;
end;


//------------------------------------------------------------------------------
// ColorMax
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorMin
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorDifference
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorDifference
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorAverage
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorScale
//------------------------------------------------------------------------------
function ColorScale_Pas(C: TColor32; W: Cardinal): TColor32;
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


//------------------------------------------------------------------------------
// ColorScreen
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorDodge
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ColorBurn
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      Blended color algebra
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// BlendColorAdd
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// BlendColorModulate
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      Misc.
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// EMMS
//------------------------------------------------------------------------------
procedure EMMS_Pas;
begin
  // Dummy
end;


//------------------------------------------------------------------------------
// LightenReg
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindingFunctions;
begin
  // pure pascal
  BlendRegistry.Add(FID_MERGEREG,       @MergeReg_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEMEM,       @MergeMem_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEMEMEX,     @MergeMemEx_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGEREGEX,     @MergeRegEx_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINE,      @MergeLine_Pas,         BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINEEX,    @MergeLineEx_Pas,       BlendBindingFlagPascal);
  BlendRegistry.Add(FID_MERGELINE1,     @MergeLine1_Pas,        BlendBindingFlagPascal);

  BlendRegistry.Add(FID_COMBINEREG,     @CombineReg_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COMBINEMEM,     @CombineMem_Pas_Retro,  BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COMBINELINE,    @CombineLine_Pas,       BlendBindingFlagPascal);

  BlendRegistry.Add(FID_BLENDREG,       @BlendReg_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEM,       @BlendMem_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMS,      @BlendMems_Pas,         BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINE,      @BlendLine_Pas,         BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDREGEX,     @BlendRegEx_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMEX,     @BlendMemEx_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINEEX,    @BlendLineEx_Pas,       BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDLINE1,     @BlendLine1_Pas,        BlendBindingFlagPascal);

  BlendRegistry.Add(FID_COLORDIV,       @ColorDiv_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORAVERAGE,   @ColorAverage_Pas,      BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMAX,       @ColorMax_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMIN,       @ColorMin_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORADD,       @ColorAdd_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSUB,       @ColorSub_Pas,          BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORMODULATE,  @ColorModulate_Pas,     BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORDIFFERENCE, @ColorDifference_Pas,  BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLOREXCLUSION, @ColorExclusion_Pas,    BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSCALE,     @ColorScale_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORSCREEN,    @ColorScreen_Pas,       BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORDODGE,     @ColorDodge_Pas,        BlendBindingFlagPascal);
  BlendRegistry.Add(FID_COLORBURN,      @ColorBurn_Pas,         BlendBindingFlagPascal);

  BlendRegistry.Add(FID_BLENDCOLORADD,  @BlendColorAdd_Pas,     BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDCOLORMODULATE, @BlendColorModulate_Pas, BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDREGRGB,    @BlendRegRGB_Pas,       BlendBindingFlagPascal);
  BlendRegistry.Add(FID_BLENDMEMRGB,    @BlendMemRGB_Pas,       BlendBindingFlagPascal);

  BlendRegistry.Add(FID_EMMS,           @EMMS_Pas,              BlendBindingFlagPascal);
  BlendRegistry.Add(FID_LIGHTEN,        @LightenReg_Pas,        BlendBindingFlagPascal);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  BlendColorAdd := BlendColorAdd_Pas; // TODO : Why?
  RegisterBindingFunctions;
finalization
end.
