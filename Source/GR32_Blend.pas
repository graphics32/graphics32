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
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32,
  GR32_Bindings;

var
  MMX_ACTIVE: Boolean; // For backward compatibility use in asm functions.


//------------------------------------------------------------------------------
//
//      Alpha Composition
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Blend
//------------------------------------------------------------------------------
//
// The Blend operation mixes a foreground color (F) onto a background color (B),
// using the alpha channel value of F:
//
//   Result Z = Fa * (Fargb - Bargb) + Bargb
//            = Fa * Fargb + (1 - Fa) * Bargb
//
// The background color is assumed to be fully opaque (i.e. Ba=255).
// If the alpha of the background is to be taken into account, the Merge
// operation should be used instead.
//
// The blend operation is commonly just referred to as "alpha blending".
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
//
// The Merge operation is based on a formula described in a paper by Bruce
// Wallace in 1981.
//
// Merging is associative, that is, A over (B over C) = (A over B) over C.
//
// The formula is,
//
//      Ra = Fa + Ba * (1 - Fa)
//      Rc = (Fa * (Fc - Bc * Ba) + Bc * Ba) / Ra
//
//    where
//
//      Rc is the resultant color,
//      Ra is the resultant alpha,
//      Fc is the foreground color,
//      Fa is the foreground alpha,
//      Bc is the background color,
//      Ba is the background alpha.
//
//    Implementation:
//
//      Ra := 1 - (1 - Fa) * (1 - Ba);
//      Wa := Fa / Ra;
//      Rc := Bc + Wa * (Fc - Bc);
//
//      (1 - Fa) * (1 - Ba) = 1 - Fa - Ba + Fa * Ba = (1 - Ra)
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Combine
//------------------------------------------------------------------------------
//
// The Combine operation performs linear interpolation, commonly refered to as
// "Lerp", between two colors (X and Y), given a weight (W):
//
//   Result Z = W * X + (1 - W) * Y
//            = W * (X - Y) + Y
//
// All channels are combined, including the alpha channel.
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//
//      Function Prototypes
//
//------------------------------------------------------------------------------
type
//------------------------------------------------------------------------------
// Blend & Merge
//------------------------------------------------------------------------------
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendMems   = procedure(F: TColor32; B: PColor32; Count: Integer);
  TBlendRegEx  = function(F, B: TColor32; M: Cardinal): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: Cardinal);
  TBlendRegRGB = function(F, B: TColor32; W: Cardinal): TColor32;
  TBlendMemRGB = procedure(F: TColor32; var B: TColor32; W: Cardinal);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  TBlendMemRGB128 = procedure(F: TColor32; var B: TColor32; W: UInt64);
{$ENDIF}
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: Cardinal);
  TBlendLine1  = TBlendMems deprecated 'Use TBlendMems';
//------------------------------------------------------------------------------
// Combine
//------------------------------------------------------------------------------
  TCombineReg  = function(X, Y: TColor32; W: Cardinal): TColor32;
  TCombineMem  = procedure(X: TColor32; var Y: TColor32; W: Cardinal);
  TCombineLine = procedure(Src, Dst: PColor32; Count: Integer; W: Cardinal);
//------------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------------
  TLightenReg  = function(C: TColor32; Amount: Integer): TColor32;
  TScaleMems = procedure(Dst: PColor32; Count: Integer; Weight: Cardinal);


//------------------------------------------------------------------------------
//
//      Function variables (i.e. delegates)
//
//------------------------------------------------------------------------------
var
//------------------------------------------------------------------------------
// Blend
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
  MergeReg: TBlendReg;
  MergeMem: TBlendMem;
  MergeMems: TBlendMems;

  MergeRegEx: TBlendRegEx;
  MergeMemEx: TBlendMemEx;

  MergeLine: TBlendLine;
  MergeLineEx: TBlendLineEx;

//------------------------------------------------------------------------------
// Combine
//------------------------------------------------------------------------------
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;
  CombineLine: TCombineLine;

//------------------------------------------------------------------------------
// Color algebra
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Blended color algebra
//------------------------------------------------------------------------------
  BlendColorAdd: TBlendReg;
  BlendColorModulate: TBlendReg;

//------------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------------
  LightenReg: TLightenReg;
  Lighten: TLightenReg absolute LightenReg; // Lighten is an alias for LigthenReg
  ScaleMems: TScaleMems;

//------------------------------------------------------------------------------
// EMMS
//------------------------------------------------------------------------------
{$IFNDEF OMIT_MMX}
  EMMS: procedure;
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Access to alpha composite functions corresponding to a combine mode
//
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      Function bindings
//
//------------------------------------------------------------------------------
function BlendRegistry: TFunctionRegistry;

const
  FID_EMMS              = 0;
  FID_MERGEREG          = 1;
  FID_MERGEMEM          = 2;
  FID_MERGELINE         = 3;
  FID_MERGEMEMS         = 4;
  FID_MERGELINE1        = FID_MERGEMEMS deprecated;
  FID_MERGEREGEX        = 5;
  FID_MERGEMEMEX        = 6;
  FID_MERGELINEEX       = 7;
  FID_COMBINEREG        = 8;
  FID_COMBINEMEM        = 9;
  FID_COMBINELINE       = 10;

  FID_BLENDREG          = 11;
  FID_BLENDMEM          = 12;
  FID_BLENDMEMS         = 13;
  FID_BLENDLINE         = 14;
  FID_BLENDREGEX        = 15;
  FID_BLENDMEMEX        = 16;
  FID_BLENDLINEEX       = 17;
  FID_BLENDLINE1        = 18 deprecated;

  FID_COLORMAX          = 19;
  FID_COLORMIN          = 20;
  FID_COLORAVERAGE      = 21;
  FID_COLORADD          = 22;
  FID_COLORSUB          = 23;
  FID_COLORDIV          = 24;
  FID_COLORMODULATE     = 25;
  FID_COLORDIFFERENCE   = 26;
  FID_COLOREXCLUSION    = 27;
  FID_COLORSCALE        = 28;
  FID_COLORSCREEN       = 29;
  FID_COLORDODGE        = 30;
  FID_COLORBURN         = 31;
  FID_BLENDCOLORADD     = 32;
  FID_BLENDCOLORMODULATE= 33;
  FID_LIGHTEN           = 34;

  FID_BLENDREGRGB       = 35;
  FID_BLENDMEMRGB       = 36;
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  FID_BLENDMEMRGB128    = 37;
{$ENDIF}

{$IFDEF OMIT_MMX}
procedure EMMS; {$IFDEF USEINLINING} inline; {$ENDIF}
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Blending related lookup tables
//
//------------------------------------------------------------------------------
type
  TLUT88 = array [Byte, Byte] of Byte;

var
  //
  // DivMul255Table contains the result of a *division* of two values, specified
  // in the byte range:
  //
  //   DivMul255Table[a, b] = Round(b * 255 / a)
  //
  // It is commonly used to perform the division of the merge operation:
  //
  //   ResultColor = DivMul255Table[ResultAlpha, Color] = Color / ResultAlpha
  //
  // or unpremultiplication:
  //
  //   Color = DivMul255Table[Alpha, PremultColor] = PremultColor / Alpha
  //
  DivMul255Table: TLUT88;

  //
  // MulDiv255Table contains the result of a *multiplication* of two values,
  // specified in the byte range:
  //
  //   MulDiv255Table[a, b] = Round(a * b / 255)
  //
  // It is commonly used to perform the multiplications of the blend or merge operations:
  //
  //   TempColor = MulDiv255Table[Color, Alpha] = Color * Alpha
  //
  // or premultiplication:
  //
  //   PremultColor = MulDiv255Table[Color, Alpha] = Color * Alpha
  //
  MulDiv255Table: TLUT88;


// RcTable and DivTable are the old names of the above two tables.
// They are kept for backward compatibility and should be considered deprecated.
var
  RcTable: TLUT88 absolute DivMul255Table;
  DivTable: TLUT88 absolute MulDiv255Table;


//------------------------------------------------------------------------------

type
  TMultEntry = array[0..3] of TColor32Entry;
  PMultEntry = ^TMultEntry;
  TMultTable = array[byte] of TMultEntry;
  PMultTable = ^TMultTable;

var
  //
  // alpha_ptr: Pointer to a 16-byte aligned array[256] of 4 cardinal values.
  // The table is used to implement vectorized division by 255 in SIMD functions:
  //
  //   (x div 255) = ((x + 128) shr 8)
  //               = ((alpha_ptr[x] + bias_ptr^) shr 8)
  //
  alpha_ptr: PMultTable;

  //
  // bias_ptr points to the middle entry of the alpha_ptr table.
  // The entry contains the 4 cardinal values 00000080 00000080 00000080 00000080.
  //
  bias_ptr: PMultEntry;


//------------------------------------------------------------------------------
//
//      Backward compatibility
//
//------------------------------------------------------------------------------
procedure MergeLine1(F: TColor32; B: PColor32; Count: Integer); {$IFDEF USEINLINING} inline; {$ENDIF} deprecated 'Use MergeMems';
procedure BlendLine1(F: TColor32; B: PColor32; Count: Integer); {$IFDEF USEINLINING} inline; {$ENDIF} deprecated 'Use BlendMems';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_System,
{$IFNDEF PUREPASCAL}
  GR32.Blend.Assembler,
{$IFNDEF OMIT_MMX}
  GR32.Blend.MMX,
{$ENDIF}
{$IFNDEF OMIT_SSE2}
  GR32.Blend.SSE2,
{$ENDIF}
{$ENDIF}
  GR32.Blend.Pascal;

{$IFDEF OMIT_MMX}
procedure EMMS;
begin
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//      GenAlphaTable
//
//------------------------------------------------------------------------------
var
  AlphaTable: Pointer;

procedure GenAlphaTable;
var
  i: Integer;
  Color: TColor32Entry;
begin
  GetMem(AlphaTable, SizeOf(TMultTable) + 16); // + 16 bytes for alignment

  // Align to 16 bytes
  alpha_ptr := pointer((NativeUInt(AlphaTable) + $F) and (not $F));

  // Values for division by 255: (x div 255) = ((alpha_ptr[x] + 128) shl 8)
  // Originally 2 entries per value for ASM and MMX.
  // Later expanded to 4 entries per value so the table can also be used with SSE.
  for i := Low(TMultTable) to High(TMultTable) do
  begin
    Color.Planes[0] := i;
    Color.Planes[1] := 0;
    Color.Planes[2] := i;
    Color.Planes[3] := 0;

    alpha_ptr[i][0] := Color;
    alpha_ptr[i][1] := Color;
    alpha_ptr[i][2] := Color;
    alpha_ptr[i][3] := Color;
  end;

  // bias_ptr points to ($80, 0, $80, 0)
  bias_ptr := @alpha_ptr[128][0];
end;

procedure FreeAlphaTable;
begin
  if (AlphaTable <> nil) then
    FreeMem(AlphaTable);
end;


//------------------------------------------------------------------------------
//
//      MakeMergeTables
//
//------------------------------------------------------------------------------
procedure MakeMergeTables;
var
  i, j: Integer;
begin
  for j := 0 to 255 do
  begin
    MulDiv255Table[0, j] := 0;
    DivMul255Table[0, j] := 0;

    for i := 1 to 255 do
    begin
      MulDiv255Table[i, j] := Round(i * j * COne255th);
      if i > j then
        DivMul255Table[i, j] := Round(j * 255 / i)
      else
        DivMul255Table[i, j] := 255;
    end;
  end;
end;


//------------------------------------------------------------------------------
//
//      Backward compatibility
//
//------------------------------------------------------------------------------
procedure MergeLine1(F: TColor32; B: PColor32; Count: Integer);
begin
  MergeMems(F, B, Count);
end;

procedure BlendLine1(F: TColor32; B: PColor32; Count: Integer);
begin
  BlendMems(F, B, Count);
end;


//------------------------------------------------------------------------------
//
//      Function bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
{$IFNDEF OMIT_MMX}
  BlendRegistry.RegisterBinding(FID_EMMS,               @@EMMS);
{$ENDIF}
  BlendRegistry.RegisterBinding(FID_BLENDREG,           @@BlendReg);
  BlendRegistry.RegisterBinding(FID_BLENDMEM,           @@BlendMem);
  BlendRegistry.RegisterBinding(FID_BLENDMEMS,          @@BlendMems);
  BlendRegistry.RegisterBinding(FID_BLENDLINE,          @@BlendLine);
  BlendRegistry.RegisterBinding(FID_BLENDREGEX,         @@BlendRegEx);
  BlendRegistry.RegisterBinding(FID_BLENDMEMEX,         @@BlendMemEx);
  BlendRegistry.RegisterBinding(FID_BLENDLINEEX,        @@BlendLineEx);
  BlendRegistry.RegisterBinding(FID_BLENDREGRGB,        @@BlendRegRGB);
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB,        @@BlendMemRGB);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB128,     @@BlendMemRGB128);
{$ENDIF}

  BlendRegistry.RegisterBinding(FID_MERGEREG,           @@MergeReg);
  BlendRegistry.RegisterBinding(FID_MERGEMEM,           @@MergeMem);
  BlendRegistry.RegisterBinding(FID_MERGEMEMS,          @@MergeMems);
  BlendRegistry.RegisterBinding(FID_MERGELINE,          @@MergeLine);
  BlendRegistry.RegisterBinding(FID_MERGEREGEX,         @@MergeRegEx);
  BlendRegistry.RegisterBinding(FID_MERGEMEMEX,         @@MergeMemEx);
  BlendRegistry.RegisterBinding(FID_MERGELINEEX,        @@MergeLineEx);

  BlendRegistry.RegisterBinding(FID_COMBINEREG,         @@CombineReg);
  BlendRegistry.RegisterBinding(FID_COMBINEMEM,         @@CombineMem);
  BlendRegistry.RegisterBinding(FID_COMBINELINE,        @@CombineLine);

  BlendRegistry.RegisterBinding(FID_COLORMAX,           @@ColorMax);
  BlendRegistry.RegisterBinding(FID_COLORMIN,           @@ColorMin);
  BlendRegistry.RegisterBinding(FID_COLORAVERAGE,       @@ColorAverage);
  BlendRegistry.RegisterBinding(FID_COLORADD,           @@ColorAdd);
  BlendRegistry.RegisterBinding(FID_COLORSUB,           @@ColorSub);
  BlendRegistry.RegisterBinding(FID_COLORDIV,           @@ColorDiv);
  BlendRegistry.RegisterBinding(FID_COLORMODULATE,      @@ColorModulate);
  BlendRegistry.RegisterBinding(FID_COLORDIFFERENCE,    @@ColorDifference);
  BlendRegistry.RegisterBinding(FID_COLOREXCLUSION,     @@ColorExclusion);
  BlendRegistry.RegisterBinding(FID_COLORSCALE,         @@ColorScale);
  BlendRegistry.RegisterBinding(FID_COLORSCREEN,        @@ColorScreen);
  BlendRegistry.RegisterBinding(FID_COLORDODGE,         @@ColorDodge);
  BlendRegistry.RegisterBinding(FID_COLORBURN,          @@ColorBurn);

  BlendRegistry.RegisterBinding(FID_BLENDCOLORADD,      @@BlendColorAdd);
  BlendRegistry.RegisterBinding(FID_BLENDCOLORMODULATE, @@BlendColorModulate);

  BlendRegistry.RegisterBinding(FID_LIGHTEN,            @@LightenReg);
  BlendRegistry.RegisterBinding(@@ScaleMems);
end;

var
  FBlendRegistry: TFunctionRegistry = nil;

function BlendRegistry: TFunctionRegistry;
begin
  if (FBlendRegistry = nil) then
  begin
    FBlendRegistry := NewRegistry('GR32_Blend bindings');
    RegisterBindings;
  end;
  Result := FBlendRegistry;
end;

//------------------------------------------------------------------------------

initialization
  BlendRegistry.RebindAll;

  MakeMergeTables;

  AlphaTable := nil;
  MMX_ACTIVE := (isMMX in CPU.InstructionSupport);
  if [isMMX, isSSE2] * CPU.InstructionSupport <> [] then
    GenAlphaTable;

finalization
  FreeAlphaTable;

end.
