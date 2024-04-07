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

{$I GR32.inc}

uses
  GR32,
  GR32_Bindings;

var
  MMX_ACTIVE: Boolean; // For backward compatibility use in asm functions.

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
  TBlendLine1  = procedure(Src: TColor32; Dst: PColor32; Count: Integer);
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
  BlendLine1: TBlendLine1;

//------------------------------------------------------------------------------
// Merge
//------------------------------------------------------------------------------
  MergeReg: TBlendReg;
  MergeMem: TBlendMem;

  MergeRegEx: TBlendRegEx;
  MergeMemEx: TBlendMemEx;

  MergeLine: TBlendLine;
  MergeLineEx: TBlendLineEx;
  MergeLine1: TBlendLine1;

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
  FID_MERGELINE1        = 4;
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
  FID_BLENDLINE1        = 18;

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

const
  BlendBindingFlagPascal = $0001;

{$IFDEF OMIT_MMX}
procedure EMMS; {$IFDEF USEINLINING} inline; {$ENDIF}
{$ENDIF}


//------------------------------------------------------------------------------
  //
//      Blending related lookup tables
//
//------------------------------------------------------------------------------
var
  //
  // RcTable contains the result of a *division* of two bytes values, specified in the byte range:
  //
  //   RcTable[a, b] = Round(b * 255 / a)
  //
  // It is commonly used to perform the division of the merge operation:
  //
  //   ResultColor = RcTable[ResultAlpha, Color] = Color / ResultAlpha
  //
  RcTable: array [Byte, Byte] of Byte;

  //
  // The, unfortunatebly named, DivTable contains the result of a *multiplication* of two bytes
  // values, specified in the byte range:
  //
  //   DivTable[a, b] = Round(a * b / 255)
  //
  // It is commonly used to perform the multiplications of the blend or merge operations:
  //
  //   TempColor = RcTable[Color, Alpha] = Color * Alpha
  //
  DivTable: array [Byte, Byte] of Byte;


//------------------------------------------------------------------------------

type
  TMultEntry = array[0..3] of TColor32Entry;
  PMultEntry = ^TMultEntry;
  TMultTable = array[byte] of TMultEntry;
  PMultTable = ^TMultTable;

var
  //
  // alpha_ptr: Pointer to a 16-byte aligned array[256] of 4 cardinal values.
  // The table is used to implement division by 255:
  //
  //   (x div 255) = ((x + 128) shr 8)
  //               = ((alpha_ptr[x] + bias_ptr^) shr 8)
  //
  alpha_ptr: PMultTable;

  //
  // bias_ptr points to the middle entry of the alpha_ptr table.
  // The entry contains the value 0080 0080.
  //
  bias_ptr: PMultEntry;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$IFNDEF PUREPASCAL}
  GR32_System,
{$ENDIF}
{$IFNDEF PUREPASCAL}
  GR32.Blend.&ASM,
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

{$IFNDEF PUREPASCAL}

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
{$ENDIF}


//------------------------------------------------------------------------------
//
//      MakeMergeTables
//
//------------------------------------------------------------------------------
procedure MakeMergeTables;
var
  i, j: Integer;
begin
  for i := 0 to 255 do
  begin
    DivTable[0, i] := 0; // Yes, [0,0] is set twice but who cares
    DivTable[i, 0] := 0;
    RcTable[0, i] := 0;
    RcTable[i, 0] := 0;
  end;

  for j := 1 to 255 do
    for i := 1 to 255 do
    begin
      DivTable[i, j] := Round(i * j * COne255th);
      if i > j then
        RcTable[i, j] := Round(j * 255 / i)
      else
        RcTable[i, j] := 255;
    end;
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
  BlendRegistry.RegisterBinding(FID_BLENDLINE1,         @@BlendLine1);
  BlendRegistry.RegisterBinding(FID_BLENDREGRGB,        @@BlendRegRGB);
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB,        @@BlendMemRGB);
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  BlendRegistry.RegisterBinding(FID_BLENDMEMRGB128,     @@BlendMemRGB128);
{$ENDIF}

  BlendRegistry.RegisterBinding(FID_MERGEREG,           @@MergeReg);
  BlendRegistry.RegisterBinding(FID_MERGEMEM,           @@MergeMem);
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

{$IFNDEF PUREPASCAL}
  AlphaTable := nil;
  MMX_ACTIVE := (isMMX in CPU.InstructionSupport);
  if [isMMX, isSSE2] * CPU.InstructionSupport <> [] then
    GenAlphaTable;
{$ELSE}
  MMX_ACTIVE := False;
{$ENDIF}

finalization
{$IFNDEF PUREPASCAL}
  FreeAlphaTable;
{$ENDIF}

end.
