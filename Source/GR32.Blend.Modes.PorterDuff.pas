unit GR32.Blend.Modes.PorterDuff;

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
 * The Original Code is Porter-Duff Blend Modes for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$define BLEND_USE_TABLES}

uses
  Classes,
  GR32,
  GR32.Blend.Modes;

//------------------------------------------------------------------------------
//
//      The 12 Porter-Duff composition operators
//
//------------------------------------------------------------------------------
//
// Key          Name
// ------------------------------
// Src          Source
// SrcOver      Source Over
// SrcIn        Source In
// SrcOut       Source Out
// SrcAtop      Source Atop
// Dest         Destination
// DestOver     Destination Over
// DestIn       Destination In
// DestOut      Destination Out
// DestAtop     Destination Atop
// Clear        Clear
// Xor          Xor
//
//------------------------------------------------------------------------------
//
// References:
//
// - Thomas Porter & Tom Duff
//   "Compositing digital images"
//   Proceedings of the 11th annual conference on Computer graphics and
//   interactive techniques.
//   SIGGRAPH July 1984. Vol. 18, No 3. pp. 253-259.
//
// - https://ssp.impulsetrain.com/porterduff.html
// - https://www.w3.org/TR/compositing-1/#porterduffcompositingoperators
// - https://dev.w3.org/SVG/modules/compositing/master/Overview.html
// - https://developer.android.com/reference/android/graphics/PorterDuff.Mode
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrc
//
//------------------------------------------------------------------------------
type
  TGraphics32BlenderSrc = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendSrc = 'Src';

resourcestring
  sBlendSrc = 'Source';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcOver
//
//------------------------------------------------------------------------------
// The source is composited over the destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderSrcOver = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
    procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); override;
  end;

const
  cBlendSrcOver = 'SrcOver';

resourcestring
  sBlendSrcOver = 'Source Over';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcIn
//
//------------------------------------------------------------------------------
// The part of the source lying inside of the destination replaces the
// destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderSrcIn = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendSrcIn = 'SrcIn';

resourcestring
  sBlendSrcIn = 'Source In';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcAtop
//
//------------------------------------------------------------------------------
// The part of the source lying inside of the destination is composited onto the
// destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderSrcAtop = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendSrcAtop = 'SrcAtop';

resourcestring
  sBlendSrcAtop = 'Source Atop';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcOut
//
//------------------------------------------------------------------------------
// The part of the source lying outside of the destination replaces the destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderSrcOut = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendSrcOut = 'SrcOut';

resourcestring
  sBlendSrcOut = 'Source Out';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDest
//
//------------------------------------------------------------------------------
// The destination is left untouched.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderDest = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
    procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); override;
  end;

const
  cBlendDest = 'Dest';

resourcestring
  sBlendDest = 'Destination';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestOver
//
//------------------------------------------------------------------------------
// The destination is composited over the source and the result replaces the
// destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderDestOver = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
    procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); override;
  end;

const
  cBlendDestOver = 'DestOver';

resourcestring
  sBlendDestOver = 'Destination Over';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestIn
//
//------------------------------------------------------------------------------
// The part of the destination lying inside of the source replaces the
// destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderDestIn = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendDestIn = 'DestIn';

resourcestring
  sBlendDestIn = 'Destination In';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestAtop
//
//------------------------------------------------------------------------------
// The part of the destination lying inside of the source is composited over the
// source and replaces the destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderDestAtop = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendDestAtop = 'DestAtop';

resourcestring
  sBlendDestAtop = 'Destination Atop';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestOut
//
//------------------------------------------------------------------------------
// The part of the destination lying outside of the source replaces the
// destination.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderDestOut = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendDestOut = 'Out';

resourcestring
  sBlendDestOut = 'Destination Out';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderClear
//
//------------------------------------------------------------------------------
// The destination is cleared. Neither the source nor the destination are used
// as input.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderClear = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendClear = 'Clear';

resourcestring
  sBlendClear = 'Clear';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderXor
//
//------------------------------------------------------------------------------
// The part of the source that lies outside of the destination is combined with
// the part of the destination that lies outside of the source.
//------------------------------------------------------------------------------
type
  TGraphics32BlenderXor = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendXor = 'Xor';

resourcestring
  sBlendXor = 'Xor';


//------------------------------------------------------------------------------
//
// Blend mode group
//
//------------------------------------------------------------------------------
const
  sPorterDuffBlendGroup = 'Porter-Duff';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_Blend,
  GR32_LowLevel;

const
  OneOver255: Single = 1 / 255;
  OneOver255x255: Single = 1 / (255 * 255);

//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrc
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSrc.Blend(F, B: TColor32): TColor32;
begin
  (*
  ** The source is copied to the destination. The destination is not used as input.
  **
  **    Result.A        = F.A
  **    Result.RGB      = F.RGB
  *)
  Result := F;
end;

class function TGraphics32BlenderSrc.GetID: string;
begin
  Result := cBlendSrc;
end;

class function TGraphics32BlenderSrc.GetName: string;
begin
  Result := sBlendSrc;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcOver
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSrcOver.Blend(F, B: TColor32): TColor32;
begin
  (*
  ** The source is composited over the destination.
  **
  **    Result.A        = F.A + B.A * (1 - F.A)
  **    Result.RGB      = (F.RGB * F.A + B.RGB * B.A * (1 - F.A)) / Result.A
  *)
  Result := MergeReg(F, B);
end;

procedure TGraphics32BlenderSrcOver.Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
  MergeMemEx(F, B, M);
end;

class function TGraphics32BlenderSrcOver.GetID: string;
begin
  Result := cBlendSrcOver;
end;

class function TGraphics32BlenderSrcOver.GetName: string;
begin
  Result := sBlendSrcOver;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcIn
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSrcIn.Blend(F, B: TColor32): TColor32;
var
  Alpha: Cardinal;
begin
  (*
  ** The part of the source lying inside of the destination replaces the destination.
  **
  **    Result.A        = F.A * B.A
  **    Result.RGB      = F.RGB
  *)
{$if defined(BLEND_USE_TABLES)}
  Alpha := MulDiv255Table[TColor32Entry(F).A, TColor32Entry(B).A];
{$else}
  Alpha := Round(TColor32Entry(F).A * TColor32Entry(B).A * OneOver255);
{$ifend}

  if (Alpha = 0) then
    Result := 0
  else
    Result := (F and $00FFFFFF) or (Alpha shl 24);
end;

class function TGraphics32BlenderSrcIn.GetID: string;
begin
  Result := cBlendSrcIn;
end;

class function TGraphics32BlenderSrcIn.GetName: string;
begin
  Result := sBlendSrcIn;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcAtop
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSrcAtop.Blend(F, B: TColor32): TColor32;
var
  AlphaSource, AlphaDest: Cardinal;
begin
  (*
  ** The part of the source lying inside of the destination is composited onto the destination.
  **
  **    Result.A        = B.A
  **    Result.RGB      = F.A * F.RGB + (1 - F.A) * B.RGB
  *)
  AlphaDest := TColor32Entry(B).A;
  if (AlphaDest = 0) then
    Exit(0);
  AlphaSource := TColor32Entry(F).A;
  Result := CombineReg(F, B, AlphaSource); // Lerp
  TColor32Entry(Result).A := AlphaDest;
end;

class function TGraphics32BlenderSrcAtop.GetID: string;
begin
  Result := cBlendSrcAtop;
end;

class function TGraphics32BlenderSrcAtop.GetName: string;
begin
  Result := sBlendSrcAtop;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSrcOut
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSrcOut.Blend(F, B: TColor32): TColor32;
var
  Alpha: Cardinal;
begin
  (*
  ** The part of the source lying outside of the destination replaces the destination.
  **
  **    Result.A        = F.A * (1 - B.A)
  **    Result.RGB      = F.RGB
  *)
{$if defined(BLEND_USE_TABLES)}
  Alpha := MulDiv255Table[TColor32Entry(F).A, 255 - TColor32Entry(B).A];
{$else}
  Alpha := Round(TColor32Entry(F).A * (255 - TColor32Entry(B).A) * OneOver255);
{$ifend}

  if (Alpha = 0) then
    Result := 0
  else
    Result := (F and $00FFFFFF) or (Alpha shl 24);
end;

class function TGraphics32BlenderSrcOut.GetID: string;
begin
  Result := cBlendSrcOut;
end;

class function TGraphics32BlenderSrcOut.GetName: string;
begin
  Result := sBlendSrcOut;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDest
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDest.Blend(F, B: TColor32): TColor32;
begin
  (*
  ** The destination is left untouched.
  **
  **    Result.A        = B.A
  **    Result.RGB      = B.RGB
  *)
  Result := B;
end;

procedure TGraphics32BlenderDest.Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
end;

class function TGraphics32BlenderDest.GetID: string;
begin
  Result := cBlendDest;
end;

class function TGraphics32BlenderDest.GetName: string;
begin
  Result := sBlendDest;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestOver
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDestOver.Blend(F, B: TColor32): TColor32;
begin
  (*
  ** The destination is composited over the source and the result replaces the destination.
  **
  **    Result.A        = B.A + F.A * (1 - B.A)
  **    Result.RGB      = (B.RGB * B.A + F.RGB * F.A * (1 - B.A)) / Result.A
  *)
  Result := MergeReg(B, F); // Note that F & B has been swapped
end;

procedure TGraphics32BlenderDestOver.Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
  // TODO : Is this correct?
  // With MergeRegEx(B, F, M) MasterAlpha will be applied to B, while usually it is applied to F.
  // Hence we reverse MasterAlpha with 255-M. I haven't verified that this is correct.

  B := MergeRegEx(B, F, 255-M); // Note that F & B has been swapped
end;

class function TGraphics32BlenderDestOver.GetID: string;
begin
  Result := cBlendDestOver;
end;

class function TGraphics32BlenderDestOver.GetName: string;
begin
  Result := sBlendDestOver;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestIn
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDestIn.Blend(F, B: TColor32): TColor32;
var
  Alpha: Cardinal;
begin
  (*
  ** The part of the destination lying inside of the source replaces the destination.
  **
  **    Result.A        = B.A * F.A
  **    Result.RGB      = B.RGB
  *)
{$if defined(BLEND_USE_TABLES)}
  Alpha := MulDiv255Table[TColor32Entry(B).A, TColor32Entry(F).A];
{$else}
  Alpha := Round(TColor32Entry(B).A * TColor32Entry(F).A * OneOver255);
{$ifend}

  if (Alpha = 0) then
    Result := 0
  else
    Result := (B and $00FFFFFF) or (Alpha shl 24);
end;

class function TGraphics32BlenderDestIn.GetID: string;
begin
  Result := cBlendDestIn;
end;

class function TGraphics32BlenderDestIn.GetName: string;
begin
  Result := sBlendDestIn;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestAtop
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDestAtop.Blend(F, B: TColor32): TColor32;
var
  AlphaSource, AlphaDest: Cardinal;
begin
  (*
  ** The part of the destination lying inside of the source is composited over the source and replaces the destination.
  **
  **    Result.A        = F.A
  **    Result.RGB      = B.A * B.RGB + (1 - B.A) * F.RGB
  *)
  AlphaSource := TColor32Entry(F).A;
  if (AlphaSource = 0) then
    Exit(0);
  AlphaDest := TColor32Entry(B).A;
  Result := CombineReg(B, F, AlphaDest); // Lerp
  TColor32Entry(Result).A := AlphaSource;
end;

class function TGraphics32BlenderDestAtop.GetID: string;
begin
  Result := cBlendDestAtop;
end;

class function TGraphics32BlenderDestAtop.GetName: string;
begin
  Result := sBlendDestAtop;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDestOut
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDestOut.Blend(F, B: TColor32): TColor32;
var
  Alpha: Cardinal;
begin
  (*
  ** The part of the destination lying outside of the source replaces the destination.
  **
  **    Result.A        = B.A * (1 - F.A)
  **    Result.RGB      = B.RGB
  *)
{$if defined(BLEND_USE_TABLES)}
  Alpha := MulDiv255Table[TColor32Entry(B).A, 255 - TColor32Entry(F).A];
{$else}
  Alpha := Round(TColor32Entry(B).A * (255 - TColor32Entry(F).A) * OneOver255);
{$ifend}

  if (Alpha = 0) then
    Result := 0
  else
    Result := (B and $00FFFFFF) or (Alpha shl 24);
end;

class function TGraphics32BlenderDestOut.GetID: string;
begin
  Result := cBlendDestOut;
end;

class function TGraphics32BlenderDestOut.GetName: string;
begin
  Result := sBlendDestOut;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderClear
//
//------------------------------------------------------------------------------
function TGraphics32BlenderClear.Blend(F, B: TColor32): TColor32;
begin
  (*
  ** The destination is cleared. Neither the source nor the destination are used as input.
  **
  **    Result.A        = 0
  **    Result.RGB      = 0
  *)
  Result := 0;
end;

class function TGraphics32BlenderClear.GetID: string;
begin
  Result := cBlendClear;
end;

class function TGraphics32BlenderClear.GetName: string;
begin
  Result := sBlendClear;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderXor
//
//------------------------------------------------------------------------------
function TGraphics32BlenderXor.Blend(F, B: TColor32): TColor32;
var
  AlphaSource, AlphaDest: Cardinal;
  WeightSource, WeightDest: Cardinal;
  AlphaResult255: Cardinal;
begin
  (*
  ** The part of the source that lies outside of the destination is combined with the part of the destination that lies outside of the source.
  **
  **    Result.A        = F.A * (1 - B.A) + B.A * (1 - F.A)
  **    Result.RGB      = (F.RGB * F.A * (1 - B.A) + B.RGB * B.A * (1 - F.A)) / Result.A
  *)
  AlphaSource := TColor32Entry(F).A;
  AlphaDest := TColor32Entry(B).A;
  WeightSource := AlphaSource * (255 - AlphaDest);
  WeightDest := AlphaDest * (255 - AlphaSource);
  AlphaResult255 := WeightSource + WeightDest;
  if (AlphaResult255 = 0) then
    Exit(0);

  TColor32Entry(Result).R := (WeightSource * TColor32Entry(F).R + WeightDest * TColor32Entry(B).R) div AlphaResult255;
  TColor32Entry(Result).G := (WeightSource * TColor32Entry(F).G + WeightDest * TColor32Entry(B).G) div AlphaResult255;
  TColor32Entry(Result).B := (WeightSource * TColor32Entry(F).B + WeightDest * TColor32Entry(B).B) div AlphaResult255;
  TColor32Entry(Result).A := Div255(AlphaResult255);
end;

class function TGraphics32BlenderXor.GetID: string;
begin
  Result := cBlendXor;
end;

class function TGraphics32BlenderXor.GetName: string;
begin
  Result := sBlendXor;
end;


//------------------------------------------------------------------------------

initialization
  Graphics32BlendService.Register(TGraphics32BlenderSrc,        [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderSrcOver,    [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderSrcIn,      [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderSrcAtop,    [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderSrcOut,     [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderDest,       [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderDestOver,   [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderDestIn,     [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderDestAtop,   [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderDestOut,    [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderClear,      [sPorterDuffBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderXor,        [sPorterDuffBlendGroup]);
end.
