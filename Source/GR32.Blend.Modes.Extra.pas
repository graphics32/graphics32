unit GR32.Blend.Modes.Extra;

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
 * The Original Code is Extra Blend Modes for Graphics32
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

uses
  Classes,
  GR32,
  GR32.Blend.Modes;

//------------------------------------------------------------------------------
//
// TGraphics32BlenderErase
//
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color from the backdrop color with the alpha channel
  /// scaled by the inverse source alpha channel.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Only the alpha channel of the source color is used; The color channels are
  /// ignored.
  /// </remarks>
  TGraphics32BlenderErase = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendErase = 'erase';

resourcestring
  sBlendErase = 'Erase';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderMask
//
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color from the backdrop color with the alpha channel
  /// scaled by the source alpha channel.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Only the alpha channel of the source color is used; The color channels are
  /// ignored.
  /// </remarks>
  TGraphics32BlenderMask = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendMask = 'mask';

resourcestring
  sBlendMask = 'Mask';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderAlpha
//
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color from the backdrop color with the alpha channel
  /// scaled by the source alpha channel and the intensity of the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The source color intensity is calculated using the average of its color
  /// channels.
  /// </remarks>
  TGraphics32BlenderAlpha = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
  end;

const
  cBlendAlpha = 'alpha';

resourcestring
  sBlendAlpha = 'Alpha';


//------------------------------------------------------------------------------
//
// Blend mode group
//
//------------------------------------------------------------------------------
const
  sExtraBlendGroup = 'Extra';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_Blend,
  GR32_LowLevel;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderErase
//
//------------------------------------------------------------------------------
function TGraphics32BlenderErase.Blend(F: TColor32; B: TColor32): TColor32;
begin
{$if defined(BLEND_USE_TABLES)}
  Result := (MulDiv255Table[TColor32Entry(B).A, 255-TColor32Entry(F).A] shl 24) or (B and $00FFFFFF);
{$else}
  Result := (Div255(TColor32Entry(B).A * (255-TColor32Entry(F).A)) shl 24) or (B and $00FFFFFF);
{$ifend}
end;

class function TGraphics32BlenderErase.GetID: string;
begin
  Result := cBlendErase;
end;

class function TGraphics32BlenderErase.GetName: string;
begin
  Result := sBlendErase;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderMask
//
//------------------------------------------------------------------------------
function TGraphics32BlenderMask.Blend(F: TColor32; B: TColor32): TColor32;
begin
{$if defined(BLEND_USE_TABLES)}
  Result := (MulDiv255Table[TColor32Entry(B).A, TColor32Entry(F).A] shl 24) or (B and $00FFFFFF);
{$else}
  Result := (Div255(TColor32Entry(B).A * TColor32Entry(F).A) shl 24) or (B and $00FFFFFF);
{$ifend}
end;

class function TGraphics32BlenderMask.GetID: string;
begin
  Result := cBlendMask;
end;

class function TGraphics32BlenderMask.GetName: string;
begin
  Result := sBlendMask;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderAlpha
//
//------------------------------------------------------------------------------
function TGraphics32BlenderAlpha.Blend(F: TColor32; B: TColor32): TColor32;
var
  Intensity: Cardinal;
  ResultAlpha: Cardinal;
begin
  if (TColor32Entry(F).A <> 0) and (TColor32Entry(B).A <> 0) then
  begin
    // Calculate intensity (average) of foreground components
    Intensity := (TColor32Entry(F).R + TColor32Entry(F).G + TColor32Entry(F).B) div 3;

    // Modulate intensity with foreground and background alpha
{$if defined(BLEND_USE_TABLES)}
    ResultAlpha := MulDiv255Table[Intensity, TColor32Entry(F).A];
    ResultAlpha := MulDiv255Table[ResultAlpha, TColor32Entry(B).A];
{$else}
    ResultAlpha := Div255(Div255(Intensity * TColor32Entry(F).A) * TColor32Entry(B).A);
{$ifend}

    // Apply modulated alpha to background color
    Result := (ResultAlpha shl 24) or (B and $00FFFFFF);
  end else
    Result := B and $00FFFFFF;
end;

class function TGraphics32BlenderAlpha.GetID: string;
begin
  Result := cBlendAlpha;
end;

class function TGraphics32BlenderAlpha.GetName: string;
begin
  Result := sBlendAlpha;
end;


//------------------------------------------------------------------------------

initialization
  Graphics32BlendService.Register(TGraphics32BlenderErase,      [sExtraBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderMask,       [sExtraBlendGroup]);
  Graphics32BlendService.Register(TGraphics32BlenderAlpha,      [sExtraBlendGroup]);
end.
