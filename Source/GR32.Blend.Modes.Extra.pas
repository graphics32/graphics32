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
// TGraphics32BlenderClear
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
  TGraphics32BlenderClear = class(TCustomGraphics32Blender)
  protected
    function GetID: string; override;
    function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendClear = 'Clear';

resourcestring
  sBlendClear = 'Clear';


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
    function GetID: string; override;
    function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
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
  /// scaled by the source alpha channel and the average of the source color
  /// channels.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  TGraphics32BlenderAlpha = class(TCustomGraphics32Blender)
  protected
    function GetID: string; override;
    function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendAlpha = 'alpha';

resourcestring
  sBlendAlpha = 'Alpha';


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
// TGraphics32BlenderClear
//
//------------------------------------------------------------------------------
function TGraphics32BlenderClear.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
begin
{$if defined(BLEND_USE_TABLES)}
  Result := (MulDiv255Table[bAlpha, 255-fAlpha] shl 24) or (bColor and $00FFFFFF);
{$else}
  Result := (Div255(bAlpha * (255-fAlpha)) shl 24) or (bColor and $00FFFFFF);
{$ifend}
end;

function TGraphics32BlenderClear.GetID: string;
begin
  Result := cBlendClear;
end;

function TGraphics32BlenderClear.GetName: string;
begin
  Result := sBlendClear;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderMask
//
//------------------------------------------------------------------------------
function TGraphics32BlenderMask.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
begin
{$if defined(BLEND_USE_TABLES)}
  Result := (MulDiv255Table[bAlpha, fAlpha] shl 24) or (bColor and $00FFFFFF);
{$else}
  Result := (Div255(bAlpha * fAlpha) shl 24) or (bColor and $00FFFFFF);
{$ifend}
end;

function TGraphics32BlenderMask.GetID: string;
begin
  Result := cBlendMask;
end;

function TGraphics32BlenderMask.GetName: string;
begin
  Result := sBlendMask;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderAlpha
//
//------------------------------------------------------------------------------
function TGraphics32BlenderAlpha.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
var
  Alpha: Cardinal;
begin
  if (fAlpha <> 0) and (bAlpha <> 0) then
  begin
    // Calculate average of f components
    Alpha := (((fColor shr 16) and $FF) + ((fColor shr 8) and $FF) + (fColor and $FF)) div 3;

    // Modulate average of f components with f alpha and b alpha
    if (fAlpha <> 255) or (bAlpha <> 255) then
{$if defined(BLEND_USE_TABLES)}
      Alpha := MulDiv255Table[MulDiv255Table[Alpha, fAlpha], bAlpha];
{$else}
      Alpha := Div255(Div255(Alpha * fAlpha) * bAlpha);
{$ifend}

    // Apply alpha to b
    Result := (Alpha shl 24) or (bColor and $00FFFFFF);
  end else
    Result := 0;
end;

function TGraphics32BlenderAlpha.GetID: string;
begin
  Result := cBlendAlpha;
end;

function TGraphics32BlenderAlpha.GetName: string;
begin
  Result := sBlendAlpha;
end;


//------------------------------------------------------------------------------

initialization
  Graphics32BlendService.Register(TGraphics32BlenderClear);
  Graphics32BlendService.Register(TGraphics32BlenderMask);
  Graphics32BlendService.Register(TGraphics32BlenderAlpha);
end.
