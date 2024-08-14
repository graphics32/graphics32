unit GR32.Blend.Modes.PhotoShop;

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
 * The Original Code is Adobe Blend Modes for Graphics32
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
//      Various Adobe/PhotoShop blend modes
//
//------------------------------------------------------------------------------
//
// References:
//
// - https://en.wikipedia.org/wiki/Blend_modes
// - https://www.w3.org/TR/compositing-1/#blending
// - https://dev.w3.org/SVG/modules/compositing/master/Overview.html
// - https://developer.android.com/reference/android/graphics/BlendMode
// - https://giggster.com/guide/basics/overview-of-blend-modes/
// - https://dunnbypaul.net/blends/
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//
// TGraphics32BlenderMultiply
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Multiplies the backdrop and source color values.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The result color is always at least as dark as either of the two constituent
  /// colors. Multiplying any color with black produces black; multiplying with white
  /// leaves the original color unchanged. Painting successive overlapping objects
  /// with a color other than black or white produces progressively darker colors.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderMultiply = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendMultiply = 'multiply';

resourcestring
  sBlendMultiply = 'Multiply';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderScreen
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Multiplies the complements of the backdrop and source color values, then
  /// complements the result.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The result color is always at least as light as either of the two constituent
  /// colors. Screening any color with white produces white; screening with black
  /// leaves the original color unchanged. The effect is similar to projecting
  /// multiple photographic slides simultaneously onto a single screen.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderScreen = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendScreen = 'screen';

resourcestring
  sBlendScreen = 'Screen';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSoftLight
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Darkens or lightens the colors, depending on the source color value.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The effect is similar to shining a diffused spotlight on the backdrop.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderSoftLight = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendSoftLight = 'SoftLight';

resourcestring
  sBlendSoftLight = 'Soft Light';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHardLight
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Multiplies or screens the colors, depending on the source color value.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The effect is similar to shining a harsh spotlight on the backdrop.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderHardLight = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendHardLight = 'HardLight';

resourcestring
  sBlendHardLight = 'Hard Light';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderOverlay
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Multiplies or screens the colors, depending on the backdrop color value.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Source colors overlay the backdrop while preserving its highlights and
  /// shadows. The backdrop color is not replaced but is mixed with the source
  /// color to reflect the lightness or darkness of the backdrop.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderOverlay = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendOverlay = 'Overlay';

resourcestring
  sBlendOverlay = 'Overlay';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearLight
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Dodges (lightens) or Burns (darkens) the backdrop color depending on the
  /// source color value.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// If the source color is more than 50%, the image is dodged (lightened).
  /// If the source color is less than 50%, the image is burned (darkened).
  /// </remarks>
  /// <seealso cref="BlendLinearDodge" />
  /// <seealso cref="BlendLinearBurn" />
  {$ENDREGION}
  TGraphics32BlenderLinearLight = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendLinearLight = 'LinearLight';

resourcestring
  sBlendLinearLight = 'Linear Light';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderPinLight
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Replaces the backdrop color, depending on the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// If the source color is lighter than 50% gray, pixels darker than the source
  /// color are replaced, and pixels lighter than the source color do not change.
  /// If the source color is darker than 50% gray, pixels lighter than the source
  /// color are replaced, and pixels darker than the source color do not change.
  /// </remarks>
  {$ENDREGION}
  TGraphics32BlenderPinLight = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendPinLight = 'PinLight';

resourcestring
  sBlendPinLight = 'Pin Light';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHardMix
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Adds the red, green and blue channel values of the source color to the RGB
  /// values of the backdrop color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// If the resulting sum for a channel is 255 or greater, it receives a value
  /// of 255; if less than 255, a value of 0.
  /// Therefore, all blended pixels have red, green, and blue channel values of
  /// either 0 or 255. This changes all pixels to primary additive colors (red,
  /// green, or blue), white, or black.
  /// </remarks>
  {$ENDREGION}
  TGraphics32BlenderHardMix = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendHardMix = 'HardMix';

resourcestring
  sBlendHardMix = 'Hard Mix';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderVividLight
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Burns or dodges the colors by increasing or decreasing the contrast,
  /// depending on the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// If the source color is lighter than 50%, the image is lightened by
  /// decreasing the contrast (Burn).
  /// If the source color is darker than 50%, the image is darkened by
  /// increasing the contrast (Dodge).
  /// </remarks>
  {$ENDREGION}
  TGraphics32BlenderVividLight = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendVividLight = 'VividLight';

resourcestring
  sBlendVividLight = 'Vivid Light';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearDodge
//
//------------------------------------------------------------------------------
// Also known as "Add"
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Sums the backdrop and source colors.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Blending with white gives white. Blending with black does not change the image.
  /// </remarks>
  /// <seealso cref="BlendLinearBurn" />
  {$ENDREGION}
  TGraphics32BlenderLinearDodge = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendLinearDodge = 'Add';

resourcestring
  sBlendLinearDodge = 'Linear Dodge (Add)';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColorDodge
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Brightens the backdrop color to reflect the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Painting with black produces no changes.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderColorDodge = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendColorDodge = 'ColorDodge';

resourcestring
  sBlendColorDodge = 'Color Dodge';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColorBurn
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Darkens the backdrop color to reflect the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Painting with white produces no change.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderColorBurn = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendColorBurn = 'ColorBurn';

resourcestring
  sBlendColorBurn = 'Color Burn';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearBurn
//
//------------------------------------------------------------------------------
// Also known as "Subtract".
// The PS documentation has different definitions for Subtract and Linear Burn.
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Subtracts the source color from the backdrop color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Also known as the Subtract blend mode.
  /// </remarks>
  {$ENDREGION}
  TGraphics32BlenderLinearBurn = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendLinearBurn = 'LinearBurn';

resourcestring
  sBlendLinearBurn = 'Linear Burn';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDifference
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Subtracts the darker of the two constituent colors from the lighter color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Painting with white inverts the backdrop color; painting with black produces
  /// no change.
  /// </remarks>
  /// <seealso cref="BlendNegation" />
  {$ENDREGION}
  TGraphics32BlenderDifference = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendDifference = 'Difference';

resourcestring
  sBlendDifference = 'Difference';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderNegation
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Subtracts the darker of the two constituent colors from the lighter color,
  /// then complements the result.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Painting with black inverts the backdrop color; painting with white produces
  /// no change.
  /// </remarks>
  /// <seealso cref="BlendDifference" />
  {$ENDREGION}
  TGraphics32BlenderNegation = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendNegation = 'Negation';

resourcestring
  sBlendNegation = 'Negation';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderExclusion
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Creates an effect similar to but lower in contrast than the Difference mode.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Blending with white inverts the base color values. Blending with black
  /// produces no change.
  /// </remarks>
  {$ENDREGION}
  TGraphics32BlenderExclusion = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendExclusion = 'Exclusion';

resourcestring
  sBlendExclusion = 'Exclusion';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLighten
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Selects the lighter of the backdrop and source colors.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The backdrop is replaced with the source where the source is lighter;
  /// otherwise, it is left unchanged.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderLighten = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendLighten = 'Lighten';

resourcestring
  sBlendLighten = 'Lighten';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDarken
//
//------------------------------------------------------------------------------
type
  {$REGION 'Documentation'}
  /// <summary>
  /// Selects the darker of the backdrop and source colors.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// The backdrop is replaced with the source where the source is darker;
  /// otherwise, it is left unchanged.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderDarken = class(TGraphics32SeparableBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    class function BlendComponent(F, B: cardinal): cardinal; override;
  public
  end;

const
  cBlendDarken = 'Darken';

resourcestring
  sBlendDarken = 'Darken';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDarkerColor
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Compares the total of all channel values for the source and backdrop color
  /// and displays the lower value color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Darker Color does not produce a third color, which can result from the
  /// Darken blend, because it chooses the lowest channel values from both the
  /// backdrop and the source color to create the result color.
  /// </remarks>
  /// <seealso cref="" />
  {$ENDREGION}
  TGraphics32BlenderDarkerColor = class(TGraphics32ComponentBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendDarkerColor = 'Darker';

resourcestring
  sBlendDarkerColor = 'Darker color';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLighterColor
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Compares the total of all channel values for the source and backdrop color
  /// and displays the higher value color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Lighter Color does not produce a third color, which can result from the
  /// Lighten blend, because it chooses the highest channel values from both the
  /// backdrop and source color to create the result color.
  /// </remarks>
  /// <seealso cref="" />
  TGraphics32BlenderLighterColor = class(TGraphics32ComponentBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendLighterColor = 'Lighter';

resourcestring
  sBlendLighterColor = 'Lighter color';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHue
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
// See:
// - https://en.wikipedia.org/wiki/Blend_modes#Hue,_saturation_and_luminosity
// - https://en.wikipedia.org/wiki/HSL_and_HSV
//------------------------------------------------------------------------------
{.$define BLENDER_HUE_CACHE}
type
  /// <summary>
  /// Creates a result color with the luminance and saturation of the backdrop
  /// color and the hue of the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  TGraphics32BlenderHue = class(TGraphics32ComponentBlender)
  private
{$ifdef BLENDER_HUE_CACHE}
    FCacheH, FCacheS, FCacheL: Byte;
    FCacheF, FCacheB, FCacheFB: TColor32;
{$endif}
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendHue = 'Hue';

resourcestring
  sBlendHue = 'Hue';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSaturation
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color with the luminance and hue of the backdrop color
  /// color and the hue of the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// Painting with this mode in an area with no (zero) saturation (i.e. gray)
  /// causes no change.
  /// </remarks>
  TGraphics32BlenderSaturation = class(TGraphics32ComponentBlender)
  private
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendSaturation = 'Saturation';

resourcestring
  sBlendSaturation = 'Saturation';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLuminance
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color with the hue and saturation of the backdrop color
  /// and the luminance of the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// This mode creates the inverse effect of the Color blend mode.
  /// </remarks>
  TGraphics32BlenderLuminance = class(TGraphics32ComponentBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendLuminosity = 'Luminosity'; // Should be "Luminance" but PS incorrectly refers to luminance as luminosity.

resourcestring
  sBlendLuminance = 'Luminosity';


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColor
//
//------------------------------------------------------------------------------
// TODO: Uses incorrect color space
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Creates a result color with the luminance of the backdrop color and the
  /// hue and saturation of the source color.
  /// </summary>
  /// <param name="F">The source/foreground color value.</param>
  /// <param name="B">The backdrop/background color value.</param>
  /// <remarks>
  /// This preserves the gray levels in the image and is useful for coloring
  /// monochrome images and for tinting color images.
  /// </remarks>
  TGraphics32BlenderColor = class(TGraphics32ComponentBlender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
    function BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
  public
  end;

const
  cBlendColor = 'Color';

resourcestring
  sBlendColor = 'Color';


//------------------------------------------------------------------------------
//
// Blend mode groups
//
//------------------------------------------------------------------------------
const
  sPhotoShopBlendGroupNormal = 'Normal';
  sPhotoShopBlendGroupDarken = 'Darken';
  sPhotoShopBlendGroupLighten = 'Lighten';
  sPhotoShopBlendGroupContrast = 'Contrast';
  sPhotoShopBlendGroupInversion = 'Inversion';
  sPhotoShopBlendGroupComponent = 'Component';

const
  PhotoShopBlendGroups: TArray<string> =
    [sPhotoShopBlendGroupNormal, sPhotoShopBlendGroupDarken, sPhotoShopBlendGroupLighten, sPhotoShopBlendGroupContrast,
     sPhotoShopBlendGroupInversion, sPhotoShopBlendGroupComponent];


// -----------------------------------------------------------------------------
//
//      Lightness/Luminance
//
// -----------------------------------------------------------------------------
// Note: PhotoShop incorrectly refers to Luminance as Luminosity.
// -----------------------------------------------------------------------------
// Chroma = Lightness = ½ × (max(R,G,B) + min(R,G,B))
function ColorLightness(Color: TColor32): integer; inline; overload;
function ColorLightness(R, G, B: Byte): integer; inline; overload;

// "Luma" = Luminance = 0.21 × R + 0.72 × G + 0.07 × B
function ColorLuminance(Color: TColor32): integer; inline; overload;
function ColorLuminance(R, G, B: Byte): integer; inline; overload;

// Average = Intensity = (R + G + B) ÷ 3
function ColorAverage(Color: TColor32): integer; inline;

// Brightness = Value = Max(R, G, B)
function ColorBrightness(Color: TColor32): integer; inline; overload;
function ColorBrightness(R, G, B: Byte): integer; inline; overload;


// -----------------------------------------------------------------------------
//
//      RGB to Luminance factors
//
// -----------------------------------------------------------------------------
// Define LUMINANCE_PHOTOSHOP to use PhotoShop's definition of luminance
{.$define LUMINANCE_PHOTOSHOP}

{$if defined(LUMINANCE_PHOTOSHOP)}
  {$define LUMINANCE_REC601_NTSC}
{$else}
  {$define LUMINANCE_REC709}
{$ifend}

const
{$if defined(LUMINANCE_REC709)}
  // Rec. 709 (also used by Gimp)
  // Y = 0.21 × R + 0.72 × G + 0.07 × B
  LuminanceMultR = 54;
  LuminanceMultG = 184;
  LuminanceMultB = 18;
{$elseif defined(LUMINANCE_REC601_NTSC)}
  // Rec. 601 NTSC
  // Y = 0.3 × R + 0.59 × G + 0.11 × B
  LuminanceMultR = 77;
  LuminanceMultG = 151;
  LuminanceMultB = 28;
{$else}
  // Graphics32 legacy
  // Y = 0.239 × R + 0.682 × G + 0.082 × B
  LuminanceMultR = 61;
  LuminanceMultG = 174;
  LuminanceMultB = 21;
{$ifend}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_Blend,
  GR32_LowLevel;


// -----------------------------------------------------------------------------
//
//      Lightness/Luminance
//
// -----------------------------------------------------------------------------
// Lightness = ½ × (max(R,G,B) + min(R,G,B))
function ColorLightness(Color: TColor32): integer; inline;
var
  R, G, B: byte;
begin
  B := (Color and $FF);
  G := (Color shr 8) and $FF;
  R := (Color shr 16) and $FF;
  Result := ColorLightness(R, G, B);
end;

function ColorLightness(R, G, B: byte): integer; inline;
begin
  Result := (Max(Max(R, G), B) + Min(Min(R, G), B)) div 2;
end;

// -----------------------------------------------------------------------------
// Luminance = 0.21 × R + 0.72 × G + 0.07 × B (something like it)
function ColorLuminance(Color: TColor32): integer; inline;
begin
  Result := (
    (Color and $00FF0000) shr 16 * LuminanceMultR +
    (Color and $0000FF00) shr 8 * LuminanceMultG +
    (Color and $000000FF) * LuminanceMultB
    ) shr 8;
end;

function ColorLuminance(R, G, B: byte): integer; inline;
begin
  Result := (
    R * LuminanceMultR +
    G * LuminanceMultG +
    B * LuminanceMultB ) shr 8;
end;

// -----------------------------------------------------------------------------
// Average = Intensity = (R + G + B) ÷ 3
function ColorAverage(Color: TColor32): integer; inline;
begin
  Result := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
end;

// -----------------------------------------------------------------------------
// Brightness = Value = Max(R, G, B)
function ColorBrightness(Color: TColor32): integer; inline;
var
  R, G, B: byte;
begin
  B := (Color and $FF);
  G := (Color shr 8) and $FF;
  R := (Color shr 16) and $FF;
  Result := ColorBrightness(R, G, B);
end;

function ColorBrightness(R, G, B: Byte): integer; inline; overload;
begin
  Result := Max(Max(R, G), B);
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderMultiply
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderMultiply.BlendComponent(F, B: cardinal): cardinal;
begin
{$if defined(BLEND_USE_TABLES)}
  Result := MulDiv255Table[F, B];
{$else}
  Result := Div255(B * F);
{$ifend}
end;

class function TGraphics32BlenderMultiply.GetID: string;
begin
  Result := cBlendMultiply;
end;

class function TGraphics32BlenderMultiply.GetName: string;
begin
  Result := sBlendMultiply;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderScreen
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderScreen.BlendComponent(F, B: cardinal): cardinal;
begin
  B := 255 - B; // Complement
  F := 255 - F; // Complement
{$if defined(BLEND_USE_TABLES)}
  Result := MulDiv255Table[B, F]; // Multiply
{$else}
  Result := Div255(B * F); // Multiply
{$ifend}
  Result := 255 - Result; // Complement
end;

class function TGraphics32BlenderScreen.GetID: string;
begin
  Result := cBlendScreen;
end;

class function TGraphics32BlenderScreen.GetName: string;
begin
  Result := sBlendScreen;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSoftLight
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderSoftLight.BlendComponent(F, B: cardinal): cardinal;
var
  n: Cardinal;
begin
{$if defined(BLEND_USE_TABLES)}
  n := MulDiv255Table[B, F];
  Result := n + MulDiv255Table[B, Cardinal(255 - MulDiv255Table[255 - B, 255 - F]) - n];
{$else}
  n := Div255(B * F);
  Result := n + Div255(B * (255 - Div255((255 - B) * (255 - F)) - n));
{$ifend}
end;

class function TGraphics32BlenderSoftLight.GetID: string;
begin
  Result := cBlendSoftLight;
end;

class function TGraphics32BlenderSoftLight.GetName: string;
begin
  Result := sBlendSoftLight;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHardLight
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderHardLight.BlendComponent(F, B: cardinal): cardinal;
begin
{$if defined(BLEND_USE_TABLES)}
{$else}
{$ifend}
  if (F < 128) then
    Result := Div127(F * B)
  else
  begin
    F := 255 - F;
    B := 255 - B;
    Result := 255 - Div127(F * B);
  end;
end;

class function TGraphics32BlenderHardLight.GetID: string;
begin
  Result := cBlendHardLight;
end;

class function TGraphics32BlenderHardLight.GetName: string;
begin
  Result := sBlendHardLight;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderOverlay
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderOverlay.BlendComponent(F, B: cardinal): cardinal;
begin
{$if defined(BLEND_USE_TABLES)}
{$else}
{$ifend}
  if (B < 128) then
    Result := Div127(B * F)
  else
  begin
    F := 255 - F;
    B := 255 - B;
    F := Div127(B  * F);
    Result := 255 - F;
  end;
end;

class function TGraphics32BlenderOverlay.GetID: string;
begin
  Result := cBlendOverlay;
end;

class function TGraphics32BlenderOverlay.GetName: string;
begin
  Result := sBlendOverlay;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearLight
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderLinearLight.BlendComponent(F,
  B: cardinal): cardinal;
begin
{$if defined(BLEND_COMPOUND)}
  if (F < 128) then
    Result := TGraphics32BlenderLinearBurn.BlendComponent(2*F, B)
  else
  if (F > 128) then
    Result := TGraphics32BlenderLinearDodge.BlendComponent(2*(F-128), B)
  else
    Result := B;
{$else}
  if (F < 128) then
  begin
    // LinearBurn(2*F, B)
    F := F * 2 + B;
    if (F < 255) then
      Result := 0
    else
      Result := F - 255;
  end else
  if (F > 128) then
  begin
    // LinearDodge(2*(F-128), B)
    Result := 2 * (F - 128) + B;
    if (F > 255) then
      Result := 255;
  end else
    Result := B;
{$ifend}
end;

class function TGraphics32BlenderLinearLight.GetID: string;
begin
  Result := cBlendLinearLight;
end;

class function TGraphics32BlenderLinearLight.GetName: string;
begin
  Result := sBlendLinearLight;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderPinLight
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderPinLight.BlendComponent(F, B: cardinal): cardinal;
begin
{$if defined(BLEND_COMPOUND)}
{$else}
  // TODO
{$ifend}
  if (F < 128) then
  begin
    F := 2 * F;
    Result := TGraphics32BlenderDarken.BlendComponent(F, B);
  end else
  begin
    F := 2 * (F - 128);
    Result := TGraphics32BlenderLighten.BlendComponent(F, B);
  end;
end;

class function TGraphics32BlenderPinLight.GetID: string;
begin
  Result := cBlendPinLight;
end;

class function TGraphics32BlenderPinLight.GetName: string;
begin
  Result := sBlendPinLight;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHardMix
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderHardMix.BlendComponent(F, B: cardinal): cardinal;
begin
{$if defined(BLEND_COMPOUND)}
{$else}
  // TODO
{$ifend}
  F := TGraphics32BlenderVividLight.BlendComponent(F, B);
  if (F < 128) then
    Result := 0
  else
    Result := 255;
end;

class function TGraphics32BlenderHardMix.GetID: string;
begin
  Result := cBlendHardMix;
end;

class function TGraphics32BlenderHardMix.GetName: string;
begin
  Result := sBlendHardMix;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderVividLight
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderVividLight.BlendComponent(F, B: cardinal): cardinal;
begin
(*
  if (F < 128) then
  begin
    F := 2 * F;
    BlendColorBurn(F, B);
  end else
  begin
    F := 2 * F - 255;
    BlendColorDodge(F, B);
  end;
//*)
//(*
  if (F < 128) then
  begin
    // ColorBurn(2*F, B)
    if (F <> 0) then
    begin
{$if defined(BLEND_USE_TABLES)}
      F := DivMul255Table[2 * F, 255 - B];
{$else}
      F := ((255 - B) shl 8) div (2 * F);
{$ifend}
      if (F > 255) then
        Result := 0
      else
        Result := 255 - F;
    end else
      Result := 0;
  end else
  begin
    // ColorDodge(2*(F-128), B)
    if (F < 255) then
    begin
//      F := 2 * F - 255;
      F := 2 * (F - 128);
{$if defined(BLEND_USE_TABLES)}
      Result := DivMul255Table[255 - F, B];
{$else}
      Result := (B shl 8) div (255 - F);
{$ifend}
      if (Result > 255) then
        Result := 255;
    end else
      Result := 255;
  end;
//*)
end;

class function TGraphics32BlenderVividLight.GetID: string;
begin
  Result := cBlendVividLight;
end;

class function TGraphics32BlenderVividLight.GetName: string;
begin
  Result := sBlendVividLight;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearDodge
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderLinearDodge.BlendComponent(F, B: cardinal): cardinal;
begin
  Result := F + B;
  if (Result > 255) then
    Result := 255;
end;

class function TGraphics32BlenderLinearDodge.GetID: string;
begin
  Result := cBlendLinearDodge;
end;

class function TGraphics32BlenderLinearDodge.GetName: string;
begin
  Result := sBlendLinearDodge;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColorDodge
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderColorDodge.BlendComponent(F, B: cardinal): cardinal;
begin
  if (F < 255) then
  begin
{$if defined(BLEND_USE_TABLES)}
    Result := DivMul255Table[255 - F, B];
{$else}
    Result := (B * 255) div (255 - F);
{$ifend}
    if (Result > 255) then
      Result := 255;
  end else
    Result := 255;
end;

class function TGraphics32BlenderColorDodge.GetID: string;
begin
  Result := cBlendColorDodge;
end;

class function TGraphics32BlenderColorDodge.GetName: string;
begin
  Result := sBlendColorDodge;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColorBurn
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderColorBurn.BlendComponent(F, B: cardinal): cardinal;
var
  FF: Cardinal;
begin
  if F > 0 then
  begin
{$if defined(BLEND_USE_TABLES)}
// Result can exceed 255 so we can't use the tables
//    FF := DivMul255Table[F, 255 - B];
{$else}
{$ifend}
    FF := (255 - B) * 255 div F;
    if (FF > 255) then
      Result := 0
    else
      Result := 255-FF;
  end else
    Result := 0
end;

class function TGraphics32BlenderColorBurn.GetID: string;
begin
  Result := cBlendColorBurn;
end;

class function TGraphics32BlenderColorBurn.GetName: string;
begin
  Result := sBlendColorBurn;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLinearBurn
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderLinearBurn.BlendComponent(F, B: cardinal): cardinal;
begin
  Result := F + B;
  if (Result < 255) then
    Result := 0
  else
    Result := Result - 255;
end;

class function TGraphics32BlenderLinearBurn.GetID: string;
begin
  Result := cBlendLinearBurn;
end;

class function TGraphics32BlenderLinearBurn.GetName: string;
begin
  Result := sBlendLinearBurn;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDifference
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderDifference.BlendComponent(F, B: cardinal): cardinal;
begin
  Result := Abs(B - F);
end;

class function TGraphics32BlenderDifference.GetID: string;
begin
  Result := cBlendDifference;
end;

class function TGraphics32BlenderDifference.GetName: string;
begin
  Result := sBlendDifference;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderNegation
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderNegation.BlendComponent(F, B: cardinal): cardinal;
begin
  Result := 255 - Abs(B - F);
end;

class function TGraphics32BlenderNegation.GetID: string;
begin
  Result := cBlendNegation;
end;

class function TGraphics32BlenderNegation.GetName: string;
begin
  Result := sBlendNegation;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderExclusion
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderExclusion.BlendComponent(F, B: cardinal): cardinal;
begin
  Result := (F + B) - Div127(F * B);
end;

class function TGraphics32BlenderExclusion.GetID: string;
begin
  Result := cBlendExclusion;
end;

class function TGraphics32BlenderExclusion.GetName: string;
begin
  Result := sBlendExclusion;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLighten
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderLighten.BlendComponent(F, B: cardinal): cardinal;
begin
  if (B > F) then
    Result := B
  else
    Result := F;
end;

class function TGraphics32BlenderLighten.GetID: string;
begin
  Result := cBlendLighten;
end;

class function TGraphics32BlenderLighten.GetName: string;
begin
  Result := sBlendLighten;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDarken
//
//------------------------------------------------------------------------------
class function TGraphics32BlenderDarken.BlendComponent(F, B: cardinal): cardinal;
begin
  if (B < F) then
    Result := B
  else
    Result := F;
end;

class function TGraphics32BlenderDarken.GetID: string;
begin
  Result := cBlendDarken;
end;

class function TGraphics32BlenderDarken.GetName: string;
begin
  Result := sBlendDarken;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderDarkerColor
//
//------------------------------------------------------------------------------
function TGraphics32BlenderDarkerColor.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
var
  lumF, lumB: Cardinal;
  Blended: TColor32;
begin
  lumF := ColorLightness(fColor);
  lumB := ColorLightness(bColor);

  // TODO : Verify this!
  if (lumB < lumF) then
    Blended := bColor
  else
    Blended := fColor;

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderDarkerColor.GetID: string;
begin
  Result := cBlendDarkerColor;
end;

class function TGraphics32BlenderDarkerColor.GetName: string;
begin
  Result := sBlendDarkerColor;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLighterColor
//
//------------------------------------------------------------------------------
function TGraphics32BlenderLighterColor.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
var
  lumF, lumB: Cardinal;
  Blended: TColor32;
begin
  lumF := ColorLightness(fColor);
  lumB := ColorLightness(bColor);

  // TODO : Verify this!
  if (lumB > lumF) then
    Blended := bColor
  else
    Blended := fColor;

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderLighterColor.GetID: string;
begin
  Result := cBlendLighterColor;
end;

class function TGraphics32BlenderLighterColor.GetName: string;
begin
  Result := sBlendLighterColor;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderHue
//
//------------------------------------------------------------------------------
function TGraphics32BlenderHue.BlendComponents(fColor: TColor32;
  fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;

  function Max(const A, B: Cardinal): Cardinal; inline;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

  function Min(const A, B: Cardinal): Cardinal; inline;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

  function Hue(RGB: TColor32): Byte; inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
    Delta, HL: Integer;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));

    if Cmax = Cmin then
      Result := 0
    else
    begin
      Delta := (Cmax - Cmin) * 255;
      Delta := Delta * 6;
      if R = Cmax then
        HL := integer(G - B) * 255 * 255 div Delta
      else
      if G = Cmax then
        HL := 255 * 2 div 6 + integer(B - R) * 255 * 255 div Delta
      else
        HL := 255 * 4 div 6 + integer(R - G) * 255 * 255 div Delta;

      if HL < 0 then
        HL := HL + 255 * 2;
      Result := HL;
    end;
  end;

  procedure RGBtoSL(RGB: TColor32; out S, L: Byte); inline;
  var
    R, G, B, Delta, Cmax, Cmin: Cardinal;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));
    L := (Cmax + Cmin) div 2;

    if Cmax = Cmin then
      S := 0
    else
    begin
      Delta := (Cmax - Cmin) * 255;
      if L <= $7F then
        S := Delta div (Cmax + Cmin)
      else
        S := Delta div (255 * 2 - Cmax - Cmin);
    end;
  end;

var
  fH, bS, bL: Byte;
  Blended: TColor32;
begin
{$ifdef BLENDER_HUE_CACHE}

  if (fColor = FCacheF) and (bColor = FCacheB) then
  begin
    Blended := FCacheFB;
  end else
  begin
    if (fColor = FCacheF) then
      fH := FCacheH
    else
    begin
      fH := Hue(fColor);
      FCacheH := fH;
      fCacheF := fColor;
    end;

    if (bColor = FCacheB) then
    begin
      bS := FCacheS;
      bL := FCacheL;
    end else
    begin
      RGBtoSL(bColor, bS, bL);
      FCacheS := bS;
      FCacheL := bL;
      FCacheB := bColor;
    end;

    Blended := HSLtoRGB(fH, bS, bL);
    FCacheFB := Blended;
  end;

{$else}

  fH := Hue(fColor);
  RGBtoSL(bColor, bS, bL);
  Blended := HSLtoRGB(fH, bS, bL);

{$endif}

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderHue.GetID: string;
begin
  Result := cBlendHue;
end;

class function TGraphics32BlenderHue.GetName: string;
begin
  Result := sBlendHue;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderSaturation
//
//------------------------------------------------------------------------------
function TGraphics32BlenderSaturation.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;

  function Max(const A, B: Cardinal): Cardinal; inline;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

  function Min(const A, B: Cardinal): Cardinal; inline;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

  function Saturation(RGB: TColor32): Byte; inline;
  var
    R, G, B, Delta, Cmax, Cmin, L: Cardinal;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));
    L := (Cmax + Cmin) div 2;

    if Cmax = Cmin then
      Result := 0
    else
    begin
      Delta := (Cmax - Cmin) * 255;
      if L <= $7F then
        Result := Delta div (Cmax + Cmin)
      else
        Result := Delta div (255 * 2 - Cmax - Cmin);
    end;
  end;

  procedure RGBtoHL(RGB: TColor32; out H, L: Byte); inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
    Delta, HL: Integer;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));
    L := (Cmax + Cmin) div 2;

    if Cmax = Cmin then
      H := 0
    else
    begin
      Delta := (Cmax - Cmin) * 255 * 6;

      if R = Cmax then
        HL := integer(G - B) * 255 * 255 div Delta
      else if G = Cmax then
        HL := 255 * 2 div 6 + integer(B - R) * 255 * 255 div Delta
      else
        HL := 255 * 4 div 6 + integer(R - G) * 255 * 255 div Delta;

      if HL < 0 then
        HL := HL + 255 * 2;
      H := HL;
    end;
  end;

var
  fS, bH, bL: Byte;
  Blended: TColor32;
begin
  fS := Saturation(fColor);
  RGBtoHL(bColor, bH, bL);
  Blended := HSLtoRGB(bH, fS, bL);

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderSaturation.GetID: string;
begin
  Result := cBlendSaturation;
end;

class function TGraphics32BlenderSaturation.GetName: string;
begin
  Result := sBlendSaturation;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderLuminance
//
//------------------------------------------------------------------------------
function TGraphics32BlenderLuminance.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;

  function Max(const A, B: Cardinal): Cardinal; inline;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

  function Min(const A, B: Cardinal): Cardinal; inline;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

  function Lightness(RGB: TColor32): Byte; inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));
    Result := (Cmax + Cmin) div 2;
  end;

  procedure RGBtoHS(RGB: TColor32; out H, S: Byte); inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
    Delta, HL: Integer;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));

    if Cmax = Cmin then
    begin
      H := 0;
      S := 0
    end
    else
    begin
      Delta := (Cmax - Cmin) * 255;
      if (Cmax + Cmin) div 2 <= $7F then
        S := Delta div integer(Cmax + Cmin)
      else
        S := Delta div integer(255 * 2 - Cmax - Cmin);

      Delta := Delta * 6;
      if R = Cmax then
        HL := integer(G - B) * 255 * 255 div Delta
      else if G = Cmax then
        HL := 255 * 2 div 6 + integer(B - R) * 255 * 255 div Delta
      else
        HL := 255 * 4 div 6 + integer(R - G) * 255 * 255 div Delta;

      if HL < 0 then
        HL := HL + 255 * 2;
      H := HL;
    end;
  end;

var
  fL, bH, bS: Byte;
  Blended: TColor32;
begin
  fL := Lightness(fColor);
  RGBtoHS(bColor, bH, bS);
  Blended := HSLtoRGB(bH, bS, fL);

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderLuminance.GetID: string;
begin
  Result := cBlendLuminosity;
end;

class function TGraphics32BlenderLuminance.GetName: string;
begin
  Result := sBlendLuminance;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderColor
//
//------------------------------------------------------------------------------
function TGraphics32BlenderColor.BlendComponents(fColor: TColor32; fAlpha: cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;

  function Max(const A, B: Cardinal): Cardinal; inline;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

  function Min(const A, B: Cardinal): Cardinal; inline;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

  function Luminance(RGB: TColor32): Byte; inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));
    Result := (Cmax + Cmin) div 2;
  end;

  procedure RGBtoHS(RGB: TColor32; out H, S: Byte); inline;
  var
    R, G, B, Cmax, Cmin: Cardinal;
    Delta, HL: Integer;
  begin
    R := (RGB shr 16) and $ff;
    G := (RGB shr 8) and $ff;
    B := RGB and $ff;

    Cmax := Max(R, Max(G, B));
    Cmin := Min(R, Min(G, B));

    if Cmax = Cmin then
    begin
      H := 0;
      S := 0
    end
    else
    begin
      Delta := (Cmax - Cmin) * 255;
      if (Cmax + Cmin) div 2 <= $7F then
        S := Delta div integer(Cmax + Cmin)
      else
        S := Delta div integer(255 * 2 - Cmax - Cmin);

      Delta := Delta * 6;
      if R = Cmax then
        HL := integer(G - B) * 255 * 255 div Delta
      else if G = Cmax then
        HL := 255 * 2 div 6 + integer(B - R) * 255 * 255 div Delta
      else
        HL := 255 * 4 div 6 + integer(R - G) * 255 * 255 div Delta;

      if HL < 0 then
        HL := HL + 255 * 2;
      H := HL;
    end;
  end;

var
  fH, fS, bL: Byte;
  Blended: TColor32;
begin
  RGBtoHS(fColor, fH, fS);
  bL := Luminance(bColor);
  Blended := HSLtoRGB(fH, fS, bL);

  Result := DoBlendComponents(fColor, fAlpha, bColor, bAlpha, Blended);
end;

class function TGraphics32BlenderColor.GetID: string;
begin
  Result := cBlendColor;
end;

class function TGraphics32BlenderColor.GetName: string;
begin
  Result := sBlendColor;
end;


//------------------------------------------------------------------------------

initialization
  // Place then standard "Normal" blend mode in the PhotoShop "Normal" group
  Graphics32BlendService.Groups.Register(sPhotoShopBlendGroupNormal, TGraphics32BlenderNormal);

  // Register the blend modes and associate them with their groups
  Graphics32BlendService.Register(TGraphics32BlenderDarken,     [sPhotoShopBlendGroupDarken]);
  Graphics32BlendService.Register(TGraphics32BlenderMultiply,   [sPhotoShopBlendGroupDarken]);
  Graphics32BlendService.Register(TGraphics32BlenderColorBurn,  [sPhotoShopBlendGroupDarken]);
  Graphics32BlendService.Register(TGraphics32BlenderLinearBurn, [sPhotoShopBlendGroupDarken]);
  Graphics32BlendService.Register(TGraphics32BlenderDarkerColor,[sPhotoShopBlendGroupDarken]);

  Graphics32BlendService.Register(TGraphics32BlenderLighten,    [sPhotoShopBlendGroupLighten]);
  Graphics32BlendService.Register(TGraphics32BlenderScreen,     [sPhotoShopBlendGroupLighten]);
  Graphics32BlendService.Register(TGraphics32BlenderColorDodge, [sPhotoShopBlendGroupLighten]);
  Graphics32BlendService.Register(TGraphics32BlenderLinearDodge,[sPhotoShopBlendGroupLighten]);
  Graphics32BlendService.Register(TGraphics32BlenderLighterColor,[sPhotoShopBlendGroupLighten]);

  Graphics32BlendService.Register(TGraphics32BlenderOverlay,    [sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderSoftLight,  [sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderHardLight,  [sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderVividLight, [sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderLinearLight,[sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderPinLight,   [sPhotoShopBlendGroupContrast]);
  Graphics32BlendService.Register(TGraphics32BlenderHardMix,    [sPhotoShopBlendGroupContrast]);

  Graphics32BlendService.Register(TGraphics32BlenderDifference, [sPhotoShopBlendGroupInversion]);
  Graphics32BlendService.Register(TGraphics32BlenderExclusion,  [sPhotoShopBlendGroupInversion]);
  Graphics32BlendService.Register(TGraphics32BlenderNegation,   [sPhotoShopBlendGroupInversion]);

  Graphics32BlendService.Register(TGraphics32BlenderHue,        [sPhotoShopBlendGroupComponent]);
  Graphics32BlendService.Register(TGraphics32BlenderSaturation, [sPhotoShopBlendGroupComponent]);
  Graphics32BlendService.Register(TGraphics32BlenderColor,      [sPhotoShopBlendGroupComponent]);
  Graphics32BlendService.Register(TGraphics32BlenderLuminance,  [sPhotoShopBlendGroupComponent]);
end.
