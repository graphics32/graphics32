unit GR32.ImageFormats.PSD.Types;

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
 * The Original Code is PSD Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Lamdalili
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Anders Melander <anders@melander.dk>
 *
 * ***** END LICENSE BLOCK ***** *)

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

{$include GR32.inc}

//------------------------------------------------------------------------------
//
//      PSD file format types and constants
//
//------------------------------------------------------------------------------
// https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// File header, Version
//------------------------------------------------------------------------------
const
  PSD_VERSION_PSD       = 1;
  PSD_VERSION_PSB       = 2;


//------------------------------------------------------------------------------
// File header, Channels; Max number
//------------------------------------------------------------------------------
const
  PSD_MAX_CHANNELS      = 56;


//------------------------------------------------------------------------------
// File header, Color mode
//------------------------------------------------------------------------------
const
  PSD_BITMAP            = 0;
  PSD_GRAYSCALE         = 1;
  PSD_INDEXED           = 2;
  PSD_RGB               = 3;
  PSD_CMYK              = 4;
  PSD_MULTICHANNEL      = 7;
  PSD_DUOTONE           = 8;
  PSD_LAB               = 9;


//------------------------------------------------------------------------------
// Channel image data & Image data, Compression
//------------------------------------------------------------------------------
const
  PSD_COMPRESSION_NONE  = 0; // RAW; No compression
  PSD_COMPRESSION_RLE   = 1; // RLE compression (a.k.a. packbits compression)
  PSD_COMPRESSION_ZIP   = 2; // ZIP compression without prediction
  PSD_COMPRESSION_ZIP_PRED = 3; // ZIP compression with prediction


//------------------------------------------------------------------------------
// Layer records, Blend mode key
//------------------------------------------------------------------------------
type
  TPSDLayerBlendMode = (
    lbmPass,
    lbmNormal,
    lbmDarken,
    lbmLighten,
    lbmHue,
    lbmSaturation,
    lbmColor,
    lbmLuminosity,
    lbmMultiply,
    lbmScreen,
    lbmDissolve,
    lbmOverlay,
    lbmHardLight,
    lbmSoftLight,
    lbmDifference,
    lbmExclusion,
    lbmColorDodge,
    lbmColorBurn,
    lbmLinearLight,
    lbmLinearBurn,
    lbmDarkerColor,
    lbmLinearDodge,
    lbmPinLight,
    lbmVividLight,
    lbmHardMix,
    lbmLighterColor,
    lbmSubtract,
    lbmDivide
  );

const
  PSDBlendModeMapping: array[TPSDLayerBlendMode] of PAnsiChar = (
    'pass', // Pass through
    'norm', // Normal
    'dark', // Darken
    'lite', // Lighten
    'hue ', // Hue
    'sat ', // Saturation
    'colr', // Color
    'lum ', // Luminosity
    'mul ', // Multiply
    'scrn', // Screen
    'diss', // Dissolve
    'over', // Overlay
    'hLit', // Hard light
    'sLit', // Soft light
    'diff', // Difference
    'smud', // Exclusion
    'div ', // Color dodge
    'idiv', // Color burn
    'lLit', // Linear light
    'lBrn', // Linear burn
    'dkCl', // Darker color
    'lddg', // Linear dodge (Add)
    'pLit', // Pin light
    'vLit', // Vivid light
    'hMix', // Hard mix
    'lgCl', // Lighter color
    'fsub', // Subtract
    'fdiv'  // Divide
  );


//------------------------------------------------------------------------------
// Layer records, Channel type
//------------------------------------------------------------------------------
const
  PSD_MASK_REALUSERDATA = -3;   // Real user supplied layer mask (when both a user mask and a vector mask are present)
  PSD_MASK_USERDATA     = -2;   // User data mask / user supplied layer mask
  PSD_MASK_ALPHA        = -1;   // Alpha channel / transparency mask
  PSD_MASK_RED          = 0;    // Red (or gray, or cyan etc.)
  PSD_MASK_GREEN        = 1;    // Green (or magenta etc.)
  PSD_MASK_BLUE         = 2;    // Blue (or yellow etc.)
  PSD_MASK_BLACK        = 3;    // Black (for CMYK images)


//------------------------------------------------------------------------------
// Layer records, Channel information
//------------------------------------------------------------------------------
type
  TPSDChannelInfo = packed record
    ChannelID: SmallInt;        // One of PSD_MASK_*
    ChannelSize: Cardinal;      // Size of channel in stream (i.e. after compression)
  end;


//------------------------------------------------------------------------------
// Additional Layer Information, Key
//------------------------------------------------------------------------------
const
  PSD_KEY_SolidColor                = 'SoCo'; // Adjustment layer, Solid color (Photoshop 4.0)
  PSD_KEY_Gradient                  = 'GdFl'; // Adjustment layer, Gradient (Photoshop 4.0)
  PSD_KEY_Pattern                   = 'PtFl'; // Adjustment layer, Pattern (Photoshop 4.0)
  PSD_KEY_BrightnessAndContrast     = 'brit'; // Adjustment layer, Brightness and contrast (Photoshop 4.0)
  PSD_KEY_Levels                    = 'levl'; // Adjustment layer, Levels (Photoshop 4.0)
  PSD_KEY_Curves                    = 'curv'; // Adjustment layer, Curves (Photoshop 4.0)
  PSD_KEY_Exposure                  = 'expA'; // Adjustment layer, Exposure (Photoshop CS3)
  PSD_KEY_Vibrance                  = 'vibA'; // Adjustment layer, Vibrance (Photoshop CS3)
  PSD_KEY_HueSaturation             = 'hue '; // Adjustment layer, Hue/saturation (Photoshop 4.0)
  PSD_KEY_HueSaturation2            = 'hue2'; // Adjustment layer, Hue/saturation (Photoshop 5.0)
  PSD_KEY_ColorBalance              = 'blnc'; // Adjustment layer, Color Balance (Photoshop 4.0)
  PSD_KEY_BlackAndWhite             = 'blwh'; // Adjustment layer, Black and white (Photoshop CS3)
  PSD_KEY_PhotoFilter               = 'phfl'; // Adjustment layer, Photo Filter (Photoshop 4.0)
  PSD_KEY_ChannelMixer2             = 'mixr'; // Adjustment layer, Channel Mixer (Photoshop 5.0)
  PSD_KEY_ColorLookup               = 'clrL'; // Adjustment layer, Color Lookup (Photoshop CS6)
  PSD_KEY_Invert                    = 'nvrt'; // Adjustment layer, Invert (Photoshop 4.0)
  PSD_KEY_Posterize                 = 'post'; // Adjustment layer, Posterize (not documented)
  PSD_KEY_Threshold                 = 'thrs'; // Adjustment layer, Threshold (Photoshop 4.0)
  PSD_KEY_GradientMap               = 'grdm'; // Adjustment layer, Gradient map (Photoshop 6.0)
  PSD_KEY_SelectiveColor            = 'selc'; // Adjustment layer, Selective color (Photoshop 4.0)
  PSD_KEY_Effects                   = 'lrFX'; // Effects layer (Photoshop 5.0)
  PSD_KEY_TypeToolInfo              = 'tySh'; // Type Tool Info (Photoshop 5.0 & 5.5 only)
  PSD_KEY_UnicodeLayerName          = 'luni'; // Unicode layer name (Photoshop 5.0)
  PSD_KEY_LayerID                   = 'lyid'; // Layer ID (Photoshop 5.0)
  PSD_KEY_OBELayerInfo              = 'lfx2'; // Object-based effects layer info (Photoshop 6.0)
  PSD_KEY_Patterns                  = 'Patt'; // Patterns (Photoshop 6.0 & CS 8.0)
  PSD_KEY_Patterns2                 = 'Pat2'; // Patterns (Photoshop 6.0 & CS 8.0)
  PSD_KEY_Patterns3                 = 'Pat3'; // Patterns (Photoshop 6.0 & CS 8.0)
  PSD_KEY_Annotations               = 'Anno'; // Annotations (Photoshop 6.0)
  PSD_KEY_BlendClippingElements     = 'clbl'; // Blend clipping elements (Photoshop 6.0)
  PSD_KEY_BlendInteriorElements     = 'infx'; // Blend interior elements (Photoshop 6.0)
  PSD_KEY_KnockoutSetting           = 'knko'; // Knockout setting (Photoshop 6.0)
  PSD_KEY_ProtectedSetting          = 'lspf'; // Protected setting (Photoshop 6.0)
  PSD_KEY_SheetColorSetting         = 'lclr'; // Sheet color setting (Photoshop 6.0)
  PSD_KEY_ReferencePoint            = 'fxrp'; // Reference point (Photoshop 6.0)
  PSD_KEY_SectionDividerSetting     = 'lsct'; // Section divider setting (Photoshop 6.0)
  PSD_KEY_SectionDividerSetting2    = 'lsdk'; // Section divider setting ?
  PSD_KEY_ChannelMixer1             = 'brst'; // Channel Mixer (Photoshop 4.0)
  PSD_KEY_VectorMaskSetting         = 'vmsk'; // Vector mask setting (Photoshop 6.0)
  PSD_KEY_VectorMaskSetting2        = 'vsms'; // Vector mask setting (Photoshop CS6)
  PSD_KEY_FilterEffects             = 'ffxi'; // Filter effects (Photoshop 5.0)
  PSD_KEY_LayerNameSource           = 'lnsr'; // Layer name source setting (Photoshop 6.0)
  PSD_KEY_LayerNameSource2          = 'lnsf'; // Layer name source setting (Photoshop 6.0) ?
  PSD_KEY_ShellAdjustmentLayer      = 'shpa'; // Shell adjustment layer (Photoshop 4.0)
  PSD_KEY_MetaData                  = 'shmd'; // Metadata setting (Photoshop 6.0)
  PSD_KEY_LayerVersion              = 'lyvr'; // Layer version (Photoshop 7.0)
  PSD_KEY_TransparencyShapesLayer   = 'tsly'; // Transparency shapes layer (Photoshop 7.0)
  PSD_KEY_LayerMaskAsGlobalMask     = 'lmgm'; // Layer mask as global mask (Photoshop 7.0)
  PSD_KEY_VectorMaskAsGlobalMask    = 'vmgm'; // Vector mask as global mask (Photoshop 7.0)
  PSD_KEY_PlacedLayer               = 'plLd'; // Vector mask as global mask (Photoshop 4.0, obsolete CS3)
  PSD_KEY_LinkedLayer               = 'lnkD'; // Linked layer (Photoshop 4.0)
  PSD_KEY_LinkedLayer2              = 'lnk2'; // Linked layer (Photoshop 4.0)
  PSD_KEY_LinkedLayer3              = 'lnk3'; // Linked layer (Photoshop 4.0)
  PSD_KEY_ContentGeneratorExtData   = 'CgEd'; // Content Generator Extra Data (Photoshop CS5)
  PSD_KEY_TextEngineData            = 'Txt2'; // Text Engine Data (Photoshop CS3)
  PSD_KEY_UnicodePathName           = 'pths'; // Unicode Path Name (Photoshop CS6)
  PSD_KEY_AnimationEffects          = 'anFX'; // Animation Effects (Photoshop CS6)
  PSD_KEY_FilterMask                = 'FMsk'; // Filter Mask (Photoshop CS3)
  PSD_KEY_PlacedLayerData           = 'SoLd'; // Placed Layer Data (Photoshop CS3)
  PSD_KEY_VectorStrokeData          = 'vstk'; // Vector Stroke Data (Photoshop CS6)
  PSD_KEY_VectorStrokeContentData   = 'vscg'; // Vector Stroke Content Data (Photoshop CS6)
  PSD_KEY_UsingAlignedRendering     = 'sn2P'; // Using Aligned Rendering (Photoshop CS6)
  PSD_KEY_VectorOriginationData     = 'vogk'; // Vector Origination Data (Photoshop CC)
  PSD_KEY_PixelSourceData           = 'PxSc'; // Pixel Source Data (Photoshop CC)
  PSD_KEY_CompositorUsed            = 'cinf'; // Compositor Used (Photoshop 2020)
  PSD_KEY_PixelSourceData2          = 'PxSD'; // Pixel Source Data (Photoshop CC 2015)
  PSD_KEY_ArtboardData              = 'artb'; // Artboard Data (Photoshop CC 2015)
  PSD_KEY_ArtboardData2             = 'artd'; // Artboard Data (Photoshop CC 2015)
  PSD_KEY_ArtboardData3             = 'abdd'; // Artboard Data (Photoshop CC 2015)
  PSD_KEY_SmartObjectLayerData      = 'SoLE'; // Smart Object Layer Data (Photoshop CC 2015)
  PSD_KEY_SavingMergedTransparency  = 'Mtrn'; // Saving Merged Transparency
  PSD_KEY_SavingMergedTransparency2 = 'Mt16'; // Saving Merged Transparency
  PSD_KEY_SavingMergedTransparency3 = 'Mt32'; // Saving Merged Transparency
  PSD_KEY_UserMask                  = 'LMsk'; // User Mask
  PSD_KEY_FilterEffects2            = 'FXid'; // Filter Effects
  PSD_KEY_FilterEffects3            = 'FEid'; // Filter Effects
  PSD_KEY_CustomAdjustmentLayer     = 'cust'; // Custom adjustment layer (Photoshop 4.0) ?
  PSD_KEY_LayerData                 = 'Layr'; // Layer Data ?
  PSD_KEY_LayerData16               = 'Lr16'; // Layer Data ?
  PSD_KEY_LayerData32               = 'Lr32'; // Layer Data ?


//------------------------------------------------------------------------------
// Image resource, ID
//------------------------------------------------------------------------------
type
  TPSD_ImageResourceID = (
    PSD_PS2_IMAGE_INFO    = 1000,         // $03e8 - Obsolete - ps 2.0 image info
    PSD_MAC_PRINT_INFO    = 1001,         // $03e9 - Optional - Mac print manager print info record
    PSD_PS2_COLOR_TAB     = 1003,         // $03eb - Obsolete - ps 2.0 indexed color table
    PSD_RESN_INFO         = 1005,         // $03ed - ResolutionInfo structure
    PSD_ALPHA_NAMES       = 1006,         // $03ee - Alpha channel names
    PSD_DISPLAY_INFO      = 1007,         // $03ef - Superceded by PSD_DISPLAY_INFO_NEW for ps CS3 and higher - DisplayInfo structure
    PSD_CAPTION           = 1008,         // $03f0 - Optional - Caption string
    PSD_BORDER_INFO       = 1009,         // $03f1 - Border info
    PSD_BACKGROUND_COL    = 1010,         // $03f2 - Background color
    PSD_PRINT_FLAGS       = 1011,         // $03f3 - Print flags
    PSD_GREY_HALFTONE     = 1012,         // $03f4 - Greyscale and multichannel halftoning info
    PSD_COLOR_HALFTONE    = 1013,         // $03f5 - Color halftoning info
    PSD_DUOTONE_HALFTONE  = 1014,         // $03f6 - Duotone halftoning info
    PSD_GREY_XFER         = 1015,         // $03f7 - Greyscale and multichannel transfer functions
    PSD_COLOR_XFER        = 1016,         // $03f8 - Color transfer functions
    PSD_DUOTONE_XFER      = 1017,         // $03f9 - Duotone transfer functions
    PSD_DUOTONE_INFO      = 1018,         // $03fa - Duotone image information
    PSD_EFFECTIVE_BW      = 1019,         // $03fb - Effective black & white values for dot range
    PSD_OBSOLETE_01       = 1020,         // $03fc - Obsolete
    PSD_EPS_OPT           = 1021,         // $03fd - EPS options
    PSD_QUICK_MASK        = 1022,         // $03fe - Quick mask info
    PSD_OBSOLETE_02       = 1023,         // $03ff - Obsolete
    PSD_LAYER_STATE       = 1024,         // $0400 - Layer state info
    PSD_WORKING_PATH      = 1025,         // $0401 - Working path (not saved)
    PSD_LAYER_GROUP       = 1026,         // $0402 - Layers group info
    PSD_OBSOLETE_03       = 1027,         // $0403 - Obsolete
    PSD_IPTC_NAA_DATA     = 1028,         // $0404 - IPTC-NAA record (IMV4.pdf)
    PSD_IMAGE_MODE_RAW    = 1029,         // $0405 - Image mode for raw format files
    PSD_JPEG_QUAL         = 1030,         // $0406 - JPEG quality
    PSD_GRID_GUIDE        = 1032,         // $0408 - Grid & guide info
    PSD_THUMB_RES         = 1033,         // $0409 - Thumbnail resource
    PSD_COPYRIGHT_FLG     = 1034,         // $040a - Copyright flag
    PSD_URL               = 1035,         // $040b - URL string
    PSD_THUMB_RES2        = 1036,         // $040c - Thumbnail resource
    PSD_GLOBAL_ANGLE      = 1037,         // $040d - Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Global angle
    PSD_COLOR_SAMPLER     = 1038,         // $040e - Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Color samplers resource
    PSD_ICC_PROFILE       = 1039,         // $040f - ICC Profile
    PSD_WATERMARK         = 1040,         // $0410 - Watermark
    PSD_ICC_UNTAGGED      = 1041,         // $0411 - Do not use ICC profile flag
    PSD_EFFECTS_VISIBLE   = 1042,         // $0412 - Show / hide all effects layers
    PSD_SPOT_HALFTONE     = 1043,         // $0413 - Spot halftone
    PSD_DOC_IDS           = 1044,         // $0414 - Document specific IDs
    PSD_ALPHA_NAMES_UNI   = 1045,         // $0415 - Unicode alpha names
    PSD_IDX_COL_TAB_CNT   = 1046,         // $0416 - Indexed color table count
    PSD_IDX_TRANSPARENT   = 1047,         // $0417 - Index of transparent color (if any)
    PSD_GLOBAL_ALT        = 1049,         // $0419 - Global altitude
    PSD_SLICES            = 1050,         // $041a - Slices
    PSD_WORKFLOW_URL_UNI  = 1051,         // $041b - Workflow URL - Unicode string
    PSD_JUMP_TO_XPEP      = 1052,         // $041c - Jump to XPEP (?)
    PSD_ALPHA_ID          = 1053,         // $041d - Alpha IDs
    PSD_URL_LIST_UNI      = 1054,         // $041e - URL list - unicode
    PSD_VERSION_INFO      = 1057,         // $0421 - Version info
    PSD_EXIF_DATA         = 1058,         // $0422 - Exif data block 1
    PSD_EXIF_DATA_3       = 1059,         // $0423 - Exif data block 3 (?)
    PSD_XMP_DATA          = 1060,         // $0424 - XMP data block
    PSD_CAPTION_DIGEST    = 1061,         // $0425 - Caption digest
    PSD_PRINT_SCALE       = 1062,         // $0426 - Print scale
    PSD_PIXEL_AR          = 1064,         // $0428 - Pixel aspect ratio
    PSD_LAYER_COMPS       = 1065,         // $0429 - Layer comps
    PSD_ALT_DUOTONE_COLOR = 1066,         // $042A - Alternative Duotone colors
    PSD_ALT_SPOT_COLOR    = 1067,         // $042B - Alternative Spot colors
    PSD_LAYER_SELECT_ID   = 1069,         // $042D - Layer selection ID
    PSD_HDR_TONING_INFO   = 1070,         // $042E - HDR toning information
    PSD_PRINT_INFO_SCALE  = 1071,         // $042F - Print scale
    PSD_LAYER_GROUP_E_ID  = 1072,         // $0430 - Layer group(s) enabled ID
    PSD_COLOR_SAMPLER_NEW = 1073,         // $0431 - Color sampler resource for ps CS3 and higher PSD files
    PSD_MEASURE_SCALE     = 1074,         // $0432 - Measurement scale
    PSD_TIMELINE_INFO     = 1075,         // $0433 - Timeline information
    PSD_SHEET_DISCLOSE    = 1076,         // $0434 - Sheet discloser
    PSD_DISPLAY_INFO_NEW  = 1077,         // $0435 - DisplayInfo structure for ps CS3 and higher PSD files
    PSD_ONION_SKINS       = 1078,         // $0436 - Onion skins
    PSD_COUNT_INFO        = 1080,         // $0438 - Count information
    PSD_PRINT_INFO        = 1082,         // $043A - Print information added in ps CS5
    PSD_PRINT_STYLE       = 1083,         // $043B - Print style
    PSD_MAC_NSPRINTINFO   = 1084,         // $043C - Mac NSPrintInfo
    PSD_WIN_DEVMODE       = 1085,         // $043D - Windows DEVMODE
    PSD_AUTO_SAVE_PATH    = 1086,         // $043E - Auto save file path
    PSD_AUTO_SAVE_FORMAT  = 1087,         // $043F - Auto save format
    PSD_PATH_INFO_FIRST   = 2000,         // $07d0 - First path info block
    PSD_PATH_INFO_LAST    = 2998,         // $0bb6 - Last path info block
    PSD_CLIPPING_PATH     = 2999,         // $0bb7 - Name of clipping path
    PSD_PLUGIN_R_FIRST    = 4000,         // $0FA0 - First plugin resource
    PSD_PLUGIN_R_LAST     = 4999,         // $1387 - Last plugin resource
    PSD_IMAGEREADY_VARS   = 7000,         // $1B58 - Imageready variables
    PSD_IMAGEREADY_DATA   = 7001,         // $1B59 - Imageready data sets
    PSD_LIGHTROOM_WORK    = 8000,         // $1F40 - Lightroom workflow
    PSD_PRINT_FLAGS_2     = 10000         // $2710 - Print flags
  );



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
