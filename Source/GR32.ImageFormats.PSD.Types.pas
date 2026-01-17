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

uses
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.PSD.Model;

//------------------------------------------------------------------------------
//
//      PSD file format types and constants
//
//------------------------------------------------------------------------------
// https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/
//------------------------------------------------------------------------------

const
  // File type
  PSD_VERSION_PSD       = 1;
  PSD_VERSION_PSB       = 2;

const
  // color modes
  PSD_BITMAP            = 0;
  PSD_GRAYSCALE         = 1;
  PSD_INDEXED           = 2;
  PSD_RGB               = 3;
  PSD_CMYK              = 4;
  PSD_MULTICHANNEL      = 7;
  PSD_DUOTONE           = 8;
  PSD_LAB               = 9;

const
  // Compression
  PSD_COMPRESSION_NONE  = 0;
  PSD_COMPRESSION_RLE   = 1; // RLE compression (a.k.a. packbits compression)
  PSD_COMPRESSION_ZIP   = 2; // ZIP compression without prediction
  PSD_COMPRESSION_ZIP_PRED = 4; // ZIP compression with prediction

const
  // Blend modes
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

const
  // Channel type
  PSD_MASK_REALUSERDATA = -3;   // -3 = real user supplied layer mask (when both a user mask and a vector mask are present)
  PSD_MASK_USERDATA     = -2;   // -2 = user data mask / user supplied layer mask
  PSD_MASK_ALPHA        = -1;   // -1 = alpha channel / transparency mask
  PSD_MASK_RED          = 0;    //  0 = red (or gray, or cyan etc.)
  PSD_MASK_GREEN        = 1;    //  1 = green (or magenta etc.)
  PSD_MASK_BLUE         = 2;    //  2 = blue (or yellow etc.)
  PSD_MASK_BLACK        = 3;    //  3 = black (for CMYK images)

const
  PSD_MAX_CHANNELS      = 56;

type
  TPSDChannelInfo = packed record
    ChannelID: SmallInt;        // One of PSD_MASK_*
    ChannelSize: Cardinal;      // Size of channel in stream (i.e. after compression)
  end;

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
