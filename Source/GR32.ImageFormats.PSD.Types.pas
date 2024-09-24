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
  GR32.ImageFormats.PSD;

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
  PSD_MASK_USERDATA     = -2;   // -2 = user data mask
  PSD_MASK_ALPHA        = -1;   // -1 = alpha channel
  PSD_MASK_RED          = 0;    //  0 = red (or gray, or cyan etc.)
  PSD_MASK_GREEN        = 1;    //  1 = green (or magenta etc.)
  PSD_MASK_BLUE         = 2;    //  2 = blue (or yellow etc.)
  PSD_MASK_BLACK        = 3;    //  3 = black (for CMYK images)

type
  TPSDChannelInfo = packed record
    ChannelID: Word;
    ChannelSize: Cardinal;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
