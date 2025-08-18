unit GR32_PortableNetworkGraphic.Types;

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
 * The Original Code is GR32PNG for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}
{$include GR32_PngCompilerSwitches.inc}

uses
  SysUtils;

//------------------------------------------------------------------------------
//
//      Color encoding types
//
//------------------------------------------------------------------------------
type
  {$A1}
  TRGB24 = packed record
    R, G, B: Byte;
  end;
  PRGB24 = ^TRGB24;

  TRGB24Array = array [0..0] of TRGB24;
  PRGB24Array = ^TRGB24Array;

type
  TRGB24Word = packed record
    R, G, B : Word;
  end;
  PRGB24Word = ^TRGB24Word;

type
  TRGB32 = packed record
    R, G, B, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRGB32Word = packed record
    R, G, B, A: Word;
  end;
  PRGB32Word = ^TRGB32Word;

//------------------------------------------------------------------------------
//
//      Misc. types
//
//------------------------------------------------------------------------------
type
  PByteArray = SysUtils.PByteArray;
  TByteArray = SysUtils.TByteArray;

  EPngError = class(Exception);

type
  {$A1}
  TColorType = (
    ctGrayscale = 0,
    ctTrueColor = 2,
    ctIndexedColor = 3,
    ctGrayscaleAlpha = 4,
    ctTrueColorAlpha = 6
  );

type
  TInterlaceMethod = (
    imNone = 0,
    imAdam7 = 1
  );

//------------------------------------------------------------------------------
//
//      Encoding types
//
//------------------------------------------------------------------------------
type
  TFilterMethod = (
    fmAdaptiveFilter = 0
  );

  TAdaptiveFilterMethod = (
    afmNone = 0,
    afmSub = 1,
    afmUp = 2,
    afmAverage = 3,
    afmPaeth = 4
  );

  TAvailableAdaptiveFilterMethod = (aafmSub, aafmUp, aafmAverage, aafmPaeth);
  TAvailableAdaptiveFilterMethods = set of TAvailableAdaptiveFilterMethod;


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function ColorTypeToString(Value: TColorType): string;
function InterlaceMethodToString(Value: TInterlaceMethod): string;


//------------------------------------------------------------------------------
//
//      Resourcestrings
//
//------------------------------------------------------------------------------
resourcestring
  RCStrAncillaryUnknownChunk = 'Unknown chunk is marked as ancillary';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  RCStrDataIncomplete = 'Data not complete';
  RCStrChunkInvalid = 'Invalid chunk data';
  RCStrDirectCompressionMethodSetError = 'Compression Method may not be specified directly yet!';
  RCStrDirectFilterMethodSetError = 'Filter Method may not be specified directly yet!';
  RCStrDirectGammaSetError = 'Gamma may not be specified directly yet!';
  RCStrDirectHeightSetError = 'Height may not be specified directly yet!';
  RCStrDirectWidthSetError = 'Width may not be specified directly yet!';
  RCStrEmptyChunkList = 'Chunk list is empty';
  RCStrHeaderInvalid = 'The provided header is not valid!';
  RCStrIncompletePalette = 'Palette is incomplete';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrNewHeaderError = 'New header may not be nil!';
  RCStrNotAValidPNGFile = 'Not a valid PNG file';
  RCStrNotYetImplemented = 'PNG feature not implemented (%s)';
  RCStrChunkNotImplemented = 'Chunk type %s not implemented';
  RCStrPaletteLimited = 'Palette is limited to 256 entries';
  RCStrSeveralChromaChunks = 'Primary chromaticities chunk defined twice!';
  RCStrSeveralGammaChunks = 'Gamma chunk defined twice!';
  RCStrSeveralPaletteChunks = 'Palette chunk defined twice!';
  RCStrSeveralTransparencyChunks = 'Transparency chunk defined twice!';
  RCStrSeveralBackgroundChunks = 'Background chunk defined twice!';
  RCStrSeveralPhysicalPixelDimensionChunks = 'Several physical pixel dimenson chunks found';
  RCStrSeveralSignificantBitsChunksFound = 'Several significant bits chunks found';
  RCStrSeveralTimeChunks = 'Time chunk appears twice!';
  RCStrMissingIDATChunk = 'IDAT chunk missing';
  RCStrUnknownColorType = 'Unknown color type!';
  RCStrUnspecifiedPixelUnit = 'Unspecified unit';
  RCStrUnsupportedCompressionMethod = 'Compression method not supported!';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrUnsupportedColorType = 'Unsupported color type';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrWrongInterlaceMethod = 'Wrong interlace method';
  RCStrWrongPixelPerUnit = 'Pixel per unit may not be zero!';
  RCStrWrongTransparencyFormat = 'Wrong transparency format';
  RCStrInvalidCompressionLevel = 'Invalid compression level';
  RCStrBitDepthTranscodingError = 'Bit depth may not be specified directly yet!';
  RCStrColorTypeTranscodingError = 'Color Type may not be specified directly yet!';
  RCStrGrayscale = 'Grayscale';
  RCStrTrueColor = 'True Color';
  RCStrIndexedColor = 'Indexed Color';
  RCStrGrayscaleAlpha = 'Transparent Grayscale';
  RCStrTrueColorAlpha = 'Transparent True Color';
  RCStrInterlacingNone = 'None';
  RCStrInterlacingAdam7 = 'Adam7';
  {$IFDEF CheckCRC}
  RCStrCRCError = 'CRC Error';
  {$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function ColorTypeToString(Value: TColorType): string;
const
  // TODO : Incorrect use of resourcestring
  CColorTypeNames : array [TColorType] of string = (RCStrGrayScale,
    'undefined', RCStrTrueColor, RCStrIndexedColor, RCStrGrayscaleAlpha,
    'undefined', RCStrTrueColorAlpha);
begin
  Result := CColorTypeNames[Value];
end;

function InterlaceMethodToString(Value: TInterlaceMethod): string;
const
  // TODO : Incorrect use of resourcestring
  CInterlaceMethodNames : array [TInterlaceMethod] of string = (RCStrInterlacingNone,
    RCStrInterlacingAdam7);
begin
  Result := CInterlaceMethodNames[Value];
end;


end.
