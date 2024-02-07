unit GR32;

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
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   J. Tulach <tulach at position.cz>
 *   Jouni Airaksinen <markvera at spacesynth.net>
 *   Timothy Weber <teejaydub at users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, Types,
{$ELSE}
  UITypes, Types, Windows,
{$ENDIF}
  Controls, Graphics, Classes, SysUtils;

{ Version Control }

const
  Graphics32Version = '3.0';

{ 32-bit Color }

type
  PColor32 = ^TColor32;
  TColor32 = type Cardinal;

  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..0] of TColor32;
  TArrayOfColor32 = array of TColor32;

{$IFNDEF RGBA_FORMAT}
  TColor32Component = (ccBlue, ccGreen, ccRed, ccAlpha);
{$ELSE}
  TColor32Component = (ccRed, ccGreen, ccBlue, ccAlpha);
{$ENDIF}
  TColor32Components = set of TColor32Component;

  PColor32Entry = ^TColor32Entry;
  TColor32Entry = packed record
    case Integer of
{$IFNDEF RGBA_FORMAT}
      0: (B, G, R, A: Byte);
{$ELSE}
      0: (R, G, B, A: Byte);
{$ENDIF}
      1: (ARGB: TColor32);
      2: (Planes: array[0..3] of Byte);
      3: (Components: array[TColor32Component] of Byte);
  end;

  PColor32EntryArray = ^TColor32EntryArray;
  TColor32EntryArray = array [0..0] of TColor32Entry;
  TArrayOfColor32Entry = array of TColor32Entry;

  PPalette32 = ^TPalette32;
  TPalette32 = array [Byte] of TColor32;

const
  // Some predefined color constants
  clBlack32               = TColor32({$IFNDEF RGBA_FORMAT} $FF000000 {$ELSE} $FF000000 {$ENDIF});
  clDimGray32             = TColor32({$IFNDEF RGBA_FORMAT} $FF3F3F3F {$ELSE} $FF3F3F3F {$ENDIF});
  clGray32                = TColor32({$IFNDEF RGBA_FORMAT} $FF7F7F7F {$ELSE} $FF7F7F7F {$ENDIF});
  clLightGray32           = TColor32({$IFNDEF RGBA_FORMAT} $FFBFBFBF {$ELSE} $FFBFBFBF {$ENDIF});
  clWhite32               = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFFFF {$ELSE} $FFFFFFFF {$ENDIF});
  clMaroon32              = TColor32({$IFNDEF RGBA_FORMAT} $FF7F0000 {$ELSE} $FF00007F {$ENDIF});
  clGreen32               = TColor32({$IFNDEF RGBA_FORMAT} $FF007F00 {$ELSE} $FF007F00 {$ENDIF});
  clOlive32               = TColor32({$IFNDEF RGBA_FORMAT} $FF7F7F00 {$ELSE} $FF007F7F {$ENDIF});
  clNavy32                = TColor32({$IFNDEF RGBA_FORMAT} $FF00007F {$ELSE} $FF7F0000 {$ENDIF});
  clPurple32              = TColor32({$IFNDEF RGBA_FORMAT} $FF7F007F {$ELSE} $FF7F007F {$ENDIF});
  clTeal32                = TColor32({$IFNDEF RGBA_FORMAT} $FF007F7F {$ELSE} $FF7F7F00 {$ENDIF});
  clRed32                 = TColor32({$IFNDEF RGBA_FORMAT} $FFFF0000 {$ELSE} $FF0000FF {$ENDIF});
  clLime32                = TColor32({$IFNDEF RGBA_FORMAT} $FF00FF00 {$ELSE} $FF00FF00 {$ENDIF});
  clYellow32              = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFF00 {$ELSE} $FF00FFFF {$ENDIF});
  clBlue32                = TColor32({$IFNDEF RGBA_FORMAT} $FF0000FF {$ELSE} $FFFF0000 {$ENDIF});
  clFuchsia32             = TColor32({$IFNDEF RGBA_FORMAT} $FFFF00FF {$ELSE} $FFFF00FF {$ENDIF});
  clAqua32                = TColor32({$IFNDEF RGBA_FORMAT} $FF00FFFF {$ELSE} $FFFFFF00 {$ENDIF});

  clAliceBlue32           = TColor32({$IFNDEF RGBA_FORMAT} $FFF0F8FF {$ELSE} $FFFFF8F0 {$ENDIF});
  clAntiqueWhite32        = TColor32({$IFNDEF RGBA_FORMAT} $FFFAEBD7 {$ELSE} $FFD7EBFA {$ENDIF});
  clAquamarine32          = TColor32({$IFNDEF RGBA_FORMAT} $FF7FFFD4 {$ELSE} $FFD4FF7F {$ENDIF});
  clAzure32               = TColor32({$IFNDEF RGBA_FORMAT} $FFF0FFFF {$ELSE} $FFFFFFF0 {$ENDIF});
  clBeige32               = TColor32({$IFNDEF RGBA_FORMAT} $FFF5F5DC {$ELSE} $FFDCF5F5 {$ENDIF});
  clBisque32              = TColor32({$IFNDEF RGBA_FORMAT} $FFFFE4C4 {$ELSE} $FFC4E4FF {$ENDIF});
  clBlancheDalmond32      = TColor32({$IFNDEF RGBA_FORMAT} $FFFFEBCD {$ELSE} $FFCDEBFF {$ENDIF});
  clBlueViolet32          = TColor32({$IFNDEF RGBA_FORMAT} $FF8A2BE2 {$ELSE} $FFE22B8A {$ENDIF});
  clBrown32               = TColor32({$IFNDEF RGBA_FORMAT} $FFA52A2A {$ELSE} $FF2A2AA5 {$ENDIF});
  clBurlyWood32           = TColor32({$IFNDEF RGBA_FORMAT} $FFDEB887 {$ELSE} $FF87B8DE {$ENDIF});
  clCadetblue32           = TColor32({$IFNDEF RGBA_FORMAT} $FF5F9EA0 {$ELSE} $FFA09E5F {$ENDIF});
  clChartReuse32          = TColor32({$IFNDEF RGBA_FORMAT} $FF7FFF00 {$ELSE} $FF00FF7F {$ENDIF});
  clChocolate32           = TColor32({$IFNDEF RGBA_FORMAT} $FFD2691E {$ELSE} $FF1E69D2 {$ENDIF});
  clCoral32               = TColor32({$IFNDEF RGBA_FORMAT} $FFFF7F50 {$ELSE} $FF507FFF {$ENDIF});
  clCornFlowerBlue32      = TColor32({$IFNDEF RGBA_FORMAT} $FF6495ED {$ELSE} $FFED9564 {$ENDIF});
  clCornSilk32            = TColor32({$IFNDEF RGBA_FORMAT} $FFFFF8DC {$ELSE} $FFDCF8FF {$ENDIF});
  clCrimson32             = TColor32({$IFNDEF RGBA_FORMAT} $FFDC143C {$ELSE} $FF3C14DC {$ENDIF});
  clDarkBlue32            = TColor32({$IFNDEF RGBA_FORMAT} $FF00008B {$ELSE} $FF8B0000 {$ENDIF});
  clDarkCyan32            = TColor32({$IFNDEF RGBA_FORMAT} $FF008B8B {$ELSE} $FF8B8B00 {$ENDIF});
  clDarkGoldenRod32       = TColor32({$IFNDEF RGBA_FORMAT} $FFB8860B {$ELSE} $FF0B86B8 {$ENDIF});
  clDarkGray32            = TColor32({$IFNDEF RGBA_FORMAT} $FFA9A9A9 {$ELSE} $FFA9A9A9 {$ENDIF});
  clDarkGreen32           = TColor32({$IFNDEF RGBA_FORMAT} $FF006400 {$ELSE} $FF006400 {$ENDIF});
  clDarkGrey32            = TColor32({$IFNDEF RGBA_FORMAT} $FFA9A9A9 {$ELSE} $FFA9A9A9 {$ENDIF});
  clDarkKhaki32           = TColor32({$IFNDEF RGBA_FORMAT} $FFBDB76B {$ELSE} $FF6BB7BD {$ENDIF});
  clDarkMagenta32         = TColor32({$IFNDEF RGBA_FORMAT} $FF8B008B {$ELSE} $FF8B008B {$ENDIF});
  clDarkOliveGreen32      = TColor32({$IFNDEF RGBA_FORMAT} $FF556B2F {$ELSE} $FF2F6B55 {$ENDIF});
  clDarkOrange32          = TColor32({$IFNDEF RGBA_FORMAT} $FFFF8C00 {$ELSE} $FF008CFF {$ENDIF});
  clDarkOrchid32          = TColor32({$IFNDEF RGBA_FORMAT} $FF9932CC {$ELSE} $FFCC3299 {$ENDIF});
  clDarkRed32             = TColor32({$IFNDEF RGBA_FORMAT} $FF8B0000 {$ELSE} $FF00008B {$ENDIF});
  clDarkSalmon32          = TColor32({$IFNDEF RGBA_FORMAT} $FFE9967A {$ELSE} $FF7A96E9 {$ENDIF});
  clDarkSeaGreen32        = TColor32({$IFNDEF RGBA_FORMAT} $FF8FBC8F {$ELSE} $FF8FBC8F {$ENDIF});
  clDarkSlateBlue32       = TColor32({$IFNDEF RGBA_FORMAT} $FF483D8B {$ELSE} $FF8B3D48 {$ENDIF});
  clDarkSlateGray32       = TColor32({$IFNDEF RGBA_FORMAT} $FF2F4F4F {$ELSE} $FF4F4F2F {$ENDIF});
  clDarkSlateGrey32       = TColor32({$IFNDEF RGBA_FORMAT} $FF2F4F4F {$ELSE} $FF4F4F2F {$ENDIF});
  clDarkTurquoise32       = TColor32({$IFNDEF RGBA_FORMAT} $FF00CED1 {$ELSE} $FFD1CE00 {$ENDIF});
  clDarkViolet32          = TColor32({$IFNDEF RGBA_FORMAT} $FF9400D3 {$ELSE} $FFD30094 {$ENDIF});
  clDeepPink32            = TColor32({$IFNDEF RGBA_FORMAT} $FFFF1493 {$ELSE} $FF9314FF {$ENDIF});
  clDeepSkyBlue32         = TColor32({$IFNDEF RGBA_FORMAT} $FF00BFFF {$ELSE} $FFFFBF00 {$ENDIF});
  clDodgerBlue32          = TColor32({$IFNDEF RGBA_FORMAT} $FF1E90FF {$ELSE} $FFFF901E {$ENDIF});
  clFireBrick32           = TColor32({$IFNDEF RGBA_FORMAT} $FFB22222 {$ELSE} $FF2222B2 {$ENDIF});
  clFloralWhite32         = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFAF0 {$ELSE} $FFF0FAFF {$ENDIF});
  clGainsBoro32           = TColor32({$IFNDEF RGBA_FORMAT} $FFDCDCDC {$ELSE} $FFDCDCDC {$ENDIF});
  clGhostWhite32          = TColor32({$IFNDEF RGBA_FORMAT} $FFF8F8FF {$ELSE} $FFFFF8F8 {$ENDIF});
  clGold32                = TColor32({$IFNDEF RGBA_FORMAT} $FFFFD700 {$ELSE} $FF00D7FF {$ENDIF});
  clGoldenRod32           = TColor32({$IFNDEF RGBA_FORMAT} $FFDAA520 {$ELSE} $FF20A5DA {$ENDIF});
  clGreenYellow32         = TColor32({$IFNDEF RGBA_FORMAT} $FFADFF2F {$ELSE} $FF2FFFAD {$ENDIF});
  clGrey32                = TColor32({$IFNDEF RGBA_FORMAT} $FF808080 {$ELSE} $FF808080 {$ENDIF});
  clHoneyDew32            = TColor32({$IFNDEF RGBA_FORMAT} $FFF0FFF0 {$ELSE} $FFF0FFF0 {$ENDIF});
  clHotPink32             = TColor32({$IFNDEF RGBA_FORMAT} $FFFF69B4 {$ELSE} $FFB469FF {$ENDIF});
  clIndianRed32           = TColor32({$IFNDEF RGBA_FORMAT} $FFCD5C5C {$ELSE} $FF5C5CCD {$ENDIF});
  clIndigo32              = TColor32({$IFNDEF RGBA_FORMAT} $FF4B0082 {$ELSE} $FF82004B {$ENDIF});
  clIvory32               = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFFF0 {$ELSE} $FFF0FFFF {$ENDIF});
  clKhaki32               = TColor32({$IFNDEF RGBA_FORMAT} $FFF0E68C {$ELSE} $FF8CE6F0 {$ENDIF});
  clLavender32            = TColor32({$IFNDEF RGBA_FORMAT} $FFE6E6FA {$ELSE} $FFFAE6E6 {$ENDIF});
  clLavenderBlush32       = TColor32({$IFNDEF RGBA_FORMAT} $FFFFF0F5 {$ELSE} $FFF5F0FF {$ENDIF});
  clLawnGreen32           = TColor32({$IFNDEF RGBA_FORMAT} $FF7CFC00 {$ELSE} $FF00FC7C {$ENDIF});
  clLemonChiffon32        = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFACD {$ELSE} $FFCDFAFF {$ENDIF});
  clLightBlue32           = TColor32({$IFNDEF RGBA_FORMAT} $FFADD8E6 {$ELSE} $FFE6D8AD {$ENDIF});
  clLightCoral32          = TColor32({$IFNDEF RGBA_FORMAT} $FFF08080 {$ELSE} $FF8080F0 {$ENDIF});
  clLightCyan32           = TColor32({$IFNDEF RGBA_FORMAT} $FFE0FFFF {$ELSE} $FFFFFFE0 {$ENDIF});
  clLightGoldenRodYellow32= TColor32({$IFNDEF RGBA_FORMAT} $FFFAFAD2 {$ELSE} $FFD2FAFA {$ENDIF});
  clLightGreen32          = TColor32({$IFNDEF RGBA_FORMAT} $FF90EE90 {$ELSE} $FF90EE90 {$ENDIF});
  clLightGrey32           = TColor32({$IFNDEF RGBA_FORMAT} $FFD3D3D3 {$ELSE} $FFD3D3D3 {$ENDIF});
  clLightPink32           = TColor32({$IFNDEF RGBA_FORMAT} $FFFFB6C1 {$ELSE} $FFC1B6FF {$ENDIF});
  clLightSalmon32         = TColor32({$IFNDEF RGBA_FORMAT} $FFFFA07A {$ELSE} $FF7AA0FF {$ENDIF});
  clLightSeagreen32       = TColor32({$IFNDEF RGBA_FORMAT} $FF20B2AA {$ELSE} $FFAAB220 {$ENDIF});
  clLightSkyblue32        = TColor32({$IFNDEF RGBA_FORMAT} $FF87CEFA {$ELSE} $FFFACE87 {$ENDIF});
  clLightSlategray32      = TColor32({$IFNDEF RGBA_FORMAT} $FF778899 {$ELSE} $FF998877 {$ENDIF});
  clLightSlategrey32      = TColor32({$IFNDEF RGBA_FORMAT} $FF778899 {$ELSE} $FF998877 {$ENDIF});
  clLightSteelblue32      = TColor32({$IFNDEF RGBA_FORMAT} $FFB0C4DE {$ELSE} $FFDEC4B0 {$ENDIF});
  clLightYellow32         = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFFE0 {$ELSE} $FFE0FFFF {$ENDIF});
  clLtGray32              = TColor32({$IFNDEF RGBA_FORMAT} $FFC0C0C0 {$ELSE} $FFC0C0C0 {$ENDIF});
  clMedGray32             = TColor32({$IFNDEF RGBA_FORMAT} $FFA0A0A4 {$ELSE} $FFA4A0A0 {$ENDIF});
  clDkGray32              = TColor32({$IFNDEF RGBA_FORMAT} $FF808080 {$ELSE} $FF808080 {$ENDIF});
  clMoneyGreen32          = TColor32({$IFNDEF RGBA_FORMAT} $FFC0DCC0 {$ELSE} $FFC0DCC0 {$ENDIF});
  clLegacySkyBlue32       = TColor32({$IFNDEF RGBA_FORMAT} $FFA6CAF0 {$ELSE} $FFF0CAA6 {$ENDIF});
  clCream32               = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFBF0 {$ELSE} $FFF0FBFF {$ENDIF});
  clLimeGreen32           = TColor32({$IFNDEF RGBA_FORMAT} $FF32CD32 {$ELSE} $FF32CD32 {$ENDIF});
  clLinen32               = TColor32({$IFNDEF RGBA_FORMAT} $FFFAF0E6 {$ELSE} $FFE6F0FA {$ENDIF});
  clMediumAquamarine32    = TColor32({$IFNDEF RGBA_FORMAT} $FF66CDAA {$ELSE} $FFAACD66 {$ENDIF});
  clMediumBlue32          = TColor32({$IFNDEF RGBA_FORMAT} $FF0000CD {$ELSE} $FFCD0000 {$ENDIF});
  clMediumOrchid32        = TColor32({$IFNDEF RGBA_FORMAT} $FFBA55D3 {$ELSE} $FFD355BA {$ENDIF});
  clMediumPurple32        = TColor32({$IFNDEF RGBA_FORMAT} $FF9370DB {$ELSE} $FFDB7093 {$ENDIF});
  clMediumSeaGreen32      = TColor32({$IFNDEF RGBA_FORMAT} $FF3CB371 {$ELSE} $FF71B33C {$ENDIF});
  clMediumSlateBlue32     = TColor32({$IFNDEF RGBA_FORMAT} $FF7B68EE {$ELSE} $FFEE687B {$ENDIF});
  clMediumSpringGreen32   = TColor32({$IFNDEF RGBA_FORMAT} $FF00FA9A {$ELSE} $FF9AFA00 {$ENDIF});
  clMediumTurquoise32     = TColor32({$IFNDEF RGBA_FORMAT} $FF48D1CC {$ELSE} $FFCCD148 {$ENDIF});
  clMediumVioletRed32     = TColor32({$IFNDEF RGBA_FORMAT} $FFC71585 {$ELSE} $FF8515C7 {$ENDIF});
  clMidnightBlue32        = TColor32({$IFNDEF RGBA_FORMAT} $FF191970 {$ELSE} $FF701919 {$ENDIF});
  clMintCream32           = TColor32({$IFNDEF RGBA_FORMAT} $FFF5FFFA {$ELSE} $FFFAFFF5 {$ENDIF});
  clMistyRose32           = TColor32({$IFNDEF RGBA_FORMAT} $FFFFE4E1 {$ELSE} $FFE1E4FF {$ENDIF});
  clMoccasin32            = TColor32({$IFNDEF RGBA_FORMAT} $FFFFE4B5 {$ELSE} $FFB5E4FF {$ENDIF});
  clNavajoWhite32         = TColor32({$IFNDEF RGBA_FORMAT} $FFFFDEAD {$ELSE} $FFADDEFF {$ENDIF});
  clOldLace32             = TColor32({$IFNDEF RGBA_FORMAT} $FFFDF5E6 {$ELSE} $FFE6F5FD {$ENDIF});
  clOliveDrab32           = TColor32({$IFNDEF RGBA_FORMAT} $FF6B8E23 {$ELSE} $FF238E6B {$ENDIF});
  clOrange32              = TColor32({$IFNDEF RGBA_FORMAT} $FFFFA500 {$ELSE} $FF00A5FF {$ENDIF});
  clOrangeRed32           = TColor32({$IFNDEF RGBA_FORMAT} $FFFF4500 {$ELSE} $FF0045FF {$ENDIF});
  clOrchid32              = TColor32({$IFNDEF RGBA_FORMAT} $FFDA70D6 {$ELSE} $FFD670DA {$ENDIF});
  clPaleGoldenRod32       = TColor32({$IFNDEF RGBA_FORMAT} $FFEEE8AA {$ELSE} $FFAAE8EE {$ENDIF});
  clPaleGreen32           = TColor32({$IFNDEF RGBA_FORMAT} $FF98FB98 {$ELSE} $FF98FB98 {$ENDIF});
  clPaleTurquoise32       = TColor32({$IFNDEF RGBA_FORMAT} $FFAFEEEE {$ELSE} $FFEEEEAF {$ENDIF});
  clPaleVioletred32       = TColor32({$IFNDEF RGBA_FORMAT} $FFDB7093 {$ELSE} $FF9370DB {$ENDIF});
  clPapayaWhip32          = TColor32({$IFNDEF RGBA_FORMAT} $FFFFEFD5 {$ELSE} $FFD5EFFF {$ENDIF});
  clPeachPuff32           = TColor32({$IFNDEF RGBA_FORMAT} $FFFFDAB9 {$ELSE} $FFB9DAFF {$ENDIF});
  clPeru32                = TColor32({$IFNDEF RGBA_FORMAT} $FFCD853F {$ELSE} $FF3F85CD {$ENDIF});
  clPlum32                = TColor32({$IFNDEF RGBA_FORMAT} $FFDDA0DD {$ELSE} $FFDDA0DD {$ENDIF});
  clPowderBlue32          = TColor32({$IFNDEF RGBA_FORMAT} $FFB0E0E6 {$ELSE} $FFE6E0B0 {$ENDIF});
  clRosyBrown32           = TColor32({$IFNDEF RGBA_FORMAT} $FFBC8F8F {$ELSE} $FF8F8FBC {$ENDIF});
  clRoyalBlue32           = TColor32({$IFNDEF RGBA_FORMAT} $FF4169E1 {$ELSE} $FFE16941 {$ENDIF});
  clSaddleBrown32         = TColor32({$IFNDEF RGBA_FORMAT} $FF8B4513 {$ELSE} $FF13458B {$ENDIF});
  clSalmon32              = TColor32({$IFNDEF RGBA_FORMAT} $FFFA8072 {$ELSE} $FF7280FA {$ENDIF});
  clSandyBrown32          = TColor32({$IFNDEF RGBA_FORMAT} $FFF4A460 {$ELSE} $FF60A4F4 {$ENDIF});
  clSeaGreen32            = TColor32({$IFNDEF RGBA_FORMAT} $FF2E8B57 {$ELSE} $FF578B2E {$ENDIF});
  clSeaShell32            = TColor32({$IFNDEF RGBA_FORMAT} $FFFFF5EE {$ELSE} $FFEEF5FF {$ENDIF});
  clSienna32              = TColor32({$IFNDEF RGBA_FORMAT} $FFA0522D {$ELSE} $FF2D52A0 {$ENDIF});
  clSilver32              = TColor32({$IFNDEF RGBA_FORMAT} $FFC0C0C0 {$ELSE} $FFC0C0C0 {$ENDIF});
  clSkyblue32             = TColor32({$IFNDEF RGBA_FORMAT} $FF87CEEB {$ELSE} $FFEBCE87 {$ENDIF});
  clSlateBlue32           = TColor32({$IFNDEF RGBA_FORMAT} $FF6A5ACD {$ELSE} $FFCD5A6A {$ENDIF});
  clSlateGray32           = TColor32({$IFNDEF RGBA_FORMAT} $FF708090 {$ELSE} $FF908070 {$ENDIF});
  clSlateGrey32           = TColor32({$IFNDEF RGBA_FORMAT} $FF708090 {$ELSE} $FF908070 {$ENDIF});
  clSnow32                = TColor32({$IFNDEF RGBA_FORMAT} $FFFFFAFA {$ELSE} $FFFAFAFF {$ENDIF});
  clSpringgreen32         = TColor32({$IFNDEF RGBA_FORMAT} $FF00FF7F {$ELSE} $FF7FFF00 {$ENDIF});
  clSteelblue32           = TColor32({$IFNDEF RGBA_FORMAT} $FF4682B4 {$ELSE} $FFB48246 {$ENDIF});
  clTan32                 = TColor32({$IFNDEF RGBA_FORMAT} $FFD2B48C {$ELSE} $FF8CB4D2 {$ENDIF});
  clThistle32             = TColor32({$IFNDEF RGBA_FORMAT} $FFD8BFD8 {$ELSE} $FFD8BFD8 {$ENDIF});
  clTomato32              = TColor32({$IFNDEF RGBA_FORMAT} $FFFF6347 {$ELSE} $FF4763FF {$ENDIF});
  clTurquoise32           = TColor32({$IFNDEF RGBA_FORMAT} $FF40E0D0 {$ELSE} $FFD0E040 {$ENDIF});
  clViolet32              = TColor32({$IFNDEF RGBA_FORMAT} $FFEE82EE {$ELSE} $FFEE82EE {$ENDIF});
  clWheat32               = TColor32({$IFNDEF RGBA_FORMAT} $FFF5DEB3 {$ELSE} $FFB3DEF5 {$ENDIF});
  clWhitesmoke32          = TColor32({$IFNDEF RGBA_FORMAT} $FFF5F5F5 {$ELSE} $FFF5F5F5 {$ENDIF});
  clYellowgreen32         = TColor32({$IFNDEF RGBA_FORMAT} $FF9ACD32 {$ELSE} $FF32CD9A {$ENDIF});

  // Some semi-transparent color constants
  clTrWhite32             = TColor32({$IFNDEF RGBA_FORMAT} $7FFFFFFF {$ELSE} $7FFFFFFF {$ENDIF});
  clTrGray32              = TColor32({$IFNDEF RGBA_FORMAT} $7F7F7F7F {$ELSE} $7F7F7F7F {$ENDIF});
  clTrBlack32             = TColor32({$IFNDEF RGBA_FORMAT} $7F000000 {$ELSE} $7F000000 {$ENDIF});
  clTrRed32               = TColor32({$IFNDEF RGBA_FORMAT} $7FFF0000 {$ELSE} $7F0000FF {$ENDIF});
  clTrGreen32             = TColor32({$IFNDEF RGBA_FORMAT} $7F00FF00 {$ELSE} $7F00FF00 {$ENDIF});
  clTrBlue32              = TColor32({$IFNDEF RGBA_FORMAT} $7F0000FF {$ELSE} $7FFF0000 {$ENDIF});

// Color construction and conversion functions
function Color32(WinColor: TColor): TColor32; overload;
function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
function WinColor(Color32: TColor32): TColor;
function ArrayOfColor32(const Colors: array of TColor32): TArrayOfColor32;

// Unconditionally swap R & B channel, leave rest alone
function SwapRedBlue(Color32: TColor32): TColor32;
procedure SwapRedBlueMem(var Color32: TColor32);

// Conditional swaps. Depends on RGBA_FORMAT define.
function Color32ToBGRA(Color32: TColor32): DWORD; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Color32ToBGRAMem(var Color32: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}
function Color32ToRGBA(Color32: TColor32): DWORD; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Color32ToRGBAMem(var Color32: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}

function BGRAToColor32(BGRA: DWORD): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure BGRAToColor32Mem(var BGRA: DWORD); {$IFDEF USEINLINING} inline; {$ENDIF}
function RGBAToColor32(RGBA: DWORD): TColor32; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RGBAToColor32mem(var RGBA: DWORD); {$IFDEF USEINLINING} inline; {$ENDIF}

// Color component access
procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte); overload;
function Color32Components(R, G, B, A: Boolean): TColor32Components;
function RedComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function GreenComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function BlueComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function AlphaComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function Intensity(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function InvertColor(Color32: TColor32): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure ModifyAlpha(var Color32: TColor32; NewAlpha: Byte); {$IFDEF USEINLINING} inline; {$ENDIF}
procedure ScaleAlpha(var Color32: TColor32; Scale: Single); {$IFDEF USEINLINING} inline; {$ENDIF}

// Color space conversion
function HSLtoRGB(H, S, L: Single; A: Integer = 255): TColor32; overload;
function HSLtoRGB(H, S, L: Integer; A: Integer = 255): TColor32; overload;
procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single); overload;
procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte); overload;
function HSVtoRGB(H, S, V: Single; A: Integer = 255): TColor32;
procedure RGBToHSV(Color: TColor32; out H, S, V: Single);

{$IFNDEF PLATFORM_INDEPENDENT}
// Palette conversion functions
{$IFNDEF RGBA_FORMAT}
function WinPalette(const P: TPalette32): HPALETTE;
{$ENDIF RGBA_FORMAT}
{$ENDIF}

{ A fixed-point type }

type
  // This type has data bits arrangement compatible with Windows.TFixed
  PFixed = ^TFixed;
  TFixed = type Integer;
  {$NODEFINE TFixed}

  {$NODEFINE PFixedRec}
  PFixedRec = ^TFixedRec;
  {$NODEFINE TFixedRec}
  TFixedRec = packed record
    case Integer of
      0: (Fixed: TFixed);
      1: (Frac: Word; Int: SmallInt);
  end;

  PFixedArray = ^TFixedArray;
  TFixedArray = array [0..0] of TFixed;
  PArrayOfFixed = ^TArrayOfFixed;
  TArrayOfFixed = array of TFixed;
  PArrayOfArrayOfFixed = ^TArrayOfArrayOfFixed;
  TArrayOfArrayOfFixed = array of TArrayOfFixed;

  // TFloat determines the precision level for certain floating-point operations
  PFloat = ^TFloat;
  TFloat = Single;

{ Other dynamic arrays }
type
  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of Byte;
  PArrayOfByte = ^TArrayOfByte;
  TArrayOfByte = array of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array [0..0] of Word;
  PArrayOfWord = ^TArrayOfWord;
  TArrayOfWord = array of Word;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array [0..0] of Integer;
  PArrayOfInteger = ^TArrayOfInteger;
  TArrayOfInteger = array of Integer;
  PArrayOfArrayOfInteger = ^TArrayOfArrayOfInteger;
  TArrayOfArrayOfInteger = array of TArrayOfInteger;

  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array [0..0] of Cardinal;
  PArrayOfCardinal = ^TArrayOfCardinal;
  TArrayOfCardinal = array of Cardinal;
  PArrayOfArrayOfCardinal = ^TArrayOfArrayOfCardinal;
  TArrayOfArrayOfCardinal = array of TArrayOfCardinal;

  PSingleArray = ^TSingleArray;
  TSingleArray = array [0..0] of Single;
  PArrayOfSingle = ^TArrayOfSingle;
  TArrayOfSingle = array of Single;

  PFloatArray = ^TFloatArray;
  TFloatArray = array [0..0] of TFloat;
  PArrayOfFloat = ^TArrayOfFloat;
  TArrayOfFloat = array of TFloat;

const
  // Fixed point math constants
  FixedOne = $10000;
  FixedHalf = $7FFF;
  FixedPI  = Round(PI * FixedOne);
  FixedToFloat = 1 / FixedOne;

  COne255th = 1 / $FF;

function Fixed(S: Single): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Fixed(I: Integer): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Points }

type
{$IFNDEF FPC}
{$IFNDEF BCB}
  PPoint = ^TPoint;
  TPoint = Windows.TPoint;
{$ENDIF}
{$ENDIF}

  PPointArray = ^TPointArray;
  TPointArray = array [0..0] of TPoint;
  PArrayOfPoint = ^TArrayOfPoint;
  TArrayOfPoint = array of TPoint;
  PArrayOfArrayOfPoint = ^TArrayOfArrayOfPoint;
  TArrayOfArrayOfPoint = array of TArrayOfPoint;

  PFloatPoint = ^TFloatPoint;
  TFloatPoint = record
    X, Y: TFloat;
  {$IFDEF SUPPORT_ENHANCED_RECORDS}
  public
    {$IFNDEF FPC}
    {$IFDEF COMPILERXE2_UP}
    constructor Create(P: TPointF); overload;
    {$ENDIF}
    constructor Create(P: TPoint); overload;
    constructor Create(X, Y: Integer); overload;
    constructor Create(X, Y: Single); overload;
    {$ENDIF}

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFloatPoint): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFloatPoint): Boolean;
    class operator Add(const Lhs, Rhs: TFloatPoint): TFloatPoint;
    class operator Subtract(const Lhs, Rhs: TFloatPoint): TFloatPoint;
    {$IFDEF COMPILERXE2_UP}
    class operator Explicit(A: TPointF): TFloatPoint;
    class operator Implicit(A: TPointF): TFloatPoint;
    {$ENDIF}

    class function Zero: TFloatPoint; inline; static;
  {$ENDIF}
  end;

  PFloatPointArray = ^TFloatPointArray;
  TFloatPointArray = array [0..0] of TFloatPoint;
  PArrayOfFloatPoint = ^TArrayOfFloatPoint;
  TArrayOfFloatPoint = array of TFloatPoint;
  PArrayOfArrayOfFloatPoint = ^TArrayOfArrayOfFloatPoint;
  TArrayOfArrayOfFloatPoint = array of TArrayOfFloatPoint;

  PFixedPoint = ^TFixedPoint;
  TFixedPoint = record
    X, Y: TFixed;
  {$IFDEF SUPPORT_ENHANCED_RECORDS}
  public
    {$IFNDEF FPC}
    {$IFDEF COMPILERXE2_UP}
    constructor Create(P: TPointF); overload;
    {$ENDIF}
    constructor Create(P: TFloatPoint); overload;
    constructor Create(X, Y: TFixed); overload;
    constructor Create(X, Y: Integer); overload;
    constructor Create(X, Y: TFloat); overload;
    {$ENDIF}

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixedPoint): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixedPoint): Boolean;
    class operator Add(const Lhs, Rhs: TFixedPoint): TFixedPoint;
    class operator Subtract(const Lhs, Rhs: TFixedPoint): TFixedPoint;

    class function Zero: TFixedPoint; inline; static;
  {$ENDIF}
  end;
  {$NODEFINE TFixedPoint}

  PFixedPointArray = ^TFixedPointArray;
  TFixedPointArray = array [0..0] of TFixedPoint;
  PArrayOfFixedPoint = ^TArrayOfFixedPoint;
  TArrayOfFixedPoint = array of TFixedPoint;
  PArrayOfArrayOfFixedPoint = ^TArrayOfArrayOfFixedPoint;
  TArrayOfArrayOfFixedPoint = array of TArrayOfFixedPoint;

// construction and conversion of point types
function Point(X, Y: Integer): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Point(const FP: TFloatPoint): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Point(const FXP: TFixedPoint): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(X, Y: Single): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(const P: TPoint): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(const FXP: TFixedPoint): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(X, Y: Integer): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(X, Y: Single): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(const P: TPoint): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(const FP: TFloatPoint): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Rectangles }

type
{$IFNDEF FPC}
  PRect = Windows.PRect;
  TRect = Windows.TRect;
{$ENDIF}

  PFloatRect = ^TFloatRect;
  {$NODEFINE TFloatRect}
{$IFDEF SupportsBoost}
  (*$HPPEMIT '#include <boost/strong_typedef.hpp>'*)
{$ENDIF}
  (*$HPPEMIT 'namespace Gr32 {'*)
{$IFDEF SupportsBoost}
  (*$HPPEMIT 'BOOST_STRONG_TYPEDEF(int, TFixed)'*)
{$ELSE}
  (*$HPPEMIT 'typedef int TFixed;'*)
{$ENDIF}
  (*$HPPEMIT 'struct TFixedPoint { float X, Y; }; typedef struct TFixedPoint TFixedPoint;'*)
  (*$HPPEMIT 'struct TFloatRect { float Left, Top, Right, Bottom; }; typedef struct TFloatRect TFloatRect;'*)
  (*$HPPEMIT 'struct TFixedRect { TFixed Left, Top, Right, Bottom; }; typedef struct TFixedRect TFixedRect;'*)
  (*$HPPEMIT '} // namespace Gr32 '*)
  TFloatRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: TFloat);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;

  {$NODEFINE PFixedRect}
  PFixedRect = ^TFixedRect;
  {$NODEFINE TFixedRect}
  TFixedRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: TFixed);
      1: (TopLeft, BottomRight: TFixedPoint);
  end;

  TRectRounding = (rrClosest, rrOutside, rrInside);

// Rectangle construction/conversion functions
function MakeRect(const L, T, R, B: Integer): TRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function MakeRect(const FR: TFloatRect; Rounding: TRectRounding = rrClosest): TRect; overload;
function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding = rrClosest): TRect; overload;
function FixedRect(const L, T, R, B: TFixed): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedRect(const TopLeft, BottomRight: TFixedPoint): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedRect(const ARect: TRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedRect(const FR: TFloatRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const L, T, R, B: TFloat): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const TopLeft, BottomRight: TFloatPoint): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const ARect: TRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const FXR: TFixedRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Some basic operations over rectangles
function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean; overload;
function IntersectRect(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean; overload;
function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean; overload;
function UnionRect(out Rect: TFloatRect; const R1, R2: TFloatRect): Boolean; overload;
function EqualRect(const R1, R2: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRect(const R1, R2: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure InflateRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure InflateRect(var FR: TFloatRect; Dx, Dy: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure OffsetRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure OffsetRect(var FR: TFloatRect; Dx, Dy: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsRectEmpty(const R: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsRectEmpty(const FR: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TRect; const P: TPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TFloatRect; const P: TPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TRect; const P: TFloatPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TFloatRect; const P: TFloatPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRectSize(const R1, R2: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRectSize(const R1, R2: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

type
{ TBitmap32 draw mode }
  TDrawMode = (dmOpaque, dmBlend, dmCustom, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TWrapMode = (wmClamp, wmRepeat, wmMirror);
  TWrapProc = function(Value, Max: Integer): Integer;
  TWrapProcEx = function(Value, Min, Max: Integer): Integer;

type
  { TPlainInterfacedPersistent }
  { TPlainInterfacedPersistent provides simple interface support with
    optional reference-counting operation. }
  TPlainInterfacedPersistent = class(TPersistent, IInterface)
  strict private
    FRefCounted: Boolean;
    FRefCount: Integer;
  protected
    { IInterface }
{$IFDEF FPC_HAS_CONSTREF}
    function QueryInterface(constref iid: TGuid; out obj): HResult; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
    function QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
{$ENDIF}
    property RefCounted: Boolean read FRefCounted write FRefCounted;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
  end;

  { TNotifiablePersistent }
  { TNotifiablePersistent provides a change notification mechanism }
  TNotifiablePersistent = class(TPlainInterfacedPersistent)
  strict private
    FUpdateCount: Integer;
    FLockUpdateCount: Integer;
    FModified: boolean;
    FOnChange: TNotifyEvent;
  strict protected
    procedure DoChanged; virtual;
  protected
    property UpdateCount: Integer read FUpdateCount;
    property LockUpdateCount: Integer read FLockUpdateCount;
    property Modified: boolean read FModified;
  public
    procedure BeforeDestruction; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Changed; virtual;

    procedure BeginLockUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure EndLockUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TThreadPersistent }
  { TThreadPersistent is an ancestor for TBitmap32 object. In addition to
    TPersistent methods, it provides thread-safe locking and change notification }
  TThreadPersistent = class(TNotifiablePersistent)
  strict private
    FLockCount: Integer;
  strict protected
    {$IFDEF FPC}
    FLock: TCriticalSection;
    {$ELSE}
    FLock: TRTLCriticalSection;
    {$ENDIF}
    property LockCount: Integer read FLockCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  { TCustomMap }
  { An ancestor for bitmaps and similar 2D distributions wich have width and
    height properties }
  TCustomMap = class(TThreadPersistent)
  protected
    FHeight: Integer;
    FWidth: Integer;
  strict protected
    FOnResize: TNotifyEvent;
    procedure SetHeight(NewHeight: Integer); virtual;
    procedure SetWidth(NewWidth: Integer); virtual;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); virtual;
  public
    constructor Create(Width, Height: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Delete; virtual;
    function  Empty: Boolean; virtual;
    procedure Resized; virtual;
    function SetSizeFrom(Source: TPersistent): Boolean;
    function SetSize(NewWidth, NewHeight: Integer): Boolean; virtual;

    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

  { TBitmap32 }
  { This is the core of Graphics32 unit. The TBitmap32 class is responsible
    for storage of a bitmap, as well as for drawing in it.
    The OnCombine event is fired only when DrawMode is set to dmCustom and two
    bitmaps are blended together. Unlike most normal events, it does not contain
    "Sender" parameter and is not called through some virtual method. This
    (a little bit non-standard) approach allows for faster operation. }

const
  // common cases
  AREAINFO_RECT         = $80000000;
  AREAINFO_LINE         = $40000000; // 24 bits for line width in pixels...
  AREAINFO_ELLIPSE      = $20000000;
  AREAINFO_ABSOLUTE     = $10000000;

  AREAINFO_MASK         = $FF000000;

type
  TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: Cardinal) of object;
  TAreaChangedEvent = procedure(Sender: TObject; const Area: TRect;
    const Info: Cardinal) of object;

  TCustomResampler = class;

  TCustomBackend = class;
  TCustomBackendClass = class of TCustomBackend;

{$ifndef FPC}
  TResourceType = PChar;
{$else FPC}
  TResourceType = LCLType.TResourceType;
{$endif FPC}

  { TCustomBitmap32 }

  TCustomBitmap32 = class(TCustomMap)
  strict private
    FBackend: TCustomBackend;
    FBits: PColor32Array;
    FClipRect: TRect;
    FFixedClipRect: TFixedRect;
    F256ClipRect: TRect;
    FClipping: Boolean;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;
    FWrapMode: TWrapMode;

    FMasterAlpha: Cardinal;
    FOuterColor: TColor32;
    FPenColor: TColor32;
    FStippleCounter: Single;
    FStipplePattern: TArrayOfColor32;
    FStippleStep: Single;
    FOnPixelCombine: TPixelCombineEvent;
    FOnAreaChanged: TAreaChangedEvent;
    FOldOnAreaChanged: TAreaChangedEvent;
    FMeasuringMode: Boolean;
    FResampler: TCustomResampler;

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; {$IFDEF USEINLINING} inline; {$ENDIF}
{$ENDIF}

    function GetPixelPtr(X, Y: Integer): PColor32;
    function GetScanLine(Y: Integer): PColor32Array;

    procedure SetCombineMode(const Value: TCombineMode);
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetWrapMode(Value: TWrapMode);
    procedure SetMasterAlpha(Value: Cardinal);
    procedure SetClipRect(const Value: TRect);
    procedure SetResampler(AResampler: TCustomResampler);
    function GetResamplerClassName: string;
    procedure SetResamplerClassName(const Value: string);
    function GetPenPos: TPoint;
    procedure SetPenPos(const Value: TPoint);
    function GetPenPosF: TFixedPoint;
    procedure SetPenPosF(const Value: TFixedPoint);
  public type
    TInfoHeaderVersion = (InfoHeaderVersion1, InfoHeaderVersion2, InfoHeaderVersion3, InfoHeaderVersion4, InfoHeaderVersion5);
  strict protected
    procedure BackendChangedHandler(Sender: TObject); virtual;
    procedure BackendChangingHandler(Sender: TObject); virtual;
  strict protected
    WrapProcHorz: TWrapProcEx;
    WrapProcVert: TWrapProcEx;
    BlendProc: Pointer;
    RasterX, RasterY: Integer;
    RasterXF, RasterYF: TFixed;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
    function  Equal(B: TCustomBitmap32): Boolean;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;

    procedure InitializeBackend(ABackendClass: TCustomBackendClass); virtual;
    procedure FinalizeBackend; virtual;
    procedure SetBackend(const ABackend: TCustomBackend); virtual;

{$IFDEF FPC_HAS_CONSTREF}
    function QueryInterface(constref iid: TGuid; out obj): HResult; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
    function QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
{$ENDIF}

  protected
    procedure CopyMapTo(Dst: TCustomBitmap32); virtual;
    procedure CopyPropertiesTo(Dst: TCustomBitmap32); virtual;

    procedure AssignTo(Dst: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;

  protected
    function LoadFromBMPStream(Stream: TStream; Size: Int64): boolean;
    function LoadFromDIBStream(Stream: TStream; Size: Int64): boolean;
    procedure SaveToDIBStream(Stream: TStream; SaveTopDown: Boolean = False); overload;
    procedure SaveToDIBStream(Stream: TStream; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion); overload;

  protected
    procedure SET_T256(X, Y: Integer; C: TColor32);
    procedure SET_TS256(X, Y: Integer; C: TColor32);
    function  GET_T256(X, Y: Integer): TColor32;
    function  GET_TS256(X, Y: Integer): TColor32;

    function  GetPixel(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelS(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelW(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    function  GetPixelF(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelFS(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelFW(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    function  GetPixelX(X, Y: TFixed): TColor32;
    function  GetPixelXS(X, Y: TFixed): TColor32;
    function  GetPixelXW(X, Y: TFixed): TColor32;

    function GetPixelFR(X, Y: Single): TColor32;
    function GetPixelXR(X, Y: TFixed): TColor32;

    function  GetPixelB(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure SetPixel(X, Y: Integer; Value: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetPixelS(X, Y: Integer; Value: TColor32);
    procedure SetPixelW(X, Y: Integer; Value: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure SetPixelF(X, Y: Single; Value: TColor32);  {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetPixelFS(X, Y: Single; Value: TColor32);
    procedure SetPixelFW(X, Y: Single; Value: TColor32);

    procedure SetPixelX(X, Y: TFixed; Value: TColor32);
    procedure SetPixelXS(X, Y: TFixed; Value: TColor32);
    procedure SetPixelXW(X, Y: TFixed; Value: TColor32);
  public
    constructor Create(ABackendClass: TCustomBackendClass); reintroduce; overload; virtual;
    constructor Create; reintroduce; overload; virtual;
    constructor Create(Width, Height: Integer); reintroduce; overload; virtual;
    destructor Destroy; override;

    class function GetPlatformBackendClass: TCustomBackendClass; virtual;

    procedure Changed; overload; override;
    procedure Changed(const Area: TRect; const Info: Cardinal = AREAINFO_RECT); reintroduce; overload; virtual;

    procedure Assign(Source: TPersistent); override;
    function  BoundsRect: TRect;
    function  Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillColor: TColor32); overload;
    procedure Delete; override;

    procedure BeginMeasuring(const Callback: TAreaChangedEvent);
    procedure EndMeasuring;

    function ReleaseBackend: TCustomBackend;

    procedure PropertyChanged; virtual;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean = False); overload; virtual;
    procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion); overload; virtual;

    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; SaveTopDown: Boolean); overload; virtual;
    procedure SaveToFile(const FileName: string; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion); overload; virtual;

    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: TResourceType = RT_BITMAP);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ResType: TResourceType = RT_BITMAP);

    procedure ResetAlpha; overload;
    procedure ResetAlpha(const AlphaValue: Byte); overload;

    procedure Draw(DstX, DstY: Integer; Src: TCustomBitmap32); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TCustomBitmap32); overload;
    procedure Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32); overload;

    procedure SetPixelT(X, Y: Integer; Value: TColor32); overload;
    procedure SetPixelT(var Ptr: PColor32; Value: TColor32); overload;
    procedure SetPixelTS(X, Y: Integer; Value: TColor32);

    procedure DrawTo(Dst: TCustomBitmap32); overload;
    procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer); overload;
    procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TCustomBitmap32; const DstRect: TRect); overload;
    procedure DrawTo(Dst: TCustomBitmap32; const DstRect, SrcRect: TRect); overload;

    procedure SetStipple(NewStipple: TArrayOfColor32); overload;
    procedure SetStipple(NewStipple: array of TColor32); overload;
    procedure AdvanceStippleCounter(LengthPixels: Single);
    function  GetStippleColor: TColor32;

    procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTSP(X1, Y, X2: Integer);
    procedure HorzLineX(X1, Y, X2: TFixed; Value: TColor32);
    procedure HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);

    procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTSP(X, Y1, Y2: Integer);
    procedure VertLineX(X, Y1, Y2: TFixed; Value: TColor32);
    procedure VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);

    procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;
    procedure LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;

    property  PenColor: TColor32 read FPenColor write FPenColor;
    procedure MoveTo(X, Y: Integer);
    procedure LineToS(X, Y: Integer);
    procedure LineToTS(X, Y: Integer);
    procedure LineToAS(X, Y: Integer);
    procedure MoveToX(X, Y: TFixed);
    procedure MoveToF(X, Y: Single);
    procedure LineToXS(X, Y: TFixed);
    procedure LineToFS(X, Y: Single);
    procedure LineToXSP(X, Y: TFixed);
    procedure LineToFSP(X, Y: Single);
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property PenPosF: TFixedPoint read GetPenPosF write SetPenPosF;

    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FillRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTSP(X1, Y1, X2, Y2: Integer);
    procedure FrameRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FrameRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer); overload;
    procedure RaiseRectTS(const ARect: TRect; Contrast: Integer); overload;

    procedure Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);
    procedure FlipHorz(Dst: TCustomBitmap32 = nil);
    procedure FlipVert(Dst: TCustomBitmap32 = nil);
    procedure Rotate90(Dst: TCustomBitmap32 = nil);
    procedure Rotate180(Dst: TCustomBitmap32 = nil);
    procedure Rotate270(Dst: TCustomBitmap32 = nil);

    procedure ResetClipRect;

    property  Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
    property  PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
    property  PixelW[X, Y: Integer]: TColor32 read GetPixelW write SetPixelW;
    property  PixelX[X, Y: TFixed]: TColor32 read GetPixelX write SetPixelX;
    property  PixelXS[X, Y: TFixed]: TColor32 read GetPixelXS write SetPixelXS;
    property  PixelXW[X, Y: TFixed]: TColor32 read GetPixelXW write SetPixelXW;
    property  PixelF[X, Y: Single]: TColor32 read GetPixelF write SetPixelF;
    property  PixelFS[X, Y: Single]: TColor32 read GetPixelFS write SetPixelFS;
    property  PixelFW[X, Y: Single]: TColor32 read GetPixelFW write SetPixelFW;
    property  PixelFR[X, Y: Single]: TColor32 read GetPixelFR;
    property  PixelXR[X, Y: TFixed]: TColor32 read GetPixelXR;

    property Backend: TCustomBackend read FBackend write SetBackend;

{$IFDEF BITS_GETTER}
    property Bits: PColor32Array read GetBits;
{$ELSE}
    property Bits: PColor32Array read FBits;
{$ENDIF}

    property ClipRect: TRect read FClipRect write SetClipRect;
    property Clipping: Boolean read FClipping;

    property PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;
    property ScanLine[Y: Integer]: PColor32Array read GetScanLine;
    property StippleCounter: Single read FStippleCounter write FStippleCounter;
    property StippleStep: Single read FStippleStep write FStippleStep;

    property MeasuringMode: Boolean read FMeasuringMode;
  published
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write SetCombineMode default cmBlend;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode default wmClamp;
    property MasterAlpha: Cardinal read FMasterAlpha write SetMasterAlpha default $FF;
    property OuterColor: TColor32 read FOuterColor write FOuterColor default 0;
    property ResamplerClassName: string read GetResamplerClassName write SetResamplerClassName;
    property Resampler: TCustomResampler read FResampler write SetResampler;
    property OnChange;
    property OnPixelCombine: TPixelCombineEvent read FOnPixelCombine write FOnPixelCombine;
    property OnAreaChanged: TAreaChangedEvent read FOnAreaChanged write FOnAreaChanged;
    property OnResize;
  end;

  TCustomBitmap32Class = class of TCustomBitmap32;


  TBitmap32 = class(TCustomBitmap32)
  strict private
    FOnHandleChanged: TNotifyEvent;

    procedure FontChanged(Sender: TObject);
    procedure CanvasChanged(Sender: TObject);
    function GetCanvas: TCanvas;         {$IFDEF USEINLINING} inline; {$ENDIF}

    function GetBitmapInfo: TBitmapInfo; {$IFDEF USEINLINING} inline; {$ENDIF}
    function GetHandle: HBITMAP;         {$IFDEF USEINLINING} inline; {$ENDIF}
    function GetHDC: HDC;                {$IFDEF USEINLINING} inline; {$ENDIF}

    function GetFont: TFont;
    procedure SetFont(Value: TFont);
  strict protected
    procedure BackendChangedHandler(Sender: TObject); override;
    procedure BackendChangingHandler(Sender: TObject); override;
    procedure FinalizeBackend; override;
    procedure SetBackend(const ABackend: TCustomBackend); override;

    procedure HandleChanged; virtual;
    procedure CopyPropertiesTo(Dst: TCustomBitmap32); override;
  public
    class function GetPlatformBackendClass: TCustomBackendClass; override;

  {$IFDEF BCB}
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal); overload;
  {$ELSE}
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
  {$ENDIF}

{$IFDEF BCB}
    procedure DrawTo(hDst: Cardinal; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: Cardinal; const DstRect, SrcRect: TRect); overload;
    procedure TileTo(hDst: Cardinal; const DstRect, SrcRect: TRect; MaxTileSize: integer = 1024); overload;
{$ELSE}
    procedure DrawTo(hDst: HDC; DstX: Integer = 0; DstY: Integer = 0); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
    procedure TileTo(hDst: HDC; const DstRect, SrcRect: TRect; MaxTileSize: integer = 1024); overload;
{$ENDIF}

{$IFDEF COMPILER2009_UP}
    procedure DrawTo(Dst: TControlCanvas; DstX: Integer = 0; DstY: Integer = 0); overload;
    procedure DrawTo(Dst: TControlCanvas; const DstRect, SrcRect: TRect); overload;
    procedure TileTo(Dst: TControlCanvas; const DstRect, SrcRect: TRect; MaxTileSize: integer = 1024); overload;
{$ENDIF}

    procedure UpdateFont;
    procedure Textout(X, Y: Integer; const Text: string); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;
    function  TextHeight(const Text: string): Integer;
    function  TextWidth(const Text: string): Integer;
    procedure RenderText(X, Y: Integer; const Text: string; AALevel: Integer; Color: TColor32);

    property  Canvas: TCanvas read GetCanvas;
    function  CanvasAllocated: Boolean;
    procedure DeleteCanvas;

    property Font: TFont read GetFont write SetFont;

    property BitmapHandle: HBITMAP read GetHandle;
    property BitmapInfo: TBitmapInfo read GetBitmapInfo;
    property Handle: HDC read GetHDC;
  published
    property OnHandleChanged: TNotifyEvent read FOnHandleChanged write FOnHandleChanged;
  end;

  { TCustomBackend }
  { This class functions as backend for the TBitmap32 class.
    It manages and provides the backing buffer as well as OS or
    graphics subsystem specific features.}

  TCustomBackend = class(TThreadPersistent)
  strict protected
    FBits: PColor32Array;
    FOwner: TCustomBitmap32;
    FOnChanging: TNotifyEvent;
  protected

    procedure Changing; virtual;
    procedure SetOwner(AOwner: TCustomBitmap32);

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; virtual;
{$ENDIF}

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); virtual;
    procedure FinalizeSurface; virtual;
  public
    constructor Create; overload; override;
    constructor Create(Owner: TCustomBitmap32); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear; virtual;
    function Empty: Boolean; virtual;

    procedure ChangeSize(out Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True); virtual;

{$IFDEF BITS_GETTER}
    property Bits: PColor32Array read GetBits;
{$ELSE}
    property Bits: PColor32Array read FBits;
{$ENDIF}

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  { TCustomSampler }
  TCustomSampler = class(TNotifiablePersistent)
  public
    function GetSampleInt(X, Y: Integer): TColor32; virtual;
    function GetSampleFixed(X, Y: TFixed): TColor32; virtual;
    function GetSampleFloat(X, Y: TFloat): TColor32; virtual;
    procedure PrepareSampling; virtual;
    procedure FinalizeSampling; virtual;
    function HasBounds: Boolean; virtual;
    function GetSampleBounds: TFloatRect; virtual;
  end;


  TPixelAccessMode = (pamUnsafe, pamSafe, pamWrap, pamTransparentEdge);

  { TCustomResampler }
  { Base class for TCustomBitmap32 specific resamplers. }
  TCustomResampler = class(TCustomSampler)
  strict private
    FBitmap: TCustomBitmap32;
    FClipRect: TRect;
    FPixelAccessMode: TPixelAccessMode;
    procedure SetPixelAccessMode(const Value: TPixelAccessMode);
  strict protected
    function GetWidth: TFloat; virtual;
    procedure DoChanged; override;
    property ClipRect: TRect read FClipRect;
  protected
    procedure AssignTo(Dst: TPersistent); override;
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(ABitmap: TCustomBitmap32); overload; virtual;
    procedure PrepareSampling; override;
    function HasBounds: Boolean; override;
    function GetSampleBounds: TFloatRect; override;
    property Bitmap: TCustomBitmap32 read FBitmap write FBitmap;
    property Width: TFloat read GetWidth;
  published
    property PixelAccessMode: TPixelAccessMode read FPixelAccessMode write SetPixelAccessMode default pamSafe;
  end;
  TCustomResamplerClass = class of TCustomResampler;

var
  StockBitmap: TBitmap;

var
  // Default file format version of BMPs written by SaveToStream/SaveToFile.
  DefaultBitmapHeaderVersion: TCustomBitmap32.TInfoHeaderVersion = InfoHeaderVersion4;

resourcestring
  RCStrUnmatchedReferenceCounting = 'Unmatched reference counting.';
  RCStrCannotSetSize = 'Can''t set size from ''%s''';
  RCStrInpropriateBackend = 'Inappropriate Backend';

implementation

uses
  Math,
  Clipbrd,
  GR32_Blend,
  GR32_LowLevel,
  GR32_Math,
  GR32_Resamplers,
  GR32_Containers,
  GR32_Gamma,
  GR32_Clipboard,
  GR32_Backends,
  GR32_Backends_Generic,
{$IFDEF FPC}
  {$IFDEF LCLWin32}
    GR32_Backends_LCL_Win,
  {$ENDIF}
  {$IF defined(LCLGtk) or defined(LCLGtk2)}
    GR32_Backends_LCL_Gtk,
  {$IFEND}
  {$IFDEF LCLCarbon}
    GR32_Backends_LCL_Carbon,
  {$ENDIF}
  {$IFDEF LCLCustomDrawn}
    GR32_Backends_LCL_CustomDrawn,
  {$ENDIF}
{$ELSE}
  GR32_Backends_VCL,
{$ENDIF}
  GR32.ImageFormats,
  GR32.ImageFormats.Default;

type
  { We can not use the Win32 defined records and constants here since we are cross-platform. }

  // Bitmap file and info headers.
  // See : https://docs.microsoft.com/en-us/windows/win32/gdi/bitmap-header-types

  // BMP file header
  TBitmapFileHeader = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;

  // OS/2 v1 header: BITMAPCOREHEADER
  // Note that this is the only header that isn't compatible with the V1 header
  TBitmapCoreHeader = packed record
    bcSize: DWORD;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;

  // V1 header: BITMAPINFOHEADER
  // All versions after BITMAPINFOHEADER are supersets of BITMAPINFOHEADER.
  // Each new version adds to the one before it.
  TBitmapInfoHeader = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;

  // OS/2 v2 header - all fields are optional: variable header size!
  TBitmapCoreHeader2 = packed record
    InfoHeader: TBitmapInfoHeader;
    bc2ResUnit: WORD;
    bc2Reserved: WORD;
    bc2Orientation: WORD;
    bc2Halftoning: WORD;
    bc2HalftoneSize1: DWORD;
    bc2HalftoneSize2: DWORD;
    bc2ColorSpace: DWORD;
    bc2AppData: DWORD;
  end;

  // Undocumented V2 header
  TBitmapV2Header = packed record
    InfoHeader: TBitmapInfoHeader;
    bV2RedMask: DWORD;
    bV2GreenMask: DWORD;
    bV2BlueMask: DWORD;
  end;

  // Undocumented V3 header
  TBitmapV3Header = packed record
    InfoHeader: TBitmapInfoHeader;
    bV3RedMask: DWORD;
    bV3GreenMask: DWORD;
    bV3BlueMask: DWORD;
    bV3AlphaMask: DWORD;
  end;

  // V4 header: BITMAPV4HEADER
  TBitmapV4Header = packed record
    bV4Size: DWORD;
    bV4Width: Longint;
    bV4Height: Longint;
    bV4Planes: Word;
    bV4BitCount: Word;
    bV4V4Compression: DWORD;
    bV4SizeImage: DWORD;
    bV4XPelsPerMeter: Longint;
    bV4YPelsPerMeter: Longint;
    bV4ClrUsed: DWORD;
    bV4ClrImportant: DWORD;
    bV4RedMask: DWORD;
    bV4GreenMask: DWORD;
    bV4BlueMask: DWORD;
    bV4AlphaMask: DWORD;
    bV4CSType: DWORD;
    bV4Endpoints: array[0..35] of byte; // TCIEXYZTriple
    bV4GammaRed: DWORD;
    bV4GammaGreen: DWORD;
    bV4GammaBlue: DWORD;
  end;

  // V5 header: BITMAPV5HEADER
  TBitmapV5Header = packed record
    bV5Size: DWORD;
    bV5Width: Longint;
    bV5Height: Longint;
    bV5Planes: Word;
    bV5BitCount: Word;
    bV5Compression: DWORD;
    bV5SizeImage: DWORD;
    bV5XPelsPerMeter: Longint;
    bV5YPelsPerMeter: Longint;
    bV5ClrUsed: DWORD;
    bV5ClrImportant: DWORD;
    bV5RedMask: DWORD;
    bV5GreenMask: DWORD;
    bV5BlueMask: DWORD;
    bV5AlphaMask: DWORD;
    bV5CSType: DWORD;
    bV5Endpoints: array[0..35] of byte; // TCIEXYZTriple
    bV5GammaRed: DWORD;
    bV5GammaGreen: DWORD;
    bV5GammaBlue: DWORD;
    bV5Intent: DWORD;
    bV5ProfileData: DWORD;
    bV5ProfileSize: DWORD;
    bV5Reserved: DWORD;
  end;

  // Just a TBitmapInfoHeader with easy access to the color masks
  TBitmapInfoEx = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..3] of DWORD;
  end;

  TBitmapHeader = packed record
    FileHeader: TBitmapFileHeader;
    case integer of
    0: (CoreHeader: TBitmapCoreHeader);         // 12 - a.k.a. OS/2 v1
    1: (InfoHeader: TBitmapInfoHeader);         // 40
    2: (V2Header: TBitmapV2Header);             // 52
    3: (V3Header: TBitmapV3Header);             // 56
    4: (V4Header: TBitmapV4Header);             // 108
    5: (V5Header: TBitmapV5Header);             // 124
    6: (CoreHeader2: TBitmapCoreHeader2);       // 16..64
    -1: (Header: TBitmapInfoEx);
  end;

const
  BI_RGB = 0;
  BI_BITFIELDS = 3;

type
  TGraphicAccess = class(TGraphic);

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{ Color construction and conversion functions }

function Color32(WinColor: TColor): TColor32; overload;
begin
  if (WinColor < 0) then
    WinColor := GetSysColor(WinColor and $000000FF);

{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
  {$IFNDEF RGBA_FORMAT}
  Result := $FF000000 or ((TColor32(WinColor) and $FF0000) shr 16) or (TColor32(WinColor) and $FF00) or ((TColor32(WinColor) and $FF) shl 16);
  {$ELSE RGBA_FORMAT}
  Result := $FF000000 or (TColor32(WinColor) and $FFFFFF);
  {$ENDIF RGBA_FORMAT}
{$ELSE}
  asm
        MOV     EAX,WinColor
        BSWAP   EAX
        MOV     AL,$FF
        ROR     EAX,8
        MOV     Result,EAX
  end;
{$IFEND}
end;

function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
begin
  {$IFNDEF RGBA_FORMAT}
  Result := (A shl 24) or (R shl 16) or (G shl  8) or B;
  {$ELSE RGBA_FORMAT}
  Result := (A shl 24) or (B shl 16) or (G shl  8) or R;
  {$ENDIF RGBA_FORMAT}
{$ELSE}
  asm
        MOV     AH, A
        SHL     EAX, 16
        MOV     AH, DL
        MOV     AL, CL
{$IFEND}
end;

function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
begin
  Result := Palette[Index];
end;

function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;
begin
  Result := TColor32(Alpha) shl 24 + TColor32(Intensity) shl 16 +
    TColor32(Intensity) shl 8 + TColor32(Intensity);
end;

function WinColor(Color32: TColor32): TColor;
{$IFDEF RGBA_FORMAT}
begin
  Result := Color32 and $00FFFFFF;
end;
{$ELSE RGBA_FORMAT}
{$IF Defined(PUREPASCAL)}
begin
  Result := ((Color32 and $00FF0000) shr 16) or
             (Color32 and $0000FF00) or
            ((Color32 and $000000FF) shl 16);
{$ELSE}
{$IFDEF FPC}assembler; nostackframe;{$ENDIF}
  asm
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
{$ENDIF}
        // the alpha channel byte is set to zero!
        ROL     EAX, 8  // ABGR  ->  BGRA
        XOR     AL, AL  // BGRA  ->  BGR0
        BSWAP   EAX     // BGR0  ->  0RGB
{$IFEND}
end;
{$ENDIF RGBA_FORMAT}

function ArrayOfColor32(const Colors: array of TColor32): TArrayOfColor32;
var
  L: Integer;
begin
  // build a dynamic color array from specified colors
  L := Length(Colors);
  SetLength(Result, L);
  MoveLongword(Colors[0], Result[0], L);
end;

procedure SwapRedBlueMem(var Color32: TColor32);
{$IF Defined(PUREPASCAL)}
var
  Temp: Byte;
begin
  Temp := TColor32Entry(Color32).R;
  TColor32Entry(Color32).R := TColor32Entry(Color32).B;
  TColor32Entry(Color32).B := Temp;
{$ELSE}
  asm
        MOV     EDX, DWORD PTR [Color32]
        ROL     EDX, 8  // ARGB  ->  RGBA
        BSWAP   EDX     // RGBA  ->  ABGR
        MOV     DWORD PTR [Color32], EDX
{$IFEND}
end;

function SwapRedBlue(Color32: TColor32): TColor32;
{$IF Defined(PUREPASCAL)}
begin
  Result := Color32;
  SwapRedBlueMem(Result);
{$ELSE}
  asm
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
{$ENDIF}
        ROL     EAX, 8  // ARGB  ->  RGBA
        BSWAP   EAX     // RGBA  ->  ABGR
{$IFEND}
end;

function Color32ToBGRA(Color32: TColor32): DWORD;
begin
{$IFNDEF RGBA_FORMAT}
  Result := Color32;
{$ELSE RGBA_FORMAT}
  Result := SwapRedBlue(Color32);
{$ENDIF RGBA_FORMAT}
end;

procedure Color32ToBGRAMem(var Color32: TColor32);
begin
{$IFDEF RGBA_FORMAT}
  SwapRedBlueMem(Color32);
{$ENDIF RGBA_FORMAT}
end;

function Color32ToRGBA(Color32: TColor32): DWORD;
begin
{$IFDEF RGBA_FORMAT}
  Result := Color32;
{$ELSE RGBA_FORMAT}
  Result := SwapRedBlue(Color32);
{$ENDIF RGBA_FORMAT}
end;

procedure Color32ToRGBAMem(var Color32: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
{$IFNDEF RGBA_FORMAT}
  SwapRedBlueMem(Color32);
{$ENDIF RGBA_FORMAT}
end;

function BGRAToColor32(BGRA: DWORD): TColor32;
begin
{$IFNDEF RGBA_FORMAT}
  Result := BGRA;
{$ELSE RGBA_FORMAT}
  Result := SwapRedBlue(BGRA);
{$ENDIF RGBA_FORMAT}
end;

procedure BGRAToColor32Mem(var BGRA: DWORD); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
{$IFDEF RGBA_FORMAT}
  SwapRedBlueMem(TColor32(BGRA));
{$ENDIF RGBA_FORMAT}
end;

function RGBAToColor32(RGBA: DWORD): TColor32;
begin
{$IFDEF RGBA_FORMAT}
  Result := RGBA;
{$ELSE RGBA_FORMAT}
  Result := SwapRedBlue(RGBA);
{$ENDIF RGBA_FORMAT}
end;

procedure RGBAToColor32mem(var RGBA: DWORD); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
{$IFNDEF RGBA_FORMAT}
  SwapRedBlueMem(TColor32(RGBA));
{$ENDIF RGBA_FORMAT}
end;

procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
begin
{$IFNDEF RGBA_FORMAT}
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := (Color32 and $000000FF);
{$ELSE RGBA_FORMAT}
  R := (Color32 and $000000FF);
  G := (Color32 and $0000FF00) shr 8;
  B := (Color32 and $00FF0000) shr 16;
{$ENDIF RGBA_FORMAT}
end;

procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);
begin
{$IFNDEF RGBA_FORMAT}
  A := Color32 shr 24;
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := (Color32 and $000000FF);
{$ELSE RGBA_FORMAT}
  A := Color32 shr 24;
  R := (Color32 and $000000FF);
  G := (Color32 and $0000FF00) shr 8;
  B := (Color32 and $00FF0000) shr 16;
{$ENDIF RGBA_FORMAT}
end;

function Color32Components(R, G, B, A: Boolean): TColor32Components;
const
  ccR : array[Boolean] of TColor32Components = ([], [ccRed]);
  ccG : array[Boolean] of TColor32Components = ([], [ccGreen]);
  ccB : array[Boolean] of TColor32Components = ([], [ccBlue]);
  ccA : array[Boolean] of TColor32Components = ([], [ccAlpha]);
begin
  Result := ccR[R] + ccG[G] + ccB[B] + ccA[A];
end;

function RedComponent(Color32: TColor32): Integer;
begin
{$IFNDEF RGBA_FORMAT}
  Result := (Color32 and $00FF0000) shr 16;
{$ELSE RGBA_FORMAT}
  Result := Color32 and $000000FF;
{$ENDIF RGBA_FORMAT}
end;

function GreenComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $0000FF00) shr 8;
end;

function BlueComponent(Color32: TColor32): Integer;
begin
{$IFNDEF RGBA_FORMAT}
  Result := Color32 and $000000FF;
{$ELSE RGBA_FORMAT}
  Result := (Color32 and $00FF0000) shr 16;
{$ENDIF RGBA_FORMAT}
end;

function AlphaComponent(Color32: TColor32): Integer;
begin
  Result := Color32 shr 24;
end;

function Intensity(Color32: TColor32): Integer;
begin
  // (R * 61 + G * 174 + B * 21) / 256
{$IFNDEF RGBA_FORMAT}
  Result := (
    (Color32 and $00FF0000) shr 16 * 61 +
    (Color32 and $0000FF00) shr 8 * 174 +
    (Color32 and $000000FF) * 21
    ) shr 8;
{$ELSE RGBA_FORMAT}
  Result := (
    (Color32 and $000000FF) * 61 +
    (Color32 and $0000FF00) shr 8 * 174 +
    (Color32 and $00FF0000) shr 16 * 21
    ) shr 8;
{$ENDIF RGBA_FORMAT}
end;

function InvertColor(Color32: TColor32): TColor32;
begin
  TColor32Entry(Result).R := $FF - TColor32Entry(Color32).R;
  TColor32Entry(Result).G := $FF - TColor32Entry(Color32).G;
  TColor32Entry(Result).B := $FF - TColor32Entry(Color32).B;
  TColor32Entry(Result).A := TColor32Entry(Color32).A;
end;

function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;
begin
  if NewAlpha < 0 then
    NewAlpha := 0
  else
  if NewAlpha > $FF then
    NewAlpha := $FF;
  Result := (Color32 and $00FFFFFF) or (TColor32(NewAlpha) shl 24);
end;

procedure ModifyAlpha(var Color32: TColor32; NewAlpha: Byte);
begin
  TColor32Entry(Color32).A := NewAlpha;
end;

procedure ScaleAlpha(var Color32: TColor32; Scale: Single);
begin
  TColor32Entry(Color32).A := Round(Scale * TColor32Entry(Color32).A);
end;

{ Color space conversions }

function HSLtoRGB(H, S, L: Single; A: Integer): TColor32;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := M2
    else
    if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 * OneOverThree - Hue) * 6
    else
      V := M1;
    Result := Round($FF * V);
  end;

begin
  if S = 0 then
  begin
    Result := Gray32(Round($FF * L), A);
    Exit;
  end;

  if L <= 0.5 then
    M2 := L * (1 + S)
  else
    M2 := L + S - L * S;
  M1 := 2 * L - M2;
  Result := Color32(
    HueToColor(H + OneOverThree),
    HueToColor(H),
    HueToColor(H - OneOverThree),
    A);
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);
const
  // reciprocal mul. opt.
  R6 = 1 / 6;
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := RedComponent(RGB) * COne255th;
  G := GreenComponent(RGB) * COne255th;
  B := BlueComponent(RGB) * COne255th;

  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) * 0.5;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);

    if R = Cmax then
      H := (G - B) / D
    else
    if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;

    H := H * R6;

    if H < 0 then
      H := H + 1
  end;
end;

function HSLtoRGB(H, S, L, A: Integer): TColor32;
var
  V, M, M1, M2, VSF: Integer;
begin
  if L <= $7F then
    V := L * (256 + S) shr 8
  else
    V := L + S - Integer(Div255(L * S));
  if V <= 0 then
    Result := Gray32(L, A)
  else
  begin
    M := L * 2 - V;
    H := H * 6;
    VSF := (V - M) * (H and $FF) shr 8;
    M1 := M + VSF;
    M2 := V - VSF;
    case H shr 8 of
      0: Result := Color32(V, M1, M, A);
      1: Result := Color32(M2, V, M, A);
      2: Result := Color32(M, V, M1, A);
      3: Result := Color32(M, M2, V, A);
      4: Result := Color32(M1, M, V, A);
      5: Result := Color32(V, M, M2, A);
    else
      Result := 0;
    end;
  end;
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte);
var
  R, G, B, D, Cmax, Cmin, HL: Integer;
begin
  R := RedComponent(RGB);
  G := GreenComponent(RGB);
  B := BlueComponent(RGB);

  Cmax := Max(R, G, B);
  Cmin := Min(R, G, B);
  L := (Cmax + Cmin) shr 1;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end else
  begin
    D := (Cmax - Cmin) * $FF;
    if L <= $7F then
      S := D div (Cmax + Cmin)
    else
      S := D div ($FF * 2 - Cmax - Cmin);

    D := D * 6;
    if R = Cmax then
      HL := (G - B) * $FF * $FF div D
    else
    if G = Cmax then
      HL := $FF * 2 div 6 + (B - R) * $FF * $FF div D
    else
      HL := $FF * 4 div 6 + (R - G) * $FF * $FF div D;

    if HL < 0 then
      HL := HL + $FF * 2;

    H := HL;
  end;
end;

function HSVtoRGB(H, S, V: Single; A: Integer): TColor32;
var
  Fraction: Single;
  Sel, Q, P: Integer;
begin
  V := 255 * V;

  if S = 0 then
  begin
    Result := Gray32(Trunc(V), A);
    Exit;
  end;

  H := (H - Floor(H)) * 6; // 0 <= H < 6
  Fraction := H - Floor(H);

  Sel := Trunc(H);
  if (Sel mod 2) = 0 then
    Fraction := 1 - Fraction;

  P := Round(V * (1 - S));
  Q := Round(V * (1 - S * Fraction));

  case Sel of
    0: Result := Color32(Trunc(V), Q, P, A);
    1: Result := Color32(Q, Trunc(V), P, A);
    2: Result := Color32(P, Trunc(V), Q, A);
    3: Result := Color32(P, Q, Trunc(V), A);
    4: Result := Color32(Q, P, Trunc(V), A);
    5: Result := Color32(Trunc(V), P, Q, A);
  else
    Result := Gray32(0, A);
  end;
end;

procedure RGBToHSV(Color: TColor32; out H, S, V: Single);
var
  Delta, RGBMin, RGBMax: integer;
  R, G, B: Integer;
const
  COneSixth = 1 / 6;
begin
  R := RedComponent(Color);
  G := GreenComponent(Color);
  B := BlueComponent(Color);

  RGBMin := Min(R, Min(G, B));
  RGBMax := Max(R, Max(G, B));
  V := RGBMax * COne255th;

  Delta := RGBMax - RGBMin;
  if RGBMax = 0 then
    S := 0
  else
    S := Delta / RGBMax;

  if S = 0.0 then
    H := 0
  else
  begin
    if R = RGBMax then
      H := COneSixth * (G - B) / Delta
    else
    if G = RGBMax then
      H := COneSixth * (2 + (B - R) / Delta)
    else
    if B = RGBMax then
      H := COneSixth * (4 + (R - G) / Delta);

    if H < 0.0 then
      H := H + 1;
  end;
end;

{ Palette conversion }

{$IFNDEF RGBA_FORMAT}
function WinPalette(const P: TPalette32): HPALETTE;
var
  L: TMaxLogPalette;
  L0: LOGPALETTE absolute L;
  I: Cardinal;
  Cl: TColor32;
begin
  L.palVersion := $300;
  L.palNumEntries := 256;
  for I := 0 to $FF do
  begin
    Cl := P[I];
    with L.palPalEntry[I] do
    begin
      peFlags := 0;
      peRed := RedComponent(Cl);
      peGreen := GreenComponent(Cl);
      peBlue := BlueComponent(Cl);
    end;
  end;
  Result := CreatePalette(l0);
end;
{$ENDIF RGBA_FORMAT}


{ Fixed-point conversion routines }

function Fixed(S: Single): TFixed;
begin
  Result := Round(S * FixedOne);
end;

function Fixed(I: Integer): TFixed;
begin
  Result := I shl 16;
end;


{ Points }

function Point(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Point(const FP: TFloatPoint): TPoint;
begin
  Result.X := Round(FP.X);
  Result.Y := Round(FP.Y);
end;

function Point(const FXP: TFixedPoint): TPoint;
begin
  Result.X := FixedRound(FXP.X);
  Result.Y := FixedRound(FXP.Y);
end;

function FloatPoint(X, Y: Single): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function FloatPoint(const P: TPoint): TFloatPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function FloatPoint(const FXP: TFixedPoint): TFloatPoint;
begin
  with FXP do
  begin
    Result.X := X * FixedToFloat;
    Result.Y := Y * FixedToFloat;
  end;
end;

{$IFDEF SUPPORT_ENHANCED_RECORDS}
{$IFNDEF FPC}
constructor TFloatPoint.Create(P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

{$IFDEF COMPILERXE2_UP}
constructor TFloatPoint.Create(P: TPointF);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;
{$ENDIF}

constructor TFloatPoint.Create(X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TFloatPoint.Create(X, Y: TFloat);
begin
  Self.X := X;
  Self.Y := Y;
end;
{$ENDIF}

// operator overloads
class operator TFloatPoint.Equal(const Lhs, Rhs: TFloatPoint): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TFloatPoint.NotEqual(const Lhs, Rhs: TFloatPoint): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TFloatPoint.Add(const Lhs, Rhs: TFloatPoint): TFloatPoint;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TFloatPoint.Subtract(const Lhs, Rhs: TFloatPoint): TFloatPoint;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

{$IFDEF COMPILERXE2_UP}
class operator TFloatPoint.Explicit(A: TPointF): TFloatPoint;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

class operator TFloatPoint.Implicit(A: TPointF): TFloatPoint;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;
{$ENDIF}

class function TFloatPoint.Zero: TFloatPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

{$IFNDEF FPC}
{$IFDEF COMPILERXE2_UP}
constructor TFixedPoint.Create(P: TPointF);
begin
  Self.X := Fixed(P.X);
  Self.Y := Fixed(P.Y);
end;
{$ENDIF}

constructor TFixedPoint.Create(P: TFloatPoint);
begin
  Self.X := Fixed(P.X);
  Self.Y := Fixed(P.Y);
end;

constructor TFixedPoint.Create(X, Y: TFixed);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TFixedPoint.Create(X, Y: Integer);
begin
  Self.X := Fixed(X);
  Self.Y := Fixed(Y);
end;

constructor TFixedPoint.Create(X, Y: TFloat);
begin
  Self.X := Fixed(X);
  Self.Y := Fixed(Y);
end;
{$ENDIF}

// operator overloads
class operator TFixedPoint.Equal(const Lhs, Rhs: TFixedPoint): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TFixedPoint.NotEqual(const Lhs, Rhs: TFixedPoint): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TFixedPoint.Add(const Lhs, Rhs: TFixedPoint): TFixedPoint;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TFixedPoint.Subtract(const Lhs, Rhs: TFixedPoint): TFixedPoint;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

class function TFixedPoint.Zero: TFixedPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;
{$ENDIF}

function FixedPoint(X, Y: Integer): TFixedPoint; overload;
begin
  Result.X := X shl 16;
  Result.Y := Y shl 16;
end;

function FixedPoint(X, Y: Single): TFixedPoint; overload;
begin
  Result.X := Round(X * FixedOne);
  Result.Y := Round(Y * FixedOne);
end;

function FixedPoint(const P: TPoint): TFixedPoint; overload;
begin
  Result.X := P.X shl 16;
  Result.Y := P.Y shl 16;
end;

function FixedPoint(const FP: TFloatPoint): TFixedPoint; overload;
begin
  Result.X := Round(FP.X * FixedOne);
  Result.Y := Round(FP.Y * FixedOne);
end;


{ Rectangles }

function MakeRect(const L, T, R, B: Integer): TRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function MakeRect(const FR: TFloatRect; Rounding: TRectRounding): TRect;
begin
  with FR do
    case Rounding of
      rrClosest:
        begin
          Result.Left := Round(Left);
          Result.Top := Round(Top);
          Result.Right := Round(Right);
          Result.Bottom := Round(Bottom);
        end;

      rrInside:
        begin
          Result.Left := Ceil(Left);
          Result.Top := Ceil(Top);
          Result.Right := Floor(Right);
          Result.Bottom := Floor(Bottom);
          if Result.Right < Result.Left then Result.Right := Result.Left;
          if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
        end;

      rrOutside:
        begin
          Result.Left := Floor(Left);
          Result.Top := Floor(Top);
          Result.Right := Ceil(Right);
          Result.Bottom := Ceil(Bottom);
        end;
    end;
end;

function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding): TRect;
begin
  with FXR do
    case Rounding of
      rrClosest:
        begin
          Result.Left := FixedRound(Left);
          Result.Top := FixedRound(Top);
          Result.Right := FixedRound(Right);
          Result.Bottom := FixedRound(Bottom);
        end;

      rrInside:
        begin
          Result.Left := FixedCeil(Left);
          Result.Top := FixedCeil(Top);
          Result.Right := FixedFloor(Right);
          Result.Bottom := FixedFloor(Bottom);
          if Result.Right < Result.Left then Result.Right := Result.Left;
          if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
        end;

      rrOutside:
        begin
          Result.Left := FixedFloor(Left);
          Result.Top := FixedFloor(Top);
          Result.Right := FixedCeil(Right);
          Result.Bottom := FixedCeil(Bottom);
        end;
    end;
end;

function FixedRect(const L, T, R, B: TFixed): TFixedRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function FixedRect(const TopLeft, BottomRight: TFixedPoint): TFixedRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

function FixedRect(const ARect: TRect): TFixedRect;
begin
  with Result do
  begin
    Left := ARect.Left shl 16;
    Top := ARect.Top shl 16;
    Right := ARect.Right shl 16;
    Bottom := ARect.Bottom shl 16;
  end;
end;

function FixedRect(const FR: TFloatRect): TFixedRect;
begin
  with Result do
  begin
    Left := Round(FR.Left * 65536);
    Top := Round(FR.Top * 65536);
    Right := Round(FR.Right * 65536);
    Bottom := Round(FR.Bottom * 65536);
  end;
end;

function FloatRect(const L, T, R, B: TFloat): TFloatRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function FloatRect(const TopLeft, BottomRight: TFloatPoint): TFloatRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

function FloatRect(const ARect: TRect): TFloatRect;
begin
  with Result do
  begin
    Left := ARect.Left;
    Top := ARect.Top;
    Right := ARect.Right;
    Bottom := ARect.Bottom;
  end;
end;

function FloatRect(const FXR: TFixedRect): TFloatRect;
begin
  with Result do
  begin
    Left := FXR.Left * FixedToFloat;
    Top := FXR.Top * FixedToFloat;
    Right := FXR.Right * FixedToFloat;
    Bottom := FXR.Bottom * FixedToFloat;
  end;
end;

function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean;
begin
  if R1.Left >= R2.Left then Dst.Left := R1.Left else Dst.Left := R2.Left;
  if R1.Right <= R2.Right then Dst.Right := R1.Right else Dst.Right := R2.Right;
  if R1.Top >= R2.Top then Dst.Top := R1.Top else Dst.Top := R2.Top;
  if R1.Bottom <= R2.Bottom then Dst.Bottom := R1.Bottom else Dst.Bottom := R2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then Dst := ZERO_RECT;
end;

function IntersectRect(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean;
begin
  Dst.Left   := Math.Max(FR1.Left,   FR2.Left);
  Dst.Right  := Math.Min(FR1.Right,  FR2.Right);
  Dst.Top    := Math.Max(FR1.Top,    FR2.Top);
  Dst.Bottom := Math.Min(FR1.Bottom, FR2.Bottom);
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then FillLongword(Dst, 4, 0);
end;

function IsRectEmpty(const R: TRect): Boolean;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

function IsRectEmpty(const FR: TFloatRect): Boolean;
begin
  Result := (FR.Right <= FR.Left) or (FR.Bottom <= FR.Top);
end;

function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;
begin
  Rect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then Rect.Left := R2.Left;
    if R2.Top < R1.Top then Rect.Top := R2.Top;
    if R2.Right > R1.Right then Rect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Rect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(Rect);
  if not Result then Rect := ZERO_RECT;
end;

function UnionRect(out Rect: TFloatRect; const R1, R2: TFloatRect): Boolean;
begin
  Rect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then Rect.Left := R2.Left;
    if R2.Top < R1.Top then Rect.Top := R2.Top;
    if R2.Right > R1.Right then Rect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Rect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(Rect);
  if not Result then FillLongword(Rect, 4, 0);
end;

function EqualRect(const R1, R2: TRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TRect));
end;

function EqualRect(const R1, R2: TFloatRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TFloatRect));
end;

function EqualRectSize(const R1, R2: TRect): Boolean;
begin
  Result := ((R1.Right - R1.Left) = (R2.Right - R2.Left)) and
    ((R1.Bottom - R1.Top) = (R2.Bottom - R2.Top));
end;

function EqualRectSize(const R1, R2: TFloatRect): Boolean;
var
  _R1: TFixedRect;
  _R2: TFixedRect;
begin
  _R1 := FixedRect(R1);
  _R2 := FixedRect(R2);
  Result := ((_R1.Right - _R1.Left) = (_R2.Right - _R2.Left)) and
    ((_R1.Bottom - _R1.Top) = (_R2.Bottom - _R2.Top));
end;

procedure InflateRect(var R: TRect; Dx, Dy: Integer);
begin
  Dec(R.Left, Dx); Dec(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure InflateRect(var FR: TFloatRect; Dx, Dy: TFloat);
begin
  with FR do
  begin
    Left := Left - Dx; Top := Top - Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

procedure OffsetRect(var R: TRect; Dx, Dy: Integer);
begin
  Inc(R.Left, Dx); Inc(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure OffsetRect(var FR: TFloatRect; Dx, Dy: TFloat);
begin
  with FR do
  begin
    Left := Left + Dx; Top := Top + Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

function PtInRect(const R: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TFloatRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TRect; const P: TFloatPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TFloatRect; const P: TFloatPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

{ TSimpleInterfacedPersistent }

function TPlainInterfacedPersistent._AddRef: Integer;
begin
  if FRefCounted then
    Result := InterlockedIncrement(FRefCount)
  else
    Result := -1;
end;

function TPlainInterfacedPersistent._Release: Integer;
begin
  if FRefCounted then
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end
  else
    Result := -1;
end;

function TPlainInterfacedPersistent.QueryInterface(
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TPlainInterfacedPersistent.AfterConstruction;
begin
  inherited;

  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TPlainInterfacedPersistent.BeforeDestruction;
begin
  if RefCounted and (RefCount <> 0) then
    raise Exception.Create(RCStrUnmatchedReferenceCounting);

  inherited;
end;

class function TPlainInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;

  // Set an implicit refcount so that refcounting
  // during construction won't destroy the object.
  TPlainInterfacedPersistent(Result).FRefCount := 1;
end;


{ TNotifiablePersistent }

procedure TNotifiablePersistent.Beforedestruction;
begin
  inherited;
  Inc(FLockUpdateCount);
  Inc(FUpdateCount);
end;

procedure TNotifiablePersistent.BeginLockUpdate;
begin
  Inc(FLockUpdateCount);
end;

procedure TNotifiablePersistent.EndLockUpdate;
begin
  Dec(FLockUpdateCount);
end;

procedure TNotifiablePersistent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TNotifiablePersistent.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TThreadPersistent.EndUpdate');
  if (FUpdateCount = 1) and (FModified) then
  begin
    DoChanged;
    FModified := False;
  end;
  Dec(FUpdateCount);
end;

procedure TNotifiablePersistent.Changed;
begin
  if (FLockUpdateCount > 0) then
    exit;
  BeginUpdate;
  FModified := True;
  EndUpdate;
end;

procedure TNotifiablePersistent.DoChanged;
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;


{ TThreadPersistent }

constructor TThreadPersistent.Create;
begin
  InitializeCriticalSection(FLock);
end;

destructor TThreadPersistent.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TThreadPersistent.Lock;
begin
  InterlockedIncrement(FLockCount);
  EnterCriticalSection(FLock);
end;

procedure TThreadPersistent.Unlock;
begin
  LeaveCriticalSection(FLock);
  InterlockedDecrement(FLockCount);
end;


{ TCustomMap }

constructor TCustomMap.Create(Width, Height: Integer);
begin
  inherited Create;
  SetSize(Width, Height);
end;

destructor TCustomMap.Destroy;
begin
  FOnResize := nil;
  inherited;
end;

procedure TCustomMap.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TCustomMap.Delete;
begin
  SetSize(0, 0);
end;

function TCustomMap.Empty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

procedure TCustomMap.Resized;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TCustomMap.SetHeight(NewHeight: Integer);
begin
  SetSize(Width, NewHeight);
end;

function TCustomMap.SetSize(NewWidth, NewHeight: Integer): Boolean;
begin
  if NewWidth < 0 then
    NewWidth := 0;
  if NewHeight < 0 then
    NewHeight := 0;

  Result := (NewWidth <> FWidth) or (NewHeight <> FHeight);

  if Result then
  begin
    BeginUpdate;
    try
      ChangeSize(FWidth, FHeight, NewWidth, NewHeight);
      Changed;
    finally
      EndUpdate;
    end;
    Resized;
  end;
end;

function TCustomMap.SetSizeFrom(Source: TPersistent): Boolean;
begin
  if Source is TCustomMap then
    Result := SetSize(TCustomMap(Source).Width, TCustomMap(Source).Height)
  else
  if Source is TGraphic then
    Result := SetSize(TGraphic(Source).Width, TGraphic(Source).Height)
  else
  if Source is TControl then
    Result := SetSize(TControl(Source).Width, TControl(Source).Height)
  else
  if Source = nil then
    Result := SetSize(0, 0)
  else
    raise Exception.CreateFmt(RCStrCannotSetSize, [Source.ClassName]);
end;

procedure TCustomMap.SetWidth(NewWidth: Integer);
begin
  SetSize(NewWidth, Height);
end;


{ TCustomBitmap32 }

constructor TCustomBitmap32.Create(ABackendClass: TCustomBackendClass);
begin
  inherited Create;

  InitializeBackend(ABackendClass);

  FOuterColor := $00000000;  // by default as full transparency black

  FMasterAlpha := $FF;
  FPenColor := clWhite32;
  FStippleStep := 1;
  FCombineMode := cmBlend;
  BlendProc := @BLEND_MEM[FCombineMode]^;
  WrapProcHorz := GetWrapProcEx(WrapMode);
  WrapProcVert := GetWrapProcEx(WrapMode);
  FResampler := TNearestResampler.Create(Self);
end;

constructor TCustomBitmap32.Create;
begin
  Create(GetPlatformBackendClass);
end;

destructor TCustomBitmap32.Destroy;
begin
  BeginLockUpdate;
  Lock;
  try
    FOnAreaChanged := nil; // Avoid notification during destruction
    SetSize(0, 0);
    FResampler.Free;
    FinalizeBackend;
  finally
    Unlock;
  end;
  inherited;
end;

procedure TCustomBitmap32.InitializeBackend(ABackendClass: TCustomBackendClass);
begin
  ABackendClass.Create(Self);
end;

procedure TCustomBitmap32.FinalizeBackend;
begin
  // Drop ownership of backend now:
  // It's a zombie now.
  FBackend.SetOwner(nil);
  FBackend.OnChange := nil;
  FBackend.OnChanging := nil;

  (*
  Release our reference to the backend

  Note: The backend won't necessarily be freed immediately.

  This is required to circumvent a problem with the magic procedure cleanup
  of interfaces that have ref-counting forcefully disabled:

  Quality Central report #9157 and #9500:
  http://qc.codegear.com/wc/qcmain.aspx?d=9157
  http://qc.codegear.com/wc/qcmain.aspx?d=9500

  if any backend interface is used within the same procedure in which
  the owner bitmap is also freed, the magic procedure cleanup will
  clear that particular interface long after the bitmap and its backend
  are gone. This will result in all sorts of madness - mostly heap corruption
  and AVs.

  Here is an example:

  procedure Test;
  var
    MyBitmap: TBitmap32;
  begin
     MyBitmap := TBitmap32.Create;
     MyBitmap.SetSize(100, 100);
     (MyBitmap.Backend as ICanvasSupport).Canvas;
     MyBitmap.Free;
  end; // _IntfClear will try to clear (MyBitmap.Backend as ICanvasSupport)
       // which points to the interface at the previous location of MyBitmap.Backend in memory.
       // MyBitmap.Backend is gone and the _Release call is invalid, so raise hell .

  Here is an example for a correct workaround:

  procedure Test;
  var
    MyBitmap: TBitmap32;
    CanvasIntf: ICanvasSupport;
  begin
    MyBitmap := TBitmap32.Create;
    MyBitmap.SetSize(100, 100);
    CanvasIntf := MyBitmap.Backend as ICanvasSupport;
    CanvasIntf.Canvas;
    CanvasIntf := nil; // this will call _IntfClear and IInterface._Release
    MyBitmap.Free;
  end; // _IntfClear will try to clear CanvasIntf,
       // it's nil, no _Release is called, everything is fine.

  Since the above code is pretty fiddly, we introduce ref-counting for the
  backend. That way the backend will be released once all references are dropped.

  So, release our reference to the backend now:
  *)
  FBackend._Release;
  FBackend := nil;
end;

procedure TCustomBitmap32.SetBackend(const ABackend: TCustomBackend);
begin
  if (ABackend = nil) or (ABackend = FBackend) then
    exit;

  BeginUpdate;
  try

    ABackend.SetOwner(Self);

    if (FBackend <> nil) then
    begin
      ABackend.Assign(FBackend);
      FinalizeBackend;
    end;

    FBackend := ABackend;
    FBackend.OnChange := BackendChangedHandler;
    FBackend.OnChanging := BackendChangingHandler;

    FBackend.Changed;

    Changed;
  finally
    EndUpdate;
  end;
end;

function TCustomBitmap32.ReleaseBackend: TCustomBackend;
begin
  FBackend._AddRef; // Increase ref-count for external use
  Result := FBackend;
end;

function TCustomBitmap32.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  Result := FBackend.QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := inherited QueryInterface(IID, Obj);
end;

procedure TCustomBitmap32.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  FBackend.ChangeSize(Width, Height, NewWidth, NewHeight);
end;

procedure TCustomBitmap32.BackendChangingHandler(Sender: TObject);
begin
  // descendants can override this method.
end;

procedure TCustomBitmap32.BackendChangedHandler(Sender: TObject);
begin
  FBits := FBackend.Bits;
  ResetClipRect;
end;

function TCustomBitmap32.Empty: Boolean;
begin
  Result := FBackend.Empty or inherited Empty;
end;

procedure TCustomBitmap32.Clear;
begin
  Clear(clBlack32);
end;

procedure TCustomBitmap32.Clear(FillColor: TColor32);
begin
  if Empty then
    Exit;

  if not MeasuringMode then
  begin
    if Clipping then
      FillRect(FClipRect.Left, FClipRect.Top, FClipRect.Right, FClipRect.Bottom, FillColor)
    else
      FillLongword(Bits[0], Width * Height, FillColor);
  end;

  Changed;
end;

procedure TCustomBitmap32.Delete;
begin
  SetSize(0, 0);
end;

procedure TCustomBitmap32.AssignTo(Dst: TPersistent);
begin
  if (not ImageFormatManager.Adapters.AssignTo(Self, Dst)) then
    inherited;
end;

procedure TCustomBitmap32.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try

    if (Source = nil) then
      SetSize(0, 0)
    else
    if (Source is TCustomBitmap32) then
    begin
      TCustomBitmap32(Source).CopyMapTo(Self);
      TCustomBitmap32(Source).CopyPropertiesTo(Self);
    end else
    if (not ImageFormatManager.Adapters.AssignFrom(Self, Source)) then
      inherited; // defer to Source.AssignTo

    Changed;
  finally;
    EndUpdate;
  end;
end;

procedure TCustomBitmap32.CopyMapTo(Dst: TCustomBitmap32);
begin
  Dst.BeginUpdate;
  try
    Dst.SetSize(Width, Height);
    if not Empty then
      MoveLongword(Bits[0], Dst.Bits[0], Width * Height);

    Dst.Changed;
  finally
    Dst.EndUpdate;
  end;
end;

procedure TCustomBitmap32.CopyPropertiesTo(Dst: TCustomBitmap32);
begin
  Dst.DrawMode := DrawMode;
  Dst.CombineMode := CombineMode;
  Dst.WrapMode := WrapMode;
  Dst.MasterAlpha := MasterAlpha;
  Dst.OuterColor := OuterColor;

  Dst.ResamplerClassName := ResamplerClassName;
  if (Dst.Resampler <> nil) and (Resampler <> nil) then
    Dst.Resampler.Assign(Resampler);
end;

constructor TCustomBitmap32.Create(Width, Height: Integer);
begin
  Create;
  SetSize(Width, Height);
end;

{$IFDEF BITS_GETTER}
function TCustomBitmap32.GetBits: PColor32Array;
begin
  Result := FBackend.Bits;
end;
{$ENDIF}

procedure TCustomBitmap32.SetPenPos(const Value: TPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

procedure TCustomBitmap32.SetPenPosF(const Value: TFixedPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

procedure TCustomBitmap32.SetPixel(X, Y: Integer; Value: TColor32);
begin
  Bits[X + Y * Width] := Value;
end;

procedure TCustomBitmap32.SetPixelS(X, Y: Integer; Value: TColor32);
begin
  if {$IFDEF CHANGED_IN_PIXELS}not FMeasuringMode and{$ENDIF}
    (X >= FClipRect.Left) and (X < FClipRect.Right) and
    (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Bits[X + Y * Width] := Value;

{$IFDEF CHANGED_IN_PIXELS}
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

function TCustomBitmap32.GetScanLine(Y: Integer): PColor32Array;
begin
  Result := @Bits[Y * FWidth];
end;

function TCustomBitmap32.GetPenPos: TPoint;
begin
  Result.X := RasterX;
  Result.Y := RasterY;
end;

function TCustomBitmap32.GetPenPosF: TFixedPoint;
begin
  Result.X := RasterXF;
  Result.Y := RasterYF;
end;

function TCustomBitmap32.GetPixel(X, Y: Integer): TColor32;
begin
  Result := Bits[X + Y * Width];
end;

function TCustomBitmap32.GetPixelS(X, Y: Integer): TColor32;
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Result := Bits[X + Y * Width]
  else
    Result := OuterColor;
end;

function TCustomBitmap32.GetPixelPtr(X, Y: Integer): PColor32;
begin
  Result := @Bits[X + Y * Width];
end;

procedure TCustomBitmap32.Draw(DstX, DstY: Integer; Src: TCustomBitmap32);
begin
  if (Src <> nil) then
    Src.DrawTo(Self, DstX, DstY);
end;

procedure TCustomBitmap32.Draw(DstX, DstY: Integer; const SrcRect: TRect;
  Src: TCustomBitmap32);
begin
  if (Src <> nil) then
    Src.DrawTo(Self, DstX, DstY, SrcRect);
end;

procedure TCustomBitmap32.Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32);
begin
  if (Src <> nil) then
    Src.DrawTo(Self, DstRect, SrcRect);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32);
begin
  BlockTransfer(Dst, 0, 0, Dst.ClipRect, Self, BoundsRect, DrawMode,
    FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.ClipRect, Self, BoundsRect, DrawMode,
    FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer;
    const SrcRect: TRect);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.ClipRect, Self, SrcRect,
    DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; const DstRect: TRect);
begin
  StretchTransfer(Dst, DstRect, Dst.ClipRect, Self, BoundsRect, Resampler,
    DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; const DstRect,
  SrcRect: TRect);
begin
  StretchTransfer(Dst, DstRect, Dst.ClipRect, Self, SrcRect, Resampler,
    DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.ResetAlpha;
begin
  ResetAlpha($FF);
end;

procedure TCustomBitmap32.ResetAlpha(const AlphaValue: Byte);
var
  I: Integer;
  P: PByteArray;
begin
  if not FMeasuringMode then
  begin
    {$IFDEF FPC}
    P := Pointer(Bits);
    for I := 0 to Width * Height - 1 do
    begin
      P^[3] := AlphaValue;
      Inc(P, 4);
    end
    {$ELSE}
    P := Pointer(Bits);
    Inc(P, 3); //shift the pointer to 'alpha' component of the first pixel

    I := Width * Height;

    if I > 16 then
    begin
      I := I * 4 - 64;
      Inc(P, I);

      //16x enrolled loop
      I := - I;
      repeat
        P^[I] := AlphaValue;
        P^[I +  4] := AlphaValue;
        P^[I +  8] := AlphaValue;
        P^[I + 12] := AlphaValue;
        P^[I + 16] := AlphaValue;
        P^[I + 20] := AlphaValue;
        P^[I + 24] := AlphaValue;
        P^[I + 28] := AlphaValue;
        P^[I + 32] := AlphaValue;
        P^[I + 36] := AlphaValue;
        P^[I + 40] := AlphaValue;
        P^[I + 44] := AlphaValue;
        P^[I + 48] := AlphaValue;
        P^[I + 52] := AlphaValue;
        P^[I + 56] := AlphaValue;
        P^[I + 60] := AlphaValue;
        Inc(I, 64)
      until I > 0;

      //eventually remaining bits
      Dec(I, 64);
      while I < 0 do
      begin
        P^[I + 64] := AlphaValue;
        Inc(I, 4);
      end;
    end
    else
    begin
      Dec(I);
      I := I * 4;
      while I >= 0 do
      begin
        P^[I] := AlphaValue;
        Dec(I, 4);
      end;
    end;
    {$ENDIF}
  end;
  Changed;
end;

function TCustomBitmap32.GetPixelB(X, Y: Integer): TColor32;
begin
  // WARNING: this function should never be used on empty bitmaps !!!
  if X < 0 then X := 0
  else if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0
  else if Y >= Height then Y := Height - 1;
  Result := Bits[X + Y * Width];
end;

procedure TCustomBitmap32.SetPixelT(X, Y: Integer; Value: TColor32);
begin
  TBlendMem(BlendProc)(Value, Bits[X + Y * Width]);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelT(var Ptr: PColor32; Value: TColor32);
begin
  TBlendMem(BlendProc)(Value, Ptr^);
  Inc(Ptr);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelTS(X, Y: Integer; Value: TColor32);
begin
  if {$IFDEF CHANGED_IN_PIXELS}not FMeasuringMode and{$ENDIF}
    (X >= FClipRect.Left) and (X < FClipRect.Right) and
    (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    TBlendMem(BlendProc)(Value, Bits[X + Y * Width]);
    EMMS;
  end;
{$IFDEF CHANGED_IN_PIXELS}
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

procedure TCustomBitmap32.SET_T256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  { Warning: EMMS should be called after using this method }

  flrx := X and $FF;
  flry := Y and $FF;

{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
  X := X div 256;
  Y := Y div 256;
{$ELSE}
  asm
    SAR X, 8
    SAR Y, 8
  end;
{$IFEND}

  P := @Bits[X + Y * FWidth];
  if FCombineMode = cmBlend then
  begin
    A := C shr 24;  // opacity
    celx := A * GAMMA_ENCODING_TABLE[flrx xor $FF];
    cely := GAMMA_ENCODING_TABLE[flry xor $FF];
    flrx := A * GAMMA_ENCODING_TABLE[flrx];
    flry := GAMMA_ENCODING_TABLE[flry];

    CombineMem(C, P^, celx * cely shr 16); Inc(P);
    CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
    CombineMem(C, P^, flrx * flry shr 16); Dec(P);
    CombineMem(C, P^, celx * flry shr 16);
  end
  else
  begin
    celx := GAMMA_ENCODING_TABLE[flrx xor $FF];
    cely := GAMMA_ENCODING_TABLE[flry xor $FF];
    flrx := GAMMA_ENCODING_TABLE[flrx];
    flry := GAMMA_ENCODING_TABLE[flry];

    CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
    CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
    CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
    CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
  end;
end;

procedure TCustomBitmap32.SET_TS256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  { Warning: EMMS should be called after using this method }

  // we're checking against Left - 1 and Top - 1 due to antialiased values...
  if (X < F256ClipRect.Left - 256) or (X >= F256ClipRect.Right) or
     (Y < F256ClipRect.Top - 256) or (Y >= F256ClipRect.Bottom) then Exit;

  flrx := X and $FF;
  flry := Y and $FF;

{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
  X := X div 256;
  Y := Y div 256;
{$ELSE}
  asm
    SAR X, 8
    SAR Y, 8
  end;
{$IFEND}

  P := @Bits[X + Y * FWidth];
  if FCombineMode = cmBlend then
  begin
    A := C shr 24;  // opacity
    celx := A * GAMMA_ENCODING_TABLE[flrx xor $FF];
    cely := GAMMA_ENCODING_TABLE[flry xor $FF];
    flrx := A * GAMMA_ENCODING_TABLE[flrx];
    flry := GAMMA_ENCODING_TABLE[flry];

    if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
       (X < FClipRect.Right - 1) and (Y < FClipRect.Bottom - 1) then
    begin
      CombineMem(C, P^, celx * cely shr 16); Inc(P);
      CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
      CombineMem(C, P^, flrx * flry shr 16); Dec(P);
      CombineMem(C, P^, celx * flry shr 16);
    end
    else // "pixel" lies on the edge of the bitmap
    with FClipRect do
    begin
      if (X >= Left) and (Y >= Top) then CombineMem(C, P^, celx * cely shr 16); Inc(P);
      if (X < Right - 1) and (Y >= Top) then CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
      if (X < Right - 1) and (Y < Bottom - 1) then CombineMem(C, P^, flrx * flry shr 16); Dec(P);
      if (X >= Left) and (Y < Bottom - 1) then CombineMem(C, P^, celx * flry shr 16);
    end;
  end
  else
  begin
    celx := GAMMA_ENCODING_TABLE[flrx xor $FF];
    cely := GAMMA_ENCODING_TABLE[flry xor $FF];
    flrx := GAMMA_ENCODING_TABLE[flrx];
    flry := GAMMA_ENCODING_TABLE[flry];

    if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
       (X < FClipRect.Right - 1) and (Y < FClipRect.Bottom - 1) then
    begin
      CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
      CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
      CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
      CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
    end
    else // "pixel" lies on the edge of the bitmap
    with FClipRect do
    begin
      if (X >= Left) and (Y >= Top) then CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
      if (X < Right - 1) and (Y >= Top) then CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
      if (X < Right - 1) and (Y < Bottom - 1) then CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
      if (X >= Left) and (Y < Bottom - 1) then CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
    end;
  end;
end;

procedure TCustomBitmap32.SetPixelF(X, Y: Single; Value: TColor32);
begin
  SET_T256(Round(X * 256), Round(Y * 256), Value);
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelX(X, Y: TFixed; Value: TColor32);
begin
  X := (X + $7F) shr 8;
  Y := (Y + $7F) shr 8;
  SET_T256(X, Y, Value);
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelFS(X, Y: Single; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}
    SET_TS256(Round(X * 256), Round(Y * 256), Value);
    EMMS;
{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(FloatRect(X, Y, X + 1, Y + 1)));
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelFW(X, Y: Single; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}
    SetPixelXW(Round(X * FixedOne), Round(Y * FixedOne), Value);
    EMMS;
{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(FloatRect(X, Y, X + 1, Y + 1)));
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelXS(X, Y: TFixed; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}

{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
    X := (X + $7F) div 256;
    Y := (Y + $7F) div 256;
{$ELSE}
    asm
          ADD X, $7F
          ADD Y, $7F
          SAR X, 8
          SAR Y, 8
    end;
{$IFEND}

    SET_TS256(X, Y, Value);
    EMMS;

{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

function TCustomBitmap32.GET_T256(X, Y: Integer): TColor32;
// When using this, remember that it interpolates towards next x and y!
var
  Pos: Integer;
begin
  Pos := (X shr 8) + (Y shr 8) * FWidth;
  Result := Interpolator(GAMMA_ENCODING_TABLE[X and $FF xor $FF],
                         GAMMA_ENCODING_TABLE[Y and $FF xor $FF],
                         @Bits[Pos], @Bits[Pos + FWidth]);
end;

function TCustomBitmap32.GET_TS256(X, Y: Integer): TColor32;
var
  Width256, Height256: Integer;
begin
  if (X >= F256ClipRect.Left) and (Y >= F256ClipRect.Top) then
  begin
    Width256 := (FClipRect.Right - 1) shl 8;
    Height256 := (FClipRect.Bottom - 1) shl 8;

    if (X < Width256) and (Y < Height256) then
      Result := GET_T256(X,Y)
    else if (X = Width256) and (Y <= Height256) then
      // We're exactly on the right border: no need to interpolate.
      Result := Pixel[FClipRect.Right - 1, Y shr 8]
    else if (X <= Width256) and (Y = Height256) then
      // We're exactly on the bottom border: no need to interpolate.
      Result := Pixel[X shr 8, FClipRect.Bottom - 1]
    else
      Result := FOuterColor;
  end
  else
    Result := FOuterColor;
end;

function TCustomBitmap32.GetPixelF(X, Y: Single): TColor32;
begin
  Result := GET_T256(Round(X * 256), Round(Y * 256));
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

function TCustomBitmap32.GetPixelFS(X, Y: Single): TColor32;
begin
  Result := GET_TS256(Round(X * 256), Round(Y * 256));
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

function TCustomBitmap32.GetPixelFW(X, Y: Single): TColor32;
begin
  Result := GetPixelXW(Round(X * FixedOne), Round(Y * FixedOne));
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

function TCustomBitmap32.GetPixelX(X, Y: TFixed): TColor32;
begin
  X := (X + $7F) shr 8;
  Y := (Y + $7F) shr 8;
  Result := GET_T256(X, Y);
{$IFNDEF OMIT_MMX}
  EMMS;
{$ENDIF}
end;

function TCustomBitmap32.GetPixelXS(X, Y: TFixed): TColor32;
{$IFDEF PUREPASCAL}
begin
  X := (X + $7F) div 256;
  Y := (Y + $7F) div 256;
  Result := GET_TS256(X, Y);
  EMMS;
{$ELSE}
{$IFDEF FPC}assembler;{$ENDIF}
asm
{$IFDEF TARGET_x64}
          PUSH    RBP
          SUB     RSP,$30
          MOV     RBP,RSP
{$ENDIF}
          ADD     X, $7F
          ADD     Y, $7F
          SAR     X, 8
          SAR     Y, 8
          CALL    TCustomBitmap32.GET_TS256
{$IFNDEF OMIT_MMX}
          CMP     MMX_ACTIVE.Integer, $00
          JZ      @Exit
          DB      $0F, $77               /// EMMS
@Exit:
{$ENDIF}

{$IFDEF TARGET_x64}
          LEA     RSP,[RBP+$30]
          POP     RBP
{$ENDIF}

{$ENDIF}
end;

function TCustomBitmap32.GetPixelFR(X, Y: Single): TColor32;
begin
  Result := FResampler.GetSampleFloat(X, Y);
end;

function TCustomBitmap32.GetPixelXR(X, Y: TFixed): TColor32;
begin
  Result := FResampler.GetSampleFixed(X, Y);
end;

function TCustomBitmap32.GetPixelW(X, Y: Integer): TColor32;
begin
  with FClipRect do
    Result := Bits[FWidth * WrapProcVert(Y, Top, Bottom - 1) + WrapProcHorz(X, Left, Right - 1)];
end;

procedure TCustomBitmap32.SetPixelW(X, Y: Integer; Value: TColor32);
begin
  with FClipRect do
    Bits[FWidth * WrapProcVert(Y, Top, Bottom - 1) + WrapProcHorz(X, Left, Right - 1)] := Value;
end;

function TCustomBitmap32.GetPixelXW(X, Y: TFixed): TColor32;
var
  X1, X2, Y1, Y2 :Integer;
  W: Integer;
begin
  X2 := TFixedRec(X).Int;
  Y2 := TFixedRec(Y).Int;

  with FClipRect do
  begin
    W := Right - 1;
    X1 := WrapProcHorz(X2, Left, W);
    X2 := WrapProcHorz(X2 + 1, Left, W);
    W := Bottom - 1;
    Y1 := WrapProcVert(Y2, Top, W) * Width;
    Y2 := WrapProcVert(Y2 + 1, Top, W) * Width;
  end;

  W := WordRec(TFixedRec(X).Frac).Hi;

  Result := CombineReg(CombineReg(Bits[X2 + Y2], Bits[X1 + Y2], W),
                       CombineReg(Bits[X2 + Y1], Bits[X1 + Y1], W),
                       WordRec(TFixedRec(Y).Frac).Hi);
  EMMS;
end;

class function TCustomBitmap32.GetPlatformBackendClass: TCustomBackendClass;
begin
  Result := TMemoryBackend;
end;

procedure TCustomBitmap32.SetPixelXW(X, Y: TFixed; Value: TColor32);
begin
{$IF Defined(PUREPASCAL) or Defined(TARGET_X64)}
  X := (X + $7F) div 256;
  Y := (Y + $7F) div 256;
{$ELSE}
  asm
        ADD X, $7F
        ADD Y, $7F
        SAR X, 8
        SAR Y, 8
  end;
{$IFEND}

  with F256ClipRect do
    SET_T256(WrapProcHorz(X, Left, Right - 128), WrapProcVert(Y, Top, Bottom - 128), Value);
  EMMS;
end;


procedure TCustomBitmap32.SetStipple(NewStipple: TArrayOfColor32);
begin
  FStippleCounter := 0;
  FStipplePattern := Copy(NewStipple, 0, Length(NewStipple));
end;

procedure TCustomBitmap32.SetStipple(NewStipple: array of TColor32);
var
  L: Integer;
begin
  FStippleCounter := 0;
  L := High(NewStipple) + 1;
  SetLength(FStipplePattern, L);
  MoveLongword(NewStipple[0], FStipplePattern[0], L);
end;

procedure TCustomBitmap32.AdvanceStippleCounter(LengthPixels: Single);
var
  L: Integer;
  Delta: Single;
begin
  L := Length(FStipplePattern);
  Delta := LengthPixels * FStippleStep;

  if (L = 0) or (Delta = 0) then
    Exit;

  FStippleCounter := FStippleCounter + Delta;
  FStippleCounter := FStippleCounter - Floor(FStippleCounter / L) * L;
end;

function TCustomBitmap32.GetStippleColor: TColor32;
var
  L: Integer;
  NextIndex, PrevIndex: Integer;
  PrevWeight: Integer;
begin
  L := Length(FStipplePattern);
  if L = 0 then
  begin
    // no pattern defined, just return something and exit
    Result := clBlack32;
    Exit;
  end;
  FStippleCounter := Wrap(FStippleCounter, L);
  {$IFDEF FPC}
  PrevIndex := Trunc(FStippleCounter);
  {$ELSE}
  PrevIndex := Round(FStippleCounter - 0.5);
  {$ENDIF}
  PrevWeight := $FF - Round($FF * (FStippleCounter - PrevIndex));
  if PrevIndex < 0 then FStippleCounter := L - 1;
  NextIndex := PrevIndex + 1;
  if NextIndex >= L then NextIndex := 0;
  if PrevWeight = $FF then Result := FStipplePattern[PrevIndex]
  else
  begin
    Result := CombineReg(
      FStipplePattern[PrevIndex],
      FStipplePattern[NextIndex],
      PrevWeight);
    EMMS;
  end;
  FStippleCounter := FStippleCounter + FStippleStep;
end;

procedure TCustomBitmap32.HorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
  if not FMeasuringMode then
    FillLongword(Bits[X1 + Y * Width], X2 - X1 + 1, Value);

  Changed(MakeRect(X1, Y, X2+1, Y+1));
end;

procedure TCustomBitmap32.HorzLineS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
    HorzLine(X1, Y, X2, Value);
end;

procedure TCustomBitmap32.HorzLineT(X1, Y, X2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  if X2 < X1 then
    Exit;

  if not FMeasuringMode then
  begin
    P := PixelPtr[X1, Y];
    BlendMem := TBlendMem(BlendProc);
    for i := X1 to X2 do
    begin
      BlendMem(Value, P^);
      Inc(P);
    end;

    EMMS;
  end;

  Changed(MakeRect(X1, Y, X2+1, Y+1));
end;

procedure TCustomBitmap32.HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
    HorzLineT(X1, Y, X2, Value);
end;

procedure TCustomBitmap32.HorzLineTSP(X1, Y, X2: Integer);
var
  I, N: Integer;
begin
  if Empty then
    Exit;

  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    if ((X1 < FClipRect.Left) and (X2 < FClipRect.Left)) or
       ((X1 >= FClipRect.Right) and (X2 >= FClipRect.Right)) then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(Abs(X2 - X1) + 1);

      Exit;
    end;

    if X1 < FClipRect.Left then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(FClipRect.Left - X1);

      X1 := FClipRect.Left;
    end else
    if X1 >= FClipRect.Right then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(X1 - (FClipRect.Right - 1));

      X1 := FClipRect.Right - 1;
    end;

    N := 0;
    if X2 < FClipRect.Left then
    begin
      N := FClipRect.Left - X2;
      X2 := FClipRect.Left;
    end else
    if X2 >= FClipRect.Right then
    begin
      N := X2 - (FClipRect.Right - 1);
      X2 := FClipRect.Right - 1;
    end;

    if not FMeasuringMode then
    begin
      if X2 >= X1 then
      begin
        for I := X1 to X2 do
          SetPixelT(I, Y, GetStippleColor);
      end else
      begin
        for I := X1 downto X2 do
          SetPixelT(I, Y, GetStippleColor);
      end;
    end;

    Changed(MakeRect(X1, Y, X2+1, Y+1));

    if (not FMeasuringMode) and (N > 0) then
      AdvanceStippleCounter(N);
  end else
  if not FMeasuringMode then
    AdvanceStippleCounter(Abs(X2 - X1) + 1);
end;

procedure TCustomBitmap32.HorzLineX(X1, Y, X2: TFixed; Value: TColor32);
//Author: Michael Hansen
var
  I: Integer;
  X1F, X2F, YF, Count: Integer;
  Wx1, Wx2, Wy, Wt: TColor32;
  PDst: PColor32;
begin
  if X1 > X2 then
    Swap(X1, X2);

  if not FMeasuringMode then
  begin
    X1F := X1 shr 16;
    X2F := X2 shr 16;
    YF := Y shr 16;

    PDst := PixelPtr[X1F, YF];

    Wy := Y and $ffff xor $ffff;
    Wx1 := X1 and $ffff xor $ffff;
    Wx2 := X2 and $ffff;

    Count := X2F - X1F - 1;
    if Wy > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wy * Wx1) shr 24]);
      Inc(PDst);

      Wt := GAMMA_ENCODING_TABLE[Wy shr 8];

      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst);
      end;

      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wy * Wx2) shr 24]);
    end;

    PDst := PixelPtr[X1F, YF + 1];

    Wy := Wy xor $ffff;
    if Wy > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wy * Wx1) shr 24]);
      Inc(PDst);

      Wt := GAMMA_ENCODING_TABLE[Wy shr 8];

      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst);
      end;

      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wy * Wx2) shr 24]);
    end;

    EMMS;
  end;

  Changed(MakeRect(FixedRect(X1, Y, X2+1, Y+1), rrOutside), AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);
//author: Michael Hansen
begin
  if X1 > X2 then
    Swap(X1, X2);

  X1 := Constrain(X1, FFixedClipRect.Left, FFixedClipRect.Right);
  X2 := Constrain(X2, FFixedClipRect.Left, FFixedClipRect.Right);

  if (Abs(X2 - X1) > FIXEDONE) and InRange(Y, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE) then
    HorzLineX(X1, Y, X2, Value)
  else
    LineXS(X1, Y, X2, Y, Value);
end;

procedure TCustomBitmap32.VertLine(X, Y1, Y2: Integer; Value: TColor32);
var
  I, NH, NL: Integer;
  P: PColor32;
begin
  if Y2 < Y1 then
    Exit;

  if not FMeasuringMode then
  begin
    P := PixelPtr[X, Y1];
    I := Y2 - Y1 + 1;
    NH := I shr 2;
    NL := I and $03;

    for I := 0 to NH - 1 do
    begin
      P^ := Value; Inc(P, Width);
      P^ := Value; Inc(P, Width);
      P^ := Value; Inc(P, Width);
      P^ := Value; Inc(P, Width);
    end;

    for I := 0 to NL - 1 do
    begin
      P^ := Value; Inc(P, Width);
    end;

  end;

  Changed(MakeRect(X, Y1, X+1, Y2+1));
end;

procedure TCustomBitmap32.VertLineS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
    VertLine(X, Y1, Y2, Value);
end;

procedure TCustomBitmap32.VertLineT(X, Y1, Y2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  if not FMeasuringMode then
  begin
    P := PixelPtr[X, Y1];
    BlendMem := TBlendMem(BlendProc);

    for i := Y1 to Y2 do
    begin
      BlendMem(Value, P^);
      Inc(P, Width);
    end;

    EMMS;
  end;

  Changed(MakeRect(X, Y1, X+1, Y2+1));
end;

procedure TCustomBitmap32.VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
    VertLineT(X, Y1, Y2, Value);
end;

procedure TCustomBitmap32.VertLineTSP(X, Y1, Y2: Integer);
var
  I, N: Integer;
begin
  if Empty then
    Exit;

  if (X >= FClipRect.Left) and (X < FClipRect.Right) then
  begin
    if ((Y1 < FClipRect.Top) and (Y2 < FClipRect.Top)) or
       ((Y1 >= FClipRect.Bottom) and (Y2 >= FClipRect.Bottom)) then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(Abs(Y2 - Y1) + 1);

      Exit;
    end;

    if Y1 < FClipRect.Top then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(FClipRect.Top - Y1);

      Y1 := FClipRect.Top;
    end else
    if Y1 >= FClipRect.Bottom then
    begin
      if not FMeasuringMode then
        AdvanceStippleCounter(Y1 - (FClipRect.Bottom - 1));

      Y1 := FClipRect.Bottom - 1;
    end;

    N := 0;
    if Y2 < FClipRect.Top then
    begin
      N := FClipRect.Top - Y2;
      Y2 := FClipRect.Top;
    end else
    if Y2 >= FClipRect.Bottom then
    begin
      N := Y2 - (FClipRect.Bottom - 1);
      Y2 := FClipRect.Bottom - 1;
    end;

    if not FMeasuringMode then
    begin
      if Y2 >= Y1 then
      begin
        for I := Y1 to Y2 do
          SetPixelT(X, I, GetStippleColor)
      end else
      begin
        for I := Y1 downto Y2 do
          SetPixelT(X, I, GetStippleColor);
      end;
    end;

    Changed(MakeRect(X, Y1, X+1, Y2+1));

    if (not FMeasuringMode) and (N > 0) then
      AdvanceStippleCounter(N);
  end else
  if not FMeasuringMode then
    AdvanceStippleCounter(Abs(Y2 - Y1) + 1);
end;

procedure TCustomBitmap32.VertLineX(X, Y1, Y2: TFixed; Value: TColor32);
//Author: Michael Hansen
var
  I: Integer;
  Y1F, Y2F, XF, Count: Integer;
  Wy1, Wy2, Wx, Wt: TColor32;
  PDst: PColor32;
begin
  if Y1 > Y2 then
    Swap(Y1, Y2);

  if not FMeasuringMode then
  begin
    Y1F := Y1 shr 16;
    Y2F := Y2 shr 16;
    XF := X shr 16;

    PDst := PixelPtr[XF, Y1F];

    Wx := X and $ffff xor $ffff;
    Wy1 := Y1 and $ffff xor $ffff;
    Wy2 := Y2 and $ffff;

    Count := Y2F - Y1F - 1;
    if Wx > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wx * Wy1) shr 24]);
      Inc(PDst, FWidth);

      Wt := GAMMA_ENCODING_TABLE[Wx shr 8];

      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst, FWidth);
      end;

      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wx * Wy2) shr 24]);
    end;

    PDst := PixelPtr[XF + 1, Y1F];

    Wx := Wx xor $ffff;
    if Wx > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wx * Wy1) shr 24]);
      Inc(PDst, FWidth);

      Wt := GAMMA_ENCODING_TABLE[Wx shr 8];

      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst, FWidth);
      end;

      CombineMem(Value, PDst^, GAMMA_ENCODING_TABLE[(Wx * Wy2) shr 24]);
    end;

    EMMS;
  end;

  Changed(MakeRect(FixedRect(X, Y1, X+1, Y2+1), rrOutside), AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);
//author: Michael Hansen
begin
  if Y1 > Y2 then
    Swap(Y1, Y2);

  Y1 := Constrain(Y1, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE);
  Y2 := Constrain(Y2, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE);

  if (Abs(Y2 - Y1) > FIXEDONE) and InRange(X, FFixedClipRect.Left, FFixedClipRect.Right - FIXEDONE) then
    VertLineX(X, Y1, Y2, Value)
  else
    LineXS(X, Y1, X, Y2, Value);
end;

procedure TCustomBitmap32.Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
begin
  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dx > 0 then
    Sx := 1
  else
  if Dx < 0 then
  begin
    Dx := -Dx;
    Sx := -1;
  end else // Dx = 0
  begin
    if Dy > 0 then
      VertLine(X1, Y1, Y2 - 1, Value)
    else
    if Dy < 0 then
      VertLine(X1, Y2 + 1, Y1, Value);

    if L then
    begin
      if not FMeasuringMode then
        Pixel[X2, Y2] := Value;
      Changed(MakeRect(X2, Y2, X2+1, Y2+1));
    end;

    Exit;
  end;

  if Dy > 0 then
    Sy := 1
  else
  if Dy < 0 then
  begin
    Dy := -Dy;
    Sy := -1;
  end else // Dy = 0
  begin
    if X2 > X1 then
      HorzLine(X1, Y1, X2 - 1, Value)
    else
      HorzLine(X2 + 1, Y1, X1, Value);

    if L then
    begin
      if not FMeasuringMode then
        Pixel[X2, Y2] := Value;
      Changed(MakeRect(X2, Y2, X2+1, Y2+1));
    end;

    Exit;
  end;

  if not FMeasuringMode then
  begin
    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;

      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Inc(Delta, Dy);

        if Delta >= Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end else // Dx < Dy
    begin
      Delta := Dy shr 1;

      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Inc(Delta, Dx);

        if Delta >= Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;

    if L then
      P^ := Value;
  end;

  Changed(MakeRect(X1, Y1, X2, Y2), AREAINFO_LINE + 1);
end;

procedure TCustomBitmap32.LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx2, Dy2,Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, e: Integer;
  OC: Int64;
  Swapped, CheckAux: Boolean;
  P: PColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then
        VertLineS(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        VertLineS(X1, Y2 + 1, Y1, Value);

      if L then
      begin
        PixelS[X2, Y2] := Value;
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end else
    if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then
        HorzLineS(X1, Y1, X2 - 1, Value)
      else
      if Dx < 0 then
        HorzLineS(X2 + 1, Y1, X1, Value);

      if L then
      begin
        PixelS[X2, Y2] := Value;
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      if (X1 > Cx2) or (X2 < Cx1) then
        Exit; // segment not visible

      Sx := 1;
    end else
    begin
      if (X2 > Cx2) or (X1 < Cx1)
        then Exit; // segment not visible

      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      if (Y1 > Cy2) or (Y2 < Cy1) then
        Exit; // segment not visible

      Sy := 1;
    end else
    begin
      if (Y2 > Cy2) or (Y1 < Cy1) then
        Exit; // segment not visible

      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swapped := True;
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    end else
      Swapped := False;

    // Bresenham's set up:
    Dx2 := Dx shl 1; Dy2 := Dy shl 1;
    xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
    CheckAux := True;

    // Clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      OC := Int64(Dx2) * (Cy1 - Y1) - Dx;
      Inc(xd, OC div Dy2);
      rem := OC mod Dy2;

      if xd > Cx2 then
        Exit;

      if xd >= Cx1 then
      begin
        yd := Cy1;
        Dec(e, rem + Dx);

        if rem > 0 then
        begin
          Inc(xd);
          Inc(e, Dy2);
        end;
        CheckAux := False; // to avoid ugly goto we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckAux and (X1 < Cx1) then
    begin
      OC := Int64(Dy2) * (Cx1 - X1);
      Inc(yd, OC div Dx2);
      rem := OC mod Dx2;

      if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then
        Exit;

      xd := Cx1;
      Inc(e, rem);

      if (rem >= Dx) then
      begin
        Inc(yd);
        Dec(e, Dx2);
      end;
    end;

    // set auxiliary var to indicate that term is not clipped, since
    // term still has the unclipped value assigned at setup.
    CheckAux := False;

    // is the segment exiting the clipping rect?
    if Y2 > Cy2 then
    begin
      OC := Int64(Dx2) * (Cy2 - Y1) + Dx;
      term := X1 + OC div Dy2;
      rem := OC mod Dy2;

      if rem = 0 then
        Dec(term);

      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
    end;

    Dec(Dx2, Dy2);

    if Swapped then
    begin
      PI := Sx * Width;
      P := @Bits[yd + xd * Width];
    end else
    begin
      PI := Sx;
      Sy := Sy * Width;
      P := @Bits[xd + yd * Width];
    end;

    // do we need to skip the last pixel of the line and is term not clipped?
    if not(L or CheckAux) then
    begin
      if xd < term then
        Dec(term)
      else
        Inc(term);
    end;

    while xd <> term do
    begin
      Inc(xd, Sx);

      P^ := Value;
      Inc(P, PI);
      if e >= 0 then
      begin
        Inc(P, Sy);
        Dec(e, Dx2);
      end else
        Inc(e, Dy2);
    end;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 1);
end;

procedure TCustomBitmap32.LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
      Sx := 1
    else
    if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end else // Dx = 0
    begin
      if Dy > 0 then
        VertLineT(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        VertLineT(X1, Y2 + 1, Y1, Value);

      if L then
      begin
        SetPixelT(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end;

    if Dy > 0 then
      Sy := 1
    else
    if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end else // Dy = 0
    begin
      if X2 > X1 then
        HorzLineT(X1, Y1, X2 - 1, Value)
      else
        HorzLineT(X2 + 1, Y1, X1, Value);

      if L then
      begin
        SetPixelT(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    BlendMem := TBlendMem(BlendProc);

    if Dx > Dy then
    begin
      Delta := Dx shr 1;

      for I := 0 to Dx - 1 do
      begin
        BlendMem(Value, P^);

        Inc(P, Sx);
        Inc(Delta, Dy);
        if Delta >= Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end else // Dx < Dy
    begin
      Delta := Dy shr 1;

      for I := 0 to Dy - 1 do
      begin
        BlendMem(Value, P^);

        Inc(P, Sy);
        Inc(Delta, Dx);
        if Delta >= Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;

    if L then
      BlendMem(Value, P^);

    EMMS;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 1);
end;

procedure TCustomBitmap32.LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, Dx2, Dy2, rem, term, e: Integer;
  OC: Int64;
  Swapped, CheckAux: Boolean;
  P: PColor32;
  BlendMem: TBlendMem;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then
        VertLineTS(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        VertLineTS(X1, Y2 + 1, Y1, Value);

      if L then
      begin
        SetPixelTS(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end else
    if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then
        HorzLineTS(X1, Y1, X2 - 1, Value)
      else
      if Dx < 0 then
        HorzLineTS(X2 + 1, Y1, X1, Value);

      if L then
      begin
        SetPixelTS(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      if (X1 > Cx2) or (X2 < Cx1) then
        Exit; // segment not visible

      Sx := 1;
    end else
    begin
      if (X2 > Cx2) or (X1 < Cx1) then
        Exit; // segment not visible

      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      if (Y1 > Cy2) or (Y2 < Cy1) then
        Exit; // segment not visible

      Sy := 1;
    end else
    begin
      if (Y2 > Cy2) or (Y1 < Cy1) then
        Exit; // segment not visible

      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swapped := True;
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    end else
      Swapped := False;

    // Bresenham's set up:
    Dx2 := Dx shl 1; Dy2 := Dy shl 1;
    xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
    CheckAux := True;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      OC := Int64(Dx2) * (Cy1 - Y1) - Dx;
      Inc(xd, OC div Dy2);
      rem := OC mod Dy2;

      if xd > Cx2 then
        Exit;

      if xd >= Cx1 then
      begin
        yd := Cy1;
        Dec(e, rem + Dx);
        if rem > 0 then
        begin
          Inc(xd);
          Inc(e, Dy2);
        end;
        CheckAux := False; // to avoid ugly goto we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckAux and (X1 < Cx1) then
    begin
      OC := Int64(Dy2) * (Cx1 - X1);
      Inc(yd, OC div Dx2);
      rem := OC mod Dx2;

      if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then
        Exit;

      xd := Cx1;
      Inc(e, rem);
      if (rem >= Dx) then
      begin
        Inc(yd);
        Dec(e, Dx2);
      end;
    end;

    // set auxiliary var to indicate that term is not clipped, since
    // term still has the unclipped value assigned at setup.
    CheckAux := False;

    // is the segment exiting the clipping rect?
    if Y2 > Cy2 then
    begin
      OC := Int64(Dx2) * (Cy2 - Y1) + Dx;
      term := X1 + OC div Dy2;
      rem := OC mod Dy2;
      if rem = 0 then
        Dec(term);
      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
    end;

    Dec(Dx2, Dy2);

    if Swapped then
    begin
      PI := Sx * Width;
      P := @Bits[yd + xd * Width];
    end else
    begin
      PI := Sx;
      Sy := Sy * Width;
      P := @Bits[xd + yd * Width];
    end;

    // do we need to skip the last pixel of the line and is term not clipped?
    if not(L or CheckAux) then
    begin
      if xd < term then
        Dec(term)
      else
        Inc(term);
    end;

    BlendMem := BLEND_MEM[FCombineMode]^;
    while xd <> term do
    begin
      Inc(xd, Sx);

      BlendMem(Value, P^);
      Inc(P, PI);
      if e >= 0 then
      begin
        Inc(P, Sy);
        Dec(e, Dx2);
      end
      else
        Inc(e, Dy2);
    end;

    EMMS;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 1);
end;

procedure TCustomBitmap32.LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp, hypl: Integer;
  A: TColor32;
  h: Single;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2), rrOutside);

  if not FMeasuringMode then
  begin
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);

    hyp := Hypot(nx, ny);
    if hyp = 0 then
      Exit;

    hypl := hyp + (Integer(L) * FixedOne);
    if (hypl < 256) then
      Exit;

    n := hypl shr 16;
    if n > 0 then
    begin
      h := 65536 / hyp;
      nx := Round(nx * h); ny := Round(ny * h);
      for i := 0 to n - 1 do
      begin
        SET_T256(X1 shr 8, Y1 shr 8, Value);
        Inc(X1, nx);
        Inc(Y1, ny);
      end;
    end;
    A := Value shr 24;
    hyp := hypl - n shl 16;
    A := A * Cardinal(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, Value and $00FFFFFF + A);

    EMMS;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineX(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

// Note:
// ClipLine has been copied from GR32_VectorUtils to avoid referencing that unit here
// since that would prevent inlining in GR32_VectorUtils due to
// H2456 Inline function '%s' has not been expanded because contained unit '%s' uses compiling unit '%s'
function ClipLine(var X1, Y1, X2, Y2: Integer; MinX, MinY, MaxX, MaxY: Integer): Boolean;
var
  C1, C2: Integer;
  V: Integer;
begin
  { Get edge codes }
  C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1 + Ord(Y1 < MinY) shl 2 + Ord(Y1 > MaxY) shl 3;
  C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1 + Ord(Y2 < MinY) shl 2 + Ord(Y2 > MaxY) shl 3;

  if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
  begin
    if (C1 and 12) <> 0 then
    begin
      if C1 < 8 then V := MinY else V := MaxY;
      Inc(X1, MulDiv(V - Y1, X2 - X1, Y2 - Y1));
      Y1 := V;
      C1 := Ord(X1 < MinX) + Ord(X1 > MaxX) shl 1;
    end;

    if (C2 and 12) <> 0 then
    begin
      if C2 < 8 then V := MinY else V := MaxY;
      Inc(X2, MulDiv(V - Y2, X2 - X1, Y2 - Y1));
      Y2 := V;
      C2 := Ord(X2 < MinX) + Ord(X2 > MaxX) shl 1;
    end;

    if ((C1 and C2) = 0) and ((C1 or C2) <> 0) then
    begin
      if C1 <> 0 then
      begin
        if C1 = 1 then V := MinX else V := MaxX;
        Inc(Y1, MulDiv(V - X1, Y2 - Y1, X2 - X1));
        X1 := V;
        C1 := 0;
      end;

      if C2 <> 0 then
      begin
        if C2 = 1 then V := MinX else V := MaxX;
        Inc(Y2, MulDiv(V - X2, Y2 - Y1, X2 - X1));
        X2 := V;
        C2 := 0;
      end;
    end;
  end;

  Result := (C1 or C2) = 0;
end;

procedure TCustomBitmap32.LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  ex, ey, nx, ny, hyp, hypl: Integer;
  A: TColor32;
  h: Single;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2), rrOutside);

  ex := X2; ey := Y2;

  // Check for visibility and clip the coordinates
  if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
    FFixedClipRect.Left - $10000,
    FFixedClipRect.Top - $10000,
    FFixedClipRect.Right, FFixedClipRect.Bottom) then
    Exit;

  if not FMeasuringMode then
  begin
    { TODO : Handle L on clipping here... }

    if (ex <> X2) or (ey <> Y2) then
      L := True;

    // Check if it lies entirely in the bitmap area. Even after clipping
    // some pixels may lie outside the bitmap due to antialiasing
    if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
       (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
       (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
       (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
    begin
      LineX(X1, Y1, X2, Y2, Value, L);
      Exit;
    end;

    // if we are still here, it means that the line touches one or several bitmap
    // boundaries. Use the safe version of antialiased pixel routine
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);

    hyp := Hypot(nx, ny);
    if hyp = 0 then
      Exit;

    hypl := hyp + (Integer(L) * FixedOne);
    if hypl < 256 then
      Exit;

    n := hypl shr 16;
    if n > 0 then
    begin
      h := 65536 / hyp;
      nx := Round(nx * h); ny := Round(ny * h);
      for i := 0 to n - 1 do
      begin
        SET_TS256(SAR_8(X1), SAR_8(Y1), Value);
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    A := Value shr 24;
    hyp := hypl - n shl 16;
    A := A * Cardinal(hyp) shl 8 and $FF000000;
    SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), Value and $00FFFFFF + A);

    EMMS;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineXS(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

procedure TCustomBitmap32.LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp, hypl: Integer;
  A, C: TColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2), rrOutside);

  if not FMeasuringMode then
  begin
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);

    hyp := Hypot(nx, ny);
    if hyp = 0 then
      Exit;

    hypl := hyp + (Integer(L) * FixedOne);
    if hypl < 256 then
      Exit;

    n := hypl shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536);
      ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_T256(X1 shr 8, Y1 shr 8, C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;

    C := GetStippleColor;
    A := C shr 24;
    hyp := hypl - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, C and $00FFFFFF + A);

    EMMS;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.LineFP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean);
const
  StippleInc: array [Boolean] of Integer = (0, 1);
var
  n, i: Integer;
  sx, sy, ex, ey, nx, ny, hyp, hypl: Integer;
  A, C: TColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2), rrOutside);

  if not FMeasuringMode then
  begin
    sx := X1; sy := Y1; ex := X2; ey := Y2;

    // Check for visibility and clip the coordinates
    if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
      FFixedClipRect.Left - $10000, FFixedClipRect.Top - $10000,
      FFixedClipRect.Right, FFixedClipRect.Bottom) then
    begin
      AdvanceStippleCounter(GR32_Math.Hypot(Integer((X2 - X1) shr 16), Integer((Y2 - Y1) shr 16) - StippleInc[L]));
      Exit;
    end;

    if (ex <> X2) or (ey <> Y2) then
      L := True;

    // Check if it lies entirely in the bitmap area. Even after clipping
    // some pixels may lie outside the bitmap due to antialiasing
    if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
       (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
       (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
       (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
    begin
      LineXP(X1, Y1, X2, Y2, L);
      Exit;
    end;

    if (sx <> X1) or (sy <> Y1) then
      AdvanceStippleCounter(GR32_Math.Hypot(Integer((X1 - sx) shr 16), Integer((Y1 - sy) shr 16)));

    // if we are still here, it means that the line touches one or several bitmap
    // boundaries. Use the safe version of antialiased pixel routine
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);

    hyp := GR32_Math.Hypot(nx, ny);
    if hyp = 0 then
      Exit;

    hypl := hyp + (Integer(L) * FixedOne);
    if hypl < 256 then
      Exit;

    n := hypl shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536); ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_TS256(SAR_8(X1), SAR_8(Y1), C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;

    C := GetStippleColor;
    A := C shr 24;
    hyp := hypl - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), C and $00FFFFFF + A);

    EMMS;

    if (ex <> X2) or (ey <> Y2) then
      AdvanceStippleCounter(GR32_Math.Hypot(Integer((X2 - ex) shr 16), Integer((Y2 - ey) shr 16) - StippleInc[L]));
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.LineFSP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXSP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx, Dy, Sx, Sy, D: Integer;
  EC, EA: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  if (X1 = X2) or (Y1 = Y2) then
  begin
    LineT(X1, Y1, X2, Y2, Value, L);
    Exit;
  end;

  if not FMeasuringMode then
  begin
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
      Sx := 1
    else
    begin
      Sx := -1;
      Dx := -Dx;
    end;

    if Dy > 0 then
      Sy := 1
    else
    begin
      Sy := -1;
      Dy := -Dy;
    end;

    EC := 0;
    BLEND_MEM[FCombineMode]^(Value, Bits[X1 + Y1 * Width]);
    BlendMemEx := BLEND_MEM_EX[FCombineMode]^;

    if Dy > Dx then
    begin
      EA := Dx shl 16 div Dy;

      if not L then
        Dec(Dy);

      while Dy > 0 do
      begin
        Dec(Dy);
        D := EC;
        Inc(EC, EA);

        if EC <= D then
          Inc(X1, Sx);

        Inc(Y1, Sy);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI xor $FF]);

        Inc(P, Sx);
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI]);
      end;
    end else // DY <= DX
    begin
      EA := Dy shl 16 div Dx;
      if not L then
        Dec(Dx);

      while Dx > 0 do
      begin
        Dec(Dx);
        D := EC;

        Inc(EC, EA);
        if EC <= D then
          Inc(Y1, Sy);

        Inc(X1, Sx);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI xor $FF]);

        if Sy = 1 then
          Inc(P, Width) else Dec(P, Width);
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI]);
      end;
    end;

    EMMS;
  end;

  Changed(MakeRect(X1, Y1, X2, Y2), AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, tmp: Integer;
  CheckVert, CornerAA, TermClipped: Boolean;
  D1, D2: PInteger;
  EC, EA, ED, D: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
  ChangedRect: TRect;
begin
  if (FClipRect.Right = FClipRect.Left) or (FClipRect.Bottom = FClipRect.Top) then
    Exit;

  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin

    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Abs(Dx) = Abs(Dy) then // diagonal line?
    begin
      LineTS(X1, Y1, X2, Y2, Value, L);

      Exit;
    end else
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then
        VertLineTS(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        VertLineTS(X1, Y2 + 1, Y1, Value);

      if L then
      begin
        SetPixelTS(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end else
    if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then
        HorzLineTS(X1, Y1, X2 - 1, Value)
      else
      if Dx < 0 then
        HorzLineTS(X2 + 1, Y1, X1, Value);

      if L then
      begin
        SetPixelTS(X2, Y2, Value);
        Changed(MakeRect(X2, Y2, X2+1, Y2+1));
      end;

      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      if (X1 > Cx2) or (X2 < Cx1) then
        Exit; // segment not visible

      Sx := 1;
    end else
    begin
      if (X2 > Cx2) or (X1 < Cx1) then
        Exit; // segment not visible

      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      if (Y1 > Cy2) or (Y2 < Cy1) then
        Exit; // segment not visible

      Sy := 1;
    end else
    begin
      if (Y2 > Cy2) or (Y1 < Cy1) then
        Exit; // segment not visible

      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
      D1 := @yd; D2 := @xd;
      PI := Sy;
    end else
    begin
      D1 := @xd; D2 := @yd;
      PI := Sy * Width;
    end;

    rem := 0;
    EA := Dy shl 16 div Dx;
    EC := 0;
    xd := X1; yd := Y1;
    CheckVert := True;
    CornerAA := False;
    BlendMemEx := BLEND_MEM_EX[FCombineMode]^;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      tmp := (Cy1 - Y1) * 65536;
      rem := tmp - 65536; // rem := (Cy1 - Y1 - 1) * 65536;
      if tmp mod EA > 0 then
        tmp := tmp div EA + 1
      else
        tmp := tmp div EA;

      xd := Math.Min(xd + tmp, X2 + 1);
      EC := tmp * EA;

      if rem mod EA > 0 then
        rem := rem div EA + 1
      else
        rem := rem div EA;

      tmp := tmp - rem;

      // check whether the line is partly visible
      if xd > Cx2 then
      begin
        // do we need to draw an antialiased part on the corner of the clip rect?
        if xd <= Cx2 + tmp then
          CornerAA := True
        else
          Exit;
      end;

      if (xd {+ 1} >= Cx1) or CornerAA then
      begin
        yd := Cy1;
        rem := xd; // save old xd

        ED := EC - EA;
        term := SwapConstrain(xd - tmp, Cx1, Cx2);

        if CornerAA then
        begin
          Dec(ED, (xd - Cx2 - 1) * EA);
          xd := Cx2 + 1;
        end;

        // do we need to negate the vars?
        if Sy = -1 then
          yd := -yd;

        if Sx = -1 then
        begin
          xd := -xd;
          term := -term;
        end;

        // draw special case horizontal line entry (draw only last half of entering segment)
        while xd <> term do
        begin
          Inc(xd, -Sx);
          BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_ENCODING_TABLE[ED shr 8]);
          Dec(ED, EA);
        end;

        EMMS;

        if CornerAA then
        begin
          // we only needed to draw the visible antialiased part of the line,
          // everything else is outside of our cliprect, so exit now since
          // there is nothing more to paint...
          Changed(MakeRect(X1, Y1, X2, Y2), AREAINFO_LINE + 2);
          Exit;
        end;

        if Sy = -1 then
          yd := -yd;  // negate back
        xd := rem;  // restore old xd
        CheckVert := False; // to avoid ugly goto we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckVert and (X1 < Cx1) then
    begin
      tmp := (Cx1 - X1) * EA;
      Inc(yd, tmp div 65536);
      EC := tmp;
      xd := Cx1;
      if (yd > Cy2) then
        Exit // Nothing modified so far - no need to call Changed
      else
      if (yd = Cy2) then
        CornerAA := True;
    end;

    term := X2;
    TermClipped := False;
    CheckVert := False;

    // horizontal exit?
    if Y2 > Cy2 then
    begin
      tmp := (Cy2 - Y1) * 65536;
      term := X1 + tmp div EA;
      if not(tmp mod EA > 0) then
        Dec(Term);

      if term < Cx2 then
      begin
        rem := tmp + 65536; // was: rem := (Cy2 - Y1 + 1) * 65536;
        if rem mod EA > 0 then
          rem := X1 + rem div EA + 1
        else
          rem := X1 + rem div EA;

        if rem > Cx2 then
          rem := Cx2;
        CheckVert := True;
      end;

      TermClipped := True;
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      TermClipped := True;
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
      rem := -rem;
    end;

    // draw line
    if not CornerAA then
    begin
      // do we need to skip the last pixel of the line and is term not clipped?
      if not(L or TermClipped) and not CheckVert then
      begin
        if xd < term then
          Dec(term)
        else
        if xd > term then
          Inc(term);
      end;

      while xd <> term do
      begin
        CI := EC shr 8;
        P := @Bits[D1^ + D2^ * Width];
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI xor $FF]);

        Inc(P, PI);
        BlendMemEx(Value, P^, GAMMA_ENCODING_TABLE[CI]);

        // check for overflow and jump to next line...
        D := EC;
        Inc(EC, EA);
        if EC <= D then
          Inc(yd, Sy);

        Inc(xd, Sx);
      end;

      EMMS;
    end;

    // draw special case horizontal line exit (draw only first half of exiting segment)
    if CheckVert then
    begin
      while xd <> rem do
      begin
        BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_ENCODING_TABLE[EC shr 8 xor $FF]);
        Inc(EC, EA);
        Inc(xd, Sx);
      end;

      EMMS;
    end;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2); // +1 for AA
end;

procedure TCustomBitmap32.MoveTo(X, Y: Integer);
begin
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToS(X, Y: Integer);
begin
  LineS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToTS(X, Y: Integer);
begin
  LineTS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToAS(X, Y: Integer);
begin
  LineAS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.MoveToX(X, Y: TFixed);
begin
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.MoveToF(X, Y: Single);
begin
  RasterXF := Fixed(X);
  RasterYF := Fixed(Y);
end;

procedure TCustomBitmap32.LineToXS(X, Y: TFixed);
begin
  LineXS(RasterXF, RasterYF, X, Y, PenColor);
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.LineToFS(X, Y: Single);
begin
  LineToXS(Fixed(X), Fixed(Y));
end;

procedure TCustomBitmap32.LineToXSP(X, Y: TFixed);
begin
  LineXSP(RasterXF, RasterYF, X, Y);
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.LineToFSP(X, Y: Single);
begin
  LineToXSP(Fixed(X), Fixed(Y));
end;

procedure TCustomBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  j: Integer;
  P: PColor32Array;
begin
  if not FMeasuringMode then
  begin
    if (FBits <> nil) then
      for j := Y1 to Y2 - 1 do
      begin
        P := Pointer(@Bits[j * FWidth]);
        FillLongword(P[X1], X2 - X1, Value);
      end;
  end;

  Changed(MakeRect(X1, Y1, X2+1, Y2+1));
end;

procedure TCustomBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then
      X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then
      Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then
      X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then
      Y2 := FClipRect.Bottom;

    FillRect(X1, Y1, X2, Y2, Value); // Calls Changed()
  end;
end;

procedure TCustomBitmap32.FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  i, j: Integer;
  P: PColor32;
  A: Integer;
  ChangedRect: TRect;
begin
  A := Value shr 24;

  if A = $FF then
    FillRect(X1, Y1, X2, Y2, Value) // calls Changed...
  else
  if A <> 0 then
  begin
    ChangedRect := MakeRect(X1, Y1, X2 + 1, Y2 + 1);

    if not FMeasuringMode then
    begin
      Dec(Y2);
      Dec(X2);
      for j := Y1 to Y2 do
      begin
        P := GetPixelPtr(X1, j);
        if CombineMode = cmBlend then
        begin
          for i := X1 to X2 do
          begin
            CombineMem(Value, P^, A);
            Inc(P);
          end;
        end else
        begin
          for i := X1 to X2 do
          begin
            MergeMem(Value, P^);
            Inc(P);
          end;
        end;
      end;

      EMMS;
    end;

    Changed(ChangedRect);
  end;
end;

procedure TCustomBitmap32.FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then
      X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then
      Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then
      X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then
      Y2 := FClipRect.Bottom;

    FillRectT(X1, Y1, X2, Y2, Value); // Calls Changed()
  end;
end;

procedure TCustomBitmap32.FillRectS(const ARect: TRect; Value: TColor32);
begin
  FillRectS(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Value);
end;

procedure TCustomBitmap32.FillRectTS(const ARect: TRect; Value: TColor32);
begin
  FillRectTS(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineS(X1, Y1, X2, Value);

    if Y2 > Y1 then
      HorzLineS(X1, Y2, X2, Value);

    if Y2 > Y1 + 1 then
    begin
      VertLineS(X1, Y1 + 1, Y2 - 1, Value);

      if X2 > X1 then
        VertLineS(X2, Y1 + 1, Y2 - 1, Value);
    end;
  end;
end;

procedure TCustomBitmap32.FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineTS(X1, Y1, X2, Value);

    if Y2 > Y1 then
      HorzLineTS(X1, Y2, X2, Value);

    if Y2 > Y1 + 1 then
    begin
      VertLineTS(X1, Y1 + 1, Y2 - 1, Value);

      if X2 > X1 then
        VertLineTS(X2, Y1 + 1, Y2 - 1, Value);
    end;
  end;
end;

procedure TCustomBitmap32.FrameRectTSP(X1, Y1, X2, Y2: Integer);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < Width) and (Y1 < Height) and  // don't check against ClipRect here
    (X2 > 0) and (Y2 > 0) then          // due to StippleCounter
  begin
    Dec(X2);
    Dec(Y2);
    if X1 = X2 then
    begin
      if Y1 = Y2 then
      begin
        SetPixelT(X1, Y1, GetStippleColor);
        Changed(MakeRect(X1, Y1, X1 + 1, Y1 + 1));
      end else
        VertLineTSP(X1, Y1, Y2)
    end else
    begin
      if Y1 = Y2 then
        HorzLineTSP(X1, Y1, X2)
      else
      begin
        HorzLineTSP(X1, Y1, X2 - 1);
        VertLineTSP(X2, Y1, Y2 - 1);
        HorzLineTSP(X2, Y2, X1 + 1);
        VertLineTSP(X1, Y2, Y1 + 1);
      end;
    end;
  end;
end;

procedure TCustomBitmap32.FrameRectS(const ARect: TRect; Value: TColor32);
begin
  FrameRectS(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectTS(const ARect: TRect; Value: TColor32);
begin
  FrameRectTS(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Value);
end;

procedure TCustomBitmap32.RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer);
var
  C1, C2: TColor32;
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
     (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
     (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if (Contrast > 0) then
    begin
      C1 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
      C2 := SetAlpha(clBlack32, Clamp(Contrast * $FF div 100));
    end else
    if Contrast < 0 then
    begin
      Contrast := -Contrast;
      C1 := SetAlpha(clBlack32, Clamp(Contrast * $FF div 100));
      C2 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
    end else
      Exit;

    Dec(X2);
    Dec(Y2);
    HorzLineTS(X1, Y1, X2, C1);
    HorzLineTS(X1, Y2, X2, C2);

    Inc(Y1);
    Dec(Y2);
    VertLineTS(X1, Y1, Y2, C1);
    VertLineTS(X2, Y1, Y2, C2);
  end;
end;

procedure TCustomBitmap32.RaiseRectTS(const ARect: TRect; Contrast: Integer);
begin
  RaiseRectTS(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Contrast);
end;

function TCustomBitmap32.LoadFromBMPStream(Stream: TStream; Size: Int64): boolean;
var
  BitmapFileHeader: TBitmapFileHeader;
begin
  Result := False;

  if (Stream.Read(BitmapFileHeader, SizeOf(TBitmapFileHeader)) < SizeOf(TBitmapFileHeader)) then
    exit;

  // Verify file signature
  if (BitmapFileHeader.bfType <> $4D42) then // BM file signature
    exit;

  if (BitmapFileHeader.bfSize <> Size) then
    BitmapFileHeader.bfSize := Size;

  Result := LoadFromDIBStream(Stream, BitmapFileHeader.bfSize - SizeOf(TBitmapFileHeader));
end;


function TCustomBitmap32.LoadFromDIBStream(Stream: TStream; Size: Int64): boolean;

  function GetShift(Mask, TargetMask: DWORD): integer;
  begin
    Result := 0;
    if (Mask = 0) then
      exit;

    // Shift the mask all the way to the right, so the mask starts at bit 0
    while (Mask and 1 = 0) do
    begin
      Mask := Mask shr 1;
      Dec(Result);
    end;

    // Shift the target mask all the way to the right, so the mask starts at bit 0
    while (TargetMask and 1 = 0) do
    begin
      TargetMask := TargetMask shr 1;
      Inc(Result);
    end;

    // The result is the number of bits the mask must be shifted in order to align
    // with the target mask.
    // Positive value means shift left, negative means shift right.
  end;

  procedure EnsureAlpha;
  var
    Alpha: PByte;
    i: integer;
  begin
    // Since the alpha channel of this format isn't defined we can either assume
    // that all pixels have Alpha=255 or we can assume that the alpha is
    // specified in the source pixel data.
    // Instead of just chosing one of these and hoping for the best we make a
    // choice based on the actual alpha values: If the bitmap contains alpha
    // values then we leave it as is. If it doesn't contain alpha values then
    // we reset the alpha of the whole bitmap to 255.
    Alpha := @(PColor32Entry(Bits).A);

    for i := 0 to Width * Height - 1 do
    begin
      if (Alpha^ <> 0) then
        exit;

      Inc(Alpha, SizeOf(DWORD));
    end;

    ResetAlpha;
  end;

type
  TChannelMask = record
    Shift: integer;
    Mask: DWORD;
    Enabled: boolean;
  end;

var
  BitmapHeader: TBitmapHeader;
  ChunkSize: integer;
  InfoHeaderVersion: TInfoHeaderVersion;
  i, j: integer;
  Row: integer;
  DeltaRow: integer;
  Pixels: PDWord;
  ChannelMasks: array[TColor32Component] of TChannelMask;
  Channel: TColor32Component;
  Value, NewValue: DWORD;
  Padding: integer;
  DataSize: integer;
  ScanlineRow: PColor32Array;
const
{$IFNDEF RGBA_FORMAT}
  Masks: array[TColor32Component] of DWORD = ($000000FF, $0000FF00, $00FF0000, $FF000000); // BGRA
  ChannelToIndex: array[TColor32Component] of integer = (2, 1, 0, 3); // Maps TColor32Component to RGBA index
{$ELSE}
  Masks: array[TColor32Component] of DWORD = ($00FF0000, $0000FF00, $000000FF, $FF000000); // RGBA
  ChannelToIndex: array[TColor32Component] of integer = (0, 1, 2, 3); // Maps TColor32Component to RGBA index
{$ENDIF}
begin
  Result := False;

  FillChar(BitmapHeader, SizeOf(TBitmapHeader), 0);

  // Read the info header size field
  Dec(Size, SizeOf(BitmapHeader.InfoHeader.biSize));
  if (Size < 0) then
    exit;
  if (Stream.Read(BitmapHeader.InfoHeader.biSize, SizeOf(BitmapHeader.InfoHeader.biSize)) <> SizeOf(BitmapHeader.InfoHeader.biSize)) then
    exit;

  // Validate info header size and determine header version
  case BitmapHeader.InfoHeader.biSize of
    SizeOf(TBitmapCoreHeader): exit;
    // Size of TBitmapCoreHeader2 vary from 16 to 64 but we assume it's 64
    SizeOf(TBitmapCoreHeader2):exit;
    SizeOf(TBitmapInfoHeader): InfoHeaderVersion := InfoHeaderVersion1;
    SizeOf(TBitmapV2Header):   InfoHeaderVersion := InfoHeaderVersion2;
    SizeOf(TBitmapV3Header):   InfoHeaderVersion := InfoHeaderVersion3;
    SizeOf(TBitmapV4Header):   InfoHeaderVersion := InfoHeaderVersion4;
    SizeOf(TBitmapV5Header):   InfoHeaderVersion := InfoHeaderVersion5;
  else
    if (BitmapHeader.InfoHeader.biSize < SizeOf(TBitmapInfoHeader)) then
      exit;

    // Guard against bogus header size. Limit is arbitrary.
    if (BitmapHeader.InfoHeader.biSize >= 4096) then
      exit;

    // Assume header is v1 compatible
    InfoHeaderVersion := InfoHeaderVersion1;
  end;

  // Get the rest of the bitmap info header
  ChunkSize := BitmapHeader.InfoHeader.biSize - SizeOf(BitmapHeader.InfoHeader.biSize);
  Dec(Size, ChunkSize);
  if (Size < 0) then
    exit;
  if (Stream.Read(BitmapHeader.InfoHeader.biWidth, ChunkSize) <> ChunkSize) then
    exit;

  // We only support 24-bit and 32-bit bitmaps
  if not (BitmapHeader.InfoHeader.biBitCount in [24, 32]) then
    exit;

  // Planes must be 1
  if (BitmapHeader.InfoHeader.biPlanes <> 1) then
    BitmapHeader.InfoHeader.biPlanes := 1;

  // Width must be positive
  if (BitmapHeader.InfoHeader.biWidth < 0) then
    BitmapHeader.InfoHeader.biWidth := -BitmapHeader.InfoHeader.biWidth;

  // Pad input rows to 32 bits
  Padding := (SizeOf(DWORD) - (((BitmapHeader.InfoHeader.biBitCount shr 3) * BitmapHeader.InfoHeader.biWidth) and (SizeOf(DWORD)-1))) and (SizeOf(DWORD)-1);
  DataSize := ((BitmapHeader.InfoHeader.biBitCount shr 3) * BitmapHeader.InfoHeader.biWidth + Padding) * Abs(BitmapHeader.InfoHeader.biHeight);
  Dec(Size, DataSize);

  if (BitmapHeader.InfoHeader.biCompression = BI_RGB) then
  begin
    // Skip color table so we're ready to read the pixel data.
    // Note: We ignore the pixel offset stored in the header since this
    // value is often incorrect.
    if (BitmapHeader.InfoHeader.biClrUsed > 0) then
    begin
      Dec(Size, BitmapHeader.InfoHeader.biClrUsed * SizeOf(DWORD));
      if (Size < 0) then
        exit;
      Stream.Seek(BitmapHeader.InfoHeader.biClrUsed * SizeOf(DWORD), soCurrent);
    end;
  end else
  if (BitmapHeader.InfoHeader.biCompression = BI_BITFIELDS) then
  begin
    // BI_BITFIELDS is only valid for 16 and 32 bit count
    if BitmapHeader.InfoHeader.biBitCount = 24 then
      exit;

    // For versions > v1 the header contains a color mask.
    // For BI_BITFIELDS the header is additionally followed by a color table with 3
    // values that can also be interpreted as a color mask.
    // Because the BI_BITFIELDS layout has been so poorly documented different
    // interpretations of it has lead to some implementations including the color
    // table and some excluding it.
    // This is true even within Windows and its utilities. In particular the Windows
    // clipboard's handling of CF_DIBV5 appears to produce both variations depending on
    // various circumstances.
    //
    // Since Windows seems able to work around the two different variations we'll try to
    // do that too; If there's exactly 12 bytes missing when we're about to read the
    // color table, then we simply skip reading it.

    // For version 1 we always need to get the mask from the color table.
    if (InfoHeaderVersion = InfoHeaderVersion1) then
    begin
      // Read the RGB mask into the v2 header RGB mask fields
      ChunkSize := 3 * SizeOf(DWORD);
      Dec(Size, ChunkSize);
      if (Size < 0) then
        exit;

      if (Stream.Read(BitmapHeader.V2Header.bV2RedMask, ChunkSize) <> ChunkSize) then
        exit;
    end else
    begin
      // Read the color table if it's there.

      // Work around BI_BITFIELDS DIBs that lack the color table after the header;
      // If there's exactly 12 bytes missing then we assume that these are the missing
      // color table and ignore it.
      ChunkSize := 3 * SizeOf(DWORD);
      if (Size >= ChunkSize) then
      begin
        Dec(Size, ChunkSize);
        // The color table is there but do we need to actually read it?
        // If the header color masks contains values then we don't need the values in
        // the color table so we just skip past them. Otherwise we read them into the
        // header color mask fields.
        if (BitmapHeader.V2Header.bV2RedMask = 0) and (BitmapHeader.V2Header.bV2GreenMask = 0) and (BitmapHeader.V2Header.bV2BlueMask = 0) then
        begin
          if (Stream.Read(BitmapHeader.V2Header.bV2RedMask, ChunkSize) <> ChunkSize) then
            exit;
        end else
          Stream.Seek(ChunkSize, soFromCurrent);
      end;
    end;

    // Check if RGBA mask is present and values are valid
    // Validate RGB mask; All components must have a value and the values must not overlap.
    if (BitmapHeader.V2Header.bV2RedMask = 0) or (BitmapHeader.V2Header.bV2GreenMask = 0) or (BitmapHeader.V2Header.bV2BlueMask = 0) or
      ((BitmapHeader.V2Header.bV2RedMask and BitmapHeader.V2Header.bV2GreenMask <> 0) or
       (BitmapHeader.V2Header.bV2RedMask and BitmapHeader.V2Header.bV2BlueMask <> 0) or
       (BitmapHeader.V2Header.bV2GreenMask and BitmapHeader.V2Header.bV2BlueMask <> 0)) then
    begin
      // Reset to 888
      BitmapHeader.V2Header.bV2RedMask := Masks[ccRed];
      BitmapHeader.V2Header.bV2GreenMask := Masks[ccGreen];
      BitmapHeader.V2Header.bV2BlueMask := Masks[ccBlue];
    end;

    // Validate A mask. Value is optional but must not overlap RGB mask.
    if (InfoHeaderVersion >= InfoHeaderVersion3) then
    begin
      if (BitmapHeader.V3Header.bV3AlphaMask and (BitmapHeader.V3Header.bV3RedMask or BitmapHeader.V3Header.bV3GreenMask or BitmapHeader.V3Header.bV3BlueMask) <> 0) then
        // Reset alpha
        BitmapHeader.V3Header.bV3AlphaMask := $FFFFFFFF and
          not(BitmapHeader.V3Header.bV3RedMask or BitmapHeader.V3Header.bV3GreenMask or BitmapHeader.V3Header.bV3BlueMask);
    end;

    // If the color mask corresponds to the BI_RGB layout then we change
    // the compression type to BI_RGB so we can read the pixels with
    // the simpler BI_RGB method.
    if (InfoHeaderVersion <= InfoHeaderVersion2) then
    begin
      if (BitmapHeader.V2Header.bV2RedMask = Masks[ccRed]) and
        (BitmapHeader.V2Header.bV2GreenMask = Masks[ccGreen]) and
        (BitmapHeader.V2Header.bV2BlueMask = Masks[ccBlue]) then
        BitmapHeader.InfoHeader.biCompression := BI_RGB;
    end else
    begin
      if (BitmapHeader.V3Header.bV3RedMask = Masks[ccRed]) and
        (BitmapHeader.V3Header.bV3GreenMask = Masks[ccGreen]) and
        (BitmapHeader.V3Header.bV3BlueMask = Masks[ccBlue]) and
        (BitmapHeader.V3Header.bV3AlphaMask = Masks[ccAlpha]) then
        BitmapHeader.InfoHeader.biCompression := BI_RGB;
    end;
  end else
    // We only support BI_RGB and BI_BITFIELDS compression
    exit;

  // Make sure there's enough data left for the pixels
  if (Size < 0) then
    exit;

  // Set bitmap size and allocate bit pixel data so we can read into it
  SetSize(BitmapHeader.InfoHeader.biWidth, Abs(BitmapHeader.InfoHeader.biHeight));

  // Check whether the bitmap is saved top-down or bottom-up:
  // - Negavive height: top-down
  // - Positive height: bottom-up
  if (BitmapHeader.InfoHeader.biHeight > 0) then
  begin
    // Bitmap is stored bottom-up
    Row := Height-1;
    DeltaRow := -1;
  end else
  begin
    // Bitmap is stored top-down
    Row := 0;
    DeltaRow := 1;
  end;


  if (BitmapHeader.InfoHeader.biCompression = BI_RGB) then
  begin

    case BitmapHeader.InfoHeader.biBitCount of
      24:
        // Read one RGB pixel at a time
        for i := 0 to Height - 1 do
        begin
          ScanlineRow := Scanline[Row];
          for j := 0 to Width - 1 do
          begin
            // Read RGB data and reset alpha
            Stream.ReadBuffer(ScanlineRow[j], 3);
            TColor32Entry(ScanlineRow[j]).A := $FF;
          end;

          if (Padding > 0) then
            Stream.Seek(Padding, soCurrent);

          Inc(Row, DeltaRow);
        end;

      32:
        begin
          Assert(Padding = 0);

          if (BitmapHeader.InfoHeader.biHeight > 0) then
          begin
            // Bitmap is stored bottom-up: Read one row at a time
            ChunkSize := Width * SizeOf(DWORD);
            for i := Height - 1 downto 0 do
              Stream.ReadBuffer(Scanline[i]^, ChunkSize);
          end else
            // Bitmap is stored top-down: Read all rows in one go
            Stream.ReadBuffer(Bits^, Width * Height * SizeOf(DWORD))
        end;
    end;

    if (InfoHeaderVersion < InfoHeaderVersion3) then
      EnsureAlpha;

{$IFDEF RGBA_FORMAT}
    // BMP stores pixels in ABGR order but we need them in ARGB order.
    // Swap R and B channels
    for i := 0 to FHeight * FWidth - 1 do
      SwapRedBlueMem(FBits[i]);
{$ENDIF}

  end else
  begin
    Assert(Padding = 0);

    // Determine how much we need to shift the masked color values in order
    // to get them into the desired position.
    for Channel := Low(TColor32Component) to High(TColor32Component) do
    begin
      ChannelMasks[Channel].Shift := GetShift(BitmapHeader.Header.bmiColors[ChannelToIndex[Channel]], Masks[Channel]);
      ChannelMasks[Channel].Mask := Masks[Channel];
      ChannelMasks[Channel].Enabled := (Channel <> ccAlpha) or (InfoHeaderVersion >= InfoHeaderVersion3);
    end;

    // Read one row at a time into our bitmap and then decode it in place.
    ChunkSize := Width * SizeOf(DWORD);
    for i := 0 to Height-1 do
    begin
      Pixels := PDWord(Scanline[Row]);
      Stream.ReadBuffer(Pixels^, ChunkSize);

      for j := 0 to Width-1 do
      begin
        Value := Pixels^;
        NewValue := 0;

        for Channel := Low(TColor32Component) to High(TColor32Component) do
        begin
          if (ChannelMasks[Channel].Enabled) then
          begin
            if (ChannelMasks[Channel].Shift > 0) then
              NewValue := NewValue or ((Value shl ChannelMasks[Channel].Shift) and ChannelMasks[Channel].Mask)
            else
            if (ChannelMasks[Channel].Shift < 0) then
              NewValue := NewValue or ((Value shr -ChannelMasks[Channel].Shift) and ChannelMasks[Channel].Mask)
            else
              NewValue := NewValue or (Value and ChannelMasks[Channel].Mask);
          end else
            NewValue := NewValue or ChannelMasks[Channel].Mask; // Set alpha=255
        end;

        Pixels^ := NewValue;
        Inc(Pixels);
      end;

      Inc(Row, DeltaRow);
    end;

{$IFDEF RGBA_FORMAT}
    // For BI_BITFIELDS we do not need to swap the R and B channels at the end because
    // we have already done it by adjusting the Masks and ChannelToIndex arrays.
{$ENDIF}
  end;

  Result := True;
end;

procedure TCustomBitmap32.LoadFromStream(Stream: TStream);
var
  SavePos: Int64;
begin
  SavePos := Stream.Position;

  if (not ImageFormatManager.Readers.LoadFromStream(Self, Stream)) then
  begin
    Stream.Position := SavePos;
    raise Exception.Create(sUnknownImageFormat);
  end;

  Changed;
end;

procedure TCustomBitmap32.SaveToStream(Stream: TStream; SaveTopDown: Boolean);
begin
  SaveToStream(Stream, SaveTopDown, DefaultBitmapHeaderVersion);
end;

procedure TCustomBitmap32.SaveToStream(Stream: TStream; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion);
var
  FileHeader: TBitmapFileHeader;
  BitmapSize: Integer;
  HeaderSize: integer;
begin
  BitmapSize := Width * Height * SizeOf(DWORD);

  case InfoHeaderVersion of
    InfoHeaderVersion1: HeaderSize := SizeOf(TBitmapInfoHeader);// 40
    InfoHeaderVersion2: HeaderSize := SizeOf(TBitmapV2Header);  // 52
    InfoHeaderVersion3: HeaderSize := SizeOf(TBitmapV3Header);  // 56
    InfoHeaderVersion4: HeaderSize := SizeOf(TBitmapV4Header);  // 108
    InfoHeaderVersion5: HeaderSize := SizeOf(TBitmapV5Header);  // 124
  else
    raise Exception.Create('Invalid header version');
  end;

  FileHeader.bfType := $4D42; // Magic bytes for Windows Bitmap
  FileHeader.bfSize := BitmapSize + SizeOf(TBitmapFileHeader) + HeaderSize; // Size of file
  FileHeader.bfReserved1 := 0;
  FileHeader.bfReserved2 := 0;
  // The offset, in bytes, from the beginning of the BITMAPFILEHEADER structure to the bitmap bits.
  FileHeader.bfOffBits := SizeOf(TBitmapFileHeader) + HeaderSize;

  // Note that the above offsets and sizes doesn't include a potential BI_BITFIELDS
  // color mask. This doesn't appear to be a problem since nobody (wisely) uses the
  // values in these fields anyway.

  Stream.WriteBuffer(FileHeader, SizeOf(FileHeader));

  SaveToDIBStream(Stream, SaveTopDown, InfoHeaderVersion);
end;

procedure TCustomBitmap32.SaveToDIBStream(Stream: TStream; SaveTopDown: Boolean);
begin
  SaveToDIBStream(Stream, SaveTopDown, DefaultBitmapHeaderVersion);
end;

procedure TCustomBitmap32.SaveToDIBStream(Stream: TStream; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion);
type
  TDIBHeader = packed record
  case TInfoHeaderVersion of
    InfoHeaderVersion1: (InfoHeader: TBitmapInfoHeader);        // 40
    InfoHeaderVersion2: (V2Header: TBitmapV2Header);            // 52
    InfoHeaderVersion3: (V3Header: TBitmapV3Header);            // 56
    InfoHeaderVersion4: (V4Header: TBitmapV4Header);            // 108
    InfoHeaderVersion5: (V5Header: TBitmapV5Header);            // 124
  end;

{$IFDEF FPC}
const
  LCS_GM_IMAGES = 4;
{$ENDIF}
var
  Header: TDIBHeader;
  i: Integer;
  W: Integer;
begin
  Header := Default(TDIBHeader);

  // Determine info header size based on header version.
  // Note: Formats lower than InfoHeaderVersion3 doesn't formally
  // support alpha but that doesn't mean that we can't store the alpha
  // anyway.
  case InfoHeaderVersion of
    InfoHeaderVersion1: Header.InfoHeader.biSize := SizeOf(TBitmapInfoHeader);// 40
    InfoHeaderVersion2: Header.InfoHeader.biSize := SizeOf(TBitmapV2Header);  // 52
    InfoHeaderVersion3: Header.InfoHeader.biSize := SizeOf(TBitmapV3Header);  // 56
    InfoHeaderVersion4: Header.InfoHeader.biSize := SizeOf(TBitmapV4Header);  // 108
    InfoHeaderVersion5: Header.InfoHeader.biSize := SizeOf(TBitmapV5Header);  // 124
  else
    raise Exception.Create('Invalid DIB header version');
  end;

  Header.InfoHeader.biWidth := Width;

  if SaveTopDown then
    Header.InfoHeader.biHeight := -Height
  else
    Header.InfoHeader.biHeight := Height;

  Header.InfoHeader.biPlanes := 1;
  Header.InfoHeader.biBitCount := 32; // This implementation only support writing 32-bit format
  Header.InfoHeader.biCompression := BI_RGB; // BI_RGB or BI_BITFIELDS
  Header.InfoHeader.biSizeImage := Width * Height * SizeOf(DWORD);
  Header.InfoHeader.biXPelsPerMeter := 0;
  Header.InfoHeader.biYPelsPerMeter := 0;
  Header.InfoHeader.biClrUsed := 0; // No palette
  Header.InfoHeader.biClrImportant := 0;

  if (InfoHeaderVersion >= InfoHeaderVersion3) then
  begin
    // First header version to formally support Alpha. But this requires that we use
    // BI_BITFIELDS compression.
    Header.InfoHeader.biCompression := BI_BITFIELDS; // Switch from BI_RGB
    Header.V3Header.bV3AlphaMask := $FF000000;
  end;

  if (Header.InfoHeader.biCompression = BI_BITFIELDS) then
  begin
    // We only support the bit masks that correspond directly to the ABGR format.
    // Also note that WIC, among others, ignores the alpha if we use a different mask.
    Header.V2Header.bV2RedMask := $00FF0000;
    Header.V2Header.bV2GreenMask := $0000FF00;
    Header.V2Header.bV2BlueMask := $000000FF;
  end;

  if (InfoHeaderVersion >= InfoHeaderVersion4) then
    Header.V4Header.bV4CSType := $73524742;// LCS_sRGB

  if (InfoHeaderVersion >= InfoHeaderVersion5) then
    Header.V5Header.bV5Intent := LCS_GM_IMAGES;

  Stream.WriteBuffer(Header, Header.InfoHeader.biSize);

  // Write the color table.
  // This is just a duplication of the first three fields of the header color mask.
  if (Header.InfoHeader.biCompression = BI_BITFIELDS) then
    Stream.WriteBuffer(Header.V2Header.bV2RedMask, 3*SizeOf(DWORD));

  // Pixel array
{$IFNDEF RGBA_FORMAT}
  if SaveTopDown then
  begin
    // NOTE: We can save the whole buffer in one run because
    // we do not support scanline strides (yet).
    Stream.WriteBuffer(Bits^, Header.InfoHeader.biSizeImage);
  end
  else
  begin
    W := Width * SizeOf(DWORD);
    for i := Height - 1 downto 0 do
      Stream.WriteBuffer(ScanLine[i]^, W);
  end;
{$ELSE RGBA_FORMAT}
  // Bits are in RGBA format but we need to write BGRA
  if SaveTopDown then
  begin
    for i := 0 to Width*Height-1 do
      Stream.Write(SwapRedBlue(Bits[i]), SizeOf(TColor32));
  end
  else
  begin
    for i := Height - 1 downto 0 do
      for W := 0 to Width-1 do
        Stream.Write(SwapRedBlue(ScanLine[i][W]), SizeOf(TColor32));
  end;
{$ENDIF RGBA_FORMAT}
end;

procedure TCustomBitmap32.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
{$if (not defined(FPC)) and (CompilerVersion >= 31.0)} // TBufferedFileStream was introduced in Delphi 10.1
  FileStream := TBufferedFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
{$else}
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
{$ifend}
  try

    if (ImageFormatManager.Readers.LoadFromStream(Self, FileStream, FileName)) then
    begin
      Changed;
      exit;
    end else
    if (ImageFormatManager.Readers.LoadFromStream(Self, FileStream)) then
    begin
      Changed;
      exit;
    end;

  finally
    FileStream.Free;
  end;

  if (ImageFormatManager.Readers.LoadFromFile(Self, FileName)) then
  begin
    Changed;
    exit;
  end;

  raise Exception.Create(sUnknownImageFormat);
end;

procedure TCustomBitmap32.SaveToFile(const FileName: string);
var
  Extension: string;
  Writer: IImageFormatWriter;
  FileStream: TFileStream;
begin
  Extension := Copy(ExtractFileExt(FileName), 2, MaxInt);

  Writer := ImageFormatManager.Writers.FindWriter(Extension);

  if (Writer <> nil) then
  begin
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      Writer.SaveToStream(Self, FileStream);
    finally
      FileStream.Free;
    end;
  end else
    SaveToFile(FileName, False);
end;

procedure TCustomBitmap32.SaveToFile(const FileName: string; SaveTopDown: Boolean);
begin
  SaveToFile(FileName, SaveTopDown, DefaultBitmapHeaderVersion);
end;

procedure TCustomBitmap32.SaveToFile(const FileName: string; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(FileStream, SaveTopDown, InfoHeaderVersion);
  finally
    FileStream.Free;
  end;
end;

procedure TCustomBitmap32.LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: TResourceType);
var
  Stream: TStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, ResType);
  try

    if (not ImageFormatManager.Readers.LoadFromResource(Self, ResType, Stream)) then
      raise Exception.Create(sUnknownImageFormat);

    Changed;

  finally
    Stream.Free;
  end;
end;

procedure TCustomBitmap32.LoadFromResourceName(Instance: THandle; const ResName: string; ResType: TResourceType);
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  try

    if (not ImageFormatManager.Readers.LoadFromResource(Self, ResType, Stream)) then
      raise Exception.Create(sUnknownImageFormat);

    Changed;

  finally
    Stream.Free;
  end;
end;

function TCustomBitmap32.Equal(B: TCustomBitmap32): Boolean;
var
  S1, S2: TMemoryStream;
begin
  Result := (B <> nil) and (ClassType = B.ClassType);

  if Empty or B.Empty then
  begin
    Result := Empty and B.Empty;
    Exit;
  end;

  if Result then
  begin
    S1 := TMemoryStream.Create;
    try
      SaveToStream(S1);
      S2 := TMemoryStream.Create;
      try
        B.SaveToStream(S2);
        Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
      finally
        S2.Free;
      end;
    finally
      S1.Free;
    end;
  end;
end;

procedure TCustomBitmap32.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TCustomBitmap32) or
        not Equal(TCustomBitmap32(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

procedure TCustomBitmap32.ReadData(Stream: TStream);
var
  Width, Height: Integer;
begin
  try
    Stream.ReadBuffer(Width, 4);
    Stream.ReadBuffer(Height, 4);
    SetSize(Width, Height);
    Stream.ReadBuffer(Bits[0], FWidth * FHeight * 4);
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.WriteData(Stream: TStream);
begin
  Stream.WriteBuffer(FWidth, 4);
  Stream.WriteBuffer(FHeight, 4);
  Stream.WriteBuffer(Bits[0], FWidth * FHeight * 4);
end;

procedure TCustomBitmap32.SetCombineMode(const Value: TCombineMode);
begin
  if FCombineMode <> Value then
  begin
    FCombineMode := Value;
    BlendProc := @BLEND_MEM[FCombineMode]^;
    Changed;
  end;
end;

procedure TCustomBitmap32.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TCustomBitmap32.SetWrapMode(Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapProcHorz := GetWrapProcEx(WrapMode, FClipRect.Left, FClipRect.Right - 1);
    WrapProcVert := GetWrapProcEx(WrapMode, FClipRect.Top, FClipRect.Bottom - 1);
    Changed;
  end;
end;

procedure TCustomBitmap32.SetMasterAlpha(Value: Cardinal);
begin
  if FMasterAlpha <> Value then
  begin
    FMasterAlpha := Value;
    Changed;
  end;
end;

procedure TCustomBitmap32.Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);
var
  Shift, L: Integer;
  R: TRect;
begin
  if Empty or ((Dx = 0) and (Dy = 0)) then
    Exit;

  if not FMeasuringMode then
  begin
    if (Abs(Dx) >= Width) or (Abs(Dy) >= Height) then
    begin
      if FillBack then
        Clear(FillColor);

      Exit;
    end;

    Shift := Dx + Dy * Width;
    L := (Width * Height - Abs(Shift));

    if Shift > 0 then
      Move(Bits[0], Bits[Shift], L shl 2)
    else
      MoveLongword(Bits[-Shift], Bits[0], L);

    if FillBack then
    begin
      R := MakeRect(0, 0, Width, Height);
      OffsetRect(R, Dx, Dy);
      IntersectRect(R, R, MakeRect(0, 0, Width, Height));

      if R.Top > 0 then
        FillRect(0, 0, Width, R.Top, FillColor)
      else
      if R.Top = 0 then
        FillRect(0, R.Bottom, Width, Height, FillColor);

      if R.Left > 0 then
        FillRect(0, R.Top, R.Left, R.Bottom, FillColor)
      else
      if R.Left = 0 then
        FillRect(R.Right, R.Top, Width, R.Bottom, FillColor);
    end;
  end;

  Changed;
end;

procedure TCustomBitmap32.FlipHorz(Dst: TCustomBitmap32);
var
  i, j: Integer;
  P1, P2: PColor32;
  tmp: TColor32;
  W, W2: Integer;
begin
  W := Width;
  if (Dst = nil) or (Dst = Self) then
  begin
    if not FMeasuringMode then
    begin
      { In-place flipping }
      P1 := PColor32(Bits);
      P2 := P1;
      Inc(P2, Width - 1);
      W2 := Width shr 1;
      for J := 0 to Height - 1 do
      begin
        for I := 0 to W2 - 1 do
        begin
          tmp := P1^;
          P1^ := P2^;
          P2^ := tmp;
          Inc(P1);
          Dec(P2);
        end;
        Inc(P1, W - W2);
        Inc(P2, W + W2);
      end;
    end;
    Changed;
  end
  else
  begin
    Dst.BeginUpdate;
    { Flip to Dst }
    if not FMeasuringMode then
    begin
      Dst.SetSize(W, Height);
      P1 := PColor32(Bits);
      P2 := PColor32(Dst.Bits);
      Inc(P2, W - 1);
      for J := 0 to Height - 1 do
      begin
        for I := 0 to W - 1 do
        begin
          P2^ := P1^;
          Inc(P1);
          Dec(P2);
        end;
        Inc(P2, W shl 1);
      end;
    end;
    Dst.Changed;
    Dst.EndUpdate;
  end;
end;

procedure TCustomBitmap32.FlipVert(Dst: TCustomBitmap32);
var
  J, J2: Integer;
  Buffer: PColor32Array;
  P1, P2: PColor32;
begin
  // TODO : MeasuringMode
  if (Dst = nil) or (Dst = Self) then
  begin
    { in-place }
    if not FMeasuringMode then
    begin
      J2 := Height - 1;
      GetMem(Buffer, Width shl 2);
      for J := 0 to Height div 2 - 1 do
      begin
        P1 := PColor32(ScanLine[J]);
        P2 := PColor32(ScanLine[J2]);
        MoveLongword(P1^, Buffer^, Width);
        MoveLongword(P2^, P1^, Width);
        MoveLongword(Buffer^, P2^, Width);
        Dec(J2);
      end;
      FreeMem(Buffer);
    end;
    Changed;
  end
  else
  begin
    if not FMeasuringMode then
    begin
      Dst.SetSize(Width, Height);
      J2 := Height - 1;
      for J := 0 to Height - 1 do
      begin
        MoveLongword(ScanLine[J]^, Dst.ScanLine[J2]^, Width);
        Dec(J2);
      end;
    end;
    Dst.Changed;
  end;
end;

procedure TCustomBitmap32.Rotate90(Dst: TCustomBitmap32);
var
  Tmp: TCustomBitmap32;
  X, Y, I, J: Integer;
begin
  if not FMeasuringMode then
  begin
    if (Dst = nil) or (Dst = Self) then
    begin
      Tmp := TCustomBitmap32.Create; // TODO : Use TMemoryBackend
      Dst := Tmp;
    end else
      Tmp := nil;
    try

      Dst.BeginUpdate;
      try

        Dst.SetSize(Height, Width);
        I := 0;
        for Y := 0 to Height - 1 do
        begin
          J := Height - 1 - Y;
          for X := 0 to Width - 1 do
          begin
            Dst.Bits[J] := Bits[I];
            Inc(I);
            Inc(J, Height);
          end;
        end;

        Dst.Changed;
      finally
        Dst.EndUpdate;
      end;

    finally
      if (Tmp <> nil) then
      begin
        Tmp.CopyMapTo(Self);
        Tmp.Free;
      end;
    end;
  end else
  if (Dst = nil) or (Dst = Self) then
    Changed
  else
    Dst.Changed;
end;

procedure TCustomBitmap32.Rotate180(Dst: TCustomBitmap32);
var
  I, I2: Integer;
  Tmp: TColor32;
begin
  if (Dst <> nil) and (Dst <> Self) then
  begin
    if not FMeasuringMode then
    begin
      Dst.SetSize(Width, Height);
      I2 := Width * Height - 1;
      for I := 0 to Width * Height - 1 do
      begin
        Dst.Bits[I2] := Bits[I];
        Dec(I2);
      end;
    end;
    Dst.Changed;
  end
  else
  begin
    if not FMeasuringMode then
    begin
      I2 := Width * Height - 1;
      for I := 0 to Width * Height div 2 - 1 do
      begin
        Tmp := Bits[I2];
        Bits[I2] := Bits[I];
        Bits[I] := Tmp;
        Dec(I2);
      end;
    end;
    Changed;
  end;
end;

procedure TCustomBitmap32.Rotate270(Dst: TCustomBitmap32);
var
  Tmp: TCustomBitmap32;
  X, Y, I, J: Integer;
begin
  if not FMeasuringMode then
  begin
    if (Dst = nil) or (Dst = Self) then
    begin
      Tmp := TCustomBitmap32.Create; { TODO : Revise creating of temporary bitmaps here... }
       // TODO : Use TMemoryBackend
      Dst := Tmp;
    end else
      Tmp := nil;
    try

      Dst.BeginUpdate;
      try

        Dst.SetSize(Height, Width);
        I := 0;
        for Y := 0 to Height - 1 do
        begin
          J := (Width - 1) * Height + Y;
          for X := 0 to Width - 1 do
          begin
            Dst.Bits[J] := Bits[I];
            Inc(I);
            Dec(J, Height);
          end;
        end;

        Dst.Changed;
      finally
        Dst.EndUpdate;
      end;

    finally
      if (Tmp <> nil) then
      begin
        Tmp.CopyMapTo(Self);
        Tmp.Free;
      end;
    end;
  end else
  if (Dst = nil) or (Dst = Self) then
    Changed
  else
    Dst.Changed;
end;

function TCustomBitmap32.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TCustomBitmap32.SetClipRect(const Value: TRect);
begin
  IntersectRect(FClipRect, Value, BoundsRect);
  FFixedClipRect := FixedRect(FClipRect);
  with FClipRect do
    F256ClipRect := Rect(Left shl 8, Top shl 8, Right shl 8, Bottom shl 8);
  FClipping := not EqualRect(FClipRect, BoundsRect);
  WrapProcHorz := GetWrapProcEx(WrapMode, FClipRect.Left, FClipRect.Right - 1);
  WrapProcVert := GetWrapProcEx(WrapMode, FClipRect.Top, FClipRect.Bottom - 1);
end;

procedure TCustomBitmap32.ResetClipRect;
begin
  ClipRect := BoundsRect;
end;

procedure TCustomBitmap32.BeginMeasuring(const Callback: TAreaChangedEvent);
begin
  FMeasuringMode := True;
  FOldOnAreaChanged := FOnAreaChanged;
  FOnAreaChanged := Callback;
end;

procedure TCustomBitmap32.EndMeasuring;
begin
  FMeasuringMode := False;
  FOnAreaChanged := FOldOnAreaChanged;
end;

procedure TCustomBitmap32.PropertyChanged;
begin
  // don't force invalidation of whole bitmap area as this is unnecessary
  inherited Changed;
end;

procedure TCustomBitmap32.Changed;
begin
  if ((LockUpdateCount = 0) or FMeasuringMode) and Assigned(FOnAreaChanged) then
    FOnAreaChanged(Self, BoundsRect, AREAINFO_RECT);

  if not FMeasuringMode then
    inherited;
end;

procedure TCustomBitmap32.Changed(const Area: TRect; const Info: Cardinal);
begin
  if ((LockUpdateCount = 0) or FMeasuringMode) and Assigned(FOnAreaChanged) then
    FOnAreaChanged(Self, Area, Info);

  if not FMeasuringMode then
    inherited Changed;
end;

procedure TCustomBitmap32.SetResampler(AResampler: TCustomResampler);
begin
  if (AResampler <> nil) and (FResampler <> AResampler) then
  begin
    FResampler.Free;
    FResampler := AResampler;
    Changed;
  end;
end;

function TCustomBitmap32.GetResamplerClassName: string;
begin
  Result := FResampler.ClassName;
end;

procedure TCustomBitmap32.SetResamplerClassName(const Value: string);
var
  ResamplerClass: TCustomResamplerClass;
begin
  if (Value <> '') and (FResampler.ClassName <> Value) and (ResamplerList <> nil) then
  begin
    ResamplerClass := TCustomResamplerClass(ResamplerList.Find(Value));
    if (ResamplerClass <> nil) then
      ResamplerClass.Create(Self);
  end;
end;

{ TBitmap32 }

procedure TBitmap32.FinalizeBackend;
var
  FontSupport: IFontSupport;
  CanvasSupport: ICanvasSupport;
begin
  if Supports(Backend, IFontSupport, FontSupport) then
  begin
    FontSupport.OnFontChange := nil;
    FontSupport := nil;
  end;

  if Supports(Backend, ICanvasSupport, CanvasSupport) then
  begin
    CanvasSupport.OnCanvasChange := nil;
    CanvasSupport := nil;
  end;

  inherited;
end;

procedure TBitmap32.BackendChangingHandler(Sender: TObject);
begin
  inherited;
  FontChanged(Self);
  DeleteCanvas;
end;

procedure TBitmap32.BackendChangedHandler(Sender: TObject);
begin
  inherited;
  HandleChanged;
end;

procedure TBitmap32.FontChanged(Sender: TObject);
begin
  // TODO: still required?
end;

procedure TBitmap32.CanvasChanged(Sender: TObject);
begin
  Changed;
end;

procedure TBitmap32.CopyPropertiesTo(Dst: TCustomBitmap32);
var
  FontSupport, DstFontSupport: IFontSupport;
begin
  inherited;

  if (Dst is TBitmap32) and
    Supports(Dst.Backend, IFontSupport, DstFontSupport) and Supports(Self.Backend, IFontSupport, FontSupport) then
    DstFontSupport.Font.Assign(FontSupport.Font);
end;

function TBitmap32.GetCanvas: TCanvas;
begin
  Result := (Backend as ICanvasSupport).Canvas;
end;

function TBitmap32.GetBitmapInfo: TBitmapInfo;
begin
  Result := (Backend as IBitmapContextSupport).BitmapInfo;
end;

function TBitmap32.GetHandle: HBITMAP;
begin
  Result := (Backend as IBitmapContextSupport).BitmapHandle;
end;

function TBitmap32.GetHDC: HDC;
begin
  Result := (Backend as IDeviceContextSupport).Handle;
end;

class function TBitmap32.GetPlatformBackendClass: TCustomBackendClass;
begin
{$IFDEF FPC}
  Result := TLCLBackend;
{$ELSE}
  Result := TGDIBackend;
{$ENDIF}
end;

function TBitmap32.GetFont: TFont;
begin
  Result := (Backend as IFontSupport).Font;
end;

procedure TBitmap32.SetBackend(const ABackend: TCustomBackend);
var
  FontSupport: IFontSupport;
  CanvasSupport: ICanvasSupport;
begin
  if (ABackend <> nil) and (Backend <> ABackend) then
  begin
    if Supports(ABackend, IFontSupport, FontSupport) then
      FontSupport.OnFontChange := FontChanged;

    if Supports(ABackend, ICanvasSupport, CanvasSupport) then
      CanvasSupport.OnCanvasChange := CanvasChanged;

    inherited;
  end;
end;

procedure TBitmap32.SetFont(Value: TFont);
begin
  (Backend as IFontSupport).Font := Value;
end;

procedure TBitmap32.HandleChanged;
begin
  if Assigned(FOnHandleChanged) then
    FOnHandleChanged(Self);
end;

{$IFDEF BCB}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ELSE}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
{$ENDIF}
begin
  (Backend as IDeviceContextSupport).Draw(DstRect, SrcRect, hSrc);
end;

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; DstX, DstY: Integer);
begin
  if not Empty then
    (Backend as IDeviceContextSupport).DrawTo(hDst, DstX, DstY);
end;

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect);
begin
  if not Empty then
    (Backend as IDeviceContextSupport).DrawTo(hDst, DstRect, SrcRect);
end;

procedure TBitmap32.TileTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect; MaxTileSize: integer);
var
  DstW, DstH: Integer;
  TilesX, TilesY: Integer;
  Buffer: TCustomBitmap32;
  I, J: Integer;
  ClipRect, R: TRect;
  X, Y: Integer;
  DeviceContextSupport: IDeviceContextSupport;
begin
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  TilesX := (DstW + MaxTileSize - 1) div MaxTileSize;
  TilesY := (DstH + MaxTileSize - 1) div MaxTileSize;
  Buffer := TBitmap32.Create;
  try
    DeviceContextSupport := Buffer.Backend as IDeviceContextSupport;
    try
      for J := 0 to TilesY - 1 do
      begin
        for I := 0 to TilesX - 1 do
        begin
          ClipRect.Left := I * MaxTileSize;
          ClipRect.Top := J * MaxTileSize;
          ClipRect.Right := (I + 1) * MaxTileSize;
          ClipRect.Bottom := (J + 1) * MaxTileSize;
          if ClipRect.Right > DstW then
            ClipRect.Right := DstW;
          if ClipRect.Bottom > DstH then
            ClipRect.Bottom := DstH;
          X := ClipRect.Left;
          Y := ClipRect.Top;
          OffsetRect(ClipRect, -X, -Y);
          R := DstRect;
          OffsetRect(R, -X - DstRect.Left, -Y - DstRect.Top);
          Buffer.SetSize(ClipRect.Right, ClipRect.Bottom);
          StretchTransfer(Buffer, R, ClipRect, Self, SrcRect, Resampler, DrawMode, OnPixelCombine);

          DeviceContextSupport.DrawTo(hDst,
            MakeRect(X + DstRect.Left, Y + DstRect.Top, X + DstRect.Left+ClipRect.Right, Y + DstRect.Top+ClipRect.Bottom),
            Buffer.BoundsRect
          );
        end;
      end;

    finally
      DeviceContextSupport := nil;
    end;
  finally
    Buffer.Free;
  end;
end;

{$IFDEF COMPILER2009_UP}
procedure TBitmap32.DrawTo(Dst: TControlCanvas; DstX, DstY: Integer);
begin
  DrawTo(Dst.Handle, DstX, DstY);
end;

procedure TBitmap32.DrawTo(Dst: TControlCanvas; const DstRect, SrcRect: TRect);
begin
  DrawTo(Dst.Handle, DstRect, SrcRect);
end;

procedure TBitmap32.TileTo(Dst: TControlCanvas; const DstRect, SrcRect: TRect; MaxTileSize: integer);
begin
  TileTo(Dst.Handle, DstRect, SrcRect, MaxTileSize);
end;
{$ENDIF}

procedure TBitmap32.UpdateFont;
begin
  (Backend as IFontSupport).UpdateFont;
end;

// Text and Fonts //

function TBitmap32.TextExtent(const Text: string): TSize;
begin
  Result := (Backend as ITextSupport).TextExtent(Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(X, Y: Integer; const Text: string);
begin
  (Backend as ITextSupport).Textout(X, Y, Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  (Backend as ITextSupport).Textout(X, Y, ClipRect, Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  (Backend as ITextSupport).Textout(DstRect, Flags, Text);
end;

// -------------------------------------------------------------------

function TBitmap32.TextHeight(const Text: string): Integer;
begin
  Result := (Backend as ITextSupport).TextExtent(Text).cY;
end;

// -------------------------------------------------------------------

function TBitmap32.TextWidth(const Text: string): Integer;
begin
  Result := (Backend as ITextSupport).TextExtent(Text).cX;
end;

// -------------------------------------------------------------------

{$IFNDEF FPC}
procedure SetFontAntialiasing(const Font: TFont; Quality: Cardinal);
var
  LogFont: TLogFont;
begin
  LogFont := Default(TLogFont);
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0; { have font mapper choose }

    {$IFDEF COMPILER2005_UP}
    lfEscapement := Font.Orientation;
    lfOrientation := Font.Orientation;
    {$ELSE}
    lfEscapement := 0;
    lfOrientation := 0;
    {$ENDIF}

    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;

    lfItalic := Byte(fsItalic in Font.Style);
    lfUnderline := Byte(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := Byte(Font.Charset);

    if AnsiCompareText(Font.Name, 'Default') = 0 then  // do not localize
      StrLCopy(lfFaceName, @DefFontData.Name[1], LF_FACESIZE-1)
    else
      StrLCopy(lfFaceName, @Font.Name[1], LF_FACESIZE-1);

    lfQuality := Quality;

    { Only True Type fonts support the angles }
    if lfOrientation <> 0 then
      lfOutPrecision := OUT_TT_ONLY_PRECIS
    else
      lfOutPrecision := OUT_DEFAULT_PRECIS;

    lfClipPrecision := CLIP_DEFAULT_PRECIS;

    case Font.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Font.Handle := CreateFontIndirect(LogFont);
end;
{$ENDIF}

procedure TextBlueToAlpha(const B: TCustomBitmap32; Color: TColor32);
(*
asm
    PUSH    EDI
    MOV     ECX, [B+$44].Integer
    IMUL    ECX, [B+$40].Integer
    MOV     EDI, [B+$54].Integer
    @PixelLoop:
    MOV     EAX, [EDI]
    SHL     EAX, 24
    ADD     EAX, Color
    MOV     [EDI], EAX
    ADD     EDI, 4
    LOOP    @PixelLoop
    POP     EDI
end;
*)
var
  I: Integer;
  P: PColor32;
begin
  // convert blue channel to alpha and fill the color
  Color := Color and $00FFFFFF;
  P := @B.Bits[0];
  for I := 0 to B.Width * B.Height - 1 do
  begin
    if P^ <> 0 then
        P^ := ((P^ and $FF) shl 24) or Color
    else
      P^ := 0;
    Inc(P);
  end;
end;

procedure TextScaleDown(const B, B2: TCustomBitmap32; const N: Integer;
  const Color: TColor32); // use only the blue channel
var
  I, J, X, Y, P, Q, Sz, S: Integer;
  Src: PColor32;
  Dst: PColor32;
begin
  Sz := 1 shl N - 1;
  Dst := PColor32(B.ScanLine[0]);
  for J := 0 to B.Height - 1 do
  begin
    Y := J shl N;
    for I := 0 to B.Width - 1 do
    begin
      X := I shl N;
      S := 0;
      for Q := Y to Y + Sz do
      begin
        Src := B2.PixelPtr[X, Q];
        for P := X to X + Sz do
        begin
          S := S + Integer(Src^ and $000000FF);
          Inc(Src);
        end;
      end;
      S := S shr N shr N;
      Dst^ := TColor32(S shl 24) + Color;
      Inc(Dst);
    end;
  end;
end;

procedure TBitmap32.RenderText(X, Y: Integer; const Text: string; AALevel: Integer; Color: TColor32);
var
  B, B2: TBitmap32;
  Sz: TSize;
{$IFNDEF PLATFORM_INDEPENDENT}
  SzSpace: TSize;
{$ENDIF}
  Alpha: TColor32;
  StockCanvas: TCanvas;
begin
  if Empty then
    Exit;

  Alpha := Color shr 24;
  Color := Color and $00FFFFFF;
  AALevel := Constrain(AALevel, -1, 4);

  {$IFDEF FPC}
  if AALevel > -1 then
    Font.Quality := fqNonAntialiased
  else
    Font.Quality := fqAntialiased;
  {$ELSE}
  if AALevel > -1 then
    SetFontAntialiasing(Font, NONANTIALIASED_QUALITY)
  else
    SetFontAntialiasing(Font, ANTIALIASED_QUALITY);
  {$ENDIF}

  { TODO : Optimize Clipping here }
  B := TBitmap32.Create;
  try
    if AALevel <= 0 then
    begin
      Sz := Self.TextExtent(Text) + Self.TextExtent(' ');
      B.SetSize(Sz.cX, Sz.cY);
      B.Font := Font;
      B.Clear(0);
      B.Font.Color := clWhite;
      B.Textout(0, 0, Text);
      TextBlueToAlpha(B, Color);
    end
    else
    begin
      StockCanvas := StockBitmap.Canvas;
      StockCanvas.Lock;
      try
        StockCanvas.Font := Font;
        StockCanvas.Font.Size := Font.Size shl AALevel;
{$IFDEF PLATFORM_INDEPENDENT}
        Sz := StockCanvas.TextExtent(Text) + StockCanvas.TextExtent(' ');
{$ELSE}
        Windows.GetTextExtentPoint32(StockCanvas.Handle, PChar(Text), Length(Text), Sz);
        Windows.GetTextExtentPoint32(StockCanvas.Handle, PChar(string(' ')), 1, SzSpace);
        Sz := Sz + SzSpace;
{$ENDIF}
        Sz.Cx := (Sz.cx shr AALevel + 1) shl AALevel;
        Sz.Cy := (Sz.cy shr AALevel + 1) shl AALevel;
        B2 := TBitmap32.Create;
        try
          B2.SetSize(Sz.Cx, Sz.Cy);
          B2.Clear(0);
          B2.Font := StockCanvas.Font;
          B2.Font.Color := clWhite;
          B2.Textout(0, 0, Text);
          B.SetSize(Sz.cx shr AALevel, Sz.cy shr AALevel);
          TextScaleDown(B, B2, AALevel, Color);
        finally
          B2.Free;
        end;
      finally
        StockCanvas.Unlock;
      end;
    end;

    B.DrawMode := dmBlend;
    B.MasterAlpha := Alpha;
    B.CombineMode := CombineMode;

    B.DrawTo(Self, X, Y);
  finally
    B.Free;
  end;

  {$IFDEF FPC}
  Font.Quality := fqDefault;
  {$ELSE}
  SetFontAntialiasing(Font, DEFAULT_QUALITY);
  {$ENDIF}
end;

// -------------------------------------------------------------------

function TBitmap32.CanvasAllocated: Boolean;
var
  CanvasSupport: ICanvasSupport;
begin
  Result := Supports(Backend, ICanvasSupport, CanvasSupport) and CanvasSupport.CanvasAllocated;
end;

procedure TBitmap32.DeleteCanvas;
var
  CanvasSupport: ICanvasSupport;
begin
  if Supports(Backend, ICanvasSupport, CanvasSupport) then
    CanvasSupport.DeleteCanvas;
end;


{ TCustomBackend }

constructor TCustomBackend.Create;
begin
  RefCounted := True;
  _AddRef;
  inherited;
end;

constructor TCustomBackend.Create(Owner: TCustomBitmap32);
begin
  FOwner := Owner;
  Create;
  if (FOwner <> nil) then
    FOwner.Backend := Self;
end;

destructor TCustomBackend.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCustomBackend.Clear;
var
  Width, Height: Integer;
begin
  if (FOwner <> nil) then
    ChangeSize(FOwner.FWidth, FOwner.FHeight, 0, 0, False)
  else
    ChangeSize(Width, Height, 0, 0, False);
end;

procedure TCustomBackend.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{$IFDEF BITS_GETTER}
function TCustomBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;
{$ENDIF}

procedure TCustomBackend.ChangeSize(out Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  try
    Changing;

    FinalizeSurface;

    Width := 0;
    Height := 0;

    if (NewWidth > 0) and (NewHeight > 0) then
      InitializeSurface(NewWidth, NewHeight, ClearBuffer);

    Width := NewWidth;
    Height := NewHeight;
  finally
    Changed;
  end;
end;

procedure TCustomBackend.Assign(Source: TPersistent);
var
  SrcBackend: TCustomBackend;
begin
  if Source is TCustomBackend then
  begin
    if (FOwner <> nil) then
    begin
      SrcBackend := TCustomBackend(Source);

      ChangeSize(
        FOwner.FWidth, FOwner.FHeight,
        SrcBackend.FOwner.Width, SrcBackend.FOwner.Height,
        False
      );

      if not SrcBackend.Empty then
        MoveLongword(
          SrcBackend.Bits[0], Bits[0],
          SrcBackend.FOwner.Width * SrcBackend.FOwner.Height
        );
    end;
  end else
    inherited;
end;

function TCustomBackend.Empty: Boolean;
begin
  Result := False;
end;

procedure TCustomBackend.FinalizeSurface;
begin
  // descendants override this method
end;

procedure TCustomBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  // descendants override this method
end;

procedure TCustomBackend.SetOwner(AOwner: TCustomBitmap32);
begin
  FOwner := AOwner;
end;

{ TCustomSampler }

function TCustomSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFixed(X * FixedOne, Y * FixedOne);
end;

function TCustomSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

function TCustomSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Result := GetSampleFixed(Fixed(X), Fixed(Y));
end;

procedure TCustomSampler.PrepareSampling;
begin
  // descendants override this method
end;

procedure TCustomSampler.FinalizeSampling;
begin
  // descendants override this method
end;

function TCustomSampler.HasBounds: Boolean;
begin
  Result := False;
end;

function TCustomSampler.GetSampleBounds: TFloatRect;
const
  InfRect: TFloatRect = (Left: -Infinity; Top: -Infinity; Right: Infinity; Bottom: Infinity);
begin
  Result := InfRect;
end;


{ TCustomResampler }

constructor TCustomResampler.Create;
begin
  inherited;
  FPixelAccessMode := pamSafe;
end;

constructor TCustomResampler.Create(ABitmap: TCustomBitmap32);
begin
  Create;
  FBitmap := ABitmap;
  if (FBitmap <> nil) then
    FBitmap.Resampler := Self;
end;

procedure TCustomResampler.DoChanged;
begin
  if (FBitmap <> nil) then
    FBitmap.Changed;
end;

procedure TCustomResampler.AssignTo(Dst: TPersistent);
begin
  if Dst is TCustomResampler then
    SmartAssign(Self, Dst)
  else
    inherited;
end;

function TCustomResampler.GetSampleBounds: TFloatRect;
begin
  Result := FloatRect(FBitmap.ClipRect);
  if PixelAccessMode = pamTransparentEdge then
    InflateRect(Result, 1, 1);
end;

function TCustomResampler.GetWidth: TFloat;
begin
  Result := 0;
end;

function TCustomResampler.HasBounds: Boolean;
begin
  Result := FPixelAccessMode <> pamWrap;
end;

procedure TCustomResampler.PrepareSampling;
begin
  FClipRect := FBitmap.ClipRect;
end;

procedure TCustomResampler.SetPixelAccessMode(
  const Value: TPixelAccessMode);
begin
  if FPixelAccessMode <> Value then
  begin
    FPixelAccessMode := Value;
    Changed;
  end;
end;

initialization
  SetGamma;
  StockBitmap := TBitmap.Create;
  StockBitmap.Width := 8;
  StockBitmap.Height := 8;

finalization
  StockBitmap.Free;

end.
