unit GR32.Text.FontFace;

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
 * The Original Code is Text Layout Engine for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Generics.Defaults,
  Graphics,
  GR32_Paths,
  GR32.Text.Types;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
//      Platform independent font API
//
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//
//      TFontKey
//
//------------------------------------------------------------------------------
// A font key is used to uniquely identify a font face, sans size, for example
// as a dictionary key.
//------------------------------------------------------------------------------
type
  TFontKey = TArray<byte>;


//------------------------------------------------------------------------------
//
//      IFontFace32
//
//------------------------------------------------------------------------------
// Wraps a font face.
// The platform layer must provide a concrete implementation.
//------------------------------------------------------------------------------
type
  IFontFace32 = interface
    procedure BeginSession;
    procedure EndSession;

    function GetFontFaceMetrics(const ATextLayout: TTextLayout; var AFontFaceMetrics: TFontFaceMetrics32): boolean;
    function GetGlyphMetrics(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;
    function GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; OffsetX: Single = 0; OffsetY: Single = 0): boolean;
    function GetKerning(AFirstGlyph, ASecondGlyph: Cardinal): Single;
  end;


//------------------------------------------------------------------------------
//
//      IFontFaceProvider32
//
//------------------------------------------------------------------------------
// A font face provider.
// The platform layer must provide a concrete implementation.
//------------------------------------------------------------------------------
type
  IFontFaceProvider32 = interface
    ['{62B072F5-7FCC-41AA-A04E-ED82B90C1F75}']
    function CreateFontFace(AFont: TFont): IFontFace32;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
