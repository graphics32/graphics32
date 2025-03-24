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
//      TGlyphMetrics32
//
//------------------------------------------------------------------------------
// Glyph metrics - of a single glyph
//------------------------------------------------------------------------------
//
//
//                 |<------ Width ---->|       |<----- Width ---->|
//                 .                   .       .                  .
//                 .                   .       .                  .
//                 .  ffffffffffffffff .       .                  .
//                 . f::::::::::::::::f        .                  .
//                  f::::::::::::::::::f       .                  .
//                  f::::::fffffff:::::f       .                  .
//                  f:::::f       ffffff       .  ggggggggg   ggggg.....-
//                  f:::::f            .       . g:::::::::ggg::::g     ^
//                 f:::::::ffffff      .        g:::::::::::::::::g     |
//                 f::::::::::::f      .       g::::::ggggg::::::gg     |
//                 f::::::::::::f      .       g:::::g     g:::::g      |
//                 f:::::::ffffff      .       g:::::g     g:::::g      |
//                  f:::::f            .       g:::::g     g:::::g      |
//                  f:::::f            .       g::::::g    g:::::g      |
//                 f:::::::f           .       g:::::::ggggg:::::g      |
//                 f:::::::f           .        g::::::::::::::::g    Height
//                 f:::::::f           .         gg::::::::::::::g      |
//    Origin-->*   fffffffff           .   *       gggggggg::::::g      |
//             .   .                   .   .               g:::::g      |
//             .   .                   .   .   gggggg      g:::::g      |
//             |<->|                   |<->|   g:::::gg   gg:::::g      |
//             . |                       | .    g::::::ggg:::::::g      |
//             . OffsetX/LSB           RSB .     gg:::::::::::::g       |
//             .                           .       ggg::::::ggg         v
//             |<------- AdvanceX -------->|          gggggg ...........-
//
//
//
//
//
//------------------------------------------------------------------------------
type
  TGlyphMetrics32 = record
  private
    function GetLeftSideBearing: Single; {$ifdef UseInlining}inline;{$endif}
    function GetRightSideBearing: Single; {$ifdef UseInlining}inline;{$endif}

  public
    Valid: boolean;
    OffsetX: Single;            // X-offset from origin to visible start of glyph
    OffsetY: Single;            // Y-offset from origin to visible start of glyph
    Width: Single;              // Visible width of glyph
    Height: Single;             // Visible height of glyph
    AdvanceX: Single;           // X-offset from origin to origin of next glyph
    AdvanceY: Single;           // Y-offset from origin to origin of next glyph

  public

    // Left Side Bearing: Distance from origin to visible start of glyph. Same as OffsetX
    property LeftSideBearing: Single read GetLeftSideBearing;

    // Right Side Bearing: Distance from visible end of glyph to origin+AdvanceX
    property RightSideBearing: Single read GetRightSideBearing;

  end;


//------------------------------------------------------------------------------
//
//      TFontFaceMetrics32
//
//------------------------------------------------------------------------------
// Font face metrics
//------------------------------------------------------------------------------
//
//
//           ..-........................................................-....
//             ^                                                        ^
//             |      ffffffffffffffff                                  |
//             |     f::::::::::::::::f                                 |
//             |    f::::::::::::::::::f                                |
//             |    f::::::fffffff:::::f                                |
//             |    f:::::f       ffffff          ggggggggg   ggggg     |
//             |    f:::::f                      g:::::::::ggg::::g     |
//    Ascent---+   f:::::::ffffff               g:::::::::::::::::g     |
//             |   f::::::::::::f              g::::::ggggg::::::gg     |
//             |   f::::::::::::f              g:::::g     g:::::g      |
//             |   f:::::::ffffff              g:::::g     g:::::g      |
//             |    f:::::f                    g:::::g     g:::::g      |
//             |    f:::::f                    g::::::g    g:::::g      |
//             |   f:::::::f                   g:::::::ggggg:::::g      |
//             |   f:::::::f                    g::::::::::::::::g    Height
//             |   f:::::::f                     gg::::::::::::::g      |
//  Baseline-->*.. fffffffff ..................... gggggggg::::::g .....|....
//             |                                           g:::::g      |
//             |                               gggggg      g:::::g      |
//   Descent---+                               g:::::gg   gg:::::g      |
//  (negative) |                                g::::::ggg:::::::g      |
//             |                                 gg:::::::::::::g       |
//             v                                   ggg::::::ggg         v
//           ..-..................................... gggggg ...........-....
//             |
//  Line gap---+
//             |
//             v
//           ..-.............................................................
//
//
//------------------------------------------------------------------------------
type
  TFontFaceMetrics32 = record
  private
    function GetHeight: Single; {$ifdef UseInlining}inline;{$endif}

  public
    Valid: boolean;
    Ascent: Single;
    Descent: Single;
    LineGap: Single;
    EMSize: integer;
    EMSpaceWidth: Single;

  public
    property Height: Single read GetHeight;
  end;


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
// The plaform layer must provide a concrete implementation.
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
// The plaform layer must provide a concrete implementation.
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
//
//      TGlyphMetrics32
//
//------------------------------------------------------------------------------
function TGlyphMetrics32.GetLeftSideBearing: Single;
begin
  Result := OffsetX;
end;

function TGlyphMetrics32.GetRightSideBearing: Single;
begin
  Result := AdvanceX - OffsetX - Width;
end;

//------------------------------------------------------------------------------
//
//      TFontFaceMetrics32
//
//------------------------------------------------------------------------------
function TFontFaceMetrics32.GetHeight: Single;
begin
  Result := Ascent - Descent;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
