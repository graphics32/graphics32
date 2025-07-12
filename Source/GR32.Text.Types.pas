unit GR32.Text.Types;

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
 * Portions created by the Initial Developer are Copyright (C) 2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32;

//------------------------------------------------------------------------------
//
//      Text layout bit flags
//
//------------------------------------------------------------------------------
// Used by
// - ITextSupport.Textout
// - ITextToPathSupport.TextToPath
// - TBitmap32.TextToPath
// - TCanvas32.RenderText
//------------------------------------------------------------------------------
// If possible, use TTextLayout instead
//------------------------------------------------------------------------------
const
  // See also Window's DrawText() flags ...
  // http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_LEFT               = $0000;
  DT_CENTER             = $0001;
  DT_RIGHT              = $0002;
  DT_TOP                = $0000;
  DT_VCENTER            = $0004;
  DT_BOTTOM             = $0008;
  DT_WORDBREAK          = $0010;
  DT_SINGLELINE         = $0020;
  DT_NOCLIP             = $0100;

  {$NODEFINE DT_LEFT}
  {$NODEFINE DT_CENTER}
  {$NODEFINE DT_RIGHT}
  {$NODEFINE DT_TOP}
  {$NODEFINE DT_VCENTER}
  {$NODEFINE DT_BOTTOM}
  {$NODEFINE DT_WORDBREAK}
  {$NODEFINE DT_SINGLELINE}
  {$NODEFINE DT_NOCLIP}

  // Graphics32 additions ...
  DT_JUSTIFY            = $0003;
  DT_HORZ_ALIGN_MASK    = $0003;
  DT_VERT_ALIGN_MASK    = $000C;


//------------------------------------------------------------------------------
//
//      Text alignment flags
//
//------------------------------------------------------------------------------
type
  // Horizontal alignment
  TTextAlignmentHorizontal = (
    TextAlignHorLeft,           // Align left

    TextAlignHorCenter,         // Align center

    TextAlignHorRight,          // Align right

    TextAlignHorJustify         // Justify
  );

  // Vertical alignment
  TTextAlignmentVertical = (
    TextAlignVerTop,            // Align top

    TextAlignVerCenter,         // Align center

    TextAlignVerBottom          // Align bottom
  );

  // Horizontal alignment of last line.
  TTextAlignmentHorizontalLast = (
    TextAlignHorLastStart,      // Align LTR script left, RTL script right
                                // Note that start alignment relies on LTR/RTL
                                // support which hasn't yet been implemented.
                                // For now we only support LTR.

    TextAlignHorLastCenter,     // Align center

    TextAlignHorLastJustify     // Justify
  );


//------------------------------------------------------------------------------
//
//      Typeface vertical metrics
//
//------------------------------------------------------------------------------
// The Typeface vertical metrics are primarily used to determine the baseline,
// line height, and vertical alignment offsets when positioning text vertically.
//------------------------------------------------------------------------------
type
  TTextLayoutVerticalMetrics = (
    vmTypographic,      // Use the typographic metrics specified in the font.
                        // On Windows the metric values would come from
                        // the otmAscent, otmDescent, and otmLineGap fields of
                        // the OUTLINETEXTMETRIC structure.
                        // Typographic metrics should normally be used unless
                        // ExtTextOut compatibility is required.

    vmWindows           // Use classic Windows verical metrics.
                        // These are the metrics used by GDI ExtTextOut and
                        // friends. On Windows the metric values would come from
                        // the tmAscent, tmDescent, and tmHeight fields of the
                        // TEXTMETRIC structure.
  );

//------------------------------------------------------------------------------
//
//      Text layout settings
//
//------------------------------------------------------------------------------
// Used by
// - ITextToPathSupport2.TextToPath
// - TCanvas32.RenderText
//------------------------------------------------------------------------------
type
  TTextLayout = record
    (*
    ** Vertical typeface metrics
    *)
    VerticalMetrics: TTextLayoutVerticalMetrics;

    (*
    ** Vertical line spacing
    *)
    InterLineFactor: Single;
    InterParagraphFactor: Single;

    (*
    ** Alignment
    *)
    AlignmentHorizontal: TTextAlignmentHorizontal;
    AlignmentVertical: TTextAlignmentVertical;

    (*
    ** AlignmentHorizontal = TextAlignHorJustify
    *)
    // Horizontal alignment of last line
    AlignmentHorizontalLastLine: TTextAlignmentHorizontalLast;

    MinInterWordSpaceFactor: Single;    // Not implemented
    MaxInterWordSpaceFactor: Single;
    MaxInterCharSpaceFactor: Single;    // Space factor relative to em-space

    (*
    ** Misc. flags
    *)
    Kerning: boolean;
    WordWrap: boolean;
    SingleLine: boolean;

    LineBreakIsParagraph: boolean;      // Interpret CR or LF characters as paragraph breaks.
                                        // Explicit  Unicode linebreak are still interpreted as line breaks.

    DoubleLineBreakIsParagraph: boolean;// Interpret two consecutive CR or LF characters as paragraph breaks.
                                        // Ignore if LineBreakIsParagraph=True

    RemoveLeadingSpace: boolean;        // Remove leading whitespace after line/paragraph breaks.
                                        // The first line is considered to after a line break.

    (*
    ** Clipping
    *)
    ClipLayout: boolean;                // Clip lines/glyphs during layout
    ClipRaster: boolean;                // Clip during path rasterization

    // Not implemented
    // CharacterTracking: Single;
    // SpaceTracking: Single;
  end;

var
  DefaultTextLayout: TTextLayout = (
    (*
    ** Vertical typeface metrics
    *)
    VerticalMetrics:            vmTypographic;

    (*
    ** Vertical line spacing
    *)
    InterLineFactor:            1.0;
    InterParagraphFactor:       1.5;

    (*
    ** Alignment
    *)
    AlignmentHorizontal:        TextAlignHorLeft;
    AlignmentVertical:          TextAlignVerTop;

    (*
    ** Horizontal justification
    *)
    AlignmentHorizontalLastLine: TextAlignHorLastStart;

    MinInterWordSpaceFactor:    0.8;
    MaxInterWordSpaceFactor:    2.0;
    MaxInterCharSpaceFactor:    0.2;


    (*
    ** Misc
    *)
    Kerning:                    False;
    WordWrap:                   False;
    SingleLine:                 True;
    LineBreakIsParagraph:       False;
    DoubleLineBreakIsParagraph: False;
    RemoveLeadingSpace:         False;

    (*
    ** Clipping
    *)
    ClipLayout:                 True;
    ClipRaster:                 False;
  );


//------------------------------------------------------------------------------
//
//      TextFlagsToLayout and LayoutToTextFlags
//
//------------------------------------------------------------------------------
// Converts between Windows style DrawText bit flags and TTextLayout.
// TextFlagsToLayout Only modifies the values specified by the flags. The rest
// are left as-is.
//------------------------------------------------------------------------------
procedure TextFlagsToLayout(AFlags: Cardinal; var ALayout: TTextLayout);
function LayoutToTextFlags(const ALayout: TTextLayout): Cardinal;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//
//      TextFlagsToLayout
//
//------------------------------------------------------------------------------
procedure TextFlagsToLayout(AFlags: Cardinal; var ALayout: TTextLayout);
begin
  case (AFlags and DT_HORZ_ALIGN_MASK) of
    DT_LEFT:
      ALayout.AlignmentHorizontal := TextAlignHorLeft;

    DT_CENTER:
      ALayout.AlignmentHorizontal := TextAlignHorCenter;

    DT_RIGHT:
      ALayout.AlignmentHorizontal := TextAlignHorRight;

    DT_JUSTIFY:
      ALayout.AlignmentHorizontal := TextAlignHorJustify;
  end;

  case (AFlags and DT_VERT_ALIGN_MASK) of
    DT_TOP:
      ALayout.AlignmentVertical := TextAlignVerTop;

    DT_VCENTER:
      ALayout.AlignmentVertical := TextAlignVerCenter;

    DT_BOTTOM:
      ALayout.AlignmentVertical := TextAlignVerBottom;
  else
    ALayout.AlignmentVertical := TextAlignVerTop;
  end;

  ALayout.WordWrap := (AFlags and DT_WORDBREAK <> 0);
  ALayout.SingleLine := (AFlags and DT_SINGLELINE <> 0);
end;

//------------------------------------------------------------------------------
//
//      TextFlagsToLayout
//
//------------------------------------------------------------------------------
function LayoutToTextFlags(const ALayout: TTextLayout): Cardinal;
const
  BitsAlignmentHorizontal: array[TTextAlignmentHorizontal] of Cardinal = (DT_LEFT, DT_CENTER, DT_RIGHT, DT_JUSTIFY);
  BitsAlignmentVertical: array[TTextAlignmentVertical] of Cardinal = (DT_TOP, DT_VCENTER, DT_BOTTOM);
begin
  Result := 0;

  Result := Result or BitsAlignmentHorizontal[ALayout.AlignmentHorizontal];
  Result := Result or BitsAlignmentVertical[ALayout.AlignmentVertical];

  if (ALayout.WordWrap) then
    Result := Result or DT_WORDBREAK;

  if (ALayout.SingleLine) then
    Result := Result or DT_SINGLELINE;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
