unit GR32.Text.Layout;

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

// Define DEBUG_TEXT_CHAR to store characters as a "Char" in addition to the Unicode
// codepoint. This makes it easier to see what a glyphstring contains during debug.
{-$define DEBUG_TEXT_CHAR}

uses
  Character,
  GR32,
  GR32_Paths,
  GR32.Text.Types,
  GR32.Text.FontFace;

//------------------------------------------------------------------------------
//
//      TTextBreakCategory
//
//------------------------------------------------------------------------------
// Specifies the breakability of a character.
// Used by the word-break/word-wrap algorithm in BreakParagraph.
//------------------------------------------------------------------------------
type
  TTextBreakCategory = (
    TextBreakUnknown,           // The breakability hasn't been determined yet
    TextBreakNoBreak,           // The line can not be broken before the character
    TextBreakBreakable,         // The line can be broken before the character
    TextBreakLine,              // Line break; The line must be broken before the character
    TextBreakParagraph          // Paragraph break; The line must be broken before the character
  );


//------------------------------------------------------------------------------
//
//      TTextParagraph
//
//------------------------------------------------------------------------------
// Represents a single paragraph.
//------------------------------------------------------------------------------
type
  TTextParagraph = record
  private
    function GetLastIndex: integer; {$ifdef UseInlining}inline;{$endif}

  public
    ParagraphIndex: integer;    // Index in paragraph list before word wrap
    StartIndex: integer;        // Index in text string of first character in paragraph/line
    Count: integer;             // Number of characters in paragraph/line
    Width: Single;              // Width, in pixels, of line
    IsParagraph: boolean;       // Specifies if line terminates a paragraph

    property LastIndex: integer read GetLastIndex;
  end;

  PTextParagraph = ^TTextParagraph;

  TTextParagraphs = TArray<TTextParagraph>;


//------------------------------------------------------------------------------
//
//      TTextCharacter
//
//------------------------------------------------------------------------------
// Represents a single UCS4 character.
//------------------------------------------------------------------------------
type
  TTextCharacter = record
{$ifdef DEBUG_TEXT_CHAR}
    Char: Char;
{$endif DEBUG_TEXT_CHAR}
    CodePoint: Cardinal;
    UnicodeCategory: TUnicodeCategory;
    BreakCategory: TTextBreakCategory;
    Metrics: TGlyphMetrics32;
  end;

  TTextCharacterString = TArray<TTextCharacter>;


//------------------------------------------------------------------------------
//
//      LayoutEngine
//
//------------------------------------------------------------------------------
// The text layout engine
//------------------------------------------------------------------------------
type
  LayoutEngine = record

    //------------------------------------------------------------------------------
    //
    //      TextToParagraphs
    //
    //------------------------------------------------------------------------------
    // Convert a string to a list of paragraphs containing codepoint strings.
    //------------------------------------------------------------------------------
    class function TextToParagraphs(const AText: string; var AParagraphs: TTextParagraphs; const TextLayout: TTextLayout): TTextCharacterString; static;


    //------------------------------------------------------------------------------
    //
    //      GetGlyphMetrics
    //
    //------------------------------------------------------------------------------
    // Populate a codepoint string with character widths.
    //------------------------------------------------------------------------------
    class procedure GetGlyphMetrics(var AText: TTextCharacterString; const AFontFace: IFontFace32); static;


    //------------------------------------------------------------------------------
    //
    //      BreakParagraph
    //
    //------------------------------------------------------------------------------
    // Applies line breaking to a single paragraph so no line exceeds the specified
    // max line width.
    // The lines of the broken paragraph is appended to the paragraph list.
    // LineCount specifies the number of entries in the list both before and after.
    //------------------------------------------------------------------------------
    class procedure BreakParagraph(const AText: TTextCharacterString; const AParagraph: TTextParagraph; AMaxWidth: Single;
      var AParagraphs: TTextParagraphs; var ALineCount: integer; const ATextLayout: TTextLayout); static;

    //------------------------------------------------------------------------------
    //
    //      BreakParagraphs
    //
    //------------------------------------------------------------------------------
    // Apply line breaking to a list of paragraphs so no line exceeds the specified
    // max line width.
    //------------------------------------------------------------------------------
    class function BreakParagraphs(const AText: TTextCharacterString; const AParagraphs: TTextParagraphs; AMaxWidth: Single; const ATextLayout: TTextLayout): TTextParagraphs; static;


    //------------------------------------------------------------------------------
    //
    //      ApplyKerning
    //
    //------------------------------------------------------------------------------
    // Applies kerning to a codepoint string.
    //------------------------------------------------------------------------------
    class procedure ApplyKerning(const FontFace: IFontFace32; var TextCharacterString: TTextCharacterString); static;


    //------------------------------------------------------------------------------
    //
    //      TextToPath
    //
    //------------------------------------------------------------------------------
    // Layout text and return as a polygon path.
    //------------------------------------------------------------------------------
    class procedure TextToPath(const AFontFace: IFontFace32; APath: TCustomPath; var ARect: TFloatRect; const AText: string; ATextLayout: TTextLayout); static;

  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$ifdef UseInlining}
  Types,
{$endif}
  Math,
  GR32.Text.Unicode;


//------------------------------------------------------------------------------
// FPC compatibility
//------------------------------------------------------------------------------
{$ifndef FPC}

function DoGetUnicodeCategory(ACodePoint: Cardinal) : TUnicodeCategory; overload; {$ifdef UseInlining}inline;{$endif}
begin
  // TODO : Verify that this calls the UCS4Char overload
  Result := Char(ACodePoint).GetUnicodeCategory;
end;

function DoGetUnicodeCategory(AChar: char) : TUnicodeCategory; overload; {$ifdef UseInlining}inline;{$endif}
begin
  Result := AChar.GetUnicodeCategory;
end;

{$else}

function DoGetUnicodeCategory(ACodePoint: Cardinal) : TUnicodeCategory; overload; {$ifdef UseInlining}inline;{$endif}
begin
  Result := TCharacter.GetUnicodeCategory(UnicodeChar(ACodePoint));
end;

function DoGetUnicodeCategory(AChar: UnicodeChar) : TUnicodeCategory; overload; {$ifdef UseInlining}inline;{$endif}
begin
  Result := TCharacter.GetUnicodeCategory(AChar);
end;

{$endif}


//------------------------------------------------------------------------------
//
//      TextToParagraphs
//
//------------------------------------------------------------------------------
class function LayoutEngine.TextToParagraphs(const AText: string; var AParagraphs: TTextParagraphs; const TextLayout: TTextLayout): TTextCharacterString;

  procedure TextToSingleLine;
  var
    TextIndex: integer;
    Count: integer;
    CodePoint: TCodePoint;
    PrevCodePoint: TCodePoint;
  begin
    Count := 0;
    TextIndex := 1;
    CodePoint := 0;


    while (TextIndex <= Length(AText)) do
    begin

      // PrevCodePoint is used to handle mixed CR, LF, and CR/LF
      PrevCodePoint := CodePoint;

      // UTF16 to UCS4 conversion
      CodePoint := Graphics32Unicode.UTF16ToUTF32(AText, TextIndex);
      Result[Count].UnicodeCategory := DoGetUnicodeCategory(CodePoint);

{$ifdef DEBUG_TEXT_CHAR}
      Result[Count].Char := Char(CodePoint);
{$endif DEBUG_TEXT_CHAR}

      case CodePoint of

        $000A: // End of Line
          if (PrevCodePoint <> $000D) then
            CodePoint := 32
          else
            continue;

        $000D: // Carriage Return
          if (PrevCodePoint <> $000A) then
            CodePoint := 32
          else
            continue;

      else

        case Result[Count].UnicodeCategory of

          TUnicodeCategory.ucControl:
            // Ignore control characters
            continue;

          TUnicodeCategory.ucLineSeparator,
          TUnicodeCategory.ucParagraphSeparator:
            CodePoint := 32

        end;

      end;

      Result[Count].CodePoint := CodePoint;
      Result[Count].Metrics.Valid := False;
      Result[Count].BreakCategory := TextBreakNoBreak;

      Inc(Count);
      Inc(AParagraphs[0].Count);

    end;

    // Trim array
    SetLength(Result, Count);

  end;

  procedure TextToMultiLine;
  var
    TextIndex: integer;
    Count: integer;
    ParagraphIndex: integer;
    ParagraphCount: integer;
    CodePoint: TCodePoint;
    PrevCodePoint: TCodePoint;
    TrimWhitespace: boolean;
    SquashedLast: boolean;
  const
    // Map line break according to LineBreakIsParagraph setting
    LineBreakCategory: array[boolean] of TTextBreakCategory =
      (TextBreakLine, TextBreakParagraph);
  begin

    // Preallocate storage
    SetLength(Result, Length(AText));
    SetLength(AParagraphs, 1);

    AParagraphs[0].ParagraphIndex := 0;
    AParagraphs[0].StartIndex := 0;
    AParagraphs[0].Count := 0;
    AParagraphs[0].IsParagraph := True;

    (*
    ** Short-circuit for single-line; No need to handle breaking
    *)
    if (TextLayout.SingleLine) then
    begin
      TextToSingleLine;
      exit;
    end;


    (*
    ** Multi-line
    *)
    Count := 0;
    TextIndex := 1;
    CodePoint := 0;

    ParaGraphIndex := 0;
    ParagraphCount := 0;
    TrimWhitespace := TextLayout.RemoveLeadingSpace;
    SquashedLast := False;

    while (TextIndex <= Length(AText)) do
    begin

      // PrevCodePoint is used to handle mixed CR, LF, and CR/LF
      PrevCodePoint := CodePoint;

      // UTF16 to UCS4 conversion
      CodePoint := Graphics32Unicode.UTF16ToUTF32(AText, TextIndex);
      Result[Count].UnicodeCategory := DoGetUnicodeCategory(CodePoint);

{$ifdef DEBUG_TEXT_CHAR}
      Result[Count].Char := Char(CodePoint);
{$endif DEBUG_TEXT_CHAR}

      // Determine break category
      case CodePoint of

        $000A: // End of Line
            if (PrevCodePoint = $000D) and (not SquashedLast) then
            begin
              // Treat CR/FL as CR
              CodePoint := $000A;
              SquashedLast := True;
              continue;
            end else
            if (TextLayout.DoubleLineBreakIsParagraph) and (Result[Count].BreakCategory = TextBreakLine) and
              ((PrevCodePoint = $000A) or (PrevCodePoint = $000D)) then
            begin
              // Change break category from Line to Paragraph
              Result[Count].BreakCategory := TextBreakParagraph;
              AParagraphs[ParagraphIndex-1].IsParagraph := True;
              SquashedLast := False;
              continue;
            end else
              Result[Count].BreakCategory := LineBreakCategory[TextLayout.LineBreakIsParagraph];

        $000D: // Carriage Return
            if (PrevCodePoint = $000A) and (not SquashedLast) then
            begin
              // Treat LF/CR as LF
              CodePoint := $000D;
              SquashedLast := True;
              continue;
            end else
            if (TextLayout.DoubleLineBreakIsParagraph) and (Result[Count].BreakCategory = TextBreakLine) and
              ((PrevCodePoint = $000A) or (PrevCodePoint = $000D)) then
            begin
              // Change break category from Line to Paragraph
              Result[Count].BreakCategory := TextBreakParagraph;
              AParagraphs[ParagraphIndex-1].IsParagraph := True;
              SquashedLast := False;
              continue;
            end else
              Result[Count].BreakCategory := LineBreakCategory[TextLayout.LineBreakIsParagraph];

        $00A0, // No-break space
        $2007, // Figure space
        $202F, // Narrow no-break space
        $2060, // Word joiner (zero width no-break space)
        $FEFF: // Zero width no-break space
          Result[Count].BreakCategory := TextBreakNoBreak;

      else

        case Result[Count].UnicodeCategory of

          TUnicodeCategory.ucControl:
            // Ignore control characters
            continue;

          TUnicodeCategory.ucLineSeparator:
            Result[Count].BreakCategory := TextBreakLine;

          TUnicodeCategory.ucParagraphSeparator:
            Result[Count].BreakCategory := TextBreakParagraph;

          TUnicodeCategory.ucSpaceSeparator:
            // The "Space Separator" category contains both breakable and non-breakable
            // spaces but we handle the non-breakable above.
            Result[Count].BreakCategory := TextBreakBreakable;

        else
          Result[Count].BreakCategory := TextBreakNoBreak;
        end;

      end;

      SquashedLast := False;

      Result[Count].CodePoint := CodePoint;
      Result[Count].Metrics.Valid := False;

      case Result[Count].BreakCategory of
        TextBreakNoBreak:
          begin
            TrimWhitespace := False;
          end;

        TextBreakBreakable:
          begin
            if (TrimWhitespace) then
              // Ignore char
              continue;
          end;

        TextBreakLine,
        TextBreakParagraph:
          begin
            // Adjust type of current paragraph
            AParagraphs[ParagraphIndex].IsParagraph := (Result[Count].BreakCategory = TextBreakParagraph);

            // Start new paragraph
            Inc(ParagraphIndex);

            if (ParagraphIndex >= ParagraphCount) then
            begin
              Inc(ParagraphCount);
              if (ParagraphCount > Length(AParagraphs)) then
                SetLength(AParagraphs, ParagraphCount * 2);
            end;

            AParagraphs[ParagraphIndex].ParagraphIndex := ParagraphIndex;
            AParagraphs[ParagraphIndex].StartIndex := Count; // Index in codepoints, not in source string!
            AParagraphs[ParagraphIndex].Count := 0;
            AParagraphs[ParagraphIndex].IsParagraph := True;

            // Trim whitespace after break
            TrimWhitespace := TextLayout.RemoveLeadingSpace;

            // Do not store break char
            continue;
          end;
      end;

      if (ParagraphIndex >= ParagraphCount) then
      begin
        Inc(ParagraphCount);
        if (ParagraphCount > Length(AParagraphs)) then
          SetLength(AParagraphs, ParagraphCount * 2);
      end;

      Inc(Count);
      Inc(AParagraphs[ParagraphIndex].Count);

    end;

    // Trim arrays
    SetLength(Result, Count);
    SetLength(AParagraphs, ParagraphCount);
  end;

begin

  // Preallocate storage
  SetLength(Result, Length(AText));
  SetLength(AParagraphs, 1);

  AParagraphs[0].ParagraphIndex := 0;
  AParagraphs[0].StartIndex := 0;
  AParagraphs[0].Count := 0;
  AParagraphs[0].IsParagraph := True;

  (*
  ** Short-circuit single-line; No need to handle breaking.
  ** Single-line path is much faster than the multi-line path.
  *)
  if (TextLayout.SingleLine) then
    TextToSingleLine
  else
    TextToMultiLine;
end;


//------------------------------------------------------------------------------
//
//      GetGlyphMetrics
//
//------------------------------------------------------------------------------
class procedure LayoutEngine.GetGlyphMetrics(var AText: TTextCharacterString; const AFontFace: IFontFace32);
var
  i: integer;
begin
  for i := 0 to High(AText) do
    if (not AFontFace.GetGlyphMetrics(AText[i].CodePoint, AText[i].Metrics)) then
      AText[i].Metrics := Default(TGlyphMetrics32);
end;


//------------------------------------------------------------------------------
//
//      BreakParagraphs
//
//------------------------------------------------------------------------------
class procedure LayoutEngine.BreakParagraph(const AText: TTextCharacterString; const AParagraph: TTextParagraph; AMaxWidth: Single;
  var AParagraphs: TTextParagraphs; var ALineCount: integer; const ATextLayout: TTextLayout);
var
  Index: integer;
  Count: integer;
  Width: Single;
  Paragraph: PTextParagraph;
  BacktrackedCount: integer;
  n: integer;
begin
  Assert(ALineCount <= Length(AParagraphs));

  if (ALineCount > High(AParagraphs)) then
  begin
    if (ALineCount = 0) then
      // Single line is the most common; Optimize for that.
      SetLength(AParagraphs, 1)
    else
      SetLength(AParagraphs, Length(AParagraphs) * 2);
  end;

  // Current index in paragraph
  Index := AParagraph.StartIndex;

  // Remaining characters in paragraph
  Count := AParagraph.Count;

  Paragraph := @AParagraphs[ALineCount];

  Paragraph.ParagraphIndex := AParagraph.ParagraphIndex;
  Paragraph.StartIndex := AParagraph.StartIndex;
  Paragraph.Count := 0;
  Paragraph.IsParagraph := False;
  Width := 0;

  while (Count > 0) do
  begin

    Assert(AText[Index].Metrics.Valid);

    (* This was an attempt at implementing MinInterWordSpaceFactor;
       The idea was that if we start with all the spaces tracked tighter then
       we have a wider range of adjustment since we can only make the spaces
       tracking loser when justifying.
       However, since tracking tighter can affect where the line is broken,
       and we do not rewrap when we adjust the line, this will not work.

    if (ATextLayout.AlignmentHorizontal = TextAlignHorJustify) and (AText[Index].BreakCategory = TextBreakBreakable) then
      AdvanceX := AdvanceX * ATextLayout.MinInterWordSpaceFactor;
    *)

    // Note: We theoretically handle negative AdvanceWidthX which shouldn't really
    // occur since none of the glyphs should be unresolved combiners.

    Width := Width + AText[Index].Metrics.AdvanceX;

    // Skip left-side-bearing of first character on line
    if (Paragraph.Count = 0) then
      Width := Width - AText[Index].Metrics.LeftSideBearing;

    // First char is mandatory. Otherwise, if the first character exceeds the limit, we will end
    // up in an endless loop trying to break the same character again and again.
    if (Paragraph.Count > 0) and (Width - AText[Index].Metrics.RightSideBearing > AMaxWidth) then
    begin
      // Width exceded; Step backward until we find a break opportunity.
      // If there is none, before we reach the start of the line, just break
      // unconditionally at the current position.
      BacktrackedCount := 0;
      n := 1;
      while (n < Paragraph.Count) do
      begin
        if (AText[Index-n].BreakCategory = TextBreakBreakable) then
        begin
          // Line can be broken here; Terminate line and start a new one.
          BacktrackedCount := n;
          break;
        end;

        Inc(n);
      end;

      Assert((BacktrackedCount = 0) or (BacktrackedCount < Paragraph.Count));

      // Give back what we backtracked
      Dec(Paragraph.Count, BacktrackedCount);
      Dec(Index, BacktrackedCount);
      Inc(Count, BacktrackedCount);

      // Trim trailing whitespace (i.e. the inter-word spacing we just broke on)
      n := Paragraph.StartIndex + Paragraph.Count - 1;
      while (Paragraph.Count > 0) and (AText[n].UnicodeCategory = TUnicodeCategory.ucSpaceSeparator) do
      begin
        Dec(Paragraph.Count);
        Dec(n);
      end;

      // Trim leading whitespace on next line
      while (Count > 0) and (AText[Index].UnicodeCategory = TUnicodeCategory.ucSpaceSeparator) do
      begin
        Inc(Index);
        Dec(Count);
      end;

      // Move on to next line
      Inc(ALineCount);
      if (ALineCount > High(AParagraphs)) then
        SetLength(AParagraphs, Length(AParagraphs) * 2);
      Paragraph := @AParagraphs[ALineCount];
      Paragraph.ParagraphIndex := AParagraph.ParagraphIndex;
      Paragraph.StartIndex := Index;
      Paragraph.Count := 0;
      Width := 0;

    end else
    begin
      Inc(Paragraph.Count);

      Inc(Index);
      Dec(Count);
    end;

  end;

  // Mark the last line with the paragraph type of the source paragraph
  AParagraphs[ALineCount].IsParagraph := AParagraph.IsParagraph;

  Inc(ALineCount);

end;


//------------------------------------------------------------------------------
//
//      BreakParagraphs
//
//------------------------------------------------------------------------------
class function LayoutEngine.BreakParagraphs(const AText: TTextCharacterString; const AParagraphs: TTextParagraphs; AMaxWidth: Single; const ATextLayout: TTextLayout): TTextParagraphs;
var
  LineCount: integer;
  Paragraph: integer;
begin
  // Preallocate to reduce reallocation
  SetLength(Result, Length(AParagraphs));

  // Actual number of elements in result
  LineCount := 0;

  for Paragraph := 0 to High(AParagraphs) do
    BreakParagraph(AText, AParagraphs[Paragraph], AMaxWidth, Result, LineCount, ATextLayout);

  SetLength(Result, LineCount);
end;


//------------------------------------------------------------------------------
//
//      TextToPath
//
//------------------------------------------------------------------------------
class procedure LayoutEngine.TextToPath(const AFontFace: IFontFace32; APath: TCustomPath; var ARect: TFloatRect; const AText: string; ATextLayout: TTextLayout);
var
  FontFaceMetrics: TFontFaceMetrics32;
  TextPath: TFlattenedPath;
  Paragraphs: TTextParagraphs;
  Lines: TTextParagraphs;
  TextCharacterString: TTextCharacterString;
  i: Integer;
  Skip: boolean;
  X, Y, XMax, Height: Single;
  ClipRect: TFLoatRect;
  OwnedPath: TFlattenedPath;
  GlyphMetrics: TGlyphMetrics32;
  SpaceWidth: Single;
  JustificationSpace: Single;
  InterWordSpaceFactor: Single;
  InterCharSpace: Single;
  LineAdvanceY: Single;
  ParagraphAdvanceY: Single;
  LineIndex: integer;
  Line: PTextParagraph;
  AdvanceX: Single;
  AlignmentHorizontal: TTextAlignmentHorizontal;
  InterCharCount: integer;
  n: integer;
const
  OneHalf: Single = 0.5; // Typed constant to avoid Double/Extended
begin

  AFontFace.GetFontFaceMetrics(ATextLayout, FontFaceMetrics);


  (*
  ** Parameter validation
  *)
  if (APath = nil) or (ARect.Right = ARect.Left) or (ARect.Top = ARect.Bottom)then
  begin

    // Either measuring AText or unbounded
    ARect.Right := MaxInt;
    ARect.Bottom := MaxInt;

    // We cannot align without bounds
    ATextLayout.AlignmentHorizontal := TextAlignHorLeft;
    ATextLayout.AlignmentVertical := TextAlignVerTop;

    // Disallow wordwrap without bounds
    ATextLayout.WordWrap := False;

  end;

  if (ATextLayout.SingleLine) then
  begin
    // Disallow horizontal justification with single-line
    if (ATextLayout.AlignmentHorizontal = TextAlignHorJustify) then
      ATextLayout.AlignmentHorizontal := TextAlignHorLeft;

    // Disallow wordwrap with single-line
    ATextLayout.WordWrap := False;
  end;


  (*
  ** Convert from AText to Unicode codepoints and separate codepoints into paragraphs
  *)
  TextCharacterString := LayoutEngine.TextToParagraphs(AText, Paragraphs, ATextLayout);


  (*
  ** Get character metrics
  *)
  LayoutEngine.GetGlyphMetrics(TextCharacterString, AFontFace);


  (*
  ** Apply kerning
  *)
  if (ATextLayout.Kerning) then
    LayoutEngine.ApplyKerning(AFontFace, TextCharacterString);

  (*
  ** Word wrap lines
  *)
  if (ATextLayout.WordWrap) then
    Lines := LayoutEngine.BreakParagraphs(TextCharacterString, Paragraphs, ARect.Width, ATextLayout)
  else
    Lines := Paragraphs;

  Paragraphs := nil;


  OwnedPath := nil;
  try

    (*
    ** Setup output outline buffer
    *)
    if (APath <> nil) then
    begin

      if (APath is TFlattenedPath) then
      begin
        TextPath := TFlattenedPath(APath);
        TextPath.Clear;
      end else
      begin
        OwnedPath := TFlattenedPath.Create;
        TextPath := OwnedPath;
      end

    end else
      TextPath := nil;


    // We want the returned rect to reflect the theoretical bounds
    // of the layout, not what we actually rendered.
    // Therefore we need to use a separate rect for clipping.
    ClipRect := ARect;

    (*
    ** Calculate vertical offsets
    *)
    // We know the number of lines so we can calculate the vertical size and
    // from that the vertical offset.
    // The vertical height of a line is Ascent + Descent = Height
    // The vertical space between lines is LineGap
    //
    // Total height of all lines, for Count > 0, is:
    //
    //   Count * (Ascent + Descent + LineGap) - LineGap = Count * (Height + LineGap) - LineGap
    //
    LineAdvanceY := FontFaceMetrics.Height + FontFaceMetrics.LineGap;
    ParagraphAdvanceY := LineAdvanceY;

    if (Length(Lines) > 0) then
    begin
      LineAdvanceY := LineAdvanceY * ATextLayout.InterLineFactor;
      ParagraphAdvanceY := ParagraphAdvanceY * ATextLayout.InterParagraphFactor;

      Height := Length(Lines) * LineAdvanceY - FontFaceMetrics.LineGap * ATextLayout.InterLineFactor;

      // If line- and paragraph-spacing differs then we need to first
      // count the number of paragraphs and adjust the height according
      // to the difference in line- and paragraph spacing
      n := 0;
      if (LineAdvanceY <> ParagraphAdvanceY) then
      begin
        // Note that the last line is always a paragraph but we treat it as a line.
        for i := 0 to High(Lines)-1 do
          if (Lines[i].IsParagraph) then
            Inc(n);

        if (n > 0) then
          Height := Height + n * (ParagraphAdvanceY - LineAdvanceY);
      end;
    end else
      Height := 0;

    case ATextLayout.AlignmentVertical of
      TextAlignVerTop:
        begin
          Y := ARect.Top;
          ARect.Bottom := ARect.Top + Height;
        end;

      TextAlignVerCenter:
        begin
          Y := ARect.Top + (ARect.Height - Height) * OneHalf;
          ARect.Top := Y;
          ARect.Bottom := Y + Height;
        end;

      TextAlignVerBottom:
        begin
          Y := ARect.Bottom - Height;
          ARect.Top := Y;
        end;

    else
      Y := 0; // Kills compiler warning
    end;

    GR32.IntersectRect(ClipRect, ClipRect, ARect);


    // Start at baseline
    Y := Y + FontFaceMetrics.Ascent;

    // Max line width can't be easily precalculated because of
    // justification, clipping, etc. (and it's easier to just
    // find it inside the loop).
    XMax := ARect.Left;

    (*
    ** Align and output each line
    *)
    for LineIndex := 0 to High(Lines) do
    begin

      // Advance vertically to next line
      // (Keep this here, at the top, so "continue" doesn't break everything :-)
      if (LineIndex > 0) then
      begin
        if (Lines[LineIndex-1].IsParagraph) then
          Y := Y + ParagraphAdvanceY
        else
          Y := Y + LineAdvanceY;
      end;


      if (ATextLayout.ClipLayout) then
      begin
        // Clip top (remember Y is baseline)
        if (Y - FontFaceMetrics.Descent < ClipRect.Top) then
          continue;

        // Clip bottom
        if (Y - FontFaceMetrics.Ascent > ClipRect.Bottom) then
          break;
      end;

      Line := @Lines[LineIndex];

      // Calculate line width if we need it
      Line.Width := 0;
      if (ATextLayout.AlignmentHorizontal <> TextAlignHorLeft) and (Line.Count > 0) then
      begin

        for i := Line.StartIndex to Line.LastIndex do
          Line.Width := Line.Width + TextCharacterString[i].Metrics.AdvanceX;

        // Remove leftmost LSB and rightmost RSB
        Line.Width := Line.Width - TextCharacterString[Line.StartIndex].Metrics.LeftSideBearing;
        Line.Width := Line.Width - TextCharacterString[Line.LastIndex].Metrics.RightSideBearing;
      end;


      (*
      ** Horizontal alignment
      *)
      AlignmentHorizontal := ATextLayout.AlignmentHorizontal;

      if (AlignmentHorizontal = TextAlignHorJustify) then
      begin

        // For horizontal justification we apply an alignment override to the last line of the paragraph
        if (LineIndex = High(Lines)) or (Line.ParagraphIndex <> Lines[LineIndex+1].ParagraphIndex) then
        begin
          case ATextLayout.AlignmentHorizontalLastLine of
            TextAlignHorLastStart:
              // Start alignment relies on LTR/RTL but for now we only support LTR
              AlignmentHorizontal := TextAlignHorLeft;

            TextAlignHorLastCenter:
              AlignmentHorizontal := TextAlignHorCenter;

            TextAlignHorLastJustify:
              AlignmentHorizontal := TextAlignHorJustify;
          end;
        end;

      end;

      case AlignmentHorizontal of
        TextAlignHorLeft:
          X := ARect.Left;

        TextAlignHorCenter:
          X := ARect.Left + (ARect.Width - Line.Width) * OneHalf;

        TextAlignHorRight:
          X := ARect.Right - Line.Width;

        TextAlignHorJustify:
          X := ARect.Left;
      else
        X := 0; // Kills compiler warning
      end;


      (*
      ** Calculate horizontal justification factors
      *)
      InterWordSpaceFactor := 1.0;
      InterCharSpace := 0.0;

      if (AlignmentHorizontal = TextAlignHorJustify) then
      begin

        // Count white-space, interchar spaces, and total white-space width
        SpaceWidth := 0.0;
        InterCharCount := 0;
        for i := Line.StartIndex to Line.LastIndex do
          if (TextCharacterString[i].UnicodeCategory = TUnicodeCategory.ucSpaceSeparator) then
            SpaceWidth := SpaceWidth + TextCharacterString[i].Metrics.AdvanceX
          else
          if (i > Line.StartIndex) and (TextCharacterString[i-1].UnicodeCategory <> TUnicodeCategory.ucSpaceSeparator) then
            Inc(InterCharCount);


        JustificationSpace := ARect.Width - Line.Width + SpaceWidth;

        // Interword spacing
        if (SpaceWidth > 0) then
        begin

           // Ratio between desired space width and current space width
          InterWordSpaceFactor := JustificationSpace / SpaceWidth;

          // Limit interword spacing
          if (InterWordSpaceFactor > ATextLayout.MaxInterWordSpaceFactor) then
            InterWordSpaceFactor := ATextLayout.MaxInterWordSpaceFactor
          else
          if (InterWordSpaceFactor < ATextLayout.MinInterWordSpaceFactor) then
            InterWordSpaceFactor := ATextLayout.MinInterWordSpaceFactor;

          JustificationSpace := JustificationSpace - SpaceWidth * InterWordSpaceFactor;
        end;

        // Interchar spacing
        if (InterCharCount > 0) and (ATextLayout.MaxInterCharSpaceFactor > 0.0) then
        begin

          // Remaining width not handled by interword spacing
          if (JustificationSpace  > 0) and (ATextLayout.MaxInterCharSpaceFactor > 0) then
          begin

            // How much extra space per character pair?
            InterCharSpace := JustificationSpace / InterCharCount;

            // Limit interchar spacing
            if (InterCharSpace > FontFaceMetrics.EMSpaceWidth * ATextLayout.MaxInterCharSpaceFactor) then
              InterCharSpace := FontFaceMetrics.EMSpaceWidth * ATextLayout.MaxInterCharSpaceFactor
            else
            if (InterCharSpace < 0) then
              InterCharSpace := 0.0;

            JustificationSpace := JustificationSpace - InterCharCount * InterCharSpace;

          end else
            InterCharSpace := 0.0;
        end;

        // If we can't justify then we might as well not try as it just makes the line
        // look bad.
        if (JustificationSpace > FontFaceMetrics.EMSpaceWidth * 0.01) then // Allow rounding errors
        begin
          InterWordSpaceFactor := 1.0;
          InterCharSpace := 0.0;
        end;
      end;

      // Batch whole line APath construction so we can be sure that the APath isn't rendered
      // while we're still building it.
      if (TextPath <> nil) then
        TextPath.BeginUpdate;

      for i := Line.StartIndex to Line.LastIndex do
      begin

        // Remove LSB from first character so it aligns against the margin
        if (i = Line.StartIndex) then
          X := X - TextCharacterString[i].Metrics.LeftSideBearing;

        if (ATextLayout.ClipLayout) then
        begin
          // Clip to left side
          Skip := (X + TextCharacterString[i].Metrics.AdvanceX < ClipRect.Left);

          // Clip to right side
          if (X - TextCharacterString[i].Metrics.LeftSideBearing > ClipRect.Right) then
            break;
        end else
          Skip := False;

        AdvanceX := TextCharacterString[i].Metrics.AdvanceX;

        // Apply horizontal justification
        if (AlignmentHorizontal = TextAlignHorJustify) then
        begin

          if (TextCharacterString[i].UnicodeCategory = TUnicodeCategory.ucSpaceSeparator) then
            AdvanceX := AdvanceX * InterWordSpaceFactor
          else
          if (i < Line.LastIndex) and (TextCharacterString[i+1].UnicodeCategory <> TUnicodeCategory.ucSpaceSeparator) then
            AdvanceX := AdvanceX + InterCharSpace
          else
            AdvanceX := AdvanceX;

        end;


        (*
        ** Output character glyph
        *)
        if (not Skip) then
          AFontFace.GetGlyphOutline(TextCharacterString[i].CodePoint, GlyphMetrics, TextPath, X, Y);

        if (X > XMax) then
          XMax := X;


        // Advance horizontally to next char
        X := X + AdvanceX;

      end;


      (*
      ** Output line
      *)
      if (TextPath <> nil) then
        TextPath.EndUpdate;

    end;


    (*
    ** Horizontally adjust returned output rect
    *)
    case ATextLayout.AlignmentHorizontal of
      TextAlignHorLeft:
        ARect.Right := XMax;

      TextAlignHorCenter:
        begin
          ARect.Left := ARect.Left + (ARect.Right - XMax) * OneHalf;
          ARect.Right := ARect.Left + XMax;
        end;

      TextAlignHorRight:
        ARect.Left := ARect.Right - (XMax - ARect.Left);

      TextAlignHorJustify:
        ;
    end;


    (*
    ** Return output buffer
    *)
    if (APath <> nil) and (APath <> TextPath) then
      APath.Assign(TextPath);

  finally
    OwnedPath.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      TextToPath
//
//------------------------------------------------------------------------------
class procedure LayoutEngine.ApplyKerning(const FontFace: IFontFace32; var TextCharacterString: TTextCharacterString);
var
  i: integer;
  First, Second: Cardinal;
  Kerning: Single;
begin
  Second := TextCharacterString[0].CodePoint;

  for i := 1 to High(TextCharacterString) do
  begin

    First := Second;
    Second := TextCharacterString[i].CodePoint;

    Kerning := FontFace.GetKerning(First, Second);

    if (Kerning <> 0) then
      TextCharacterString[i-1].Metrics.AdvanceX := TextCharacterString[i-1].Metrics.AdvanceX + Kerning;

  end;
end;

//------------------------------------------------------------------------------
//
//      Text to paragraph
//
//------------------------------------------------------------------------------
function TTextParagraph.GetLastIndex: integer;
begin
  Result := StartIndex + Count - 1;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
