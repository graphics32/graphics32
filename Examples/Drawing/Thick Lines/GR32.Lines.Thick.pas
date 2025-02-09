unit GR32.Lines.Thick;

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
 * The Original Code is Thick Lines for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Math,
  GR32;

//------------------------------------------------------------------------------
//
//      DrawThickLine
//
//------------------------------------------------------------------------------
// Aliased, opaque thick line.
// For anti-aliased & alpha blended lines use TCanvas32 instead.
//------------------------------------------------------------------------------
//
// Draws a thick line using a modified Bresenham algorithm.
//
// We basicall draw a thick line by drawing a number of single-pixel width
// lines, one pixel apart. We do this with a Bresenham loop inside a Bresenham
// loop. The inner loop draws a line and the outer loop moves the line end-points.
//
// For a very similar algorithm see:
//
// - Line Thickening by Modification To Bresenham's Algorithm
//   Alan S. Murphy
//   IBM Technical Disclosure Bulletin, Vol. 20, No. 12, May 1978.
//   http://www.zoo.co.uk/murphy/thickline/
//   http://homepages.enterprise.net/murphy/thickline/index.html
//
// See the following for a good explanation of the above:
// - http://kt8216.unixcab.org/murphy/index.html
//
// Murphy's algorithm above moves along the base line and draw lines
// perpendicular to the base line.
// This version however draw lines parallel to the base line.
// This implementation was adapted from code by
// - Armin Joachimsmeyer, Copyright (C) 2013-2022
//
// Missing features:
// - Anti-aliasing.
// - Blending
// - Clipping
//
//------------------------------------------------------------------------------
type
  TLineWidthMode = (
    LineWidthMiddle,              // Start point is on the line at center of the thick line
    LineWidthDrawClockWise,       // Start point is on the counter-clockwise border line
    LineWidthDrawCounterClockwise // Start point is on the clockwise border line
  );

procedure DrawThickLine(Bitmap: TBitmap32; StartX, StartY, EndX, EndY: integer; Width: SmallInt; Color: TColor32;
  WidthMode: TLineWidthMode = LineWidthMiddle);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

type
  TLineOverlap = set of (
                        // None: No line overlap, like in standard Bresenham
    LineOverlapMajor,   // First go major then minor direction. Pixel is drawn as extension after actual line
    LineOverlapMinor    // First go minor then major direction. Pixel is drawn as extension before next line
  );

//------------------------------------------------------------------------------
//
//      InnerDrawThickLine
//
//------------------------------------------------------------------------------
procedure InnerDrawThickLine(Bitmap: TBitmap32; StartX, StartY, EndX, EndY: integer; Overlap: TLineOverlap; Color: TColor32);
var
  DeltaX, DeltaY, TwoDeltaX, TwoDeltaY: SmallInt;
  Error: SmallInt;
  StepX, StepY: SmallInt;
begin

  if (StartX = EndX) then
  begin
    if (StartY < EndY) then
      Bitmap.VertLineS(StartX, StartY, EndY, Color)
    else
      Bitmap.VertLineS(StartX, EndY, StartY, Color);
  end else
  if (StartY = EndY) then
  begin
    if (StartX < EndX) then
      Bitmap.HorzLineS(StartX, StartY, EndX, Color)
    else
      Bitmap.HorzLineS(StartX, EndX, StartY, Color);
  end else
  begin
    { calculate direction }
    DeltaX := EndX - StartX;
    DeltaY := EndY - StartY;

    if DeltaX < 0 then
    begin
      DeltaX := -DeltaX;
      StepX := -1;
    end else
      StepX := 1;

    if DeltaY < 0 then
    begin
      DeltaY := -DeltaY;
      StepY := -1;
    end else
      StepY := 1;

    TwoDeltaX := DeltaX * 2;
    TwoDeltaY := DeltaY * 2;

    { draw start pixel }
    Bitmap.PixelS[StartX, StartY] := Color;

    if DeltaX > DeltaY then
    begin
      { start value represents a half step in Y direction }
      Error := TwoDeltaY - DeltaX;

      while (StartX <> EndX) do
      begin
        { step in main direction }
        Inc(StartX, StepX);

        if (Error >= 0) then
        begin
          if (LineOverlapMajor in Overlap) then
            { draw pixel in major direction before changing }
            Bitmap.PixelS[StartX, StartY] := Color;

          { change Y }
          Inc(StartY, StepY);

          if (LineOverlapMinor in Overlap) then
            { draw pixel in minor direction before changing }
            Bitmap.PixelS[StartX - StepX, StartY] := Color;

          Dec(Error, TwoDeltaX);
        end;

        Inc(Error, TwoDeltaY);

        Bitmap.PixelS[StartX, StartY] := Color;
      end;
    end else
    begin
      { start value represents a half step in X direction }
      Error := TwoDeltaX - DeltaY;

      while (StartY <> EndY) do
      begin
        Inc(StartY, StepY);

        if (Error >= 0) then
        begin
          if (LineOverlapMajor in Overlap) then
            { draw pixel in major direction before changing }
            Bitmap.PixelS[StartX, StartY] := Color;

          { change X }
          Inc(StartX, StepX);

          if (LineOverlapMinor in Overlap) then
            { draw pixel in minor direction before changing }
            Bitmap.PixelS[StartX, StartY - StepY] := Color;

          Dec(Error, TwoDeltaY);
        end;

        Inc(Error, TwoDeltaX);

        Bitmap.PixelS[StartX, StartY] := Color;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
//
//      InnerDrawThickLine
//
//------------------------------------------------------------------------------
procedure DrawThickLine(Bitmap: TBitmap32; StartX, StartY, EndX, EndY: integer; Width: SmallInt; Color: TColor32; WidthMode: TLineWidthMode = LineWidthMiddle);
var
  i: integer;
  DeltaX, DeltaY, TwoDeltaX, TwoDeltaY: SmallInt;
  Error: SmallInt;
  StepX, StepY: SmallInt;
  MirrorQuadrant: Boolean;
  Overlap: TLineOverlap;
  DrawStartAdjustCount: integer;
begin
  if (StartX = EndX) and (StartY = EndY) then
    exit;

  DeltaY := EndX - StartX;
  DeltaX := EndY - StartY;

  // Since we're not drawing anti-aliased we have to adjust the width for the
  // angle of the line. Otherwise diagonal lines would be wider (by Sqrt(2))
  // than straight lines.
  // Note:
  // - Even though were executing a costly Sqrt and float division,
  //   the adjustment actually makes the routine faster on average since we're
  //   potentially reducing the number of lines drawn.
  // - We're using Ceil instead of Trunc or Round to make the width better
  //   match the width of a GDI thick line.
  Width := Ceil(Width * Hypot(DeltaX, DeltaY) / (Abs(DeltaX) + Abs(DeltaY)));

  if (Width <= 1) then
  begin
    InnerDrawThickLine(Bitmap, StartX, StartY, EndX, EndY, [], Color);
    exit;
  end;

  // Bresenham's algorithm only works in quadrant 1, so mirror 4 quadrants to one
  // and adjust deltas and stepping direction.

  // Make sure we are in quadrant 1 or 4
  MirrorQuadrant := True;
  if (DeltaX < 0) then
  begin
    DeltaX := -DeltaX;
    StepX := -1;
    MirrorQuadrant := not MirrorQuadrant;
  end else
    StepX := 1;

  // Make sure we are in quadrant 1
  if (DeltaY < 0) then
  begin
    DeltaY := -DeltaY;
    StepY := -1;
    MirrorQuadrant := not MirrorQuadrant;
  end else
    StepY := 1;

  // Now Delta* are positive and Step* define the direction.
  // MirrorQuadrant is False if we mirrored only once.

  TwoDeltaX := DeltaX * 2;
  TwoDeltaY := DeltaY * 2;

  // Adjust for right direction of thickness from line origin
  case WidthMode of
    LineWidthMiddle:
      DrawStartAdjustCount := Width div 2;

    LineWidthDrawCounterClockwise:
      DrawStartAdjustCount := Width - 1;

    LineWidthDrawClockWise:
      DrawStartAdjustCount := 0
  else
    DrawStartAdjustCount := 0; // Shut compiler up
  end;

  // Which octant are we now?
  if (DeltaX >= DeltaY) then
  begin

    // Octant 1, 3, 5, 7 (between 0 and 45, 90 and 135, etc. degree)

    if (MirrorQuadrant) then
    begin
      DrawStartAdjustCount := (Width - 1) - DrawStartAdjustCount;
      StepY := -StepY;
    end else
      StepX := -StepX;

    // Vector for draw direction of the starting points of lines is perpendicular
    // and counter-clockwise to main line direction.
    // Therefore no pixel will be missed if LineOverlapMajor is used on change
    // in minor perpendicular direction.

    // Adjust draw start point
    Error := TwoDeltaY - DeltaX;
    for i := 0 to DrawStartAdjustCount-1 do
    begin
      // Change X (main direction here)
      Dec(StartX, StepX);
      Dec(EndX, StepX);

      // Change Y
      if (Error >= 0) then
      begin
        Dec(StartY, StepY);
        Dec(EndY, StepY);
        Dec(Error, TwoDeltaX);
      end;

      Inc(Error, TwoDeltaY);
    end;

    // Draw start line.
    InnerDrawThickLine(Bitmap, StartX, StartY, EndX, EndY, [], Color);

    // Draw Width-1 number of lines (-1 because we have already drawn one)
    Error := TwoDeltaY - DeltaX;
    for i := 0 to Width-2 do
    begin
      // Change X (main direction here)
      Inc(StartX, StepX);
      Inc(EndX, StepX);
      Overlap := [];

      // Change Y
      if (Error >= 0) then
      begin
        Inc(StartY, StepY);
        Inc(EndY, StepY);
        Dec(Error, TwoDeltaX);
        Overlap := [LineOverlapMajor];
      end;

      Inc(Error, TwoDeltaY);

      InnerDrawThickLine(Bitmap, StartX, StartY, EndX, EndY, Overlap, Color);
    end;

  end else
  begin

    // Octant 2, 4, 6, 8 (between 45 and 90, 135 and 180, etc. degree)

    if (MirrorQuadrant) then
      StepX := -StepX
    else
    begin
      DrawStartAdjustCount := (Width - 1) - DrawStartAdjustCount;
      StepY := -StepY;
    end;

    // Adjust draw start point
    Error := TwoDeltaX - DeltaY;
    for i := 0 to DrawStartAdjustCount-1 do
    begin
      Dec(StartY, StepY);
      Dec(EndY, StepY);

      if (Error >= 0) then
      begin
        Dec(StartX, StepX);
        Dec(EndX, StepX);
        Dec(Error, TwoDeltaY);
      end;

      Inc(Error, TwoDeltaX);
    end;

    // Draw start line.
    InnerDrawThickLine(Bitmap, StartX, StartY, EndX, EndY, [], Color);

    // Draw Width-1 number of lines (-1 because we have already drawn one)
    Error := TwoDeltaX - DeltaY;
    for i := 0 to Width-2 do
    begin
      // Change Y (main direction here)
      Inc(StartY, StepY);
      Inc(EndY, StepY);
      Overlap := [];

      // Change X
      if (Error >= 0) then
      begin
        Inc(StartX, StepX);
        Inc(EndX, StepX);
        Dec(Error, TwoDeltaY);
        Overlap := [LineOverlapMajor];
      end;

      Inc(Error, TwoDeltaX);

      InnerDrawThickLine(Bitmap, StartX, StartY, EndX, EndY, Overlap, Color);
    end;

  end;
end;

end.
