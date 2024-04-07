unit GR32_VPR;

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
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  GR32;

type
  PSingleArray = GR32.PSingleArray;

  TValueSpan = record
    X1, X2: Integer;
    Values: PSingleArray;
  end;

  TRenderSpanEvent = procedure(const Span: TValueSpan; DstY: Integer) of object;
  TRenderSpanProc = procedure(Data: Pointer; const Span: TValueSpan; DstY: Integer);

procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;
procedure RenderPolygon(const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;
procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent); overload;
procedure RenderPolygon(const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent); overload;

implementation

{$if Defined(FPC) and Defined(CPUx86_64) }
// Must apply work around for negative array index on FPC 64-bit.
// See:
//   - https://github.com/graphics32/graphics32/issues/51
//   - https://forum.lazarus.freepascal.org/index.php/topic,44655.0.html
  {$define NEGATIVE_INDEX_64}
{$ifend}

uses
  Math,
  GR32_Math,
  GR32_LowLevel,
  GR32_VectorUtils;

(* Mattias Andersson (from glmhlg$rf3$1@news.graphics32.org):

> Which algorithm are you using for coverage calculation?

I don't have any references, since it's entirely my own design. Here is
a basic outline of how I compute the coverage values:

1. split each line segment into smaller segments in a vertical buffer,
   such that y-values are between 0 and 1;

2. poly-polygons involves a merge step for vertical buffers;

3. Extract spans of coverage values for each scanline:
    (a) set the length of the span to the horizontal range of that row;
    (b) if a line segment goes from row Y to row Y + 1 then we need to add
        or subtract 1 from the (X, X + 1) indexes at the crossing (depending on
        line orientation);
    (c) compute cumulative sum of span values (expensive!);
    (d) integrate each line segment and accumulate span buffer.

The rendering step takes the coverage values and transforms that into an
alpha buffer that is blended onto the target bitmap (here we use the
non-zero and even-odd fill rules).

Initially I was sorting the crossing points of each scanline, but I
realized that by performing a cumulative sum, this would be completely
redundant.

Currently I only compute a single span of coverage values for each
scanline, but I think I should also implement a case where I compute
multiple RLE encoded spans (which I think could be faster in some cases).

There is a tricky case that might not always yield an accurate coverage
value (when we have positively and negatively oriented lines of two
different polygons/faces in the same pixel). The only way to overcome
this would be by preprocessing the polygons and remove intersections. I
believe this very problem exists in AGG and FreeType too.

*)

type
  PLineSegment = ^TLineSegment;
  TLineSegment = array [0..1] of TFloatPoint;

  PLineSegmentArray = ^TLineSegmentArray;
  TLineSegmentArray = array [0..0] of TLineSegment;

  PScanLine = ^TScanLine;
  TScanLine = record
    Segments: PLineSegmentArray;
    Count: Integer;
    Y: Integer;
  end;
  TScanLines = array of TScanLine;
  PScanLineArray = ^TScanLineArray;
  TScanLineArray = array [0..0] of TScanLine;

procedure IntegrateSegment(var P1, P2: TFloatPoint; Values: PSingleArray);
var
{$if Defined(NEGATIVE_INDEX_64) }
  X1, X2: Int64;
{$else}
  X1, X2: Integer;
{$ifend}
  I: Integer;
  Dx, Dy, DyDx, Sx, Y, fracX1, fracX2: TFloat;
begin
  X1 := Round(P1.X);
  X2 := Round(P2.X);
  if X1 = X2 then
  begin
    Values[X1] := Values[X1] + 0.5 * (P2.X - P1.X) * (P1.Y + P2.Y);
  end
  else
  begin
    fracX1 := P1.X - X1;
    fracX2 := P2.X - X2;

    Dx := P2.X - P1.X;
    Dy := P2.Y - P1.Y;
    DyDx := Dy/Dx;

    if X1 < X2 then
    begin
      Sx := 1 - fracX1;
      Y := P1.Y + Sx * DyDx;
      Values[X1] := Values[X1] + 0.5 * (P1.Y + Y) * Sx;
      for I := X1 + 1 to X2 - 1 do
      begin
        Values[I] := Values[I] + (Y + DyDx * 0.5);     // N: Sx = 1
        Y := Y + DyDx;
      end;

      Sx := fracX2;
      Values[X2] := Values[X2] + 0.5 * (Y + P2.Y) * Sx;
    end
    else // X1 > X2
    begin
      Sx := fracX1;
      Y := P1.Y - Sx * DyDx;
      Values[X1] := Values[X1] - 0.5 * (P1.Y + Y) * Sx;
      for I := X1 - 1 downto X2 + 1 do
      begin
        Values[I] := Values[I] - (Y - DyDx * 0.5);    // N: Sx = -1
        Y := Y - DyDx;
      end;
      Sx := 1 - fracX2;
      Values[X2] := Values[X2] - 0.5 * (Y + P2.Y) * Sx;
    end;
  end;
end;

procedure ExtractSingleSpan(const ScanLine: TScanLine; out Span: TValueSpan;
  SpanData: PSingleArray);
var
  I: Integer;
{$if Defined(NEGATIVE_INDEX_64) }
  X: Int64;
{$else}
  X: Integer;
{$ifend}
  P: PFloatPoint;
  S: PLineSegment;
  fracX: TFloat;
  Points: PFloatPointArray;
  N: Integer;
begin
  N := ScanLine.Count * 2;
  Points := @ScanLine.Segments[0];
  Span.X1 := High(Integer);
  Span.X2 := Low(Integer);

  P := @Points[0];
  for I := 0 to N - 1 do
  begin
    // Since we know X >= 0 we could have used Trunc here but unfortunately
    // Delphi's Trunc is much slower than Round because it modifies the FPU
    // control word.
    X := Round(P.X);

    if X < Span.X1 then
      Span.X1 := X;

    if P.Y = 1 then
    begin
      fracX := P.X - X;
      if Odd(I) then
      begin
        SpanData[X] := SpanData[X] + (1 - fracX); Inc(X);
        SpanData[X] := SpanData[X] + fracX;
      end
      else
      begin
        SpanData[X] := SpanData[X] - (1 - fracX); Inc(X);
        SpanData[X] := SpanData[X] - fracX;
      end;
    end;

    if X > Span.X2 then
      Span.X2 := X;

    inc(P);
  end;

  X := Span.X1; // Use X so NEGATIVE_INDEX_64 is handled
  Span.Values := @SpanData[X];

  CumSum(Span.Values, Span.X2 - Span.X1 + 1);

  for I := 0 to ScanLine.Count - 1 do
  begin
    S := @ScanLine.Segments[I];
    IntegrateSegment(S[0], S[1], SpanData);
  end;
end;

procedure AddSegment(const X1, Y1, X2, Y2: TFloat; var ScanLine: TScanLine); {$IFDEF USEINLINING} inline; {$ENDIF}
var
  S: PLineSegment;
  Y1bin: Cardinal absolute Y1;
  Y2bin: Cardinal absolute Y2;
begin
  // Fast way of checking a Single = 0.
  if (Y1bin shl 1 = 0) and (Y2bin shl 1 = 0) then
  // if (Y1 = 0) and (Y2 = 0) then
    Exit;  { needed for proper clipping }

  S := @ScanLine.Segments[ScanLine.Count];
  Inc(ScanLine.Count);

  S[0].X := X1;
  S[0].Y := Y1;
  S[1].X := X2;
  S[1].Y := Y2;
end;

procedure DivideSegment(var P1, P2: TFloatPoint; const ScanLines: PScanLineArray);
var
  Y, Y1, Y2: Integer;
  X, X2: TFloat;
  k: TFloat;
begin
  (*
  ** Split each line segment into smaller segments in a vertical buffer,
  ** such that y-values are between 0 and 1.
  *)

  Y1 := Round(P1.Y);
  Y2 := Round(P2.Y);

  // Special case for horizontal line; It just produces a single segment.
  if Y1 = Y2 then
  begin
    AddSegment(P1.X, P1.Y - Y1, P2.X, P2.Y - Y1, ScanLines[Y1]);
  end
  else
  begin
    // k: Inverse slope; For each change in Y, how much does X change
    // k is expanded below to limit rounding errors.
    k := (P2.X - P1.X) / (P2.Y - P1.Y);

    if Y1 < Y2 then // Y is increasing
    begin
      X := P1.X + (Y1 + 1 - P1.Y) * { k } (P2.X - P1.X) / (P2.Y - P1.Y);
      AddSegment(P1.X, P1.Y - Y1, X, 1, ScanLines[Y1]);

      for Y := Y1 + 1 to Y2 - 1 do
      begin
        // Note: Iteratively calculating the next X value based on the previous value and an
        // increment accumulates the rounding error.
        // Ideally we would repeat the calculation of X from Y for each Y to avoid this but
        // that is too expensive.
        // Because of the rounding error we can end up with a tiny negative X value (when X
        // almost equals k) and, because we've set the rounding mode to rmDown, this negative
        // X value will later be rounded down to -1 in ExtractSingleSpan.
        // This is the cause of issue #272.
        // The Max(0, ...) below works around this problem.

        X2 := Max(0, X + k);
        AddSegment(X, 0, X2, 1, ScanLines[Y]);
        X := X2;
      end;

      AddSegment(X, 0, P2.X, P2.Y - Y2, ScanLines[Y2]);
    end
    else
    begin
      X := P1.X + (Y1 - P1.Y) * { k } (P2.X - P1.X) / (P2.Y - P1.Y);
      AddSegment(P1.X, P1.Y - Y1, X, 0, ScanLines[Y1]);

      for Y := Y1 - 1 downto Y2 + 1 do
      begin
        X2 := Max(0, X - k);
        AddSegment(X, 1, X2, 0, ScanLines[Y]);
        X := X2;
      end;

      AddSegment(X, 1, P2.X, P2.Y - Y2, ScanLines[Y2]);
    end;
  end;
end;

procedure BuildScanLines(const Points: TArrayOfArrayOfFloatPoint;
  out ScanLines: TScanLines);
var
  I,J,K, M,N, Y0,Y1,Y, YMin,YMax: Integer;
  PY: PSingle;
  PPt1, PPt2: PFloatPoint;
  PScanLines: PScanLineArray;
begin

  (*
  ** Determine range of Y values (i.e. scanlines)
  *)
  YMin := MaxInt;
  YMax := -MaxInt;
  M := High(Points);
  for K := 0 to M do
  begin
    N := High(Points[K]);
    if N < 2 then
      Continue;

    PY := @Points[K][0].Y;
    for I := 0 to N do
    begin
      Y := Round(PY^);

      if YMin > Y then
        YMin := Y;
      if YMax < Y then
        YMax := Y;

      inc(PY, 2); // skips X value
    end;
  end;

  if YMin > YMax then
    Exit;

  SetLength(ScanLines, YMax - YMin + 2);

  // Offset scanline pointer so we don't have to offset the Y coordinate
  PScanLines := @ScanLines[-YMin];

  (*
  ** Compute array sizes for each scanline
  *)
  for K := 0 to M do
  begin
    N := High(Points[K]);
    if N < 2 then
      Continue;

    Y0 := Round(Points[K][N].Y);
    PY := @Points[K][0].Y;
    for I := 0 to N do
    begin
      Y1 := Round(PY^);
      if Y0 <= Y1 then
      begin
        Inc(PScanLines[Y0].Count);
        Dec(PScanLines[Y1 + 1].Count);
      end
      else
      begin
        Inc(PScanLines[Y1].Count);
        Dec(PScanLines[Y0 + 1].Count);
      end;
      Y0 := Y1;
      inc(PY, 2); // skips X value
    end;
  end;

  (*
  ** Allocate memory
  *)
  J := 0;
  for I := 0 to High(ScanLines) do
  begin
    Inc(J, ScanLines[I].Count);
    GetMem(ScanLines[I].Segments, J * SizeOf(TLineSegment));

    ScanLines[I].Count := 0;
    ScanLines[I].Y := YMin + I;
  end;

  (*
  ** Divide all segments of the polygon into scanline fragments
  *)
  for K := 0 to M do
  begin
    N := High(Points[K]);
    if N < 2 then
      Continue;

    // Start with the segment going from the last vertex to the first
    PPt1 := @Points[K][N];
    PPt2 := @Points[K][0];

    for I := 0 to N do
    begin
      DivideSegment(PPt1^, PPt2^, PScanLines);

      // Move on to the next segment
      PPt1 := PPt2;
      Inc(PPt2);
    end;
  end;
end;

procedure RenderScanline(var ScanLine: TScanLine;
  RenderProc: TRenderSpanProc; Data: Pointer; SpanData: PSingleArray; ClipX1, ClipX2: Integer);
var
  Span: TValueSpan;
{$if Defined(NEGATIVE_INDEX_64) }
  X: Int64;
{$else}
  X: Integer;
{$ifend}
begin
  if ScanLine.Count > 0 then
  begin
    ExtractSingleSpan(ScanLine, Span, SpanData);

    if Span.X1 < ClipX1 then
      Span.X1 := ClipX1;
    if Span.X2 > ClipX2 then
      Span.X2 := ClipX2;
    if Span.X2 < Span.X1 then
      Exit;

    RenderProc(Data, Span, ScanLine.Y);

    X := Span.X1;
    FillLongWord(SpanData[X], Span.X2 - Span.X1 + 1, 0);
  end;
end;

{$ifdef FPC}
type
  TRoundingMode = Math.TFPURoundingMode;
{$endif}

procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer);
var
  ScanLines: TScanLines;
  I, Len: Integer;
  Poly: TArrayOfArrayOfFloatPoint;
  SavedRoundMode: TRoundingMode;
  CX1, CX2: Integer;
  SpanData: PSingleArray;
begin
  Len := Length(Points);
  if Len = 0 then
    Exit;

  SavedRoundMode := SetRoundMode(rmDown); // TODO : Not thread safe. Another thread can modify the mode
  try
    SetLength(Poly, Len);
    for i := 0 to Len -1 do
      Poly[i] := ClipPolygon(Points[i], ClipRect);

    BuildScanLines(Poly, ScanLines);

    if (Length(ScanLines) > 0) then
    begin
      CX1 := Round(ClipRect.Left); // Round down = Floor
      CX2 := -Round(-ClipRect.Right) - 1; // Round up = Ceil

      I := CX2 - CX1 + 4;

      GetMem(SpanData, I * SizeOf(Single));

      FillLongWord(SpanData^, I, 0);

      for I := 0 to High(ScanLines) do
      begin
        RenderScanline(ScanLines[I], RenderProc, Data, @SpanData[-CX1 + 1], CX1, CX2);
        FreeMem(ScanLines[I].Segments);
      end;

      FreeMem(SpanData);
    end;
  finally
    SetRoundMode(SavedRoundMode);
  end;
end;

procedure RenderPolygon(const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer);
begin
  RenderPolyPolygon(PolyPolygon(Points), ClipRect, RenderProc, Data);
end;

procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent);
begin
  RenderPolyPolygon(Points, ClipRect, TRenderSpanProc(TMethod(RenderProc).Code), TMethod(RenderProc).Data);
end;

procedure RenderPolygon(const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent);
begin
  RenderPolygon(Points, ClipRect, TRenderSpanProc(TMethod(RenderProc).Code), TMethod(RenderProc).Data);
end;

end.
