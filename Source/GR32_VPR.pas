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

{$include GR32.inc}

uses
  GR32;

type
  PSingleArray = GR32.PSingleArray;

  TValueSpan = record
    LowX, HighX: Integer;
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

{$if defined(FPC) and defined(CPUx86_64) }
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
  GR32_VectorUtils,
  GR32.Types.SIMD,
  GR32_Bindings;


var
  IntegrateSegment: procedure (const P1, P2: TFloatPoint; Values: PSingleArray);

// FastFloor is slow on x86 due to call overhead
{$if (not defined(PUREPASCAL)) and defined(CPUx86_64)}
// Use of FastFloor in VPR currently corrupts the memory manager of FPC
// so temporarily disabled there.
  {$if (not defined(FPC))}
    {$define USE_POLYFLOOR}
  {$ifend}
{$ifend}

function PolyFloor(Value: Single): integer; overload; {$ifndef DEBUG} inline; {$endif}
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastFloorSingle(Value);
{$else}
  Result := Round(Value);
{$ifend}
end;

function PolyFloor(Value: Double): integer; overload; {$ifndef DEBUG} inline; {$endif}
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastFloorDouble(Value);
{$else}
  Result := Round(Value);
{$ifend}
end;

function PolyCeil(Value: Single): integer; overload; {$ifndef DEBUG} inline; {$endif}
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastCeilSingle(Value);
{$else}
  Result := -Round(-Value);
{$ifend}
end;

function PolyCeil(Value: Double): integer; overload; {$ifndef DEBUG} inline; {$endif}
begin
{$if defined(USE_POLYFLOOR)}
  Result := FastCeilDouble(Value);
{$else}
  Result := -Round(-Value);
{$ifend}
end;

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

  TScanLine = record
    Segments: PLineSegmentArray;
    Count: Integer;
    Y: Integer;
  end;
  TScanLines = array of TScanLine;
  PScanLineArray = ^TScanLineArray;
  TScanLineArray = array [0..0] of TScanLine;

procedure IntegrateSegment_SSE2(const P1, P2: TFloatPoint; Values: PSingleArray); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
  // EAX <- P1
  // EDX <- P2
  // ECX <- Count
        PUSH EDI
        PUSH ESI
        MOVQ      XMM0, [EAX]                   // P1
        MOVQ      XMM1, [EDX]                   // P2
        MOV       EDX, ECX                      // EDX <- Values
        MOVLHPS   XMM0, XMM1                    // XMM0 <- P2.Y P2.X P1.Y P1.Y
        CVTPS2DQ  XMM1, XMM0                    // XMM1 <- Rounded Y2 X2 Y1 X1
        PSHUFD    XMM7 ,XMM1, $EE               // XMM7 <- Y2, X2, Y2, X2
        MOVD      EDI, XMM7                     // ECX <- X2
        MOVD      ESI, XMM1                     // EAX <- X1
        CMP       EDI, ESI
        JE        @Equals

        CVTDQ2PS  XMM3, XMM1
        MOVAPS    XMM2, XMM0
        SUBPS     XMM2, XMM3                    // XMM2 <- [. fracX2 . fracX1]
        PSHUFD    XMM3, XMM0, $EE
        SUBPS     XMM3, XMM0                    // XMM3 <- [. . Dy Dx]
        MOVAPS    XMM4, XMM3
        PSHUFD    XMM3, XMM3, $55
        DIVSS     XMM3, XMM4                    // XMM3 <- [. . . DyDx]
        CMP       ESI, EDI
        JG        @X1_Greater                   // if X1 < X2

@X1_Smaller:                                    // X1 < X2
        // Sx := 1 - fracX1;
        MOVAPS    XMM4, DQWORD PTR [SSE_FloatOne_ALIGNED]
        SUBPS     XMM4, XMM2                    // XMM4 <- [. . . Sx]
        MOVAPS    XMM5, XMM4                    // Sx
        // Y := P1.Y + Sx * DyDx;
        MULPS     XMM4, XMM3                    // XMM4 <- [. . . Sx * DyDx]
        PSHUFD    XMM6, XMM0, $55               // XMM6 <- P1.Y P1.Y P1.Y P1.Y
        ADDPS     XMM4, XMM6                    // XMM4 <- [. . . Y]
        // Values[X1] := Values[X1] + 0.5 * (P1.Y + Y) * Sx;
        ADDPS     XMM6, XMM4                    // XMM6 <- (P1.Y + Y)
        MULPS     XMM5, XMM6                    // XMM5 <- (P1.Y + Y) * Sx
        MULPS     XMM5, DQWORD PTR [SSE_FloatHalf_ALIGNED] // XMM5 <- 0.5 * (P1.Y + Y) * Sx
        MOVSS     XMM6, [EDX + ESI * 4]         // Offset to Values[X1]
        ADDSS     XMM6, XMM5                    // XMM6 <- Values[X1] + 0.5 * (P1.Y + Y) * Sx
        MOVSS     [EDX + ESI * 4], XMM6         // Values[X1] := XMM6
        INC       ESI
        DEC       EDI
        CMP       EDI, ESI
        JL        @Last1

        MOVAPS    XMM6, XMM3                    // XMM6 <- DyDx
        MULPS     XMM6, DQWORD PTR [SSE_FloatHalf_ALIGNED] // XMM6 <- dydx * 0.5

@Loop1:
        // Values[I] := Values[I] + (Y + DyDx * 0.5);
        MOVAPS    XMM7, XMM6                    // XMM7 <- DyDx * 0.5
        ADDPS     XMM7, XMM4                    // + Y
        ADDSS     XMM7, [EDX + ESI * 4]         // + Values[I]
        MOVSS     [EDX + ESI * 4], XMM7
        ADDPS     XMM4, XMM3                    // Y := Y + DyDx;
        INC       ESI
        CMP       EDI, ESI
        JGE       @Loop1

@Last1:
        // Sx := fracX2;
        // Values[X2] := Values[X2] + 0.5 * (Y + P2.Y) * Sx;
        SUB       ESI, 1
        PSHUFD    XMM2, XMM2, $AA               // XMM2 <- frac2
        PSHUFD    XMM0, XMM0, $FF
        ADDPS     XMM0, XMM4
        MULSS     XMM0, XMM2
        ADD       EDI, 1
        MULPS     XMM0, DQWORD PTR [SSE_FloatHalf_ALIGNED]
        MOVSS     XMM6, [EDX + EDI * 4]
        ADDPS     XMM6, XMM0
        MOVSS     [EDX + EDI * 4], XMM6
        JMP       @Done

@X1_Greater:                                    // X1 > X2
        MOVAPS    XMM4, XMM2                    // Sx, fracX1
        // Sx := fracX1;
        // Y := P1.Y - Sx * DyDx;
        MULSS     XMM4, XMM3                    // XMM4 <- [. . . Sx * DyDx]
        PSHUFD    XMM6, XMM0, $55               // XMM6 <- P1.Y P1.Y P1.Y P1.Y
        MOVAPS    XMM5, XMM6                    // XMM5 <- P1.Y
        SUBSS     XMM6, XMM4                    // XMM6 <- Y
        // Values[X1] := Values[X1] - 0.5 * (P1.Y + Y) * Sx;
        ADDSS     XMM5, XMM6                    // XMM6 <- (P1.Y + Y)
        MULSS     XMM5, XMM2                    // XMM6 <- (P1.Y + Y) * Sx
        MULPS     XMM5, DQWORD PTR [SSE_FloatHalf_ALIGNED] // XMM6 <- 0.5 * (P1.Y + Y) * Sx
        MOVSS     XMM4, [EDX + ESI * 4]         // Offset to Values[X1]
        SUBSS     XMM4, XMM5                    // XMM4 <- Values[X1] - 0.5 * (P1.Y + Y) * Sx
        MOVSS     [EDX + ESI * 4], XMM4         // Values[X1] := XMM6
        // for I := X1 - 1 downto X2 + 1 do
        DEC       ESI
        INC       EDI
        CMP       ESI, EDI
        JL        @Last2

        MOVAPS    XMM4, XMM3                    // XMM4 <- DyDx
        MULPS     XMM4, DQWORD PTR [SSE_FloatHalf_ALIGNED] // XMM4 <- DyDx * 0.5

@Loop2:
        // Values[I] := Values[I] - (Y - DyDx * 0.5);
        MOVSS     XMM5, [EDX + ESI * 4]         // XMM5 <- Values[I]
        SUBSS     XMM5, XMM6                    // - Y
        ADDSS     XMM5, XMM4                    // + DyDx * 0.5 (Changed sign)
        MOVSS     [EDX + ESI * 4], XMM5
        // Y := Y - DyDx;
        SUBPS     XMM6, XMM3
        DEC       ESI
        CMP       EDI, ESI
        JLE       @Loop2

@Last2:
        // Sx := 1 - fracX2;
        // Values[X2] := Values[X2] - 0.5 * (Y + P2.Y) * Sx;
        PSHUFD    XMM3, XMM2, $AA               // XMM2 <- frac2
        MOVAPS    XMM2, DQWORD PTR [SSE_FloatOne_ALIGNED]
        SUBPS     XMM2, XMM3                    // XMM2 <- Sx
        PSHUFD    XMM0, XMM0, $FF               // XMM0 <- P2.Y P2.Y P2.Y P2.Y
        ADDPS     XMM0, XMM6
        MULSS     XMM0, XMM2
        MULPS     XMM0, DQWORD PTR [SSE_FloatHalf_ALIGNED]
        MOVSS     XMM6, [EDX + ESI * 4]
        SUBSS     XMM6, XMM0
        MOVSS     [EDX + ESI * 4], XMM6
        JMP       @Done

(*
@Equals:
  // Values[X1] := Values[X1] + 0.5 * (P2.X - P1.X) * (P1.Y + P2.Y);
  // XMM0 <- (P2.Y P2.X P1.Y P1.X)  * (1 1 1 -1) (XMM6)
        XORPS     XMM0, DQWORD PTR [SSE_80000000_ALIGNED] // P1.X sign change: XMM0 <- (P2.Y P2.X P1.Y -P1.X)
        PSHUFD    XMM1, XMM0, $EE               // XMM1 <- P2.Y P2.X P2.Y P2.X
        ADDPS     XMM0, XMM1                    // Add
        PSHUFD    XMM1, XMM0, $55               // XMM1 <- (P2.X - P1.X), (P1.Y + P2.Y), (P2.X - P1.X), (P1.Y + P2.Y)
        MULPS     XMM0, XMM1
        MULPS     XMM0, DQWORD PTR [SSE_FloatHalf_ALIGNED] // * 0.5
        ADDSS     XMM0, [EDX + ESI * 4]
        MOVSS     [EDX + ESI * 4], XMM0
*)

@Equals:
        // XMM0 contains [Y2, X2, Y1, X1]
        PSHUFD    XMM1, XMM0, $EE               // XMM1 = [Y2, X2, Y2, X2]
        MOVAPS    XMM2, XMM1                    // XMM2 = [Y2, X2, Y2, X2]
        SUBSS     XMM1, XMM0                    // XMM1[0] = X2 - X1
        ADDPS     XMM0, XMM2                    // XMM0 = [Y2+Y2, X2+X2, Y1+Y2, X1+X2]
        PSHUFD    XMM0, XMM0, $55               // XMM0 = [Y1+Y2, Y1+Y2, Y1+Y2, Y1+Y2]
        MULSS     XMM0, XMM1                    // XMM0[0] = (Y1 + Y2) * (X2 - X1)
        MULPS     XMM0, DQWORD PTR [SSE_FloatHalf_ALIGNED] // XMM0[0] = (Y1 + Y2) * (X2 - X1) * 0.5
        ADDSS     XMM0, [EDX + ESI * 4]
        MOVSS     [EDX + ESI * 4], XMM0

@Done:
        POP       ESI
        POP       EDI
{$ENDIF TARGET_x86}
{$IFDEF CPUX64}
        SUB       RSP, 40
        MOVUPS    [RSP], XMM6
        MOVUPS    [RSP + 16], XMM7

        MOVQ      XMM0, [RCX]                   // P1
        MOVQ      XMM1, [RDX]                   // P2
        MOVLHPS   XMM0, XMM1                    // XMM0 <- P2.Y P2.X P1.Y P1.Y
        CVTPS2DQ  XMM1, XMM0                    // XMM1 <- Rounded Y2 X2 Y1 X1
        PSHUFD    XMM7, XMM1, $EE               // XMM7 <- Y2, X2, Y2, X2
        MOVD      R11D, XMM7                    // R11D <- X2
        MOVSXD    R11, R11D                     // R11 <- X2 extend sign
        MOVD      R10D, XMM1
        MOVSXD    R10, R10D                     // R10 <= X1 extend sign
        CMP       R11, R10
        JE        @Equals

        CVTDQ2PS  XMM3, XMM1
        MOVAPS    XMM2, XMM0
        SUBPS     XMM2, XMM3                    // XMM2 <- [. fracX2 . fracX1]
        PSHUFD    XMM3, XMM0, $EE
        SUBPS     XMM3, XMM0                    // XMM3 <- [. . Dy Dx]
        MOVAPS    XMM4, XMM3
        PSHUFD    XMM3, XMM3, $55
        DIVSS     XMM3, XMM4                    // XMM3[0] = DyDx
        CMP       R10, R11
        JG        @X1_Greater                   // if X1 < X2

@X1_Smaller:                                    // X1 < X2
        // Sx := 1 - fracX1;
        MOVAPS    XMM4, DQWORD PTR [SSE_FloatOne_ALIGNED]
        SUBPS     XMM4, XMM2                    // XMM4 <- [. . . Sx]
        MOVAPS    XMM5, XMM4                    // Sx
        // Y := P1.Y + Sx * DyDx;
        MULPS     XMM4, XMM3                    // XMM4 <- [. . . Sx * DyDx]
        PSHUFD    XMM6, XMM0, $55               // XMM6 <- P1.Y P1.Y P1.Y P1.Y
        ADDPS     XMM4, XMM6                    // XMM4 <- [. . . Y]
        // Values[X1] := Values[X1] + 0.5 * (P1.Y + Y) * Sx;
        ADDPS     XMM6, XMM4                    // XMM6 <- (P1.Y + Y)
        MULPS     XMM5, XMM6                    // XMM5 <- (P1.Y + Y) * Sx
        MOVAPS    XMM6, DQWORD PTR [SSE_FloatHalf_ALIGNED]
        MULPS     XMM5, XMM6
        MOVSS     XMM7, [R8 + R10 * 4]          // Offset to Values[X1]
        ADDSS     XMM7, XMM5                    // XMM6 <- Values[X1] + 0.5 * (P1.Y + Y) * Sx
        MOVSS     [R8 + R10 * 4], XMM7          // Values[X1] := XMM6
        INC       R10
        DEC       R11
        CMP       R11, R10
        JL        @Last1
        MOVAPS    XMM7, XMM3                    // XMM6 <- DyDx
        MULPS     XMM7, XMM6                    // XMM7 <- dydx * 0.5

@Loop1:
        // Values[I] := Values[I] + (Y + DyDx * 0.5);
        MOVAPS    XMM5, XMM7                    // XMM7 <- DyDx * 0.5
        ADDPS     XMM5, XMM4                    // + Y
        ADDSS     XMM5, [R8 + R10 * 4]          // + Values[I]
        MOVSS     [R8 + R10 * 4], XMM5
        ADDPS     XMM4, XMM3                    // Y := Y + DyDx;
        INC       R10
        CMP       R11, R10
        JGE       @Loop1

@Last1:
        // Sx := fracX2;
        // Values[X2] := Values[X2] + 0.5 * (Y + P2.Y) * Sx;
        DEC       R10
        PSHUFD    XMM2, XMM2, $AA               // XMM2 <- frac2
        PSHUFD    XMM0, XMM0, $FF
        ADDPS     XMM0, XMM4
        MULSS     XMM0, XMM2
        MULPS     XMM0, XMM6
        INC       R11
        MOVSS     XMM5, [R8 + R11 * 4]
        ADDPS     XMM5, XMM0
        MOVSS     [R8 + R11 * 4], XMM5
        JMP       @Done

@X1_Greater:                                    // X1 > X2
        MOVAPS    XMM4, XMM2                    // sx, fracX1
        // Sx := fracX1;
        // Y := P1.Y - Sx * DyDx;
        MULSS     XMM4, XMM3                    // XMM4 <- [. . . Sx * DyDx]
        PSHUFD    XMM5, XMM0, $55               // XMM6 <- P1.Y P1.Y P1.Y P1.Y
        MOVAPS    XMM6, XMM5                    // XMM5 <- P1.Y
        SUBSS     XMM6, XMM4                    // XMM6 <- Y
        // Values[X1] := Values[X1] - 0.5 * (P1.Y + Y) * Sx;
        ADDSS     XMM5, XMM6                    // XMM6 <- (P1.Y + Y)
        MULSS     XMM5, XMM2                    // XMM6 <- (P1.Y + Y) * Sx
        MOVAPS    XMM4, DQWORD PTR [SSE_FloatHalf_ALIGNED]
        MULPS     XMM5, XMM4                    // XMM6 <- 0.5 * (P1.Y + Y) * Sx
        MOVSS     XMM7, [R8 + R10 * 4]          // Offset to Values[X1]
        SUBSS     XMM7, XMM5                    // XMM4 <- Values[X1] - 0.5 * (P1.Y + Y) * Sx
        MOVSS     [R8 + R10 * 4], XMM7          // Values[X1] := XMM6
        // for I := X1 - 1 downto X2 + 1 do
        DEC       R10
        INC       R11
        CMP       R10, R11
        JL        @Last2

        MOVAPS    XMM5, XMM3                    // XMM4 <- DyDx
        MULPS     XMM5, XMM4                    // XMM4 <- DyDx * 0.5

@Loop2:
        // Values[I] := Values[I] - (Y - DyDx * 0.5);
        MOVSS     XMM7, [R8 + R10 * 4]          // XMM5 <- Values[I]
        SUBSS     XMM7, XMM6                    // - Y
        ADDSS     XMM7, XMM5                    // + DyDx * 0.5 (Changed sign)
        MOVSS     [R8 + R10 * 4], XMM7
        SUBPS     XMM6, XMM3
        DEC       R10
        CMP       R11, R10
        JLE       @Loop2

@Last2:
        // Sx := 1 - fracX2;
        // Values[X2] := Values[X2] - 0.5 * (Y + P2.Y) * Sx;
        PSHUFD    XMM3, XMM2, $AA
        MOVAPS    XMM2, DQWORD PTR [SSE_FloatOne_ALIGNED]
        SUBPS     XMM2, XMM3                    // XMM2 <- 1 - Sx
        PSHUFD    XMM0, XMM0, $FF               // XMM0 <- P2.Y P2.Y P2.Y P2.Y
        ADDPS     XMM0, XMM6
        MULSS     XMM0, XMM2
        MULPS     XMM0, XMM4
        MOVSS     XMM7, [R8 + R10 * 4]
        SUBSS     XMM7, XMM0
        MOVSS     [R8 + R10 * 4], XMM7
        JMP       @Done

@Equals:
        // Values[X1] := Values[X1] + 0.5 * (P2.X - P1.X) * (P1.Y + P2.Y);
        // XMM0 <- (P2.Y P2.X P1.Y P1.X)  * (1 1 1 -1) (XMM6)
        PSHUFD    XMM1, XMM0, $EE               // XMM1 <- P2.Y P2.X P2.Y P2.X
        MOVAPS    XMM2, XMM1                    // Add
        SUBSS     XMM1, XMM0                    // XMM1 <- (P2.X - P1.X), (P1.Y + P2.Y), (P2.X - P1.X), (P1.Y + P2.Y)
        ADDPS     XMM0, XMM2
        PSHUFD    XMM0, XMM0, $55
        MULSS     XMM0, XMM1                    // * 0.5
        MOVAPS    XMM4, DQWORD PTR [SSE_FloatHalf_ALIGNED]
        MULSS     XMM0, XMM4
        ADDSS     XMM0, [R8 + R10 * 4]
        MOVSS     [R8 + R10 * 4], XMM0

@Done:
        MOVUPS    XMM6, [RSP]
        MOVUPS    XMM7, [RSP + 16]
        ADD       RSP, 40
{$ENDIF}
end;

procedure IntegrateSegment_Pas(const P1, P2: TFloatPoint; Values: PSingleArray);
var
{$if defined(NEGATIVE_INDEX_64) }
  X1, X2: Int64;
{$else}
  X1, X2: Integer;
{$ifend}
  i: Integer;
  Dx, Dy, DyDx, Y: TFloat;
  fracX1, fracX2: TFloat;
const
  HalfFloat: TFloat = 0.5;
begin
  (*
  ** We have a line segment going from (X1,Y1) to (X2,Y2):
  **
  **        X1     X2
  **       +---------
  **    Y1 | *
  **       |  *
  **       |   *
  **       |    *
  **       |     *
  **       |      *
  **    Y2 |       *
  **
  ** The Y values in the segment belongs to a single scanline so the line segment is 1 pixel high.
  ** Additionally, we know that the Y values are in the range [0..1].
  ** In the example below, we have a segment where X2-X1=6. Each box is a pixel.
  **
  **        X1                       X2
  **       +---+---+---+---+---+---+---+
  **    Y1 | * |   |   |   |   |   |   |
  **       |   | * |   |   |   |   |   |
  **       |   |   | * |   |   |   |   |
  **       |   |   |   | * |   |   |   |
  **       |   |   |   |   | * |   |   |
  **       |   |   |   |   |   | * |   |
  **    Y2 |   |   |   |   |   |   | * |
  **       +---+---+---+---+---+---+---+
  **
  ** For each X, we need to calculate the area below (or above) the line segment.
  ** We do this by calculating the slope of the line, and from that we can find the Y value
  ** given an X value.
  ** Once we have an X and an Y value we calculate the area as X*Y/2.
  **
  **        X1                       X2
  **       +---+---+---+---+---+---+---+
  **    Y1 | * |   |   |   |   |   |   |
  **       | * | * |   |   |   |   |   |
  **       | * | * | * |   |   |   |   |
  **       | * | * | * | * |   |   |   |
  **       | * | * | * | * | * |   |   |
  **       | * | * | * | * | * | * |   |
  **    Y2 | * | * | * | * | * | * | * |
  **       +---+---+---+---+---+---+---+
  **
  *)


  X1 := PolyFloor(P1.X);
  X2 := PolyFloor(P2.X);

  // Vertical segment (within one pixel)
  if X1 = X2 then
  begin

    Values[X1] := Values[X1] + HalfFloat * (P2.X - P1.X) * (P1.Y + P2.Y);

  end else
  // Everything else
  begin

    Dx := P2.X - P1.X;
    Dy := P2.Y - P1.Y;
    DyDx := Dy/Dx; // For each X, how much does Y increment

    if X1 < X2 then
    begin

      fracX1 := 1 - (P1.X - X1);
      fracX2 := P2.X - X2;

      Y := P1.Y + fracX1 * DyDx;

      // First fractional X (fracX1..1)
      Values[X1] := Values[X1] + HalfFloat * (P1.Y + Y) * fracX1;

      // Whole Xs (1..1)
      for i := X1 + 1 to X2 - 1 do
      begin
        Values[i] := Values[i] + (Y + DyDx * HalfFloat);     // N: Sx = 1
        Y := Y + DyDx;
      end;

      // Last fractional X (1..fracX2)
      Values[X2] := Values[X2] + HalfFloat * (Y + P2.Y) * fracX2;

    end else // X1 > X2
    begin

      fracX1 := P1.X - X1;
      fracX2 := 1 - (P2.X - X2);

      Y := P1.Y - fracX1 * DyDx;

      // First fractional X (fracX1..1)
      Values[X1] := Values[X1] - HalfFloat * (P1.Y + Y) * fracX1;

      // Whole Xs (1..1)
      for i := X1 - 1 downto X2 + 1 do
      begin
        Values[i] := Values[i] - (Y - DyDx * HalfFloat);    // N: Sx = -1
        Y := Y - DyDx;
      end;

      // Last fractional X (1..fracX2)
      Values[X2] := Values[X2] - HalfFloat * (Y + P2.Y) * fracX2;

    end;

  end;
end;

procedure ExtractSingleSpan(const ScanLine: TScanLine; out Span: TValueSpan; SpanData: PSingleArray);
var
  i: Integer;
{$if defined(NEGATIVE_INDEX_64) }
  X: Int64;
{$else}
  X: Integer;
{$ifend}
  P: PFloatPoint;
  Segment: PLineSegment;
  fracX: TFloat;
  Points: PFloatPointArray;
  N: Integer;
begin
  (*
  ** Extract spans of coverage values for a scanline.
  **
  ** We do this by looking at the scanline segments. Each segment indicates
  ** where on the X-axis the line, that the segment was extracted from,
  ** crosses the scanline.
  **
  ** At the point where the line crosses, we update the coverage value.
  ** For example, four crossings could produce the following coverage values:
  **   [     1     -1    1   -1     ]
  ** Note that the actual coverage values will be [0..1].
  **
  ** When all segments has been processed like this, we convert the values
  ** to a sequence of values using the CumSum function:
  **   [     11111111    111111     ]
  **
  *)

  N := ScanLine.Count * 2; // Pairs of TFloatPoint, so double the count
  Points := @ScanLine.Segments[0];
  // Low/High bound of span
  Span.LowX := High(Integer);
  Span.HighX := Low(Integer);


  (*
  ** (a) set the length of the span to the horizontal range of that row.
  **
  ** (b) if a line segment goes from row Y to row Y + 1 then we need to add
  **     or subtract 1 from the (X, X + 1) indexes at the crossing (depending on
  **     line orientation).
  *)

  P := @Points[0];
  for i := 0 to N - 1 do
  begin
    // Since we know X >= 0 we could have used Trunc here but unfortunately
    // Delphi's Trunc is much slower than Round because it modifies the FPU
    // control word.
    // Note: We're using FastFloor now so the above comment is no longer relevant.
    X := PolyFloor(P.X);

    // (a1) Find the lower bound of the horizontal span
    if X < Span.LowX then
      Span.LowX := X;

    // (b) if a line segment goes from row Y to row Y + 1 then...
    if P.Y = 1 then
    begin
      fracX := P.X - X;

      if Odd(i) then
      begin // Right edge
        SpanData[X] := SpanData[X] + (1 - fracX);
        Inc(X);
        SpanData[X] := SpanData[X] + fracX;
      end else
      begin // Left edge
        SpanData[X] := SpanData[X] - (1 - fracX);
        Inc(X);
        SpanData[X] := SpanData[X] - fracX;
      end;
    end;

    // (a2) Find the upper bound of the horizontal span
    if X > Span.HighX then
      Span.HighX := X;

    inc(P);
  end;


  (*
  ** (c) compute cumulative sum of span values.
  *)
  X := Span.LowX; // Use X so NEGATIVE_INDEX_64 is handled
  Span.Values := @SpanData[X];

  CumSum(Span.Values, Span.HighX - Span.LowX + 1);


  (*
  ** (d) integrate each line segment and accumulate span buffer.
  *)
  for i := 0 to ScanLine.Count - 1 do
  begin
    Segment := @ScanLine.Segments[i];
    IntegrateSegment(Segment[0], Segment[1], SpanData);
  end;
end;

procedure AddSegment(const X1, Y1, X2, Y2: TFloat; var ScanLine: TScanLine);// {$IFDEF USEINLINING} inline; {$ENDIF}
var
  S: PLineSegment;
  Y1bin: Cardinal absolute Y1;
  Y2bin: Cardinal absolute Y2;
begin
  // Fast way of checking a Single = 0.
  //   if (Y1bin shl 1 = 0) and (Y2bin shl 1 = 0) then
  // Likely even faster:
  if ((Y1bin or Y2bin) shl 1 = 0) then
  // Original:
  //   if (Y1 = 0) and (Y2 = 0) then
    Exit;  { needed for proper clipping }

  // Add segment to the scanline's list of segments
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
  n: TFloat;
begin
  (*
  ** Split each line segment into smaller segments in a vertical buffer,
  ** such that y-values are between 0 and 1.
  *)

  Y1 := PolyFloor(P1.Y);
  Y2 := PolyFloor(P2.Y);

  // Special case for horizontal line; It just produces a single segment.
  if Y1 = Y2 then // TODO : Should also handle "Y1 almost equal Y2" ?
  begin

    AddSegment(P1.X, P1.Y - Y1, P2.X, P2.Y - Y1, ScanLines[Y1]);

  end else
  begin

    // k: Inverse slope; For each change in Y, how much does X change
    // k is expanded below to limit rounding errors.
    k := (P2.X - P1.X) / (P2.Y - P1.Y);

    // TODO : We should also special case "P1.X almost equal P2.X" ?

    if Y1 < Y2 then // Y is increasing
    begin
      X := P1.X + (Y1 + 1 - P1.Y) * { k } (P2.X - P1.X) / (P2.Y - P1.Y);

      // First fractional scanline (n..1)
      n := P1.Y - Y1;
      AddSegment(P1.X, n, X, 1, ScanLines[Y1]);

      // Whole scanlines (0..1)
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

      // Last fractional scanline (0..n)
      n := P2.Y - Y2;
      AddSegment(X, 0, P2.X, n, ScanLines[Y2]);

    end else
    begin

      X := P1.X + (Y1 - P1.Y) * { k } (P2.X - P1.X) / (P2.Y - P1.Y);

      // First fractional scanline (n..0)
      n := P1.Y - Y1;
      AddSegment(P1.X, n, X, 0, ScanLines[Y1]);

      // Whole scanlines (1..0)
      for Y := Y1 - 1 downto Y2 + 1 do
      begin
        X2 := Max(0, X - k);
        AddSegment(X, 1, X2, 0, ScanLines[Y]);
        X := X2;
      end;

      // Last fractional scanline (1..n)
      n := P2.Y - Y2;
      AddSegment(X, 1, P2.X, n, ScanLines[Y2]);

    end;

  end;
end;

procedure BuildScanLines(const Points: TArrayOfArrayOfFloatPoint;
  out ScanLines: TScanLines);
var
  PolygonIndex, MaxPolygon, MaxVertex: Integer;
  i, Y0,Y1,Y, YMin,YMax: Integer;
  SegmentCount: Integer;
  pY: PSingle;
  pPoint1, PPoint2: PFloatPoint;
  pScanLines: PScanLineArray;
begin

  (*
  ** Determine range of Y values (i.e. number of scanlines)
  *)
  YMin := MaxInt;
  YMax := -MaxInt;
  MaxPolygon := High(Points);
  for PolygonIndex := 0 to MaxPolygon do
  begin
    MaxVertex := High(Points[PolygonIndex]);
    if MaxVertex < 2 then
      Continue;

    pY := @Points[PolygonIndex][0].Y;
    for i := 0 to MaxVertex do
    begin
      Y := PolyFloor(pY^);

      if YMin > Y then
        YMin := Y;
      if YMax < Y then
        YMax := Y;

      inc(PFloatPoint(pY)); // skips X value
    end;
  end;

  if YMin > YMax then
    Exit;

  SetLength(ScanLines, YMax - YMin + 2);

  // Offset scanline pointer so we don't have to offset the Y coordinate
  pScanLines := @ScanLines[-YMin];

  (*
  ** Compute array sizes for each scanline
  *)
  // For each polygon...
  for PolygonIndex := 0 to MaxPolygon do
  begin
    MaxVertex := High(Points[PolygonIndex]);
    if MaxVertex < 2 then
      Continue; // No line segments in this polygon

    // Start with the line segment going from the last vertex to the first
    Y0 := PolyFloor(Points[PolygonIndex][MaxVertex].Y);

    pY := @Points[PolygonIndex][0].Y;
    // For each line of the polygon...
    for i := 0 to MaxVertex do
    begin
      // Calculate the max fragment count; Start of line vertex increments
      // the running fragment count for the start scanline and the end of
      // line vertex decrements the running fragment count for the end
      // scanline.
      //
      //    Polygon     Scanline                  Lines(Y0, Y1)              Count Sum
      //                       (4, 0) (0, 2) (2, 1) (1, 3) (3, 7) (7, 4)
      //
      //       *           0      1      1                                     2   2
      //      /\           1                    1      1                       2   4
      //     /  \/\        2                                                   0   4
      //    /      \       3            -1     -1             1               -1   3
      //   /       /       4                          -1             1         0   3
      //   \      /        5     -1                                           -1   2
      //    \    /         6                                                   0   2
      //     \  /          7                                                   0   2
      //      \/           8                                 -1     -1        -2   0
      //


      Y1 := PolyFloor(pY^);

      // Line has positive slope
      if Y0 <= Y1 then
      begin
        Inc(pScanLines[Y0].Count);
        Dec(pScanLines[Y1 + 1].Count);
      end
      else
      // Line has negative slope
      begin
        Inc(pScanLines[Y1].Count);
        Dec(pScanLines[Y0 + 1].Count);
      end;

      // Move to next line
      Y0 := Y1;
      inc(PFloatPoint(pY)); // skips X value
    end;
  end;

  (*
  ** Allocate memory
  *)
  SegmentCount := 0;
  for i := 0 to High(ScanLines) do
  begin
    // Adjust running fragment count
    Inc(SegmentCount, ScanLines[i].Count);

    GetMem(ScanLines[i].Segments, SegmentCount * SizeOf(TLineSegment));

    ScanLines[i].Count := 0;
    ScanLines[i].Y := YMin + i;
  end;

  (*
  ** Divide all segments of the polygon into scanline fragments
  *)
  for PolygonIndex := 0 to MaxPolygon do
  begin
    MaxVertex := High(Points[PolygonIndex]);
    if MaxVertex < 2 then
      Continue;

    // Start with the line segment going from the last vertex to the first
    pPoint1 := @Points[PolygonIndex][MaxVertex];
    PPoint2 := @Points[PolygonIndex][0];

    for i := 0 to MaxVertex do
    begin
      DivideSegment(pPoint1^, PPoint2^, pScanLines);

      // Move on to the next segment
      pPoint1 := PPoint2;
      Inc(PPoint2);
    end;
  end;
end;

procedure RenderScanline(var ScanLine: TScanLine;
  RenderProc: TRenderSpanProc; Data: Pointer; SpanData: PSingleArray; ClipX1, ClipX2: Integer);
var
  Span: TValueSpan;
{$if defined(NEGATIVE_INDEX_64) }
  X: Int64;
{$else}
  X: Integer;
{$ifend}
begin
  if ScanLine.Count = 0 then
    exit;

  ExtractSingleSpan(ScanLine, Span, SpanData);

  // Clip
  if Span.LowX < ClipX1 then
    Span.LowX := ClipX1;
  if Span.HighX > ClipX2 then
    Span.HighX := ClipX2;

  if Span.HighX < Span.LowX then
    Exit;

  RenderProc(Data, Span, ScanLine.Y);

  X := Span.LowX;
  FillLongWord(SpanData[X], Span.HighX - Span.LowX + 1, 0);
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
  CX1, CX2: Integer;
  SpanData: PSingleArray;
{$if not defined(USE_POLYFLOOR)}
  SavedRoundingMode: TRoundingMode;
{$ifend}
  SavedSSERoundingMode: TSSERoundingMode;
begin
  Len := Length(Points);
  if Len = 0 then
    Exit;

  SetLength(Poly, Len);
  for i := 0 to Len -1 do
    Poly[i] := ClipPolygon(Points[i], ClipRect);

{$if not defined(USE_POLYFLOOR)}
  SavedRoundingMode := SetRoundMode(rmDown);
  try
{$ifend}
    SavedSSERoundingMode := SetSSERoundMode(rmDown);
    try
    BuildScanLines(Poly, ScanLines);

    if (Length(ScanLines) > 0) then
    begin
      CX1 := PolyFloor(ClipRect.Left);
      CX2 := PolyCeil(ClipRect.Right) - 1;

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
      SetSSERoundMode(SavedSSERoundingMode);
    end;

{$if not defined(USE_POLYFLOOR)}
  finally
    SetRoundMode(SavedRoundingMode);
  end;
{$ifend}
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

var
  Registry: TFunctionRegistry;

procedure RegisterBindings;
begin

  Registry := NewRegistry('GR32_VPRs bindings');
  Registry.RegisterBinding(@@IntegrateSegment, 'IntegrateSergment');

  Registry[@@IntegrateSegment].Add(   @IntegrateSegment_Pas,       [isPascal]).Name := 'IntegrateSegment_Pas';
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  Registry[@@IntegrateSegment].Add(   @IntegrateSegment_SSE2,       [isSSE2]).Name := 'IntegrateSegment_SSE2';
{$ifend}

  Registry.RebindAll;
end;

initialization
  RegisterBindings;

end.
