unit GR32_Clipper;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  4.9.0                                                           *
* Date      :  9 October 2012                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2012                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
* Attributions:                                                                *
* The code in this library is an extension of Bala Vatti's clipping algorithm: *
* "A generic solution to polygon clipping"                                     *
* Communications of the ACM, Vol 35, Issue 7 (July 1992) PP 56-63.             *
* http://portal.acm.org/citation.cfm?id=129906                                 *
*                                                                              *
* Computer graphics and geometric modeling: implementation and algorithms      *
* By Max K. Agoston                                                            *
* Springer; 1 edition (January 4, 2005)                                        *
* http://books.google.com/books?q=vatti+clipping+agoston                       *
*                                                                              *
* See also:                                                                    *
* "Polygon Offsetting by Computing Winding Numbers"                            *
* Paper no. DETC2005-85513 PP. 565-575                                         *
* ASME 2005 International Design Engineering Technical Conferences             *
* and Computers and Information in Engineering Conference (IDETC/CIE2005)      *
* September 24–28, 2005 , Long Beach, California, USA                          *
* http://www.me.berkeley.edu/~mcmains/pubs/DAC05OffsetPolygon.pdf              *
*                                                                              *
*******************************************************************************)

interface

uses
  Classes, Math, GR32;

type
  PIntPoint = ^TIntPoint;
  TIntPoint = record X, Y: Int64; end;
  TIntRect = record Left, Top, Right, Bottom: Int64; end;

  TClipType = (ctIntersection, ctUnion, ctDifference, ctXor);
  TPolyType = (ptSubject, ptClip);
  // By far the most widely used winding rules for polygon filling are
  // EvenOdd & NonZero (GDI, GDI+, XLib, OpenGL, Cairo, AGG, Quartz, SVG, Gr32)
  // Others rules include Positive, Negative and ABS_GTR_EQ_TWO (only in OpenGL)
  // see http://glprogramming.com/red/chapter11.html
  // nb: Alternate is the same as EvenOdd and Winding is the same as NonZero.
  TPolyFillType = (pftEvenOdd, pftNonZero, pftPositive, pftNegative, pftAlternate = 0, pftWinding);

  // TJoinType - used by InflatePolygons()
  TJoinType = (jtSquare, jtRound, jtMiter);

  // used internally ...
  TEdgeSide = (esLeft, esRight);
  TEdgeSides = set of TEdgeSide;
  TIntersectProtect = (ipLeft, ipRight);
  TIntersectProtects = set of TIntersectProtect;
  TDirection = (dRightToLeft, dLeftToRight);
  TPolygon = array of TIntPoint;
  TPolygons = array of TPolygon;

  PEdge = ^TEdge;
  TEdge = record
    XBot : Int64;  // bottom
    YBot : Int64;
    XCurr: Int64;  // current (ie relative to bottom of current scanbeam)
    YCurr: Int64;
    XTop : Int64;  // top
    YTop : Int64;
    TmpX :  Int64;
    Dx   : Double;   // the inverse of slope
    DeltaX: Int64;
    DeltaY: Int64;
    PolyType : TPolyType;
    Side     : TEdgeSide;
    WindDelta: Integer; //  or -1 depending on winding direction
    WindCnt  : Integer;
    WindCnt2 : Integer;  // winding count of the opposite PolyType
    OutIdx   : Integer;
    Next     : PEdge;
    Prev     : PEdge;
    NextInLML: PEdge;
    PrevInAEL: PEdge;
    NextInAEL: PEdge;
    PrevInSEL: PEdge;
    NextInSEL: PEdge;
  end;

  PEdgeArray = ^TEdgeArray;
  TEdgeArray = array[0.. MaxInt div sizeof(TEdge) -1] of TEdge;

  PScanbeam = ^TScanbeam;
  TScanbeam = record
    Y   : Int64;
    Next: PScanbeam;
  end;

  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    Edge1: PEdge;
    Edge2: PEdge;
    Pt   : TIntPoint;
    Next : PIntersectNode;
  end;

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    Y         : Int64;
    LeftBound : PEdge;
    RightBound: PEdge;
    Next      : PLocalMinima;
  end;

  POutPt = ^TOutPt;

  POutRec = ^TOutRec;
  TOutRec = record
    Idx         : Integer;
    BottomPt    : POutPt;
    IsHole      : Boolean;
    FirstLeft   : POutRec;
    AppendLink  : POutRec;
    Pts         : POutPt;
    Sides       : TEdgeSides;
    BottomFlag  : POutPt;
  end;
  TArrayOfOutRec = array of POutRec;

  TOutPt = record
    Idx      : Integer;
    Pt       : TIntPoint;
    Next     : POutPt;
    Prev     : POutPt;
  end;

  PJoinRec = ^TJoinRec;
  TJoinRec = record
    Pt1a     : TIntPoint;
    Pt1b     : TIntPoint;
    Poly1Idx : Integer;
    Pt2a     : TIntPoint;
    Pt2b     : TIntPoint;
    Poly2Idx : Integer;
  end;

  PHorzRec = ^THorzRec;
  THorzRec = record
    Edge     : PEdge;
    SavedIdx : Integer;
    Next     : PHorzRec;
    Prev     : PHorzRec;
  end;

  TClipperBase = class
  private
    FEdgeList      : TList;
    FLmList        : PLocalMinima; // localMinima list
    FCurrLm        : PLocalMinima; // current localMinima node
    FUse64BitRange : Boolean;      // see LoRange and HiRange consts notes below
    procedure DisposeLocalMinimaList;
  protected
    procedure Reset; virtual;
    procedure PopLocalMinima;
    property CurrentLm: PLocalMinima read FCurrLm;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const FxdPts: TArrayOfFixedPoint; PolyType: TPolyType): Boolean; overload;
    function Add(const FxdPts: TArrayOfArrayOfFixedPoint; PolyType: TPolyType): Boolean; overload;
    function Add(const FltPts: TArrayOfFloatPoint; PolyType: TPolyType): Boolean; overload;
    function Add(const FltPts: TArrayOfArrayOfFloatPoint; PolyType: TPolyType): Boolean; overload;
    procedure Clear; virtual;
  end;

  TClipper = class(TClipperBase)
  private
    FPolyOutList   : TList;
    FJoinList      : TList;
    FClipType      : TClipType;
    FScanbeam      : PScanbeam; // scanbeam list
    FActiveEdges   : PEdge;     // active Edge list
    FSortedEdges   : PEdge;     // used for temporary sorting
    FIntersectNodes: PIntersectNode;
    FClipFillType  : TPolyFillType;
    FSubjFillType  : TPolyFillType;
    FExecuteLocked : Boolean;
    FHorizJoins    : PHorzRec;
    FReverseOutput : Boolean;
    procedure DisposeScanbeamList;
    procedure InsertScanbeam(const Y: Int64);
    function PopScanbeam: Int64;
    procedure SetWindingCount(Edge: PEdge);
    function IsEvenOddFillType(Edge: PEdge): Boolean;
    function IsEvenOddAltFillType(Edge: PEdge): Boolean;
    procedure AddEdgeToSEL(Edge: PEdge);
    procedure CopyAELToSEL;
    procedure InsertLocalMinimaIntoAEL(const BottomY: Int64);
    procedure SwapPositionsInAEL(E1, E2: PEdge);
    procedure SwapPositionsInSEL(E1, E2: PEdge);
    function IsTopHorz(const XPos: Int64): Boolean;
    procedure ProcessHorizontal(HorzEdge: PEdge);
    procedure ProcessHorizontals;
    procedure AddIntersectNode(E1, E2: PEdge; const Pt: TIntPoint);
    function ProcessIntersections(const BottomY, TopY: Int64): Boolean;
    procedure BuildIntersectList(const BottomY, TopY: Int64);
    procedure ProcessIntersectList;
    procedure DeleteFromAEL(E: PEdge);
    procedure DeleteFromSEL(E: PEdge);
    procedure IntersectEdges(E1,E2: PEdge;
      const Pt: TIntPoint; protects: TIntersectProtects = []);
    procedure DoMaxima(E: PEdge; const TopY: Int64);
    procedure UpdateEdgeIntoAEL(var E: PEdge);
    function FixupIntersections: Boolean;
    procedure SwapIntersectNodes(Int1, Int2: PIntersectNode);
    procedure ProcessEdgesAtTopOfScanbeam(const TopY: Int64);
    function IsContributing(Edge: PEdge): Boolean;
    function CreateOutRec: POutRec;
    procedure AddOutPt(E: PEdge; const Pt: TIntPoint);
    procedure AddLocalMaxPoly(E1, E2: PEdge; const Pt: TIntPoint);
    procedure AddLocalMinPoly(E1, E2: PEdge; const Pt: TIntPoint);
    procedure AppendPolygon(E1, E2: PEdge);
    procedure DisposeBottomPt(OutRec: POutRec);
    procedure DisposePolyPts(PP: POutPt);
    procedure DisposeAllPolyPts;
    procedure DisposeOutRec(Index: Integer);
    procedure DisposeIntersectNodes;
    function GetResultAsFloatPoints: TArrayOfArrayOfFloatPoint;
    function GetResultAsFixedPoints: TArrayOfArrayOfFixedPoint;
    procedure FixupOutPolygon(OutRec: POutRec);
    procedure SetHoleState(E: PEdge; OutRec: POutRec);
    procedure AddJoin(E1, E2: PEdge;
      E1OutIdx: Integer = -1; E2OutIdx: Integer = -1);
    procedure ClearJoins;
    procedure AddHorzJoin(E: PEdge; Idx: Integer);
    procedure ClearHorzJoins;
    procedure CheckHoleLinkages1(const OutRec1, OutRec2: POutRec);
    procedure CheckHoleLinkages2(const OutRec1, OutRec2: POutRec);
    procedure JoinCommonEdges(FixHoleLinkages: Boolean);
    procedure FixHoleLinkage(OutRec: POutRec);
  protected
    procedure Reset; override;
    function ExecuteInternal(FixHoleLinkages: Boolean): Boolean; virtual;
  public
    function Execute(clipType: TClipType;
      out solution: TArrayOfArrayOfFloatPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;
    function Execute(clipType: TClipType;
      out solution: TArrayOfArrayOfFixedPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    // ReverseSolution: reverses the default orientation
    property ReverseSolution: Boolean read FReverseOutput write FReverseOutput;
  end;

function Orientation(const Pts: TArrayOfFloatPoint): Boolean; overload;
function Area(const Pts: TArrayOfFloatPoint): Double;
function ReversePolygon(const Pts: TArrayOfFloatPoint): TArrayOfFloatPoint;
function ReversePolygons(const Pts: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;

// InflatePolygons precondition: outer polygons MUST be oriented clockwise,
// and inner 'hole' polygons must be oriented counter-clockwise ...
function InflatePolygons(const FltPts: TArrayOfArrayOfFloatPoint; const Delta: TFloat;
  JoinType: TJoinType = jtSquare; MiterLimit: TFloat = 2): TArrayOfArrayOfFloatPoint;

// SimplifyPolygon converts A self-intersecting polygon into A simple polygon.
function SimplifyPolygon(const poly: TArrayOfFloatPoint; FillType: TPolyFillType = pftEvenOdd): TArrayOfArrayOfFloatPoint;
function SimplifyPolygons(const polys: TArrayOfArrayOfFloatPoint; FillType: TPolyFillType = pftEvenOdd): TArrayOfArrayOfFloatPoint;

implementation

uses
  SysUtils, Types, GR32_VectorUtils;

type
  TDoublePoint = record X, Y: Double; end;
  TArrayOfDoublePoint = array of TDoublePoint;

const
  CHorizontal: Double = -3.4e+38;
  // Cross-Product (see Orientation) places the most limits on coordinate values
  // So, to avoid overflow errors, they must not exceed the following values...
  CLoRange: Int64 = $3FFFFFFF;          // 1.0e+9
  CHiRange: Int64 = $3FFFFFFFFFFFFFFF;  // 4.6e+18
  // Also, if all coordinates are within +/-LoRange, then calculations will be
  // faster. Otherwise using Int128 math will render the library ~10-15% slower.

resourcestring
  rsMissingRightbound = 'InsertLocalMinimaIntoAEL: missing RightBound';
  rsDoMaxima = 'DoMaxima error';
  rsUpdateEdgeIntoAEL = 'UpdateEdgeIntoAEL error';
  rsHorizontal = 'ProcessHorizontal error';
  rsInvalidInt = 'Coordinate exceeds range bounds';
  rsJoinError = 'Join Output polygons error';
  rsHoleLinkError = 'HoleLinkage error';

//------------------------------------------------------------------------------
// Int128 Functions ...
//------------------------------------------------------------------------------

const
  Mask32Bits = $FFFFFFFF;

  MulExp  = 10;
  MulFrac = 1 shl MulExp; //1 shl 10 = 1024
  DivFrac = 1 / MulFrac;
  ScaleExp = 16 - MulExp;
  
type
  TInt128 = record
    Lo   : Int64;
    Hi   : Int64;
  end;

{$OVERFLOWCHECKS OFF}
procedure Int128Negate(var Val: TInt128);
begin
  if Val.Lo = 0 then
  begin
    if Val.Hi = 0 then Exit;
    Val.Hi := -Val.Hi;
  end else
  begin
    Val.Lo := -Val.Lo;
    Val.Hi := not Val.Hi;
  end;
end;
//------------------------------------------------------------------------------

function Int128(const val: Int64): TInt128; overload;
begin
  Result.Lo := val;
  if val < 0 then
    Result.Hi := -1 else
    Result.Hi := 0;
end;
//------------------------------------------------------------------------------

function Int128Equal(const Int1, Int2: TInt128): Boolean;
begin
  Result := (Int1.Lo = Int2.Lo) and (Int1.Hi = Int2.Hi);
end;
//------------------------------------------------------------------------------

function Int128LessThan(const Int1, Int2: TInt128): Boolean;
begin
  if (Int1.Hi <> Int2.Hi) then
    Result := Int1.Hi < Int2.Hi else
    Result := Int1.Lo < Int2.Lo;
end;
//---------------------------------------------------------------------------

function Int128Add(const Int1, Int2: TInt128): TInt128;
begin
  Result.Lo := Int1.Lo + Int2.Lo;
  Result.Hi := Int1.Hi + Int2.Hi;
  if Int64Rec(Result.Lo).Hi < Int64Rec(Int1.Lo).Hi then inc(Result.Hi);
end;
//------------------------------------------------------------------------------

function Int128Sub(Int1, Int2: TInt128): TInt128;
begin
  Int128Negate(Int2);
  Result := Int128Add(Int1, Int2);
end;
//------------------------------------------------------------------------------

function Int128Mul(Int1, Int2: Int64): TInt128;
var
  A, B, c: Int64;
  Int1Hi, Int1Lo, Int2Hi, Int2Lo: Int64;
  Negate: Boolean;
begin
  // save the Result's sign before clearing both sign bits ...
  Negate := (Int1 < 0) <> (Int2 < 0);
  if Int1 < 0 then Int1 := -Int1;
  if Int2 < 0 then Int2 := -Int2;

  Int1Hi := Int1 shr 32;
  Int1Lo := Int1 and Mask32Bits;
  Int2Hi := Int2 shr 32;
  Int2Lo := Int2 and Mask32Bits;

  A := Int1Hi * Int2Hi;
  B := Int1Lo * Int2Lo;
  // because the high (sign) bits in both int1Hi & int2Hi have been zeroed,
  // there's no risk of 64 bit overflow in the following assignment
  //(ie: $7FFFFFFF*$FFFFFFFF + $7FFFFFFF*$FFFFFFFF < 64bits)
  c := Int1Hi*Int2Lo + Int2Hi*Int1Lo;
  // Result = A shl 64 + c shl 32 + B ...
  Result.Hi := A + (c shr 32);
  A := c shl 32;

  Result.Lo := A + B;
  if Int64Rec(Result.Lo).Hi < Int64Rec(A).Hi then inc(Result.Hi);

  if Negate then Int128Negate(Result);
end;
//------------------------------------------------------------------------------

function Int128Div(Num, Denom: TInt128): TInt128;
var
  I: Integer;
  P, P2: TInt128;
  Negate: Boolean;
begin
  if (Denom.Lo = 0) and (Denom.Hi = 0) then
    raise Exception.create('int128Div error: divide by zero');

  Negate := (Denom.Hi < 0) <> (Num.Hi < 0);
  if Num.Hi < 0 then Int128Negate(Num);
  if Denom.Hi < 0 then Int128Negate(Denom);
  if (Denom.Hi > Num.Hi) or ((Denom.Hi = Num.Hi) and (Denom.Lo > Num.Lo)) then
  begin
    Result := Int128(0); // Result is only A fraction of 1
    Exit;
  end;
  Int128Negate(Denom);

  P := int128(0);
  Result := Num;
  for I := 0 to 127 do // long division
  begin
    P.Hi := P.Hi shl 1;
    if P.Lo < 0 then inc(P.Hi);
    P.Lo := P.Lo shl 1;
    if Result.Hi < 0 then inc(P.Lo);
    Result.Hi := Result.Hi shl 1;
    if Result.Lo < 0 then inc(Result.Hi);
    Result.Lo := Result.Lo shl 1;
    P2 := P;
    P := Int128Add(P, Denom);
    if P.Hi < 0 then
      P := P2 else
      inc(Result.Lo);
  end;
  if Negate then Int128Negate(Result);
end;
//---------------------------------------------------------------------------

function Int128AsDouble(val: TInt128): Double;
const
  shift64: Double = 18446744073709551616.0;
  bit64  : Double =  9223372036854775808.0; // ie high (sign) bit of Int64
begin
  if (val.Hi < 0) then
  begin
    Int128Negate(val);
    if val.Lo < 0 then
      Result := val.Lo - bit64 - (val.Hi * shift64) else
      Result := -val.Lo - (val.Hi * shift64);
  end else
  begin
    if val.Lo < 0 then
      Result := -val.Lo + bit64 + (val.Hi * shift64) else
      Result := val.Lo + (val.Hi * shift64);
  end;
end;
//------------------------------------------------------------------------------

// procedure int128DivBase(val: TInt128; base: cardinal; out Result: TInt128; out remainder: Int64);
// var
//  I: Integer;
//  Negate: Boolean;
// begin
//  Negate := (val.Hi < 0);
//  if Negate then Int128Negate(val);
//
//  Result.Lo := 0;
//  Result.Hi := 0;
//  if (val.Hi = 0) and (val.Lo >= 0) and (base > val.Lo) then
//  begin
//    if Negate then remainder := -val.Lo else remainder := val.Lo;
//    Exit;
//  end;
//
//  remainder := 0;
//  for I := 63 downto 0 do
//  begin
//    if (val.Hi and (Int64(1) shl I)) <> 0 then
//      remainder := remainder * 2 + 1 else
//      remainder := remainder *2;
//    if remainder >= base then
//    begin
//      Result.Hi := Result.Hi + (Int64(1) shl I);
//      dec(remainder, base);
//    end;
//  end;
//  for I := 63 downto 0 do
//  begin
//    if (val.Lo and (Int64(1) shl I)) <> 0 then
//      remainder := remainder * 2 + 1 else
//      remainder := remainder *2;
//    if remainder >= base then
//    begin
//      Result.Lo := Result.Lo + (Int64(1) shl I);
//      dec(remainder, base);
//    end;
//  end;
//  if Negate then Int128Negate(Result);
// end;
//------------------------------------------------------------------------------

// function int128AsString(val: TInt128): string;
// var
//  valDiv10: TInt128;
//  R: Int64;
//  isNeg: Boolean;
// begin
//  Result := '';
//  if val.Hi < 0 then
//  begin
//    Int128Negate(val);
//    isNeg := True;
//  end else
//    isNeg := False;
//  while (val.Hi <> 0) or (val.Lo <> 0) do
//  begin
//    int128DivBase(val, 10, valDiv10, R);
//    Result := inttostr(R) + Result;
//    val := valDiv10;
//  end;
//  if Result = '' then Result := '0';
//  if isNeg then Result := '-' + Result;
// end;
{$OVERFLOWCHECKS ON}

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function FullRangeNeeded(const Pts: TPolygon): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to high(Pts) do
  begin
    if (abs(Pts[I].X) > CHiRange) or (abs(Pts[I].Y) > CHiRange) then
      raise exception.Create(rsInvalidInt)
    else if (abs(Pts[I].X) > CLoRange) or (abs(Pts[I].Y) > CLoRange) then
      Result := True;
  end;
end;
//------------------------------------------------------------------------------

function PointCount(Pts: POutPt): Integer;
var
  P: POutPt;
begin
  Result := 0;
  if not assigned(Pts) then Exit;
  P := Pts;
  repeat
    inc(Result);
    P := P.Next;
  until P = Pts;
end;
//------------------------------------------------------------------------------

function PointsEqual(const P1, P2: TIntPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;
//------------------------------------------------------------------------------

function IntPoint(const X, Y: Int64): TIntPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Orientation(const Pts: TArrayOfFloatPoint): Boolean;
var
  I, J, JPlus, JMinus, HighI: Integer;
  Vec1, Vec2: TFloatPoint;
begin
  Result := True;
  HighI := high(Pts);
  if HighI < 2 then Exit;
  J := 0;
  for I := 0 to HighI do
  begin
    if (Pts[I].Y < Pts[J].Y) then Continue;
    if ((Pts[I].Y > Pts[J].Y) or (Pts[I].X < Pts[J].X)) then J := I;
  end;
  if J = HighI then JPlus := 0
  else JPlus := J+1;
  if J = 0 then JMinus := HighI
  else JMinus := J - 1;

  // get cross product of vectors of edges adjacent the point with largest Y ...
  Vec1.X := Pts[J].X - Pts[JMinus].X;
  Vec1.Y := Pts[J].Y - Pts[JMinus].Y;
  Vec2.X := Pts[JPlus].X - Pts[J].X;
  Vec2.Y := Pts[JPlus].Y - Pts[J].Y;

  Result := ((Vec1.X * Vec2.Y) - (Vec2.X * Vec1.Y)) >= 0;
end;
//------------------------------------------------------------------------------

function Orientation(OutRec: POutRec; UseFullInt64Range: Boolean): Boolean; overload;
var
  Op, OpBottom, OpPrev, OpNext: POutPt;
  Vec1, Vec2: TIntPoint;
  Cross: TInt128;
begin
  // first make sure BottomPt is correctly assigned ...
  OpBottom := OutRec.Pts;
  Op := OpBottom.Next;
  while Op <> OutRec.Pts do
  begin
    if Op.Pt.Y >= OpBottom.Pt.Y then
    begin
      if (Op.Pt.Y > OpBottom.Pt.Y) or (Op.Pt.X < OpBottom.Pt.X) then
        OpBottom := Op;
    end;
    Op := Op.Next;
  end;
  OutRec.BottomPt := OpBottom;
  OpBottom.Idx := OutRec.Idx;
  Op := OpBottom;

  // find vertices either Side of BottomPt (skipping duplicate points) ....
  OpPrev := Op.Prev;
  while (Op <> OpPrev) and PointsEqual(Op.Pt, OpPrev.Pt) do
    OpPrev := OpPrev.Prev;
  OpNext := Op.Next;
  while (Op <> OpNext) and PointsEqual(Op.Pt, OpNext.Pt) do
    OpNext := OpNext.Next;

  Vec1.X := Op.Pt.X - OpPrev.Pt.X;
  Vec1.Y := Op.Pt.Y - OpPrev.Pt.Y;
  Vec2.X := OpNext.Pt.X - Op.Pt.X;
  Vec2.Y := OpNext.Pt.Y - Op.Pt.Y;

  // perform cross product to determine left or right 'turning' ...
  if UseFullInt64Range then
  begin
    Cross := Int128Sub(Int128Mul(Vec1.X, Vec2.Y), Int128Mul(Vec2.X, Vec1.Y));
    Result := Cross.Hi >= 0;
  end else
    Result := ((Vec1.X * Vec2.Y) - (Vec2.X * Vec1.Y)) >= 0;

end;
//------------------------------------------------------------------------------

function Area(const Pts: TArrayOfFloatPoint): Double; overload;
var
  I, HighI: Integer;
  D: Double;
begin
  Result := 0;
  HighI := high(Pts);
  if HighI < 2 then Exit;
  D := Pts[HighI].X * Pts[0].Y - Pts[0].X * Pts[HighI].Y;
  for I := 1 to HighI do
    D := D + (Pts[I - 1].X * Pts[I].Y) - (Pts[I].X * Pts[I - 1].Y);
  Result := D / 2;
end;
//------------------------------------------------------------------------------

function Area(OutRec: POutRec; UseFullInt64Range: Boolean): Double; overload;
var
  Op: POutPt;
  D: Double;
  A: TInt128;
begin
  Op := OutRec.Pts;
  if UseFullInt64Range then
  begin
    A := Int128(0);
    repeat
      A := Int128Add(A, Int128Sub(
          Int128Mul(Op.Pt.X, Op.Next.Pt.Y), Int128Mul(Op.Next.Pt.X, Op.Pt.Y)));
      Op := Op.Next;
    until Op = OutRec.Pts;
    Result := Int128AsDouble(A) / 2;
  end else
  begin
    D := 0;
    repeat
      D := D + (Op.Pt.X * Op.Next.Pt.Y) - (Op.Next.Pt.X * Op.Pt.Y);
      Op := Op.Next;
    until Op = OutRec.Pts;
    Result := D / 2;
  end;
end;
//------------------------------------------------------------------------------

function ReversePolygon(const Pts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  I, HighI: Integer;
begin
  HighI := high(Pts);
  SetLength(Result, HighI +1);
  for I := 0 to HighI do
    Result[I] := Pts[HighI - I];
end;
//------------------------------------------------------------------------------

function ReversePolygons(const Pts: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
var
  I, J, highJ: Integer;
begin
  I := length(Pts);
  SetLength(Result, I);
  for I := 0 to I - 1 do
  begin
    highJ := high(Pts[I]);
    SetLength(Result[I], highJ + 1);
    for J := 0 to highJ do
      Result[I][J] := Pts[I][highJ - J];
  end;
end;
//------------------------------------------------------------------------------

function PointIsVertex(const Pt: TIntPoint; PP: POutPt): Boolean;
var
  Pp2: POutPt;
begin
  Result := True;
  Pp2 := PP;
  repeat
    if PointsEqual(Pp2.Pt, Pt) then Exit;
    Pp2 := Pp2.Next;
  until Pp2 = PP;
  Result := False;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const Pt: TIntPoint;
  PP: POutPt; UseFullInt64Range: Boolean): Boolean;
var
  Pp2: POutPt;
  A, B: TInt128;
begin
  Result := False;
  Pp2 := PP;
  if UseFullInt64Range then
  begin
    repeat
      if (((Pp2.Pt.Y <= Pt.Y) and (Pt.Y < Pp2.Prev.Pt.Y)) or
        ((Pp2.Prev.Pt.Y <= Pt.Y) and (Pt.Y < Pp2.Pt.Y))) then
      begin
        A := Int128(Pt.X - Pp2.Pt.X);
        B := Int128Div( Int128Mul(Pp2.Prev.Pt.X - Pp2.Pt.X,
          Pt.Y - Pp2.Pt.Y), Int128(Pp2.Prev.Pt.Y - Pp2.Pt.Y) );
        if Int128LessThan(A, B) then Result := not Result;
      end;
      Pp2 := Pp2.Next;
    until Pp2 = PP;
  end else
  begin
    repeat
      if ((((Pp2.Pt.Y <= Pt.Y) and (Pt.Y < Pp2.Prev.Pt.Y)) or
        ((Pp2.Prev.Pt.Y <= Pt.Y) and (Pt.Y < Pp2.Pt.Y))) and
        (Pt.X < (Pp2.Prev.Pt.X - Pp2.Pt.X) * (Pt.Y - Pp2.Pt.Y) /
        (Pp2.Prev.Pt.Y - Pp2.Pt.Y) + Pp2.Pt.X)) then Result := not Result;
      Pp2 := Pp2.Next;
    until Pp2 = PP;
  end;
end;
//------------------------------------------------------------------------------

function SlopesEqual(E1, E2: PEdge;
  UseFullInt64Range: Boolean): Boolean; overload;
begin
  if UseFullInt64Range then
    Result := Int128Equal(Int128Mul(E1.DeltaY, E2.DeltaX),
      Int128Mul(E1.DeltaX, E2.DeltaY))
  else
    Result := E1.DeltaY * E2.DeltaX = E1.DeltaX * E2.DeltaY;
end;
//---------------------------------------------------------------------------

function SlopesEqual(const Pt1, Pt2, Pt3: TIntPoint;
  UseFullInt64Range: Boolean): Boolean; overload;
begin
  if UseFullInt64Range then
    Result := Int128Equal(
      Int128Mul(Pt1.Y-Pt2.Y, Pt2.X-Pt3.X), Int128Mul(Pt1.X-Pt2.X, Pt2.Y-Pt3.Y))
  else
    Result := (Pt1.Y-Pt2.Y)*(Pt2.X-Pt3.X) = (Pt1.X-Pt2.X)*(Pt2.Y-Pt3.Y);
end;
//---------------------------------------------------------------------------

function SlopesEqual(const Pt1, Pt2, Pt3, Pt4: TIntPoint;
  UseFullInt64Range: Boolean): Boolean; overload;
begin
  if UseFullInt64Range then
    Result := Int128Equal( Int128Mul(Pt1.Y-Pt2.Y, Pt3.X-Pt4.X),
      Int128Mul(Pt1.X-Pt2.X, Pt3.Y-Pt4.Y))
  else
    Result := (Pt1.Y-Pt2.Y)*(Pt3.X-Pt4.X) = (Pt1.X-Pt2.X)*(Pt3.Y-Pt4.Y);
end;
//---------------------------------------------------------------------------

//                 0(90º)                                                  //
//                 |                                                       //
// +inf (180º) --- o --- -inf (0º)                                         //
function GetDx(const Pt1, Pt2: TIntPoint): Double;
begin
  if (Pt1.Y = Pt2.Y) then Result := CHorizontal
  else Result := (Pt2.X - Pt1.X)/(Pt2.Y - Pt1.Y);
end;
//---------------------------------------------------------------------------

procedure SetDx(E: PEdge);
begin
  E.DeltaX := (E.XTop - E.XBot);
  E.DeltaY := (E.YTop - E.YBot);
  if E.DeltaY = 0 then E.Dx := CHorizontal
  else E.Dx := E.DeltaX/E.DeltaY;
end;
//---------------------------------------------------------------------------

procedure SwapSides(Edge1, Edge2: PEdge);
var
  Side: TEdgeSide;
begin
  Side :=  Edge1.Side;
  Edge1.Side := Edge2.Side;
  Edge2.Side := Side;
end;
//------------------------------------------------------------------------------

procedure SwapPolyIndexes(Edge1, Edge2: PEdge);
var
  OutIdx: Integer;
begin
  OutIdx :=  Edge1.OutIdx;
  Edge1.OutIdx := Edge2.OutIdx;
  Edge2.OutIdx := OutIdx;
end;
//------------------------------------------------------------------------------

function TopX(Edge: PEdge; const currentY: Int64): Int64; overload;
begin
  if currentY = Edge.YTop then Result := Edge.XTop
  else if Edge.XTop = Edge.XBot then Result := Edge.XBot
  else Result := Edge.XBot + round(Edge.Dx*(currentY - Edge.YBot));
end;
//------------------------------------------------------------------------------

function IntersectPoint(Edge1, Edge2: PEdge;
  out ip: TIntPoint; UseFullInt64Range: Boolean): Boolean; overload;
var
  B1,B2: Double;
begin
  if SlopesEqual(Edge1, Edge2, UseFullInt64Range) then
  begin
    Result := False;
    Exit;
  end;
  if Edge1.Dx = 0 then
  begin
    ip.X := Edge1.XBot;
    if Edge2.Dx = CHorizontal then
      ip.Y := Edge2.YBot
    else
    begin
      with Edge2^ do B2 := YBot - (XBot/Dx);
      ip.Y := round(ip.X/Edge2.Dx + B2);
    end;
  end
  else if Edge2.Dx = 0 then
  begin
    ip.X := Edge2.XBot;
    if Edge1.Dx = CHorizontal then
      ip.Y := Edge1.YBot
    else
    begin
      with Edge1^ do B1 := YBot - (XBot/Dx);
      ip.Y := round(ip.X/Edge1.Dx + B1);
    end;
  end else
  begin
    with Edge1^ do B1 := XBot - YBot * Dx;
    with Edge2^ do B2 := XBot - YBot * Dx;
    B2 := (B2-B1)/(Edge1.Dx - Edge2.Dx);
    ip.Y := round(B2);
    ip.X := round(Edge1.Dx * B2 + B1);
  end;

  // The precondition - E.TmpX > eNext.TmpX - indicates that the two edges do
  // intersect below TopY (and hence below the tops of either Edge). However,
  // when edges are almost parallel, rounding errors may cause False positives -
  // indicating intersections when there really aren't any. Also, floating point
  // imprecision can incorrectly place an intersect point beyond/above an Edge.
  // Therfore, further validation of the IP is warranted ...
  if (ip.Y < Edge1.YTop) or (ip.Y < Edge2.YTop) then
  begin
    // Find the lower top of the two edges and compare X's at this Y.
    // If Edge1's X is greater than Edge2's X then it's fair to assume an
    // intersection really has occurred...
    if (Edge1.YTop > Edge2.YTop) then
    begin
      Result := TopX(Edge2, Edge1.YTop) < Edge1.XTop;
      ip.X := Edge1.XTop;
      ip.Y := Edge1.YTop;
    end else
    begin
      Result := TopX(Edge1, Edge2.YTop) > Edge2.XTop;
      ip.X := Edge2.XTop;
      ip.Y := Edge2.YTop;
    end;
  end else
    Result := True;
end;
//------------------------------------------------------------------------------

procedure ReversePolyPtLinks(PP: POutPt);
var
  Pp1,Pp2: POutPt;
begin
  Pp1 := PP;
  repeat
    Pp2:= Pp1.Next;
    Pp1.Next := Pp1.Prev;
    Pp1.Prev := Pp2;
    Pp1 := Pp2;
  until Pp1 = PP;
end;
//------------------------------------------------------------------------------

function FixedPointToIntPoint(const FxPt: TFixedPoint): TIntPoint;
begin
  Result.X := FxPt.X shr ScaleExp;
  Result.Y := FxPt.Y shr ScaleExp;
end;
//------------------------------------------------------------------------------

function IntPointToFixedPoint(const IntPt: TIntPoint): TFixedPoint;
begin
  Result.X := IntPt.X shl ScaleExp;
  Result.Y := IntPt.Y shl ScaleExp;
end;
//------------------------------------------------------------------------------

function FloatPointToIntPoint(const FltPt: TFloatPoint): TIntPoint;
begin
  Result.X := Round(FltPt.X * MulFrac);
  Result.Y := Round(FltPt.Y * MulFrac);
end;
//------------------------------------------------------------------------------

function IntPointToFloatPoint(const IntPt: TIntPoint): TFloatPoint;
begin
  Result.X := IntPt.X * DivFrac;
  Result.Y := IntPt.Y * DivFrac;
end;

//------------------------------------------------------------------------------
// TClipperBase methods ...
//------------------------------------------------------------------------------

constructor TClipperBase.Create;
begin
  FEdgeList := TList.Create;
  FLmList := nil;
  FCurrLm := nil;
  FUse64BitRange := False; // ie default is False
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  FEdgeList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TClipperBase.Add(const FltPts: TArrayOfFloatPoint; PolyType: TPolyType): Boolean;

  //----------------------------------------------------------------------

  procedure InitEdge(E, eNext, ePrev: PEdge; const Pt: TIntPoint);
  begin
    fillChar(E^, sizeof(TEdge), 0);
    E.Next := eNext;
    E.Prev := ePrev;
    E.XCurr := Pt.X;
    E.YCurr := Pt.Y;
    if E.YCurr >= E.Next.YCurr then
    begin
      E.XBot := E.XCurr;
      E.YBot := E.YCurr;
      E.XTop := E.Next.XCurr;
      E.YTop := E.Next.YCurr;
      E.WindDelta := 1;
    end else
    begin
      E.XTop := E.XCurr;
      E.YTop := E.YCurr;
      E.XBot := E.Next.XCurr;
      E.YBot := E.Next.YCurr;
      E.WindDelta := -1;
    end;
    SetDx(E);
    E.PolyType := PolyType;
    E.OutIdx := -1;
  end;
  //----------------------------------------------------------------------

  procedure SwapX(E: PEdge);
  begin
    // swap horizontal edges' top and bottom x's so they follow the natural
    // progression of the bounds - ie so their xbots will align with the
    // adjoining lower Edge. [Helpful in the ProcessHorizontal() method.]
    E.XCurr := E.XTop;
    E.XTop := E.XBot;
    E.XBot := E.XCurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertLocalMinima(lm: PLocalMinima);
  var
    TmpLm: PLocalMinima;
  begin
    if not assigned(fLmList) then
    begin
      FLmList := lm;
    end
    else if (lm.Y >= FLmList.Y) then
    begin
      lm.Next := FLmList;
      FLmList := lm;
    end else
    begin
      TmpLm := FLmList;
      while assigned(TmpLm.Next) and (lm.Y < TmpLm.Next.Y) do
          TmpLm := TmpLm.Next;
      lm.Next := TmpLm.Next;
      TmpLm.Next := lm;
    end;
  end;
  //----------------------------------------------------------------------

  function AddBoundsToLML(E: PEdge): PEdge;
  var
    NewLm: PLocalMinima;
  begin
    // Starting at the top of one bound we progress to the bottom where there's
    // A local minima. We then go to the top of the Next bound. These two bounds
    // form the left and right (or right and left) bounds of the local minima.
    E.NextInLML := nil;
    E := E.Next;
    repeat
      if E.Dx = CHorizontal then
      begin
        // nb: proceed through horizontals when approaching from their right,
        //    but break on horizontal minima if approaching from their left.
        //    This ensures 'local minima' are always on the left of horizontals.
        if (E.Next.YTop < E.YTop) and (E.Next.XBot > E.Prev.XBot) then break;
        if (E.XTop <> E.Prev.XBot) then SwapX(E);
        // E.WindDelta := 0; safe option to consider when redesigning
        E.NextInLML := E.Prev;
      end
      else if (E.YBot = E.Prev.YBot) then break
      else E.NextInLML := E.Prev;
      E := E.Next;
    until False;

    // E and E.Prev are now at A local minima ...
    new(NewLm);
    NewLm.Y := E.Prev.YBot;
    NewLm.Next := nil;
    if E.Dx = CHorizontal then // Horizontal edges never start A left bound
    begin
      if (E.XBot <> E.Prev.XBot) then SwapX(E);
      NewLm.LeftBound := E.Prev;
      NewLm.RightBound := E;
    end else if (E.Dx < E.Prev.Dx) then
    begin
      NewLm.LeftBound := E.Prev;
      NewLm.RightBound := E;
    end else
    begin
      NewLm.LeftBound := E;
      NewLm.RightBound := E.Prev;
    end;
    NewLm.LeftBound.Side := esLeft;
    NewLm.RightBound.Side := esRight;

    InsertLocalMinima(NewLm);
    // now process the ascending bound ....
    repeat
      if (E.Next.YTop = E.YTop) and not (E.Next.Dx = CHorizontal) then break;
      E.NextInLML := E.Next;
      E := E.Next;
      if (E.Dx = CHorizontal) and (E.XBot <> E.Prev.XTop) then SwapX(E);
    until False;
    Result := E.Next;
  end;
  //----------------------------------------------------------------------

var
  I, J, len: Integer;
  Edges: PEdgeArray;
  E, EHighest: PEdge;
  Pg: TPolygon;
  MaxVal: Int64;
  IntPtI: TIntPoint;
begin
  Result := False; // ie assume nothing added
  len := length(FltPts);
  if len < 3 then Exit;
  setlength(Pg, len);
  Pg[0] := FloatPointToIntPoint(FltPts[0]);
  J := 0;
  //1. check that coordinate values are within the valid range, and
  //2. remove duplicate points and co-linear points
  if FUse64BitRange then MaxVal := CHiRange else MaxVal := CLoRange;
  for I := 1 to len - 1 do
  begin
    IntPtI := FloatPointToIntPoint(FltPts[I]);
    if ((abs(FltPts[I].X) > MaxVal) or (abs(FltPts[I].Y) > MaxVal)) then
    begin
      if ((abs(FltPts[I].X) > CHiRange) or (abs(FltPts[I].Y) > CHiRange)) then
        raise exception.Create(rsInvalidInt);
      MaxVal := CHiRange;
      FUse64BitRange := True;
    end;
    if PointsEqual(Pg[J], IntPtI) then Continue
    else if (J > 0) and SlopesEqual(Pg[J - 1], Pg[J], IntPtI, FUse64BitRange) then
    begin
      if PointsEqual(Pg[J - 1], IntPtI) then dec(J);
    end else inc(J);
    Pg[J] := IntPtI;
  end;
  if (J < 2) then Exit;

  // now remove duplicate points and co-linear edges at the loop around of the
  // start and end coordinates ...
  len := J+1;
  while len > 2 do
  begin
    // nb: test for point equality before testing slopes ...
    if PointsEqual(Pg[J], Pg[0]) then dec(J)
    else if PointsEqual(Pg[0], Pg[1]) or
      SlopesEqual(Pg[J], Pg[0], Pg[1], FUse64BitRange) then
    begin
      Pg[0] := Pg[J];
      dec(J);
    end
    else if SlopesEqual(Pg[J - 1], Pg[J], Pg[0], FUse64BitRange) then dec(J)
    else if SlopesEqual(Pg[0], Pg[1], Pg[2], FUse64BitRange) then
    begin
      for I := 2 to J do Pg[I - 1] := Pg[I];
      dec(J);
    end
    else
      break;
    dec(len);
  end;
  if len < 3 then Exit;
  Result := True;

  GetMem(Edges, sizeof(TEdge)*len);
  FEdgeList.Add(Edges);

  // convert vertices to A Double-linked-list of edges and initialize ...
  Edges[0].XCurr := Pg[0].X;
  Edges[0].YCurr := Pg[0].Y;
  InitEdge(@Edges[len - 1], @Edges[0], @Edges[len - 2], Pg[len - 1]);
  for I := len-2 downto 1 do
    InitEdge(@Edges[I], @Edges[I+1], @Edges[I - 1], Pg[I]);
  InitEdge(@Edges[0], @Edges[1], @Edges[len - 1], Pg[0]);
  // reset XCurr & YCurr and find the 'highest' Edge. (nb: since I'm much more
  // familiar with positive downwards Y axes, 'highest' here will be the Edge
  // with the *smallest* YTop.)
  E := @Edges[0];
  EHighest := E;
  repeat
    E.XCurr := E.XBot;
    E.YCurr := E.YBot;
    if E.YTop < EHighest.YTop then EHighest := E;
    E := E.Next;
  until E = @Edges[0];

  // make sure eHighest is positioned so the following loop works safely ...
  if EHighest.WindDelta > 0 then EHighest := EHighest.Next;
  if (EHighest.Dx = CHorizontal) then EHighest := EHighest.Next;

  // finally insert each local minima ...
  E := EHighest;
  repeat
    E := AddBoundsToLML(E);
  until (E = EHighest);
end;
//------------------------------------------------------------------------------

function TClipperBase.Add(const FltPts: TArrayOfArrayOfFloatPoint; PolyType: TPolyType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to high(FltPts) do
    if Add(FltPts[I], PolyType) then Result := True;
end;
//------------------------------------------------------------------------------

function TClipperBase.Add(const FxdPts: TArrayOfFixedPoint; PolyType: TPolyType): Boolean;
begin
  Result := Add(FixedPointToFloatPoint(FxdPts), PolyType);
end;
//------------------------------------------------------------------------------

function TClipperBase.Add(const FxdPts: TArrayOfArrayOfFixedPoint; PolyType: TPolyType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to high(FxdPts) do
    if Add(FxdPts[I], PolyType) then Result := True;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
var
  I: Integer;
begin
  DisposeLocalMinimaList;
  for I := 0 to FEdgeList.Count - 1 do dispose(PEdgeArray(fEdgeList[I]));
  FEdgeList.Clear;
  FUse64BitRange := False;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Reset;
var
  E: PEdge;
  Lm: PLocalMinima;
begin
  // Reset() allows various clipping operations to be executed
  // multiple times on the same polygon sets.

  FCurrLm := FLmList;
  // reset all edges ...
  Lm := FCurrLm;
  while assigned(Lm) do
  begin
    E := Lm.LeftBound;
    while assigned(E) do
    begin
      E.XCurr := E.XBot;
      E.YCurr := E.YBot;
      E.Side := esLeft;
      E.OutIdx := -1;
      E := E.NextInLML;
    end;
    E := Lm.RightBound;
    while assigned(E) do
    begin
      E.XCurr := E.XBot;
      E.YCurr := E.YBot;
      E.Side := esRight;
      E.OutIdx := -1;
      E := E.NextInLML;
    end;
    Lm := Lm.Next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeLocalMinimaList;
begin
  while assigned(fLmList) do
  begin
    FCurrLm := FLmList.Next;
    Dispose(fLmList);
    FLmList := FCurrLm;
  end;
  FCurrLm := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.PopLocalMinima;
begin
  if not assigned(fCurrLM) then Exit;
  FCurrLM := FCurrLM.Next;
end;

//------------------------------------------------------------------------------
// TClipper methods ...
//------------------------------------------------------------------------------

constructor TClipper.Create;
begin
  inherited Create;
  FJoinList := TList.Create;
  FPolyOutList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipper.Destroy;
begin
  inherited; // this must be first since inherited Destroy calls Clear.
  DisposeScanbeamList;
  FJoinList.Free;
  FPolyOutList.Free;
end;
//------------------------------------------------------------------------------

procedure TClipper.Clear;
begin
  DisposeAllPolyPts;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeScanbeamList;
var
  SB: PScanbeam;
begin
  while assigned(fScanbeam) do
  begin
    SB := FScanbeam.Next;
    Dispose(fScanbeam);
    FScanbeam := SB;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  Lm: PLocalMinima;
begin
  inherited Reset;
  FScanbeam := nil;
  DisposeAllPolyPts;
  Lm := FLmList;
  while assigned(Lm) do
  begin
    InsertScanbeam(Lm.Y);
    InsertScanbeam(Lm.LeftBound.YTop);
    Lm := Lm.Next;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  out solution: TArrayOfArrayOfFloatPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): Boolean;
begin
  Result := False;
  solution := nil;
  if FExecuteLocked then Exit;
  try
    FExecuteLocked := True;
    FSubjFillType := subjFillType;
    FClipFillType := clipFillType;
    FClipType := clipType;
    Result := ExecuteInternal(False);
    if Result then solution := GetResultAsFloatPoints;
  finally
    FExecuteLocked := False;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  out solution: TArrayOfArrayOfFixedPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): Boolean;
begin
  Result := False;
  solution := nil;
  if FExecuteLocked then Exit;
  try
    FExecuteLocked := True;
    FSubjFillType := subjFillType;
    FClipFillType := clipFillType;
    FClipType := clipType;
    Result := ExecuteInternal(False);
    if Result then solution := GetResultAsFixedPoints;
  finally
    FExecuteLocked := False;
  end;
end;
//------------------------------------------------------------------------------

function PolySort(item1, item2: pointer): Integer;
var
  P1, P2: POutRec;
  Idx1, Idx2: Integer;
begin
  Result := 0;
  if item1 = item2 then Exit;
  P1 := item1; P2 := item2;
  if not assigned(P1.Pts) or not assigned(P2.Pts) then
  begin
    if assigned(P1.Pts) then Result := -1
    else if assigned(P2.Pts) then Result := 1;
    Exit;
  end;
  if P1.IsHole then Idx1 := P1.FirstLeft.Idx
  else Idx1 := P1.Idx;
  if P2.IsHole then Idx2 := P2.FirstLeft.Idx
  else Idx2 := P2.Idx;
  Result := Idx1 - Idx2;
  if (Result = 0) and (P1.IsHole <> P2.IsHole) then
  begin
    if P1.IsHole then Result := 1
    else Result := -1;
  end;
end;
//------------------------------------------------------------------------------

function FindAppendLinkEnd(OutRec: POutRec): POutRec;
begin
  while assigned(OutRec.AppendLink) do
    OutRec := OutRec.AppendLink;
  Result := OutRec;
end;
//------------------------------------------------------------------------------

procedure TClipper.FixHoleLinkage(OutRec: POutRec);
var
  Tmp: POutRec;
begin
  if assigned(OutRec.BottomPt) then
    Tmp := POutRec(fPolyOutList[OutRec.BottomPt.Idx]).FirstLeft else
    Tmp := OutRec.FirstLeft;
    if (OutRec = Tmp) then
      raise exception.Create(rsHoleLinkError);

  if assigned(Tmp) then
  begin
    if assigned(Tmp.AppendLink) then
      Tmp := FindAppendLinkEnd(Tmp);
    if Tmp = OutRec then Tmp := nil
    else if Tmp.IsHole then
    begin
      FixHoleLinkage(Tmp);
      Tmp := Tmp.FirstLeft;
    end;
  end;
  OutRec.FirstLeft := Tmp;
  if not assigned(Tmp) then OutRec.IsHole := False;
  OutRec.AppendLink := nil;
end;
//------------------------------------------------------------------------------

function TClipper.ExecuteInternal(FixHoleLinkages: Boolean): Boolean;
var
  I: Integer;
  OutRec: POutRec;
  BottomY, TopY: Int64;
begin
  Result := False;
  try try
    Reset;
    if not assigned(fScanbeam) then
    begin
      Result := True;
      Exit;
    end;

    BottomY := PopScanbeam;
    repeat
      InsertLocalMinimaIntoAEL(BottomY);
      ClearHorzJoins;
      ProcessHorizontals;
      TopY := PopScanbeam;
      if not ProcessIntersections(BottomY, TopY) then Exit;
      ProcessEdgesAtTopOfScanbeam(TopY);
      BottomY := TopY;
    until FScanbeam = nil;

    // tidy up output polygons and fix orientations where necessary ...
    for I := 0 to FPolyOutList.Count - 1 do
    begin
      OutRec := FPolyOutList[I];
      if not assigned(OutRec.Pts) then Continue;
      FixupOutPolygon(OutRec);
      if not assigned(OutRec.Pts) then Continue;

      if OutRec.IsHole and FixHoleLinkages then
        FixHoleLinkage(OutRec);
      // OutRec.BottomPt might've been cleaned up already so retest orientation
      if (OutRec.BottomPt = OutRec.BottomFlag) and
        (Orientation(OutRec, FUse64BitRange) <> (Area(OutRec, FUse64BitRange) > 0)) then
          DisposeBottomPt(OutRec);
      if (OutRec.IsHole = FReverseOutput) xor Orientation(OutRec, FUse64BitRange) then
          ReversePolyPtLinks(OutRec.Pts);
    end;
    if FJoinList.count > 0 then
      JoinCommonEdges(FixHoleLinkages);

    if FixHoleLinkages then FPolyOutList.Sort(PolySort);
    Result := True;
  except
    Result := False;
  end;
  finally
    ClearJoins;
    ClearHorzJoins;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertScanbeam(const Y: Int64);
var
  Sb, Sb2: PScanbeam;
begin
  new(Sb);
  Sb.Y := Y;
  if not assigned(fScanbeam) then
  begin
    FScanbeam := Sb;
    Sb.Next := nil;
  end else if Y > FScanbeam.Y then
  begin
    Sb.Next := FScanbeam;
    FScanbeam := Sb;
  end else
  begin
    Sb2 := FScanbeam;
    while assigned(Sb2.Next) and (Y <= Sb2.Next.Y) do Sb2 := Sb2.Next;
    if Y <> Sb2.Y then
    begin
      Sb.Next := Sb2.Next;
      Sb2.Next := Sb;
    end
    else dispose(Sb); // ie ignores duplicates
  end;
end;
//------------------------------------------------------------------------------

function TClipper.PopScanbeam: Int64;
var
  Sb: PScanbeam;
begin
  Result := FScanbeam.Y;
  Sb := FScanbeam;
  FScanbeam := FScanbeam.Next;
  dispose(Sb);
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeBottomPt(OutRec: POutRec);
var
  Next, Prev: POutPt;
begin
  Next := OutRec.BottomPt.Next;
  Prev := OutRec.BottomPt.Prev;
  if OutRec.Pts = OutRec.BottomPt then
    OutRec.Pts := Next;
  dispose(OutRec.BottomPt);
  Next.Prev := Prev;
  Prev.Next := Next;
  OutRec.BottomPt := Next;
  FixupOutPolygon(OutRec);
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposePolyPts(PP: POutPt);
var
  TmpPp: POutPt;
begin
  PP.Prev.Next := nil;
  while assigned(PP) do
  begin
    TmpPp := PP;
    PP := PP.Next;
    dispose(TmpPp);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeAllPolyPts;
var
  I: Integer;
begin
  for I := 0 to FPolyOutList.Count - 1 do DisposeOutRec(I);
  FPolyOutList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(Index: Integer);
var
  OutRec: POutRec;
begin
  OutRec := FPolyOutList[Index];
  if assigned(OutRec.Pts) then DisposePolyPts(OutRec.Pts);
  Dispose(OutRec);
  FPolyOutList[Index] := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindingCount(Edge: PEdge);
var
  E: PEdge;
begin
  E := Edge.PrevInAEL;
  // find the Edge of the same PolyType that immediately preceeds 'Edge' in AEL
  while assigned(E) and (E.PolyType <> Edge.PolyType) do E := E.PrevInAEL;
  if not assigned(E) then
  begin
    Edge.WindCnt := Edge.WindDelta;
    Edge.WindCnt2 := 0;
    E := FActiveEdges; // ie get ready to calc WindCnt2
  end else if IsEvenOddFillType(Edge) then
  begin
    // even-odd filling ...
    Edge.WindCnt := 1;
    Edge.WindCnt2 := E.WindCnt2;
    E := E.NextInAEL; // ie get ready to calc WindCnt2
  end else
  begin
    // NonZero, Positive, or Negative filling ...
    if E.WindCnt * E.WindDelta < 0 then
    begin
      if (abs(E.WindCnt) > 1) then
      begin
        if (E.WindDelta * Edge.WindDelta < 0) then Edge.WindCnt := E.WindCnt
        else Edge.WindCnt := E.WindCnt + Edge.WindDelta;
      end else
        Edge.WindCnt := E.WindCnt + E.WindDelta + Edge.WindDelta;
    end else
    begin
      if (abs(E.WindCnt) > 1) and (E.WindDelta * Edge.WindDelta < 0) then
        Edge.WindCnt := E.WindCnt
      else if E.WindCnt + Edge.WindDelta = 0 then
        Edge.WindCnt := E.WindCnt
      else Edge.WindCnt := E.WindCnt + Edge.WindDelta;
    end;
    Edge.WindCnt2 := E.WindCnt2;
    E := E.NextInAEL; // ie get ready to calc WindCnt2
  end;

  // update WindCnt2 ...
  if IsEvenOddAltFillType(Edge) then
  begin
    // even-odd filling ...
    while (E <> Edge) do
    begin
      if Edge.WindCnt2 = 0 then Edge.WindCnt2 := 1 else Edge.WindCnt2 := 0;
      E := E.NextInAEL;
    end;
  end else
  begin
    // NonZero, Positive, or Negative filling ...
    while (E <> Edge) do
    begin
      inc(Edge.WindCnt2, E.WindDelta);
      E := E.NextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddFillType(Edge: PEdge): Boolean;
begin
  if Edge.PolyType = ptSubject then
    Result := FSubjFillType = pftEvenOdd else
    Result := FClipFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddAltFillType(Edge: PEdge): Boolean;
begin
  if Edge.PolyType = ptSubject then
    Result := FClipFillType = pftEvenOdd else
    Result := FSubjFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributing(Edge: PEdge): Boolean;
var
  Pft, Pft2: TPolyFillType;
begin
  if Edge.PolyType = ptSubject then
  begin
    Pft := FSubjFillType;
    Pft2 := FClipFillType;
  end else
  begin
    Pft := FClipFillType;
    Pft2 := FSubjFillType
  end;
  case Pft of
    pftEvenOdd, pftNonZero: Result := abs(Edge.WindCnt) = 1;
    pftPositive: Result := (Edge.WindCnt = 1);
    else Result := (Edge.WindCnt = -1);
  end;
  if not Result then Exit;

  case FClipType of
    ctIntersection:
      case Pft2 of
        pftEvenOdd, pftNonZero: Result := (Edge.WindCnt2 <> 0);
        pftPositive: Result := (Edge.WindCnt2 > 0);
        pftNegative: Result := (Edge.WindCnt2 < 0);
      end;
    ctUnion:
      case Pft2 of
        pftEvenOdd, pftNonZero: Result := (Edge.WindCnt2 = 0);
        pftPositive: Result := (Edge.WindCnt2 <= 0);
        pftNegative: Result := (Edge.WindCnt2 >= 0);
      end;
    ctDifference:
      if Edge.PolyType = ptSubject then
        case Pft2 of
          pftEvenOdd, pftNonZero: Result := (Edge.WindCnt2 = 0);
          pftPositive: Result := (Edge.WindCnt2 <= 0);
          pftNegative: Result := (Edge.WindCnt2 >= 0);
        end
      else
        case Pft2 of
          pftEvenOdd, pftNonZero: Result := (Edge.WindCnt2 <> 0);
          pftPositive: Result := (Edge.WindCnt2 > 0);
          pftNegative: Result := (Edge.WindCnt2 < 0);
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(E1, E2: PEdge; const Pt: TIntPoint);
var
  E, prevE: PEdge;
begin
  if (E2.Dx = CHorizontal) or (E1.Dx > E2.Dx) then
  begin
    AddOutPt(E1, Pt);
    E2.OutIdx := E1.OutIdx;
    E1.Side := esLeft;
    E2.Side := esRight;
    E := E1;
    if E.PrevInAEL = E2 then
      prevE := E2.PrevInAEL
    else
      prevE := E.PrevInAEL;
  end else
  begin
    AddOutPt(E2, Pt);
    E1.OutIdx := E2.OutIdx;
    E1.Side := esRight;
    E2.Side := esLeft;
    E := E2;
    if E.PrevInAEL = E1 then
      prevE := E1.PrevInAEL
    else
      prevE := E.PrevInAEL;
  end;

  if assigned(prevE) and (prevE.OutIdx >= 0) and
    (TopX(prevE, Pt.Y) = TopX(E, Pt.Y)) and
     SlopesEqual(E, prevE, FUse64BitRange) then
       AddJoin(E, prevE);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMaxPoly(E1, E2: PEdge; const Pt: TIntPoint);
begin
  AddOutPt(E1, Pt);
  if (E1.OutIdx = E2.OutIdx) then
  begin
    E1.OutIdx := -1;
    E2.OutIdx := -1;
  end
  else if E1.OutIdx < E2.OutIdx then
    AppendPolygon(E1, E2)
  else
    AppendPolygon(E2, E1);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddEdgeToSEL(Edge: PEdge);
begin
  // SEL pointers in PEdge are reused to build A list of horizontal edges.
  // However, we don't need to worry about order with horizontal Edge processing.
  if not assigned(fSortedEdges) then
  begin
    FSortedEdges := Edge;
    Edge.PrevInSEL := nil;
    Edge.NextInSEL := nil;
  end else
  begin
    Edge.NextInSEL := FSortedEdges;
    Edge.PrevInSEL := nil;
    FSortedEdges.PrevInSEL := Edge;
    FSortedEdges := Edge;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CopyAELToSEL;
var
  E: PEdge;
begin
  E := FActiveEdges;
  FSortedEdges := E;
  if not assigned(fActiveEdges) then Exit;

  FSortedEdges.PrevInSEL := nil;
  E := E.NextInAEL;
  while assigned(E) do
  begin
    E.PrevInSEL := E.PrevInAEL;
    E.PrevInSEL.NextInSEL := E;
    E.NextInSEL := nil;
    E := E.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddJoin(E1, E2: PEdge;
  E1OutIdx: Integer = -1; E2OutIdx: Integer = -1);
var
  Jr: PJoinRec;
begin
  new(Jr);
  if E1OutIdx >= 0 then
    Jr.Poly1Idx := E1OutIdx else
    Jr.Poly1Idx := E1.OutIdx;
  with E1^ do
  begin
    Jr.Pt1a := IntPoint(XCurr, YCurr);
    Jr.Pt1b := IntPoint(XTop, YTop);
  end;
  if E2OutIdx >= 0 then
    Jr.Poly2Idx := E2OutIdx else
    Jr.Poly2Idx := E2.OutIdx;
  with E2^ do
  begin
    Jr.Pt2a := IntPoint(XCurr, YCurr);
    Jr.Pt2b := IntPoint(XTop, YTop);
  end;
  FJoinList.add(Jr);
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearJoins;
var
  I: Integer;
begin
  for I := 0 to FJoinList.Count - 1 do
    Dispose(PJoinRec(fJoinList[I]));
  FJoinList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddHorzJoin(E: PEdge; Idx: Integer);
var
  Hr: PHorzRec;
begin
  new(Hr);
  Hr.Edge := E;
  Hr.SavedIdx := Idx;
  if FHorizJoins = nil then
  begin
    FHorizJoins := Hr;
    Hr.Next := Hr;
    Hr.Prev := Hr;
  end else
  begin
    Hr.Next := FHorizJoins;
    Hr.Prev := FHorizJoins.Prev;
    FHorizJoins.Prev.Next := Hr;
    FHorizJoins.Prev := Hr;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearHorzJoins;
var
  M, M2: PHorzRec;
begin
  if not assigned(fHorizJoins) then Exit;
  M := FHorizJoins;
  M.Prev.Next := nil;
  while assigned(M) do
  begin
    M2 := M.Next;
    dispose(M);
    M := M2;
  end;
  FHorizJoins := nil;
end;
//------------------------------------------------------------------------------

procedure SwapPoints(var Pt1, Pt2: TIntPoint);
var
  Tmp: TIntPoint;
begin
  Tmp := Pt1;
  Pt1 := Pt2;
  Pt2 := Tmp;
end;
//------------------------------------------------------------------------------

function GetOverlapSegment(Pt1a, Pt1b, Pt2a, Pt2b: TIntPoint;
  out Pt1, Pt2: TIntPoint): Boolean;
begin
  // precondition: segments are colinear
  if (Pt1a.Y = Pt1b.Y) or (abs((Pt1a.X - Pt1b.X)/(Pt1a.Y - Pt1b.Y)) > 1) then
  begin
    if Pt1a.X > Pt1b.X then SwapPoints(Pt1a, Pt1b);
    if Pt2a.X > Pt2b.X then SwapPoints(Pt2a, Pt2b);
    if (Pt1a.X > Pt2a.X) then Pt1 := Pt1a else Pt1 := Pt2a;
    if (Pt1b.X < Pt2b.X) then Pt2 := Pt1b else Pt2 := Pt2b;
    Result := Pt1.X < Pt2.X;
  end else
  begin
    if Pt1a.Y < Pt1b.Y then SwapPoints(Pt1a, Pt1b);
    if Pt2a.Y < Pt2b.Y then SwapPoints(Pt2a, Pt2b);
    if (Pt1a.Y < Pt2a.Y) then Pt1 := Pt1a else Pt1 := Pt2a;
    if (Pt1b.Y > Pt2b.Y) then Pt2 := Pt1b else Pt2 := Pt2b;
    Result := Pt1.Y > Pt2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const BottomY: Int64);

  function E2InsertsBeforeE1(E1,E2: PEdge): Boolean;
  begin
    if E2.XCurr = E1.XCurr then
      Result := E2.Dx > E1.Dx else
      Result := E2.XCurr < E1.XCurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertEdgeIntoAEL(Edge: PEdge);
  var
    E: PEdge;
  begin
    Edge.PrevInAEL := nil;
    Edge.NextInAEL := nil;
    if not assigned(fActiveEdges) then
    begin
      FActiveEdges := Edge;
    end else if E2InsertsBeforeE1(fActiveEdges, Edge) then
    begin
      Edge.NextInAEL := FActiveEdges;
      FActiveEdges.PrevInAEL := Edge;
      FActiveEdges := Edge;
    end else
    begin
      E := FActiveEdges;
      while assigned(E.NextInAEL) and
        not E2InsertsBeforeE1(E.NextInAEL, Edge) do
          E := E.NextInAEL;
      Edge.NextInAEL := E.NextInAEL;
      if assigned(E.NextInAEL) then E.NextInAEL.PrevInAEL := Edge;
      Edge.PrevInAEL := E;
      E.NextInAEL := Edge;
    end;
  end;
  //----------------------------------------------------------------------

var
  E: PEdge;
  Pt, Pt2: TIntPoint;
  Lb, Rb: PEdge;
  Hj: PHorzRec;
begin
  while assigned(CurrentLm) and (CurrentLm.Y = BottomY) do
  begin
    Lb := CurrentLm.LeftBound;
    Rb := CurrentLm.RightBound;

    InsertEdgeIntoAEL(Lb);
    InsertScanbeam(Lb.YTop);
    InsertEdgeIntoAEL(Rb);

    // set Edge winding states ...
    if IsEvenOddFillType(Lb) then
    begin
      Lb.WindDelta := 1;
      Rb.WindDelta := 1;
    end else
    begin
      Rb.WindDelta := -Lb.WindDelta
    end;
    SetWindingCount(Lb);
    Rb.WindCnt := Lb.WindCnt;
    Rb.WindCnt2 := Lb.WindCnt2;

    if Rb.Dx = CHorizontal then
    begin
      AddEdgeToSEL(Rb);
      InsertScanbeam(Rb.NextInLML.YTop);
    end else
      InsertScanbeam(Rb.YTop);

    if IsContributing(Lb) then
      AddLocalMinPoly(Lb, Rb, IntPoint(Lb.XCurr, CurrentLm.Y));

    // if output polygons share an Edge with rb, they'll need joining later ...
    if (Rb.OutIdx >= 0) then
    begin
      if (Rb.Dx = CHorizontal) then
      begin
        if assigned(fHorizJoins) then
        begin
          Hj := FHorizJoins;
          repeat
            // if horizontals rb & hj.Edge overlap, flag for joining later ...
            if GetOverlapSegment(IntPoint(Hj.Edge.XBot, Hj.Edge.YBot),
              IntPoint(Hj.Edge.XTop, Hj.Edge.YTop), IntPoint(Rb.XBot, Rb.YBot),
              IntPoint(Rb.XTop, Rb.YTop), Pt, Pt2) then
                AddJoin(Hj.Edge, Rb, Hj.SavedIdx);
            Hj := Hj.Next;
          until Hj = FHorizJoins;
        end;
      end;
    end;

    if (Lb.NextInAEL <> Rb) then
    begin
      if (Rb.OutIdx >= 0) and (Rb.PrevInAEL.OutIdx >= 0) and
        SlopesEqual(Rb.PrevInAEL, Rb, FUse64BitRange) then
          AddJoin(Rb, Rb.PrevInAEL);

      E := Lb.NextInAEL;
      Pt := IntPoint(Lb.XCurr,Lb.YCurr);
      while E <> Rb do
      begin
        if not assigned(E) then raise exception.Create(rsMissingRightbound);
        // nb: For calculating winding counts etc, IntersectEdges() assumes
        // that param1 will be to the right of param2 ABOVE the intersection ...
        IntersectEdges(Rb, E, Pt);
        E := E.NextInAEL;
      end;
    end;
    PopLocalMinima;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromAEL(E: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  AelPrev := E.PrevInAEL;
  AelNext := E.NextInAEL;
  if not assigned(AelPrev) and not assigned(AelNext) and
    (E <> FActiveEdges) then Exit; // already deleted
  if assigned(AelPrev) then AelPrev.NextInAEL := AelNext
  else FActiveEdges := AelNext;
  if assigned(AelNext) then AelNext.PrevInAEL := AelPrev;
  E.NextInAEL := nil;
  E.PrevInAEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromSEL(E: PEdge);
var
  SelPrev, SelNext: PEdge;
begin
  SelPrev := E.PrevInSEL;
  SelNext := E.NextInSEL;
  if not assigned(SelPrev) and not assigned(SelNext) and
    (E <> FSortedEdges) then Exit; // already deleted
  if assigned(SelPrev) then SelPrev.NextInSEL := SelNext
  else FSortedEdges := SelNext;
  if assigned(SelNext) then SelNext.PrevInSEL := SelPrev;
  E.NextInSEL := nil;
  E.PrevInSEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.IntersectEdges(E1,E2: PEdge;
  const Pt: TIntPoint; protects: TIntersectProtects = []);

  procedure DoEdge1;
  begin
    AddOutPt(E1, Pt);
    SwapSides(E1, E2);
    SwapPolyIndexes(E1, E2);
  end;
  //----------------------------------------------------------------------

  procedure DoEdge2;
  begin
    AddOutPt(E2, Pt);
    SwapSides(E1, E2);
    SwapPolyIndexes(E1, E2);
  end;
  //----------------------------------------------------------------------

  procedure DoBothEdges;
  begin
    AddOutPt(E1, Pt);
    AddOutPt(E2, Pt);
    SwapSides(E1, E2);
    SwapPolyIndexes(E1, E2);
  end;
  //----------------------------------------------------------------------

var
  E1stops, E2stops: Boolean;
  E1Contributing, E2contributing: Boolean;
  E1FillType, E2FillType, E1FillType2, E2FillType2: TPolyFillType;
  E1Wc, E2Wc, E1Wc2, E2Wc2: Integer;
begin
  {IntersectEdges}

  // E1 will be to the left of E2 BELOW the intersection. Therefore E1 is before
  // E2 in AEL except when E1 is being inserted at the intersection point ...

  E1stops := not (ipLeft in protects) and not assigned(E1.NextInLML) and
    (E1.XTop = Pt.x) and (E1.YTop = Pt.Y);
  E2stops := not (ipRight in protects) and not assigned(E2.NextInLML) and
    (E2.XTop = Pt.x) and (E2.YTop = Pt.Y);
  E1Contributing := (E1.OutIdx >= 0);
  E2contributing := (E2.OutIdx >= 0);

  // update winding counts...
  // assumes that E1 will be to the right of E2 ABOVE the intersection
  if E1.PolyType = E2.PolyType then
  begin
    if IsEvenOddFillType(E1) then
    begin
      E1Wc := E1.WindCnt;
      E1.WindCnt := E2.WindCnt;
      E2.WindCnt := E1Wc;
    end else
    begin
      if E1.WindCnt + E2.WindDelta = 0 then
        E1.WindCnt := -E1.WindCnt else
        inc(E1.WindCnt, E2.WindDelta);
      if E2.WindCnt - E1.WindDelta = 0 then
        E2.WindCnt := -E2.WindCnt else
        dec(E2.WindCnt, E1.WindDelta);
    end;
  end else
  begin
    if not IsEvenOddFillType(E2) then inc(E1.WindCnt2, E2.WindDelta)
    else if E1.WindCnt2 = 0 then E1.WindCnt2 := 1
    else E1.WindCnt2 := 0;
    if not IsEvenOddFillType(E1) then dec(E2.WindCnt2, E1.WindDelta)
    else if E2.WindCnt2 = 0 then E2.WindCnt2 := 1
    else E2.WindCnt2 := 0;
  end;

  if E1.PolyType = ptSubject then
  begin
    E1FillType := FSubjFillType;
    E1FillType2 := FClipFillType;
  end else
  begin
    E1FillType := FClipFillType;
    E1FillType2 := FSubjFillType;
  end;
  if E2.PolyType = ptSubject then
  begin
    E2FillType := FSubjFillType;
    E2FillType2 := FClipFillType;
  end else
  begin
    E2FillType := FClipFillType;
    E2FillType2 := FSubjFillType;
  end;

  case E1FillType of
    pftPositive: E1Wc := E1.WindCnt;
    pftNegative : E1Wc := -E1.WindCnt;
    else E1Wc := abs(E1.WindCnt);
  end;
  case E2FillType of
    pftPositive: E2Wc := E2.WindCnt;
    pftNegative : E2Wc := -E2.WindCnt;
    else E2Wc := abs(E2.WindCnt);
  end;

  if E1Contributing and E2contributing then
  begin
    if E1stops or E2stops or not (E1Wc in [0,1]) or not (E2Wc in [0,1]) or
      ((E1.PolyType <> E2.PolyType) and (fClipType <> ctXor)) then
        AddLocalMaxPoly(E1, E2, Pt) else
        DoBothEdges;
  end else if E1Contributing then
  begin
    if ((E2Wc = 0) or (E2Wc = 1)) and
      ((fClipType <> ctIntersection) or (E2.PolyType = ptSubject) or
        (E2.WindCnt2 <> 0)) then DoEdge1;
  end
  else if E2contributing then
  begin
    if ((E1Wc = 0) or (E1Wc = 1)) and
      ((fClipType <> ctIntersection) or (E1.PolyType = ptSubject) or
        (E1.WindCnt2 <> 0)) then DoEdge2;
  end
  else if  ((E1Wc = 0) or (E1Wc = 1)) and ((E2Wc = 0) or (E2Wc = 1)) and
    not E1stops and not E2stops then
  begin
    // neither Edge is currently contributing ...

    case E1FillType2 of
      pftPositive: E1Wc2 := E1.WindCnt2;
      pftNegative : E1Wc2 := -E1.WindCnt2;
      else E1Wc2 := abs(E1.WindCnt2);
    end;
    case E2FillType2 of
      pftPositive: E2Wc2 := E2.WindCnt2;
      pftNegative : E2Wc2 := -E2.WindCnt2;
      else E2Wc2 := abs(E2.WindCnt2);
    end;

    if (E1.PolyType <> E2.PolyType) then
      AddLocalMinPoly(E1, E2, Pt)
    else if (E1Wc = 1) and (E2Wc = 1) then
      case FClipType of
        ctIntersection:
          if (E1Wc2 > 0) and (E2Wc2 > 0) then
            AddLocalMinPoly(E1, E2, Pt);
        ctUnion:
          if (E1Wc2 <= 0) and (E2Wc2 <= 0) then
            AddLocalMinPoly(E1, E2, Pt);
        ctDifference:
          if ((E1.PolyType = ptClip) and (E1Wc2 > 0) and (E2Wc2 > 0)) or
            ((E1.PolyType = ptSubject) and (E1Wc2 <= 0) and (E2Wc2 <= 0)) then
              AddLocalMinPoly(E1, E2, Pt);
        ctXor:
          AddLocalMinPoly(E1, E2, Pt);
      end
    else
      swapsides(E1,E2);
  end;

  if (E1stops <> E2stops) and
    ((E1stops and (E1.OutIdx >= 0)) or (E2stops and (E2.OutIdx >= 0))) then
  begin
    swapsides(E1,E2);
    SwapPolyIndexes(E1, E2);
  end;

  // finally, delete any non-contributing maxima edges  ...
  if E1stops then deleteFromAEL(E1);
  if E2stops then deleteFromAEL(E2);
end;
//------------------------------------------------------------------------------

function FirstIsBottomPt(btmPt1, btmPt2: POutPt): Boolean;
var
  Dx1n, Dx1p, Dx2n, Dx2p: Double;
  P: POutPt;
begin
  P := btmPt1.Prev;
  while PointsEqual(P.Pt, btmPt1.Pt) and (P <> btmPt1) do P := P.Prev;
  Dx1p := abs(GetDx(btmPt1.Pt, P.Pt));
  P := btmPt1.Next;
  while PointsEqual(P.Pt, btmPt1.Pt) and (P <> btmPt1) do P := P.Next;
  Dx1n := abs(GetDx(btmPt1.Pt, P.Pt));

  P := btmPt2.Prev;
  while PointsEqual(P.Pt, btmPt2.Pt) and (P <> btmPt2) do P := P.Prev;
  Dx2p := abs(GetDx(btmPt2.Pt, P.Pt));
  P := btmPt2.Next;
  while PointsEqual(P.Pt, btmPt2.Pt) and (P <> btmPt2) do P := P.Next;
  Dx2n := abs(GetDx(btmPt2.Pt, P.Pt));
  Result := ((Dx1p >= Dx2p) and (Dx1p >= Dx2n)) or
    ((Dx1n >= Dx2p) and (Dx1n >= Dx2n));
end;
//------------------------------------------------------------------------------

function GetBottomPt(PP: POutPt): POutPt;
var
  P, Dups: POutPt;
begin
  Dups := nil;
  P := PP.Next;
  while P <> PP do
  begin
    if P.Pt.Y > PP.Pt.Y then
    begin
      PP := P;
      Dups := nil;
    end
    else if (P.Pt.Y = PP.Pt.Y) and (P.Pt.X <= PP.Pt.X) then
    begin
      if (P.Pt.X < PP.Pt.X) then
      begin
        Dups := nil;
        PP := P;
      end else
      begin
        if (P.Next <> PP) and (P.Prev <> PP) then Dups := P;
      end;
    end;
    P := P.Next;
  end;
  if assigned(Dups) then
  begin
    // there appears to be at least 2 vertices at BottomPt so ...
    while Dups <> P do
    begin
      if not FirstIsBottomPt(P, Dups) then PP := Dups;
      Dups := Dups.Next;
      while not PointsEqual(Dups.Pt, PP.Pt) do Dups := Dups.Next;
    end;
  end;
  Result := PP;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetHoleState(E: PEdge; OutRec: POutRec);
var
  E2: PEdge;
  IsHole: Boolean;
begin
  IsHole := False;
  E2 := E.PrevInAEL;
  while assigned(E2) do
  begin
    if (E2.OutIdx >= 0) then
    begin
      IsHole := not IsHole;
      if not assigned(OutRec.FirstLeft) then
        OutRec.FirstLeft := POutRec(fPolyOutList[E2.OutIdx]);
    end;
    E2 := E2.PrevInAEL;
  end;
  if IsHole then
    OutRec.IsHole := True;
end;
//------------------------------------------------------------------------------

function GetLowermostRec(OutRec1, OutRec2: POutRec): POutRec;
var
  OutPt1, OutPt2: POutPt;
begin
  OutPt1 := OutRec1.BottomPt;
  OutPt2 := OutRec2.BottomPt;
  if (OutPt1.Pt.Y > OutPt2.Pt.Y) then Result := OutRec1
  else if (OutPt1.Pt.Y < OutPt2.Pt.Y) then Result := OutRec2
  else if (OutPt1.Pt.X < OutPt2.Pt.X) then Result := OutRec1
  else if (OutPt1.Pt.X > OutPt2.Pt.X) then Result := OutRec2
  else if (OutPt1.Next = OutPt1) then Result := OutRec2
  else if (OutPt2.Next = OutPt2) then Result := OutRec1
  else if FirstIsBottomPt(OutPt1, OutPt2) then
    Result := OutRec1 else
    Result := OutRec2;
end;
//------------------------------------------------------------------------------

function Param1RightOfParam2(OutRec1, OutRec2: POutRec): Boolean;
begin
  Result := True;
  repeat
    OutRec1 := OutRec1.FirstLeft;
    if OutRec1 = OutRec2 then Exit;
  until not assigned(OutRec1);
  Result := False;
end;
//------------------------------------------------------------------------------

procedure TClipper.AppendPolygon(E1, E2: PEdge);
var
  HoleStateRec, OutRec1, OutRec2: POutRec;
  P1_lft, P1_rt, P2_lft, P2_rt: POutPt;
  NewSide: TEdgeSide;
  I, OKIdx, ObsoleteIdx: Integer;
  E: PEdge;
  Jr: PJoinRec;
  H: PHorzRec;
begin
  OutRec1 := FPolyOutList[E1.OutIdx];
  OutRec2 := FPolyOutList[E2.OutIdx];

  // work out which polygon fragment has the correct hole state ...
  if Param1RightOfParam2(OutRec1, OutRec2) then HoleStateRec := OutRec2
  else if Param1RightOfParam2(OutRec2, OutRec1) then HoleStateRec := OutRec1
  else HoleStateRec := GetLowermostRec(OutRec1, OutRec2);

  // get the start and ends of both output polygons ...
  P1_lft := OutRec1.Pts;
  P2_lft := OutRec2.Pts;
  P1_rt := P1_lft.Prev;
  P2_rt := P2_lft.Prev;

  // join E2 poly onto E1 poly and delete pointers to E2 ...
  if E1.Side = esLeft then
  begin
    if E2.Side = esLeft then
    begin
      // z y x a b c
      ReversePolyPtLinks(P2_lft);
      P2_lft.Next := P1_lft;
      P1_lft.Prev := P2_lft;
      P1_rt.Next := P2_rt;
      P2_rt.Prev := P1_rt;
      OutRec1.Pts := P2_rt;
    end else
    begin
      // x y z a b c
      P2_rt.Next := P1_lft;
      P1_lft.Prev := P2_rt;
      P2_lft.Prev := P1_rt;
      P1_rt.Next := P2_lft;
      OutRec1.Pts := P2_lft;
    end;
    NewSide := esLeft;
  end else
  begin
    if E2.Side = esRight then
    begin
      // a b c z y x
      ReversePolyPtLinks(P2_lft);
      P1_rt.Next := P2_rt;
      P2_rt.Prev := P1_rt;
      P2_lft.Next := P1_lft;
      P1_lft.Prev := P2_lft;
    end else
    begin
      // a b c x y z
      P1_rt.Next := P2_lft;
      P2_lft.Prev := P1_rt;
      P1_lft.Prev := P2_rt;
      P2_rt.Next := P1_lft;
    end;
    NewSide := esRight;
  end;

  if HoleStateRec = OutRec2 then
  begin
    OutRec1.BottomPt := OutRec2.BottomPt;
    OutRec1.BottomPt.Idx := OutRec1.Idx;
    if OutRec2.FirstLeft <> OutRec1 then
      OutRec1.FirstLeft := OutRec2.FirstLeft;
    OutRec1.IsHole := OutRec2.IsHole;
  end;
  OutRec2.Pts := nil;
  OutRec2.BottomPt := nil;
  OutRec2.AppendLink := OutRec1;
  OKIdx := OutRec1.Idx;
  ObsoleteIdx := OutRec2.Idx;

  E1.OutIdx := -1; // nb: safe because we only get here via AddLocalMaxPoly
  E2.OutIdx := -1;

  E := FActiveEdges;
  while assigned(E) do
  begin
    if (E.OutIdx = ObsoleteIdx) then
    begin
      E.OutIdx := OKIdx;
      E.Side := NewSide;
      break;
    end;
    E := E.NextInAEL;
  end;

  for I := 0 to FJoinList.Count - 1 do
  begin
    Jr := FJoinList[I];
    if Jr.Poly1Idx = ObsoleteIdx then Jr.Poly1Idx := OKIdx;
    if Jr.Poly2Idx = ObsoleteIdx then Jr.Poly2Idx := OKIdx;
  end;
  if assigned(fHorizJoins) then
  begin
    H := FHorizJoins;
    repeat
      if H.SavedIdx = ObsoleteIdx then H.SavedIdx := OKIdx;
      H := H.Next;
    until H = FHorizJoins;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.CreateOutRec: POutRec;
begin
  new(Result);
  Result.IsHole := False;
  Result.FirstLeft := nil;
  Result.AppendLink := nil;
  Result.Pts := nil;
  Result.BottomPt := nil;
  Result.Sides := [];
  Result.BottomFlag := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddOutPt(E: PEdge; const Pt: TIntPoint);
var
  OutRec: POutRec;
  Op, op2, opBot: POutPt;
  ToFront: Boolean;
begin
  ToFront := E.Side = esLeft;
  if E.OutIdx < 0 then
  begin
    OutRec := CreateOutRec;
    OutRec.Idx := FPolyOutList.Add(OutRec);
    E.OutIdx := OutRec.Idx;
    new(Op);
    OutRec.Pts := Op;
    OutRec.BottomPt := Op;

    Op.Pt := Pt;
    Op.Next := Op;
    Op.Prev := Op;
    Op.Idx := OutRec.Idx;
    SetHoleState(E, OutRec);
  end else
  begin
    OutRec := FPolyOutList[E.OutIdx];
    Op := OutRec.Pts;
    if (ToFront and PointsEqual(Pt, Op.Pt)) or
      (not ToFront and PointsEqual(Pt, Op.Prev.Pt)) then Exit;

    if not (E.Side in OutRec.Sides) then
    begin

      // check for 'rounding' artefacts ...
      if (OutRec.Sides = []) and (Pt.Y = Op.Pt.Y) then
        if ToFront then
        begin
          if (Pt.X = Op.Pt.X +1) then Exit;    // ie wrong Side of BottomPt
        end
        else if (Pt.X = Op.Pt.X -1) then Exit; // ie wrong Side of BottomPt

      OutRec.Sides := OutRec.Sides + [E.Side];
      if OutRec.Sides = [esLeft, esRight] then
      begin
        // A vertex from each Side has now been added.
        // Vertices of one Side of an output polygon are quite commonly close to
        // or even 'touching' edges of the other Side of the output polygon.
        // Very occasionally vertices from one Side can 'cross' an Edge on the
        // the other Side. The distance 'crossed' is always less that A unit
        // and is purely an artefact of coordinate rounding. Nevertheless, this
        // results in very tiny self-intersections. Because of the way
        // orientation is calculated, even tiny self-intersections can cause
        // the Orientation function to return the wrong Result. Therefore, it's
        // important to ensure that any self-intersections close to BottomPt are
        // detected and removed before orientation is assigned.

        if ToFront then
        begin
          opBot := OutRec.Pts;
          op2 := opBot.Next; // op2 == right Side
          if (opBot.Pt.Y <> op2.Pt.Y) and (opBot.Pt.Y <> Pt.Y) and
            ((opBot.Pt.X - Pt.X)/(opBot.Pt.Y - Pt.Y) <
            (opBot.Pt.X - op2.Pt.X)/(opBot.Pt.Y - op2.Pt.Y)) then
               OutRec.BottomFlag := opBot;
        end else
        begin
          opBot := OutRec.Pts.Prev;
          op2 := opBot.Prev; // op2 == left Side
          if (opBot.Pt.Y <> op2.Pt.Y) and (opBot.Pt.Y <> Pt.Y) and
            ((opBot.Pt.X - Pt.X)/(opBot.Pt.Y - Pt.Y) >
            (opBot.Pt.X - op2.Pt.X)/(opBot.Pt.Y - op2.Pt.Y)) then
               OutRec.BottomFlag := opBot;
        end;
      end;
    end;

    new(op2);
    op2.Pt := Pt;
    op2.Idx := OutRec.Idx;
    if (op2.Pt.Y = OutRec.BottomPt.Pt.Y) and
      (op2.Pt.X < OutRec.BottomPt.Pt.X) then
        OutRec.BottomPt := op2;
    op2.Next := Op;
    op2.Prev := Op.Prev;
    Op.Prev.Next := op2;
    Op.Prev := op2;
    if ToFront then OutRec.Pts := op2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontals;
var
  E: PEdge;
begin
  while assigned(fSortedEdges) do
  begin
    E := FSortedEdges;
    DeleteFromSEL(E);
    ProcessHorizontal(E);
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsTopHorz(const XPos: Int64): Boolean;
var
  E: PEdge;
begin
  Result := False;
  E := FSortedEdges;
  while assigned(E) do
  begin
    if (XPos >= min(E.XCurr,E.XTop)) and (XPos <= max(E.XCurr,E.XTop)) then Exit;
    E := E.NextInSEL;
  end;
  Result := True;
end;
//------------------------------------------------------------------------------

function IsMinima(E: PEdge): Boolean;
begin
  Result := assigned(E) and (E.Prev.NextInLML <> E) and (E.Next.NextInLML <> E);
end;
//------------------------------------------------------------------------------

function IsMaxima(E: PEdge; const Y: Int64): Boolean;
begin
  Result := assigned(E) and (E.YTop = Y) and not assigned(E.NextInLML);
end;
//------------------------------------------------------------------------------

function IsIntermediate(E: PEdge; const Y: Int64): Boolean;
begin
  Result := (E.YTop = Y) and assigned(E.NextInLML);
end;
//------------------------------------------------------------------------------

function GetMaximaPair(E: PEdge): PEdge;
begin
  Result := E.Next;
  if not IsMaxima(Result, E.YTop) or (Result.XTop <> E.XTop) then
    Result := E.Prev;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(E1, E2: PEdge);
var
  Prev,Next: PEdge;
begin
  with E1^ do if not assigned(NextInAEL) and not assigned(PrevInAEL) then Exit;
  with E2^ do if not assigned(NextInAEL) and not assigned(PrevInAEL) then Exit;

  if E1.NextInAEL = E2 then
  begin
    Next := E2.NextInAEL;
    if assigned(Next) then Next.PrevInAEL := E1;
    Prev := E1.PrevInAEL;
    if assigned(Prev) then Prev.NextInAEL := E2;
    E2.PrevInAEL := Prev;
    E2.NextInAEL := E1;
    E1.PrevInAEL := E2;
    E1.NextInAEL := Next;
  end
  else if E2.NextInAEL = E1 then
  begin
    Next := E1.NextInAEL;
    if assigned(Next) then Next.PrevInAEL := E2;
    Prev := E2.PrevInAEL;
    if assigned(Prev) then Prev.NextInAEL := E1;
    E1.PrevInAEL := Prev;
    E1.NextInAEL := E2;
    E2.PrevInAEL := E1;
    E2.NextInAEL := Next;
  end else
  begin
    Next := E1.NextInAEL;
    Prev := E1.PrevInAEL;
    E1.NextInAEL := E2.NextInAEL;
    if assigned(E1.NextInAEL) then E1.NextInAEL.PrevInAEL := E1;
    E1.PrevInAEL := E2.PrevInAEL;
    if assigned(E1.PrevInAEL) then E1.PrevInAEL.NextInAEL := E1;
    E2.NextInAEL := Next;
    if assigned(E2.NextInAEL) then E2.NextInAEL.PrevInAEL := E2;
    E2.PrevInAEL := Prev;
    if assigned(E2.PrevInAEL) then E2.PrevInAEL.NextInAEL := E2;
  end;
  if not assigned(E1.PrevInAEL) then FActiveEdges := E1
  else if not assigned(E2.PrevInAEL) then FActiveEdges := E2;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInSEL(E1, E2: PEdge);
var
  Prev,Next: PEdge;
begin
  if E1.NextInSEL = E2 then
  begin
    Next    := E2.NextInSEL;
    if assigned(Next) then Next.PrevInSEL := E1;
    Prev    := E1.PrevInSEL;
    if assigned(Prev) then Prev.NextInSEL := E2;
    E2.PrevInSEL := Prev;
    E2.NextInSEL := E1;
    E1.PrevInSEL := E2;
    E1.NextInSEL := Next;
  end
  else if E2.NextInSEL = E1 then
  begin
    Next    := E1.NextInSEL;
    if assigned(Next) then Next.PrevInSEL := E2;
    Prev    := E2.PrevInSEL;
    if assigned(Prev) then Prev.NextInSEL := E1;
    E1.PrevInSEL := Prev;
    E1.NextInSEL := E2;
    E2.PrevInSEL := E1;
    E2.NextInSEL := Next;
  end else
  begin
    Next    := E1.NextInSEL;
    Prev    := E1.PrevInSEL;
    E1.NextInSEL := E2.NextInSEL;
    if assigned(E1.NextInSEL) then E1.NextInSEL.PrevInSEL := E1;
    E1.PrevInSEL := E2.PrevInSEL;
    if assigned(E1.PrevInSEL) then E1.PrevInSEL.NextInSEL := E1;
    E2.NextInSEL := Next;
    if assigned(E2.NextInSEL) then E2.NextInSEL.PrevInSEL := E2;
    E2.PrevInSEL := Prev;
    if assigned(E2.PrevInSEL) then E2.PrevInSEL.NextInSEL := E2;
  end;
  if not assigned(E1.PrevInSEL) then FSortedEdges := E1
  else if not assigned(E2.PrevInSEL) then FSortedEdges := E2;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontal(HorzEdge: PEdge);

  function GetNextInAEL(E: PEdge; Direction: TDirection): PEdge;
  begin
    if Direction = dLeftToRight then
      Result := E.NextInAEL else
      Result := E.PrevInAEL;
  end;
  //------------------------------------------------------------------------

var
  E, eNext, eMaxPair: PEdge;
  HorzLeft, HorzRight: Int64;
  Direction: TDirection;
const
  ProtectLeft: array[Boolean] of TIntersectProtects = ([ipRight], [ipLeft,ipRight]);
  ProtectRight: array[Boolean] of TIntersectProtects = ([ipLeft], [ipLeft,ipRight]);
begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of A scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with other HE xbots only [#],    *
* and with other non-horizontal edges [*]. Once these intersections are        *
* processed, intermediate HEs then 'promote' the Edge above (NextInLML) into   *
* the AEL. These 'promoted' edges may in turn intersect [%] with other HEs.    *
*******************************************************************************)

(*******************************************************************************
*           \   nb: HE processing order doesn't matter         /          /    *
*            \                                                /          /     *
* { --------  \  -------------------  /  \  - (3) o==========%==========o  - } *
* {            o==========o (2)      /    \       .          .               } *
* {                       .         /      \      .          .               } *
* { ----  o===============#========*========*=====#==========o  (1)  ------- } *
*        /                 \      /          \   /                             *
*******************************************************************************)

  if HorzEdge.XCurr < HorzEdge.XTop then
  begin
    HorzLeft := HorzEdge.XCurr;
    HorzRight := HorzEdge.XTop;
    Direction := dLeftToRight;
  end else
  begin
    HorzLeft := HorzEdge.XTop;
    HorzRight := HorzEdge.XCurr;
    Direction := dRightToLeft;
  end;

  if assigned(HorzEdge.NextInLML) then
    eMaxPair := nil else
    eMaxPair := GetMaximaPair(HorzEdge);

  E := GetNextInAEL(HorzEdge, Direction);
  while assigned(E) do
  begin
    eNext := GetNextInAEL(E, Direction);
    if assigned(eMaxPair) or
       ((Direction = dLeftToRight) and (E.XCurr <= HorzRight)) or
      ((Direction = dRightToLeft) and (E.XCurr >= HorzLeft)) then
    begin
      // ok, so far it looks like we're still in range of the horizontal Edge

      if (E.XCurr = HorzEdge.XTop) and not assigned(eMaxPair) then
      begin
        if SlopesEqual(E, HorzEdge.NextInLML, FUse64BitRange) then
        begin
          // if output polygons share an Edge, they'll need joining later ...
          if (HorzEdge.OutIdx >= 0) and (E.OutIdx >= 0) then
            AddJoin(HorzEdge.NextInLML, E, HorzEdge.OutIdx);
          break; // we've reached the end of the horizontal line
        end
        else if (E.Dx < HorzEdge.NextInLML.Dx) then
        // we really have got to the end of the intermediate horz Edge so quit.
        // nb: More -ve slopes follow more +ve slopes ABOVE the horizontal.
          break;
      end;

      if (E = eMaxPair) then
      begin
        // HorzEdge is evidently A maxima horizontal and we've arrived at its end.
        if Direction = dLeftToRight then
          IntersectEdges(HorzEdge, E, IntPoint(E.XCurr, HorzEdge.YCurr)) else
          IntersectEdges(E, HorzEdge, IntPoint(E.XCurr, HorzEdge.YCurr));

        if (eMaxPair.OutIdx >= 0) then raise exception.Create(rsHorizontal);
        Exit;
      end
      else if (E.Dx = CHorizontal) and not IsMinima(E) and not (E.XCurr > E.XTop) then
      begin
        // An overlapping horizontal Edge. Overlapping horizontal edges are
        // processed as if layered with the current horizontal Edge (horizEdge)
        // being infinitesimally lower that the Next (E). Therfore, we
        // intersect with E only if E.XCurr is within the bounds of HorzEdge ...
        if Direction = dLeftToRight then
          IntersectEdges(HorzEdge, E, IntPoint(E.XCurr, HorzEdge.YCurr),
            ProtectRight[not IsTopHorz(E.XCurr)])
        else
          IntersectEdges(E, HorzEdge, IntPoint(E.XCurr, HorzEdge.YCurr),
            ProtectLeft[not IsTopHorz(E.XCurr)]);
      end
      else if (Direction = dLeftToRight) then
        IntersectEdges(HorzEdge, E, IntPoint(E.XCurr, HorzEdge.YCurr),
          ProtectRight[not IsTopHorz(E.XCurr)])
      else
        IntersectEdges(E, HorzEdge, IntPoint(E.XCurr, HorzEdge.YCurr),
          ProtectLeft[not IsTopHorz(E.XCurr)]);
      SwapPositionsInAEL(HorzEdge, E);
    end
    else if ((Direction = dLeftToRight) and
      (E.XCurr > HorzRight) and assigned(fSortedEdges)) or
      ((Direction = dRightToLeft) and
      (E.XCurr < HorzLeft) and assigned(fSortedEdges)) then
        break;
    E := eNext;
  end;

  if assigned(HorzEdge.NextInLML) then
  begin
    if (HorzEdge.OutIdx >= 0) then
      AddOutPt(HorzEdge, IntPoint(HorzEdge.XTop, HorzEdge.YTop));
    UpdateEdgeIntoAEL(HorzEdge);
  end else
  begin
    if HorzEdge.OutIdx >= 0 then
      IntersectEdges(HorzEdge, eMaxPair,
        IntPoint(HorzEdge.XTop, HorzEdge.YCurr), [ipLeft,ipRight]);

    if eMaxPair.OutIdx >= 0 then raise exception.Create(rsHorizontal);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(HorzEdge);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var E: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  if not assigned(E.NextInLML) then raise exception.Create(rsUpdateEdgeIntoAEL);
  AelPrev := E.PrevInAEL;
  AelNext := E.NextInAEL;
  E.NextInLML.OutIdx := E.OutIdx;
  if assigned(AelPrev) then
    AelPrev.NextInAEL := E.NextInLML else
    FActiveEdges := E.NextInLML;
  if assigned(AelNext) then
    AelNext.PrevInAEL := E.NextInLML;
  E.NextInLML.Side := E.Side;
  E.NextInLML.WindDelta := E.WindDelta;
  E.NextInLML.WindCnt := E.WindCnt;
  E.NextInLML.WindCnt2 := E.WindCnt2;
  E := E.NextInLML;
  E.PrevInAEL := AelPrev;
  E.NextInAEL := AelNext;
  if E.Dx <> CHorizontal then
    InsertScanbeam(E.YTop);
end;
//------------------------------------------------------------------------------

function TClipper.ProcessIntersections(const BottomY, TopY: Int64): Boolean;
begin
  Result := True;
  try
    BuildIntersectList(BottomY, TopY);
    if FIntersectNodes = nil then Exit;
    if FixupIntersections then ProcessIntersectList
    else Result := False;
  finally
    // if there's been an error, clean up the mess ...
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeIntersectNodes;
var
  N: PIntersectNode;
begin
  while assigned(fIntersectNodes) do
  begin
    N := FIntersectNodes.Next;
    dispose(fIntersectNodes);
    FIntersectNodes := N;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildIntersectList(const BottomY, TopY: Int64);
var
  E, eNext: PEdge;
  Pt: TIntPoint;
  IsModified: Boolean;
begin
  if not assigned(fActiveEdges) then Exit;

  // prepare for sorting ...
  E := FActiveEdges;
  E.TmpX := TopX(E, TopY);
  FSortedEdges := E;
  FSortedEdges.PrevInSEL := nil;
  E := E.NextInAEL;
  while assigned(E) do
  begin
    E.PrevInSEL := E.PrevInAEL;
    E.PrevInSEL.NextInSEL := E;
    E.NextInSEL := nil;
    E.TmpX := TopX(E, TopY);
    E := E.NextInAEL;
  end;

  try
    // bubblesort ...
    IsModified := True;
    while IsModified and assigned(fSortedEdges) do
    begin
      IsModified := False;
      E := FSortedEdges;
      while assigned(E.NextInSEL) do
      begin
        eNext := E.NextInSEL;
        if (E.TmpX > eNext.TmpX) and
          IntersectPoint(E, eNext, Pt, FUse64BitRange) then
        begin
          if Pt.Y > BottomY then
          begin
            Pt.Y := BottomY;
            Pt.X := TopX(E, Pt.Y);
          end;
          AddIntersectNode(E, eNext, Pt);
          SwapPositionsInSEL(E, eNext);
          IsModified := True;
        end else
          E := eNext;
      end;
      if assigned(E.PrevInSEL) then E.PrevInSEL.NextInSEL := nil else break;
    end;
  finally
    FSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddIntersectNode(E1, E2: PEdge; const Pt: TIntPoint);

  function ProcessParam1BeforeParam2(node1, node2: PIntersectNode): Boolean;
  begin
    if node1.Pt.Y = node2.Pt.Y then
    begin
      if (node1.Edge1 = node2.Edge1) or (node1.Edge2 = node2.Edge1) then
      begin
        Result := node2.Pt.X > node1.Pt.X;
        if node2.Edge1.Dx > 0 then Result := not Result;
      end
      else if (node1.Edge1 = node2.Edge2) or (node1.Edge2 = node2.Edge2) then
      begin
        Result := node2.Pt.X > node1.Pt.X;
        if node2.Edge2.Dx > 0 then Result := not Result;
      end else
        Result := node2.Pt.X > node1.Pt.X;
    end
    else Result := node1.Pt.Y > node2.Pt.Y;
  end;
  //----------------------------------------------------------------------------

var
  Node, NewNode: PIntersectNode;
begin
  new(NewNode);
  NewNode.Edge1 := E1;
  NewNode.Edge2 := E2;
  NewNode.Pt := Pt;
  NewNode.Next := nil;
  if not assigned(fIntersectNodes) then
    FIntersectNodes := NewNode
  else if ProcessParam1BeforeParam2(NewNode, FIntersectNodes) then
  begin
    NewNode.Next := FIntersectNodes;
    FIntersectNodes := NewNode;
  end else
  begin
    Node := FIntersectNodes;
    while assigned(Node.Next) and
      ProcessParam1BeforeParam2(Node.Next, NewNode) do
      Node := Node.Next;
    NewNode.Next := Node.Next;
    Node.Next := NewNode;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersectList;
var
  Node: PIntersectNode;
begin
  while assigned(fIntersectNodes) do
  begin
    Node := FIntersectNodes.Next;
    with FIntersectNodes^ do
    begin
      IntersectEdges(Edge1, Edge2, Pt, [ipLeft,ipRight]);
      SwapPositionsInAEL(Edge1, Edge2);
    end;
    dispose(fIntersectNodes);
    FIntersectNodes := Node;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DoMaxima(E: PEdge; const TopY: Int64);
var
  ENext, EMaxPair: PEdge;
  X: Int64;
begin
  EMaxPair := GetMaximaPair(E);
  X := E.XTop;
  ENext := E.NextInAEL;
  while ENext <> EMaxPair do
  begin
    if not assigned(ENext) then raise exception.Create(rsDoMaxima);
    IntersectEdges(E, ENext, IntPoint(X, TopY), [ipLeft, ipRight]);
    ENext := ENext.NextInAEL;
  end;
  if (E.OutIdx < 0) and (EMaxPair.OutIdx < 0) then
  begin
    DeleteFromAEL(E);
    DeleteFromAEL(EMaxPair);
  end
  else if (E.OutIdx >= 0) and (EMaxPair.OutIdx >= 0) then
  begin
    IntersectEdges(E, EMaxPair, IntPoint(X, TopY));
  end
  else raise exception.Create(rsDoMaxima);
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessEdgesAtTopOfScanbeam(const TopY: Int64);
var
  E, ePrev, eNext: PEdge;
  Hj: PHorzRec;
  Pt, Pt2: TIntPoint;
begin
(*******************************************************************************
* Notes: Processing edges at scanline intersections (ie at the top or bottom   *
* of A scanbeam) needs to be done in multiple stages and in the correct order. *
* Firstly, edges forming A 'maxima' need to be processed and then removed.     *
* Next, 'intermediate' and 'maxima' horizontal edges are processed. Then edges *
* that intersect exactly at the top of the scanbeam are processed [%].         *
* Finally, new minima are added and any intersects they create are processed.  *
*******************************************************************************)

(*******************************************************************************
*     \                          /    /          \   /                         *
*      \   Horizontal minima    /    /            \ /                          *
* { --  o======================#====o   --------   .     ------------------- } *
* {       Horizontal maxima    .                   %  scanline intersect     } *
* { -- o=======================#===================#========o     ---------- } *
*      |                      /                   / \        \                 *
*      + maxima intersect    /                   /   \        \                *
*     /|\                   /                   /     \        \               *
*    / | \                 /                   /       \        \              *
*******************************************************************************)

  E := FActiveEdges;
  while assigned(E) do
  begin
    //1. process maxima, treating them as if they're 'bent' horizontal edges,
    //   but exclude maxima with Horizontal edges. nb: E can't be A Horizontal.
    if IsMaxima(E, TopY) and (GetMaximaPair(E).Dx <> CHorizontal) then
    begin
      //'E' might be removed from AEL, as may any following edges so ...
      ePrev := E.PrevInAEL;
      DoMaxima(E, TopY);
      if not assigned(ePrev) then
        E := FActiveEdges else
        E := ePrev.NextInAEL;
    end else
    begin
      //2. promote horizontal edges, otherwise update XCurr and YCurr ...
      if IsIntermediate(E, TopY) and (E.NextInLML.Dx = CHorizontal) then
      begin
        if (E.OutIdx >= 0) then
        begin
          AddOutPt(E, IntPoint(E.XTop, E.YTop));

          Hj := FHorizJoins;
          if assigned(Hj) then
          repeat
            if GetOverlapSegment(IntPoint(Hj.Edge.XBot, Hj.Edge.YBot),
              IntPoint(Hj.Edge.XTop, Hj.Edge.YTop),
              IntPoint(E.NextInLML.XBot, E.NextInLML.YBot),
              IntPoint(E.NextInLML.XTop, E.NextInLML.YTop), Pt, Pt2) then
                AddJoin(Hj.Edge, E.NextInLML, Hj.SavedIdx, E.OutIdx);
            Hj := Hj.Next;
          until Hj = FHorizJoins;

          AddHorzJoin(E.NextInLML, E.OutIdx);
        end;
        UpdateEdgeIntoAEL(E);
        AddEdgeToSEL(E);
      end else
      begin
        // this just simplifies horizontal processing ...
        E.XCurr := TopX(E, TopY);
        E.YCurr := TopY;
      end;
      E := E.NextInAEL;
    end;
  end;

  //3. Process horizontals at the top of the scanbeam ...
  ProcessHorizontals;

  //4. Promote intermediate vertices ...
  E := FActiveEdges;
  while assigned(E) do
  begin
    if IsIntermediate(E, TopY) then
    begin
      if (E.OutIdx >= 0) then AddOutPt(E, IntPoint(E.XTop, E.YTop));
      UpdateEdgeIntoAEL(E);

      // if output polygons share an Edge, they'll need joining later ...
      ePrev := E.PrevInAEL;
      eNext  := E.NextInAEL;
      if assigned(ePrev) and (ePrev.XCurr = E.XBot) and
        (ePrev.YCurr = E.YBot) and (E.OutIdx >= 0) and
        (ePrev.OutIdx >= 0) and (ePrev.YCurr > ePrev.YTop) and
        SlopesEqual(E, ePrev, FUse64BitRange) then
      begin
        AddOutPt(ePrev, IntPoint(E.XBot, E.YBot));
        AddJoin(E, ePrev);
      end
      else if assigned(eNext) and (eNext.XCurr = E.XBot) and
        (eNext.YCurr = E.YBot) and (E.OutIdx >= 0) and
        (eNext.OutIdx >= 0) and (eNext.YCurr > eNext.YTop) and
        SlopesEqual(E, eNext, FUse64BitRange) then
      begin
        AddOutPt(eNext, IntPoint(E.XBot, E.YBot));
        AddJoin(E, eNext);
      end;
    end;
    E := E.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.GetResultAsFloatPoints: TArrayOfArrayOfFloatPoint;
var
  I, J, K, Cnt: Integer;
  OutRec: POutRec;
  Op: POutPt;
begin
  K := 0;
  setLength(Result, FPolyOutList.Count);
  for I := 0 to FPolyOutList.Count - 1 do
    if assigned(fPolyOutList[I]) then
    begin
      // make sure each polygon has at least 3 vertices ...
      OutRec := FPolyOutList[I];
      Op := OutRec.Pts;
      if not assigned(Op) then Continue; // nb: not sorted
      Cnt := PointCount(Op);
      if (Cnt < 3) then Continue;

      setLength(Result[K], Cnt);
      for J := 0 to Cnt - 1 do
      begin
        Result[K][J] := IntPointToFloatPoint(Op.Pt);
        Op := Op.Next;
      end;
      inc(K);
    end;
  setLength(Result, K);
end;
//------------------------------------------------------------------------------

function TClipper.GetResultAsFixedPoints: TArrayOfArrayOfFixedPoint;
var
  I, J, K, Cnt: Integer;
  OutRec: POutRec;
  Op: POutPt;
begin
  K := 0;
  setLength(Result, FPolyOutList.Count);
  for I := 0 to FPolyOutList.Count - 1 do
    if assigned(fPolyOutList[I]) then
    begin
      // make sure each polygon has at least 3 vertices ...
      OutRec := FPolyOutList[I];
      Op := OutRec.Pts;
      if not assigned(Op) then Continue; // nb: not sorted
      Cnt := PointCount(Op);
      if (Cnt < 3) then Continue;

      setLength(Result[K], Cnt);
      for J := 0 to Cnt - 1 do
      begin
        Result[K][J] := IntPointToFixedPoint(Op.Pt);
        Op := Op.Next;
      end;
      inc(K);
    end;
  setLength(Result, K);
end;
//------------------------------------------------------------------------------

procedure TClipper.FixupOutPolygon(OutRec: POutRec);
var
  PP, Tmp, LastOK: POutPt;
begin
  // FixupOutPolygon() - removes duplicate points and simplifies consecutive
  // parallel edges by removing the middle vertex.
  LastOK := nil;
  OutRec.Pts := OutRec.BottomPt;
  PP := OutRec.Pts;
  while True do
  begin
    if (PP.Prev = PP) or (PP.Next = PP.Prev) then
    begin
      DisposePolyPts(PP);
      OutRec.Pts := nil;
      OutRec.BottomPt := nil;
      Exit;
    end;

    // test for duplicate points and for colinear edges ...
    if PointsEqual(PP.Pt, PP.Next.Pt) or
      SlopesEqual(PP.Prev.Pt, PP.Pt, PP.Next.Pt, FUse64BitRange) then
    begin
      // OK, we need to delete A point ...
      LastOK := nil;
      Tmp := PP;
      if PP = OutRec.BottomPt then
        OutRec.BottomPt := nil; // flags need for updating
      PP.Prev.Next := PP.Next;
      PP.Next.Prev := PP.Prev;
      PP := PP.Prev;
      dispose(Tmp);
    end
    else if PP = LastOK then break
    else
    begin
      if not assigned(LastOK) then LastOK := PP;
      PP := PP.Next;
    end;
  end;
  if not assigned(OutRec.BottomPt) then
  begin
    OutRec.BottomPt := GetBottomPt(PP);
    OutRec.BottomPt.Idx := OutRec.Idx;
    OutRec.Pts := OutRec.BottomPt;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.FixupIntersections: Boolean;
var
  E1, E2: PEdge;
  Int1, Int2: PIntersectNode;
begin
  Result := not assigned(fIntersectNodes.Next);
  if Result then Exit;
  // logic: only swap (intersect) adjacent edges ...
  try
    CopyAELToSEL;
    Int1 := FIntersectNodes;
    Int2 := FIntersectNodes.Next;
    while assigned(Int2) do
    begin
      E1 := Int1.Edge1;
      if (E1.PrevInSEL = Int1.Edge2) then E2 := E1.PrevInSEL
      else if (E1.NextInSEL = Int1.Edge2) then E2 := E1.NextInSEL
      else
      begin
        // The current intersection is out of order, so try and swap it with
        // A subsequent intersection ...
        while assigned(Int2) do
        begin
          if (Int2.Edge1.NextInSEL = Int2.Edge2) or
            (Int2.Edge1.PrevInSEL = Int2.Edge2) then break
          else Int2 := Int2.Next;
        end;
        if not assigned(Int2) then Exit; // oops!!!
        // found an intersect node that can be swapped ...
        SwapIntersectNodes(Int1, Int2);
        E1 := Int1.Edge1;
        E2 := Int1.Edge2;
      end;
      SwapPositionsInSEL(E1, E2);
      Int1 := Int1.Next;
      Int2 := Int1.Next;
    end;

    // finally, check the last intersection too ...
    Result := (Int1.Edge1.PrevInSEL = Int1.Edge2) or
      (Int1.Edge1.NextInSEL = Int1.Edge2);
  finally
    FSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapIntersectNodes(Int1, Int2: PIntersectNode);
var
  E1,E2: PEdge;
  P: TIntPoint;
begin
  with Int1^ do
  begin
    E1 := Edge1;
    Edge1 := Int2.Edge1;
    E2 := Edge2;
    Edge2 := Int2.Edge2;
    P := Pt;
    Pt := Int2.Pt;
  end;
  with Int2^ do
  begin
    Edge1 := E1;
    Edge2 := E2;
    Pt := P;
  end;
end;
//------------------------------------------------------------------------------

function FindSegment(var PP: POutPt; var Pt1, Pt2: TIntPoint): Boolean;
var
  Pp2: POutPt;
  Pt1a, Pt2a: TIntPoint;
begin
  if not assigned(PP) then begin Result := False; Exit; end;
  Result := True;
  Pt1a := Pt1; Pt2a := Pt2;
  Pp2 := PP;
  repeat
    // test for co-linearity before testing for overlap ...
    if SlopesEqual(Pt1a, Pt2a, PP.Pt, PP.Prev.Pt, True) and
      SlopesEqual(Pt1a, Pt2a, PP.Pt, True) and
        GetOverlapSegment(Pt1a, Pt2a, PP.Pt, PP.Prev.Pt, Pt1, Pt2) then Exit;
    PP := PP.Next;
  until PP = Pp2;
  Result := False;
end;
//------------------------------------------------------------------------------

function Pt3IsBetweenPt1AndPt2(const Pt1, Pt2, Pt3: TIntPoint): Boolean;
begin
  if PointsEqual(Pt1, Pt3) or PointsEqual(Pt2, Pt3) then Result := True
  else if (Pt1.X <> Pt2.X) then Result := (Pt1.X < Pt3.X) = (Pt3.X < Pt2.X)
  else Result := (Pt1.Y < Pt3.Y) = (Pt3.Y < Pt2.Y);
end;
//------------------------------------------------------------------------------

function InsertPolyPtBetween(p1, P2: POutPt; const Pt: TIntPoint): POutPt;
begin
  if (p1 = P2) then raise exception.Create(rsJoinError);

  new(Result);
  Result.Pt := Pt;
  Result.Idx := p1.Idx;
  if P2 = p1.Next then
  begin
    p1.Next := Result;
    P2.Prev := Result;
    Result.Next := P2;
    Result.Prev := p1;
  end else
  begin
    P2.Next := Result;
    p1.Prev := Result;
    Result.Next := p1;
    Result.Prev := P2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages1(const OutRec1, OutRec2: POutRec);
var
  I: Integer;
begin
  //when A polygon is split into 2 polygons, make sure any holes the original
  //polygon contained link to the correct polygon ...
  if not OutRec1.IsHole then Exit; 
  for I := 0 to FPolyOutList.Count - 1 do
    with POutRec(fPolyOutList[I])^ do
      if IsHole and assigned(BottomPt) and (FirstLeft = OutRec1) then
        FirstLeft := OutRec2;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages2(const OutRec1, OutRec2: POutRec);
var
  I: Integer;
begin
  // if A hole is owned by OutRec2 then make it owned by OutRec1 ...
  for I := 0 to FPolyOutList.Count - 1 do
    with POutRec(fPolyOutList[I])^ do
      if IsHole and assigned(BottomPt) and (FirstLeft = OutRec2) then
        FirstLeft := OutRec1;
end;
//------------------------------------------------------------------------------

procedure TClipper.JoinCommonEdges(FixHoleLinkages: Boolean);
var
  I, J, OKIdx, ObsoleteIdx: Integer;
  Jr, Jr2: PJoinRec;
  OutRec1, OutRec2: POutRec;
  Prev, p1, P2, p3, p4, Pp1a, Pp2a: POutPt;
  Pt1, Pt2, Pt3, Pt4: TIntPoint;
begin
  for I := 0 to FJoinList.count - 1 do
  begin
    Jr := FJoinList[I];
    OutRec1 := FPolyOutList[Jr.Poly1Idx];
    if not assigned(OutRec1) then Continue;
    Pp1a := OutRec1.Pts;
    OutRec2 := FPolyOutList[Jr.Poly2Idx];
    if not assigned(OutRec2) then Continue;
    Pp2a := OutRec2.Pts;
    Pt1 := Jr.Pt2a; Pt2 := Jr.Pt2b;
    Pt3 := Jr.Pt1a; Pt4 := Jr.Pt1b;
    if not FindSegment(Pp1a, Pt1, Pt2) then Continue;
    if (Jr.Poly1Idx = Jr.Poly2Idx) then
    begin
      // we're searching the same polygon for overlapping segments so
      // segment 2 mustn't be the same as segment 1 ...
      Pp2a := Pp1a.Next;
      if not FindSegment(Pp2a, Pt3, Pt4) or (Pp2a = Pp1a) then Continue;
    end else
      if not FindSegment(Pp2a, Pt3, Pt4) then Continue;

    if not GetOverlapSegment(Pt1, Pt2, Pt3, Pt4, Pt1, Pt2) then Continue;

    Prev := Pp1a.Prev;
    if PointsEqual(Pp1a.Pt, Pt1) then p1 := Pp1a
    else if PointsEqual(Prev.Pt, Pt1) then p1 := Prev
    else p1 := InsertPolyPtBetween(Pp1a, Prev, Pt1);

    if PointsEqual(Pp1a.Pt, Pt2) then P2 := Pp1a
    else if PointsEqual(Prev.Pt, Pt2) then P2 := Prev
    else if (p1 = Pp1a) or (p1 = Prev) then
      P2 := InsertPolyPtBetween(Pp1a, Prev, Pt2)
    else if Pt3IsBetweenPt1AndPt2(Pp1a.Pt, p1.Pt, Pt2) then
      P2 := InsertPolyPtBetween(Pp1a, p1, Pt2)
    else
      P2 := InsertPolyPtBetween(p1, Prev, Pt2);

    Prev := Pp2a.Prev;
    if PointsEqual(Pp2a.Pt, Pt1) then p3 := Pp2a
    else if PointsEqual(Prev.Pt, Pt1) then p3 := Prev
    else p3 := InsertPolyPtBetween(Pp2a, Prev, Pt1);

    if PointsEqual(Pp2a.Pt, Pt2) then p4 := Pp2a
    else if PointsEqual(Prev.Pt, Pt2) then p4 := Prev
    else if (p3 = Pp2a) or (p3 = Prev) then
      p4 := InsertPolyPtBetween(Pp2a, Prev, Pt2)
    else if Pt3IsBetweenPt1AndPt2(Pp2a.Pt, p3.Pt, Pt2) then
      p4 := InsertPolyPtBetween(Pp2a, p3, Pt2)
    else
      p4 := InsertPolyPtBetween(p3, Prev, Pt2);

    // p1.Pt == p3.Pt and P2.Pt == p4.Pt so join p1 to p3 and P2 to p4 ...
    if (p1.Next = P2) and (p3.Prev = p4) then
    begin
      p1.Next := p3;
      p3.Prev := p1;
      P2.Prev := p4;
      p4.Next := P2;
    end
    else if (p1.Prev = P2) and (p3.Next = p4) then
    begin
      p1.Prev := p3;
      p3.Next := p1;
      P2.Next := p4;
      p4.Prev := P2;
    end
    else
      // it's very rare to get here, and when we do almost invariably
      // p1.Idx == P2.Idx, otherwise it's an orientation error.
      Continue;

    if (Jr.Poly2Idx = Jr.Poly1Idx) then
    begin
      // instead of joining two polygons, we've just created A new one by
      // splitting one polygon into two.
      OutRec1.Pts := GetBottomPt(p1);
      OutRec1.BottomPt := OutRec1.Pts;
      OutRec1.BottomPt.Idx := OutRec1.Idx;
      OutRec2 := CreateOutRec;
      OutRec2.Idx := FPolyOutList.Add(OutRec2);
      Jr.Poly2Idx := OutRec2.Idx;
      OutRec2.Pts := GetBottomPt(P2);
      OutRec2.BottomPt := OutRec2.Pts;
      OutRec2.BottomPt.Idx := OutRec2.Idx;

      if PointInPolygon(OutRec2.Pts.Pt, OutRec1.Pts, FUse64BitRange) then
      begin
        // OutRec2 is contained by OutRec1 ...
        OutRec2.IsHole := not OutRec1.IsHole;
        OutRec2.FirstLeft := OutRec1;
        FixupOutPolygon(OutRec1); //nb: do this before testing orientation
        FixupOutPolygon(OutRec2);
        if (OutRec2.IsHole = FReverseOutput) xor Orientation(OutRec2, FUse64BitRange) then
          ReversePolyPtLinks(OutRec2.Pts);
      end else if PointInPolygon(OutRec1.Pts.Pt, OutRec2.Pts, FUse64BitRange) then
      begin
        // OutRec1 is contained by OutRec2 ...
        OutRec2.IsHole := OutRec1.IsHole;
        OutRec1.IsHole := not OutRec2.IsHole;
        OutRec2.FirstLeft := OutRec1.FirstLeft;
        OutRec1.FirstLeft := OutRec2;
        FixupOutPolygon(OutRec1); //nb: do this before testing orientation
        FixupOutPolygon(OutRec2);
        if (OutRec1.IsHole = FReverseOutput) xor Orientation(OutRec1, FUse64BitRange) then
          ReversePolyPtLinks(OutRec1.Pts);
        // make sure any contained holes now link to the correct polygon ...
        if FixHoleLinkages then CheckHoleLinkages1(OutRec1, OutRec2);
      end else
      begin
        OutRec2.IsHole := OutRec1.IsHole;
        OutRec2.FirstLeft := OutRec1.FirstLeft;
        FixupOutPolygon(OutRec1);
        FixupOutPolygon(OutRec2);
        // make sure any contained holes now link to the correct polygon ...
        if FixHoleLinkages then CheckHoleLinkages1(OutRec1, OutRec2);
      end;

      // now fixup any subsequent joins that match this polygon
      for J := I+1 to FJoinList.count - 1 do
      begin
        Jr2 := FJoinList[J];
        if (Jr2.Poly1Idx = Jr.Poly1Idx) and PointIsVertex(Jr2.Pt1a, P2) then
          Jr2.Poly1Idx := Jr.Poly2Idx;
        if (Jr2.Poly2Idx = Jr.Poly1Idx) and PointIsVertex(Jr2.Pt2a, P2) then
          Jr2.Poly2Idx := Jr.Poly2Idx;
      end;

      if (Orientation(OutRec1, FUse64BitRange) <> (Area(OutRec1, FUse64BitRange) > 0)) then
        DisposeBottomPt(OutRec1);
      if (Orientation(OutRec2, FUse64BitRange) <> (Area(OutRec2, FUse64BitRange) > 0)) then
        DisposeBottomPt(OutRec2);

    end else
    begin
      // joined 2 polygons together ...

      // make sure any holes contained by OutRec2 now link to OutRec1 ...
      if FixHoleLinkages then CheckHoleLinkages2(OutRec1, OutRec2);

      // cleanup edges ...
      FixupOutPolygon(OutRec1);

      if assigned(OutRec1.Pts) then
      begin
        OutRec1.IsHole := not Orientation(OutRec1, FUse64BitRange);
        if OutRec1.IsHole and not assigned(OutRec1.FirstLeft) then
          OutRec1.FirstLeft := OutRec2.FirstLeft;
      end;

      // delete the obsolete pointer ...
      OKIdx := OutRec1.Idx;
      ObsoleteIdx := OutRec2.Idx;
      OutRec2.Pts := nil;
      OutRec2.BottomPt := nil;
      OutRec2.AppendLink := OutRec1;

      // now fixup any subsequent joins ...
      for J := I+1 to FJoinList.count - 1 do
      begin
        Jr2 := FJoinList[J];
        if (Jr2.Poly1Idx = ObsoleteIdx) then Jr2.Poly1Idx := OKIdx;
        if (Jr2.Poly2Idx = ObsoleteIdx) then Jr2.Poly2Idx := OKIdx;
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------
// InflatePolygons ...
//------------------------------------------------------------------------------

function GetUnitNormal(const Pt1, Pt2: TFloatPoint): TDoublePoint;
var
  Dx, Dy, F: TFloat;
begin
  if (Abs(Pt2.X - Pt1.X) < 0.000000001) and
    (Abs(Pt2.Y - Pt1.Y) < 0.000000001) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  Dx := (Pt2.X - Pt1.X);
  Dy := (Pt2.Y - Pt1.Y);
  F := 1 / Hypot(Dx, Dy);
  Dx := Dx * F;
  Dy := Dy * F;
  Result.X := Dy;
  Result.Y := -Dx
end;
//------------------------------------------------------------------------------

function BuildArc(const Pt: TFloatPoint; A1, A2, R: TFloat): TArrayOfFloatPoint;
var
  I, N: Integer;
  A, D: Double;
  Steps: Int64;
  S, C: Extended; // sin & cos
begin
  Steps := Max(4, Round(Sqrt(Abs(R)) * Abs(A2 - A1)));
  if Steps > $100000 then Steps := $100000;
  SetLength(Result, Steps);
  N := Steps - 1;
  D := (A2 - A1) / N;
  A := A1;
  for I := 0 to N do
  begin
    SinCos(A, S, C);
    Result[I].X := Pt.X + C * R;
    Result[I].Y := Pt.Y + S * R;
    A := A + D;
  end;
end;
//------------------------------------------------------------------------------

function GetBounds(const Pts: TArrayOfArrayOfFloatPoint): TFloatRect;
var
  I,J: Integer;
begin
  with Result do
  begin
    Left := CHiRange; Top := CHiRange;
    Right := -CHiRange; Bottom := -CHiRange;
  end;
  for I := 0 to high(Pts) do
    for J := 0 to high(Pts[I]) do
    begin
      if Pts[I][J].X < Result.Left then Result.Left := Pts[I][J].X;
      if Pts[I][J].X > Result.Right then Result.Right := Pts[I][J].X;
      if Pts[I][J].Y < Result.Top then Result.Top := Pts[I][J].Y;
      if Pts[I][J].Y > Result.Bottom then Result.Bottom := Pts[I][J].Y;
    end;
  if Result.left = CHiRange then
    with Result do begin Left := 0; Top := 0; Right := 0; Bottom := 0; end;
end;
//------------------------------------------------------------------------------

function InflatePolygons(const FltPts: TArrayOfArrayOfFloatPoint; const Delta: TFloat;
  JoinType: TJoinType = jtSquare; MiterLimit: TFloat = 2): TArrayOfArrayOfFloatPoint;
var
  I, J, K, Len, OutLen: Integer;
  Normals: TArrayOfDoublePoint;
  R, RMin: Double;
  Pt1, Pt2: TFloatPoint;
  Outer: TArrayOfFloatPoint;
  Bounds: TFloatRect;
const
  BuffLength: Integer = 128;

  procedure AddPoint(const Pt: TFloatPoint);
  var
    Len: Integer;
  begin
    Len := length(Result[I]);
    if OutLen = Len then
      setlength(Result[I], Len + BuffLength);
    Result[I][OutLen] := Pt;
    inc(OutLen);
  end;

  procedure DoSquare(mul: Double = 1.0);
  var
    A1, A2, Dx: Double;
  begin
    Pt1.X := FltPts[I][J].X + Normals[K].X * Delta;
    Pt1.Y := FltPts[I][J].Y + Normals[K].Y * Delta;
    Pt2.X := FltPts[I][J].X + Normals[J].X * Delta;
    Pt2.Y := FltPts[I][J].Y + Normals[J].Y * Delta;
    if ((Normals[K].X*Normals[J].Y-Normals[J].X*Normals[K].Y) * Delta >= 0) then
    begin
      A1 := ArcTan2(Normals[K].Y, Normals[K].X);
      A2 := ArcTan2(-Normals[J].Y, -Normals[J].X);
      A1 := abs(A2 - A1);
      if A1 > Pi then A1 := 2 * pi - A1;
      Dx := Tan((Pi - A1)/4) * Abs(Delta*mul);

      Pt1 := FloatPoint(Pt1.X -Normals[K].Y * Dx, Pt1.Y + Normals[K].X * Dx);
      AddPoint(Pt1);
      Pt2 := FloatPoint(Pt2.X + Normals[J].Y * Dx, Pt2.Y - Normals[J].X * Dx);
      AddPoint(Pt2);
    end else
    begin
      AddPoint(Pt1);
      AddPoint(FltPts[I][J]);
      AddPoint(Pt2);
    end;
  end;

  procedure DoMiter;
  var
    Q: Double;
  begin
    if ((Normals[K].X*Normals[J].Y-Normals[J].X*Normals[K].Y)*Delta >= 0) then
    begin
      Q := Delta / R;
      AddPoint(FloatPoint(FltPts[I][J].X + (Normals[K].X + Normals[J].X) *Q,
        FltPts[I][J].Y + (Normals[K].Y + Normals[J].Y) *Q));
    end else
    begin
      Pt1.X := FltPts[I][J].X + Normals[K].X * Delta;
      Pt1.Y := FltPts[I][J].Y + Normals[K].Y * Delta;
      Pt2.X := FltPts[I][J].X + Normals[J].X * Delta;
      Pt2.Y := FltPts[I][J].Y + Normals[J].Y * Delta;
      AddPoint(Pt1);
      AddPoint(FltPts[I][J]);
      AddPoint(Pt2);
    end;
  end;

  procedure DoRound;
  var
    M: Integer;
    Arc: TArrayOfFloatPoint;
    A1, A2: Double;
  begin
    Pt1.X := FltPts[I][J].X + Normals[K].X * Delta;
    Pt1.Y := FltPts[I][J].Y + Normals[K].Y * Delta;
    Pt2.X := FltPts[I][J].X + Normals[J].X * Delta;
    Pt2.Y := FltPts[I][J].Y + Normals[J].Y * Delta;
    AddPoint(Pt1);
    // round off reflex angles (ie > 180 deg) unless almost flat (ie < 10deg).
    //(N1.X * N2.Y - N2.X * N1.Y) == unit normal "cross product" == sin(angle)
    //(N1.X * N2.X + N1.Y * N2.Y) == unit normal "dot product" == cos(angle)
    // dot product Normals == 1 -> no angle
    if ((Normals[K].X*Normals[J].Y - Normals[J].X*Normals[K].Y)*Delta >= 0) then
    begin
      if ((Normals[J].X*Normals[K].X+Normals[J].Y*Normals[K].Y) < 0.985) then
      begin
        A1 := ArcTan2(Normals[K].Y, Normals[K].X);
        A2 := ArcTan2(Normals[J].Y, Normals[J].X);
        if (Delta > 0) and (A2 < A1) then A2 := A2 + pi*2
        else if (Delta < 0) and (A2 > A1) then A2 := A2 - pi*2;
        Arc := BuildArc(FltPts[I][J], A1, A2, Delta);
        for M := 1 to high(Arc) - 1 do
          AddPoint(Arc[M]);
      end;
    end else
      AddPoint(FltPts[I][J]);
    AddPoint(Pt2);
  end;

begin
  // MiterLimit defaults to twice Delta's width ...
  if MiterLimit <= 1 then MiterLimit := 1;
  RMin := 2 / (Sqr(MiterLimit));

  setLength(Result, length(FltPts));
  for I := 0 to high(FltPts) do
  begin
    Result[I] := nil;
    Len := length(FltPts[I]);
    if (Len > 1) and (FltPts[I][0].X = FltPts[I][Len - 1].X) and
        (FltPts[I][0].Y = FltPts[I][Len - 1].Y) then dec(Len);

    if (Len < 3) and (Delta < 0) then Continue;

    if (Len = 1) then
    begin
      Result[I] := BuildArc(FltPts[I][0], 0, 2 * pi, Delta);
      Continue;
    end;

    // build Normals ...
    SetLength(Normals, Len);
    for J := 0 to Len - 2 do
      Normals[J] := GetUnitNormal(FltPts[I][J], FltPts[I][J + 1]);
    Normals[Len - 1] := GetUnitNormal(FltPts[I][Len - 1], FltPts[I][0]);

    OutLen := 0;
    K := Len - 1;
    for J := 0 to Len - 1 do
    begin
      case JoinType of
        jtMiter:
        begin
          R := 1 + (Normals[J].X * Normals[K].X + Normals[J].Y * Normals[K].Y);
          if (R >= RMin) then
            DoMiter
          else
            DoSquare(MiterLimit);
        end;
        jtSquare: DoSquare;
        jtRound: DoRound;
      end;
      K := J;
    end;
    SetLength(Result[I], OutLen);
  end;

  // finally, clean up untidy corners ...
  with TClipper.Create do
  try
    Add(Result, ptSubject);
    if Delta > 0 then
    begin
      Execute(ctUnion, Result, pftPositive, pftPositive);
    end else
    begin
      Bounds := GetBounds(Result);
      SetLength(Outer, 4);
      Outer[0] := FloatPoint(Bounds.Left - 10, Bounds.Bottom + 10);
      Outer[1] := FloatPoint(Bounds.Right + 10, Bounds.Bottom + 10);
      Outer[2] := FloatPoint(Bounds.Right + 10, Bounds.Top - 10);
      Outer[3] := FloatPoint(Bounds.Left - 10, Bounds.Top - 10);
      Add(Outer, ptSubject);
      Execute(ctUnion, Result, pftNegative, pftNegative);

      // delete the outer rectangle ...
      Len := length(Result);
      for J := 1 to Len - 1 do Result[J - 1] := Result[J];
      if Len > 0 then
        setlength(Result, Len - 1);

      // restore polygon orientation ...
      Result := ReversePolygons(Result);
    end;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function SimplifyPolygon(const Poly: TArrayOfFloatPoint; FillType: TPolyFillType = pftEvenOdd): TArrayOfArrayOfFloatPoint;
begin
  with TClipper.Create do
  try
    Add(Poly, ptSubject);
    Execute(ctUnion, Result, FillType, FillType);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function SimplifyPolygons(const Polys: TArrayOfArrayOfFloatPoint; FillType: TPolyFillType = pftEvenOdd): TArrayOfArrayOfFloatPoint;
begin
  with TClipper.Create do
  try
    Add(Polys, ptSubject);
    Execute(ctUnion, Result, FillType, FillType);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
