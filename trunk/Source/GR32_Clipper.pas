unit GR32_Clipper;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  4.8.9                                                           *
* Date      :  25 September 2012                                               *
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
* Communications of the ACM, Vol 35, Issue 7 (July 1992) pp 56-63.             *
* http://portal.acm.org/citation.cfm?id=129906                                 *
*                                                                              *
* Computer graphics and geometric modeling: implementation and algorithms      *
* By Max K. Agoston                                                            *
* Springer; 1 edition (January 4, 2005)                                        *
* http://books.google.com/books?q=vatti+clipping+agoston                       *
*                                                                              *
* See also:                                                                    *
* "Polygon Offsetting by Computing Winding Numbers"                            *
* Paper no. DETC2005-85513 pp. 565-575                                         *
* ASME 2005 International Design Engineering Technical Conferences             *
* and Computers and Information in Engineering Conference (IDETC/CIE2005)      *
* September 24–28, 2005 , Long Beach, California, USA                          *
* http://www.me.berkeley.edu/~mcmains/pubs/DAC05OffsetPolygon.pdf              *
*                                                                              *
*******************************************************************************)

interface

uses
  Classes, GR32;

type
  TClipType = (ctIntersection, ctUnion, ctDifference, ctXor);
  TPolyType = (ptSubject, ptClip);
  //By far the most widely used winding rules for polygon filling are
  //EvenOdd & NonZero (GDI, GDI+, XLib, OpenGL, Cairo, AGG, Quartz, SVG, Gr32)
  //Others rules include Positive, Negative and ABS_GTR_EQ_TWO (only in OpenGL)
  //see http://glprogramming.com/red/chapter11.html
  TPolyFillType = (pftEvenOdd, pftNonZero, pftPositive, pftNegative);

  //TJoinType - used by OffsetPolygons()
  TJoinType = (jtSquare, jtRound, jtMiter);

  //used internally ...
  TEdgeSide = (esLeft, esRight);
  TEdgeSides = set of TEdgeSide;
  TIntersectProtect = (ipLeft, ipRight);
  TIntersectProtects = set of TIntersectProtect;
  TDirection = (dRightToLeft, dLeftToRight);

  PEdge = ^TEdge;
  TEdge = record
    xbot : TFixed;  //bottom
    ybot : TFixed;
    xcurr: TFixed;  //current (ie relative to bottom of current scanbeam)
    ycurr: TFixed;
    xtop : TFixed;  //top
    ytop : TFixed;
    tmpX :  TFixed;
    dx   : Double;   //the inverse of slope
    polyType : TPolyType;
    side     : TEdgeSide;
    windDelta: Integer; //1 or -1 depending on winding direction
    windCnt  : Integer;
    windCnt2 : Integer;  //winding count of the opposite polytype
    outIdx   : Integer;
    next     : PEdge;
    prev     : PEdge;
    nextInLML: PEdge;
    prevInAEL: PEdge;
    nextInAEL: PEdge;
    prevInSEL: PEdge;
    nextInSEL: PEdge;
  end;

  PEdgeArray = ^TEdgeArray;
  TEdgeArray = array[0.. MaxInt div SizeOf(TEdge) - 1] of TEdge;

  PScanbeam = ^TScanbeam;
  TScanbeam = record
    y   : TFixed;
    next: PScanbeam;
  end;

  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    edge1: PEdge;
    edge2: PEdge;
    pt   : TFixedPoint;
    next : PIntersectNode;
  end;

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    y         : TFixed;
    leftBound : PEdge;
    rightBound: PEdge;
    next      : PLocalMinima;
  end;

  POutPt = ^TOutPt;

  POutRec = ^TOutRec;
  TOutRec = record
    idx         : Integer;
    bottomPt    : POutPt;
    isHole      : Boolean;
    FirstLeft   : POutRec;
    AppendLink  : POutRec;
    Pts         : POutPt;
    sides       : TEdgeSides;
    bottomFlag  : POutPt;
  end;
  TArrayOfOutRec = array of POutRec;

  TOutPt = record
    idx      : Integer;
    pt       : TFixedPoint;
    next     : POutPt;
    prev     : POutPt;
  end;

  PJoinRec = ^TJoinRec;
  TJoinRec = record
    pt1a     : TFixedPoint;
    pt1b     : TFixedPoint;
    poly1Idx : Integer;
    pt2a     : TFixedPoint;
    pt2b     : TFixedPoint;
    poly2Idx : Integer;
  end;

  PHorzRec = ^THorzRec;
  THorzRec = record
    edge     : PEdge;
    savedIdx : Integer;
    next     : PHorzRec;
    prev     : PHorzRec;
  end;

  TClipperBase = class
  private
    FEdgeList      : TList;
    FLmList        : PLocalMinima; //localMinima list
    FCurrLm        : PLocalMinima; //current localMinima node
    FUse64BitRange : Boolean;      //see CLoRange and hiRange consts notes below
    procedure DisposeLocalMinimaList;
  protected
    procedure Reset; virtual;
    procedure PopLocalMinima;
    property CurrentLm: PLocalMinima read FCurrLm;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddArrayOfFixedPoint(const afp: TArrayOfFixedPoint; polyType: TPolyType): Boolean;
    function AddArrayOfArrayOfFixedPoint(const aafp: TArrayOfArrayOfFixedPoint; polyType: TPolyType): Boolean;
    procedure Clear; virtual;
  end;

  TClipper = class(TClipperBase)
  private
    FPolyOutList   : TList;
    FJoinList      : TList;
    FClipType      : TClipType;
    FScanBeam      : PScanbeam; //scanbeam list
    FActiveEdges   : PEdge;     //active edge list
    FSortedEdges   : PEdge;     //used for temporary sorting
    FIntersectNodes: PIntersectNode;
    FClipFillType  : TPolyFillType;
    FSubjFillType  : TPolyFillType;
    FExecuteLocked : Boolean;
    FHorizJoins    : PHorzRec;
    procedure DisposeScanbeamList;
    procedure InsertScanbeam(const y: TFixed);
    function PopScanbeam: TFixed;
    procedure SetWindingCount(edge: PEdge);
    function IsEvenOddFillType(edge: PEdge): Boolean;
    function IsEvenOddAltFillType(edge: PEdge): Boolean;
    procedure AddEdgeToSEL(edge: PEdge);
    procedure CopyAELToSEL;
    procedure InsertLocalMinimaIntoAEL(const botY: TFixed);
    procedure SwapPositionsInAEL(e1, e2: PEdge);
    procedure SwapPositionsInSEL(e1, e2: PEdge);
    function IsTopHorz(const XPos: TFixed): Boolean;
    procedure ProcessHorizontal(horzEdge: PEdge);
    procedure ProcessHorizontals;
    procedure AddIntersectNode(e1, e2: PEdge; const pt: TFixedPoint);
    function ProcessIntersections(const botY, topY: TFixed): Boolean;
    procedure BuildIntersectList(const botY, topY: TFixed);
    procedure ProcessIntersectList;
    procedure DeleteFromAEL(e: PEdge);
    procedure DeleteFromSEL(e: PEdge);
    procedure IntersectEdges(e1,e2: PEdge;
      const pt: TFixedPoint; protects: TIntersectProtects = []);
    procedure DoMaxima(e: PEdge; const topY: TFixed);
    procedure UpdateEdgeIntoAEL(var e: PEdge);
    function FixupIntersections: Boolean;
    procedure SwapIntersectNodes(int1, int2: PIntersectNode);
    procedure ProcessEdgesAtTopOfScanbeam(const topY: TFixed);
    function IsContributing(edge: PEdge): Boolean;
    function CreateOutRec: POutRec;
    procedure AddOutPt(e: PEdge; const pt: TFixedPoint);
    procedure AddLocalMaxPoly(e1, e2: PEdge; const pt: TFixedPoint);
    procedure AddLocalMinPoly(e1, e2: PEdge; const pt: TFixedPoint);
    procedure AppendPolygon(e1, e2: PEdge);
    procedure DisposeBottomPt(outRec: POutRec);
    procedure DisposePolyPts(pp: POutPt);
    procedure DisposeAllPolyPts;
    procedure DisposeOutRec(index: Integer);
    procedure DisposeIntersectNodes;
    function GetResult: TArrayOfArrayOfFixedPoint;
    procedure FixupOutPolygon(outRec: POutRec);
    procedure SetHoleState(e: PEdge; outRec: POutRec);
    procedure AddJoin(e1, e2: PEdge;
      e1OutIdx: Integer = -1; e2OutIdx: Integer = -1);
    procedure ClearJoins;
    procedure AddHorzJoin(e: PEdge; idx: Integer);
    procedure ClearHorzJoins;
    procedure CheckHoleLinkages1(const outRec1, outRec2: POutRec);
    procedure CheckHoleLinkages2(const outRec1, outRec2: POutRec);
    procedure JoinCommonEdges(fixHoleLinkages: Boolean);
    procedure FixHoleLinkage(outRec: POutRec);
  protected
    procedure Reset; override;
    function ExecuteInternal(fixHoleLinkages: Boolean): Boolean; virtual;
  public
    function Execute(
      ClipType: TClipType;
      out Solution: TArrayOfArrayOfFixedPoint;
      SubjFillType: TPolyFillType = pftEvenOdd;
      ClipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;

    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
  end;

function Orientation(const Pts: TArrayOfFixedPoint): Boolean; overload;

//OffsetPolygons precondition: Outer polygons MUST be oriented clockwise,
//and inner 'hole' polygons MUST be oriented counter-clockwise ...
function OffsetPolygons(const aaFxPts: TArrayOfArrayOfFixedPoint; const Delta: TFixed;
  JoinType: TJoinType = jtSquare; MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint;

implementation

uses
  SysUtils, Types, Math, GR32_VectorUtils;

type
  TDoublePoint = record X, Y: Double; end;
  TArrayOfDoublePoint = array of TDoublePoint;

const
  CHorizontal: Double = -3.4e+38;
  CLoRange: TFixed = $3FFFFFFF;

resourcestring
  rsMissingRightbound = 'InsertLocalMinimaIntoAEL: missing rightbound';
  rsDoMaxima = 'DoMaxima error';
  rsUpdateEdgeIntoAEL = 'UpdateEdgeIntoAEL error';
  rsHorizontal = 'ProcessHorizontal error';
  rsInvalidInt = 'Coordinate exceeds range bounds';
  rsJoinError = 'Join Output polygons error';
  rsHoleLinkError = 'HoleLinkage error';

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function PointCount(Pts: POutPt): Integer;
var
  p: POutPt;
begin
  Result := 0;
  if not Assigned(Pts) then Exit;
  p := Pts;
  repeat
    Inc(Result);
    p := p.next;
  until p = Pts;
end;
//------------------------------------------------------------------------------

function PointsEqual(const P1, P2: TFixedPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;
//------------------------------------------------------------------------------

function FixedPoint2(const X, Y: TFixed): TFixedPoint; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Orientation(const Pts: TArrayOfFixedPoint): Boolean; overload;
var
  i, j, jplus, jminus, highI: Integer;
  vec1X, vec2X, vec1Y, vec2Y: Int64;
begin
  Result := True;
  highI := High(Pts);
  if highI < 2 then Exit;
  j := 0;
  for i := 0 to highI do
  begin
    if (Pts[i].Y < Pts[j].Y) then Continue;
    if ((Pts[i].Y > Pts[j].Y) or (Pts[i].X < Pts[j].X)) then j := i;
  end;
  if j = highI then jplus := 0
  else jplus := j + 1;
  if j = 0 then jminus := highI
  else jminus := j - 1;

  //get cross product of vectors of edges adjacent the point with largest Y ...
  vec1X := Pts[j].X - Pts[jminus].X;
  vec1Y := Pts[j].Y - Pts[jminus].Y;
  vec2X := Pts[jplus].X - Pts[j].X;
  vec2Y := Pts[jplus].Y - Pts[j].Y;
  Result := ((vec1X * vec2Y) - (vec2X * vec1Y)) >= 0;
end;
//------------------------------------------------------------------------------

function Orientation(outRec: POutRec): Boolean; overload;
var
  op, opBottom, opPrev, opNext: POutPt;
  vec1X, vec2X, vec1Y, vec2Y: Int64;
begin
  //first make sure bottomPt is correctly Assigned ...
  opBottom := outRec.Pts;
  op := opBottom.next;
  while op <> outRec.Pts do
  begin
    if op.pt.Y >= opBottom.pt.Y then
    begin
      if (op.pt.Y > opBottom.pt.Y) or (op.pt.X < opBottom.pt.X) then
        opBottom := op;
    end;
    op := op.next;
  end;
  outRec.bottomPt := opBottom;
  opBottom.idx := outRec.idx;
  op := opBottom;

  //find vertices either side of bottomPt (skipping duplicate points) ....
  opPrev := op.prev;
  while (op <> opPrev) and PointsEqual(op.pt, opPrev.pt) do
    opPrev := opPrev.prev;
  opNext := op.next;
  while (op <> opNext) and PointsEqual(op.pt, opNext.pt) do
    opNext := opNext.next;

  vec1X := op.pt.X - opPrev.pt.X;
  vec1Y := op.pt.Y - opPrev.pt.Y;
  vec2X := opNext.pt.X - op.pt.X;
  vec2Y := opNext.pt.Y - op.pt.Y;

  //perform cross product to determine left or right 'turning' ...
  Result := ((vec1X * vec2Y) - (vec2X * vec1Y)) >= 0;
end;
//------------------------------------------------------------------------------

function Area(const Pts: TArrayOfFixedPoint): Double; overload;
var
  i, highI: Integer;
  d: Double;
begin
  Result := 0;
  highI := High(Pts);
  if highI < 2 then Exit;
  d := Pts[highI].X * Pts[0].Y - Pts[0].X * Pts[highI].Y;
  for i := 1 to highI do
    d := d + (Pts[i - 1].X * Pts[i].Y) - (Pts[i].X * Pts[i - 1].Y);
  Result := d / 2;
end;
//------------------------------------------------------------------------------

function Area(outRec: POutRec): Double; overload;
var
  op: POutPt;
  d: Double;
begin
  op := outRec.Pts;
  d := 0;
  repeat
    d := d + (op.pt.X * op.next.pt.Y) - (op.next.pt.X * op.pt.Y);
    op := op.next;
  until op = outRec.Pts;
  Result := d / 2;
end;
//------------------------------------------------------------------------------

function ReversePolygons(const Pts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFixedPoint;
var
  i, j, highJ: Integer;
begin
  i := Length(Pts);
  SetLength(Result, i);
  for i := 0 to i - 1 do
  begin
    highJ := High(Pts[i]);
    SetLength(Result[i], highJ + 1);
    for j := 0 to highJ do
      Result[i, j] := Pts[i, highJ - j];
  end;
end;
//------------------------------------------------------------------------------

function PointIsVertex(const pt: TFixedPoint; pp: POutPt): Boolean;
var
  pp2: POutPt;
begin
  Result := True;
  pp2 := pp;
  repeat
    if PointsEqual(pp2.pt, pt) then Exit;
    pp2 := pp2.next;
  until pp2 = pp;
  Result := False;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TFixedPoint; pp: POutPt): Boolean;
var
  pp2: POutPt;
begin
  Result := False;
  pp2 := pp;
  repeat
    if ((((pp2.pt.Y <= pt.Y) and (pt.Y < pp2.prev.pt.Y)) or
      ((pp2.prev.pt.Y <= pt.Y) and (pt.Y < pp2.pt.Y))) and
      (pt.X < (pp2.prev.pt.X - pp2.pt.X) * (pt.Y - pp2.pt.Y) /
      (pp2.prev.pt.Y - pp2.pt.Y) + pp2.pt.X)) then Result := not Result;
    pp2 := pp2.next;
  until pp2 = pp;
end;
//------------------------------------------------------------------------------

function SlopesEqual(e1, e2: PEdge): Boolean; overload;
var
  Dx1, Dy1, Dx2, Dy2: Int64;
begin
  Dx1 := e1.xtop - e1.xbot;
  Dy1 := e1.ytop - e1.ybot;
  Dx2 := e2.xtop - e2.xbot;
  Dy2 := e2.ytop - e2.ybot;
  Result := Dy1 * Dx2 = Dx1 * Dy2;
end;
//---------------------------------------------------------------------------

function SlopesEqual(const pt1, pt2, pt3: TFixedPoint): Boolean; overload;
var
  Dx1, Dy1, Dx2, Dy2: Int64;
begin
  Dx1 := pt1.X - pt2.X;
  Dy1 := pt1.Y - pt2.Y;
  Dx2 := pt2.X - pt3.X;
  Dy2 := pt2.Y - pt3.Y;
  Result := Dy1 * Dx2 = Dx1 * Dy2;
end;
//---------------------------------------------------------------------------

function SlopesEqual(const pt1, pt2, pt3, pt4: TFixedPoint): Boolean; overload;
var
  Dx1, Dy1, Dx2, Dy2: Int64;
begin
  Dx1 := pt1.X - pt2.X;
  Dy1 := pt1.Y - pt2.Y;
  Dx2 := pt3.X - pt4.X;
  Dy2 := pt3.Y - pt4.Y;
  Result := Dy1 * Dx2 = Dx1 * Dy2;
end;
//---------------------------------------------------------------------------

//                 0(90º)                                                  //
//                 |                                                       //
// +inf (180º) --- o --- -inf (0º)                                         //
function GetDx(const pt1, pt2: TFixedPoint): Double;
begin
  if (pt1.Y = pt2.Y) then Result := CHorizontal
  else Result := (pt2.X - pt1.X)/(pt2.Y - pt1.Y);
end;
//---------------------------------------------------------------------------

procedure SetDx(e: PEdge);
begin
  if (e.ybot = e.ytop) then e.dx := CHorizontal
  else e.dx := (e.xtop - e.xbot)/(e.ytop - e.ybot);
end;
//---------------------------------------------------------------------------

procedure SwapSides(edge1, edge2: PEdge);
var
  side: TEdgeSide;
begin
  side :=  edge1.side;
  edge1.side := edge2.side;
  edge2.side := side;
end;
//------------------------------------------------------------------------------

procedure SwapPolyIndexes(edge1, edge2: PEdge);
var
  outIdx: Integer;
begin
  outIdx :=  edge1.outIdx;
  edge1.outIdx := edge2.outIdx;
  edge2.outIdx := outIdx;
end;
//------------------------------------------------------------------------------

function TopX(edge: PEdge; const currentY: TFixed): TFixed; overload;
begin
  if currentY = edge.ytop then Result := edge.xtop
  else if edge.xtop = edge.xbot then Result := edge.xbot
  else Result := edge.xbot + Round(edge.dx*(currentY - edge.ybot));
end;
//------------------------------------------------------------------------------

function IntersectPoint(edge1, edge2: PEdge; out ip: TFixedPoint): Boolean; overload;
var
  b1,b2: Double;
begin
  if SlopesEqual(edge1, edge2) then
  begin
    Result := False;
    Exit;
  end;
  if edge1.dx = 0 then
  begin
    ip.X := edge1.xbot;
    if edge2.dx = CHorizontal then
      ip.Y := edge2.ybot
    else
    begin
      with edge2^ do b2 := ybot - (xbot/dx);
      ip.Y := Round(ip.X/edge2.dx + b2);
    end;
  end
  else if edge2.dx = 0 then
  begin
    ip.X := edge2.xbot;
    if edge1.dx = CHorizontal then
      ip.Y := edge1.ybot
    else
    begin
      with edge1^ do b1 := ybot - (xbot/dx);
      ip.Y := Round(ip.X/edge1.dx + b1);
    end;
  end else
  begin
    with edge1^ do b1 := xbot - ybot *dx;
    with edge2^ do b2 := xbot - ybot *dx;
    b2 := (b2-b1)/(edge1.dx - edge2.dx);
    ip.Y := Round(b2);
    ip.X := Round(edge1.dx * b2 + b1);
  end;

  //The precondition - e.tmpX > eNext.tmpX - indicates that the two edges do
  //intersect below topY (and hence below the tops of either edge). However,
  //when edges are almost parallel, rounding errors may cause False positives -
  //indicating intersections when there really aren't any. Also, floating point
  //imprecision can incorrectly place an intersect point beyond/above an edge.
  //Therfore, further validation of the IP is warranted ...
  if (ip.Y < edge1.ytop) or (ip.Y < edge2.ytop) then
  begin
    //Find the lower top of the two edges and compare X's at this Y.
    //If edge1's X is greater than edge2's X then it's fair to assume an
    //intersection really has occurred...
    if (edge1.ytop > edge2.ytop) then
    begin
      Result := TopX(edge2, edge1.ytop) < edge1.xtop;
      ip.X := edge1.xtop;
      ip.Y := edge1.ytop;
    end else
    begin
      Result := TopX(edge1, edge2.ytop) > edge2.xtop;
      ip.X := edge2.xtop;
      ip.Y := edge2.ytop;
    end;
  end else
    Result := True;
end;
//------------------------------------------------------------------------------

procedure ReversePolyPtLinks(pp: POutPt);
var
  pp1,pp2: POutPt;
begin
  pp1 := pp;
  repeat
    pp2:= pp1.next;
    pp1.next := pp1.prev;
    pp1.prev := pp2;
    pp1 := pp2;
  until pp1 = pp;
end;

//------------------------------------------------------------------------------
// TClipperBase methods ...
//------------------------------------------------------------------------------

constructor TClipperBase.Create;
begin
  FEdgeList := TList.Create;
  FLmList := nil;
  FCurrLm := nil;
  FUse64BitRange := False; //ie default is False
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  FEdgeList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddArrayOfFixedPoint(
  const afp: TArrayOfFixedPoint; polyType: TPolyType): Boolean;

  //----------------------------------------------------------------------

  procedure InitEdge(e, eNext, ePrev: PEdge; const pt: TFixedPoint);
  begin
    FillChar(e^, SizeOf(TEdge), 0);
    e.next := eNext;
    e.prev := ePrev;
    e.xcurr := pt.X;
    e.ycurr := pt.Y;
    if e.ycurr >= e.next.ycurr then
    begin
      e.xbot := e.xcurr;
      e.ybot := e.ycurr;
      e.xtop := e.next.xcurr;
      e.ytop := e.next.ycurr;
      e.windDelta := 1;
    end else
    begin
      e.xtop := e.xcurr;
      e.ytop := e.ycurr;
      e.xbot := e.next.xcurr;
      e.ybot := e.next.ycurr;
      e.windDelta := -1;
    end;
    SetDx(e);
    e.polyType := polyType;
    e.outIdx := -1;
  end;
  //----------------------------------------------------------------------

  procedure SwapX(e: PEdge);
  begin
    //swap CHorizontal edges' top and bottom x's so they follow the natural
    //progression of the Bounds - ie so their xbots will align with the
    //adjoining lower edge. [Helpful in the ProcessHorizontal() method.]
    e.xcurr := e.xtop;
    e.xtop := e.xbot;
    e.xbot := e.xcurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertLocalMinima(lm: PLocalMinima);
  var
    tmpLm: PLocalMinima;
  begin
    if not Assigned(FLmList) then
    begin
      FLmList := lm;
    end
    else if (lm.y >= FLmList.y) then
    begin
      lm.next := FLmList;
      FLmList := lm;
    end else
    begin
      tmpLm := FLmList;
      while Assigned(tmpLm.next) and (lm.y < tmpLm.next.y) do
          tmpLm := tmpLm.next;
      lm.next := tmpLm.next;
      tmpLm.next := lm;
    end;
  end;
  //----------------------------------------------------------------------

  function AddBoundsToLML(e: PEdge): PEdge;
  var
    newLm: PLocalMinima;
  begin
    //Starting at the top of one bound we progress to the bottom where there's
    //a local minima. We then go to the top of the next bound. These two Bounds
    //form the left and right (or right and left) Bounds of the local minima.
    e.nextInLML := nil;
    e := e.next;
    repeat
      if e.dx = CHorizontal then
      begin
        //nb: proceed through horizontals when approaching from their right,
        //    but break on CHorizontal minima if approaching from their left.
        //    This ensures 'local minima' are always on the left of horizontals.
        if (e.next.ytop < e.ytop) and (e.next.xbot > e.prev.xbot) then break;
        if (e.xtop <> e.prev.xbot) then SwapX(e);
        //e.windDelta := 0; safe option to consider when redesigning
        e.nextInLML := e.prev;
      end
      else if (e.ybot = e.prev.ybot) then break
      else e.nextInLML := e.prev;
      e := e.next;
    until False;

    //e and e.prev are now at a local minima ...
    New(newLm);
    newLm.y := e.prev.ybot;
    newLm.next := nil;
    if e.dx = CHorizontal then //CHorizontal edges never start a left bound
    begin
      if (e.xbot <> e.prev.xbot) then SwapX(e);
      newLm.leftBound := e.prev;
      newLm.rightBound := e;
    end else if (e.dx < e.prev.dx) then
    begin
      newLm.leftBound := e.prev;
      newLm.rightBound := e;
    end else
    begin
      newLm.leftBound := e;
      newLm.rightBound := e.prev;
    end;
    newLm.leftBound.side := esLeft;
    newLm.rightBound.side := esRight;

    InsertLocalMinima(newLm);
    //now process the ascending bound ....
    repeat
      if (e.next.ytop = e.ytop) and not (e.next.dx = CHorizontal) then break;
      e.nextInLML := e.next;
      e := e.next;
      if (e.dx = CHorizontal) and (e.xbot <> e.prev.xtop) then SwapX(e);
    until False;
    Result := e.next;
  end;
  //----------------------------------------------------------------------

var
  i, j, Len: Integer;
  edges: PEdgeArray;
  e, eHighest: PEdge;
  pg: TArrayOfFixedPoint;
begin
  Result := False; //ie assume nothing added
  Len := Length(afp);
  if Len < 3 then Exit;
  SetLength(pg, Len);
  pg[0].X := afp[0].X;
  pg[0].Y := afp[0].Y;
  j := 0;
  //1. check that coordinate values are within the valid range, and
  //2. remove duplicate points and co-linear points
  for i := 1 to Len - 1 do
  begin
    if ((abs(afp[i].X) > CLoRange) or (abs(afp[i].Y) > CLoRange)) then
      raise Exception.Create(rsInvalidInt);
    if (pg[j].X = afp[i].X) and (pg[j].Y = afp[i].Y) then Continue
    else if (j > 0) and SlopesEqual(pg[j - 1], pg[j], afp[i]) then
    begin
      if (pg[j - 1].X = afp[i].X) and (pg[j -1].Y = afp[i].Y) then Dec(j);
    end else Inc(j);
    pg[j].X := afp[i].X;
    pg[j].Y := afp[i].Y;
  end;
  if (j < 2) then Exit;

  //now remove duplicate points and co-linear edges at the loop around of the
  //start and end coordinates ...
  Len := j + 1;
  while Len > 2 do
  begin
    //nb: test for point equality before testing slopes ...
    if PointsEqual(pg[j], pg[0]) then Dec(j)
    else if PointsEqual(pg[0], pg[1]) or SlopesEqual(pg[j], pg[0], pg[1]) then
    begin
      pg[0] := pg[j];
      Dec(j);
    end
    else if SlopesEqual(pg[j - 1], pg[j], pg[0]) then Dec(j)
    else if SlopesEqual(pg[0], pg[1], pg[2]) then
    begin
      for i := 2 to j do pg[i - 1] := pg[i];
      Dec(j);
    end
    else
      break;
    Dec(Len);
  end;
  if Len < 3 then Exit;
  Result := True;

  GetMem(edges, SizeOf(TEdge)*Len);
  FEdgeList.Add(edges);

  //convert vertices to a Double-linked-list of edges and initialize ...
  edges[0].xcurr := pg[0].X;
  edges[0].ycurr := pg[0].Y;
  InitEdge(@edges[Len - 1], @edges[0], @edges[Len - 2], pg[Len - 1]);
  for i := Len - 2 downto 1 do
    InitEdge(@edges[i], @edges[i + 1], @edges[i - 1], pg[i]);
  InitEdge(@edges[0], @edges[1], @edges[Len - 1], pg[0]);
  //reset xcurr & ycurr and find the 'highest' edge. (nb: since I'm much more
  //familiar with positive downwards Y axes, 'highest' here will be the edge
  //with the *smallest* ytop.)
  e := @edges[0];
  eHighest := e;
  repeat
    e.xcurr := e.xbot;
    e.ycurr := e.ybot;
    if e.ytop < eHighest.ytop then eHighest := e;
    e := e.next;
  until e = @edges[0];

  //make sure eHighest is positioned so the following loop works safely ...
  if eHighest.windDelta > 0 then eHighest := eHighest.next;
  if (eHighest.dx = CHorizontal) then eHighest := eHighest.next;

  //finally insert each local minima ...
  e := eHighest;
  repeat
    e := AddBoundsToLML(e);
  until (e = eHighest);
end;
//------------------------------------------------------------------------------

function TClipperBase.AddArrayOfArrayOfFixedPoint(
  const aafp: TArrayOfArrayOfFixedPoint; polyType: TPolyType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(aafp) do
    if AddArrayOfFixedPoint(aafp[i], polyType) then Result := True;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
var
  i: Integer;
begin
  DisposeLocalMinimaList;
  for i := 0 to FEdgeList.Count - 1 do dispose(PEdgeArray(FEdgeList[i]));
  FEdgeList.Clear;
  FUse64BitRange := False;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Reset;
var
  e: PEdge;
  lm: PLocalMinima;
begin
  //Reset() allows various clipping operations to be executed
  //multiple times on the same polygon sets.

  FCurrLm := FLmList;
  //reset all edges ...
  lm := FCurrLm;
  while Assigned(lm) do
  begin
    e := lm.leftBound;
    while Assigned(e) do
    begin
      e.xcurr := e.xbot;
      e.ycurr := e.ybot;
      e.side := esLeft;
      e.outIdx := -1;
      e := e.nextInLML;
    end;
    e := lm.rightBound;
    while Assigned(e) do
    begin
      e.xcurr := e.xbot;
      e.ycurr := e.ybot;
      e.side := esRight;
      e.outIdx := -1;
      e := e.nextInLML;
    end;
    lm := lm.next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.DisposeLocalMinimaList;
begin
  while Assigned(FLmList) do
  begin
    FCurrLm := FLmList.next;
    Dispose(FLmList);
    FLmList := FCurrLm;
  end;
  FCurrLm := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.PopLocalMinima;
begin
  if not Assigned(FCurrLm) then Exit;
  FCurrLm := FCurrLm.next;
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
  inherited; //this must be first since inherited Destroy calls Clear.
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
  sb: PScanbeam;
begin
  while Assigned(FScanBeam) do
  begin
    sb := FScanBeam.next;
    Dispose(FScanBeam);
    FScanBeam := sb;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  lm: PLocalMinima;
begin
  inherited Reset;
  FScanBeam := nil;
  DisposeAllPolyPts;
  lm := FLmList;
  while Assigned(lm) do
  begin
    InsertScanbeam(lm.y);
    InsertScanbeam(lm.leftbound.ytop);
    lm := lm.next;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(
  ClipType: TClipType;
  out Solution: TArrayOfArrayOfFixedPoint;
  SubjFillType: TPolyFillType = pftEvenOdd;
  ClipFillType: TPolyFillType = pftEvenOdd): Boolean;
begin
  Result := False;
  Solution := nil;
  if FExecuteLocked then Exit;
  try
    FExecuteLocked := True;
    FSubjFillType := SubjFillType;
    FClipFillType := ClipFillType;
    FClipType := ClipType;
    Result := ExecuteInternal(False);
    if Result then Solution := GetResult;
  finally
    FExecuteLocked := False;
  end;
end;
//------------------------------------------------------------------------------

function PolySort(item1, item2: pointer): Integer;
var
  p1, p2: POutRec;
  i1, i2: Integer;
begin
  Result := 0;
  if item1 = item2 then Exit;
  p1 := item1; p2 := item2;
  if not Assigned(p1.Pts) or not Assigned(p2.Pts) then
  begin
    if Assigned(p1.Pts) then Result := -1
    else if Assigned(p2.Pts) then Result := 1;
    Exit;
  end;
  if p1.isHole then i1 := p1.FirstLeft.idx
  else i1 := p1.idx;
  if p2.isHole then i2 := p2.FirstLeft.idx
  else i2 := p2.idx;
  Result := i1 - i2;
  if (Result = 0) and (p1.isHole <> p2.isHole) then
  begin
    if p1.isHole then Result := 1
    else Result := -1;
  end;
end;
//------------------------------------------------------------------------------

function FindAppendLinkEnd(outRec: POutRec): POutRec;
begin
  while Assigned(outRec.AppendLink) do
    outRec := outRec.AppendLink;
  Result := outRec;
end;
//------------------------------------------------------------------------------

procedure TClipper.FixHoleLinkage(outRec: POutRec);
var
  tmp: POutRec;
begin
  if Assigned(outRec.bottomPt) then
    tmp := POutRec(FPolyOutList[outRec.bottomPt.idx]).FirstLeft else
    tmp := outRec.FirstLeft;
    if (outRec = tmp) then
      raise Exception.Create(rsHoleLinkError);

  if Assigned(tmp) then
  begin
    if Assigned(tmp.AppendLink) then
      tmp := FindAppendLinkEnd(tmp);
    if tmp = outRec then tmp := nil
    else if tmp.isHole then
    begin
      FixHoleLinkage(tmp);
      tmp := tmp.FirstLeft;
    end;
  end;
  outRec.FirstLeft := tmp;
  if not Assigned(tmp) then outRec.isHole := False;
  outRec.AppendLink := nil;
end;
//------------------------------------------------------------------------------

function TClipper.ExecuteInternal(fixHoleLinkages: Boolean): Boolean;
var
  i: Integer;
  outRec: POutRec;
  botY, topY: TFixed;
begin
  Result := False;
  try try
    Reset;
    if not Assigned(FScanBeam) then
    begin
      Result := True;
      Exit;
    end;

    botY := PopScanbeam;
    repeat
      InsertLocalMinimaIntoAEL(botY);
      ClearHorzJoins;
      ProcessHorizontals;
      topY := PopScanbeam;
      if not ProcessIntersections(botY, topY) then Exit;
      ProcessEdgesAtTopOfScanbeam(topY);
      botY := topY;
    until FScanBeam = nil;

    //tidy up output polygons and fix orientations where necessary ...
    for i := 0 to FPolyOutList.Count - 1 do
    begin
      outRec := FPolyOutList[i];
      if not Assigned(outRec.Pts) then Continue;
      FixupOutPolygon(outRec);
      if not Assigned(outRec.Pts) then Continue;

      if outRec.isHole and fixHoleLinkages then
        FixHoleLinkage(outRec);
      //outRec.bottomPt might've been cleaned up already so retest orientation
      if (outRec.bottomPt = outRec.bottomFlag) and
        (Orientation(outRec) <> (Area(outRec) > 0)) then
          DisposeBottomPt(outRec);
      if outRec.isHole = Orientation(outRec) then
          ReversePolyPtLinks(outRec.Pts);
    end;
    if FJoinList.count > 0 then
      JoinCommonEdges(fixHoleLinkages);

    if fixHoleLinkages then FPolyOutList.Sort(PolySort);
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

procedure TClipper.InsertScanbeam(const y: TFixed);
var
  sb, sb2: PScanbeam;
begin
  New(sb);
  sb.y := y;
  if not Assigned(FScanBeam) then
  begin
    FScanBeam := sb;
    sb.next := nil;
  end else if y > FScanBeam.y then
  begin
    sb.next := FScanBeam;
    FScanBeam := sb;
  end else
  begin
    sb2 := FScanBeam;
    while Assigned(sb2.next) and (y <= sb2.next.y) do sb2 := sb2.next;
    if y <> sb2.y then
    begin
      sb.next := sb2.next;
      sb2.next := sb;
    end
    else dispose(sb); //ie ignores duplicates
  end;
end;
//------------------------------------------------------------------------------

function TClipper.PopScanbeam: TFixed;
var
  sb: PScanbeam;
begin
  Result := FScanBeam.y;
  sb := FScanBeam;
  FScanBeam := FScanBeam.next;
  dispose(sb);
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeBottomPt(outRec: POutRec);
var
  next, prev: POutPt;
begin
  next := outRec.bottomPt.next;
  prev := outRec.bottomPt.prev;
  if outRec.Pts = outRec.bottomPt then
    outRec.Pts := next;
  dispose(outRec.bottomPt);
  next.prev := prev;
  prev.next := next;
  outRec.bottomPt := next;
  FixupOutPolygon(outRec);
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposePolyPts(pp: POutPt);
var
  tmpPp: POutPt;
begin
  pp.prev.next := nil;
  while Assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.next;
    dispose(tmpPp);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeAllPolyPts;
var
  i: Integer;
begin
  for i := 0 to FPolyOutList.Count - 1 do DisposeOutRec(i);
  FPolyOutList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(index: Integer);
var
  outRec: POutRec;
begin
  outRec := FPolyOutList[index];
  if Assigned(outRec.Pts) then DisposePolyPts(outRec.Pts);
  Dispose(outRec);
  FPolyOutList[index] := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindingCount(edge: PEdge);
var
  e: PEdge;
begin
  e := edge.prevInAEL;
  //find the edge of the same polytype that immediately preceeds 'edge' in AEL
  while Assigned(e) and (e.polyType <> edge.polyType) do e := e.prevInAEL;
  if not Assigned(e) then
  begin
    edge.windCnt := edge.windDelta;
    edge.windCnt2 := 0;
    e := FActiveEdges; //ie get ready to calc windCnt2
  end else if IsEvenOddFillType(edge) then
  begin
    //even-odd filling ...
    edge.windCnt := 1;
    edge.windCnt2 := e.windCnt2;
    e := e.nextInAEL; //ie get ready to calc windCnt2
  end else
  begin
    //NonZero, Positive, or Negative filling ...
    if e.windCnt * e.windDelta < 0 then
    begin
      if (abs(e.windCnt) > 1) then
      begin
        if (e.windDelta * edge.windDelta < 0) then edge.windCnt := e.windCnt
        else edge.windCnt := e.windCnt + edge.windDelta;
      end else
        edge.windCnt := e.windCnt + e.windDelta + edge.windDelta;
    end else
    begin
      if (abs(e.windCnt) > 1) and (e.windDelta * edge.windDelta < 0) then
        edge.windCnt := e.windCnt
      else if e.windCnt + edge.windDelta = 0 then
        edge.windCnt := e.windCnt
      else edge.windCnt := e.windCnt + edge.windDelta;
    end;
    edge.windCnt2 := e.windCnt2;
    e := e.nextInAEL; //ie get ready to calc windCnt2
  end;

  //update windCnt2 ...
  if IsEvenOddAltFillType(edge) then
  begin
    //even-odd filling ...
    while (e <> edge) do
    begin
      if edge.windCnt2 = 0 then edge.windCnt2 := 1 else edge.windCnt2 := 0;
      e := e.nextInAEL;
    end;
  end else
  begin
    //NonZero, Positive, or Negative filling ...
    while (e <> edge) do
    begin
      Inc(edge.windCnt2, e.windDelta);
      e := e.nextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddFillType(edge: PEdge): Boolean;
begin
  if edge.polyType = ptSubject then
    Result := FSubjFillType = pftEvenOdd else
    Result := FClipFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddAltFillType(edge: PEdge): Boolean;
begin
  if edge.polyType = ptSubject then
    Result := FClipFillType = pftEvenOdd else
    Result := FSubjFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributing(edge: PEdge): Boolean;
var
  pft,pft2: TPolyFillType;
begin
  if edge.polyType = ptSubject then
  begin
    pft := FSubjFillType;
    pft2 := FClipFillType;
  end else
  begin
    pft := FClipFillType;
    pft2 := FSubjFillType
  end;
  case pft of
    pftEvenOdd, pftNonZero: Result := abs(edge.windCnt) = 1;
    pftPositive: Result := (edge.windCnt = 1);
    else Result := (edge.windCnt = -1);
  end;
  if not Result then Exit;

  case FClipType of
    ctIntersection:
      case pft2 of
        pftEvenOdd, pftNonZero: Result := (edge.windCnt2 <> 0);
        pftPositive: Result := (edge.windCnt2 > 0);
        pftNegative: Result := (edge.windCnt2 < 0);
      end;
    ctUnion:
      case pft2 of
        pftEvenOdd, pftNonZero: Result := (edge.windCnt2 = 0);
        pftPositive: Result := (edge.windCnt2 <= 0);
        pftNegative: Result := (edge.windCnt2 >= 0);
      end;
    ctDifference:
      if edge.polyType = ptSubject then
        case pft2 of
          pftEvenOdd, pftNonZero: Result := (edge.windCnt2 = 0);
          pftPositive: Result := (edge.windCnt2 <= 0);
          pftNegative: Result := (edge.windCnt2 >= 0);
        end
      else
        case pft2 of
          pftEvenOdd, pftNonZero: Result := (edge.windCnt2 <> 0);
          pftPositive: Result := (edge.windCnt2 > 0);
          pftNegative: Result := (edge.windCnt2 < 0);
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(e1, e2: PEdge; const pt: TFixedPoint);
var
  e, prevE: PEdge;
begin
  if (e2.dx = CHorizontal) or (e1.dx > e2.dx) then
  begin
    AddOutPt(e1, pt);
    e2.outIdx := e1.outIdx;
    e1.side := esLeft;
    e2.side := esRight;
    e := e1;
    if e.prevInAEL = e2 then
      prevE := e2.prevInAEL
    else
      prevE := e.prevInAEL;
  end else
  begin
    AddOutPt(e2, pt);
    e1.outIdx := e2.outIdx;
    e1.side := esRight;
    e2.side := esLeft;
    e := e2;
    if e.prevInAEL = e1 then
      prevE := e1.prevInAEL
    else
      prevE := e.prevInAEL;
  end;

  if Assigned(prevE) and (prevE.outIdx >= 0) and
    (TopX(prevE, pt.Y) = TopX(e, pt.Y)) and SlopesEqual(e, prevE) then
       AddJoin(e, prevE);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMaxPoly(e1, e2: PEdge; const pt: TFixedPoint);
begin
  AddOutPt(e1, pt);
  if (e1.outIdx = e2.outIdx) then
  begin
    e1.outIdx := -1;
    e2.outIdx := -1;
  end
  else if e1.outIdx < e2.outIdx then
    AppendPolygon(e1, e2)
  else
    AppendPolygon(e2, e1);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddEdgeToSEL(edge: PEdge);
begin
  //SEL pointers in PEdge are reused to build a list of CHorizontal edges.
  //However, we don't need to worry about order with horizontal edge processing.
  if not Assigned(FSortedEdges) then
  begin
    FSortedEdges := edge;
    edge.prevInSEL := nil;
    edge.nextInSEL := nil;
  end else
  begin
    edge.nextInSEL := FSortedEdges;
    edge.prevInSEL := nil;
    FSortedEdges.prevInSEL := edge;
    FSortedEdges := edge;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CopyAELToSEL;
var
  e: PEdge;
begin
  e := FActiveEdges;
  FSortedEdges := e;
  if not Assigned(FActiveEdges) then Exit;

  FSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while Assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.prevInSEL.nextInSEL := e;
    e.nextInSEL := nil;
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddJoin(e1, e2: PEdge;
  e1OutIdx: Integer = -1; e2OutIdx: Integer = -1);
var
  jr: PJoinRec;
begin
  New(jr);
  if e1OutIdx >= 0 then
    jr.poly1Idx := e1OutIdx else
    jr.poly1Idx := e1.outIdx;
  with e1^ do
  begin
    jr.pt1a := FixedPoint2(xcurr, ycurr);
    jr.pt1b := FixedPoint2(xtop, ytop);
  end;
  if e2OutIdx >= 0 then
    jr.poly2Idx := e2OutIdx else
    jr.poly2Idx := e2.outIdx;
  with e2^ do
  begin
    jr.pt2a := FixedPoint2(xcurr, ycurr);
    jr.pt2b := FixedPoint2(xtop, ytop);
  end;
  FJoinList.add(jr);
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearJoins;
var
  i: Integer;
begin
  for i := 0 to FJoinList.count - 1 do
    Dispose(PJoinRec(FJoinList[i]));
  FJoinList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddHorzJoin(e: PEdge; idx: Integer);
var
  hr: PHorzRec;
begin
  New(hr);
  hr.edge := e;
  hr.savedIdx := idx;
  if FHorizJoins = nil then
  begin
    FHorizJoins := hr;
    hr.next := hr;
    hr.prev := hr;
  end else
  begin
    hr.next := FHorizJoins;
    hr.prev := FHorizJoins.prev;
    FHorizJoins.prev.next := hr;
    FHorizJoins.prev := hr;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearHorzJoins;
var
  m, m2: PHorzRec;
begin
  if not Assigned(FHorizJoins) then Exit;
  m := FHorizJoins;
  m.prev.next := nil;
  while Assigned(m) do
  begin
    m2 := m.next;
    dispose(m);
    m := m2;
  end;
  FHorizJoins := nil;
end;
//------------------------------------------------------------------------------

procedure SwapPoints(var pt1, pt2: TFixedPoint);
var
  tmp: TFixedPoint;
begin
  tmp := pt1;
  pt1 := pt2;
  pt2 := tmp;
end;
//------------------------------------------------------------------------------

function GetOverlapSegment(pt1a, pt1b, pt2a, pt2b: TFixedPoint;
  out pt1, pt2: TFixedPoint): Boolean;
begin
  //precondition: segments are colinear
  if (pt1a.Y = pt1b.Y) or (abs((pt1a.X - pt1b.X)/(pt1a.Y - pt1b.Y)) > 1) then
  begin
    if pt1a.X > pt1b.X then SwapPoints(pt1a, pt1b);
    if pt2a.X > pt2b.X then SwapPoints(pt2a, pt2b);
    if (pt1a.X > pt2a.X) then pt1 := pt1a else pt1 := pt2a;
    if (pt1b.X < pt2b.X) then pt2 := pt1b else pt2 := pt2b;
    Result := pt1.X < pt2.X;
  end else
  begin
    if pt1a.Y < pt1b.Y then SwapPoints(pt1a, pt1b);
    if pt2a.Y < pt2b.Y then SwapPoints(pt2a, pt2b);
    if (pt1a.Y < pt2a.Y) then pt1 := pt1a else pt1 := pt2a;
    if (pt1b.Y > pt2b.Y) then pt2 := pt1b else pt2 := pt2b;
    Result := pt1.Y > pt2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const botY: TFixed);

  function E2InsertsBeforeE1(e1,e2: PEdge): Boolean;
  begin
    if e2.xcurr = e1.xcurr then
      Result := e2.dx > e1.dx else
      Result := e2.xcurr < e1.xcurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertEdgeIntoAEL(edge: PEdge);
  var
    e: PEdge;
  begin
    edge.prevInAEL := nil;
    edge.nextInAEL := nil;
    if not Assigned(FActiveEdges) then
    begin
      FActiveEdges := edge;
    end else if E2InsertsBeforeE1(FActiveEdges, edge) then
    begin
      edge.nextInAEL := FActiveEdges;
      FActiveEdges.prevInAEL := edge;
      FActiveEdges := edge;
    end else
    begin
      e := FActiveEdges;
      while Assigned(e.nextInAEL) and not E2InsertsBeforeE1(e.nextInAEL, edge) do
        e := e.nextInAEL;
      edge.nextInAEL := e.nextInAEL;
      if Assigned(e.nextInAEL) then e.nextInAEL.prevInAEL := edge;
      edge.prevInAEL := e;
      e.nextInAEL := edge;
    end;
  end;
  //----------------------------------------------------------------------

var
  e: PEdge;
  pt, pt2: TFixedPoint;
  lb, rb: PEdge;
  hj: PHorzRec;
begin
  while Assigned(CurrentLm) and (CurrentLm.Y = botY) do
  begin
    lb := CurrentLm.leftBound;
    rb := CurrentLm.rightBound;

    InsertEdgeIntoAEL(lb);
    InsertScanbeam(lb.ytop);
    InsertEdgeIntoAEL(rb);

    //set edge winding states ...
    if IsEvenOddFillType(lb) then
    begin
      lb.windDelta := 1;
      rb.windDelta := 1;
    end else
    begin
      rb.windDelta := -lb.windDelta
    end;
    SetWindingCount(lb);
    rb.windCnt := lb.windCnt;
    rb.windCnt2 := lb.windCnt2;

    if rb.dx = CHorizontal then
    begin
      AddEdgeToSEL(rb);
      InsertScanbeam(rb.nextInLML.ytop);
    end else
      InsertScanbeam(rb.ytop);

    if IsContributing(lb) then
      AddLocalMinPoly(lb, rb, FixedPoint2(lb.xcurr, CurrentLm.y));

    //if output polygons share an edge with rb, they'll need joining later ...
    if (rb.outIdx >= 0) then
    begin
      if (rb.dx = CHorizontal) then
      begin
        if Assigned(FHorizJoins) then
        begin
          hj := FHorizJoins;
          repeat
            //if horizontals rb & hj.edge overlap, flag for joining later ...
            if GetOverlapSegment(FixedPoint2(hj.edge.xbot, hj.edge.ybot),
              FixedPoint2(hj.edge.xtop, hj.edge.ytop), FixedPoint2(rb.xbot, rb.ybot),
              FixedPoint2(rb.xtop, rb.ytop), pt, pt2) then
                AddJoin(hj.edge, rb, hj.savedIdx);
            hj := hj.next;
          until hj = FHorizJoins;
        end;
      end;
    end;

    if (lb.nextInAEL <> rb) then
    begin
      if (rb.outIdx >= 0) and (rb.prevInAEL.outIdx >= 0) and
        SlopesEqual(rb.prevInAEL, rb) then
          AddJoin(rb, rb.prevInAEL);

      e := lb.nextInAEL;
      pt := FixedPoint2(lb.xcurr,lb.ycurr);
      while e <> rb do
      begin
        if not Assigned(e) then raise Exception.Create(rsMissingRightbound);
        //nb: For calculating winding counts etc, IntersectEdges() assumes
        //that param1 will be to the right of param2 ABOVE the intersection ...
        IntersectEdges(rb, e, pt);
        e := e.nextInAEL;
      end;
    end;
    PopLocalMinima;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromAEL(e: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  AelPrev := e.prevInAEL;
  AelNext := e.nextInAEL;
  if not Assigned(AelPrev) and not Assigned(AelNext) and
    (e <> FActiveEdges) then Exit; //already deleted
  if Assigned(AelPrev) then AelPrev.nextInAEL := AelNext
  else FActiveEdges := AelNext;
  if Assigned(AelNext) then AelNext.prevInAEL := AelPrev;
  e.nextInAEL := nil;
  e.prevInAEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromSEL(e: PEdge);
var
  SelPrev, SelNext: PEdge;
begin
  SelPrev := e.prevInSEL;
  SelNext := e.nextInSEL;
  if not Assigned(SelPrev) and not Assigned(SelNext) and
    (e <> FSortedEdges) then Exit; //already deleted
  if Assigned(SelPrev) then SelPrev.nextInSEL := SelNext
  else FSortedEdges := SelNext;
  if Assigned(SelNext) then SelNext.prevInSEL := SelPrev;
  e.nextInSEL := nil;
  e.prevInSEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.IntersectEdges(e1,e2: PEdge;
  const pt: TFixedPoint; protects: TIntersectProtects = []);

  procedure DoEdge1;
  begin
    AddOutPt(e1, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

  procedure DoEdge2;
  begin
    AddOutPt(e2, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

  procedure DoBothEdges;
  begin
    AddOutPt(e1, pt);
    AddOutPt(e2, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

var
  e1stops, e2stops: Boolean;
  e1Contributing, e2contributing: Boolean;
  e1FillType, e2FillType, e1FillType2, e2FillType2: TPolyFillType;
  e1Wc, e2Wc, e1Wc2, e2Wc2: Integer;
begin
  {IntersectEdges}

  //e1 will be to the left of e2 BELOW the intersection. Therefore e1 is before
  //e2 in AEL except when e1 is being inserted at the intersection point ...

  e1stops := not (ipLeft in protects) and not Assigned(e1.nextInLML) and
    (e1.xtop = pt.x) and (e1.ytop = pt.y);
  e2stops := not (ipRight in protects) and not Assigned(e2.nextInLML) and
    (e2.xtop = pt.x) and (e2.ytop = pt.y);
  e1Contributing := (e1.outIdx >= 0);
  e2contributing := (e2.outIdx >= 0);

  //update winding counts...
  //assumes that e1 will be to the right of e2 ABOVE the intersection
  if e1.polyType = e2.polyType then
  begin
    if IsEvenOddFillType(e1) then
    begin
      e1Wc := e1.windCnt;
      e1.windCnt := e2.windCnt;
      e2.windCnt := e1Wc;
    end else
    begin
      if e1.windCnt + e2.windDelta = 0 then
        e1.windCnt := -e1.windCnt else
        Inc(e1.windCnt, e2.windDelta);
      if e2.windCnt - e1.windDelta = 0 then
        e2.windCnt := -e2.windCnt else
        Dec(e2.windCnt, e1.windDelta);
    end;
  end else
  begin
    if not IsEvenOddFillType(e2) then Inc(e1.windCnt2, e2.windDelta)
    else if e1.windCnt2 = 0 then e1.windCnt2 := 1
    else e1.windCnt2 := 0;
    if not IsEvenOddFillType(e1) then Dec(e2.windCnt2, e1.windDelta)
    else if e2.windCnt2 = 0 then e2.windCnt2 := 1
    else e2.windCnt2 := 0;
  end;

  if e1.polyType = ptSubject then
  begin
    e1FillType := FSubjFillType;
    e1FillType2 := FClipFillType;
  end else
  begin
    e1FillType := FClipFillType;
    e1FillType2 := FSubjFillType;
  end;
  if e2.polyType = ptSubject then
  begin
    e2FillType := FSubjFillType;
    e2FillType2 := FClipFillType;
  end else
  begin
    e2FillType := FClipFillType;
    e2FillType2 := FSubjFillType;
  end;

  case e1FillType of
    pftPositive: e1Wc := e1.windCnt;
    pftNegative : e1Wc := -e1.windCnt;
    else e1Wc := abs(e1.windCnt);
  end;
  case e2FillType of
    pftPositive: e2Wc := e2.windCnt;
    pftNegative : e2Wc := -e2.windCnt;
    else e2Wc := abs(e2.windCnt);
  end;

  if e1Contributing and e2contributing then
  begin
    if e1stops or e2stops or not (e1Wc in [0,1]) or not (e2Wc in [0,1]) or
      ((e1.polytype <> e2.polytype) and (FClipType <> ctXor)) then
        AddLocalMaxPoly(e1, e2, pt) else
        DoBothEdges;
  end else if e1Contributing then
  begin
    if ((e2Wc = 0) or (e2Wc = 1)) and
      ((FClipType <> ctIntersection) or (e2.polyType = ptSubject) or
        (e2.windCnt2 <> 0)) then DoEdge1;
  end
  else if e2contributing then
  begin
    if ((e1Wc = 0) or (e1Wc = 1)) and
      ((FClipType <> ctIntersection) or (e1.polyType = ptSubject) or
        (e1.windCnt2 <> 0)) then DoEdge2;
  end
  else if  ((e1Wc = 0) or (e1Wc = 1)) and ((e2Wc = 0) or (e2Wc = 1)) and
    not e1stops and not e2stops then
  begin
    //neither edge is currently contributing ...

    case e1FillType2 of
      pftPositive: e1Wc2 := e1.windCnt2;
      pftNegative : e1Wc2 := -e1.windCnt2;
      else e1Wc2 := abs(e1.windCnt2);
    end;
    case e2FillType2 of
      pftPositive: e2Wc2 := e2.windCnt2;
      pftNegative : e2Wc2 := -e2.windCnt2;
      else e2Wc2 := abs(e2.windCnt2);
    end;

    if (e1.polytype <> e2.polytype) then
      AddLocalMinPoly(e1, e2, pt)
    else if (e1Wc = 1) and (e2Wc = 1) then
      case FClipType of
        ctIntersection:
          if (e1Wc2 > 0) and (e2Wc2 > 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctUnion:
          if (e1Wc2 <= 0) and (e2Wc2 <= 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctDifference:
          if ((e1.polyType = ptClip) and (e1Wc2 > 0) and (e2Wc2 > 0)) or
            ((e1.polyType = ptSubject) and (e1Wc2 <= 0) and (e2Wc2 <= 0)) then
              AddLocalMinPoly(e1, e2, pt);
        ctXor:
          AddLocalMinPoly(e1, e2, pt);
      end
    else
      swapsides(e1,e2);
  end;

  if (e1stops <> e2stops) and
    ((e1stops and (e1.outIdx >= 0)) or (e2stops and (e2.outIdx >= 0))) then
  begin
    swapsides(e1,e2);
    SwapPolyIndexes(e1, e2);
  end;

  //finally, delete any non-contributing maxima edges  ...
  if e1stops then deleteFromAEL(e1);
  if e2stops then deleteFromAEL(e2);
end;
//------------------------------------------------------------------------------

function FirstIsBottomPt(btmPt1, btmPt2: POutPt): Boolean;
var
  dx1n, dx1p, dx2n, dx2p: Double;
  p: POutPt;
begin
  p := btmPt1.prev;
  while PointsEqual(p.pt, btmPt1.pt) and (p <> btmPt1) do p := p.prev;
  dx1p := abs(GetDx(btmPt1.pt, p.pt));
  p := btmPt1.next;
  while PointsEqual(p.pt, btmPt1.pt) and (p <> btmPt1) do p := p.next;
  dx1n := abs(GetDx(btmPt1.pt, p.pt));

  p := btmPt2.prev;
  while PointsEqual(p.pt, btmPt2.pt) and (p <> btmPt2) do p := p.prev;
  dx2p := abs(GetDx(btmPt2.pt, p.pt));
  p := btmPt2.next;
  while PointsEqual(p.pt, btmPt2.pt) and (p <> btmPt2) do p := p.next;
  dx2n := abs(GetDx(btmPt2.pt, p.pt));
  Result := ((dx1p >= dx2p) and (dx1p >= dx2n)) or
    ((dx1n >= dx2p) and (dx1n >= dx2n));
end;
//------------------------------------------------------------------------------

function GetBottomPt(pp: POutPt): POutPt;
var
  p, dups: POutPt;
begin
  dups := nil;
  p := pp.next;
  while p <> pp do
  begin
    if p.pt.Y > pp.pt.Y then
    begin
      pp := p;
      dups := nil;
    end
    else if (p.pt.Y = pp.pt.Y) and (p.pt.X <= pp.pt.X) then
    begin
      if (p.pt.X < pp.pt.X) then
      begin
        dups := nil;
        pp := p;
      end else
      begin
        if (p.next <> pp) and (p.prev <> pp) then dups := p;
      end;
    end;
    p := p.next;
  end;
  if Assigned(dups) then
  begin
    //there appears to be at least 2 vertices at bottomPt so ...
    while dups <> p do
    begin
      if not FirstIsBottomPt(p, dups) then pp := dups;
      dups := dups.next;
      while not PointsEqual(dups.pt, pp.pt) do dups := dups.next;
    end;
  end;
  Result := pp;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetHoleState(e: PEdge; outRec: POutRec);
var
  e2: PEdge;
  isHole: Boolean;
begin
  isHole := False;
  e2 := e.prevInAEL;
  while Assigned(e2) do
  begin
    if (e2.outIdx >= 0) then
    begin
      isHole := not isHole;
      if not Assigned(outRec.FirstLeft) then
        outRec.FirstLeft := POutRec(FPolyOutList[e2.outIdx]);
    end;
    e2 := e2.prevInAEL;
  end;
  if isHole then
    outRec.isHole := True;
end;
//------------------------------------------------------------------------------

function GetLowermostRec(outRec1, outRec2: POutRec): POutRec;
var
  outPt1, outPt2: POutPt;
begin
  outPt1 := outRec1.bottomPt;
  outPt2 := outRec2.bottomPt;
  if (outPt1.pt.Y > outPt2.pt.Y) then Result := outRec1
  else if (outPt1.pt.Y < outPt2.pt.Y) then Result := outRec2
  else if (outPt1.pt.X < outPt2.pt.X) then Result := outRec1
  else if (outPt1.pt.X > outPt2.pt.X) then Result := outRec2
  else if (outPt1.next = outPt1) then Result := outRec2
  else if (outPt2.next = outPt2) then Result := outRec1
  else if FirstIsBottomPt(outPt1, outPt2) then
    Result := outRec1 else
    Result := outRec2;
end;
//------------------------------------------------------------------------------

function Param1RightOfParam2(outRec1, outRec2: POutRec): Boolean;
begin
  Result := True;
  repeat
    outRec1 := outRec1.FirstLeft;
    if outRec1 = outRec2 then Exit;
  until not Assigned(outRec1);
  Result := False;
end;
//------------------------------------------------------------------------------

procedure TClipper.AppendPolygon(e1, e2: PEdge);
var
  holeStateRec, outRec1, outRec2: POutRec;
  p1_lft, p1_rt, p2_lft, p2_rt: POutPt;
  newSide: TEdgeSide;
  i, OKIdx, ObsoleteIdx: Integer;
  e: PEdge;
  jr: PJoinRec;
  h: PHorzRec;
begin
  outRec1 := FPolyOutList[e1.outIdx];
  outRec2 := FPolyOutList[e2.outIdx];

  //work out which polygon fragment has the correct hole state ...
  if Param1RightOfParam2(outRec1, outRec2) then holeStateRec := outRec2
  else if Param1RightOfParam2(outRec2, outRec1) then holeStateRec := outRec1
  else holeStateRec := GetLowermostRec(outRec1, outRec2);

  //get the start and ends of both output polygons ...
  p1_lft := outRec1.Pts;
  p2_lft := outRec2.Pts;
  p1_rt := p1_lft.prev;
  p2_rt := p2_lft.prev;

  //join e2 poly onto e1 poly and delete pointers to e2 ...
  if e1.side = esLeft then
  begin
    if e2.side = esLeft then
    begin
      //z y x a b c
      ReversePolyPtLinks(p2_lft);
      p2_lft.next := p1_lft;
      p1_lft.prev := p2_lft;
      p1_rt.next := p2_rt;
      p2_rt.prev := p1_rt;
      outRec1.Pts := p2_rt;
    end else
    begin
      //x y z a b c
      p2_rt.next := p1_lft;
      p1_lft.prev := p2_rt;
      p2_lft.prev := p1_rt;
      p1_rt.next := p2_lft;
      outRec1.Pts := p2_lft;
    end;
    newSide := esLeft;
  end else
  begin
    if e2.side = esRight then
    begin
      //a b c z y x
      ReversePolyPtLinks(p2_lft);
      p1_rt.next := p2_rt;
      p2_rt.prev := p1_rt;
      p2_lft.next := p1_lft;
      p1_lft.prev := p2_lft;
    end else
    begin
      //a b c x y z
      p1_rt.next := p2_lft;
      p2_lft.prev := p1_rt;
      p1_lft.prev := p2_rt;
      p2_rt.next := p1_lft;
    end;
    newSide := esRight;
  end;

  if holeStateRec = outRec2 then
  begin
    outRec1.bottomPt := outRec2.bottomPt;
    outRec1.bottomPt.idx := outRec1.idx;
    if outRec2.FirstLeft <> outRec1 then
      outRec1.FirstLeft := outRec2.FirstLeft;
    outRec1.isHole := outRec2.isHole;
  end;
  outRec2.Pts := nil;
  outRec2.bottomPt := nil;
  outRec2.AppendLink := outRec1;
  OKIdx := outRec1.idx;
  ObsoleteIdx := outRec2.idx;

  e1.outIdx := -1; //nb: safe because we only get here via AddLocalMaxPoly
  e2.outIdx := -1;

  e := FActiveEdges;
  while Assigned(e) do
  begin
    if (e.outIdx = ObsoleteIdx) then
    begin
      e.outIdx := OKIdx;
      e.side := newSide;
      break;
    end;
    e := e.nextInAEL;
  end;

  for i := 0 to FJoinList.Count - 1 do
  begin
    jr := FJoinList[i];
    if jr.poly1Idx = ObsoleteIdx then jr.poly1Idx := OKIdx;
    if jr.poly2Idx = ObsoleteIdx then jr.poly2Idx := OKIdx;
  end;
  if Assigned(FHorizJoins) then
  begin
    h := FHorizJoins;
    repeat
      if h.savedIdx = ObsoleteIdx then h.SavedIdx := OKIdx;
      h := h.next;
    until h = FHorizJoins;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.CreateOutRec: POutRec;
begin
  New(Result);
  Result.isHole := False;
  Result.FirstLeft := nil;
  Result.AppendLink := nil;
  Result.Pts := nil;
  Result.bottomPt := nil;
  Result.sides := [];
  Result.bottomFlag := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddOutPt(e: PEdge; const pt: TFixedPoint);
var
  outRec: POutRec;
  op, op2, opBot: POutPt;
  ToFront: Boolean;
begin
  ToFront := e.side = esLeft;
  if e.outIdx < 0 then
  begin
    outRec := CreateOutRec;
    outRec.idx := FPolyOutList.Add(outRec);
    e.outIdx := outRec.idx;
    New(op);
    outRec.Pts := op;
    outRec.bottomPt := op;

    op.pt := pt;
    op.next := op;
    op.prev := op;
    op.idx := outRec.idx;
    SetHoleState(e, outRec);
  end else
  begin
    outRec := FPolyOutList[e.outIdx];
    op := outRec.Pts;
    if (ToFront and PointsEqual(pt, op.pt)) or
      (not ToFront and PointsEqual(pt, op.prev.pt)) then Exit;

    if not (e.side in outRec.sides) then
    begin

      //check for 'rounding' artefacts ...
      if (outRec.sides = []) and (pt.Y = op.pt.Y) then
        if ToFront then
        begin
          if (pt.X = op.pt.X + 1) then Exit;    //ie wrong side of bottomPt
        end
        else if (pt.X = op.pt.X - 1) then Exit; //ie wrong side of bottomPt

      outRec.sides := outRec.sides + [e.side];
      if outRec.sides = [esLeft, esRight] then
      begin
        //A vertex from each side has now been added.
        //Vertices of one side of an output polygon are quite commonly close to
        //or even 'touching' edges of the other side of the output polygon.
        //Very occasionally vertices from one side can 'cross' an edge on the
        //the other side. The distance 'crossed' is always less that a unit
        //and is purely an artefact of coordinate rounding. Nevertheless, this
        //results in very tiny self-intersections. Because of the way
        //orientation is calculated, even tiny self-intersections can cause
        //the Orientation function to return the wrong Result. Therefore, it's
        //important to ensure that any self-intersections close to BottomPt are
        //detected and removed before orientation is Assigned.

        if ToFront then
        begin
          opBot := outRec.Pts;
          op2 := opBot.next; //op2 == right side
          if (opBot.pt.Y <> op2.pt.Y) and (opBot.pt.Y <> pt.Y) and
            ((opBot.pt.X - pt.X)/(opBot.pt.Y - pt.Y) <
            (opBot.pt.X - op2.pt.X)/(opBot.pt.Y - op2.pt.Y)) then
               outRec.bottomFlag := opBot;
        end else
        begin
          opBot := outRec.Pts.prev;
          op2 := opBot.prev; //op2 == left side
          if (opBot.pt.Y <> op2.pt.Y) and (opBot.pt.Y <> pt.Y) and
            ((opBot.pt.X - pt.X)/(opBot.pt.Y - pt.Y) >
            (opBot.pt.X - op2.pt.X)/(opBot.pt.Y - op2.pt.Y)) then
               outRec.bottomFlag := opBot;
        end;
      end;
    end;

    New(op2);
    op2.pt := pt;
    op2.idx := outRec.idx;
    if (op2.pt.Y = outRec.bottomPt.pt.Y) and
      (op2.pt.X < outRec.bottomPt.pt.X) then
        outRec.bottomPt := op2;
    op2.next := op;
    op2.prev := op.prev;
    op.prev.next := op2;
    op.prev := op2;
    if ToFront then outRec.Pts := op2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontals;
var
  e: PEdge;
begin
  while Assigned(FSortedEdges) do
  begin
    e := FSortedEdges;
    DeleteFromSEL(e);
    ProcessHorizontal(e);
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsTopHorz(const XPos: TFixed): Boolean;
var
  e: PEdge;
begin
  Result := False;
  e := FSortedEdges;
  while Assigned(e) do
  begin
    if (XPos >= min(e.xcurr,e.xtop)) and (XPos <= max(e.xcurr,e.xtop)) then Exit;
    e := e.nextInSEL;
  end;
  Result := True;
end;
//------------------------------------------------------------------------------

function IsMinima(e: PEdge): Boolean;
begin
  Result := Assigned(e) and (e.prev.nextInLML <> e) and (e.next.nextInLML <> e);
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PEdge; const Y: TFixed): Boolean;
begin
  Result := Assigned(e) and (e.ytop = Y) and not Assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function IsIntermediate(e: PEdge; const Y: TFixed): Boolean;
begin
  Result := (e.ytop = Y) and Assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PEdge): PEdge;
begin
  Result := e.next;
  if not IsMaxima(Result, e.ytop) or (Result.xtop <> e.xtop) then
    Result := e.prev;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  with e1^ do if not Assigned(nextInAEL) and not Assigned(prevInAEL) then Exit;
  with e2^ do if not Assigned(nextInAEL) and not Assigned(prevInAEL) then Exit;

  if e1.nextInAEL = e2 then
  begin
    next := e2.nextInAEL;
    if Assigned(next) then next.prevInAEL := e1;
    prev := e1.prevInAEL;
    if Assigned(prev) then prev.nextInAEL := e2;
    e2.prevInAEL := prev;
    e2.nextInAEL := e1;
    e1.prevInAEL := e2;
    e1.nextInAEL := next;
  end
  else if e2.nextInAEL = e1 then
  begin
    next := e1.nextInAEL;
    if Assigned(next) then next.prevInAEL := e2;
    prev := e2.prevInAEL;
    if Assigned(prev) then prev.nextInAEL := e1;
    e1.prevInAEL := prev;
    e1.nextInAEL := e2;
    e2.prevInAEL := e1;
    e2.nextInAEL := next;
  end else
  begin
    next := e1.nextInAEL;
    prev := e1.prevInAEL;
    e1.nextInAEL := e2.nextInAEL;
    if Assigned(e1.nextInAEL) then e1.nextInAEL.prevInAEL := e1;
    e1.prevInAEL := e2.prevInAEL;
    if Assigned(e1.prevInAEL) then e1.prevInAEL.nextInAEL := e1;
    e2.nextInAEL := next;
    if Assigned(e2.nextInAEL) then e2.nextInAEL.prevInAEL := e2;
    e2.prevInAEL := prev;
    if Assigned(e2.prevInAEL) then e2.prevInAEL.nextInAEL := e2;
  end;
  if not Assigned(e1.prevInAEL) then FActiveEdges := e1
  else if not Assigned(e2.prevInAEL) then FActiveEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInSEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  if e1.nextInSEL = e2 then
  begin
    next    := e2.nextInSEL;
    if Assigned(next) then next.prevInSEL := e1;
    prev    := e1.prevInSEL;
    if Assigned(prev) then prev.nextInSEL := e2;
    e2.prevInSEL := prev;
    e2.nextInSEL := e1;
    e1.prevInSEL := e2;
    e1.nextInSEL := next;
  end
  else if e2.nextInSEL = e1 then
  begin
    next    := e1.nextInSEL;
    if Assigned(next) then next.prevInSEL := e2;
    prev    := e2.prevInSEL;
    if Assigned(prev) then prev.nextInSEL := e1;
    e1.prevInSEL := prev;
    e1.nextInSEL := e2;
    e2.prevInSEL := e1;
    e2.nextInSEL := next;
  end else
  begin
    next    := e1.nextInSEL;
    prev    := e1.prevInSEL;
    e1.nextInSEL := e2.nextInSEL;
    if Assigned(e1.nextInSEL) then e1.nextInSEL.prevInSEL := e1;
    e1.prevInSEL := e2.prevInSEL;
    if Assigned(e1.prevInSEL) then e1.prevInSEL.nextInSEL := e1;
    e2.nextInSEL := next;
    if Assigned(e2.nextInSEL) then e2.nextInSEL.prevInSEL := e2;
    e2.prevInSEL := prev;
    if Assigned(e2.prevInSEL) then e2.prevInSEL.nextInSEL := e2;
  end;
  if not Assigned(e1.prevInSEL) then FSortedEdges := e1
  else if not Assigned(e2.prevInSEL) then FSortedEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontal(horzEdge: PEdge);

  function GetNextInAEL(e: PEdge; Direction: TDirection): PEdge;
  begin
    if Direction = dLeftToRight then
      Result := e.nextInAEL else
      Result := e.prevInAEL;
  end;
  //------------------------------------------------------------------------

var
  e, eNext, eMaxPair: PEdge;
  horzLeft, horzRight: TFixed;
  Direction: TDirection;
const
  ProtectLeft: array[Boolean] of TIntersectProtects = ([ipRight], [ipLeft,ipRight]);
  ProtectRight: array[Boolean] of TIntersectProtects = ([ipLeft], [ipLeft,ipRight]);
begin
(*******************************************************************************
* Notes: CHorizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with other HE xbots only [#],    *
* and with other non-CHorizontal edges [*]. Once these intersections are        *
* processed, intermediate HEs then 'promote' the edge above (nextInLML) into   *
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

  if horzEdge.xcurr < horzEdge.xtop then
  begin
    horzLeft := horzEdge.xcurr;
    horzRight := horzEdge.xtop;
    Direction := dLeftToRight;
  end else
  begin
    horzLeft := horzEdge.xtop;
    horzRight := horzEdge.xcurr;
    Direction := dRightToLeft;
  end;

  if Assigned(horzEdge.nextInLML) then
    eMaxPair := nil else
    eMaxPair := GetMaximaPair(horzEdge);

  e := GetNextInAEL(horzEdge, Direction);
  while Assigned(e) do
  begin
    eNext := GetNextInAEL(e, Direction);
    if Assigned(eMaxPair) or
       ((Direction = dLeftToRight) and (e.xcurr <= horzRight)) or
      ((Direction = dRightToLeft) and (e.xcurr >= horzLeft)) then
    begin
      //ok, so far it looks like we're still in range of the horizontal edge

      if (e.xcurr = horzEdge.xtop) and not Assigned(eMaxPair) then
      begin
        if SlopesEqual(e, horzEdge.nextInLML) then
        begin
          //if output polygons share an edge, they'll need joining later ...
          if (horzEdge.outIdx >= 0) and (e.outIdx >= 0) then
            AddJoin(horzEdge.nextInLML, e, horzEdge.outIdx);
          break; //we've reached the end of the horizontal line
        end
        else if (e.dx < horzEdge.nextInLML.dx) then
        //we really have got to the end of the intermediate horz edge so quit.
        //nb: More -ve slopes follow more +ve slopes ABOVE the CHorizontal.
          break;
      end;

      if (e = eMaxPair) then
      begin
        //horzEdge is evidently a maxima CHorizontal and we've arrived at its end.
        if Direction = dLeftToRight then
          IntersectEdges(horzEdge, e, FixedPoint2(e.xcurr, horzEdge.ycurr)) else
          IntersectEdges(e, horzEdge, FixedPoint2(e.xcurr, horzEdge.ycurr));

        if (eMaxPair.outIdx >= 0) then raise Exception.Create(rsHorizontal);
        Exit;
      end
      else if (e.dx = CHorizontal) and not IsMinima(e) and not (e.xcurr > e.xtop) then
      begin
        //An overlapping CHorizontal edge. Overlapping CHorizontal edges are
        //processed as if layered with the current CHorizontal edge (horizEdge)
        //being infinitesimally lower that the next (e). Therfore, we
        //intersect with e only if e.xcurr is within the Bounds of horzEdge ...
        if Direction = dLeftToRight then
          IntersectEdges(horzEdge, e, FixedPoint2(e.xcurr, horzEdge.ycurr),
            ProtectRight[not IsTopHorz(e.xcurr)])
        else
          IntersectEdges(e, horzEdge, FixedPoint2(e.xcurr, horzEdge.ycurr),
            ProtectLeft[not IsTopHorz(e.xcurr)]);
      end
      else if (Direction = dLeftToRight) then
        IntersectEdges(horzEdge, e, FixedPoint2(e.xcurr, horzEdge.ycurr),
          ProtectRight[not IsTopHorz(e.xcurr)])
      else
        IntersectEdges(e, horzEdge, FixedPoint2(e.xcurr, horzEdge.ycurr),
          ProtectLeft[not IsTopHorz(e.xcurr)]);
      SwapPositionsInAEL(horzEdge, e);
    end
    else if ((Direction = dLeftToRight) and
      (e.xcurr > horzRight) and Assigned(FSortedEdges)) or
      ((Direction = dRightToLeft) and
      (e.xcurr < horzLeft) and Assigned(FSortedEdges)) then
        break;
    e := eNext;
  end;

  if Assigned(horzEdge.nextInLML) then
  begin
    if (horzEdge.outIdx >= 0) then
      AddOutPt(horzEdge, FixedPoint2(horzEdge.xtop, horzEdge.ytop));
    UpdateEdgeIntoAEL(horzEdge);
  end else
  begin
    if horzEdge.outIdx >= 0 then
      IntersectEdges(horzEdge, eMaxPair,
        FixedPoint2(horzEdge.xtop, horzEdge.ycurr), [ipLeft,ipRight]);

    if eMaxPair.outIdx >= 0 then raise Exception.Create(rsHorizontal);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(horzEdge);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var e: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  if not Assigned(e.nextInLML) then raise Exception.Create(rsUpdateEdgeIntoAEL);
  AelPrev := e.prevInAEL;
  AelNext := e.nextInAEL;
  e.nextInLML.outIdx := e.outIdx;
  if Assigned(AelPrev) then
    AelPrev.nextInAEL := e.nextInLML else
    FActiveEdges := e.nextInLML;
  if Assigned(AelNext) then
    AelNext.prevInAEL := e.nextInLML;
  e.nextInLML.side := e.side;
  e.nextInLML.windDelta := e.windDelta;
  e.nextInLML.windCnt := e.windCnt;
  e.nextInLML.windCnt2 := e.windCnt2;
  e := e.nextInLML;
  e.prevInAEL := AelPrev;
  e.nextInAEL := AelNext;
  if e.dx <> CHorizontal then
    InsertScanbeam(e.ytop);
end;
//------------------------------------------------------------------------------

function TClipper.ProcessIntersections(const botY, topY: TFixed): Boolean;
begin
  Result := True;
  try
    BuildIntersectList(botY, topY);
    if FIntersectNodes = nil then Exit;
    if FixupIntersections then ProcessIntersectList
    else Result := False;
  finally
    //if there's been an error, clean up the mess ...
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeIntersectNodes;
var
  n: PIntersectNode;
begin
  while Assigned(FIntersectNodes) do
  begin
    n := FIntersectNodes.next;
    dispose(FIntersectNodes);
    FIntersectNodes := n;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildIntersectList(const botY, topY: TFixed);
var
  e, eNext: PEdge;
  pt: TFixedPoint;
  isModified: Boolean;
begin
  if not Assigned(FActiveEdges) then Exit;

  //prepare for sorting ...
  e := FActiveEdges;
  e.tmpX := TopX(e, topY);
  FSortedEdges := e;
  FSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while Assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.prevInSEL.nextInSEL := e;
    e.nextInSEL := nil;
    e.tmpX := TopX(e, topY);
    e := e.nextInAEL;
  end;

  try
    //bubblesort ...
    isModified := True;
    while isModified and Assigned(FSortedEdges) do
    begin
      isModified := False;
      e := FSortedEdges;
      while Assigned(e.nextInSEL) do
      begin
        eNext := e.nextInSEL;
        if (e.tmpX > eNext.tmpX) and
          IntersectPoint(e, eNext, pt) then
        begin
          if pt.Y > botY then
          begin
            pt.Y := botY;
            pt.X := TopX(e, pt.Y);
          end;
          AddIntersectNode(e, eNext, pt);
          SwapPositionsInSEL(e, eNext);
          isModified := True;
        end else
          e := eNext;
      end;
      if Assigned(e.prevInSEL) then e.prevInSEL.nextInSEL := nil else break;
    end;
  finally
    FSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddIntersectNode(e1, e2: PEdge; const pt: TFixedPoint);

  function ProcessParam1BeforeParam2(node1, node2: PIntersectNode): Boolean;
  begin
    if node1.pt.Y = node2.pt.Y then
    begin
      if (node1.edge1 = node2.edge1) or (node1.edge2 = node2.edge1) then
      begin
        Result := node2.pt.X > node1.pt.X;
        if node2.edge1.dx > 0 then Result := not Result;
      end
      else if (node1.edge1 = node2.edge2) or (node1.edge2 = node2.edge2) then
      begin
        Result := node2.pt.X > node1.pt.X;
        if node2.edge2.dx > 0 then Result := not Result;
      end else
        Result := node2.pt.X > node1.pt.X;
    end
    else Result := node1.pt.Y > node2.pt.Y;
  end;
  //----------------------------------------------------------------------------

var
  node, newNode: PIntersectNode;
begin
  New(newNode);
  newNode.edge1 := e1;
  newNode.edge2 := e2;
  newNode.pt := pt;
  newNode.next := nil;
  if not Assigned(FIntersectNodes) then
    FIntersectNodes := newNode
  else if ProcessParam1BeforeParam2(newNode, FIntersectNodes) then
  begin
    newNode.next := FIntersectNodes;
    FIntersectNodes := newNode;
  end else
  begin
    node := FIntersectNodes;
    while Assigned(node.next) and
      ProcessParam1BeforeParam2(node.next, newNode) do
      node := node.next;
    newNode.next := node.next;
    node.next := newNode;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersectList;
var
  node: PIntersectNode;
begin
  while Assigned(FIntersectNodes) do
  begin
    node := FIntersectNodes.next;
    with FIntersectNodes^ do
    begin
      IntersectEdges(edge1, edge2, pt, [ipLeft,ipRight]);
      SwapPositionsInAEL(edge1, edge2);
    end;
    dispose(FIntersectNodes);
    FIntersectNodes := node;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DoMaxima(e: PEdge; const topY: TFixed);
var
  eNext, eMaxPair: PEdge;
  X: TFixed;
begin
  eMaxPair := GetMaximaPair(e);
  X := e.xtop;
  eNext := e.nextInAEL;
  while eNext <> eMaxPair do
  begin
    if not Assigned(eNext) then raise Exception.Create(rsDoMaxima);
    IntersectEdges(e, eNext, FixedPoint2(X, topY), [ipLeft, ipRight]);
    eNext := eNext.nextInAEL;
  end;
  if (e.outIdx < 0) and (eMaxPair.outIdx < 0) then
  begin
    DeleteFromAEL(e);
    DeleteFromAEL(eMaxPair);
  end
  else if (e.outIdx >= 0) and (eMaxPair.outIdx >= 0) then
  begin
    IntersectEdges(e, eMaxPair, FixedPoint2(X, topY));
  end
  else raise Exception.Create(rsDoMaxima);
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessEdgesAtTopOfScanbeam(const topY: TFixed);
var
  e, ePrior: PEdge;
  hj: PHorzRec;
  pt, pt2: TFixedPoint;
begin
(*******************************************************************************
* Notes: Processing edges at scanline intersections (ie at the top or bottom   *
* of a scanbeam) needs to be done in multiple stages and in the correct order. *
* Firstly, edges forming a 'maxima' need to be processed and then removed.     *
* Next, 'intermediate' and 'maxima' CHorizontal edges are processed. Then edges *
* that intersect exactly at the top of the scanbeam are processed [%].         *
* Finally, New minima are added and any intersects they create are processed.  *
*******************************************************************************)

(*******************************************************************************
*     \                          /    /          \   /                         *
*      \   CHorizontal minima    /    /            \ /                          *
* { --  o======================#====o   --------   .     ------------------- } *
* {       CHorizontal maxima    .                   %  scanline intersect     } *
* { -- o=======================#===================#========o     ---------- } *
*      |                      /                   / \        \                 *
*      + maxima intersect    /                   /   \        \                *
*     /|\                   /                   /     \        \               *
*    / | \                 /                   /       \        \              *
*******************************************************************************)

  e := FActiveEdges;
  while Assigned(e) do
  begin
    //1. process maxima, treating them as if they're 'bent' horizontal edges,
    //   but exclude maxima with CHorizontal edges. nb: e can't be a horizontal.
    if IsMaxima(e, topY) and (GetMaximaPair(e).dx <> CHorizontal) then
    begin
      //'e' might be removed from AEL, as may any following edges so ...
      ePrior := e.prevInAEL;
      DoMaxima(e, topY);
      if not Assigned(ePrior) then
        e := FActiveEdges else
        e := ePrior.nextInAEL;
    end else
    begin
      //2. promote CHorizontal edges, otherwise update xcurr and ycurr ...
      if IsIntermediate(e, topY) and (e.nextInLML.dx = CHorizontal) then
      begin
        if (e.outIdx >= 0) then
        begin
          AddOutPt(e, FixedPoint2(e.xtop, e.ytop));

          hj := FHorizJoins;
          if Assigned(hj) then
          repeat
            if GetOverlapSegment(FixedPoint2(hj.edge.xbot, hj.edge.ybot),
              FixedPoint2(hj.edge.xtop, hj.edge.ytop),
              FixedPoint2(e.nextInLML.xbot, e.nextInLML.ybot),
              FixedPoint2(e.nextInLML.xtop, e.nextInLML.ytop), pt, pt2) then
                AddJoin(hj.edge, e.nextInLML, hj.savedIdx, e.outIdx);
            hj := hj.next;
          until hj = FHorizJoins;

          AddHorzJoin(e.nextInLML, e.outIdx);
        end;
        UpdateEdgeIntoAEL(e);
        AddEdgeToSEL(e);
      end else
      begin
        //this just simplifies CHorizontal processing ...
        e.xcurr := TopX(e, topY);
        e.ycurr := topY;
      end;
      e := e.nextInAEL;
    end;
  end;

  //3. Process horizontals at the top of the scanbeam ...
  ProcessHorizontals;

  //4. Promote intermediate vertices ...
  e := FActiveEdges;
  while Assigned(e) do
  begin
    if IsIntermediate(e, topY) then
    begin
      if (e.outIdx >= 0) then AddOutPt(e, FixedPoint2(e.xtop, e.ytop));
      UpdateEdgeIntoAEL(e);

      //if output polygons share an edge, they'll need joining later ...
      if (e.outIdx >= 0) and Assigned(e.prevInAEL) and
        (e.prevInAEL.outIdx >= 0) and
        (e.prevInAEL.xcurr = e.xbot) and (e.prevInAEL.ycurr = e.ybot) and
        SlopesEqual(FixedPoint2(e.xbot,e.ybot), FixedPoint2(e.xtop, e.ytop),
          FixedPoint2(e.xbot,e.ybot),
          FixedPoint2(e.prevInAEL.xtop, e.prevInAEL.ytop)) then
      begin
        AddOutPt(e.prevInAEL, FixedPoint2(e.xbot, e.ybot));
        AddJoin(e, e.prevInAEL);
      end
      else if (e.outIdx >= 0) and Assigned(e.nextInAEL) and
        (e.nextInAEL.outIdx >= 0) and (e.nextInAEL.ycurr > e.nextInAEL.ytop) and
        (e.nextInAEL.ycurr <= e.nextInAEL.ybot) and
        (e.nextInAEL.xcurr = e.xbot) and (e.nextInAEL.ycurr = e.ybot) and
        SlopesEqual(FixedPoint2(e.xbot,e.ybot), FixedPoint2(e.xtop, e.ytop),
          FixedPoint2(e.xbot,e.ybot),
          FixedPoint2(e.nextInAEL.xtop, e.nextInAEL.ytop)) then
      begin
        AddOutPt(e.nextInAEL, FixedPoint2(e.xbot, e.ybot));
        AddJoin(e, e.nextInAEL);
      end;
    end;
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.GetResult: TArrayOfArrayOfFixedPoint;
var
  i,j,k,cnt: Integer;
  outRec: POutRec;
  op: POutPt;
begin
  k := 0;
  SetLength(Result, FPolyOutList.Count);
  for i := 0 to FPolyOutList.Count - 1 do
    if Assigned(FPolyOutList[i]) then
    begin
      //make sure each polygon has at least 3 vertices ...
      outRec := FPolyOutList[i];
      op := outRec.Pts;
      if not Assigned(op) then Continue; //nb: not sorted
      cnt := PointCount(op);
      if (cnt < 3) then Continue;

      SetLength(Result[k], cnt);
      for j := 0 to cnt - 1 do
      begin
        Result[k, j].X := op.pt.X;
        Result[k, j].Y := op.pt.Y;
        op := op.next;
      end;
      Inc(k);
    end;
  SetLength(Result, k);
end;
//------------------------------------------------------------------------------

procedure TClipper.FixupOutPolygon(outRec: POutRec);
var
  pp, tmp, lastOK: POutPt;
begin
  //FixupOutPolygon() - removes duplicate points and simplifies consecutive
  //parallel edges by removing the middle vertex.
  lastOK := nil;
  outRec.Pts := outRec.bottomPt;
  pp := outRec.Pts;
  while True do
  begin
    if (pp.prev = pp) or (pp.next = pp.prev) then
    begin
      DisposePolyPts(pp);
      outRec.Pts := nil;
      outRec.bottomPt := nil;
      Exit;
    end;

    //test for duplicate points and for colinear edges ...
    if PointsEqual(pp.pt, pp.next.pt) or
      SlopesEqual(pp.prev.pt, pp.pt, pp.next.pt) then
    begin
      //OK, we need to delete a point ...
      lastOK := nil;
      tmp := pp;
      if pp = outRec.bottomPt then
        outRec.bottomPt := nil; //flags need for updating
      pp.prev.next := pp.next;
      pp.next.prev := pp.prev;
      pp := pp.prev;
      dispose(tmp);
    end
    else if pp = lastOK then break
    else
    begin
      if not Assigned(lastOK) then lastOK := pp;
      pp := pp.next;
    end;
  end;
  if not Assigned(outRec.bottomPt) then
  begin
    outRec.bottomPt := GetBottomPt(pp);
    outRec.bottomPt.idx := outRec.idx;
    outRec.Pts := outRec.bottomPt;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.FixupIntersections: Boolean;
var
  e1, e2: PEdge;
  int1, int2: PIntersectNode;
begin
  Result := not Assigned(FIntersectNodes.next);
  if Result then Exit;
  //logic: only swap (intersect) adjacent edges ...
  try
    CopyAELToSEL;
    int1 := FIntersectNodes;
    int2 := FIntersectNodes.next;
    while Assigned(int2) do
    begin
      e1 := int1.edge1;
      if (e1.prevInSEL = int1.edge2) then e2 := e1.prevInSEL
      else if (e1.nextInSEL = int1.edge2) then e2 := e1.nextInSEL
      else
      begin
        //The current intersection is out of order, so try and swap it with
        //a subsequent intersection ...
        while Assigned(int2) do
        begin
          if (int2.edge1.nextInSEL = int2.edge2) or
            (int2.edge1.prevInSEL = int2.edge2) then break
          else int2 := int2.next;
        end;
        if not Assigned(int2) then Exit; //oops!!!
        //found an intersect node that can be swapped ...
        SwapIntersectNodes(int1, int2);
        e1 := int1.edge1;
        e2 := int1.edge2;
      end;
      SwapPositionsInSEL(e1, e2);
      int1 := int1.next;
      int2 := int1.next;
    end;

    //finally, check the last intersection too ...
    Result := (int1.edge1.prevInSEL = int1.edge2) or
      (int1.edge1.nextInSEL = int1.edge2);
  finally
    FSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapIntersectNodes(int1, int2: PIntersectNode);
var
  e1,e2: PEdge;
  p: TFixedPoint;
begin
  with int1^ do
  begin
    e1 := edge1;
    edge1 := int2.edge1;
    e2 := edge2;
    edge2 := int2.edge2;
    p := pt;
    pt := int2.pt;
  end;
  with int2^ do
  begin
    edge1 := e1;
    edge2 := e2;
    pt := p;
  end;
end;
//------------------------------------------------------------------------------

function FindSegment(var pp: POutPt; var pt1, pt2: TFixedPoint): Boolean;
var
  pp2: POutPt;
  pt1a, pt2a: TFixedPoint;
begin
  if not Assigned(pp) then begin Result := False; Exit; end;
  Result := True;
  pt1a := pt1; pt2a := pt2;
  pp2 := pp;
  repeat
    //test for co-linearity before testing for overlap ...
    if SlopesEqual(pt1a, pt2a, pp.pt, pp.prev.pt) and
      SlopesEqual(pt1a, pt2a, pp.pt) and
        GetOverlapSegment(pt1a, pt2a, pp.pt, pp.prev.pt, pt1, pt2) then Exit;
    pp := pp.next;
  until pp = pp2;
  Result := False;
end;
//------------------------------------------------------------------------------

function Pt3IsBetweenPt1AndPt2(const pt1, pt2, pt3: TFixedPoint): Boolean;
begin
  if PointsEqual(pt1, pt3) then Result := True
  else if PointsEqual(pt2, pt3) then Result := True
  else if (pt1.X <> pt2.X) then Result := (pt1.X < pt3.X) = (pt3.X < pt2.X)
  else Result := (pt1.Y < pt3.Y) = (pt3.Y < pt2.Y);
end;
//------------------------------------------------------------------------------

function InsertPolyPtBetween(p1, p2: POutPt; const pt: TFixedPoint): POutPt;
begin
  if (p1 = p2) then raise Exception.Create(rsJoinError);

  New(Result);
  Result.pt := pt;
  Result.idx := p1.idx;
  if p2 = p1.next then
  begin
    p1.next := Result;
    p2.prev := Result;
    Result.next := p2;
    Result.prev := p1;
  end else
  begin
    p2.next := Result;
    p1.prev := Result;
    Result.next := p1;
    Result.prev := p2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages1(const outRec1, outRec2: POutRec);
var
  i: Integer;
begin
  //when a polygon is split into 2 polygons, make sure any holes the original
  //polygon contained link to the correct polygon ...
  for i := 0 to FPolyOutList.Count - 1 do
    with POutRec(FPolyOutList[i])^ do
      if isHole and Assigned(bottomPt) and (FirstLeft = OutRec1) and
         not PointInPolygon(BottomPt.pt, outRec1.Pts) then
          FirstLeft := outRec2;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages2(const outRec1, outRec2: POutRec);
var
  i: Integer;
begin
  //if a hole is owned by outRec2 then make it owned by outRec1 ...
  for i := 0 to FPolyOutList.Count - 1 do
    with POutRec(FPolyOutList[i])^ do
      if isHole and Assigned(bottomPt) and (FirstLeft = OutRec2) then
        FirstLeft := outRec1;
end;
//------------------------------------------------------------------------------

procedure TClipper.JoinCommonEdges(fixHoleLinkages: Boolean);
var
  i, j, OKIdx, ObsoleteIdx: Integer;
  jr, jr2: PJoinRec;
  outRec1, outRec2: POutRec;
  prev, p1, p2, p3, p4, pp1a, pp2a: POutPt;
  pt1, pt2, pt3, pt4: TFixedPoint;
begin
  for i := 0 to FJoinList.count - 1 do
  begin
    jr := FJoinList[i];
    outRec1 := FPolyOutList[jr.poly1Idx];
    if not Assigned(outRec1) then Continue;
    pp1a := outRec1.Pts;
    outRec2 := FPolyOutList[jr.poly2Idx];
    if not Assigned(outRec2) then Continue;
    pp2a := outRec2.Pts;
    pt1 := jr.pt2a; pt2 := jr.pt2b;
    pt3 := jr.pt1a; pt4 := jr.pt1b;
    if not FindSegment(pp1a, pt1, pt2) then Continue;
    if (jr.poly1Idx = jr.poly2Idx) then
    begin
      //we're searching the same polygon for overlapping segments so
      //segment 2 mustn't be the same as segment 1 ...
      pp2a := pp1a.next;
      if not FindSegment(pp2a, pt3, pt4) or (pp2a = pp1a) then Continue;
    end else
      if not FindSegment(pp2a, pt3, pt4) then Continue;

    if not GetOverlapSegment(pt1, pt2, pt3, pt4, pt1, pt2) then Continue;

    prev := pp1a.prev;
    if PointsEqual(pp1a.pt, pt1) then p1 := pp1a
    else if PointsEqual(prev.pt, pt1) then p1 := prev
    else p1 := InsertPolyPtBetween(pp1a, prev, pt1);

    if PointsEqual(pp1a.pt, pt2) then p2 := pp1a
    else if PointsEqual(prev.pt, pt2) then p2 := prev
    else if (p1 = pp1a) or (p1 = prev) then
      p2 := InsertPolyPtBetween(pp1a, prev, pt2)
    else if Pt3IsBetweenPt1AndPt2(pp1a.pt, p1.pt, pt2) then
      p2 := InsertPolyPtBetween(pp1a, p1, pt2)
    else
      p2 := InsertPolyPtBetween(p1, prev, pt2);

    prev := pp2a.prev;
    if PointsEqual(pp2a.pt, pt1) then p3 := pp2a
    else if PointsEqual(prev.pt, pt1) then p3 := prev
    else p3 := InsertPolyPtBetween(pp2a, prev, pt1);

    if PointsEqual(pp2a.pt, pt2) then p4 := pp2a
    else if PointsEqual(prev.pt, pt2) then p4 := prev
    else if (p3 = pp2a) or (p3 = prev) then
      p4 := InsertPolyPtBetween(pp2a, prev, pt2)
    else if Pt3IsBetweenPt1AndPt2(pp2a.pt, p3.pt, pt2) then
      p4 := InsertPolyPtBetween(pp2a, p3, pt2)
    else
      p4 := InsertPolyPtBetween(p3, prev, pt2);

    //p1.pt == p3.pt and p2.pt == p4.pt so join p1 to p3 and p2 to p4 ...
    if (p1.next = p2) and (p3.prev = p4) then
    begin
      p1.next := p3;
      p3.prev := p1;
      p2.prev := p4;
      p4.next := p2;
    end
    else if (p1.prev = p2) and (p3.next = p4) then
    begin
      p1.prev := p3;
      p3.next := p1;
      p2.next := p4;
      p4.prev := p2;
    end
    else
      //it's very rare to get here, and when we do almost invariably
      //p1.idx == p2.idx, otherwise it's an orientation error.
      Continue;

    if (jr.poly2Idx = jr.poly1Idx) then
    begin
      //instead of joining two polygons, we've just created a new one by
      //splitting one polygon into two.
      outRec1.Pts := GetBottomPt(p1);
      outRec1.bottomPt := outRec1.Pts;
      outRec1.bottomPt.idx := outRec1.idx;
      outRec2 := CreateOutRec;
      outRec2.idx := FPolyOutList.Add(outRec2);
      jr.poly2Idx := outRec2.idx;
      outRec2.Pts := GetBottomPt(p2);
      outRec2.bottomPt := outRec2.Pts;
      outRec2.bottomPt.idx := outRec2.idx;

      if PointInPolygon(outRec2.Pts.pt, outRec1.Pts) then
      begin
        //outRec2 is contained by outRec1 ...
        outRec2.isHole := not outRec1.isHole;
        outRec2.FirstLeft := outRec1;
        if not Orientation(outRec2) then
          ReversePolyPtLinks(outRec2.Pts);
      end else if PointInPolygon(outRec1.Pts.pt, outRec2.Pts) then
      begin
        //outRec1 is contained by outRec2 ...
        outRec2.isHole := outRec1.isHole;
        outRec1.isHole := not outRec2.isHole;
        outRec2.FirstLeft := outRec1.FirstLeft;
        outRec1.FirstLeft := outRec2;
        if not Orientation(outRec1) then
          ReversePolyPtLinks(outRec1.Pts);
        //make sure any contained holes now link to the correct polygon ...
        if fixHoleLinkages then CheckHoleLinkages1(outRec1, outRec2);
      end else
      begin
        outRec2.isHole := outRec1.isHole;
        outRec2.FirstLeft := outRec1.FirstLeft;
        //make sure any contained holes now link to the correct polygon ...
        if fixHoleLinkages then CheckHoleLinkages1(outRec1, outRec2);
      end;

      //now fixup any subsequent joins that match this polygon
      for j := i + 1 to FJoinList.count - 1 do
      begin
        jr2 := FJoinList[j];
        if (jr2.poly1Idx = jr.poly1Idx) and PointIsVertex(jr2.pt1a, p2) then
          jr2.poly1Idx := jr.poly2Idx;
        if (jr2.poly2Idx = jr.poly1Idx) and PointIsVertex(jr2.pt2a, p2) then
          jr2.poly2Idx := jr.poly2Idx;
      end;

      //cleanup edges ...
      FixupOutPolygon(outRec1);
      FixupOutPolygon(outRec2);

      if (Orientation(outRec1) <> (Area(outRec1) > 0)) then
        DisposeBottomPt(outRec1);
      if (Orientation(outRec2) <> (Area(outRec2) > 0)) then
        DisposeBottomPt(outRec2);

    end else
    begin
      //joined 2 polygons together ...

      //make sure any holes contained by outRec2 now link to outRec1 ...
      if fixHoleLinkages then CheckHoleLinkages2(outRec1, outRec2);

      //cleanup edges ...
      FixupOutPolygon(outRec1);

      if Assigned(outRec1.Pts) then
      begin
        outRec1.isHole := not Orientation(outRec1);
        if outRec1.isHole and not Assigned(outRec1.FirstLeft) then
          outRec1.FirstLeft := outRec2.FirstLeft;
      end;

      //delete the obsolete pointer ...
      OKIdx := outRec1.idx;
      ObsoleteIdx := outRec2.idx;
      outRec2.Pts := nil;
      outRec2.bottomPt := nil;
      outRec2.AppendLink := outRec1;

      //now fixup any subsequent joins ...
      for j := i + 1 to FJoinList.count - 1 do
      begin
        jr2 := FJoinList[j];
        if (jr2.poly1Idx = ObsoleteIdx) then jr2.poly1Idx := OKIdx;
        if (jr2.poly2Idx = ObsoleteIdx) then jr2.poly2Idx := OKIdx;
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------
// OffsetPolygons ...
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TFixedPoint): TDoublePoint;
var
  dx, dy, f: Single;
begin
  if (pt2.X = pt1.X) and (pt2.Y = pt1.Y) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;

  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  f := 1 / Hypot(dx, dy);
  dx := dx * f;
  dy := dy * f;
  Result.X := dy;
  Result.Y := -dx
end;
//------------------------------------------------------------------------------

function GetBounds(const a: TArrayOfArrayOfFixedPoint): TFixedRect;
var
  i,j: Integer;
begin
  with Result do
  begin
    Left := CLoRange; Top := CLoRange;
    Right := -CLoRange; Bottom := -CLoRange;
  end;
  for i := 0 to High(a) do
    for j := 0 to High(a[i]) do
    begin
      if a[i, j].X < Result.Left then Result.Left := a[i, j].X;
      if a[i, j].X > Result.Right then Result.Right := a[i, j].X;
      if a[i, j].Y < Result.Top then Result.Top := a[i, j].Y;
      if a[i, j].Y > Result.Bottom then Result.Bottom := a[i, j].Y;
    end;
  if Result.left = CLoRange then
    with Result do begin Left := 0; Top := 0; Right := 0; Bottom := 0; end;
end;
//------------------------------------------------------------------------------

function OffsetPolygons(const aaFxPts: TArrayOfArrayOfFixedPoint; const Delta: TFixed;
  JoinType: TJoinType = jtSquare; MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint;
var
  i, j, k, Len, OutLen: Integer;
  Normals: TArrayOfDoublePoint;
  R, RMin: Double;
  pt1, pt2: TFixedPoint;
  Outer: TArrayOfFixedPoint;
  Bounds: TFixedRect;
const
  BuffLength: Integer = 128;

  procedure AddPoint(const pt: TFixedPoint);
  var
    Len: Integer;
  begin
    Len := Length(Result[i]);
    if OutLen = Len then
      SetLength(Result[i], Len + BuffLength);
    Result[i, OutLen].X := pt.X;
    Result[i, OutLen].Y := pt.Y;
    Inc(OutLen);
  end;

  procedure DoSquare(mul: Double = 1.0);
  var
    a1, a2, dx: Double;
  begin
    pt1.X := Round(aaFxPts[i, j].X + Normals[k].X * Delta);
    pt1.Y := Round(aaFxPts[i, j].Y + Normals[k].Y * Delta);
    pt2.X := Round(aaFxPts[i, j].X + Normals[j].X * Delta);
    pt2.Y := Round(aaFxPts[i, j].Y + Normals[j].Y * Delta);
    if ((Normals[k].X*Normals[j].Y-Normals[j].X*Normals[k].Y) * Delta >= 0) then
    begin
      a1 := ArcTan2(Normals[k].Y, Normals[k].X);
      a2 := ArcTan2(-Normals[j].Y, -Normals[j].X);
      a1 := abs(a2 - a1);
      if a1 > pi then a1 := pi*2 - a1;
      dx := tan((pi - a1)/4) * abs(Delta*mul);

      pt1 := FixedPoint2(Round(pt1.X -Normals[k].Y *dx),
        Round(pt1.Y + Normals[k].X *dx));
      AddPoint(pt1);
      pt2 := FixedPoint2(Round(pt2.X + Normals[j].Y *dx),
        Round(pt2.Y - Normals[j].X *dx));
      AddPoint(pt2);
    end else
    begin
      AddPoint(pt1);
      AddPoint(aaFxPts[i, j]);
      AddPoint(pt2);
    end;
  end;

  procedure DoMiter;
  var
    q: Double;
  begin
    if ((Normals[k].X * Normals[j].Y - Normals[j].X * Normals[k].Y) * Delta >= 0) then
    begin
      q := Delta / R;
      AddPoint(FixedPoint2(Round(aaFxPts[i, j].X + (Normals[k].X + Normals[j].X) * q),
        Round(aaFxPts[i, j].Y + (Normals[k].Y + Normals[j].Y) *q)));
    end else
    begin
      pt1.X := Round(aaFxPts[i, j].X + Normals[k].X * Delta);
      pt1.Y := Round(aaFxPts[i, j].Y + Normals[k].Y * Delta);
      pt2.X := Round(aaFxPts[i, j].X + Normals[j].X * Delta);
      pt2.Y := Round(aaFxPts[i, j].Y + Normals[j].Y * Delta);
      AddPoint(pt1);
      AddPoint(aaFxPts[i, j]);
      AddPoint(pt2);
    end;
  end;

  procedure DoRound;
  var
    m: Integer;
    arc: TArrayOfFixedPoint;
    a1, a2: Double;
  begin
    pt1.X := Round(aaFxPts[i, j].X + Normals[k].X * Delta);
    pt1.Y := Round(aaFxPts[i, j].Y + Normals[k].Y * Delta);
    pt2.X := Round(aaFxPts[i, j].X + Normals[j].X * Delta);
    pt2.Y := Round(aaFxPts[i, j].Y + Normals[j].Y * Delta);
    AddPoint(pt1);
    //Round off reflex angles (ie > 180 deg) unless almost flat (ie < 10deg).
    //(N1.X * N2.Y - N2.X * N1.Y) == unit normal "cross product" == sin(angle)
    //(N1.X * N2.X + N1.Y * N2.Y) == unit normal "dot product" == cos(angle)
    //dot product Normals == 1 -> no angle
    if ((Normals[k].X*Normals[j].Y - Normals[j].X*Normals[k].Y)*Delta >= 0) then
    begin
      if ((Normals[j].X*Normals[k].X+Normals[j].Y*Normals[k].Y) < 0.985) then
      begin
        a1 := ArcTan2(Normals[k].Y, Normals[k].X);
        a2 := ArcTan2(Normals[j].Y, Normals[j].X);
        if (Delta > 0) and (a2 < a1) then a2 := a2 + pi*2
        else if (Delta < 0) and (a2 > a1) then a2 := a2 - pi*2;
        arc := BuildArc(aaFxPts[i, j], a1, a2, Delta);
        for m := 0 to High(arc) do
          AddPoint(arc[m]);
      end;
    end else
      AddPoint(aaFxPts[i, j]);
    AddPoint(pt2);
  end;

begin
  //MiterLimit defaults to twice Delta's width ...
  if MiterLimit <= 1 then MiterLimit := 1;
  RMin := 2/(Sqr(MiterLimit));

  SetLength(Result, Length(aaFxPts));
  i := Length(Result);

  for i := 0 to High(aaFxPts) do
  begin
    Result[i] := nil;
    Len := Length(aaFxPts[i]);
    if (Len > 1) and (aaFxPts[i, 0].X = aaFxPts[i, Len - 1].X) and
        (aaFxPts[i, 0].Y = aaFxPts[i, Len - 1].Y) then Dec(Len);

    if (Len = 0) or ((Len < 3) and (Delta < 0)) then Continue;

    if (Len = 1) then
    begin
      Result[i] := BuildArc(aaFxPts[i, 0], 0, 2*pi, Delta);
      Continue;
    end;

    //build Normals ...
    SetLength(Normals, Len);
    for j := 0 to Len-2 do
      Normals[j] := GetUnitNormal(aaFxPts[i, j], aaFxPts[i, j + 1]);
    Normals[Len - 1] := GetUnitNormal(aaFxPts[i, Len-1], aaFxPts[i, 0]);

    OutLen := 0;
    k := Len - 1;
    for j := 0 to Len - 1 do
    begin
      case JoinType of
        jtMiter:
        begin
          R := 1 + (Normals[j].X * Normals[k].X + Normals[j].Y * Normals[k].Y);
          if (R >= RMin) then
            DoMiter else
            DoSquare(MiterLimit);
        end;
        jtSquare: DoSquare;
        jtRound: DoRound;
      end;
      k := j;
    end;
    SetLength(Result[i], OutLen);
  end;

  //finally, clean up untidy corners ...
  with TClipper.Create do
  try
    AddArrayOfArrayOfFixedPoint(Result, ptSubject);
    if Delta > 0 then
    begin
      Execute(ctUnion, Result, pftPositive, pftPositive);
    end else
    begin
      Bounds := GetBounds(Result);
      SetLength(Outer, 4);
      Outer[0] := TFixedPoint(GR32.Point(Bounds.left - 10, Bounds.bottom + 10));
      Outer[1] := TFixedPoint(GR32.Point(Bounds.right + 10, Bounds.bottom + 10));
      Outer[2] := TFixedPoint(GR32.Point(Bounds.right + 10, Bounds.top - 10));
      Outer[3] := TFixedPoint(GR32.Point(Bounds.left - 10, Bounds.top - 10));
      AddArrayOfFixedPoint(Outer, ptSubject);
      Execute(ctUnion, Result, pftNegative, pftNegative);
      //delete the Outer rectangle ...
      Len := Length(Result);
      for j := 1 to Len - 1 do Result[j - 1] := Result[j];
      if Len > 0 then
        SetLength(Result, Len - 1);
      //restore polygon orientation ...
      Result := ReversePolygons(Result);
    end;
  finally
    free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
