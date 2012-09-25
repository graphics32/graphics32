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
  TEdgeArray = array[0.. MaxInt div sizeof(TEdge) -1] of TEdge;

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
    pts         : POutPt;
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
    fEdgeList      : TList;
    fLmList        : PLocalMinima; //localMinima list
    fCurrLm        : PLocalMinima; //current localMinima node
    fUse64BitRange : Boolean;      //see loRange and hiRange consts notes below
    procedure DisposeLocalMinimaList;
  protected
    procedure Reset; virtual;
    procedure PopLocalMinima;
    property CurrentLm: PLocalMinima read fCurrLm;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddArrayOfFixedPoint(const afp: TArrayOfFixedPoint; polyType: TPolyType): Boolean;
    function AddArrayOfArrayOfFixedPoint(const aafp: TArrayOfArrayOfFixedPoint; polyType: TPolyType): Boolean;
    procedure Clear; virtual;
  end;

  TClipper = class(TClipperBase)
  private
    fPolyOutList   : TList;
    fJoinList      : TList;
    fClipType      : TClipType;
    fScanbeam      : PScanbeam; //scanbeam list
    fActiveEdges   : PEdge;     //active edge list
    fSortedEdges   : PEdge;     //used for temporary sorting
    fIntersectNodes: PIntersectNode;
    fClipFillType  : TPolyFillType;
    fSubjFillType  : TPolyFillType;
    fExecuteLocked : Boolean;
    fHorizJoins    : PHorzRec;
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
      clipType: TClipType;
      out solution: TArrayOfArrayOfFixedPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): Boolean; overload;

    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
  end;

function Orientation(const pts: TArrayOfFixedPoint): Boolean; overload;

//OffsetPolygons precondition: outer polygons MUST be oriented clockwise,
//and inner 'hole' polygons MUST be oriented counter-clockwise ...
function OffsetPolygons(const aaFxPts: TArrayOfArrayOfFixedPoint; const delta: TFixed;
  JoinType: TJoinType = jtSquare; MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint;

implementation

uses SysUtils, Types, Math;

type
  TDoublePoint = record X, Y: Double; end;
  TArrayOfDoublePoint = array of TDoublePoint;

const
  horizontal: Double = -3.4e+38;
  loRange: TFixed = $3FFFFFFF;          

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

function PointCount(pts: POutPt): Integer;
var
  p: POutPt;
begin
  result := 0;
  if not assigned(pts) then Exit;
  p := pts;
  repeat
    inc(Result);
    p := p.next;
  until p = pts;
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

function Orientation(const pts: TArrayOfFixedPoint): Boolean; overload;
var
  i, j, jplus, jminus, highI: Integer;
  vec1X, vec2X, vec1Y, vec2Y: Int64;
begin
  result := true;
  highI := high(pts);
  if highI < 2 then exit;
  j := 0;
  for i := 0 to highI do
  begin
    if (pts[i].Y < pts[j].Y) then continue;
    if ((pts[i].Y > pts[j].Y) or (pts[i].X < pts[j].X)) then j := i;
  end;
  if j = highI then jplus := 0
  else jplus := j+1;
  if j = 0 then jminus := highI
  else jminus := j-1;

  //get cross product of vectors of edges adjacent the point with largest Y ...
  vec1X := pts[j].X - pts[jminus].X;
  vec1Y := pts[j].Y - pts[jminus].Y;
  vec2X := pts[jplus].X - pts[j].X;
  vec2Y := pts[jplus].Y - pts[j].Y;
  result := ((vec1X * vec2Y) - (vec2X * vec1Y)) >= 0;
end;
//------------------------------------------------------------------------------

function Orientation(outRec: POutRec): Boolean; overload;
var
  op, opBottom, opPrev, opNext: POutPt;
  vec1X, vec2X, vec1Y, vec2Y: Int64;
begin
  //first make sure bottomPt is correctly assigned ...
  opBottom := outRec.pts;
  op := opBottom.next;
  while op <> outRec.pts do
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
  result := ((vec1X * vec2Y) - (vec2X * vec1Y)) >= 0;
end;
//------------------------------------------------------------------------------

function Area(const pts: TArrayOfFixedPoint): Double; overload;
var
  i, highI: Integer;
  d: Double;
begin
  result := 0;
  highI := high(pts);
  if highI < 2 then exit;
  d := pts[highI].X * pts[0].Y - pts[0].X * pts[highI].Y;
  for i := 1 to highI do
    d := d + (pts[i-1].X * pts[i].Y) - (pts[i].X * pts[i-1].Y);
  result := d / 2;
end;
//------------------------------------------------------------------------------

function Area(outRec: POutRec): Double; overload;
var
  op: POutPt;
  d: Double;
begin
  op := outRec.pts;
  d := 0;
  repeat
    d := d + (op.pt.X * op.next.pt.Y) - (op.next.pt.X * op.pt.Y);
    op := op.next;
  until op = outRec.pts;
  result := d / 2;
end;
//------------------------------------------------------------------------------

function ReversePolygons(const pts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFixedPoint;
var
  i, j, highJ: Integer;
begin
  i := length(pts);
  SetLength(result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(pts[i]);
    SetLength(result[i], highJ+1);
    for j := 0 to highJ do
      result[i][j] := pts[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

function PointIsVertex(const pt: TFixedPoint; pp: POutPt): Boolean;
var
  pp2: POutPt;
begin
  Result := true;
  pp2 := pp;
  repeat
    if PointsEqual(pp2.pt, pt) then exit;
    pp2 := pp2.next;
  until pp2 = pp;
  Result := false;
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
      (pp2.prev.pt.Y - pp2.pt.Y) + pp2.pt.X)) then result := not result;
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
  result := Dy1 * Dx2 = Dx1 * Dy2;
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
  result := Dy1 * Dx2 = Dx1 * Dy2;
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
  result := Dy1 * Dx2 = Dx1 * Dy2;
end;
//---------------------------------------------------------------------------

//                 0(90º)                                                  //
//                 |                                                       //
// +inf (180º) --- o --- -inf (0º)                                         //
function GetDx(const pt1, pt2: TFixedPoint): Double;
begin
  if (pt1.Y = pt2.Y) then result := horizontal
  else result := (pt2.X - pt1.X)/(pt2.Y - pt1.Y);
end;
//---------------------------------------------------------------------------

procedure SetDx(e: PEdge);
begin
  if (e.ybot = e.ytop) then e.dx := horizontal
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
  if currentY = edge.ytop then result := edge.xtop
  else if edge.xtop = edge.xbot then result := edge.xbot
  else result := edge.xbot + round(edge.dx*(currentY - edge.ybot));
end;
//------------------------------------------------------------------------------

function IntersectPoint(edge1, edge2: PEdge; out ip: TFixedPoint): Boolean; overload;
var
  b1,b2: Double;
begin
  if SlopesEqual(edge1, edge2) then
  begin
    result := false;
    exit;
  end;
  if edge1.dx = 0 then
  begin
    ip.X := edge1.xbot;
    if edge2.dx = horizontal then
      ip.Y := edge2.ybot
    else
    begin
      with edge2^ do b2 := ybot - (xbot/dx);
      ip.Y := round(ip.X/edge2.dx + b2);
    end;
  end
  else if edge2.dx = 0 then
  begin
    ip.X := edge2.xbot;
    if edge1.dx = horizontal then
      ip.Y := edge1.ybot
    else
    begin
      with edge1^ do b1 := ybot - (xbot/dx);
      ip.Y := round(ip.X/edge1.dx + b1);
    end;
  end else
  begin
    with edge1^ do b1 := xbot - ybot *dx;
    with edge2^ do b2 := xbot - ybot *dx;
    b2 := (b2-b1)/(edge1.dx - edge2.dx);
    ip.Y := round(b2);
    ip.X := round(edge1.dx * b2 + b1);
  end;

  //The precondition - e.tmpX > eNext.tmpX - indicates that the two edges do
  //intersect below topY (and hence below the tops of either edge). However,
  //when edges are almost parallel, rounding errors may cause false positives -
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
      result := TopX(edge2, edge1.ytop) < edge1.xtop;
      ip.X := edge1.xtop;
      ip.Y := edge1.ytop;
    end else
    begin
      result := TopX(edge1, edge2.ytop) > edge2.xtop;
      ip.X := edge2.xtop;
      ip.Y := edge2.ytop;
    end;
  end else
    result := true;
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
  fEdgeList := TList.Create;
  fLmList := nil;
  fCurrLm := nil;
  fUse64BitRange := false; //ie default is false
end;
//------------------------------------------------------------------------------

destructor TClipperBase.Destroy;
begin
  Clear;
  fEdgeList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TClipperBase.AddArrayOfFixedPoint(
  const afp: TArrayOfFixedPoint; polyType: TPolyType): Boolean;

  //----------------------------------------------------------------------

  procedure InitEdge(e, eNext, ePrev: PEdge; const pt: TFixedPoint);
  begin
    fillChar(e^, sizeof(TEdge), 0);
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
    //swap horizontal edges' top and bottom x's so they follow the natural
    //progression of the bounds - ie so their xbots will align with the
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
    if not assigned(fLmList) then
    begin
      fLmList := lm;
    end
    else if (lm.y >= fLmList.y) then
    begin
      lm.next := fLmList;
      fLmList := lm;
    end else
    begin
      tmpLm := fLmList;
      while assigned(tmpLm.next) and (lm.y < tmpLm.next.y) do
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
    //a local minima. We then go to the top of the next bound. These two bounds
    //form the left and right (or right and left) bounds of the local minima.
    e.nextInLML := nil;
    e := e.next;
    repeat
      if e.dx = horizontal then
      begin
        //nb: proceed through horizontals when approaching from their right,
        //    but break on horizontal minima if approaching from their left.
        //    This ensures 'local minima' are always on the left of horizontals.
        if (e.next.ytop < e.ytop) and (e.next.xbot > e.prev.xbot) then break;
        if (e.xtop <> e.prev.xbot) then SwapX(e);
        //e.windDelta := 0; safe option to consider when redesigning
        e.nextInLML := e.prev;
      end
      else if (e.ybot = e.prev.ybot) then break
      else e.nextInLML := e.prev;
      e := e.next;
    until false;

    //e and e.prev are now at a local minima ...
    new(newLm);
    newLm.y := e.prev.ybot;
    newLm.next := nil;
    if e.dx = horizontal then //horizontal edges never start a left bound
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
      if (e.next.ytop = e.ytop) and not (e.next.dx = horizontal) then break;
      e.nextInLML := e.next;
      e := e.next;
      if (e.dx = horizontal) and (e.xbot <> e.prev.xtop) then SwapX(e);
    until false;
    result := e.next;
  end;
  //----------------------------------------------------------------------

var
  i, j, len: Integer;
  edges: PEdgeArray;
  e, eHighest: PEdge;
  pg: TArrayOfFixedPoint;
begin
  result := false; //ie assume nothing added
  len := length(afp);
  if len < 3 then exit;
  setlength(pg, len);
  pg[0].X := afp[0].X;
  pg[0].Y := afp[0].Y;
  j := 0;
  //1. check that coordinate values are within the valid range, and
  //2. remove duplicate points and co-linear points
  for i := 1 to len-1 do
  begin
    if ((abs(afp[i].X) > loRange) or (abs(afp[i].Y) > loRange)) then
      raise exception.Create(rsInvalidInt);
    if (pg[j].X = afp[i].X) and (pg[j].Y = afp[i].Y) then continue
    else if (j > 0) and SlopesEqual(pg[j-1], pg[j], afp[i]) then
    begin
      if (pg[j -1].X = afp[i].X) and (pg[j -1].Y = afp[i].Y) then dec(j);
    end else inc(j);
    pg[j].X := afp[i].X;
    pg[j].Y := afp[i].Y;
  end;
  if (j < 2) then exit;

  //now remove duplicate points and co-linear edges at the loop around of the
  //start and end coordinates ...
  len := j+1;
  while len > 2 do
  begin
    //nb: test for point equality before testing slopes ...
    if PointsEqual(pg[j], pg[0]) then dec(j)
    else if PointsEqual(pg[0], pg[1]) or SlopesEqual(pg[j], pg[0], pg[1]) then
    begin
      pg[0] := pg[j];
      dec(j);
    end
    else if SlopesEqual(pg[j-1], pg[j], pg[0]) then dec(j)
    else if SlopesEqual(pg[0], pg[1], pg[2]) then
    begin
      for i := 2 to j do pg[i-1] := pg[i];
      dec(j);
    end
    else
      break;
    dec(len);
  end;
  if len < 3 then exit;
  result := true;

  GetMem(edges, sizeof(TEdge)*len);
  fEdgeList.Add(edges);

  //convert vertices to a Double-linked-list of edges and initialize ...
  edges[0].xcurr := pg[0].X;
  edges[0].ycurr := pg[0].Y;
  InitEdge(@edges[len-1], @edges[0], @edges[len-2], pg[len-1]);
  for i := len-2 downto 1 do
    InitEdge(@edges[i], @edges[i+1], @edges[i-1], pg[i]);
  InitEdge(@edges[0], @edges[1], @edges[len-1], pg[0]);
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
  if (eHighest.dx = horizontal) then eHighest := eHighest.next;

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
  result := false;
  for i := 0 to high(aafp) do
    if AddArrayOfFixedPoint(aafp[i], polyType) then result := true;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Clear;
var
  i: Integer;
begin
  DisposeLocalMinimaList;
  for i := 0 to fEdgeList.Count -1 do dispose(PEdgeArray(fEdgeList[i]));
  fEdgeList.Clear;
  fUse64BitRange := false;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.Reset;
var
  e: PEdge;
  lm: PLocalMinima;
begin
  //Reset() allows various clipping operations to be executed
  //multiple times on the same polygon sets.

  fCurrLm := fLmList;
  //reset all edges ...
  lm := fCurrLm;
  while assigned(lm) do
  begin
    e := lm.leftBound;
    while assigned(e) do
    begin
      e.xcurr := e.xbot;
      e.ycurr := e.ybot;
      e.side := esLeft;
      e.outIdx := -1;
      e := e.nextInLML;
    end;
    e := lm.rightBound;
    while assigned(e) do
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
  while assigned(fLmList) do
  begin
    fCurrLm := fLmList.next;
    Dispose(fLmList);
    fLmList := fCurrLm;
  end;
  fCurrLm := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperBase.PopLocalMinima;
begin
  if not assigned(fCurrLM) then exit;
  fCurrLM := fCurrLM.next;
end;

//------------------------------------------------------------------------------
// TClipper methods ...
//------------------------------------------------------------------------------

constructor TClipper.Create;
begin
  inherited Create;
  fJoinList := TList.Create;
  fPolyOutList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipper.Destroy;
begin
  inherited; //this must be first since inherited Destroy calls Clear.
  DisposeScanbeamList;
  fJoinList.Free;
  fPolyOutList.Free;
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
  while assigned(fScanbeam) do
  begin
    sb := fScanbeam.next;
    Dispose(fScanbeam);
    fScanbeam := sb;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  lm: PLocalMinima;
begin
  inherited Reset;
  fScanbeam := nil;
  DisposeAllPolyPts;
  lm := fLmList;
  while assigned(lm) do
  begin
    InsertScanbeam(lm.y);
    InsertScanbeam(lm.leftbound.ytop);
    lm := lm.next;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(
  clipType: TClipType;
  out solution: TArrayOfArrayOfFixedPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): Boolean;
begin
  result := false;
  solution := nil;
  if fExecuteLocked then exit;
  try
    fExecuteLocked := true;
    fSubjFillType := subjFillType;
    fClipFillType := clipFillType;
    fClipType := clipType;
    result := ExecuteInternal(false);
    if result then solution := GetResult;
  finally
    fExecuteLocked := false;
  end;
end;
//------------------------------------------------------------------------------

function PolySort(item1, item2: pointer): Integer;
var
  p1, p2: POutRec;
  i1, i2: Integer;
begin
  result := 0;
  if item1 = item2 then exit;
  p1 := item1; p2 := item2;
  if not assigned(p1.pts) or not assigned(p2.pts) then
  begin
    if assigned(p1.pts) then result := -1
    else if assigned(p2.pts) then result := 1;
    exit;
  end;
  if p1.isHole then i1 := p1.FirstLeft.idx
  else i1 := p1.idx;
  if p2.isHole then i2 := p2.FirstLeft.idx
  else i2 := p2.idx;
  result := i1 - i2;
  if (result = 0) and (p1.isHole <> p2.isHole) then
  begin
    if p1.isHole then result := 1
    else result := -1;
  end;
end;
//------------------------------------------------------------------------------

function FindAppendLinkEnd(outRec: POutRec): POutRec;
begin
  while assigned(outRec.AppendLink) do
    outRec := outRec.AppendLink;
  result := outRec;
end;
//------------------------------------------------------------------------------

procedure TClipper.FixHoleLinkage(outRec: POutRec);
var
  tmp: POutRec;
begin
  if assigned(outRec.bottomPt) then
    tmp := POutRec(fPolyOutList[outRec.bottomPt.idx]).FirstLeft else
    tmp := outRec.FirstLeft;
    if (outRec = tmp) then
      raise exception.Create(rsHoleLinkError);

  if assigned(tmp) then
  begin
    if assigned(tmp.AppendLink) then
      tmp := FindAppendLinkEnd(tmp);
    if tmp = outRec then tmp := nil
    else if tmp.isHole then
    begin
      FixHoleLinkage(tmp);
      tmp := tmp.FirstLeft;
    end;
  end;
  outRec.FirstLeft := tmp;
  if not assigned(tmp) then outRec.isHole := false;
  outRec.AppendLink := nil;
end;
//------------------------------------------------------------------------------

function TClipper.ExecuteInternal(fixHoleLinkages: Boolean): Boolean;
var
  i: Integer;
  outRec: POutRec;
  botY, topY: TFixed;
begin
  result := false;
  try try
    Reset;
    if not assigned(fScanbeam) then
    begin
      result := true;
      exit;
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
    until fScanbeam = nil;

    //tidy up output polygons and fix orientations where necessary ...
    for i := 0 to fPolyOutList.Count -1 do
    begin
      outRec := fPolyOutList[i];
      if not assigned(outRec.pts) then continue;
      FixupOutPolygon(outRec);
      if not assigned(outRec.pts) then continue;

      if outRec.isHole and fixHoleLinkages then
        FixHoleLinkage(outRec);
      //outRec.bottomPt might've been cleaned up already so retest orientation
      if (outRec.bottomPt = outRec.bottomFlag) and
        (Orientation(outRec) <> (Area(outRec) > 0)) then
          DisposeBottomPt(outRec);
      if outRec.isHole = Orientation(outRec) then
          ReversePolyPtLinks(outRec.pts);
    end;
    if fJoinList.count > 0 then
      JoinCommonEdges(fixHoleLinkages);

    if fixHoleLinkages then fPolyOutList.Sort(PolySort);
    result := true;
  except
    result := false;
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
  new(sb);
  sb.y := y;
  if not assigned(fScanbeam) then
  begin
    fScanbeam := sb;
    sb.next := nil;
  end else if y > fScanbeam.y then
  begin
    sb.next := fScanbeam;
    fScanbeam := sb;
  end else
  begin
    sb2 := fScanbeam;
    while assigned(sb2.next) and (y <= sb2.next.y) do sb2 := sb2.next;
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
  result := fScanbeam.y;
  sb := fScanbeam;
  fScanbeam := fScanbeam.next;
  dispose(sb);
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeBottomPt(outRec: POutRec);
var
  next, prev: POutPt;
begin
  next := outRec.bottomPt.next;
  prev := outRec.bottomPt.prev;
  if outRec.pts = outRec.bottomPt then
    outRec.pts := next;
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
  while assigned(pp) do
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
  for i := 0 to fPolyOutList.Count -1 do DisposeOutRec(i);
  fPolyOutList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(index: Integer);
var
  outRec: POutRec;
begin
  outRec := fPolyOutList[index];
  if assigned(outRec.pts) then DisposePolyPts(outRec.pts);
  Dispose(outRec);
  fPolyOutList[index] := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindingCount(edge: PEdge);
var
  e: PEdge;
begin
  e := edge.prevInAEL;
  //find the edge of the same polytype that immediately preceeds 'edge' in AEL
  while assigned(e) and (e.polyType <> edge.polyType) do e := e.prevInAEL;
  if not assigned(e) then
  begin
    edge.windCnt := edge.windDelta;
    edge.windCnt2 := 0;
    e := fActiveEdges; //ie get ready to calc windCnt2
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
      inc(edge.windCnt2, e.windDelta);
      e := e.nextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddFillType(edge: PEdge): Boolean;
begin
  if edge.polyType = ptSubject then
    result := fSubjFillType = pftEvenOdd else
    result := fClipFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsEvenOddAltFillType(edge: PEdge): Boolean;
begin
  if edge.polyType = ptSubject then
    result := fClipFillType = pftEvenOdd else
    result := fSubjFillType = pftEvenOdd;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributing(edge: PEdge): Boolean;
var
  pft,pft2: TPolyFillType;
begin
  if edge.polyType = ptSubject then
  begin
    pft := fSubjFillType;
    pft2 := fClipFillType;
  end else
  begin
    pft := fClipFillType;
    pft2 := fSubjFillType
  end;
  case pft of
    pftEvenOdd, pftNonZero: result := abs(edge.windCnt) = 1;
    pftPositive: result := (edge.windCnt = 1);
    else result := (edge.windCnt = -1);
  end;
  if not result then exit;

  case fClipType of
    ctIntersection:
      case pft2 of
        pftEvenOdd, pftNonZero: result := (edge.windCnt2 <> 0);
        pftPositive: result := (edge.windCnt2 > 0);
        pftNegative: result := (edge.windCnt2 < 0);
      end;
    ctUnion:
      case pft2 of
        pftEvenOdd, pftNonZero: result := (edge.windCnt2 = 0);
        pftPositive: result := (edge.windCnt2 <= 0);
        pftNegative: result := (edge.windCnt2 >= 0);
      end;
    ctDifference:
      if edge.polyType = ptSubject then
        case pft2 of
          pftEvenOdd, pftNonZero: result := (edge.windCnt2 = 0);
          pftPositive: result := (edge.windCnt2 <= 0);
          pftNegative: result := (edge.windCnt2 >= 0);
        end
      else
        case pft2 of
          pftEvenOdd, pftNonZero: result := (edge.windCnt2 <> 0);
          pftPositive: result := (edge.windCnt2 > 0);
          pftNegative: result := (edge.windCnt2 < 0);
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(e1, e2: PEdge; const pt: TFixedPoint);
var
  e, prevE: PEdge;
begin
  if (e2.dx = horizontal) or (e1.dx > e2.dx) then
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

  if assigned(prevE) and (prevE.outIdx >= 0) and
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
  //SEL pointers in PEdge are reused to build a list of horizontal edges.
  //However, we don't need to worry about order with horizontal edge processing.
  if not assigned(fSortedEdges) then
  begin
    fSortedEdges := edge;
    edge.prevInSEL := nil;
    edge.nextInSEL := nil;
  end else
  begin
    edge.nextInSEL := fSortedEdges;
    edge.prevInSEL := nil;
    fSortedEdges.prevInSEL := edge;
    fSortedEdges := edge;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CopyAELToSEL;
var
  e: PEdge;
begin
  e := fActiveEdges;
  fSortedEdges := e;
  if not assigned(fActiveEdges) then exit;

  fSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while assigned(e) do
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
  new(jr);
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
  fJoinList.add(jr);
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearJoins;
var
  i: Integer;
begin
  for i := 0 to fJoinList.count -1 do
    Dispose(PJoinRec(fJoinList[i]));
  fJoinList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddHorzJoin(e: PEdge; idx: Integer);
var
  hr: PHorzRec;
begin
  new(hr);
  hr.edge := e;
  hr.savedIdx := idx;
  if fHorizJoins = nil then
  begin
    fHorizJoins := hr;
    hr.next := hr;
    hr.prev := hr;
  end else
  begin
    hr.next := fHorizJoins;
    hr.prev := fHorizJoins.prev;
    fHorizJoins.prev.next := hr;
    fHorizJoins.prev := hr;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ClearHorzJoins;
var
  m, m2: PHorzRec;
begin
  if not assigned(fHorizJoins) then exit;
  m := fHorizJoins;
  m.prev.next := nil;
  while assigned(m) do
  begin
    m2 := m.next;
    dispose(m);
    m := m2;
  end;
  fHorizJoins := nil;
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
    result := pt1.X < pt2.X;
  end else
  begin
    if pt1a.Y < pt1b.Y then SwapPoints(pt1a, pt1b);
    if pt2a.Y < pt2b.Y then SwapPoints(pt2a, pt2b);
    if (pt1a.Y < pt2a.Y) then pt1 := pt1a else pt1 := pt2a;
    if (pt1b.Y > pt2b.Y) then pt2 := pt1b else pt2 := pt2b;
    result := pt1.Y > pt2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const botY: TFixed);

  function E2InsertsBeforeE1(e1,e2: PEdge): Boolean;
  begin
    if e2.xcurr = e1.xcurr then
      result := e2.dx > e1.dx else
      result := e2.xcurr < e1.xcurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertEdgeIntoAEL(edge: PEdge);
  var
    e: PEdge;
  begin
    edge.prevInAEL := nil;
    edge.nextInAEL := nil;
    if not assigned(fActiveEdges) then
    begin
      fActiveEdges := edge;
    end else if E2InsertsBeforeE1(fActiveEdges, edge) then
    begin
      edge.nextInAEL := fActiveEdges;
      fActiveEdges.prevInAEL := edge;
      fActiveEdges := edge;
    end else
    begin
      e := fActiveEdges;
      while assigned(e.nextInAEL) and not E2InsertsBeforeE1(e.nextInAEL, edge) do
        e := e.nextInAEL;
      edge.nextInAEL := e.nextInAEL;
      if assigned(e.nextInAEL) then e.nextInAEL.prevInAEL := edge;
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
  while assigned(CurrentLm) and (CurrentLm.Y = botY) do
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

    if rb.dx = horizontal then
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
      if (rb.dx = horizontal) then
      begin
        if assigned(fHorizJoins) then
        begin
          hj := fHorizJoins;
          repeat
            //if horizontals rb & hj.edge overlap, flag for joining later ...
            if GetOverlapSegment(FixedPoint2(hj.edge.xbot, hj.edge.ybot),
              FixedPoint2(hj.edge.xtop, hj.edge.ytop), FixedPoint2(rb.xbot, rb.ybot),
              FixedPoint2(rb.xtop, rb.ytop), pt, pt2) then
                AddJoin(hj.edge, rb, hj.savedIdx);
            hj := hj.next;
          until hj = fHorizJoins;
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
        if not assigned(e) then raise exception.Create(rsMissingRightbound);
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
  if not assigned(AelPrev) and not assigned(AelNext) and
    (e <> fActiveEdges) then exit; //already deleted
  if assigned(AelPrev) then AelPrev.nextInAEL := AelNext
  else fActiveEdges := AelNext;
  if assigned(AelNext) then AelNext.prevInAEL := AelPrev;
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
  if not assigned(SelPrev) and not assigned(SelNext) and
    (e <> fSortedEdges) then exit; //already deleted
  if assigned(SelPrev) then SelPrev.nextInSEL := SelNext
  else fSortedEdges := SelNext;
  if assigned(SelNext) then SelNext.prevInSEL := SelPrev;
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

  e1stops := not (ipLeft in protects) and not assigned(e1.nextInLML) and
    (e1.xtop = pt.x) and (e1.ytop = pt.y);
  e2stops := not (ipRight in protects) and not assigned(e2.nextInLML) and
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
        inc(e1.windCnt, e2.windDelta);
      if e2.windCnt - e1.windDelta = 0 then
        e2.windCnt := -e2.windCnt else
        dec(e2.windCnt, e1.windDelta);
    end;
  end else
  begin
    if not IsEvenOddFillType(e2) then inc(e1.windCnt2, e2.windDelta)
    else if e1.windCnt2 = 0 then e1.windCnt2 := 1
    else e1.windCnt2 := 0;
    if not IsEvenOddFillType(e1) then dec(e2.windCnt2, e1.windDelta)
    else if e2.windCnt2 = 0 then e2.windCnt2 := 1
    else e2.windCnt2 := 0;
  end;

  if e1.polyType = ptSubject then
  begin
    e1FillType := fSubjFillType;
    e1FillType2 := fClipFillType;
  end else
  begin
    e1FillType := fClipFillType;
    e1FillType2 := fSubjFillType;
  end;
  if e2.polyType = ptSubject then
  begin
    e2FillType := fSubjFillType;
    e2FillType2 := fClipFillType;
  end else
  begin
    e2FillType := fClipFillType;
    e2FillType2 := fSubjFillType;
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
      ((e1.polytype <> e2.polytype) and (fClipType <> ctXor)) then
        AddLocalMaxPoly(e1, e2, pt) else
        DoBothEdges;
  end else if e1Contributing then
  begin
    if ((e2Wc = 0) or (e2Wc = 1)) and
      ((fClipType <> ctIntersection) or (e2.polyType = ptSubject) or
        (e2.windCnt2 <> 0)) then DoEdge1;
  end
  else if e2contributing then
  begin
    if ((e1Wc = 0) or (e1Wc = 1)) and
      ((fClipType <> ctIntersection) or (e1.polyType = ptSubject) or
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
      case fClipType of
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
  result := ((dx1p >= dx2p) and (dx1p >= dx2n)) or
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
  if assigned(dups) then
  begin
    //there appears to be at least 2 vertices at bottomPt so ...
    while dups <> p do
    begin
      if not FirstIsBottomPt(p, dups) then pp := dups;
      dups := dups.next;
      while not PointsEqual(dups.pt, pp.pt) do dups := dups.next;
    end;
  end;
  result := pp;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetHoleState(e: PEdge; outRec: POutRec);
var
  e2: PEdge;
  isHole: Boolean;
begin
  isHole := false;
  e2 := e.prevInAEL;
  while assigned(e2) do
  begin
    if (e2.outIdx >= 0) then
    begin
      isHole := not isHole;
      if not assigned(outRec.FirstLeft) then
        outRec.FirstLeft := POutRec(fPolyOutList[e2.outIdx]);
    end;
    e2 := e2.prevInAEL;
  end;
  if isHole then
    outRec.isHole := true;
end;
//------------------------------------------------------------------------------

function GetLowermostRec(outRec1, outRec2: POutRec): POutRec;
var
  outPt1, outPt2: POutPt;
begin
  outPt1 := outRec1.bottomPt;
  outPt2 := outRec2.bottomPt;
  if (outPt1.pt.Y > outPt2.pt.Y) then result := outRec1
  else if (outPt1.pt.Y < outPt2.pt.Y) then result := outRec2
  else if (outPt1.pt.X < outPt2.pt.X) then result := outRec1
  else if (outPt1.pt.X > outPt2.pt.X) then result := outRec2
  else if (outPt1.next = outPt1) then result := outRec2
  else if (outPt2.next = outPt2) then result := outRec1
  else if FirstIsBottomPt(outPt1, outPt2) then
    result := outRec1 else
    result := outRec2;
end;
//------------------------------------------------------------------------------

function Param1RightOfParam2(outRec1, outRec2: POutRec): Boolean;
begin
  result := true;
  repeat
    outRec1 := outRec1.FirstLeft;
    if outRec1 = outRec2 then exit;
  until not assigned(outRec1);
  result := false;
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
  outRec1 := fPolyOutList[e1.outIdx];
  outRec2 := fPolyOutList[e2.outIdx];

  //work out which polygon fragment has the correct hole state ...
  if Param1RightOfParam2(outRec1, outRec2) then holeStateRec := outRec2
  else if Param1RightOfParam2(outRec2, outRec1) then holeStateRec := outRec1
  else holeStateRec := GetLowermostRec(outRec1, outRec2);

  //get the start and ends of both output polygons ...
  p1_lft := outRec1.pts;
  p2_lft := outRec2.pts;
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
      outRec1.pts := p2_rt;
    end else
    begin
      //x y z a b c
      p2_rt.next := p1_lft;
      p1_lft.prev := p2_rt;
      p2_lft.prev := p1_rt;
      p1_rt.next := p2_lft;
      outRec1.pts := p2_lft;
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
  outRec2.pts := nil;
  outRec2.bottomPt := nil;
  outRec2.AppendLink := outRec1;
  OKIdx := outRec1.idx;
  ObsoleteIdx := outRec2.idx;

  e1.outIdx := -1; //nb: safe because we only get here via AddLocalMaxPoly
  e2.outIdx := -1;

  e := fActiveEdges;
  while assigned(e) do
  begin
    if (e.outIdx = ObsoleteIdx) then
    begin
      e.outIdx := OKIdx;
      e.side := newSide;
      break;
    end;
    e := e.nextInAEL;
  end;

  for i := 0 to fJoinList.count -1 do
  begin
    jr := fJoinList[i];
    if jr.poly1Idx = ObsoleteIdx then jr.poly1Idx := OKIdx;
    if jr.poly2Idx = ObsoleteIdx then jr.poly2Idx := OKIdx;
  end;
  if assigned(fHorizJoins) then
  begin
    h := fHorizJoins;
    repeat
      if h.savedIdx = ObsoleteIdx then h.SavedIdx := OKIdx;
      h := h.next;
    until h = fHorizJoins;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.CreateOutRec: POutRec;
begin
  new(result);
  result.isHole := false;
  result.FirstLeft := nil;
  result.AppendLink := nil;
  result.pts := nil;
  result.bottomPt := nil;
  result.sides := [];
  result.bottomFlag := nil;
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
    outRec.idx := fPolyOutList.Add(outRec);
    e.outIdx := outRec.idx;
    new(op);
    outRec.pts := op;
    outRec.bottomPt := op;

    op.pt := pt;
    op.next := op;
    op.prev := op;
    op.idx := outRec.idx;
    SetHoleState(e, outRec);
  end else
  begin
    outRec := fPolyOutList[e.outIdx];
    op := outRec.pts;
    if (ToFront and PointsEqual(pt, op.pt)) or
      (not ToFront and PointsEqual(pt, op.prev.pt)) then exit;

    if not (e.side in outRec.sides) then
    begin

      //check for 'rounding' artefacts ...
      if (outRec.sides = []) and (pt.Y = op.pt.Y) then
        if ToFront then
        begin
          if (pt.X = op.pt.X +1) then exit;    //ie wrong side of bottomPt
        end
        else if (pt.X = op.pt.X -1) then exit; //ie wrong side of bottomPt

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
        //the Orientation function to return the wrong result. Therefore, it's
        //important to ensure that any self-intersections close to BottomPt are
        //detected and removed before orientation is assigned.

        if ToFront then
        begin
          opBot := outRec.pts;
          op2 := opBot.next; //op2 == right side
          if (opBot.pt.Y <> op2.pt.Y) and (opBot.pt.Y <> pt.Y) and
            ((opBot.pt.X - pt.X)/(opBot.pt.Y - pt.Y) <
            (opBot.pt.X - op2.pt.X)/(opBot.pt.Y - op2.pt.Y)) then
               outRec.bottomFlag := opBot;
        end else
        begin
          opBot := outRec.pts.prev;
          op2 := opBot.prev; //op2 == left side
          if (opBot.pt.Y <> op2.pt.Y) and (opBot.pt.Y <> pt.Y) and
            ((opBot.pt.X - pt.X)/(opBot.pt.Y - pt.Y) >
            (opBot.pt.X - op2.pt.X)/(opBot.pt.Y - op2.pt.Y)) then
               outRec.bottomFlag := opBot;
        end;
      end;
    end;

    new(op2);
    op2.pt := pt;
    op2.idx := outRec.idx;
    if (op2.pt.Y = outRec.bottomPt.pt.Y) and
      (op2.pt.X < outRec.bottomPt.pt.X) then
        outRec.bottomPt := op2;
    op2.next := op;
    op2.prev := op.prev;
    op.prev.next := op2;
    op.prev := op2;
    if ToFront then outRec.pts := op2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontals;
var
  e: PEdge;
begin
  while assigned(fSortedEdges) do
  begin
    e := fSortedEdges;
    DeleteFromSEL(e);
    ProcessHorizontal(e);
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsTopHorz(const XPos: TFixed): Boolean;
var
  e: PEdge;
begin
  result := false;
  e := fSortedEdges;
  while assigned(e) do
  begin
    if (XPos >= min(e.xcurr,e.xtop)) and (XPos <= max(e.xcurr,e.xtop)) then exit;
    e := e.nextInSEL;
  end;
  result := true;
end;
//------------------------------------------------------------------------------

function IsMinima(e: PEdge): Boolean;
begin
  result := assigned(e) and (e.prev.nextInLML <> e) and (e.next.nextInLML <> e);
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PEdge; const Y: TFixed): Boolean;
begin
  result := assigned(e) and (e.ytop = Y) and not assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function IsIntermediate(e: PEdge; const Y: TFixed): Boolean;
begin
  result := (e.ytop = Y) and assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PEdge): PEdge;
begin
  result := e.next;
  if not IsMaxima(result, e.ytop) or (result.xtop <> e.xtop) then
    result := e.prev;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  with e1^ do if not assigned(nextInAEL) and not assigned(prevInAEL) then exit;
  with e2^ do if not assigned(nextInAEL) and not assigned(prevInAEL) then exit;

  if e1.nextInAEL = e2 then
  begin
    next := e2.nextInAEL;
    if assigned(next) then next.prevInAEL := e1;
    prev := e1.prevInAEL;
    if assigned(prev) then prev.nextInAEL := e2;
    e2.prevInAEL := prev;
    e2.nextInAEL := e1;
    e1.prevInAEL := e2;
    e1.nextInAEL := next;
  end
  else if e2.nextInAEL = e1 then
  begin
    next := e1.nextInAEL;
    if assigned(next) then next.prevInAEL := e2;
    prev := e2.prevInAEL;
    if assigned(prev) then prev.nextInAEL := e1;
    e1.prevInAEL := prev;
    e1.nextInAEL := e2;
    e2.prevInAEL := e1;
    e2.nextInAEL := next;
  end else
  begin
    next := e1.nextInAEL;
    prev := e1.prevInAEL;
    e1.nextInAEL := e2.nextInAEL;
    if assigned(e1.nextInAEL) then e1.nextInAEL.prevInAEL := e1;
    e1.prevInAEL := e2.prevInAEL;
    if assigned(e1.prevInAEL) then e1.prevInAEL.nextInAEL := e1;
    e2.nextInAEL := next;
    if assigned(e2.nextInAEL) then e2.nextInAEL.prevInAEL := e2;
    e2.prevInAEL := prev;
    if assigned(e2.prevInAEL) then e2.prevInAEL.nextInAEL := e2;
  end;
  if not assigned(e1.prevInAEL) then fActiveEdges := e1
  else if not assigned(e2.prevInAEL) then fActiveEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInSEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  if e1.nextInSEL = e2 then
  begin
    next    := e2.nextInSEL;
    if assigned(next) then next.prevInSEL := e1;
    prev    := e1.prevInSEL;
    if assigned(prev) then prev.nextInSEL := e2;
    e2.prevInSEL := prev;
    e2.nextInSEL := e1;
    e1.prevInSEL := e2;
    e1.nextInSEL := next;
  end
  else if e2.nextInSEL = e1 then
  begin
    next    := e1.nextInSEL;
    if assigned(next) then next.prevInSEL := e2;
    prev    := e2.prevInSEL;
    if assigned(prev) then prev.nextInSEL := e1;
    e1.prevInSEL := prev;
    e1.nextInSEL := e2;
    e2.prevInSEL := e1;
    e2.nextInSEL := next;
  end else
  begin
    next    := e1.nextInSEL;
    prev    := e1.prevInSEL;
    e1.nextInSEL := e2.nextInSEL;
    if assigned(e1.nextInSEL) then e1.nextInSEL.prevInSEL := e1;
    e1.prevInSEL := e2.prevInSEL;
    if assigned(e1.prevInSEL) then e1.prevInSEL.nextInSEL := e1;
    e2.nextInSEL := next;
    if assigned(e2.nextInSEL) then e2.nextInSEL.prevInSEL := e2;
    e2.prevInSEL := prev;
    if assigned(e2.prevInSEL) then e2.prevInSEL.nextInSEL := e2;
  end;
  if not assigned(e1.prevInSEL) then fSortedEdges := e1
  else if not assigned(e2.prevInSEL) then fSortedEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontal(horzEdge: PEdge);

  function GetNextInAEL(e: PEdge; Direction: TDirection): PEdge;
  begin
    if Direction = dLeftToRight then
      result := e.nextInAEL else
      result := e.prevInAEL;
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
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with other HE xbots only [#],    *
* and with other non-horizontal edges [*]. Once these intersections are        *
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

  if assigned(horzEdge.nextInLML) then
    eMaxPair := nil else
    eMaxPair := GetMaximaPair(horzEdge);

  e := GetNextInAEL(horzEdge, Direction);
  while assigned(e) do
  begin
    eNext := GetNextInAEL(e, Direction);
    if assigned(eMaxPair) or
       ((Direction = dLeftToRight) and (e.xcurr <= horzRight)) or
      ((Direction = dRightToLeft) and (e.xcurr >= horzLeft)) then
    begin
      //ok, so far it looks like we're still in range of the horizontal edge

      if (e.xcurr = horzEdge.xtop) and not assigned(eMaxPair) then
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
        //nb: More -ve slopes follow more +ve slopes ABOVE the horizontal.
          break;
      end;

      if (e = eMaxPair) then
      begin
        //horzEdge is evidently a maxima horizontal and we've arrived at its end.
        if Direction = dLeftToRight then
          IntersectEdges(horzEdge, e, FixedPoint2(e.xcurr, horzEdge.ycurr)) else
          IntersectEdges(e, horzEdge, FixedPoint2(e.xcurr, horzEdge.ycurr));

        if (eMaxPair.outIdx >= 0) then raise exception.Create(rsHorizontal);
        exit;
      end
      else if (e.dx = horizontal) and not IsMinima(e) and not (e.xcurr > e.xtop) then
      begin
        //An overlapping horizontal edge. Overlapping horizontal edges are
        //processed as if layered with the current horizontal edge (horizEdge)
        //being infinitesimally lower that the next (e). Therfore, we
        //intersect with e only if e.xcurr is within the bounds of horzEdge ...
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
      (e.xcurr > horzRight) and assigned(fSortedEdges)) or
      ((Direction = dRightToLeft) and
      (e.xcurr < horzLeft) and assigned(fSortedEdges)) then
        break;
    e := eNext;
  end;

  if assigned(horzEdge.nextInLML) then
  begin
    if (horzEdge.outIdx >= 0) then
      AddOutPt(horzEdge, FixedPoint2(horzEdge.xtop, horzEdge.ytop));
    UpdateEdgeIntoAEL(horzEdge);
  end else
  begin
    if horzEdge.outIdx >= 0 then
      IntersectEdges(horzEdge, eMaxPair,
        FixedPoint2(horzEdge.xtop, horzEdge.ycurr), [ipLeft,ipRight]);

    if eMaxPair.outIdx >= 0 then raise exception.Create(rsHorizontal);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(horzEdge);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var e: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  if not assigned(e.nextInLML) then raise exception.Create(rsUpdateEdgeIntoAEL);
  AelPrev := e.prevInAEL;
  AelNext := e.nextInAEL;
  e.nextInLML.outIdx := e.outIdx;
  if assigned(AelPrev) then
    AelPrev.nextInAEL := e.nextInLML else
    fActiveEdges := e.nextInLML;
  if assigned(AelNext) then
    AelNext.prevInAEL := e.nextInLML;
  e.nextInLML.side := e.side;
  e.nextInLML.windDelta := e.windDelta;
  e.nextInLML.windCnt := e.windCnt;
  e.nextInLML.windCnt2 := e.windCnt2;
  e := e.nextInLML;
  e.prevInAEL := AelPrev;
  e.nextInAEL := AelNext;
  if e.dx <> horizontal then
    InsertScanbeam(e.ytop);
end;
//------------------------------------------------------------------------------

function TClipper.ProcessIntersections(const botY, topY: TFixed): Boolean;
begin
  result := true;
  try
    BuildIntersectList(botY, topY);
    if fIntersectNodes = nil then exit;
    if FixupIntersections then ProcessIntersectList
    else result := false;
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
  while assigned(fIntersectNodes) do
  begin
    n := fIntersectNodes.next;
    dispose(fIntersectNodes);
    fIntersectNodes := n;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildIntersectList(const botY, topY: TFixed);
var
  e, eNext: PEdge;
  pt: TFixedPoint;
  isModified: Boolean;
begin
  if not assigned(fActiveEdges) then exit;

  //prepare for sorting ...
  e := fActiveEdges;
  e.tmpX := TopX(e, topY);
  fSortedEdges := e;
  fSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.prevInSEL.nextInSEL := e;
    e.nextInSEL := nil;
    e.tmpX := TopX(e, topY);
    e := e.nextInAEL;
  end;

  try
    //bubblesort ...
    isModified := true;
    while isModified and assigned(fSortedEdges) do
    begin
      isModified := false;
      e := fSortedEdges;
      while assigned(e.nextInSEL) do
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
          isModified := true;
        end else
          e := eNext;
      end;
      if assigned(e.prevInSEL) then e.prevInSEL.nextInSEL := nil else break;
    end;
  finally
    fSortedEdges := nil;
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
        result := node2.pt.X > node1.pt.X;
        if node2.edge1.dx > 0 then result := not result;
      end
      else if (node1.edge1 = node2.edge2) or (node1.edge2 = node2.edge2) then
      begin
        result := node2.pt.X > node1.pt.X;
        if node2.edge2.dx > 0 then result := not result;
      end else
        result := node2.pt.X > node1.pt.X;
    end
    else result := node1.pt.Y > node2.pt.Y;
  end;
  //----------------------------------------------------------------------------

var
  node, newNode: PIntersectNode;
begin
  new(newNode);
  newNode.edge1 := e1;
  newNode.edge2 := e2;
  newNode.pt := pt;
  newNode.next := nil;
  if not assigned(fIntersectNodes) then
    fIntersectNodes := newNode
  else if ProcessParam1BeforeParam2(newNode, fIntersectNodes) then
  begin
    newNode.next := fIntersectNodes;
    fIntersectNodes := newNode;
  end else
  begin
    node := fIntersectNodes;
    while assigned(node.next) and
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
  while assigned(fIntersectNodes) do
  begin
    node := fIntersectNodes.next;
    with fIntersectNodes^ do
    begin
      IntersectEdges(edge1, edge2, pt, [ipLeft,ipRight]);
      SwapPositionsInAEL(edge1, edge2);
    end;
    dispose(fIntersectNodes);
    fIntersectNodes := node;
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
    if not assigned(eNext) then raise exception.Create(rsDoMaxima);
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
  else raise exception.Create(rsDoMaxima);
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
* Next, 'intermediate' and 'maxima' horizontal edges are processed. Then edges *
* that intersect exactly at the top of the scanbeam are processed [%].         *
* Finally, new minima are added and any intersects they create are processed.  *
*******************************************************************************)

(*******************************************************************************
*     \                          /    /          \   /                         *
*      \   horizontal minima    /    /            \ /                          *
* { --  o======================#====o   --------   .     ------------------- } *
* {       horizontal maxima    .                   %  scanline intersect     } *
* { -- o=======================#===================#========o     ---------- } *
*      |                      /                   / \        \                 *
*      + maxima intersect    /                   /   \        \                *
*     /|\                   /                   /     \        \               *
*    / | \                 /                   /       \        \              *
*******************************************************************************)

  e := fActiveEdges;
  while assigned(e) do
  begin
    //1. process maxima, treating them as if they're 'bent' horizontal edges,
    //   but exclude maxima with horizontal edges. nb: e can't be a horizontal.
    if IsMaxima(e, topY) and (GetMaximaPair(e).dx <> horizontal) then
    begin
      //'e' might be removed from AEL, as may any following edges so ...
      ePrior := e.prevInAEL;
      DoMaxima(e, topY);
      if not assigned(ePrior) then
        e := fActiveEdges else
        e := ePrior.nextInAEL;
    end else
    begin
      //2. promote horizontal edges, otherwise update xcurr and ycurr ...
      if IsIntermediate(e, topY) and (e.nextInLML.dx = horizontal) then
      begin
        if (e.outIdx >= 0) then
        begin
          AddOutPt(e, FixedPoint2(e.xtop, e.ytop));

          hj := fHorizJoins;
          if assigned(hj) then
          repeat
            if GetOverlapSegment(FixedPoint2(hj.edge.xbot, hj.edge.ybot),
              FixedPoint2(hj.edge.xtop, hj.edge.ytop),
              FixedPoint2(e.nextInLML.xbot, e.nextInLML.ybot),
              FixedPoint2(e.nextInLML.xtop, e.nextInLML.ytop), pt, pt2) then
                AddJoin(hj.edge, e.nextInLML, hj.savedIdx, e.outIdx);
            hj := hj.next;
          until hj = fHorizJoins;

          AddHorzJoin(e.nextInLML, e.outIdx);
        end;
        UpdateEdgeIntoAEL(e);
        AddEdgeToSEL(e);
      end else
      begin
        //this just simplifies horizontal processing ...
        e.xcurr := TopX(e, topY);
        e.ycurr := topY;
      end;
      e := e.nextInAEL;
    end;
  end;

  //3. Process horizontals at the top of the scanbeam ...
  ProcessHorizontals;

  //4. Promote intermediate vertices ...
  e := fActiveEdges;
  while assigned(e) do
  begin
    if IsIntermediate(e, topY) then
    begin
      if (e.outIdx >= 0) then AddOutPt(e, FixedPoint2(e.xtop, e.ytop));
      UpdateEdgeIntoAEL(e);

      //if output polygons share an edge, they'll need joining later ...
      if (e.outIdx >= 0) and assigned(e.prevInAEL) and
        (e.prevInAEL.outIdx >= 0) and
        (e.prevInAEL.xcurr = e.xbot) and (e.prevInAEL.ycurr = e.ybot) and
        SlopesEqual(FixedPoint2(e.xbot,e.ybot), FixedPoint2(e.xtop, e.ytop),
          FixedPoint2(e.xbot,e.ybot),
          FixedPoint2(e.prevInAEL.xtop, e.prevInAEL.ytop)) then
      begin
        AddOutPt(e.prevInAEL, FixedPoint2(e.xbot, e.ybot));
        AddJoin(e, e.prevInAEL);
      end
      else if (e.outIdx >= 0) and assigned(e.nextInAEL) and
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
  setLength(result, fPolyOutList.Count);
  for i := 0 to fPolyOutList.Count -1 do
    if assigned(fPolyOutList[i]) then
    begin
      //make sure each polygon has at least 3 vertices ...
      outRec := fPolyOutList[i];
      op := outRec.pts;
      if not assigned(op) then continue; //nb: not sorted
      cnt := PointCount(op);
      if (cnt < 3) then continue;

      setLength(result[k], cnt);
      for j := 0 to cnt -1 do
      begin
        result[k][j].X := op.pt.X;
        result[k][j].Y := op.pt.Y;
        op := op.next;
      end;
      inc(k);
    end;
  setLength(result, k);
end;
//------------------------------------------------------------------------------

procedure TClipper.FixupOutPolygon(outRec: POutRec);
var
  pp, tmp, lastOK: POutPt;
begin
  //FixupOutPolygon() - removes duplicate points and simplifies consecutive
  //parallel edges by removing the middle vertex.
  lastOK := nil;
  outRec.pts := outRec.bottomPt;
  pp := outRec.pts;
  while true do
  begin
    if (pp.prev = pp) or (pp.next = pp.prev) then
    begin
      DisposePolyPts(pp);
      outRec.pts := nil;
      outRec.bottomPt := nil;
      exit;
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
      if not assigned(lastOK) then lastOK := pp;
      pp := pp.next;
    end;
  end;
  if not assigned(outRec.bottomPt) then
  begin
    outRec.bottomPt := GetBottomPt(pp);
    outRec.bottomPt.idx := outRec.idx;
    outRec.pts := outRec.bottomPt;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.FixupIntersections: Boolean;
var
  e1, e2: PEdge;
  int1, int2: PIntersectNode;
begin
  result := not assigned(fIntersectNodes.next);
  if result then exit;
  //logic: only swap (intersect) adjacent edges ...
  try
    CopyAELToSEL;
    int1 := fIntersectNodes;
    int2 := fIntersectNodes.next;
    while assigned(int2) do
    begin
      e1 := int1.edge1;
      if (e1.prevInSEL = int1.edge2) then e2 := e1.prevInSEL
      else if (e1.nextInSEL = int1.edge2) then e2 := e1.nextInSEL
      else
      begin
        //The current intersection is out of order, so try and swap it with
        //a subsequent intersection ...
        while assigned(int2) do
        begin
          if (int2.edge1.nextInSEL = int2.edge2) or
            (int2.edge1.prevInSEL = int2.edge2) then break
          else int2 := int2.next;
        end;
        if not assigned(int2) then exit; //oops!!!
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
    result := (int1.edge1.prevInSEL = int1.edge2) or
      (int1.edge1.nextInSEL = int1.edge2);
  finally
    fSortedEdges := nil;
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
  if not assigned(pp) then begin result := false; exit; end;
  result := true;
  pt1a := pt1; pt2a := pt2;
  pp2 := pp;
  repeat
    //test for co-linearity before testing for overlap ...
    if SlopesEqual(pt1a, pt2a, pp.pt, pp.prev.pt) and
      SlopesEqual(pt1a, pt2a, pp.pt) and
        GetOverlapSegment(pt1a, pt2a, pp.pt, pp.prev.pt, pt1, pt2) then exit;
    pp := pp.next;
  until pp = pp2;
  result := false;
end;
//------------------------------------------------------------------------------

function Pt3IsBetweenPt1AndPt2(const pt1, pt2, pt3: TFixedPoint): Boolean;
begin
  if PointsEqual(pt1, pt3) then result := true
  else if PointsEqual(pt2, pt3) then result := true
  else if (pt1.X <> pt2.X) then result := (pt1.X < pt3.X) = (pt3.X < pt2.X)
  else result := (pt1.Y < pt3.Y) = (pt3.Y < pt2.Y);
end;
//------------------------------------------------------------------------------

function InsertPolyPtBetween(p1, p2: POutPt; const pt: TFixedPoint): POutPt;
begin
  if (p1 = p2) then raise exception.Create(rsJoinError);

  new(result);
  result.pt := pt;
  result.idx := p1.idx;
  if p2 = p1.next then
  begin
    p1.next := result;
    p2.prev := result;
    result.next := p2;
    result.prev := p1;
  end else
  begin
    p2.next := result;
    p1.prev := result;
    result.next := p1;
    result.prev := p2;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages1(const outRec1, outRec2: POutRec);
var
  i: Integer;
begin
  //when a polygon is split into 2 polygons, make sure any holes the original
  //polygon contained link to the correct polygon ...
  for i := 0 to fPolyOutList.Count - 1 do
    with POutRec(fPolyOutList[i])^ do
      if isHole and assigned(bottomPt) and (FirstLeft = OutRec1) and
         not PointInPolygon(BottomPt.pt, outRec1.pts) then
          FirstLeft := outRec2;
end;
//------------------------------------------------------------------------------

procedure TClipper.CheckHoleLinkages2(const outRec1, outRec2: POutRec);
var
  i: Integer;
begin
  //if a hole is owned by outRec2 then make it owned by outRec1 ...
  for i := 0 to fPolyOutList.Count - 1 do
    with POutRec(fPolyOutList[i])^ do
      if isHole and assigned(bottomPt) and (FirstLeft = OutRec2) then
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
  for i := 0 to fJoinList.count -1 do
  begin
    jr := fJoinList[i];
    outRec1 := fPolyOutList[jr.poly1Idx];
    if not assigned(outRec1) then Continue;
    pp1a := outRec1.pts;
    outRec2 := fPolyOutList[jr.poly2Idx];
    if not assigned(outRec2) then Continue;
    pp2a := outRec2.pts;
    pt1 := jr.pt2a; pt2 := jr.pt2b;
    pt3 := jr.pt1a; pt4 := jr.pt1b;
    if not FindSegment(pp1a, pt1, pt2) then continue;
    if (jr.poly1Idx = jr.poly2Idx) then
    begin
      //we're searching the same polygon for overlapping segments so
      //segment 2 mustn't be the same as segment 1 ...
      pp2a := pp1a.next;
      if not FindSegment(pp2a, pt3, pt4) or (pp2a = pp1a) then continue;
    end else
      if not FindSegment(pp2a, pt3, pt4) then continue;

    if not GetOverlapSegment(pt1, pt2, pt3, pt4, pt1, pt2) then continue;

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
      continue;

    if (jr.poly2Idx = jr.poly1Idx) then
    begin
      //instead of joining two polygons, we've just created a new one by
      //splitting one polygon into two.
      outRec1.pts := GetBottomPt(p1);
      outRec1.bottomPt := outRec1.pts;
      outRec1.bottomPt.idx := outRec1.idx;
      outRec2 := CreateOutRec;
      outRec2.idx := fPolyOutList.Add(outRec2);
      jr.poly2Idx := outRec2.idx;
      outRec2.pts := GetBottomPt(p2);
      outRec2.bottomPt := outRec2.pts;
      outRec2.bottomPt.idx := outRec2.idx;

      if PointInPolygon(outRec2.pts.pt, outRec1.pts) then
      begin
        //outRec2 is contained by outRec1 ...
        outRec2.isHole := not outRec1.isHole;
        outRec2.FirstLeft := outRec1;
        if not Orientation(outRec2) then
          ReversePolyPtLinks(outRec2.pts);
      end else if PointInPolygon(outRec1.pts.pt, outRec2.pts) then
      begin
        //outRec1 is contained by outRec2 ...
        outRec2.isHole := outRec1.isHole;
        outRec1.isHole := not outRec2.isHole;
        outRec2.FirstLeft := outRec1.FirstLeft;
        outRec1.FirstLeft := outRec2;
        if not Orientation(outRec1) then
          ReversePolyPtLinks(outRec1.pts);
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
      for j := i+1 to fJoinList.count -1 do
      begin
        jr2 := fJoinList[j];
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

      if assigned(outRec1.pts) then
      begin
        outRec1.isHole := not Orientation(outRec1);
        if outRec1.isHole and not assigned(outRec1.FirstLeft) then
          outRec1.FirstLeft := outRec2.FirstLeft;
      end;

      //delete the obsolete pointer ...
      OKIdx := outRec1.idx;
      ObsoleteIdx := outRec2.idx;
      outRec2.pts := nil;
      outRec2.bottomPt := nil;
      outRec2.AppendLink := outRec1;

      //now fixup any subsequent joins ...
      for j := i+1 to fJoinList.count -1 do
      begin
        jr2 := fJoinList[j];
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
  dx, dy, f: single;
begin
  if (pt2.X = pt1.X) and (pt2.Y = pt1.Y) then
  begin
    result.X := 0;
    result.Y := 0;
    exit;
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

function BuildArc(const pt: TFixedPoint; a1, a2, r: single): TArrayOfFixedPoint;
var
  i, N: Integer;
  a, da: Double;
  Steps: TFixed;
  S, C: Extended; //sin & cos
begin
  Steps := Max(6, Round(Sqrt(Abs(r)) * Abs(a2 - a1)));
  if Steps > $100000 then Steps := $100000;
  SetLength(Result, Steps);
  N := Steps - 1;
  da := (a2 - a1) / N;
  a := a1;
  for i := 0 to N do
  begin
    SinCos(a, S, C);
    Result[i].X := pt.X + Round(C * r);
    Result[i].Y := pt.Y + Round(S * r);
    a := a + da;
  end;
end;
//------------------------------------------------------------------------------

function GetBounds(const a: TArrayOfArrayOfFixedPoint): TFixedRect;
var
  i,j: Integer;
begin
  with result do
  begin
    Left := loRange; Top := loRange;
    Right := -loRange; Bottom := -loRange;
  end;
  for i := 0 to high(a) do
    for j := 0 to high(a[i]) do
    begin
      if a[i][j].X < result.Left then result.Left := a[i][j].X;
      if a[i][j].X > result.Right then result.Right := a[i][j].X;
      if a[i][j].Y < result.Top then result.Top := a[i][j].Y;
      if a[i][j].Y > result.Bottom then result.Bottom := a[i][j].Y;
    end;
  if result.left = loRange then
    with result do begin Left := 0; Top := 0; Right := 0; Bottom := 0; end;
end;
//------------------------------------------------------------------------------

function OffsetPolygons(const aaFxPts: TArrayOfArrayOfFixedPoint; const delta: TFixed;
  JoinType: TJoinType = jtSquare; MiterLimit: Double = 2): TArrayOfArrayOfFixedPoint;
var
  i, j, k, len, out_len: Integer;
  normals: TArrayOfDoublePoint;
  R, RMin: Double;
  pt1, pt2: TFixedPoint;
  outer: TArrayOfFixedPoint;
  bounds: TFixedRect;
const
  buffLength: Integer = 128;

  procedure AddPoint(const pt: TFixedPoint);
  var
    len: Integer;
  begin
    len := length(result[i]);
    if out_len = len then
      setlength(result[i], len + buffLength);
    result[i][out_len].X := pt.X;
    result[i][out_len].Y := pt.Y;
    inc(out_len);
  end;

  procedure DoSquare(mul: Double = 1.0);
  var
    a1, a2, dx: Double;
  begin
    pt1.X := round(aaFxPts[i][j].X + normals[k].X * delta);
    pt1.Y := round(aaFxPts[i][j].Y + normals[k].Y * delta);
    pt2.X := round(aaFxPts[i][j].X + normals[j].X * delta);
    pt2.Y := round(aaFxPts[i][j].Y + normals[j].Y * delta);
    if ((normals[k].X*normals[j].Y-normals[j].X*normals[k].Y) * delta >= 0) then
    begin
      a1 := ArcTan2(normals[k].Y, normals[k].X);
      a2 := ArcTan2(-normals[j].Y, -normals[j].X);
      a1 := abs(a2 - a1);
      if a1 > pi then a1 := pi*2 - a1;
      dx := tan((pi - a1)/4) * abs(delta*mul);

      pt1 := FixedPoint2(round(pt1.X -normals[k].Y *dx),
        round(pt1.Y + normals[k].X *dx));
      AddPoint(pt1);
      pt2 := FixedPoint2(round(pt2.X + normals[j].Y *dx),
        round(pt2.Y - normals[j].X *dx));
      AddPoint(pt2);
    end else
    begin
      AddPoint(pt1);
      AddPoint(aaFxPts[i][j]);
      AddPoint(pt2);
    end;
  end;

  procedure DoMiter;
  var
    q: Double;
  begin
    if ((normals[k].X*normals[j].Y-normals[j].X*normals[k].Y)*delta >= 0) then
    begin
      q := delta / R;
      AddPoint(FixedPoint2(round(aaFxPts[i][j].X + (normals[k].X + normals[j].X) *q),
        round(aaFxPts[i][j].Y + (normals[k].Y + normals[j].Y) *q)));
    end else
    begin
      pt1.X := round(aaFxPts[i][j].X + normals[k].X * delta);
      pt1.Y := round(aaFxPts[i][j].Y + normals[k].Y * delta);
      pt2.X := round(aaFxPts[i][j].X + normals[j].X * delta);
      pt2.Y := round(aaFxPts[i][j].Y + normals[j].Y * delta);
      AddPoint(pt1);
      AddPoint(aaFxPts[i][j]);
      AddPoint(pt2);
    end;
  end;

  procedure DoRound;
  var
    m: Integer;
    arc: TArrayOfFixedPoint;
    a1, a2: Double;
  begin
    pt1.X := round(aaFxPts[i][j].X + normals[k].X * delta);
    pt1.Y := round(aaFxPts[i][j].Y + normals[k].Y * delta);
    pt2.X := round(aaFxPts[i][j].X + normals[j].X * delta);
    pt2.Y := round(aaFxPts[i][j].Y + normals[j].Y * delta);
    AddPoint(pt1);
    //round off reflex angles (ie > 180 deg) unless almost flat (ie < 10deg).
    //(N1.X * N2.Y - N2.X * N1.Y) == unit normal "cross product" == sin(angle)
    //(N1.X * N2.X + N1.Y * N2.Y) == unit normal "dot product" == cos(angle)
    //dot product normals == 1 -> no angle
    if ((normals[k].X*normals[j].Y - normals[j].X*normals[k].Y)*delta >= 0) then
    begin
      if ((normals[j].X*normals[k].X+normals[j].Y*normals[k].Y) < 0.985) then
      begin
        a1 := ArcTan2(normals[k].Y, normals[k].X);
        a2 := ArcTan2(normals[j].Y, normals[j].X);
        if (delta > 0) and (a2 < a1) then a2 := a2 + pi*2
        else if (delta < 0) and (a2 > a1) then a2 := a2 - pi*2;
        arc := BuildArc(aaFxPts[i][j], a1, a2, delta);
        for m := 0 to high(arc) do
          AddPoint(arc[m]);
      end;
    end else
      AddPoint(aaFxPts[i][j]);
    AddPoint(pt2);
  end;

begin
  //MiterLimit defaults to twice delta's width ...
  if MiterLimit <= 1 then MiterLimit := 1;
  RMin := 2/(sqr(MiterLimit));

  setLength(result, length(aaFxPts));
  i := length(result);

  for i := 0 to high(aaFxPts) do
  begin
    result[i] := nil;
    len := length(aaFxPts[i]);
    if (len > 1) and (aaFxPts[i][0].X = aaFxPts[i][len - 1].X) and
        (aaFxPts[i][0].Y = aaFxPts[i][len - 1].Y) then dec(len);

    if (len = 0) or ((len < 3) and (delta < 0)) then continue;

    if (len = 1) then
    begin
      result[i] := BuildArc(aaFxPts[i][0], 0, 2*pi, delta);
      continue;
    end;

    //build normals ...
    setLength(normals, len);
    for j := 0 to len-2 do
      normals[j] := GetUnitNormal(aaFxPts[i][j], aaFxPts[i][j+1]);
    normals[len-1] := GetUnitNormal(aaFxPts[i][len-1], aaFxPts[i][0]);

    out_len := 0;
    k := len -1;
    for j := 0 to len-1 do
    begin
      case JoinType of
        jtMiter:
        begin
          R := 1 + (normals[j].X*normals[k].X + normals[j].Y*normals[k].Y);
          if (R >= RMin) then
            DoMiter else
            DoSquare(MiterLimit);
        end;
        jtSquare: DoSquare;
        jtRound: DoRound;
      end;
      k := j;
    end;
    setLength(result[i], out_len);
  end;

  //finally, clean up untidy corners ...
  with TClipper.Create do
  try
    AddArrayOfArrayOfFixedPoint(result, ptSubject);
    if delta > 0 then
    begin
      Execute(ctUnion, result, pftPositive, pftPositive);
    end else
    begin
      bounds := GetBounds(result);
      setlength(outer, 4);
      outer[0] := TFixedPoint(Point(bounds.left -10, bounds.bottom +10));
      outer[1] := TFixedPoint(Point(bounds.right +10, bounds.bottom +10));
      outer[2] := TFixedPoint(Point(bounds.right +10, bounds.top -10));
      outer[3] := TFixedPoint(Point(bounds.left -10, bounds.top -10));
      AddArrayOfFixedPoint(outer, ptSubject);
      Execute(ctUnion, result, pftNegative, pftNegative);
      //delete the outer rectangle ...
      len := length(result);
      for j := 1 to len -1 do result[j-1] := result[j];
      if len > 0 then
        setlength(result, len -1);
      //restore polygon orientation ...
      result := ReversePolygons(result);
    end;
  finally
    free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
