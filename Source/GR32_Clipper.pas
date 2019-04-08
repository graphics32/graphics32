unit GR32_Clipper;

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
 * The Original Code is GR32_Clipper
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson
 *
 * Portions created by the Initial Developer are Copyright (C) 2012-2019
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$IFDEF FPC}
  {$DEFINE USEINLINING}
{$ELSE}
  {$IF CompilerVersion >= 18}         // Delphi 2007
    // While USEINLINING has been supported since D2005, both D2005 and D2006
    // have an Inline codegen bug (QC41166) so ignore Inline until D2007.
    {$DEFINE USEINLINING}
    {$IF CompilerVersion >= 25.0}     // Delphi XE4+
      {$LEGACYIFEND ON}
    {$IFEND}
  {$IFEND}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF USEINLINING}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, GR32;

type
  TPoint64 = record X, Y: Int64; end;

  // TPath: a simple data structure to represent a series of vertices, whether
  // open (poly-line) or closed (polygon). A path may be simple or complex (self
  // intersecting). For simple polygons, path orientation (whether clockwise or
  // counterclockwise) is generally used to differentiate outer paths from inner
  // paths (holes). For complex polygons (and also for overlapping polygons),
  // explicit 'filling rules' (see below) are used to indicate regions that are
  // inside (filled) and regions that are outside (unfilled) a specific polygon.
  TPath = array of TPoint64;
  TPaths = array of TPath;
  TArrayOfPaths = array of TPaths;

  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  // Note: all clipping operations except for Difference are commutative.
  TPathType = (ptSubject, ptClip);
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  EClipperLibException = class(Exception);

  TJoinType = (jtSquare, jtRound, jtRoundEx, jtMiter);
  TEndType = (etPolygon, etOpenJoined, etOpenButt, etOpenSquare, etOpenRound);


  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  PVertex = ^TVertex;
  TVertex = record
    Pt    : TPoint64;
    next  : PVertex;
    prev  : PVertex;
    flags : TVertexFlags;
  end;

  PVertexArray = ^TVertexArray;
  TVertexArray = array[0..MaxInt div sizeof(TVertex) -1] of TVertex;

  // Every closed path (or polygon) is made up of a series of vertices forming
  // edges that alternate between going up (relative to the Y-axis) and going
  // down. Edges consecutively going up or consecutively going down are called
  // 'bounds' (or sides if they're simple polygons). 'Local Minima' refer to
  // vertices where descending bounds become ascending ones.

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    vertex    : PVertex;
    PolyType  : TPathType;
    IsOpen    : Boolean;
  end;

  TOutRec = class;

  TOutPt = class
    Pt       : TPoint64;
    Next     : TOutPt;
    Prev     : TOutPt;
    OutRec   : TOutRec;       // used in descendant classes
  end;

  PActive = ^TActive;
  TActive = record
    op       : TOutPt;        // used in descendant classes
    Bot      : TPoint64;
    Top      : TPoint64;
    CurrX    : Int64;
    Dx       : Double;        // inverse of edge slope (zero = vertical)
    WindDx   : Integer;       // wind direction (ascending: +1; descending: -1)
    WindCnt  : Integer;       // current wind count
    WindCnt2 : Integer;       // current wind count of the opposite TPolyType
    OutRec   : TOutRec;
    // AEL: 'active edge list' (Vatti's AET - active edge table)
    //      a linked list of all edges (from left to right) that are present
    //      (or 'active') within the current scanbeam (a horizontal 'beam' that
    //      sweeps from bottom to top over the paths in the clipping operation).
    PrevInAEL: PActive;
    NextInAEL: PActive;
    // SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //      linked list used when sorting edges into their new positions at the
    //      top of scanbeams, but also (re)used to process horizontals.
    PrevInSEL: PActive;
    NextInSEL: PActive;
    Jump     : PActive;       // for merge sorting (see BuildIntersectList())
    VertTop  : PVertex;
    LocMin   : PLocalMinima;  // the bottom of an edge 'bound' (also Vatti)
  end;

  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    Edge1  : PActive;
    Edge2  : PActive;
    Pt     : TPoint64;
  end;

  PScanLine = ^TScanLine;
  TScanLine = record
    Y        : Int64;
    Next     : PScanLine;
  end;

  TOutRecState = (osUndefined, osOpen, osOuter,
    osOuterCheck, osInner, osInnerCheck);

  // OutRec: contains a path in the clipping solution. Edges in the AEL will
  // have OutRec pointers assigned when they form part of the clipping solution.
  TOutRec = class
    Idx      : Integer;
    Owner    : TOutRec;
    frontE   : PActive;
    backE    : PActive;
    Pts      : TOutPt;
    State    : TOutRecState;
  end;

  TClipper = class
  private
    FBotY               : Int64;
    FScanLine           : PScanLine;
    FLocMinListSorted   : Boolean;
    FHasOpenPaths       : Boolean;
    FCurrentLocMinIdx   : Integer;
    FClipType           : TClipType;
    FFillRule           : TFillRule;
    FIntersectList      : TList;
    FOutRecList         : TList;
    FLocMinList         : TList;
    FActives            : PActive; // see AEL above
    FSel                : PActive; // see SEL above
    FVertexList         : TList;
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function PopScanLine(out Y: Int64): Boolean;
    function PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRec(index: Integer);
    procedure DisposeAllOutRecs;
    procedure DisposeVerticesAndLocalMinima;
    procedure AddPathToVertexList(const path: TArrayOfFloatPoint;
      polyType: TPathType; isOpen: Boolean);
    function IsContributingClosed(e: PActive): Boolean;
    function IsContributingOpen(e: PActive): Boolean;
    procedure SetWindCountForClosedPathEdge(e: PActive);
    procedure SetWindCountForOpenPathEdge(e: PActive);
    procedure InsertLocalMinimaIntoAEL(const botY: Int64);
    procedure InsertLeftEdge(e: PActive);
    procedure PushHorz(e: PActive); {$IFDEF USEINLINING} inline; {$ENDIF}
    function PopHorz(out e: PActive): Boolean;
      {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure StartOpenPath(e: PActive; const pt: TPoint64);
    procedure UpdateEdgeIntoAEL(var e: PActive);
    procedure IntersectEdges(e1, e2: PActive;
      const pt: TPoint64; orientationCheckRequired: Boolean = false);
    procedure DeleteFromAEL(e: PActive);
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure DoIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure AddNewIntersectNode(e1, e2: PActive; topY: Int64);
    function BuildIntersectList(const topY: Int64): Boolean;
    procedure ProcessIntersectList;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    procedure DoHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    function DoMaxima(e: PActive): PActive;
    function AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
    procedure AddLocalMinPoly(e1, e2: PActive; const pt: TPoint64;
      IsNew: Boolean = false; orientationCheckRequired: Boolean = false);
    procedure AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
    procedure JoinOutrecPaths(e1, e2: PActive);
    function GetIntersectNode(index: Integer): PIntersectNode;
      {$IFDEF USEINLINING} inline; {$ENDIF}
  protected
    procedure CleanUp; // unlike Clear, CleanUp preserves added paths
    procedure ExecuteInternal(clipType: TClipType;
      fillRule: TFillRule); virtual;
    function BuildResult(out closedPaths,
      openPaths: TArrayOfArrayOfFloatPoint): Boolean;
    property OutRecList: TList read FOutRecList;
    property IntersectNode[index: Integer]: PIntersectNode
      read GetIntersectNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TFloatRect;

    // ADDPATH & ADDPATHS METHODS ...
    procedure AddPath(const path64: TArrayOfFloatPoint;
      polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;
    procedure AddPath(const path: TArrayOfFixedPoint;
      polyType: TPathType = ptSubject; isOpen: Boolean = false); overload;
    procedure AddPaths(const paths64: TArrayOfArrayOfFloatPoint;
      polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;
    procedure AddPaths(const paths: TArrayOfArrayOfFixedPoint;
      polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;

    // EXECUTE METHODS ...
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths, openPaths: TArrayOfArrayOfFloatPoint): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths, openPaths: TArrayOfArrayOfFixedPoint): Boolean; overload;
  end;

  TClipperOffset = class
  private
    FDelta: Double;
    FJoinType: TJoinType;
    FEndType  : TEndType;

    FStepSizeSin, FStepSizeCos: Extended;
    FMiterLim, FMiterLimit: Double;
    FStepsPerRad: Double;
    FArcTolerance: Double;

    FNorms: TArrayOfFloatPoint;
    FSolution: TArrayOfArrayOfFloatPoint;
    FSolutionLen: integer;

    FPathsIn: TArrayOfArrayOfFloatPoint;
    FPathIn: TArrayOfFloatPoint;
    FPathOut: TArrayOfFloatPoint;
    FPathOutLen: Integer;

    procedure AddPoint(const pt: TFloatPoint);
    procedure DoSquare(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosAplus1: Double);
    procedure DoRound(j, k: Integer);
    procedure OffsetPoint(j,k: Integer);

    function CheckPaths: boolean;
    function GetLowestPolygonIdx: integer;
    procedure OffsetPaths;
    procedure BuildNormals;
    procedure ReverseNormals;
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath;
  public
    constructor Create(MiterLimit: Double = 2.0; ArcTolerance: Double = 0.0);
    destructor Destroy; override;
    procedure AddPath(const path: TArrayOfFloatPoint);
    procedure AddPaths(const paths: TArrayOfArrayOfFloatPoint);
    procedure Clear;
    procedure Execute(delta: Double; jt: TJoinType; et: TEndType;
      out solution: TArrayOfArrayOfFloatPoint);
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property ArcTolerance: Double read FArcTolerance write FArcTolerance;
  end;

  function InflatePaths(const paths: TArrayOfArrayOfFloatPoint;
    delta: Double; jt: TJoinType; et: TEndType;
    miterLimit: single = 0): TArrayOfArrayOfFloatPoint;

implementation

const
  Tolerance           : Double = 1.0E-15;
  DefaultArcFrac      : Double = 0.02;
  Two_Pi              : Double = 2 * PI;
  LowestIp            : TPoint64 = (X: High(Int64); Y: High(Int64));

// OVERFLOWCHECKS OFF is a necessary workaround for a compiler bug that very
// occasionally report incorrect overflow errors in Delphi versions before 10.2.
// see https://forums.embarcadero.com/message.jspa?messageID=871444
{$OVERFLOWCHECKS OFF}

resourcestring
  rsClipper_OpenPathErr = 'Only subject paths can be open.';
  rsClipper_ClippingErr = 'Undefined clipping error';

//------------------------------------------------------------------------------
//  Miscellaneous Functions ...
//------------------------------------------------------------------------------

function Point64(const fp: TFloatPoint): TPoint64; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result.X := Round(fp.X * FixedOne);
  Result.Y := Round(fp.Y * FixedOne);
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Int64): TPoint64; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function FloatPoint(const pt: TPoint64): TFloatPoint; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result.X := pt.X * FixedToFloat;
  Result.Y := pt.Y * FixedToFloat;
end;
//------------------------------------------------------------------------------

function FixedToFloat(const fixed: TArrayOfFixedPoint): TArrayOfFloatPoint;
var
  i, len: Integer;
begin
  len := length(fixed);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatPoint(fixed[i]);
end;
//------------------------------------------------------------------------------

function FloatToFixed(const float: TArrayOfFloatPoint):
  TArrayOfFixedPoint; overload;
var
  i, len: Integer;
begin
  len := length(float);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FixedPoint(float[i]);
end;
//------------------------------------------------------------------------------

function FloatToFixed(const float: TArrayOfArrayOfFloatPoint):
  TArrayOfArrayOfFixedPoint; overload;
var
  i, len: Integer;
begin
  len := length(float);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatToFixed(float[i]);
end;
//------------------------------------------------------------------------------

function PointsEqual(const p1, p2: TPoint64): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function PointsEqual(const p1, p2: TFloatPoint): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function IsOpen(e: PActive): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.IsOpen;
end;
//------------------------------------------------------------------------------

function IsOpen(outrec: TOutRec): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOpen;
end;
//------------------------------------------------------------------------------

function IsOuter(outrec: TOutRec): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := outrec.State in [osOuter, osOuterCheck];
end;
//------------------------------------------------------------------------------

procedure SetAsOuter(outrec: TOutRec); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  outrec.State := osOuter;
end;
//------------------------------------------------------------------------------

function IsInner(outrec: TOutRec): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := outrec.State in [osInner, osInnerCheck];
end;
//------------------------------------------------------------------------------

procedure SetAsInner(outrec: TOutRec); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  outrec.State := osInner;
end;
//------------------------------------------------------------------------------

procedure SetCheckFlag(outrec: TOutRec); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if outrec.State = osInner then
    outrec.State := osInnerCheck
  else if outrec.State = osOuter then
    outrec.State := osOuterCheck;
end;
//------------------------------------------------------------------------------

procedure UnsetCheckFlag(outrec: TOutRec); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if outrec.State = osInnerCheck then outrec.State := osInner
  else if outrec.State = osOuterCheck then outrec.State := osOuter;
end;
//------------------------------------------------------------------------------

function IsHotEdge(e: PActive): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := assigned(e.OutRec);
end;
//------------------------------------------------------------------------------

function GetPrevHotEdge(e: PActive): PActive;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := e.PrevInAEL;
  while assigned(Result) and (IsOpen(Result) or not IsHotEdge(Result)) do
    Result := Result.PrevInAEL;
end;
//------------------------------------------------------------------------------

function IsFront(e: PActive): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  // the front edge will be the LEFT edge when it's an OUTER polygon
  // so that outer polygons will be orientated clockwise
  Result := (e = e.OutRec.frontE);
end;
//------------------------------------------------------------------------------

function IsInvalidPath(op: TOutPt): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := not assigned(op) or (op.Next = op);
end;
//------------------------------------------------------------------------------

(*******************************************************************************
*  Dx:                             0(90deg)                                    *
*                                  |                                           *
*               +inf (180deg) <--- o ---> -inf (0deg)                          *
*******************************************************************************)

function GetDx(const pt1, pt2: TPoint64): Double;
  {$IFDEF USEINLINING} inline; {$ENDIF}
var
  dy: Int64;
begin
  dy := (pt2.Y - pt1.Y);
  if dy <> 0 then Result := (pt2.X - pt1.X) / dy
  else if (pt2.X > pt1.X) then Result := NegInfinity
  else Result := Infinity;
end;
//------------------------------------------------------------------------------

function TopX(e: PActive; const currentY: Int64): Int64; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if (currentY = e.Top.Y) or (e.Top.X = e.Bot.X) then Result := e.Top.X
  else Result := e.Bot.X + Round(e.Dx*(currentY - e.Bot.Y));
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint64; const Y: Int64): Int64; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
var
  dx: Double;
begin
  if (Y = pt1.Y) then Result := pt1.X
  else if (Y = pt2.Y) then Result := pt2.X
  else if (pt1.Y = pt2.Y) or (pt1.X = pt2.X) then Result := pt2.X
  else
  begin
    dx := GetDx(pt1, pt2);
    Result := pt1.X + Round(dx * (Y - pt1.Y));
  end;
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PActive): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := (e.Top.Y = e.Bot.Y);
end;
//------------------------------------------------------------------------------

function IsHeadingRightHorz(e: PActive): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = NegInfinity);
end;
//------------------------------------------------------------------------------

function IsHeadingLeftHorz(e: PActive): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = Infinity);
end;
//------------------------------------------------------------------------------

procedure SwapActives(var e1, e2: PActive);
  {$IFDEF USEINLINING} inline; {$ENDIF}
var
  e: PActive;
begin
  e := e1; e1 := e2; e2 := e;
end;
//------------------------------------------------------------------------------

function GetPolyType(const e: PActive): TPathType;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function IsSamePolyType(const e1, e2: PActive): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := e1.LocMin.PolyType = e2.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(e1, e2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  if (e1.Dx = e2.Dx) then
  begin
    Result := e1.Top;
    Exit;
  end
  else if e1.Dx = 0 then
  begin
    Result.X := e1.Bot.X;
    if IsHorizontal(e2) then
      Result.Y := e2.Bot.Y
    else
    begin
      with e2^ do b2 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e2.Dx + b2);
    end;
  end
  else if e2.Dx = 0 then
  begin
    Result.X := e2.Bot.X;
    if IsHorizontal(e1) then
      Result.Y := e1.Bot.Y
    else
    begin
      with e1^ do b1 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e1.Dx + b1);
    end;
  end else
  begin
    with e1^ do b1 := Bot.X - Bot.Y * Dx;
    with e2^ do b2 := Bot.X - Bot.Y * Dx;
    m := (b2-b1)/(e1.Dx - e2.Dx);
    Result.Y := round(m);
    if Abs(e1.Dx) < Abs(e2.Dx) then
      Result.X := round(e1.Dx * m + b1) else
      Result.X := round(e2.Dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

procedure SetDx(e: PActive);  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  e.Dx := GetDx(e.Bot, e.Top);
end;
//------------------------------------------------------------------------------

function IsLeftBound(e: PActive): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := e.WindDx > 0;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if IsLeftBound(e) then
    Result := e.vertTop.next else
    Result := e.vertTop.prev;
end;
//------------------------------------------------------------------------------

function NextVertex(op: PVertex; goingFwd: Boolean): PVertex; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if goingFwd then Result := op.next
  else Result := op.prev;
end;
//------------------------------------------------------------------------------

function PrevVertex(op: PVertex; goingFwd: Boolean): PVertex;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  if goingFwd then Result := op.prev
  else Result := op.next;
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPoint64): Double;
var
  x1,x2,y1,y2: Double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  Result := (x1 * y2 - y1 * x2);
end;
//---------------------------------------------------------------------------

function IsClockwise(vertex: PVertex): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := CrossProduct(vertex.prev.Pt, vertex.Pt, vertex.next.Pt) >= 0;
end;
//----------------------------------------------------------------------

function IsClockwise(op: TOutPt): Boolean; overload;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := CrossProduct(op.prev.Pt, op.Pt, op.next.Pt) >= 0;
end;
//----------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.flags;
end;
//------------------------------------------------------------------------------

procedure TerminateHotOpen(e: PActive);
begin
  if e.OutRec.frontE = e then
    e.OutRec.frontE := nil else
    e.OutRec.backE := nil;
  e.OutRec := nil;
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  if IsHorizontal(e) then
  begin
    // we can't be sure whether the MaximaPair is on the left or right, so ...
    Result := e.PrevInAEL;
    while assigned(Result) and (Result.CurrX >= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  // Found!
      Result := Result.PrevInAEL;
    end;
    Result := e.NextInAEL;
    while assigned(Result) and (TopX(Result, e.Top.Y) <= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  // Found!
      Result := Result.NextInAEL;
    end;
  end else
  begin
    Result := e.NextInAEL;
    while assigned(Result) do
    begin
      if Result.vertTop = e.vertTop then Exit;  // Found!
      Result := Result.NextInAEL;
    end;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function PointCount(pts: TOutPt): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
var
  p: TOutPt;
begin
  Result := 0;
  if not Assigned(pts) then Exit;
  p := pts;
  repeat
    Inc(Result);
    p := p.Next;
  until p = pts;
end;
//------------------------------------------------------------------------------

function BuildPath(op: TOutPt): TArrayOfFloatPoint;
var
  i,j, opCnt: Integer;
begin
  Result := nil;
  opCnt := PointCount(op);
  if (opCnt < 2) then Exit;
  setLength(Result, opCnt);
  Result[0] := FloatPoint(op.Pt);
  op := op.Next;
  j := 1;
  for i := 0 to opCnt -2 do
  begin
    Result[j] := FloatPoint(op.Pt);
    if not PointsEqual(Result[j], Result[j-1]) then inc(j);
    op := op.Next;
  end;
  setLength(Result, j);
end;
//------------------------------------------------------------------------------

procedure DisposeOutPt(pp: TOutPt); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  pp.Prev.Next := pp.Next;
  pp.Next.Prev := pp.Prev;
  pp.Free;
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(pp: TOutPt);  {$IFDEF USEINLINING} inline; {$ENDIF}
var
  tmpPp: TOutPt;
begin
  pp.Prev.Next := nil;
  while Assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.Next;
    tmpPp.Free;
  end;
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  dy: Int64;
begin
  dy := PLocalMinima(item2).vertex.Pt.Y - PLocalMinima(item1).vertex.Pt.Y;
  if dy < 0 then Result := -1
  else if dy > 0 then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

procedure SetSides(outRec: TOutRec; startEdge, endEdge: PActive);
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  outRec.frontE := startEdge;
  outRec.backE := endEdge;
end;
//------------------------------------------------------------------------------

procedure SwapOutRecs(e1, e2: PActive);
var
  or1, or2: TOutRec;
  e: PActive;
begin
  or1 := e1.OutRec;
  or2 := e2.OutRec;
  if (or1 = or2) then
  begin
    e := or1.frontE;
    or1.frontE := or1.backE;
    or1.backE := e;
    Exit;
  end;
  if assigned(or1) then
  begin
    if e1 = or1.frontE then
      or1.frontE := e2 else
      or1.backE := e2;
  end;
  if assigned(or2) then
  begin
    if e2 = or2.frontE then
      or2.frontE := e1 else
      or2.backE := e1;
  end;
  e1.OutRec := or2;
  e2.OutRec := or1;
end;
//------------------------------------------------------------------------------

function Area(const path: TArrayOfFloatPoint): Double; overload;
var
  i, j, highI: Integer;
  d: Double;
begin
  Result := 0.0;
  highI := High(path);
  if (highI < 2) then Exit;
  j := highI;
  for i := 0 to highI do
  begin
    d := (path[j].X + path[i].X);
    Result := Result + d * (path[j].Y - path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

function Area(op: TOutPt): Double; overload;
var
  op2: TOutPt;
  d: Double;
begin
  // positive results are clockwise
  Result := 0;
  op2 := op;
  if Assigned(op2) then
  repeat
    d := op2.Prev.Pt.X + op2.Pt.X;
    Result := Result + d * (op2.Prev.Pt.Y - op2.Pt.Y);
    op2 := op2.Next;
  until op2 = op;
  Result := Result * -0.5;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: TOutPt);
var
  op1, op2: TOutPt;
begin
  if not Assigned(op) then Exit;
  op1 := op;
  repeat
    op2:= op1.Next;
    op1.Next := op1.Prev;
    op1.Prev := op2;
    op1 := op2;
  until op1 = op;
end;
//------------------------------------------------------------------------------

function RecheckInnerOuter(e: PActive): Boolean;
var
  wasOuter, isOuter: Boolean;
  e2: PActive;
  area: Double;
begin
  area := GR32_Clipper.Area(e.outrec.Pts);
  Result := area <> 0;
  if not Result then Exit; // returns false when area == 0

  wasOuter := GR32_Clipper.IsOuter(e.OutRec);
  isOuter := true;
  e2 := e.PrevInAEL;
  while assigned(e2) do
  begin
   if IsHotEdge(e2) and not IsOpen(e2) then isOuter := not isOuter;
   e2 := e2.PrevInAEL;
  end;

  if isOuter <> wasOuter then
  begin
    if isOuter then SetAsOuter(e.outrec)
    else SetAsInner(e.outrec);
  end;

  e2 := GetPrevHotEdge(e);
  if isOuter then
  begin
    if assigned(e2) and IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec
    else e.OutRec.Owner := nil;
  end else
  begin
    if not assigned(e2) then SetAsOuter(e.OutRec)
    else if IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec.Owner
    else e.OutRec.Owner := e2.OutRec;
  end;

  if (area > 0) <> isOuter then ReverseOutPts(e.outrec.Pts);
  UnsetCheckFlag(e.OutRec);
end;
//------------------------------------------------------------------------------

procedure SwapSides(outRec: TOutRec); {$IFDEF USEINLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  e2 := outRec.frontE;
  outRec.frontE := outRec.backE;
  outRec.backE := e2;
  outRec.Pts := outRec.Pts.Next;
end;
//------------------------------------------------------------------------------

function FixSides(e: PActive): Boolean;
begin
  Result := not RecheckInnerOuter(e) or (IsOuter(e.OutRec) <> IsFront(e));
  if Result then SwapSides(e.OutRec);
end;
//------------------------------------------------------------------------------

procedure SetOwnerAndInnerOuterState(e: PActive);
var
  e2: PActive;
  outRec: TOutRec;
begin
  outRec := e.OutRec;
  if IsOpen(e) then
  begin
    outRec.Owner := nil;
    outRec.State := osOpen;
    Exit;
  end;
  // set owner ...
  if IsHeadingLeftHorz(e) then
  begin
    e2 := e.NextInAEL; // ie assess state from opposite direction
    while assigned(e2) and (not IsHotEdge(e2) or IsOpen(e2)) do
      e2 := e2.NextInAEL;
    if not assigned(e2) then outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.frontE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end else
  begin
    e2 := GetPrevHotEdge(e);
    if not assigned(e2) then
      outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.backE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end;

  // set inner/outer ...
  if not assigned(outRec.Owner) or IsInner(outRec.Owner) then
    outRec.State := osOuter else
    outRec.State := osInner;

end;
//------------------------------------------------------------------------------

function EdgesAdjacentInAEL(node: PIntersectNode): Boolean;
  {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  with node^ do
    Result := (Edge1.NextInAEL = Edge2) or (Edge1.PrevInAEL = Edge2);
end;
//------------------------------------------------------------------------------

function IntersectListSort(node1, node2: Pointer): Integer;
begin
  Result := PIntersectNode(node2).Pt.Y - PIntersectNode(node1).Pt.Y;
  if (Result = 0) and (node1 <> node2) then
    Result := PIntersectNode(node1).Pt.X - PIntersectNode(node2).Pt.X;
end;

//------------------------------------------------------------------------------
//  TClipper methods ...
//------------------------------------------------------------------------------

constructor TClipper.Create;
begin
  FLocMinList := TList.Create;
  FOutRecList := TList.Create;
  FIntersectList := TList.Create;
  FVertexList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipper.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FIntersectList.Free;
  FVertexList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipper.CleanUp;
var
  dummy: Int64;
begin
  try
    // in case of exceptions ...
    while assigned(FActives) do DeleteFromAEL(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;

    DisposeScanLineList;
    DisposeAllOutRecs;
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Clear;
begin
  CleanUp;
  DisposeVerticesAndLocalMinima;
  FCurrentLocMinIdx := 0;
  FLocMinListSorted := false;
  FHasOpenPaths := False;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  i: Integer;
begin
  if not FLocMinListSorted then
  begin
    FLocMinList.Sort(LocMinListSort);
    FLocMinListSorted := true;
  end;
  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMinima(FLocMinList[i]).vertex.Pt.Y);
  FCurrentLocMinIdx := 0;
  FActives := nil;
  FSel := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  // The scanline list is a single-linked list of all the Y coordinates of
  // subject and clip vertices in the clipping operation (sorted descending).
  // However, only scanline Y's at Local Minima are inserted before clipping
  // starts. While scanlines are removed sequentially during the sweep, new
  // scanlines are only inserted whenever edge bounds are updated. This keeps
  // the scanline list relatively short, optimising performance.
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.Y := Y;
    FScanLine := newSl;
    newSl.Next := nil;
  end else if Y > FScanLine.Y then
  begin
    new(newSl);
    newSl.Y := Y;
    newSl.Next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.Next) and (Y <= sl.Next.Y) do
      sl := sl.Next;
    if Y = sl.Y then Exit; // skip duplicates
    new(newSl);
    newSl.Y := Y;
    newSl.Next := sl.Next;
    sl.Next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.PopScanLine(out Y: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  Y := FScanLine.Y;
  sl := FScanLine;
  FScanLine := FScanLine.Next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipper.PopLocalMinima(Y: Int64;
  out localMinima: PLocalMinima): Boolean;
begin
  Result := false;
  if FCurrentLocMinIdx = FLocMinList.Count then Exit;
  localMinima := PLocalMinima(FLocMinList[FCurrentLocMinIdx]);
  if (localMinima.vertex.Pt.Y = Y) then
  begin
    inc(FCurrentLocMinIdx);
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeScanLineList;
var
  sl: PScanLine;
begin
  while Assigned(FScanLine) do
  begin
    sl := FScanLine.Next;
    Dispose(FScanLine);
    FScanLine := sl;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(index: Integer);
var
  outRec: TOutRec;
begin
  outRec := FOutRecList[index];
  if Assigned(outRec.Pts) then DisposePolyPts(outRec.Pts);
  outRec.Free;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeAllOutRecs;
var
  i: Integer;
begin
  for i := 0 to FOutRecList.Count -1 do DisposeOutRec(i);
  FOutRecList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMinima(FLocMinList[i]));
  FLocMinList.Clear;
  for i := 0 to FVertexList.Count -1 do FreeMem(FVertexList[i]);
  FVertexList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPathToVertexList(const path: TArrayOfFloatPoint;
  polyType: TPathType; isOpen: Boolean);
var
  i, j, pathLen: Integer;
  isFlat, goingUp, p0IsMinima, p0IsMaxima: Boolean;
  v: PVertex;
  va: PVertexArray;

  procedure AddLocMin(vert: PVertex);
  var
    lm: PLocalMinima;
  begin
    if vfLocMin in vert.flags then Exit; // ensures vertex is added only once
    Include(vert.flags, vfLocMin);
    new(lm);
    lm.vertex := vert;
    lm.PolyType := polyType;
    lm.IsOpen := isOpen;
    FLocMinList.Add(lm);                 // nb: sorted in Reset()
  end;
  //----------------------------------------------------------------------------

begin
  pathLen := length(path);
  if (pathLen < 2) then Exit;

  p0IsMinima := false;
  p0IsMaxima := false;
  i := 1;
  // find the first non-horizontal segment in the path ...
  while (i < pathLen) and (path[i].Y = path[0].Y) do inc(i);
  isFlat := i = pathLen;
  if isFlat then
  begin
    if not isOpen then Exit;    // Ignore closed paths that have ZERO area.
    goingUp := false;           // And this just stops a compiler warning.
  end else
  begin
    goingUp := path[i].Y < path[0].Y;
    if goingUp then
    begin
      i := pathLen -1;
      while path[i].Y = path[0].Y do dec(i);
      p0IsMinima := path[i].Y < path[0].Y; // p[0].Y == a minima
    end else
    begin
      i := pathLen -1;
      while path[i].Y = path[0].Y do dec(i);
      p0IsMaxima := path[i].Y > path[0].Y; // p[0].Y == a maxima
    end;
  end;

  GetMem(va, sizeof(TVertex) * pathLen);
  FVertexList.Add(va);

  va[0].Pt := Point64(path[0]);
  va[0].flags := [];
  if isOpen then
  begin
    include(va[0].flags, vfOpenStart);
    if goingUp then
      AddLocMin(@va[0]) else
      include(va[0].flags, vfLocMax);
  end;

  // nb: polygon orientation is determined later (see InsertLocalMinimaIntoAEL).
  i := 0;
  for j := 1 to pathLen -1 do
  begin
    va[j].Pt := Point64(path[j]);
    if PointsEqual(va[j].Pt, va[i].Pt) then Continue;
    va[j].flags := [];
    va[i].next := @va[j];
    va[j].prev := @va[i];
    if (path[j].Y > path[i].Y) and goingUp then
    begin
      include(va[i].flags, vfLocMax);
      goingUp := false;
    end
    else if (path[j].Y < path[i].Y) and not goingUp then
    begin
      goingUp := true;
      AddLocMin(@va[i]);
    end;
    i := j;
  end;
  // i: index of the last vertex in the path.
  va[i].next := @va[0];
  va[0].prev := @va[i];

  if isOpen then
  begin
    include(va[i].flags, vfOpenEnd);
    if goingUp then
      include(va[i].flags, vfLocMax) else
      AddLocMin(@va[i]);
  end
  else if goingUp then
  begin
    // going up so find local maxima ...
    v := @va[i];
    while (v.Next.Pt.Y <= v.Pt.Y) do v := v.next;
    include(v.flags, vfLocMax);
    if p0IsMinima then AddLocMin(@va[0]); // ie just turned to going up
  end else
  begin
    // going down so find local minima ...
    v := @va[i];
    while (v.Next.Pt.Y >= v.Pt.Y) do v := v.next;
    AddLocMin(v);
    if p0IsMaxima then include(va[0].flags, vfLocMax);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPath(const path64: TArrayOfFloatPoint;
  PolyType: TPathType; isOpen: Boolean);
begin
  if isOpen then
  begin
    if (PolyType = ptClip) then
      raise EClipperLibException.Create(rsClipper_OpenPathErr);
    FHasOpenPaths := true;
  end;
  FLocMinListSorted := false;
  AddPathToVertexList(path64, polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPath(const path: TArrayOfFixedPoint;
  PolyType: TPathType; isOpen: Boolean);
begin
  AddPathToVertexList(FixedToFloat(path), polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths64: TArrayOfArrayOfFloatPoint;
  polyType: TPathType; isOpen: Boolean);
var
  i: Integer;
begin
  for i := 0 to high(paths64) do AddPath(paths64[i], polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths: TArrayOfArrayOfFixedPoint;
  polyType: TPathType = ptSubject; isOpen: Boolean = false);
var
  i: Integer;
begin
  for i := 0 to high(paths) do AddPath(paths[i], polyType, isOpen);
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingClosed(e: PActive): Boolean;
begin
  Result := false;
  case FFillRule of
    frNonZero: if abs(e.WindCnt) <> 1 then Exit;
    frPositive: if (e.WindCnt <> 1) then Exit;
    frNegative: if (e.WindCnt <> -1) then Exit;
  end;

  case FClipType of
    ctIntersection:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
        frPositive: Result := (e.WindCnt2 > 0);
        frNegative: Result := (e.WindCnt2 < 0);
      end;
    ctUnion:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
        frPositive: Result := (e.WindCnt2 <= 0);
        frNegative: Result := (e.WindCnt2 >= 0);
      end;
    ctDifference:
      if GetPolyType(e) = ptSubject then
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
          frPositive: Result := (e.WindCnt2 <= 0);
          frNegative: Result := (e.WindCnt2 >= 0);
        end
      else
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
          frPositive: Result := (e.WindCnt2 > 0);
          frNegative: Result := (e.WindCnt2 < 0);
        end;
    ctXor:
        Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingOpen(e: PActive): Boolean;
begin
    case FClipType of
      ctIntersection:
        Result := (e.WindCnt2 <> 0);
      ctXor:
        Result := (e.WindCnt <> 0) <> (e.WindCnt2 <> 0);
      ctDifference:
        Result := (e.WindCnt2 = 0);
      else // ctUnion:
        Result := (e.WindCnt = 0) and (e.WindCnt2 = 0);
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindCountForClosedPathEdge(e: PActive);
var
  e2: PActive;
begin
  // Wind counts refer to polygon regions not edges, so here an edge's WindCnt
  // indicates the higher of the wind counts for the two regions touching the
  // edge. (nb: Adjacent regions can only ever have their wind counts differ by
  // one. Also, open paths have no meaningful wind directions or counts.)

  e2 := e.PrevInAEL;
  // find the nearest closed edge of the same PolyType in AEL (heading left)
  while Assigned(e2) and (not IsSamePolyType(e2, e) or IsOpen(e2)) do
    e2 := e2.PrevInAEL;

  if not Assigned(e2) then
  begin
    e.WindCnt := e.WindDx;
    e2 := FActives;
  end
  else if (FFillRule = frEvenOdd) then
  begin
    e.WindCnt := e.WindDx;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end else
  begin
    // NonZero, positive, or negative filling here ...
    // if e's WindCnt is in the SAME direction as its WindDx, then polygon
    // filling will be on the right of 'e'.
    // nb: neither e2.WindCnt nor e2.WindDx should ever be 0.
    if (e2.WindCnt * e2.WindDx < 0) then
    begin
      // opposite directions so 'e' is outside 'e2' ...
      if (Abs(e2.WindCnt) > 1) then
      begin
        // outside prev poly but still inside another.
        if (e2.WindDx * e.WindDx < 0) then
          // reversing direction so use the same WC
          e.WindCnt := e2.WindCnt else
          // otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
          e.WindCnt := e2.WindCnt + e.WindDx;
      end
      // now outside all polys of same polytype so set own WC ...
      else e.WindCnt := e.WindDx;
    end else
    begin
      // 'e' must be inside 'e2'
      if (e2.WindDx * e.WindDx < 0) then
        // reversing direction so use the same WC
        e.WindCnt := e2.WindCnt
      else
        // otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
        e.WindCnt := e2.WindCnt + e.WindDx;
    end;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end;

  // update WindCnt2 ...
  if FFillRule = frEvenOdd then
    while (e2 <> e) do
    begin
      if IsSamePolyType(e2, e) or IsOpen(e2) then // do nothing
      else if e.WindCnt2 = 0 then e.WindCnt2 := 1
      else e.WindCnt2 := 0;
      e2 := e2.NextInAEL;
    end
  else
    while (e2 <> e) do
    begin
      if not IsSamePolyType(e2, e) and not IsOpen(e2) then
        Inc(e.WindCnt2, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindCountForOpenPathEdge(e: PActive);
var
  e2: PActive;
  cnt1, cnt2: Integer;
begin
  e2 := FActives;
  if FFillRule = frEvenOdd then
  begin
    cnt1 := 0;
    cnt2 := 0;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(cnt2)
      else if not IsOpen(e2) then inc(cnt1);
      e2 := e2.NextInAEL;
    end;
    if Odd(cnt1) then e.WindCnt := 1 else e.WindCnt := 0;
    if Odd(cnt2) then e.WindCnt2 := 1 else e.WindCnt2 := 0;
  end else
  begin
    // if FClipType in [ctUnion, ctDifference] then e.WindCnt := e.WindDx;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(e.WindCnt2, e2.WindDx)
      else if not IsOpen(e2) then inc(e.WindCnt, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidAelOrder(a1, a2: PActive): Boolean;
var
  pt1, pt2: TPoint64;
  op1, op2: PVertex;
  X: Int64;
begin
  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  pt1 := a1.Bot; pt2 := a2.Bot;
  op1 := a1.VertTop; op2 := a2.VertTop;
  while true do
  begin
    if op1.Pt.Y >= op2.Pt.Y then
    begin
      X := TopX(pt2, op2.Pt, op1.Pt.Y) - op1.Pt.X;
      Result := X > 0;
      if X <> 0 then Exit;
      if op2.Pt.Y = op1.Pt.Y then
      begin
        pt2 := op2.Pt;
        op2 := NextVertex(op2, IsLeftBound(a2));
      end;
      pt1 := op1.Pt;
      op1 := NextVertex(op1, IsLeftBound(a1));
    end else
    begin
      X := op2.Pt.X - TopX(pt1, op1.Pt, op2.Pt.Y);
      Result := X > 0;
      if X <> 0 then Exit;
      pt2 := op2.Pt;
      op2 := NextVertex(op2, IsLeftBound(a2));
    end;

    if (op1.Pt.Y > pt1.Y) then
    begin
      Result := (a1.WindDx > 0) <> IsClockwise(PrevVertex(op1, a1.WindDx > 0));
      Exit;
    end else if (op2.Pt.Y > pt2.Y) then
    begin
      Result := (a2.WindDx > 0) = IsClockwise(PrevVertex(op2, a2.WindDx > 0));
      Exit;
    end;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLeftEdge(e: PActive);
var
  e2: PActive;
begin
  if not Assigned(FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := nil;
    FActives := e;
  end
  else if IsValidAelOrder(e, FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := FActives;
    FActives.PrevInAEL := e;
    FActives := e;
  end else
  begin
    e2 := FActives;
    while Assigned(e2.NextInAEL) and IsValidAelOrder(e2.NextInAEL, e) do
      e2 := e2.NextInAEL;
    e.NextInAEL := e2.NextInAEL;
    if Assigned(e2.NextInAEL) then e2.NextInAEL.PrevInAEL := e;
    e.PrevInAEL := e2;
    e2.NextInAEL := e;
  end;
end;
//----------------------------------------------------------------------

procedure InsertRightEdge(e, e2: PActive);
begin
  e2.NextInAEL := e.NextInAEL;
  if Assigned(e.NextInAEL) then e.NextInAEL.PrevInAEL := e2;
  e2.PrevInAEL := e;
  e.NextInAEL := e2;
end;
//----------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const botY: Int64);
var
  leftB, rightB: PActive;
  locMin: PLocalMinima;
  contributing: Boolean;
begin
  // Add local minima (if any) at BotY ...
  // nb: horizontal local minima edges should contain locMin.vertex.prev

  while PopLocalMinima(botY, locMin) do
  begin
    if (vfOpenStart in locMin.vertex.flags) then
    begin
      leftB := nil;
    end else
    begin
      new(leftB);
      FillChar(leftB^, sizeof(TActive), 0);
      leftB.LocMin := locMin;
      leftB.OutRec := nil;
      leftB.Bot := locMin.vertex.Pt;
      leftB.vertTop := locMin.vertex.prev; // ie descending
      leftB.Top := leftB.vertTop.Pt;
      leftB.CurrX := leftB.Bot.X;
      leftB.WindDx := -1;
      SetDx(leftB);
    end;

    if (vfOpenEnd in locMin.vertex.flags) then
    begin
      rightB := nil;
    end else
    begin
      new(rightB);
      FillChar(rightB^, sizeof(TActive), 0);
      rightB.LocMin := locMin;
      rightB.OutRec := nil;
      rightB.Bot := locMin.vertex.Pt;
      rightB.vertTop := locMin.vertex.next; // ie ascending
      rightB.Top := rightB.vertTop.Pt;
      rightB.CurrX := rightB.Bot.X;
      rightB.WindDx := 1;
      SetDx(rightB);
    end;
    // Currently LeftB is just the descending bound and RightB is the ascending.
    // Now if the LeftB isn't on the left of RightB then we need swap them.
    if assigned(leftB) and assigned(rightB) then
    begin
      if IsHorizontal(leftB) then
      begin
        if IsHeadingRightHorz(leftB) then SwapActives(leftB, rightB);
      end
      else if IsHorizontal(rightB) then
      begin
        if IsHeadingLeftHorz(rightB) then SwapActives(leftB, rightB);
      end
      else if (leftB.Dx < rightB.Dx) then SwapActives(leftB, rightB);
    end
    else if not assigned(leftB) then
    begin
      leftB := rightB;
      rightB := nil;
    end;

    InsertLeftEdge(leftB);                   // /// //
    // todo: further validation of position in AEL ???

    if IsOpen(leftB) then
    begin
      SetWindCountForOpenPathEdge(leftB);
      contributing := IsContributingOpen(leftB);
    end else
    begin
      SetWindCountForClosedPathEdge(leftB);
      contributing := IsContributingClosed(leftB);
    end;

    if assigned(rightB) then
    begin
      rightB.WindCnt := leftB.WindCnt;
      rightB.WindCnt2 := leftB.WindCnt2;
      InsertRightEdge(leftB, rightB);        // /// //
      if contributing then
        AddLocalMinPoly(leftB, rightB, leftB.Bot, true);

      if IsHorizontal(rightB) then
        PushHorz(rightB) else
        InsertScanLine(rightB.Top.Y);
    end
    else if contributing then
      StartOpenPath(leftB, leftB.Bot);

    if IsHorizontal(leftB) then
      PushHorz(leftB) else
      InsertScanLine(leftB.Top.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.PushHorz(e: PActive);
begin
  if assigned(FSel) then
    e.NextInSEL := FSel else
    e.NextInSEL := nil;
  FSel := e;
end;
//------------------------------------------------------------------------------

function TClipper.PopHorz(out e: PActive): Boolean;
begin
  Result := assigned(FSel);
  if not Result then Exit;
  e := FSel;
  FSel := FSel.NextInSEL;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(e1, e2: PActive; const pt: TPoint64;
  IsNew: Boolean = false; orientationCheckRequired: Boolean = false);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Pts := nil;

  e1.OutRec := outRec;
  SetOwnerAndInnerOuterState(e1);
  // flag when orientatation needs to be rechecked later ...
  if orientationCheckRequired then SetCheckFlag(outRec);

  e2.OutRec := outRec;
  if not IsOpen(e1) then
  begin
    // Setting the owner and inner/outer states (above) is an essential
    // precursor to setting edge 'sides' (ie left and right sides of output
    // polygons) and hence the orientation of output paths ...
    if IsOuter(outRec) = IsNew then
      SetSides(outRec, e1, e2) else
      SetSides(outRec, e2, e1);
  end;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;

  // nb: currently e1.NextInAEL == e2 but this could change on return
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
var
  op: TOutPt;
begin
  if not IsOpen(e1) and (IsFront(e1) = IsFront(e2)) then
    if not FixSides(e1) then FixSides(e2);

  op := AddOutPt(e1, pt);
  // AddOutPt(e2, pt); // this may no longer be necessary

  if  (e1.OutRec = e2.OutRec) then
  begin
    if e1.OutRec.State in [osOuterCheck, osInnerCheck] then
      RecheckInnerOuter(e1);

    // nb: IsClockwise() is generally faster than Area() but will occasionally
    // give false positives when there are tiny self-intersections at the top...
    if IsOuter(e1.OutRec) then
    begin
      if not IsClockwise(op) and (Area(op) < 0) then
        ReverseOutPts(e1.OutRec.Pts);
    end else
    begin
      if IsClockwise(op) and (Area(op) > 0) then
        ReverseOutPts(e1.OutRec.Pts);
    end;

    e1.outRec.frontE := nil;
    e1.outRec.backE := nil;
    e1.OutRec := nil;
    e2.OutRec := nil;
  end
  // and to preserve the winding orientation of Outrec ...
  else if e1.OutRec.Idx < e2.OutRec.Idx then
    JoinOutrecPaths(e1, e2) else
    JoinOutrecPaths(e2, e1);

end;
//------------------------------------------------------------------------------

procedure TClipper.JoinOutrecPaths(e1, e2: PActive);
var
  p1_start, p1_end, p2_start, p2_end: TOutPt;
begin
  if (IsFront(e1) = IsFront(e2)) then
  begin
    // one or other 'side' must be wrong ...
    if IsOpen(e1) then SwapSides(e2.OutRec)
    else if not FixSides(e1) and not FixSides(e2) then
      raise EClipperLibException.Create(rsClipper_ClippingErr);
    if e1.OutRec.Owner = e2.OutRec then
      e1.OutRec.Owner := e2.OutRec.Owner;
  end;

  // join e2 outrec path onto e1 outrec path and then delete e2 outrec path
  // pointers. (see joining_outpt.svg)
  p1_start :=  e1.OutRec.Pts;
  p2_start :=  e2.OutRec.Pts;
  p1_end := p1_start.Next;
  p2_end := p2_start.Next;

  if IsFront(e1) then
  begin
    p2_end.Prev := p1_start;
    p1_start.Next := p2_end;
    p2_start.Next := p1_end;
    p1_end.Prev := p2_start;
    e1.OutRec.Pts := p2_start;
    e1.OutRec.frontE := e2.OutRec.frontE;
    if not IsOpen(e1) then e1.OutRec.frontE.OutRec := e1.OutRec;
    // strip duplicates ...
    if (p2_end <> p2_start) and PointsEqual(p2_end.Pt, p2_end.Prev.Pt) then
      DisposeOutPt(p2_end);
  end else
  begin
    p1_end.Prev := p2_start;
    p2_start.Next := p1_end;
    p1_start.Next := p2_end;
    p2_end.Prev := p1_start;
    e1.OutRec.backE := e2.OutRec.backE;
    if not IsOpen(e1) then e1.OutRec.backE.OutRec := e1.OutRec;
    // strip duplicates ...
    if (p1_end <> p1_start) and PointsEqual(p1_end.Pt, p1_end.Prev.Pt) then
      DisposeOutPt(p1_end);
  end;

  if PointsEqual(e1.OutRec.Pts.Pt, e1.OutRec.Pts.Prev.Pt) and
    not IsInvalidPath(e1.OutRec.Pts) then
      DisposeOutPt(e1.OutRec.Pts.Prev);

  // after joining, the e2.OutRec must contains no vertices ...
  e2.OutRec.frontE := nil;
  e2.OutRec.backE := nil;
  e2.OutRec.Pts := nil;
  e2.OutRec.Owner := e1.OutRec; // this may be redundant

  // and e1 and e2 are maxima and are about to be dropped from the Actives list.
  e1.OutRec := nil;
  e2.OutRec := nil;
end;
//------------------------------------------------------------------------------

function TClipper.AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
var
  opFront, opBack: TOutPt;
  toFront: Boolean;
  outrec: TOutRec;
begin
  // Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
  // opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
  outrec := e.OutRec;
  toFront := IsFront(e);
  opFront := outrec.Pts;
  opBack := opFront.Next;
  if toFront and PointsEqual(pt, opFront.Pt) then
    Result := opFront
  else if not toFront and PointsEqual(pt, opBack.Pt) then
    Result := opBack
  else
  begin
    Result := TOutPt.Create;
    Result.Pt := pt;
    opBack.Prev := Result;
    Result.Prev := opFront;
    Result.Next := opBack;
    opFront.Next := Result;
    if toFront then outrec.Pts := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.StartOpenPath(e: PActive; const pt: TPoint64);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Owner := nil;
  outRec.State := osOpen;
  outRec.Pts := nil;
  outRec.frontE := nil;
  outRec.backE := nil;
  e.OutRec := outRec;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var e: PActive);
begin
  e.Bot := e.Top;
  e.vertTop := NextVertex(e);
  e.Top := e.vertTop.Pt;
  e.CurrX := e.Bot.X;
  SetDx(e);
  if not IsHorizontal(e) then InsertScanLine(e.Top.Y);
end;
//------------------------------------------------------------------------------

procedure TClipper.IntersectEdges(e1, e2: PActive;
  const pt: TPoint64; orientationCheckRequired: Boolean = false);
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
begin

  // MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
  if FHasOpenPaths and (IsOpen(e1) or IsOpen(e2)) then
  begin
    if (IsOpen(e1) and IsOpen(e2) ) then Exit;
    // the following line avoids duplicating a whole lot of code ...
    if IsOpen(e2) then SwapActives(e1, e2);
    case FClipType of
      ctIntersection, ctDifference:
        if IsSamePolyType(e1, e2) or (abs(e2.WindCnt) <> 1) then Exit;
      ctUnion:
        if IsHotEdge(e1) <> ((abs(e2.WindCnt) <> 1) or
          (IsHotEdge(e1) <> (e2.WindCnt2 <> 0))) then Exit; // just works!
      ctXor:
        if (abs(e2.WindCnt) <> 1) then Exit;
    end;
    // toggle contribution ...
    if IsHotEdge(e1) then
    begin
      AddOutPt(e1, pt);
      TerminateHotOpen(e1);
    end
    else StartOpenPath(e1, pt);
    Exit;
  end;

  // UPDATE WINDING COUNTS...

  if IsSamePolyType(e1, e2) then
  begin
    if FFillRule = frEvenOdd then
    begin
      e1WindCnt := e1.WindCnt;
      e1.WindCnt := e2.WindCnt;
      e2.WindCnt := e1WindCnt;
    end else
    begin
      if e1.WindCnt + e2.WindDx = 0 then
        e1.WindCnt := -e1.WindCnt else
        Inc(e1.WindCnt, e2.WindDx);
      if e2.WindCnt - e1.WindDx = 0 then
        e2.WindCnt := -e2.WindCnt else
        Dec(e2.WindCnt, e1.WindDx);
    end;
  end else
  begin
    if FFillRule <> frEvenOdd then Inc(e1.WindCnt2, e2.WindDx)
    else if e1.WindCnt2 = 0 then e1.WindCnt2 := 1
    else e1.WindCnt2 := 0;

    if FFillRule <> frEvenOdd then Dec(e2.WindCnt2, e1.WindDx)
    else if e2.WindCnt2 = 0 then e2.WindCnt2 := 1
    else e2.WindCnt2 := 0;
  end;

  case FFillRule of
    frPositive:
      begin
        e1WindCnt := e1.WindCnt;
        e2WindCnt := e2.WindCnt;
      end;
    frNegative:
      begin
        e1WindCnt := -e1.WindCnt;
        e2WindCnt := -e2.WindCnt;
      end;
    else
      begin
        e1WindCnt := abs(e1.WindCnt);
        e2WindCnt := abs(e2.WindCnt);
      end;
  end;

  if (not IsHotEdge(e1) and not (e1WindCnt in [0,1])) or
    (not IsHotEdge(e2) and not (e2WindCnt in [0,1])) then Exit;

  // NOW PROCESS THE INTERSECTION ...

  // if both edges are 'hot' ...
  if IsHotEdge(e1) and IsHotEdge(e2) then
  begin
    if not (e1WindCnt in [0,1]) or not (e2WindCnt in [0,1]) or
      (not IsSamePolyType(e1, e2) and (fClipType <> ctXor)) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
    end else if IsFront(e1) or (e1.OutRec = e2.OutRec) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
      AddLocalMinPoly(e1, e2, pt);
    end else
    begin
      // right & left bounds touching, NOT maxima & minima ...
      AddOutPt(e1, pt);
      AddOutPt(e2, pt);
      SwapOutRecs(e1, e2);
    end;
  end

  // if one or other edge is 'hot' ...
  else if IsHotEdge(e1) then
  begin
    AddOutPt(e1, pt);
    SwapOutRecs(e1, e2);
  end
  else if IsHotEdge(e2) then
  begin
    AddOutPt(e2, pt);
    SwapOutRecs(e1, e2);
  end

  else // neither edge is 'hot'
  begin
    case FFillRule of
      frPositive:
      begin
        e1WindCnt2 := e1.WindCnt2;
        e2WindCnt2 := e2.WindCnt2;
      end;
      frNegative:
      begin
        e1WindCnt2 := -e1.WindCnt2;
        e2WindCnt2 := -e2.WindCnt2;
      end
      else
      begin
        e1WindCnt2 := abs(e1.WindCnt2);
        e2WindCnt2 := abs(e2.WindCnt2);
      end;
    end;

    if not IsSamePolyType(e1, e2) then
    begin
      AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
    end
    else if (e1WindCnt = 1) and (e2WindCnt = 1) then
      case FClipType of
        ctIntersection:
          if (e1WindCnt2 > 0) and (e2WindCnt2 > 0) then
            AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctUnion:
          if (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0) then
            AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctDifference:
          if ((GetPolyType(e1) = ptClip) and
                (e1WindCnt2 > 0) and (e2WindCnt2 > 0)) or
              ((GetPolyType(e1) = ptSubject) and
                (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0)) then
                  AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctXor:
          AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
      end
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromAEL(e: PActive);
var
  aelPrev, aelNext: PActive;
begin
  aelPrev := e.PrevInAEL;
  aelNext := e.NextInAEL;
  if not Assigned(aelPrev) and not Assigned(aelNext) and
    (e <> FActives) then Exit; // already deleted
  if Assigned(aelPrev) then aelPrev.NextInAEL := aelNext
  else FActives := aelNext;
  if Assigned(aelNext) then aelNext.PrevInAEL := aelPrev;
  Dispose(e);
end;
//------------------------------------------------------------------------------

procedure TClipper.AdjustCurrXAndCopyToSEL(topY: Int64);
var
  e: PActive;
begin
  FSel := FActives;
  e := FActives;
  while Assigned(e) do
  begin
    e.PrevInSEL := e.PrevInAEL;
    e.NextInSEL := e.NextInAEL;
    e.CurrX := TopX(e, topY);
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ExecuteInternal(clipType: TClipType;
  fillRule: TFillRule);
var
  Y: Int64;
  e: PActive;
begin
  if clipType = ctNone then Exit;
  FFillRule := fillRule;
  FClipType := clipType;
  Reset;
  if not PopScanLine(Y) then Exit;
  while true do
  begin
    InsertLocalMinimaIntoAEL(Y);
    while PopHorz(e) do DoHorizontal(e);
    FBotY := Y;                       // FBotY == bottom of scanbeam
    if not PopScanLine(Y) then Break; // Y new top of scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  dummy: TArrayOfArrayOfFloatPoint;
begin
  Result := true;
  closedPaths := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildResult(closedPaths, dummy);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  tmp: TArrayOfArrayOfFloatPoint;
begin
  Result := Execute(clipType, fillRule, tmp);
  closedPaths := FloatToFixed(tmp);
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths, openPaths: TArrayOfArrayOfFloatPoint): Boolean;
begin
  Result := true;
  closedPaths := nil;
  openPaths := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildResult(closedPaths, openPaths);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths, openPaths: TArrayOfArrayOfFixedPoint): Boolean;
var
  tmp, tmp2: TArrayOfArrayOfFloatPoint;
begin
  Result := Execute(clipType, fillRule, tmp, tmp2);
  closedPaths := FloatToFixed(tmp);
  openPaths := FloatToFixed(tmp2);
end;
//------------------------------------------------------------------------------

procedure TClipper.DoIntersections(const topY: Int64);
begin
  if BuildIntersectList(topY) then
  try
    ProcessIntersectList;
  finally
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeIntersectNodes;
var
  i: Integer;
begin
  for i := 0 to FIntersectList.Count - 1 do
    Dispose(IntersectNode[i]);
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddNewIntersectNode(e1, e2: PActive; topY: Int64);
var
  pt: TPoint64;
  node: PIntersectNode;
begin
  pt := GetIntersectPoint(e1, e2);
  // Rounding errors can occasionally place the calculated intersection
  // point either below or above the scanbeam, so check and correct ...
  if (pt.Y > FBotY) then
  begin
    // E.Curr.Y is still at the bottom of scanbeam here
    pt.Y := FBotY;
    // use the more vertical of the 2 edges to derive pt.X ...
    if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := TopX(e1, FBotY) else
      pt.X := TopX(e2, FBotY);
  end
  else if pt.Y < topY then
  begin
    // TopY = top of scanbeam
    pt.Y := topY;
    if e1.Top.Y = topY then
      pt.X := e1.Top.X
    else if e2.Top.Y = topY then
      pt.X := e2.Top.X
    else if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := e1.CurrX
    else
      pt.X := e2.CurrX;
  end;

  new(node);
  node.Edge1 := e1;
  node.Edge2 := e2;
  node.Pt := pt;
  FIntersectList.Add(node);
end;
//------------------------------------------------------------------------------

function TClipper.BuildIntersectList(const topY: Int64): Boolean;
var
  i, lCnt, rCnt, jumpSize: Integer;
  first, second, base, prevBase, p, n, tmp: PActive;
begin
  Result := false;
  if not Assigned(FActives) or not Assigned(FActives.NextInAEL) then Exit;

  // Calculate edge positions at the top of the current scanbeam, and from this
  // we will determine the intersections required to reach these new positions.
  AdjustCurrXAndCopyToSEL(topY);
  // Track every edge intersection between the bottom and top of each scanbeam,
  // using a stable merge sort to ensure edges are adjacent when intersecting.
  // Re merge sorts see https://stackoverflow.com/a/46319131/359538
	jumpSize := 1;
	while (true) do
  begin
    first := FSel;
    prevBase := nil;
		// sort successive larger jump counts of nodes ...
		while assigned(first) do
    begin
			if (jumpSize = 1) then
      begin
        second := first.NextInSEL;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.NextInSEL;
      end else
      begin
			  second := first.Jump;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.Jump;
      end;

      // now sort first and second groups ...
      base := first;
			lCnt := jumpSize; rCnt := jumpSize;
			while (lCnt > 0) and (rCnt > 0) do
			begin
				if (first.CurrX > second.CurrX) then
        begin
          tmp := second.PrevInSEL;

          // create intersect 'node' events for each time 'second' needs to
          // move left, ie intersecting with its prior edge ...
          for i := 1 to lCnt do
          begin
            AddNewIntersectNode(tmp, second, topY);
            tmp := tmp.PrevInSEL;
          end;

          // now move the out of place 'second' to it's new position in SEL ...
          if (first = base) then
          begin
            if assigned(prevBase) then prevBase.Jump := second;
            base := second;
            base.Jump := first.Jump;
            if (first.PrevInSEL = nil) then FSel := second;
          end;
          tmp := second.NextInSEL;

          // first remove 'second' from list ...
          p := second.PrevInSEL;
          n := second.NextInSEL;
          p.NextInSEL := n;
          if Assigned(n) then n.PrevInSEL := p;
          // and then reinsert 'second' into list just before 'first' ...
          p := first.PrevInSEL;
          if assigned(p) then p.NextInSEL := second;
          first.PrevInSEL := second;
          second.PrevInSEL := p;
          second.NextInSEL := first;

          second := tmp;
          if not assigned(second) then break;
          dec(rCnt);
        end else
        begin
          first := first.NextInSEL;
          dec(lCnt);
        end;
      end;
      first := base.Jump;
      prevBase := base;
    end;
    if FSel.Jump = nil then Break
    else jumpSize := jumpSize shl 1;
  end;
  Result := FIntersectList.Count > 0;
end;
//------------------------------------------------------------------------------

function TClipper.GetIntersectNode(index: Integer): PIntersectNode;
begin
  Result := PIntersectNode(FIntersectList[index]);
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersectList;
var
  i, j, highI: Integer;
  node: PIntersectNode;
begin
  // We now have a list of intersections required so that edges will be
  // correctly positioned at the top of the scanbeam. However, it's important
  // that edge intersections are processed from the bottom up, but it's also
  // crucial that intersections only occur between adjacent edges.

  // First we do a quicksort so intersections proceed in a bottom up order ...
  FIntersectList.Sort(IntersectListSort);

  // Now as we process these intersections, we must sometimes adjust the order
  // to ensure that intersecting edges are always adjacent ...
  highI := FIntersectList.Count - 1;
  for i := 0 to highI do
  begin
    if not EdgesAdjacentInAEL(FIntersectList[i]) then
    begin
      j := i + 1;
      while not EdgesAdjacentInAEL(FIntersectList[j]) do inc(j);
      // Swap IntersectNodes ...
      node := FIntersectList[i];
      FIntersectList[i] := FIntersectList[j];
      FIntersectList[j] := node;
    end;

    with IntersectNode[i]^ do
    begin
      // Occasionally a non-minima intersection is processed before its own
      // minima. This causes problems with orientation so we need to flag it ...
      if (i < highI) and (IntersectNode[i+1].Pt.Y > Pt.Y) then
          IntersectEdges(Edge1, Edge2, Pt, true) else
          IntersectEdges(Edge1, Edge2, Pt);
      SwapPositionsInAEL(Edge1, Edge2);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  // preconditon: e1 must be immediately to the left of e2
  next := e2.NextInAEL;
  if Assigned(next) then next.PrevInAEL := e1;
  prev := e1.PrevInAEL;
  if Assigned(prev) then prev.NextInAEL := e2;
  e2.PrevInAEL := prev;
  e2.NextInAEL := e1;
  e1.PrevInAEL := e2;
  e1.NextInAEL := next;
  if not Assigned(e2.PrevInAEL) then FActives := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.DoHorizontal(horzEdge: PActive);
var
  e, maxPair: PActive;
  horzLeft, horzRight: Int64;
  isLeftToRight: Boolean;
  pt: TPoint64;
  isMax: Boolean;

  procedure ResetHorzDirection;
  var
    e: PActive;
  begin
    if (horzEdge.Bot.X = horzEdge.Top.X) then
    begin
      // the horizontal edge is going nowhere ...
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.CurrX;
      e := horzEdge.NextInAEL;
      while assigned(e) and (e <> maxPair) do
        e := e.NextInAEL;
      isLeftToRight := assigned(e);
    end
    else if horzEdge.CurrX < horzEdge.Top.X then
    begin
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.Top.X;
      isLeftToRight := true;
    end else
    begin
      horzLeft := horzEdge.Top.X;
      horzRight := horzEdge.CurrX;
      isLeftToRight := false;
    end;
  end;
  //------------------------------------------------------------------------

begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with the bottom vertices of      *
* other HEs [#] and with non-horizontal edges [*]. Once these intersections    *
* are completed, intermediate HEs are 'promoted' to the next edge in their     *
* bounds, and they in turn may be intersected [%] by other HEs.                *
*                                                                              *
* eg: 3 horizontals at a scanline:  /   |                     /          /     *
*              |                   /    |    (HE3) o=========%==========o      *
*              o=======o (HE2)    /     |         /         /                  *
*         o============#=========*======*========#=========o (HE1)             *
*        /             |        /       |       /                              *
*******************************************************************************)

  // with closed paths, simplify consecutive horizontals into a 'single' edge
  if not IsOpen(horzEdge) then
  begin
    pt := horzEdge.Bot;
    while not IsMaxima(horzEdge) and
      (NextVertex(horzEdge).Pt.Y = pt.Y) do
        UpdateEdgeIntoAEL(horzEdge);
    horzEdge.Bot := pt;
    horzEdge.CurrX := pt.X;
    // update Dx in case of direction change ...
    if horzEdge.Bot.X < horzEdge.Top.X then
      horzEdge.Dx := NegInfinity else
      horzEdge.Dx := Infinity;
  end;

  maxPair := nil;
  if IsMaxima(horzEdge) and (not IsOpen(horzEdge) or
      ([vfOpenStart, vfOpenEnd] * horzEdge.vertTop.flags = [])) then
        maxPair := GetMaximaPair(horzEdge);

  ResetHorzDirection;
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, Point64(horzEdge.CurrX, horzEdge.Bot.Y));

  while true do // loops through consec. horizontal edges (if open)
  begin
    isMax := IsMaxima(horzEdge);
    if isLeftToRight  then
      e := horzEdge.NextInAEL else
      e := horzEdge.PrevInAEL;

    while assigned(e) do
    begin
      // Break if we've gone past the end of the horizontal ...
      if (isLeftToRight and (e.CurrX > horzRight)) or
        (not isLeftToRight and (e.CurrX < horzLeft)) then Break;
      // or if we've got to the end of an intermediate horizontal edge ...
      if (E.CurrX = horzEdge.Top.X) and not isMax and not IsHorizontal(e) then
      begin
        pt := NextVertex(horzEdge).Pt;
        if(isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
          (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
      end;

      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge)  then
        begin
          if isLeftToRight then
            AddLocalMaxPoly(horzEdge, e, horzEdge.Top) else
            AddLocalMaxPoly(e, horzEdge, horzEdge.Top);
        end;
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      pt := Point64(e.CurrX, horzEdge.Bot.Y);
      if (isLeftToRight) then
      begin
        IntersectEdges(horzEdge, e, pt);
        SwapPositionsInAEL(horzEdge, e);
        e := horzEdge.NextInAEL;
      end else
      begin
        IntersectEdges(e, horzEdge, pt);
        SwapPositionsInAEL(e, horzEdge);
        e := horzEdge.PrevInAEL;
      end;
    end;

    // check if we've finished with (consecutive) horizontals ...
    if isMax or (NextVertex(horzEdge).Pt.Y <> horzEdge.Top.Y) then Break;

    // still more horizontals in bound to process ...
    UpdateEdgeIntoAEL(horzEdge);
    ResetHorzDirection;

    if IsOpen(horzEdge) then
    begin
      if IsMaxima(horzEdge) then maxPair := GetMaximaPair(horzEdge);
      if IsHotEdge(horzEdge) then AddOutPt(horzEdge, horzEdge.Bot);
    end;
  end;

  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, horzEdge.Top);

  if not IsOpen(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge) // this is the end of an intermediate horiz.
  else if not IsMaxima(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge)
  else if not assigned(maxPair) then // ie open at top
    DeleteFromAEL(horzEdge)
  else if IsHotEdge(horzEdge) then
      AddLocalMaxPoly(horzEdge, maxPair, horzEdge.Top)
  else
  begin
    DeleteFromAEL(maxPair); DeleteFromAEL(horzEdge);
  end;

end;
//------------------------------------------------------------------------------

procedure TClipper.DoTopOfScanbeam(Y: Int64);
var
  e: PActive;
begin
  FSel := nil; // FSel is reused to flag horizontals (see PushHorz below)
  e := FActives;
  while Assigned(e) do
  begin
    // nb: 'e' will never be horizontal here
    if (e.Top.Y = Y) then
    begin
      // the following helps to avoid micro self-intersections
      // with negligible impact on performance ...
      e.CurrX := e.Top.X;
      if assigned(e.PrevInAEL) and (e.PrevInAEL.CurrX = e.CurrX) and
        (e.PrevInAEL.Bot.Y <> Y) and  IsHotEdge(e.PrevInAEL) then
          AddOutPt(e.PrevInAEL, e.Top);
      if assigned(e.NextInAEL) and (e.NextInAEL.CurrX = e.CurrX) and
        (e.NextInAEL.Top.Y <> Y) and IsHotEdge(e.NextInAEL) then
          AddOutPt(e.NextInAEL, e.Top);

      if IsMaxima(e) then
      begin
        e := DoMaxima(e);  // TOP OF BOUND (MAXIMA)
        Continue;
      end else
      begin
        // INTERMEDIATE VERTEX ...
        UpdateEdgeIntoAEL(e);
        if IsHotEdge(e) then AddOutPt(e, e.Bot);
        if IsHorizontal(e) then
          PushHorz(e);     // horizontals are processed later
      end;
    end;
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.DoMaxima(e: PActive): PActive;
var
  eNext, ePrev, eMaxPair: PActive;
begin
  ePrev := e.PrevInAEL;
  eNext := e.NextInAEL;
  Result := eNext;

  if IsOpen(e) and ([vfOpenStart, vfOpenEnd] * e.vertTop.flags <> []) then
  begin
    if IsHotEdge(e) then AddOutPt(e, e.Top);
    if not IsHorizontal(e) then
    begin
      if IsHotEdge(e) then TerminateHotOpen(e);
      DeleteFromAEL(e);
    end;
    Exit;
  end else
  begin
    eMaxPair := GetMaximaPair(e);
    if not assigned(eMaxPair) then Exit; // EMaxPair is a horizontal ...
  end;

  // only non-horizontal maxima here.
  // process any edges between maxima pair ...
  while (eNext <> eMaxPair) do
  begin
    IntersectEdges(e, eNext, e.Top);
    SwapPositionsInAEL(e, eNext);
    eNext := e.NextInAEL;
  end;

  if IsOpen(e) then
  begin
    if IsHotEdge(e) then
    begin
      if assigned(eMaxPair) then
        AddLocalMaxPoly(e, eMaxPair, e.Top) else
        AddOutPt(e, e.Top);
    end;
    if assigned(eMaxPair) then
      DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
      Result := FActives;
    Exit;
  end;

  // here E.NextInAEL == ENext == EMaxPair ...
  if IsHotEdge(e) then
    AddLocalMaxPoly(e, eMaxPair, e.Top);

  DeleteFromAEL(e);
  DeleteFromAEL(eMaxPair);
  if assigned(ePrev) then
    Result := ePrev.NextInAEL else
    Result := FActives;
end;
//------------------------------------------------------------------------------

function TClipper.BuildResult(out closedPaths,
  openPaths: TArrayOfArrayOfFloatPoint): Boolean;
var
  i, j, cntClosed, cntOpen: Integer;
  outRec: TOutRec;
begin
  try
    cntClosed := 0; cntOpen := 0;
    SetLength(closedPaths, FOutRecList.Count);
    SetLength(openPaths, FOutRecList.Count);
    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      if IsOpen(outRec) then
      begin
        openPaths[cntOpen] := BuildPath(outRec.Pts);
        if length(openPaths[cntOpen]) > 1 then inc(cntOpen);
      end else
      begin
        closedPaths[cntClosed] := BuildPath(outRec.Pts);
        j := high(closedPaths[cntClosed]);
        if (j > 1) and PointsEqual(closedPaths[cntClosed][0],
          closedPaths[cntClosed][j]) then
            setlength(closedPaths[cntClosed], j);
        if j > 1 then inc(cntClosed);
      end;
    end;
    SetLength(closedPaths, cntClosed);
    SetLength(openPaths, cntOpen);
    Result := true;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.GetBounds: TFloatRect;
var
  i: Integer;
  v, vStart: PVertex;
begin
  if FVertexList.Count = 0 then
    Result := FloatRect(0, 0, 0, 0)
  else
    with PVertex(FVertexList[0]).Pt do
      Result := FloatRect(X, Y, X, Y);
  for i := 0 to FVertexList.Count -1 do
  begin
    vStart := FVertexList[i];
    v := vStart;
    repeat
      if v.Pt.X < Result.Left then Result.Left := v.Pt.X
      else if v.Pt.X > Result.Right then Result.Right := v.Pt.X;
      if v.Pt.Y < Result.Top then Result.Top := v.Pt.Y
      else if v.Pt.Y > Result.Bottom then Result.Bottom := v.Pt.Y;
      v := v.next;
    until v = vStart;
  end;
end;

//------------------------------------------------------------------------------
//   Miscellaneous ClipperOffset support functions
//------------------------------------------------------------------------------

const
  MinFloat = -3.49E38;
  MaxFloat =  3.49E38;

procedure AppendPath(var paths: TArrayOfArrayOfFloatPoint;
  const extra: TArrayOfFloatPoint);
var
  len: Integer;
begin
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
//------------------------------------------------------------------------------

procedure StripDuplicates(var path: TArrayOfFloatPoint);
var
  i, len: integer;
begin
  len := length(path);
  i := 1;
  while i < len do
  begin
    if (path[i].X = path[i-1].X)  and (path[i].Y = path[i-1].Y) then
    begin
      dec(len);
      if (i < len) then
        Move(path[i+1], path[i], (len-i)*SizeOf(TFloatPoint));
      SetLength(path, len);
    end else
      inc(i);
  end;
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TFloatPoint): TFloat;
begin
  Result := (pt1.X - pt2.X)*(pt1.X - pt2.X) + (pt1.Y - pt2.Y)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TFloatPoint): TFloatPoint;
var
  dx, dy, inverseHypot: Double;
begin
  if PointsEqual(pt1, pt2) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  Result.X := dy;
  Result.Y := -dx
end;

//------------------------------------------------------------------------------
//  TClipperOffset methods
//------------------------------------------------------------------------------

constructor TClipperOffset.Create(MiterLimit: Double; ArcTolerance: Double);
begin
  inherited Create;
  if MiterLimit = 0 then MiterLimit := 2;
  FMiterLimit := MiterLimit;
  FArcTolerance := ArcTolerance;
end;
//------------------------------------------------------------------------------

destructor TClipperOffset.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Clear;
begin
  FPathsIn := nil;
  FNorms := nil;
  FSolution := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPath(const path: TArrayOfFloatPoint);
begin
  if assigned(path) then
    AppendPath(FPathsIn, path);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const paths: TArrayOfArrayOfFloatPoint);
var
  i: Integer;
begin
  for i := 0 to High(paths) do
    AddPath(paths[i]);
end;
//------------------------------------------------------------------------------

function TClipperOffset.GetLowestPolygonIdx: integer;
var
  i,j, len: Integer;
  pt: TFloatPoint;
  p: TArrayOfFloatPoint;
begin
  result := -1;
  pt := FloatPoint(MaxFloat, MinFloat);
  for i := 0 to high(FPathsIn) do
  begin
    if FPathsIn[i] = nil then
      Continue;
    p := FPathsIn[i];
    len := length(p);
    for j := 0 to len -1 do
    begin
      if (p[j].Y < pt.Y) then
        continue;
      if (p[j].Y > pt.Y) or (p[j].X < pt.X) then
      begin
        pt := p[j];
        result := i;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.CheckPaths: boolean;
var
  i,len, minLen: Integer;
  openPaths: Boolean;
begin
  Result := False;
  openPaths := not (FEndType in [etPolygon, etOpenJoined]);
  if openPaths then minLen := 1 else minLen := 3;
  for i := 0 to high(FPathsIn) do
  begin
    StripDuplicates(FPathsIn[i]);
    len := length(FPathsIn[i]);
    if not openPaths and (len > 1) and
      PointsEqual(FPathsIn[i][0], FPathsIn[i][len-1]) then
    begin
      setlength(FPathsIn[i], len -1);
      dec(len);
    end;
    if len < minLen then
      FPathsIn[i] := nil
    else
      Result := True;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPaths;
var
  i, len: Integer;
  arcTol, absDelta, steps: Double;
  tmpEndType: TEndType;
begin
  absDelta := Abs(FDelta);
  len := length(FPathsIn);

  // if a Zero offset, then simply copy paths to FSolution and return ...
  if absDelta < Tolerance then
  begin
    FSolutionLen := 0;
    SetLength(FSolution, len);
    for i := 0 to high(FPathsIn) do
      if assigned(FPathsIn[i]) then
      begin
        FSolution[FSolutionLen] := FPathsIn[i];
        inc(FSolutionLen);
      end;
    SetLength(FSolution, FSolutionLen);
    Exit;
  end;

  // FMiterLimit: see offset_triginometry3.svg
  if FMiterLimit > 1 then FMiterLim := 2/(sqr(FMiterLimit))
  else FMiterLim := 2;

  if (FArcTolerance <= DefaultArcFrac) then
    arcTol := DefaultArcFrac else
    arcTol := FArcTolerance;

  if (FJoinType in [jtRound, jtRoundEx]) or (FEndType = etOpenRound) then
  begin
    // get steps per 360 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / absDelta);

    // avoid excessive precision ...
    if (steps > absDelta * Pi) then steps := absDelta * Pi;
    FStepsPerRad := steps / Two_Pi;
    Math.SinCos(Two_Pi / steps, FStepSizeSin, FStepSizeCos);
    if FDelta < 0 then FStepSizeSin := -FStepSizeSin;
  end;

  if (FEndType = etOpenJoined) then
    SetLength(FSolution, len *2) else
    SetLength(FSolution, len);

  FSolutionLen := 0;
  for i := 0 to len -1 do
  begin
    FPathIn := FPathsIn[i];
    if FPathIn = nil then Continue;

    FPathOutLen := 0;
    FPathOut := nil;

    if Length(FPathIn) = 1 then
    begin
      // a simple workaround using OffsetOpenPath to construct
      // either a circle or a square point offset ...
      tmpEndType := FEndType;
      if FEndType = etOpenButt then FEndType := etOpenSquare;
      SetLength(FPathIn, 2);
      FPathIn[1] := FPathIn[0];
      SetLength(FNorms, 2);
      FNorms[0] := FloatPoint(1,0);
      OffsetOpenPath;
      FEndType := tmpEndType;
    end else
    begin
      BuildNormals;
      if FEndType = etPolygon then
        OffsetPolygon
      else if FEndType = etOpenJoined then
        OffsetOpenJoined
      else
        OffsetOpenPath;
    end;

    if FPathOutLen = 0 then Continue;

    SetLength(FPathOut, FPathOutLen);
    FSolution[FSolutionLen] := FPathOut;
    Inc(FSolutionLen);
  end;
  SetLength(FSolution, FSolutionLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.BuildNormals;
var
  i, len: integer;
begin
  len := Length(FPathIn);
  SetLength(FNorms, len);
  for i := 0 to len-2 do
    FNorms[i] := GetUnitNormal(FPathIn[i], FPathIn[i+1]);
  FNorms[len -1] := GetUnitNormal(FPathIn[len -1], FPathIn[0]);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.ReverseNormals;
var
  i, highI: integer;
  tmp: TFloatPoint;
begin
  FNorms := ReversePath(FNorms);
  highI := high(FNorms);
  tmp := FNorms[0];
  for i := 1 to highI  do
  begin
    FNorms[i-1].X := -FNorms[i].X;
    FNorms[i-1].Y := -FNorms[i].Y;
  end;
  FNorms[highI].X := -tmp.X;
  FNorms[highI].Y := -tmp.Y;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPolygon;
var
  i,j: integer;
begin
  j := high(FPathIn);
  for i := 0 to high(FPathIn) do
  begin
    OffsetPoint(i, j);
    j := i;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenJoined;
begin
  OffsetPolygon;
  FPathIn := ReversePath(FPathIn);

  SetLength(FPathOut, FPathOutLen);
  FSolution[FSolutionLen] := FPathOut;
  Inc(FSolutionLen);
  FPathOutLen := 0;
  FPathOut := nil;

  ReverseNormals;
  OffsetPolygon;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenPath;

  procedure DoButtCap(highI: integer);
  begin
    AddPoint(FloatPoint(FPathIn[highI].X + FNorms[highI-1].X *FDelta,
      FPathIn[highI].Y + FNorms[highI-1].Y * FDelta));
    AddPoint(FloatPoint(FPathIn[highI].X - FNorms[highI-1].X *FDelta,
      FPathIn[highI].Y - FNorms[highI-1].Y * FDelta));
  end;

  procedure DoSquareCap(highI: integer; toStart: Boolean);
  var
    pt: TFloatPoint;
  const
    sc: array[boolean] of integer = (1, -1);
  begin
    pt := FloatPoint(FPathIn[highI].X + FNorms[highI-1].X *FDelta,
      FPathIn[highI].Y + FNorms[highI-1].Y * FDelta);
    AddPoint(pt);
    AddPoint(FloatPoint(pt.X - FNorms[highI-1].Y *FDelta,
      pt.Y - FNorms[highI-1].X * FDelta * sc[true]));
    pt := FloatPoint(FPathIn[highI].X - FNorms[highI-1].X *FDelta,
      FPathIn[highI].Y - FNorms[highI-1].Y * FDelta);
    AddPoint(FloatPoint(pt.X - FNorms[highI-1].Y *FDelta,
      pt.Y - FNorms[highI-1].X * FDelta * sc[true]));
    AddPoint(pt);
  end;

  procedure DoRoundCap(highI: integer); // 180 degrees
  var
    i: integer;
    steps: Integer;
    pt: TFloatPoint;
  begin
    steps := Round(FStepsPerRad * PI);
    pt.X := FNorms[highI-1].X * FDelta;
    pt.Y := FNorms[highI-1].Y * FDelta;
    for i := 1 to steps do
    begin
      AddPoint(FloatPoint(FPathIn[highI].X + pt.X, FPathIn[highI].Y + pt.Y));
      pt := FloatPoint(pt.X * FStepSizeCos - FStepSizeSin * pt.Y,
        pt.X * FStepSizeSin + pt.Y * FStepSizeCos);
    end;
  end;

var
  i,j, highI: integer;
begin
  highI := high(FPathIn);
  j := 0;
  for i := 1 to highI -1 do
  begin
    OffsetPoint(i, j);
    j := i;
  end;

  // cap the end first ...
  case FEndType of
    etOpenButt: DoButtCap(highI);
    etOpenRound: DoRoundCap(highI);
    else DoSquareCap(highI, false);
  end;

  FPathIn := ReversePath(FPathIn);
  ReverseNormals;
  j := 0;
  for i := 0 to highI -1 do
  begin
    OffsetPoint(i, j);
    j := i;
  end;

  // now cap the start ...
  case FEndType of
    etOpenButt: DoButtCap(highI);
    etOpenRound: DoRoundCap(highI);
    else DoSquareCap(highI, true);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double; jt: TJoinType; et: TEndType;
      out solution: TArrayOfArrayOfFloatPoint);
var
  negate: Boolean;
  lowestIdx: integer;
begin
  solution := nil;
  if length(FPathsIn) = 0 then Exit;
  FJoinType := jt;
  FEndType := et;

  if (not CheckPaths) then
    exit;

  negate := false;
  if (et = etPolygon) then
  begin
    // the lowermost polygon must be an outer polygon. So we can use that as the
    // designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx;
    negate := (Area(FPathsIn[lowestIdx]) < 0);
    // if polygon orientations are reversed, then 'negate' ...
    // if negate then FDelta := FDelta;
  end;

  if FEndType <> etPolygon then
    FDelta := Abs(delta) else
    FDelta := delta;
  OffsetPaths;

//  solution := FSolution;

  // clean up self-intersections ...
  with TClipper.Create do
  try
    AddPaths(FSolution, ptSubject);
    if negate then
      Execute(ctUnion, frNegative, solution) else
      Execute(ctUnion, frPositive, solution);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(const pt: TFloatPoint);
const
  BuffLength = 32;
begin
  if FPathOutLen = length(FPathOut) then
    SetLength(FPathOut, FPathOutLen + BuffLength);
  if (FPathOutLen > 0) and PointsEqual(FPathOut[FPathOutLen-1], pt) then Exit;
  FPathOut[FPathOutLen] := pt;
  Inc(FPathOutLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
begin
  // Two vertices, one using the prior offset's (k) normal one the current (j).
  // Do a 'normal' offset (by delta) and then another by 'de-normaling' the
  // normal hence parallel to the direction of the respective edges.
  if FDelta > 0 then
  begin
    AddPoint(FloatPoint(
      FPathIn[j].X + FDelta * (FNorms[k].X - FNorms[k].Y),
      FPathIn[j].Y + FDelta * (FNorms[k].Y + FNorms[k].X)));
    AddPoint(FloatPoint(
      FPathIn[j].X + FDelta * (FNorms[j].X + FNorms[j].Y),
      FPathIn[j].Y + FDelta * (FNorms[j].Y - FNorms[j].X)));
  end else
  begin
    AddPoint(FloatPoint(
      FPathIn[j].X + FDelta * (FNorms[k].X + FNorms[k].Y),
      FPathIn[j].Y + FDelta * (FNorms[k].Y - FNorms[k].X)));
    AddPoint(FloatPoint(
      FPathIn[j].X + FDelta * (FNorms[j].X - FNorms[j].Y),
      FPathIn[j].Y + FDelta * (FNorms[j].Y + FNorms[j].X)));
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosAplus1: Double);
var
  q: Double;
begin
  // see offset_triginometry4.svg
  q := FDelta / cosAplus1; // 0 < cosAplus1 <= 2
  AddPoint(FloatPoint(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*q,
    FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*q));
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoRound(j, k: Integer);
var
  i, m,n, steps: Integer;
  a, delta, sinA, cosA: Double;
  pt, pt2, pt3: TFloatPoint;
begin
  sinA := FNorms[k].X * FNorms[j].Y - FNorms[k].Y * FNorms[j].X;
  cosA := FNorms[j].X * FNorms[k].X + FNorms[j].Y * FNorms[k].Y;
  a := ArcTan2(sinA, cosA);
  steps := Round(FStepsPerRad * Abs(a));

  if (FDelta * sinA < 0) then // ie concave
  begin
    a := FDelta / (cosA +1);
    if (j = 0) then m := high(FPathIn) else m := j -1;
    if j = high(FPathIn) then n := 0 else n := j +1;

    // offset pt of concave vertex ...
    pt.X := round(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*a);
    pt.Y := round(FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*a);

    a := Min(DistanceSqr(FPathIn[m], FPathIn[j]),
      DistanceSqr(FPathIn[n], FPathIn[j]));

    // there's no space to draw anything ...
    if DistanceSqr(pt, FPathIn[j]) > a then
    begin
      // get the perpendicular offsets from pt2 ...
      // this creates a self-intersection that'll be clipped later
      pt2.X := round(FPathIn[j].X + FNorms[k].X * FDelta);
      pt2.Y := round(FPathIn[j].Y + FNorms[k].Y * FDelta);
      pt3.X := round(FPathIn[j].X + FNorms[j].X * FDelta);
      pt3.Y := round(FPathIn[j].Y + FNorms[j].Y * FDelta);
      AddPoint(pt2);
      AddPoint(pt3);
      Exit;
    end;

    a := Sqrt(a);
    // get the point on each edge being the distance of the shortest edge
    // from the concave vertex. (nb: unit normals to unit vectors here)
    pt2.X := round(FPathIn[j].X + FNorms[k].Y * a);
    pt2.Y := round(FPathIn[j].Y - FNorms[k].X * a);
    pt3.X := round(FPathIn[j].X - FNorms[j].Y * a);
    pt3.Y := round(FPathIn[j].Y + FNorms[j].X * a);

    // now FDelta offset these points ...
    pt2.X := round(pt2.X + FNorms[k].X * FDelta);
    pt2.Y := round(pt2.Y + FNorms[k].Y * FDelta);
    pt3.X := round(pt3.X + FNorms[j].X * FDelta);
    pt3.Y := round(pt3.Y + FNorms[j].Y * FDelta);

    if DistanceSqr(pt2, pt3) < Sqr(FDelta *2/MiterLimit) then
      delta := Sqrt(DistanceSqr(pt2, pt3))/2 else
      delta := FDelta/MiterLimit;

    a := (delta + FDelta) / (cosA +1);
    pt.X := round(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*a);
    pt.Y := round(FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*a);

    pt2.X := -FNorms[k].X * delta;
    pt2.Y := -FNorms[k].Y * delta;
    AddPoint(FloatPoint(pt.X + pt2.X, pt.Y + pt2.Y));
    for i := 1 to steps -1 do
    begin
      pt2 := FloatPoint(pt2.X * FStepSizeCos + FStepSizeSin * pt2.Y,
        -pt2.X * FStepSizeSin + pt2.Y * FStepSizeCos);
      AddPoint(FloatPoint(pt.X + pt2.X, pt.Y + pt2.Y));
    end;
  end else
  begin
    // a convex vertex ...
    pt := FPathIn[j];
    pt2.X := FNorms[k].X * FDelta;
    pt2.Y := FNorms[k].Y * FDelta;
    AddPoint(FloatPoint(pt.X + pt2.X, pt.Y + pt2.Y));
    for i := 1 to steps -1 do
    begin
      pt2 := FloatPoint(pt2.X * FStepSizeCos - FStepSizeSin * pt2.Y,
        pt2.X * FStepSizeSin + pt2.Y * FStepSizeCos);
      AddPoint(FloatPoint(pt.X + pt2.X, pt.Y + pt2.Y));
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j,k: Integer);
var
  sinA, cosA: Double;
begin
  // A: angle between adjoining edges (on left side WRT winding direction).
  // A == 0 deg (or A == 360 deg): collinear edges heading in same direction
  // A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
  // sin(A) < 0: convex on left.
  // cos(A) > 0: angles on both left and right sides > 90 degrees
  sinA := (FNorms[k].X * FNorms[j].Y - FNorms[j].X * FNorms[k].Y);
  cosA := (FNorms[j].X * FNorms[k].X + FNorms[j].Y * FNorms[k].Y);

  if (Abs(sinA * FDelta) < 1.0) then // angle is close to 0 or 180 deg.
  begin
    if (cosA > 0) then // given condition above the angle is approaching 0 deg.
    begin
      if FJoinType = jtRoundEx then
        DoRound(j, k)
      else
      // with angles approaching 0 deg collinear (whether concave or convex),
      // offsetting with two or more vertices (that would be so close together)
      // occasionally causes tiny self-intersections due to rounding.
      // So we offset with just a single vertex here ...
      AddPoint(FloatPoint(FPathIn[j].X + FNorms[k].X * FDelta,
        FPathIn[j].Y + FNorms[k].Y * FDelta));
      Exit;
    end;
    // else angle must be approaching 180 deg.
  end
  else if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  if (FJoinType = jtRoundEx) then
  begin
    DoRound(j, k)
  end
  else if sinA * FDelta < 0 then // ie a concave offset
  begin
    AddPoint(FloatPoint(FPathIn[j].X + FNorms[k].X * FDelta,
      FPathIn[j].Y + FNorms[k].Y * FDelta));
    AddPoint(FPathIn[j]); // this improves clipping removal later
    AddPoint(FloatPoint(FPathIn[j].X + FNorms[j].X * FDelta,
      FPathIn[j].Y + FNorms[j].Y * FDelta));
  end
  else
  begin
    // convex offsets here ...
    case FJoinType of
      jtMiter:
        // see offset_triginometry3.svg
        if (1 + cosA < FMiterLim) then DoSquare(j, k)
        else DoMiter(j, k, 1 + cosA);
      jtSquare:
        // angles >= 90 deg. don't need squaring
        if cosA >= 0 then
          DoMiter(j, k, 1 + cosA) else
          DoSquare(j, k);

      else DoRound(j, k);
    end;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePaths(const paths: TArrayOfArrayOfFloatPoint;
  delta: Double; jt: TJoinType; et: TEndType;
  miterLimit: single): TArrayOfArrayOfFloatPoint;
begin
  with TClipperOffset.Create(miterLimit) do
  try
    AddPaths(paths);
    Execute(delta, jt, et, Result);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

end.

