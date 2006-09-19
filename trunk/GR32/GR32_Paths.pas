unit GR32_Paths;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Windows, Classes, Math, GR32, GR32_Polygons, GR32_Resamplers, GR32_Transforms;

type
  { TCustomShape }
  TCustomShape = class(TPersistent)
  private
{     procedure SetPoint(Index: Integer; Value: TFloatPoint);
    function GetEndPoint: PFloatPoint;
    function GetStartPoint: PFloatPoint;
    function GetPoint(Index: Integer): TFloatPoint; }
    //function GetPointPtr(Index: Integer): PFloatPoint;
  protected
{     function GetPointPtr(Index: Integer): PFloatPoint; virtual; abstract;
    function GetNumPoints: Integer; virtual; abstract; }
    procedure AssignTo(Dest: TPersistent); override;
  public
    function AsPolygon: TPolygon32;
    //function FindClosestPoint(const P: TFloatPoint): PFloatPoint;
    //function ClosestPointOnCurve(const P: TFloatPoint): TFloatPoint; virtual; abstract;
    procedure AppendToPolygon(Polygon: TPolygon32); virtual; abstract;
{    procedure AddPoint(const P: TFloatPoint); virtual; abstract;
    procedure RemovePoint(Index: Integer); virtual; abstract;
    property StartPoint: PFloatPoint read GetStartPoint;
    property EndPoint: PFloatPoint read GetEndPoint;
    property PointPtr[Index: Integer]: PFloatPoint read GetPointPtr;
    property Point[Index: Integer]: TFloatPoint read GetPoint write SetPoint;
    property NumPoints: Integer read GetNumPoints; }
  end;


  { TCustomCurve }
  TCustomCurve = class(TCustomShape)
  public
    procedure Delete; virtual;
  end;

  
  { TBezierCurve }

  PBezierVertex = ^TBezierVertex;
  TBezierVertex = record
    case Integer of
      0: (Point: TFloatPoint; ControlPoints: array [0..1] of TFloatPoint);
      1: (Points: array [0..2] of TFloatPoint);
  end;

  TBezierSegment = record
    P1, P2, P3, P4: TFloatPoint;
  end;

  TArrayOfBezierVertex = array of TBezierVertex;
  TArrayOfArrayOfBezierVertex = array of TArrayOfBezierVertex;

  TBezierCurve = class(TCustomCurve)
  private
    FVertices: TArrayOfArrayOfBezierVertex;
{   protected
    function GetPointPtr(Index: Integer): PFloatPoint; override;
    function GetNumPoints: Integer; override;
    procedure AssignTo(Dest: TPersistent); override; }
  public
    constructor Create;
    procedure Delete; override;
    procedure NewContour;
{     procedure AddPoint(const P: TFloatPoint); override;
    procedure RemovePoint(Index: Integer); override; }
    procedure AddVertex(const V: TBezierVertex);
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    //function ClosestPointOnCurve(const P: TFloatPoint): TFloatPoint; override;
    property Vertices: TArrayOfArrayOfBezierVertex read FVertices write FVertices;
  end;

  { TSplineCurve }
  TSplineCurve = class(TCustomShape)
  private
    FPoints: TArrayOfArrayOfFloatPoint;
    FKernel: TCustomKernel;
    procedure SetKernel(const Value: TCustomKernel);
  public
    constructor Create;
    procedure NewContour;
    procedure AddPoint(const Point: TFloatPoint);
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    property Points: TArrayOfArrayOfFloatPoint read FPoints write FPoints;
    property Kernel: TCustomKernel read FKernel write SetKernel;
  end;

  { TEllipse }
  TEllipse = class(TCustomShape)
  private
    FBoundsRect: TFloatRect;
  public
    procedure AppendToPolygon(Polygon: TPolygon32); override;
    property BoundsRect: TFloatRect read FBoundsRect write FBoundsRect;
  end;

{   TRegularPolygon = class(TCustomShape)
  public
  public
    property Corners: Integer;
    property BoundsRect: TFloatRect;
  end; }

(*
  TPolyLine = class(TCustomShape)
  private
    FPoints: TArrayOfFloatPoint;
  protected
    constructor Create;
    function GetPointPtr(Index: Integer): PFloatPoint; override;
    function GetNumPoints: Integer; override;
  public
    procedure AddPoint(const P: TFloatPoint); override;
    procedure RemovePoint(Index: Integer); override;
    procedure AppendToPolygon(Polygon: TPolygon32); override;
  end;
*)

  // TParametricCurve = class(TCustomShape)
  // end;
//============================================================================//

(*
  TCustomSegment = class(TPersistent)
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); virtual; abstract;
    function FindClosestPoint(const P1, P2, P: TFloatPoint): TFloatPoint; virtual; abstract;
  end;

  TLineSegment = class(TCustomSegment)
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); override;
    function FindClosestPoint(const P1, P2, P: TFloatPoint): TFloatPoint; override;
  end;

  PControlPoints = ^TControlPoints;
  TControlPoints = array [0..1] of TFloatPoint;

  TBezierSegment = class(TCustomSegment)
  private
    FControlPoints: TControlPoints;
    function GetControlPoints: PControlPoints;
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); override;
    function FindClosestPoint(const P1, P2, P: TFloatPoint): TFloatPoint; override;
  public
    constructor Create; overload;
    constructor Create(const C1, C2: TFloatPoint); overload;
    function GetCoefficient(const P1, P2, P: TFloatPoint): TFloat;
    procedure CurveThroughPoint(Path: TCustomPath; Index: Integer; P: TFloatPoint;
      Coeff: TFloat);
    property ControlPoints: PControlPoints read GetControlPoints;
  end;

  TSegmentList = class(TList)
  private
    function GetCustomSegment(Index: Integer): TCustomSegment;
    procedure SetCustomSegment(Index: Integer;
      const Value: TCustomSegment);
  public
    property Items[Index: Integer]: TCustomSegment read GetCustomSegment write SetCustomSegment; default;
  end;

  TCustomPath = class(TPersistent)
  private
    FVertices: TArrayOfFloatPoint;
    FOutput: TArrayOfFloatPoint;
    FSegmentList: TSegmentList;
    FVertCount: Integer;
    FOutCount: Integer;
    function GetOutputCapacity: Integer;
    function GetVertexCapacity: Integer;
    procedure SetOutputCapacity(const Value: Integer);
    procedure SetVertexCapacity(const Value: Integer);
    function GetEndPoint: TFloatPoint;
    function GetStartPoint: TFloatPoint;
    procedure SetEndPoint(const Value: TFloatPoint);
    procedure SetStartPoint(const Value: TFloatPoint);
    function GetSegment(Index: Integer): TCustomSegment;
    function GetVertex(Index: Integer): TFloatPoint;
    function GetLastSegment: TCustomSegment;
    function GetSegmentCount: Integer;
  protected
    procedure AppendPoint(const Point: TFloatPoint);
  public
    constructor Create;
    destructor Destroy; override;
    function AsPolygon: TPolygon32;
    function FindClosestVertex(const P: TFloatPoint): PFloatPoint;
    function FindClosestSegment(const P: TFloatPoint;
      out Segment: TCustomSegment; out OutPoint: TFloatPoint): Integer;
    function GetCoefficient(const P: TFloatPoint; Index: Integer): TFloat;
    procedure CurveThroughPoint(const P: TFloatPoint; Index: Integer; Coeff: TFloat);
    procedure AddStartPoint(const P: TFloatPoint);
    procedure AddSegment(const P: TFloatPoint; Segment: TCustomSegment);
    procedure InsertSegment(const P: TFloatPoint; Segment: TCustomSegment; Index: Integer);
    procedure DeleteSegment(Index: Integer);
    procedure RemoveSegment(Segment: TCustomSegment);
    function IndexOf(Segment: TCustomSegment): Integer;
    procedure Offset(const Dx, Dy: TFloat);
    property VertexCapacity: Integer read GetVertexCapacity write SetVertexCapacity;
    property OutputCapacity: Integer read GetOutputCapacity write SetOutputCapacity;
    property StartPoint: TFloatPoint read GetStartPoint write SetStartPoint;
    property EndPoint: TFloatPoint read GetEndPoint write SetEndPoint;
    property Segments[Index: Integer]: TCustomSegment read GetSegment;
    property Vertices[Index: Integer]: TFloatPoint read GetVertex;
    property VertexCount: Integer read FVertCount;
    property SegmentCount: Integer read GetSegmentCount;
    property LastSegment: TCustomSegment read GetLastSegment;
  end;
*)

const
  Epsilon = 1;

var
  BezierTolerance: Single = 0.25;

function SqrDistance(const A, B: TFloatPoint): TFloat;
function DotProduct(const A, B: TFloatPoint): TFloat;
function AddPoints(const A, B: TFloatPoint): TFloatPoint;
function SubPoints(const A, B: TFloatPoint): TFloatPoint;

{ Bezier curve routines }
function BezierCurveToPolygonX(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFixedPoint; overload;
function BezierCurveToPolygonF(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFloatPoint; overload;

{ Text routines }
procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor: TColor32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation = nil); overload;
procedure DrawTextOutline(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Color: TColor32; Transformation: TTransformation = nil);
procedure FillText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Filler: TCustomPolygonFiller; Transformation: TTransformation = nil);
function TextToPolygon(Dst: TBitmap32; X, Y: TFloat; const Text: WideString): TPolygon32;


// function EllipseToPolygon

implementation

uses
  SysUtils, GR32_LowLevel;

function AddPoints(const A, B: TFloatPoint): TFloatPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function SubPoints(const A, B: TFloatPoint): TFloatPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

// Returns square of the distance between points A and B.
function SqrDistance(const A, B: TFloatPoint): TFloat;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function DotProduct(const A, B: TFloatPoint): TFloat;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

// Returns a point on the line from A to B perpendicular to C.
function PointOnLine(const A, B, C: TFloatPoint): TFloatPoint;
var
  dx, dy, r: Single;
begin
  dx := B.X - A.X;
  dy := B.Y - A.Y;
  r := ((C.X - A.X) * dx + (C.Y - A.Y) * dy) / (Sqr(dx) + Sqr(dy));
  if InRange(r, 0, 1) then
  begin
    Result.X := A.X + r * dx;
    Result.Y := A.Y + r * dy;
  end
  else
  begin
    if SqrDistance(A, C) < SqrDistance(B, C) then
      Result := A
    else
      Result := B;
  end;
end;

function Flatness(P1, P2, P3, P4: TFloatPoint): Single;
begin
  Result :=
    Abs(P1.X + P3.X - 2*P2.X) +
    Abs(P1.Y + P3.Y - 2*P2.Y) +
    Abs(P2.X + P4.X - 2*P3.X) +
    Abs(P2.Y + P4.Y - 2*P3.Y);
end;


procedure OffsetVertex(var Vertex: TBezierVertex; const Dx, Dy: TFloat);
begin
  Vertex.Points[0].X := Vertex.Points[0].X + Dx;
  Vertex.Points[0].Y := Vertex.Points[0].Y + Dy;
  Vertex.Points[1].X := Vertex.Points[1].X + Dx;
  Vertex.Points[1].Y := Vertex.Points[1].Y + Dy;
  Vertex.Points[2].X := Vertex.Points[2].X + Dx;
  Vertex.Points[2].Y := Vertex.Points[2].Y + Dy;
end;


function ZeroVertex(const Point: TFloatPoint): TBezierVertex;
begin
  Result.Points[0] := Point;
  Result.Points[1] := Point;
  Result.Points[2] := Point;
end;

function BezierSegment(const Vertices: TArrayOfBezierVertex; Index: Integer): TBezierSegment;
begin
  if Index = High(Vertices) then
  begin
    Result.P1 := Vertices[Index].Points[0];
    Result.P2 := Vertices[Index].Points[2];
    Result.P3 := Vertices[0].Points[1];
    Result.P4 := Vertices[0].Points[0];
  end
  else
  begin
    Result.P1 := Vertices[Index].Points[0];
    Result.P2 := Vertices[Index].Points[2];
    Result.P3 := Vertices[Index + 1].Points[1];
    Result.P4 := Vertices[Index + 1].Points[0];
  end;
end;
(*

{ TLineSegment }

procedure TLineSegment.BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint);
begin
  Path.AppendPoint(P2);
end;

function TLineSegment.FindClosestPoint(const P1, P2, P: TFloatPoint): TFloatPoint;
begin
  Result := PointOnLine(P1, P2, P);
end;

{ TBezierSegment }

procedure TBezierSegment.BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint);
var
  C1, C2: TFloatPoint;

  procedure Recurse(const P1, P2, P3, P4: TFloatPoint);
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      Path.AppendPoint(P4);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 1/2;
      P12.Y   := (P1.Y + P2.Y) * 1/2;
      P23.X   := (P2.X + P3.X) * 1/2;
      P23.Y   := (P2.Y + P3.Y) * 1/2;
      P34.X   := (P3.X + P4.X) * 1/2;
      P34.Y   := (P3.Y + P4.Y) * 1/2;
      P123.X  := (P12.X + P23.X) * 1/2;
      P123.Y  := (P12.Y + P23.Y) * 1/2;
      P234.X  := (P23.X + P34.X) * 1/2;
      P234.Y  := (P23.Y + P34.Y) * 1/2;
      P1234.X := (P123.X + P234.X) * 1/2;
      P1234.Y := (P123.Y + P234.Y) * 1/2;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  C1 := AddPoints(FControlPoints[0], P1);
  C2 := AddPointS(FControlPoints[1], P2);
  Recurse(P1, C1, C2, P2);
end;

constructor TBezierSegment.Create(const C1, C2: TFloatPoint);
begin
  FControlPoints[0] := C1;
  FControlPoints[1] := C2;
end;

constructor TBezierSegment.Create;
begin
  //
end;

function TBezierSegment.FindClosestPoint(const P1, P2, P: TFloatPoint): TFloatPoint;
var
  C1, C2: TFloatPoint;

  function FindPoint(const P1, P2, P3, P4: TFloatPoint; D1, D2: TFloat): TFloatPoint;
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
    NewD: TFloat;
  begin
    P12.X   := (P1.X + P2.X) * 1/2;
    P12.Y   := (P1.Y + P2.Y) * 1/2;
    P23.X   := (P2.X + P3.X) * 1/2;
    P23.Y   := (P2.Y + P3.Y) * 1/2;
    P34.X   := (P3.X + P4.X) * 1/2;
    P34.Y   := (P3.Y + P4.Y) * 1/2;
    P123.X  := (P12.X + P23.X) * 1/2;
    P123.Y  := (P12.Y + P23.Y) * 1/2;
    P234.X  := (P23.X + P34.X) * 1/2;
    P234.Y  := (P23.Y + P34.Y) * 1/2;
    P1234.X := (P123.X + P234.X) * 1/2;
    P1234.Y := (P123.Y + P234.Y) * 1/2;

    NewD := SqrDistance(P, P1234);
    if (NewD < D1) and (NewD < D2) then
    begin
      P12 := FindPoint(P1, P12, P123, P1234, D1, NewD);
      P23 := FindPoint(P1234, P234, P34, P4, NewD, D2);
      if SqrDistance(P, P12) < SqrDistance(P, P23) then
        Result := P12
      else
        Result := P23;
    end
    else
      case CompareValue(D1, D2, Epsilon) of
       -1: Result := FindPoint(P1, P12, P123, P1234, D1, NewD);
        1: Result := FindPoint(P1234, P234, P34, P4, NewD, D2);
        0: Result := P1234;
      end;
  end;

begin
  C1 := AddPoints(FControlPoints[0], P1);
  C2 := AddPointS(FControlPoints[1], P2);
  Result := FindPoint(P1, C1, C2, P2, Infinity, Infinity);
end;

procedure TBezierSegment.CurveThroughPoint(Path: TCustomPath; Index: Integer;
  P: TFloatPoint; Coeff: TFloat);
var
  P1, P2, C1, C2, D1, D2: TFloatPoint;
  t, t3, s, s3, w, a, ax, ay, a1, a2: TFloat;
begin
  P1 := Path.Vertices[Index];
  P2 := Path.Vertices[Index + 1];
  C1 := FControlPoints[0];
  C2 := FControlPoints[1];

  t := Coeff;
  t3 := t * t * t;

  s := 1 - t;
  s3 := s * s * s;

  w := (P.X - s3 * P1.X - t3 * P2.X) / (3 * s * t);
  ax := ((w - P1.X - t * (P2.X - P1.X)) / (C1.X + t * (C2.X - C1.X)));

  w := (P.Y - s3 * P1.Y - t3 * P2.Y) / (3 * s * t);
  ay := ((w - P1.Y - t * (P2.Y - P1.Y)) / (C1.Y + t * (C2.Y - C1.Y)));

  C1.X := ax * C1.X;
  C1.Y := ay * C1.Y;
                                                 
  C2.X := ax * C2.X;
  C2.Y := ay * C2.Y;

  FControlPoints[0] := C1;
  FControlPoints[1] := C2;

  if (Index > 0) and (Path.Segments[Index - 1] is TBezierSegment) then
  begin
    D1 := TBezierSegment(Path.Segments[Index - 1]).ControlPoints[1];
    a1 := Sqrt(Sqr(D1.X) + Sqr(D1.Y));

    D1.X := ax * D1.X;
    D1.Y := ay * D1.Y;

    a := Sqrt(Sqr(D1.X) + Sqr(D1.Y));
    D1.X := a1 / a * D1.X;
    D1.Y := a1 / a * D1.Y;
    (Path.Segments[Index - 1] as TBezierSegment).ControlPoints[1] := D1;
  end;

  if (Index < Path.SegmentCount - 1) and (Path.Segments[Index + 1] is TBezierSegment) then
  begin
    D2 := TBezierSegment(Path.Segments[Index + 1]).ControlPoints[0];
    a2 := Sqrt(Sqr(D2.X) + Sqr(D2.Y));

    D2.X := ax * D2.X;
    D2.Y := ay * D2.Y;

    a := Sqrt(Sqr(D2.X) + Sqr(D2.Y));
    D2.X := a2 / a * D2.X;
    D2.Y := a2 / a * D2.Y;
    (Path.Segments[Index + 1] as TBezierSegment).ControlPoints[0] := D2;
  end;
end;


function TBezierSegment.GetControlPoints: PControlPoints;
begin
  Result := @FControlPoints;
end;

function TBezierSegment.GetCoefficient(const P1, P2, P: TFloatPoint): TFloat;
var
  C1, C2, OutPoint: TFloatPoint;

  function FindPoint(const P1, P2, P3, P4: TFloatPoint; D1, D2, Delta, Pos: TFloat; out POut: TFloatPoint): TFloat;
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
    R1, R2, NewD: TFloat;
  begin
    P12.X   := (P1.X + P2.X) * 1/2;
    P12.Y   := (P1.Y + P2.Y) * 1/2;
    P23.X   := (P2.X + P3.X) * 1/2;
    P23.Y   := (P2.Y + P3.Y) * 1/2;
    P34.X   := (P3.X + P4.X) * 1/2;
    P34.Y   := (P3.Y + P4.Y) * 1/2;
    P123.X  := (P12.X + P23.X) * 1/2;
    P123.Y  := (P12.Y + P23.Y) * 1/2;
    P234.X  := (P23.X + P34.X) * 1/2;
    P234.Y  := (P23.Y + P34.Y) * 1/2;
    P1234.X := (P123.X + P234.X) * 1/2;
    P1234.Y := (P123.Y + P234.Y) * 1/2;

    Delta := Delta * 1/2;
    NewD := SqrDistance(P, P1234);
    if (NewD < D1) and (NewD < D2) then
    begin
      R1 := FindPoint(P1, P12, P123, P1234, D1, NewD, Delta, Pos - Delta, P12);
      R2 := FindPoint(P1234, P234, P34, P4, NewD, D2, Delta, Pos + Delta, P23);
      if SqrDistance(P, P12) < SqrDistance(P, P23) then
      begin
        POut := P12;
        Result := R1;
      end
      else
      begin
        POut := P23;
        Result := R2;
      end;
    end
    else
      case CompareValue(D1, D2, Epsilon) of
       -1: Result := FindPoint(P1, P12, P123, P1234, D1, NewD, Delta, Pos - Delta, POut);
        1: Result := FindPoint(P1234, P234, P34, P4, NewD, D2, Delta, Pos + Delta, POut);
        0:
          begin
            POut := P1234;
            Result := Pos;
          end;
      end;
  end;

begin
  C1 := AddPoints(P1, FControlPoints[0]);
  C2 := AddPoints(P2, FControlPoints[1]);
  Result := FindPoint(P1, C1, C2, P2, Infinity, Infinity, 0.5, 0.5, OutPoint);
end;

{ TCustomPath }

procedure TCustomPath.AddSegment(const P: TFloatPoint; Segment: TCustomSegment);
begin
  FSegmentList.Add(Segment);
  if High(FVertices) < FVertCount then
    SetLength(FVertices, Length(FVertices) * 2);

  FVertices[FVertCount] := P;
  Inc(FVertCount);
end;

procedure TCustomPath.AddStartPoint(const P: TFloatPoint);
begin
  FVertices[0] := P;
  Inc(FVertCount);
end;

procedure TCustomPath.AppendPoint(const Point: TFloatPoint);
begin
  if High(FOutput) < FOutCount then
    SetLength(FOutput, Length(FOutput) * 2);
  FOutput[FOutCount] := Point;
  Inc(FOutCount);
end;

function TCustomPath.AsPolygon: TPolygon32;
var
  I: Integer;
  FixedPoints: TArrayOfFixedPoint;
begin
  FOutput := nil;
  SetLength(FOutput, 4);
  FOutCount := 0;
  
  AppendPoint(StartPoint);
  for I := 0 to FSegmentList.Count - 1 do
    FSegmentList[I].BreakSegment(Self, FVertices[I], FVertices[I + 1]);

  Result := TPolygon32.Create;
  SetLength(FixedPoints, FOutCount);
  for I := 0 to FOutCount - 1 do
    FixedPoints[I] := FixedPoint(FOutput[I]);
  Result.Points[0] := FixedPoints;
end;

constructor TCustomPath.Create;
begin
  FVertCount := 0;
  FOutCount := 0;
  SetLength(FVertices, 4);
  SetLengtH(FOutput, 4);
  FSegmentList := TSegmentList.Create;
end;

procedure TCustomPath.CurveThroughPoint(const P: TFloatPoint; Index: Integer; Coeff: TFloat);
var
  S: TCustomSegment;
begin
  S := FSegmentList[Index];
  if S is TBezierSegment then
    TBezierSegment(S).CurveThroughPoint(Self, Index, P, Coeff);
end;

procedure TCustomPath.DeleteSegment(Index: Integer);
var
  S: TCustomSegment;
begin
  S := FSegmentList[Index];
  if Assigned(S) then
  begin
    FreeAndNil(S);
    MoveLongWord(FVertices[Index+1], FVertices[Index], FVertCount - Index);
    Dec(FVertCount);
  end;
end;

destructor TCustomPath.Destroy;
var
  I: Integer;
begin
  FVertices := nil;
  FOutput := nil;
  for I := 0 to FSegmentList.Count - 1 do
    FSegmentList[I].Free;
  FSegmentList.Clear;
  FSegmentList.Free;
  inherited;
end;

function TCustomPath.FindClosestSegment(const P: TFloatPoint;
  out Segment: TCustomSegment; out OutPoint: TFloatPoint): Integer;
var
  S: TCustomSegment;
  Q: TFloatPoint;
  I: Integer;
  d, d_min: TFloat;
begin
  d_min := MaxSingle;
  Segment := nil;
  for I := 0 to FSegmentList.Count - 1 do
  begin
    S := FSegmentList[I];
    Q := S.FindClosestPoint(FVertices[I], FVertices[I + 1], P);
    d := SqrDistance(P, Q);
    if d < d_min then
    begin
      d_min := d;
      Segment := S;
      OutPoint := Q;
      Result := I;
    end;
  end;
end;

function TCustomPath.FindClosestVertex(const P: TFloatPoint): PFloatPoint;
var
  I: Integer;
  D, MinD: TFloat;
begin
  MinD := Infinity;
  for I := 0 to FVertCount - 1 do
  begin
    D := SqrDistance(P, FVertices[I]);
    if D < MinD then
    begin
      MinD := D;
      Result := @FVertices[I];
    end;
  end;
end;

function TCustomPath.GetCoefficient(const P: TFloatPoint;
  Index: Integer): TFloat;
var
  S: TCustomSegment;
begin
  Result := 0;
  S := FSegmentList[Index];
  if S is TBezierSegment then
    Result := TBezierSegment(S).GetCoefficient(FVertices[Index], FVertices[Index + 1], P);
end;

function TCustomPath.GetEndPoint: TFloatPoint;
begin
  Result := FVertices[FVertCount - 1];
end;

function TCustomPath.GetLastSegment: TCustomSegment;
begin
  Result := FSegmentList.Last;
end;

function TCustomPath.GetOutputCapacity: Integer;
begin
  Result := Length(FOutput);
end;

function TCustomPath.GetSegment(Index: Integer): TCustomSegment;
begin
  Result := FSegmentList[Index];
end;

function TCustomPath.GetSegmentCount: Integer;
begin
  Result := FSegmentList.Count;
end;

function TCustomPath.GetStartPoint: TFloatPoint;
begin
  Result := FVertices[0];
end;

function TCustomPath.GetVertex(Index: Integer): TFloatPoint;
begin
  Result := FVertices[Index];
end;

function TCustomPath.GetVertexCapacity: Integer;
begin
  Result := Length(FVertices);
end;

function TCustomPath.IndexOf(Segment: TCustomSegment): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FSegmentList.Count - 1 do
    if FSegmentList[I] = Segment then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TCustomPath.InsertSegment(const P: TFloatPoint;
  Segment: TCustomSegment; Index: Integer);
begin
  FSegmentList.Insert(Index, Segment);
  if High(FVertices) < FVertCount then
    SetLength(FVertices, Length(FVertices) * 2);

  Move(FVertices[Index], FVertices[Index + 1], (FVertCount - Index) * SizeOf(TFloatPoint));
  FVertices[Index] := P;
  Inc(FVertCount);

end;

procedure TCustomPath.Offset(const Dx, Dy: TFloat);
var
  I: Integer;
begin
  for I := 0 to FVertCount - 1 do
    with FVertices[I] do
    begin
      X := X + Dx;
      Y := Y + Dy;
    end;
end;

procedure TCustomPath.RemoveSegment(Segment: TCustomSegment);
begin
  DeleteSegment(IndexOf(Segment));
end;

procedure TCustomPath.SetEndPoint(const Value: TFloatPoint);
begin
  FVertices[FVertCount - 1] := Value;
end;

procedure TCustomPath.SetOutputCapacity(const Value: Integer);
begin
  SetLength(FOutput, Value);
end;

procedure TCustomPath.SetStartPoint(const Value: TFloatPoint);
begin
  FVertices[0] := Value;
end;

procedure TCustomPath.SetVertexCapacity(const Value: Integer);
begin
  SetLength(FVertices, Value);
end;

{ TSegmentList }

function TSegmentList.GetCustomSegment(Index: Integer): TCustomSegment;
begin
  Result := List[Index];
end;

procedure TSegmentList.SetCustomSegment(Index: Integer;
  const Value: TCustomSegment);
begin
  List[Index] := Value;
end;
*)

{ TBezierCurve }

{ procedure TBezierCurve.AddPoint(const P: TFloatPoint);
var
  V: TBezierVertex;
begin
  V.Point := P;
  V.ControlPoints[0] := P;
  V.ControlPoints[1] := P;
  AddVertex(V);
end; }

procedure TBezierCurve.AddVertex(const V: TBezierVertex);
var
  H, L: Integer;
begin
  H := High(FVertices);
  L := Length(FVertices[H]);
  SetLength(FVertices[H], L + 1);
  FVertices[H][L] := V;
end;

function BezierCurveToPolygonX(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFixedPoint;
var
  I, Index: Integer;

  procedure Recurse(const P1, P2, P3, P4: TFloatPoint);
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      if High(Result) < Index then
        SetLength(Result, Length(Result) * 2);
      Result[Index] := FixedPoint(P4);
      Inc(Index);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 1/2;
      P12.Y   := (P1.Y + P2.Y) * 1/2;
      P23.X   := (P2.X + P3.X) * 1/2;
      P23.Y   := (P2.Y + P3.Y) * 1/2;
      P34.X   := (P3.X + P4.X) * 1/2;
      P34.Y   := (P3.Y + P4.Y) * 1/2;
      P123.X  := (P12.X + P23.X) * 1/2;
      P123.Y  := (P12.Y + P23.Y) * 1/2;
      P234.X  := (P23.X + P34.X) * 1/2;
      P234.Y  := (P23.Y + P34.Y) * 1/2;
      P1234.X := (P123.X + P234.X) * 1/2;
      P1234.Y := (P123.Y + P234.Y) * 1/2;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  Index := 0;
  SetLength(Result, 8);
  for I := 0 to High(Vertices) + Ord(Closed) - 1 do
    with BezierSegment(Vertices, I) do
      Recurse(P1, P2, P3, P4);
  SetLength(Result, Index);
end;

function BezierCurveToPolygonF(const Vertices: TArrayOfBezierVertex;
  Closed: Boolean = True): TArrayOfFloatPoint;
var
  I, Index: Integer;

  procedure Recurse(const P1, P2, P3, P4: TFloatPoint);
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
  begin
    if Flatness(P1, P2, P3, P4) < BezierTolerance then
    begin
      if High(Result) < Index then
        SetLength(Result, Length(Result) * 2);
      Result[Index] := P4;
      Inc(Index);
    end
    else
    begin
      P12.X   := (P1.X + P2.X) * 1/2;
      P12.Y   := (P1.Y + P2.Y) * 1/2;
      P23.X   := (P2.X + P3.X) * 1/2;
      P23.Y   := (P2.Y + P3.Y) * 1/2;
      P34.X   := (P3.X + P4.X) * 1/2;
      P34.Y   := (P3.Y + P4.Y) * 1/2;
      P123.X  := (P12.X + P23.X) * 1/2;
      P123.Y  := (P12.Y + P23.Y) * 1/2;
      P234.X  := (P23.X + P34.X) * 1/2;
      P234.Y  := (P23.Y + P34.Y) * 1/2;
      P1234.X := (P123.X + P234.X) * 1/2;
      P1234.Y := (P123.Y + P234.Y) * 1/2;

      Recurse(P1, P12, P123, P1234);
      Recurse(P1234, P234, P34, P4);
    end;
  end;

begin
  Index := 0;
  SetLength(Result, 8);
  for I := 0 to High(Vertices) + Ord(Closed) - 1 do
    with BezierSegment(Vertices, I) do
      Recurse(P1, P2, P3, P4);
  SetLength(Result, Index);
end;


procedure TBezierCurve.AppendToPolygon(Polygon: TPolygon32);
var
  I, J: Integer;
begin
  J := High(Polygon.Points);
  for I := 0 to High(FVertices) do
  begin
    Polygon.NewLine;
    Polygon.Points[I + J] := BezierCurveToPolygonX(FVertices[I]);
    //BezierCurveToPolygon(Polygon, FVertices[I]);
  end;
end;

(*
function TBezierCurve.ClosestPointOnCurve(const P: TFloatPoint): TFloatPoint;
var
  P1, P2, C1, C2: TFloatPoint;
  I: Integer;

  function FindPoint(const P1, P2, P3, P4: TFloatPoint; D1, D2: TFloat): TFloatPoint;
  var
    P12, P23, P34, P123, P234, P1234: TFloatPoint;
    NewD: TFloat;
  begin
    P12.X   := (P1.X + P2.X) * 1/2;
    P12.Y   := (P1.Y + P2.Y) * 1/2;
    P23.X   := (P2.X + P3.X) * 1/2;
    P23.Y   := (P2.Y + P3.Y) * 1/2;
    P34.X   := (P3.X + P4.X) * 1/2;
    P34.Y   := (P3.Y + P4.Y) * 1/2;
    P123.X  := (P12.X + P23.X) * 1/2;
    P123.Y  := (P12.Y + P23.Y) * 1/2;
    P234.X  := (P23.X + P34.X) * 1/2;
    P234.Y  := (P23.Y + P34.Y) * 1/2;
    P1234.X := (P123.X + P234.X) * 1/2;
    P1234.Y := (P123.Y + P234.Y) * 1/2;

    NewD := SqrDistance(P, P1234);
    if (NewD < D1) and (NewD < D2) then
    begin
      P12 := FindPoint(P1, P12, P123, P1234, D1, NewD);
      P23 := FindPoint(P1234, P234, P34, P4, NewD, D2);
      if SqrDistance(P, P12) < SqrDistance(P, P23) then
        Result := P12
      else
        Result := P23;
    end
    else
      case CompareValue(D1, D2, Epsilon) of
       -1: Result := FindPoint(P1, P12, P123, P1234, D1, NewD);
        1: Result := FindPoint(P1234, P234, P34, P4, NewD, D2);
        0: Result := P1234;
      end;
  end;

begin
{   for I := 0 to FVertexCount - 2 do
  begin
    P1 := FVertices[I].P;
    C1 :=
    C2 :=
  end; }
end; *)

constructor TBezierCurve.Create;
begin

end;

{ function TBezierCurve.GetFirstContour: TArrayOfBezierVertex;
begin
  Result := nil;
  if High(FVertices) >= 0 then
    Result := FVertices[0];
end;

function TBezierCurve.GetFirstVertex: PBezierVertex;
var
  C: TArrayOfBezierVertex;
begin
  C := LastContour;
  Result := nil;
  if Assigned(C) and (High(C) >= 0) then
    Result := @C[0];
end;

function TBezierCurve.GetLastContour: TArrayOfBezierVertex;
var
  H: Integer;
begin
  Result := nil;
  H := High(FVertices);
  if H >= 0 then
    Result := FVertices[H];
end;

function TBezierCurve.GetLastVertex: PBezierVertex;
var
  C: TArrayOfBezierVertex;
begin
  C := LastContour;
  Result := nil;
  if Assigned(C) and (High(C) >= 0) then
    Result := @C[High(C)];
end; }

{ function TBezierCurve.GetNumPoints: Integer;
begin
  Result := Length(FVertices);
end;

function TBezierCurve.GetPointPtr(Index: Integer): PFloatPoint;
begin
  Result := @FVertices[High(FVertices)][Index].Point;
end; }

procedure TBezierCurve.Delete;
begin
  FVertices := nil;
end;

procedure TBezierCurve.NewContour;
begin
  SetLength(FVertices, Length(FVertices) + 1);
end;

{ procedure TBezierCurve.RemovePoint(Index: Integer);
var
  L: Integer;
begin
  L := Length(FVertices);
  Move(FVertices[Index + 1], FVertices[Index], (L - Index) * SizeOf(TBezierVertex));
  SetLength(FVertices, L - 1);
end; }


(*
{ TPolyLine }

procedure TPolyLine.AddPoint(const P: TFloatPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := P;
end;

procedure TPolyLine.AppendToPolygon(Polygon: TPolygon32);
begin

end;

constructor TPolyLine.Create;
begin

end;


function TPolyLine.GetNumPoints: Integer;
begin
  Result := Length(FPoints);
end;

function TPolyLine.GetPointPtr(Index: Integer): PFloatPoint;
begin
  Result := @FPoints[Index];
end;

procedure TPolyLine.RemovePoint(Index: Integer);
var
  L: Integer;
begin
  L := Length(FPoints);
  Move(FPoints[Index + 1], FPoints[Index], (L - Index) * SizeOf(TFloatPoint));
  SetLength(FPoints, L - 1);
end;

*)

{ TCustomShape }

procedure TCustomShape.AssignTo(Dest: TPersistent);
begin
  if Dest is TPolygon32 then
  begin
    TPolygon32(Dest).Points := nil;
    TPolygon32(Dest).Normals := nil;
    AppendToPolygon(TPolygon32(Dest));
  end;
end;

function TCustomShape.AsPolygon: TPolygon32;
begin
  Result := TPolygon32.Create;
  AppendToPolygon(Result);
end;

{ function TCustomShape.FindClosestPoint(const P: TFloatPoint): PFloatPoint;
var
  I: Integer;
  D, MinD: TFloat;
begin
  MinD := Infinity;
  for I := 0 to NumPoints - 1 do
  begin
    D := SqrDistance(P, Point[I]);
    if D < MinD then
    begin
      Result := PointPtr[I];
      MinD := D;
    end;
  end;
end;

function TCustomShape.GetEndPoint: PFloatPoint;
begin
  Result := GetPointPtr(NumPoints - 1);
end;

function TCustomShape.GetPoint(Index: Integer): TFloatPoint;
begin
  Result := GetPointPtr(Index)^;
end;

function TCustomShape.GetStartPoint: PFloatPoint;
begin
  Result := GetPointPtr(0);
end;

procedure TCustomShape.SetPoint(Index: Integer; Value: TFloatPoint);
begin
  GetPointPtr(Index)^ := Value;
end;
 }

//============================================================================//
// Text routines
//============================================================================//

const
  Identity_mat2: tmat2 = (
    eM11: (fract: 0; Value: 1);
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: 1);
  );

  VertFlip_mat2: tmat2 = (
    eM11: (fract: 0; Value: 1);
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: -1);
  );

function QuadToBezier(Q0, Q1, Q2: TFixedPoint): TBezierVertex;
// Q-spline to Bezier curve:
// B0 = Q0
// B1 = (Q0 + 2*Q1) / 3
// B2 = (Q0 + 2*Q2) / 3
begin
  with Result do
  begin
    Points[0] := FloatPoint(Q0);
    Points[1].X := (Q0.X + 2*Q1.X) / (3 * FixedOne);
    Points[1].Y := (Q0.Y + 2*Q1.Y) / (3 * FixedOne);
    Points[2].X := (Q0.X + 2*Q2.X) / (3 * FixedOne);
    Points[2].Y := (Q0.Y + 2*Q2.Y) / (3 * FixedOne);
  end;
end;

function PointFXtoFixedPoint(const Point: tagPointFX): TFixedPoint;
begin
  Result.X := Point.X.Value shl 16 or Point.X.Fract;
  Result.Y := Point.Y.Value shl 16 or Point.Y.Fract;
end;


function GlyphOutlineToBezierCurve(Dst: TBitmap32; DstX, DstY: TFloat;
  const Character: WideChar; out Metrics: TGlyphMetrics): TBezierCurve;
var
  I, J, K, S, Res: Integer;
  Code: LongWord;
  PGlyphMem, PBuffer: PTTPolygonHeader;
  PPCurve: PTTPolyCurve;

  Handle: HDC;
  BezierCurve: TBezierCurve;
  P: TFloatPoint;
  V: TBezierVertex;
  Q0, Q1, Q2: TFixedPoint;

  procedure AddToBezierCurve;
  begin
    V := QuadToBezier(Q0, Q2, Q1);
    OffsetVertex(V, DstX, DstY);
    BezierCurve.AddVertex(V);
  end;

begin
  Dst.UpdateFont;
  Handle := Dst.Handle;

  Code := Ord(Character);
  Res := GetGlyphOutlineW(Handle, Code, GGO_NATIVE, Metrics, 0, nil, VertFlip_mat2);

  PGlyphMem := StackAlloc(Res);
  PBuffer := PGlyphMem;

  Res := GetGlyphOutlineW(Handle, Code, GGO_NATIVE, Metrics, Res, PBuffer, VertFlip_mat2);

  if (Res = GDI_ERROR) or (PBuffer^.dwType <> TT_POLYGON_TYPE) then
  begin
    Result := nil;
    StackFree(PGlyphMem);
    Exit;
  end;

  BezierCurve := TBezierCurve.Create;
  try
    while Res > 0 do
    begin
      BezierCurve.NewContour;

      S := PBuffer.cb - SizeOf(TTTPolygonHeader);
      PChar(PPCurve) := PChar(PBuffer) + SizeOf(TTTPolygonHeader);
      Q0 := PointFXtoFixedPoint(PBuffer.pfxStart);
      Q2 := Q0;

      while S > 0 do
      begin
        case PPCurve.wType of
          TT_PRIM_LINE:
            begin
              Q1 := Q0;
              AddToBezierCurve;

              P := FloatPoint(TFixedPoint(Q0));
              P.X := P.X + DstX;
              P.Y := P.Y + DstY;
              BezierCurve.AddVertex(ZeroVertex(P));
              for J := 0 to PPCurve.cpfx - 1 do
              begin
                P := FloatPoint(TFixedPoint(PPCurve.apfx[J]));
                P.X := P.X + DstX;
                P.Y := P.Y + DstY;
                BezierCurve.AddVertex(ZeroVertex(P));
              end;

              Q0 := PointFXtoFixedPoint(PPCurve.apfx[PPCurve.cpfx - 1]);
              Q2 := Q0;
            end;
          TT_PRIM_QSPLINE:
            begin

              for J := 0 to PPCurve.cpfx - 2 do
              begin
                Q1 := PointFXtoFixedPoint(PPCurve.apfx[J]);
                AddToBezierCurve;

                if J < PPCurve.cpfx - 2 then
                with PointFXtoFixedPoint(PPCurve.apfx[J + 1]) do
                  begin
                    Q0.x := (Q1.x + x) div 2;
                    Q0.y := (Q1.y + y) div 2;
                  end
                else
                  Q0 := PointFXtoFixedPoint(PPCurve.apfx[J + 1]);

                Q2 := Q1;
              end;
            end;
        end;
        K := (PPCurve.cpfx - 1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
        Dec(S, K);
        Inc(PChar(PPCurve), K);
      end;

      Dec(PChar(PPCurve), K);
      if PPCurve.wType = TT_PRIM_QSPLINE then
      begin
        Q1 := PointFXtoFixedPoint(PPCurve.apfx[PPCurve.cpfx - 1]);
        AddToBezierCurve;
      end;

      Dec(Res, PBuffer.cb);
      Inc(PChar(PBuffer), PBuffer.cb);
    end;

  except
    BezierCurve.Free;
  end;
  Result := BezierCurve;
  StackFree(PGlyphMem);
end;

{ function GlyphOutlineToBezierCurve(Dst: TBitmap32; DstX, DstY: TFloat;
  const Character: Char; out Metrics: TGlyphMetrics): TBezierCurve; overload;
var
  C: WideChar;
begin
  C := WideChar(Character);
  Result := GlyphOutlineToBezierCurve(Dst, DstX, DstY, C, Metrics);
end; }

{ procedure RenderText(Dst: TBitmap32; X, Y: TFloat; const Text: string;
  OutlineColor, FillColor: TColor32; FillCallback: TFillLineEvent;
  Transformation: TTransformation = nil);
var
  I: Integer;
  B: TBezierCurve;
  P: TPolygon32;
  Metrics: TGlyphMetrics;
begin
  P := TPolygon32.Create;
  P.Antialiased := True;
  P.AntialiasMode := am32times;
  P.FillMode := pfWinding;
  Y := Y - Dst.Font.Height;
  try
    for I := 1 to Length(Text) do
    begin
      B := GlyphOutlineToBezierCurve(Dst, X, Y, Text[I], Metrics);
      try
        P.Assign(B);
        if Assigned(FillCallback) then
          P.Draw(Dst, OutlineColor, FillCallback, Transformation)
        else
          P.Draw(Dst, OutlineColor, FillColor, Transformation);
        X := X + Metrics.gmCellIncX;
      finally
        B.Free;
      end;
    end;
  finally
    P.Free;
  end;
end; }

function TextToPolygon(Dst: TBitmap32; X, Y: TFloat; const Text: WideString): TPolygon32;
var
  I: Integer;
  B: TBezierCurve;
  Metrics: TGlyphMetrics;
  TextMetric: TTextMetric;
begin
  Result := TPolygon32.Create;
  Result.Antialiased := True;
  Result.AntialiasMode := am4times;
  Result.FillMode := pfWinding;

  Dst.UpdateFont;
  SelectObject(Dst.Handle, Dst.Font.Handle);
  GetTextMetrics(Dst.Handle, TextMetric);
  Y := Y + TextMetric.tmAscent;
  for I := 1 to Length(Text) do
  begin
    B := GlyphOutlineToBezierCurve(Dst, X, Y, Text[I], Metrics);
    if Assigned(B) then
    try
      B.AppendToPolygon(Result);
    finally
      B.Free;
    end;
    X := X + Metrics.gmCellIncX;
  end;
end;

procedure RenderText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; FillCallback: TFillLineEvent;
  Transformation: TTransformation = nil);
var
  P: TPolygon32;
begin
  P := TextToPolygon(Dst, X, Y, Text);
  try
    if Assigned(FillCallback) then
      P.Draw(Dst, OutlineColor, FillCallback, Transformation)
    else
      P.Draw(Dst, OutlineColor, FillColor, Transformation);
  finally
    P.Free;
  end;
end;

procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
begin
  RenderText(Dst, X, Y, Text, OutlineColor, FillColor, nil, Transformation);
end;

procedure DrawText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  OutlineColor: TColor32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation = nil); overload;
begin
  RenderText(Dst, X, Y, Text, OutlineColor, 0, Filler.FillLine, Transformation);
end;

procedure DrawTextOutline(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Color: TColor32; Transformation: TTransformation = nil);
begin
  RenderText(Dst, X, Y, Text, Color, 0, nil, Transformation);
end;

procedure FillText(Dst: TBitmap32; X, Y: TFloat; const Text: WideString;
  Filler: TCustomPolygonFiller; Transformation: TTransformation = nil);
begin
  RenderText(Dst, X, Y, Text, 0, 0, Filler.FillLine, Transformation);
end;


//============================================================================//

// SplineToPolygon
function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFixedPoint; overload;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, dx, dy, X, Y: Single;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFixedPoint;
const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  LastIndex := H - Ord(not Closed);
  Steps := 0;
  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Inc(Steps, Floor(Hypot(dx, dy) / StepSize) + 1);
  end;

  SetLength(Result, Steps);
  PPoint := @Result[0];

  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Steps := Floor(Hypot(dx, dy) / StepSize);
    if Steps > 0 then
    begin
      K := 1 / Steps;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        V := J * K;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FixedPoint(X, Y);
        Inc(PPoint);
      end;
    end;
  end;
end;

function MakeCurve(const Points: TArrayOfFixedPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFixedPoint; overload;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, dx, dy, X, Y: Single;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFixedPoint;
const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := WRAP_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  LastIndex := H - Ord(not Closed);
  Steps := 0;
  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Inc(Steps, Floor(Hypot(dx, dy) / StepSize) + 1);
  end;

  SetLength(Result, Steps);
  PPoint := @Result[0];

  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    dx := Points[Index].X - Points[I].X;
    dy := Points[Index].Y - Points[I].Y;
    Steps := Floor(Hypot(dx, dy) / StepSize);
    if Steps > 0 then
    begin
      K := 1 / Steps;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        V := J * K;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FixedPoint(X, Y);
        Inc(PPoint);
      end;
    end;
  end;
end;

{ TCustomCurve }

procedure TCustomCurve.Delete;
begin
//
end;

{ TSplineCurve }

procedure TSplineCurve.AddPoint(const Point: TFloatPoint);
var
  H, L: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + 1);
  Points[H][L] := Point;
end;

procedure TSplineCurve.AppendToPolygon(Polygon: TPolygon32);
var
  P: TArrayOfFixedPoint;
  I, H: Integer;
begin
  H := High(Polygon.Points);
  for I := 0 to High(FPoints) do
  begin
    Polygon.NewLine;
    Inc(H);
    Polygon.Points[H] := MakeCurve(FPoints[I], FKernel, True, 2);
  end;
end;

constructor TSplineCurve.Create;
begin
  FKernel := TCubicKernel.Create;
  NewContour;
end;

procedure TSplineCurve.NewContour;
begin
  SetLength(FPoints, Length(FPoints) + 1);
end;

procedure TSplineCurve.SetKernel(const Value: TCustomKernel);
begin
  if Assigned(FKernel) then
    FKernel.Free;
  FKernel := Value;
end;

{ TEllipse }

procedure TEllipse.AppendToPolygon(Polygon: TPolygon32);
begin

end;

end.
