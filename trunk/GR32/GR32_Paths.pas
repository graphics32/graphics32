unit GR32_Paths;

interface

uses
  Classes, GR32, GR32_Polygons;

type
  TCustomPath = class;

  TCustomSegment = class(TPersistent)
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); virtual; abstract;
    function FindClosestPoint(const P1, P2, P3: TFloatPoint): TFloatPoint; virtual; abstract;
  end;

  TLineSegment = class(TCustomSegment)
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); override;
    function FindClosestPoint(const P1, P2, P3: TFloatPoint): TFloatPoint; override;
  end;

  PControlPoints = ^TControlPoints;
  TControlPoints = array [0..1] of TFloatPoint;

  TBezierSegment = class(TCustomSegment)
  private
    FControlPoints: TControlPoints;
    function GetControlPoints: PControlPoints;
  protected
    procedure BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint); override;
    function FindClosestPoint(const P1, P2, P3: TFloatPoint): TFloatPoint; override;
  public
    constructor Create; overload;
    constructor Create(const C1, C2: TFloatPoint); overload;
    property ControlPoints: PControlPoints read GetControlPoints;
  end;

  TCustomPath = class(TPersistent)
  private
    FVertices: TArrayOfFloatPoint;
    FOutput: TArrayOfFloatPoint;
    FSegmentList: TList;
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
  protected
    procedure AppendPoint(const Point: TFloatPoint);
  public
    constructor Create;
    destructor Destroy; override;
    function ConvertToPolygon: TPolygon32;
    procedure FindClosestSegment(const P: TFloatPoint;
      out Segment: TCustomSegment; out OutPoint: TFloatPoint);
    procedure AddStartPoint(const P: TFloatPoint);
    procedure AddSegment(const P: TFloatPoint; Segment: TCustomSegment);
    procedure DeleteSegment(Index: Integer);
    procedure RemoveSegment(Segment: TCustomSegment);
    function IndexOf(Segment: TCustomSegment): Integer;
    property VertexCapacity: Integer read GetVertexCapacity write SetVertexCapacity;
    property OutputCapacity: Integer read GetOutputCapacity write SetOutputCapacity;
    property Count: Integer read FVertCount;
    property StartPoint: TFloatPoint read GetStartPoint write SetStartPoint;
    property EndPoint: TFloatPoint read GetEndPoint write SetEndPoint;
    property Segments[Index: Integer]: TCustomSegment read GetSegment;
  end;

var
  BezierTolerance: Single = 1;

implementation

uses
  SysUtils, GR32_LowLevel, Math;

// Returns square of the distance between points A and B.
function SqrDistance(const A, B: TFloatPoint): TFloat;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


// Returns a point on the line from A to B perpendicular to C.
function PointOnLine(const A, B, C: TFloatPoint): TFloatPoint;
var
  dx, dy, r: Single;
begin
  dx := B.X - A.X;
  dy := B.Y - A.Y;
  r := ((C.X - A.X) * dx + (C.Y - A.Y) * dy) / (Sqr(dx) + Sqr(dy));
  Result.X := A.X + r * dx;
  Result.Y := A.Y + r * dy;
end;

function Flatness(P1, P2, P3, P4: TFloatPoint): Single;
begin
  Result :=
    Abs(P1.X + P3.X - 2*P2.X) +
    Abs(P1.Y + P3.Y - 2*P2.Y) +
    Abs(P2.X + P4.X - 2*P3.X) +
    Abs(P2.Y + P4.Y - 2*P3.Y);
end;

{ TLineSegment }

procedure TLineSegment.BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint);
begin
  Path.AppendPoint(P2);
end;

function TLineSegment.FindClosestPoint(const P1, P2, P3: TFloatPoint): TFloatPoint;
begin
  Result := PointOnLine(P1, P2, P3);
end;

{ TBezierSegment }

procedure TBezierSegment.BreakSegment(Path: TCustomPath; const P1, P2: TFloatPoint);

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
  Recurse(P1, FControlPoints[0], FControlPoints[1], P2);
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

function TBezierSegment.FindClosestPoint(const P1, P2, P3: TFloatPoint): TFloatPoint;
begin
{      = (1-t)^3 c30 +
       3(1-t)^2 t c21 +
       3(1-t) t^2 c12 +
       t^3 c03. }

  Result := PointOnLine(P1, P2, P3);
end;

function TBezierSegment.GetControlPoints: PControlPoints;
begin
  Result := @FControlPoints;
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

function TCustomPath.ConvertToPolygon: TPolygon32;
var
  I: Integer;
  FixedPoints: TArrayOfFixedPoint;
begin
  AppendPoint(StartPoint);
  for I := 0 to FSegmentList.Count - 1 do
  begin
    TCustomSegment(FSegmentList[I]).BreakSegment(Self, FVertices[I], FVertices[I + 1]);
  end;

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
  FSegmentList := TList.Create;
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
    TCustomSegment(FSegmentList[I]).Free;
  FSegmentList.Clear;
  FSegmentList.Free;
  inherited;
end;

procedure TCustomPath.FindClosestSegment(const P: TFloatPoint;
  out Segment: TCustomSegment; out OutPoint: TFloatPoint);
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
    S := TCustomSegment(FSegmentList[I]);
    Q := S.FindClosestPoint(FVertices[I], FVertices[I + 1], P);
    d := SqrDistance(P, Q);
    if d < d_min then
    begin
      d_min := d;
      Segment := S;
      OutPoint := Q;
    end;
  end;
end;

function TCustomPath.GetEndPoint: TFloatPoint;
begin
  Result := FVertices[FVertCount - 1];
end;

function TCustomPath.GetOutputCapacity: Integer;
begin
  Result := Length(FOutput);
end;

function TCustomPath.GetSegment(Index: Integer): TCustomSegment;
begin
  Result := FSegmentList[Index];
end;

function TCustomPath.GetStartPoint: TFloatPoint;
begin
  Result := FVertices[0];
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

end.
