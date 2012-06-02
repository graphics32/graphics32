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
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Classes, SysUtils, GR32, GR32_Polygons, GR32_Transforms;

type
  { TCustomPath }
  TCustomPath = class(TThreadPersistent)
  private
    FCurrentPoint: TFloatPoint;
    FLastControlPoint: TFloatPoint;
    FStrokeWidth: TFloat;
    FStrokeColor: TColor32;
    FFillColor: TColor32;
    FFillOpacity: TFloat;
    FStrokeOpacity: TFloat;
    FStrokeDashOffset: TFloat;
    FStrokeDashArray: TArrayOfFloat;
    FJoinStyle: TJoinStyle;
    FEndStyle: TEndStyle;
    FFillMode: TPolyFillMode;
    FMiterLimit: TFloat;
  protected
    procedure SetFillMode(Value: TPolyFillMode); virtual;
    procedure SetStrokeDashArray(const DashArray: array of TFloat); virtual;
    procedure SetStrokeDashOffset(const Offset: TFloat); virtual;
    procedure SetStrokeColor(const Color: TColor32); overload; virtual;
    procedure SetFillColor(const Color: TColor32); overload; virtual;
    procedure SetFillOpacity(const Value: TFloat); virtual;
    procedure SetStrokeOpacity(const Value: TFloat); virtual;
    procedure SetStrokeWidth(const Value: TFloat); virtual;
    procedure SetJoinStyle(const Value: TJoinStyle); virtual;
    procedure SetEndStyle(const Value: TEndStyle); virtual;
    procedure SetMiterLimit(const Value: TFloat); virtual;
    procedure AddPoint(const Point: TFloatPoint); virtual;
  public
    property CurrentPoint: TFloatPoint read FCurrentPoint write FCurrentPoint;
    property StrokeWidth: TFloat read FStrokeWidth write SetStrokeWidth;
    property StrokeColor: TColor32 read FStrokeColor write SetStrokeColor;
    property StrokeOpacity: TFloat read FStrokeOpacity write SetStrokeOpacity;
    property FillColor: TColor32 read FFillColor write SetFillColor;
    property FillOpacity: TFloat read FFillOpacity write SetFillOpacity;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property EndStyle: TEndStyle read FEndStyle write SetEndStyle;
    property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;
    property FillMode: TPolyFillMode read FFillMode write SetFillMode;

    constructor Create; override;
    procedure MoveTo(const X, Y: TFloat); overload;
    procedure MoveTo(const P: TFloatPoint); overload; virtual;
    procedure LineTo(const X, Y: TFloat); overload;
    procedure LineTo(const P: TFloatPoint); overload; virtual;
    procedure CurveTo(const X1, Y1, X2, Y2, X, Y: TFloat); overload;
    procedure CurveTo(const X2, Y2, X, Y: TFloat); overload;
    procedure CurveTo(const C1, C2, P: TFloatPoint); overload; virtual;
    procedure CurveTo(const C2, P: TFloatPoint); overload; virtual;
    procedure ConicTo(const X1, Y1, X, Y: TFloat); overload;
    procedure ConicTo(const P1, P: TFloatPoint); overload; virtual;
    procedure ConicTo(const X, Y: TFloat); overload;
    procedure ConicTo(const P: TFloatPoint); overload; virtual;
    procedure BeginPath; virtual;
    procedure EndPath; virtual;
    procedure ClosePath; virtual;
    procedure Rectangle(const Rect: TFloatRect); virtual;
    procedure Ellipse(Rx, Ry: TFloat); overload; virtual;
    procedure Ellipse(const Cx, Cy, Rx, Ry: TFloat); overload; virtual;
    procedure Circle(const Cx, Cy, R: TFloat); virtual;
    procedure Polygon(const APoints: TArrayOfFloatPoint); virtual;
  end;

  { TFlattenedPath }
  TFlattenedPath = class(TCustomPath)
  private
    FPath: TArrayOfArrayOfFloatPoint;
    FPoints: TArrayOfFloatPoint;
    FPointIndex: Integer;
    FOnBeginPath: TNotifyEvent;
    FOnEndPath: TNotifyEvent;
    FOnClosePath: TNotifyEvent;
  protected
    procedure AddPoint(const Point: TFloatPoint); override;
    property Points: TArrayOfFloatPoint read FPoints;
    property Path: TArrayOfArrayOfFloatPoint read FPath;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawPath; virtual;
    procedure MoveTo(const P: TFloatPoint); override;
    procedure ClosePath; override;
    procedure BeginPath; override;
    procedure EndPath; override;
    property OnBeginPath: TNotifyEvent read FOnBeginPath write FOnBeginPath;
    property OnEndPath: TNotifyEvent read FOnEndPath write FOnEndPath;
    property OnClosePath: TNotifyEvent read FOnClosePath write FOnClosePath;
  end;

  { TCustomCanvas }
  TCustomCanvas = class(TThreadPersistent)
  private
    FPath: TFlattenedPath;
    FTransformation: TTransformation;
    procedure SetTransformation(const Value: TTransformation);
  protected
    procedure DrawPath; virtual; abstract;
    procedure DoBeginPath(Sender: TObject); virtual;
    procedure DoEndPath(Sender: TObject); virtual;
    procedure DoClosePath(Sender: TObject); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Transformation: TTransformation read FTransformation write SetTransformation;
    property Path: TFlattenedPath read FPath;
  end;

  { TCanvas32 }
  TCanvas32 = class(TCustomCanvas)
  private
    FBitmap: TBitmap32;
    FRenderer: TPolygonRenderer32;
    function GetRendererClassName: string;
    procedure SetRendererClassName(const Value: string);
    procedure SetRenderer(ARenderer: TPolygonRenderer32);
  protected
    procedure DrawPath; override;
    class function GetPolygonRendererClass: TPolygonRenderer32Class; virtual;
  public
    constructor Create(ABitmap: TBitmap32); reintroduce; virtual;
    destructor Destroy; override;
    property Bitmap: TBitmap32 read FBitmap;
    property Renderer: TPolygonRenderer32 read FRenderer write SetRenderer;
    property RendererClassName: string read GetRendererClassName write SetRendererClassName;
  end;

var
  CubicBezierTolerance: TFloat = 0.25;
  QuadraticBezierTolerance: TFloat = 0.25;

type
  TAddPointEvent = procedure(const Point: TFloatPoint) of object;

implementation

uses
  GR32_Math, GR32_VectorUtils;

function CBezierFlatness(const P1, P2, P3, P4: TFloatPoint): TFloat; inline;
begin
  Result :=
    Abs(P1.X + P3.X - 2*P2.X) +
    Abs(P1.Y + P3.Y - 2*P2.Y) +
    Abs(P2.X + P4.X - 2*P3.X) +
    Abs(P2.Y + P4.Y - 2*P3.Y);
end;

function QBezierFlatness(const P1, P2, P3: TFloatPoint): TFloat; inline;
begin
  Result :=
    Abs(P1.x + P3.x - 2*P2.x) +
    abs(P1.y + P3.y - 2*P2.y);
end;

procedure BezierCurve(const P1, P2, P3, P4: TFloatPoint;
  const AddPoint: TAddPointEvent; const Tolerance: TFloat);
var
  P12, P23, P34, P123, P234, P1234: TFloatPoint;
begin
  if CBezierFlatness(P1, P2, P3, P4) < Tolerance then
  begin
    AddPoint(P1);
  end
  else
  begin
    P12.X   := (P1.X + P2.X) * 0.5;
    P12.Y   := (P1.Y + P2.Y) * 0.5;
    P23.X   := (P2.X + P3.X) * 0.5;
    P23.Y   := (P2.Y + P3.Y) * 0.5;
    P34.X   := (P3.X + P4.X) * 0.5;
    P34.Y   := (P3.Y + P4.Y) * 0.5;
    P123.X  := (P12.X + P23.X) * 0.5;
    P123.Y  := (P12.Y + P23.Y) * 0.5;
    P234.X  := (P23.X + P34.X) * 0.5;
    P234.Y  := (P23.Y + P34.Y) * 0.5;
    P1234.X := (P123.X + P234.X) * 0.5;
    P1234.Y := (P123.Y + P234.Y) * 0.5;

    BezierCurve(P1, P12, P123, P1234, AddPoint, Tolerance);
    BezierCurve(P1234, P234, P34, P4, AddPoint, Tolerance);
  end;
end;

procedure QSpline(const P1, P2, P3: TFloatPoint; const AddPoint: TAddPointEvent;
  const Tolerance: TFloat);
var
  p12, p23, p123: TFloatPoint;
begin
  //assess flatness of curve ...
  if QBezierFlatness(P1, P2, P3) < Tolerance then
  begin
    AddPoint(P1);
  end
  else
  begin
    p12.X := (p1.X + p2.X) * 0.5;
    p12.Y := (p1.Y + p2.Y) * 0.5;
    p23.X := (p2.X + p3.X) * 0.5;
    p23.Y := (p2.Y + p3.Y) * 0.5;
    p123.X := (p12.X + p23.X) * 0.5;
    p123.Y := (p12.Y + p23.Y) * 0.5;

    QSpline(p1, p12, p123, AddPoint, Tolerance);
    QSpline(p123, p23, p3, AddPoint, Tolerance);
  end;
end;


//============================================================================//

{ TCustomPath }

procedure TCustomPath.SetFillColor(const Color: TColor32);
begin
  FFillColor := Color;
end;

procedure TCustomPath.SetFillOpacity(const Value: TFloat);
begin
  FFillOpacity := Value;
end;

procedure TCustomPath.SetStrokeColor(const Color: TColor32);
begin
  FStrokeColor := Color;
end;

procedure TCustomPath.SetStrokeOpacity(const Value: TFloat);
begin
  FStrokeOpacity := Value;
end;

procedure TCustomPath.SetStrokeWidth(const Value: TFloat);
begin
  FStrokeWidth := Value;
end;

procedure TCustomPath.SetEndStyle(const Value: TEndStyle);
begin
  FEndStyle := Value;
end;

procedure TCustomPath.SetJoinStyle(const Value: TJoinStyle);
begin
  FJoinStyle := Value;
end;

procedure TCustomPath.SetMiterLimit(const Value: TFloat);
begin
  FMiterLimit := Value;
end;

procedure TCustomPath.SetStrokeDashArray(
  const DashArray: array of TFloat);
var
  L: Integer;
begin
  L := Length(DashArray);
  SetLength(FStrokeDashArray, L);
  Move(DashArray[0], FStrokeDashArray[0], L * SizeOf(TFloat));
end;

procedure TCustomPath.SetStrokeDashOffset(const Offset: TFloat);
begin
  FStrokeDashOffset := Offset;
end;

procedure TCustomPath.SetFillMode(Value: TPolyFillMode);
begin
  FFillMode := Value;
end;

{ TCustomPath }

procedure TCustomPath.CurveTo(const X1, Y1, X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(X1, Y1), FloatPoint(X2, Y2), FloatPoint(X, Y));
end;

procedure TCustomPath.LineTo(const X, Y: TFloat);
begin
  LineTo(FloatPoint(X, Y));
end;

procedure TCustomPath.MoveTo(const X, Y: TFloat);
begin
  MoveTo(FloatPoint(X, Y));
end;

procedure TCustomPath.AddPoint(const Point: TFloatPoint);
begin
end;

procedure TCustomPath.BeginPath;
begin

end;

procedure TCustomPath.Circle(const Cx, Cy, R: TFloat);
begin
  Ellipse(Cx, Cy, R, R);
end;

procedure TCustomPath.ClosePath;
begin
end;

procedure TCustomPath.ConicTo(const P1, P: TFloatPoint);
begin
  QSpline(FCurrentPoint, P1, P, LineTo, QuadraticBezierTolerance);
  LineTo(P);
  FCurrentPoint := P;
end;

procedure TCustomPath.Ellipse(const Cx, Cy, Rx, Ry: TFloat);
const
  Steps = 360;
var
  I: Integer;
  A, CosA, SinA: TFloat;
begin
  BeginPath;
  MoveTo(Cx + Rx, Cy);
  for I := 1 to Steps - 1 do
  begin
    A := I / Steps * 2 * Pi;
    GR32_Math.SinCos(A, SinA, CosA);
    LineTo(Cx + Rx * CosA, Cy + Ry * SinA);
  end;
  ClosePath;
  EndPath;
end;

procedure TCustomPath.EndPath;
begin

end;

procedure TCustomPath.LineTo(const P: TFloatPoint);
begin
  AddPoint(P);
  FCurrentPoint := P;
end;

procedure TCustomPath.Rectangle(const Rect: TFloatRect);
begin
  BeginPath;
  MoveTo(Rect.Left, Rect.Top);
  LineTo(Rect.Right, Rect.Top);
  LineTo(Rect.Right, Rect.Bottom);
  LineTo(Rect.Left, Rect.Bottom);
  ClosePath;
  EndPath;
end;

procedure TCustomPath.Ellipse(Rx, Ry: TFloat);
begin
  with FCurrentPoint do Ellipse(X, Y, Rx, Ry);
end;

procedure TCustomPath.ConicTo(const X, Y: TFloat);
begin
  ConicTo(FloatPoint(X, Y));
end;

procedure TCustomPath.ConicTo(const P: TFloatPoint);
var
  P1: TFloatPoint;
begin
  P1.X := FCurrentPoint.X - (FLastControlPoint.X - FCurrentPoint.X);
  P1.Y := FCurrentPoint.Y - (FLastControlPoint.Y - FCurrentPoint.Y);
  ConicTo(P1, P);
end;

constructor TCustomPath.Create;
begin
  inherited;
  FFillOpacity := 1;
  FStrokeOpacity := 0;
  FStrokeWidth := 1.0;
  FFillColor := clWhite32;
  FStrokeColor := clBlack32;
  FMiterLimit := DEFAULT_MITER_LIMIT;
end;

procedure TCustomPath.CurveTo(const X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(X2, Y2), FloatPoint(X, Y));
end;

procedure TCustomPath.CurveTo(const C2, P: TFloatPoint);
var
  C1: TFloatPoint;
begin
  C1.X := FCurrentPoint.X - (FLastControlPoint.X - FCurrentPoint.X);
  C1.Y := FCurrentPoint.Y - (FLastControlPoint.Y - FCurrentPoint.Y);
  CurveTo(C1, C2, P);
end;

procedure TCustomPath.CurveTo(const C1, C2, P: TFloatPoint);
begin
  BezierCurve(FCurrentPoint, C1, C2, P, LineTo, CubicBezierTolerance);
  LineTo(P);
  FCurrentPoint := P;
end;

procedure TCustomPath.Polygon(const APoints: TArrayOfFloatPoint);
var
  I: Integer;
begin
  BeginPath;
  MoveTo(APoints[0]);
  for I := 1 to High(APoints) do
    LineTo(APoints[I]);
  ClosePath;
  EndPath;
end;

procedure TCustomPath.ConicTo(const X1, Y1, X, Y: TFloat);
begin
  ConicTo(FloatPoint(X1, Y1), FloatPoint(X, Y));
end;

procedure TCustomPath.MoveTo(const P: TFloatPoint);
begin
  FCurrentPoint := P;
  AddPoint(P);
end;

{ TFlattenedPath }

procedure TFlattenedPath.ClosePath;
var
  N: Integer;
begin
  if Length(FPoints) <> 0 then
  begin
    N := Length(FPath);
    SetLength(FPath, N + 1);
    FPath[N] := Copy(FPoints, 0, FPointIndex);
    FPoints := nil;
    FPointIndex := 0;
  end;
  if Assigned(FOnClosePath) then FOnClosePath(Self);
end;

procedure TFlattenedPath.MoveTo(const P: TFloatPoint);
begin
  FCurrentPoint := P;
  if Length(FPoints) <> 0 then
    ClosePath;
  AddPoint(P);
end;

procedure TFlattenedPath.BeginPath;
begin
  FPath := nil;
  FPoints := nil;
  FPointIndex := 0;
  if Assigned(FOnBeginPath) then FOnBeginPath(Self);
end;

procedure TFlattenedPath.AddPoint(const Point: TFloatPoint);
const
  BUFFSIZEINCREMENT = 128;
var
  L: Integer;
begin
  L := Length(FPoints);
  if FPointIndex >= L then
    SetLength(FPoints, L + BUFFSIZEINCREMENT);
  FPoints[FPointIndex] := Point;
  Inc(FPointIndex);
end;

procedure TFlattenedPath.EndPath;
begin
  if Assigned(FOnEndPath) then FOnEndPath(Self);
end;

constructor TFlattenedPath.Create;
begin
  inherited;
//  FPolygonRenderer := GetPolygonRendererClass.Create;
end;

destructor TFlattenedPath.Destroy;
begin
//  FPolygonRenderer.Free;
  inherited;
end;

procedure TFlattenedPath.DrawPath;
begin
  // implemented by descendants
end;


{ TCustomCanvas }

constructor TCustomCanvas.Create;
begin
  FPath := TFlattenedPath.Create;
  FPath.OnBeginPath := DoBeginPath;
  FPath.OnEndPath := DoEndPath;
  FPath.OnClosePath := DoClosePath;
end;

destructor TCustomCanvas.Destroy;
begin
  FPath.Free;
  inherited;
end;

procedure TCustomCanvas.DoBeginPath(Sender: TObject);
begin

end;

procedure TCustomCanvas.DoClosePath(Sender: TObject);
begin

end;

procedure TCustomCanvas.DoEndPath(Sender: TObject);
begin
  DrawPath;
end;

procedure TCustomCanvas.SetTransformation(const Value: TTransformation);
begin
  if FTransformation <> Value then
  begin
    FTransformation := Value;
    Changed;
  end;
end;

{ TCanvas32 }

constructor TCanvas32.Create(ABitmap: TBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
  FRenderer := GetPolygonRendererClass.Create;
  FRenderer.Bitmap := ABitmap;
end;

destructor TCanvas32.Destroy;
begin
  Renderer.Free;
  inherited;
end;

procedure TCanvas32.DrawPath;
var
  FC, SC: TColor32Entry;
  ClipRect: TFloatRect;
  APoints: TArrayOfArrayOfFloatPoint;
begin
  ClipRect := FloatRect(Bitmap.ClipRect);
  with Path do
  begin
    FC.ARGB := FillColor;
    SC.ARGB := StrokeColor;
    FC.A := Round(FillOpacity * 255);
    SC.A := Round(StrokeOpacity * 255);

    // fill path
    Renderer.Color := FC.ARGB;
    Renderer.PolyPolygonFS(Path, ClipRect, Transformation);

    if (StrokeWidth > 0) and (SC.A <> 0) then
    begin
      // stroke the contours of closed paths
      Renderer.Color := SC.ARGB;
      APoints := BuildPolyPolyline(Path, True, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
      Renderer.PolyPolygonFS(APoints, ClipRect, Transformation);

      // stroke open path
      if Length(Points) > 0 then
      begin
        SetLength(APoints, 1);
        APoints[0] := BuildPolyline(Points, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
        Renderer.PolyPolygonFS(APoints, ClipRect, Transformation);
      end;
    end;
  end;
end;


class function TCanvas32.GetPolygonRendererClass: TPolygonRenderer32Class;
begin
  Result := DefaultPolygonRendererClass;
end;

function TCanvas32.GetRendererClassName: string;
begin
  Result := FRenderer.ClassName;
end;

procedure TCanvas32.SetRenderer(ARenderer: TPolygonRenderer32);
begin
  if Assigned(ARenderer) and (FRenderer <> ARenderer) then
  begin
    if Assigned(FRenderer) then FRenderer.Free;
    FRenderer := ARenderer;
    Changed;
  end;
end;

procedure TCanvas32.SetRendererClassName(const Value: string);
var
  RendererClass: TPolygonRenderer32Class;
begin
  if (Value <> '') and (FRenderer.ClassName <> Value) and Assigned(PolygonRendererList) then
  begin
    RendererClass := TPolygonRenderer32Class(PolygonRendererList.Find(Value));
    if Assigned(RendererClass) then
      Renderer := RendererClass.Create;
  end;
end;

end.
