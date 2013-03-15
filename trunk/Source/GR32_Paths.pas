unit GR32_Paths;

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
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Classes, SysUtils, GR32, GR32_Polygons, GR32_Transforms,
  GR32_Brushes, GR32_Geometry;

const
  DefaultCircleSteps = 100;
  DefaultBezierTolerance = 0.25;

type
  TControlPointOrigin = (cpNone, cpCubic, cpConic);

  { TCustomPath }
  TCustomPath = class(TThreadPersistent)
  private
    FCurrentPoint: TFloatPoint;
    FLastControlPoint: TFloatPoint;
    FControlPointOrigin: TControlPointOrigin;
  protected
    procedure AddPoint(const Point: TFloatPoint); virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure Clear; virtual;

    procedure MoveTo(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure MoveTo(const P: TFloatPoint); overload; virtual;
    procedure MoveToRelative(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure MoveToRelative(const P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure LineTo(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure LineTo(const P: TFloatPoint); overload; virtual;
    procedure LineToRelative(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure LineToRelative(const P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure HorizontalLineTo(const X: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure HorizontalLineToRelative(const X: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure VerticalLineTo(const Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure VerticalLineToRelative(const Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveTo(const X1, Y1, X2, Y2, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveTo(const X2, Y2, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveTo(const C1, C2, P: TFloatPoint); overload; virtual;
    procedure CurveTo(const C2, P: TFloatPoint); overload; virtual;
    procedure CurveToRelative(const X1, Y1, X2, Y2, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveToRelative(const X2, Y2, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveToRelative(const C1, C2, P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure CurveToRelative(const C2, P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicTo(const X1, Y1, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicTo(const P1, P: TFloatPoint); overload; virtual;
    procedure ConicTo(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicTo(const P: TFloatPoint); overload; virtual;
    procedure ConicToRelative(const X1, Y1, X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicToRelative(const P1, P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicToRelative(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure ConicToRelative(const P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure BeginPath; virtual;
    procedure EndPath; virtual;
    procedure ClosePath; virtual;
    procedure Rectangle(const Rect: TFloatRect); virtual;
    procedure RoundRect(const Rect: TFloatRect; const Radius: TFloat); virtual;
    procedure Arc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat); virtual;
    procedure Ellipse(Rx, Ry: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;
    procedure Ellipse(const Cx, Cy, Rx, Ry: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;
    procedure Circle(const Cx, Cy, Radius: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;
    procedure Circle(const Center: TFloatPoint; Radius: TFloat; Steps: Integer = DefaultCircleSteps); overload; virtual;
    procedure Polygon(const APoints: TArrayOfFloatPoint); virtual;
    property CurrentPoint: TFloatPoint read FCurrentPoint write FCurrentPoint;
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
    function GetPoints: TArrayOfFloatPoint;
  protected
    procedure AddPoint(const Point: TFloatPoint); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear; override;
    procedure DrawPath; virtual;
    procedure MoveTo(const P: TFloatPoint); override;
    procedure ClosePath; override;
    procedure BeginPath; override;
    procedure EndPath; override;

    procedure Polygon(const APoints: TArrayOfFloatPoint); override;

    property Points: TArrayOfFloatPoint read GetPoints;
    property Path: TArrayOfArrayOfFloatPoint read FPath;

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
    procedure AssignTo(Dest: TPersistent); override;
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
    FBrushes: TBrushCollection;
    function GetRendererClassName: string;
    procedure SetRendererClassName(const Value: string);
    procedure SetRenderer(ARenderer: TPolygonRenderer32);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DrawPath; override;
    class function GetPolygonRendererClass: TPolygonRenderer32Class; virtual;
    procedure BrushCollectionChangeHandler(Sender: TObject); virtual;
  public
    constructor Create(ABitmap: TBitmap32); reintroduce; virtual;
    destructor Destroy; override;

    procedure RenderText(X, Y: TFloat; const Text: WideString); overload;
    procedure RenderText(const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal); overload;
    function MeasureText(const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal): TFloatRect;

    property Bitmap: TBitmap32 read FBitmap;
    property Renderer: TPolygonRenderer32 read FRenderer write SetRenderer;
    property RendererClassName: string read GetRendererClassName write SetRendererClassName;
    property Brushes: TBrushCollection read FBrushes;
  end;

var
  CBezierTolerance: TFloat = 0.25;
  QBezierTolerance: TFloat = 0.25;

type
  TAddPointEvent = procedure(const Point: TFloatPoint) of object;

implementation

uses
  Math, GR32_Backends, GR32_VectorUtils;

function CubicBezierFlatness(const P1, P2, P3, P4: TFloatPoint): TFloat; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result :=
    Abs(P1.X + P3.X - 2 * P2.X) +
    Abs(P1.Y + P3.Y - 2 * P2.Y) +
    Abs(P2.X + P4.X - 2 * P3.X) +
    Abs(P2.Y + P4.Y - 2 * P3.Y);
end;

function QuadraticBezierFlatness(const P1, P2, P3: TFloatPoint): TFloat; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result :=
    Abs(P1.x + P3.x - 2 * P2.x) +
    Abs(P1.y + P3.y - 2 * P2.y);
end;

procedure CubicBezierCurve(const P1, P2, P3, P4: TFloatPoint;
  const AddPoint: TAddPointEvent; const Tolerance: TFloat);
var
  P12, P23, P34, P123, P234, P1234: TFloatPoint;
begin
  if CubicBezierFlatness(P1, P2, P3, P4) < Tolerance then
    AddPoint(P1)
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

    CubicBezierCurve(P1, P12, P123, P1234, AddPoint, Tolerance);
    CubicBezierCurve(P1234, P234, P34, P4, AddPoint, Tolerance);
  end;
end;

procedure QuadraticBezierCurve(const P1, P2, P3: TFloatPoint; const AddPoint: TAddPointEvent;
  const Tolerance: TFloat);
var
  P12, P23, P123: TFloatPoint;
begin
  if QuadraticBezierFlatness(P1, P2, P3) < Tolerance then
    AddPoint(P1)
  else
  begin
    P12.X := (P1.X + P2.X) * 0.5;
    P12.Y := (P1.Y + P2.Y) * 0.5;
    P23.X := (P2.X + P3.X) * 0.5;
    P23.Y := (P2.Y + P3.Y) * 0.5;
    P123.X := (P12.X + P23.X) * 0.5;
    P123.Y := (P12.Y + P23.Y) * 0.5;

    QuadraticBezierCurve(P1, P12, P123, AddPoint, Tolerance);
    QuadraticBezierCurve(P123, P23, P3, AddPoint, Tolerance);
  end;
end;


//============================================================================//

{ TCustomPath }

constructor TCustomPath.Create;
begin
  inherited;
  FControlPointOrigin := cpNone;
end;

procedure TCustomPath.AddPoint(const Point: TFloatPoint);
begin
end;

procedure TCustomPath.Arc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat);
begin
  Polygon(BuildArc(P, StartAngle, EndAngle, Radius));
end;

procedure TCustomPath.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomPath then
    with TCustomPath(Dest) do
    begin
      FCurrentPoint := Self.FCurrentPoint;
      FLastControlPoint := Self.FLastControlPoint;
      FControlPointOrigin := Self.FControlPointOrigin;
    end
  else
    inherited;
end;

procedure TCustomPath.BeginPath;
begin
end;

procedure TCustomPath.Circle(const Cx, Cy, Radius: TFloat; Steps: Integer);
begin
  Polygon(GR32_VectorUtils.Circle(Cx, Cy, Radius, Steps));
end;

procedure TCustomPath.Circle(const Center: TFloatPoint; Radius: TFloat;
  Steps: Integer);
begin
  Polygon(GR32_VectorUtils.Circle(Center.X, Center.Y, Radius, Steps));
end;

procedure TCustomPath.Clear;
begin
  FControlPointOrigin := cpNone;
end;

procedure TCustomPath.ClosePath;
begin
end;

procedure TCustomPath.ConicTo(const P1, P: TFloatPoint);
begin
  QuadraticBezierCurve(FCurrentPoint, P1, P, AddPoint, QBezierTolerance);
  AddPoint(P);
  FCurrentPoint := P;
  FLastControlPoint := P1;
  FControlPointOrigin := cpConic;
end;

procedure TCustomPath.ConicTo(const X1, Y1, X, Y: TFloat);
begin
  ConicTo(FloatPoint(X1, Y1), FloatPoint(X, Y));
end;

procedure TCustomPath.ConicTo(const X, Y: TFloat);
begin
  ConicTo(FloatPoint(X, Y));
end;

procedure TCustomPath.ConicTo(const P: TFloatPoint);
var
  P1: TFloatPoint;
begin
  if FControlPointOrigin = cpConic then
  begin
    P1.X := FCurrentPoint.X + (FCurrentPoint.X - FLastControlPoint.X);
    P1.Y := FCurrentPoint.Y + (FCurrentPoint.Y - FLastControlPoint.Y);
  end
  else
    P1 := FCurrentPoint;
  ConicTo(P1, P);
end;

procedure TCustomPath.ConicToRelative(const X, Y: TFloat);
begin
  ConicTo(FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.ConicToRelative(const P: TFloatPoint);
begin
  ConicTo(OffsetPoint(P, FCurrentPoint));
end;

procedure TCustomPath.ConicToRelative(const X1, Y1, X, Y: TFloat);
begin
  ConicTo(FloatPoint(FCurrentPoint.X + X1, FCurrentPoint.Y + Y1),
    FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.ConicToRelative(const P1, P: TFloatPoint);
begin
  ConicTo(OffsetPoint(P1, FCurrentPoint), OffsetPoint(P, FCurrentPoint));
end;

procedure TCustomPath.CurveTo(const C1, C2, P: TFloatPoint);
begin
  CubicBezierCurve(FCurrentPoint, C1, C2, P, AddPoint, CBezierTolerance);
  AddPoint(P);
  FCurrentPoint := P;
  FLastControlPoint := C2;
  FControlPointOrigin := cpCubic;
end;

procedure TCustomPath.CurveTo(const X1, Y1, X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(X1, Y1), FloatPoint(X2, Y2), FloatPoint(X, Y));
end;

procedure TCustomPath.CurveTo(const X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(X2, Y2), FloatPoint(X, Y));
end;

procedure TCustomPath.CurveTo(const C2, P: TFloatPoint);
var
  C1: TFloatPoint;
begin
  if FControlPointOrigin = cpCubic then
  begin
    C1.X := FCurrentPoint.X - (FLastControlPoint.X - FCurrentPoint.X);
    C1.Y := FCurrentPoint.Y - (FLastControlPoint.Y - FCurrentPoint.Y);
  end
  else
    C1 := FCurrentPoint;
  CurveTo(C1, C2, P);
end;

procedure TCustomPath.CurveToRelative(const X1, Y1, X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(FCurrentPoint.X + X1, FCurrentPoint.Y + Y1),
    FloatPoint(FCurrentPoint.X + X2, FCurrentPoint.Y + Y2),
    FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.CurveToRelative(const X2, Y2, X, Y: TFloat);
begin
  CurveTo(FloatPoint(FCurrentPoint.X + X2, FCurrentPoint.Y + Y2),
    FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.CurveToRelative(const C1, C2, P: TFloatPoint);
begin
  CurveTo(OffsetPoint(C1, FCurrentPoint), OffsetPoint(C2, FCurrentPoint),
    OffsetPoint(P, FCurrentPoint));
end;

procedure TCustomPath.CurveToRelative(const C2, P: TFloatPoint);
begin
  CurveTo(OffsetPoint(C2, FCurrentPoint), OffsetPoint(P, FCurrentPoint));
end;

procedure TCustomPath.Ellipse(const Cx, Cy, Rx, Ry: TFloat; Steps: Integer);
begin
  Polygon(GR32_VectorUtils.Ellipse(Cx, Cy, Rx, Ry, Steps));
end;

procedure TCustomPath.Ellipse(Rx, Ry: TFloat; Steps: Integer);
begin
  with FCurrentPoint do Ellipse(X, Y, Rx, Ry);
end;

procedure TCustomPath.EndPath;
begin
end;

procedure TCustomPath.HorizontalLineTo(const X: TFloat);
begin
  LineTo(FloatPoint(X, FCurrentPoint.Y));
end;

procedure TCustomPath.HorizontalLineToRelative(const X: TFloat);
begin
  LineTo(FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y));
end;

procedure TCustomPath.LineTo(const X, Y: TFloat);
begin
  LineTo(FloatPoint(X, Y));
end;

procedure TCustomPath.LineTo(const P: TFloatPoint);
begin
  AddPoint(P);
  FCurrentPoint := P;
  FControlPointOrigin := cpNone;
end;

procedure TCustomPath.LineToRelative(const X, Y: TFloat);
begin
  LineTo(FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.LineToRelative(const P: TFloatPoint);
begin
  LineTo(FloatPoint(FCurrentPoint.X + P.X, FCurrentPoint.Y + P.Y));
end;

procedure TCustomPath.MoveTo(const X, Y: TFloat);
begin
  MoveTo(FloatPoint(X, Y));
end;

procedure TCustomPath.MoveToRelative(const X, Y: TFloat);
begin
  MoveTo(FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.MoveToRelative(const P: TFloatPoint);
begin
  MoveTo(FloatPoint(FCurrentPoint.X + P.X, FCurrentPoint.Y + P.Y));
end;

procedure TCustomPath.Rectangle(const Rect: TFloatRect);
begin
  Polygon(GR32_VectorUtils.Rectangle(Rect));
end;

procedure TCustomPath.RoundRect(const Rect: TFloatRect; const Radius: TFloat);
begin
  Polygon(GR32_VectorUtils.RoundRect(Rect, Radius));
end;

procedure TCustomPath.VerticalLineTo(const Y: TFloat);
begin
  LineTo(FloatPoint(FCurrentPoint.X, Y));
end;

procedure TCustomPath.VerticalLineToRelative(const Y: TFloat);
begin
  LineTo(FloatPoint(FCurrentPoint.X, FCurrentPoint.Y + Y));
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

procedure TCustomPath.MoveTo(const P: TFloatPoint);
begin
  FCurrentPoint := P;
  FControlPointOrigin := cpNone;
end;

{ TFlattenedPath }

procedure TFlattenedPath.BeginPath;
begin
  FPath := nil;
  FPoints := nil;
  FPointIndex := 0;
  if Assigned(FOnBeginPath) then
    FOnBeginPath(Self);
end;

procedure TFlattenedPath.EndPath;
begin
  if Assigned(FOnEndPath) then
    FOnEndPath(Self);
end;

procedure TFlattenedPath.Clear;
begin
  inherited;
  FPath := nil;
  FPoints := nil;
  FPointIndex := 0;
end;

procedure TFlattenedPath.ClosePath;
var
  N: Integer;
begin
  if Length(FPoints) <> 0 then
  begin
    FCurrentPoint := FPoints[0];
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
  inherited;
  if Length(FPoints) <> 0 then
    ClosePath;
  AddPoint(P);
end;

procedure TFlattenedPath.Polygon(const APoints: TArrayOfFloatPoint);
var
  I: Integer;
begin
  if Length(APoints) = 0 then
    Exit;

  BeginPath;
  for I := 0 to High(APoints) do
    AddPoint(APoints[I]);
  FCurrentPoint := APoints[High(APoints)];
  ClosePath;
  EndPath;
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

procedure TFlattenedPath.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  inherited;

  if Dest is TFlattenedPath then
    with TFlattenedPath(Dest) do
    begin
      BeginPath;
      SetLength(FPath, Length(Self.FPath));
      for I := 0 to High(Self.FPath) do
      begin
        SetLength(FPath[I], Length(Self.FPath[I]));
        Move(Self.FPath[I, 0], FPath[I, 0], Length(Self.FPath[I]) *
          SizeOf(TFloatPoint));
      end;
      EndPath;
    end;
end;

function TFlattenedPath.GetPoints: TArrayOfFloatPoint;
begin
  Result := Copy(FPoints, 0, FPointIndex);
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

procedure TCustomCanvas.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomCanvas then
    with TCustomCanvas(Dest) do
    begin
      FPath.Assign(Self.FPath);
      FTransformation := Self.FTransformation;
    end
  else
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

procedure TCanvas32.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCanvas32 then
    with TCanvas32(Dest) do
    begin
      FBitmap := Self.Bitmap;
      FRenderer.Assign(Self.FRenderer);
      FBrushes.Assign(Self.FBrushes);
    end;
end;

procedure TCanvas32.BrushCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

constructor TCanvas32.Create(ABitmap: TBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
  FRenderer := GetPolygonRendererClass.Create;
  FRenderer.Bitmap := ABitmap;
  FBrushes := TBrushCollection.Create(Self);
  FBrushes.OnChange := BrushCollectionChangeHandler;
end;

destructor TCanvas32.Destroy;
begin
  FBrushes.Free;
  FRenderer.Free;
  inherited;
end;

procedure TCanvas32.DrawPath;
var
  ClipRect: TFloatRect;
  I: Integer;
  P: TArrayOfFloatPoint;
begin
  ClipRect := FloatRect(Bitmap.ClipRect);
  Renderer.Bitmap := Bitmap;
  P := Path.Points;
  for I := 0 to FBrushes.Count - 1 do
  begin
    with FBrushes[I] do
      if Visible then
      begin
        PolyPolygonFS(Renderer, Path.Path, ClipRect, Transformation, True);
        if Length(P) > 0 then
          PolygonFS(Renderer, P, ClipRect, Transformation, False);
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

function TCanvas32.MeasureText(const DstRect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;
var
  Intf: ITextToPathSupport;
begin
  if Supports(Bitmap.Backend, ITextToPathSupport, Intf) then
    Result := Intf.MeasureText(DstRect, Text, Flags)
  else
    raise Exception.Create(RCStrInpropriateBackend);
end;

procedure TCanvas32.RenderText(const DstRect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
var
  Intf: ITextToPathSupport;
begin
  if Supports(Bitmap.Backend, ITextToPathSupport, Intf) then
    Intf.TextToPath(Path, DstRect, Text, Flags)
  else
    raise Exception.Create(RCStrInpropriateBackend);
end;

procedure TCanvas32.RenderText(X, Y: TFloat; const Text: WideString);
var
  Intf: ITextToPathSupport;
begin
  if Supports(Bitmap.Backend, ITextToPathSupport, Intf) then
    Intf.TextToPath(Path, X, Y, Text)
  else
    raise Exception.Create(RCStrInpropriateBackend);
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
