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
    FChanged: boolean;
  protected
    procedure AddPoint(const Point: TFloatPoint); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged; virtual;
  public
    constructor Create; override;
    procedure Clear; virtual;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure Changed; override;

    procedure BeginPath; deprecated; // No longer necessary
    procedure EndPath(Close: boolean = False); virtual;
    procedure ClosePath; deprecated; // Use EndPath(True) instead

    // Movement
    procedure MoveTo(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure MoveTo(const P: TFloatPoint); overload; virtual;
    procedure MoveToRelative(const X, Y: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure MoveToRelative(const P: TFloatPoint); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

    // Lines and Curves
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

    // Polylines
    procedure Arc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat);
    procedure PolyLine(const APoints: TArrayOfFloatPoint); virtual;

    // Closed Polygons
    procedure Rectangle(const Rect: TFloatRect); virtual;
    procedure RoundRect(const Rect: TFloatRect; const Radius: TFloat); virtual;
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
  protected
    function GetPoints: TArrayOfFloatPoint;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AddPoint(const Point: TFloatPoint); override;
    procedure DoBeginPath; virtual;
    procedure DoEndPath; virtual;
    procedure ClearPoints;
  public
    procedure Clear; override;

    procedure EndPath(Close: boolean = False); override;

    // MoveTo* implicitly ends the current path.
    procedure MoveTo(const P: TFloatPoint); override;
    procedure Polygon(const APoints: TArrayOfFloatPoint); override;

    property Points: TArrayOfFloatPoint read GetPoints;
    property Path: TArrayOfArrayOfFloatPoint read FPath;

    property OnBeginPath: TNotifyEvent read FOnBeginPath write FOnBeginPath;
    property OnEndPath: TNotifyEvent read FOnEndPath write FOnEndPath;
  end;

  { TCustomCanvas }
  TCustomCanvas = class(TFlattenedPath)
  private
    FTransformation: TTransformation;
  protected
    procedure SetTransformation(const Value: TTransformation);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged; override;
    procedure DrawPath(const Path: TFlattenedPath); virtual; abstract;
  public
    property Transformation: TTransformation read FTransformation write SetTransformation;
    function Path: TFlattenedPath; deprecated; // No longer necessary
  end;

  { TCanvas32 }
  TCanvas32 = class(TCustomCanvas)
  private
    FBitmap: TBitmap32;
    FRenderer: TPolygonRenderer32;
    FBrushes: TBrushCollection;
  protected
    function GetRendererClassName: string;
    procedure SetRendererClassName(const Value: string);
    procedure SetRenderer(ARenderer: TPolygonRenderer32);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DrawPath(const Path: TFlattenedPath); override;
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
  Math,
  GR32_Backends,
  GR32_VectorUtils;

const
  VertexBufferSizeLow = 256;
  VertexBufferSizeGrow = 128;

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

procedure CubicBezierCurve(const P1, P2, P3, P4: TFloatPoint; const AddPoint: TAddPointEvent; const Tolerance: TFloat);
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

procedure QuadraticBezierCurve(const P1, P2, P3: TFloatPoint; const AddPoint: TAddPointEvent; const Tolerance: TFloat);
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

procedure TCustomPath.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TCustomPath.EndUpdate;
begin
  inherited EndUpdate;

  if (UpdateCount = 0) and (FChanged) then
  begin
    FChanged := False;
    DoChanged;
  end;
end;

procedure TCustomPath.Changed;
begin
  BeginUpdate;
  FChanged := True;
  EndUpdate;
end;

procedure TCustomPath.DoChanged;
begin
  // Execute OnChange event
  inherited Changed;
end;

procedure TCustomPath.AddPoint(const Point: TFloatPoint);
begin
end;

procedure TCustomPath.Arc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat);
begin
  PolyLine(BuildArc(P, StartAngle, EndAngle, Radius));
end;

procedure TCustomPath.AssignTo(Dest: TPersistent);
begin
  if (Dest is TCustomPath) then
  begin
    TCustomPath(Dest).Clear;
    TCustomPath(Dest).FCurrentPoint := FCurrentPoint;
    TCustomPath(Dest).FLastControlPoint := FLastControlPoint;
    TCustomPath(Dest).FControlPointOrigin := FControlPointOrigin;
  end else
    inherited;
end;

procedure TCustomPath.BeginPath;
begin
end;

procedure TCustomPath.Circle(const Cx, Cy, Radius: TFloat; Steps: Integer);
begin
  Polygon(GR32_VectorUtils.Circle(Cx, Cy, Radius, Steps));
end;

procedure TCustomPath.Circle(const Center: TFloatPoint; Radius: TFloat; Steps: Integer);
begin
  Polygon(GR32_VectorUtils.Circle(Center.X, Center.Y, Radius, Steps));
end;

procedure TCustomPath.Clear;
begin
  FControlPointOrigin := cpNone;
end;

procedure TCustomPath.ClosePath;
begin
  EndPath(True);
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
  ConicTo(FloatPoint(FCurrentPoint.X + X1, FCurrentPoint.Y + Y1), FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
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
  CurveTo(FloatPoint(FCurrentPoint.X + X2, FCurrentPoint.Y + Y2), FloatPoint(FCurrentPoint.X + X, FCurrentPoint.Y + Y));
end;

procedure TCustomPath.CurveToRelative(const C1, C2, P: TFloatPoint);
begin
  CurveTo(OffsetPoint(C1, FCurrentPoint), OffsetPoint(C2, FCurrentPoint), OffsetPoint(P, FCurrentPoint));
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

procedure TCustomPath.EndPath(Close: boolean = False);
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
  i: Integer;
begin
  if (Length(APoints) = 0) then
    Exit;

  MoveTo(APoints[0]);
  for i := 1 to High(APoints) do
    LineTo(APoints[i]);
  EndPath(True); // TODO : Was EndPath with no ClosePath...
end;

procedure TCustomPath.PolyLine(const APoints: TArrayOfFloatPoint);
var
  i: Integer;
begin
  if Length(APoints) = 0 then
    Exit;

  BeginUpdate;

  for i := 0 to High(APoints) do
    LineTo(APoints[i]);

  EndUpdate;
end;

procedure TCustomPath.MoveTo(const P: TFloatPoint);
begin
  FCurrentPoint := P;
  FControlPointOrigin := cpNone;
end;

{ TFlattenedPath }

procedure TFlattenedPath.EndPath(Close: boolean = False);
var
  n: Integer;
begin
  if (FPointIndex = 0) then
    exit;

  if (Close) then
    AddPoint(FPoints[0]);

  CurrentPoint := FPoints[0];

  // Grow path list
  n := Length(FPath);
  SetLength(FPath, n + 1);

  // Save vertex buffer in path list
  FPath[n] := Copy(FPoints, 0, FPointIndex);

  ClearPoints;

  DoEndPath;
end;

procedure TFlattenedPath.Clear;
begin
  inherited;

  // Clear path list
  FPath := nil;
  // ...and vertex buffer
  ClearPoints;
end;

procedure TFlattenedPath.ClearPoints;
begin
  // Reset vertex counter...
  FPointIndex := 0;
  // ...but try to be clever about buffer size to minimize
  // reallocation and memory waste
  if (Length(FPoints) > VertexBufferSizeLow) then
    SetLength(FPoints, VertexBufferSizeLow);
  // FPoints := nil;
end;

procedure TFlattenedPath.DoBeginPath;
begin
  EndPath; //implicitly finish a prior path

  if (Assigned(FOnBeginPath)) then
    FOnBeginPath(Self);
end;

procedure TFlattenedPath.DoEndPath;
begin
  if (Assigned(FOnEndPath)) then
    FOnEndPath(Self);

  Changed;
end;

procedure TFlattenedPath.MoveTo(const P: TFloatPoint);
begin
  EndPath;

  inherited;

  AddPoint(P);
end;

procedure TFlattenedPath.Polygon(const APoints: TArrayOfFloatPoint);
begin
  if (Length(APoints) = 0) then
    Exit;

  BeginUpdate;

  PolyLine(APoints);
  EndPath(True);

  CurrentPoint := APoints[High(APoints)];

  EndUpdate;
end;

procedure TFlattenedPath.AddPoint(const Point: TFloatPoint);
var
  n: Integer;
begin
  if (FPointIndex = 0) then
    DoBeginPath;

  // Grow buffer if required
  n := Length(FPoints);
  if (FPointIndex >= n) then
    SetLength(FPoints, n + VertexBufferSizeGrow);

  // Add vertex to buffer
  FPoints[FPointIndex] := Point;
  Inc(FPointIndex);
end;

procedure TFlattenedPath.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if (Dest is TFlattenedPath) then
  begin
    TFlattenedPath(Dest).BeginUpdate;
    try
      inherited;

      TFlattenedPath(Dest).DoBeginPath;
      SetLength(TFlattenedPath(Dest).FPath, Length(FPath));
      for i := 0 to High(FPath) do
      begin
        SetLength(TFlattenedPath(Dest).FPath[i], Length(FPath[i]));
        Move(FPath[i, 0], TFlattenedPath(Dest).FPath[i, 0], Length(FPath[i]) * SizeOf(TFloatPoint));
      end;
      TFlattenedPath(Dest).DoEndPath;

      TFlattenedPath(Dest).Changed;
    finally
      TFlattenedPath(Dest).EndUpdate;
    end;
  end else
    inherited;
end;

function TFlattenedPath.GetPoints: TArrayOfFloatPoint;
begin
  Result := Copy(FPoints, 0, FPointIndex);
end;



{ TCustomCanvas }

procedure TCustomCanvas.AssignTo(Dest: TPersistent);
begin
  if (Dest is TCustomCanvas) then
  begin
    TCustomCanvas(Dest).BeginUpdate;
    inherited;
    TCustomCanvas(Dest).Transformation := FTransformation;
    TCustomCanvas(Dest).EndUpdate;
  end else
    inherited;
end;

procedure TCustomCanvas.DoChanged;
begin
  inherited;

  DrawPath(Self);
  Clear;
end;

function TCustomCanvas.Path: TFlattenedPath;
begin
  Result := Self;
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
  if (Dest is TCanvas32) then
  begin
    TCanvas32(Dest).BeginUpdate;
    inherited;
    TCanvas32(Dest).FBitmap := FBitmap; // TODO : Shouldn't this be .FBitmap.Assign(FBitmap)?
    TCanvas32(Dest).FRenderer.Assign(FRenderer);
    TCanvas32(Dest).FBrushes.Assign(FBrushes);
    TCanvas32(Dest).Changed;
    TCanvas32(Dest).EndUpdate;
  end else
    inherited;
end;

procedure TCanvas32.BrushCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

constructor TCanvas32.Create(ABitmap: TBitmap32);
begin
  if (ABitmap = nil) then
    raise Exception.Create('Bitmap parameter required');

  inherited Create;

  FBitmap := ABitmap;
  FRenderer := GetPolygonRendererClass.Create;
  // No need to set Bitmap here. It's done in DrawPath()
  // FRenderer.Bitmap := ABitmap;
  FBrushes := TBrushCollection.Create(Self);
  FBrushes.OnChange := BrushCollectionChangeHandler;
end;

destructor TCanvas32.Destroy;
begin
  FBrushes.Free;
  FRenderer.Free;
  inherited;
end;

procedure TCanvas32.DrawPath(const Path: TFlattenedPath);
var
  ClipRect: TFloatRect;
  i: Integer;
begin
  if (Length(Path.Path) = 0) then
    exit;

  ClipRect := FloatRect(Bitmap.ClipRect);
  Renderer.Bitmap := Bitmap;

  for i := 0 to FBrushes.Count-1 do
    if FBrushes[i].Visible then
      FBrushes[i].PolyPolygonFS(Renderer, Path.Path, ClipRect, Transformation, True);
end;


class function TCanvas32.GetPolygonRendererClass: TPolygonRenderer32Class;
begin
  Result := DefaultPolygonRendererClass;
end;

function TCanvas32.GetRendererClassName: string;
begin
  Result := FRenderer.ClassName;
end;

function TCanvas32.MeasureText(const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal): TFloatRect;
var
  TextToPath: ITextToPathSupport;
begin
  if (not Supports(Bitmap.Backend, ITextToPathSupport, TextToPath)) then
    raise Exception.Create(RCStrInpropriateBackend);

  Result := TextToPath.MeasureText(DstRect, Text, Flags);
end;

procedure TCanvas32.RenderText(const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal);
var
  TextToPath: ITextToPathSupport;
begin
  if (not Supports(Bitmap.Backend, ITextToPathSupport, TextToPath)) then
    raise Exception.Create(RCStrInpropriateBackend);

  TextToPath.TextToPath(Self, DstRect, Text, Flags);
end;

procedure TCanvas32.RenderText(X, Y: TFloat; const Text: WideString);
var
  TextToPath: ITextToPathSupport;
begin
  if (not Supports(Bitmap.Backend, ITextToPathSupport, TextToPath)) then
    raise Exception.Create(RCStrInpropriateBackend);

  TextToPath.TextToPath(Self, X, Y, Text);
end;

procedure TCanvas32.SetRenderer(ARenderer: TPolygonRenderer32);
begin
  if (ARenderer <> nil) and (FRenderer <> ARenderer) then
  begin
    if (FRenderer <> nil) then
      FRenderer.Free;
    FRenderer := ARenderer;
    Changed;
  end;
end;

procedure TCanvas32.SetRendererClassName(const Value: string);
var
  RendererClass: TPolygonRenderer32Class;
begin
  if (Value <> '') and (FRenderer.ClassName <> Value) and (PolygonRendererList <> nil) then
  begin
    RendererClass := TPolygonRenderer32Class(PolygonRendererList.Find(Value));
    if (RendererClass <> nil) then
      Renderer := RendererClass.Create;
  end;
end;

end.
