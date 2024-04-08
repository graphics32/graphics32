unit MainUnit;

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
 * The Original Code is Curves Example (based on VPR example)
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls,
  GR32,
  GR32_Image,
  GR32_Polygons,
  GR32_Transforms,
  GR32_Paths,
  GR32_Brushes;

type
  // Draws a single dot at each vertex
  TDotBrush = class(TCustomBrush)
  strict private
    FColor: TColor32;
  private
    procedure SetColor(const Value: TColor32);
  protected
    procedure RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); override;
  public
    constructor Create(BrushCollection: TBrushCollection); override;

    property Color: TColor32 read FColor write SetColor;
  end;

type
  // Draws a circle, using the nested brushes, at each vertex
  TCircleBrush = class(TNestedBrush)
  strict private
    FRadius: TFloat;
  private
    procedure SetRadius(const Value: TFloat);
  protected
  public
    constructor Create(BrushCollection: TBrushCollection); override;

    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); override;
    procedure PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray); override;

    property Radius: TFloat read FRadius write SetRadius;
  end;

type
  TMainForm = class(TForm)
    Paintbox: TPaintBox32;
    Panel1: TPanel;
    BtnDrawCurve: TButton;
    CbxUpdate: TCheckBox;
    procedure BtnDrawCurveClick(Sender: TObject);
    procedure CbxUpdateClick(Sender: TObject);
    procedure ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
    procedure PaintboxClick(Sender: TObject);
    procedure PaintboxPaintBuffer(Sender: TObject);
  private
    FCanvas32: TCanvas32;
    FCurveBrushes: TNestedBrush;
    FPointsBrushes: TNestedBrush;

    FBrushFill: TSolidBrush;
    FBrushDash: TDashedBrush;
    FBrushDot: TDotBrush;
    FBrushCircle: TCircleBrush;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math,
  Types,
  GR32_Math,
  GR32_Geometry,
  GR32_VectorUtils,
  GR32_Resamplers,
  GR32_LowLevel;

constructor TDotBrush.Create(BrushCollection: TBrushCollection);
begin
  inherited;
  FColor := clWhite32;
end;

procedure TDotBrush.RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation);
var
  i, j: integer;
  Bitmap: TCustomBitmap32;
begin
  Bitmap := (Renderer as TPolygonRenderer32).Bitmap;
  for i := 0 to High(Points) do
    for j := 0 to High(Points[i]) do
      Bitmap.PixelFS[Points[i, j].X, Points[i, j].Y] := FColor;
end;

procedure TDotBrush.SetColor(const Value: TColor32);
begin
  if (FColor = Value) then
    exit;
  FColor := Value;
  Changed;
end;

constructor TCircleBrush.Create(BrushCollection: TBrushCollection);
begin
  inherited;
  FRadius := 2.0;
end;

procedure TCircleBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
var
  CirclePoints: TArrayOfFloatPoint;
  Center, LastCenter, Delta: TFloatPoint;
  i, j, k: integer;
begin
  LastCenter := FloatPoint(0, 0);
  CirclePoints := Circle(LastCenter, FRadius);

  for i := 0 to High(Points) do
    for j := 0 to High(Points[i]) do
    begin
      Center := Points[i, j];

      Delta := Center - LastCenter;

      // Translate circle to new center in-place
      for k := 0 to High(CirclePoints) do
        CirclePoints[k] := CirclePoints[k] + Delta;

      inherited PolyPolygonFS(Renderer, [CirclePoints], ClipRect, Transformation, True);

      LastCenter := Center;
    end;
end;

procedure TCircleBrush.PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray);
begin
  PolyPolygonFS(Renderer, Points, ClipRect, Transformation, True);
end;

procedure TCircleBrush.SetRadius(const Value: TFloat);
begin
  if (FRadius = Value) then
    exit;
  FRadius := Value;
  Changed;
end;

function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean): TArrayOfFloatPoint;
const
  TOLERANCE: TFloat = 20.0;
  THRESHOLD: TFloat = 0.5;
var
  I, H, R: Integer;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;

  procedure AddPoint(const P: TFloatPoint);
  var
    L: Integer;
  begin
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L] := P;
  end;

  function GetPoint(I: Integer; t: TFloat = 0.0): TFloatPoint;
  var
    f, Index: Integer;
    W: TFloat;
  begin
    Result.X := 0; Result.Y := 0;
    for f := -R to R do
    begin
      Index := WrapProc(I - f, H);
      W := Filter(f + t);
      Result.X := Result.X + W * Points[Index].X;
      Result.Y := Result.Y + W * Points[Index].Y;
    end;
  end;

  procedure Recurse(I: Integer; const P1, P2: TFloatPoint; const t1, t2: TFloat);
  var
    Temp: TFloat;
    P: TFloatPoint;
  begin
    AddPoint(P1);
    Temp := (t1 + t2) * 0.5;
    P := GetPoint(I, Temp);

    if (Abs(CrossProduct(FloatPoint(P1.X - P.X, P1.Y - P.Y),
      FloatPoint(P.X - P2.X, P.Y - P2.Y))) > TOLERANCE) or (t2 - t1 >= THRESHOLD) then
    begin
      Recurse(I, P1, P, t1, Temp);
      Recurse(I, P, P2, Temp, t2);
    end
    else AddPoint(P);
  end;

const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  SetLength(Result, 0);

  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  for I := 0 to H - 1 do
    Recurse(I, GetPoint(I), GetPoint(I + 1), 0, 1);

  if Closed then
    Recurse(H, GetPoint(H), GetPoint(0), 0, 1)
  else
    AddPoint(GetPoint(H));
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  BrushCircleFill: TSolidBrush;
  BrushCircleStroke: TStrokeBrush;
begin
  inherited;

  FCanvas32 := TCanvas32.Create(Paintbox.Buffer);

  FCurveBrushes := TNestedBrush(FCanvas32.Brushes.Add(TNestedBrush));
  FPointsBrushes := TNestedBrush(FCanvas32.Brushes.Add(TNestedBrush));


  FBrushFill := TSolidBrush(FCurveBrushes.Brushes.Add(TSolidBrush));
  FBrushFill.FillColor := clIndianRed32;
  FBrushFill.FillMode := pfEvenOdd;

  FBrushDash := TDashedBrush(FCurveBrushes.Brushes.Add(TDashedBrush));
  FBrushDash.FillColor := clWhite32;
  FBrushDash.StrokeWidth := 6;
  FBrushDash.DashArray := [10, 5];
  FBrushDash.Visible := False;

  FBrushDot := TDotBrush(FCurveBrushes.Brushes.Add(TDotBrush));
  FBrushDot.Color := clLime32;


  FBrushCircle := TCircleBrush(FPointsBrushes.Brushes.Add(TCircleBrush));
  FBrushCircle.Radius := 4;

  BrushCircleFill := TSolidBrush(FBrushCircle.Brushes.Add(TSolidBrush));
  BrushCircleFill.FillColor := clBlue32;

  BrushCircleStroke := TStrokeBrush(FBrushCircle.Brushes.Add(TStrokeBrush));
  BrushCircleStroke.FillColor := clWhite32;
  BrushCircleStroke.StrokeWidth := 1.5;
end;

destructor TMainForm.Destroy;
begin
  FCanvas32.Free;
  inherited;
end;

procedure TMainForm.PaintboxClick(Sender: TObject);
begin
  FBrushDash.Visible := not FBrushDash.Visible;
  Paintbox.ForceFullInvalidate;
end;

procedure TMainForm.BtnDrawCurveClick(Sender: TObject);
begin
  Paintbox.ForceFullInvalidate;
end;

procedure TMainForm.PaintboxPaintBuffer(Sender: TObject);
var
  Points, Curve: TArrayOfFloatPoint;
  I: Integer;
  K: TCustomKernel;
begin
  Paintbox.Buffer.Clear($FF333333);
  SetLength(Points, 8);

  // Create a set of random data points
  for I := 0 to High(Points) do
    Points[I] := FloatPoint(Random(Paintbox.Buffer.Width), Random(Paintbox.Buffer.Height));

  // Create interpolation kernel
  K := TGaussianKernel.Create;
  try
    // Subdivide recursively and interpolate
    Curve := MakeCurve(Points, K, True);
  finally
    K.Free;
  end;

  // Draw result polygon
  FCanvas32.BeginUpdate;
  try
    FCurveBrushes.Visible := True;
    FPointsBrushes.Visible := False;

    FCanvas32.Polygon(Curve);
  finally
    FCanvas32.EndUpdate;
  end;

  // Draw control points
  FCanvas32.BeginUpdate;
  try
    FCurveBrushes.Visible := False;
    FPointsBrushes.Visible := True;

    FCanvas32.Polygon(Points);
  finally
    FCanvas32.EndUpdate;
  end;
end;

procedure TMainForm.ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
begin
  Paintbox.ForceFullInvalidate;
end;

procedure TMainForm.CbxUpdateClick(Sender: TObject);
begin
  if CbxUpdate.Checked then
    Application.OnIdle := ApplicationIdleHandler
  else
    Application.OnIdle := nil;
end;

end.
