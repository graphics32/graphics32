unit MainUnit;

{$MODE DELPHI}

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
 * The Original Code is Lion Example
 *
 * The Initial Developer(s) of the Original Code is:
 * Christian-W. Budde <Christian@savioursofsoul.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2012
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, GR32, GR32_Image,
  GR32_Polygons, GR32_Paths, GR32_Brushes, GR32_Transforms, GR32_RangeBars,
  LionData;

{$I GR32.inc}

type
  TFrmTiger = class(TForm)
    CbxClearBackground: TCheckBox;
    GbrAlpha: TGaugeBar;
    GbrWidth: TGaugeBar;
    LblAlpha: TLabel;
    LblStrokeWidth: TLabel;
    PaintBox32: TPaintBox32;
    PnlInteraction: TPanel;
    PnlSampler: TPanel;
    PnlSettings: TPanel;
    RgpBrush: TRadioGroup;
    RgpMouse: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CbxClearBackgroundClick(Sender: TObject);
    procedure GbrAlphaChange(Sender: TObject);
    procedure GbrWidthChange(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32Resize(Sender: TObject);
    procedure RgpBrushClick(Sender: TObject);
  private
    FRenderer: TPolygonRenderer32VPR;
    FTransformation: TAffineTransformation;
    FCenter, FOffset: TFloatPoint;
    FLastSqrDistance: TFloat;
    FLastAngle: TFloat;
    FLastPoint: TPoint;
    FCurrentScale: TFloat;
    FCurrentAngle: TFloat;
    procedure UpdateTransformation;
    procedure PaintBox32PaintAlphaBuffer(Sender: TObject);
    procedure PaintBox32PaintOutlineAlphaBuffer(Sender: TObject);
    procedure UpdateOnPaintBuffer;
  end;

var
  FrmTiger: TFrmTiger;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Types, GR32_Math, GR32_Geometry, GR32_VectorUtils;

procedure TFrmTiger.FormCreate(Sender: TObject);
begin
  FRenderer := TPolygonRenderer32VPR.Create(PaintBox32.Buffer);
  FTransformation := TAffineTransformation.Create;
  FCurrentScale := 1;
  FCurrentAngle := 0;
end;

procedure TFrmTiger.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
  FTransformation.Free;
end;

procedure TFrmTiger.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FCurrentScale := FCurrentScale * Power(2, 0.01 * WheelDelta);
  UpdateTransformation;
end;

procedure TFrmTiger.GbrAlphaChange(Sender: TObject);
begin
  UpdateOnPaintBuffer;
end;

procedure TFrmTiger.GbrWidthChange(Sender: TObject);
begin
  if RgpBrush.ItemIndex = 1 then
    PaintBox32.Invalidate;
end;

procedure TFrmTiger.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PaintBox32.OnMouseMove := PaintBox32MouseMove;
  FLastSqrDistance := SqrDistance(FCenter, FloatPoint(X, Y)) / FCurrentScale;
  FLastAngle := ArcTan2(FCenter.Y - Y, FCenter.X - X) + FCurrentAngle;
  FLastPoint := GR32.Point(X, Y);
end;

procedure TFrmTiger.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Angle, SqrDistance: TFloat;
begin
  if ssLeft in Shift then
  begin
    SqrDistance := GR32_Geometry.SqrDistance(FCenter, FloatPoint(X, Y));
    Angle := ArcTan2(FCenter.Y - Y, FCenter.X - X);

    FCurrentScale := SqrDistance / FLastSqrDistance;
    FCurrentAngle := FLastAngle - Angle;
  end;
  if ssRight in Shift then
  begin
    FCenter.X := FCenter.X + (X - FLastPoint.X);
    FCenter.Y := FCenter.Y + (Y - FLastPoint.Y);

    FLastPoint := GR32.Point(X, Y);
  end;

  UpdateTransformation;
end;

procedure TFrmTiger.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PaintBox32.OnMouseMove := nil;
end;

procedure TFrmTiger.UpdateTransformation;
begin
  // update transformation
  FTransformation.Clear;
  FTransformation.Translate(-FOffset.X, -FOffset.Y);
  FTransformation.Scale(FCurrentScale);
  FTransformation.Rotate(RadToDeg(FCurrentAngle));
  FTransformation.Translate(FCenter.X, FCenter.Y);

  PaintBox32.Invalidate;
end;

procedure TFrmTiger.CbxClearBackgroundClick(Sender: TObject);
begin
  UpdateOnPaintBuffer;
end;

procedure TFrmTiger.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
begin
  PaintBox32.Buffer.Clear($FFFFFFFF);

  for Index := 0 to High(GLion.ColoredPolygons) do
  begin
    FRenderer.Color := GLion.ColoredPolygons[Index].Color;
    FRenderer.PolyPolygonFS(GLion.ColoredPolygons[Index].Polygon,
      FloatRect(PaintBox32.Buffer.ClipRect), FTransformation);
  end;
end;

procedure TFrmTiger.PaintBox32PaintAlphaBuffer(Sender: TObject);
var
  Index: Integer;
  Alpha: Byte;
begin
  if CbxClearBackground.Checked then
    PaintBox32.Buffer.Clear($FFFFFFFF);

  Alpha := GbrAlpha.Position;
  for Index := 0 to High(GLion.ColoredPolygons) do
  begin
    FRenderer.Color := SetAlpha(GLion.ColoredPolygons[Index].Color, Alpha);
    FRenderer.PolyPolygonFS(GLion.ColoredPolygons[Index].Polygon,
      FloatRect(PaintBox32.Buffer.ClipRect), FTransformation);
  end;
end;

procedure TFrmTiger.PaintBox32PaintOutlineAlphaBuffer(Sender: TObject);
var
  Index: Integer;
  Alpha: Byte;
begin
  if CbxClearBackground.Checked then
    PaintBox32.Buffer.Clear($FFFFFFFF);

  Alpha := GbrAlpha.Position;
  for Index := 0 to High(GLion.ColoredPolygons) do
    with GLion.ColoredPolygons[Index] do
    begin
      FRenderer.Color := SetAlpha(Color, Alpha);
      FRenderer.PolyPolygonFS(BuildPolyPolyLine(Polygon, True,
        0.1 * GbrWidth.Position), FloatRect(PaintBox32.Buffer.ClipRect),
        FTransformation);
    end;
end;

procedure TFrmTiger.PaintBox32Resize(Sender: TObject);
begin
  FCenter := FloatPoint(0.5 * PaintBox32.Width, 0.5 * PaintBox32.Height);
  FOffset := FloatPoint(0.5 * (GLion.Bounds.Right - GLion.Bounds.Left),
    0.5 * (GLion.Bounds.Bottom - GLion.Bounds.Top));
  FTransformation.Translate(FCenter.X - FOffset.X, FCenter.Y - FOffset.Y);
end;

procedure TFrmTiger.UpdateOnPaintBuffer;
begin
  case RgpBrush.ItemIndex of
    0:
      if CbxClearBackground.Checked and (GbrAlpha.Position = $FF) then
        PaintBox32.OnPaintBuffer := PaintBox32PaintBuffer
      else
        PaintBox32.OnPaintBuffer := PaintBox32PaintAlphaBuffer;
    1: PaintBox32.OnPaintBuffer := PaintBox32PaintOutlineAlphaBuffer;
  end;
  PaintBox32.Invalidate;
end;

procedure TFrmTiger.RgpBrushClick(Sender: TObject);
begin
  UpdateOnPaintBuffer;
  LblStrokeWidth.Visible := RgpBrush.ItemIndex = 1;
  GbrWidth.Visible := LblStrokeWidth.Visible;
end;

initialization

  SetGamma(1);

end.
