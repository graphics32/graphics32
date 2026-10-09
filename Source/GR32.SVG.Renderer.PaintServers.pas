unit GR32.SVG.Renderer.PaintServers;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Math, Types,
  GR32,
  GR32_Polygons,
  GR32.SVG.Types,
  GR32.SVG.Tree,
  GR32.SVG.Renderer;


//------------------------------------------------------------------------------
//
//      TPaintServerRenderer
//
//------------------------------------------------------------------------------
// Abstract base class for paint server renderers
//------------------------------------------------------------------------------
type
  TPaintServerRenderer = class abstract
  public
    class function CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller; virtual;
  end;

  TPaintServerRendererClass = class of TPaintServerRenderer;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererPattern
//
//------------------------------------------------------------------------------
// Painter server renderer for TSvgPatternNode
//------------------------------------------------------------------------------
type
  TPaintServerRendererPattern = class(TPaintServerRenderer)
  public
    class function CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller; override;
  end;

  TPaintServerRendererPatternClass = class of TPaintServerRendererPattern;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererGradient
//
//------------------------------------------------------------------------------
// Painter server renderer base class for TSvgGradientNode
//------------------------------------------------------------------------------
type
  TPaintServerRendererGradient = class abstract(TPaintServerRenderer)
  protected const
    WrapMode: array[TSvgSpreadMethod] of TWrapMode = (wmClamp, wmReflect, wmRepeat);
  protected
    class function DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller; virtual; abstract;
  public
    class function CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller; override;
  end;

  TPaintServerRendererGradientClass = class of TPaintServerRendererGradient;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererLinearGradient
//
//------------------------------------------------------------------------------
// Painter server renderer for TSvgLinearGradientNode
//------------------------------------------------------------------------------
type
  TPaintServerRendererLinearGradient = class abstract(TPaintServerRendererGradient)
  protected
    class function DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller; override;
  end;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererRadialGradient
//
//------------------------------------------------------------------------------
// Painter server renderer for TSvgRadialGradientNode
//------------------------------------------------------------------------------
type
  TPaintServerRendererRadialGradient = class abstract(TPaintServerRendererGradient)
  protected
    class function DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller; override;
  end;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererConicalGradient
//
//------------------------------------------------------------------------------
// Painter server renderer for TSvgConicalGradientNode
//------------------------------------------------------------------------------
type
  TPaintServerRendererConicalGradient = class abstract(TPaintServerRendererGradient)
  protected
    class function DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller; override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_LowLevel,
  GR32_Math,
  GR32_Transforms,
  GR32_Resamplers,
  GR32_VectorUtils,
  GR32_ColorGradients;

const
  OneOver255: Single = 1 / 255;

//------------------------------------------------------------------------------
//
//      TPaintServerRenderer
//
//------------------------------------------------------------------------------
class function TPaintServerRenderer.CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
begin
  Result := nil;
end;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererGradient
//
//------------------------------------------------------------------------------

class function TPaintServerRendererGradient.CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
begin
  Result := nil;
  if (ANode = nil) or (TSvgGradientNode(ANode).Stops.Count = 0) then
    Exit;

  Result := DoCreateFiller(ARenderer, TSvgGradientNode(ANode), ABounds, AOpacity);
end;

//------------------------------------------------------------------------------
//
//      TPaintServerRendererLinearGradient
//
//------------------------------------------------------------------------------

class function TPaintServerRendererLinearGradient.DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
var
  i: Integer;
  Stop: TSvgGradientStop;
  StopColor: TColor32;
  BoundsWidth, BoundsHeight: Single;
  LinearNode: TSvgLinearGradientNode;
  LinearFiller: TLinearGradientPolygonFiller;
  Transform, TotalTransform, BboxMat: TFloatMatrixHelper;
  PointStart, PointEnd: TFloatPoint;
begin
  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  Transform.Matrix := AGradNode.Transform;

  LinearNode := TSvgLinearGradientNode(AGradNode);

  if LinearNode.GradientUnits = guObjectBoundingBox then
  begin
    PointStart.X := LinearNode.X1.ToPixels(1.0);
    PointStart.Y := LinearNode.Y1.ToPixels(1.0);
    PointEnd.X := LinearNode.X2.ToPixels(1.0);
    PointEnd.Y := LinearNode.Y2.ToPixels(1.0);

    BboxMat.Matrix := IdentityMatrix;
    BboxMat.Scale(BoundsWidth, BoundsHeight);
    BboxMat.Translate(ABounds.Left, ABounds.Top);
    // Transform gradient coordinates in normalized [0..1] space before mapping to bounding box
    TotalTransform := Transform * BboxMat;
  end else
  begin
    PointStart.X := LinearNode.X1.ToPixels(ARenderer.ViewportRect.Width);
    PointStart.Y := LinearNode.Y1.ToPixels(ARenderer.ViewportRect.Height);
    PointEnd.X := LinearNode.X2.ToPixels(ARenderer.ViewportRect.Width);
    PointEnd.Y := LinearNode.Y2.ToPixels(ARenderer.ViewportRect.Height);
    TotalTransform := Transform * ARenderer.Transformation.Matrix;
  end;

  if (not TotalTransform.IsIdentity) then
  begin
    PointStart := TotalTransform.TransformPoint(PointStart);
    PointEnd := TotalTransform.TransformPoint(PointEnd);
  end;

  LinearFiller := TLinearGradientPolygonFiller.Create;
  try
    LinearFiller.StartPoint := PointStart;
    LinearFiller.EndPoint := PointEnd;
    LinearFiller.WrapMode := WrapMode[AGradNode.SpreadMethod];

    LinearFiller.Gradient.ClearColorStops;
    for i := 0 to AGradNode.Stops.Count - 1 do
    begin
      Stop := AGradNode.Stops[i];
      StopColor := Stop.Color.Color;
      if (Stop.Opacity < 1.0) or (AOpacity < 1.0) then
        ScaleAlpha(StopColor, Stop.Opacity * AOpacity);
      LinearFiller.Gradient.AddColorStop(Stop.Offset, StopColor);
    end;

  except
    LinearFiller.Free;
    raise;
  end;

  Result := LinearFiller;
end;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererRadialGradient
//
//------------------------------------------------------------------------------
class function TPaintServerRendererRadialGradient.DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
var
  i: Integer;
  Stop: TSvgGradientStop;
  StopColor: TColor32;
  BoundsWidth, BoundsHeight: Single;
  RadialNode: TSvgRadialGradientNode;
  cx, cy, r, fx, fy, rx, ry, ScaleX, ScaleY: Single;
  RadialFiller: TSVGRadialGradientPolygonFiller;
  Transform, TotalTransform, BboxMat: TFloatMatrixHelper;
  PointC, PointF: TFloatPoint;
begin
  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  Transform.Matrix := AGradNode.Transform;

  RadialNode := TSvgRadialGradientNode(AGradNode);

  if RadialNode.GradientUnits = guObjectBoundingBox then
  begin
    cx := RadialNode.Cx.ToPixels(1.0);
    cy := RadialNode.Cy.ToPixels(1.0);
    r := RadialNode.R.ToPixels(1.0);
    fx := RadialNode.Fx.ToPixels(1.0);
    fy := RadialNode.Fy.ToPixels(1.0);

    BboxMat.Matrix := IdentityMatrix;
    BboxMat.Scale(BoundsWidth, BoundsHeight);
    BboxMat.Translate(ABounds.Left, ABounds.Top);
    // Transform gradient coordinates in normalized [0..1] space before mapping to bounding box
    TotalTransform := Transform * BboxMat;
  end else
  begin
    cx := RadialNode.Cx.ToPixels(ARenderer.ViewportRect.Width);
    cy := RadialNode.Cy.ToPixels(ARenderer.ViewportRect.Height);
    r := RadialNode.R.ToPixels(GR32_Math.Hypot(ARenderer.ViewportRect.Width, ARenderer.ViewportRect.Height) * Sqrt(0.5));
    fx := RadialNode.Fx.ToPixels(ARenderer.ViewportRect.Width);
    fy := RadialNode.Fy.ToPixels(ARenderer.ViewportRect.Height);
    TotalTransform := Transform * ARenderer.Transformation.Matrix;
  end;

  PointC := FloatPoint(cx, cy);
  PointF := FloatPoint(fx, fy);

  rx := r;
  ry := r;

  if (not TotalTransform.IsIdentity) then
  begin
    PointC := TotalTransform.TransformPoint(PointC);
    PointF := TotalTransform.TransformPoint(PointF);

    ScaleX := GR32_Math.Hypot(TotalTransform.Matrix[0, 0], TotalTransform.Matrix[0, 1]);
    ScaleY := GR32_Math.Hypot(TotalTransform.Matrix[1, 0], TotalTransform.Matrix[1, 1]);
    if ScaleX > 0 then
      rx := rx * ScaleX;
    if ScaleY > 0 then
      ry := ry * ScaleY;
  end;

  RadialFiller := TSVGRadialGradientPolygonFiller.Create;
  try
    RadialFiller.EllipseBounds := FloatRect(PointC.X - rx, PointC.Y - ry, PointC.X + rx, PointC.Y + ry);
    RadialFiller.FocalPoint := PointF;
    RadialFiller.WrapMode := WrapMode[AGradNode.SpreadMethod];

    RadialFiller.Gradient.ClearColorStops;
    for i := 0 to AGradNode.Stops.Count - 1 do
    begin
      Stop := AGradNode.Stops[i];
      StopColor := Stop.Color.Color;
      if (Stop.Opacity < 1.0) or (AOpacity < 1.0) then
        ScaleAlpha(StopColor, Stop.Opacity * AOpacity);
      RadialFiller.Gradient.AddColorStop(Stop.Offset, StopColor);
    end;
  except
    RadialFiller.Free;
    raise;
  end;

  Result := RadialFiller;
end;

//------------------------------------------------------------------------------
//
//      TPaintServerRendererConicalGradient
//
//------------------------------------------------------------------------------
class function TPaintServerRendererConicalGradient.DoCreateFiller(ARenderer: TSvgRenderer; AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
var
  i: Integer;
  Stop: TSvgGradientStop;
  StopColor: TColor32;
  BoundsWidth, BoundsHeight: Single;
  ConicalNode: TSvgConicalGradientNode;
  cx, cy: Single;
  ConicalFiller: TSVGConicalGradientPolygonFiller;
  Transform, TotalTransform, BboxMat: TFloatMatrixHelper;
begin
  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  Transform.Matrix := AGradNode.Transform;

  ConicalNode := TSvgConicalGradientNode(AGradNode);

  if ConicalNode.GradientUnits = guObjectBoundingBox then
  begin
    cx := ConicalNode.Cx.ToPixels(1.0);
    cy := ConicalNode.Cy.ToPixels(1.0);

    BboxMat.Matrix := IdentityMatrix;
    BboxMat.Scale(BoundsWidth, BoundsHeight);
    BboxMat.Translate(ABounds.Left, ABounds.Top);
    // Transform gradient coordinates in normalized [0..1] space before mapping to bounding box
    TotalTransform := Transform * BboxMat;
  end else
  begin
    cx := ConicalNode.Cx.ToPixels(ARenderer.ViewportRect.Width);
    cy := ConicalNode.Cy.ToPixels(ARenderer.ViewportRect.Height);
    TotalTransform := Transform * ARenderer.Transformation.Matrix;
  end;

  ConicalFiller := TSVGConicalGradientPolygonFiller.Create;
  try
    ConicalFiller.Center := FloatPoint(cx, cy);
    ConicalFiller.Angle := DegToRad(ConicalNode.Angle);
    ConicalFiller.StartAngle := DegToRad(ConicalNode.StartAngle);
    ConicalFiller.EndAngle := DegToRad(ConicalNode.EndAngle);
    ConicalFiller.TransformMatrix := TotalTransform.Matrix;
    ConicalFiller.WrapMode := WrapMode[AGradNode.SpreadMethod];

    ConicalFiller.Gradient.ClearColorStops;
    for i := 0 to AGradNode.Stops.Count - 1 do
    begin
      Stop := AGradNode.Stops[i];
      StopColor := Stop.Color.Color;
      if (Stop.Opacity < 1.0) or (AOpacity < 1.0) then
        ScaleAlpha(StopColor, Stop.Opacity * AOpacity);
      ConicalFiller.Gradient.AddColorStop(Stop.Offset, StopColor);
    end;
  except
    ConicalFiller.Free;
    raise;
  end;

  Result := ConicalFiller;
end;


//------------------------------------------------------------------------------
//
//      TPaintServerRendererPattern
//
//------------------------------------------------------------------------------
class function TPaintServerRendererPattern.CreateFiller(ARenderer: TSvgRenderer; ANode: TSvgGroupNode;
  const ABounds: TFloatRect; AOpacity: Single): TCustomPolygonFiller;
var
  PatternNode: TSvgPatternNode;
  BoundsWidth, BoundsHeight, ViewportWidth, ViewportHeight: Single;
  TileX, TileY, TileWidth, TileHeight, TileRatioX, TileRatioY: Single;
  PatternBitmap: TBitmap32;
  i, BitmapWidth, BitmapHeight: Integer;
  SavedViewport: TFloatRect;
  ContentMat: TFloatMatrixHelper;
  origPt: TFloatPoint;
  TileViewBox: TSvgViewBox;
  MatrixScaleX, MatrixScaleY: Single;
  PatternTransform: TFloatMatrixHelper;
  TotalTransform: TFloatMatrixHelper;
  InvMat: TFloatMatrix;
  PatternFiller: TSvgPatternPolygonFiller;
const
  cMaxPatternDimension = 4096;
begin
  Result := nil;
  if (ANode = nil) or (ANode.Children.Count = 0) then
    Exit;

  PatternNode := TSvgPatternNode(ANode);

  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  ViewportWidth := ARenderer.ViewportRect.Width;
  ViewportHeight := ARenderer.ViewportRect.Height;
  if ViewportWidth <= 0 then
    ViewportWidth := 1.0;
  if ViewportHeight <= 0 then
    ViewportHeight := 1.0;

  PatternTransform.Matrix := PatternNode.PatternTransform;
  TotalTransform := PatternTransform * ARenderer.Transformation.Matrix;

  // Calculates pattern tile origin and bounds in user space.
  // When patternUnits = guObjectBoundingBox (default), tile attributes x, y, width, height
  // are defined in normalized bounding box units [0..1] relative to target bounds in user space.
  if PatternNode.PatternUnits = guObjectBoundingBox then
  begin
    InvMat := ARenderer.Transformation.Matrix;
    GR32_Transforms.Invert(InvMat);

    origPt := TFloatMatrixHelper(InvMat).TransformPoint(FloatPoint(ABounds.Left, ABounds.Top));

    MatrixScaleX := GR32_Math.Hypot(ARenderer.Transformation.Matrix[0, 0], ARenderer.Transformation.Matrix[0, 1]);
    MatrixScaleY := GR32_Math.Hypot(ARenderer.Transformation.Matrix[1, 0], ARenderer.Transformation.Matrix[1, 1]);
    if MatrixScaleX <= 0 then MatrixScaleX := 1.0;
    if MatrixScaleY <= 0 then MatrixScaleY := 1.0;

    TileRatioX := BoundsWidth / MatrixScaleX;
    TileRatioY := BoundsHeight / MatrixScaleY;
    TileWidth := PatternNode.Width.ToPixels(1.0) * TileRatioX;
    TileHeight := PatternNode.Height.ToPixels(1.0) * TileRatioY;
    TileX := origPt.X + PatternNode.X.ToPixels(1.0) * TileRatioX;
    TileY := origPt.Y + PatternNode.Y.ToPixels(1.0) * TileRatioY;
  end else
  begin
    TileX := PatternNode.X.ToPixels(ViewportWidth);
    TileY := PatternNode.Y.ToPixels(ViewportHeight);
    TileWidth := PatternNode.Width.ToPixels(ViewportWidth);
    TileHeight := PatternNode.Height.ToPixels(ViewportHeight);
  end;

  if (TileWidth <= 0) or (TileHeight <= 0) then
    Exit;

  MatrixScaleX := GR32_Math.Hypot(TotalTransform.Matrix[0, 0], TotalTransform.Matrix[0, 1]);
  MatrixScaleY := GR32_Math.Hypot(TotalTransform.Matrix[1, 0], TotalTransform.Matrix[1, 1]);
  if MatrixScaleX <= 0 then
    MatrixScaleX := 1.0;
  if MatrixScaleY <= 0 then
    MatrixScaleY := 1.0;

  BitmapWidth := Min(cMaxPatternDimension, Max(1, Round(TileWidth * MatrixScaleX)));
  BitmapHeight := Min(cMaxPatternDimension, Max(1, Round(TileHeight * MatrixScaleY)));

  PatternBitmap := TBitmap32.Create;
  try
    PatternBitmap.SetSize(BitmapWidth, BitmapHeight);
    PatternBitmap.DrawMode := dmBlend;

    ARenderer.Transformation.Push;
    try
      SavedViewport := ARenderer.ViewportRect;

      ARenderer.Transformation.Clear;
      ARenderer.ViewportRect := FloatRect(0, 0, BitmapWidth, BitmapHeight);

      // Sets up ContentMat to render pattern child geometry onto offscreen tile bitmap
      ContentMat.Matrix := IdentityMatrix;
      if PatternNode.ViewBox.IsValid then
      begin
        TileViewBox := PatternNode.ViewBox;
        ContentMat.Matrix := TileViewBox.GetTransform(FloatRect(0, 0, BitmapWidth, BitmapHeight), PatternNode.PreserveAspectRatio);
      end else
      if PatternNode.PatternContentUnits = guObjectBoundingBox then
      begin
        TileRatioX := BitmapWidth / TileWidth;
        TileRatioY := BitmapHeight / TileHeight;
        ContentMat.Scale(BoundsWidth * TileRatioX, BoundsHeight * TileRatioY);
        ContentMat.Translate(-TileX * TileRatioX, -TileY * TileRatioY);
      end else
      begin
        TileRatioX := BitmapWidth / TileWidth;
        TileRatioY := BitmapHeight / TileHeight;
        ContentMat.Scale(TileRatioX, TileRatioY);
        ContentMat.Translate(-TileX * TileRatioX, -TileY * TileRatioY);
      end;

      ARenderer.ApplyMatrix(ContentMat.Matrix);

      for i := 0 to PatternNode.Children.Count - 1 do
        ARenderer.RenderNode(PatternBitmap, PatternNode.Children[i]);

    finally
      ARenderer.Transformation.Pop;
      ARenderer.ViewportRect := SavedViewport;
    end;

    if (AOpacity < 1.0) then
      PatternBitmap.MasterAlpha := Round(AOpacity * 255);

    PatternFiller := TSvgPatternPolygonFiller.Create(PatternBitmap);

    if IsIdentityMatrix(TotalTransform.Matrix) then
    begin
      // Pattern can be blitted 1:1 by filler
      PatternFiller.AffineTransform := False;
      PatternFiller.OffsetX := Round(TileX);
      PatternFiller.OffsetY := Round(TileY);
    end else
    begin
      // Pattern must be transformed and sampled by filler
      InvMat := TotalTransform.Matrix;
      GR32_Transforms.Invert(InvMat);

      PatternFiller.AffineTransform := True;
      PatternFiller.InvMatrix := InvMat;
      PatternFiller.TileX := TileX;
      PatternFiller.TileY := TileY;
      PatternFiller.TileWidth := TileWidth;
      PatternFiller.TileHeight := TileHeight;
      PatternFiller.ScaleBmpX := BitmapWidth / TileWidth;
      PatternFiller.ScaleBmpY := BitmapHeight / TileHeight;
    end;

    Result := PatternFiller;
  except
    PatternBitmap.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure RegisterRenderers;
begin
  TSvgPatternNode.RegisterRenderClass(TPaintServerRendererPattern);

  TSvgLinearGradientNode.RegisterRenderClass(TPaintServerRendererLinearGradient);
  TSvgRadialGradientNode.RegisterRenderClass(TPaintServerRendererRadialGradient);
  TSvgConicalGradientNode.RegisterRenderClass(TPaintServerRendererConicalGradient);
end;

initialization
  RegisterRenderers;
end.
