unit GR32.SVG.Renderer;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Generics.Collections,
  GR32, GR32_Transforms, GR32_Polygons, GR32_VectorUtils, GR32_ColorGradients,
  GR32.SVG.Types, GR32.SVG.Tree;

type
  { TSvgBitmapPool: Reusable pool of intermediate TBitmap32 offscreen surfaces to eliminate
    frequent heap allocations/deallocations during nested group opacity, clip path, and mask compositing. }
  TSvgBitmapPool = class(TObject)
  private
    FPool: TObjectList<TCustomBitmap32>;
    FBitmapMaxExcess: NativeInt;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure Release(ABitmap: TCustomBitmap32);
    procedure Clear;
    property BitmapMaxExcess: NativeInt read FBitmapMaxExcess write FBitmapMaxExcess;
  end;

  TSvgPatternPolygonFiller = class(TBitmapPolygonFiller)
  private
    FPatternBmp: TBitmap32;
  public
    constructor Create(APatternBmp: TBitmap32); reintroduce;
    destructor Destroy; override;
    property PatternBmp: TBitmap32 read FPatternBmp;
  end;

  TSvgRenderer = class(TObject)
  private
    FTarget: TCustomBitmap32;
    FMatrixStack: TList<TFloatMatrix>;
    FCurrentMatrix: TFloatMatrix;
    FViewportRect: TFloatRect;
    FDocumentRoot: TSvgDocumentNode;
    FBitmapPool: TSvgBitmapPool;
  protected
    procedure RenderPathNode(APathNode: TSvgPathNode); virtual;
    procedure RenderGroupNode(AGroupNode: TSvgGroupNode); virtual;
    procedure RenderClipPathNode(AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; AMaskBmp: TCustomBitmap32); virtual;
    procedure RenderMaskNode(AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; AMaskBmp: TCustomBitmap32); virtual;
    procedure RenderMarker(AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single); virtual; // Angle is in radians!
    procedure RenderMarkers(APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single); virtual;
    function GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
    function GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
    function CreateGradientFiller(AGradNode: TSvgGradientNode; const ABounds: TFloatRect): TCustomPolygonFiller;
    function CreatePatternFiller(APatternNode: TSvgPatternNode; const ABounds: TFloatRect): TCustomPolygonFiller;
    function ExtractUrlId(const AUrlStr: string): string;
    function GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure ReleaseOffscreenBitmap(ABitmap: TCustomBitmap32);
  public
    constructor Create(ATarget: TCustomBitmap32 = nil); virtual;
    destructor Destroy; override;

    procedure PushMatrix;
    procedure PopMatrix;
    procedure ApplyMatrix(const AMatrix: TFloatMatrix);

    procedure RenderDocument(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect); overload;
    procedure RenderDocument(ADoc: TSvgDocumentNode); overload;
    procedure RenderNode(ANode: TSvgNode); virtual;

    property Target: TCustomBitmap32 read FTarget write FTarget;
    property CurrentMatrix: TFloatMatrix read FCurrentMatrix write FCurrentMatrix;
    property ViewportRect: TFloatRect read FViewportRect write FViewportRect;
  end;

implementation

uses
  Types,
  Math,
  GR32_Blend,
  GR32_Math,
  GR32_LowLevel,
  GR32_Backends_Generic;

function GetMatrixScale(const AMatrix: TFloatMatrix): Single;
var
  det: Single;
begin
  det := AMatrix[0, 0] * AMatrix[1, 1] - AMatrix[0, 1] * AMatrix[1, 0];
  Result := Sqrt(Abs(det));
  if Result <= 0 then
    Result := 1.0;
end;

function IsClosedContour(const AContour: TArrayOfFloatPoint): Boolean;
var
  len: Integer;
begin
  len := Length(AContour);
  if len < 3 then
    Exit(False);
  Result := (Abs(AContour[0].X - AContour[len - 1].X) < 0.001) and
            (Abs(AContour[0].Y - AContour[len - 1].Y) < 0.001);
end;

{ TSvgPatternPolygonFiller }

constructor TSvgPatternPolygonFiller.Create(APatternBmp: TBitmap32);
begin
  inherited Create;
  FPatternBmp := APatternBmp;
  Pattern := FPatternBmp;
end;

destructor TSvgPatternPolygonFiller.Destroy;
begin
  FPatternBmp.Free;
  inherited Destroy;
end;

{ TSvgBitmapPool }

constructor TSvgBitmapPool.Create;
begin
  inherited Create;
  FPool := TObjectList<TCustomBitmap32>.Create(True);
end;

destructor TSvgBitmapPool.Destroy;
begin
  FPool.Free;
  inherited Destroy;
end;

function TSvgBitmapPool.Acquire(AWidth, AHeight: Integer; AClear: Boolean): TCustomBitmap32;
var
  i, BestIndex: Integer;
  TargetSize, CandidateSize, Delta, BestDelta: Integer;
  Candidate: TCustomBitmap32;
begin
  Result := nil;
  TargetSize := AWidth * AHeight;
  BestDelta := MaxInt;
  BestIndex := -1;

  // Search pool for Candidate surface with existing buffer dimensions sufficient
  // for requested bounds (Width >= AWidth, Height >= AHeight) to avoid costly
  // buffer reallocations. Pick Candidate with smallest excess area (BestDelta).
  for i := 0 to FPool.Count - 1 do
  begin
    Candidate := FPool[i];

    CandidateSize := Candidate.Width * Candidate.Height;
    Delta := CandidateSize - TargetSize;

    if (Delta < 0) then
      Continue;

    if Delta < BestDelta then
    begin
      Result := Candidate;
      BestIndex := i;
      if Delta = 0 then
        Break;
      BestDelta := Delta;
    end;
  end;

  if Result = nil then
  begin
    // Instantiate a new surface with TMemoryBackend if no Candidate with
    // sufficient buffer dimensions exists in pool
    Result := TBitmap32.Create(TMemoryBackend);
    TMemoryBackend(Result.Backend).MaxExcess := BitmapMaxExcess;
    Result.SetSize(AWidth, AHeight, AClear);
  end else
  begin
    FPool.ExtractAt(BestIndex);
    Result.SetSize(AWidth, AHeight, AClear);
  end;
end;

procedure TSvgBitmapPool.Release(ABitmap: TCustomBitmap32);
begin
  if ABitmap <> nil then
    FPool.Add(ABitmap);
end;

procedure TSvgBitmapPool.Clear;
begin
  FPool.Clear;
end;

{ TSvgRenderer }

constructor TSvgRenderer.Create(ATarget: TCustomBitmap32);
begin
  inherited Create;
  FTarget := ATarget;
  FMatrixStack := TList<TFloatMatrix>.Create;
  FCurrentMatrix := IdentityMatrix;
  FViewportRect := FloatRect(0, 0, 0, 0);
  FDocumentRoot := nil;
  FBitmapPool := TSvgBitmapPool.Create;
  FBitmapPool.BitmapMaxExcess := 1024; // Magic!
end;

destructor TSvgRenderer.Destroy;
begin
  FBitmapPool.Free;
  FMatrixStack.Free;
  inherited Destroy;
end;

function TSvgRenderer.GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean): TCustomBitmap32;
begin
  Result := FBitmapPool.Acquire(AWidth, AHeight, AClear);
end;

procedure TSvgRenderer.ReleaseOffscreenBitmap(ABitmap: TCustomBitmap32);
begin
  FBitmapPool.Release(ABitmap);
end;

procedure TSvgRenderer.PushMatrix;
begin
  FMatrixStack.Add(FCurrentMatrix);
end;

procedure TSvgRenderer.PopMatrix;
begin
  if FMatrixStack.Count > 0 then
  begin
    FCurrentMatrix := FMatrixStack[FMatrixStack.Count - 1];
    FMatrixStack.Delete(FMatrixStack.Count - 1);
  end;
end;

procedure TSvgRenderer.ApplyMatrix(const AMatrix: TFloatMatrix);
begin
  FCurrentMatrix := Mult(FCurrentMatrix, AMatrix);
end;

function TSvgRenderer.ExtractUrlId(const AUrlStr: string): string;
var
  pStart, pEnd: Integer;
begin
  Result := Trim(AUrlStr);
  pStart := Pos('url(', LowerCase(Result));
  if pStart > 0 then
  begin
    Delete(Result, 1, pStart + 3);
    pEnd := Pos(')', Result);
    if pEnd > 0 then
      Result := Copy(Result, 1, pEnd - 1);
    Result := Trim(Result);
    if (Length(Result) > 0) and (Result[1] in ['"', '''']) then
    begin
      Delete(Result, 1, 1);
      if (Length(Result) > 0) and (Result[Length(Result)] in ['"', '''']) then
        Delete(Result, Length(Result), 1);
    end;
  end;
  if (Length(Result) > 0) and (Result[1] = '#') then
    Delete(Result, 1, 1);
end;

function TSvgRenderer.GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
var
  i, j, len: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    len := Length(APoints[i]);
    SetLength(Result[i], len);
    for j := 0 to len - 1 do
      Result[i][j] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(APoints[i][j]);
  end;
end;

function TSvgRenderer.GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
var
  i, j: Integer;
  pt: TFloatPoint;
  first: Boolean;
begin
  first := True;
  Result := FloatRect(0, 0, 0, 0);
  for i := 0 to High(APoints) do
  begin
    for j := 0 to High(APoints[i]) do
    begin
      pt := APoints[i][j];
      if first then
      begin
        Result := FloatRect(pt.X, pt.Y, pt.X, pt.Y);
        first := False;
      end
      else
      begin
        if pt.X < Result.Left then Result.Left := pt.X;
        if pt.X > Result.Right then Result.Right := pt.X;
        if pt.Y < Result.Top then Result.Top := pt.Y;
        if pt.Y > Result.Bottom then Result.Bottom := pt.Y;
      end;
    end;
  end;
end;

function TSvgRenderer.CreateGradientFiller(AGradNode: TSvgGradientNode; const ABounds: TFloatRect): TCustomPolygonFiller;
var
  i: Integer;
  stop: TSvgGradientStop;
  stopColor: TColor32;
  bWidth, bHeight: Single;
  linNode: TSvgLinearGradientNode;
  radNode: TSvgRadialGradientNode;
  x1, y1, x2, y2: Single;
  cx, cy, r, fx, fy, rx, ry, scaleX, scaleY: Single;
  linFiller: TLinearGradientPolygonFiller;
  radFiller: TSVGRadialGradientPolygonFiller;
  totalTransform, gradTransform, bboxMat: TFloatMatrixHelper;
  ptStart, ptEnd, ptC, ptF: TFloatPoint;
const
  WrapMode: array[TSvgSpreadMethod] of TWrapMode = (wmClamp, wmReflect, wmRepeat);
begin
  Result := nil;
  if (AGradNode = nil) or (AGradNode.Stops.Count = 0) then
    Exit;

  bWidth := ABounds.Right - ABounds.Left;
  bHeight := ABounds.Bottom - ABounds.Top;
  if bWidth <= 0 then
    bWidth := 1.0;
  if bHeight <= 0 then
    bHeight := 1.0;

  gradTransform.Matrix := AGradNode.Transform;

  if AGradNode is TSvgLinearGradientNode then
  begin
    linNode := TSvgLinearGradientNode(AGradNode);

    if linNode.GradientUnits = guObjectBoundingBox then
    begin
      x1 := linNode.X1.ToPixels(1.0);
      y1 := linNode.Y1.ToPixels(1.0);
      x2 := linNode.X2.ToPixels(1.0);
      y2 := linNode.Y2.ToPixels(1.0);

      bboxMat.Matrix := IdentityMatrix;
      bboxMat.Scale(bWidth, bHeight);
      bboxMat.Translate(ABounds.Left, ABounds.Top);
      totalTransform := bboxMat * gradTransform;
    end else
    begin
      x1 := linNode.X1.ToPixels(FViewportRect.Right - FViewportRect.Left);
      y1 := linNode.Y1.ToPixels(FViewportRect.Bottom - FViewportRect.Top);
      x2 := linNode.X2.ToPixels(FViewportRect.Right - FViewportRect.Left);
      y2 := linNode.Y2.ToPixels(FViewportRect.Bottom - FViewportRect.Top);
      totalTransform := gradTransform * FCurrentMatrix;
    end;

    ptStart := FloatPoint(x1, y1);
    ptEnd := FloatPoint(x2, y2);

    if (not totalTransform.IsIdentity) then
    begin
      ptStart := totalTransform.TransformPoint(ptStart);
      ptEnd := totalTransform.TransformPoint(ptEnd);
    end;

    linFiller := TLinearGradientPolygonFiller.Create;
    linFiller.StartPoint := ptStart;
    linFiller.EndPoint := ptEnd;
    linFiller.WrapMode := WrapMode[AGradNode.SpreadMethod];

    linFiller.Gradient.ClearColorStops;
    for i := 0 to AGradNode.Stops.Count - 1 do
    begin
      stop := AGradNode.Stops[i];
      stopColor := stop.Color.Color;
      if stop.Opacity < 1.0 then
        ScaleAlpha(stopColor, stop.Opacity);
      linFiller.Gradient.AddColorStop(stop.Offset, stopColor);
    end;

    Result := linFiller;
  end else

  if AGradNode is TSvgRadialGradientNode then
  begin
    radNode := TSvgRadialGradientNode(AGradNode);

    if radNode.GradientUnits = guObjectBoundingBox then
    begin
      cx := radNode.Cx.ToPixels(1.0);
      cy := radNode.Cy.ToPixels(1.0);
      r := radNode.R.ToPixels(1.0);
      fx := radNode.Fx.ToPixels(1.0);
      fy := radNode.Fy.ToPixels(1.0);

      bboxMat.Matrix := IdentityMatrix;
      bboxMat.Scale(bWidth, bHeight);
      bboxMat.Translate(ABounds.Left, ABounds.Top);
      totalTransform := bboxMat * gradTransform;
    end else
    begin
      cx := radNode.Cx.ToPixels(FViewportRect.Right - FViewportRect.Left);
      cy := radNode.Cy.ToPixels(FViewportRect.Bottom - FViewportRect.Top);
      r := radNode.R.ToPixels(Sqrt(Sqr(FViewportRect.Right - FViewportRect.Left) + Sqr(FViewportRect.Bottom - FViewportRect.Top)) * Sqrt(0.5));
      fx := radNode.Fx.ToPixels(FViewportRect.Right - FViewportRect.Left);
      fy := radNode.Fy.ToPixels(FViewportRect.Bottom - FViewportRect.Top);
      totalTransform := gradTransform * FCurrentMatrix;
    end;

    ptC := FloatPoint(cx, cy);
    ptF := FloatPoint(fx, fy);

    rx := r;
    ry := r;

    if (not totalTransform.IsIdentity) then
    begin
      ptC := totalTransform.TransformPoint(ptC);
      ptF := totalTransform.TransformPoint(ptF);

      scaleX := GR32_Math.Hypot(totalTransform.Matrix[0, 0], totalTransform.Matrix[0, 1]);
      scaleY := GR32_Math.Hypot(totalTransform.Matrix[1, 0], totalTransform.Matrix[1, 1]);
      if scaleX > 0 then
        rx := rx * scaleX;
      if scaleY > 0 then
        ry := ry * scaleY;
    end;

    radFiller := TSVGRadialGradientPolygonFiller.Create;
    try
      radFiller.EllipseBounds := FloatRect(ptC.X - rx, ptC.Y - ry, ptC.X + rx, ptC.Y + ry);
      radFiller.FocalPoint := ptF;
      radFiller.WrapMode := WrapMode[AGradNode.SpreadMethod];

      radFiller.Gradient.ClearColorStops;
      for i := 0 to AGradNode.Stops.Count - 1 do
      begin
        stop := AGradNode.Stops[i];
        stopColor := stop.Color.Color;
        if stop.Opacity < 1.0 then
          ScaleAlpha(stopColor, stop.Opacity);
        radFiller.Gradient.AddColorStop(stop.Offset, stopColor);
      end;
    except
      radFiller.Free;
      raise;
    end;

    Result := radFiller;
  end;
end;

function TSvgRenderer.CreatePatternFiller(APatternNode: TSvgPatternNode; const ABounds: TFloatRect): TCustomPolygonFiller;
var
  bWidth, bHeight, vpWidth, vpHeight: Single;
  tileX, tileY, tileW, tileH: Single;
  patternBmp: TBitmap32;
  i, bmpW, bmpH: Integer;
  savedTarget: TCustomBitmap32;
  savedMatrix: TFloatMatrix;
  savedViewport: TFloatRect;
  contentMat, patTransMat: TFloatMatrix;
  origPt: TFloatPoint;
  tileViewBox: TSvgViewBox;
begin
  Result := nil;
  if (APatternNode = nil) or (APatternNode.Children.Count = 0) then
    Exit;

  bWidth := ABounds.Right - ABounds.Left;
  bHeight := ABounds.Bottom - ABounds.Top;
  if bWidth <= 0 then
    bWidth := 1.0;
  if bHeight <= 0 then
    bHeight := 1.0;

  vpWidth := FViewportRect.Right - FViewportRect.Left;
  vpHeight := FViewportRect.Bottom - FViewportRect.Top;
  if vpWidth <= 0 then
    vpWidth := 1.0;
  if vpHeight <= 0 then
    vpHeight := 1.0;

  if APatternNode.PatternUnits = guObjectBoundingBox then
  begin
    tileX := ABounds.Left + APatternNode.X.ToPixels(bWidth);
    tileY := ABounds.Top + APatternNode.Y.ToPixels(bHeight);
    tileW := APatternNode.Width.ToPixels(bWidth);
    tileH := APatternNode.Height.ToPixels(bHeight);
  end else
  begin
    tileX := APatternNode.X.ToPixels(vpWidth);
    tileY := APatternNode.Y.ToPixels(vpHeight);
    tileW := APatternNode.Width.ToPixels(vpWidth);
    tileH := APatternNode.Height.ToPixels(vpHeight);
  end;

  patTransMat := APatternNode.PatternTransform;
  if not IsIdentityMatrix(patTransMat) then
  begin
    origPt := TFloatMatrixHelper(patTransMat).TransformPoint(FloatPoint(tileX, tileY));
    tileX := origPt.X;
    tileY := origPt.Y;
  end;

  bmpW := Round(tileW);
  bmpH := Round(tileH);
  if (bmpW <= 0) or (bmpH <= 0) then
    Exit;

  patternBmp := TBitmap32.Create;
  try
    patternBmp.SetSize(bmpW, bmpH);
    patternBmp.DrawMode := dmBlend;

    savedTarget := FTarget;
    savedMatrix := FCurrentMatrix;
    savedViewport := FViewportRect;

    FTarget := patternBmp;
    FCurrentMatrix := IdentityMatrix;
    FViewportRect := FloatRect(0, 0, bmpW, bmpH);

    contentMat := IdentityMatrix;
    if APatternNode.ViewBox.IsValid then
    begin
      tileViewBox := APatternNode.ViewBox;
      contentMat := tileViewBox.GetTransform(FloatRect(0, 0, tileW, tileH), APatternNode.PreserveAspectRatio);
    end else
    if APatternNode.PatternContentUnits = guObjectBoundingBox then
    begin
      contentMat := IdentityMatrix;
      TFloatMatrixHelper(contentMat).Scale(bWidth, bHeight);
    end;

    PushMatrix;
    try
      ApplyMatrix(contentMat);
      for i := 0 to APatternNode.Children.Count - 1 do
        RenderNode(APatternNode.Children[i]);
    finally
      PopMatrix;
      FTarget := savedTarget;
      FCurrentMatrix := savedMatrix;
      FViewportRect := savedViewport;
    end;

    Result := TSvgPatternPolygonFiller.Create(patternBmp);
    TSvgPatternPolygonFiller(Result).OffsetX := Round(tileX);
    TSvgPatternPolygonFiller(Result).OffsetY := Round(tileY);
  except
    patternBmp.Free;
    raise;
  end;
end;

procedure TSvgRenderer.RenderPathNode(APathNode: TSvgPathNode);
var
  transformedPts: TArrayOfArrayOfFloatPoint;
  polyRenderer: TPolygonRenderer32;
  fillColor, strokeColor: TColor32;
  strokeWidth, matScale, scaledOffset: Single;
  strokePts, dashedPts: TArrayOfArrayOfFloatPoint;
  scaledDashArray: TArrayOfFloat;
  i, j, k: Integer;
  filler: TCustomPolygonFiller;
  bounds, strokeBounds: TFloatRect;
  targetNode: TSvgNode;
  urlId: string;
begin
  if (APathNode = nil) or (Length(APathNode.PathData) = 0) or (FTarget = nil) then
    Exit;

  transformedPts := GetTransformedPoints(APathNode.PathData);
  bounds := GetPathBounds(transformedPts);

  // TODO : Cache the polygon renderer. There's no need to create it more than once.
  polyRenderer := DefaultPolygonRendererClass.Create(FTarget);
  try
    // 1. Fill Rendering
    if APathNode.Fill.Url <> '' then
    begin
      urlId := ExtractUrlId(APathNode.Fill.Url);

      if (FDocumentRoot <> nil) then
      begin
        targetNode := FDocumentRoot.FindNodeById(urlId);

        filler := nil;
        if targetNode is TSvgGradientNode then
          filler := CreateGradientFiller(TSvgGradientNode(targetNode), bounds)
        else
        if targetNode is TSvgPatternNode then
          filler := CreatePatternFiller(TSvgPatternNode(targetNode), bounds);

        if filler <> nil then
        begin
          try
            polyRenderer.Filler := filler;
            try
              polyRenderer.FillMode := APathNode.Fill.FillRule;
              polyRenderer.PolyPolygonFS(transformedPts);
            finally
              polyRenderer.Filler := nil;
            end;
          finally
            filler.Free;
          end;
        end;
      end;
    end else
    if not APathNode.Fill.Color.IsNone then
    begin
      fillColor := APathNode.Fill.Color.Color;
      if APathNode.Fill.Opacity < 1.0 then
        ScaleAlpha(fillColor, APathNode.Fill.Opacity);

      if AlphaComponent(fillColor) > 0 then
      begin
        polyRenderer.Color := fillColor;
        polyRenderer.FillMode := APathNode.Fill.FillRule;
        polyRenderer.PolyPolygonFS(transformedPts);
      end;
    end;

    // 2. Stroke Rendering
    strokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Right - FViewportRect.Left);

    if strokeWidth > 0 then
    begin
      matScale := GetMatrixScale(FCurrentMatrix);
      strokeWidth := strokeWidth * matScale;

      scaledDashArray := nil;
      scaledOffset := 0;
      if Length(APathNode.Stroke.DashArray) > 0 then
      begin
        SetLength(scaledDashArray, Length(APathNode.Stroke.DashArray));
        for k := 0 to High(APathNode.Stroke.DashArray) do
          scaledDashArray[k] := APathNode.Stroke.DashArray[k] * matScale;
        scaledOffset := APathNode.Stroke.DashOffset * matScale;
      end;

      if (APathNode.Stroke.Url <> '') and (FDocumentRoot <> nil) then
      begin
        urlId := ExtractUrlId(APathNode.Stroke.Url);
        targetNode := FDocumentRoot.FindNodeById(urlId);

        if targetNode <> nil then
        begin
          strokePts := nil;
          for i := 0 to High(transformedPts) do
          begin
            if Length(scaledDashArray) > 0 then
            begin
              dashedPts := BuildDashedLine(transformedPts[i], scaledDashArray, scaledOffset, IsClosedContour(transformedPts[i]));
              for j := 0 to High(dashedPts) do
                strokePts := strokePts + BuildPolyPolyLine([dashedPts[j]], False, strokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
            end else
              strokePts := strokePts + BuildPolyPolyLine([transformedPts[i]], IsClosedContour(transformedPts[i]), strokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
          end;

          strokeBounds := GetPathBounds(strokePts);
          filler := nil;
          if targetNode is TSvgGradientNode then
            filler := CreateGradientFiller(TSvgGradientNode(targetNode), strokeBounds)
          else
          if targetNode is TSvgPatternNode then
            filler := CreatePatternFiller(TSvgPatternNode(targetNode), strokeBounds);

          if filler <> nil then
          begin
            try
              polyRenderer.Filler := filler;
              try
                polyRenderer.FillMode := pfWinding;
                polyRenderer.PolyPolygonFS(strokePts);
              finally
                polyRenderer.Filler := nil;
              end;
            finally
              filler.Free;
            end;
          end;
        end;
      end else
      if not APathNode.Stroke.Color.IsNone then
      begin
        strokeColor := APathNode.Stroke.Color.Color;
        if APathNode.Stroke.Opacity < 1.0 then
          ScaleAlpha(strokeColor, APathNode.Stroke.Opacity);

        if AlphaComponent(strokeColor) > 0 then
        begin
          strokePts := nil;
          for i := 0 to High(transformedPts) do
          begin
            if Length(scaledDashArray) > 0 then
            begin
              dashedPts := BuildDashedLine(transformedPts[i], scaledDashArray, scaledOffset, IsClosedContour(transformedPts[i]));
              for j := 0 to High(dashedPts) do
                strokePts := strokePts + BuildPolyPolyLine([dashedPts[j]], False, strokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
            end else
              strokePts := strokePts + BuildPolyPolyLine([transformedPts[i]], IsClosedContour(transformedPts[i]), strokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
          end;

          polyRenderer.Color := strokeColor;
          polyRenderer.FillMode := pfWinding;
          polyRenderer.PolyPolygonFS(strokePts);
        end;
      end;
    end;

    // 3. Markers Rendering (per SVG specification, markers paint on top of fill and stroke)
    if (APathNode.MarkerStart <> '') or (APathNode.MarkerMid <> '') or (APathNode.MarkerEnd <> '') then
    begin
      strokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Right - FViewportRect.Left);
      RenderMarkers(APathNode, APathNode.PathData, strokeWidth);
    end;
  finally
    polyRenderer.Free;
  end;
end;

procedure TSvgRenderer.RenderClipPathNode(AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; AMaskBmp: TCustomBitmap32);
var
  savedTarget: TCustomBitmap32;
  savedMatrix: TFloatMatrix;
  bWidth, bHeight: Single;
  i: Integer;
begin
  { Renders child nodes of a <clipPath> onto a temporary alpha surface.
    If clipPathUnits = guObjectBoundingBox, applies translation and scale derived from target object bounds. }
  if (AClipNode = nil) or (AMaskBmp = nil) then Exit;

  savedTarget := FTarget;
  savedMatrix := FCurrentMatrix;
  FTarget := AMaskBmp;
  try
    if AClipNode.ClipPathUnits = guObjectBoundingBox then
    begin
      bWidth := ATargetBounds.Right - ATargetBounds.Left;
      bHeight := ATargetBounds.Bottom - ATargetBounds.Top;
      if bWidth <= 0 then bWidth := 1.0;
      if bHeight <= 0 then bHeight := 1.0;

      FCurrentMatrix := IdentityMatrix;
      TFloatMatrixHelper(FCurrentMatrix).Scale(bWidth, bHeight);
      TFloatMatrixHelper(FCurrentMatrix).Translate(ATargetBounds.Left, ATargetBounds.Top);
    end;

    AMaskBmp.Clear(0); // Clear to 0 transparent so filled shapes paint non-zero alpha inside clip region
    for i := 0 to AClipNode.Children.Count - 1 do
      RenderNode(AClipNode.Children[i]);
  finally
    FTarget := savedTarget;
    FCurrentMatrix := savedMatrix;
  end;
end;

procedure TSvgRenderer.RenderMaskNode(AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; AMaskBmp: TCustomBitmap32);
var
  savedTarget: TCustomBitmap32;
  savedMatrix: TFloatMatrix;
  bWidth, bHeight: Single;
  i: Integer;
begin
  { Renders child nodes of a <mask> onto a temporary luminance/alpha surface.
    If maskContentUnits = guObjectBoundingBox, applies translation and scale derived from target object bounds. }
  if (AMaskNode = nil) or (AMaskBmp = nil) then Exit;

  savedTarget := FTarget;
  savedMatrix := FCurrentMatrix;
  FTarget := AMaskBmp;
  try
    if AMaskNode.MaskContentUnits = guObjectBoundingBox then
    begin
      bWidth := ATargetBounds.Right - ATargetBounds.Left;
      bHeight := ATargetBounds.Bottom - ATargetBounds.Top;
      if bWidth <= 0 then bWidth := 1.0;
      if bHeight <= 0 then bHeight := 1.0;

      FCurrentMatrix := IdentityMatrix;
      TFloatMatrixHelper(FCurrentMatrix).Scale(bWidth, bHeight);
      TFloatMatrixHelper(FCurrentMatrix).Translate(ATargetBounds.Left, ATargetBounds.Top);
    end;

    AMaskBmp.Clear(clBlack32);
    for i := 0 to AMaskNode.Children.Count - 1 do
      RenderNode(AMaskNode.Children[i]);
  finally
    FTarget := savedTarget;
    FCurrentMatrix := savedMatrix;
  end;
end;

procedure TSvgRenderer.RenderMarker(AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single);
var
  mw, mh, rx, ry, scaleX, scaleY: Single;
  markerMat: TFloatMatrixHelper;
  viewMat: TFloatMatrix;
  vpW, vpH: Single;
  i: Integer;
  markerViewBox: TSvgViewBox;
begin
  if (AMarker = nil) or (AMarker.Children.Count = 0) then
    Exit;

  vpW := FViewportRect.Right - FViewportRect.Left;
  vpH := FViewportRect.Bottom - FViewportRect.Top;
  if vpW <= 0 then
    vpW := 1.0;
  if vpH <= 0 then
    vpH := 1.0;

  mw := AMarker.MarkerWidth.ToPixels(vpW);
  mh := AMarker.MarkerHeight.ToPixels(vpH);

  if AMarker.MarkerUnits = muStrokeWidth then
  begin
    if AStrokeWidth <= 0 then
      AStrokeWidth := 1.0;
    scaleX := mw * AStrokeWidth;
    scaleY := mh * AStrokeWidth;
  end else
  begin
    scaleX := mw;
    scaleY := mh;
  end;

  rx := AMarker.RefX.ToPixels(mw);
  ry := AMarker.RefY.ToPixels(mh);

  // SVG Marker transformation sequence (innermost to outermost):
  // 1. Local marker content -> Shift by (-refX, -refY) in viewBox user space
  // 2. Map viewBox (0, 0, vbW, vbH) onto marker viewport (0, 0, mw, mh)
  // 3. Scale by AStrokeWidth if markerUnits="strokeWidth"
  // 4. Rotate by AAngle around origin (refX, refY)
  // 5. Translate origin (refX, refY) to path vertex AVertex
  if AMarker.ViewBox.IsValid then
  begin
    markerViewBox := AMarker.ViewBox;
    viewMat := markerViewBox.GetTransform(FloatRect(0, 0, mw, mh), AMarker.PreserveAspectRatio);
    markerMat.Matrix := IdentityMatrix;
    markerMat.Translate(-AMarker.RefX.Value, -AMarker.RefY.Value);
    markerMat := markerMat * viewMat;
    if AMarker.MarkerUnits = muStrokeWidth then
      markerMat.Scale(AStrokeWidth, AStrokeWidth);
  end else
  begin
    markerMat.Matrix := IdentityMatrix;
    markerMat.Translate(-rx, -ry);
    markerMat.Scale(scaleX / mw, scaleY / mh);
  end;

  if (AAngle <> 0) then
    markerMat.Rotate(RadToDeg(AAngle));
  markerMat.Translate(AVertex.X, AVertex.Y);

  PushMatrix;
  try
    ApplyMatrix(markerMat.Matrix);
    for i := 0 to AMarker.Children.Count - 1 do
      RenderNode(AMarker.Children[i]);
  finally
    PopMatrix;
  end;
end;

procedure TSvgRenderer.RenderMarkers(APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single);

  function GetVectorAngle(const P1, P2: TFloatPoint): Single;
  var
    dx, dy: Single;
  begin
    dx := P2.X - P1.X;
    dy := P2.Y - P1.Y;
    if (Abs(dx) < 1E-6) and (Abs(dy) < 1E-6) then
      Result := 0.0
    else
      Result := ArcTan2(dy, dx);
  end;

  function BisectAngles(const InAngle, OutAngle: Single): Single;
  var
    SinIn, CosIn: Single;
    SinOut, CosOut: Single;
  begin
    GR32_Math.SinCos(InAngle, SinIn, CosIn);
    GR32_Math.SinCos(OutAngle, SinOut, CosOut);

    Result := ArcTan2(SinIn + SinOut, CosIn + CosOut);
  end;

var
  startMarker, midMarker, endMarker: TSvgMarkerNode;
  contourIdx, ptCount, i: Integer;
  contour: TArrayOfFloatPoint;
  inAngle, outAngle, vAngle: Single;
begin
  if (APathNode = nil) or (Length(APoints) = 0) then
    Exit;

  startMarker := APathNode.ResolvedMarkerStart;
  midMarker := APathNode.ResolvedMarkerMid;
  endMarker := APathNode.ResolvedMarkerEnd;

  if (startMarker = nil) and (midMarker = nil) and (endMarker = nil) then
    Exit;

  for contourIdx := 0 to High(APoints) do
  begin
    contour := APoints[contourIdx];
    ptCount := Length(contour);
    if ptCount < 2 then
      Continue;

    // Start Vertex Marker
    if startMarker <> nil then
    begin
      case startMarker.Orient of
        moAuto:
          vAngle := GetVectorAngle(contour[0], contour[1]);

        moAutoStartReverse:
          vAngle := GetVectorAngle(contour[0], contour[1]) + Pi;

        moAngle:
          vAngle := DegToRad(startMarker.OrientAngle);
      else
        vAngle := 0;
      end;
      RenderMarker(startMarker, contour[0], vAngle, AStrokeWidth);
    end;

    // Mid Vertices Markers
    if (midMarker <> nil) and (ptCount > 2) then
    begin
      for i := 1 to ptCount - 2 do
      begin
        case midMarker.Orient of
          moAuto, moAutoStartReverse:
            begin
              inAngle := GetVectorAngle(contour[i - 1], contour[i]);
              outAngle := GetVectorAngle(contour[i], contour[i + 1]);
              // Bisector angle of incoming and outgoing segment vectors
              vAngle := BisectAngles(inAngle, outAngle);
            end;

          moAngle:
            vAngle := DegToRad(midMarker.OrientAngle);
        else
          vAngle := 0;
        end;
        RenderMarker(midMarker, contour[i], vAngle, AStrokeWidth);
      end;
    end;

    // End Vertex Marker
    if endMarker <> nil then
    begin
      case endMarker.Orient of
        moAuto, moAutoStartReverse:
          vAngle := GetVectorAngle(contour[ptCount - 2], contour[ptCount - 1]);

        moAngle:
          vAngle := DegToRad(endMarker.OrientAngle);
      else
        vAngle := 0;
      end;
      RenderMarker(endMarker, contour[ptCount - 1], vAngle, AStrokeWidth);
    end;
  end;
end;

procedure TSvgRenderer.RenderGroupNode(AGroupNode: TSvgGroupNode);
var
  i, k: Integer;
  offscreenBmp, clipMaskBmp, maskBmp: TCustomBitmap32;
  savedTarget: TCustomBitmap32;
  x: Integer;
  srcP, dstP: PColor32;
  gray: Byte;
  clipNodeTarget: TSvgClipPathNode;
  maskNodeTarget: TSvgMaskNode;
  clipTargetNode, maskTargetNode: TSvgNode;
  alphaVal: Byte;
  oldMatrix: TFloatMatrix;
  clipId, maskId: string;
  groupBounds, targetWorldBounds: TFloatRect;
  pts: array[0..3] of TFloatPoint;
begin
  if AGroupNode = nil then
    Exit;

  // Offscreen rendering required if Opacity < 1.0, ClipPathID <> '', or MaskID <> ''
  if (AGroupNode.Opacity < 1.0) or (AGroupNode.ClipPathID <> '') or (AGroupNode.MaskID <> '') then
  begin
    if (FTarget = nil) then
      Exit;

    // Calculate group bounding box in world space for objectBoundingBox units
    groupBounds := AGroupNode.GetObjectBoundingBox;
    pts[0] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(groupBounds.Left, groupBounds.Top));
    pts[1] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(groupBounds.Right, groupBounds.Top));
    pts[2] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(groupBounds.Right, groupBounds.Bottom));
    pts[3] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(groupBounds.Left, groupBounds.Bottom));

    targetWorldBounds := FloatRect(pts[0].X, pts[0].Y, pts[0].X, pts[0].Y);
    for k := 1 to 3 do
    begin
      if pts[k].X < targetWorldBounds.Left then targetWorldBounds.Left := pts[k].X;
      if pts[k].X > targetWorldBounds.Right then targetWorldBounds.Right := pts[k].X;
      if pts[k].Y < targetWorldBounds.Top then targetWorldBounds.Top := pts[k].Y;
      if pts[k].Y > targetWorldBounds.Bottom then targetWorldBounds.Bottom := pts[k].Y;
    end;

    // Acquire reusable offscreen scratchpad surface from bitmap pool
    offscreenBmp := GetOffscreenBitmap(FTarget.Width, FTarget.Height, True);
    clipMaskBmp := nil;
    maskBmp := nil;
    try
      offscreenBmp.SetSize(FTarget.Width, FTarget.Height);

      savedTarget := FTarget;
      FTarget := offscreenBmp;
      try
        for i := 0 to AGroupNode.Children.Count - 1 do
          RenderNode(AGroupNode.Children[i]);
      finally
        FTarget := savedTarget;
      end;

      // Apply ClipPath
      if (AGroupNode.ClipPathID <> '') and (FDocumentRoot <> nil) then
      begin
        clipId := ExtractUrlId(AGroupNode.ClipPathID);
        clipTargetNode := FDocumentRoot.FindNodeById(clipId);
        if clipTargetNode is TSvgClipPathNode then
        begin
          clipNodeTarget := TSvgClipPathNode(clipTargetNode);
          clipMaskBmp := GetOffscreenBitmap(FTarget.Width, FTarget.Height, True);

          oldMatrix := FCurrentMatrix;
          RenderClipPathNode(clipNodeTarget, targetWorldBounds, clipMaskBmp);
          FCurrentMatrix := oldMatrix;

          srcP := PColor32(offscreenBmp.Bits);
          dstP := PColor32(clipMaskBmp.Bits);
          for x := 0 to offscreenBmp.PixelCount - 1 do
          begin
            alphaVal := AlphaComponent(dstP^);
            if alphaVal < 255 then
              ScaleAlpha(srcP^, alphaVal / 255.0);
            Inc(srcP);
            Inc(dstP);
          end;
        end;
      end;

      // Apply Alpha Mask
      if (AGroupNode.MaskID <> '') and (FDocumentRoot <> nil) then
      begin
        maskId := ExtractUrlId(AGroupNode.MaskID);
        maskTargetNode := FDocumentRoot.FindNodeById(maskId);
        if maskTargetNode is TSvgMaskNode then
        begin
          maskNodeTarget := TSvgMaskNode(maskTargetNode);
          maskBmp := GetOffscreenBitmap(FTarget.Width, FTarget.Height, False);

          oldMatrix := FCurrentMatrix;
          RenderMaskNode(maskNodeTarget, targetWorldBounds, maskBmp);
          FCurrentMatrix := oldMatrix;

          srcP := PColor32(offscreenBmp.Bits);
          dstP := PColor32(maskBmp.Bits);
          for x := 0 to offscreenBmp.PixelCount - 1 do
          begin
            // Grayscale luminance conversion: Y = 0.299 R + 0.587 G + 0.114 B
            gray := Intensity(dstP^);
            gray := Round(gray * (AlphaComponent(dstP^) / 255.0));
            ScaleAlpha(srcP^, gray / 255.0);
            Inc(srcP);
            Inc(dstP);
          end;
        end;
      end;

      // Apply Group Opacity and blend to parent target
      if AGroupNode.Opacity < 1.0 then
      begin
        srcP := PColor32(offscreenBmp.Bits);
        for x := 0 to offscreenBmp.PixelCount - 1 do
        begin
          ScaleAlpha(srcP^, AGroupNode.Opacity);
          Inc(srcP);
        end;
      end;

      // Blend offscreen surface onto main target
      offscreenBmp.DrawMode := dmBlend;
      offscreenBmp.CombineMode := cmBlend;
      offscreenBmp.DrawTo(FTarget, 0, 0);

    finally
      // Release surfaces back to bitmap surface pool for reuse in subsequent groups/frames
      ReleaseOffscreenBitmap(offscreenBmp);
      ReleaseOffscreenBitmap(clipMaskBmp);
      ReleaseOffscreenBitmap(maskBmp);
    end;
    Exit;
  end;

  for i := 0 to AGroupNode.Children.Count - 1 do
    RenderNode(AGroupNode.Children[i]);
end;

procedure TSvgRenderer.RenderNode(ANode: TSvgNode);
begin
  if (ANode = nil) or (not ANode.Visible) or (not ANode.IsRenderable) then
    Exit;

  PushMatrix;
  try
    ApplyMatrix(ANode.Transform);

    if ANode is TSvgPathNode then
      RenderPathNode(TSvgPathNode(ANode))
    else
    if ANode is TSvgGroupNode then
      RenderGroupNode(TSvgGroupNode(ANode));
  finally
    PopMatrix;
  end;
end;

procedure TSvgRenderer.RenderDocument(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect);
var
  vpMat: TFloatMatrix;
  viewBox: TSvgViewBox;
  docW, docH: Single;
begin
  if (ADoc = nil) or (FTarget = nil) then
    Exit;

  FDocumentRoot := ADoc;
  FViewportRect := ATargetRect;

  docW := ADoc.Width.ToPixels(ATargetRect.Right - ATargetRect.Left);
  docH := ADoc.Height.ToPixels(ATargetRect.Bottom - ATargetRect.Top);

  viewBox := ADoc.ViewBox;
  if not viewBox.IsValid then
  begin
    if (docW > 0) and (docH > 0) then
      viewBox := TSvgViewBox.Create(0, 0, docW, docH)
    else
      viewBox := TSvgViewBox.Create(0, 0, FTarget.Width, FTarget.Height);
  end;

  vpMat := viewBox.GetTransform(ATargetRect, ADoc.PreserveAspectRatio);

  FCurrentMatrix := IdentityMatrix;
  FMatrixStack.Clear;

  PushMatrix;
  try
    ApplyMatrix(vpMat);
    RenderNode(ADoc);
  finally
    PopMatrix;
  end;
end;

procedure TSvgRenderer.RenderDocument(ADoc: TSvgDocumentNode);
begin
  if FTarget <> nil then
    RenderDocument(ADoc, FloatRect(0, 0, FTarget.Width, FTarget.Height));
end;

end.
