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
  SysUtils, Classes, Graphics, Generics.Collections,
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

  TFontInfo = record
    FontFamily: string;
    Style: TFontStyles;
    Size: integer;
  end;

  TSvgRenderer = class(TObject)
  private
    FTarget: TCustomBitmap32;
    FMatrixStack: TList<TFloatMatrix>;
    FCurrentMatrix: TFloatMatrix;
    FViewportRect: TFloatRect;
    FDocumentRoot: TSvgDocumentNode;
    FBitmapPool: TSvgBitmapPool;
    FPolyRenderer: TPolygonRenderer32;
    FAllowExternalImages: Boolean;
    FRecursionDepth: integer;
  protected
    procedure RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
    procedure RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
    procedure RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);
    procedure RenderGroupNode(ATarget: TCustomBitmap32; AGroupNode: TSvgGroupNode);
    procedure RenderClipPathNode(AMaskBmp: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMaskNode(AMaskBmp: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMarker(ATarget: TCustomBitmap32; AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single); // Angle is in radians!
    procedure RenderMarkers(ATarget: TCustomBitmap32; APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single);
    procedure RenderFilter(ATarget: TCustomBitmap32; AFilterNode: TSvgFilterNode; ANode: TSvgNode);
    procedure RenderNodeContent(ATarget: TCustomBitmap32; ANode: TSvgNode);
    procedure RenderNodeUnfiltered(ATarget: TCustomBitmap32; ANode: TSvgNode);
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode; AX: Integer; AY: Integer); overload;
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode); overload;
    function GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
    function GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
    function CreateGradientFiller(AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
    function CreatePatternFiller(APatternNode: TSvgPatternNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
    function GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure ReleaseOffscreenBitmap(ABitmap: TCustomBitmap32);
    procedure MapFont(const AFontFamily, AWeightStr, AStyleStr: string; ASize: integer; var AFontInfo: TFontInfo); virtual;
  public
    constructor Create(ATarget: TCustomBitmap32 = nil); virtual;
    destructor Destroy; override;

    procedure PushMatrix;
    procedure PopMatrix;
    procedure ApplyMatrix(const AMatrix: TFloatMatrix);

    procedure RenderDocument(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect); overload;
    procedure RenderDocument(ADoc: TSvgDocumentNode); overload;
    procedure RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode); overload;
    procedure RenderNode(ANode: TSvgNode); overload;

    property Target: TCustomBitmap32 read FTarget write FTarget;
    property CurrentMatrix: TFloatMatrix read FCurrentMatrix write FCurrentMatrix;
    property ViewportRect: TFloatRect read FViewportRect write FViewportRect;
    property AllowExternalImages: Boolean read FAllowExternalImages write FAllowExternalImages;
  end;

implementation

uses
  Types,
  Math,
  GR32_Blend,
  GR32_Math,
  GR32_LowLevel,
  GR32_Backends_Generic,
  GR32_Paths,
  GR32.Text.Types,
  GR32.Text.Win,
  GR32.Text.FontFace,
  GR32.Blur,
  GR32.Blend.Modes,
  GR32.Blend.Modes.PorterDuff,
  GR32.Blend.Modes.PhotoShop;

const
  ZERO_WIDTH_SPACE = $200B; // Unicode ZERO WIDTH SPACE

const
  cMaxRecursions = 20;

const
  cMaxStackDepth = 80; // Checked in PushMatrix. Exception is exceeded.

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

function TransformPathPoints(const APoints: TArrayOfArrayOfFloatPoint; const AMatrix: TFloatMatrix): TArrayOfArrayOfFloatPoint;
var
  i, j, len: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    len := Length(APoints[i]);
    SetLength(Result[i], len);
    for j := 0 to len - 1 do
      Result[i][j] := TFloatMatrixHelper(AMatrix).TransformPoint(APoints[i][j]);
  end;
end;

function GetTotalPathLength(const APoints: TArrayOfArrayOfFloatPoint): Single;
var
  i, j: Integer;
  p1, p2: TFloatPoint;
begin
  Result := 0;
  for i := 0 to High(APoints) do
  begin
    for j := 0 to High(APoints[i]) - 1 do
    begin
      p1 := APoints[i][j];
      p2 := APoints[i][j + 1];
      Result := Result + GR32_Math.Hypot(p2.X - p1.X, p2.Y - p1.Y);
    end;
  end;
end;

(*
  GetPointAndTangentAtDistance:
  Calculates the 2D position (APoint) and tangent orientation angle in degrees (ATangentAngle)
  at a specified linear distance (ADistance) along a poly-polygon path (APoints).
  Returns True if ADistance falls within the total length of the path.
  Returns False if ADistance is less than 0 or exceeds the total path length (W3C SVG 1.1 §10.10 clipping behavior).
*)
function GetPointAndTangentAtDistance(const APoints: TArrayOfArrayOfFloatPoint; ADistance: Single; out APoint: TFloatPoint; out ATangentAngle: Single): Boolean;
var
  i, j: Integer;
  p1, p2: TFloatPoint;
  SegmentLength, AccumulatedLength, RemainingDistance, dx, dy, t: Single;
begin
  Result := False;
  APoint := FloatPoint(0, 0);
  ATangentAngle := 0;
  if (Length(APoints) = 0) or (ADistance < 0) then
    Exit;

  AccumulatedLength := 0;
  for i := 0 to High(APoints) do
  begin
    for j := 0 to High(APoints[i]) - 1 do
    begin
      p1 := APoints[i][j];
      p2 := APoints[i][j + 1];
      dx := p2.X - p1.X;
      dy := p2.Y - p1.Y;

      if (dx = 0) and (dy = 0) then
        Continue;

      SegmentLength := GR32_Math.Hypot(dx, dy);

      if (ADistance >= AccumulatedLength) and (ADistance <= AccumulatedLength + SegmentLength) then
      begin
        RemainingDistance := ADistance - AccumulatedLength;
        t := RemainingDistance / SegmentLength;
        // Lerp
        APoint.X := p1.X + t * dx;
        APoint.Y := p1.Y + t * dy;
        ATangentAngle := RadToDeg(ArcTan2(dy, dx));
        Exit(True);
      end;

      AccumulatedLength := AccumulatedLength + SegmentLength;
    end;
  end;
end;

function GetSubtreeText(ANode: TSvgNode): string;
var
  child: TSvgNode;
begin
  Result := '';
  if ANode is TSvgTextPositioningNode then
    Result := TSvgTextPositioningNode(ANode).TextContent;
  if ANode is TSvgGroupNode then
  begin
    for child in TSvgGroupNode(ANode).Children do
      Result := Result + GetSubtreeText(child);
  end;
end;

function SvgBlendModeToBlenderClass(ABlendMode: TSvgBlendMode): TGraphics32BlenderClass;
const
  cBlendModeMap: array[TSvgBlendMode] of string = (
    '',
    cBlendMultiply,
    cBlendScreen,
    cBlendOverlay,
    cBlendDarken,
    cBlendLighten,
    cBlendColorDodge,
    cBlendColorBurn,
    cBlendHardLight,
    cBlendSoftLight,
    cBlendDifference,
    cBlendExclusion
  );
begin
  Result := Graphics32BlendService.BlenderByID(cBlendModeMap[ABlendMode]);
  if (Result = nil) then
    Result := TGraphics32BlenderNormal;
end;

function GetEffectiveMixBlendMode(ANode: TSvgNode): TSvgBlendMode;
var
  Node: TSvgNode;
begin
  if ANode = nil then
    Exit(bmNormal);

  Node := ANode;
  while (Node <> nil) do
  begin
    if (Node.MixBlendMode <> bmNormal) then
      Exit(Node.MixBlendMode);

    if (Node is TSvgGroupNode) and (TSvgGroupNode(Node).Isolation = isoIsolate) then
      Break;

    Node := Node.Parent;
  end;
  Result := bmNormal;
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
  // Scan from most recent (most likely to match size) to least recent.
  for i := FPool.Count - 1 downto 0 do
  begin
    Candidate := FPool[i];

    CandidateSize := Candidate.PixelCount;
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

  Result.MasterAlpha := 255;
  Result.DrawMode := dmBlend;
  Result.CombineMode := cmMerge;
end;

procedure TSvgBitmapPool.Release(ABitmap: TCustomBitmap32);
begin
  if ABitmap = nil then
    exit;

  FPool.Add(ABitmap);
  ABitmap.OnPixelCombine := nil;
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
  FAllowExternalImages := False;
end;

destructor TSvgRenderer.Destroy;
begin
  FBitmapPool.Free;
  FMatrixStack.Free;
  FPolyRenderer.Free;
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
  if (FMatrixStack.Count > cMaxStackDepth) then
    raise Exception.Create('Max stack depth exceeded; Likely invalid recursion in svg references');
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

procedure TSvgRenderer.BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode; AX: Integer; AY: Integer);
var
  BlenderClass: TGraphics32BlenderClass;
  Blender: TCustomGraphics32Blender;
  Combiner: TPixelCombineEvent;
begin
  if (ASource = nil) or (ATarget = nil) then
    Exit;

  if (ABlendMode <> bmNormal) then
  begin
    BlenderClass := SvgBlendModeToBlenderClass(ABlendMode);

    if (BlenderClass <> nil) and (BlenderClass <> TGraphics32BlenderNormal) then
    begin
      Blender := BlenderClass.Create;
      try
        Blender.GetPixelCombiner(Combiner);

        if Assigned(Combiner) then
        begin
          ASource.DrawMode := dmCustom;
          ASource.OnPixelCombine := Combiner;

          ASource.DrawTo(ATarget, AX, AY);

          ASource.OnPixelCombine := nil;
          exit;
        end;
      finally
        Blender.Free;
      end;
    end;
  end;

  ASource.DrawMode := dmBlend;
  ASource.CombineMode := cmMerge;
  ASource.DrawTo(ATarget, AX, AY);
end;

procedure TSvgRenderer.BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode);
begin
  BlendOffscreenSurface(ATarget, ASource, ABlendMode, 0, 0);
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

function TSvgRenderer.CreateGradientFiller(AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
var
  i: Integer;
  Stop: TSvgGradientStop;
  StopColor: TColor32;
  BoundsWidth, BoundsHeight: Single;
  LinearNode: TSvgLinearGradientNode;
  RadialNode: TSvgRadialGradientNode;
  cx, cy, r, fx, fy, rx, ry, ScaleX, ScaleY: Single;
  LinearFiller: TLinearGradientPolygonFiller;
  RadialFiller: TSVGRadialGradientPolygonFiller;
  TotalTransform, GradTransform, BboxMat: TFloatMatrixHelper;
  PointStart, PointEnd, PointC, PointF: TFloatPoint;
const
  WrapMode: array[TSvgSpreadMethod] of TWrapMode = (wmClamp, wmReflect, wmRepeat);
begin
  Result := nil;
  if (AGradNode = nil) or (AGradNode.Stops.Count = 0) then
    Exit;

  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  GradTransform.Matrix := AGradNode.Transform;

  if AGradNode is TSvgLinearGradientNode then
  begin
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
      TotalTransform := BboxMat * GradTransform;
    end else
    begin
      PointStart.X := LinearNode.X1.ToPixels(FViewportRect.Width);
      PointStart.Y := LinearNode.Y1.ToPixels(FViewportRect.Height);
      PointEnd.X := LinearNode.X2.ToPixels(FViewportRect.Width);
      PointEnd.Y := LinearNode.Y2.ToPixels(FViewportRect.Height);
      TotalTransform := GradTransform * FCurrentMatrix;
    end;

    if (not TotalTransform.IsIdentity) then
    begin
      PointStart := TotalTransform.TransformPoint(PointStart);
      PointEnd := TotalTransform.TransformPoint(PointEnd);
    end;

    LinearFiller := TLinearGradientPolygonFiller.Create;
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

    Result := LinearFiller;
  end else

  if AGradNode is TSvgRadialGradientNode then
  begin
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
      TotalTransform := BboxMat * GradTransform;
    end else
    begin
      cx := RadialNode.Cx.ToPixels(FViewportRect.Width);
      cy := RadialNode.Cy.ToPixels(FViewportRect.Height);
      r := RadialNode.R.ToPixels(GR32_Math.Hypot(FViewportRect.Width, FViewportRect.Height) * Sqrt(0.5));
      fx := RadialNode.Fx.ToPixels(FViewportRect.Width);
      fy := RadialNode.Fy.ToPixels(FViewportRect.Height);
      TotalTransform := GradTransform * FCurrentMatrix;
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
end;

function TSvgRenderer.CreatePatternFiller(APatternNode: TSvgPatternNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
var
  BoundsWidth, BoundsHeight, ViewportWidth, ViewportHeight: Single;
  TileX, TileY, TileWidth, TileHeight: Single;
  TileWidthPx, TileHeightPx: Single;
  PatternBitmap: TBitmap32;
  i, BitmapWidth, BitmapHeight: Integer;
  SavedMatrix: TFloatMatrix;
  SavedViewport: TFloatRect;
  ContentMat: TFloatMatrixHelper;
  origPt: TFloatPoint;
  TileViewBox: TSvgViewBox;
  MatrixScaleX, MatrixScaleY: Single;
const
  cMaxPatternDimension = 4096;
begin
  Result := nil;
  if (APatternNode = nil) or (APatternNode.Children.Count = 0) then
    Exit;

  BoundsWidth := ABounds.Width;
  BoundsHeight := ABounds.Height;
  if BoundsWidth <= 0 then
    BoundsWidth := 1.0;
  if BoundsHeight <= 0 then
    BoundsHeight := 1.0;

  ViewportWidth := FViewportRect.Width;
  ViewportHeight := FViewportRect.Height;
  if ViewportWidth <= 0 then
    ViewportWidth := 1.0;
  if ViewportHeight <= 0 then
    ViewportHeight := 1.0;

  MatrixScaleX := GR32_Math.Hypot(FCurrentMatrix[0, 0], FCurrentMatrix[0, 1]);
  MatrixScaleY := GR32_Math.Hypot(FCurrentMatrix[1, 0], FCurrentMatrix[1, 1]);
  if MatrixScaleX <= 0 then
    MatrixScaleX := 1.0;
  if MatrixScaleY <= 0 then
    MatrixScaleY := 1.0;

  // Calculates pattern tile origin and pixel dimensions based on patternUnits.
  // When patternUnits = guObjectBoundingBox (default), tile attributes x, y, width, height
  // are defined in normalized bounding box units [0..1] (e.g. 10% = 0.1).
  if APatternNode.PatternUnits = guObjectBoundingBox then
  begin
    TileX := APatternNode.X.ToPixels(1.0);
    TileY := APatternNode.Y.ToPixels(1.0);
    TileWidth := APatternNode.Width.ToPixels(1.0);
    TileHeight := APatternNode.Height.ToPixels(1.0);

    TileWidthPx := TileWidth * BoundsWidth;
    TileHeightPx := TileHeight * BoundsHeight;

    BitmapWidth := Min(cMaxPatternDimension, Max(1, Round(TileWidthPx)));
    BitmapHeight := Min(cMaxPatternDimension, Max(1, Round(TileHeightPx)));

    TileX := ABounds.Left + TileX * BoundsWidth;
    TileY := ABounds.Top + TileY * BoundsHeight;
  end
  else
  begin
    TileX := APatternNode.X.ToPixels(ViewportWidth);
    TileY := APatternNode.Y.ToPixels(ViewportHeight);
    TileWidth := APatternNode.Width.ToPixels(ViewportWidth);
    TileHeight := APatternNode.Height.ToPixels(ViewportHeight);

    TileWidthPx := TileWidth * MatrixScaleX;
    TileHeightPx := TileHeight * MatrixScaleY;

    BitmapWidth := Min(cMaxPatternDimension, Max(1, Round(TileWidthPx)));
    BitmapHeight := Min(cMaxPatternDimension, Max(1, Round(TileHeightPx)));

    origPt := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(TileX, TileY));
    TileX := origPt.X;
    TileY := origPt.Y;
  end;

  if (BitmapWidth <= 0) or (BitmapHeight <= 0) then
    Exit;

  PatternBitmap := TBitmap32.Create;
  try
    PatternBitmap.SetSize(BitmapWidth, BitmapHeight);
    PatternBitmap.DrawMode := dmBlend;

    SavedMatrix := FCurrentMatrix;
    SavedViewport := FViewportRect;

    FCurrentMatrix := IdentityMatrix;
    FViewportRect := FloatRect(0, 0, BitmapWidth, BitmapHeight);

    // Sets up ContentMat to render pattern child geometry onto offscreen tile bitmap
    ContentMat.Matrix := IdentityMatrix;
    if APatternNode.ViewBox.IsValid then
    begin
      TileViewBox := APatternNode.ViewBox;
      ContentMat.Matrix := TileViewBox.GetTransform(FloatRect(0, 0, TileWidthPx, TileHeightPx), APatternNode.PreserveAspectRatio);
    end
    else
    if APatternNode.PatternContentUnits = guObjectBoundingBox then
      ContentMat.Scale(BoundsWidth * MatrixScaleX, BoundsHeight * MatrixScaleY)
    else
      ContentMat.Scale(MatrixScaleX, MatrixScaleY);

    PushMatrix;
    try
      ApplyMatrix(ContentMat.Matrix);

      for i := 0 to APatternNode.Children.Count - 1 do
        RenderNode(PatternBitmap, APatternNode.Children[i]);

    finally
      PopMatrix;
      FCurrentMatrix := SavedMatrix;
      FViewportRect := SavedViewport;
    end;

    if (AOpacity < 1.0) then
      PatternBitmap.MasterAlpha := Round(AOpacity * 256);

    Result := TSvgPatternPolygonFiller.Create(PatternBitmap);

    TSvgPatternPolygonFiller(Result).OffsetX := Round(TileX);
    TSvgPatternPolygonFiller(Result).OffsetY := Round(TileY);
  except
    PatternBitmap.Free;
    raise;
  end;
end;

procedure TSvgRenderer.RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
var
  PathPoints, TransformedPoints: TArrayOfArrayOfFloatPoint;
  Color: TColor32;
  StrokeWidth, MatScale, ScaledOffset: Single;
  Points, StrokePoints, AllRenderPoints: TArrayOfArrayOfFloatPoint;
  ScaledDashArray: TArrayOfFloat;
  i, j, k, x: Integer;
  Filler: TCustomPolygonFiller;
  Bounds, StrokeBounds, FloatRoi: TFloatRect;
  PaintServerNode: TSvgNode;
  RoiRect: TRect;
  NeedsOffscreen: Boolean;
  EffectiveBlendMode: TSvgBlendMode;
  RenderBmp, OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  SourceP, DestP: PColor32;
  AlphaVal, Gray: Byte;
  OldMatrix: TFloatMatrix;
const
  OneOver255: Single = 1 / 255;
begin
  if (APathNode = nil) or (ATarget = nil) then
    Exit;

  // 1. Generate path data in user space
  PathPoints := APathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
  if Length(PathPoints) = 0 then
    Exit;

  // Transform path points into world coordinates
  TransformedPoints := GetTransformedPoints(PathPoints);

  // 2. Generate stroke poly-polygon if stroked
  StrokePoints := nil;
  StrokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Width);
  if (StrokeWidth > 0) and ((APathNode.Stroke.ResolvedPaintServer <> nil) or (not APathNode.Stroke.Color.IsNone)) then
  begin
    MatScale := GetMatrixScale(FCurrentMatrix);
    StrokeWidth := StrokeWidth * MatScale;

    ScaledDashArray := nil;
    ScaledOffset := 0;
    if (APathNode.Stroke.DashArray <> nil) then
    begin
      SetLength(ScaledDashArray, Length(APathNode.Stroke.DashArray));
      for i := 0 to High(APathNode.Stroke.DashArray) do
        ScaledDashArray[i] := APathNode.Stroke.DashArray[i] * MatScale;
      ScaledOffset := APathNode.Stroke.DashOffset * MatScale;
    end;

    for i := 0 to High(TransformedPoints) do
    begin
      if (ScaledDashArray <> nil) then
      begin
        Points := BuildDashedLine(TransformedPoints[i], ScaledDashArray, ScaledOffset, IsClosedContour(TransformedPoints[i]));
        for j := 0 to High(Points) do
          StrokePoints := StrokePoints + BuildPolyPolyLine([Points[j]], False, StrokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
      end else
        StrokePoints := StrokePoints + BuildPolyPolyLine([TransformedPoints[i]], IsClosedContour(TransformedPoints[i]), StrokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
    end;
  end;

  // 3. Calculate ROI bounding box using PolyPolygonBounds AFTER stroking
  // Collect all output poly-polygons produced and rendered by the polygon node
  SetLength(AllRenderPoints, 0);
  if (APathNode.Fill.ResolvedPaintServer <> nil) or (not APathNode.Fill.Color.IsNone) then
    AllRenderPoints := AllRenderPoints + TransformedPoints;
  if Length(StrokePoints) > 0 then
    AllRenderPoints := AllRenderPoints + StrokePoints;

  if Length(AllRenderPoints) = 0 then
    AllRenderPoints := TransformedPoints;

  // Hard Requirement: The PolyPolygonBounds function MUST be used to calculate the ROI AFTER stroking.
  FloatRoi := PolyPolygonBounds(AllRenderPoints);
  RoiRect := MakeRect(FloatRoi, rrOutside);

  // Intersect calculated ROI with target canvas bounds to skip off-screen geometry
  if not GR32.IntersectRect(RoiRect, RoiRect, ATarget.BoundsRect) then
    Exit;

  // 4. Check whether offscreen bitmap compositing is required (opacity, clip-path, mask, blend-mode)
  EffectiveBlendMode := GetEffectiveMixBlendMode(APathNode);
  NeedsOffscreen := (EffectiveBlendMode <> bmNormal) or (APathNode.ResolvedClipPath <> nil) or
                    (APathNode.ResolvedMask <> nil) or (APathNode.Opacity < 1.0);

  OffscreenBmp := nil;
  ClipMaskBmp := nil;
  MaskBmp := nil;

  if NeedsOffscreen then
  begin
    // Defer allocation of ROI bitmap until bounds are actually known after stroking
    OffscreenBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, True);
    RenderBmp := OffscreenBmp;

    // Translate output poly-polygons to ROI coordinate space prior to rendering
    TranslatePolyPolygonInplace(TransformedPoints, -RoiRect.Left, -RoiRect.Top);
    if Length(StrokePoints) > 0 then
      TranslatePolyPolygonInplace(StrokePoints, -RoiRect.Left, -RoiRect.Top);
  end else
    RenderBmp := ATarget;

  try
    if (FPolyRenderer = nil) then
      FPolyRenderer := DefaultPolygonRendererClass.Create(RenderBmp)
    else
      FPolyRenderer.Bitmap := RenderBmp;

    // 5. Fill Rendering
    Bounds := GetPathBounds(TransformedPoints);
    if APathNode.Fill.ResolvedPaintServer <> nil then
    begin
      PaintServerNode := TSvgNode(APathNode.Fill.ResolvedPaintServer);

      Filler := nil;
      if PaintServerNode is TSvgGradientNode then
        Filler := CreateGradientFiller(TSvgGradientNode(PaintServerNode), Bounds, APathNode.Fill.Opacity)
      else
      if PaintServerNode is TSvgPatternNode then
        Filler := CreatePatternFiller(TSvgPatternNode(PaintServerNode), Bounds, APathNode.Fill.Opacity);

      if (Filler <> nil) then
      begin
        try
          FPolyRenderer.Bitmap := RenderBmp;
          FPolyRenderer.Filler := Filler;
          try
            FPolyRenderer.FillMode := APathNode.Fill.FillRule;
            FPolyRenderer.PolyPolygonFS(TransformedPoints);
          finally
            FPolyRenderer.Filler := nil;
          end;
        finally
          Filler.Free;
        end;
      end;
    end else
    if (not APathNode.Fill.Color.IsNone) then
    begin
      Color := APathNode.Fill.Color.Color;
      if (APathNode.Fill.Opacity < 1.0) then
        ScaleAlpha(Color, APathNode.Fill.Opacity);

      if (AlphaComponent(Color) > 0) then
      begin
        FPolyRenderer.Bitmap := RenderBmp;
        FPolyRenderer.Color := Color;
        FPolyRenderer.FillMode := APathNode.Fill.FillRule;

        FPolyRenderer.PolyPolygonFS(TransformedPoints);
      end;
    end;

    // 6. Stroke Rendering
    if Length(StrokePoints) > 0 then
    begin
      if (APathNode.Stroke.ResolvedPaintServer <> nil) then
      begin
        PaintServerNode := TSvgNode(APathNode.Stroke.ResolvedPaintServer);
        StrokeBounds := GetPathBounds(StrokePoints);
        Filler := nil;
        if PaintServerNode is TSvgGradientNode then
          Filler := CreateGradientFiller(TSvgGradientNode(PaintServerNode), StrokeBounds, APathNode.Stroke.Opacity)
        else
        if PaintServerNode is TSvgPatternNode then
          Filler := CreatePatternFiller(TSvgPatternNode(PaintServerNode), StrokeBounds, APathNode.Stroke.Opacity);

        if (Filler <> nil) then
        begin
          try
            FPolyRenderer.Bitmap := RenderBmp;
            FPolyRenderer.Filler := Filler;
            try
              FPolyRenderer.FillMode := pfWinding;
              FPolyRenderer.PolyPolygonFS(StrokePoints);
            finally
              FPolyRenderer.Filler := nil;
            end;
          finally
            Filler.Free;
          end;
        end;
      end else
      if (not APathNode.Stroke.Color.IsNone) then
      begin
        Color := APathNode.Stroke.Color.Color;
        if (APathNode.Stroke.Opacity < 1.0) then
          ScaleAlpha(Color, APathNode.Stroke.Opacity);

        if (AlphaComponent(Color) > 0) then
        begin
          FPolyRenderer.Bitmap := RenderBmp;
          FPolyRenderer.Color := Color;
          FPolyRenderer.FillMode := pfWinding;

          FPolyRenderer.PolyPolygonFS(StrokePoints);
        end;
      end;
    end;

    // 7. Markers Rendering (per SVG specification, markers paint on top of fill and stroke)
    if (APathNode.ResolvedMarkerStart <> nil) or (APathNode.ResolvedMarkerMid <> nil) or (APathNode.ResolvedMarkerEnd <> nil) then
    begin
      StrokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Width);
      if NeedsOffscreen then
      begin
        PushMatrix;
        try
          TFloatMatrixHelper(FCurrentMatrix).Translate(-RoiRect.Left, -RoiRect.Top);
          RenderMarkers(RenderBmp, APathNode, PathPoints, StrokeWidth);
        finally
          PopMatrix;
        end;
      end else
        RenderMarkers(RenderBmp, APathNode, PathPoints, StrokeWidth);
    end;

    // 8. Apply Offscreen Compositing (ClipPath, Mask, Opacity, Blend)
    if NeedsOffscreen then
    begin
      // Apply ClipPath in ROI space
      if (APathNode.ResolvedClipPath <> nil) then
      begin
        ClipNodeTarget := APathNode.ResolvedClipPath;
        ClipMaskBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, True);

        OldMatrix := FCurrentMatrix;
        PushMatrix;
        try
          TFloatMatrixHelper(FCurrentMatrix).Translate(-RoiRect.Left, -RoiRect.Top);
          RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, FloatRoi, RoiRect);
        finally
          PopMatrix;
          FCurrentMatrix := OldMatrix;
        end;

        SourceP := PColor32(OffscreenBmp.Bits);
        DestP := PColor32(ClipMaskBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          AlphaVal := AlphaComponent(DestP^);
          if AlphaVal < 255 then
            ScaleAlpha(SourceP^, AlphaVal * OneOver255);
          Inc(SourceP);
          Inc(DestP);
        end;
      end;

      // Apply Alpha Mask in ROI space
      if (APathNode.ResolvedMask <> nil) then
      begin
        MaskNodeTarget := APathNode.ResolvedMask;
        MaskBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, False);

        OldMatrix := FCurrentMatrix;
        PushMatrix;
        try
          TFloatMatrixHelper(FCurrentMatrix).Translate(-RoiRect.Left, -RoiRect.Top);
          RenderMaskNode(MaskBmp, MaskNodeTarget, FloatRoi, RoiRect);
        finally
          PopMatrix;
          FCurrentMatrix := OldMatrix;
        end;

        SourceP := PColor32(OffscreenBmp.Bits);
        DestP := PColor32(MaskBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          Gray := Intensity(DestP^);
          Gray := Round(Gray * (AlphaComponent(DestP^) * OneOver255));
          ScaleAlpha(SourceP^, Gray * OneOver255);
          Inc(SourceP);
          Inc(DestP);
        end;
      end;

      // Apply Opacity
      if (APathNode.Opacity < 1.0) then
      begin
        SourceP := PColor32(OffscreenBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          ScaleAlpha(SourceP^, APathNode.Opacity);
          Inc(SourceP);
        end;
      end;

      // Blend ROI offscreen surface onto destination target at ROI origin
      BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, RoiRect.Left, RoiRect.Top);
    end;

  finally
    ReleaseOffscreenBitmap(OffscreenBmp);
    ReleaseOffscreenBitmap(ClipMaskBmp);
    ReleaseOffscreenBitmap(MaskBmp);
  end;
end;

procedure TSvgRenderer.RenderClipPathNode(AMaskBmp: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
var
  SavedMatrix: TFloatMatrix;
  Width, Height, OffsetX, OffsetY: Single;
  i: Integer;
begin
  // Renders child nodes of a <clipPath> onto a temporary alpha surface.
  // If clipPathUnits = guObjectBoundingBox, applies translation and
  // scale derived from target object bounds in ROI coordinate space.
  if (AClipNode = nil) or (AMaskBmp = nil) then
    Exit;

  SavedMatrix := FCurrentMatrix;
  try
    if AClipNode.ClipPathUnits = guObjectBoundingBox then
    begin
      Width := ATargetBounds.Width;
      Height := ATargetBounds.Height;
      if Width <= 0 then
        Width := 1.0;
      if Height <= 0 then
        Height := 1.0;

      OffsetX := ATargetBounds.Left - ARoiRect.Left;
      OffsetY := ATargetBounds.Top - ARoiRect.Top;

      FCurrentMatrix := IdentityMatrix;
      TFloatMatrixHelper(FCurrentMatrix).Scale(Width, Height);
      TFloatMatrixHelper(FCurrentMatrix).Translate(OffsetX, OffsetY);
    end;

    // Pre-multiply the matrix transformation on the <clipPath> element itself
    // (e.g. transform="rotate(...)" or transform="matrix(...)")
    if not IsIdentityMatrix(AClipNode.Transform) then
      ApplyMatrix(AClipNode.Transform);

    AMaskBmp.Clear(0); // Clear to 0 transparent so filled shapes paint non-zero alpha inside clip region

    for i := 0 to AClipNode.Children.Count - 1 do
      RenderNode(AMaskBmp, AClipNode.Children[i]);
  finally
    FCurrentMatrix := SavedMatrix;
  end;
end;

procedure TSvgRenderer.RenderMaskNode(AMaskBmp: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
var
  SavedMatrix: TFloatMatrix;
  Width, Height, OffsetX, OffsetY: Single;
  i: Integer;
begin
  // Renders child nodes of a <mask> onto a temporary luminance/alpha surface.
  // If maskContentUnits = guObjectBoundingBox, applies translation and scale
  // derived from target object bounds in ROI coordinate space.

  if (AMaskNode = nil) or (AMaskBmp = nil) then
    Exit;

  SavedMatrix := FCurrentMatrix;
  try
    if AMaskNode.MaskContentUnits = guObjectBoundingBox then
    begin
      Width := ATargetBounds.Width;
      Height := ATargetBounds.Height;
      if Width <= 0 then
        Width := 1.0;
      if Height <= 0 then
        Height := 1.0;

      OffsetX := ATargetBounds.Left - ARoiRect.Left;
      OffsetY := ATargetBounds.Top - ARoiRect.Top;

      FCurrentMatrix := IdentityMatrix;
      TFloatMatrixHelper(FCurrentMatrix).Scale(Width, Height);
      TFloatMatrixHelper(FCurrentMatrix).Translate(OffsetX, OffsetY);
    end;

    // Pre-multiply matrix transformation on <mask transform="..."> elements
    if not IsIdentityMatrix(AMaskNode.Transform) then
      ApplyMatrix(AMaskNode.Transform);

    AMaskBmp.Clear(clBlack32);

    for i := 0 to AMaskNode.Children.Count - 1 do
      RenderNode(AMaskBmp, AMaskNode.Children[i]);
  finally
    FCurrentMatrix := SavedMatrix;
  end;
end;

procedure TSvgRenderer.RenderMarker(ATarget: TCustomBitmap32; AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single);
var
  mw, mh, rx, ry, scaleX, scaleY: Single;
  markerMat: TFloatMatrixHelper;
  viewMat: TFloatMatrix;
  vpW, vpH: Single;
  i: Integer;
  markerViewBox: TSvgViewBox;
begin
  if (AMarker = nil) or (AMarker.Children.Count = 0) or (ATarget = nil) then
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
      RenderNode(ATarget, AMarker.Children[i]);
  finally
    PopMatrix;
  end;
end;

procedure TSvgRenderer.RenderMarkers(ATarget: TCustomBitmap32; APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single);

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
      RenderMarker(ATarget, startMarker, contour[0], vAngle, AStrokeWidth);
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
        RenderMarker(ATarget, midMarker, contour[i], vAngle, AStrokeWidth);
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
      RenderMarker(ATarget, endMarker, contour[ptCount - 1], vAngle, AStrokeWidth);
    end;
  end;
end;

procedure TSvgRenderer.RenderGroupNode(ATarget: TCustomBitmap32; AGroupNode: TSvgGroupNode);
var
  i, k: Integer;
  OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  x: Integer;
  SourceP, DestP: PColor32;
  Gray: Byte;
  Node: TSvgNode;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  AlphaVal: Byte;
  OldMatrix: TFloatMatrix;
  GroupBounds, TargetWorldBounds: TFloatRect;
  GroupRoi: TRect;
  Points: array[0..3] of TFloatPoint;
const
  OneOver255: Single = 1 / 255;
begin
  if (AGroupNode = nil) or AGroupNode.IsDisplayNone or (not AGroupNode.PassesConditionalProcessing) then
    Exit;

  // Offscreen rendering required if we have transparency, a ClipPath, a Mask, or if Isolation = isoIsolate
  if (AGroupNode.Opacity < 1.0) or (AGroupNode.ResolvedClipPath <> nil) or (AGroupNode.ResolvedMask <> nil) or
     (AGroupNode.Isolation = isoIsolate) then
  begin
    if (ATarget = nil) then
      Exit;

    // Calculate group bounding box in world space for objectBoundingBox units and ROI determination
    GroupBounds := AGroupNode.GetObjectBoundingBox;
    Points[0] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(GroupBounds.Left, GroupBounds.Top));
    Points[1] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(GroupBounds.Right, GroupBounds.Top));
    Points[2] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(GroupBounds.Right, GroupBounds.Bottom));
    Points[3] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(GroupBounds.Left, GroupBounds.Bottom));

    TargetWorldBounds := FloatRect(Points[0].X, Points[0].Y, Points[0].X, Points[0].Y);
    for k := 1 to 3 do
    begin
      if (Points[k].X < TargetWorldBounds.Left) then
        TargetWorldBounds.Left := Points[k].X;
      if (Points[k].X > TargetWorldBounds.Right) then
        TargetWorldBounds.Right := Points[k].X;
      if (Points[k].Y < TargetWorldBounds.Top) then
        TargetWorldBounds.Top := Points[k].Y;
      if (Points[k].Y > TargetWorldBounds.Bottom) then
        TargetWorldBounds.Bottom := Points[k].Y;
    end;

    // Convert world target bounds to integer pixel ROI and intersect with target bitmap bounds
    GroupRoi := MakeRect(TargetWorldBounds, rrOutside);
    if not GR32.IntersectRect(GroupRoi, GroupRoi, ATarget.BoundsRect) then
      Exit;

    // Acquire reusable offscreen surface sized strictly to the calculated ROI
    OffscreenBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, True);
    ClipMaskBmp := nil;
    MaskBmp := nil;
    try
      // Translate matrix to ROI coordinate space
      PushMatrix;
      try
        TFloatMatrixHelper(FCurrentMatrix).Translate(-GroupRoi.Left, -GroupRoi.Top);

        if AGroupNode is TSvgSwitchNode then
        begin
          if (TSvgSwitchNode(AGroupNode).SelectedChild <> nil) then
            RenderNode(OffscreenBmp, TSvgSwitchNode(AGroupNode).SelectedChild);
        end else
        begin
          for i := 0 to AGroupNode.Children.Count - 1 do
            RenderNode(OffscreenBmp, AGroupNode.Children[i]);
        end;
      finally
        PopMatrix;
      end;

      // Apply ClipPath in ROI coordinate space
      if (AGroupNode.ResolvedClipPath <> nil) then
      begin
        ClipNodeTarget := AGroupNode.ResolvedClipPath;
        ClipMaskBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, True);

        OldMatrix := FCurrentMatrix;
        PushMatrix;
        try
          TFloatMatrixHelper(FCurrentMatrix).Translate(-GroupRoi.Left, -GroupRoi.Top);
          RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, TargetWorldBounds, GroupRoi);
        finally
          PopMatrix;
          FCurrentMatrix := OldMatrix;
        end;

        SourceP := PColor32(OffscreenBmp.Bits);
        DestP := PColor32(ClipMaskBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          AlphaVal := AlphaComponent(DestP^);
          if AlphaVal < 255 then
            ScaleAlpha(SourceP^, AlphaVal * OneOver255);
          Inc(SourceP);
          Inc(DestP);
        end;
      end;

      // Apply Alpha Mask in ROI coordinate space
      if (AGroupNode.ResolvedMask <> nil) then
      begin
        MaskNodeTarget := AGroupNode.ResolvedMask;
        MaskBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, False);

        OldMatrix := FCurrentMatrix;
        PushMatrix;
        try
          TFloatMatrixHelper(FCurrentMatrix).Translate(-GroupRoi.Left, -GroupRoi.Top);
          RenderMaskNode(MaskBmp, MaskNodeTarget, TargetWorldBounds, GroupRoi);
        finally
          PopMatrix;
          FCurrentMatrix := OldMatrix;
        end;

        SourceP := PColor32(OffscreenBmp.Bits);
        DestP := PColor32(MaskBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          Gray := Intensity(DestP^);
          Gray := Round(Gray * (AlphaComponent(DestP^) / 255.0));
          ScaleAlpha(SourceP^, Gray / 255.0);
          Inc(SourceP);
          Inc(DestP);
        end;
      end;

      // Apply Group Opacity
      if (AGroupNode.Opacity < 1.0) then
      begin
        SourceP := PColor32(OffscreenBmp.Bits);
        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          ScaleAlpha(SourceP^, AGroupNode.Opacity);
          Inc(SourceP);
        end;
      end;

      // Blend ROI offscreen surface onto target canvas at group ROI origin
      BlendOffscreenSurface(ATarget, OffscreenBmp, AGroupNode.MixBlendMode, GroupRoi.Left, GroupRoi.Top);

    finally
      ReleaseOffscreenBitmap(OffscreenBmp);
      ReleaseOffscreenBitmap(ClipMaskBmp);
      ReleaseOffscreenBitmap(MaskBmp);
    end;

    Exit;
  end;

  if AGroupNode is TSvgSwitchNode then
  begin
    if (TSvgSwitchNode(AGroupNode).SelectedChild <> nil) then
      RenderNode(ATarget, TSvgSwitchNode(AGroupNode).SelectedChild);
  end else
  begin
    for i := 0 to AGroupNode.Children.Count - 1 do
      RenderNode(ATarget, AGroupNode.Children[i]);
  end;
end;

procedure TSvgRenderer.RenderFilter(ATarget: TCustomBitmap32; AFilterNode: TSvgFilterNode; ANode: TSvgNode);

  function ResolveSurface(const AInput: TSvgFilterInput; SourceGraphic, SourceAlpha, CurrentSurface, DefaultFallback: TCustomBitmap32; const NamedSurfaces: TArray<TCustomBitmap32>): TCustomBitmap32;
  begin
    Result := DefaultFallback;

    case AInput.Kind of
      fikSourceGraphic:
        Result := SourceGraphic;

      fikSourceAlpha:
        Result := SourceAlpha;

      fikNamedResult:
        if (AInput.Index >= 0) and (AInput.Index <= High(NamedSurfaces)) and (NamedSurfaces[AInput.Index] <> nil) then
          Result := NamedSurfaces[AInput.Index];
    end;
  end;

  procedure ApplyColorMatrix(ASrc, ADest: TCustomBitmap32; AType: TSvgFeColorMatrixType; const AValues: TArrayOfFloat);
  var
    i: Integer;
    pSource, pDest: PColor32Entry;
    M: array[0..19] of integer;
    CosVal, SinVal: Single;
    Lum: integer;
    n: integer;
    LastSource, LastDest: TColor32;
    HasLast: Boolean;
  begin
    pSource := PColor32Entry(ASrc.Bits);
    pDest := PColor32Entry(ADest.Bits);

    // Cache last procesed pixel; If the new input is the same as the previous, the new output will be the same as the previous
    HasLast := False;
    LastSource := 0;
    LastDest := 0;

    case AType of
      cmMatrix:
        if (Length(AValues) > 0) then
        begin
          n := Min(High(M), High(AValues));
          for i := 0 to n do
          begin
            case i of
              4, 9, 14, 19:
                // Column index 4, 9, 14, 19 are constant offsets scaled by 255
                M[i] := Round(AValues[i] * 16711680);
            else
              M[i] := Round(AValues[i] * 65536);
            end;
          end;
          for i := n + 1 to High(M) do
          begin
            case i of
              0, 6, 12, 18:
                // R = R, G = G, B = B, A = A
                M[i] := 65536;
            else
              M[i] := 0;
            end;
          end;

          for i := 0 to ASrc.PixelCount - 1 do
          begin
            if (not HasLast) or (pSource.ARGB <> LastSource) then
            begin
              pDest.R := Clamp((M[0]  * pSource.R + M[1]  * pSource.G + M[2]  * pSource.B + M[3]  * pSource.A + M[4])  div 65536);
              pDest.G := Clamp((M[5]  * pSource.R + M[6]  * pSource.G + M[7]  * pSource.B + M[8]  * pSource.A + M[9])  div 65536);
              pDest.B := Clamp((M[10] * pSource.R + M[11] * pSource.G + M[12] * pSource.B + M[13] * pSource.A + M[14]) div 65536);
              pDest.A := Clamp((M[15] * pSource.R + M[16] * pSource.G + M[17] * pSource.B + M[18] * pSource.A + M[19]) div 65536);

              LastSource := pSource.ARGB;
              LastDest := pDest.ARGB;
              HasLast := True;
            end else
              pDest.ARGB := LastDest;

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;

      cmSaturate:
        if (Length(AValues) > 0) and (AValues[0] <> 1.0) then
        begin
          // For Saturate, W3C SVG specifies interpolating each pixel's color
          // towards its luminance based on saturation:
          //
          //   ColorOut = Y + Saturation x (ColorIn - Y)
          //
          // W3C NTSC/Rec. 601 luminance:
          //
          //   Y = 0.213 x R + 0.715 x G + 0.072 x B
          //
          // We convert 0.213, 0.715, 0.072 to Q16 fixed-point
          //   13959 + 46858 + 4719 = 65536
          // and store Saturation as a Q8 fixed-point factor
          //   Round(n * 256)

          // Coefficient (Q8)
          n := Clamp(Round(AValues[0] * 256), 0, 65535);

          for i := 0 to ASrc.PixelCount - 1 do
          begin
            if (not HasLast) or ((pSource.ARGB and $00FFFFFF) <> LastSource) then
            begin
              // Rec. 601 W3C NTSC Luminance (Q8)
              Lum := (integer(13959 * pSource.R) + integer(46858 * pSource.G) + integer(4719 * pSource.B)) div 65536;

              // Output = Luminance + Saturation * (Channel - Luminance)
              pDest.R := Clamp(Lum + ((n * (integer(pSource.R) - Lum)) div 256));
              pDest.G := Clamp(Lum + ((n * (integer(pSource.G) - Lum)) div 256));
              pDest.B := Clamp(Lum + ((n * (integer(pSource.B) - Lum)) div 256));
              pDest.A := pSource.A; // Preserve alpha

              LastSource := pSource.ARGB and $00FFFFFF;
              LastDest := pDest.ARGB and $00FFFFFF;
              HasLast := True;
            end else
              pDest.ARGB := LastDest or (pSource.ARGB and $FF000000);

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;

      cmLuminanceToAlpha:
        begin
          for i := 0 to ASrc.PixelCount - 1 do
          begin
            // W3C SVG / Rec. 709 luminance: Y = 0.2126*R + 0.7152*G + 0.0722*B
            // Note: Do not use ColorLightness as that depends on various compiler defines
            pDest.ARGB := ((13933 * pSource.R + 46871 * pSource.G + 4732 * pSource.B) div 65536) shl 24;

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;

      cmHueRotate:
        if (Length(AValues) > 0) and (AValues[0] <> 0.0) then
        begin
          // For Hue Rotate, the W3C matrix rotates linear RGB around the luminance
          // axis. Since the 5x4 matrix is constant across all pixels for a
          // given angle, we can pre-compute the 3 RGB row linear equations in
          // Q16 fixed-point math once per filter step.

          GR32_Math.SinCos(DegToRad(AValues[0]), SinVal, CosVal);

          // Scale matrix coefficients to Q16 integer (65536 = 1.0)
          M[0] := Round((0.213 + CosVal * 0.787 - SinVal * 0.213) * 65536);
          M[1] := Round((0.715 - CosVal * 0.715 - SinVal * 0.715) * 65536);
          M[2] := Round((0.072 - CosVal * 0.072 + SinVal * 0.928) * 65536);

          M[3] := Round((0.213 - CosVal * 0.213 + SinVal * 0.143) * 65536);
          M[4] := Round((0.715 + CosVal * 0.285 + SinVal * 0.140) * 65536);
          M[5] := Round((0.072 - CosVal * 0.072 - SinVal * 0.283) * 65536);

          M[6] := Round((0.213 - CosVal * 0.213 - SinVal * 0.787) * 65536);
          M[7] := Round((0.715 - CosVal * 0.715 + SinVal * 0.715) * 65536);
          M[8] := Round((0.072 + CosVal * 0.928 + SinVal * 0.072) * 65536);

          for i := 0 to ASrc.PixelCount - 1 do
          begin
            if (not HasLast) or ((pSource.ARGB and $00FFFFFF) <> LastSource) then
            begin
              pDest.R := Clamp((M[0] * pSource.R + M[1] * pSource.G + M[2] * pSource.B) div 65536);
              pDest.G := Clamp((M[3] * pSource.R + M[4] * pSource.G + M[5] * pSource.B) div 65536);
              pDest.B := Clamp((M[6] * pSource.R + M[7] * pSource.G + M[8] * pSource.B) div 65536);
              pDest.A := pSource.A; // Preserve alpha

              LastSource := pSource.ARGB and $00FFFFFF;
              LastDest := pDest.ARGB and $00FFFFFF;
              HasLast := True;
            end else
              pDest.ARGB := LastDest or (pSource.ARGB and $FF000000);

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;
    end;

    // Default: Pass-through
    Move(pSource^, pDest^, ASrc.ByteCount);
  end;

  procedure ApplyComposite(ASrc1, ASrc2, ADest: TCustomBitmap32; AOp: TSvgCompositeOperator; K1, K2, K3, K4: Single);
  type
    TPixelCombiner = function(F: TColor32; B: TColor32): TColor32 of object;
  var
    i, Count: integer;
    pSource1, pSource2, pDest: PColor32Entry;
    Blender: TCustomGraphics32Blender;
    Combiner: TPixelCombiner;
    c1, c2, c3, c4: Int64;
    vR, vG, vB, vA: integer;
  const
    OneOver255: Single = 1 / 255;
  begin
    Count := ASrc1.PixelCount;
    pSource1 := PColor32Entry(ASrc1.Bits);
    pSource2 := PColor32Entry(ASrc2.Bits);
    pDest := PColor32Entry(ADest.Bits);

    case AOp of
      coOver:
        begin
          // ASrc1 (in) composited over ASrc2 (in2) onto ADest
          ASrc2.DrawTo(ADest, 0, 0);
          ASrc1.DrawTo(ADest, 0, 0);
        end;

      coIn:
        begin
          Blender := TGraphics32BlenderSrcIn.Create;
          try
            Combiner := Blender.Blend;
            for i := 0 to Count - 1 do
            begin
              pDest.ARGB := Combiner(pSource1.ARGB, pSource2.ARGB);
              Inc(pSource1); Inc(pSource2); Inc(pDest);
            end;
          finally
            Blender.Free;
          end;
        end;

      coOut:
        begin
          Blender := TGraphics32BlenderSrcOut.Create;
          try
            Combiner := Blender.Blend;
            for i := 0 to Count - 1 do
            begin
              pDest.ARGB := Combiner(pSource1.ARGB, pSource2.ARGB);
              Inc(pSource1); Inc(pSource2); Inc(pDest);
            end;
          finally
            Blender.Free;
          end;
        end;

      coAtop:
        begin
          Blender := TGraphics32BlenderSrcAtop.Create;
          try
            Combiner := Blender.Blend;
            for i := 0 to Count - 1 do
            begin
              pDest.ARGB := Combiner(pSource1.ARGB, pSource2.ARGB);
              Inc(pSource1); Inc(pSource2); Inc(pDest);
            end;
          finally
            Blender.Free;
          end;
        end;

      coXor:
        begin
          Blender := TGraphics32BlenderXor.Create;
          try
            Combiner := Blender.Blend;
            for i := 0 to Count - 1 do
            begin
              pDest.ARGB := Combiner(pSource1.ARGB, pSource2.ARGB);
              Inc(pSource1); Inc(pSource2); Inc(pDest);
            end;
          finally
            Blender.Free;
          end;
        end;

      coLighter:
        begin
          for i := 0 to Count - 1 do
          begin
            pDest.ARGB := ColorAdd(pSource1.ARGB, pSource2.ARGB);
            Inc(pSource1); Inc(pSource2); Inc(pDest);
          end;
        end;

      coArithmetic:
        begin
          // W3C SVG arithmetic composite operator formula:
          // result = K1 * in1 * in2 + K2 * in1 + K3 * in2 + K4
          // Inputs and outputs normalized to [0, 1]. For byte values in [0, 255]:
          // result_byte = K1 * (F * B / 255) + K2 * F + K3 * B + K4 * 255
          // We precalculate Q16 fixed-point factors scaled by 65536.
          c1 := Round((K1 * OneOver255) * 65536.0);
          c2 := Round(K2 * 65536.0);
          c3 := Round(K3 * 65536.0);
          c4 := Round(K4 * 255.0 * 65536.0);

          for i := 0 to Count - 1 do
          begin
            vR := (c1 * pSource1.R * pSource2.R + c2 * pSource1.R + c3 * pSource2.R + c4) div 65536;
            vG := (c1 * pSource1.G * pSource2.G + c2 * pSource1.G + c3 * pSource2.G + c4) div 65536;
            vB := (c1 * pSource1.B * pSource2.B + c2 * pSource1.B + c3 * pSource2.B + c4) div 65536;
            // Note: The W3C SVG specs require that the same formula is used on all four channels.
            // Some implementations incorrectly uses the Porter-Duff alpha formula:
            //   1 - (1-F.A) * (1-B.A) = (((F.A xor 255) * (B.A xor 255)) shr 8) xor 255
            vA := (c1 * pSource1.A * pSource2.A + c2 * pSource1.A + c3 * pSource2.A + c4) div 65536;

            pDest.R := Clamp(vR);
            pDest.G := Clamp(vG);
            pDest.B := Clamp(vB);
            pDest.A := Clamp(vA);

            Inc(pSource1); Inc(pSource2); Inc(pDest);
          end;
        end;
    end;
  end;

var
  SourceGraphic, SourceAlpha, CurrentSurface, Input1, Input2, TempSurface, DestSurface, CachedSurface: TCustomBitmap32;
  NamedSurfaces: TArray<TCustomBitmap32>;
  i, j, Count: Integer;
  Node, ChildNode: TSvgNode;
  pSource, pDest: PColor32;
  FloodColor: TColor32;
  dxInt, dyInt: Integer;
  SourceBounds, FilterBounds: TFloatRect;
  PathNode: TSvgPathNode;
  PathPoints, TransformedPoints, StrokePoints, AllRenderPoints: TArrayOfArrayOfFloatPoint;
  StrokeWidth, MatScale, ScaledOffset: Single;
  ScaledDashArray: TArrayOfFloat;
  Points: TArrayOfArrayOfFloatPoint;
  NodeBounds: TFloatRect;
  Pts: array[0..3] of TFloatPoint;
  MarginX, MarginY, RadiusX, RadiusY: Integer;
  FilterRoi: TRect;
begin
  if (AFilterNode = nil) or (ANode = nil) or (ATarget = nil) then
    Exit;

  PushMatrix;
  try
    ApplyMatrix(ANode.Transform);

    // 1. Calculate source bounds in world target space.
    // For polygon nodes, source bounds are calculated using PolyPolygonBounds after stroking.
    MatScale := GetMatrixScale(FCurrentMatrix);

    if ANode is TSvgPathNode then
    begin
      PathNode := TSvgPathNode(ANode);
      PathPoints := PathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
      if Length(PathPoints) > 0 then
      begin
        TransformedPoints := GetTransformedPoints(PathPoints);
        StrokePoints := nil;
        StrokeWidth := PathNode.Stroke.Width.ToPixels(FViewportRect.Width);
        if (StrokeWidth > 0) and ((PathNode.Stroke.ResolvedPaintServer <> nil) or (not PathNode.Stroke.Color.IsNone)) then
        begin
          StrokeWidth := StrokeWidth * MatScale;
          ScaledDashArray := nil;
          ScaledOffset := 0;
          if (PathNode.Stroke.DashArray <> nil) then
          begin
            SetLength(ScaledDashArray, Length(PathNode.Stroke.DashArray));
            for i := 0 to High(PathNode.Stroke.DashArray) do
              ScaledDashArray[i] := PathNode.Stroke.DashArray[i] * MatScale;
            ScaledOffset := PathNode.Stroke.DashOffset * MatScale;
          end;

          for i := 0 to High(TransformedPoints) do
          begin
            if (ScaledDashArray <> nil) then
            begin
              Points := BuildDashedLine(TransformedPoints[i], ScaledDashArray, ScaledOffset, IsClosedContour(TransformedPoints[i]));
              for j := 0 to High(Points) do
                StrokePoints := StrokePoints + BuildPolyPolyLine([Points[j]], False, StrokeWidth, PathNode.Stroke.JoinStyle, PathNode.Stroke.EndStyle, PathNode.Stroke.MiterLimit);
            end else
              StrokePoints := StrokePoints + BuildPolyPolyLine([TransformedPoints[i]], IsClosedContour(TransformedPoints[i]), StrokeWidth, PathNode.Stroke.JoinStyle, PathNode.Stroke.EndStyle, PathNode.Stroke.MiterLimit);
          end;
        end;

        SetLength(AllRenderPoints, 0);
        if (PathNode.Fill.ResolvedPaintServer <> nil) or (not PathNode.Fill.Color.IsNone) then
          AllRenderPoints := AllRenderPoints + TransformedPoints;
        if Length(StrokePoints) > 0 then
          AllRenderPoints := AllRenderPoints + StrokePoints;
        if Length(AllRenderPoints) = 0 then
          AllRenderPoints := TransformedPoints;

        SourceBounds := PolyPolygonBounds(AllRenderPoints);
      end else
        SourceBounds := FloatRect(0, 0, 0, 0);
    end else
    begin
      // For non-polygon nodes, calculate object bounds in world target space
      NodeBounds := ANode.GetObjectBoundingBox;
      Pts[0] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Left, NodeBounds.Top));
      Pts[1] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Right, NodeBounds.Top));
      Pts[2] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Right, NodeBounds.Bottom));
      Pts[3] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Left, NodeBounds.Bottom));

      SourceBounds := FloatRect(Pts[0].X, Pts[0].Y, Pts[0].X, Pts[0].Y);
      for i := 1 to 3 do
      begin
        if (Pts[i].X < SourceBounds.Left) then SourceBounds.Left := Pts[i].X;
        if (Pts[i].X > SourceBounds.Right) then SourceBounds.Right := Pts[i].X;
        if (Pts[i].Y < SourceBounds.Top) then SourceBounds.Top := Pts[i].Y;
        if (Pts[i].Y > SourceBounds.Bottom) then SourceBounds.Bottom := Pts[i].Y;
      end;
    end;

    // 2. Calculate filter-dependent margins (e.g. Gaussian blur radius, offset dx/dy)
    MarginX := 0;
    MarginY := 0;
    for Node in AFilterNode.Children do
    begin
      if Node is TSvgFeGaussianBlurNode then
      begin
        RadiusX := Ceil(TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * MatScale);
        RadiusY := Ceil(TSvgFeGaussianBlurNode(Node).StdDeviationY * GaussianSigmaToRadius * MatScale);
        if RadiusX > MarginX then MarginX := RadiusX + 2;
        if RadiusY > MarginY then MarginY := RadiusY + 2;
      end else
      if Node is TSvgFeOffsetNode then
      begin
        MarginX := MarginX + Ceil(Abs(TSvgFeOffsetNode(Node).Dx) * MatScale);
        MarginY := MarginY + Ceil(Abs(TSvgFeOffsetNode(Node).Dy) * MatScale);
      end;
    end;

    // 3. Inflate source bounds by filter margins to determine total Filter ROI
    FilterBounds := SourceBounds;
    FilterBounds.Left := FilterBounds.Left - MarginX;
    FilterBounds.Top := FilterBounds.Top - MarginY;
    FilterBounds.Right := FilterBounds.Right + MarginX;
    FilterBounds.Bottom := FilterBounds.Bottom + MarginY;

    FilterRoi := MakeRect(FilterBounds, rrOutside);
    if not GR32.IntersectRect(FilterRoi, FilterRoi, ATarget.BoundsRect) then
      Exit;

    // 4. Allocate intermediate filter surfaces constrained to the ROI dimensions
    SourceGraphic := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);
    SourceAlpha := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);
    NamedSurfaces := nil;
    try
      // Render element into SourceGraphic in ROI coordinate space
      PushMatrix;
      try
        TFloatMatrixHelper(FCurrentMatrix).Translate(-FilterRoi.Left, -FilterRoi.Top);
        RenderNodeContent(SourceGraphic, ANode);
      finally
        PopMatrix;
      end;

      // Derive SourceAlpha from SourceGraphic
      pSource := PColor32(SourceGraphic.Bits);
      pDest := PColor32(SourceAlpha.Bits);
      for i := 0 to SourceGraphic.PixelCount - 1 do
      begin
        PColor32Entry(pDest).ARGB := PColor32Entry(pSource).A shl 24;
        Inc(pSource);
        Inc(pDest);
      end;

      CurrentSurface := SourceGraphic;

      // Count number of named surfaces so we can preallocate the surface array
      Count := 0;
      for Node in AFilterNode.Children do
        if (Node is TSvgFilterPrimitiveNode) and TSvgFilterPrimitiveNode(Node).IsReferenceTarget then
          Inc(Count);
      SetLength(NamedSurfaces, Count);
      Count := 0;

      // 5. Process filter primitive nodes sequentially on ROI surfaces
      CachedSurface := nil;
      for Node in AFilterNode.Children do
      begin
        if not (Node is TSvgFilterPrimitiveNode) then
          Continue;

        if Node is TSvgFeGaussianBlurNode then
        begin
          Input1 := ResolveSurface(TSvgFeGaussianBlurNode(Node).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
          ReleaseOffscreenBitmap(CachedSurface);
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          if TSvgFeGaussianBlurNode(Node).StdDeviationX > 0 then
            Blur32(Input1, DestSurface, TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * MatScale)
          else
            Input1.CopyMapTo(DestSurface);

          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeColorMatrixNode then
        begin
          Input1 := ResolveSurface(TSvgFeColorMatrixNode(Node).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
          ReleaseOffscreenBitmap(CachedSurface);
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          ApplyColorMatrix(Input1, DestSurface, TSvgFeColorMatrixNode(Node).MatrixType, TSvgFeColorMatrixNode(Node).Values);
          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeBlendNode then
        begin
          Input1 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
          Input2 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn2, SourceGraphic, SourceAlpha, CurrentSurface, SourceGraphic, NamedSurfaces);
          ReleaseOffscreenBitmap(CachedSurface);
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          Input2.DrawTo(DestSurface, 0, 0);
          BlendOffscreenSurface(DestSurface, Input1, TSvgFeBlendNode(Node).Mode);

          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeCompositeNode then
        begin
          Input1 := ResolveSurface(TSvgFeCompositeNode(Node).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
          Input2 := ResolveSurface(TSvgFeCompositeNode(Node).ResolvedIn2, SourceGraphic, SourceAlpha, CurrentSurface, SourceGraphic, NamedSurfaces);
          ReleaseOffscreenBitmap(CachedSurface);
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          ApplyComposite(Input1, Input2, DestSurface, TSvgFeCompositeNode(Node).CompositeOperator,
            TSvgFeCompositeNode(Node).K1, TSvgFeCompositeNode(Node).K2, TSvgFeCompositeNode(Node).K3, TSvgFeCompositeNode(Node).K4);
          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeMergeNode then
        begin
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          for ChildNode in TSvgFeMergeNode(Node).Children do
          begin
            if ChildNode is TSvgFeMergeNodeChild then
            begin
              Input1 := ResolveSurface(TSvgFeMergeNodeChild(ChildNode).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
              BlendOffscreenSurface(DestSurface, Input1, bmNormal);
            end;
          end;
          ReleaseOffscreenBitmap(CachedSurface);
          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeOffsetNode then
        begin
          Input1 := ResolveSurface(TSvgFeOffsetNode(Node).ResolvedIn1, SourceGraphic, SourceAlpha, CurrentSurface, CurrentSurface, NamedSurfaces);
          ReleaseOffscreenBitmap(CachedSurface);
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);

          dxInt := Round(TSvgFeOffsetNode(Node).Dx * MatScale);
          dyInt := Round(TSvgFeOffsetNode(Node).Dy * MatScale);
          Input1.DrawTo(DestSurface, dxInt, dyInt);
          CurrentSurface := DestSurface;
        end else

        if Node is TSvgFeFloodNode then
        begin
          DestSurface := GetOffscreenBitmap(FilterRoi.Width, FilterRoi.Height, True);
          ReleaseOffscreenBitmap(CachedSurface);

          FloodColor := TSvgFeFloodNode(Node).FloodColor.Color;
          if TSvgFeFloodNode(Node).FloodOpacity < 1.0 then
            ScaleAlpha(FloodColor, TSvgFeFloodNode(Node).FloodOpacity);
          DestSurface.Clear(FloodColor);
          CurrentSurface := DestSurface;
        end;

        if TSvgFilterPrimitiveNode(Node).IsReferenceTarget then
        begin
          NamedSurfaces[Count] := CurrentSurface;
          CachedSurface := nil;
          Inc(Count);
        end else
          CachedSurface := CurrentSurface;
      end;

      ReleaseOffscreenBitmap(CachedSurface);

      // 6. Blend final filtered result surface onto target canvas at ROI origin
      if CurrentSurface <> nil then
        BlendOffscreenSurface(ATarget, CurrentSurface, bmNormal, FilterRoi.Left, FilterRoi.Top);

    finally
      for CurrentSurface in NamedSurfaces do
        ReleaseOffscreenBitmap(CurrentSurface);
      ReleaseOffscreenBitmap(SourceGraphic);
      ReleaseOffscreenBitmap(SourceAlpha);
    end;
  finally
    PopMatrix;
  end;
end;

//------------------------------------------------------------------------------
//
//      Data URI Parsing Helpers
//
//------------------------------------------------------------------------------
// Used by TSvgRenderer.RenderImageNode
//------------------------------------------------------------------------------
function UrlDecode(const AStr: string): string;
var
  i, len: Integer;
  c: Char;
  code: Integer;
begin
  Result := '';
  len := Length(AStr);
  i := 1;
  while i <= len do
  begin
    c := AStr[i];
    if (c = '%') and (i + 2 <= len) then
    begin
      code := StrToIntDef('$' + Copy(AStr, i + 1, 2), -1);
      if code >= 0 then
      begin
        Result := Result + Char(code);
        Inc(i, 3);
        Continue;
      end;
    end;
    if c = '+' then
      Result := Result + ' '
    else
      Result := Result + c;
    Inc(i);
  end;
end;

procedure DecodeBase64ToStream(const ABase64Str: string; AStream: TStream);
var
  i, len: Integer;
  b1, b2, b3: Byte;
  v1, v2, v3, v4: Integer;
  buf: array[0..2] of Byte;

  function DecodeChar(c: Char): Integer;
  begin
    case c of
      'A'..'Z': Result := Ord(c) - Ord('A');
      'a'..'z': Result := Ord(c) - Ord('a') + 26;
      '0'..'9': Result := Ord(c) - Ord('0') + 52;
      '+': Result := 62;
      '/': Result := 63;
      '=': Result := -2;
    else
      Result := -1;
    end;
  end;

begin
  len := Length(ABase64Str);
  i := 1;
  while i <= len do
  begin
    v1 := -1;
    while (i <= len) and (v1 = -1) do
    begin
      v1 := DecodeChar(ABase64Str[i]);
      Inc(i);
    end;
    if (v1 < 0) then Break;

    v2 := -1;
    while (i <= len) and (v2 = -1) do
    begin
      v2 := DecodeChar(ABase64Str[i]);
      Inc(i);
    end;
    if (v2 < 0) then Break;

    v3 := -1;
    while (i <= len) and (v3 = -1) do
    begin
      v3 := DecodeChar(ABase64Str[i]);
      Inc(i);
    end;
    if (v3 < -1) then v3 := -2;

    v4 := -1;
    while (i <= len) and (v4 = -1) do
    begin
      v4 := DecodeChar(ABase64Str[i]);
      Inc(i);
    end;
    if (v4 < -1) then v4 := -2;

    b1 := (v1 shl 2) or ((v2 and $30) shr 4);
    buf[0] := b1;
    if (v3 >= 0) then
    begin
      b2 := ((v2 and $0F) shl 4) or ((v3 and $3C) shr 2);
      buf[1] := b2;
      if (v4 >= 0) then
      begin
        b3 := ((v3 and $03) shl 6) or v4;
        buf[2] := b3;
        AStream.WriteBuffer(buf[0], 3);
      end else
        AStream.WriteBuffer(buf[0], 2);
    end else
      AStream.WriteBuffer(buf[0], 1);
  end;
end;


//------------------------------------------------------------------------------
//
//      TSvgRenderer.RenderImageNode
//
//------------------------------------------------------------------------------
procedure TSvgRenderer.RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
var
  WidthPx, HeightPx, xPx, yPx: Single;
  TargetRect: TFloatRect;
  hrefStr, MimeType, DataStr, DecodedStr: string;
  isBase64, isSvg: Boolean;
  CommaPos: Integer;
  Stream: TMemoryStream;
  SubDoc: TSvgDocumentNode;
  SubRenderer: TSvgRenderer;
  Bitmap: TBitmap32;
  AspectMat, TotalMat: TFloatMatrixHelper;
  Transformation: TAffineTransformation;
  DestBounds: TFloatRect;
  DestClip: TRect;
  SourceViewBox: TSvgViewBox;
  utf8Bytes: UTF8String;
begin
  if (ATarget = nil) or (AImageNode = nil) then
    Exit;

  WidthPx := AImageNode.Width.ToPixels(FViewportRect.Width);
  HeightPx := AImageNode.Height.ToPixels(FViewportRect.Height);
  if (WidthPx <= 0) or (HeightPx <= 0) then
    Exit;

  xPx := AImageNode.X.ToPixels(FViewportRect.Width);
  yPx := AImageNode.Y.ToPixels(FViewportRect.Height);
  TargetRect := FloatRect(xPx, yPx, xPx + WidthPx, yPx + HeightPx);

  hrefStr := Trim(AImageNode.Href);
  if (hrefStr = '') then
    Exit;

  // TODO : The string handling here is horrible! Optimizer later for zero allocation.
  // Replace stream with "pull" decode on-demand stream

  Stream := TMemoryStream.Create;
  try
    isSvg := False;
    if SameText(Copy(hrefStr, 1, 5), 'data:') then
    begin
      CommaPos := Pos(',', hrefStr);
      if CommaPos > 0 then
      begin
        MimeType := LowerCase(Copy(hrefStr, 6, CommaPos - 6));
        isBase64 := Pos(';base64', MimeType) > 0;
        isSvg := (Pos('image/svg+xml', MimeType) > 0) or (Pos('image/svg', MimeType) > 0);
        DataStr := Copy(hrefStr, CommaPos + 1, MaxInt);

        if isBase64 then
          DecodeBase64ToStream(DataStr, Stream)
        else
        begin
          DecodedStr := UrlDecode(DataStr);
          utf8Bytes := UTF8String(DecodedStr);
          if Length(utf8Bytes) > 0 then
            Stream.WriteBuffer(utf8Bytes[1], Length(utf8Bytes));
        end;
      end;
    end else
    if FAllowExternalImages and FileExists(hrefStr) then
    begin
      Stream.LoadFromFile(hrefStr);
      if SameText(ExtractFileExt(hrefStr), '.svg') then
        isSvg := True;
    end;

    if Stream.Size = 0 then Exit;
    Stream.Position := 0;

    // Check if content is SVG if not determined by MIME or file extension
    if not isSvg then
    begin
      if Stream.Size > 4 then
      begin
        SetLength(DataStr, Min(100, Stream.Size));
        Stream.ReadBuffer(DataStr[1], Length(DataStr));
        Stream.Position := 0;
        if (Pos('<svg', LowerCase(DataStr)) > 0) or (Pos('<?xml', LowerCase(DataStr)) > 0) then
          isSvg := True;
      end;
    end;

    if isSvg then
    begin
      // TODO : We could also just let TBitmap.LoadFromStream handle it...
      SubDoc := ParseSvgXml(PAnsiChar(Stream.Memory), Stream.Size);
      if SubDoc = nil then
        exit;

      try
        SubDoc.Resolve;

        if SubDoc.ViewBox.IsValid then
          SourceViewBox := SubDoc.ViewBox
        else
        begin
          WidthPx := SubDoc.Width.ToPixels(TargetRect.Width);
          HeightPx := SubDoc.Height.ToPixels(TargetRect.Height);
          if WidthPx <= 0 then
            WidthPx := TargetRect.Width;
          if HeightPx <= 0 then
            HeightPx := TargetRect.Height;
          SourceViewBox := TSvgViewBox.Create(0, 0, WidthPx, HeightPx);
        end;

        AspectMat.Matrix := SourceViewBox.GetTransform(TargetRect, AImageNode.PreserveAspectRatio);

        PushMatrix;
        try
          ApplyMatrix(AspectMat.Matrix);

          SubRenderer := TSvgRenderer.Create(ATarget);
          try
            SubRenderer.AllowExternalImages := FAllowExternalImages;
            SubRenderer.CurrentMatrix := FCurrentMatrix;
            SubRenderer.ViewportRect := FViewportRect;

            SubRenderer.RenderNode(ATarget, SubDoc);
          finally
            SubRenderer.Free;
          end;
        finally
          PopMatrix;
        end;
      finally
        SubDoc.Free;
      end;
    end else
    begin
      Bitmap := TBitmap32.Create;
      try
        Bitmap.LoadFromStream(Stream);

        if (Bitmap.Empty) then
          exit;

        SourceViewBox := TSvgViewBox.Create(0, 0, Bitmap.Width, Bitmap.Height);
        AspectMat.Matrix := SourceViewBox.GetTransform(TargetRect, AImageNode.PreserveAspectRatio);
        TotalMat := AspectMat * FCurrentMatrix;

        Transformation := TAffineTransformation.Create;
        try
          Transformation.Clear(TotalMat.Matrix);

          DestBounds := Transformation.GetTransformedBounds(FloatRect(0, 0, Bitmap.Width, Bitmap.Height));
          DestClip := MakeRect(DestBounds, rrOutside);

          Bitmap.DrawMode := dmBlend;
          Bitmap.CombineMode := cmMerge;

          // Transform and render onto target
          Transform(ATarget, Bitmap, Transformation, DestClip, True);
        finally
          Transformation.Free;
        end;
      finally
        Bitmap.Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TSvgRenderer.MapFont(const AFontFamily, AWeightStr, AStyleStr: string; ASize: integer; var AFontInfo: TFontInfo);
var
  s: string;
begin
  // TODO : Delegate to event
  (*
  if (Assigned(FOnMapFont)) then
  begin
    AHandled := False;
    FOnMapFont(AFontFamily, AWeightStr, AStyleStr, ASize, AFontInfo, AHandled);
    if (AHandled) then
      exit;
  end;
  *)

  s := LowerCase(Trim(AFontFamily));

  if (s = '') or (s = 'sans-serif') or (s = 'sans') or (s = 'noto sans') or (s = 'system-ui') then
    AFontInfo.FontFamily := 'Arial'
  else
  if (s = 'serif') or (s = 'times') then
    AFontInfo.FontFamily := 'Times New Roman'
  else
  if (s = 'monospace') or (s = 'mono') or (s = 'courier') then
    AFontInfo.FontFamily := 'Courier New'
  else
    AFontInfo.FontFamily := AFontFamily;

  s := LowerCase(AWeightStr);
  if (s = 'bold') or (s = '700') or (s = '800') or (s = '900') then
    Include(AFontInfo.Style, fsBold);

  s := LowerCase(AStyleStr);
  if (s = 'italic') or (s = 'oblique') then
    Include(AFontInfo.Style, fsItalic);

  AFontInfo.Size := ASize;
end;

procedure TSvgRenderer.RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);

  procedure RenderTextPathData(const APathPoints: TArrayOfArrayOfFloatPoint; const AFill: TSvgFill; const AStroke: TSvgStroke);
  var
    TransformedPts, StrokePts, DashedPts: TArrayOfArrayOfFloatPoint;
    Bounds, StrokeBounds: TFloatRect;
    PolyRenderer: TPolygonRenderer32;
    Filler: TCustomPolygonFiller;
    TargetNode: TSvgNode;
    FillColor, StrokeColor: TColor32;
    StrokeWidth, MatScale, ScaledOffset: Single;
    ScaledDashArray: TArrayOfFloat;
    i, j, k: Integer;
  begin
    if (Length(APathPoints) = 0) or (ATarget = nil) then
      Exit;

    TransformedPts := GetTransformedPoints(APathPoints);
    Bounds := GetPathBounds(TransformedPts);

    PolyRenderer := DefaultPolygonRendererClass.Create(ATarget);
    try
      // 1. Fill Rendering
      if AFill.ResolvedPaintServer <> nil then
      begin
        TargetNode := TSvgNode(AFill.ResolvedPaintServer);
        Filler := nil;
        if TargetNode is TSvgGradientNode then
          Filler := CreateGradientFiller(TSvgGradientNode(TargetNode), Bounds, AFill.Opacity)
        else
        if TargetNode is TSvgPatternNode then
          Filler := CreatePatternFiller(TSvgPatternNode(TargetNode), Bounds, AFill.Opacity);

        if Filler <> nil then
        begin
          try
            PolyRenderer.Filler := Filler;
            try
              PolyRenderer.FillMode := AFill.FillRule;
              PolyRenderer.PolyPolygonFS(TransformedPts);
            finally
              PolyRenderer.Filler := nil;
            end;
          finally
            Filler.Free;
          end;
        end;
      end else
      if not AFill.Color.IsNone then
      begin
        FillColor := AFill.Color.Color;
        if AFill.Opacity < 1.0 then
          ScaleAlpha(FillColor, AFill.Opacity);

        if AlphaComponent(FillColor) > 0 then
        begin
          PolyRenderer.Color := FillColor;
          PolyRenderer.FillMode := AFill.FillRule;
          PolyRenderer.PolyPolygonFS(TransformedPts);
        end;
      end;

      // 2. Stroke Rendering
      StrokeWidth := AStroke.Width.ToPixels(FViewportRect.Right - FViewportRect.Left);
      if StrokeWidth > 0 then
      begin
        MatScale := GetMatrixScale(FCurrentMatrix);
        StrokeWidth := StrokeWidth * MatScale;

        ScaledDashArray := nil;
        ScaledOffset := 0;
        if Length(AStroke.DashArray) > 0 then
        begin
          SetLength(ScaledDashArray, Length(AStroke.DashArray));
          for k := 0 to High(AStroke.DashArray) do
            ScaledDashArray[k] := AStroke.DashArray[k] * MatScale;
          ScaledOffset := AStroke.DashOffset * MatScale;
        end;

        if AStroke.ResolvedPaintServer <> nil then
        begin
          TargetNode := TSvgNode(AStroke.ResolvedPaintServer);
          StrokePts := nil;
          for i := 0 to High(TransformedPts) do
          begin
            if Length(ScaledDashArray) > 0 then
            begin
              DashedPts := BuildDashedLine(TransformedPts[i], ScaledDashArray, ScaledOffset, IsClosedContour(TransformedPts[i]));
              for j := 0 to High(DashedPts) do
                StrokePts := StrokePts + BuildPolyPolyLine([DashedPts[j]], False, StrokeWidth, AStroke.JoinStyle, AStroke.EndStyle, AStroke.MiterLimit);
            end else
              StrokePts := StrokePts + BuildPolyPolyLine([TransformedPts[i]], IsClosedContour(TransformedPts[i]), StrokeWidth, AStroke.JoinStyle, AStroke.EndStyle, AStroke.MiterLimit);
          end;

          StrokeBounds := GetPathBounds(StrokePts);
          Filler := nil;
          if TargetNode is TSvgGradientNode then
            Filler := CreateGradientFiller(TSvgGradientNode(TargetNode), StrokeBounds, AStroke.Opacity)
          else
          if TargetNode is TSvgPatternNode then
            Filler := CreatePatternFiller(TSvgPatternNode(TargetNode), StrokeBounds, AStroke.Opacity);

          if Filler <> nil then
          begin
            try
              PolyRenderer.Filler := Filler;
              try
                PolyRenderer.FillMode := pfWinding;
                PolyRenderer.PolyPolygonFS(StrokePts);
              finally
                PolyRenderer.Filler := nil;
              end;
            finally
              Filler.Free;
            end;
          end;
        end else
        if not AStroke.Color.IsNone then
        begin
          StrokeColor := AStroke.Color.Color;
          if AStroke.Opacity < 1.0 then
            ScaleAlpha(StrokeColor, AStroke.Opacity);

          if AlphaComponent(StrokeColor) > 0 then
          begin
            StrokePts := nil;
            for i := 0 to High(TransformedPts) do
            begin
              if Length(ScaledDashArray) > 0 then
              begin
                DashedPts := BuildDashedLine(TransformedPts[i], ScaledDashArray, ScaledOffset, IsClosedContour(TransformedPts[i]));
                for j := 0 to High(DashedPts) do
                  StrokePts := StrokePts + BuildPolyPolyLine([DashedPts[j]], False, StrokeWidth, AStroke.JoinStyle, AStroke.EndStyle, AStroke.MiterLimit);
              end else
                StrokePts := StrokePts + BuildPolyPolyLine([TransformedPts[i]], IsClosedContour(TransformedPts[i]), StrokeWidth, AStroke.JoinStyle, AStroke.EndStyle, AStroke.MiterLimit);
            end;

            PolyRenderer.Color := StrokeColor;
            PolyRenderer.FillMode := pfWinding;
            PolyRenderer.PolyPolygonFS(StrokePts);
          end;
        end;
      end;
    finally
      PolyRenderer.Free;
    end;
  end;

  // Evaluates path arc length and places glyphs at interpolated path distance points
  // aligned to segment tangent orientation angles.
  procedure ProcessTextPathNode(ANode: TSvgTextPathNode; Canvas: TCanvas32);
  var
    Text, CharString: string;
    RawPts, PathPts: TArrayOfArrayOfFloatPoint;
    TotalLen, Offset, CurrentDistance, CharWidth, TextWidth, TangAngle, DrawY: Single;
    FontSizePx: Integer;
    FontInfo: TFontInfo;
    TextLayout: TTextLayout;
    MeasureRect: TFloatRect;
    Point: TFloatPoint;
    RotationMat: TFloatMatrixHelper;
    GlyphIdx: Integer;
    FontFace: IFontFace32;
    FontFaceMetrics: TFontFaceMetrics32;
    RotateArray: TArrayOfFloat;
    w1, w2, CharacterRotationAngle: Single;
    ZeroWidth: Single;
  begin
    if (ANode = nil) or ANode.IsDisplayNone or (not ANode.Visible) then
      Exit;

    Text := ANode.TextContent;
    if Text = '' then
      Text := GetSubtreeText(ANode);

    if (ANode.ResolvedPathNode = nil) or (Text = '') then
      Exit;

    RawPts := ANode.ResolvedPathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
    if Length(RawPts) = 0 then
      Exit;

    if not IsIdentityMatrix(ANode.ResolvedPathNode.Transform) then
      PathPts := TransformPathPoints(RawPts, ANode.ResolvedPathNode.Transform)
    else
      PathPts := RawPts;

    TotalLen := GetTotalPathLength(PathPts);
    if TotalLen <= 0 then
      Exit;

    Offset := ANode.StartOffset.ToPixels(TotalLen);

    FontSizePx := Round(ANode.FontSize.ToPixels(FViewportRect.Height));
    if FontSizePx <= 0 then
      FontSizePx := 12;

    FontInfo := Default(TFontInfo);
    MapFont(ANode.FontFamily, ANode.FontWeight, ANode.FontStyle, FontSizePx, FontInfo);

    Canvas.Bitmap.Font.Name := FontInfo.FontFamily;
    Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
    Canvas.Bitmap.Font.Style := FontInfo.Style;

    TextLayout := DefaultTextLayout;
    TextLayout.ClipLayout := False;
    TextLayout.AlignmentHorizontal := TextAlignHorLeft;
    TextLayout.AlignmentVertical := TextAlignVerTop;

    if ANode.TextAnchor <> taStart then
    begin
      MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, Text, TextLayout);
      TextWidth := MeasureRect.Width;
      if ANode.TextAnchor = taMiddle then
        Offset := Offset - TextWidth * 0.5
      else if ANode.TextAnchor = taEnd then
        Offset := Offset - TextWidth;
    end;

    FontFace := TFontFace32.Create(Canvas.Bitmap.Font.Handle);
    try
      FontFace.GetFontFaceMetrics(TextLayout, FontFaceMetrics);
      DrawY := -FontFaceMetrics.Ascent;
    finally
      FontFace := nil;
    end;

    if (Length(ANode.Rotate) = 0) and (ANode.Parent <> nil) and (ANode.Parent is TSvgTextPositioningNode) then
      RotateArray := TSvgTextPositioningNode(ANode.Parent).Rotate
    else
      RotateArray := ANode.Rotate;

    ZeroWidth := NaN;
    SetLength(CharString, 2);
    CharString[2] := Char(ZERO_WIDTH_SPACE);

    CharacterRotationAngle := 0;
    CurrentDistance := Offset;
    for GlyphIdx := 1 to Length(Text) do
    begin
      // Get the rotation angle. If there's too few we just reuse the previous
      if GlyphIdx - 1 <= High(RotateArray) then
        CharacterRotationAngle := RotateArray[GlyphIdx - 1];

      // Note: The width returned by MeasureText excludes the AdvanceWidth of the last character.
      // For example, since 'space' has a very small Width + a larger AdvanceWidth, the total
      // width returned by MeasureText(' ') is almost zero. We work around this by adding a
      // "zero width space" as the last character.
      // The width actually also excludes the LSB (Left Side Bearing) of the first character,
      // but we don't do anything about that.
      CharString[1] := Text[GlyphIdx];

      if (CharString[1] = ' ') then
      begin
        if (IsNaN(ZeroWidth)) then
        begin
          MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, CharString, TextLayout);
          ZeroWidth := MeasureRect.Width;
        end;

        if (ZeroWidth <= 0.001) then
        begin
          w1 := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, 'x x', TextLayout).Width;
          w2 := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, 'xx', TextLayout).Width;
          ZeroWidth := Max(1.0, w1 - w2);
        end;

        CharWidth := ZeroWidth;
      end else
      begin
        MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, CharString, TextLayout);
        CharWidth := MeasureRect.Width;

        if GetPointAndTangentAtDistance(pathPts, CurrentDistance + CharWidth * 0.5, Point, tangAngle) then
        begin
          RotationMat.Matrix := IdentityMatrix;
          RotationMat.Rotate(tangAngle + CharacterRotationAngle);
          RotationMat.Translate(Point.X, Point.Y);

          PushMatrix;
          try
            ApplyMatrix(RotationMat.Matrix);
            Canvas.Clear;
            Canvas.BeginUpdate;
            Canvas.RenderText(-charWidth * 0.5, drawY, Text[GlyphIdx], TextLayout);
            if (Canvas.Path <> nil) then
              RenderTextPathData(Canvas.Path, ANode.Fill, ANode.Stroke);
            Canvas.Clear;
            Canvas.EndUpdate;
          finally
            PopMatrix;
          end;
        end;
      end;

      CurrentDistance := CurrentDistance + CharWidth;
    end;
  end;

  procedure ProcessTextPositioningNode(ANode: TSvgTextPositioningNode; Canvas: TCanvas32; var Cursor: TFloatPoint);
  var
    Child: TSvgNode;
    TextWidth, CharWidth: Single;
    FontSizePx: integer;
    FontInfo: TFontInfo;
    TextLayout: TTextLayout;
    MeasureRect: TFloatRect;
    DrawPoint, LastDrawPoint: TFloatPoint;
    RotationMat: TFloatMatrixHelper;
    HasNodeTransform: Boolean;
    CharIndex: Integer;
    CharString: UnicodeString;
    CharacterRotationAngle: Single;
    ZeroWidth: Single;
  begin
    if (ANode = nil) or ANode.IsDisplayNone or (not ANode.Visible) then
      Exit;

    HasNodeTransform := not IsIdentityMatrix(ANode.Transform);

    if HasNodeTransform then
    begin
      PushMatrix;
      ApplyMatrix(ANode.Transform);
    end;
    try
      if ANode.HasX then
        Cursor.X := ANode.X.ToPixels(FViewportRect.Width);
      if ANode.HasDx then
        Cursor.X := Cursor.X + ANode.Dx.ToPixels(FViewportRect.Width);

      if ANode.HasY then
        Cursor.Y := ANode.Y.ToPixels(FViewportRect.Height);
      if ANode.HasDy then
        Cursor.Y := Cursor.Y + ANode.Dy.ToPixels(FViewportRect.Height);

      if (ANode.TextContent <> '') then
      begin
        FontSizePx := Round(ANode.FontSize.ToPixels(FViewportRect.Height));
        if FontSizePx <= 0 then
          FontSizePx := 12;

        FontInfo := Default(TFontInfo);
        MapFont(ANode.FontFamily, ANode.FontWeight, ANode.FontStyle, FontSizePx, FontInfo);

        Canvas.Bitmap.Font.Name := FontInfo.FontFamily;
        Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Bitmap.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;

        MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, ANode.TextContent, TextLayout);
        TextWidth := MeasureRect.Width;

        // Align with font baseline.
        // Unfortunately TCanvas32 doesn't currently support this.
        // TODO : Windows specific
        var FontFace: IFontFace32 := TFontFace32.Create(Canvas.Bitmap.Font.Handle);
        try
          var FontFaceMetrics: TFontFaceMetrics32;
          FontFace.GetFontFaceMetrics(TextLayout, FontFaceMetrics);

          DrawPoint.Y := Cursor.Y - FontFaceMetrics.Ascent;
        finally
          FontFace := nil;
        end;

        case ANode.TextAnchor of
          taMiddle: DrawPoint.X := Cursor.X - TextWidth * 0.5;
          taEnd:    DrawPoint.X := Cursor.X - TextWidth;
        else
          DrawPoint.X := Cursor.X;
        end;

        if Length(ANode.Rotate) > 0 then
        begin
          CharacterRotationAngle := 0;
          ZeroWidth := NaN;
          SetLength(CharString, 2);
          CharString[2] := Char(ZERO_WIDTH_SPACE);

          for CharIndex := 1 to Length(ANode.TextContent) do
          begin
            // Get the rotation angle. If there's too few we just reuse the previous (we know there's at least one)
            if CharIndex - 1 <= High(ANode.Rotate) then
            begin
              CharacterRotationAngle := ANode.Rotate[CharIndex - 1];

              LastDrawPoint := DrawPoint;
            end;

            CharString[1] := Char(ANode.TextContent[CharIndex]);

            if (CharString[1] = ' ') then
            begin
              if (IsNaN(ZeroWidth)) then
              begin
                MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, CharString, TextLayout);
                ZeroWidth := MeasureRect.Width;
              end;

              CharWidth := ZeroWidth;
            end else
            begin

              MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, CharString, TextLayout);
              CharWidth := MeasureRect.Width;

              RotationMat.Matrix := IdentityMatrix;
              RotationMat.Rotate(DrawPoint.X, Cursor.Y, CharacterRotationAngle);

              PushMatrix;
              try
                ApplyMatrix(RotationMat.Matrix);
                Canvas.Clear;
                Canvas.BeginUpdate;

                Canvas.RenderText(DrawPoint.X, DrawPoint.Y, CharString, TextLayout);
                if (Canvas.Path <> nil) then
                  RenderTextPathData(Canvas.Path, ANode.Fill, ANode.Stroke);

                Canvas.Clear;
                Canvas.EndUpdate;
              finally
                PopMatrix;
              end;

            end;

            DrawPoint.X := DrawPoint.X + CharWidth;
          end;
        end else
        begin
          Canvas.Clear;
          Canvas.BeginUpdate;

          Canvas.RenderText(DrawPoint.X, DrawPoint.Y, ANode.TextContent, TextLayout);

          if (Canvas.Path <> nil) then
            RenderTextPathData(Canvas.Path, ANode.Fill, ANode.Stroke);

          Canvas.Clear;
          Canvas.EndUpdate;
        end;

        Cursor.X := Cursor.X + TextWidth;
      end;

      for Child in ANode.Children do
      begin
        if Child is TSvgTextPathNode then
          ProcessTextPathNode(TSvgTextPathNode(Child), Canvas)
        else
        if Child is TSvgTSpanNode then
          ProcessTextPositioningNode(TSvgTSpanNode(Child), Canvas, Cursor)
        else
        if Child is TSvgTextPositioningNode then
          ProcessTextPositioningNode(TSvgTextPositioningNode(Child), Canvas, Cursor);
      end;
    finally
      if HasNodeTransform then
        PopMatrix;
    end;
  end;

var
  Canvas: TCanvas32;
  Cursor: TFloatPoint;
begin
  // RenderTextNode decomposes <text> and <tspan> elements into vector path outlines using TCanvas32.
  // It applies font properties, measures text width for text-anchor alignment, updates cursor positions,
  // and paints vector glyph geometry with solid/gradient/pattern fills and strokes.

  if (ATextNode = nil) or ATextNode.IsDisplayNone or (not ATextNode.Visible) then
    Exit;

  if (FTarget = nil) then
    exit;

  // Note: FTarget, not ATarget; We need access to the main bitmap's font properties
  // and we aren't rendering via TCanvas32
  Canvas := TCanvas32.Create(TBitmap32(FTarget));
  try
    Cursor.X := 0;
    Cursor.Y := 0;

    Canvas.BeginLockUpdate;

    ProcessTextPositioningNode(ATextNode, Canvas, Cursor);

    Canvas.EndLockUpdate;
    Canvas.Clear;
  finally
    Canvas.Free;
  end;
end;

procedure TSvgRenderer.RenderNodeContent(ATarget: TCustomBitmap32; ANode: TSvgNode);
begin
  if ANode is TSvgPathNode then
    RenderPathNode(ATarget, TSvgPathNode(ANode))
  else
  if ANode is TSvgImageNode then
    RenderImageNode(ATarget, TSvgImageNode(ANode))
  else
  if ANode is TSvgTextNode then
    RenderTextNode(ATarget, TSvgTextNode(ANode))
  else
  if ANode is TSvgGroupNode then
    RenderGroupNode(ATarget, TSvgGroupNode(ANode));
end;

procedure TSvgRenderer.RenderNodeUnfiltered(ATarget: TCustomBitmap32; ANode: TSvgNode);
begin
  PushMatrix;
  try
    ApplyMatrix(ANode.Transform);
    RenderNodeContent(ATarget, ANode);
  finally
    PopMatrix;
  end;
end;

procedure TSvgRenderer.RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode);
var
  OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  EffectiveBlendMode: TSvgBlendMode;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  OldMatrix: TFloatMatrix;
  NodeBounds, TargetWorldBounds: TFloatRect;
  NodeRoi: TRect;
  Points: array[0..3] of TFloatPoint;
  SourceP, DestP: PColor32;
  x, k: Integer;
  AlphaVal, Gray: Byte;
  NeedsOffscreen: Boolean;
const
  OneOver255: Single = 1 / 255;
begin
  if (ANode = nil) or ANode.IsDisplayNone or (not ANode.Visible) or (not ANode.IsRenderable) or (not ANode.PassesConditionalProcessing) then
    Exit;

  // Guard against self-referencing clip-path, clip-mask etc.
  if (FRecursionDepth > cMaxRecursions) then
    Exit;

  Inc(FRecursionDepth);
  try

    // Filter processing takes precedence over direct element rendering
    if ANode.ResolvedFilter <> nil then
    begin
      RenderFilter(ATarget, ANode.ResolvedFilter, ANode);
      Exit;
    end;

    EffectiveBlendMode := GetEffectiveMixBlendMode(ANode);

    // Non-group renderable nodes need offscreen surface if they have ClipPath, Mask, Opacity < 1, or Non-normal blend mode
    NeedsOffscreen := (not (ANode is TSvgGroupNode)) and
      ((EffectiveBlendMode <> bmNormal) or (ANode.ResolvedClipPath <> nil) or (ANode.ResolvedMask <> nil) or (ANode.Opacity < 1.0));

    // Polygon nodes (TSvgPathNode) handle their own ROI calculation after stroking inside RenderPathNode
    if NeedsOffscreen and (ATarget <> nil) and (not (ANode is TSvgPathNode)) then
    begin
      PushMatrix;
      try
        ApplyMatrix(ANode.Transform);

        // Calculate object bounding box in world space
        NodeBounds := ANode.GetObjectBoundingBox;
        Points[0] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Left, NodeBounds.Top));
        Points[1] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Right, NodeBounds.Top));
        Points[2] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Right, NodeBounds.Bottom));
        Points[3] := TFloatMatrixHelper(FCurrentMatrix).TransformPoint(FloatPoint(NodeBounds.Left, NodeBounds.Bottom));

        TargetWorldBounds := FloatRect(Points[0].X, Points[0].Y, Points[0].X, Points[0].Y);
        for k := 1 to 3 do
        begin
          if (Points[k].X < TargetWorldBounds.Left) then TargetWorldBounds.Left := Points[k].X;
          if (Points[k].X > TargetWorldBounds.Right) then TargetWorldBounds.Right := Points[k].X;
          if (Points[k].Y < TargetWorldBounds.Top) then TargetWorldBounds.Top := Points[k].Y;
          if (Points[k].Y > TargetWorldBounds.Bottom) then TargetWorldBounds.Bottom := Points[k].Y;
        end;

        NodeRoi := MakeRect(TargetWorldBounds, rrOutside);
        if not GR32.IntersectRect(NodeRoi, NodeRoi, ATarget.BoundsRect) then
          Exit;

        OffscreenBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, True);
        ClipMaskBmp := nil;
        MaskBmp := nil;
        try
          PushMatrix;
          try
            TFloatMatrixHelper(FCurrentMatrix).Translate(-NodeRoi.Left, -NodeRoi.Top);
            RenderNodeContent(OffscreenBmp, ANode);
          finally
            PopMatrix;
          end;

        // Apply ClipPath
        if (ANode.ResolvedClipPath <> nil) then
        begin
          ClipNodeTarget := ANode.ResolvedClipPath;
          ClipMaskBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, True);

          OldMatrix := FCurrentMatrix;
          PushMatrix;
          try
            TFloatMatrixHelper(FCurrentMatrix).Translate(-NodeRoi.Left, -NodeRoi.Top);
            RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, TargetWorldBounds, NodeRoi);
          finally
            PopMatrix;
            FCurrentMatrix := OldMatrix;
          end;

          SourceP := PColor32(OffscreenBmp.Bits);
          DestP := PColor32(ClipMaskBmp.Bits);
          for x := 0 to OffscreenBmp.PixelCount - 1 do
          begin
            AlphaVal := AlphaComponent(DestP^);
            if AlphaVal < 255 then
              ScaleAlpha(SourceP^, AlphaVal * OneOver255);
            Inc(SourceP);
            Inc(DestP);
          end;
        end;

        // Apply Alpha Mask
        if (ANode.ResolvedMask <> nil) then
        begin
          MaskNodeTarget := ANode.ResolvedMask;
          MaskBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, False);

          OldMatrix := FCurrentMatrix;
          PushMatrix;
          try
            TFloatMatrixHelper(FCurrentMatrix).Translate(-NodeRoi.Left, -NodeRoi.Top);
            RenderMaskNode(MaskBmp, MaskNodeTarget, TargetWorldBounds, NodeRoi);
          finally
            PopMatrix;
            FCurrentMatrix := OldMatrix;
          end;

          SourceP := PColor32(OffscreenBmp.Bits);
          DestP := PColor32(MaskBmp.Bits);
          for x := 0 to OffscreenBmp.PixelCount - 1 do
          begin
            Gray := Intensity(DestP^);
            Gray := Round(Gray * (AlphaComponent(DestP^) / 255.0));
            ScaleAlpha(SourceP^, Gray / 255.0);
            Inc(SourceP);
            Inc(DestP);
          end;
        end;

        // Apply Opacity
        if (ANode.Opacity < 1.0) then
        begin
          SourceP := PColor32(OffscreenBmp.Bits);
          for x := 0 to OffscreenBmp.PixelCount - 1 do
          begin
            ScaleAlpha(SourceP^, ANode.Opacity);
            Inc(SourceP);
          end;
        end;

        BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, NodeRoi.Left, NodeRoi.Top);
      finally
        ReleaseOffscreenBitmap(OffscreenBmp);
        ReleaseOffscreenBitmap(ClipMaskBmp);
        ReleaseOffscreenBitmap(MaskBmp);
      end;
      finally
        PopMatrix;
      end;
      Exit;
    end;

    RenderNodeUnfiltered(ATarget, ANode);

  finally
    Dec(FRecursionDepth);
  end;
end;

procedure TSvgRenderer.RenderNode(ANode: TSvgNode);
begin
  RenderNode(FTarget, ANode);
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

  FViewportRect := FloatRect(viewBox.X, viewBox.Y, viewBox.X + viewBox.Width, viewBox.Y + viewBox.Height);

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
