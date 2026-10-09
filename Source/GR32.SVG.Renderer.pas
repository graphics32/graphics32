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

{$define USE_SIMD_MASK_FILTERS}

uses
  SysUtils, Classes, Graphics, Generics.Collections,
  GR32, GR32_Transforms, GR32_Polygons, GR32_VectorUtils, GR32_ColorGradients,
  GR32.SVG.Types, GR32.SVG.Tree;

type
  { TSvgBitmapPool: Reusable pool of intermediate TBitmap32 offscreen surfaces to eliminate
    frequent heap allocations/deallocations during nested group opacity, clip path, and mask compositing. }
  TMapPool<T: TCustomMap; TElement> = class abstract(TObject)
  private
    FPool: TObjectList<T>;
    FMaxSize: Int64;
    FPoolSize: Int64;
  protected
    function CreateNewMap: T; virtual; abstract;
    procedure PrepareMap(Map: T); virtual;
    function CandidateWouldReallocate(Candidate: T; TargetSize: Int64): boolean; virtual;
  public
    constructor Create(AMaxSize: Int64 = 0);
    destructor Destroy; override;
    function Acquire(AWidth, AHeight: Integer; AClear: Boolean = True): T;
    procedure Release(Map: T); virtual;
    procedure Clear;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
    property PoolSize: Int64 read FPoolSize;
  end;

  TSvgBitmapPool = class(TMapPool<TCustomBitmap32, TColor32>)
  private
    FBitmapMaxOversize: NativeInt;
  protected
    function CreateNewMap: TCustomBitmap32; override;
    procedure PrepareMap(Map: TCustomBitmap32); override;
    function CandidateWouldReallocate(Candidate: TCustomBitmap32; TargetSize: Int64): boolean; override;
  public
    procedure Release(Map: TCustomBitmap32); override;
    property BitmapMaxOversize: NativeInt read FBitmapMaxOversize write FBitmapMaxOversize;
  end;

  TSvgPatternPolygonFiller = class(TBitmapPolygonFiller)
  private
    FPatternBmp: TBitmap32;
    FAffineTransform: Boolean;
    FInvMatrix: TFloatMatrix;
    FTileX: Single;
    FTileY: Single;
    FTileWidth: Single;
    FTileHeight: Single;
    FScaleBmpX: Single;
    FScaleBmpY: Single;
    FWrapProcX, FWrapProcY: TWrapProc;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineTransformed(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(APatternBmp: TBitmap32); reintroduce;
    destructor Destroy; override;

    procedure BeginRendering; override;

    property PatternBmp: TBitmap32 read FPatternBmp;
    property AffineTransform: Boolean read FAffineTransform write FAffineTransform;
    property InvMatrix: TFloatMatrix read FInvMatrix write FInvMatrix;
    property TileX: Single read FTileX write FTileX;
    property TileY: Single read FTileY write FTileY;
    property TileWidth: Single read FTileWidth write FTileWidth;
    property TileHeight: Single read FTileHeight write FTileHeight;
    property ScaleBmpX: Single read FScaleBmpX write FScaleBmpX;
    property ScaleBmpY: Single read FScaleBmpY write FScaleBmpY;
  end;

  TFontInfo = record
    FontFamily: UTF8String;
    Style: TFontStyles;
    Size: integer;
  end;

  TSvgRenderer = class(TObject)
  private
    FTarget: TCustomBitmap32;
    FViewportRect: TFloatRect;
    FDocumentRoot: TSvgDocumentNode;
    FBitmapPool: TSvgBitmapPool;
    FPolyRenderer: TPolygonRenderer32;
    FAllowExternalImages: Boolean;
    FRecursionDepth: integer;
    FTransformation: TAffineTransformation;
    FThemeFillColor: TSvgColor;
    FThemeStrokeColor: TSvgColor;
    FCurrentColor: TSvgColor;
    function GetTransformation: TAffineTransformation;
    procedure SetThemeFillColor32(AColor: TColor32);
    procedure SetThemeStrokeColor32(AColor: TColor32);
    function GetThemeFillColor32: TColor32;
    function GetThemeStrokeColor32: TColor32;
    procedure SetCurrentColor32(AColor: TColor32);
    function GetCurrentColor32: TColor32;
  protected type
    TThemeColor = (tcStroke, tcFill);

  protected
    function GetEffectiveColor(ANode: TSvgNode; AColor: TSvgColor; AThemeColor: TThemeColor = tcFill): TSvgColor;
    procedure RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint; AOpacity: Single; AColor: TSvgColor; AFillMode: TPolyFillMode = pfWinding);

    procedure RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
    procedure RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
    procedure RenderTextPathData(ATarget: TCustomBitmap32; const APathPoints: TArrayOfArrayOfFloatPoint; ANode: TSvgNode);
    procedure RenderTextAreaNode(ATarget: TCustomBitmap32; ATextAreaNode: TSvgTextAreaNode);
    procedure RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);
    procedure RenderGroupNode(ATarget: TCustomBitmap32; AGroupNode: TSvgGroupNode);
    procedure RenderClipPathNode(AMaskBmp: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMaskNode(AMaskBmp: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMarker(ATarget: TCustomBitmap32; AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single); // Angle is in radians!
    procedure RenderMarkers(ATarget: TCustomBitmap32; APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single);
    procedure VerticalBlur32(ASource, ADest: TCustomBitmap32; ARadius: TFloat);
    procedure RenderFilter(ATarget: TCustomBitmap32; AFilterNode: TSvgFilterNode; ANode: TSvgNode);
    procedure RenderNodeContent(ATarget: TCustomBitmap32; ANode: TSvgNode);
    procedure RenderNodeUnfiltered(ATarget: TCustomBitmap32; ANode: TSvgNode);
    function GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
    function GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
    procedure MapFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo); virtual;
  public
    constructor Create(ATarget: TCustomBitmap32 = nil); virtual;
    destructor Destroy; override;

    function GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure ReleaseOffscreenBitmap(var ABitmap: TCustomBitmap32);
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode; AX: Integer; AY: Integer); overload;
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode); overload;

    procedure ApplyMatrix(const AMatrix: TFloatMatrix);
    procedure ClearThemeColors;

    procedure RenderDocument(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect); overload;
    procedure RenderDocument(ADoc: TSvgDocumentNode); overload;
    procedure RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode); overload;
    procedure RenderNode(ANode: TSvgNode); overload;

    property Target: TCustomBitmap32 read FTarget write FTarget;
    property Transformation: TAffineTransformation read GetTransformation;
    property ViewportRect: TFloatRect read FViewportRect write FViewportRect;
    property AllowExternalImages: Boolean read FAllowExternalImages write FAllowExternalImages;
    property ThemeFillColor: TSvgColor read FThemeFillColor write FThemeFillColor;
    property ThemeFillColor32: TColor32 read GetThemeFillColor32 write SetThemeFillColor32;
    property ThemeStrokeColor: TSvgColor read FThemeStrokeColor write FThemeStrokeColor;
    property ThemeStrokeColor32: TColor32 read GetThemeStrokeColor32 write SetThemeStrokeColor32;
    property CurrentColor: TSvgColor read FCurrentColor write FCurrentColor;
    property CurrentColor32: TColor32 read GetCurrentColor32 write SetCurrentColor32;
  end;

//------------------------------------------------------------------------------

var
  SvgSystemFonts: record
    SansSerif: UTF8String;
    Serif: UTF8String;
    Monospace: UTF8String;
  end = (
    SansSerif:          'Arial';
    Serif:              'Times New Roman';
    Monospace:          'Courier New'
  );

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types,
  Math,
  GR32_Blend,
  GR32_Math,
  GR32_LowLevel,
  GR32_OrdinalMaps,
  GR32_Backends_Generic,
  GR32_Paths,
  GR32_Resamplers,
  GR32_Filters,
  GR32.Text.Types,
  GR32.Text.Win,
  GR32.Text.FontFace,
  GR32.Blur,
  GR32.Transpose,
  GR32.Blend.Modes,
  GR32.Blend.Modes.PorterDuff,
  GR32.Blend.Modes.PhotoShop,
  GR32.SVG.Utf8,
  // The following units need to be referenced so the can register their renderers
  GR32.SVG.Renderer.Filters,
  GR32.SVG.Renderer.PaintServers;

const
  ZERO_WIDTH_SPACE = $200B; // Unicode ZERO WIDTH SPACE

const
  OneOver255: Single = 1 / 255;

const
  cMaxRecursions = 20;
//  if (FMatrixStack.Count > cMaxStackDepth) then
//    raise Exception.Create('Max stack depth exceeded; Likely invalid recursion in svg references');

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

function TransformPathPoints(var APoints: TArrayOfArrayOfFloatPoint; const AMatrix: TFloatMatrix): TArrayOfArrayOfFloatPoint;
var
  i, j: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    SetLength(Result[i], Length(APoints[i]));
    for j := 0 to High(APoints[i]) do
      Result[i][j] := TFloatMatrixHelper(AMatrix).TransformPoint(APoints[i][j]);
  end;
end;

procedure TransformPathPointsInplace(var APoints: TArrayOfArrayOfFloatPoint; const AMatrix: TFloatMatrix);
var
  i, j: Integer;
begin
  for i := 0 to High(APoints) do
    for j := 0 to High(APoints[i]) do
      APoints[i][j] := TFloatMatrixHelper(AMatrix).TransformPoint(APoints[i][j]);
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

function GetSubtreeText(ANode: TSvgNode): UnicodeString;
var
  Child: TSvgNode;
begin
  Result := '';
  if ANode is TSvgTextPositioningNode then
    Result := TSvgTextPositioningNode(ANode).TextContent;

  if ANode is TSvgGroupNode then
  begin
    for Child in TSvgGroupNode(ANode).Children do
      Result := Result + GetSubtreeText(Child);
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

function TSvgRenderer.GetEffectiveColor(ANode: TSvgNode; AColor: TSvgColor; AThemeColor: TThemeColor): TSvgColor;
begin
  if (AColor.IsNone) then
    Exit(AColor);

  if AColor.IsCurrentColor then
  begin
    if FCurrentColor.IsSet then
      Result := FCurrentColor
    else
    if ANode <> nil then
    begin
      Result := ANode.Color;
      if Result.IsCurrentColor then
        Result := TSvgColor.Create(clBlack32);
    end else
      Result := TSvgColor.Create(clBlack32);
  end else
  begin
    case AThemeColor of
      tcStroke:
        if (FThemeStrokeColor.IsSet) then
          Result := FThemeStrokeColor
        else
          Result := AColor;

      tcFill:
        if (FThemeFillColor.IsSet) then
          Result := FThemeFillColor
        else
          Result := AColor;
    end;
  end;
end;

procedure TSvgRenderer.SetThemeFillColor32(AColor: TColor32);
begin
  FThemeFillColor := TSvgColor.Create(AColor);
end;

function TSvgRenderer.GetThemeFillColor32: TColor32;
begin
  Result := FThemeFillColor.Color;
end;

procedure TSvgRenderer.SetThemeStrokeColor32(AColor: TColor32);
begin
  FThemeStrokeColor := TSvgColor.Create(AColor);
end;

function TSvgRenderer.GetThemeStrokeColor32: TColor32;
begin
  Result := FThemeStrokeColor.Color;
end;

procedure TSvgRenderer.SetCurrentColor32(AColor: TColor32);
begin
  FCurrentColor := TSvgColor.Create(AColor);
end;

function TSvgRenderer.GetCurrentColor32: TColor32;
begin
  Result := FCurrentColor.Color;
end;

procedure TSvgRenderer.ClearThemeColors;
begin
  FThemeFillColor := TSvgColor.Unset;
  FThemeStrokeColor := TSvgColor.Unset;
  FCurrentColor := TSvgColor.Unset;
end;

{ TSvgPatternPolygonFiller }

procedure TSvgPatternPolygonFiller.BeginRendering;
begin
  inherited;

  // Pre-fetch optimized integer wrapping procedures for pattern bitmap pixel dimensions.
  // Uses WrapPow2 (bitwise AND) if tile dimension is a power-of-two or assembly Wrap otherwise.
  FWrapProcX := GetOptimalWrap(FPatternBmp.Width - 1);
  FWrapProcY := GetOptimalWrap(FPatternBmp.Height - 1);
end;

constructor TSvgPatternPolygonFiller.Create(APatternBmp: TBitmap32);
begin
  inherited Create;
  FPatternBmp := APatternBmp;
  Pattern := FPatternBmp;
  FAffineTransform := False;
  FInvMatrix := IdentityMatrix;
  FTileX := 0;
  FTileY := 0;
  FTileWidth := 1;
  FTileHeight := 1;
  FScaleBmpX := 1;
  FScaleBmpY := 1;
end;

destructor TSvgPatternPolygonFiller.Destroy;
begin
  FPatternBmp.Free;
  inherited Destroy;
end;

function TSvgPatternPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if FAffineTransform then
    Result := FillLineTransformed
  else
    Result := inherited GetFillLine;
end;

procedure TSvgPatternPolygonFiller.FillLineTransformed(Dst: PColor32; DstX, DstY, Length: Integer;
  AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  PtX, PtY: Single;
  Dx, Dy: Single;
  U, V, Upx, Vpx: Single;
  Ix, Iy: Integer;
  Wx, Wy: Cardinal;
  X1, X2, Y1, Y2: Integer;
  BitmapWidth, BitmapHeight: Integer;
  SrcColor: TColor32;
  BlendMem: TBlendMem;
  BlendMemEx: TBlendMemEx;
  MasterAlpha: Integer;
  Row1, Row2: array[0..1] of TColor32;
begin
  if (FPatternBmp = nil) or (FPatternBmp.Width <= 0) or (FPatternBmp.Height <= 0) or
     (FTileWidth <= 0) or (FTileHeight <= 0) then
    Exit;

  BitmapWidth := FPatternBmp.Width;
  BitmapHeight := FPatternBmp.Height;

  // Calculates starting point in pattern coordinate space for target pixel (DstX, DstY)
  PtX := DstX * FInvMatrix[0, 0] + DstY * FInvMatrix[1, 0] + FInvMatrix[2, 0];
  PtY := DstX * FInvMatrix[0, 1] + DstY * FInvMatrix[1, 1] + FInvMatrix[2, 1];

  // Incremental step per X pixel advancement along the scanline
  Dx := FInvMatrix[0, 0];
  Dy := FInvMatrix[0, 1];

  BlendMem := BLEND_MEM[CombineMode]^;
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  MasterAlpha := FPatternBmp.MasterAlpha;

  for X := 0 to Length - 1 do
  begin
    // Use Wrap to constrain continuous float pattern coordinates into tile
    // bounds [0..FTileWidth) and [0..FTileHeight)
    U := GR32_LowLevel.Wrap(PtX - FTileX, FTileWidth);
    V := GR32_LowLevel.Wrap(PtY - FTileY, FTileHeight);

    // Maps tile coordinate (U, V) to continuous bitmap pixel space
    Upx := U * FScaleBmpX - 0.5;
    Vpx := V * FScaleBmpY - 0.5;

    Ix := Floor(Upx);
    Iy := Floor(Vpx);

    Wx := Clamp(Round((Upx - Ix) * 256.0), 0, 256);
    Wy := Clamp(Round((Vpx - Iy) * 256.0), 0, 256);

    // Tile modulo wrapping across 4 neighbor pixels for continuous bilinear sampling
    X1 := FWrapProcX(Ix, BitmapWidth - 1);
    X2 := FWrapProcX(Ix + 1, BitmapWidth - 1);

    Y1 := FWrapProcY(Iy, BitmapHeight - 1);
    Y2 := FWrapProcY(Iy + 1, BitmapHeight - 1);

    // The funky array ordering matches AlphaInterpolator parameter convention (X2 at
    // index 0, X1 at index 1) and is due to the way AlphaInterpolator is implemented
    // and what its normal purpose is; It's used internally by the resamplers and
    // this is the setup they use.
    Row1[1] := FPatternBmp.Bits[X1 + Y1 * BitmapWidth];
    Row1[0] := FPatternBmp.Bits[X2 + Y1 * BitmapWidth];

    Row2[1] := FPatternBmp.Bits[X1 + Y2 * BitmapWidth];
    Row2[0] := FPatternBmp.Bits[X2 + Y2 * BitmapWidth];

    // Performs 2D bilinear interpolation across 4 neighbor pixels
    SrcColor := AlphaInterpolator(Wx, Wy, @Row2[0], @Row1[0]);

    if (MasterAlpha < 255) then
      ScaleAlpha(SrcColor, MasterAlpha * OneOver255);

    if (AlphaValues <> nil) then
    begin
      BlendMemEx(SrcColor, Dst^, AlphaValues^);
      Inc(AlphaValues);
    end else
    if (FPatternBmp.DrawMode = dmBlend) then
      BlendMem(SrcColor, Dst^)
    else
      Dst^ := SrcColor;

    Inc(Dst);
    PtX := PtX + Dx;
    PtY := PtY + Dy;
  end;
end;

{ TMapPool<T> }

constructor TMapPool<T, TElement>.Create(AMaxSize: Int64);
begin
  inherited Create;
  FPool := TObjectList<T>.Create(True);
  FMaxSize := AMaxSize;
end;

destructor TMapPool<T, TElement>.Destroy;
begin
  FPool.Free;
  inherited Destroy;
end;

function TMapPool<T, TElement>.Acquire(AWidth, AHeight: Integer; AClear: Boolean): T;
var
  LowIdx, HighIdx, MidIdx, CandidateIdx: Integer;
  TargetSize, CandidateSize: Int64;
  Candidate: T;
begin
  // Binary search the pool for candidate surface with existing buffer size (in
  // bytes) sufficient for requested bounds, in order to avoid costly buffer
  // reallocations.
  CandidateIdx := -1;
  TargetSize := AWidth * AHeight * SizeOf(TElement);
  LowIdx := 0;
  HighIdx := FPool.Count - 1;
  while LowIdx <= HighIdx do
  begin
    MidIdx := (LowIdx + HighIdx) div 2;
    Candidate := FPool[MidIdx];
    CandidateSize := Candidate.ByteCount;
    if (CandidateSize >= TargetSize) then
    begin
      CandidateIdx := MidIdx;
      if (CandidateSize = TargetSize) then
        break;
      HighIdx := MidIdx - 1;
    end else
      LowIdx := MidIdx + 1;
  end;

  // If the candidate is oversize and would cause a reallocation
  // once resized below, we might as well keep it in the pool and
  // instead create a new item
  if (CandidateIdx <> -1) then
  begin
    Candidate := FPool[CandidateIdx];
    if (Candidate.ByteCount > TargetSize) and CandidateWouldReallocate(Candidate, TargetSize) then
       CandidateIdx := -1;
  end;

  if (CandidateIdx = -1) then
  begin
    // Instantiate a new map if no candidate with sufficient buffer dimensions
    // exists in pool
    Result := CreateNewMap;
    Result.SetSize(AWidth, AHeight, AClear);
  end else
  begin
    Result := FPool.ExtractAt(CandidateIdx);
    Dec(FPoolSize, Result.ByteCount);

    // Set dimensions. If we're lucky then this doesn't cause a reallocation
    Result.SetSize(AWidth, AHeight, AClear);
{$ifdef DEBUG}
    Result.EndLockUpdate; // For debug: Signal that bitmap is out of pool
{$endif DEBUG}
  end;

  PrepareMap(Result);
end;

function TMapPool<T, TElement>.CandidateWouldReallocate(Candidate: T; TargetSize: Int64): boolean;
begin
  Result := False;
end;

procedure TMapPool<T, TElement>.Clear;
begin
  FPool.Clear;
end;

procedure TMapPool<T, TElement>.PrepareMap(Map: T);
begin
end;

procedure TMapPool<T, TElement>.Release(Map: T);
var
  LowIdx, HighIdx, MidIdx: Integer;
  TargetByteCount, MidByteCount: Int64;
begin
  if Map = nil then
    exit;

  // Is there room in the pool?
  if (FMaxSize > 0) and (FPoolSize + Map.ByteCount > FMaxSize) then
  begin
    // Can we make room by deleting the largest item in the pool?
    // We only do this if the largest item is at least twice as big
    // as the one we want to add. This gives priority to smaller
    // items and avoids large items trashing the pool.
    if (FPool.Count = 0) or (FPoolSize + Map.ByteCount - FPool.Last.ByteCount > FMaxSize) then
    begin
      Map.Free;
      exit;
    end else
      FPool.Delete(FPool.Count-1);
    // Fall though to insert in pool
  end;

  // Insert ordered by ByteCount
  TargetByteCount := Map.ByteCount;
  LowIdx := 0;
  HighIdx := FPool.Count - 1;
  while LowIdx <= HighIdx do
  begin
    MidIdx := (LowIdx + HighIdx) div 2;
    MidByteCount := FPool[MidIdx].ByteCount;
    if (TargetByteCount < MidByteCount) then
      HighIdx := MidIdx - 1
    else
      LowIdx := MidIdx + 1;
  end;

  FPool.Insert(LowIdx, Map);

{$ifdef DEBUG}
  Map.BeginLockUpdate; // For debug: Signal that bitmap is in pool
{$endif DEBUG}
  Inc(FPoolSize, Map.ByteCount);
end;

{ TSvgBitmapPool }

function TSvgBitmapPool.CandidateWouldReallocate(Candidate: TCustomBitmap32; TargetSize: Int64): boolean;
begin
  if (Candidate.Backend is TMemoryBackend) and (TMemoryBackend(Candidate.Backend).MaxOversize <> 0) then
  begin
    Result := (Candidate.ByteCount - TargetSize > TMemoryBackend(Candidate.Backend).MaxOversize);
  end else
    Result := False;
end;

function TSvgBitmapPool.CreateNewMap: TCustomBitmap32;
begin
  Result := TBitmap32.Create(TMemoryBackend);
  TMemoryBackend(Result.Backend).MaxOversize := BitmapMaxOversize;
end;

procedure TSvgBitmapPool.PrepareMap(Map: TCustomBitmap32);
begin
  Map.MasterAlpha := 255;
  Map.DrawMode := dmBlend;
  Map.CombineMode := cmMerge;
end;


procedure TSvgBitmapPool.Release(Map: TCustomBitmap32);
begin
  if (Map <> nil) then
  begin
    if not(Map.Backend is TMemoryBackend) then
    begin
      // Not one of ours; Kill it!
      Map.Free;
      exit;
    end;

    Map.OnPixelCombine := nil;
  end;

  inherited;
end;

{ TSvgRenderer }

constructor TSvgRenderer.Create(ATarget: TCustomBitmap32);
begin
  inherited Create;
  FTarget := ATarget;
  FTransformation := TAffineTransformation.Create;
  FViewportRect := FloatRect(0, 0, 0, 0);
  FDocumentRoot := nil;
  FBitmapPool := TSvgBitmapPool.Create;
  FBitmapPool.BitmapMaxOversize := 64*1024; // Magic!
  FBitmapPool.MaxSize := 256*1024*1024; // More magic!
  FAllowExternalImages := False;
  FThemeFillColor := TSvgColor.Unset;
  FThemeStrokeColor := TSvgColor.Unset;
  FCurrentColor := TSvgColor.Unset;
end;

destructor TSvgRenderer.Destroy;
begin
  FTransformation.Free;
  FBitmapPool.Free;
  FPolyRenderer.Free;
  inherited Destroy;
end;

function TSvgRenderer.GetTransformation: TAffineTransformation;
begin
  Result := FTransformation;
end;

function TSvgRenderer.GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean): TCustomBitmap32;
begin
  Result := FBitmapPool.Acquire(AWidth, AHeight, AClear);
end;

procedure TSvgRenderer.ReleaseOffscreenBitmap(var ABitmap: TCustomBitmap32);
begin
  FBitmapPool.Release(ABitmap);
  ABitmap := nil;
end;

procedure TSvgRenderer.ApplyMatrix(const AMatrix: TFloatMatrix);
begin
  FTransformation.Clear(Mult(FTransformation.Matrix, AMatrix));
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
      Result[i][j] := FTransformation.Transform(APoints[i][j]);
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

procedure TSvgRenderer.RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint;
  AOpacity: Single; AColor: TSvgColor; AFillMode: TPolyFillMode);
var
  PaintServerNode: TSvgGroupNode;
  Bounds: TFloatRect;
  Filler: TCustomPolygonFiller;
  Color: TColor32;
  PaintServerRenderer: TPaintServerRendererClass;
begin
  if (APoints = nil) then
    exit;

  if (APaintServer <> nil) then
  begin
    Bounds := GetPathBounds(APoints);

    PaintServerNode := TSvgGroupNode(APaintServer);

    Filler := nil;
    if (PaintServerNode is TSvgGradientNode) or (PaintServerNode is TSvgPatternNode) then
    begin
      // Get a paint server renderer...
      PaintServerRenderer := TPaintServerRendererGradientClass(PaintServerNode.GetRenderClass);
      if (PaintServerRenderer <> nil) then
        // ...and get a filler from it
        Filler := PaintServerRenderer.CreateFiller(Self, PaintServerNode, Bounds, AOpacity);
    end;

    if (Filler = nil) then
      exit;

    try
      if (FPolyRenderer = nil) then
        FPolyRenderer := DefaultPolygonRendererClass.Create(ATarget);

      FPolyRenderer.Bitmap := ATarget;
      FPolyRenderer.Filler := Filler;
      try
        FPolyRenderer.FillMode := AFillMode;

        FPolyRenderer.PolyPolygonFS(APoints);
      finally
        FPolyRenderer.Filler := nil;
      end;
    finally
      Filler.Free;
    end;
  end else
  if (AColor.IsVisible) then
  begin
    Color := AColor.Color;
    if (AOpacity < 1.0) then
      ScaleAlpha(Color, AOpacity);

    if (AlphaComponent(Color) = 0) then
      exit;

    if (FPolyRenderer = nil) then
      FPolyRenderer := DefaultPolygonRendererClass.Create(ATarget);

    FPolyRenderer.Bitmap := ATarget;
    FPolyRenderer.Color := Color;
    FPolyRenderer.FillMode := AFillMode;

    FPolyRenderer.PolyPolygonFS(APoints);
  end;
end;


procedure TSvgRenderer.RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
var
  PathPoints, TransformedPoints: TArrayOfArrayOfFloatPoint;
  UserStrokeWidth, StrokeWidth, MatScale, ScaledOffset: Single;
  Points, StrokePoints: TArrayOfArrayOfFloatPoint;
  ScaledDashArray: TArrayOfFloat;
  i, j: Integer;
  FloatRoi: TFloatRect;
  RoiRect: TRect;
  NeedsOffscreen: Boolean;
  EffectiveBlendMode: TSvgBlendMode;
  RenderBmp, OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  FillColor, StrokeColor: TSvgColor;
  HasFill, HasStroke, HasMarkers: boolean;
{$if not defined(USE_SIMD_MASK_FILTERS)}
  SourceP, DestP: PColor32;
  x: Integer;
  AlphaVal, Gray: Byte;
{$ifend}
begin
  if (APathNode = nil) or (ATarget = nil) then
    Exit;

  (*
  ** 1. Determine what elements we need to process here
  *)
  UserStrokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Width);
  if (APathNode.Stroke.ResolvedPaintServer = nil) and (APathNode.Stroke.Opacity > 0) and (UserStrokeWidth > 0) then
    StrokeColor := GetEffectiveColor(APathNode, APathNode.Stroke.Color, tcStroke)
  else
    StrokeColor := TSvgColor.Unset; // No visible stroke or use Paint Server
  HasStroke := (UserStrokeWidth > 0) and ((APathNode.Stroke.ResolvedPaintServer <> nil) or (StrokeColor.IsVisible));

  if (APathNode.Fill.ResolvedPaintServer = nil) and (APathNode.Fill.Opacity > 0) then
    FillColor := GetEffectiveColor(APathNode, APathNode.Fill.Color, tcFill)
  else
    FillColor := TSvgColor.Unset; // No visible fill or use Paint Server
  HasFill := (APathNode.Fill.ResolvedPaintServer <> nil) or (FillColor.IsVisible);

  HasMarkers := (APathNode.ResolvedMarkerStart <> nil) or (APathNode.ResolvedMarkerMid <> nil) or (APathNode.ResolvedMarkerEnd <> nil);

  (*
  ** 2. Generate path in user space
  *)
  PathPoints := APathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
  if Length(PathPoints) = 0 then
    Exit;

  // Transform path points into world coordinates
  TransformedPoints := GetTransformedPoints(PathPoints);

  (*
  ** 3. Generate stroke polygons if stroked
  *)
  StrokePoints := nil;
  if (HasStroke) then
  begin
    MatScale := GetMatrixScale(FTransformation.Matrix);
    StrokeWidth := UserStrokeWidth * MatScale;
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

  (*
  ** 4. Calculate ROI bounding box AFTER stroking and collect all output
  **    poly-polygons produced and rendered by the polygon node
  *)
  if (HasFill or HasStroke) then
  begin
    if (HasFill) then
    begin
      FloatRoi := PolyPolygonBounds(TransformedPoints);
      if (HasStroke) then
        FloatRoi := FloatRoi + PolyPolygonBounds(StrokePoints);
    end else
      FloatRoi := PolyPolygonBounds(StrokePoints);
  end else
    FloatRoi := PolyPolygonBounds(TransformedPoints); // Only markers

  RoiRect := MakeRect(FloatRoi, rrOutside);

  // Intersect calculated ROI with target canvas bounds to skip off-screen geometry
  if not GR32.IntersectRect(RoiRect, RoiRect, ATarget.BoundsRect) then
    Exit;

  (*
  ** 5. Check whether offscreen bitmap compositing is required (opacity, clip-path, mask, blend-mode)
  *)
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
    if (HasFill or HasStroke) then
    begin
      if NeedsOffscreen then
      begin
        FTransformation.Push;
        FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);
      end;
      try
        (*
        ** 6. Fill Rendering
        *)
        RenderPolyPolygon(RenderBmp, APathNode.Fill.ResolvedPaintServer, TransformedPoints, APathNode.Fill.Opacity, FillColor, APathNode.Fill.FillRule);

        (*
        ** 7. Stroke Rendering
        *)
        RenderPolyPolygon(RenderBmp, APathNode.Stroke.ResolvedPaintServer, StrokePoints, APathNode.Stroke.Opacity, StrokeColor);
      finally
        if NeedsOffscreen then
          FTransformation.Pop;
      end;
    end;

    (*
    ** 8. Markers Rendering (per SVG specification, markers paint on top of fill and stroke)
    *)
    if (HasMarkers) then
    begin
      if NeedsOffscreen then
      begin
        FTransformation.Push;
        try
          FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);

          RenderMarkers(RenderBmp, APathNode, PathPoints, UserStrokeWidth);
        finally
          FTransformation.Pop;
        end;
      end else
        RenderMarkers(RenderBmp, APathNode, PathPoints, UserStrokeWidth);
    end;

    (*
    ** 9. Apply Offscreen Compositing (ClipPath, Mask, Opacity, Blend)
    *)
    if NeedsOffscreen then
    begin
      // Apply ClipPath in ROI space
      if (APathNode.ResolvedClipPath <> nil) then
      begin
        ClipNodeTarget := APathNode.ResolvedClipPath;
        ClipMaskBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, True);

        FTransformation.Push;
        try
          FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);

          RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, FloatRoi, RoiRect);
        finally
          FTransformation.Pop;
        end;

{$if defined(USE_SIMD_MASK_FILTERS)}
        ScaleAlphaLine(PColor32(ClipMaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
        DestP := PColor32(OffscreenBmp.Bits);
        SourceP := PColor32(ClipMaskBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          AlphaVal := AlphaComponent(SourceP^);
          if AlphaVal < 255 then
            ScaleAlpha(DestP^, AlphaVal * OneOver255);
          Inc(DestP);
          Inc(SourceP);
        end;
{$ifend}
      end;

      // Apply Alpha Mask in ROI space
      if (APathNode.ResolvedMask <> nil) then
      begin
        MaskNodeTarget := APathNode.ResolvedMask;
        MaskBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, False);

        FTransformation.Push;
        try
          FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);

          RenderMaskNode(MaskBmp, MaskNodeTarget, FloatRoi, RoiRect);
        finally
          FTransformation.Pop;
        end;

{$if defined(USE_SIMD_MASK_FILTERS)}
        ApplyAlphaMask(PColor32(MaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
        SourceP := PColor32(MaskBmp.Bits);
        DestP := PColor32(OffscreenBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          Gray := Intensity(SourceP^);
          Gray := Round(Gray * (AlphaComponent(SourceP^) * OneOver255));
          ScaleAlpha(DestP^, Gray * OneOver255);
          Inc(DestP);
          Inc(SourceP);
        end;
{$ifend}
      end;

      // Apply Opacity
      if (APathNode.Opacity < 1.0) then
      begin
{$if defined(USE_SIMD_MASK_FILTERS)}
        ScaleAlphaMems(PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount, APathNode.Opacity);
{$else}
        SourceP := PColor32(OffscreenBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          ScaleAlpha(SourceP^, APathNode.Opacity);
          Inc(SourceP);
        end;
{$ifend}
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
  Width, Height, OffsetX, OffsetY: Single;
  i: Integer;
begin
  // Renders child nodes of a <clipPath> onto a temporary alpha surface.
  // If clipPathUnits = guObjectBoundingBox, applies translation and
  // scale derived from target object bounds in ROI coordinate space.
  if (AClipNode = nil) or (AMaskBmp = nil) then
    Exit;

  FTransformation.Push;
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

      FTransformation.Clear;
      FTransformation.Scale(Width, Height);
      FTransformation.Translate(OffsetX, OffsetY);
    end;

    AMaskBmp.Clear(0); // Clear to 0 transparent so filled shapes paint non-zero alpha inside clip region

    for i := 0 to AClipNode.Children.Count - 1 do
      RenderNode(AMaskBmp, AClipNode.Children[i]);
  finally
    FTransformation.Pop;
  end;
end;

procedure TSvgRenderer.RenderMaskNode(AMaskBmp: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
var
  Width, Height, OffsetX, OffsetY: Single;
  i: Integer;
begin
  // Renders child nodes of a <mask> onto a temporary luminance/alpha surface.
  // If maskContentUnits = guObjectBoundingBox, applies translation and scale
  // derived from target object bounds in ROI coordinate space.

  if (AMaskNode = nil) or (AMaskBmp = nil) then
    Exit;

  FTransformation.Push;
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

      FTransformation.Clear;
      FTransformation.Scale(Width, Height);
      FTransformation.Translate(OffsetX, OffsetY);
    end;

    AMaskBmp.Clear(clBlack32);

    for i := 0 to AMaskNode.Children.Count - 1 do
      RenderNode(AMaskBmp, AMaskNode.Children[i]);
  finally
    FTransformation.Pop;
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

  FTransformation.Push;
  try
    ApplyMatrix(markerMat.Matrix);

    for i := 0 to AMarker.Children.Count - 1 do
      RenderNode(ATarget, AMarker.Children[i]);
  finally
    FTransformation.Pop;
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

  procedure RenderChildren(ATargetSurface: TCustomBitmap32);
  var
    DocNode: TSvgDocumentNode;
    DocW, DocH, DocX, DocY: Single;
    DocViewBox: TSvgViewBox;
    DocVpMat: TFloatMatrix;
    DocTargetRect, SavedViewport: TFloatRect;
    i: Integer;
  begin
    if (AGroupNode is TSvgDocumentNode) and (AGroupNode <> FDocumentRoot) then
    begin

      DocNode := TSvgDocumentNode(AGroupNode);

      DocW := DocNode.Width.ToPixels(FViewportRect.Width);
      DocH := DocNode.Height.ToPixels(FViewportRect.Height);
      DocX := DocNode.X.ToPixels(FViewportRect.Width);
      DocY := DocNode.Y.ToPixels(FViewportRect.Height);

      DocTargetRect := FloatRect(DocX, DocY, DocX + DocW, DocY + DocH);

      DocViewBox := DocNode.ViewBox;
      if not DocViewBox.IsValid then
      begin
        if (DocW > 0) and (DocH > 0) then
          DocViewBox := TSvgViewBox.Create(0, 0, DocW, DocH)
        else
          DocViewBox := TSvgViewBox.Create(0, 0, FViewportRect.Width, FViewportRect.Height);
      end;

      DocVpMat := DocViewBox.GetTransform(DocTargetRect, DocNode.PreserveAspectRatio);

      SavedViewport := FViewportRect;
      FViewportRect := FloatRect(DocViewBox.X, DocViewBox.Y, DocViewBox.X + DocViewBox.Width, DocViewBox.Y + DocViewBox.Height);
      try
        FTransformation.Push;
        try
          ApplyMatrix(DocVpMat);

          for i := 0 to DocNode.Children.Count - 1 do
            RenderNode(ATargetSurface, DocNode.Children[i]);

        finally
          FTransformation.Pop;
        end;
      finally
        FViewportRect := SavedViewport;
      end;

    end else
    if AGroupNode is TSvgSwitchNode then
    begin

      if (TSvgSwitchNode(AGroupNode).SelectedChild <> nil) then
        RenderNode(ATargetSurface, TSvgSwitchNode(AGroupNode).SelectedChild);

    end else
    begin

      for i := 0 to AGroupNode.Children.Count - 1 do
        RenderNode(ATargetSurface, AGroupNode.Children[i]);

    end;
  end;

var
  k: Integer;
  OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  GroupBounds, TargetWorldBounds: TFloatRect;
  GroupRoi: TRect;
  Points: array[0..3] of TFloatPoint;
{$if not defined(USE_SIMD_MASK_FILTERS)}
  SourceP, DestP: PColor32;
  x: Integer;
  Gray: Byte;
  AlphaVal: Byte;
{$ifend}
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
    if (GroupBounds.Right <= GroupBounds.Left) or (GroupBounds.Bottom <= GroupBounds.Top) then
    begin
      // Fallback for empty or degenerate group bounding boxes;
      // If GetObjectBoundingBox returns empty/inverted bounds,
      // we fall back to the target canvas/viewport dimensions,
      // ensuring offscreen surfaces are allocated properly
      // rather than disappearing.
      // See identical logic in TSvgRenderer.RenderNode & TSvgRenderer.RenderFilter
      TargetWorldBounds := FloatRect(0, 0, ATarget.Width, ATarget.Height);
    end else
    begin
      Points[0] := FTransformation.Transform(FloatPoint(GroupBounds.Left, GroupBounds.Top));
      Points[1] := FTransformation.Transform(FloatPoint(GroupBounds.Right, GroupBounds.Top));
      Points[2] := FTransformation.Transform(FloatPoint(GroupBounds.Right, GroupBounds.Bottom));
      Points[3] := FTransformation.Transform(FloatPoint(GroupBounds.Left, GroupBounds.Bottom));

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
      FTransformation.Push;
      try
        FTransformation.Translate(-GroupRoi.Left, -GroupRoi.Top);

        RenderChildren(OffscreenBmp);
      finally
        FTransformation.Pop;
      end;

      // Apply ClipPath in ROI coordinate space
      if (AGroupNode.ResolvedClipPath <> nil) then
      begin
        ClipNodeTarget := AGroupNode.ResolvedClipPath;
        ClipMaskBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, True);

        FTransformation.Push;
        try
          FTransformation.Translate(-GroupRoi.Left, -GroupRoi.Top);

          RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, TargetWorldBounds, GroupRoi);
        finally
          FTransformation.Pop;
        end;

{$if defined(USE_SIMD_MASK_FILTERS)}
        ScaleAlphaLine(PColor32(ClipMaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
        SourceP := PColor32(ClipMaskBmp.Bits);
        DestP := PColor32(OffscreenBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          AlphaVal := AlphaComponent(SourceP^);
          if AlphaVal < 255 then
            ScaleAlpha(DestP^, AlphaVal * OneOver255);
          Inc(DestP);
          Inc(SourceP);
        end;
{$ifend}
      end;

      // Apply Alpha Mask in ROI coordinate space
      if (AGroupNode.ResolvedMask <> nil) then
      begin
        MaskNodeTarget := AGroupNode.ResolvedMask;
        MaskBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, False);

        FTransformation.Push;
        try
          FTransformation.Translate(-GroupRoi.Left, -GroupRoi.Top);

          RenderMaskNode(MaskBmp, MaskNodeTarget, TargetWorldBounds, GroupRoi);
        finally
          FTransformation.Pop;
        end;

{$if defined(USE_SIMD_MASK_FILTERS)}
        ApplyAlphaMask(PColor32(MaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
        DestP := PColor32(OffscreenBmp.Bits);
        SourceP := PColor32(MaskBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          Gray := Intensity(SourceP^);
          Gray := Round(Gray * (AlphaComponent(SourceP^) * OneOver255));
          ScaleAlpha(DestP^, Gray * OneOver255);
          Inc(DestP);
          Inc(SourceP);
        end;
{$ifend}
      end;

      // Apply Group Opacity
      if (AGroupNode.Opacity < 1.0) then
      begin
{$if defined(USE_SIMD_MASK_FILTERS)}
        ScaleAlphaMems(PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount, AGroupNode.Opacity);
{$else}
        SourceP := PColor32(OffscreenBmp.Bits);

        for x := 0 to OffscreenBmp.PixelCount - 1 do
        begin
          ScaleAlpha(SourceP^, AGroupNode.Opacity);
          Inc(SourceP);
        end;
{$ifend}
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

  RenderChildren(ATarget);
end;

procedure TSvgRenderer.VerticalBlur32(ASource, ADest: TCustomBitmap32; ARadius: TFloat);
var
  TransposedSrc, TransposedDst: TCustomBitmap32;
begin
  if (ASource = nil) or (ADest = nil) then
    Exit;

  if (ARadius < Blur32MinRadius) then
  begin
    ASource.CopyMapTo(ADest);
    Exit;
  end;

  TransposedSrc := GetOffscreenBitmap(ASource.Height, ASource.Width, False);
  TransposedDst := GetOffscreenBitmap(ASource.Height, ASource.Width, False);
  try
    Transpose32(ASource.Bits, TransposedSrc.Bits, ASource.Width, ASource.Height);
    HorizontalBlur32(TransposedSrc, TransposedDst, ARadius);
    Transpose32(TransposedDst.Bits, ADest.Bits, TransposedDst.Width, TransposedDst.Height);
  finally
    ReleaseOffscreenBitmap(TransposedSrc);
    ReleaseOffscreenBitmap(TransposedDst);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgRenderer.RenderFilter(ATarget: TCustomBitmap32; AFilterNode: TSvgFilterNode; ANode: TSvgNode);
var
  RenderData: TFilterRenderData;
  i, Count: Integer;
  Node: TSvgNode;
  pSource, pDest: PColor32;
  SourceBounds, FilterBounds, FilterRegionRect: TFloatRect;
  PathNode: TSvgPathNode;
  PathPoints, TransformedPoints, StrokePoints: TArrayOfArrayOfFloatPoint;
  StrokeWidth, BBoxWidth, BBoxHeight, RegionLeft, RegionTop, RegionWidth, RegionHeight: Single;
  NodeBounds: TFloatRect;
  Pts: array[0..3] of TFloatPoint;
  Margin: TFloatRect;
  MarginX, MarginY, RadiusX, RadiusY: Integer;
  StrokeColor: TSvgColor;
  HasStroke: boolean;
  FilterNode: TSvgFilterPrimitiveNode;
  FilterRenderer: TFilterRendererClass;
begin
  if (AFilterNode = nil) or (ANode = nil) or (ATarget = nil) then
    Exit;

  FTransformation.Push;
  try
    ApplyMatrix(ANode.Transform);

    // 1. Calculate source bounds in screen/target space at full resolution.
    RenderData.Scale := GetMatrixScale(FTransformation.Matrix);

    if ANode is TSvgPathNode then
    begin
      PathNode := TSvgPathNode(ANode);
      PathPoints := PathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
      NodeBounds := PolyPolygonBounds(PathPoints);

      if Length(PathPoints) > 0 then
      begin
        TransformedPoints := GetTransformedPoints(PathPoints);
        SourceBounds := PolyPolygonBounds(TransformedPoints);

        StrokePoints := nil;
        StrokeWidth := PathNode.Stroke.Width.ToPixels(FViewportRect.Width);
        if (PathNode.Stroke.ResolvedPaintServer = nil) then
          StrokeColor := GetEffectiveColor(PathNode, PathNode.Stroke.Color, tcStroke)
        else
          StrokeColor := TSvgColor.Unset;
        HasStroke := (StrokeWidth > 0) and ((PathNode.Stroke.ResolvedPaintServer <> nil) or (StrokeColor.IsVisible));

        if (HasStroke) then
        begin
          // Calculate how far beyond the polygon bounding box the stroke will potentially extend
          StrokeWidth := StrokeWidth * RenderData.Scale / 2;

          if (PathNode.Stroke.JoinStyle = jsMiter) then
            StrokeWidth := StrokeWidth * Max(1.0, PathNode.Stroke.MiterLimit);

          GR32.InflateRect(SourceBounds, StrokeWidth, StrokeWidth);
        end;

      end else
        SourceBounds := FloatRect(0, 0, 0, 0);

    end else
    begin
      PathPoints := nil;

      // For non-polygon nodes, calculate object bounds in world/screen space
      NodeBounds := ANode.GetObjectBoundingBox;
      Pts[0] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Top));
      Pts[1] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Top));
      Pts[2] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Bottom));
      Pts[3] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Bottom));

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
        Margin := TFilterRendererGaussianBlur.GetMargins(TSvgFeGaussianBlurNode(Node), RenderData.Scale);

        RadiusX := Ceil(Margin.Left);
        RadiusY := Ceil(Margin.Top);
        if RadiusX > MarginX then MarginX := RadiusX;
        if RadiusY > MarginY then MarginY := RadiusY;
      end else
      if Node is TSvgFeOffsetNode then
      begin
        Margin := TFilterRendererOffset.GetMargins(TSvgFeOffsetNode(Node), RenderData.Scale);
        MarginX := MarginX + Ceil(Margin.Left);
        MarginY := MarginY + Ceil(Margin.Top);
      end else
      if Node is TSvgFeDropShadowNode then
      begin
        Margin := TFilterRendererDropShadow.GetMargins(TSvgFeDropShadowNode(Node), RenderData.Scale);
        MarginX := MarginX + Ceil(Margin.Left);
        MarginY := MarginY + Ceil(Margin.Top);
      end else
      if Node is TSvgFeMorphologyNode then
      begin
        Margin := TFilterRendererMorphology.GetMargins(TSvgFeMorphologyNode(Node), RenderData.Scale);
        MarginX := MarginX + Ceil(Margin.Left);
        MarginY := MarginY + Ceil(Margin.Top);
      end else
      if Node is TSvgFeDisplacementMapNode then
      begin
        Margin := TFilterRendererDisplacementMap.GetMargins(TSvgFeDisplacementMapNode(Node), RenderData.Scale);
        MarginX := MarginX + Ceil(Margin.Left);
        MarginY := MarginY + Ceil(Margin.Top);
      end;
    end;

    // Calculate filter region defined on AFilterNode (x, y, width, height, filterUnits)
    //
    // Fallback for empty or degenerate group bounding boxes;
    // If GetObjectBoundingBox returns empty/inverted bounds,
    // we fall back to the target canvas/viewport dimensions,
    // ensuring offscreen surfaces are allocated properly
    // rather than disappearing.
    // See identical logic in TSvgRenderer.RenderNode & TSvgRenderer.RenderGroupNode
    if (NodeBounds.Right <= NodeBounds.Left) or (NodeBounds.Bottom <= NodeBounds.Top) then
      NodeBounds := FViewportRect;

    BBoxWidth := NodeBounds.Right - NodeBounds.Left;
    BBoxHeight := NodeBounds.Bottom - NodeBounds.Top;
    // We have already handled these cases above, but whatever
    if BBoxWidth <= 0 then BBoxWidth := 1.0;
    if BBoxHeight <= 0 then BBoxHeight := 1.0;

    if AFilterNode.FilterUnits = guObjectBoundingBox then
    begin
      RegionLeft := NodeBounds.Left + AFilterNode.X.ToPixels(1.0) * BBoxWidth;
      RegionTop := NodeBounds.Top + AFilterNode.Y.ToPixels(1.0) * BBoxHeight;
      RegionWidth := AFilterNode.Width.ToPixels(1.0) * BBoxWidth;
      RegionHeight := AFilterNode.Height.ToPixels(1.0) * BBoxHeight;
    end else
    begin
      RegionLeft := AFilterNode.X.ToPixels(FViewportRect.Width);
      RegionTop := AFilterNode.Y.ToPixels(FViewportRect.Height);
      RegionWidth := AFilterNode.Width.ToPixels(FViewportRect.Width);
      RegionHeight := AFilterNode.Height.ToPixels(FViewportRect.Height);
    end;

    FilterRegionRect := FloatRect(RegionLeft, RegionTop, RegionLeft + RegionWidth, RegionTop + RegionHeight);

    // Transform FilterRegionRect to world/screen space
    Pts[0] := FTransformation.Transform(FloatPoint(FilterRegionRect.Left, FilterRegionRect.Top));
    Pts[1] := FTransformation.Transform(FloatPoint(FilterRegionRect.Right, FilterRegionRect.Top));
    Pts[2] := FTransformation.Transform(FloatPoint(FilterRegionRect.Right, FilterRegionRect.Bottom));
    Pts[3] := FTransformation.Transform(FloatPoint(FilterRegionRect.Left, FilterRegionRect.Bottom));

    FilterRegionRect := FloatRect(Pts[0].X, Pts[0].Y, Pts[0].X, Pts[0].Y);
    for i := 1 to 3 do
    begin
      if (Pts[i].X < FilterRegionRect.Left) then FilterRegionRect.Left := Pts[i].X;
      if (Pts[i].X > FilterRegionRect.Right) then FilterRegionRect.Right := Pts[i].X;
      if (Pts[i].Y < FilterRegionRect.Top) then FilterRegionRect.Top := Pts[i].Y;
      if (Pts[i].Y > FilterRegionRect.Bottom) then FilterRegionRect.Bottom := Pts[i].Y;
    end;

    // 3. Inflate source bounds by filter margins to determine total Filter ROI, then clip to FilterRegionRect
    FilterBounds := SourceBounds;
    GR32.InflateRect(FilterBounds, MarginX, MarginY);

    if FilterBounds.Left < FilterRegionRect.Left then FilterBounds.Left := FilterRegionRect.Left;
    if FilterBounds.Top < FilterRegionRect.Top then FilterBounds.Top := FilterRegionRect.Top;
    if FilterBounds.Right > FilterRegionRect.Right then FilterBounds.Right := FilterRegionRect.Right;
    if FilterBounds.Bottom > FilterRegionRect.Bottom then FilterBounds.Bottom := FilterRegionRect.Bottom;

    if (FilterBounds.Right <= FilterBounds.Left) or (FilterBounds.Bottom <= FilterBounds.Top) then
      Exit;

    RenderData.ROI := MakeRect(FilterBounds, rrOutside);
    if not GR32.IntersectRect(RenderData.ROI, RenderData.ROI, ATarget.BoundsRect) then
      Exit;

    // 4. Allocate intermediate filter surfaces constrained to the ROI dimensions in full screen resolution
    RenderData.SourceGraphic := GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);
    RenderData.SourceAlpha := GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);
    RenderData.NamedSurfaces := nil;
    try
      // Render element into SourceGraphic in ROI coordinate space
      FTransformation.Push;
      try
        FTransformation.Translate(-RenderData.ROI.Left, -RenderData.ROI.Top);

        RenderNodeContent(RenderData.SourceGraphic, ANode);
      finally
        FTransformation.Pop;
      end;

      // Derive SourceAlpha from SourceGraphic
      pSource := PColor32(RenderData.SourceGraphic.Bits);
      pDest := PColor32(RenderData.SourceAlpha.Bits);
      for i := 0 to RenderData.SourceGraphic.PixelCount - 1 do
      begin
        PColor32Entry(pDest).ARGB := PColor32Entry(pSource).A shl 24;
        Inc(pSource);
        Inc(pDest);
      end;

      RenderData.CurrentSurface := RenderData.SourceGraphic;
      RenderData.UnnamedSurface := nil; // Indicates that CurrentSurface's ownership is "somebody else's problem".

      // Count number of named surfaces so we can preallocate the surface array
      Count := 0;
      for Node in AFilterNode.Children do
        if (Node is TSvgFilterPrimitiveNode) and TSvgFilterPrimitiveNode(Node).IsReferenceTarget then
          Inc(Count);
      SetLength(RenderData.NamedSurfaces, Count);
      Count := 0;

      // 5. Process filter primitive nodes sequentially on ROI surfaces
      for Node in AFilterNode.Children do
      begin
        if not (Node is TSvgFilterPrimitiveNode) then
          Continue;

        // Get the renderer that handles the filter node and...
        FilterNode := TSvgFilterPrimitiveNode(Node);
        FilterRenderer := TFilterRendererClass(FilterNode.GetRenderClass);

        // ...and render if we got one
        if (FilterRenderer <> nil) then
          FilterRenderer.Render(Self, FilterNode, RenderData)
        else
        begin
          // Unsupported filter; Keep the current intermediate result and let
          // the next filter continue with it.
          if (RenderData.CurrentSurface = RenderData.SourceGraphic) then
            continue; // No filter has yet produced anything
        end;

        (*
        ** CurrentSurface has been allocated by the filter.
        **
        ** We either:
        **
        ** - Save it in the NamedSurfaces array and nil UnnamedSurface,
        **
        ** or
        **
        ** - Save a pointer to it in UnnamedSurface.
        **
        ** When the next filter's Render method has called "Input := ResolveSurface(...)", it
        ** either:
        **
        ** - Keeps the bitmap alive if Input=CurrentSurface=UnnamedSurface,
        **
        ** or
        **
        ** - Releases the bitmap and nills UnnamedSurface.
        *)


        // CurrentSurface=SourceGraphic will be true if no filter has yet produced anything.
        if (RenderData.CurrentSurface <> RenderData.SourceGraphic) then
        begin
          // UnnamedSurface should be nil, but release anything it points to just
          // in case the filter failed to do it.
          ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

          if FilterNode.IsReferenceTarget then
          begin
            RenderData.UnnamedSurface := nil; // Transfer ownership of the bitmap to NamedSurfaces[]
            RenderData.NamedSurfaces[Count] := RenderData.CurrentSurface;
            Inc(Count);
          end else
            // Transfer ownership of the bitmap to UnnamedSurface
            RenderData.UnnamedSurface := RenderData.CurrentSurface;
        end;
      end;

      if (RenderData.UnnamedSurface <> RenderData.CurrentSurface) then
        ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

      // 6. Blend final filtered result surface onto target canvas at full resolution
      if (RenderData.CurrentSurface <> nil) then
        BlendOffscreenSurface(ATarget, RenderData.CurrentSurface, bmNormal, RenderData.ROI.Left, RenderData.ROI.Top);

      ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

    finally
      for i := 0 to High(RenderData.NamedSurfaces) do
        ReleaseOffscreenBitmap(RenderData.NamedSurfaces[i]);
      ReleaseOffscreenBitmap(RenderData.SourceGraphic);
      ReleaseOffscreenBitmap(RenderData.SourceAlpha);
    end;

  finally
    FTransformation.Pop;
  end;
end;

//------------------------------------------------------------------------------
//
//      Data URI Parsing Helpers
//
//------------------------------------------------------------------------------
// Used by TSvgRenderer.RenderImageNode
//------------------------------------------------------------------------------
function UrlDecode(AStr: TValuePUtf8Char): UTF8String;
var
  Code: Integer;
  Count: integer;
begin
  SetLength(Result, AStr.Len);
  Count := 0;

  while (AStr.Len > 0) do
  begin
    Code := 0;
    case AStr.Text^ of
      '%':
        if (AStr.Len > 2) then
        begin
          AStr.Skip;
          // First digit
          case AStr.Text^ of
            '0'..'9': Code := Ord(AStr.Text^) - Ord('0');
            'a'..'f': Code := Ord(AStr.Text^) - Ord('a') + 10;
            'A'..'F': Code := Ord(AStr.Text^) - Ord('A') + 10;
          else
            Code := 0;
          end;
          AStr.Skip;
          // Second digit
          case AStr.Text^ of
            '0'..'9': Code := Code shl 4 + Ord(AStr.Text^) - Ord('0');
            'a'..'f': Code := Code shl 4 + Ord(AStr.Text^) - Ord('a') + 10;
            'A'..'F': Code := Code shl 4 + Ord(AStr.Text^) - Ord('A') + 10;
          else
            Code := 0;
          end;
        end;

      '+':
        Code := 32;

    else
      Code := Ord(AStr.Text^);
    end;

    if (Code <> 0) then
    begin
      Inc(Count);
      if (Count > Length(Result)) then
        SetLength(Result, Count * 2);
      Result[Count] := UTF8Char(Code);
    end;

    AStr.Skip;
  end;

  SetLength(Result, Count);
end;

procedure DecodeBase64ToStream(ABase64: TValuePUtf8Char; AStream: TStream);

  function DecodeChar(c: UTF8Char): Integer;
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

var
  b1, b2, b3: Byte;
  v1, v2, v3, v4: Integer;
  buf: array[0..2] of Byte;
begin
  while (ABase64.Len > 0) do
  begin
    v1 := -1;
    while (ABase64.Len > 0) and (v1 = -1) do
    begin
      v1 := DecodeChar(ABase64.Text^);
      ABase64.Skip;
    end;
    if (v1 < 0) then
      Break;

    v2 := -1;
    while (ABase64.Len > 0) and (v2 = -1) do
    begin
      v2 := DecodeChar(ABase64.Text^);
      ABase64.Skip;
    end;
    if (v2 < 0) then
      Break;

    v3 := -1;
    while (ABase64.Len > 0) and (v3 = -1) do
    begin
      v3 := DecodeChar(ABase64.Text^);
      ABase64.Skip;
    end;
    if (v3 < -1) then
      v3 := -2;

    v4 := -1;
    while (ABase64.Len > 0) and (v4 = -1) do
    begin
      v4 := DecodeChar(ABase64.Text^);
      ABase64.Skip;
    end;
    if (v4 < -1) then
      v4 := -2;

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
  isSvg: Boolean;
  Stream: TMemoryStream;
  SubDoc: TSvgDocumentNode;
  SubRenderer: TSvgRenderer;
  Bitmap: TBitmap32;
  AspectMat, TotalMat: TFloatMatrixHelper;
  DestBounds: TFloatRect;
  DestClip: TRect;
  SourceViewBox: TSvgViewBox;
  UTF8Bytes: UTF8String;
  hRef, MimeType: TValuePUtf8Char;
  s: string;
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

  if (AImageNode.Href = '') then
    Exit;

  // TODO : Replace stream with "pull" decode on-demand stream

  Stream := TMemoryStream.Create;
  try
    hRef := TValuePUtf8Char.FromString(AImageNode.Href);

    if hRef.StartsText('data:', True) then
    begin
      MimeType := hRef.Split(',', True);
      if (MimeType.Len = 0) then
        exit;

      isSvg := MimeType.ContainsText('image/svg+xml') or MimeType.ContainsText('image/svg');

      if MimeType.ContainsText(';base64') then
        DecodeBase64ToStream(hRef, Stream)
      else
      begin
        UTF8Bytes := UrlDecode(hRef);
        if Length(UTF8Bytes) > 0 then
          Stream.WriteBuffer(UTF8Bytes[1], Length(UTF8Bytes));
      end;
    end else
    if FAllowExternalImages then
    begin
      s := hRef.ToString;
      if not FileExists(s) then
        exit;

      Stream.LoadFromFile(s);
      isSvg := hRef.EndsText('.svg');
    end else
      exit;

    if (Stream.Size = 0) then
      Exit;
    Stream.Position := 0;

    // Check if content is SVG if not determined by MIME or file extension
    if (not isSvg) then
    begin
      hRef.Text := Stream.Memory;
      hRef.Len := Min(100, Stream.Size); // Limit the scan to something reasonable
      isSvg := hRef.ContainsText('<svg') or hRef.ContainsText('<?xml');
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

        FTransformation.Push;
        try
          ApplyMatrix(AspectMat.Matrix);

          SubRenderer := TSvgRenderer.Create(ATarget);
          try
            SubRenderer.AllowExternalImages := FAllowExternalImages;
            SubRenderer.Transformation.Clear(FTransformation.Matrix);
            SubRenderer.ViewportRect := FViewportRect;

            SubRenderer.RenderNode(ATarget, SubDoc);
          finally
            SubRenderer.Free;
          end;
        finally
          FTransformation.Pop;
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
        TotalMat := AspectMat * FTransformation.Matrix;

        FTransformation.Push;
        try
          FTransformation.Clear(TotalMat.Matrix);

          DestBounds := FTransformation.GetTransformedBounds(FloatRect(0, 0, Bitmap.Width, Bitmap.Height));
          DestClip := MakeRect(DestBounds, rrOutside);

          Bitmap.DrawMode := dmBlend;
          Bitmap.CombineMode := cmMerge;

          // Transform and render onto target
          Transform(ATarget, Bitmap, FTransformation, DestClip, True);
        finally
          FTransformation.Pop;
        end;
      finally
        Bitmap.Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TSvgRenderer.MapFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo);
var
  Parser: TValuePUtf8Char;
  Token: TValuePUtf8Char;
  PrimaryCandidate: TValuePUtf8Char;
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

  Parser := TValuePUtf8Char.FromString(AFontFamily);
  Parser.Trim;
  Parser.TrimEnd;

  // Strip surrounding quotes if present
  Parser.TrimQuotes;
  AFontInfo.FontFamily := '';
  PrimaryCandidate.Len := 0;

  while (Parser.Len > 0) and (AFontInfo.FontFamily = '') do
  begin

    // Extract first font family candidate if a comma-separated fallback list is provided
    Token := Parser.Split(',', True);
    Token.Trim;
    Token.TrimQuotes;

    if (Token.Len = 0) then
      continue;

    if (PrimaryCandidate.Len = 0) then
      PrimaryCandidate := Token;

    case Token.Len of
      0:
        AFontInfo.FontFamily := SvgSystemFonts.SansSerif;

      4:
        case Token.Text^ of
          's', 'S':
            if Token.CompareText('sans') then
              AFontInfo.FontFamily := SvgSystemFonts.SansSerif;

          'm', 'M':
            if Token.CompareText('mono') then
              AFontInfo.FontFamily := SvgSystemFonts.Monospace;
        end;

      5:
        case Token.Text^ of
          's', 'S':
            if Token.CompareText('serif') then
              AFontInfo.FontFamily := SvgSystemFonts.Serif;

          't', 'T':
            if Token.CompareText('times') then
              AFontInfo.FontFamily := SvgSystemFonts.Serif;
        end;

      7:
        case Token.Text^ of
          'c', 'C':
            if Token.CompareText('courier') then
              AFontInfo.FontFamily := SvgSystemFonts.Monospace;
        end;

      9:
        case Token.Text^ of
          'n', 'N':
            if Token.CompareText('noto sans') then
              AFontInfo.FontFamily := SvgSystemFonts.SansSerif;

          'm', 'M':
            if Token.CompareText('monospace') then
              AFontInfo.FontFamily := SvgSystemFonts.Monospace;
        end;

      10:
        case Token.Text[1] of
          's', 'A':
            if Token.CompareText('sans-serif') then
              AFontInfo.FontFamily := SvgSystemFonts.SansSerif;

          'y', 'Y':
            if Token.CompareText('system-ui') then
              AFontInfo.FontFamily := SvgSystemFonts.SansSerif;
        end;
    end;

  end;

  if (AFontInfo.FontFamily = '') then
  begin
    if (PrimaryCandidate.Len <> 0) then
      AFontInfo.FontFamily := PrimaryCandidate.ToUtf8
    else
      AFontInfo.FontFamily := SvgSystemFonts.SansSerif;
  end;

  Parser := TValuePUtf8Char.FromString(AWeightStr);
  Parser.Trim;
  Parser.TrimEnd;

  if (Parser.Len = 4) and (Parser.CompareText('bold') or Parser.Equal('700') or Parser.Equal('800') or Parser.Equal('900')) then
    Include(AFontInfo.Style, fsBold);

  Parser := TValuePUtf8Char.FromString(AStyleStr);
  Parser.Trim;
  Parser.TrimEnd;
  if Parser.CompareText('italic') or Parser.CompareText('oblique') then
    Include(AFontInfo.Style, fsItalic);

  AFontInfo.Size := ASize;
end;

procedure TSvgRenderer.RenderTextPathData(ATarget: TCustomBitmap32; const APathPoints: TArrayOfArrayOfFloatPoint; ANode: TSvgNode);

  function GetAccumulatedTextOpacity(Node: TSvgNode): Single;
  begin
    Result := 1.0;
    while (Node <> nil) and (Node is TSvgTextPositioningNode) do
    begin
      Result := Result * Node.Opacity;
      Node := Node.Parent;
    end;
  end;

var
  TransformedPts, StrokePts, DashedPts: TArrayOfArrayOfFloatPoint;
  StrokeWidth, MatScale, ScaledOffset: Single;
  ScaledDashArray: TArrayOfFloat;
  i, j, k: Integer;
  AccumulatedOpacity, FillOpacity, StrokeOpacity: Single;
  FillColor, StrokeColor: TSvgColor;
  HasFill, HasStroke: boolean;
begin
  if (Length(APathPoints) = 0) or (ATarget = nil) then
    Exit;

  AccumulatedOpacity := GetAccumulatedTextOpacity(ANode);
  FillOpacity := ANode.Fill.Opacity * AccumulatedOpacity;
  StrokeOpacity := ANode.Stroke.Opacity * AccumulatedOpacity;

  if (ANode.Fill.ResolvedPaintServer = nil) then
    FillColor := GetEffectiveColor(ANode, ANode.Fill.Color, tcFill)
  else
    FillColor := TSvgColor.Unset;
  HasFill := (ANode.Fill.ResolvedPaintServer <> nil) or ((FillColor.IsVisible) and (FillOpacity > 0));

  StrokeWidth := ANode.Stroke.Width.ToPixels(FViewportRect.Width);
  if (ANode.Stroke.ResolvedPaintServer = nil) and (StrokeWidth > 0) then
    StrokeColor := GetEffectiveColor(ANode, ANode.Stroke.Color, tcStroke)
  else
    StrokeColor := TSvgColor.Unset;
  HasStroke := (StrokeWidth > 0) and ((ANode.Stroke.ResolvedPaintServer <> nil) or ((StrokeColor.IsVisible) and (StrokeOpacity > 0)));

  if (not HasFill) and (not HasStroke) then
    exit;

  TransformedPts := GetTransformedPoints(APathPoints);

  // 1. Fill Rendering
  if (HasFill) then
    RenderPolyPolygon(ATarget, ANode.Fill.ResolvedPaintServer, TransformedPts, FillOpacity, FillColor, ANode.Fill.FillRule);

  // 2. Stroke Rendering
  if (HasStroke) then
  begin
    MatScale := GetMatrixScale(FTransformation.Matrix);
    StrokeWidth := StrokeWidth * MatScale;

    ScaledDashArray := nil;
    ScaledOffset := 0;
    if Length(ANode.Stroke.DashArray) > 0 then
    begin
      SetLength(ScaledDashArray, Length(ANode.Stroke.DashArray));
      for k := 0 to High(ANode.Stroke.DashArray) do
        ScaledDashArray[k] := ANode.Stroke.DashArray[k] * MatScale;
      ScaledOffset := ANode.Stroke.DashOffset * MatScale;
    end;

    StrokePts := nil;
    for i := 0 to High(TransformedPts) do
    begin
      if Length(ScaledDashArray) > 0 then
      begin
        DashedPts := BuildDashedLine(TransformedPts[i], ScaledDashArray, ScaledOffset, IsClosedContour(TransformedPts[i]));
        for j := 0 to High(DashedPts) do
          StrokePts := StrokePts + BuildPolyPolyLine([DashedPts[j]], False, StrokeWidth, ANode.Stroke.JoinStyle, ANode.Stroke.EndStyle, ANode.Stroke.MiterLimit);
      end else
        StrokePts := StrokePts + BuildPolyPolyLine([TransformedPts[i]], IsClosedContour(TransformedPts[i]), StrokeWidth, ANode.Stroke.JoinStyle, ANode.Stroke.EndStyle, ANode.Stroke.MiterLimit);
    end;

    RenderPolyPolygon(ATarget, ANode.Stroke.ResolvedPaintServer, StrokePts, StrokeOpacity, StrokeColor);
  end;
end;

procedure TSvgRenderer.RenderTextAreaNode(ATarget: TCustomBitmap32; ATextAreaNode: TSvgTextAreaNode);
var
  Text: UnicodeString;
  TextRect: TFloatRect;
  FontSizePx: Integer;
  FontInfo: TFontInfo;
  TextLayout: TTextLayout;
  Canvas: TCanvas32;
begin
  if (ATextAreaNode = nil) or ATextAreaNode.IsDisplayNone or (not ATextAreaNode.Visible) then
    Exit;

  if (ATarget = nil) then
    Exit;

  TextRect.Left := ATextAreaNode.X.ToPixels(FViewportRect.Width);
  TextRect.Top := ATextAreaNode.Y.ToPixels(FViewportRect.Height);
  TextRect.Right := TextRect.Left + ATextAreaNode.Width.ToPixels(FViewportRect.Width);
  TextRect.Bottom := TextRect.Top + ATextAreaNode.Height.ToPixels(FViewportRect.Height);

  if (TextRect.IsEmpty) then
    Exit;

  Text := string(ATextAreaNode.TextContent);
  if (Text = '') or (ATextAreaNode.Children.Count > 0) then
    Text := GetSubtreeText(ATextAreaNode);

  if (Text = '') then
    Exit;

  Canvas := TCanvas32.Create(TBitmap32(FTarget));
  try
    Canvas.BeginLockUpdate;

    FontSizePx := Round(ATextAreaNode.FontSize.ToPixels(FViewportRect.Height));
    if FontSizePx <= 0 then
      FontSizePx := 12;

    FontInfo := Default(TFontInfo);
    MapFont(ATextAreaNode.FontFamily, ATextAreaNode.FontWeight, ATextAreaNode.FontStyle, FontSizePx, FontInfo);

    Canvas.Bitmap.Font.Name := string(FontInfo.FontFamily);
    Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
    Canvas.Bitmap.Font.Style := FontInfo.Style;

    TextLayout := DefaultTextLayout;
    TextLayout.ClipLayout := True;
    TextLayout.WordWrap := True;
    TextLayout.SingleLine := False;
    TextLayout.RemoveLeadingSpace := True;

    case ATextAreaNode.TextAlign of
      taHorLeft:
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;

      taHorRight:
        TextLayout.AlignmentHorizontal := TextAlignHorRight;

      taHorCenter:
        TextLayout.AlignmentHorizontal := TextAlignHorCenter;

      taHorJustify:
        TextLayout.AlignmentHorizontal := TextAlignHorJustify;
    else
      case ATextAreaNode.TextAnchor of
        taMiddle:
          TextLayout.AlignmentHorizontal := TextAlignHorCenter;

        taEnd:
          TextLayout.AlignmentHorizontal := TextAlignHorRight;
      else
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
      end;
    end;

    Canvas.Clear;
    Canvas.BeginUpdate;

    Canvas.RenderText(TextRect, Text, TextLayout);

    if (Canvas.Path <> nil) then
      RenderTextPathData(ATarget, Canvas.Path, ATextAreaNode);

    Canvas.Clear;
    Canvas.EndUpdate;

    Canvas.EndLockUpdate;
  finally
    Canvas.Free;
  end;
end;

procedure TSvgRenderer.RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);

  procedure RenderTextPathSubtree(Canvas: TCanvas32; ParentNode, CurrentNode: TSvgTextPositioningNode; const PathPts: TArrayOfArrayOfFloatPoint; var ActiveDy: Single; TotalLen: Single; var CurrentDistance: Single);
  var
    Child: TSvgNode;
    Text, CharString: UnicodeString;
    CharWidth, TangAngle, DrawY, RadAngle, NormX, NormY, BaseX, BaseY: Single;
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
    HasSubtreeTransform: Boolean;
  begin
    if (CurrentNode = nil) or CurrentNode.IsDisplayNone or (not CurrentNode.Visible) then
      Exit;

    HasSubtreeTransform := (CurrentNode <> ParentNode) and (not IsIdentityMatrix(CurrentNode.Transform));
    if HasSubtreeTransform then
    begin
      FTransformation.Push;
      ApplyMatrix(CurrentNode.Transform);
    end;
    try
      if CurrentNode.HasDx then
        CurrentDistance := CurrentDistance + CurrentNode.Dx.ToPixels(TotalLen);

      if CurrentNode.HasDy then
        ActiveDy := ActiveDy + CurrentNode.Dy.ToPixels(FViewportRect.Height);

      Text := CurrentNode.TextContent;
      if (Text <> '') then
      begin
        FontSizePx := Round(CurrentNode.FontSize.ToPixels(FViewportRect.Height));
        if FontSizePx <= 0 then
          FontSizePx := 12;

        FontInfo := Default(TFontInfo);
        MapFont(CurrentNode.FontFamily, CurrentNode.FontWeight, CurrentNode.FontStyle, FontSizePx, FontInfo);

        Canvas.Bitmap.Font.Name := string(FontInfo.FontFamily);
        Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Bitmap.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;

        FontFace := TFontFace32.Create(Canvas.Bitmap.Font.Handle);
        try
          FontFace.GetFontFaceMetrics(TextLayout, FontFaceMetrics);
          DrawY := -FontFaceMetrics.Ascent;
        finally
          FontFace := nil;
        end;

        if (Length(CurrentNode.Rotate) = 0) and (CurrentNode.Parent <> nil) and (CurrentNode.Parent is TSvgTextPositioningNode) then
          RotateArray := TSvgTextPositioningNode(CurrentNode.Parent).Rotate
        else
          RotateArray := CurrentNode.Rotate;

        ZeroWidth := NaN;
        SetLength(CharString, 2);
        CharString[2] := Char(ZERO_WIDTH_SPACE);

        CharacterRotationAngle := 0;
        for GlyphIdx := 1 to Length(Text) do
        begin
          if GlyphIdx - 1 <= High(RotateArray) then
            CharacterRotationAngle := RotateArray[GlyphIdx - 1];

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

            if GetPointAndTangentAtDistance(PathPts, CurrentDistance + CharWidth * 0.5, Point, TangAngle) then
            begin
              BaseX := Point.X;
              BaseY := Point.Y;

              if (ActiveDy <> 0) then
              begin
                RadAngle := DegToRad(TangAngle);
                GR32_Math.SinCos(RadAngle, NormX, NormY);
                // Normal vector (-sin(Angle), cos(Angle)) perpendicular to path tangent
                BaseX := BaseX - ActiveDy * NormX;
                BaseY := BaseY + ActiveDy * NormY;
              end;

              RotationMat.Matrix := IdentityMatrix;
              RotationMat.Rotate(TangAngle + CharacterRotationAngle);
              RotationMat.Translate(BaseX, BaseY);

              FTransformation.Push;
              try
                ApplyMatrix(RotationMat.Matrix);
                Canvas.Clear;
                Canvas.BeginUpdate;

                Canvas.RenderText(-CharWidth * 0.5, DrawY, Text[GlyphIdx], TextLayout);
                if (Canvas.Path <> nil) then
                  RenderTextPathData(ATarget, Canvas.Path, CurrentNode);

                Canvas.Clear;
                Canvas.EndUpdate;
              finally
                FTransformation.Pop;
              end;
            end;
          end;

          CurrentDistance := CurrentDistance + CharWidth;
        end;
      end;

      for Child in CurrentNode.Children do
      begin
        if Child is TSvgTSpanNode then
          RenderTextPathSubtree(Canvas, ParentNode, TSvgTextPositioningNode(Child), PathPts, ActiveDy, TotalLen, CurrentDistance)
        else
        if Child is TSvgTextPositioningNode then
          RenderTextPathSubtree(Canvas, ParentNode, TSvgTextPositioningNode(Child), PathPts, ActiveDy, TotalLen, CurrentDistance);
      end;
    finally
      if HasSubtreeTransform then
        FTransformation.Pop;
    end;
  end;

  // Evaluates path arc length and places glyphs at interpolated path distance points
  // aligned to segment tangent orientation angles.
  procedure ProcessTextPathNode(ANode: TSvgTextPathNode; Canvas: TCanvas32);
  var
    PathPts: TArrayOfArrayOfFloatPoint;
    TotalLen, Offset, CurrentDistance: Single;
    HasNodeTransform: Boolean;
  var
    FullText: UnicodeString;
    TextLayout: TTextLayout;
    MeasureRect: TFloatRect;
    TextWidth, ActiveDy: Single;
  begin
    if (ANode = nil) or ANode.IsDisplayNone or (not ANode.Visible) then
      Exit;

    if (ANode.ResolvedPathNode = nil) then
      Exit;

    HasNodeTransform := not IsIdentityMatrix(ANode.Transform);

    if HasNodeTransform then
      FTransformation.Push;
    try
      if HasNodeTransform then
        ApplyMatrix(ANode.Transform);

      PathPts := ANode.ResolvedPathNode.GetPathData(FViewportRect.Width, FViewportRect.Height);
      if Length(PathPts) = 0 then
        Exit;

      if not IsIdentityMatrix(ANode.ResolvedPathNode.Transform) then
      begin
        // Avoid mutating the node's internal PathData
        if (PathPts <> ANode.ResolvedPathNode.PathData) then
          TransformPathPointsInplace(PathPts, ANode.ResolvedPathNode.Transform)
        else
          PathPts := TransformPathPoints(PathPts, ANode.ResolvedPathNode.Transform);
      end;

      TotalLen := GetTotalPathLength(PathPts);
      if TotalLen <= 0 then
        Exit;

      Offset := ANode.StartOffset.ToPixels(TotalLen);

      if (ANode.TextAnchor <> taStart) then
      begin
        FullText := GetSubtreeText(ANode);

        if (FullText <> '') then
        begin
          TextLayout := DefaultTextLayout;
          TextLayout.ClipLayout := False;
          TextLayout.AlignmentHorizontal := TextAlignHorLeft;
          TextLayout.AlignmentVertical := TextAlignVerTop;

          if (ANode.TextAnchor in [taMiddle, taEnd]) then
          begin
            MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, FullText, TextLayout);

            TextWidth := MeasureRect.Width;

            case ANode.TextAnchor of
              taMiddle: Offset := Offset - TextWidth * 0.5;
              taEnd: Offset := Offset - TextWidth;
            end;
          end;
        end;
      end;

      CurrentDistance := Offset;
      ActiveDy := 0;
      RenderTextPathSubtree(Canvas, ANode, ANode, PathPts, ActiveDy, TotalLen, CurrentDistance);
    finally
      if HasNodeTransform then
        FTransformation.Pop;
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

    HasNodeTransform := (ANode <> ATextNode) and (not IsIdentityMatrix(ANode.Transform));

    if HasNodeTransform then
      FTransformation.Push;
    try
      if HasNodeTransform then
        ApplyMatrix(ANode.Transform);

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

        Canvas.Bitmap.Font.Name := string(FontInfo.FontFamily);
        Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Bitmap.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;
        TextLayout.RemoveLeadingSpace := True;

        MeasureRect := Canvas.MeasureText(Canvas.Bitmap.BoundsRect, ANode.TextContent + Char(ZERO_WIDTH_SPACE), TextLayout);
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

              FTransformation.Push;
              try
                ApplyMatrix(RotationMat.Matrix);
                Canvas.Clear;
                Canvas.BeginUpdate;

                Canvas.RenderText(DrawPoint.X, DrawPoint.Y, CharString, TextLayout);
                if (Canvas.Path <> nil) then
                  RenderTextPathData(ATarget, Canvas.Path, ANode);

                Canvas.Clear;
                Canvas.EndUpdate;
              finally
                FTransformation.Pop;
              end;

            end;

            DrawPoint.X := DrawPoint.X + CharWidth;
          end;
        end else
        begin
          Canvas.Clear;
          Canvas.BeginUpdate;

          Canvas.RenderText(DrawPoint.X, DrawPoint.Y, ANode.TextContent + Char(ZERO_WIDTH_SPACE), TextLayout);

          if (Canvas.Path <> nil) then
            RenderTextPathData(ATarget, Canvas.Path, ANode);

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
        FTransformation.Pop;
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
  if ANode is TSvgTextAreaNode then
    RenderTextAreaNode(ATarget, TSvgTextAreaNode(ANode))
  else
  if ANode is TSvgTextNode then
    RenderTextNode(ATarget, TSvgTextNode(ANode))
  else
  if ANode is TSvgGroupNode then
    RenderGroupNode(ATarget, TSvgGroupNode(ANode));
end;

procedure TSvgRenderer.RenderNodeUnfiltered(ATarget: TCustomBitmap32; ANode: TSvgNode);
begin
  FTransformation.Push;
  try
    ApplyMatrix(ANode.Transform);
    RenderNodeContent(ATarget, ANode);
  finally
    FTransformation.Pop;
  end;
end;

procedure TSvgRenderer.RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode);
var
  OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  EffectiveBlendMode: TSvgBlendMode;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
  NodeBounds, TargetWorldBounds: TFloatRect;
  NodeRoi: TRect;
  Points: array[0..3] of TFloatPoint;
  k: Integer;
  NeedsOffscreen: Boolean;
{$if not defined(USE_SIMD_MASK_FILTERS)}
  SourceP, DestP: PColor32;
  x: Integer;
  AlphaVal, Gray: Byte;
{$ifend}
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
      FTransformation.Push;
      try
        ApplyMatrix(ANode.Transform);

        // Calculate object bounding box in world space
        NodeBounds := ANode.GetObjectBoundingBox;
        if (NodeBounds.Right <= NodeBounds.Left) or (NodeBounds.Bottom <= NodeBounds.Top) then
        begin
          // Fallback for empty or degenerate group bounding boxes;
          // If GetObjectBoundingBox returns empty/inverted bounds,
          // we fall back to the target canvas/viewport dimensions,
          // ensuring offscreen surfaces are allocated properly
          // rather than disappearing.
          // See identical logic in TSvgRenderer.RenderGroupNode & TSvgRenderer.RenderFilter
          TargetWorldBounds := FloatRect(0, 0, ATarget.Width, ATarget.Height);
        end else
        begin
          Points[0] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Top));
          Points[1] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Top));
          Points[2] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Bottom));
          Points[3] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Bottom));

          TargetWorldBounds := FloatRect(Points[0].X, Points[0].Y, Points[0].X, Points[0].Y);
          for k := 1 to 3 do
          begin
            if (Points[k].X < TargetWorldBounds.Left) then TargetWorldBounds.Left := Points[k].X;
            if (Points[k].X > TargetWorldBounds.Right) then TargetWorldBounds.Right := Points[k].X;
            if (Points[k].Y < TargetWorldBounds.Top) then TargetWorldBounds.Top := Points[k].Y;
            if (Points[k].Y > TargetWorldBounds.Bottom) then TargetWorldBounds.Bottom := Points[k].Y;
          end;
        end;

        NodeRoi := MakeRect(TargetWorldBounds, rrOutside);
        if not GR32.IntersectRect(NodeRoi, NodeRoi, ATarget.BoundsRect) then
          Exit;

        OffscreenBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, True);
        ClipMaskBmp := nil;
        MaskBmp := nil;
        try
          FTransformation.Push;
          try
            FTransformation.Translate(-NodeRoi.Left, -NodeRoi.Top);

            RenderNodeContent(OffscreenBmp, ANode);
          finally
            FTransformation.Pop;
          end;

          // Apply ClipPath
          if (ANode.ResolvedClipPath <> nil) then
          begin
            ClipNodeTarget := ANode.ResolvedClipPath;
            ClipMaskBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, True);

            FTransformation.Push;
            try
              FTransformation.Translate(-NodeRoi.Left, -NodeRoi.Top);

              RenderClipPathNode(ClipMaskBmp, ClipNodeTarget, TargetWorldBounds, NodeRoi);
            finally
              FTransformation.Pop;
            end;

{$if defined(USE_SIMD_MASK_FILTERS)}
            ScaleAlphaLine(PColor32(ClipMaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
            DestP := PColor32(OffscreenBmp.Bits);
            SourceP := PColor32(ClipMaskBmp.Bits);

            for x := 0 to OffscreenBmp.PixelCount - 1 do
            begin
              AlphaVal := AlphaComponent(SourceP^);
              if AlphaVal < 255 then
                ScaleAlpha(DestP^, AlphaVal * OneOver255);
              Inc(DestP);
              Inc(SourceP);
            end;
{$ifend}
          end;

          // Apply Alpha Mask
          if (ANode.ResolvedMask <> nil) then
          begin
            MaskNodeTarget := ANode.ResolvedMask;
            MaskBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, False);

            FTransformation.Push;
            try
              FTransformation.Translate(-NodeRoi.Left, -NodeRoi.Top);

              RenderMaskNode(MaskBmp, MaskNodeTarget, TargetWorldBounds, NodeRoi);
            finally
              FTransformation.Pop;
            end;

{$if defined(USE_SIMD_MASK_FILTERS)}
            ApplyAlphaMask(PColor32(MaskBmp.Bits), PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount);
{$else}
            SourceP := PColor32(MaskBmp.Bits);
            DestP := PColor32(OffscreenBmp.Bits);

            for x := 0 to OffscreenBmp.PixelCount - 1 do
            begin
              Gray := Intensity(SourceP^);
              Gray := Round(Gray * (AlphaComponent(SourceP^) * OneOver255));
              ScaleAlpha(DestP^, Gray * OneOver255);
              Inc(DestP);
              Inc(SourceP);
            end;
{$ifend}
          end;

          // Apply Opacity
          if (ANode.Opacity < 1.0) then
          begin
{$if defined(USE_SIMD_MASK_FILTERS)}
            ScaleAlphaMems(PColor32(OffscreenBmp.Bits), OffscreenBmp.PixelCount, ANode.Opacity);
{$else}
            SourceP := PColor32(OffscreenBmp.Bits);
            for x := 0 to OffscreenBmp.PixelCount - 1 do
            begin
              ScaleAlpha(SourceP^, ANode.Opacity);
              Inc(SourceP);
            end;
{$ifend}
          end;

          BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, NodeRoi.Left, NodeRoi.Top);
        finally
          ReleaseOffscreenBitmap(OffscreenBmp);
          ReleaseOffscreenBitmap(ClipMaskBmp);
          ReleaseOffscreenBitmap(MaskBmp);
        end;
      finally
        FTransformation.Pop;
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

  FTransformation.Clear;

  FTransformation.Push;
  try
    ApplyMatrix(vpMat);
    RenderNode(ADoc);
  finally
    FTransformation.Pop;
  end;
end;

procedure TSvgRenderer.RenderDocument(ADoc: TSvgDocumentNode);
begin
  if FTarget <> nil then
    RenderDocument(ADoc, FloatRect(0, 0, FTarget.Width, FTarget.Height));
end;

end.
