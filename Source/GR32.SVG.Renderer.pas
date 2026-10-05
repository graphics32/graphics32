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
  TMapPool<T: TCustomMap> = class abstract(TObject)
  private
    FPool: TObjectList<T>;
    FMaxSize: Int64;
    FPoolSize: Int64;
  protected
    function CreateNewMap: T; virtual; abstract;
    procedure PrepareMap(Map: T); virtual;
  public
    constructor Create(AMaxSize: Int64 = 0);
    destructor Destroy; override;
    function Acquire(AWidth, AHeight: Integer; AClear: Boolean = True): T;
    procedure Release(Map: T); virtual;
    procedure Clear;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
    property PoolSize: Int64 read FPoolSize;
  end;

  TSvgBitmapPool = class(TMapPool<TCustomBitmap32>)
  private
    FBitmapMaxOversize: NativeInt;
  protected
    function CreateNewMap: TCustomBitmap32; override;
    procedure PrepareMap(Map: TCustomBitmap32); override;
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
    FontFamily: AnsiString;
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
    function GetTransformation: TAffineTransformation;
  protected
    function CanRenderPolyPolygon(APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint; AOpacity: Single; AColor: TSvgColor): boolean;
    procedure RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint; AOpacity: Single; AColor: TSvgColor; AFillMode: TPolyFillMode = pfWinding);

    procedure RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
    procedure RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
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
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode; AX: Integer; AY: Integer); overload;
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode); overload;
    function GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
    function GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
    function CreateGradientFiller(AGradNode: TSvgGradientNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
    function CreatePatternFiller(APatternNode: TSvgPatternNode; const ABounds: TFloatRect; AOpacity: Single = 1.0): TCustomPolygonFiller;
    function GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure ReleaseOffscreenBitmap(var ABitmap: TCustomBitmap32);
    procedure MapFont(const AFontFamily, AWeightStr, AStyleStr: AnsiString; ASize: integer; var AFontInfo: TFontInfo); virtual;
  public
    constructor Create(ATarget: TCustomBitmap32 = nil); virtual;
    destructor Destroy; override;

    procedure ApplyMatrix(const AMatrix: TFloatMatrix);

    procedure RenderDocument(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect); overload;
    procedure RenderDocument(ADoc: TSvgDocumentNode); overload;
    procedure RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode); overload;
    procedure RenderNode(ANode: TSvgNode); overload;

    property Target: TCustomBitmap32 read FTarget write FTarget;
    property Transformation: TAffineTransformation read GetTransformation;
    property ViewportRect: TFloatRect read FViewportRect write FViewportRect;
    property AllowExternalImages: Boolean read FAllowExternalImages write FAllowExternalImages;
  end;

//------------------------------------------------------------------------------

var
  SvgSystemFonts: record
    SansSerif: AnsiString;
    Serif: AnsiString;
    Monospace: AnsiString;
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
  GR32.SVG.Utf8,
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
  GR32.Blend.Modes.PhotoShop;

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

function GetSubtreeText(ANode: TSvgNode): string;
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

function GetEffectiveColor(ANode: TSvgNode; AColor: TSvgColor): TSvgColor;
begin
  if AColor.IsCurrentColor then
  begin
    if ANode <> nil then
      Result := ANode.Color
    else
      Result := TSvgColor.Create(clBlack32);
  end else
    Result := AColor;
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

constructor TMapPool<T>.Create(AMaxSize: Int64);
begin
  inherited Create;
  FPool := TObjectList<T>.Create(True);
  FMaxSize := AMaxSize;
end;

destructor TMapPool<T>.Destroy;
begin
  FPool.Free;
  inherited Destroy;
end;

function TMapPool<T>.Acquire(AWidth, AHeight: Integer; AClear: Boolean): T;
var
  i, BestIndex: Integer;
  TargetSize, CandidateSize, Delta, BestDelta: Integer;
  Candidate: T;
begin
  Result := nil;
  TargetSize := AWidth * AHeight;
  BestDelta := MaxInt;
  BestIndex := -1;

  // Search pool for Candidate surface with existing buffer dimensions sufficient
  // for requested bounds (Width >= AWidth, Height >= AHeight) to avoid costly
  // buffer reallocations. Pick Candidate with smallest excess area (BestDelta).
  // Scan from most recent (most likely to match size) to least recent.
  // TODO : Binary search
  for i := FPool.Count - 1 downto 0 do
  begin
    Candidate := FPool[i];

    CandidateSize := Candidate.ByteCount;
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
    // Instantiate a new map if no Candidate with sufficient buffer dimensions
    // exists in pool
    Result := CreateNewMap;
    Result.SetSize(AWidth, AHeight, AClear);
  end else
  begin
    FPool.ExtractAt(BestIndex);
    Dec(FPoolSize, Result.ByteCount);

    Result.SetSize(AWidth, AHeight, AClear);
{$ifdef DEBUG}
    Result.EndLockUpdate; // For debug: Signal that bitmap is out of pool
{$endif DEBUG}
  end;

  PrepareMap(Result);
end;

procedure TMapPool<T>.Clear;
begin
  FPool.Clear;
end;

procedure TMapPool<T>.PrepareMap(Map: T);
begin
end;

procedure TMapPool<T>.Release(Map: T);
begin
  if Map = nil then
    exit;

  if (FMaxSize > 0) and (FPoolSize + Map.ByteCount > FMaxSize) then
  begin
    Map.Free;
    exit;
  end;

  FPool.Add(Map);
{$ifdef DEBUG}
  Map.BeginLockUpdate; // For debug: Signal that bitmap is in pool
{$endif DEBUG}
  Inc(FPoolSize, Map.ByteCount);
end;

{ TSvgBitmapPool }

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
    Map.OnPixelCombine := nil;
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
  FBitmapPool.BitmapMaxOversize := 1024; // Magic!
  FBitmapPool.MaxSize := 256*1024*1024; // More magic!
  FAllowExternalImages := False;
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
      // Transform gradient coordinates in normalized [0..1] space before mapping to bounding box
      TotalTransform := GradTransform * BboxMat;
    end else
    begin
      PointStart.X := LinearNode.X1.ToPixels(FViewportRect.Width);
      PointStart.Y := LinearNode.Y1.ToPixels(FViewportRect.Height);
      PointEnd.X := LinearNode.X2.ToPixels(FViewportRect.Width);
      PointEnd.Y := LinearNode.Y2.ToPixels(FViewportRect.Height);
      TotalTransform := GradTransform * FTransformation.Matrix;
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
      // Transform gradient coordinates in normalized [0..1] space before mapping to bounding box
      TotalTransform := GradTransform * BboxMat;
    end else
    begin
      cx := RadialNode.Cx.ToPixels(FViewportRect.Width);
      cy := RadialNode.Cy.ToPixels(FViewportRect.Height);
      r := RadialNode.R.ToPixels(GR32_Math.Hypot(FViewportRect.Width, FViewportRect.Height) * Sqrt(0.5));
      fx := RadialNode.Fx.ToPixels(FViewportRect.Width);
      fy := RadialNode.Fy.ToPixels(FViewportRect.Height);
      TotalTransform := GradTransform * FTransformation.Matrix;
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

  PatternTransform.Matrix := APatternNode.PatternTransform;
  TotalTransform := PatternTransform * FTransformation.Matrix;

  // Calculates pattern tile origin and bounds in user space.
  // When patternUnits = guObjectBoundingBox (default), tile attributes x, y, width, height
  // are defined in normalized bounding box units [0..1] relative to target bounds in user space.
  if APatternNode.PatternUnits = guObjectBoundingBox then
  begin
    InvMat := FTransformation.Matrix;
    GR32_Transforms.Invert(InvMat);

    origPt := TFloatMatrixHelper(InvMat).TransformPoint(FloatPoint(ABounds.Left, ABounds.Top));

    MatrixScaleX := GR32_Math.Hypot(FTransformation.Matrix[0, 0], FTransformation.Matrix[0, 1]);
    MatrixScaleY := GR32_Math.Hypot(FTransformation.Matrix[1, 0], FTransformation.Matrix[1, 1]);
    if MatrixScaleX <= 0 then MatrixScaleX := 1.0;
    if MatrixScaleY <= 0 then MatrixScaleY := 1.0;

    TileRatioX := BoundsWidth / MatrixScaleX;
    TileRatioY := BoundsHeight / MatrixScaleY;
    TileWidth := APatternNode.Width.ToPixels(1.0) * TileRatioX;
    TileHeight := APatternNode.Height.ToPixels(1.0) * TileRatioY;
    TileX := origPt.X + APatternNode.X.ToPixels(1.0) * TileRatioX;
    TileY := origPt.Y + APatternNode.Y.ToPixels(1.0) * TileRatioY;
  end else
  begin
    TileX := APatternNode.X.ToPixels(ViewportWidth);
    TileY := APatternNode.Y.ToPixels(ViewportHeight);
    TileWidth := APatternNode.Width.ToPixels(ViewportWidth);
    TileHeight := APatternNode.Height.ToPixels(ViewportHeight);
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

    FTransformation.Push;
    try
      SavedViewport := FViewportRect;

      FTransformation.Clear;
      FViewportRect := FloatRect(0, 0, BitmapWidth, BitmapHeight);

      // Sets up ContentMat to render pattern child geometry onto offscreen tile bitmap
      ContentMat.Matrix := IdentityMatrix;
      if APatternNode.ViewBox.IsValid then
      begin
        TileViewBox := APatternNode.ViewBox;
        ContentMat.Matrix := TileViewBox.GetTransform(FloatRect(0, 0, BitmapWidth, BitmapHeight), APatternNode.PreserveAspectRatio);
      end
      else
      if APatternNode.PatternContentUnits = guObjectBoundingBox then
      begin
        TileRatioX := BitmapWidth / TileWidth;
        TileRatioY := BitmapHeight / TileHeight;
        ContentMat.Scale(BoundsWidth * TileRatioX, BoundsHeight * TileRatioY);
        ContentMat.Translate(-TileX * TileRatioX, -TileY * TileRatioY);
      end
      else
      begin
        TileRatioX := BitmapWidth / TileWidth;
        TileRatioY := BitmapHeight / TileHeight;
        ContentMat.Scale(TileRatioX, TileRatioY);
        ContentMat.Translate(-TileX * TileRatioX, -TileY * TileRatioY);
      end;

      ApplyMatrix(ContentMat.Matrix);

      for i := 0 to APatternNode.Children.Count - 1 do
        RenderNode(PatternBitmap, APatternNode.Children[i]);

    finally
      FTransformation.Pop;
      FViewportRect := SavedViewport;
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
    end
    else
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

function TSvgRenderer.CanRenderPolyPolygon(APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint; AOpacity: Single; AColor: TSvgColor): boolean;
begin
  Result := (APoints <> nil) and
    ((APaintServer <> nil) or
     ((not AColor.IsNone) and (Round(AlphaComponent(AColor.Color) * AOpacity) > 0)));
end;

procedure TSvgRenderer.RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TObject; const APoints: TArrayOfArrayOfFloatPoint;
  AOpacity: Single; AColor: TSvgColor; AFillMode: TPolyFillMode);
var
  PaintServerNode: TSvgNode;
  Bounds: TFloatRect;
  Filler: TCustomPolygonFiller;
  Color: TColor32;
begin
  if (APoints = nil) then
    exit;

  if (APaintServer <> nil) then
  begin
    Bounds := GetPathBounds(APoints);

    PaintServerNode := TSvgNode(APaintServer);

    Filler := nil;
    if PaintServerNode is TSvgGradientNode then
      Filler := CreateGradientFiller(TSvgGradientNode(PaintServerNode), Bounds, AOpacity)
    else
    if PaintServerNode is TSvgPatternNode then
      Filler := CreatePatternFiller(TSvgPatternNode(PaintServerNode), Bounds, AOpacity);

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
  if (not AColor.IsNone) then
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
  StrokeWidth, MatScale, ScaledOffset: Single;
  Points, StrokePoints, AllRenderPoints: TArrayOfArrayOfFloatPoint;
  ScaledDashArray: TArrayOfFloat;
  i, j: Integer;
  FloatRoi: TFloatRect;
  RoiRect: TRect;
  NeedsOffscreen: Boolean;
  EffectiveBlendMode: TSvgBlendMode;
  RenderBmp, OffscreenBmp, ClipMaskBmp, MaskBmp: TCustomBitmap32;
  ClipNodeTarget: TSvgClipPathNode;
  MaskNodeTarget: TSvgMaskNode;
{$if not defined(USE_SIMD_MASK_FILTERS)}
  SourceP, DestP: PColor32;
  x: Integer;
  AlphaVal, Gray: Byte;
{$ifend}
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
  if (StrokeWidth > 0) and ((APathNode.Stroke.ResolvedPaintServer <> nil) or (not GetEffectiveColor(APathNode, APathNode.Stroke.Color).IsNone)) then
  begin
    MatScale := GetMatrixScale(FTransformation.Matrix);
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
  if (APathNode.Fill.ResolvedPaintServer <> nil) or (not GetEffectiveColor(APathNode, APathNode.Fill.Color).IsNone) then
    AllRenderPoints := AllRenderPoints + TransformedPoints;
  if Length(StrokePoints) > 0 then
    AllRenderPoints := AllRenderPoints + StrokePoints;

  if Length(AllRenderPoints) = 0 then
    AllRenderPoints := TransformedPoints;

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
    if NeedsOffscreen then
    begin
      FTransformation.Push;
      FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);
    end;
    try
      // 5. Fill Rendering
      RenderPolyPolygon(RenderBmp, APathNode.Fill.ResolvedPaintServer, TransformedPoints, APathNode.Fill.Opacity, GetEffectiveColor(APathNode, APathNode.Fill.Color), APathNode.Fill.FillRule);

      // 6. Stroke Rendering
      RenderPolyPolygon(RenderBmp, APathNode.Stroke.ResolvedPaintServer, StrokePoints, APathNode.Stroke.Opacity, GetEffectiveColor(APathNode, APathNode.Stroke.Color));
    finally
      if NeedsOffscreen then
        FTransformation.Pop;
    end;

    // 7. Markers Rendering (per SVG specification, markers paint on top of fill and stroke)
    if (APathNode.ResolvedMarkerStart <> nil) or (APathNode.ResolvedMarkerMid <> nil) or (APathNode.ResolvedMarkerEnd <> nil) then
    begin
      StrokeWidth := APathNode.Stroke.Width.ToPixels(FViewportRect.Width);
      if NeedsOffscreen then
      begin
        FTransformation.Push;
        try
          FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);

          RenderMarkers(RenderBmp, APathNode, PathPoints, StrokeWidth);
        finally
          FTransformation.Pop;
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
//
//      TFilterRenderer
//
//------------------------------------------------------------------------------
// Abstract base class for filter renderers
//------------------------------------------------------------------------------
type
  TNamedSurfaces = TArray<TCustomBitmap32>;

  TFilterRenderData = record
    SourceGraphic: TCustomBitmap32;
    SourceAlpha: TCustomBitmap32;
    CurrentSurface: TCustomBitmap32;
    UnnamedSurface: TCustomBitmap32;
    ROI: TRect;
    Scale: Single;
    NamedSurfaces: TNamedSurfaces;
  end;

  TFilterRenderer = class abstract
  protected
    class function ResolveSurface(const AInput: TSvgFilterInput; const RenderData: TFilterRenderData; DefaultFallback: TCustomBitmap32 = nil): TCustomBitmap32;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; virtual;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); virtual; abstract;
  end;

class function TFilterRenderer.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
begin
  Result := Default(TFloatRect);
end;

class function TFilterRenderer.ResolveSurface(const AInput: TSvgFilterInput; const RenderData: TFilterRenderData; DefaultFallback: TCustomBitmap32): TCustomBitmap32;
begin
  case AInput.Kind of
    fikSourceGraphic:
      Result := RenderData.SourceGraphic;

    fikSourceAlpha:
      Result := RenderData.SourceAlpha;

    fikNamedResult:
      if (AInput.Index >= 0) and (AInput.Index <= High(RenderData.NamedSurfaces)) and (RenderData.NamedSurfaces[AInput.Index] <> nil) then
        Result := RenderData.NamedSurfaces[AInput.Index]
      else
        Result := nil;

    fikPreviousResult:
      Result := RenderData.CurrentSurface;
  else
    Result := nil;
  end;

  if (Result = nil) then
  begin
    if (DefaultFallback <> nil) then
      Result := DefaultFallback
    else
      Result := RenderData.SourceGraphic;
  end;
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererGaussianBlur
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeGaussianBlurNode
//------------------------------------------------------------------------------
type
  TFilterRendererGaussianBlur = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;

    class procedure ApplyBlur(Renderer: TSvgRenderer; Input, Output: TCustomBitmap32; RadiusX, RadiusY: Single); static;
  end;

class function TFilterRendererGaussianBlur.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
begin
  Result.Left := TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * Scale + 2;
  Result.Top := TSvgFeGaussianBlurNode(Node).StdDeviationY * GaussianSigmaToRadius * Scale + 2;
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
end;

class procedure TFilterRendererGaussianBlur.ApplyBlur(Renderer: TSvgRenderer; Input, Output: TCustomBitmap32; RadiusX, RadiusY: Single);
var
  Temp, Transposed: TCustomBitmap32;
begin
  // Limit the blur radius to something reasonable. The blur function can handle
  // whatever we throw at it without crashing but the result might be junk due
  // to numeric overflows.
  If (RadiusX > 100) then
    RadiusX := 100;
  If (RadiusY > 100) then
    RadiusY := 100;

  // Apply blur based on horizontal and vertical radii (RadiusX and RadiusY).
  // If only RadiusX or RadiusY is specified, perform 1D horizontal or vertical blur accordingly,
  // avoiding unwanted blur and edge clipping along the un-blurred dimension.
  if (RadiusX < Blur32MinRadius) and (RadiusY < Blur32MinRadius) then
    // No blur
    Input.CopyMapTo(Output)
  else
  if (RadiusY < Blur32MinRadius) then
    // Horizontal blur
    FastHorizontalAlphaBlur32(Input, Output, RadiusX)
  else
  if (RadiusX < Blur32MinRadius) then
  begin
    // Vertical blur
    Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False); // Note: Width/Height swapped for transpose
    Temp := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
    try
      // Transpose: Input (W x H) -> Transposed (H x W)
      Transpose32(Input.Bits, Transposed.Bits, Input.Width, Input.Height);

      // Blur: Transposed (H x W) -> Temp (H x W)
      FastHorizontalAlphaBlur32(Transposed, Temp, RadiusY);

      // Transpose: Temp (H x W) -> Output (W x H)
      Transpose32(Temp.Bits, Output.Bits, Input.Height, Input.Width);
    finally
      Renderer.ReleaseOffscreenBitmap(Transposed);
      Renderer.ReleaseOffscreenBitmap(Temp);
    end;
  end else
  if Abs(RadiusX - RadiusY) < 1e-4 then
    // Isotropic 2D blur
    FastAlphaBlur32(Input, Output, RadiusX)
  else
  begin
    // Anisotropic 2D blur
    Temp := Renderer.GetOffscreenBitmap(Input.Width, Input.Height, False);
    try
      // TODO : Premultiply->HorBlur->Transpose->HorBlur->Transpose->Unpremultiply; Might save a bit - might not

      // Blur: Input (W x H) -> Temp (W x H)
      FastHorizontalAlphaBlur32(Input, Temp, RadiusX);

      Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False); // Note: Width/Height swapped for transpose
      try
        // Transpose: Temp (W x H) -> Transposed (H x W)
        Transpose32(Temp.Bits, Transposed.Bits, Input.Width, Input.Height);

        // Blur: Transposed (H x W) -> Temp (H x W)
        // - Blur will call SetSize on Temp but since the pixel count stays the
        //   same, no reallocation is actually done.
        // - Do not change this unless the SetSize behavior changes!
        FastHorizontalBlur32(Transposed, Temp, RadiusY);

        // Transpose: Temp (H x W) -> Output (W x H)
        Transpose32(Temp.Bits, Output.Bits, Input.Height, Input.Width);
      finally
        Renderer.ReleaseOffscreenBitmap(Transposed);
      end;
    finally
      Renderer.ReleaseOffscreenBitmap(Temp);
    end;
  end;
end;

class procedure TFilterRendererGaussianBlur.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TCustomBitmap32;
  RadiusX, RadiusY: Single;
begin
  Input := ResolveSurface(Node.ResolvedIn1, RenderData);

  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  RadiusX := TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * RenderData.Scale;
  RadiusY := TSvgFeGaussianBlurNode(Node).StdDeviationY * GaussianSigmaToRadius * RenderData.Scale;

  ApplyBlur(Renderer, Input, RenderData.CurrentSurface, RadiusX, RadiusY);
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererColorMatrix
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeColorMatrixNode
//------------------------------------------------------------------------------
type
  TFilterRendererColorMatrix = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererColorMatrix.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);

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
              Lum := Luminance601(pSource.ARGB);

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
            pDest.ARGB := Luminance709(pSource.ARGB);

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

var
  Input: TCustomBitmap32;
begin
  Input := ResolveSurface(TSvgFeColorMatrixNode(Node).ResolvedIn1, RenderData);
  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  ApplyColorMatrix(Input, RenderData.CurrentSurface, TSvgFeColorMatrixNode(Node).MatrixType, TSvgFeColorMatrixNode(Node).Values);
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererBlend
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeBlendNode
//------------------------------------------------------------------------------
type
  TFilterRendererBlend = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererBlend.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input1: TCustomBitmap32;
  Input2: TCustomBitmap32;
begin
  Input1 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn1, RenderData);
  Input2 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn2, RenderData, RenderData.SourceGraphic);
  if (Input1 <> RenderData.UnnamedSurface) and (Input2 <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  Input2.CopyMapTo(RenderData.CurrentSurface);
  Renderer.BlendOffscreenSurface(RenderData.CurrentSurface, Input1, TSvgFeBlendNode(Node).Mode);
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererComposite
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeCompositeNode
//------------------------------------------------------------------------------
type
  TFilterRendererComposite = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererComposite.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input1: TCustomBitmap32;
  Input2: TCustomBitmap32;

type
  TPixelCombiner = function(F: TColor32; B: TColor32): TColor32 of object;
var
  i, Count: integer;
  pSource1, pSource2, pDest: PColor32Entry;
  Blender: TCustomGraphics32Blender;
  Combiner: TPixelCombiner;
  c1, c2, c3, c4: Int64;
  vR, vG, vB, vA: integer;
begin
  Input1 := ResolveSurface(TSvgFeCompositeNode(Node).ResolvedIn1, RenderData);
  Input2 := ResolveSurface(TSvgFeCompositeNode(Node).ResolvedIn2, RenderData, RenderData.SourceGraphic);
  if (Input1 <> RenderData.UnnamedSurface) and (Input2 <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  Count := Input1.PixelCount;
  pSource1 := PColor32Entry(Input1.Bits);
  pSource2 := PColor32Entry(Input2.Bits);
  pDest := PColor32Entry(RenderData.CurrentSurface.Bits);

  case TSvgFeCompositeNode(Node).CompositeOperator of
    coOver:
      begin
        // Input1 (in) composited over Input2 (in2) onto ADest
        Input2.CopyMapTo(RenderData.CurrentSurface);
        Input1.DrawTo(RenderData.CurrentSurface, 0, 0);
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
        c1 := Round((TSvgFeCompositeNode(Node).K1 * OneOver255) * 65536.0);
        c2 := Round(TSvgFeCompositeNode(Node).K2 * 65536.0);
        c3 := Round(TSvgFeCompositeNode(Node).K3 * 65536.0);
        c4 := Round(TSvgFeCompositeNode(Node).K4 * 255.0 * 65536.0);

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

//------------------------------------------------------------------------------
//
//      TFilterRendererMerge
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeMergeNode
//------------------------------------------------------------------------------
type
  TFilterRendererMerge = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererMerge.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input, DestSurface: TCustomBitmap32;
  ChildNode: TSvgNode;
begin
  DestSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True); // Must clear surface for child render

  for ChildNode in TSvgFeMergeNode(Node).Children do
    if ChildNode is TSvgFeMergeNodeChild then
    begin
      Input := ResolveSurface(TSvgFeMergeNodeChild(ChildNode).ResolvedIn1, RenderData);
      // Recurse to render child
      Renderer.BlendOffscreenSurface(DestSurface, Input, bmNormal);
    end;

  Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);
  RenderData.CurrentSurface := DestSurface;
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererOffset
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeOffsetNode
//------------------------------------------------------------------------------
type
  TFilterRendererOffset = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class function TFilterRendererOffset.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
begin
  Result.Left := TSvgFeOffsetNode(Node).Dx * Scale;
  Result.Top := TSvgFeOffsetNode(Node).Dy * Scale;
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
end;

class procedure TFilterRendererOffset.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TCustomBitmap32;
  DestX, DestY: integer;
begin
  Input := ResolveSurface(TSvgFeOffsetNode(Node).ResolvedIn1, RenderData);
  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);

  DestX := Round(TSvgFeOffsetNode(Node).Dx * RenderData.Scale);
  DestY := Round(TSvgFeOffsetNode(Node).Dy * RenderData.Scale);

  Input.DrawTo(RenderData.CurrentSurface, DestX, DestY);
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererFlood
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeFloodNode
//------------------------------------------------------------------------------
type
  TFilterRendererFlood = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererFlood.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Color: TColor32;
begin
  Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  Color := TSvgFeFloodNode(Node).FloodColor.Color;
  if TSvgFeFloodNode(Node).FloodOpacity < 1.0 then
    ScaleAlpha(Color, TSvgFeFloodNode(Node).FloodOpacity);

  RenderData.CurrentSurface.Clear(Color);
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererMorphology
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeMorphologyNode
//------------------------------------------------------------------------------
type
  TFilterRendererMorphology = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;

    class procedure ApplyMorphology(Input, Output: TCustomBitmap32; Op: TSvgMorphologyOperator; RadiusX, RadiusY: Integer; TempBitmap: TCustomBitmap32 = nil); static;
  end;

class function TFilterRendererMorphology.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
var
  MorphNode: TSvgFeMorphologyNode;
begin
  MorphNode := TSvgFeMorphologyNode(Node);
  Result.Left := MorphNode.RadiusX * Scale;
  Result.Top := MorphNode.RadiusY * Scale;
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
end;

class procedure TFilterRendererMorphology.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input, Temp: TCustomBitmap32;
  MorphNode: TSvgFeMorphologyNode;
  RadiusX, RadiusY: Integer;
begin
  MorphNode := TSvgFeMorphologyNode(Node);
  Input := ResolveSurface(MorphNode.ResolvedIn1, RenderData);

  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  RadiusX := Round(MorphNode.RadiusX * RenderData.Scale);
  RadiusY := Round(MorphNode.RadiusY * RenderData.Scale);

  if (RadiusX > 0) and (RadiusY > 0) then
    Temp := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False)
  else
    Temp := nil;
  try

    ApplyMorphology(Input, RenderData.CurrentSurface, MorphNode.MorphologyOperator, RadiusX, RadiusY, Temp);

  finally
    if Temp <> nil then
      Renderer.ReleaseOffscreenBitmap(Temp);
  end;
end;

class procedure TFilterRendererMorphology.ApplyMorphology(Input, Output: TCustomBitmap32; Op: TSvgMorphologyOperator; RadiusX, RadiusY: Integer; TempBitmap: TCustomBitmap32);
var
  W, H, X, Y, i, j, minX, maxX, minY, maxY: Integer;
  pSrc, pDst: PColor32Entry;
  minA, minR, minG, minB: Byte;
  maxA, maxR, maxG, maxB: Byte;
  Intermediate, SrcPass: TCustomBitmap32;
begin
  // TODO : Completely unoptimized - but does anyone really use this filter?

  W := Input.Width;
  H := Input.Height;

  if (W = 0) or (H = 0) then
    Exit;

  if (RadiusX <= 0) and (RadiusY <= 0) then
  begin
    Input.CopyMapTo(Output);
    Exit;
  end;

  if (RadiusX > 0) and (RadiusY > 0) and (TempBitmap <> nil) then
    Intermediate := TempBitmap
  else
    Intermediate := Output;

  // 1D Horizontal Pass
  if RadiusX > 0 then
  begin

    case Op of
      moErode:
        for Y := 0 to H - 1 do
          for X := 0 to W - 1 do
          begin
            pDst := PColor32Entry(Intermediate.PixelPtr[X, Y]);
            if (X - RadiusX < 0) or (X + RadiusX >= W) then
              pDst.ARGB := 0
            else
            begin
              minA := $FF; minR := $FF; minG := $FF; minB := $FF;
              for i := X - RadiusX to X + RadiusX do
              begin
                pSrc := PColor32Entry(Input.PixelPtr[i, Y]);
                if pSrc.A < minA then minA := pSrc.A;
                if pSrc.R < minR then minR := pSrc.R;
                if pSrc.G < minG then minG := pSrc.G;
                if pSrc.B < minB then minB := pSrc.B;
              end;
              pDst.A := minA; pDst.R := minR; pDst.G := minG; pDst.B := minB;
            end;
          end;

      moDilate:
        for Y := 0 to H - 1 do
          for X := 0 to W - 1 do
          begin
            pDst := PColor32Entry(Intermediate.PixelPtr[X, Y]);
            minX := X - RadiusX;
            if minX < 0 then
              minX := 0;
            maxX := X + RadiusX;
            if maxX >= W then
              maxX := W - 1;

            maxA := 0; maxR := 0; maxG := 0; maxB := 0;
            for i := minX to maxX do
            begin
              pSrc := PColor32Entry(Input.PixelPtr[i, Y]);
              if pSrc.A > maxA then maxA := pSrc.A;
              if pSrc.R > maxR then maxR := pSrc.R;
              if pSrc.G > maxG then maxG := pSrc.G;
              if pSrc.B > maxB then maxB := pSrc.B;
            end;
            pDst.A := maxA; pDst.R := maxR; pDst.G := maxG; pDst.B := maxB;
          end;
    end;

  end else
  if Intermediate <> Input then
    Input.CopyMapTo(Intermediate);

  // 1D Vertical Pass
  if RadiusY > 0 then
  begin
    if RadiusX > 0 then
      SrcPass := Intermediate
    else
      SrcPass := Input;

    case Op of
      moErode:
        for Y := 0 to H - 1 do
        begin
          if (Y - RadiusY < 0) or (Y + RadiusY >= H) then
          begin
            for X := 0 to W - 1 do
              PColor32Entry(Output.PixelPtr[X, Y]).ARGB := 0;
          end else
          begin
            minY := Y - RadiusY;
            maxY := Y + RadiusY;
            for X := 0 to W - 1 do
            begin
              pDst := PColor32Entry(Output.PixelPtr[X, Y]);
              minA := $FF; minR := $FF; minG := $FF; minB := $FF;
              for j := minY to maxY do
              begin
                pSrc := PColor32Entry(SrcPass.PixelPtr[X, j]);
                if pSrc.A < minA then minA := pSrc.A;
                if pSrc.R < minR then minR := pSrc.R;
                if pSrc.G < minG then minG := pSrc.G;
                if pSrc.B < minB then minB := pSrc.B;
              end;
              pDst.A := minA; pDst.R := minR; pDst.G := minG; pDst.B := minB;
            end;
          end;
        end;

      moDilate:
        for Y := 0 to H - 1 do
        begin
          minY := Y - RadiusY;
          if minY < 0 then
            minY := 0;
          maxY := Y + RadiusY;
          if maxY >= H then
            maxY := H - 1;

          for X := 0 to W - 1 do
          begin
            pDst := PColor32Entry(Output.PixelPtr[X, Y]);
            maxA := 0; maxR := 0; maxG := 0; maxB := 0;
            for j := minY to maxY do
            begin
              pSrc := PColor32Entry(SrcPass.PixelPtr[X, j]);
              if pSrc.A > maxA then maxA := pSrc.A;
              if pSrc.R > maxR then maxR := pSrc.R;
              if pSrc.G > maxG then maxG := pSrc.G;
              if pSrc.B > maxB then maxB := pSrc.B;
            end;
            pDst.A := maxA; pDst.R := maxR; pDst.G := maxG; pDst.B := maxB;
          end;
        end;
    end;
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererComponentTransfer
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeComponentTransferNode
//------------------------------------------------------------------------------
type
  TFilterRendererComponentTransfer = class(TFilterRenderer)
  private
    class procedure BuildLUT(FuncNode: TSvgFeFuncNode; var LUT: TLUT8);
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class procedure TFilterRendererComponentTransfer.BuildLUT(FuncNode: TSvgFeFuncNode; var LUT: TLUT8);
var
  i, n, k: Integer;
  c, val, t, fIndex: Single;
  v1, v2: Single;
begin
  // Initialize identity lookup table [0..255]
  for i := 0 to 255 do
    LUT[i] := i;

  if (FuncNode = nil) then
    Exit;

  case FuncNode.FuncType of
    ctIdentity:
      exit; // Identity; Already done that

    ctTable:
      begin
        n := Length(FuncNode.TableValues);
        if (n = 0) then
          exit; // Identity; Already done that

        if (n = 1) then
        begin
          for i := 0 to 255 do
            LUT[i] := Clamp(Round(FuncNode.TableValues[0] * 255.0));
        end else
        begin
          for i := 0 to 255 do
          begin
            c := i * OneOver255;
            fIndex := c * (n - 1);
            k := Trunc(fIndex);
            if (k >= n - 1) then
              val := FuncNode.TableValues[n - 1]
            else
            begin
              t := fIndex - k;
              v1 := FuncNode.TableValues[k];
              v2 := FuncNode.TableValues[k + 1];
              val := v1 + t * (v2 - v1);
            end;
            LUT[i] := Clamp(Round(val * 255.0));
          end;
        end;
      end;

    ctDiscrete:
      begin
        n := Length(FuncNode.TableValues);
        if (n = 0) then
          exit; // Identity; Already done that

        for i := 0 to 255 do
        begin
          c := i * OneOver255;
          k := Trunc(c * n);
          if (k >= n) then
            k := n - 1;
          LUT[i] := Clamp(Round(FuncNode.TableValues[k] * 255.0));
        end;
      end;

    ctLinear:
      begin
        for i := 0 to 255 do
        begin
          c := i * OneOver255;
          val := FuncNode.Slope * c + FuncNode.Intercept;
          LUT[i] := Clamp(Round(val * 255.0));
        end;
      end;

    ctGamma:
      begin
        for i := 0 to 255 do
        begin
          c := i * OneOver255;
          val := FuncNode.Amplitude * System.Math.Power(c, FuncNode.Exponent) + FuncNode.Offset;
          LUT[i] := Clamp(Round(val * 255.0));
        end;
      end;
  end;
end;

class procedure TFilterRendererComponentTransfer.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TCustomBitmap32;
  ChildNode: TSvgNode;
  FuncR, FuncG, FuncB, FuncA: TSvgFeFuncNode;
  LutR, LutG, LutB, LutA: TLUT8;
  i, Count: Integer;
  pSource, pDest: PColor32Entry;
begin
  Input := ResolveSurface(Node.ResolvedIn1, RenderData);
  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);

  FuncR := nil;
  FuncG := nil;
  FuncB := nil;
  FuncA := nil;

  for ChildNode in Node.Children do
  begin
    if ChildNode is TSvgFeFuncRNode then
      FuncR := TSvgFeFuncRNode(ChildNode)
    else
    if ChildNode is TSvgFeFuncGNode then
      FuncG := TSvgFeFuncGNode(ChildNode)
    else
    if ChildNode is TSvgFeFuncBNode then
      FuncB := TSvgFeFuncBNode(ChildNode)
    else
    if ChildNode is TSvgFeFuncANode then
      FuncA := TSvgFeFuncANode(ChildNode);
  end;

  BuildLUT(FuncR, LutR);
  BuildLUT(FuncG, LutG);
  BuildLUT(FuncB, LutB);
  BuildLUT(FuncA, LutA);

  Count := Input.PixelCount;
  pSource := PColor32Entry(Input.Bits);
  pDest := PColor32Entry(RenderData.CurrentSurface.Bits);

  for i := 0 to Count - 1 do
  begin
    pDest.R := LutR[pSource.R];
    pDest.G := LutG[pSource.G];
    pDest.B := LutB[pSource.B];
    pDest.A := LutA[pSource.A];
    Inc(pSource);
    Inc(pDest);
  end;
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererDropShadow
//
//------------------------------------------------------------------------------
// Renderer for TSvgFeDropShadowNode
//------------------------------------------------------------------------------
type
  TFilterRendererDropShadow = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;

class function TFilterRendererDropShadow.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
var
  DropNode: TSvgFeDropShadowNode;
  BlurRadX, BlurRadY: Single;
begin
  DropNode := TSvgFeDropShadowNode(Node);
  BlurRadX := DropNode.StdDeviationX * GaussianSigmaToRadius * Scale + 2;
  BlurRadY := DropNode.StdDeviationY * GaussianSigmaToRadius * Scale + 2;
  Result.Left := Abs(DropNode.Dx) * Scale + BlurRadX;
  Result.Top := Abs(DropNode.Dy) * Scale + BlurRadY;
  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
end;

class procedure TFilterRendererDropShadow.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  DropNode: TSvgFeDropShadowNode;
  Input, BlurredShadow: TCustomBitmap32;
  AlphaMap, BlurredAlphaMap: TByteMap;
  RadiusX, RadiusY: Single;
  DestX, DestY: Integer;
  ShadowColor: TColor32Entry;
  ShadowOpacity: Byte;
  pIn: PColor32Entry;
  pAlpha: PByte;
  pBlurred: PColor32Entry;
  pBlurredAlpha: PByte;
  i, PixelCount: Integer;

  procedure ApplyBlur8(Src, Dst: TByteMap; RadX, RadY: Single);
  var
    Temp: TByteMap;
  begin
    if (RadX > 100) then RadX := 100;
    if (RadY > 100) then RadY := 100;

    if (RadX < Blur32MinRadius) and (RadY < Blur32MinRadius) then
      Dst.Assign(Src)
    else
    if (RadY < Blur32MinRadius) then
      FastHorizontalBlur8(Src, Dst, RadX)
    else
    if (RadX < Blur32MinRadius) then
    begin
      Temp := TByteMap.Create;
      try
        Transpose8(Src, Temp);
        FastHorizontalBlur8(Temp, Dst, RadY);
        Transpose8(Dst, Dst); // TODO : Do we support inline transpose?
      finally
        Temp.Free;
      end;
    end
    else
    if Abs(RadX - RadY) < 1e-4 then
      FastBlur8(Src, Dst, RadX)
    else
    begin
      Temp := TByteMap.Create;
      try
        FastHorizontalBlur8(Src, Temp, RadX);
        Transpose8(Temp, Dst);
        FastHorizontalBlur8(Dst, Temp, RadY);
        Transpose8(Temp, Dst);
      finally
        Temp.Free;
      end;
    end;
  end;

begin
  DropNode := TSvgFeDropShadowNode(Node);

  Input := ResolveSurface(DropNode.ResolvedIn1, RenderData);
  if (Input <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  ShadowColor := TColor32Entry(DropNode.FloodColor.Color);
  ShadowOpacity := Clamp(Round(ShadowColor.A * DropNode.FloodOpacity));

  PixelCount := RenderData.ROI.Width * RenderData.ROI.Height;

  AlphaMap := TByteMap.Create;
  BlurredAlphaMap := TByteMap.Create;
  try
    // 1. Extract 8-bit alpha shadow map
    AlphaMap.SetSize(RenderData.ROI.Width, RenderData.ROI.Height, False);
    BlurredAlphaMap.SetSize(RenderData.ROI.Width, RenderData.ROI.Height, False);

    pIn := PColor32Entry(Input.Bits);
    pAlpha := PByte(AlphaMap.Bits);

    for i := 0 to PixelCount - 1 do
    begin
      pAlpha^ := MulDiv255Table[pIn.A, ShadowOpacity];
      Inc(pIn);
      Inc(pAlpha);
    end;

    // 2. Apply fast 1-channel blur to 8-bit shadow alpha map
    RadiusX := DropNode.StdDeviationX * GaussianSigmaToRadius * RenderData.Scale;
    RadiusY := DropNode.StdDeviationY * GaussianSigmaToRadius * RenderData.Scale;

    ApplyBlur8(AlphaMap, BlurredAlphaMap, RadiusX, RadiusY);

    // 3. Reconstruct blurred 32-bit ARGB shadow surface
    BlurredShadow := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, False);
    try
      pBlurred := PColor32Entry(BlurredShadow.Bits);
      pBlurredAlpha := PByte(BlurredAlphaMap.Bits);

      for i := 0 to PixelCount - 1 do
      begin
        ShadowColor.A := pBlurredAlpha^;
        pBlurred^ := ShadowColor;
        Inc(pBlurred);
        Inc(pBlurredAlpha);
      end;

      // 4. Composite blurred shadow shifted by (Dx, Dy) and overlay Input at (0, 0)
      RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);

      DestX := Round(DropNode.Dx * RenderData.Scale);
      DestY := Round(DropNode.Dy * RenderData.Scale);

      // Paint the shadow...
      BlockTransfer(RenderData.CurrentSurface, DestX, DestY, RenderData.CurrentSurface.ClipRect,
        BlurredShadow, BlurredShadow.BoundsRect, dmOpaque);
      // ...and the input image on top of it
      Renderer.BlendOffscreenSurface(RenderData.CurrentSurface, Input, bmNormal);

    finally
      Renderer.ReleaseOffscreenBitmap(BlurredShadow);
    end;
  finally
    BlurredAlphaMap.Free;
    AlphaMap.Free;
  end;
end;

//------------------------------------------------------------------------------

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
  RenderData: TFilterRenderData;
  i, j, Count: Integer;
  Node: TSvgNode;
  pSource, pDest: PColor32;
  SourceBounds, FilterBounds, FilterRegionRect: TFloatRect;
  PathNode: TSvgPathNode;
  PathPoints, TransformedPoints, StrokePoints, AllRenderPoints: TArrayOfArrayOfFloatPoint;
  StrokeWidth, ScaledOffset, BBoxWidth, BBoxHeight, RegionLeft, RegionTop, RegionWidth, RegionHeight: Single;
  ScaledDashArray: TArrayOfFloat;
  Points: TArrayOfArrayOfFloatPoint;
  NodeBounds: TFloatRect;
  Pts: array[0..3] of TFloatPoint;
  Margin: TFloatRect;
  MarginX, MarginY, RadiusX, RadiusY: Integer;
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
      if Length(PathPoints) > 0 then
      begin
        TransformedPoints := GetTransformedPoints(PathPoints);

        StrokePoints := nil;
        StrokeWidth := PathNode.Stroke.Width.ToPixels(FViewportRect.Width);
        if (StrokeWidth > 0) and ((PathNode.Stroke.ResolvedPaintServer <> nil) or (not GetEffectiveColor(PathNode, PathNode.Stroke.Color).IsNone)) then
        begin
          StrokeWidth := StrokeWidth * RenderData.Scale;
          ScaledDashArray := nil;
          ScaledOffset := 0;
          if (PathNode.Stroke.DashArray <> nil) then
          begin
            SetLength(ScaledDashArray, Length(PathNode.Stroke.DashArray));
            for i := 0 to High(PathNode.Stroke.DashArray) do
              ScaledDashArray[i] := PathNode.Stroke.DashArray[i] * RenderData.Scale;
            ScaledOffset := PathNode.Stroke.DashOffset * RenderData.Scale;
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
        if (PathNode.Fill.ResolvedPaintServer <> nil) or (not GetEffectiveColor(PathNode, PathNode.Fill.Color).IsNone) then
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
      end;
    end;

    // Calculate filter region defined on AFilterNode (x, y, width, height, filterUnits)
    if ANode is TSvgPathNode then
    begin
      PathPoints := TSvgPathNode(ANode).GetPathData(FViewportRect.Width, FViewportRect.Height);
      NodeBounds := PolyPolygonBounds(PathPoints);
    end else
      NodeBounds := ANode.GetObjectBoundingBox;

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
    FilterBounds.Left := FilterBounds.Left - MarginX;
    FilterBounds.Top := FilterBounds.Top - MarginY;
    FilterBounds.Right := FilterBounds.Right + MarginX;
    FilterBounds.Bottom := FilterBounds.Bottom + MarginY;

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

        if Node is TSvgFeGaussianBlurNode then
          TFilterRendererGaussianBlur.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeColorMatrixNode then
          TFilterRendererColorMatrix.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeBlendNode then
          TFilterRendererBlend.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeCompositeNode then
          TFilterRendererComposite.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeMergeNode then
          TFilterRendererMerge.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeOffsetNode then
          TFilterRendererOffset.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeDropShadowNode then
          TFilterRendererDropShadow.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeFloodNode then
          TFilterRendererFlood.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeMorphologyNode then
          TFilterRendererMorphology.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
        else
        if Node is TSvgFeComponentTransferNode then
          TFilterRendererComponentTransfer.Render(Self, TSvgFilterPrimitiveNode(Node), RenderData)
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

          if TSvgFilterPrimitiveNode(Node).IsReferenceTarget then
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

procedure TSvgRenderer.MapFont(const AFontFamily, AWeightStr, AStyleStr: AnsiString; ASize: integer; var AFontInfo: TFontInfo);
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

procedure TSvgRenderer.RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);

  function GetAccumulatedTextOpacity(Node: TSvgNode): Single;
  begin
    Result := 1.0;
    while (Node <> nil) and (Node is TSvgTextPositioningNode) do
    begin
      Result := Result * Node.Opacity;
      Node := Node.Parent;
    end;
  end;

  procedure RenderTextPathData(const APathPoints: TArrayOfArrayOfFloatPoint; ANode: TSvgNode);
  var
    TransformedPts, StrokePts, DashedPts: TArrayOfArrayOfFloatPoint;
    StrokeWidth, MatScale, ScaledOffset: Single;
    ScaledDashArray: TArrayOfFloat;
    i, j, k: Integer;
    AccumulatedOpacity, FillOpacity, StrokeOpacity: Single;
  begin
    if (Length(APathPoints) = 0) or (ATarget = nil) then
      Exit;

    AccumulatedOpacity := GetAccumulatedTextOpacity(ANode);
    FillOpacity := ANode.Fill.Opacity * AccumulatedOpacity;
    StrokeOpacity := ANode.Stroke.Opacity * AccumulatedOpacity;

    TransformedPts := GetTransformedPoints(APathPoints);

    // 1. Fill Rendering
    RenderPolyPolygon(ATarget, ANode.Fill.ResolvedPaintServer, TransformedPts, FillOpacity, GetEffectiveColor(ANode, ANode.Fill.Color), ANode.Fill.FillRule);

    // 2. Stroke Rendering
    StrokeWidth := ANode.Stroke.Width.ToPixels(FViewportRect.Width);
    if (StrokeWidth > 0) and (CanRenderPolyPolygon(ANode.Stroke.ResolvedPaintServer, TransformedPts, StrokeOpacity, GetEffectiveColor(ANode, ANode.Stroke.Color))) then
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

      RenderPolyPolygon(ATarget, ANode.Stroke.ResolvedPaintServer, StrokePts, StrokeOpacity, GetEffectiveColor(ANode, ANode.Stroke.Color));
    end;
  end;

  // Evaluates path arc length and places glyphs at interpolated path distance points
  // aligned to segment tangent orientation angles.
  procedure ProcessTextPathNode(ANode: TSvgTextPathNode; Canvas: TCanvas32);
  var
    Text, CharString: string;
    PathPts: TArrayOfArrayOfFloatPoint;
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
    HasNodeTransform: Boolean;
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

      Text := ANode.TextContent;
      if Text = '' then
        Text := GetSubtreeText(ANode);

      if (Text = '') then
        Exit;

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

            FTransformation.Push;
            try

              ApplyMatrix(RotationMat.Matrix);
              Canvas.Clear;
              Canvas.BeginUpdate;

              Canvas.RenderText(-charWidth * 0.5, drawY, Text[GlyphIdx], TextLayout);
              if (Canvas.Path <> nil) then
                RenderTextPathData(Canvas.Path, ANode);

              Canvas.Clear;
              Canvas.EndUpdate;

            finally
              FTransformation.Pop;
            end;
          end;
        end;

        CurrentDistance := CurrentDistance + CharWidth;
      end;
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

        Canvas.Bitmap.Font.Name := FontInfo.FontFamily;
        Canvas.Bitmap.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Bitmap.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;
        TextLayout.RemoveLeadingSpace := False;

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
                  RenderTextPathData(Canvas.Path, ANode);

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
            RenderTextPathData(Canvas.Path, ANode);

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
