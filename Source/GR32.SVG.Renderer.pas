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

{$define SVG32_USE_SIMD_MASK_FILTERS}
{-$define SVG32_ROI_DEBUGDRAW} // Visualize ROI
{$define SVG32_CACHE_BITMAPS}

// Define SVG32_HIQ_AFFINE_TRANSFORM to perform affine bitmap transformations
// using TKernelResampler instead of TLinearResampler.
//
// By default TLinearResampler is used to perform affine bitmap transformations.
// Unfortunately this resampler does not handle semitransparency correctly (it
// resamples straight RGB instead of premultiplied RGB) so it might produce dark
// edges in the transitions between opaque and transparent pixels.
// To avoid this problem a kernel resampler (TKernelResampler) can be used
// instead at the cost of some performance.
{-$define SVG32_HIQ_AFFINE_TRANSFORM}

{$if defined(DCC) and (CompilerVersion >= 28.0)}
  {$define USE_PPL} // Use Delphi's Parallel Programming Library (introduced XE7)
{$ifend}

uses
  SysUtils, Classes, Graphics, Generics.Collections,
  GR32,
  GR32_Transforms,
  GR32_Rasterizers,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_ColorGradients,
  GR32.SVG.Types,
  GR32.SVG.Tree;

//------------------------------------------------------------------------------
//
//      TMapPool<T>
//
//------------------------------------------------------------------------------
// Manages a pool of TCustomMap objects.
//------------------------------------------------------------------------------
type
  TMapPool<T: TCustomMap; TElement> = class abstract(TObject)
  private
    FPool: TObjectList<T>; // List of maps ordered by byte size. Smallest first.
    FMaxSize: Int64;
    FPoolSize: Int64;
  protected
    function CreateNewMap: T; virtual; abstract;
    procedure PrepareMap(Map: T); virtual;
    function CandidateWouldReallocate(Candidate: T; TargetSize: Int64): boolean; virtual;
    procedure PurgeFromPool(Index: integer);
  public
    constructor Create(AMaxSize: Int64 = 0);
    destructor Destroy; override;
    function Acquire(AWidth, AHeight: Integer; AClear: Boolean = True): T;
    procedure Release(Map: T); virtual;
    procedure Clear;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
    property PoolSize: Int64 read FPoolSize;
  end;

//------------------------------------------------------------------------------
//
//      TSvgBitmapPool
//
//------------------------------------------------------------------------------
// Reusable pool of intermediate TCustomBitmap32 offscreen surfaces to eliminate
// frequent heap allocations/deallocations during nested group opacity, clip
// path, and mask compositing.
// All bitmaps use the TMemoryBackend backend.
//------------------------------------------------------------------------------
type
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


//------------------------------------------------------------------------------
//
//      TSvgPatternPolygonFiller
//
//------------------------------------------------------------------------------
// Tiling bitmap polygon filler with transformation support.
//------------------------------------------------------------------------------
type
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


//------------------------------------------------------------------------------
//
//      TSvgRenderer
//
//------------------------------------------------------------------------------
type
  TFontInfo = record
    FontFamily: UTF8String;
    Style: TFontStyles;
    Size: integer;
  end;

  TResamplerQuality = (
    rqFast,             // Resample using TNearestResampler
    rqNormal,           // Resample using TLinearResampler
    rqHigh              // Resample using TKernelResampler
  );

  TSvgRendererResolveExternalFileEvent = procedure(Sender: TObject; const Filename: string; Stream: TStream; var Allow: boolean) of object;
  TSvgRendererResolveFontEvent = procedure(Sender: TObject; const AFontFamily, AWeightStr, AStyleStr: string; ASize: integer; var AFontInfo: TFontInfo);

  TSvgRenderer = class(TObject)
  private
    FTarget: TCustomBitmap32;
    FViewportRect: TFloatRect;
    FDocumentRoot: TSvgDocumentNode;
    FBitmapPool: TSvgBitmapPool;
    FPolyRenderer: TPolygonRenderer32;
    FAllowExternalImages: Boolean;
    FRecursionDepth: integer;
    FRenderingClipPath: integer;
    FFilteredNode: TSvgNode;
    FTransformation: TAffineTransformation;
    FResamplers: array[TResamplerQuality] of TCustomResampler;
    FRasterizer: TRasterizer;
    FTransformer: TCustomSampler;

    FThemeFillColor: TSvgColor;
    FThemeStrokeColor: TSvgColor;
    FCurrentColor: TSvgColor;
    FOverridePreserveAspectRatio: Boolean;
    FPreserveAspectRatioOverride: TSvgPreserveAspectRatio;
    FOffscreenOffset: TPoint;
    FOnResolveExternalFile: TSvgRendererResolveExternalFileEvent;
    FOnResolveFontEvent: TSvgRendererResolveFontEvent;
    function GetTransformation: TAffineTransformation;
    procedure SetThemeFillColor32(AColor: TColor32);
    procedure SetThemeStrokeColor32(AColor: TColor32);
    function GetThemeFillColor32: TColor32;
    function GetThemeStrokeColor32: TColor32;
    procedure SetCurrentColor32(AColor: TColor32);
    function GetCurrentColor32: TColor32;
    function GetIsRenderingClipPath: boolean;
    function GetResampler(Quality: TResamplerQuality): TCustomResampler;
  protected type
    TThemeColor = (tcStroke, tcFill);

  protected
    // Fill/stroke a polypolygon OR fill a bitmap
    // If APoint=nil then ATarget is filled directly, otherwise the polypolygon is
    // rendered onto it using the fill/stroke.
    procedure RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TSvgPaintServerNode; AOpacity: Single; AColor: TSvgColor; const APoints: TArrayOfArrayOfFloatPoint = nil; AFillMode: TPolyFillMode = pfWinding);

    procedure SetupResampler(ABitmap: TCustomBitmap32; AQuality: TResamplerQuality);
    procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect; Reverse: boolean; AResampler: TCustomResampler);

    procedure RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
    procedure RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
    procedure RenderImage(ATarget: TCustomBitmap32; const AHref: UTF8String; const ATargetRect: TFloatRect; const APreserveAspectRatio: TSvgPreserveAspectRatio);
    procedure RenderTextPathData(ATarget: TCustomBitmap32; const APathPoints: TArrayOfArrayOfFloatPoint; ANode: TSvgNode);
    procedure RenderTextAreaNode(ATarget: TCustomBitmap32; ATextAreaNode: TSvgTextAreaNode);
    procedure RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);
    procedure RenderGroupNode(ATarget: TCustomBitmap32; AGroupNode: TSvgCustomGroupNode);
    procedure RenderClipPathNode(AMaskBmp: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMaskNode(AMaskBmp: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    procedure RenderMarker(ATarget: TCustomBitmap32; AMarker: TSvgMarkerNode; const AVertex: TFloatPoint; AAngle: Single; AStrokeWidth: Single); // Angle is in radians!
    procedure RenderMarkers(ATarget: TCustomBitmap32; APathNode: TSvgPathNode; const APoints: TArrayOfArrayOfFloatPoint; AStrokeWidth: Single);
    procedure RenderFilter(ATarget: TCustomBitmap32; AFilterNode: TSvgFilterNode; ANode: TSvgNode);
    procedure RenderNodeContent(ATarget: TCustomBitmap32; ANode: TSvgNode);
    procedure RenderNodeUnfiltered(ATarget: TCustomBitmap32; ANode: TSvgNode);
    procedure ApplyClipPathNode(ATarget: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect; ATranslate: boolean = True);
    procedure ApplyMaskNode(ATarget: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
    function GetTransformedPoints(const APoints: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFloatPoint;
    function GetPathBounds(const APoints: TArrayOfArrayOfFloatPoint): TFloatRect;
    function GetEffectiveColor(ANode: TSvgNode; AColor: TSvgColor; AThemeColor: TThemeColor = tcFill): TSvgColor; // protected to be accesible by TFilterContext
    procedure VerticalBlur32(ASource, ADest: TCustomBitmap32; ARadius: TFloat);
    procedure ResolveFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo);
    procedure DoResolveFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo); virtual;
    function DoResolveExternalFile(const Filename: string; Stream: TStream): boolean; virtual;
    procedure BeginRenderClipPath;
    procedure EndRenderClipPath;
    property IsRenderingClipPath: boolean read GetIsRenderingClipPath;

    property Target: TCustomBitmap32 read FTarget;
    property Resampler[Quality: TResamplerQuality]: TCustomResampler read GetResampler;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetOffscreenBitmap(AWidth, AHeight: Integer; AClear: Boolean = True): TCustomBitmap32;
    procedure ReleaseOffscreenBitmap(var ABitmap: TCustomBitmap32);
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode; AX: Integer; AY: Integer); overload;
    procedure BlendOffscreenSurface(ATarget, ASource: TCustomBitmap32; ABlendMode: TSvgBlendMode); overload;

    procedure ApplyMatrix(const AMatrix: TFloatMatrix);
    procedure ClearThemeColors;

    procedure RenderDocument(ATarget: TCustomBitmap32; ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect); overload;
    procedure RenderDocument(ATarget: TCustomBitmap32; ADoc: TSvgDocumentNode); overload;
    procedure RenderNode(ATarget: TCustomBitmap32; ANode: TSvgNode);

    function GetDocumentBounds(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect; ATarget: TCustomBitmap32 = nil): TFloatRect;

    property Transformation: TAffineTransformation read GetTransformation;
    property ViewportRect: TFloatRect read FViewportRect write FViewportRect;
    property AllowExternalImages: Boolean read FAllowExternalImages write FAllowExternalImages;
    property ThemeFillColor: TSvgColor read FThemeFillColor write FThemeFillColor;
    property ThemeFillColor32: TColor32 read GetThemeFillColor32 write SetThemeFillColor32;
    property ThemeStrokeColor: TSvgColor read FThemeStrokeColor write FThemeStrokeColor;
    property ThemeStrokeColor32: TColor32 read GetThemeStrokeColor32 write SetThemeStrokeColor32;
    property CurrentColor: TSvgColor read FCurrentColor write FCurrentColor;
    property CurrentColor32: TColor32 read GetCurrentColor32 write SetCurrentColor32;
    property OverridePreserveAspectRatio: Boolean read FOverridePreserveAspectRatio write FOverridePreserveAspectRatio;
    property PreserveAspectRatioOverride: TSvgPreserveAspectRatio read FPreserveAspectRatioOverride write FPreserveAspectRatioOverride;

    property OnResolveExternalFile: TSvgRendererResolveExternalFileEvent read FOnResolveExternalFile write FOnResolveExternalFile;
    property OnResolveFontEvent: TSvgRendererResolveFontEvent read FOnResolveFontEvent write FOnResolveFontEvent;
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
  GR32_Backends,
  GR32_Backends_Generic,
  GR32_Paths,
  GR32_Resamplers,
  GR32_Filters,
  GR32.Text.Types,
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
  OneOver255: Single = 1 / 255;

const
  cMaxRecursions = 20;

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

  if ANode is TSvgCustomGroupNode then
  begin
    for Child in TSvgCustomGroupNode(ANode).Children do
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

    if (Node is TSvgCustomGroupNode) and (TSvgCustomGroupNode(Node).Isolation = isoIsolate) then
      Break;

    Node := Node.Parent;
  end;
  Result := bmNormal;
end;

procedure ApplyAlphaMask(AMask, ATarget: TCustomBitmap32);
{$if not defined(SVG32_USE_SIMD_MASK_FILTERS)}
var
  SourceP, DestP: PColor32;
  i: Integer;
  AlphaVal: Byte;
{$ifend}
begin
{$if defined(SVG32_USE_SIMD_MASK_FILTERS)}
  ScaleAlphaLine(PColor32(AMask.Bits), PColor32(ATarget.Bits), ATarget.PixelCount);
{$else}
    SourceP := PColor32(AMask.Bits);
    DestP := PColor32(ATarget.Bits);

    for i := 0 to ATarget.PixelCount - 1 do
    begin
      AlphaVal := AlphaComponent(SourceP^);
      if AlphaVal < 255 then
        ScaleAlpha(DestP^, AlphaVal * OneOver255);
      Inc(DestP);
      Inc(SourceP);
    end;
{$ifend}
end;

procedure ApplyLuminanceMask(AMask, ATarget: TCustomBitmap32);
{$if not defined(SVG32_USE_SIMD_MASK_FILTERS)}
var
  SourceP: PColor32Entry;
  DestP: PColor32;
  i: Integer;
  Gray: Byte;
{$ifend}
begin
{$if defined(SVG32_USE_SIMD_MASK_FILTERS)}
  GR32_Filters.ApplyAlphaMask(PColor32(AMask.Bits), PColor32(ATarget.Bits), ATarget.PixelCount);
{$else}
    SourceP := PColor32Entry(AMask.Bits);
    DestP := PColor32(ATarget.Bits);

    for i := 0 to ATarget.PixelCount - 1 do
    begin
      Gray := Intensity(SourceP.ARGB);
      Gray := Round(Gray * SourceP.A * OneOver255);
      if Gray < 255 then
        ScaleAlpha(DestP^, Gray * OneOver255);
      Inc(DestP);
      Inc(SourceP);
    end;
{$ifend}
end;

procedure ApplyOpacity(ATarget: TCustomBitmap32; AOpacity: Single);
{$if not defined(SVG32_USE_SIMD_MASK_FILTERS)}
var
  SourceP, DestP: PColor32;
  i: Integer;
{$ifend}
begin
  if (AOpacity >= 1.0) then
    exit;

{$if defined(SVG32_USE_SIMD_MASK_FILTERS)}
    ScaleAlphaMems(PColor32(ATarget.Bits), ATarget.PixelCount, AOpacity);
{$else}
    SourceP := PColor32(ATarget.Bits);
    for i := 0 to ATarget.PixelCount - 1 do
    begin
      ScaleAlpha(SourceP^, AOpacity);
      Inc(SourceP);
    end;
{$ifend}
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

procedure TSvgRenderer.BeginRenderClipPath;
begin
  Inc(FRenderingClipPath);
end;

procedure TSvgRenderer.EndRenderClipPath;
begin
  Dec(FRenderingClipPath);
end;

function TSvgRenderer.GetIsRenderingClipPath: boolean;
begin
  Result := (FRenderingClipPath > 0);
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

procedure TSvgRenderer.SetupResampler(ABitmap: TCustomBitmap32; AQuality: TResamplerQuality);
begin
  case AQuality of
    rqFast:
      ABitmap.ResamplerClassName := TNearestResampler.ClassName;

    rqNormal:
      ABitmap.ResamplerClassName := TLinearResampler.ClassName;

    rqHigh:
      begin
        ABitmap.ResamplerClassName := TKernelResampler.ClassName;
        TKernelResampler(ABitmap.Resampler).KernelClassName := TLinearKernel.ClassName; // Default is TBoxKernel
      end;
  end;

  ABitmap.Resampler.PixelAccessMode := pamTransparentEdge; // Required for proper AA
end;

procedure TSvgRenderer.Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect; Reverse: boolean;
  AResampler: TCustomResampler);
var
  DstRect: TRect;
begin
  if (not GR32.IntersectRect(DstRect, DstClip, Dst.ClipRect)) then
    Exit;

  if (FTransformer = nil) then
    FTransformer := TTransformer.Create(nil, nil);

  if (FRasterizer = nil) then
  begin
{$if defined(USE_PPL)}
    FRasterizer := TMultithreadedRegularRasterizer.Create;
{$else}
    FRasterizer := DefaultRasterizerClass.Create;
{$ifend}
    FRasterizer.Sampler := FTransformer;
  end;

  TTransformer(FTransformer).Sampler := AResampler;
  TTransformer(FTransformer).Transformation := Transformation;
  TTransformer(FTransformer).ReverseTransform := Reverse;
  try

    AResampler.Bitmap := Src;
    try

      FRasterizer.Rasterize(Dst, DstRect, Src);

    finally
      AResampler.Bitmap := nil;
    end;

  finally
    TTransformer(FTransformer).Sampler := nil;
    TTransformer(FTransformer).Transformation := nil;
    TTransformer(FTransformer).ReverseTransform := Reverse;
  end;

  Dst.Changed(DstRect);
end;

function TSvgRenderer.GetResampler(Quality: TResamplerQuality): TCustomResampler;
begin
  Result := FResamplers[Quality];

  if (Result = nil) then
  begin
    case Quality of
      rqFast:
        Result := TNearestResampler.Create;

      rqNormal:
        Result := TLinearResampler.Create;

      rqHigh:
        begin
          Result := TKernelResampler.Create;
          TKernelResampler(Result).KernelClassName := TLinearKernel.ClassName; // Default is TBoxKernel
        end;
    end;

    Result.PixelAccessMode := pamTransparentEdge; // Required for proper AA
    FResamplers[Quality] := Result;
  end;
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
  i: integer;
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
    try

      while True do
      begin
        try

          Result.SetSize(AWidth, AHeight, AClear);
          break;

        except
          on E: EOutOfMemory do
          begin
            // Try to recover from out-of-memory by freeing items from the pool.
            // Note that this might fail, even though there is available memory,
            // due to memory fragmentation.
            // We don't worry about performance here. We just try to survive.
            if (FPool.IsEmpty) then
              raise;
            // Delete the last (i.e. largest) item
            PurgeFromPool(FPool.Count-1);
            // And delete a few items from the start (i.e. the smallest)
            i := 16;
            while (not FPool.IsEmpty) and (i > 0) do
            begin
              PurgeFromPool(0);
              Dec(i);
            end;
          end;
        end;
      end;

    except
      // SetSize might bomb on out-of-memory or too large bitmap
      Result.Free;
      raise;
    end;
  end else
  begin
{$ifndef FPC}
    Result := FPool.ExtractAt(CandidateIdx);
{$else LetsMimicTheGrownupsButFail}
    Result := FPool.ExtractIndex(CandidateIdx);
{$endif}
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

procedure TMapPool<T, TElement>.PurgeFromPool(Index: integer);
begin
  Dec(FPoolSize, FPool[Index].ByteCount);
  FPool.Delete(Index);
end;

procedure TMapPool<T, TElement>.Release(Map: T);
{$if defined(SVG32_CACHE_BITMAPS)}
var
  LowIdx, HighIdx, MidIdx: Integer;
  TargetByteCount, MidByteCount: Int64;
{$ifend}
begin
  if Map = nil then
    exit;

{$if defined(SVG32_CACHE_BITMAPS)}

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
      PurgeFromPool(FPool.Count-1);
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

{$else}
  Map.Free;
{$ifend}
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
  // Prevent update notification overhead; Nobody's listening
  Result.BeginLockUpdate;
end;

procedure TSvgBitmapPool.PrepareMap(Map: TCustomBitmap32);
begin
  Map.MasterAlpha := 255;
  Map.DrawMode := dmBlend;
  Map.CombineMode := cmMerge;
  Map.ResetClipRect;
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
  end else
    exit;

  inherited;
end;

{ TSvgRenderer }

constructor TSvgRenderer.Create;
begin
  inherited Create;
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
  FResamplers[rqFast].Free;
  FResamplers[rqNormal].Free;
  FResamplers[rqHigh].Free;
  FRasterizer.Free;
  FTransformer.Free;
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

{$if defined(SVG32_ROI_DEBUGDRAW)}
  if (ATarget <> nil) then
  begin
    var DebugRect := ASource.BoundsRect;
    GR32.OffsetRect(DebugRect, AX, AY);
    var DebugColor := Random($7F) or (Random($7F) shl 8) or (Random($7F) shl 16);
    ATarget.FrameRectTS(DebugRect, $7F000000 or DebugColor);
    GR32.InflateRect(DebugRect, 1, 1);
    DebugColor := (DebugColor shl 1);
    ATarget.FillRectTS(DebugRect, $3F000000 or DebugColor);
  end;
{$ifend}

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

procedure TSvgRenderer.RenderPolyPolygon(ATarget: TCustomBitmap32; APaintServer: TSvgPaintServerNode; AOpacity: Single; AColor: TSvgColor;
  const APoints: TArrayOfArrayOfFloatPoint; AFillMode: TPolyFillMode);
var
  Bounds: TFloatRect;
  Filler: TCustomPolygonFiller;
  Color: TColor32;
  PaintServerRenderer: TPaintServerRendererClass;
begin
  (*
  ** Fill using paint server
  *)
  if (APaintServer <> nil) then
  begin
    Bounds := GetPathBounds(APoints);

    Filler := nil;
    // Get a paint server renderer...
    PaintServerRenderer := TPaintServerRendererGradientClass(APaintServer.GetRenderClass);
    if (PaintServerRenderer <> nil) then
      // ...and get a filler from it
      Filler := PaintServerRenderer.CreateFiller(Self, APaintServer, Bounds, AOpacity);

    if (Filler <> nil) then
    begin
      try

        if (APoints <> nil) then
        begin
          // Fill target using polygon filler
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
        end else
          // Fill target using filler
          FillBitmap(ATarget, Filler);

      finally
        Filler.Free;
      end;
      exit;
    end;
    // Fall-through to paint solid
  end;

  (*
  ** Fill using solid color
  *)
  if (AColor.IsVisible) then
  begin
    Color := AColor.Color;
    if (AOpacity < 1.0) then
      ScaleAlpha(Color, AOpacity);

    if (AlphaComponent(Color) = 0) then
      exit;

    if (APoints <> nil) then
    begin
      // Fill target using solid polygon filler
      if (FPolyRenderer = nil) then
        FPolyRenderer := DefaultPolygonRendererClass.Create(ATarget);

      FPolyRenderer.Bitmap := ATarget;
      FPolyRenderer.Color := Color;
      FPolyRenderer.FillMode := AFillMode;

      FPolyRenderer.PolyPolygonFS(APoints);
    end else
      // Clear target to color
      ATarget.Clear(Color);
  end;
end;

procedure TSvgRenderer.RenderPathNode(ATarget: TCustomBitmap32; APathNode: TSvgPathNode);
var
  TransformedPoints, MarkerPoints: TArrayOfArrayOfFloatPoint;
  UserStrokeWidth, StrokeWidth, MaxStrokeExtent: Single;
  MatScale, ScaledOffset: Single;
  Points, StrokePoints: TArrayOfArrayOfFloatPoint;
  ScaledDashArray: TArrayOfFloat;
  i, j: Integer;
  FloatRoi: TFloatRect;
  RoiRect: TRect;
  NeedsOffscreen: Boolean;
  EffectiveBlendMode: TSvgBlendMode;
  RenderBmp, OffscreenBmp: TCustomBitmap32;
  FillColor, StrokeColor: TSvgColor;
  HasFill, HasStroke, HasMarkers: boolean;
  FillPaintServer, StrokePaintServer: TSvgPaintServerNode;
begin
  if (APathNode = nil) or (ATarget = nil) then
    Exit;

  (*
  ** 1. Determine what elements we need to process here
  **    We need to calculate the effective colors even if the stroke or fill
  **    has a paint-server, because the paint-server might not be able to
  **    create a filler (e.g. if the fill area is empty) in which case we must
  **    fall back to solid fill/stroke.
  *)
  UserStrokeWidth := APathNode.GetSizePx(APathNode.Stroke.Width, FViewportRect.Width);
  if (APathNode.Stroke.Opacity > 0) and (UserStrokeWidth > 0) then
    StrokeColor := GetEffectiveColor(APathNode, APathNode.Stroke.Color, tcStroke)
  else
    StrokeColor := TSvgColor.Unset;
  StrokePaintServer := APathNode.Stroke.ResolvedPaintServer;
  HasStroke := (UserStrokeWidth > 0) and ((StrokePaintServer <> nil) or (StrokeColor.IsVisible));

  if (APathNode.Fill.Opacity > 0) then
    FillColor := GetEffectiveColor(APathNode, APathNode.Fill.Color, tcFill)
  else
    FillColor := TSvgColor.Unset;
  FillPaintServer := APathNode.Fill.ResolvedPaintServer;
  HasFill := (FillPaintServer <> nil) or (FillColor.IsVisible);

  HasMarkers := (APathNode.ResolvedMarkerStart <> nil) or (APathNode.ResolvedMarkerMid <> nil) or (APathNode.ResolvedMarkerEnd <> nil);

  (*
  ** 2. Generate path and transform into world coordinates
  *)
  MatScale := TFloatMatrixHelper(FTransformation.Matrix).GetScale;
  TransformedPoints := APathNode.GetPathData(FViewportRect, FTransformation);
  if (TransformedPoints = nil) then
    Exit;

  (*
  ** 3. Generate stroke polygons if stroked
  *)
  StrokePoints := nil;
  StrokeWidth := 0; // Silences compile warning
  if (HasStroke) then
  begin
    StrokeWidth := UserStrokeWidth * MatScale;
    ScaledDashArray := nil;
    ScaledOffset := 0;
    if (APathNode.Stroke.DashArray <> nil) then
    begin
      SetLength(ScaledDashArray, Length(APathNode.Stroke.DashArray));
      for i := 0 to High(APathNode.Stroke.DashArray) do
        ScaledDashArray[i] := APathNode.Stroke.DashArray[i] * MatScale;
      ScaledOffset := APathNode.GetSizePx(APathNode.Stroke.DashOffset, FViewportRect) * MatScale;
    end;

    for i := 0 to High(TransformedPoints) do
    begin
      if (ScaledDashArray <> nil) then
      begin
        Points := BuildDashedLine(TransformedPoints[i], ScaledDashArray, ScaledOffset, APathNode.IsPathClosed(i));
        for j := 0 to High(Points) do
          StrokePoints := StrokePoints + BuildPolyPolyLine([Points[j]], False, StrokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
      end else
        StrokePoints := StrokePoints + BuildPolyPolyLine([TransformedPoints[i]], APathNode.IsPathClosed(i), StrokeWidth, APathNode.Stroke.JoinStyle, APathNode.Stroke.EndStyle, APathNode.Stroke.MiterLimit);
    end;
  end;

  (*
  ** 4. Calculate ROI bounding box after stroking and collect all output
  **    poly-polygons produced and rendered by the polygon node
  *)
  FloatRoi := PolyPolygonBounds(TransformedPoints);
  if HasStroke then
  begin
    MaxStrokeExtent := (StrokeWidth * 0.5) * Max(1.0, APathNode.Stroke.MiterLimit);
    GR32.InflateRect(FloatRoi, MaxStrokeExtent, MaxStrokeExtent);
  end;

  // Intersect FloatRoi with ATarget.ClipRect so offscreen surface allocations remain bounded strictly 
  // to the visible target viewport instead of allocating huge buffers when zoomed in.
  if not GR32.IntersectRect(FloatRoi, FloatRoi, FloatRect(ATarget.ClipRect)) then
    Exit;

  RoiRect := MakeRect(FloatRoi, rrOutside);

  (*
  ** 5. Check whether offscreen bitmap compositing is required (opacity, clip-path, mask, blend-mode)
  *)
  if IsRenderingClipPath then
  begin
    // Per W3C SVG 1.1 Specification §14.3.5 ("Clipping paths"):
    // The raw geometry of each child in a 'clipPath' element is used, conceptually
    // forming a 100% opaque binary clipping region. Presentation properties such as
    // 'opacity', 'fill-opacity', 'stroke', 'filter', and 'mask' on children of 'clipPath'
    // are ignored, and shape fill geometry is rendered at 100% alpha (clWhite32) into
    // the clip mask surface to prevent child opacity from dimming clipped content.

    // Per W3C SVG 1.1 Specification §14.3.5 ("Clipping paths"):
    // "The 'fill' and 'stroke' properties on children of a 'clipPath' element are ignored.
    // Shape fill geometry is used as the clipping path, regardless of whether 'fill' is
    // specified or set to 'none'."
    FillColor := TSvgColor.Create(clWhite32);
    HasFill := True;

    if HasFill then
    begin
      // W3C SVG 1.1 §14.3.5:
      // If a child inside <clipPath> has a 'clip-path' property, the child shape
      // is clipped by that referenced clipPath before unioning with other children.
      if (APathNode.ResolvedClipPath <> nil) then
      begin
        OffscreenBmp := GetOffscreenBitmap(RoiRect.Width, RoiRect.Height, True);
        try
          TranslatePolyPolygonInplace(TransformedPoints, -RoiRect.Left, -RoiRect.Top);
          RenderPolyPolygon(OffscreenBmp, nil, 1.0, FillColor, TransformedPoints, APathNode.Fill.FillRule);

          ApplyClipPathNode(OffscreenBmp, APathNode.ResolvedClipPath, FloatRoi, RoiRect);
          BlendOffscreenSurface(ATarget, OffscreenBmp, bmNormal, RoiRect.Left, RoiRect.Top);
        finally
          ReleaseOffscreenBitmap(OffscreenBmp);
        end;
      end else
        RenderPolyPolygon(ATarget, nil, 1.0, FillColor, TransformedPoints, APathNode.Fill.FillRule);
    end;
    Exit;
  end;

  // Per W3C SVG 1.1 Specification §15.2, when rendering the SourceGraphic input for a filter,
  // the target node's clip-path, mask, opacity, and mix-blend-mode are bypassed up front
  // so the filter primitives operate on unclipped raw graphic content. Those presentation
  // properties are subsequently applied to the filtered result surface in RenderFilter.
  if (APathNode = FFilteredNode) then
  begin
    EffectiveBlendMode := bmNormal;
    NeedsOffscreen := False;
  end else
  begin
    EffectiveBlendMode := GetEffectiveMixBlendMode(APathNode);
    NeedsOffscreen := (EffectiveBlendMode <> bmNormal) or (APathNode.ResolvedClipPath <> nil) or
                      (APathNode.ResolvedMask <> nil) or (APathNode.Opacity < 1.0);
  end;

  OffscreenBmp := nil;

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
        Inc(FOffscreenOffset.X, RoiRect.Left);
        Inc(FOffscreenOffset.Y, RoiRect.Top);
        FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);
      end;
      try
        var ObjBox := APathNode.GetObjectBoundingBox(FViewportRect);

        (*
        ** 6. Fill Rendering
        *)
        if (FillPaintServer <> nil) and (not FillPaintServer.ShouldApply(ObjBox)) then
          FillPaintServer := nil;

        RenderPolyPolygon(RenderBmp, FillPaintServer, APathNode.Fill.Opacity, FillColor, TransformedPoints, APathNode.Fill.FillRule);


        (*
        ** 7. Stroke Rendering
        *)
        if (StrokePoints <> nil) then
        begin
          if (StrokePaintServer <> nil) and (not StrokePaintServer.ShouldApply(ObjBox)) then
            StrokePaintServer := nil;

          RenderPolyPolygon(RenderBmp, StrokePaintServer, APathNode.Stroke.Opacity, StrokeColor, StrokePoints);
        end;
      finally
        if NeedsOffscreen then
        begin
          Dec(FOffscreenOffset.X, RoiRect.Left);
          Dec(FOffscreenOffset.Y, RoiRect.Top);
          FTransformation.Pop;
        end;
      end;
    end;

    (*
    ** 8. Markers Rendering (per SVG specification, markers paint on top of fill and stroke)
    *)
    if (HasMarkers) then
      // Note: Markers are in user space
      MarkerPoints := APathNode.GetControlPoints(FViewportRect)
    else
      MarkerPoints := nil;
    if (MarkerPoints <> nil) then
    begin

      if NeedsOffscreen then
      begin
        FTransformation.Push;
        try
          FTransformation.Translate(-RoiRect.Left, -RoiRect.Top);

          RenderMarkers(RenderBmp, APathNode, MarkerPoints, UserStrokeWidth);
        finally
          FTransformation.Pop;
        end;
      end else
        RenderMarkers(RenderBmp, APathNode, MarkerPoints, UserStrokeWidth);
    end;

    (*
    ** 9. Apply Offscreen Compositing (ClipPath, Mask, Opacity, Blend)
    *)
    if NeedsOffscreen then
    begin
      // Apply ClipPath in ROI space
      if (APathNode.ResolvedClipPath <> nil) then
        ApplyClipPathNode(OffscreenBmp, APathNode.ResolvedClipPath, FloatRoi, RoiRect);

      // Apply Alpha Mask in ROI space
      if (APathNode.ResolvedMask <> nil) then
        ApplyMaskNode(OffscreenBmp, APathNode.ResolvedMask, FloatRoi, RoiRect);

      // Apply Opacity
      if (APathNode.Opacity < 1.0) then
        ApplyOpacity(OffscreenBmp, APathNode.Opacity);

      // Blend ROI offscreen surface onto destination target at ROI origin
      BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, RoiRect.Left, RoiRect.Top);
    end;

  finally
    ReleaseOffscreenBitmap(OffscreenBmp);
  end;
end;

procedure TSvgRenderer.ApplyMaskNode(ATarget: TCustomBitmap32; AMaskNode: TSvgMaskNode; const ATargetBounds: TFloatRect; const ARoiRect: TRect);
var
  MaskBmp: TCustomBitmap32;
begin
  if (AMaskNode = nil) then
    exit;

  MaskBmp := GetOffscreenBitmap(ATarget.Width, ATarget.Height, False); // RenderMaskNode clears bitmap
  try

    FTransformation.Push;
    try
      FTransformation.Translate(-ARoiRect.Left, -ARoiRect.Top);

      RenderMaskNode(MaskBmp, AMaskNode, ATargetBounds, ARoiRect);
    finally
      FTransformation.Pop;
    end;

    ApplyLuminanceMask(MaskBmp, ATarget);

  finally
    ReleaseOffscreenBitmap(MaskBmp);
  end;
end;

procedure TSvgRenderer.ApplyClipPathNode(ATarget: TCustomBitmap32; AClipNode: TSvgClipPathNode; const ATargetBounds: TFloatRect;
  const ARoiRect: TRect; ATranslate: boolean);
var
  MaskBmp: TCustomBitmap32;
begin
  if (AClipNode = nil) then
    exit;

  if (ATranslate) then
    FTransformation.Push;
  try
    if (ATranslate) then
      FTransformation.Translate(-ARoiRect.Left, -ARoiRect.Top);

    MaskBmp := GetOffscreenBitmap(ATarget.Width, ATarget.Height, False); // RenderClipPathNode clears bitmap
    try
      RenderClipPathNode(MaskBmp, AClipNode, ATargetBounds, ARoiRect);

      ApplyAlphaMask(MaskBmp, ATarget);

    finally
      ReleaseOffscreenBitmap(MaskBmp);
    end;
  finally
    if (ATranslate) then
      FTransformation.Pop;
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
  //
  // Per W3C SVG 1.1 Specification §14.3.5 ("Clipping paths"), children of a <clipPath>
  // evaluate strictly as raw geometric clipping shapes at 100% alpha (solid coverage).
  // We set FIsRenderingClipPath := True to bypass visual presentation properties
  // (filters, offscreen opacity compositing, masks, sub-clipPaths, and paint servers).
  if (AMaskBmp = nil) then
    Exit;

  AMaskBmp.Clear(0); // Clear to 0 transparent so filled shapes paint non-zero alpha inside clip region

  if (AClipNode = nil) then
    Exit;

  BeginRenderClipPath;
  try
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

      for i := 0 to AClipNode.Children.Count - 1 do
        RenderNode(AMaskBmp, AClipNode.Children[i]);

      // W3C SVG 1.1 §14.3.5 ("Clipping paths"):
      // If a 'clip-path' property is specified on the <clipPath> element itself,
      // intersect the resulting clipping path with the referenced clipPath.
      ApplyClipPathNode(AMaskBmp, AClipNode.ResolvedClipPath, ATargetBounds, ARoiRect, False);

    finally
      FTransformation.Pop;
    end;
  finally
    EndRenderClipPath;
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

  if (AMaskBmp = nil) then
    exit;

  if (AMaskNode = nil) then
  begin
    AMaskBmp.Clear(clBlack32);
    exit;
  end;

  // If the 'mask' property references a 'mask' element containing no children,
  // the element referencing it is not rendered.
  if (AMaskNode.Children.Count = 0) then
  begin
    AMaskBmp.Clear(0);
    exit;
  end;

  AMaskBmp.Clear(clBlack32);

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
  RefPt: TFloatPoint;
begin
  if (AMarker = nil) or (AMarker.Children.Count = 0) or (ATarget = nil) then
    Exit;

  vpW := FViewportRect.Right - FViewportRect.Left;
  vpH := FViewportRect.Bottom - FViewportRect.Top;
  if vpW <= 0 then
    vpW := 1.0;
  if vpH <= 0 then
    vpH := 1.0;

  mw := AMarker.GetSizePx(AMarker.MarkerWidth, vpW);
  mh := AMarker.GetSizePx(AMarker.MarkerHeight, vpH);

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

  rx := AMarker.GetSizePx(AMarker.RefX, mw);
  ry := AMarker.GetSizePx(AMarker.RefY, mh);

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
    RefPt := TFloatMatrixHelper(viewMat).TransformPoint(
      FloatPoint(
        AMarker.GetSizePx(AMarker.RefX, markerViewBox.Width),
        AMarker.GetSizePx(AMarker.RefY, markerViewBox.Height)
      ));
    markerMat.Matrix := viewMat;
    markerMat.Translate(-RefPt.X, -RefPt.Y);
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

      // TODO : Bug here! We're drawing markers on the vertices of the *flattened* path.
      // We should be drawing marker on the unflattened control points. Unfortunately
      // we do not keep that data anywhere and I'm not sure it's worth fixing this bug
      // due to the overhead.

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

procedure TSvgRenderer.RenderGroupNode(ATarget: TCustomBitmap32; AGroupNode: TSvgCustomGroupNode);

  procedure RenderChildren(ATargetSurface: TCustomBitmap32);
  var
    DocNode: TSvgDocumentNode;
    DocW, DocH, DocX, DocY: Single;
    DocViewBox: TSvgViewBox;
    DocVpMat: TFloatMatrix;
    DocTargetRect, SavedViewport, TransformedDocTarget: TFloatRect;
    SaveSurfaceClip, DocClipRect: TRect;
    i: Integer;
  begin
    if (AGroupNode is TSvgDocumentNode) and (AGroupNode <> FDocumentRoot) then
    begin

      DocNode := TSvgDocumentNode(AGroupNode);

      DocW := DocNode.GetSizePx(DocNode.Width, FViewportRect.Width);
      DocH := DocNode.GetSizePx(DocNode.Height, FViewportRect.Height);
      DocX := DocNode.GetSizePx(DocNode.X, FViewportRect.Width);
      DocY := DocNode.GetSizePx(DocNode.Y, FViewportRect.Height);

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
        SaveSurfaceClip := ATargetSurface.ClipRect;
        try
          ApplyMatrix(DocVpMat);

          // Clip nested <svg> document viewport if overflow <> ofVisible
          if (DocNode.Overflow <> ofVisible) then
          begin
            TransformedDocTarget := FTransformation.GetTransformedBounds(DocTargetRect);
            DocClipRect := MakeRect(TransformedDocTarget, rrOutside);
            GR32.IntersectRect(DocClipRect, ATargetSurface.ClipRect, DocClipRect);
            ATargetSurface.ClipRect := DocClipRect;
          end;

          for i := 0 to DocNode.Children.Count - 1 do
            RenderNode(ATargetSurface, DocNode.Children[i]);

        finally
          ATargetSurface.ClipRect := SaveSurfaceClip;
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
  OffscreenBmp: TCustomBitmap32;
  GroupBounds, TargetWorldBounds: TFloatRect;
  GroupRoi: TRect;
  Points: array[0..3] of TFloatPoint;
  NeedsOffscreen: Boolean;
  EffectiveBlendMode: TSvgBlendMode;
begin
  if (AGroupNode = nil) or AGroupNode.IsDisplayNone or (not AGroupNode.PassesConditionalProcessing) then
    Exit;

  if (ATarget = nil) then
    Exit;

  // Offscreen rendering required if we have transparency, a ClipPath, a Mask,
  // or if Isolation = isoIsolate.
  // Per W3C SVG 1.1 §14.3.5, offscreen opacity and mask/clip compositing are
  // bypassed when rendering children inside a <clipPath>, but clip-path
  // references on group elements still apply.
  // Per W3C SVG 1.1 §15.2, clip-path, mask, and opacity on the filtered group itself
  // are bypassed during SourceGraphic rasterization and applied after filter execution in RenderFilter.
  if (AGroupNode = FFilteredNode) then
    NeedsOffscreen := False
  else
    NeedsOffscreen := (AGroupNode.ResolvedClipPath <> nil) or
                      ((not IsRenderingClipPath) and
                       ((AGroupNode.Opacity < 1.0) or
                        (AGroupNode.ResolvedMask <> nil) or
                        (AGroupNode.Isolation = isoIsolate)));

  if not NeedsOffscreen then
  begin
    RenderChildren(ATarget);
    exit;
  end;

  // Calculate group bounding box in world space for objectBoundingBox units and ROI determination
  GroupBounds := AGroupNode.GetVisualBoundingBox(FViewportRect);
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

  // Intersect TargetWorldBounds with ATarget.ClipRect so offscreen surface allocations
  // remain bounded strictly to the visible target viewport instead of allocating huge 
  // buffers when zoomed in.
  if not GR32.IntersectRect(TargetWorldBounds, TargetWorldBounds, FloatRect(ATarget.ClipRect)) then
    exit;

  GroupRoi := MakeRect(TargetWorldBounds, rrOutside);

  // Acquire reusable offscreen surface sized strictly to the calculated ROI
  OffscreenBmp := GetOffscreenBitmap(GroupRoi.Width, GroupRoi.Height, True);
  try
    // Translate matrix to ROI coordinate space
    FTransformation.Push;
    Inc(FOffscreenOffset.X, GroupRoi.Left);
    Inc(FOffscreenOffset.Y, GroupRoi.Top);
    try
      FTransformation.Translate(-GroupRoi.Left, -GroupRoi.Top);

      RenderChildren(OffscreenBmp);
    finally
      Dec(FOffscreenOffset.X, GroupRoi.Left);
      Dec(FOffscreenOffset.Y, GroupRoi.Top);
      FTransformation.Pop;
    end;

    // Apply ClipPath in ROI coordinate space
    if (AGroupNode.ResolvedClipPath <> nil) then
      ApplyClipPathNode(OffscreenBmp, AGroupNode.ResolvedClipPath, TargetWorldBounds, GroupRoi);

    // Apply Alpha Mask in ROI coordinate space (bypassed inside clipPath)
    if (not IsRenderingClipPath) and (AGroupNode.ResolvedMask <> nil) then
      ApplyMaskNode(OffscreenBmp, AGroupNode.ResolvedMask, TargetWorldBounds, GroupRoi);

    // Apply Group Opacity (bypassed inside clipPath)
    if (not IsRenderingClipPath) and (AGroupNode.Opacity < 1.0) then
      ApplyOpacity(OffscreenBmp, AGroupNode.Opacity);

    if IsRenderingClipPath then
      EffectiveBlendMode := bmNormal
    else
      EffectiveBlendMode := AGroupNode.MixBlendMode;

    // Blend ROI offscreen surface onto target canvas at group ROI origin
    BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, GroupRoi.Left, GroupRoi.Top);

  finally
    ReleaseOffscreenBitmap(OffscreenBmp);
  end;
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
  FilterContext: TFilterContext;
  i, Count: Integer;
  Node, SavedFilteredNode: TSvgNode;
  SourceBounds, FilterBounds, FilterRegionRect, TargetWorldBounds: TFloatRect;
  PathNode: TSvgPathNode;
  TransformedPoints, StrokePoints: TArrayOfArrayOfFloatPoint;
  StrokeWidth, BBoxWidth, BBoxHeight: Single;
  FilterRegionWidth, FilterRegionHeight: Single;
  NodeBounds, UserFilterRegionRect: TFloatRect;
  Pts: array[0..3] of TFloatPoint;
  StrokeColor: TSvgColor;
  HasStroke: boolean;
  FilterNode: TSvgFilterPrimitiveNode;
  FilterRenderer: TFilterRendererClass;
  PrimRect: TFloatRect;
  SpecifiedROI, SaveTargetClipRect, DestClip, LocalClipRect: TRect;
  IsAxisAligned: Boolean;
  TotalMat, CompositeMat: TFloatMatrixHelper;
  InvMat: TFloatMatrix;
  S_local, OneOverS_local: Single;
  AffineTransform: TAffineTransformation;
  DestBounds: TFloatRect;
begin
  if (AFilterNode = nil) or (ANode = nil) or (ATarget = nil) then
    Exit;

  FilterContext := TFilterContext.Init(Self, ATarget, ANode);
  FilterContext.OffscreenOffset := FOffscreenOffset;

  TotalMat.Matrix := Mult(FTransformation.Matrix, ANode.Transform);
  IsAxisAligned := TotalMat.IsAxisAligned;
  OneOverS_local := 1.0;

  FTransformation.Push;
  try
    if IsAxisAligned then
    begin
      ApplyMatrix(ANode.Transform);
      FilterContext.Scale := TFloatMatrixHelper(FTransformation.Matrix).GetScale;
    end else
    begin
      S_local := TotalMat.GetScale;
      if S_local <= 0 then
        S_local := 1.0
      else
        OneOverS_local := 1 / S_local;
      FTransformation.Clear;
      FTransformation.Scale(S_local, S_local);
      FilterContext.Scale := S_local;
    end;

    // 1. Calculate source bounds in screen/target space at full resolution.

    if ANode is TSvgPathNode then
    begin
      PathNode := TSvgPathNode(ANode);
      NodeBounds := PathNode.GetObjectBoundingBox(FViewportRect);
      TransformedPoints := PathNode.GetPathData(FViewportRect, FTransformation);

      if Length(TransformedPoints) > 0 then
      begin
        SourceBounds := PolyPolygonBounds(TransformedPoints);

        StrokePoints := nil;
        StrokeWidth := PathNode.GetSizePx(PathNode.Stroke.Width, FViewportRect.Width);
        if (PathNode.Stroke.ResolvedPaintServer = nil) then
          StrokeColor := GetEffectiveColor(PathNode, PathNode.Stroke.Color, tcStroke)
        else
          StrokeColor := TSvgColor.Unset;
        HasStroke := (StrokeWidth > 0) and ((PathNode.Stroke.ResolvedPaintServer <> nil) or (StrokeColor.IsVisible));

        if (HasStroke) then
        begin
          // Calculate how far beyond the polygon bounding box the stroke will potentially extend
          StrokeWidth := StrokeWidth * FilterContext.Scale / 2;

          if (PathNode.Stroke.JoinStyle = jsMiter) then
            StrokeWidth := StrokeWidth * Max(1.0, PathNode.Stroke.MiterLimit);

          GR32.InflateRect(SourceBounds, StrokeWidth, StrokeWidth);
        end;

      end else
        SourceBounds := FloatRect(0, 0, 0, 0);

    end else
    begin
      // For non-polygon nodes, calculate object bounds in world/screen space
      NodeBounds := ANode.GetObjectBoundingBox(FViewportRect);
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

    // Calculate filter region defined on AFilterNode (x, y, width, height, filterUnits)
    //
    // Fallback for empty or degenerate group bounding boxes;
    // If GetObjectBoundingBox returns empty/inverted bounds,
    // we fall back to the target canvas/viewport dimensions,
    // ensuring offscreen surfaces are allocated properly
    // rather than disappearing.
    // See identical logic in TSvgRenderer.RenderNode & TSvgRenderer.RenderGroupNode
    if (NodeBounds.IsEmpty) then
      NodeBounds := FViewportRect;

    BBoxWidth := NodeBounds.Right - NodeBounds.Left;
    BBoxHeight := NodeBounds.Bottom - NodeBounds.Top;
    // We have already handled these cases above, but whatever
    if BBoxWidth <= 0 then BBoxWidth := 1.0;
    if BBoxHeight <= 0 then BBoxHeight := 1.0;

    FilterContext.NodeBounds := NodeBounds;
    FilterContext.BBoxWidth := BBoxWidth;
    FilterContext.BBoxHeight := BBoxHeight;
    FilterContext.PrimitiveUnits := AFilterNode.PrimitiveUnits;

    if AFilterNode.FilterUnits = guObjectBoundingBox then
    begin
      // Note: Value returned by ToPixels is always treated as a factor
      UserFilterRegionRect.Left := NodeBounds.Left + AFilterNode.GetSizePx(AFilterNode.X, 1.0) * BBoxWidth;
      UserFilterRegionRect.Top := NodeBounds.Top + AFilterNode.GetSizePx(AFilterNode.Y, 1.0) * BBoxHeight;
      UserFilterRegionRect.Right := UserFilterRegionRect.Left + AFilterNode.GetSizePx(AFilterNode.Width, 1.0) * BBoxWidth;
      UserFilterRegionRect.Bottom := UserFilterRegionRect.Top + AFilterNode.GetSizePx(AFilterNode.Height, 1.0) * BBoxHeight;
    end else
    begin
      UserFilterRegionRect.Left := AFilterNode.GetSizePx(AFilterNode.X, FViewportRect.Width);
      UserFilterRegionRect.Top := AFilterNode.GetSizePx(AFilterNode.Y, FViewportRect.Height);
      UserFilterRegionRect.Right := UserFilterRegionRect.Left + AFilterNode.GetSizePx(AFilterNode.Width, FViewportRect.Width);
      UserFilterRegionRect.Bottom := UserFilterRegionRect.Top + AFilterNode.GetSizePx(AFilterNode.Height, FViewportRect.Height);
    end;

    FilterRegionRect := UserFilterRegionRect;

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

    // If SourceBounds is empty or degenerate (such as when rendering an empty <g filter="..."/> container element),
    // fall back SourceBounds to FilterRegionRect so generator primitives (feFlood, feTurbulence, feTile)
    // or empty element filters render across the filter region.
    if (SourceBounds.IsEmpty) then
      SourceBounds := FilterRegionRect;

    FilterBounds := SourceBounds;

    // 2. Calculate filter-dependent directional margins (e.g. Gaussian blur radius, offset dx/dy)
    // and adjust FilterBounds output rectangle
    for Node in AFilterNode.Children do
    begin
      if not (Node is TSvgFilterPrimitiveNode) then
        Continue;

      FilterNode := TSvgFilterPrimitiveNode(Node);
      FilterRenderer := TFilterRendererClass(FilterNode.GetRenderClass);

      if (FilterRenderer <> nil) then
      begin
        FilterRenderer.AdjustOutputRect(FilterNode, FilterContext, FilterBounds);

        if FilterRenderer.DefaultSubregionIsFilterRegion then
        begin
          if not ([fppX, fppY, fppWidth, fppHeight] <= FilterNode.Specified) then
            GR32.UnionRect(FilterBounds, FilterBounds, FilterRegionRect);
        end;

        if (FilterNode.Specified <> []) then
        begin
          FilterRegionWidth := UserFilterRegionRect.Right - UserFilterRegionRect.Left;
          FilterRegionHeight := UserFilterRegionRect.Bottom - UserFilterRegionRect.Top;

          // --- Resolve X / Left ---
          if fppX in FilterNode.Specified then
          begin
            if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
              PrimRect.Left := NodeBounds.Left + FilterNode.GetSizePx(FilterNode.X, 1.0) * BBoxWidth
            else
              PrimRect.Left := FilterNode.GetSizePx(FilterNode.X, FViewportRect.Width);
          end else
            PrimRect.Left := UserFilterRegionRect.Left;

          // --- Resolve Y / Top ---
          if fppY in FilterNode.Specified then
          begin
            if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
              PrimRect.Top := NodeBounds.Top + FilterNode.GetSizePx(FilterNode.Y, 1.0) * BBoxHeight
            else
              PrimRect.Top := FilterNode.GetSizePx(FilterNode.Y, FViewportRect.Height);
          end else
            PrimRect.Top := UserFilterRegionRect.Top;

          // --- Resolve Width / Right ---
          if fppWidth in FilterNode.Specified then
          begin
            if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
              PrimRect.Right := PrimRect.Left + FilterNode.GetSizePx(FilterNode.Width, 1.0) * BBoxWidth
            else
              PrimRect.Right := PrimRect.Left + FilterNode.GetSizePx(FilterNode.Width, FViewportRect.Width);
          end else
            PrimRect.Right := PrimRect.Left + FilterRegionWidth;

          // --- Resolve Height / Bottom ---
          if fppHeight in FilterNode.Specified then
          begin
            if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
              PrimRect.Bottom := PrimRect.Top + FilterNode.GetSizePx(FilterNode.Height, 1.0) * BBoxHeight
            else
              PrimRect.Bottom := PrimRect.Top + FilterNode.GetSizePx(FilterNode.Height, FViewportRect.Height);
          end else
            PrimRect.Bottom := PrimRect.Top + FilterRegionHeight;

          // Transform PrimRect to world/screen space
          Pts[0] := FTransformation.Transform(FloatPoint(PrimRect.Left, PrimRect.Top));
          Pts[1] := FTransformation.Transform(FloatPoint(PrimRect.Right, PrimRect.Top));
          Pts[2] := FTransformation.Transform(FloatPoint(PrimRect.Right, PrimRect.Bottom));
          Pts[3] := FTransformation.Transform(FloatPoint(PrimRect.Left, PrimRect.Bottom));

          PrimRect := FloatRect(Pts[0].X, Pts[0].Y, Pts[0].X, Pts[0].Y);
          for i := 1 to 3 do
          begin
            if (Pts[i].X < PrimRect.Left) then PrimRect.Left := Pts[i].X;
            if (Pts[i].X > PrimRect.Right) then PrimRect.Right := Pts[i].X;
            if (Pts[i].Y < PrimRect.Top) then PrimRect.Top := Pts[i].Y;
            if (Pts[i].Y > PrimRect.Bottom) then PrimRect.Bottom := Pts[i].Y;
          end;

          GR32.UnionRect(FilterBounds, FilterBounds, PrimRect);
        end;
      end;
    end;

    // 3. Clip FilterBounds to FilterRegionRect to determine total Filter ROI

    if FilterBounds.Left < FilterRegionRect.Left then FilterBounds.Left := FilterRegionRect.Left;
    if FilterBounds.Top < FilterRegionRect.Top then FilterBounds.Top := FilterRegionRect.Top;
    if FilterBounds.Right > FilterRegionRect.Right then FilterBounds.Right := FilterRegionRect.Right;
    if FilterBounds.Bottom > FilterRegionRect.Bottom then FilterBounds.Bottom := FilterRegionRect.Bottom;

    if (FilterBounds.IsEmpty) then
      Exit;

    // Clip FilterBounds to visible viewport ATarget.ClipRect (both axis-aligned and non-axis-aligned)
    // to bound offscreen surface allocations (FilterContext.ROI) strictly to the visible area.
    if IsAxisAligned then
    begin
      if not GR32.IntersectRect(FilterBounds, FilterBounds, FloatRect(ATarget.ClipRect)) then
        Exit;
    end else
    begin
      // For non-axis-aligned filters (e.g. rotate/skew), transform ATarget.ClipRect from screen space
      // back into local filter user space (where FTransformation = Scale(S_local, S_local)) and clip FilterBounds.
      CompositeMat.Matrix := IdentityMatrix;
      CompositeMat.Scale(OneOverS_local, OneOverS_local);
      CompositeMat := CompositeMat * TotalMat;

      if CompositeMat.Determinant <> 0 then
      begin
        InvMat := CompositeMat.Matrix;
        GR32_Transforms.Invert(InvMat);

        AffineTransform := TAffineTransformation.Create;
        try
          AffineTransform.Clear(InvMat);
          DestBounds := AffineTransform.GetTransformedBounds(FloatRect(ATarget.ClipRect));
          if not GR32.IntersectRect(FilterBounds, FilterBounds, DestBounds) then
            Exit;
        finally
          AffineTransform.Free;
        end;
      end;
    end;

    if (FilterBounds.IsEmpty) then
      Exit;

    FilterContext.ROI := MakeRect(FilterBounds, rrOutside);

    // Protect against too large subregions -> Exception when allocating surface bitmaps
    if (Int64(FilterContext.ROI.Width) * Int64(FilterContext.ROI.Height) > TCustomBitmap32.MaxPixelCount) then
      exit;

    // Verify whether the filter ROI is visible on screen before surface allocation.
    if IsAxisAligned then
    begin
      if not GR32.IntersectRect(FilterContext.ROI, FilterContext.ROI, ATarget.ClipRect) then
        Exit;
    end;

    // 4. Allocate intermediate filter surfaces constrained to the ROI dimensions in full screen resolution
    FilterContext.SourceGraphic.Bitmap := GetOffscreenBitmap(FilterContext.ROI.Width, FilterContext.ROI.Height, True);
    try

      // Render element into SourceGraphic in ROI coordinate space.
      // Track FFilteredNode := ANode so child rendering methods bypass ANode's own
      // clip-path, mask, opacity, and mix-blend-mode when generating SourceGraphic.
      // Track FOffscreenOffset so child elements, gradients, and patterns map to the clipped surface.
      FTransformation.Push;
      SavedFilteredNode := FFilteredNode;
      FFilteredNode := ANode;
      Inc(FOffscreenOffset.X, FilterContext.ROI.Left);
      Inc(FOffscreenOffset.Y, FilterContext.ROI.Top);
      try
        FTransformation.Translate(-FilterContext.ROI.Left, -FilterContext.ROI.Top);

        RenderNodeContent(FilterContext.SourceGraphic.Bitmap, ANode);
      finally
        Dec(FOffscreenOffset.X, FilterContext.ROI.Left);
        Dec(FOffscreenOffset.Y, FilterContext.ROI.Top);
        FFilteredNode := SavedFilteredNode;
        FTransformation.Pop;
      end;

      FilterContext.SourceGraphic.Region := Rect(0, 0, FilterContext.ROI.Width, FilterContext.ROI.Height);

      FilterContext.CurrentSurface := FilterContext.SourceGraphic;
      FilterContext.UnnamedSurface := nil; // Indicates that CurrentSurface's ownership is outside the filter loop

      // Count number of named surfaces so we can preallocate the surface array
      Count := 0;
      for Node in AFilterNode.Children do
        if (Node is TSvgFilterPrimitiveNode) and TSvgFilterPrimitiveNode(Node).IsReferenceTarget then
          Inc(Count);
      SetLength(FilterContext.NamedSurfaces, Count);
      Count := 0;

      // 5. Process filter primitive nodes sequentially on ROI surfaces
      for Node in AFilterNode.Children do
      begin
        if not (Node is TSvgFilterPrimitiveNode) then
          Continue;

        FilterNode := TSvgFilterPrimitiveNode(Node);

        // Get the renderer that handles the filter node
        FilterRenderer := TFilterRendererClass(FilterNode.GetRenderClass);

        // Calculate primitive subregion bounds (x, y, width, height) per
        // W3C SVG 1.1 Specification §15.5 and W3C Filter Effects Module Level 1 §7.1.
        //
        // - For primitiveUnits="objectBoundingBox", lengths evaluate relative
        //   to the target element bounding box (BBoxWidth, BBoxHeight).
        //   A percentage value like 120% means 1.2*Value and a unitless value
        //   like 1.2 also means 1.2*Value.
        //
        // - For primitiveUnits="userSpaceOnUse", percentage lengths (including
        //   defaults 0%, 0%, 100%, 100%) evaluate relative to the filter
        //   region bounds (UserFilterRegionRect), while absolute lengths
        //   express user-space coordinates.
        //
        // - For omitted subregion attributes (fppX, fppY, fppWidth, fppHeight
        //   not in FilterNode.Specified):
        //
        //   1) Generator and tile primitives (where FilterRenderer's
        //      DefaultSubregionIsFilterRegion = True, e.g. feTile, feFlood,
        //      feTurbulence, feImage), default omitted dimensions to (0%, 0%,
        //      100%, 100%) of the filter region (full ROI).
        //
        //   2) Other primitives default omitted dimensions to the bounding
        //      union of their input surface region(s).
        //

        if (FilterRenderer <> nil) then
        begin
          FilterContext.CurrentSubregion := FilterRenderer.GetRoiRect(FilterNode, FilterContext);

          if (FilterContext.CurrentSubregion.IsEmpty) then
          begin
            if (FilterContext.CurrentSurface.Bitmap <> FilterContext.SourceGraphic.Bitmap) then
            begin
              FilterContext.CurrentSurface := TSurface.Empty;
              ReleaseOffscreenBitmap(FilterContext.UnnamedSurface);
            end;
            FilterRenderer := nil;
          end else
          begin
            // 1. Resolve user-space / objectBoundingBox coordinates upfront for specified dimensions
            FilterRegionWidth := UserFilterRegionRect.Right - UserFilterRegionRect.Left;
            FilterRegionHeight := UserFilterRegionRect.Bottom - UserFilterRegionRect.Top;

            // --- Resolve X / Left ---
            if fppX in FilterNode.Specified then
            begin
              if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
                PrimRect.Left := NodeBounds.Left + FilterNode.GetSizePx(FilterNode.X, 1.0) * BBoxWidth
              else
                PrimRect.Left := FilterNode.GetSizePx(FilterNode.X, FViewportRect.Width);
            end else
              // Un-specified: Left will be inherited from RefRoiRect.Left after transformation
              PrimRect.Left := UserFilterRegionRect.Left;

            // --- Resolve Y / Top ---
            if fppY in FilterNode.Specified then
            begin
              if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
                PrimRect.Top := NodeBounds.Top + FilterNode.GetSizePx(FilterNode.Y, 1.0) * BBoxHeight
              else
                PrimRect.Top := FilterNode.GetSizePx(FilterNode.Y, FViewportRect.Height);
            end else
              // Un-specified: Top will be inherited from RefRoiRect.Top after transformation
              PrimRect.Top := UserFilterRegionRect.Top;

            // --- Resolve Width / Right ---
            if fppWidth in FilterNode.Specified then
            begin
              if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
                PrimRect.Right := PrimRect.Left + FilterNode.GetSizePx(FilterNode.Width, 1.0) * BBoxWidth
              else
                PrimRect.Right := PrimRect.Left + FilterNode.GetSizePx(FilterNode.Width, FViewportRect.Width);
            end else
              PrimRect.Right := PrimRect.Left + FilterRegionWidth;

            // --- Resolve Height / Bottom ---
            if fppHeight in FilterNode.Specified then
            begin
              if (AFilterNode.PrimitiveUnits = guObjectBoundingBox) then
                PrimRect.Bottom := PrimRect.Top + FilterNode.GetSizePx(FilterNode.Height, 1.0) * BBoxHeight
              else
                PrimRect.Bottom := PrimRect.Top + FilterNode.GetSizePx(FilterNode.Height, FViewportRect.Height);
            end else
              PrimRect.Bottom := PrimRect.Top + FilterRegionHeight;
          end;
        end;

        if (FilterRenderer <> nil) then
        begin

          // 2. Transform PrimRect to world/screen space and clip to FilterRegionRect
          Pts[0] := FTransformation.Transform(FloatPoint(PrimRect.Left, PrimRect.Top));
          Pts[1] := FTransformation.Transform(FloatPoint(PrimRect.Right, PrimRect.Top));
          Pts[2] := FTransformation.Transform(FloatPoint(PrimRect.Right, PrimRect.Bottom));
          Pts[3] := FTransformation.Transform(FloatPoint(PrimRect.Left, PrimRect.Bottom));

          PrimRect := FloatRect(Pts[0].X, Pts[0].Y, Pts[0].X, Pts[0].Y);
          for i := 1 to 3 do
          begin
            if (Pts[i].X < PrimRect.Left) then PrimRect.Left := Pts[i].X;
            if (Pts[i].X > PrimRect.Right) then PrimRect.Right := Pts[i].X;
            if (Pts[i].Y < PrimRect.Top) then PrimRect.Top := Pts[i].Y;
            if (Pts[i].Y > PrimRect.Bottom) then PrimRect.Bottom := Pts[i].Y;
          end;

          // Convert PrimRect to ROI-relative integer TRect
          SpecifiedROI := MakeRect(FloatRect(
            PrimRect.Left - FilterContext.ROI.Left,
            PrimRect.Top - FilterContext.ROI.Top,
            PrimRect.Right - FilterContext.ROI.Left,
            PrimRect.Bottom - FilterContext.ROI.Top), rrOutside);

          // 3. Combine specified dimensions with CurrentSubregion fallback
          if fppX in FilterNode.Specified then
          begin
            FilterContext.CurrentSubregion.Left := SpecifiedROI.Left;
            FilterContext.CurrentSubregion.Right := SpecifiedROI.Right;
          end else
          if fppWidth in FilterNode.Specified then
          begin
            FilterContext.CurrentSubregion.Right := FilterContext.CurrentSubregion.Left + SpecifiedROI.Width;
          end;

          if fppY in FilterNode.Specified then
          begin
            FilterContext.CurrentSubregion.Top := SpecifiedROI.Top;
            FilterContext.CurrentSubregion.Bottom := SpecifiedROI.Bottom;
          end else
          if fppHeight in FilterNode.Specified then
          begin
            FilterContext.CurrentSubregion.Bottom := FilterContext.CurrentSubregion.Top + SpecifiedROI.Height;
          end;

          // Clip FilterContext.CurrentSubregion to the visible target clip rect mapped to local ROI space,
          // ensuring offscreen surface bitmap allocations in filter primitive renderers (e.g. feFlood, feGaussianBlur)
          // remain small and bounded strictly to the visible canvas area when zoomed in.
          if IsAxisAligned then
          begin
            LocalClipRect := ATarget.ClipRect;
            GR32.OffsetRect(LocalClipRect, -FilterContext.ROI.Left, -FilterContext.ROI.Top);
            if not GR32.IntersectRect(FilterContext.CurrentSubregion, FilterContext.CurrentSubregion, LocalClipRect) then
              FilterContext.CurrentSubregion := Default(TRect);
          end else
          begin
            // For non-axis-aligned filters (e.g. rotated elements), FilterContext.ROI is in local user space.
            // Intersect CurrentSubregion with local surface bounds (0, 0, ROI.Width, ROI.Height) so subregion
            // allocations remain strictly bounded to the local surface size.
            if not GR32.IntersectRect(FilterContext.CurrentSubregion, FilterContext.CurrentSubregion, Rect(0, 0, FilterContext.ROI.Width, FilterContext.ROI.Height)) then
              FilterContext.CurrentSubregion := Default(TRect);
          end;

          (*
          ** Render the filter
          *)
          if (not FilterContext.CurrentSubregion.IsEmpty) and
             (Int64(FilterContext.CurrentSubregion.Width) * Int64(FilterContext.CurrentSubregion.Height) <= TCustomBitmap32.MaxPixelCount) then
          begin

            FilterRenderer.Render(Self, FilterNode, FilterContext);

          end;

        end else
        begin
          // Unsupported filter; Keep the current intermediate result and let
          // the next filter continue with it.
          if (FilterContext.CurrentSurface.Bitmap = FilterContext.SourceGraphic.Bitmap) then
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


        // CurrentSurface.Bitmap=SourceGraphic.Bitmap will be true if no filter has yet produced anything.
        if (FilterContext.CurrentSurface.Bitmap <> FilterContext.SourceGraphic.Bitmap) then
        begin
          // Note that FilterContext.CurrentSurface.Bitmap can be nil here if the
          // filter didn't produce anything.

          // UnnamedSurface should be nil, but release anything it points to just
          // in case the filter failed to do it.
          ReleaseOffscreenBitmap(FilterContext.UnnamedSurface);

          if FilterNode.IsReferenceTarget then
          begin
            FilterContext.UnnamedSurface := nil; // Transfer ownership of the bitmap to NamedSurfaces[]
            FilterContext.NamedSurfaces[Count] := FilterContext.CurrentSurface;
            Inc(Count);
          end else
            // Transfer ownership of the bitmap to UnnamedSurface
            FilterContext.UnnamedSurface := FilterContext.CurrentSurface.Bitmap;
        end;
      end;

      if (FilterContext.UnnamedSurface <> FilterContext.CurrentSurface.Bitmap) then
        ReleaseOffscreenBitmap(FilterContext.UnnamedSurface);

      // 6. Blend final filtered result surface onto target canvas at full resolution,
      //    clipped to the filter region rect per W3C SVG 1.1 Specification §15.5.
      if (FilterContext.CurrentSurface.Bitmap <> FilterContext.SourceGraphic.Bitmap) then
      begin
        FTransformation.Push;
        try
          ApplyMatrix(ANode.Transform);

          // Compute TargetWorldBounds for clip path / mask resolution
          Pts[0] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Top));
          Pts[1] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Top));
          Pts[2] := FTransformation.Transform(FloatPoint(NodeBounds.Right, NodeBounds.Bottom));
          Pts[3] := FTransformation.Transform(FloatPoint(NodeBounds.Left, NodeBounds.Bottom));

          TargetWorldBounds := FloatRect(Pts[0].X, Pts[0].Y, Pts[0].X, Pts[0].Y);
          for i := 1 to 3 do
          begin
            if (Pts[i].X < TargetWorldBounds.Left) then TargetWorldBounds.Left := Pts[i].X;
            if (Pts[i].X > TargetWorldBounds.Right) then TargetWorldBounds.Right := Pts[i].X;
            if (Pts[i].Y < TargetWorldBounds.Top) then TargetWorldBounds.Top := Pts[i].Y;
            if (Pts[i].Y > TargetWorldBounds.Bottom) then TargetWorldBounds.Bottom := Pts[i].Y;
          end;

          FTransformation.Translate(-FilterContext.ROI.Left, -FilterContext.ROI.Top);

          // Per W3C SVG 1.1 §15.2 ("Filter Effects Overview"):
          // "Note that 'clip-path', 'mask' and 'opacity' apply to the target element AFTER 'filter' is applied."
          // Apply target element clip-path, mask, and opacity directly onto the filtered result surface
          // before compositing onto the target canvas.
          if (ANode.ResolvedClipPath <> nil) then
            ApplyClipPathNode(FilterContext.CurrentSurface.Bitmap, ANode.ResolvedClipPath, TargetWorldBounds, FilterContext.ROI, False);

          if (ANode.ResolvedMask <> nil) then
            ApplyMaskNode(FilterContext.CurrentSurface.Bitmap, ANode.ResolvedMask, TargetWorldBounds, FilterContext.ROI);

          if (ANode.Opacity < 1.0) then
            ApplyOpacity(FilterContext.CurrentSurface.Bitmap, ANode.Opacity);

        finally
          FTransformation.Pop;
        end;

        SaveTargetClipRect := ATarget.ClipRect;
        try

          if IsAxisAligned then
          begin
            ATarget.ClipRect := MakeRect(FilterRegionRect, rrOutside);

            BlendOffscreenSurface(ATarget, FilterContext.CurrentSurface.Bitmap, ANode.MixBlendMode,
              FilterContext.ROI.Left + FilterContext.CurrentSurface.Region.Left,
              FilterContext.ROI.Top + FilterContext.CurrentSurface.Region.Top);
          end else
          begin
            // Non-axis-aligned filter composition (skew/rotation on element or ancestors):
            // Per W3C SVG 1.1 §15.2, filter effects evaluate in local user coordinate space
            // before element/ancestor affine transforms.
            CompositeMat.Matrix := IdentityMatrix;
            CompositeMat.Translate(FilterContext.ROI.Left + FilterContext.CurrentSurface.Region.Left,
                                   FilterContext.ROI.Top + FilterContext.CurrentSurface.Region.Top);
            CompositeMat.Scale(OneOverS_local, OneOverS_local);
            CompositeMat := CompositeMat * TotalMat;

            AffineTransform := TAffineTransformation.Create;
            try
              AffineTransform.Clear(CompositeMat.Matrix);
              DestBounds := AffineTransform.GetTransformedBounds(FloatRect(0, 0,
                FilterContext.CurrentSurface.Bitmap.Width, FilterContext.CurrentSurface.Bitmap.Height));
              DestClip := MakeRect(DestBounds, rrOutside);

              if GR32.IntersectRect(DestClip, SaveTargetClipRect, DestClip) then
              begin
{$if defined(SVG32_HIQ_AFFINE_TRANSFORM)}
                Transform(ATarget, FilterContext.CurrentSurface.Bitmap, AffineTransform, DestClip, True, Resampler[rqHigh]);
{$else}
                Transform(ATarget, FilterContext.CurrentSurface.Bitmap, AffineTransform, DestClip, True, Resampler[rqNormal]);
{$ifend}
              end;
            finally
              AffineTransform.Free;
            end;
          end;

        finally
          ATarget.ClipRect := SaveTargetClipRect;
        end;
      end;

    finally
      FilterContext.Finalize;
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
//      TSvgRenderer.RenderImage
//
//------------------------------------------------------------------------------
procedure TSvgRenderer.RenderImage(ATarget: TCustomBitmap32; const AHref: UTF8String; const ATargetRect: TFloatRect; const APreserveAspectRatio: TSvgPreserveAspectRatio);
var
  WidthPx, HeightPx: Single;
  isSvg: Boolean;
  Stream: TMemoryStream;
  SubDoc: TSvgDocumentNode;
  SubRenderer: TSvgRenderer;
  Bitmap: TBitmap32;
  AspectMat, TotalMat: TFloatMatrixHelper;
  DestBounds, SubViewportRect: TFloatRect;
  DestRect, DestClip, SaveTargetClipRect, ImageClipRect: TRect;
  SourceViewBox: TSvgViewBox;
  UTF8Bytes: UTF8String;
  hRef, MimeType: TValuePUtf8Char;
  s: string;
begin
  if (ATarget = nil) or (AHref = '') then
    Exit;

  if (ATargetRect.Right <= ATargetRect.Left) or (ATargetRect.Bottom <= ATargetRect.Top) then
    Exit;

  Stream := TMemoryStream.Create;
  try
    hRef := TValuePUtf8Char.FromString(AHref);

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

      if (not DoResolveExternalFile(s, Stream)) then
        exit;

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
      SubDoc := ParseSvgXml(PAnsiChar(Stream.Memory), Stream.Size);
      if SubDoc = nil then
        exit;

      try
        SubDoc.Resolve;

        // W3C SVG 1.1 Specification §11.6 & §11.8:
        // An <image> element referencing an SVG document establishes a new viewport defined
        // by the x, y, width, and height attributes of the <image> element.
        if SubDoc.ViewBox.IsValid then
        begin
          // 1. Referenced SVG has a valid viewBox: map viewBox onto <image> target viewport
          //    using preserveAspectRatio transformation rules.
          SourceViewBox := SubDoc.ViewBox;
          AspectMat.Matrix := SourceViewBox.GetTransform(ATargetRect, APreserveAspectRatio);
          SubViewportRect := SourceViewBox;
        end else
        begin
          // 2. Referenced SVG has NO viewBox: per W3C SVG 1.1 §11.6, preserveAspectRatio is ignored,
          //    and the embedded SVG image's top-left corner is aligned with the top-left corner of the
          //    viewport established by the <image> element without scaling.
          WidthPx := SubDoc.GetSizePx(SubDoc.Width, ATargetRect.Width);
          HeightPx := SubDoc.GetSizePx(SubDoc.Height, ATargetRect.Height);
          if WidthPx <= 0 then
            WidthPx := ATargetRect.Width;
          if HeightPx <= 0 then
            HeightPx := ATargetRect.Height;

          AspectMat.Matrix := IdentityMatrix;
          AspectMat.Translate(ATargetRect.Left, ATargetRect.Top);
          SubViewportRect := FloatRect(0, 0, WidthPx, HeightPx);
        end;

        // Establish viewport clip rect for the <image> element in target screen space
        SaveTargetClipRect := ATarget.ClipRect;
        ImageClipRect := MakeRect(FTransformation.GetTransformedBounds(ATargetRect), rrOutside);
        if not GR32.IntersectRect(ImageClipRect, SaveTargetClipRect, ImageClipRect) then
          Exit;

        FTransformation.Push;
        try
          ApplyMatrix(AspectMat.Matrix);

          ATarget.ClipRect := ImageClipRect;

          SubRenderer := TSvgRenderer.Create;
          try
            SubRenderer.FRecursionDepth := FRecursionDepth; // Let's close this little recursion loophole

            // Set FDocumentRoot so SubDoc is recognized as the root document of the sub-renderer
            // rather than a nested <svg> element, preventing duplicate viewBox transform applications.
            SubRenderer.FDocumentRoot := SubDoc;
            SubRenderer.AllowExternalImages := FAllowExternalImages;
            SubRenderer.Transformation.Clear(FTransformation.Matrix);
            SubRenderer.ViewportRect := SubViewportRect;

            SubRenderer.RenderNode(ATarget, SubDoc);
          finally
            SubRenderer.Free;
          end;
        finally
          ATarget.ClipRect := SaveTargetClipRect;
          FTransformation.Pop;
        end;
      finally
        SubDoc.Free;
      end;
    end else
    begin
      // Let TBitmap32's image format manager have a go at it. We'll fail with
      // an exception here if no registered image format readers can process
      // the data.
      // Do not use a bitmap from the pool, or at least don't return it to the
      // pool; We don't know what the LoadFromStream does to the bitmap, so we
      // can't guarantee that it can be used elsewhere.
      Bitmap := TBitmap32.Create;
      try
        Bitmap.LoadFromStream(Stream);

        if (Bitmap.Empty) then
          exit;

        Bitmap.DrawMode := dmBlend;
        Bitmap.CombineMode := cmMerge;

        SourceViewBox := TSvgViewBox.Create(0, 0, Bitmap.Width, Bitmap.Height);
        AspectMat.Matrix := SourceViewBox.GetTransform(ATargetRect, APreserveAspectRatio);
        TotalMat := AspectMat * FTransformation.Matrix;

        FTransformation.Push;
        try
          FTransformation.Clear(TotalMat.Matrix);

          DestBounds := FTransformation.GetTransformedBounds(FloatRect(0, 0, Bitmap.Width, Bitmap.Height));
          DestClip := MakeRect(DestBounds, rrOutside);

          // If the target is axis-aligned then we don't need to do a transformation.
          // Instead we can do a fast BlockTransfer or StretchTransfer. We might be
          // off by a fractional pixel, but that's not really noticeable.
          if (TotalMat.IsAxisAligned) then
          begin
            DestRect := MakeRect(DestBounds, rrClosest);

            // Check if 1:1 scale and integer alignment -> fast 1:1 BlockTransfer
            if (Abs(TotalMat.Matrix[0, 0] - 1.0) < 1e-4) and (Abs(TotalMat.Matrix[1, 1] - 1.0) < 1e-4) and
               (Abs(TotalMat.Matrix[2, 0] - Round(TotalMat.Matrix[2, 0])) < 1e-4) and (Abs(TotalMat.Matrix[2, 1] - Round(TotalMat.Matrix[2, 1])) < 1e-4) then
              BlockTransfer(ATarget, DestRect.Left, DestRect.Top, DestClip, Bitmap, Bitmap.BoundsRect, dmBlend)
            else
              StretchTransfer(ATarget, DestRect, DestClip, Bitmap, Bitmap.BoundsRect, Resampler[rqNormal], dmBlend);
          end else
            // Transform and render onto target
            Transform(ATarget, Bitmap, FTransformation, DestClip, True, Resampler[rqNormal]);

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

//------------------------------------------------------------------------------

procedure TSvgRenderer.RenderImageNode(ATarget: TCustomBitmap32; AImageNode: TSvgImageNode);
var
  WidthPx, HeightPx, xPx, yPx: Single;
  TargetRect: TFloatRect;
begin
  if (ATarget = nil) or (AImageNode = nil) then
    Exit;

  WidthPx := AImageNode.GetSizePx(AImageNode.Width, FViewportRect.Width);
  HeightPx := AImageNode.GetSizePx(AImageNode.Height, FViewportRect.Height);
  if (WidthPx <= 0) or (HeightPx <= 0) then
    Exit;

  xPx := AImageNode.GetSizePx(AImageNode.X, FViewportRect.Width);
  yPx := AImageNode.GetSizePx(AImageNode.Y, FViewportRect.Height);
  TargetRect := FloatRect(xPx, yPx, xPx + WidthPx, yPx + HeightPx);

  RenderImage(ATarget, AImageNode.Href, TargetRect, AImageNode.PreserveAspectRatio);
end;

//------------------------------------------------------------------------------

function TSvgRenderer.DoResolveExternalFile(const Filename: string; Stream: TStream): boolean;
begin
  Result := False;
  if (Assigned(FOnResolveExternalFile)) then
    FOnResolveExternalFile(Self, Filename, Stream, Result);
end;

//------------------------------------------------------------------------------

procedure TSvgRenderer.DoResolveFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo);
begin
  if (Assigned(FOnResolveFontEvent)) then
    FOnResolveFontEvent(Self, string(AFontFamily), string(AWeightStr), string(AStyleStr), ASize, AFontInfo);
end;

procedure TSvgRenderer.ResolveFont(const AFontFamily, AWeightStr, AStyleStr: UTF8String; ASize: integer; var AFontInfo: TFontInfo);
var
  Parser: TValuePUtf8Char;
  Token: TValuePUtf8Char;
  PrimaryCandidate: TValuePUtf8Char;
begin
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

  // Let the user have a go at it
  DoResolveFont(AFontFamily, AWeightStr, AStyleStr, ASize, AFontInfo);
end;

//------------------------------------------------------------------------------

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
  FillPaintServer, StrokePaintServer: TSvgPaintServerNode;
begin
  if (Length(APathPoints) = 0) or (ATarget = nil) then
    Exit;

  AccumulatedOpacity := GetAccumulatedTextOpacity(ANode);
  FillOpacity := ANode.Fill.Opacity * AccumulatedOpacity;
  StrokeOpacity := ANode.Stroke.Opacity * AccumulatedOpacity;

  if (ANode.Fill.Opacity > 0) then
    FillColor := GetEffectiveColor(ANode, ANode.Fill.Color, tcFill)
  else
    FillColor := TSvgColor.Unset;
  FillPaintServer := ANode.Fill.ResolvedPaintServer;
  HasFill := (FillPaintServer <> nil) or ((FillColor.IsVisible) and (FillOpacity > 0));

  StrokeWidth := ANode.GetSizePx(ANode.Stroke.Width, FViewportRect.Width);
  if (ANode.Stroke.Opacity > 0) and (StrokeWidth > 0) then
    StrokeColor := GetEffectiveColor(ANode, ANode.Stroke.Color, tcStroke)
  else
    StrokeColor := TSvgColor.Unset;
  StrokePaintServer := ANode.Stroke.ResolvedPaintServer;
  HasStroke := (StrokeWidth > 0) and ((StrokePaintServer <> nil) or ((StrokeColor.IsVisible) and (StrokeOpacity > 0)));

  if (not HasFill) and (not HasStroke) then
    exit;

  TransformedPts := GetTransformedPoints(APathPoints);
  var ObjBox := ANode.GetObjectBoundingBox(FViewportRect);

  // 1. Fill Rendering
  if (HasFill) then
  begin
    if (FillPaintServer <> nil) and (not FillPaintServer.ShouldApply(ObjBox)) then
      FillPaintServer := nil;

    RenderPolyPolygon(ATarget, FillPaintServer, FillOpacity, FillColor, TransformedPts, ANode.Fill.FillRule);
  end;

  // 2. Stroke Rendering
  if (HasStroke) then
  begin
    MatScale := TFloatMatrixHelper(FTransformation.Matrix).GetScale;
    StrokeWidth := StrokeWidth * MatScale;

    ScaledDashArray := nil;
    ScaledOffset := 0;
    if Length(ANode.Stroke.DashArray) > 0 then
    begin
      SetLength(ScaledDashArray, Length(ANode.Stroke.DashArray));
      for k := 0 to High(ANode.Stroke.DashArray) do
        ScaledDashArray[k] := ANode.Stroke.DashArray[k] * MatScale;
      ScaledOffset := ANode.GetSizePx(ANode.Stroke.DashOffset, FViewportRect) * MatScale;
    end;

    StrokePts := nil;
    for i := 0 to High(TransformedPts) do
    begin
      if Length(ScaledDashArray) > 0 then
      begin
        DashedPts := BuildDashedLine(TransformedPts[i], ScaledDashArray, ScaledOffset, True);
        for j := 0 to High(DashedPts) do
          StrokePts := StrokePts + BuildPolyPolyLine([DashedPts[j]], False, StrokeWidth, ANode.Stroke.JoinStyle, ANode.Stroke.EndStyle, ANode.Stroke.MiterLimit);
      end else
        StrokePts := StrokePts + BuildPolyPolyLine([TransformedPts[i]], True, StrokeWidth, ANode.Stroke.JoinStyle, ANode.Stroke.EndStyle, ANode.Stroke.MiterLimit);
    end;

    if (StrokePts <> nil) then
    begin
      if (StrokePaintServer <> nil) and (not StrokePaintServer.ShouldApply(ObjBox)) then
        StrokePaintServer := nil;

      RenderPolyPolygon(ATarget, StrokePaintServer, StrokeOpacity, StrokeColor, StrokePts);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSvgRenderer.RenderTextAreaNode(ATarget: TCustomBitmap32; ATextAreaNode: TSvgTextAreaNode);
var
  Text: UnicodeString;
  TextRect: TFloatRect;
  FontSizePx: Integer;
  FontInfo: TFontInfo;
  TextLayout: TTextLayout;
  Canvas: TCustomCanvas32;
  TempBitmap: TBitmap32;
  BitmapWithFont: TCustomBitmap32;
begin
  if (ATextAreaNode = nil) or ATextAreaNode.IsDisplayNone or (not ATextAreaNode.Visible) then
    Exit;

  if (ATarget = nil) then
    Exit;

  TextRect.Left := ATextAreaNode.GetSizePx(ATextAreaNode.X, FViewportRect.Width);
  TextRect.Top := ATextAreaNode.GetSizePx(ATextAreaNode.Y, FViewportRect.Height);
  TextRect.Right := TextRect.Left + ATextAreaNode.GetSizePx(ATextAreaNode.Width, FViewportRect.Width);
  TextRect.Bottom := TextRect.Top + ATextAreaNode.GetSizePx(ATextAreaNode.Height, FViewportRect.Height);

  if (TextRect.IsEmpty) then
    Exit;

  Text := string(ATextAreaNode.TextContent);
  if (Text = '') or (ATextAreaNode.Children.Count > 0) then
    Text := GetSubtreeText(ATextAreaNode);

  if (Text = '') then
    Exit;

  TempBitmap := nil;
  try
    if (Supports(ATarget.Backend, IFontSupport)) then
      BitmapWithFont := ATarget
    else
    if (FTarget is TBitmap32) or (Supports(FTarget.Backend, IFontSupport)) then
      BitmapWithFont := FTarget
    else
    begin
      TempBitmap := TBitmap32.Create;
      BitmapWithFont := TempBitmap;
    end;

    Canvas := TCustomCanvas32.Create(BitmapWithFont);
    try
      Canvas.BeginLockUpdate;

      FontSizePx := Round(ATextAreaNode.GetFontSizePx);
      if FontSizePx <= 0 then
        FontSizePx := 12;

      FontInfo := Default(TFontInfo);
      ResolveFont(ATextAreaNode.FontFamily, ATextAreaNode.FontWeight, ATextAreaNode.FontStyle, FontSizePx, FontInfo);

      Canvas.Font.Name := string(FontInfo.FontFamily);
      Canvas.Font.Name := string(FontInfo.FontFamily);
      Canvas.Font.Height := -Max(1, FontInfo.Size);
      Canvas.Font.Style := FontInfo.Style;

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
  finally
    TempBitmap.Free;
  end;
end;

procedure TSvgRenderer.RenderTextNode(ATarget: TCustomBitmap32; ATextNode: TSvgTextNode);

  procedure ComputeTextLengthAdjust(
    ANode: TSvgTextPositioningNode;
    RefWidth, NaturalWidth: Single;
    CharCount: Integer;
    out ExtraSpacing, ScaleFactor: Single;
    out LengthAdjust: TSvgLengthAdjust);
  var
    TargetLength: Single;
  begin
    ExtraSpacing := 0.0;
    ScaleFactor := 1.0;
    LengthAdjust := ANode.LengthAdjust;

    if ANode.HasTextLength then
    begin
      TargetLength := ANode.GetSizePx(ANode.TextLength, RefWidth);
      if (TargetLength > 0) and (NaturalWidth > 0) then
      begin
        if LengthAdjust = laSpacingAndGlyphs then
        begin
          ScaleFactor := TargetLength / NaturalWidth;
        end else
        begin // laSpacing
          if CharCount > 1 then
            ExtraSpacing := (TargetLength - NaturalWidth) / (CharCount - 1);
        end;
      end;
    end;
  end;

  procedure MeasureTextSubtree(
    Canvas: TCustomCanvas32;
    ParentNode, CurrentNode: TSvgTextPositioningNode;
    RefLen: Single;
    var TotalWidth: Single;
    var CharCount: Integer);
  var
    Child: TSvgNode;
    Text: string;
    CharWidth, DefaultAdvance, LetterSpacing, ChildTargetLength: Single;
    FontSizePx: Integer;
    FontInfo: TFontInfo;
    TextLayout: TTextLayout;
    FontFaceMetrics: TFontFaceMetrics32;
    GlyphMetrics: TGlyphStringMetrics32;
    i: Integer;
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
      if (CurrentNode <> ParentNode) and CurrentNode.HasTextLength then
      begin
        ChildTargetLength := CurrentNode.GetSizePx(CurrentNode.TextLength, RefLen);
        TotalWidth := TotalWidth + ChildTargetLength;
        Inc(CharCount);
      end else
      begin
        Text := string(CurrentNode.TextContent);
        if (Text <> '') then
        begin
          FontSizePx := Round(CurrentNode.GetFontSizePx);
          if FontSizePx <= 0 then
            FontSizePx := 12;

          FontInfo := Default(TFontInfo);
          ResolveFont(CurrentNode.FontFamily, CurrentNode.FontWeight, CurrentNode.FontStyle, FontSizePx, FontInfo);

          Canvas.Font.Name := string(FontInfo.FontFamily);
          Canvas.Font.Height := -Max(1, FontInfo.Size);
          Canvas.Font.Style := FontInfo.Style;

          TextLayout := DefaultTextLayout;
          TextLayout.ClipLayout := False;
          TextLayout.AlignmentHorizontal := TextAlignHorLeft;
          TextLayout.AlignmentVertical := TextAlignVerTop;

          if Canvas.GetFontFaceMetrics(TextLayout, FontFaceMetrics) and (FontFaceMetrics.EMSpaceWidth > 0) then
            DefaultAdvance := FontFaceMetrics.EMSpaceWidth
          else
            DefaultAdvance := FontSizePx * 0.5;

          Canvas.GetGlyphMetrics(Text, GlyphMetrics);

          LetterSpacing := CurrentNode.GetLetterSpacingPx(RefLen);

          for i := 0 to Length(Text)-1 do
          begin
            Inc(CharCount);
            if (CurrentNode.HasDx) and (i <= High(CurrentNode.DxArray)) then
              TotalWidth := TotalWidth + CurrentNode.GetSizePx(CurrentNode.DxArray[i], RefLen);

            if (i <= High(GlyphMetrics)) and GlyphMetrics[i].Valid then
              CharWidth := GlyphMetrics[i].AdvanceX + LetterSpacing
            else
              CharWidth := DefaultAdvance + LetterSpacing;

            TotalWidth := TotalWidth + CharWidth;
          end;
        end;

        for Child in CurrentNode.Children do
        begin
          if Child is TSvgTextPositioningNode then
            MeasureTextSubtree(Canvas, ParentNode, TSvgTextPositioningNode(Child), RefLen, TotalWidth, CharCount);
        end;
      end;
    finally
      if HasSubtreeTransform then
        FTransformation.Pop;
    end;
  end;

  procedure RenderTextPathSubtree(
    Canvas: TCustomCanvas32;
    ParentNode, CurrentNode: TSvgTextPositioningNode;
    const PathPts: TArrayOfArrayOfFloatPoint;
    var ActiveDy: Single;
    TotalLen: Single;
    var CurrentDistance: Single;
    ExtraSpacing, ScaleFactor: Single;
    LengthAdjust: TSvgLengthAdjust);
  var
    Child: TSvgNode;
    Text: string;
    NaturalCharWidth, CharWidth, DefaultAdvance, TangAngle, DrawY, RadAngle, NormX, NormY, BaseX, BaseY, DxVal, LetterSpacing: Single;
    FontSizePx: Integer;
    FontInfo: TFontInfo;
    TextLayout: TTextLayout;
    Point: TFloatPoint;
    RotationMat: TFloatMatrixHelper;
    i: Integer;
    FontFaceMetrics: TFontFaceMetrics32;
    GlyphMetrics: TGlyphStringMetrics32;
    RotateArray: TArrayOfFloat;
    CharacterRotationAngle: Single;
    HasSubtreeTransform: Boolean;
    ChildExtraSpacing, ChildScaleFactor: Single;
    ChildLengthAdjust: TSvgLengthAdjust;
    NodeExtraSpacing, NodeScaleFactor: Single;
    NodeLengthAdjust: TSvgLengthAdjust;
    ChildNaturalWidth: Single;
    ChildCharCount: Integer;
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
      if (CurrentNode <> ParentNode) and CurrentNode.HasTextLength then
      begin
        ChildNaturalWidth := 0.0;
        ChildCharCount := 0;
        MeasureTextSubtree(Canvas, CurrentNode, CurrentNode, TotalLen, ChildNaturalWidth, ChildCharCount);
        ComputeTextLengthAdjust(CurrentNode, TotalLen, ChildNaturalWidth, ChildCharCount, ChildExtraSpacing, ChildScaleFactor, ChildLengthAdjust);
        NodeExtraSpacing := ChildExtraSpacing;
        NodeScaleFactor := ChildScaleFactor;
        NodeLengthAdjust := ChildLengthAdjust;
      end else
      begin
        NodeExtraSpacing := ExtraSpacing;
        NodeScaleFactor := ScaleFactor;
        NodeLengthAdjust := LengthAdjust;
      end;

      Text := string(CurrentNode.TextContent);
      if (Text <> '') then
      begin
        FontSizePx := Round(CurrentNode.GetFontSizePx);
        if FontSizePx <= 0 then
          FontSizePx := 12;

        FontInfo := Default(TFontInfo);
        ResolveFont(CurrentNode.FontFamily, CurrentNode.FontWeight, CurrentNode.FontStyle, FontSizePx, FontInfo);

        Canvas.Font.Name := string(FontInfo.FontFamily);
        Canvas.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;

        if Canvas.GetFontFaceMetrics(TextLayout, FontFaceMetrics) then
        begin
          DrawY := -FontFaceMetrics.Ascent;
          if (FontFaceMetrics.EMSpaceWidth > 0) then
            DefaultAdvance := FontFaceMetrics.EMSpaceWidth
          else
            DefaultAdvance := FontSizePx * 0.5;
        end else
        begin
          DrawY := 0;
          DefaultAdvance := FontSizePx * 0.5;
        end;

        Canvas.GetGlyphMetrics(Text, GlyphMetrics);

        if (Length(CurrentNode.Rotate) = 0) and (CurrentNode.Parent <> nil) and (CurrentNode.Parent is TSvgTextPositioningNode) then
          RotateArray := TSvgTextPositioningNode(CurrentNode.Parent).Rotate
        else
          RotateArray := CurrentNode.Rotate;

        LetterSpacing := CurrentNode.GetLetterSpacingPx(FViewportRect.Width);

        CharacterRotationAngle := 0;
        for i := 0 to Length(Text)-1 do
        begin
          if (CurrentNode.HasDx) and (i <= High(CurrentNode.DxArray)) then
          begin
            DxVal := CurrentNode.GetSizePx(CurrentNode.DxArray[i], TotalLen);
            if (NodeLengthAdjust = laSpacingAndGlyphs) then
              DxVal := DxVal * NodeScaleFactor;
            CurrentDistance := CurrentDistance + DxVal;
          end;

          if (CurrentNode.HasDy) and (i <= High(CurrentNode.DyArray)) then
            ActiveDy := ActiveDy + CurrentNode.GetSizePx(CurrentNode.DyArray[i], FViewportRect.Height);

          if (i <= High(RotateArray)) then
            CharacterRotationAngle := RotateArray[i];

          if (i <= High(GlyphMetrics)) and GlyphMetrics[i].Valid then
            NaturalCharWidth := GlyphMetrics[i].AdvanceX + LetterSpacing
          else
            NaturalCharWidth := DefaultAdvance + LetterSpacing;

          if (NodeLengthAdjust = laSpacingAndGlyphs) then
            CharWidth := NaturalCharWidth * NodeScaleFactor
          else
            CharWidth := NaturalCharWidth;

          if (Text[i+1] <> ' ') and (GetPointAndTangentAtDistance(PathPts, CurrentDistance + CharWidth * 0.5, Point, TangAngle)) then
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
            if (NodeLengthAdjust = laSpacingAndGlyphs) and (NodeScaleFactor <> 1.0) then
              RotationMat.Scale(NodeScaleFactor, 1.0);
            RotationMat.Rotate(TangAngle + CharacterRotationAngle);
            RotationMat.Translate(BaseX, BaseY);

            FTransformation.Push;
            try
              ApplyMatrix(RotationMat.Matrix);
              Canvas.Clear;
              Canvas.BeginUpdate;

              // Vectorize a single glyph...
              Canvas.RenderText(-NaturalCharWidth * 0.5, DrawY, Text[i+1], TextLayout);

              // ...and rasterize it
              if (Canvas.Path <> nil) then
                RenderTextPathData(ATarget, Canvas.Path, CurrentNode);

              Canvas.Clear;
              Canvas.EndUpdate;
            finally
              FTransformation.Pop;
            end;
          end;

          CurrentDistance := CurrentDistance + CharWidth + NodeExtraSpacing;
        end;
      end;

      for Child in CurrentNode.Children do
      begin
        if Child is TSvgTextPositioningNode then
          RenderTextPathSubtree(Canvas, ParentNode, TSvgTextPositioningNode(Child), PathPts, ActiveDy, TotalLen, CurrentDistance, NodeExtraSpacing, NodeScaleFactor, NodeLengthAdjust);
      end;
    finally
      if HasSubtreeTransform then
        FTransformation.Pop;
    end;
  end;

  // Evaluates path arc length and places glyphs at interpolated path distance points
  // aligned to segment tangent orientation angles.
  procedure ProcessTextPathNode(ANode: TSvgTextPathNode; Canvas: TCustomCanvas32);
  var
    PathPts: TArrayOfArrayOfFloatPoint;
    TotalLen, Offset, CurrentDistance, NaturalWidth, TextWidth, ExtraSpacing, ScaleFactor: Single;
    CharCount: Integer;
    HasNodeTransform: Boolean;
    ActiveDy: Single;
    LengthAdjust: TSvgLengthAdjust;
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

      FTransformation.Push;
      try

        FTransformation.Clear(ANode.ResolvedPathNode.Transform);

        PathPts := ANode.ResolvedPathNode.GetPathData(FViewportRect, FTransformation);

      finally
        FTransformation.Pop;
      end;

      if Length(PathPts) = 0 then
        Exit;

      TotalLen := GetTotalPathLength(PathPts);
      if TotalLen <= 0 then
        Exit;

      Offset := ANode.GetSizePx(ANode.StartOffset, TotalLen);

      NaturalWidth := 0.0;
      CharCount := 0;
      MeasureTextSubtree(Canvas, ANode, ANode, TotalLen, NaturalWidth, CharCount);

      ComputeTextLengthAdjust(ANode, FViewportRect.Width, NaturalWidth, CharCount, ExtraSpacing, ScaleFactor, LengthAdjust);

      if ANode.HasTextLength then
        TextWidth := ANode.GetSizePx(ANode.TextLength, FViewportRect.Width)
      else
        TextWidth := NaturalWidth;

      if (ANode.TextAnchor <> taStart) then
      begin
        if (ANode.TextAnchor in [taMiddle, taEnd]) then
        begin
          case ANode.TextAnchor of
            taMiddle: Offset := Offset - TextWidth * 0.5;
            taEnd: Offset := Offset - TextWidth;
          end;
        end;
      end;

      CurrentDistance := Offset;
      ActiveDy := 0;
      RenderTextPathSubtree(Canvas, ANode, ANode, PathPts, ActiveDy, TotalLen, CurrentDistance, ExtraSpacing, ScaleFactor, LengthAdjust);
    finally
      if HasNodeTransform then
        FTransformation.Pop;
    end;
  end;

  procedure ProcessTextPositioningNode(
    ANode: TSvgTextPositioningNode;
    Canvas: TCustomCanvas32;
    var Cursor: TFloatPoint;
    InheritedExtraSpacing: Single = 0.0;
    InheritedScaleFactor: Single = 1.0;
    InheritedLengthAdjust: TSvgLengthAdjust = laSpacing);
  var
    Child: TSvgNode;
    Text: string;
    NaturalCharWidth, CharWidth, DefaultAdvance, LetterSpacing, DxVal, DyVal: Single;
    FontSizePx: Integer;
    FontInfo: TFontInfo;
    TextLayout: TTextLayout;
    DrawPoint: TFloatPoint;
    TransformMat: TFloatMatrixHelper;
    HasNodeTransform: Boolean;
    i, CharCount: Integer;
    CharacterRotationAngle: Single;
    FontFaceMetrics: TFontFaceMetrics32;
    GlyphMetrics: TGlyphStringMetrics32;
    NaturalWidth, TargetLength, EffectiveTextWidth: Single;
    ActiveExtraSpacing, ActiveScaleFactor: Single;
    ActiveLengthAdjust: TSvgLengthAdjust;
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
        Cursor.X := ANode.GetSizePx(ANode.X, FViewportRect.Width);
      if ANode.HasDx then
        Cursor.X := Cursor.X + ANode.GetSizePx(ANode.Dx, FViewportRect.Width);

      if ANode.HasY then
        Cursor.Y := ANode.GetSizePx(ANode.Y, FViewportRect.Height);
      if ANode.HasDy then
        Cursor.Y := Cursor.Y + ANode.GetSizePx(ANode.Dy, FViewportRect.Height);

      // Compute local textLength and lengthAdjust parameters if specified on ANode
      if ANode.HasTextLength then
      begin
        NaturalWidth := 0.0;
        CharCount := 0;
        MeasureTextSubtree(Canvas, ANode, ANode, FViewportRect.Width, NaturalWidth, CharCount);
        ComputeTextLengthAdjust(ANode, FViewportRect.Width, NaturalWidth, CharCount, ActiveExtraSpacing, ActiveScaleFactor, ActiveLengthAdjust);
        TargetLength := ANode.GetSizePx(ANode.TextLength, FViewportRect.Width);
        EffectiveTextWidth := TargetLength;
      end else
      begin
        ActiveExtraSpacing := InheritedExtraSpacing;
        ActiveScaleFactor := InheritedScaleFactor;
        ActiveLengthAdjust := InheritedLengthAdjust;
        EffectiveTextWidth := 0.0;
      end;

      Text := string(ANode.TextContent);
      if (Text <> '') then
      begin
        FontSizePx := Round(ANode.GetFontSizePx);
        if FontSizePx <= 0 then
          FontSizePx := 12;

        FontInfo := Default(TFontInfo);
        ResolveFont(ANode.FontFamily, ANode.FontWeight, ANode.FontStyle, FontSizePx, FontInfo);

        Canvas.Font.Name := string(FontInfo.FontFamily);
        Canvas.Font.Height := -Max(1, FontInfo.Size);
        Canvas.Font.Style := FontInfo.Style;

        TextLayout := DefaultTextLayout;
        TextLayout.ClipLayout := False;
        TextLayout.AlignmentHorizontal := TextAlignHorLeft;
        TextLayout.AlignmentVertical := TextAlignVerTop;
        TextLayout.RemoveLeadingSpace := True;

        if Canvas.GetFontFaceMetrics(TextLayout, FontFaceMetrics) then
        begin
          DrawPoint.Y := Cursor.Y - FontFaceMetrics.Ascent;
          if (FontFaceMetrics.EMSpaceWidth > 0) then
            DefaultAdvance := FontFaceMetrics.EMSpaceWidth
          else
            DefaultAdvance := FontSizePx * 0.5;
        end else
        begin
          DrawPoint.Y := Cursor.Y;
          DefaultAdvance := FontSizePx * 0.5;
        end;

        Canvas.GetGlyphMetrics(Text, GlyphMetrics);

        LetterSpacing := ANode.GetLetterSpacingPx(FViewportRect.Width);

        if not ANode.HasTextLength then
        begin
          NaturalWidth := 0.0;
          for i := 0 to High(GlyphMetrics) do
          begin
            if GlyphMetrics[i].Valid then
              NaturalCharWidth := GlyphMetrics[i].AdvanceX + LetterSpacing
            else
              NaturalCharWidth := DefaultAdvance + LetterSpacing;

            if ActiveLengthAdjust = laSpacingAndGlyphs then
              CharWidth := NaturalCharWidth * ActiveScaleFactor
            else
              CharWidth := NaturalCharWidth;

            NaturalWidth := NaturalWidth + CharWidth + ActiveExtraSpacing;
          end;
          EffectiveTextWidth := NaturalWidth;
        end;

        case ANode.TextAnchor of
          taMiddle: DrawPoint.X := Cursor.X - EffectiveTextWidth * 0.5;
          taEnd:    DrawPoint.X := Cursor.X - EffectiveTextWidth;
        else
          DrawPoint.X := Cursor.X;
        end;

        CharacterRotationAngle := 0.0;

        for i := 0 to Length(Text)-1 do
        begin
          if (ANode.HasDx) and (i <= High(ANode.DxArray)) then
          begin
            DxVal := ANode.GetSizePx(ANode.DxArray[i], FViewportRect.Width);
            if ActiveLengthAdjust = laSpacingAndGlyphs then
              DxVal := DxVal * ActiveScaleFactor;
            DrawPoint.X := DrawPoint.X + DxVal;
          end;

          if (ANode.HasDy) and (i <= High(ANode.DyArray)) then
          begin
            DyVal := ANode.GetSizePx(ANode.DyArray[i], FViewportRect.Height);
            DrawPoint.Y := DrawPoint.Y + DyVal;
          end;

          // Get the rotation angle. If there's too few we just reuse the previous (we know there's at least one)
          if (i <= High(ANode.Rotate)) then
            CharacterRotationAngle := ANode.Rotate[i];

          if (i <= High(GlyphMetrics)) and GlyphMetrics[i].Valid then
            NaturalCharWidth := GlyphMetrics[i].AdvanceX + LetterSpacing
          else
            NaturalCharWidth := DefaultAdvance + LetterSpacing;

          if ActiveLengthAdjust = laSpacingAndGlyphs then
            CharWidth := NaturalCharWidth * ActiveScaleFactor
          else
            CharWidth := NaturalCharWidth;

          if (Text[i+1] <> ' ') then
          begin
            TransformMat.Matrix := IdentityMatrix;

            if (ActiveLengthAdjust = laSpacingAndGlyphs) and (ActiveScaleFactor <> 1.0) then
            begin
              TransformMat.Translate(-DrawPoint.X, -DrawPoint.Y);
              TransformMat.Scale(ActiveScaleFactor, 1.0);
              if CharacterRotationAngle <> 0 then
                TransformMat.Rotate(CharacterRotationAngle);
              TransformMat.Translate(DrawPoint.X, DrawPoint.Y);
            end else
            if CharacterRotationAngle <> 0 then
              TransformMat.Rotate(DrawPoint.X, DrawPoint.Y, CharacterRotationAngle);

            FTransformation.Push;
            try
              if not IsIdentityMatrix(TransformMat.Matrix) then
                ApplyMatrix(TransformMat.Matrix);

              Canvas.Clear;
              Canvas.BeginUpdate;

              // TODO : We should batch all glyphs into a single polypolygon and rasterize them in one go

              // Vectorize a single glyph...
              Canvas.RenderText(DrawPoint.X, DrawPoint.Y, Text[i+1], TextLayout);

              // ...and rasterize it
              if (Canvas.Path <> nil) then
                RenderTextPathData(ATarget, Canvas.Path, ANode);

              Canvas.Clear;
              Canvas.EndUpdate;
            finally
              FTransformation.Pop;
            end;
          end;

          DrawPoint.X := DrawPoint.X + CharWidth + ActiveExtraSpacing;
        end;

        Cursor.X := DrawPoint.X;
      end;

      for Child in ANode.Children do
      begin
        if Child is TSvgTextPathNode then
          ProcessTextPathNode(TSvgTextPathNode(Child), Canvas)
        else
        if Child is TSvgTSpanNode then
          ProcessTextPositioningNode(TSvgTSpanNode(Child), Canvas, Cursor, ActiveExtraSpacing, ActiveScaleFactor, ActiveLengthAdjust)
        else
        if Child is TSvgTextPositioningNode then
          ProcessTextPositioningNode(TSvgTextPositioningNode(Child), Canvas, Cursor, ActiveExtraSpacing, ActiveScaleFactor, ActiveLengthAdjust);
      end;
    finally
      if HasNodeTransform then
        FTransformation.Pop;
    end;
  end;

var
  Canvas: TCustomCanvas32;
  TempBitmap: TBitmap32;
  BitmapWithFont: TCustomBitmap32;
  Cursor: TFloatPoint;
begin
  // RenderTextNode decomposes <text> and <tspan> elements into vector path outlines using TCanvas32.
  // It applies font properties, measures text width for text-anchor alignment, updates cursor positions,
  // and paints vector glyph geometry with solid/gradient/pattern fills and strokes.

  if (ATextNode = nil) or ATextNode.IsDisplayNone or (not ATextNode.Visible) then
    Exit;

  if (ATarget = nil) then
    exit;

  // We can't be sure that the FTarget bitmap has a backend that supports
  // the Font property and the ATarget bitmap is most likely an off-screen
  // cached bitmap with a TMemoryBackend. If none of them can support Font
  // then we create a temporary bitmap that does.
  TempBitmap := nil;
  try
    if (Supports(ATarget.Backend, IFontSupport)) then
      BitmapWithFont := ATarget
    else
    if (FTarget is TBitmap32) or (Supports(FTarget.Backend, IFontSupport)) then
      BitmapWithFont := FTarget
    else
    begin
      TempBitmap := TBitmap32.Create;
      BitmapWithFont := TempBitmap;
    end;

    Canvas := TCustomCanvas32.Create(BitmapWithFont);
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
  finally
    TempBitmap.Free;
  end;
end;

procedure TSvgRenderer.RenderNodeContent(ATarget: TCustomBitmap32; ANode: TSvgNode);
begin
  // TODO : Replace with direct dispatch
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
  if ANode is TSvgCustomGroupNode then
    RenderGroupNode(ATarget, TSvgCustomGroupNode(ANode));
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
  OffscreenBmp: TCustomBitmap32;
  EffectiveBlendMode: TSvgBlendMode;
  NodeBounds, TargetWorldBounds: TFloatRect;
  NodeRoi: TRect;
  Points: array[0..3] of TFloatPoint;
  k: Integer;
  NeedsOffscreen: Boolean;
begin
  if (ATarget = nil) then
    exit;

  if (ANode = nil) or ANode.IsDisplayNone or (not ANode.Visible) or (not ANode.IsRenderable) or (not ANode.PassesConditionalProcessing) then
    Exit;

  if (ANode.IsRendering) then
    exit;

  // Guard against self-referencing clip-path, clip-mask etc.
  if (FRecursionDepth > cMaxRecursions) then
    Exit;

  ANode.IsRendering := True;
  Inc(FRecursionDepth);
  try

    // Filter processing takes precedence over direct element rendering.
    // Per W3C SVG 1.1 §14.3.5, filter processing is bypassed when evaluating
    // child elements of a <clipPath>.
    if (ANode.FilterID <> '') and (not IsRenderingClipPath) then
    begin
      // Exit without rendering when a filter ID is specified but not
      // resolved (W3C SVG 1.1 §15.2).
      if ANode.ResolvedFilter <> nil then
        RenderFilter(ATarget, ANode.ResolvedFilter, ANode);
      Exit;
    end;

    EffectiveBlendMode := GetEffectiveMixBlendMode(ANode);

    // Non-group renderable nodes need offscreen surface if they have ClipPath,
    // Mask, Opacity < 1, or Non-normal blend mode.
    // Per W3C SVG 1.1 §14.3.5, offscreen opacity/mask/clip surface allocation
    // is bypassed inside a <clipPath>.
    // Per W3C SVG 1.1 §15.2, bypass offscreen surface creation for the node being filtered when generating SourceGraphic
    if (ANode = FFilteredNode) then
      NeedsOffscreen := False
    else
      NeedsOffscreen := (not IsRenderingClipPath) and (not (ANode is TSvgCustomGroupNode)) and
        ((EffectiveBlendMode <> bmNormal) or (ANode.ResolvedClipPath <> nil) or (ANode.ResolvedMask <> nil) or (ANode.Opacity < 1.0));

    // Polygon nodes (TSvgPathNode) handle their own ROI calculation after stroking inside RenderPathNode
    if NeedsOffscreen and (ATarget <> nil) and (not (ANode is TSvgPathNode)) then
    begin
      FTransformation.Push;
      try
        ApplyMatrix(ANode.Transform);

        // Calculate object bounding box in world space
        NodeBounds := ANode.GetVisualBoundingBox(FViewportRect);
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

        if not GR32.IntersectRect(TargetWorldBounds, TargetWorldBounds, FloatRect(ATarget.ClipRect)) then
          Exit;

        NodeRoi := MakeRect(TargetWorldBounds, rrOutside);

        OffscreenBmp := GetOffscreenBitmap(NodeRoi.Width, NodeRoi.Height, True);
        try
          Inc(FOffscreenOffset.X, NodeRoi.Left);
          Inc(FOffscreenOffset.Y, NodeRoi.Top);
          FTransformation.Push;
          try
            FTransformation.Translate(-NodeRoi.Left, -NodeRoi.Top);

            RenderNodeContent(OffscreenBmp, ANode);
          finally
            Dec(FOffscreenOffset.X, NodeRoi.Left);
            Dec(FOffscreenOffset.Y, NodeRoi.Top);
            FTransformation.Pop;
          end;

          // Apply ClipPath
          if (ANode.ResolvedClipPath <> nil) then
            ApplyClipPathNode(OffscreenBmp, ANode.ResolvedClipPath, TargetWorldBounds, NodeRoi);

          // Apply Alpha Mask
          if (ANode.ResolvedMask <> nil) then
            ApplyMaskNode(OffscreenBmp, ANode.ResolvedMask, TargetWorldBounds, NodeRoi);

          // Apply Opacity
          if (ANode.Opacity < 1.0) then
            ApplyOpacity(OffscreenBmp, ANode.Opacity);

          BlendOffscreenSurface(ATarget, OffscreenBmp, EffectiveBlendMode, NodeRoi.Left, NodeRoi.Top);
        finally
          ReleaseOffscreenBitmap(OffscreenBmp);
        end;
      finally
        FTransformation.Pop;
      end;
      Exit;
    end;

    RenderNodeUnfiltered(ATarget, ANode);

  finally
    Dec(FRecursionDepth);
    ANode.IsRendering := False;
  end;
end;

function TSvgRenderer.GetDocumentBounds(ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect; ATarget: TCustomBitmap32): TFloatRect;
var
  vpMat: TFloatMatrix;
  viewBox: TSvgViewBox;
  docW, docH: Single;
  Aspect: TSvgPreserveAspectRatio;
  UserVisualBounds, RenderedBounds, ClippedRenderedBounds: TFloatRect;
begin
  if (ADoc = nil) then
    Exit(FloatRect(0, 0, 0, 0));

  if FOverridePreserveAspectRatio then
    Aspect := FPreserveAspectRatioOverride
  else
    Aspect := ADoc.PreserveAspectRatio;

  docW := ADoc.GetSizePx(ADoc.Width, ATargetRect.Width);
  docH := ADoc.GetSizePx(ADoc.Height, ATargetRect.Height);

  viewBox := ADoc.ViewBox;
  if not viewBox.IsValid then
  begin
    if (docW > 0) and (docH > 0) then
      viewBox := TSvgViewBox.Create(0, 0, docW, docH)
    else
    if (ATarget <> nil) then
      viewBox := TSvgViewBox.Create(0, 0, ATarget.Width, ATarget.Height)
    else
      viewBox := TSvgViewBox.Create(0, 0, ATargetRect.Width, ATargetRect.Height);
  end;

  // 1. Calculate actual visual geometry bounds in user space
  UserVisualBounds := ADoc.GetVisualBoundingBox(FloatRect(viewBox.X, viewBox.Y, viewBox.X + viewBox.Width, viewBox.Y + viewBox.Height));

  // 2. Compute viewBox-to-target transform matrix
  vpMat := viewBox.GetTransform(ATargetRect, Aspect);

  // 3. Transform user space visual bounds to target screen space
  FTransformation.Push;
  try
    FTransformation.Clear(VpMat);
    RenderedBounds := FTransformation.GetTransformedBounds(UserVisualBounds);
  finally
    FTransformation.Pop;
  end;

  // 4. If overflow <> ofVisible, clip rendered bounds to ATargetRect
  if (ADoc.Overflow <> ofVisible) then
  begin
    if GR32.IntersectRect(ClippedRenderedBounds, RenderedBounds, ATargetRect) then
      RenderedBounds := ClippedRenderedBounds
    else
      RenderedBounds := ATargetRect;
  end;

  Result := RenderedBounds;
end;

procedure TSvgRenderer.RenderDocument(ATarget: TCustomBitmap32; ADoc: TSvgDocumentNode; const ATargetRect: TFloatRect);
var
  vpMat: TFloatMatrix;
  viewBox: TSvgViewBox;
  docW, docH: Single;
  Aspect: TSvgPreserveAspectRatio;
  SaveClip: TRect;
  DocClipRect: TRect;
begin
  if (ADoc = nil) or (ATarget = nil) then
    Exit;

  FDocumentRoot := ADoc;
  FViewportRect := ATargetRect;

  if FOverridePreserveAspectRatio then
    Aspect := FPreserveAspectRatioOverride
  else
    Aspect := ADoc.PreserveAspectRatio;

  docW := ADoc.GetSizePx(ADoc.Width, ATargetRect.Width);
  docH := ADoc.GetSizePx(ADoc.Height, ATargetRect.Height);

  viewBox := ADoc.ViewBox;
  if not viewBox.IsValid then
  begin
    if (docW > 0) and (docH > 0) then
      viewBox := TSvgViewBox.Create(0, 0, docW, docH)
    else
      viewBox := TSvgViewBox.Create(0, 0, ATarget.Width, ATarget.Height);
  end;

  FViewportRect := FloatRect(viewBox.X, viewBox.Y, viewBox.X + viewBox.Width, viewBox.Y + viewBox.Height);

  vpMat := viewBox.GetTransform(ATargetRect, Aspect);

  FTransformation.Clear;

  FTransformation.Push;
  try
    ApplyMatrix(vpMat);

    ATarget.BeginLockUpdate;
    SaveClip := ATarget.ClipRect;
    try
      // Per W3C SVG 1.1 §14.3.3 ("The 'overflow' property"), root document content is clipped
      // to the target viewport rectangle unless overflow="visible" is explicitly specified.
      if (ADoc.Overflow <> ofVisible) then
      begin
        DocClipRect := MakeRect(ATargetRect, rrOutside);
        GR32.IntersectRect(DocClipRect, ATarget.ClipRect, DocClipRect);
        ATarget.ClipRect := DocClipRect;
      end;

      FTarget := ATarget; // Save root bitmap so we get access to its backend if we need it
      try

        RenderNode(ATarget, ADoc);

      finally
        FTarget := nil;
      end;

    finally
      ATarget.ClipRect := SaveClip;
      ATarget.EndLockUpdate;
    end;
    ATarget.Changed; // Bunch all updates up into a single whole-scene update

  finally
    FTransformation.Pop;
  end;
end;

procedure TSvgRenderer.RenderDocument(ATarget: TCustomBitmap32; ADoc: TSvgDocumentNode);
begin
  if (ATarget <> nil) then
    RenderDocument(ATarget, ADoc, FloatRect(0, 0, ATarget.Width, ATarget.Height));
end;

end.
