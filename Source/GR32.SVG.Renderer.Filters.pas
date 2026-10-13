unit GR32.SVG.Renderer.Filters;

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
  GR32_Filters,
  GR32_Transforms,
  GR32_OrdinalMaps,
  GR32.SVG.Types,
  GR32.SVG.Tree,
  GR32.SVG.Renderer;

type
//------------------------------------------------------------------------------
//
//      TSurface
//
//------------------------------------------------------------------------------
// Encapsulates an intermediate filter surface bitmap together with its active
// subregion rectangle (in local ROI surface coordinates), as defined by W3C
// SVG 1.1 Specification §15.5 ("Filter primitive subregion").
//
// The Region field specifies the rectangle, relative to RenderData.ROI.TopLeft,
// that the surface occupies within the overall filter region of interest.
// For full-ROI surfaces (like SourceGraphic), Region covers the entire ROI
// rectangle (0, 0, ROI.Width, ROI.Height).
// For intermediate sub-surfaces created by primitives with specific subregion
// attributes (x, y, width, height), Region records the subregion rectangle.
//------------------------------------------------------------------------------
  TSurface = record
    Bitmap: TCustomBitmap32;
    Region: TRect;
    class function Create(ABitmap: TCustomBitmap32; const ARegion: TRect): TSurface; static;
    class function Empty: TSurface; static;
    function IsEmpty: Boolean;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRenderData
//
//------------------------------------------------------------------------------
// Filter rendering context
//------------------------------------------------------------------------------
type
  TNamedSurfaces = TArray<TSurface>;

  TFilterRenderData = record
    SourceGraphic: TSurface;
    SourceAlpha: TSurface;
    CurrentSurface: TSurface;
    CurrentSubregion: TRect; // The cached result of Renderer.GetRoiRect
    UnnamedSurface: TCustomBitmap32;
    ROI: TRect;
    Scale: Single;
    NamedSurfaces: TNamedSurfaces;
  end;

//------------------------------------------------------------------------------
//
//      TFilterRenderer
//
//------------------------------------------------------------------------------
// Abstract base class for filter renderers
//------------------------------------------------------------------------------
type
  TFilterRenderer = class abstract
  protected
    class function ResolveSurface(const AInput: TSvgFilterInput; const RenderData: TFilterRenderData; DefaultFallback: TCustomBitmap32 = nil): TSurface; static;
    class function NeedsSubSurface(Bitmap: TCustomBitmap32; const Subregion: TRect): boolean; static;
    class procedure CopySurfaceRegion(const ASrc: TSurface; ADst: TCustomBitmap32; const ADstRegion: TRect; CombineOp: TDrawMode = dmOpaque); static;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; virtual;
    class function GetRoiRect(Node: TSvgFilterPrimitiveNode; const RenderData: TFilterRenderData): TRect; virtual;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); virtual; abstract;
    class function DefaultSubregionIsFilterRegion: Boolean; virtual;
  end;

  TFilterRendererClass = class of TFilterRenderer;


//------------------------------------------------------------------------------
//
//      TFilterRendererGaussianBlur
//
//------------------------------------------------------------------------------
// feGaussianBlur
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeGaussianBlurNode
//------------------------------------------------------------------------------
type
  TFilterRendererGaussianBlur = class(TFilterRenderer)
  private
    class procedure ApplyBlur(Renderer: TSvgRenderer; Input, Output: TCustomBitmap32; RadiusX, RadiusY: Single); static;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererColorMatrix
//
//------------------------------------------------------------------------------
// feColorMatrix
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeColorMatrixNode
//------------------------------------------------------------------------------
type
  TFilterRendererColorMatrix = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererBlend
//
//------------------------------------------------------------------------------
// feBlend
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeBlendNode
//------------------------------------------------------------------------------
type
  TFilterRendererBlend = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererComposite
//
//------------------------------------------------------------------------------
// feComposite
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeCompositeNode
//------------------------------------------------------------------------------
type
  TFilterRendererComposite = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererMerge
//
//------------------------------------------------------------------------------
// feMerge
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeMergeNode
//------------------------------------------------------------------------------
type
  TFilterRendererMerge = class(TFilterRenderer)
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
    class function GetRoiRect(Node: TSvgFilterPrimitiveNode; const RenderData: TFilterRenderData): TRect; override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererOffset
//
//------------------------------------------------------------------------------
// feOffset
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeOffsetNode
//------------------------------------------------------------------------------
type
  TFilterRendererOffset = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererFlood
//
//------------------------------------------------------------------------------
// feFlood
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeFloodNode
//------------------------------------------------------------------------------
type
  TFilterRendererFlood = class(TFilterRenderer)
  protected
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
    class function DefaultSubregionIsFilterRegion: Boolean; override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererMorphology
//
//------------------------------------------------------------------------------
// feMorphology
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeMorphologyNode
//------------------------------------------------------------------------------
type
  TFilterRendererMorphology = class(TFilterRenderer)
  private
    class procedure ApplyMorphology(Input, Output: TCustomBitmap32; Op: TSvgMorphologyOperator; RadiusX, RadiusY: Integer; TempBitmap: TCustomBitmap32 = nil); static;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererComponentTransfer
//
//------------------------------------------------------------------------------
// feComponentTransfer
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeComponentTransferNode
//------------------------------------------------------------------------------
type
  TFilterRendererComponentTransfer = class(TFilterRenderer)
  private
    class procedure BuildLUT(FuncNode: TSvgFeFuncNode; var LUT: TLUT8);
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererDropShadow
//
//------------------------------------------------------------------------------
// feDropShadow
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeDropShadowNode
//------------------------------------------------------------------------------
type
  TFilterRendererDropShadow = class(TFilterRenderer)
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererTurbulence
//
//------------------------------------------------------------------------------
// feTurbulence
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeTurbulenceNode
//------------------------------------------------------------------------------
type
  TFilterRendererTurbulence = class(TFilterRenderer)
  protected
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
    class function DefaultSubregionIsFilterRegion: Boolean; override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererDisplacementMap
//
//------------------------------------------------------------------------------
// feDisplacementMap
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeDisplacementMapNode
//------------------------------------------------------------------------------
type
  TFilterRendererDisplacementMap = class(TFilterRenderer)
  private
    class function GetChannelValue(const Color: TColor32Entry; Channel: TSvgChannelSelector): Byte; static;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
  end;


//------------------------------------------------------------------------------
//
//      TFilterRendererTile
//
//------------------------------------------------------------------------------
// feTile
//------------------------------------------------------------------------------
// Filter primitive renderer for TSvgFeTileNode
//------------------------------------------------------------------------------
type
  TFilterRendererTile = class(TFilterRenderer)
  protected
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
    class function DefaultSubregionIsFilterRegion: Boolean; override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_LowLevel,
  GR32_Blend,
  GR32_Math,
  GR32_Resamplers,
  GR32.Blur,
  GR32.Transpose,
  GR32.Noise.Perlin,
  GR32.Blend.Modes,
  GR32.Blend.Modes.PorterDuff;

type
  TLUT8 = GR32_Filters.TLUT8; // TLUT8 is also declared in GR32_Blend :-(

const
  OneOver255: Single = 1 / 255;

//------------------------------------------------------------------------------
//
//      TSurface
//
//------------------------------------------------------------------------------
class function TSurface.Create(ABitmap: TCustomBitmap32; const ARegion: TRect): TSurface;
begin
  Result.Bitmap := ABitmap;
  Result.Region := ARegion;
end;

class function TSurface.Empty: TSurface;
begin
  Result := Default(TSurface);
end;

function TSurface.IsEmpty: Boolean;
begin
  Result := (Bitmap = nil) or (Bitmap.Empty) or (Region.IsEmpty);
end;


//------------------------------------------------------------------------------
//
//      TFilterRenderer
//
//------------------------------------------------------------------------------

class function TFilterRenderer.DefaultSubregionIsFilterRegion: Boolean;
begin
  Result := False;
end;

class function TFilterRenderer.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
begin
  Result := Default(TFloatRect);
end;

class function TFilterRenderer.GetRoiRect(Node: TSvgFilterPrimitiveNode; const RenderData: TFilterRenderData): TRect;
var
  HasRect: boolean;
  Surface: TSurface;
begin
  if (DefaultSubregionIsFilterRegion) then
  begin
    Result := Rect(0, 0, RenderData.ROI.Width, RenderData.ROI.Height);
    exit;
  end;

  Result := Default(TRect);

  Surface := TFilterRenderer.ResolveSurface(Node.ResolvedIn1, RenderData);
  Result := Surface.Region;
  HasRect := not Surface.IsEmpty;

  Surface := TFilterRenderer.ResolveSurface(Node.ResolvedIn2, RenderData);
  if (not Surface.IsEmpty) then
  begin
    if HasRect then
      GR32.UnionRect(Result, Result, Surface.Region)
    else
    begin
      Result := Surface.Region;
      HasRect := True;
    end;
  end;

  if not HasRect then
    Result := Rect(0, 0, RenderData.ROI.Width, RenderData.ROI.Height);
end;

class function TFilterRenderer.ResolveSurface(const AInput: TSvgFilterInput; const RenderData: TFilterRenderData; DefaultFallback: TCustomBitmap32): TSurface;
begin
  case AInput.Kind of
    fikSourceGraphic:
      Result := RenderData.SourceGraphic;

    fikSourceAlpha:
      Result := RenderData.SourceAlpha;

    fikNamedResult:
      if (AInput.Index >= 0) and (AInput.Index <= High(RenderData.NamedSurfaces)) then
        Result := RenderData.NamedSurfaces[AInput.Index]
      else
        Result.Bitmap := nil;

    fikPreviousResult:
      Result := RenderData.CurrentSurface;
  else
    Result.Bitmap := nil;
  end;

  if (Result.Bitmap = nil) then
  begin
    if (DefaultFallback <> nil) then
    begin
      Result.Bitmap := DefaultFallback;
      if (DefaultFallback = RenderData.SourceGraphic.Bitmap) then
        Result.Region := RenderData.SourceGraphic.Region
      else
        Result.Region := DefaultFallback.BoundsRect;
    end else
      Result := RenderData.SourceGraphic;

    if (Result.Bitmap = nil) then
      Result.Region := Rect(0, 0, RenderData.ROI.Width, RenderData.ROI.Height);
  end;
end;

class function TFilterRenderer.NeedsSubSurface(Bitmap: TCustomBitmap32; const Subregion: TRect): boolean;
begin
  Result := (Subregion.Left <> 0) or (Subregion.Top <> 0) or (Subregion.Width <> Bitmap.Width) or (Subregion.Height <> Bitmap.Height);
end;

class procedure TFilterRenderer.CopySurfaceRegion(const ASrc: TSurface; ADst: TCustomBitmap32; const ADstRegion: TRect; CombineOp: TDrawMode);
var
  OverlapRect: TRect;
  SrcRect: TRect;
  DstX, DstY: Integer;
begin
  if (ASrc.IsEmpty) or (ADst = nil) or (ADst.Empty) or (ADstRegion.IsEmpty) then
    Exit;

  if not GR32.IntersectRect(OverlapRect, ASrc.Region, ADstRegion) then
    Exit;

  SrcRect := Rect(
    OverlapRect.Left - ASrc.Region.Left,
    OverlapRect.Top - ASrc.Region.Top,
    OverlapRect.Right - ASrc.Region.Left,
    OverlapRect.Bottom - ASrc.Region.Top);

  DstX := OverlapRect.Left - ADstRegion.Left;
  DstY := OverlapRect.Top - ADstRegion.Top;

  BlockTransfer(ADst, DstX, DstY, ADst.BoundsRect, ASrc.Bitmap, SrcRect, CombineOp);
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererGaussianBlur
//
//------------------------------------------------------------------------------

class function TFilterRendererGaussianBlur.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
begin
  Result.Left := TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * Scale + 2; // +2 for AA
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
  if (RadiusX > 100) then
    RadiusX := 100;
  if (RadiusY > 100) then
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
    Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
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

      Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
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
  Input: TSurface;
  RadiusX, RadiusY: Single;
  TempInput: TCustomBitmap32;
begin
  Input := ResolveSurface(Node.ResolvedIn1, RenderData);

  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  RadiusX := TSvgFeGaussianBlurNode(Node).StdDeviationX * GaussianSigmaToRadius * RenderData.Scale;
  RadiusY := TSvgFeGaussianBlurNode(Node).StdDeviationY * GaussianSigmaToRadius * RenderData.Scale;

  TempInput := nil;
  try
    if (NeedsSubSurface(Input.Bitmap, RenderData.CurrentSubregion)) then
    begin
      TempInput := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input, TempInput, RenderData.CurrentSubregion);
      Input.Bitmap := TempInput;
    end;

    ApplyBlur(Renderer, Input.Bitmap, RenderData.CurrentSurface.Bitmap, RadiusX, RadiusY);
  finally
    Renderer.ReleaseOffscreenBitmap(TempInput);
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererColorMatrix
//
//------------------------------------------------------------------------------

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

    // Cache last procesed pixel; If the new input is the same as the previous,
    // the new output will be the same as the previous
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

              // Cache
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

              pDest.R := Clamp(Lum + ((n * (integer(pSource.R) - Lum)) div 256));
              pDest.G := Clamp(Lum + ((n * (integer(pSource.G) - Lum)) div 256));
              pDest.B := Clamp(Lum + ((n * (integer(pSource.B) - Lum)) div 256));
              pDest.A := pSource.A;

              // Cache
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
            pDest.ARGB := Luminance709(pSource.ARGB) shl 24;

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
              pDest.A := pSource.A;

              // Cache
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
    if (pSource <> pDest) then
      Move(pSource^, pDest^, ASrc.ByteCount);
  end;

var
  Input: TSurface;
begin
  Input := ResolveSurface(TSvgFeColorMatrixNode(Node).ResolvedIn1, RenderData);
  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  if (NeedsSubSurface(Input.Bitmap, RenderData.CurrentSubregion)) then
    // Copy to output and apply in-place
    CopySurfaceRegion(Input, RenderData.CurrentSurface.Bitmap, RenderData.CurrentSubregion);

  ApplyColorMatrix(Input.Bitmap, RenderData.CurrentSurface.Bitmap, TSvgFeColorMatrixNode(Node).MatrixType, TSvgFeColorMatrixNode(Node).Values);
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererBlend
//
//------------------------------------------------------------------------------

class procedure TFilterRendererBlend.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input1: TSurface;
  Input2: TSurface;
  Buf1, Buf2: TCustomBitmap32;
begin
  Input1 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn1, RenderData);
  Input2 := ResolveSurface(TSvgFeBlendNode(Node).ResolvedIn2, RenderData, RenderData.SourceGraphic.Bitmap);
  if (Input1.Bitmap <> RenderData.UnnamedSurface) and (Input2.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, True);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  Buf1 := nil;
  Buf2 := nil;
  try
    if NeedsSubSurface(Input1.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf1 := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input1, Buf1, RenderData.CurrentSubregion);
      Input1.Bitmap := Buf1;
    end;
    if NeedsSubSurface(Input2.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf2 := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input2, Buf2, RenderData.CurrentSubregion);
      Input2.Bitmap := Buf2;
    end;

    Input2.Bitmap.CopyMapTo(RenderData.CurrentSurface.Bitmap);
    Renderer.BlendOffscreenSurface(RenderData.CurrentSurface.Bitmap, Input1.Bitmap, TSvgFeBlendNode(Node).Mode);

  finally
    Renderer.ReleaseOffscreenBitmap(Buf1);
    Renderer.ReleaseOffscreenBitmap(Buf2);
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererComposite
//
//------------------------------------------------------------------------------

class procedure TFilterRendererComposite.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input1: TSurface;
  Input2: TSurface;
  Buf1, Buf2: TCustomBitmap32;

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
  Input2 := ResolveSurface(TSvgFeCompositeNode(Node).ResolvedIn2, RenderData, RenderData.SourceGraphic.Bitmap);
  if (Input1.Bitmap <> RenderData.UnnamedSurface) and (Input2.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  Buf1 := nil;
  Buf2 := nil;
  try
    if NeedsSubSurface(Input1.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf1 := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input1, Buf1, RenderData.CurrentSubregion);
      Input1.Bitmap := Buf1;
    end;
    if NeedsSubSurface(Input2.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf2 := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input2, Buf2, RenderData.CurrentSubregion);
      Input2.Bitmap := Buf2;
    end;

    Count := Input1.Bitmap.PixelCount;
    pSource1 := PColor32Entry(Input1.Bitmap.Bits);
    pSource2 := PColor32Entry(Input2.Bitmap.Bits);
    pDest := PColor32Entry(RenderData.CurrentSurface.Bitmap.Bits);

    case TSvgFeCompositeNode(Node).CompositeOperator of
      coOver:
        begin
          // Input1 (in) composited over Input2 (in2) onto ADest
          Input2.Bitmap.CopyMapTo(RenderData.CurrentSurface.Bitmap);
          Input1.Bitmap.DrawTo(RenderData.CurrentSurface.Bitmap, 0, 0);
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
  finally
    Renderer.ReleaseOffscreenBitmap(Buf1);
    Renderer.ReleaseOffscreenBitmap(Buf2);
  end;
end;

class function TFilterRendererMerge.GetRoiRect(Node: TSvgFilterPrimitiveNode; const RenderData: TFilterRenderData): TRect;
var
  Surface: TSurface;
  i: integer;
begin
  Result := inherited;

  for i := 0 to Node.Children.Count - 1 do
    if Node.Children[i] is TSvgFeMergeNodeChild then
    begin
      Surface := TFilterRenderer.ResolveSurface(TSvgFeMergeNodeChild(Node.Children[i]).ResolvedIn1, RenderData);

      if (not Surface.IsEmpty) then
        GR32.UnionRect(Result, Result, Surface.Region)
    end;
end;

//------------------------------------------------------------------------------
//
//      TFilterRendererMerge
//
//------------------------------------------------------------------------------

class procedure TFilterRendererMerge.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TSurface;
  DestSurface: TCustomBitmap32;
  ChildNode: TSvgNode;
  First: boolean;
begin
  DestSurface := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  try

    // Children must be merged, so first child can draw opaque but the rest must
    // blend.
    First := True;

    for ChildNode in TSvgFeMergeNode(Node).Children do
      if ChildNode is TSvgFeMergeNodeChild then
      begin
        Input := ResolveSurface(TSvgFeMergeNodeChild(ChildNode).ResolvedIn1, RenderData);
        // Recurse to render child
        // TODO : There's no recursion here... Should there be?
        if (Input.Bitmap <> nil) then
        begin
          if (First) then
          begin
            CopySurfaceRegion(Input, DestSurface, RenderData.CurrentSubregion, dmOpaque);
            First := False;
          end else
            CopySurfaceRegion(Input, DestSurface, RenderData.CurrentSubregion, dmBlend);
        end;
      end;

    // Clear result if none of the children did
    if (First) then
      DestSurface.Clear;

    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  except
    Renderer.ReleaseOffscreenBitmap(DestSurface);
    raise;
  end;
  RenderData.CurrentSurface.Bitmap := DestSurface;
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererOffset
//
//------------------------------------------------------------------------------

class function TFilterRendererOffset.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
var
  Dx, Dy: Single;
begin
  Dx := TSvgFeOffsetNode(Node).Dx * Scale;
  Dy := TSvgFeOffsetNode(Node).Dy * Scale;
  Result.Left := Max(0.0, -Dx);
  Result.Top := Max(0.0, -Dy);
  Result.Right := Max(0.0, Dx);
  Result.Bottom := Max(0.0, Dy);
end;

class procedure TFilterRendererOffset.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TSurface;
  DestX, DestY: integer;
begin
  Input := ResolveSurface(TSvgFeOffsetNode(Node).ResolvedIn1, RenderData);
  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, True);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  DestX := Input.Region.Left + Round(TSvgFeOffsetNode(Node).Dx * RenderData.Scale) - RenderData.CurrentSubregion.Left;
  DestY := Input.Region.Top + Round(TSvgFeOffsetNode(Node).Dy * RenderData.Scale) - RenderData.CurrentSubregion.Top;

  // TODO : Why not simply offset the region and copy the source to the output? Size of output should be size of input
  if not Input.IsEmpty then
    Input.Bitmap.DrawTo(RenderData.CurrentSurface.Bitmap, DestX, DestY);
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererFlood
//
//------------------------------------------------------------------------------

class function TFilterRendererFlood.DefaultSubregionIsFilterRegion: Boolean;
begin
  Result := True;
end;

class procedure TFilterRendererFlood.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Color: TColor32;
begin
  Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  Color := TSvgFeFloodNode(Node).FloodColor.Color;
  if TSvgFeFloodNode(Node).FloodOpacity < 1.0 then
    ScaleAlpha(Color, TSvgFeFloodNode(Node).FloodOpacity);

  RenderData.CurrentSurface.Bitmap.Clear(Color);
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererMorphology
//
//------------------------------------------------------------------------------

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
  Input: TSurface;
  Temp, InputBuf: TCustomBitmap32;
  MorphNode: TSvgFeMorphologyNode;
  RadiusX, RadiusY: Integer;
begin
  MorphNode := TSvgFeMorphologyNode(Node);
  Input := ResolveSurface(MorphNode.ResolvedIn1, RenderData);

  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  RadiusX := Round(MorphNode.RadiusX * RenderData.Scale);
  RadiusY := Round(MorphNode.RadiusY * RenderData.Scale);

  InputBuf := nil;
  try
    if NeedsSubSurface(Input.Bitmap, RenderData.CurrentSubregion) then
    begin
      InputBuf := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input, InputBuf, RenderData.CurrentSubregion);
      Input.Bitmap := InputBuf;
    end;

    if (RadiusX > 0) and (RadiusY > 0) then
      Temp := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False)
    else
      Temp := nil;
    try

      ApplyMorphology(Input.Bitmap, RenderData.CurrentSurface.Bitmap, MorphNode.MorphologyOperator, RadiusX, RadiusY, Temp);

    finally
      Renderer.ReleaseOffscreenBitmap(Temp);
    end;
  finally
    Renderer.ReleaseOffscreenBitmap(InputBuf);
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
  if (Intermediate <> Input) then
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
  Input: TSurface;
  ChildNode: TSvgNode;
  FuncR, FuncG, FuncB, FuncA: TSvgFeFuncNode;
  LutR, LutG, LutB, LutA: TLUT8;
  i, Count: Integer;
  pSource, pDest: PColor32Entry;
begin
  Input := ResolveSurface(Node.ResolvedIn1, RenderData);
  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  CopySurfaceRegion(Input, RenderData.CurrentSurface.Bitmap, RenderData.CurrentSubregion);

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

  Count := RenderData.CurrentSurface.Bitmap.PixelCount;
  pSource := PColor32Entry(RenderData.CurrentSurface.Bitmap.Bits);
  pDest := pSource;

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

class function TFilterRendererDropShadow.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
var
  DropNode: TSvgFeDropShadowNode;
  BlurRadX, BlurRadY, Dx, Dy: Single;
begin
  DropNode := TSvgFeDropShadowNode(Node);
  BlurRadX := DropNode.StdDeviationX * GaussianSigmaToRadius * Scale + 2;
  BlurRadY := DropNode.StdDeviationY * GaussianSigmaToRadius * Scale + 2;
  Dx := DropNode.Dx * Scale;
  Dy := DropNode.Dy * Scale;
  Result.Left := Max(0.0, -Dx) + BlurRadX;
  Result.Top := Max(0.0, -Dy) + BlurRadY;
  Result.Right := Max(0.0, Dx) + BlurRadX;
  Result.Bottom := Max(0.0, Dy) + BlurRadY;
end;

class procedure TFilterRendererDropShadow.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  DropNode: TSvgFeDropShadowNode;
  Input: TSurface;
  BlurredShadow, InputBuf: TCustomBitmap32;
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
    end else
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
  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  ShadowColor := TColor32Entry(DropNode.FloodColor.Color);
  ShadowOpacity := Clamp(Round(ShadowColor.A * DropNode.FloodOpacity));

  PixelCount := RenderData.CurrentSubregion.Width * RenderData.CurrentSubregion.Height;

  InputBuf := nil;
  AlphaMap := TByteMap.Create;
  BlurredAlphaMap := TByteMap.Create;
  try
    if NeedsSubSurface(Input.Bitmap, RenderData.CurrentSubregion) then
    begin
      InputBuf := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
      CopySurfaceRegion(Input, InputBuf, RenderData.CurrentSubregion);
      Input.Bitmap := InputBuf;
    end;

    // 1. Extract 8-bit alpha shadow map
    AlphaMap.SetSize(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
    BlurredAlphaMap.SetSize(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);

    pIn := PColor32Entry(Input.Bitmap.Bits);
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
    BlurredShadow := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, False);
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
      RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(RenderData.CurrentSubregion.Width, RenderData.CurrentSubregion.Height, True);
      RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

      DestX := Round(DropNode.Dx * RenderData.Scale);
      DestY := Round(DropNode.Dy * RenderData.Scale);

      // Paint the shadow...
      BlockTransfer(RenderData.CurrentSurface.Bitmap, DestX, DestY, RenderData.CurrentSurface.Bitmap.ClipRect,
        BlurredShadow, BlurredShadow.BoundsRect, dmOpaque);
      // ...and the input image on top of it
      Renderer.BlendOffscreenSurface(RenderData.CurrentSurface.Bitmap, Input.Bitmap, bmNormal);

    finally
      Renderer.ReleaseOffscreenBitmap(BlurredShadow);
    end;
  finally
    BlurredAlphaMap.Free;
    AlphaMap.Free;
    Renderer.ReleaseOffscreenBitmap(InputBuf);
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererTurbulence
//
//------------------------------------------------------------------------------

class function TFilterRendererTurbulence.DefaultSubregionIsFilterRegion: Boolean;
begin
  Result := True;
end;

class procedure TFilterRendererTurbulence.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  TurbNode: TSvgFeTurbulenceNode;
  NoiseGen: TPerlinNoise;
  BaseFreqX, BaseFreqY: Double;
  NumOctaves: Integer;
  Seed: Double;
  StitchTiles, IsFractalSum: Boolean;
  TileX, TileY, TileW, TileH: Double;
  X, Y, Width, Height: Integer;
  UserPointX, UserPointY: Double;
  R, G, B, A: Double;
  pDest: PColor32Entry;
  Scale, OneOverScale: Double;
begin
  Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  Width := RenderData.CurrentSubregion.Width;
  Height := RenderData.CurrentSubregion.Height;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(Width, Height, False);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  TurbNode := TSvgFeTurbulenceNode(Node);
  BaseFreqX := TurbNode.BaseFrequencyX;
  BaseFreqY := TurbNode.BaseFrequencyY;
  NumOctaves := TurbNode.NumOctaves;
  Seed := TurbNode.Seed;
  StitchTiles := (TurbNode.StitchTiles = stStitch);
  IsFractalSum := (TurbNode.TurbulenceType = ttFractalNoise);

  Scale := RenderData.Scale;
  if Scale <= 0 then
  begin
    Scale := 1.0;
    OneOverScale := 1.0;
  end else
    OneOverScale := 1 / Scale;

  TileX := (RenderData.ROI.Left + RenderData.CurrentSubregion.Left) * OneOverScale;
  TileY := (RenderData.ROI.Top + RenderData.CurrentSubregion.Top) * OneOverScale;
  TileW := Width * OneOverScale;
  TileH := Height * OneOverScale;

  NoiseGen := TPerlinNoise.Create(Seed);
  try
    pDest := PColor32Entry(RenderData.CurrentSurface.Bitmap.Bits);

    for Y := 0 to Height - 1 do
    begin
      UserPointY := TileY + (Y * OneOverScale);
      for X := 0 to Width - 1 do
      begin
        UserPointX := TileX + (X * OneOverScale);

        R := NoiseGen.Turbulence(0, UserPointX, UserPointY, BaseFreqX, BaseFreqY, NumOctaves, IsFractalSum, StitchTiles, TileX, TileY, TileW, TileH);
        G := NoiseGen.Turbulence(1, UserPointX, UserPointY, BaseFreqX, BaseFreqY, NumOctaves, IsFractalSum, StitchTiles, TileX, TileY, TileW, TileH);
        B := NoiseGen.Turbulence(2, UserPointX, UserPointY, BaseFreqX, BaseFreqY, NumOctaves, IsFractalSum, StitchTiles, TileX, TileY, TileW, TileH);
        A := NoiseGen.Turbulence(3, UserPointX, UserPointY, BaseFreqX, BaseFreqY, NumOctaves, IsFractalSum, StitchTiles, TileX, TileY, TileW, TileH);

        if IsFractalSum then
        begin
          pDest.R := Clamp(Round(((R * 255.0) + 255.0) * 0.5));
          pDest.G := Clamp(Round(((G * 255.0) + 255.0) * 0.5));
          pDest.B := Clamp(Round(((B * 255.0) + 255.0) * 0.5));
          pDest.A := Clamp(Round(((A * 255.0) + 255.0) * 0.5));
        end else
        begin
          pDest.R := Clamp(Round(R * 255.0));
          pDest.G := Clamp(Round(G * 255.0));
          pDest.B := Clamp(Round(B * 255.0));
          pDest.A := Clamp(Round(A * 255.0));
        end;

        Inc(pDest);
      end;
    end;
  finally
    NoiseGen.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererDisplacementMap
//
//------------------------------------------------------------------------------

class function TFilterRendererDisplacementMap.GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect;
var
  DispNode: TSvgFeDisplacementMapNode;
  MaxDisp: Single;
begin
  DispNode := TSvgFeDisplacementMapNode(Node);
  MaxDisp := Abs(DispNode.Scale) * 0.5 * Scale;
  Result.Left := MaxDisp;
  Result.Top := MaxDisp;
  Result.Right := MaxDisp;
  Result.Bottom := MaxDisp;
end;

class function TFilterRendererDisplacementMap.GetChannelValue(const Color: TColor32Entry; Channel: TSvgChannelSelector): Byte;
begin
  case Channel of
    csR: Result := Color.R;
    csG: Result := Color.G;
    csB: Result := Color.B;
    csA: Result := Color.A;
  else
    Result := Color.A;
  end;
end;

class procedure TFilterRendererDisplacementMap.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  DispNode: TSvgFeDisplacementMapNode;
  Input1, Input2: TSurface;
  Buf1, Buf2: TCustomBitmap32;
  Width, Height, X, Y: Integer;
  ScaledScale, Dx, Dy: Single;
  ValX, ValY: Byte;
  RemapTransform: TRemapTransformation;
  pMap: PColor32Array;
begin
  DispNode := TSvgFeDisplacementMapNode(Node);
  Input1 := ResolveSurface(DispNode.ResolvedIn1, RenderData);
  Input2 := ResolveSurface(DispNode.ResolvedIn2, RenderData, RenderData.SourceGraphic.Bitmap);

  if (Input1.Bitmap <> RenderData.UnnamedSurface) and (Input2.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  Width := RenderData.CurrentSubregion.Width;
  Height := RenderData.CurrentSubregion.Height;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(Width, Height, True); // TODO : Not sure if we need to clear here. Depends on what Transform does with the destination bitmap
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  Buf1 := nil;
  Buf2 := nil;
  try
    if NeedsSubSurface(Input1.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf1 := Renderer.GetOffscreenBitmap(Width, Height, True);
      CopySurfaceRegion(Input1, Buf1, RenderData.CurrentSubregion);
      Input1.Bitmap := Buf1;
    end;
    if NeedsSubSurface(Input2.Bitmap, RenderData.CurrentSubregion) then
    begin
      Buf2 := Renderer.GetOffscreenBitmap(Width, Height, True);
      CopySurfaceRegion(Input2, Buf2, RenderData.CurrentSubregion);
      Input2.Bitmap := Buf2;
    end;

    ScaledScale := DispNode.Scale * RenderData.Scale;

    if (Abs(ScaledScale) < 1e-6) or (Width <= 1) or (Height <= 1) then
    begin
      Input1.Bitmap.CopyMapTo(RenderData.CurrentSurface.Bitmap);
      Exit;
    end;

    RemapTransform := TRemapTransformation.Create;
    try
      RemapTransform.VectorMap.SetSize(Width, Height);
      RemapTransform.SrcRect := FloatRect(0, 0, Width - 1, Height - 1);
      RemapTransform.MappingRect := FloatRect(0, 0, Width - 1, Height - 1);

      for Y := 0 to Height - 1 do
      begin
        pMap := Input2.Bitmap.ScanLine[Y];
        for X := 0 to Width - 1 do
        begin
          ValX := GetChannelValue(TColor32Entry(pMap[X]), DispNode.XChannelSelector);
          ValY := GetChannelValue(TColor32Entry(pMap[X]), DispNode.YChannelSelector);
          Dx := ScaledScale * (ValX * OneOver255 - 0.5);
          Dy := ScaledScale * (ValY * OneOver255 - 0.5);
          RemapTransform.VectorMap.FloatVector[X, Y] := FloatPoint(Dx, Dy);
        end;
      end;

      // Use TLinearResampler for smooth resampling
      // TODO : This alters the state of a pooled bitmap. Might not be an issue since TLinearResampler is an okay default.
      TLinearResampler.Create(Input1.Bitmap);
      Transform(RenderData.CurrentSurface.Bitmap, Input1.Bitmap, RemapTransform);
    finally
      RemapTransform.Free;
    end;
  finally
    Renderer.ReleaseOffscreenBitmap(Buf1);
    Renderer.ReleaseOffscreenBitmap(Buf2);
  end;
end;


//------------------------------------------------------------------------------
//
//      TFilterRendererTile
//
//------------------------------------------------------------------------------

class function TFilterRendererTile.DefaultSubregionIsFilterRegion: Boolean;
begin
  Result := True;
end;

class procedure TFilterRendererTile.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input: TSurface;
  TileX, TileY, X: Integer;
  TileW, TileH: Integer;
  Width, Height: Integer;
begin
  Input := ResolveSurface(Node.ResolvedIn1, RenderData);
  if (Input.Bitmap <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  if (Input.IsEmpty) then
  begin
    RenderData.CurrentSurface := TSurface.Empty;
    Exit;
  end;

  Width := RenderData.CurrentSubregion.Width;
  Height := RenderData.CurrentSubregion.Height;

  RenderData.CurrentSurface.Bitmap := Renderer.GetOffscreenBitmap(Width, Height, True);
  RenderData.CurrentSurface.Region := RenderData.CurrentSubregion;

  if (Input.IsEmpty) then
    Exit;

  // Per SVG 1.1 §15.22: "The tile location and size are defined by the filter
  // primitive subregion of the referenced input node."
  TileW := Input.Region.Width;
  TileH := Input.Region.Height;

  TileY := -(RenderData.CurrentSubregion.Top - Input.Region.Top) mod TileH;
  if TileY > 0 then
    Dec(TileY, TileH);

  TileX := -(RenderData.CurrentSubregion.Left - Input.Region.Left) mod TileW;
  if TileX > 0 then
    Dec(TileX, TileW);

  while TileY < Height do
  begin
    X := TileX;

    while X < Width do
    begin
      Input.Bitmap.DrawTo(RenderData.CurrentSurface.Bitmap, X, TileY);

      Inc(X, TileW);
    end;
    Inc(TileY, TileH);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure RegisterRenderers;
begin
  TSvgFeGaussianBlurNode.RegisterRenderClass(TFilterRendererGaussianBlur);
  TSvgFeColorMatrixNode.RegisterRenderClass(TFilterRendererColorMatrix);
  TSvgFeBlendNode.RegisterRenderClass(TFilterRendererBlend);
  TSvgFeCompositeNode.RegisterRenderClass(TFilterRendererComposite);
  TSvgFeMergeNode.RegisterRenderClass(TFilterRendererMerge);
  TSvgFeOffsetNode.RegisterRenderClass(TFilterRendererOffset);
  TSvgFeDropShadowNode.RegisterRenderClass(TFilterRendererDropShadow);
  TSvgFeFloodNode.RegisterRenderClass(TFilterRendererFlood);
  TSvgFeMorphologyNode.RegisterRenderClass(TFilterRendererMorphology);
  TSvgFeComponentTransferNode.RegisterRenderClass(TFilterRendererComponentTransfer);
  TSvgFeTurbulenceNode.RegisterRenderClass(TFilterRendererTurbulence);
  TSvgFeDisplacementMapNode.RegisterRenderClass(TFilterRendererDisplacementMap);
  TSvgFeTileNode.RegisterRenderClass(TFilterRendererTile);
end;

initialization
  RegisterRenderers;
end.
