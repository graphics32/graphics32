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
    class function ResolveSurface(const AInput: TSvgFilterInput; const RenderData: TFilterRenderData; DefaultFallback: TCustomBitmap32 = nil): TCustomBitmap32;
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; virtual;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); virtual; abstract;
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
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;

    class procedure ApplyBlur(Renderer: TSvgRenderer; Input, Output: TCustomBitmap32; RadiusX, RadiusY: Single); static;
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
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
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
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;

    class procedure ApplyMorphology(Input, Output: TCustomBitmap32; Op: TSvgMorphologyOperator; RadiusX, RadiusY: Integer; TempBitmap: TCustomBitmap32 = nil); static;
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
  public
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
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
  public
    class function GetMargins(Node: TSvgFilterPrimitiveNode; Scale: Single): TFloatRect; override;
    class procedure Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData); override;
    class function GetChannelValue(const Color: TColor32Entry; Channel: TSvgChannelSelector): Byte; static;
  end;

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
//      TFilterRenderer
//
//------------------------------------------------------------------------------

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
      if (AInput.Index >= 0) and (AInput.Index <= High(RenderData.NamedSurfaces)) then
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
  if (RadiusX > 100) then
    RadiusX := 100;
  if (RadiusY > 100) then
    RadiusY := 100;

  if (RadiusX < Blur32MinRadius) and (RadiusY < Blur32MinRadius) then
    Input.CopyMapTo(Output)
  else
  if (RadiusY < Blur32MinRadius) then
    FastHorizontalAlphaBlur32(Input, Output, RadiusX)
  else
  if (RadiusX < Blur32MinRadius) then
  begin
    Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
    Temp := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
    try
      Transpose32(Input.Bits, Transposed.Bits, Input.Width, Input.Height);
      FastHorizontalAlphaBlur32(Transposed, Temp, RadiusY);
      Transpose32(Temp.Bits, Output.Bits, Input.Height, Input.Width);
    finally
      Renderer.ReleaseOffscreenBitmap(Transposed);
      Renderer.ReleaseOffscreenBitmap(Temp);
    end;
  end
  else
  if Abs(RadiusX - RadiusY) < 1e-4 then
    FastAlphaBlur32(Input, Output, RadiusX)
  else
  begin
    Temp := Renderer.GetOffscreenBitmap(Input.Width, Input.Height, False);
    try
      FastHorizontalAlphaBlur32(Input, Temp, RadiusX);

      Transposed := Renderer.GetOffscreenBitmap(Input.Height, Input.Width, False);
      try
        Transpose32(Temp.Bits, Transposed.Bits, Input.Width, Input.Height);
        FastHorizontalBlur32(Transposed, Temp, RadiusY);
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
                M[i] := Round(AValues[i] * 16711680);
            else
              M[i] := Round(AValues[i] * 65536);
            end;
          end;
          for i := n + 1 to High(M) do
          begin
            case i of
              0, 6, 12, 18:
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
            end
            else
              pDest.ARGB := LastDest;

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;

      cmSaturate:
        if (Length(AValues) > 0) and (AValues[0] <> 1.0) then
        begin
          n := Clamp(Round(AValues[0] * 256), 0, 65535);

          for i := 0 to ASrc.PixelCount - 1 do
          begin
            if (not HasLast) or ((pSource.ARGB and $00FFFFFF) <> LastSource) then
            begin
              Lum := Luminance601(pSource.ARGB);

              pDest.R := Clamp(Lum + ((n * (integer(pSource.R) - Lum)) div 256));
              pDest.G := Clamp(Lum + ((n * (integer(pSource.G) - Lum)) div 256));
              pDest.B := Clamp(Lum + ((n * (integer(pSource.B) - Lum)) div 256));
              pDest.A := pSource.A;

              LastSource := pSource.ARGB and $00FFFFFF;
              LastDest := pDest.ARGB and $00FFFFFF;
              HasLast := True;
            end
            else
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
            pDest.ARGB := Luminance709(pSource.ARGB);

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;

      cmHueRotate:
        if (Length(AValues) > 0) and (AValues[0] <> 0.0) then
        begin
          GR32_Math.SinCos(DegToRad(AValues[0]), SinVal, CosVal);

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

              LastSource := pSource.ARGB and $00FFFFFF;
              LastDest := pDest.ARGB and $00FFFFFF;
              HasLast := True;
            end
            else
              pDest.ARGB := LastDest or (pSource.ARGB and $FF000000);

            Inc(pSource);
            Inc(pDest);
          end;

          exit;
        end;
    end;

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
        c1 := Round((TSvgFeCompositeNode(Node).K1 * OneOver255) * 65536.0);
        c2 := Round(TSvgFeCompositeNode(Node).K2 * 65536.0);
        c3 := Round(TSvgFeCompositeNode(Node).K3 * 65536.0);
        c4 := Round(TSvgFeCompositeNode(Node).K4 * 255.0 * 65536.0);

        for i := 0 to Count - 1 do
        begin
          vR := (c1 * pSource1.R * pSource2.R + c2 * pSource1.R + c3 * pSource2.R + c4) div 65536;
          vG := (c1 * pSource1.G * pSource2.G + c2 * pSource1.G + c3 * pSource2.G + c4) div 65536;
          vB := (c1 * pSource1.B * pSource2.B + c2 * pSource1.B + c3 * pSource2.B + c4) div 65536;
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

class procedure TFilterRendererMerge.Render(Renderer: TSvgRenderer; Node: TSvgFilterPrimitiveNode; var RenderData: TFilterRenderData);
var
  Input, DestSurface: TCustomBitmap32;
  ChildNode: TSvgNode;
begin
  DestSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);

  for ChildNode in TSvgFeMergeNode(Node).Children do
    if ChildNode is TSvgFeMergeNodeChild then
    begin
      Input := ResolveSurface(TSvgFeMergeNodeChild(ChildNode).ResolvedIn1, RenderData);
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
  end
  else
    if Intermediate <> Input then
      Input.CopyMapTo(Intermediate);

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
          end
          else
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
  for i := 0 to 255 do
    LUT[i] := i;

  if (FuncNode = nil) then
    Exit;

  case FuncNode.FuncType of
    ctIdentity:
      exit;

    ctTable:
      begin
        n := Length(FuncNode.TableValues);
        if (n = 0) then
          exit;

        if (n = 1) then
        begin
          for i := 0 to 255 do
            LUT[i] := Clamp(Round(FuncNode.TableValues[0] * 255.0));
        end
        else
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
          exit;

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
        Transpose8(Dst, Dst);
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

    RadiusX := DropNode.StdDeviationX * GaussianSigmaToRadius * RenderData.Scale;
    RadiusY := DropNode.StdDeviationY * GaussianSigmaToRadius * RenderData.Scale;

    ApplyBlur8(AlphaMap, BlurredAlphaMap, RadiusX, RadiusY);

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

      RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(RenderData.ROI.Width, RenderData.ROI.Height, True);

      DestX := Round(DropNode.Dx * RenderData.Scale);
      DestY := Round(DropNode.Dy * RenderData.Scale);

      BlockTransfer(RenderData.CurrentSurface, DestX, DestY, RenderData.CurrentSurface.ClipRect,
        BlurredShadow, BlurredShadow.BoundsRect, dmOpaque);
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
//
//      TFilterRendererTurbulence
//
//------------------------------------------------------------------------------

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
  Scale: Double;
begin
  Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  TurbNode := TSvgFeTurbulenceNode(Node);
  Width := RenderData.ROI.Width;
  Height := RenderData.ROI.Height;

  if (Width <= 0) or (Height <= 0) then
    Exit;

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(Width, Height, False);

  BaseFreqX := TurbNode.BaseFrequencyX;
  BaseFreqY := TurbNode.BaseFrequencyY;
  NumOctaves := TurbNode.NumOctaves;
  Seed := TurbNode.Seed;
  StitchTiles := (TurbNode.StitchTiles = stStitch);
  IsFractalSum := (TurbNode.TurbulenceType = ttFractalNoise);

  Scale := RenderData.Scale;
  if Scale <= 0 then
    Scale := 1.0;

  TileX := RenderData.ROI.Left / Scale;
  TileY := RenderData.ROI.Top / Scale;
  TileW := Width / Scale;
  TileH := Height / Scale;

  NoiseGen := TPerlinNoise.Create(Seed);
  try
    pDest := PColor32Entry(RenderData.CurrentSurface.Bits);

    for Y := 0 to Height - 1 do
    begin
      UserPointY := TileY + (Y / Scale);
      for X := 0 to Width - 1 do
      begin
        UserPointX := TileX + (X / Scale);

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
        end
        else
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
  Input1, Input2: TCustomBitmap32;
  Width, Height, X, Y: Integer;
  ScaledScale, Dx, Dy: Single;
  ValX, ValY: Byte;
  RemapTransform: TRemapTransformation;
  pMap: PColor32Array;
begin
  DispNode := TSvgFeDisplacementMapNode(Node);
  Input1 := ResolveSurface(DispNode.ResolvedIn1, RenderData);
  Input2 := ResolveSurface(DispNode.ResolvedIn2, RenderData, RenderData.SourceGraphic);

  if (Input1 <> RenderData.UnnamedSurface) and (Input2 <> RenderData.UnnamedSurface) then
    Renderer.ReleaseOffscreenBitmap(RenderData.UnnamedSurface);

  Width := RenderData.ROI.Width;
  Height := RenderData.ROI.Height;

  if (Width <= 0) or (Height <= 0) then
    Exit;

  RenderData.CurrentSurface := Renderer.GetOffscreenBitmap(Width, Height, True);

  if (Input1 = nil) or (Input2 = nil) then
    Exit;

  ScaledScale := DispNode.Scale * RenderData.Scale;

  if (Abs(ScaledScale) < 1e-6) or (Width <= 1) or (Height <= 1) then
  begin
    Input1.CopyMapTo(RenderData.CurrentSurface);
    Exit;
  end;

  RemapTransform := TRemapTransformation.Create;
  try
    RemapTransform.VectorMap.SetSize(Width, Height);
    RemapTransform.SrcRect := FloatRect(0, 0, Width - 1, Height - 1);
    RemapTransform.MappingRect := FloatRect(0, 0, Width - 1, Height - 1);

    for Y := 0 to Height - 1 do
    begin
      pMap := Input2.ScanLine[Y];
      for X := 0 to Width - 1 do
      begin
        ValX := GetChannelValue(TColor32Entry(pMap[X]), DispNode.XChannelSelector);
        ValY := GetChannelValue(TColor32Entry(pMap[X]), DispNode.YChannelSelector);
        Dx := ScaledScale * (ValX * OneOver255 - 0.5);
        Dy := ScaledScale * (ValY * OneOver255 - 0.5);
        RemapTransform.VectorMap.FloatVector[X, Y] := FloatPoint(Dx, Dy);
      end;
    end;

    TLinearResampler.Create(Input1);
    Transform(RenderData.CurrentSurface, Input1, RemapTransform);
  finally
    RemapTransform.Free;
  end;
end;

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
end;

initialization
  RegisterRenderers;
end.
