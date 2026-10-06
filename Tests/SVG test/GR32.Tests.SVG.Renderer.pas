unit GR32.Tests.SVG.Renderer;

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

{$I GR32.inc}

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  SysUtils, Classes,
  GR32, GR32_Transforms, GR32_Polygons,
  GR32.SVG.Types, GR32.SVG.Tree, GR32.SVG.Renderer;

type
  TTestSvgRenderer = class(TTestCase)
  published
    procedure TestPathRasterization;
    procedure TestTransformStack;
    procedure TestGradientFillRendering;
    procedure TestGroupOpacityCompositing;
    procedure TestClipPathCompositing;
    procedure TestDirectShapeClipPath;
    procedure TestClipRuleEvenOdd;
    procedure TestMaskCompositing;
    procedure TestBitmapPool;
    procedure TestPatternFillAndStrokeRendering;
    procedure TestPatternWithDefaultChildFill;
    procedure TestPatternSizingAndLargeBounds;
    procedure TestInkscapeGradientWithFallbackColor;
    procedure TestStrokeWidthRendering;
    procedure TestRadialGradientReflect;
    procedure TestMarkerRendering;
    procedure TestObjectBoundingBoxMaskAndGradient;
    procedure TestMixBlendModeAndIsolationRendering;
    procedure TestSymbolRendering;
    procedure TestFilterRendering;
    procedure TestFeColorMatrixRendering;
    procedure TestFeCompositeArithmeticRendering;
    procedure TestFeCompositeDropShadowRendering;
    procedure TestFeComponentTransferRendering;
    procedure TestNestedSvgViewportComponentTransfer;
    procedure TestFeDropShadowRendering;
    procedure TestFeDropShadowFilterRegionClipping;
    procedure TestFeDropShadowWithPercentageCoordinates;
    procedure TestFeDropShadowAnisotropicBlur;
    procedure TestFeTurbulenceRendering;
    procedure TestFeMorphologyFilterRendering;
    procedure TestFeDisplacementMapFilterRendering;
    procedure TestTextRendering;
    procedure TestTextRotationRendering;
    procedure TestTextPathRendering;
    procedure TestTextOpacityRendering;
    procedure TestEscapedTextRendering;
    procedure TestUserTransformTextSnippet;
    procedure TestImageRendering;
    procedure TestNestedSvgAndCurrentColorRendering;
    procedure TestSwitchRendering;
    procedure TestUserSpaceOnUsePercentageGradient;
    procedure TestPatternScaling;
    procedure TestRoiPolygonRendering;
    procedure TestRoiFilterBlurRendering;
    procedure TestTransformedFilterRendering;
    procedure TestObjectBoundingBoxClipPathRoi;
    procedure TestGradientAndPatternFillOpacity;
    procedure TestPatternTransform;
    procedure TestGradientTransformObjectBoundingBox;
    procedure TestFeGaussianBlurDirectional;
    procedure TestThemeFillAndStrokeColor;
    procedure TestConicalGradientRendering;
  end;

implementation

uses
  Types,
  GR32.SVG.Utf8;

{ TTestSvgRenderer }

procedure TTestSvgRenderer.TestPathRasterization;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  centerPixel: TColor32;
  expectedRangeCount, doubleTransformedRangeCount, x, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <rect x="25" y="25" width="50" height="50" fill="red"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        centerPixel := bmp.Pixel[50, 50];
        CheckEquals(clRed32, centerPixel, 'Center pixel should be red');
        CheckEquals(clWhite32, bmp.Pixel[5, 5], 'Top-left pixel should be white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // Test transformed text on path (<text transform="translate(0, 50)"> <textPath href="#curve3">)
    // Ensures text is transformed exactly 1x (Y=100) rather than double-transformed (Y=150)
    bmp.Clear(clWhite32);
    xml := '<svg width="200" height="200">' +
           '  <defs><path id="curve3" d="M 10 50 L 190 50"/></defs>' +
           '  <text transform="translate(0, 50)" font-size="20px" fill="blue">' +
           '    <textPath href="#curve3">Transformed Text</textPath>' +
           '  </text>' +
           '</svg>';
    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Transformed textPath docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        expectedRangeCount := 0;
        doubleTransformedRangeCount := 0;

        for y := 80 to 120 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] <> clWhite32 then
              Inc(expectedRangeCount);

        for y := 140 to 180 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] <> clWhite32 then
              Inc(doubleTransformedRangeCount);

        Check(expectedRangeCount > 30, Format('Transformed text on path should render around expected Y=100 (found %d pixels)', [expectedRangeCount]));
        CheckEquals(0, doubleTransformedRangeCount, Format('Transformed text on path should not be double-transformed to Y=150 (found %d pixels)', [doubleTransformedRangeCount]));
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeGaussianBlurDirectional;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);

    // 1. stdDeviation="5 0" on unrotated rect x=40, y=40, width=120, height=120
    bmp.Clear(clWhite32);
    xml := '<svg viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '  <filter id="f1"><feGaussianBlur stdDeviation="5 0"/></filter>' +
           '  <rect x="40" y="40" width="120" height="120" fill="seagreen" filter="url(#f1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center inside rect (100, 100) should be seagreen
        Check(bmp.Pixel[100, 100] <> clWhite32, 'Center of rect should be painted');

        // Horizontal blur expansion at x=30, y=100 (left of rect x=40) should contain green channel
        Check(bmp.Pixel[30, 100] <> clWhite32, 'Horizontal blur should expand to the left of rect at x=30');

        // Vertical boundary at y=30, x=100 (above rect y=40) MUST remain pure white because stdDeviationY = 0
        CheckEquals(clWhite32, bmp.Pixel[100, 30], 'Pixel above rect at y=30 must remain white when stdDeviationY=0');

        // Vertical boundary at y=170, x=100 (below rect y=160) MUST remain pure white because stdDeviationY = 0
        CheckEquals(clWhite32, bmp.Pixel[100, 170], 'Pixel below rect at y=170 must remain white when stdDeviationY=0');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // 2. stdDeviation="12 0" on rotated rect rotate(45 60 60)
    bmp.Clear(clWhite32);
    xml := '<svg viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '  <filter id="f1"><feGaussianBlur stdDeviation="12 0"/></filter>' +
           '  <rect x="80" y="10" width="80" height="80" fill="seagreen" filter="url(#f1)" transform="rotate(45 60 60)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Rotated rect docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Rotated rect region in world space around (116, 50) should be painted
        Check(bmp.Pixel[116, 50] <> clWhite32, 'Rotated rect body region should be painted');

        // Outside unblurred local top edge at (100, 30) MUST remain pure white when stdDeviationY=0
        CheckEquals(clWhite32, bmp.Pixel[100, 30], 'Pixel outside local top edge of rotated rect at (100, 30) must remain white when stdDeviationY=0');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // 3. Vertical-only blur stdDeviation="0 8" on rect x=40, y=40, width=120, height=120
    bmp.Clear(clWhite32);
    xml := '<svg viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '  <filter id="f1"><feGaussianBlur stdDeviation="0 8"/></filter>' +
           '  <rect x="40" y="40" width="120" height="120" fill="seagreen" filter="url(#f1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Vertical blur docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Vertical blur expansion at y=30, x=100 (above rect y=40) should contain green channel
        Check(bmp.Pixel[100, 30] <> clWhite32, 'Vertical blur should expand above rect at y=30');

        // Horizontal boundary at x=30, y=100 (left of rect x=40) MUST remain pure white when stdDeviationX = 0
        CheckEquals(clWhite32, bmp.Pixel[30, 100], 'Pixel left of rect at x=30 must remain white when stdDeviationX=0');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestPatternTransform;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg id="svg1" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '    <pattern id="patt1" patternUnits="userSpaceOnUse" width="40" height="25" patternTransform="skewX(10)">' +
           '        <rect id="rect1" x="0" y="0" width="100" height="40" fill="none" stroke="green"/>' +
           '        <rect id="rect2" x="0" y="0" width="20" height="25" fill="red"/>' +
           '    </pattern>' +
           '    <rect id="rect3" x="20" y="20" width="160" height="160" fill="url(#patt1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Pattern docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center of rectangle filled with pattern should be painted
        Check(bmp.Pixel[100, 100] <> clWhite32, 'Center of rect filled with pattern should be painted');
        // Pixel at (36, 100) shifted by skewX(10) should contain red tile fill
        Check(bmp.Pixel[36, 100] = clRed32, 'Transformed pattern tile at skewed coordinate should be painted red');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestTransformedFilterRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(300, 300);
    bmp.Clear(clWhite32);

    // Filtered group with transform matrix/translation
    xml := '<svg width="300" height="300">' +
           '  <defs>' +
           '    <filter id="f_offset">' +
           '      <feOffset dx="10" dy="10" result="off"/>' +
           '      <feMerge>' +
           '        <feMergeNode in="off"/>' +
           '        <feMergeNode in="SourceGraphic"/>' +
           '      </feMerge>' +
           '    </filter>' +
           '  </defs>' +
           '  <g transform="translate(100, 100)" filter="url(#f_offset)">' +
           '    <rect x="0" y="0" width="50" height="50" fill="red"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Transformed filter docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center of rect translated to (100, 100) + (25, 25) = (125, 125) should be red
        CheckEquals(clRed32, bmp.Pixel[125, 125], 'Center of transformed filtered rect should be red');

        // Offset rect at (125+10, 125+10) = (135, 135) should also be red
        CheckEquals(clRed32, bmp.Pixel[135, 135], 'Offset position of transformed filter should be red');

        // Pixel outside ROI at (20, 20) should remain pure white
        CheckEquals(clWhite32, bmp.Pixel[20, 20], 'Pixel outside ROI at (20, 20) must remain white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestPatternScaling;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg id="svg1" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '  <defs>' +
           '    <pattern id="patt1" patternUnits="userSpaceOnUse" width="20" height="20">' +
           '      <rect id="rect1" x="0" y="0" width="10" height="10" fill="grey"/>' +
           '      <rect id="rect2" x="10" y="10" width="10" height="10" fill="green"/>' +
           '    </pattern>' +
           '  </defs>' +
           '  <rect id="rect3" x="20" y="20" width="160" height="160" rx="20" ry="20" fill="url(#patt1)" stroke="darkblue"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Pattern docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Rect center region should be painted with pattern colors, NOT plain white
        Check(bmp.Pixel[100, 100] <> clWhite32, 'Center of rect filled with pattern should be painted');
        Check((bmp.Pixel[100, 100] = clGreen32) or (RedComponent(bmp.Pixel[100, 100]) < 200),
          'Pattern pixels should be green or grey');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestGradientAndPatternFillOpacity;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  pxGrad, pxPatt: TColor32;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="100">' +
           '  <defs>' +
           '    <linearGradient id="lg1">' +
           '      <stop offset="0" stop-color="black"/>' +
           '      <stop offset="1" stop-color="black"/>' +
           '    </linearGradient>' +
           '    <pattern id="patt1" patternUnits="userSpaceOnUse" width="20" height="20">' +
           '      <rect x="0" y="0" width="20" height="20" fill="black"/>' +
           '    </pattern>' +
           '  </defs>' +
           '  <rect id="r1" x="0" y="0" width="100" height="100" fill="url(#lg1)" fill-opacity="0.5"/>' +
           '  <rect id="r2" x="100" y="0" width="100" height="100" fill="url(#patt1)" fill-opacity="0.5"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        pxGrad := bmp.Pixel[50, 50];
        pxPatt := bmp.Pixel[150, 50];
        // On white canvas, black at 0.5 opacity blends to approx 128 gray
        Check(RedComponent(pxGrad) > 50, 'Gradient fill-opacity 0.5 on white should not be solid black');
        Check(RedComponent(pxGrad) < 200, 'Gradient fill-opacity 0.5 on white should not be pure white');
        Check(RedComponent(pxPatt) > 50, 'Pattern fill-opacity 0.5 on white should not be solid black');
        Check(RedComponent(pxPatt) < 200, 'Pattern fill-opacity 0.5 on white should not be pure white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestObjectBoundingBoxClipPathRoi;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    xml := '<svg id="svg1" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
           '  <clipPath id="clip1" clipPathUnits="objectBoundingBox">' +
           '    <circle id="circle1" cx="0.5" cy="0.5" r="0.45"/>' +
           '  </clipPath>' +
           '  <g id="g2" clip-path="url(#clip1)">' +
           '    <rect id="rect3" x="20" y="20" width="160" height="160" fill="red" visibility="hidden"/>' +
           '    <rect id="rect4" x="40" y="40" width="120" height="120" fill="green"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'objectBoundingBox clipPath docNode should not be nil');
    try
      // 1. Square target canvas (200x200)
      bmp.SetSize(200, 200);
      bmp.Clear(clWhite32);
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clGreen32, bmp.Pixel[100, 100], 'Center (100,100) inside square canvas objectBoundingBox clipPath must be green');
        CheckEquals(clGreen32, bmp.Pixel[50, 50], 'Top-left (50,50) inside square canvas objectBoundingBox clipPath must be green');
      finally
        renderer.Free;
      end;

      // 2. Resized non-square target canvas (400x200) - viewBox 200x200 scaled with xMidYMid meet alignment
      bmp.SetSize(400, 200);
      bmp.Clear(clWhite32);
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clGreen32, bmp.Pixel[200, 100], 'Center (200,100) inside resized non-square canvas objectBoundingBox clipPath must be green');
        CheckEquals(clGreen32, bmp.Pixel[150, 50], 'Top-left (150,50) inside resized non-square canvas objectBoundingBox clipPath must be green');
      finally
        renderer.Free;
      end;

    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestClipRuleEvenOdd;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="200">' +
           '  <defs>' +
           '    <clipPath id="clip1">' +
           '      <path d="M 100 15 l 50 160 l -130 -100 l 160 0 l -130 100 z" clip-rule="evenodd"/>' +
           '    </clipPath>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="200" height="200" fill="green" clip-path="url(#clip1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clGreen32, bmp.Pixel[100, 30], 'Star arm point should be clipped inside (green)');
        CheckEquals(clWhite32, bmp.Pixel[100, 100], 'Center pentagon hole with evenodd clip-rule should be outside (white)');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestDirectShapeClipPath;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <clipPath id="clip1">' +
           '      <rect x="0" y="0" width="50" height="100"/>' +
           '    </clipPath>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="100" height="100" fill="green" clip-path="url(#clip1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clGreen32, bmp.Pixel[25, 50], 'Clipped region inside direct rect should be green');
        CheckEquals(clWhite32, bmp.Pixel[75, 50], 'Clipped region outside direct rect should be white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestInkscapeGradientWithFallbackColor;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(406, 206);
    bmp.Clear(clWhite32);

    xml := '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" height="206" width="406">' +
           '  <defs>' +
           '    <linearGradient id="linearGradient2195">' +
           '      <stop id="stop2197" offset="0" style="stop-color: rgb(50, 50, 50); stop-opacity: 1;"/>' +
           '      <stop id="stop2199" offset="1" style="stop-color: rgb(150, 150, 150); stop-opacity: 1;"/>' +
           '    </linearGradient>' +
           '    <radialGradient xlink:href="#linearGradient2195" id="radialGradient3100" gradientUnits="userSpaceOnUse" ' +
           '      gradientTransform="matrix(0.793492, 0, 0, 1.26025, -124.125, -349.237)" spreadMethod="reflect" ' +
           '      cx="195.339" cy="367.994" fx="195.339" fy="367.994" r="10.189"/>' +
           '  </defs>' +
           '  <rect height="200" id="rect2052" rx="20" ry="20" style="fill: url(#radialGradient3100) rgb(0, 0, 0); fill-opacity: 1;" width="375" x="25" y="3"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Inkscape gradient docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Rect should be painted with gradient colors, NOT opaque black
        Check(bmp.Pixel[200, 100] <> clBlack32, 'Center of rect with Inkscape url(#id) rgb(...) should be painted with gradient, not solid black');
        Check(bmp.Pixel[200, 100] <> clWhite32, 'Center of rect should be painted');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestPatternWithDefaultChildFill;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(230, 100);
    bmp.Clear(clWhite32);

    xml := '<svg viewBox="0 0 230 100" xmlns="http://www.w3.org/2000/svg">' +
           '  <defs>' +
           '    <pattern id="star" viewBox="0,0,10,10" width="10%" height="10%">' +
           '      <polygon points="0,0 2,5 0,10 5,8 10,10 8,5 10,0 5,2"/>' +
           '    </pattern>' +
           '  </defs>' +
           '  <circle cx="60" cy="50" r="50" fill="url(#star)"/>' +
           '  <circle cx="170" cy="50" r="40" fill="none" stroke-width="20" stroke="url(#star)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Pattern with default child fill docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Pattern fill check on the left circle (cx=60, cy=50)
        Check(bmp.Pixel[60, 50] <> clWhite32, 'Center region of left circle should be painted by pattern fill');

        // Pattern stroke check on the right circle (cx=170, cy=50, r=40, stroke-width=20)
        Check(bmp.Pixel[170, 10] <> clWhite32, 'Stroke ring top edge of right circle should be painted by pattern stroke');
        Check(bmp.Pixel[170, 50] = clWhite32, 'Center of hollow stroked circle should remain white background');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestTextRotationRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  nonWhiteCount, x, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="200">' +
           '  <text x="100" y="100" font-size="32px" fill="red" rotate="45">Rotated</text>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Rotated text docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        nonWhiteCount := 0;
        for y := 0 to 199 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] <> clWhite32 then
              Inc(nonWhiteCount);

        Check(nonWhiteCount > 50, Format('Rotated text should render pixels on canvas (found %d non-white pixels)', [nonWhiteCount]));
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestTextOpacityRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  pxTextOpacity, pxPathOpacity: TColor32;
  x, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="100">' +
           '  <defs><path id="curve" d="M 10 70 L 190 70"/></defs>' +
           '  <text x="10" y="30" font-size="24px" fill="black" opacity="0.5">Opacity Text</text>' +
           '  <text font-size="24px" fill="black" opacity="0.5">' +
           '    <textPath href="#curve">Opacity TextPath</textPath>' +
           '  </text>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        pxTextOpacity := clWhite32;
        pxPathOpacity := clWhite32;

        for y := 0 to 99 do
          for x := 0 to 199 do
          begin
            if (bmp.Pixel[x, y] <> clWhite32) then
            begin
              if (y < 45) and (pxTextOpacity = clWhite32) then
                pxTextOpacity := bmp.Pixel[x, y]
              else if (y >= 45) and (pxPathOpacity = clWhite32) then
                pxPathOpacity := bmp.Pixel[x, y];
            end;
          end;

        Check(pxTextOpacity <> clWhite32, 'Text with opacity="0.5" should render pixels');
        Check(RedComponent(pxTextOpacity) > 50, 'Text with opacity="0.5" on white should not be solid black');
        Check(RedComponent(pxTextOpacity) < 200, 'Text with opacity="0.5" on white should not be pure white');

        Check(pxPathOpacity <> clWhite32, 'TextPath with opacity="0.5" should render pixels');
        Check(RedComponent(pxPathOpacity) > 50, 'TextPath with opacity="0.5" on white should not be solid black');
        Check(RedComponent(pxPathOpacity) < 200, 'TextPath with opacity="0.5" on white should not be pure white');

      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestTextPathRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  nonWhiteCount, x, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="200">' +
           '  <defs>' +
           '    <path id="curve" d="M 20 100 Q 100 20 180 100"/>' +
           '  </defs>' +
           '  <text font-size="24px" fill="blue">' +
           '    <textPath href="#curve" startOffset="10px" rotate="-10 15 20">Text with spaces on path</textPath>' +
           '  </text>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'TextPath docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        nonWhiteCount := 0;
        for y := 0 to 199 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] <> clWhite32 then
              Inc(nonWhiteCount);

        Check(nonWhiteCount > 50, Format('Text along path should render pixels on canvas (found %d non-white pixels)', [nonWhiteCount]));
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // Test startOffset="-9999" (all glyphs clipped off-path)
    bmp.Clear(clWhite32);
    xml := '<svg width="200" height="200">' +
           '  <defs><path id="curve2" d="M 20 100 L 180 100"/></defs>' +
           '  <text font-size="24px" fill="blue">' +
           '    <textPath href="#curve2" startOffset="-9999px">Clipped Away</textPath>' +
           '  </text>' +
           '</svg>';
    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Out of bounds textPath docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        nonWhiteCount := 0;
        for y := 0 to 199 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] <> clWhite32 then
              Inc(nonWhiteCount);

        CheckEquals(0, nonWhiteCount, 'Text positioned completely before start of path should be clipped away (0 rendered pixels)');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestEscapedTextRendering;
var
  docNode: TSvgDocumentNode;
  textNode: TSvgTextNode;
  xml: UTF8String;
begin
  xml := '<svg width="200" height="100">' +
         '  <text id="t1" x="20" y="50" font-size="24px" fill="black">&lt;A &amp; B&gt;</text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'Escaped text docNode should not be nil');
  try
    textNode := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(textNode <> nil, 'textNode t1 should exist');
    CheckEquals('<A & B>', textNode.TextContent, 'Escaped text content should be unescaped during XML parsing');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeDisplacementMapFilterRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp1, bmp2, bmp3: TBitmap32;
  HasNonZero: Boolean;
  x, y: Integer;
  p1: TColor32;
begin
  xml := '<svg width="40" height="40">' +
         '  <defs>' +
         '    <filter id="f_disp">' +
         '      <feTurbulence type="turbulence" baseFrequency="0.1" numOctaves="1" seed="1" result="turb"/>' +
         '      <feDisplacementMap in="SourceGraphic" in2="turb" scale="10" xChannelSelector="R" yChannelSelector="G"/>' +
         '    </filter>' +
         '    <filter id="f_disp_zero">' +
         '      <feTurbulence type="turbulence" baseFrequency="0.1" numOctaves="1" seed="1" result="turb"/>' +
         '      <feDisplacementMap in="SourceGraphic" in2="turb" scale="0" xChannelSelector="R" yChannelSelector="G"/>' +
         '    </filter>' +
         '    <filter id="f_disp_const">' +
         '      <feFlood flood-color="rgb(255, 128, 128)" flood-opacity="1" result="map"/>' +
         '      <feDisplacementMap in="SourceGraphic" in2="map" scale="10" xChannelSelector="R" yChannelSelector="G"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect id="r_disp" x="10" y="10" width="20" height="20" fill="red" filter="url(#f_disp)"/>' +
         '  <rect id="r_zero" x="10" y="10" width="20" height="20" fill="red" filter="url(#f_disp_zero)"/>' +
         '  <rect id="r_const" x="10" y="10" width="20" height="20" fill="red" filter="url(#f_disp_const)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp1 := TBitmap32.Create;
  bmp2 := TBitmap32.Create;
  bmp3 := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp1);
  try
    bmp1.SetSize(40, 40);
    bmp1.Clear(0);

    // Render rect with scale=10 turbulence displacement
    renderer.RenderNode(bmp1, docNode.FindNodeById('r_disp'));

    HasNonZero := False;
    for y := 0 to 39 do
      for x := 0 to 39 do
      begin
        p1 := bmp1.Pixel[x, y];
        if (p1 <> 0) then
          HasNonZero := True;
      end;
    Check(HasNonZero, 'feDisplacementMap should produce non-zero pixels');

    // Render rect with scale=0
    renderer.Target := bmp2;
    bmp2.SetSize(40, 40);
    bmp2.Clear(0);
    renderer.RenderNode(bmp2, docNode.FindNodeById('r_zero'));

    // Center pixel (20, 20) should be red in scale=0
    CheckEquals(clRed32, bmp2.Pixel[20, 20], 'Zero scale center pixel should be red');

    // Render rect with constant map displacement (R=255 => Dx=+5, G=128 => Dy=0)
    renderer.Target := bmp3;
    bmp3.SetSize(40, 40);
    bmp3.Clear(0);
    renderer.RenderNode(bmp3, docNode.FindNodeById('r_const'));

    // Pixel at (5, 20) looks up source pixel (5+5, 20) = (10, 20) which is inside red rect
    CheckEquals(clRed32, bmp3.Pixel[5, 20], 'Pixel (5, 20) should be displaced red pixel');

    // Pixel at (28, 20) looks up source pixel (28+5, 20) = (33, 20) which is outside red rect
    CheckEquals(0, bmp3.Pixel[28, 20], 'Pixel (28, 20) should be empty after displacement shift');
  finally
    renderer.Free;
    bmp1.Free;
    bmp2.Free;
    bmp3.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeMorphologyFilterRendering;
var
  xmlDilate, xmlErode: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  // 1. Dilate test: A 20x20 white square from (40,40) to (60,60) on a black background dilated with radius 10
  // should expand to a 40x40 square from (30,30) to (70,70).
  xmlDilate := '<svg width="100" height="100">' +
               '  <defs>' +
               '    <filter id="f_dilate" x="0" y="0" width="1" height="1">' +
               '      <feMorphology operator="dilate" radius="10"/>' +
               '    </filter>' +
               '  </defs>' +
               '  <rect x="40" y="40" width="20" height="20" fill="white" filter="url(#f_dilate)"/>' +
               '</svg>';

  docNode := ParseSvgXml(xmlDilate);
  Check(docNode <> nil, 'docNode should not be nil for dilate test');
  bmp := TBitmap32.Create;
  bmp.SetSize(100, 100);
  bmp.Clear(clBlack32);
  renderer := TSvgRenderer.Create;
  try
    renderer.RenderDocument(bmp, docNode);
    // (35, 35) was originally outside the square (40,40..60,60), but with dilate radius=10 it becomes white.
    CheckEquals(clWhite32, bmp.Pixel[35, 35], 'Dilate filter should expand white square to include (35,35)');
    CheckEquals(clWhite32, bmp.Pixel[50, 50], 'Center of dilated square should remain white');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;

  // 2. Erode test: A 40x40 white square from (30,30) to (70,70) eroded with radius 10
  // should shrink to a 20x20 square from (40,40) to (60,60).
  xmlErode := '<svg width="100" height="100">' +
              '  <defs>' +
              '    <filter id="f_erode" x="0" y="0" width="1" height="1">' +
              '      <feMorphology operator="erode" radius="10"/>' +
              '    </filter>' +
              '  </defs>' +
              '  <rect x="30" y="30" width="40" height="40" fill="white" filter="url(#f_erode)"/>' +
              '</svg>';

  docNode := ParseSvgXml(xmlErode);
  Check(docNode <> nil, 'docNode should not be nil for erode test');
  bmp := TBitmap32.Create;
  bmp.SetSize(100, 100);
  bmp.Clear(clBlack32);
  renderer := TSvgRenderer.Create;
  try
    renderer.RenderDocument(bmp, docNode);
    // (35, 35) was inside the original 40x40 square (30,30..70,70), but after erosion radius=10 it becomes black/transparent.
    CheckEquals(clBlack32, bmp.Pixel[35, 35], 'Erode filter should shrink white square away from (35,35)');
    CheckEquals(clWhite32, bmp.Pixel[50, 50], 'Center of eroded square should remain white');
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestConicalGradientRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pRight, pLeft: TColor32;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <conicGradient id="cg1" cx="50" cy="50" angle="0">' +
           '      <stop offset="0" stop-color="red"/>' +
           '      <stop offset="0.5" stop-color="green"/>' +
           '      <stop offset="1" stop-color="blue"/>' +
           '    </conicGradient>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="100" height="100" fill="url(#cg1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        pRight := bmp.PixelS[75, 50];  // 0 deg (Offset 0.0) -> Red
        pLeft := bmp.PixelS[25, 50];   // 180 deg (Offset 0.5) -> Green

        Check(RedComponent(pRight) > 200, 'Right edge should be predominantly red at offset 0');
        Check(GreenComponent(pLeft) > 200, 'Left edge should be predominantly green at offset 0.5');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestThemeFillAndStrokeColor;
var
  bmp: TBitmap32;
  renderer: TSvgRenderer;
  docNode: TSvgDocumentNode;
  xml: UTF8String;
begin
  xml := '<svg width="100" height="100">' +
         '  <rect id="r1" x="10" y="10" width="30" height="30" fill="red" stroke="black" stroke-width="4"/>' +
         '  <rect id="r2" x="60" y="10" width="30" height="30" fill="none" stroke="black" stroke-width="4"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(100, 100);

    // 1. Default rendering (no theme overrides active)
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clRed32, bmp.Pixel[25, 25], 'Default fill should be red');
    CheckEquals(clBlack32, bmp.Pixel[10, 25], 'Default stroke should be black');
    CheckEquals(clWhite32, bmp.Pixel[75, 25], 'Unfilled rect interior should remain white');

    // 2. Set ThemeFillColor to Lime32 and ThemeStrokeColor to Blue32
    renderer.ThemeFillColor := clLime32;
    renderer.ThemeStrokeColor := clBlue32;
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clLime32, bmp.Pixel[25, 25], 'Theme fill should override red with lime');
    CheckEquals(clBlue32, bmp.Pixel[10, 25], 'Theme stroke should override black with blue');
    CheckEquals(clWhite32, bmp.Pixel[75, 25], 'Unfilled rect interior must remain white even with ThemeFillColor active');

    // 3. Clear theme colors and re-verify original rendering
    renderer.ClearThemeColors;
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clRed32, bmp.Pixel[25, 25], 'Restored fill should be red');
    CheckEquals(clBlack32, bmp.Pixel[10, 25], 'Restored stroke should be black');

  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestGradientTransformObjectBoundingBox;
var
  bmp: TBitmap32;
  renderer: TSvgRenderer;
  docNode: TSvgDocumentNode;
  xml: UTF8String;
  topPixel, bottomPixel: TColor32;
begin
  // Verifies that gradientTransform on linearGradient with objectBoundingBox units
  // is correctly transformed in normalized [0..1] space before mapping to element bounding box.
  xml := '<svg viewBox="0 0 10 10" width="100" height="100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">' +
         '  <defs>' +
         '    <circle id="myCircle" cx="0" cy="0" r="5" />' +
         '    <linearGradient id="myGradient" gradientTransform="rotate(90)">' +
         '      <stop offset="20%" stop-color="gold" />' +
         '      <stop offset="90%" stop-color="red" />' +
         '    </linearGradient>' +
         '  </defs>' +
         '  <use x="5" y="5" xlink:href="#myCircle" fill="url(#myGradient)" />' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    docNode.Resolve;
    bmp := TBitmap32.Create;
    renderer := TSvgRenderer.Create(bmp);
    try
      bmp.SetSize(100, 100);
      bmp.Clear(clWhite32);
      renderer.RenderDocument(docNode);

      // Top inside circle (around y=25 on 100x100 canvas, i.e. y=2.5 in 10x10 viewBox)
      topPixel := bmp.Pixel[50, 25];
      // Bottom inside circle (around y=85 on 100x100 canvas, i.e. y=8.5 in 10x10 viewBox)
      bottomPixel := bmp.Pixel[50, 85];

      // Gold stop (offset 20%) at top has high green component (> 150)
      Check(GreenComponent(topPixel) > 150, 'Top of circle should be gold (high green component)');
      // Red stop (offset 90%) at bottom has low green component (< 50)
      Check(GreenComponent(bottomPixel) < 50, 'Bottom of circle should be red (low green component)');
    finally
      renderer.Free;
      bmp.Free;
    end;
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeDropShadowWithPercentageCoordinates;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  // Test user's SVG snippet with viewBox="0 0 30 10" and percentage coordinates cy="50%"
  xml := '<svg viewBox="0 0 30 10" xmlns="http://www.w3.org/2000/svg">' +
         '  <defs>' +
         '    <filter id="shadow">' +
         '      <feDropShadow dx="0.2" dy="0.4" stdDeviation="0.2"/>' +
         '    </filter>' +
         '    <filter id="shadow2">' +
         '      <feDropShadow dx="0" dy="0" stdDeviation="0.5" flood-color="cyan"/>' +
         '    </filter>' +
         '    <filter id="shadow3">' +
         '      <feDropShadow dx="-0.8" dy="-0.8" stdDeviation="0" flood-color="pink" flood-opacity="0.5"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <circle cx="5" cy="50%" r="4" style="fill:pink; filter:url(#shadow);"/>' +
         '  <circle cx="15" cy="50%" r="4" style="fill:pink; filter:url(#shadow2);"/>' +
         '  <circle cx="25" cy="50%" r="4" style="fill:pink; filter:url(#shadow3);"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(300, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Circle 1 center (cx=5, cy=5) -> bitmap (50, 50) should be painted pink
    Check(bmp.Pixel[50, 50] <> clWhite32, 'Circle 1 center at (50, 50) should be painted');

    // Circle 2 center (cx=15, cy=5) -> bitmap (150, 50) should be painted pink
    Check(bmp.Pixel[150, 50] <> clWhite32, 'Circle 2 center at (150, 50) should be painted');

    // Circle 3 center (cx=25, cy=5) -> bitmap (250, 50) should be painted pink
    Check(bmp.Pixel[250, 50] <> clWhite32, 'Circle 3 center at (250, 50) should be painted');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeTurbulenceRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp1, bmp2: TBitmap32;
  p1, p2: TColor32;
  x, y: Integer;
  HasNonZero: Boolean;
begin
  // Test feTurbulence and feFractalNoise generating non-empty pixels
  xml := '<svg width="20" height="20">' +
         '  <defs>' +
         '    <filter id="f_turb">' +
         '      <feTurbulence type="turbulence" baseFrequency="0.1" numOctaves="2" seed="1"/>' +
         '    </filter>' +
         '    <filter id="f_fractal">' +
         '      <feTurbulence type="fractalNoise" baseFrequency="0.1" numOctaves="2" seed="1"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect id="r1" width="20" height="20" filter="url(#f_turb)"/>' +
         '  <rect id="r2" width="20" height="20" filter="url(#f_fractal)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp1 := TBitmap32.Create;
  bmp2 := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp1);
  try
    bmp1.SetSize(20, 20);
    bmp1.Clear(0);

    // Render r1 (turbulence)
    renderer.RenderNode(bmp1, docNode.FindNodeById('r1'));

    HasNonZero := False;
    for y := 0 to 19 do
      for x := 0 to 19 do
      begin
        p1 := bmp1.Pixel[x, y];
        if (p1 <> 0) then
          HasNonZero := True;
      end;
    Check(HasNonZero, 'feTurbulence should produce non-zero pixels');

    // Render r2 (fractalNoise)
    renderer.Target := bmp2;
    bmp2.SetSize(20, 20);
    bmp2.Clear(0);

    renderer.RenderNode(bmp2, docNode.FindNodeById('r2'));

    HasNonZero := False;
    for y := 0 to 19 do
      for x := 0 to 19 do
      begin
        p2 := bmp2.Pixel[x, y];
        if (p2 <> 0) then
          HasNonZero := True;
      end;
    Check(HasNonZero, 'feFractalNoise should produce non-zero pixels');

    // Compare turbulence vs fractalNoise output at same seed
    p1 := bmp1.Pixel[10, 10];
    p2 := bmp2.Pixel[10, 10];
    Check(p1 <> p2, 'feTurbulence and feFractalNoise should produce distinct results');

  finally
    renderer.Free;
    bmp1.Free;
    bmp2.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeDropShadowAnisotropicBlur;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  // Test anisotropic drop shadow (stdDeviationX != stdDeviationY) on a non-square surface (200x100)
  xml := '<svg width="200" height="100">' +
         '  <defs>' +
         '    <filter id="f_aniso">' +
         '      <feDropShadow dx="10" dy="15" stdDeviation="12 3" flood-color="blue"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect x="20" y="20" width="120" height="40" fill="red" filter="url(#f_aniso)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Center of rect (80, 40) should be painted red
    CheckEquals(clRed32, bmp.Pixel[80, 40], 'Center of rect should be red');

    // Anisotropic shadow region at (80+10, 40+15) = (90, 55) should contain blue shadow
    Check(bmp.Pixel[90, 55] <> clWhite32, 'Shadow region at (90, 55) should be painted');
    Check(BlueComponent(bmp.Pixel[90, 55]) > 0, 'Shadow region should contain blue channel');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeDropShadowFilterRegionClipping;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  // Circle at cx=100, cy=100, r=60 -> bbox [40, 40, 160, 160] (120x120)
  // Default filter region x=-10%, y=-10%, width=120%, height=120% -> [28, 28, 172, 172]
  xml := '<svg viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">' +
         '  <filter id="filter1">' +
         '    <feDropShadow dx="20" dy="30" stdDeviation="6" flood-color="red"/>' +
         '  </filter>' +
         '  <circle id="circle1" cx="100" cy="100" r="60" fill="seagreen" filter="url(#filter1)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Inside circle (100, 100) should be seagreen
    Check(bmp.Pixel[100, 100] <> clWhite32, 'Center of circle should be painted');

    // Inside filter region shadow area (165, 165) should contain red shadow
    Check(bmp.Pixel[165, 165] <> clWhite32, 'Shadow inside filter region at (165, 165) should be painted');

    // Outside default filter region [28, 28, 172, 172] at (180, 180) MUST remain pure white (clipped to filter region)
    CheckEquals(clWhite32, bmp.Pixel[180, 180], 'Shadow extending outside filter region at (180, 180) must be clipped to white');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeDropShadowRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pCenter, pShadow: TColor32;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <filter id="f_ds">' +
         '      <feDropShadow dx="20" dy="20" stdDeviation="0" flood-color="red"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect x="20" y="20" width="50" height="50" fill="blue" filter="url(#f_ds)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Center of original rect (45, 45) should be blue (SourceGraphic composited over shadow)
    pCenter := bmp.Pixel[45, 45];
    CheckEquals(clBlue32, pCenter, 'Original rect area should be blue');

    // Offset shadow region at (20+20+25, 20+20+25) = (65, 65) should be red shadow
    pShadow := bmp.Pixel[65, 65];
    CheckEquals(clRed32, pShadow, 'Offset shadow region at (65, 65) should be red');

    // Unpainted area at (5, 5) should remain white
    CheckEquals(clWhite32, bmp.Pixel[5, 5], 'Background at (5, 5) should remain white');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestPatternSizingAndLargeBounds;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(1000, 1000);
    bmp.Clear(clWhite32);

    xml := '<svg viewBox="0 0 1000 1000" xmlns="http://www.w3.org/2000/svg">' +
           '  <defs>' +
           '    <pattern id="star" viewBox="0,0,10,10" width="10%" height="10%">' +
           '      <polygon points="0,0 2,5 0,10 5,8 10,10 8,5 10,0 5,2" fill="green"/>' +
           '    </pattern>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="1000" height="1000" fill="url(#star)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil for large pattern fill');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center of rect should be painted by green star pattern tile, not blank white
        Check(bmp.Pixel[500, 500] <> clWhite32, 'Center region of large rect should be painted by pattern fill');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestUserSpaceOnUsePercentageGradient;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);

    xml := '<svg id="svg1" viewBox="0 0 200 100" xmlns="http://www.w3.org/2000/svg">' +
           '  <linearGradient id="lg1" x1="15%" y2="80%" gradientUnits="userSpaceOnUse">' +
           '    <stop offset="0.4" stop-color="white"/>' +
           '    <stop offset="0.6" stop-color="black"/>' +
           '  </linearGradient>' +
           '  <rect id="rect1" x="20" y="20" width="160" height="60" fill="url(#lg1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'UserSpaceOnUse % gradient docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Rect center at (100, 50) should be transition color (~gray), NOT pure white
        Check(bmp.Pixel[100, 50] <> clWhite32, 'Center of rect with userSpaceOnUse % gradient should not be pure white');

        // Rect near bottom-right at (170, 75) should be dark/black (stop 0.6)
        Check(RedComponent(bmp.Pixel[170, 75]) < 50, 'Bottom-right of rect should be near black');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestRoiPolygonRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(500, 500);
    bmp.Clear(clWhite32);

    // Polygon with stroke and opacity rendering into a 500x500 canvas
    xml := '<svg width="500" height="500">' +
           '  <polygon points="100,100 200,100 200,200 100,200" fill="blue" stroke="red" stroke-width="20" opacity="0.8"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Polygon ROI docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center inside polygon (150, 150) should be painted semi-transparent blue
        Check(bmp.Pixel[150, 150] <> clWhite32, 'Center inside ROI polygon should be painted');
        Check(BlueComponent(bmp.Pixel[150, 150]) > 150, 'Center inside ROI polygon should be predominantly blue');

        // Stroke ring region at (95, 150) should be painted semi-transparent red
        Check(bmp.Pixel[95, 150] <> clWhite32, 'Stroked ROI expansion region at (95,150) should be painted');
        Check(RedComponent(bmp.Pixel[95, 150]) > 150, 'Stroked ROI expansion region should be predominantly red');

        // Region far outside ROI (10, 10) and (450, 450) must remain untouched white background
        CheckEquals(clWhite32, bmp.Pixel[10, 10], 'Pixel far outside ROI at (10,10) must remain white');
        CheckEquals(clWhite32, bmp.Pixel[450, 450], 'Pixel far outside ROI at (450,450) must remain white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestRoiFilterBlurRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(500, 500);
    bmp.Clear(clWhite32);

    // Small shape with Gaussian blur filter in large 500x500 canvas
    xml := '<svg width="500" height="500">' +
           '  <defs>' +
           '    <filter id="f_blur">' +
           '      <feGaussianBlur stdDeviation="10"/>' +
           '    </filter>' +
           '  </defs>' +
           '  <rect x="200" y="200" width="100" height="100" fill="red" filter="url(#f_blur)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Filter ROI blur docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center of blurred rect (250, 250) should be painted red
        Check(RedComponent(bmp.Pixel[250, 250]) > 150, 'Center of blurred rect should be red');

        // Blur margin region (180, 250) should contain soft blurred red alpha
        Check(bmp.Pixel[180, 250] <> clWhite32, 'Blur margin region at (180,250) should contain blurred pixels');
        Check(RedComponent(bmp.Pixel[180, 250]) > 0, 'Blur margin region should have red channel from blur');

        // Pixels outside filter ROI (50, 50) and (450, 450) must remain pure white
        CheckEquals(clWhite32, bmp.Pixel[50, 50], 'Pixel far outside blur ROI at (50,50) must remain white');
        CheckEquals(clWhite32, bmp.Pixel[450, 450], 'Pixel far outside blur ROI at (450,450) must remain white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestUserTransformTextSnippet;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  blackPixelCount, x, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg id="svg1" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg" font-family="Noto Sans" font-size="64">' +
           '  <g id="g1" transform="skewX(30) translate(-40 0)">' +
           '    <text id="text1" x="100" y="100" text-anchor="middle">Text</text>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'User snippet docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        blackPixelCount := 0;
        for y := 0 to 199 do
          for x := 0 to 199 do
            if bmp.Pixel[x, y] = clBlack32 then
              Inc(blackPixelCount);

        Check(blackPixelCount > 100, Format('Transformed text should render non-white pixels (found %d black pixels)', [blackPixelCount]));
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestBitmapPool;
var
  pool: TSvgBitmapPool;
  bmp1, bmp2, bmp3: TCustomBitmap32;
begin
  pool := TSvgBitmapPool.Create;
  try
    pool.BitmapMaxOversize := 128*1024; // Larger than 200*200*4-100*100*4

    // 1. Acquire new bitmap
    bmp1 := pool.Acquire(200, 200, True);
    Check(bmp1 <> nil, 'Acquire should return a valid TBitmap32');
    CheckEquals(200, bmp1.Width, 'Bitmap width should be 200');
    CheckEquals(200, bmp1.Height, 'Bitmap height should be 200');
    CheckEquals(0, Integer(bmp1.Pixel[0, 0]), 'Bitmap should be cleared');

    // Paint pixels to verify reuse on re-acquisition
    bmp1.Clear(clRed32);

    // 2. Release bitmap back to pool
    pool.Release(bmp1);

    // 3. Acquire smaller surface (100x100) -> Pool candidate (200x200) has sufficient buffer size
    bmp2 := pool.Acquire(100, 100, False);
    Check(bmp2 = bmp1, 'Pool should reuse candidate bitmap bmp1');
    CheckEquals(100, bmp2.Width, 'Bitmap physical width should be 100');
    CheckEquals(100, bmp2.Height, 'Bitmap physical height should be 100');
    CheckEquals(clRed32, bmp2.Pixel[10, 10], 'Surface should survive resize');

    // 4. Acquire surface requiring larger buffer (300x300) -> Candidate (200x200) is too small
    bmp3 := pool.Acquire(300, 300, False);
    Check(bmp3 <> bmp2, 'Pool should instantiate a new bitmap when candidate buffer is too small');
    CheckEquals(300, bmp3.Width, 'New bitmap width should be 300');
    CheckEquals(300, bmp3.Height, 'New bitmap height should be 300');

    pool.Release(bmp2);
    pool.Release(bmp3);
  finally
    pool.Free;
  end;
end;

procedure TTestSvgRenderer.TestTransformStack;

  function ParseSvgTransformHelper(const AStr: string): TFloatMatrix;
  var
    Value: TValuePUtf8Char;
  begin
    Value.Text := pointer(AnsiString(AStr));
    Value.Len := Length(AStr);
    Result := ParseSvgTransform(Value);
  end;

var
  renderer: TSvgRenderer;
  initialMat: TFloatMatrix;
begin
  renderer := TSvgRenderer.Create(nil);
  try
    initialMat := renderer.Transformation.Matrix;
    renderer.Transformation.Push;
    renderer.ApplyMatrix(ParseSvgTransformHelper('translate(20, 30)'));

    Check(FloatRect(0, 0, 0, 0) <> FloatRect(1, 1, 1, 1), 'Dummy check');
    Check(renderer.Transformation.Matrix[2, 0] = 20.0, 'Translate X should be 20');
    Check(renderer.Transformation.Matrix[2, 1] = 30.0, 'Translate Y should be 30');

    renderer.Transformation.Pop;
    Check(renderer.Transformation.Matrix[2, 0] = initialMat[2, 0], 'Matrix X should be restored');
    Check(renderer.Transformation.Matrix[2, 1] = initialMat[2, 1], 'Matrix Y should be restored');
  finally
    renderer.Free;
  end;
end;

procedure TTestSvgRenderer.TestGradientFillRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">' +
           '      <stop offset="0%" stop-color="red"/>' +
           '      <stop offset="100%" stop-color="blue"/>' +
           '    </linearGradient>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="100" height="100" fill="url(#grad1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Gradient docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        Check(bmp.Pixel[5, 50] <> clWhite32, 'Left side should be colored');
        Check(bmp.Pixel[95, 50] <> clWhite32, 'Right side should be colored');
        Check(RedComponent(bmp.Pixel[5, 50]) > BlueComponent(bmp.Pixel[5, 50]), 'Left side should be predominantly red');
        Check(BlueComponent(bmp.Pixel[95, 50]) > RedComponent(bmp.Pixel[95, 50]), 'Right side should be predominantly blue');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestGroupOpacityCompositing;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <g opacity="0.5">' +
           '    <rect x="0" y="0" width="100" height="100" fill="black"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Opacity docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        // Black rendered with 0.5 opacity over white results in gray (~127..128)
        Check(RedComponent(bmp.Pixel[50, 50]) > 100, 'Semi-transparent black over white should be gray');
        Check(RedComponent(bmp.Pixel[50, 50]) < 160, 'Semi-transparent black over white should be gray');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestClipPathCompositing;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <clipPath id="clip1">' +
           '      <rect x="0" y="0" width="50" height="100"/>' +
           '    </clipPath>' +
           '  </defs>' +
           '  <g clip-path="url(#clip1)">' +
           '    <rect x="0" y="0" width="100" height="100" fill="red"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'ClipPath docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clRed32, bmp.Pixel[25, 50], 'Clipped region inside should be red');
        CheckEquals(clWhite32, bmp.Pixel[75, 50], 'Clipped region outside should be white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestPatternFillAndStrokeRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);

    xml := '<svg viewBox="0 0 200 100" xmlns="http://www.w3.org/2000/svg">' +
           '  <defs>' +
           '    <pattern id="star" viewBox="0,0,10,10" width="10" height="10" patternUnits="userSpaceOnUse">' +
           '      <polygon points="0,0 2,5 0,10 5,8 10,10 8,5 10,0 5,2" fill="red"/>' +
           '    </pattern>' +
           '  </defs>' +
           '  <circle cx="50" cy="50" r="40" fill="url(#star)"/>' +
           '  <circle cx="150" cy="50" r="30" fill="none" stroke-width="20" stroke="url(#star)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Pattern fill/stroke docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Check top-left corner (0,0) where <defs> was placed - must remain white background!
        CheckEquals(clWhite32, bmp.Pixel[0, 0], 'Top-left corner where <defs> is defined must not render directly');

        // Pattern fill check on the left circle
        Check(bmp.Pixel[50, 50] <> clWhite32, 'Center of left circle should be painted by pattern fill');

        // Pattern stroke check on the right circle (ring region around r=30)
        Check(bmp.Pixel[150, 20] <> clWhite32, 'Stroke ring of right circle top edge should be painted by pattern stroke');
        Check(bmp.Pixel[150, 50] = clWhite32, 'Center of hollow stroked circle should remain white background');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestStrokeWidthRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  minY, maxY, y: Integer;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    // Line from x=10 to x=90 at y=50 with stroke-width 10
    xml := '<svg width="100" height="100">' +
           '  <line x1="10" y1="50" x2="90" y2="50" stroke="green" stroke-width="10"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        minY := 100;
        maxY := -1;
        for y := 0 to 99 do
        begin
          if bmp.Pixel[50, y] = clGreen32 then
          begin
            if y < minY then minY := y;
            if y > maxY then maxY := y;
          end;
        end;

        // Line y=50, stroke-width=10 -> expected green pixels from y=45 to y=55 (height 10 or 11)
        Check(minY >= 44, Format('minY was %d, expected >= 44', [minY]));
        Check(maxY <= 55, Format('maxY was %d, expected <= 55', [maxY]));
        Check((maxY - minY + 1) <= 12, Format('Stroke height was %d, expected ~10', [maxY - minY + 1]));

      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // Test User complete SVG snippet with viewBox scale (12cm x 5.25cm, viewBox 1200x400)
    bmp.Clear(clWhite32);
    bmp.SetSize(454, 198); // 12cm x 5.25cm at 96 DPI
    xml := '<?xml version="1.0" standalone="no"?>' +
           '<svg width="12cm" height="5.25cm" viewBox="0 0 1200 400" xmlns="http://www.w3.org/2000/svg" version="1.1">' +
           '  <rect x="10" y="5" width="1140" height="390" fill="white" stroke="green" stroke-width="10" />' +
           '  <path d="M300,200 h-150 a150,150 0 1,0 150,-150 z" fill="red" stroke="blue" stroke-width="5" />' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'user snippet docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Rect top edge: rect y=5 in viewBox 1200x400 scaled to 454x198 bitmap under xMidYMid meet alignment.
        // scale = Min(454/1200, 198/400) = 0.378333.
        // Vertical offset ty = (198 - 400 * 0.378333) / 2 = 23.33 pixels.
        // Rect y=5 in viewBox -> bitmap y = 23.33 + 5 * 0.378333 = 25.22.
        // stroke-width 10 in viewBox -> ~3.8 pixels on bitmap (y = 23..27).
        minY := 198;
        maxY := -1;
        for y := 0 to 50 do
        begin
          if bmp.Pixel[200, y] = clGreen32 then
          begin
            if y < minY then minY := y;
            if y > maxY then maxY := y;
          end;
        end;

        Check(minY >= 21, Format('ViewBox scaled rect top minY was %d, expected ~23', [minY]));
        Check(maxY <= 29, Format('ViewBox scaled rect top maxY was %d, expected ~27', [maxY]));
        Check((maxY - minY + 1) <= 6, Format('ViewBox scaled stroke height was %d, expected ~4 pixels', [maxY - minY + 1]));

      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestMixBlendModeAndIsolationRendering;
var
  bmpAuto, bmpIsolate: TBitmap32;
  docNodeAuto, docNodeIsolate: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xmlAuto, xmlIsolate: UTF8String;
  overlapPixel: TColor32;
  pixelAuto, pixelIsolate: TColor32;
begin
  bmpAuto := TBitmap32.Create;
  bmpIsolate := TBitmap32.Create;
  try
    bmpAuto.SetSize(100, 100);
    bmpIsolate.SetSize(100, 100);

    // 1. Multiply blend mode test: Red (255,0,0) * Blue (0,0,255) = Black (0,0,0)
    bmpAuto.Clear(clWhite32);
    xmlAuto := '<svg width="100" height="100">' +
               '  <rect x="0" y="0" width="100" height="100" fill="red"/>' +
               '  <rect x="0" y="0" width="100" height="100" fill="blue" mix-blend-mode="multiply"/>' +
               '</svg>';

    docNodeAuto := ParseSvgXml(xmlAuto);
    Check(docNodeAuto <> nil, 'Multiply blend docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmpAuto);
      try
        renderer.RenderDocument(docNodeAuto);
        overlapPixel := bmpAuto.Pixel[50, 50];
        CheckEquals(clBlack32, overlapPixel, 'Red multiplied by Blue should yield Black');
      finally
        renderer.Free;
      end;
    finally
      docNodeAuto.Free;
    end;

    // 2. Screen blend mode test: Red (255,0,0) + Blue (0,0,255) = Magenta (255,0,255)
    bmpAuto.Clear(clWhite32);
    xmlAuto := '<svg width="100" height="100">' +
               '  <rect x="0" y="0" width="100" height="100" fill="red"/>' +
               '  <rect x="0" y="0" width="100" height="100" fill="blue" mix-blend-mode="screen"/>' +
               '</svg>';

    docNodeAuto := ParseSvgXml(xmlAuto);
    Check(docNodeAuto <> nil, 'Screen blend docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmpAuto);
      try
        renderer.RenderDocument(docNodeAuto);
        overlapPixel := bmpAuto.Pixel[50, 50];
        CheckEquals(Color32(255, 0, 255, 255), overlapPixel, 'Red screened with Blue should yield Magenta');
      finally
        renderer.Free;
      end;
    finally
      docNodeAuto.Free;
    end;

    // 3. Compare isolation="auto" vs isolation="isolate" on a group with mix-blend-mode="multiply"
    xmlAuto := '<svg width="100" height="100">' +
               '  <rect width="100" height="100" fill="#CCCCCC"/>' +
               '  <g style="isolation: auto; mix-blend-mode: multiply;">' +
               '    <rect width="100" height="100" fill="#00FFFF"/>' +
               '  </g>' +
               '</svg>';

    xmlIsolate := '<svg width="100" height="100">' +
                 '  <rect width="100" height="100" fill="#CCCCCC"/>' +
                 '  <g style="isolation: isolate; mix-blend-mode: multiply;">' +
                 '    <rect width="100" height="100" fill="#00FFFF"/>' +
                 '  </g>' +
                 '</svg>';

    docNodeAuto := ParseSvgXml(xmlAuto);
    docNodeIsolate := ParseSvgXml(xmlIsolate);
    try
      bmpAuto.Clear(clWhite32);
      renderer := TSvgRenderer.Create(bmpAuto);
      try
        renderer.RenderDocument(docNodeAuto);
      finally
        renderer.Free;
      end;

      bmpIsolate.Clear(clWhite32);
      renderer := TSvgRenderer.Create(bmpIsolate);
      try
        renderer.RenderDocument(docNodeIsolate);
      finally
        renderer.Free;
      end;

      pixelAuto := bmpAuto.Pixel[50, 50];
      pixelIsolate := bmpIsolate.Pixel[50, 50];

      CheckEquals(pixelAuto, pixelIsolate, 'Single child inside group should match under both isolation modes');
    finally
      docNodeAuto.Free;
      docNodeIsolate.Free;
    end;

  finally
    bmpAuto.Free;
    bmpIsolate.Free;
  end;
end;

procedure TTestSvgRenderer.TestMaskCompositing;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <mask id="mask1">' +
           '      <rect x="0" y="0" width="50" height="100" fill="white"/>' +
           '    </mask>' +
           '  </defs>' +
           '  <g mask="url(#mask1)">' +
           '    <rect x="0" y="0" width="100" height="100" fill="blue"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Mask docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clBlue32, bmp.Pixel[25, 50], 'Masked region inside should be blue');
        CheckEquals(clWhite32, bmp.Pixel[75, 50], 'Masked region outside should remain white background');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestMarkerRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <marker id="dot" refX="5" refY="5" markerWidth="10" markerHeight="10" markerUnits="userSpaceOnUse">' +
           '      <circle cx="5" cy="5" r="5" fill="red"/>' +
           '    </marker>' +
           '    <marker id="arrow" refX="0" refY="5" markerWidth="10" markerHeight="10" orient="auto" markerUnits="userSpaceOnUse">' +
           '      <rect x="0" y="0" width="10" height="10" fill="green"/>' +
           '    </marker>' +
           '  </defs>' +
           '  <path d="M 20 20 L 80 20 L 80 80" stroke="blue" stroke-width="2" marker-start="url(#dot)" marker-mid="url(#dot)" marker-end="url(#dot)"/>' +
           '  <path d="M 10 90 L 90 90" stroke="black" stroke-width="2" marker-end="url(#arrow)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Marker docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Check top-left corner where <defs> is defined - must remain white background!
        CheckEquals(clWhite32, bmp.Pixel[0, 0], 'Top-left corner where <defs> is defined must not render directly');

        // Check marker position 1 (start vertex at 20, 20)
        CheckEquals(clRed32, bmp.Pixel[20, 20], 'Marker start vertex at (20,20) should render red dot');

        // Check marker position 2 (mid vertex at 80, 20)
        CheckEquals(clRed32, bmp.Pixel[80, 20], 'Marker mid vertex at (80,20) should render red dot');

        // Check marker position 3 (end vertex at 80, 80)
        CheckEquals(clRed32, bmp.Pixel[80, 80], 'Marker end vertex at (80,80) should render red dot');

        // Check angled marker position 4 (end vertex at 90, 90)
        CheckEquals(clGreen32, bmp.Pixel[95, 90], 'Angled arrow marker at (90,90) should render green rect at target location');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // Additional check: Marker with viewBox (0 0 20 20) scaling to markerWidth 10 height 10 and refX="10" refY="10"
    bmp.Clear(clWhite32);
    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <marker id="vb_marker" viewBox="0 0 20 20" refX="10" refY="10" markerWidth="10" markerHeight="10" markerUnits="userSpaceOnUse">' +
           '      <circle cx="10" cy="10" r="10" fill="red"/>' +
           '    </marker>' +
           '  </defs>' +
           '  <path d="M 50 50 L 90 50" stroke="blue" stroke-width="2" marker-start="url(#vb_marker)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'viewBox marker docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clRed32, bmp.Pixel[50, 50], 'viewBox marker with refX=10, refY=10 should center red dot at (50,50)');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestObjectBoundingBoxMaskAndGradient;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="100" height="100">' +
           '  <defs>' +
           '    <mask id="m1" maskContentUnits="objectBoundingBox">' +
           '      <rect x="0" y="0" width="0.5" height="1.0" fill="white"/>' +
           '    </mask>' +
           '  </defs>' +
           '  <g mask="url(#m1)">' +
           '    <rect x="20" y="20" width="60" height="60" fill="red"/>' +
           '  </g>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'ObjectBoundingBox mask docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        CheckEquals(clRed32, bmp.Pixel[35, 50], 'Inside objectBoundingBox mask should be red');
        CheckEquals(clWhite32, bmp.Pixel[65, 50], 'Outside objectBoundingBox mask should remain white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestRadialGradientReflect;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
  p1, p2, p3: TColor32;
  differentColors: Boolean;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<svg width="200" height="200">' +
           '  <defs>' +
           '    <linearGradient id="baseGrad">' +
           '      <stop offset="0" stop-color="black"/>' +
           '      <stop offset="1" stop-color="white"/>' +
           '    </linearGradient>' +
           '    <radialGradient id="radReflect" href="#baseGrad" cx="10" cy="10" r="10" spreadMethod="reflect" gradientUnits="userSpaceOnUse"/>' +
           '  </defs>' +
           '  <rect x="0" y="0" width="200" height="200" fill="url(#radReflect)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Radial reflect docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center near (10, 10) is dark/black (stop 0)
        // Distance 10 (e.g. 20, 10) is white (stop 1)
        // Distance 20 (e.g. 30, 10) reflects back to black (stop 0)
        p1 := bmp.Pixel[10, 10];
        p2 := bmp.Pixel[20, 10];
        p3 := bmp.Pixel[30, 10];

        Check(RedComponent(p1) < 50, 'Center at (10,10) should be near black');
        Check(RedComponent(p2) > 200, 'Radius at (20,10) should be near white');
        Check(RedComponent(p3) < 50, 'Reflected radius at (30,10) should reflect back to near black');

        differentColors := (p1 <> p2) and (p2 <> p3);
        Check(differentColors, 'Radial gradient with spreadMethod="reflect" must render varying reflection wave colors across canvas');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

    // Test user Inkscape wave pattern snippet with gradientTransform, spreadMethod="reflect", and style="..." on <stop> tags
    bmp.Clear(clWhite32);
    bmp.SetSize(406, 206);
    xml := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<svg width="406.25" height="206.25">' +
           '  <defs>' +
           '    <linearGradient id="g1">' +
           '      <stop offset="0" style="stop-color: rgb(50, 50, 50); stop-opacity: 1;"/>' +
           '      <stop offset="1" style="stop-color: rgb(150, 150, 150); stop-opacity: 1;"/>' +
           '    </linearGradient>' +
           '    <radialGradient id="r1" href="#g1" gradientUnits="userSpaceOnUse" ' +
           '      gradientTransform="matrix(0.793492, 0, 0, 1.26025, -124.125, -349.237)" ' +
           '      spreadMethod="reflect" cx="195.33907" cy="367.99432" r="10.189606"/>' +
           '  </defs>' +
           '  <rect x="25.875" y="3.125" width="375" height="200" fill="url(#r1)"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'User snippet docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Center is at (30.875, 114.528) -> rgb(50,50,50)
        // Radii rx ~ 8, ry ~ 12.8 -> alternating rgb(150,150,150) and rgb(50,50,50) wave rings
        p1 := bmp.Pixel[31, 115];
        p2 := bmp.Pixel[39, 115];
        p3 := bmp.Pixel[47, 115];

        Check(RedComponent(p1) < 80, 'User snippet center at (31,115) should be near rgb(50,50,50)');
        Check(RedComponent(p2) > 120, 'User snippet radius at (39,115) should be near rgb(150,150,150)');
        Check(RedComponent(p3) < 80, 'User snippet reflected radius at (47,115) should reflect back to near rgb(50,50,50)');

        differentColors := (p1 <> p2) and (p2 <> p3);
        Check(differentColors, 'Inkscape wave snippet must render varying stop colors across canvas');

      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;

  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestSymbolRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 200);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="200">' +
           '  <defs>' +
           '    <symbol id="mySymbol" viewBox="0 0 20 20">' +
           '      <circle cx="10" cy="10" r="10" fill="red"/>' +
           '    </symbol>' +
           '  </defs>' +
           '  <use href="#mySymbol" x="50" y="50" width="100" height="100"/>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'docNode should not be nil');
      try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Check top-left corner (0,0) where <defs> symbol template was defined - must remain white background!
        CheckEquals(clWhite32, bmp.Pixel[10, 10], 'Unreferenced symbol template must not render at top-left origin');

        // Check center of instantiated symbol at x=50, y=50, width=100, height=100 (center at 100, 100)
        CheckEquals(clRed32, bmp.Pixel[100, 100], 'Center of instantiated symbol at (100,100) should be rendered red');

        // Check outside instantiated symbol bounds (e.g. 10, 100)
        CheckEquals(clWhite32, bmp.Pixel[10, 100], 'Outside instantiated symbol bounds must remain white');
      finally
        renderer.Free;
      end;
      finally
      docNode.Free;
      end;
    finally
    bmp.Free;
    end;
end;

procedure TTestSvgRenderer.TestFilterRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pCenter, pOffset: TColor32;
begin
  // Test feGaussianBlur, feOffset, feFlood, feComposite, feBlend, feMerge primitive rendering
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <filter id="f_offset">' +
         '      <feOffset in="SourceGraphic" dx="10" dy="10" result="off1"/>' +
         '      <feMerge>' +
         '        <feMergeNode in="off1"/>' +
         '        <feMergeNode in="SourceGraphic"/>' +
         '      </feMerge>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect x="10" y="10" width="30" height="30" fill="blue" filter="url(#f_offset)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    Check(not renderer.AllowExternalImages, 'AllowExternalImages should default to False');

    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    pCenter := bmp.Pixel[20, 20];
    pOffset := bmp.Pixel[35, 35];

    CheckEquals(clBlue32, pCenter, 'Original rect position should be painted blue');
    CheckEquals(clBlue32, pOffset, 'Offset merged rect position should also be painted blue');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestSwitchRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  xml := '<svg width="100" height="100">' +
         '  <switch>' +
         '    <rect width="100" height="100" fill="red" systemLanguage="fr"/>' +
         '    <rect width="100" height="100" fill="green" systemLanguage="en"/>' +
         '    <rect width="100" height="100" fill="blue"/>' +
         '  </switch>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(100, 100);

    // 1. Render when SystemLanguage is 'en' -> Green rectangle selected
    SetSystemLanguage('en');
    docNode.Resolve; // switch is resolved at parse time, so we must resolve again
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clGreen32, bmp.Pixel[50, 50], 'Switch should render green rect when language is en');

    // 2. Render when SystemLanguage is 'fr' -> Red rectangle selected
    SetSystemLanguage('fr');
    docNode.Resolve; // switch is resolved at parse time, so we must resolve again
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clRed32, bmp.Pixel[50, 50], 'Switch should render red rect when language is fr');

    // 3. Render when SystemLanguage is 'de' -> Blue fallback rectangle selected
    SetSystemLanguage('de');
    docNode.Resolve; // switch is resolved at parse time, so we must resolve again
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clBlue32, bmp.Pixel[50, 50], 'Switch should render blue fallback rect when language is de');

  finally
    SetSystemLanguage('en');
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestTextRendering;
var
  bmp: TBitmap32;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  xml: UTF8String;
begin
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(200, 100);
    bmp.Clear(clWhite32);

    xml := '<svg width="200" height="100">' +
           '  <text x="20" y="50" font-size="24px" fill="red">SVG Text</text>' +
           '</svg>';

    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'Text docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);
        // Background at (0,0) must remain white
        CheckEquals(clWhite32, bmp.Pixel[0, 0], 'Background at (0,0) should remain white');
      finally
        renderer.Free;
      end;
    finally
      docNode.Free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeColorMatrixRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pPixel: TColor32;
begin
  // Test 1: Custom diagonal values in feColorMatrix (swap Red and Blue channels)
  xml := '<svg width="10" height="10">' +
         '  <defs>' +
         '    <filter id="f_swap">' +
         '      <feColorMatrix type="matrix" values="0 0 1 0 0  0 1 0 0 0  1 0 0 0 0  0 0 0 1 0"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect width="10" height="10" fill="red" filter="url(#f_swap)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(10, 10);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    pPixel := bmp.Pixel[5, 5];
    CheckEquals(clBlue32, pPixel, 'Red channel swapped to Blue should render clBlue32');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeCompositeArithmeticRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pPixel: TColor32;
begin
  // Test 2: Arithmetic compositing with K2=0.5, K3=0.5
  xml := '<svg width="10" height="10">' +
         '  <defs>' +
         '    <filter id="f_arith">' +
         '      <feFlood flood-color="blue" result="blue_bg"/>' +
         '      <feComposite in="SourceGraphic" in2="blue_bg" operator="arithmetic" k1="0" k2="0.5" k3="0.5" k4="0"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect width="10" height="10" fill="red" filter="url(#f_arith)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(10, 10);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    pPixel := bmp.Pixel[5, 5];
    if (Abs(RedComponent(pPixel) - 128) > 1) then
      CheckEquals(128, RedComponent(pPixel), 'Arithmetic K2=0.5 of Red (255) should give 128 Red');
    if (Abs(BlueComponent(pPixel) - 128) > 1) then
      CheckEquals(128, BlueComponent(pPixel), 'Arithmetic K3=0.5 of Blue (255) should give 128 Blue');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeCompositeDropShadowRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  shadowPixel: TColor32;
begin
  // Test feOffset -> feGaussianBlur -> feFlood -> feComposite (in2 omitted) -> feComposite (SourceGraphic over shadow)
  xml := '<svg width="100" height="100" viewBox="0 0 100 100">' +
         '  <defs>' +
         '    <filter id="f_dropshadow" x="-20%" y="-20%" width="160%" height="160%">' +
         '      <feOffset dx="10" dy="10"/>' +
         '      <feGaussianBlur result="blur" stdDeviation="0"/>' +
         '      <feFlood flood-color="#000000" flood-opacity="1"/>' +
         '      <feComposite in2="blur" operator="in"/>' +
         '      <feComposite in="SourceGraphic"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect x="10" y="10" width="30" height="30" fill="red" filter="url(#f_dropshadow)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // SourceGraphic red square is at (10,10)..(40,40)
    CheckEquals(clRed32, bmp.Pixel[20, 20], 'Original shape at (20,20) should render red');

    // Shadow offset square is at (20,20)..(50,50). At (45,45), shape is absent but shadow is present.
    shadowPixel := bmp.Pixel[45, 45];
    CheckEquals(clBlack32, shadowPixel, 'Offset drop shadow at (45,45) should render black');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestFeComponentTransferRendering;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pixel: TColor32;
begin
  // Test feComponentTransfer with linear and table functions
  xml := '<svg width="10" height="10">' +
         '  <defs>' +
         '    <filter id="f_ct">' +
         '      <feComponentTransfer>' +
         '        <feFuncR type="linear" slope="0.5" intercept="0"/>' +
         '        <feFuncG type="table" tableValues="1 0"/>' +
         '        <feFuncB type="discrete" tableValues="0 1"/>' +
         '      </feComponentTransfer>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect width="10" height="10" fill="rgb(200,255,0)" filter="url(#f_ct)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(10, 10);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    pixel := bmp.Pixel[5, 5];
    // R: 200 * 0.5 = 100
    CheckEquals(100, RedComponent(pixel), 'Red channel should be scaled by slope 0.5 to 100');
    // G: table "1 0" inverts 255 (1.0) to 0 (0.0)
    CheckEquals(0, GreenComponent(pixel), 'Green channel 255 inverted via table "1 0" should be 0');
    // B: discrete "0 1" for 0 maps to index 0 -> 0
    CheckEquals(0, BlueComponent(pixel), 'Blue channel 0 mapped via discrete "0 1" should be 0');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestNestedSvgViewportComponentTransfer;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
  pIdentity, pTable, pLinear, pGamma: TColor32;
begin
  // Test nested <svg> element with viewBox, linearGradient, and feComponentTransfer (identity, table, linear, gamma)
  xml := '<svg width="480" height="360" viewBox="0 0 480 360">' +
         '  <g>' +
         '    <svg x="15" y="5" width="450" height="300" viewBox="0 0 630 420">' +
         '      <defs>' +
         '        <linearGradient id="MyGrad" gradientUnits="userSpaceOnUse" x1="10" y1="0" x2="590" y2="0">' +
         '          <stop offset="0" stop-color="#ff0000"/>' +
         '          <stop offset="0.33" stop-color="#00ff00"/>' +
         '          <stop offset="0.67" stop-color="#0000ff"/>' +
         '          <stop offset="1" stop-color="#000000"/>' +
         '        </linearGradient>' +
         '        <filter id="Identity">' +
         '          <feComponentTransfer>' +
         '            <feFuncR type="identity"/>' +
         '            <feFuncG type="identity"/>' +
         '            <feFuncB type="identity"/>' +
         '            <feFuncA type="identity"/>' +
         '          </feComponentTransfer>' +
         '        </filter>' +
         '        <filter id="Table">' +
         '          <feComponentTransfer>' +
         '            <feFuncR type="table" tableValues="0 0 1 1"/>' +
         '            <feFuncG type="table" tableValues="1 1 0 0"/>' +
         '            <feFuncB type="table" tableValues="0 1 1 0"/>' +
         '          </feComponentTransfer>' +
         '        </filter>' +
         '        <filter id="Linear">' +
         '          <feComponentTransfer>' +
         '            <feFuncR type="linear" slope="0.5" intercept="0.25"/>' +
         '            <feFuncG type="linear" slope="0.5" intercept="0"/>' +
         '            <feFuncB type="linear" slope="0.5" intercept="0.5"/>' +
         '          </feComponentTransfer>' +
         '        </filter>' +
         '        <filter id="Gamma">' +
         '          <feComponentTransfer>' +
         '            <feFuncR type="gamma" amplitude="2" exponent="5" offset="0"/>' +
         '            <feFuncG type="gamma" amplitude="2" exponent="3" offset="0"/>' +
         '            <feFuncB type="gamma" amplitude="2" exponent="1" offset="0"/>' +
         '          </feComponentTransfer>' +
         '        </filter>' +
         '      </defs>' +
         '      <rect x="10" y="10" width="580" height="40" fill="url(#MyGrad)" filter="url(#Identity)"/>' +
         '      <rect x="10" y="110" width="580" height="40" fill="url(#MyGrad)" filter="url(#Table)"/>' +
         '      <rect x="10" y="210" width="580" height="40" fill="url(#MyGrad)" filter="url(#Linear)"/>' +
         '      <rect x="10" y="310" width="580" height="40" fill="url(#MyGrad)" filter="url(#Gamma)"/>' +
         '    </svg>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(480, 360);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Inner <svg> (x=15, y=5, w=450, h=300, viewBox=630x420) scales 450/630 = 0.714
    // Rect 1 (Identity): x=10..590, y=10..50 -> screen x=22..428, y=12..40
    pIdentity := bmp.Pixel[25, 25];
    Check(RedComponent(pIdentity) > 200, 'Identity rect at left edge should render red gradient start');

    // Rect 2 (Table): y=110..150 -> screen y=83..112
    pTable := bmp.Pixel[25, 95];
    Check(AlphaComponent(pTable) > 200, 'Table rect should be rendered with valid alpha');

    // Rect 3 (Linear): y=210..250 -> screen y=155..183
    pLinear := bmp.Pixel[25, 170];
    Check(AlphaComponent(pLinear) > 200, 'Linear rect should be rendered with valid alpha');

    // Rect 4 (Gamma): y=310..350 -> screen y=226..255
    pGamma := bmp.Pixel[25, 240];
    Check(AlphaComponent(pGamma) > 200, 'Gamma rect should be rendered with valid alpha');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestImageRendering;
var
  bmp: TBitmap32;
  renderer: TSvgRenderer;
  docNode: TSvgDocumentNode;
  xml: UTF8String;
  pPixel: TColor32;
begin
  xml := '<svg width="100" height="100">' +
         '  <image x="10" y="10" width="50" height="50" href="data:image/svg+xml;utf8,&lt;svg width=&quot;50&quot; height=&quot;50&quot;&gt;&lt;rect width=&quot;50&quot; height=&quot;50&quot; fill=&quot;red&quot;/&gt;&lt;/svg&gt;"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    pPixel := bmp.Pixel[30, 30];
    CheckEquals(clRed32, pPixel, 'Pixel inside rendered embedded SVG image should be red');
    CheckEquals(clWhite32, bmp.Pixel[5, 5], 'Pixel outside rendered embedded SVG image should be white');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

procedure TTestSvgRenderer.TestNestedSvgAndCurrentColorRendering;
var
  bmp: TBitmap32;
  renderer: TSvgRenderer;
  docNode: TSvgDocumentNode;
  xml: UTF8String;
begin
  xml := '<svg width="100" height="100" color="green">' +
         '  <defs>' +
         '    <g id="defSymbol">' +
         '      <rect width="40" height="40" fill="currentColor"/>' +
         '    </g>' +
         '  </defs>' +
         '  <svg color="blue" viewBox="0 0 100 100">' +
         '    <use xlink:href="#defSymbol" x="10" y="10"/>' +
         '    <rect x="60" y="60" width="30" height="30" fill="currentColor"/>' +
         '  </svg>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  bmp := TBitmap32.Create;
  renderer := TSvgRenderer.Create(bmp);
  try
    bmp.SetSize(100, 100);
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);

    // Inner <svg color="blue"> overrides inherited green color for currentColor fills inside innerSvg
    CheckEquals(clBlue32, bmp.Pixel[20, 20], 'Pixel inside use symbol with currentColor should be blue');
    CheckEquals(clBlue32, bmp.Pixel[70, 70], 'Pixel inside rect with currentColor should be blue');
    CheckEquals(clWhite32, bmp.Pixel[5, 5], 'Background pixel should remain white');
  finally
    renderer.Free;
    bmp.Free;
    docNode.Free;
  end;
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgRenderer.Suite);
{$ELSE}
  RegisterTest(TTestSvgRenderer.Suite);
{$ENDIF}

end.
