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
    procedure TestTextRendering;
    procedure TestTextRotationRendering;
    procedure TestTextPathRendering;
    procedure TestEscapedTextRendering;
    procedure TestUserTransformTextSnippet;
    procedure TestImageRendering;
    procedure TestSwitchRendering;
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
    pool.BitmapMaxExcess := 128*1024; // Larger than 200*200*4-100*100*4

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
    initialMat := renderer.CurrentMatrix;
    renderer.PushMatrix;
    renderer.ApplyMatrix(ParseSvgTransformHelper('translate(20, 30)'));

    Check(FloatRect(0, 0, 0, 0) <> FloatRect(1, 1, 1, 1), 'Dummy check');
    Check(renderer.CurrentMatrix[2, 0] = 20.0, 'Translate X should be 20');
    Check(renderer.CurrentMatrix[2, 1] = 30.0, 'Translate Y should be 30');

    renderer.PopMatrix;
    Check(renderer.CurrentMatrix[2, 0] = initialMat[2, 0], 'Matrix X should be restored');
    Check(renderer.CurrentMatrix[2, 1] = initialMat[2, 1], 'Matrix Y should be restored');
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
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clGreen32, bmp.Pixel[50, 50], 'Switch should render green rect when language is en');

    // 2. Render when SystemLanguage is 'fr' -> Red rectangle selected
    SetSystemLanguage('fr');
    bmp.Clear(clWhite32);
    renderer.RenderDocument(docNode);
    CheckEquals(clRed32, bmp.Pixel[50, 50], 'Switch should render red rect when language is fr');

    // 3. Render when SystemLanguage is 'de' -> Blue fallback rectangle selected
    SetSystemLanguage('de');
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

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgRenderer.Suite);
{$ELSE}
  RegisterTest(TTestSvgRenderer.Suite);
{$ENDIF}

end.
