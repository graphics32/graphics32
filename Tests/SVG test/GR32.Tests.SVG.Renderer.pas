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
    procedure TestMaskCompositing;
    procedure TestBitmapPool;
    procedure TestPatternFillAndStrokeRendering;
    procedure TestStrokeWidthRendering;
  end;

implementation

uses
  Types;

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
var
  renderer: TSvgRenderer;
  initialMat: TFloatMatrix;
begin
  renderer := TSvgRenderer.Create(nil);
  try
    initialMat := renderer.CurrentMatrix;
    renderer.PushMatrix;
    renderer.ApplyMatrix(ParseSvgTransform('translate(20, 30)'));

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

    // Test Rectangle stroke width
    bmp.Clear(clWhite32);
    xml := '<svg width="1200" height="400">' +
           '  <rect x="10" y="5" width="1140" height="390" fill="white" stroke="green" stroke-width="10" />' +
           '</svg>';

    bmp.SetSize(1200, 400);
    docNode := ParseSvgXml(xml);
    Check(docNode <> nil, 'rect docNode should not be nil');
    try
      renderer := TSvgRenderer.Create(bmp);
      try
        renderer.RenderDocument(docNode);

        // Top edge: rect y=5, stroke-width=10 -> expected green from y=0 to y=10
        minY := 400;
        maxY := -1;
        for y := 0 to 30 do
        begin
          if bmp.Pixel[500, y] = clGreen32 then
          begin
            if y < minY then minY := y;
            if y > maxY then maxY := y;
          end;
        end;

        Check(minY >= 0, Format('Rect top minY was %d', [minY]));
        Check(maxY <= 11, Format('Rect top maxY was %d', [maxY]));
        Check((maxY - minY + 1) <= 12, Format('Rect top stroke height was %d, expected ~10', [maxY - minY + 1]));

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

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgRenderer.Suite);
{$ELSE}
  RegisterTest(TTestSvgRenderer.Suite);
{$ENDIF}

end.
