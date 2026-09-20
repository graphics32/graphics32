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
