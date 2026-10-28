unit GR32.Tests.SVG.Gradients;

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

{$WARN IMPLICIT_STRING_CAST OFF}

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  Types, SysUtils, Classes,
  GR32, GR32.SVG.Types, GR32.SVG.Tree, GR32.SVG.Renderer;

type
  TTestSvgGradients = class(TTestCase)
  published
    procedure TestLinearGradientParsing;
    procedure TestRadialGradientParsing;
    procedure TestGradientInheritance;
    procedure TestGradientInheritanceUserSpaceOnUse;
    procedure TestClipPathAndMaskParsing;
    procedure TestConicalGradientParsing;
    procedure TestConicalGradientInheritance;
    procedure TestZeroDimensionsObjectBoundingBoxGradientFallback;
  end;

implementation

{ TTestSvgGradients }

procedure TTestSvgGradients.TestLinearGradientParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  gradNode: TSvgLinearGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="100%" spreadMethod="reflect" gradientUnits="userSpaceOnUse">' +
         '      <stop offset="0%" stop-color="red" stop-opacity="1.0"/>' +
         '      <stop offset="100%" stop-color="blue" stop-opacity="0.5"/>' +
         '    </linearGradient>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(1, docNode.Children.Count);
    Check(docNode.Children[0] is TSvgDefsNode, 'Child should be TSvgDefsNode');
    defsNode := TSvgDefsNode(docNode.Children[0]);

    CheckEquals(1, defsNode.Children.Count);
    Check(defsNode.Children[0] is TSvgLinearGradientNode, 'Child should be TSvgLinearGradientNode');

    gradNode := TSvgLinearGradientNode(defsNode.Children[0]);
    CheckEquals('grad1', gradNode.ID);
    CheckEquals(Ord(smReflect), Ord(gradNode.SpreadMethod));
    CheckEquals(Ord(guUserSpaceOnUse), Ord(gradNode.GradientUnits));

    CheckEquals(2, gradNode.Stops.Count);
    CheckEquals(0.0, gradNode.Stops[0].Offset, 1E-4);
    CheckEquals(clRed32, gradNode.Stops[0].Color.Color);
    CheckEquals(1.0, gradNode.Stops[0].Opacity, 1E-4);

    CheckEquals(1.0, gradNode.Stops[1].Offset, 1E-4);
    CheckEquals(clBlue32, gradNode.Stops[1].Color.Color);
    CheckEquals(0.5, gradNode.Stops[1].Opacity, 1E-4);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestRadialGradientParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  gradNode: TSvgRadialGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <radialGradient id="grad2" cx="50%" cy="50%" r="50%" fx="30%" fy="30%">' +
         '      <stop offset="0%" stop-color="white"/>' +
         '      <stop offset="100%" stop-color="black"/>' +
         '    </radialGradient>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(1, docNode.Children.Count);
    defsNode := TSvgDefsNode(docNode.Children[0]);

    CheckEquals(1, defsNode.Children.Count);
    Check(defsNode.Children[0] is TSvgRadialGradientNode, 'Child should be TSvgRadialGradientNode');

    gradNode := TSvgRadialGradientNode(defsNode.Children[0]);
    CheckEquals('grad2', gradNode.ID);
    CheckEquals(50.0, gradNode.Cx.Value, 1E-4);
    CheckEquals(50.0, gradNode.Cy.Value, 1E-4);
    CheckEquals(50.0, gradNode.R.Value, 1E-4);
    CheckEquals(30.0, gradNode.Fx.Value, 1E-4);
    CheckEquals(30.0, gradNode.Fy.Value, 1E-4);

    CheckEquals(2, gradNode.Stops.Count);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestGradientInheritance;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  derivedGrad: TSvgLinearGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <linearGradient id="baseGrad" spreadMethod="repeat">' +
         '      <stop offset="0%" stop-color="yellow"/>' +
         '      <stop offset="100%" stop-color="green"/>' +
         '    </linearGradient>' +
         '    <linearGradient id="derivedGrad" href="#baseGrad" x1="10" y1="10" x2="90" y2="90"/>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    docNode.Resolve;
    defsNode := TSvgDefsNode(docNode.Children[0]);
    CheckEquals(2, defsNode.Children.Count);

    derivedGrad := TSvgLinearGradientNode(defsNode.Children[1]);

    CheckEquals('#baseGrad', derivedGrad.Href);
    CheckEquals(2, derivedGrad.Stops.Count, 'Derived gradient should inherit stops from baseGrad');
    CheckEquals(clYellow32, derivedGrad.Stops[0].Color.Color);
    CheckEquals(clGreen32, derivedGrad.Stops[1].Color.Color);
    CheckEquals(Ord(smRepeat), Ord(derivedGrad.SpreadMethod), 'Derived gradient should inherit spreadMethod from baseGrad');
    CheckEquals(10.0, derivedGrad.X1.Value, 1E-4);
    CheckEquals(10.0, derivedGrad.Y1.Value, 1E-4);
    CheckEquals(90.0, derivedGrad.X2.Value, 1E-4);
    CheckEquals(90.0, derivedGrad.Y2.Value, 1E-4);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestGradientInheritanceUserSpaceOnUse;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  derivedGrad: TSvgLinearGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="600" height="440" viewBox="0 0 150 110">' +
         '  <defs>' +
         '    <linearGradient id="baseGrad">' +
         '      <stop offset="0" stop-color="#00ff00"/>' +
         '      <stop offset="1" stop-color="#ff7d00"/>' +
         '    </linearGradient>' +
         '    <linearGradient id="derivedGrad" href="#baseGrad" x1="112.5" y1="111.2" x2="111.6" y2="148.6" gradientUnits="userSpaceOnUse"/>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    docNode.Resolve;
    defsNode := TSvgDefsNode(docNode.Children[0]);
    derivedGrad := TSvgLinearGradientNode(defsNode.Children[1]);

    CheckEquals('#baseGrad', derivedGrad.Href);
    CheckEquals(2, derivedGrad.Stops.Count, 'Derived gradient should inherit stops from baseGrad');
    CheckEquals(Ord(guUserSpaceOnUse), Ord(derivedGrad.GradientUnits), 'GradientUnits should be preserved as userSpaceOnUse');
    CheckEquals(112.5, derivedGrad.X1.Value, 1E-4, 'X1 coordinate should be preserved');
    CheckEquals(111.2, derivedGrad.Y1.Value, 1E-4, 'Y1 coordinate should be preserved');
    CheckEquals(111.6, derivedGrad.X2.Value, 1E-4, 'X2 coordinate should be preserved');
    CheckEquals(148.6, derivedGrad.Y2.Value, 1E-4, 'Y2 coordinate should be preserved');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestConicalGradientParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  gradNode: TSvgConicalGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <conicGradient id="grad3" cx="50%" cy="50%" angle="90deg" start-angle="0rad" end-angle="1turn" spreadMethod="reflect">' +
         '      <stop offset="0" stop-color="red"/>' +
         '      <stop offset="1" stop-color="blue"/>' +
         '    </conicGradient>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(1, docNode.Children.Count);
    defsNode := TSvgDefsNode(docNode.Children[0]);

    CheckEquals(1, defsNode.Children.Count);
    Check(defsNode.Children[0] is TSvgConicalGradientNode, 'Child should be TSvgConicalGradientNode');

    gradNode := TSvgConicalGradientNode(defsNode.Children[0]);
    CheckEquals('grad3', gradNode.ID);
    CheckEquals(50.0, gradNode.Cx.Value, 1E-4);
    CheckEquals(50.0, gradNode.Cy.Value, 1E-4);
    CheckEquals(90.0, gradNode.Angle, 1E-4);
    CheckEquals(0.0, gradNode.StartAngle, 1E-4);
    CheckEquals(360.0, gradNode.EndAngle, 1E-4);
    CheckEquals(Ord(smReflect), Ord(gradNode.SpreadMethod));

    CheckEquals(2, gradNode.Stops.Count);
    CheckEquals(clRed32, gradNode.Stops[0].Color.Color);
    CheckEquals(clBlue32, gradNode.Stops[1].Color.Color);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestConicalGradientInheritance;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  derivedGrad: TSvgConicalGradientNode;
  defsNode: TSvgDefsNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <sweepGradient id="baseConic" angle="45" spreadMethod="repeat">' +
         '      <stop offset="0" stop-color="green"/>' +
         '      <stop offset="1" stop-color="purple"/>' +
         '    </sweepGradient>' +
         '    <conicalGradient id="derivedConic" href="#baseConic" cx="30" cy="30"/>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    docNode.Resolve;
    defsNode := TSvgDefsNode(docNode.Children[0]);
    CheckEquals(2, defsNode.Children.Count);

    derivedGrad := TSvgConicalGradientNode(defsNode.Children[1]);

    CheckEquals('#baseConic', derivedGrad.Href);
    CheckEquals(2, derivedGrad.Stops.Count, 'Derived gradient should inherit stops');
    CheckEquals(clGreen32, derivedGrad.Stops[0].Color.Color);
    CheckEquals(clPurple32, derivedGrad.Stops[1].Color.Color);
    CheckEquals(45.0, derivedGrad.Angle, 1E-4, 'Derived gradient should inherit angle');
    CheckEquals(Ord(smRepeat), Ord(derivedGrad.SpreadMethod));
    CheckEquals(30.0, derivedGrad.Cx.Value, 1E-4);
    CheckEquals(30.0, derivedGrad.Cy.Value, 1E-4);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestClipPathAndMaskParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  defsNode: TSvgDefsNode;
  clipNode: TSvgClipPathNode;
  maskNode: TSvgMaskNode;
begin
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <clipPath id="clip1" clipPathUnits="objectBoundingBox">' +
         '      <rect x="0" y="0" width="50" height="50"/>' +
         '    </clipPath>' +
         '    <mask id="mask1" maskUnits="userSpaceOnUse" maskContentUnits="objectBoundingBox">' +
         '      <circle cx="50" cy="50" r="20"/>' +
         '    </mask>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    defsNode := TSvgDefsNode(docNode.Children[0]);
    CheckEquals(2, defsNode.Children.Count);

    Check(defsNode.Children[0] is TSvgClipPathNode, 'First defs child should be TSvgClipPathNode');
    clipNode := TSvgClipPathNode(defsNode.Children[0]);
    CheckEquals('clip1', clipNode.ID);
    CheckEquals(Ord(guObjectBoundingBox), Ord(clipNode.ClipPathUnits));

    Check(defsNode.Children[1] is TSvgMaskNode, 'Second defs child should be TSvgMaskNode');
    maskNode := TSvgMaskNode(defsNode.Children[1]);
    CheckEquals('mask1', maskNode.ID);
    CheckEquals(Ord(guUserSpaceOnUse), Ord(maskNode.MaskUnits));
    CheckEquals(Ord(guObjectBoundingBox), Ord(maskNode.MaskContentUnits));
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgGradients.TestZeroDimensionsObjectBoundingBoxGradientFallback;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  renderer: TSvgRenderer;
  bmp: TBitmap32;
begin
  // Horizontal line (height=0) and vertical line (width=0) with gradientUnits="objectBoundingBox"
  xml := '<svg width="100" height="100">' +
         '  <defs>' +
         '    <linearGradient id="grad1" gradientUnits="objectBoundingBox">' +
         '      <stop offset="0" stop-color="black"/>' +
         '      <stop offset="1" stop-color="white"/>' +
         '    </linearGradient>' +
         '  </defs>' +
         '  <line x1="10" y1="30" x2="90" y2="30" stroke="url(#grad1) #00ff00" stroke-width="10"/>' +
         '  <line x1="10" y1="60" x2="90" y2="60" stroke="url(#grad1)" stroke-width="10"/>' +
         '  <line x1="10" y1="10" x2="90" y2="90" stroke="url(#grad1)" stroke-width="10"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should parse');
  try
    docNode.Resolve;

    bmp := TBitmap32.Create;
    try
      bmp.SetSize(100, 100);
      bmp.Clear(clWhite32);

      renderer := TSvgRenderer.Create;
      try
        renderer.RenderDocument(bmp, docNode);

        // Line 1 (horizontal, y=30, with fallback #00ff00 green): gradient invalid on 1D line, fallback green should render
        CheckEquals(clLime32, bmp.Pixel[50, 30], 'Line with invalid objectBoundingBox gradient and fallback #00ff00 should render green fallback stroke');

        // Line 2 (horizontal, y=60, without fallback): gradient invalid on 1D line, no fallback -> remains white background
        CheckEquals(clWhite32, bmp.Pixel[50, 60], 'Line with invalid objectBoundingBox gradient and no fallback should not render (white background)');

        // Line 3 (diagonal, x1=10 y1=10 x2=90 y2=90, 2D object bounding box): gradient is valid -> renders painted pixels
        Check(bmp.Pixel[50, 50] <> clWhite32, 'Diagonal line with valid 2D objectBoundingBox gradient should render gradient stroke');
      finally
        renderer.Free;
      end;
    finally
      bmp.Free;
    end;
  finally
    docNode.Free;
  end;
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgGradients.Suite);
{$ELSE}
  RegisterTest(TTestSvgGradients.Suite);
{$ENDIF}

end.
