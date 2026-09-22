unit GR32.Tests.SVG.Tree;

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
  GR32.SVG.Types, GR32.SVG.Tree;

type
  TTestSvgTree = class(TTestCase)
  published
    procedure TestNodeHierarchy;
    procedure TestPrimitiveShapeConverters;
    procedure TestTransformParsing;
    procedure TestXmlParsingContainer;
    procedure TestFillAndStrokeParsing;
    procedure TestNonSelfClosingElements;
    procedure TestCyclicUseProtection;
    procedure TestPatternParsingAndInheritance;
    procedure TestDocTypeParsing;
    procedure TestUseNodeStyleInheritance;
    procedure TestStrokeDashArrayAndOffsetParsing;
  end;

implementation

uses
  Types;

{ TTestSvgTree }

procedure TTestSvgTree.TestNodeHierarchy;
var
  docNode: TSvgDocumentNode;
  groupNode: TSvgGroupNode;
  pathNode: TSvgPathNode;
begin
  docNode := TSvgDocumentNode.Create(nil);
  try
    Check(docNode.Visible, 'Document node should be visible by default');
    CheckEquals(0, docNode.Children.Count);

    groupNode := TSvgGroupNode.Create(docNode);
    groupNode.ID := 'g1';
    docNode.AddChild(groupNode);

    CheckEquals(1, docNode.Children.Count);
    Check(groupNode.Parent = docNode, 'Group node parent should be docNode');

    pathNode := TSvgPathNode.Create(groupNode);
    pathNode.ID := 'p1';
    groupNode.AddChild(pathNode);

    CheckEquals(1, groupNode.Children.Count);
    Check(pathNode.Parent = groupNode, 'Path node parent should be groupNode');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestUseNodeStyleInheritance;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  useNode: TSvgUseNode;
  clonedPath: TSvgNode;
begin
  xml := '<svg width="150" height="100">' +
         '  <g fill="grey">' +
         '    <path id="heart" d="M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 z"/>' +
         '  </g>' +
         '  <use id="heart_use" href="#heart" fill="none" stroke="red"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    useNode := TSvgUseNode(docNode.FindNodeById('heart_use'));
    Check(useNode <> nil, 'heart_use node should exist');
    CheckEquals(1, useNode.Children.Count, 'useNode should contain 1 cloned child');

    clonedPath := useNode.Children[0];
    Check(clonedPath.Fill.Color.IsNone, 'Cloned path should inherit fill="none" from <use>');
    Check(not clonedPath.Stroke.Color.IsNone, 'Cloned path should inherit stroke="red" from <use>');
    CheckEquals(clRed32, clonedPath.Stroke.Color.Color, 'Cloned path stroke should be red');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestDocTypeParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
begin
  xml := '<?xml version="1.0" encoding="UTF-8"?>' +
         '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">' +
         '<svg width="100" height="100">' +
         '  <rect x="0" y="0" width="10" height="10"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode when input contains DOCTYPE');
  try
    CheckEquals(100.0, docNode.Width.Value, 1E-4);
    CheckEquals(100.0, docNode.Height.Value, 1E-4);
    CheckEquals(1, docNode.Children.Count, 'Document should parse root svg children successfully past DOCTYPE');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestPrimitiveShapeConverters;
var
  pts: TArrayOfArrayOfFloatPoint;
begin
  // Rect
  pts := CreateRectPath(10, 20, 100, 50, 0, 0);
  Check(Length(pts) > 0, 'Rect path should produce points');
  Check(Length(pts[0]) >= 4, 'Rect path should have at least 4 vertices');

  // Rounded Rect
  pts := CreateRectPath(10, 20, 100, 50, 5, 5);
  Check(Length(pts) > 0, 'Rounded rect path should produce points');

  // Circle
  pts := CreateCirclePath(50, 50, 25);
  Check(Length(pts) > 0, 'Circle path should produce points');

  // Ellipse
  pts := CreateEllipsePath(50, 50, 30, 20);
  Check(Length(pts) > 0, 'Ellipse path should produce points');

  // Line
  pts := CreateLinePath(0, 0, 100, 100);
  Check(Length(pts) > 0, 'Line path should produce points');
  CheckEquals(2, Length(pts[0]));

  // Polyline
  pts := CreatePolylinePath('10,10 20,20 30,10', False);
  Check(Length(pts) > 0, 'Polyline path should produce points');
  CheckEquals(3, Length(pts[0]));

  // Polygon
  pts := CreatePolylinePath('10,10 20,20 30,10', True);
  Check(Length(pts) > 0, 'Polygon path should produce points');
  CheckEquals(4, Length(pts[0])); // Closing path adds start point
end;

procedure TTestSvgTree.TestTransformParsing;
var
  mat: TFloatMatrix;
  pt, resPt: TFloatPoint;
  helper: TFloatMatrixHelper;
begin
  mat := ParseSvgTransform('translate(10, 20) scale(2, 3)');
  helper.Matrix := mat;

  pt := FloatPoint(5, 5);
  resPt := helper.TransformPoint(pt);

  // SVG transform list 'translate(10, 20) scale(2, 3)' applies scale first, then translate:
  // (5 * 2) + 10 = 20, (5 * 3) + 20 = 35
  CheckEquals(20.0, resPt.X, 1E-4);
  CheckEquals(35.0, resPt.Y, 1E-4);

  // Test complex multi-transform list: 'rotate(-10 50 100) translate(-36 45.5) skewX(40) scale(1 0.5)'
  mat := ParseSvgTransform('rotate(-10 50 100) translate(-36 45.5) skewX(40) scale(1 0.5)');
  helper.Matrix := mat;
  pt := FloatPoint(10, 30);
  resPt := helper.TransformPoint(pt);

  // Evaluate step by step right-to-left:
  // 1. scale(1, 0.5): (10, 30) -> (10, 15)
  // 2. skewX(40): tan(40deg) ≈ 0.8390996, x' = 10 + 15 * 0.8390996 = 22.586494, y' = 15
  // 3. translate(-36, 45.5): x' = 22.586494 - 36 = -13.413506, y' = 15 + 45.5 = 60.5
  // 4. rotate(-10deg, 50, 100):
  //    dx = -13.413506 - 50 = -63.413506, dy = 60.5 - 100 = -39.5
  //    rad = -10deg, cos(-10deg) ≈ 0.98480775, sin(-10deg) ≈ -0.17364818
  //    x'' = -63.413506 * cos - -39.5 * sin + 50 = -62.450125 - 6.859103 + 50 = -19.309228
  //    y'' = -63.413506 * sin + -39.5 * cos + 100 = 11.011642 - 38.899906 + 100 = 72.111736
  CheckEquals(-19.309228, resPt.X, 1E-3);
  CheckEquals(72.111736, resPt.Y, 1E-3);
end;

procedure TTestSvgTree.TestXmlParsingContainer;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  groupNode: TSvgGroupNode;
  rectNode, circleNode: TSvgPathNode;
begin
  xml := '<svg width="200" height="100" viewBox="0 0 200 100">' +
         '  <g id="main_group" opacity="0.75">' +
         '    <rect x="10" y="20" width="50" height="30"/>' +
         '    <circle cx="100" cy="50" r="25"/>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(200.0, docNode.Width.Value, 1E-4);
    CheckEquals(100.0, docNode.Height.Value, 1E-4);
    Check(docNode.ViewBox.IsDefined, 'ViewBox should be defined');
    CheckEquals(200.0, docNode.ViewBox.Width, 1E-4);

    CheckEquals(1, docNode.Children.Count);
    Check(docNode.Children[0] is TSvgGroupNode, 'Child should be TSvgGroupNode');

    groupNode := TSvgGroupNode(docNode.Children[0]);
    CheckEquals('main_group', groupNode.ID);
    CheckEquals(0.75, groupNode.Opacity, 1E-4);
    CheckEquals(2, groupNode.Children.Count);

    Check(groupNode.Children[0] is TSvgPathNode, 'First child should be TSvgPathNode');
    rectNode := TSvgPathNode(groupNode.Children[0]);
    Check(Length(rectNode.PathData) > 0, 'Rect node should have generated PathData');

    Check(groupNode.Children[1] is TSvgPathNode, 'Second child should be TSvgPathNode');
    circleNode := TSvgPathNode(groupNode.Children[1]);
    Check(Length(circleNode.PathData) > 0, 'Circle node should have generated PathData');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestFillAndStrokeParsing;
var
  node: TSvgPathNode;
begin
  node := TSvgPathNode.Create(nil);
  try
    node.ParseAttribute('fill', '#FF0000');
    node.ParseAttribute('fill-opacity', '0.5');
    node.ParseAttribute('fill-rule', 'evenodd');
    node.ParseAttribute('stroke', 'blue');
    node.ParseAttribute('stroke-width', '2.5px');
    node.ParseAttribute('stroke-linecap', 'round');

    CheckEquals(clRed32, node.Fill.Color.Color);
    CheckEquals(0.5, node.Fill.Opacity, 1E-4);
    CheckEquals(Ord(pfAlternate), Ord(node.Fill.FillRule));

    CheckEquals(clBlue32, node.Stroke.Color.Color);
    CheckEquals(2.5, node.Stroke.Width.Value, 1E-4);
    CheckEquals(Ord(esRound), Ord(node.Stroke.EndStyle));
  finally
    node.Free;
  end;
end;

procedure TTestSvgTree.TestNonSelfClosingElements;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  groupNode: TSvgGroupNode;
begin
  xml := '<svg width="200" height="100">' +
         '  <g id="test_group">' +
         '    <path d="M0 0 L10 10"></path>' +
         '    <rect x="0" y="0" width="10" height="10"/>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(1, docNode.Children.Count, 'Document should have 1 child group');
    Check(docNode.Children[0] is TSvgGroupNode, 'Child should be TSvgGroupNode');

    groupNode := TSvgGroupNode(docNode.Children[0]);
    CheckEquals(2, groupNode.Children.Count, 'Group should contain both path and rect children');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestPatternParsingAndInheritance;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  p1, p2: TSvgPatternNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <pattern id="basePat" width="20" height="20" patternUnits="userSpaceOnUse">' +
         '      <rect width="10" height="10" fill="red"/>' +
         '    </pattern>' +
         '    <pattern id="derivedPat" href="#basePat" x="5" y="5"/>' +
         '  </defs>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    p1 := TSvgPatternNode(docNode.FindNodeById('basePat'));
    p2 := TSvgPatternNode(docNode.FindNodeById('derivedPat'));

    Check(p1 <> nil, 'basePat should exist');
    Check(p2 <> nil, 'derivedPat should exist');

    CheckEquals(20.0, p1.Width.Value, 1E-4);
    CheckEquals(20.0, p1.Height.Value, 1E-4);
    CheckEquals(1, p1.Children.Count, 'basePat should contain 1 child node');

    // Test pattern inheritance via href
    CheckEquals(20.0, p2.Width.Value, 1E-4, 'derivedPat should inherit Width');
    CheckEquals(20.0, p2.Height.Value, 1E-4, 'derivedPat should inherit Height');
    CheckEquals(5.0, p2.X.Value, 1E-4);
    CheckEquals(5.0, p2.Y.Value, 1E-4);
    CheckEquals(1, p2.Children.Count, 'derivedPat should inherit children from basePat');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestCyclicUseProtection;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  u1, u2: TSvgUseNode;
begin
  // 1. Test direct self-referencing <use> element (<use id="u1" href="#u1"/>)
  xml := '<svg width="100" height="100">' +
         '  <use id="u1" href="#u1"/>' +
         '</svg>';
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should process direct self-referencing <use> without throwing stack overflow');
  try
    CheckEquals(1, docNode.Children.Count);
    Check(docNode.Children[0] is TSvgUseNode);
    // The self-referencing use node should not clone itself continuously
    CheckEquals(0, TSvgUseNode(docNode.Children[0]).Children.Count, 'Self-referencing <use> should not expand children');
  finally
    docNode.Free;
  end;

  // 2. Test mutual circular reference between groups (<g id="A"><use href="#B"/></g>, <g id="B"><use href="#A"/></g>)
  xml := '<svg width="100" height="100">' +
         '  <g id="groupA">' +
         '    <use href="#groupB"/>' +
         '  </g>' +
         '  <g id="groupB">' +
         '    <use href="#groupA"/>' +
         '  </g>' +
         '</svg>';
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should process mutual group circular references safely');
  try
    CheckEquals(2, docNode.Children.Count);
  finally
    docNode.Free;
  end;

  // 3. Test multi-node chain cycle (<use id="u1" href="#u2"/>, <use id="u2" href="#u3"/>, <use id="u3" href="#u1"/>)
  xml := '<svg width="100" height="100">' +
         '  <use id="u1" href="#u2"/>' +
         '  <use id="u2" href="#u3"/>' +
         '  <use id="u3" href="#u1"/>' +
         '</svg>';
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should process multi-node chain cycles safely');
  try
    CheckEquals(3, docNode.Children.Count);
  finally
    docNode.Free;
  end;

  // 4. Verify valid non-cyclic <use> elements expand correctly
  xml := '<svg width="100" height="100">' +
         '  <g id="box">' +
         '    <rect x="0" y="0" width="10" height="10"/>' +
         '  </g>' +
         '  <use id="u1" href="#box" x="10"/>' +
         '  <use id="u2" href="#box" x="20"/>' +
         '</svg>';
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should process valid non-cyclic <use> elements');
  try
    CheckEquals(3, docNode.Children.Count);
    u1 := TSvgUseNode(docNode.FindNodeById('u1'));
    u2 := TSvgUseNode(docNode.FindNodeById('u2'));
    Check(u1 <> nil, 'u1 should exist');
    Check(u2 <> nil, 'u2 should exist');
    CheckEquals(1, u1.Children.Count, 'u1 should expand cloned <g id="box"> child');
    CheckEquals(1, u2.Children.Count, 'u2 should expand cloned <g id="box"> child');
  finally
    docNode.Free;
  end;

  // 5. Test cyclic group pair referenced from inside an independent container group
  xml := '<svg width="100" height="100">' +
         '  <g id="main">' +
         '    <use href="#groupA"/>' +
         '  </g>' +
         '  <g id="groupA">' +
         '    <use href="#groupB"/>' +
         '  </g>' +
         '  <g id="groupB">' +
         '    <use href="#groupA"/>' +
         '  </g>' +
         '</svg>';
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should process cyclic groups referenced from an outer container group safely');
  try
    CheckEquals(3, docNode.Children.Count);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestStrokeDashArrayAndOffsetParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  p1, p2, p3: TSvgNode;
begin
  xml := '<svg width="600" height="240">' +
         '  <path id="p1" stroke="blue" stroke-dasharray="0.15, 0.025" stroke-dashoffset="0.01"/>' +
         '  <path id="p2" stroke="red" stroke-dasharray="1 2 3" stroke-dashoffset="5px"/>' +
         '  <path id="p3" stroke="green" style="stroke-dasharray: none; stroke-dashoffset: 0;"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    p1 := docNode.FindNodeById('p1');
    Check(p1 <> nil, 'p1 should exist');
    CheckEquals(2, Length(p1.Stroke.DashArray), 'p1 dash array length should be 2');
    CheckEquals(0.15, p1.Stroke.DashArray[0], 1E-4);
    CheckEquals(0.025, p1.Stroke.DashArray[1], 1E-4);
    CheckEquals(0.01, p1.Stroke.DashOffset, 1E-4);

    p2 := docNode.FindNodeById('p2');
    Check(p2 <> nil, 'p2 should exist');
    // Odd number of values (3) in stroke-dasharray should double array length to 6 per SVG spec
    CheckEquals(6, Length(p2.Stroke.DashArray), 'p2 dash array should double length from 3 to 6 for odd-count input');
    CheckEquals(1.0, p2.Stroke.DashArray[0], 1E-4);
    CheckEquals(2.0, p2.Stroke.DashArray[1], 1E-4);
    CheckEquals(3.0, p2.Stroke.DashArray[2], 1E-4);
    CheckEquals(1.0, p2.Stroke.DashArray[3], 1E-4);
    CheckEquals(2.0, p2.Stroke.DashArray[4], 1E-4);
    CheckEquals(3.0, p2.Stroke.DashArray[5], 1E-4);
    CheckEquals(5.0, p2.Stroke.DashOffset, 1E-4);

    p3 := docNode.FindNodeById('p3');
    Check(p3 <> nil, 'p3 should exist');
    CheckEquals(0, Length(p3.Stroke.DashArray), 'p3 dash array should be empty for "none"');
    CheckEquals(0.0, p3.Stroke.DashOffset, 1E-4);
  finally
    docNode.Free;
  end;
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgTree.Suite);
{$ELSE}
  RegisterTest(TTestSvgTree.Suite);
{$ENDIF}

end.
