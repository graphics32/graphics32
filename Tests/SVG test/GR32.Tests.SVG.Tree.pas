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
  CheckEquals(3, Length(pts[0]));
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

  // Translate then scale: (5*2 + 10, 5*3 + 20) = (20, 35)
  CheckEquals(20.0, resPt.X, 1E-4);
  CheckEquals(35.0, resPt.Y, 1E-4);
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

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgTree.Suite);
{$ELSE}
  RegisterTest(TTestSvgTree.Suite);
{$ENDIF}

end.
