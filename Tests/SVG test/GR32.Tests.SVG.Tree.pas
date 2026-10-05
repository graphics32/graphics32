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
    procedure TestMarkerParsingAndTreeStructure;
    procedure TestGetObjectBoundingBox;
    procedure TestVisibilityVsDisplayBoundingBox;
    procedure TestResolvedReferencesInTree;
    procedure TestMixBlendModeAndIsolationParsing;
    procedure TestSymbolParsingAndUseResolution;
    procedure TestFilterASTAndReferenceResolution;
    procedure TestFeDropShadowParsingAndResolution;
    procedure TestPrimitiveShapePercentageUnits;
    procedure TestTextAndTSpanParsing;
    procedure TestTextRotationParsing;
    procedure TestTextPathParsingAndResolution;
    procedure TestImageNodeParsingAndAttributes;
    procedure TestSwitchNodeAndConditionalProcessing;
    procedure TestContainerFontInheritance;
    procedure TestDefSingularTagParsing;
    procedure TestFontShorthandParsing;
    procedure TestNestedSvgIdResolutionAndCurrentColor;
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

procedure TTestSvgTree.TestDefSingularTagParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  defsNode: TSvgNode;
  useNode1, useNode2: TSvgUseNode;
begin
  xml := '<svg viewBox="0 0 32 16">' +
         '  <def>' +
         '    <circle id="circ" cx="8" cy="8" r="5" fill="none"/>' +
         '  </def>' +
         '  <use id="u1" href="#circ" stroke="green" stroke-width="0.25"/>' +
         '  <use id="u2" href="#circ" transform="translate(16)" stroke="green" stroke-width="1.0"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    defsNode := docNode.FindNodeById('circ');
    Check(defsNode <> nil, 'Circle element inside singular <def> tag should be registered in ID table');

    useNode1 := TSvgUseNode(docNode.FindNodeById('u1'));
    Check(useNode1 <> nil, 'u1 should exist');
    CheckEquals(1, useNode1.Children.Count, 'u1 should expand child referenced from <def>');

    useNode2 := TSvgUseNode(docNode.FindNodeById('u2'));
    Check(useNode2 <> nil, 'u2 should exist');
    CheckEquals(1, useNode2.Children.Count, 'u2 should expand child referenced from <def>');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestFontShorthandParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  text1, text2, text3: TSvgTextNode;
begin
  xml := '<svg width="200" height="100">' +
         '  <text id="t1" x="10" y="20" font="italic bold 16px/1.2 &quot;Courier New&quot;, monospace">Inline Font</text>' +
         '  <text id="t2" x="10" y="40" style="font: small-caps 700 24px Arial;">Style Font</text>' +
         '  <text id="t3" x="10" y="60" font="small Times New Roman">Keyword Size</text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    text1 := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(text1 <> nil, 'textNode t1 should exist');
    CheckEquals('italic', text1.FontStyle);
    CheckEquals('bold', text1.FontWeight);
    CheckEquals(16.0, text1.FontSize.Value, 1E-4);
    CheckEquals('"Courier New", monospace', text1.FontFamily);

    text2 := TSvgTextNode(docNode.FindNodeById('t2'));
    Check(text2 <> nil, 'textNode t2 should exist');
    CheckEquals('700', text2.FontWeight);
    CheckEquals(24.0, text2.FontSize.Value, 1E-4);

    text3 := TSvgTextNode(docNode.FindNodeById('t3'));
    Check(text3 <> nil, 'textNode t3 should exist');
    CheckEquals(13.0, text3.FontSize.Value, 1E-4);
    CheckEquals('Times New Roman', text3.FontFamily);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestFeDropShadowParsingAndResolution;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  filterNode1, filterNode2: TSvgFilterNode;
  dsNodeDefault, dsNodeExplicit: TSvgFeDropShadowNode;
  rectNode: TSvgNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <filter id="f_default">' +
         '      <feDropShadow/>' +
         '    </filter>' +
         '    <filter id="f_explicit">' +
         '      <feDropShadow dx="3" dy="5" stdDeviation="4.5" flood-color="red" flood-opacity="0.6" result="ds_res"/>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect id="r1" x="10" y="10" width="50" height="50" filter="url(#f_explicit)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    filterNode1 := TSvgFilterNode(docNode.FindNodeById('f_default'));
    Check(filterNode1 <> nil, 'filterNode f_default should exist');
    CheckEquals(1, filterNode1.Children.Count, 'Filter should contain 1 child node');
    Check(filterNode1.Children[0] is TSvgFeDropShadowNode, 'Child should be TSvgFeDropShadowNode');

    dsNodeDefault := TSvgFeDropShadowNode(filterNode1.Children[0]);
    CheckEquals(2.0, dsNodeDefault.Dx, 1E-4, 'Default dx should be 2');
    CheckEquals(2.0, dsNodeDefault.Dy, 1E-4, 'Default dy should be 2');
    CheckEquals(2.0, dsNodeDefault.StdDeviationX, 1E-4, 'Default stdDeviationX should be 2');
    CheckEquals(2.0, dsNodeDefault.StdDeviationY, 1E-4, 'Default stdDeviationY should be 2');
    CheckEquals(clBlack32, dsNodeDefault.FloodColor.Color, 'Default flood-color should be black');
    CheckEquals(1.0, dsNodeDefault.FloodOpacity, 1E-4, 'Default flood-opacity should be 1');

    filterNode2 := TSvgFilterNode(docNode.FindNodeById('f_explicit'));
    Check(filterNode2 <> nil, 'filterNode f_explicit should exist');
    Check(filterNode2.Children[0] is TSvgFeDropShadowNode, 'Child should be TSvgFeDropShadowNode');

    dsNodeExplicit := TSvgFeDropShadowNode(filterNode2.Children[0]);
    CheckEquals(3.0, dsNodeExplicit.Dx, 1E-4, 'Explicit dx should be 3');
    CheckEquals(5.0, dsNodeExplicit.Dy, 1E-4, 'Explicit dy should be 5');
    CheckEquals(4.5, dsNodeExplicit.StdDeviationX, 1E-4, 'Explicit stdDeviationX should be 4.5');
    CheckEquals(4.5, dsNodeExplicit.StdDeviationY, 1E-4, 'Explicit stdDeviationY should be 4.5');
    CheckEquals(clRed32, dsNodeExplicit.FloodColor.Color, 'Explicit flood-color should be red');
    CheckEquals(0.6, dsNodeExplicit.FloodOpacity, 1E-4, 'Explicit flood-opacity should be 0.6');
    CheckEquals('ds_res', dsNodeExplicit.ResultName, 'Explicit result should be ds_res');

    rectNode := docNode.FindNodeById('r1');
    Check(rectNode <> nil, 'rectNode r1 should exist');
    Check(rectNode.ResolvedFilter <> nil, 'ResolvedFilter on rectNode should be resolved');
    CheckEquals('f_explicit', rectNode.ResolvedFilter.ID);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestTextRotationParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  textNode: TSvgTextNode;
  tspanNode: TSvgTSpanNode;
begin
  xml := '<svg width="200" height="100">' +
         '  <text id="t1" x="10" y="20" rotate="-45">' +
         '    Hello' +
         '    <tspan id="ts1" rotate="-10 -20 30">World</tspan>' +
         '  </text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    textNode := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(textNode <> nil, 'textNode t1 should exist');
    CheckEquals(1, Length(textNode.Rotate));
    CheckEquals(-45.0, textNode.Rotate[0], 1E-4);

    tspanNode := TSvgTSpanNode(docNode.FindNodeById('ts1'));
    Check(tspanNode <> nil, 'tspanNode ts1 should exist');
    CheckEquals(3, Length(tspanNode.Rotate));
    CheckEquals(-10.0, tspanNode.Rotate[0], 1E-4);
    CheckEquals(-20.0, tspanNode.Rotate[1], 1E-4);
    CheckEquals(30.0, tspanNode.Rotate[2], 1E-4);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestTextPathParsingAndResolution;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  textPathNode: TSvgTextPathNode;
begin
  xml := '<svg width="200" height="100">' +
         '  <defs>' +
         '    <path id="path1" d="M 10 50 Q 50 10 90 50"/>' +
         '  </defs>' +
         '  <text>' +
         '    <textPath id="tp1" href="#path1" startOffset="10px">Text on path</textPath>' +
         '  </text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    textPathNode := TSvgTextPathNode(docNode.FindNodeById('tp1'));
    Check(textPathNode <> nil, 'textPathNode tp1 should exist');
    CheckEquals('#path1', textPathNode.Href);
    CheckEquals(10.0, textPathNode.StartOffset.Value, 1E-4);
    CheckEquals('Text on path', textPathNode.TextContent);
    Check(textPathNode.ResolvedPathNode <> nil, 'ResolvedPathNode should be resolved');
    CheckEquals('path1', textPathNode.ResolvedPathNode.ID);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestContainerFontInheritance;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  textNode: TSvgTextNode;
begin
  xml := '<svg width="200" height="100" font-family="Noto Sans" font-size="64px">' +
         '  <text id="t1" x="10" y="20">Inherited Font</text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    textNode := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(textNode <> nil, 'textNode t1 should exist');
    CheckEquals('Noto Sans', textNode.FontFamily, 'Text node should inherit font-family from root <svg>');
    CheckEquals(64.0, textNode.FontSize.Value, 1E-4, 'Text node should inherit font-size from root <svg>');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestTextAndTSpanParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  textNode: TSvgTextNode;
  tspanNode: TSvgTSpanNode;
begin
  xml := '<svg width="200" height="100">' +
         '  <text id="t1" x="10" y="20" dx="2" dy="4" font-family="Arial" font-size="16px" font-weight="bold" font-style="italic" text-anchor="middle" fill="black">' +
         '    Hello' +
         '    <tspan id="ts1" dx="5" fill="red" text-anchor="end">World</tspan>' +
         '  </text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    textNode := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(textNode <> nil, 'textNode t1 should exist');
    CheckEquals(10.0, textNode.X.Value, 1E-4);
    CheckEquals(20.0, textNode.Y.Value, 1E-4);
    CheckEquals(2.0, textNode.Dx.Value, 1E-4);
    CheckEquals(4.0, textNode.Dy.Value, 1E-4);
    CheckEquals('Arial', textNode.FontFamily);
    CheckEquals(16.0, textNode.FontSize.Value, 1E-4);
    CheckEquals('bold', textNode.FontWeight);
    CheckEquals('italic', textNode.FontStyle);
    CheckEquals(Ord(taMiddle), Ord(textNode.TextAnchor));
    CheckEquals('Hello', textNode.TextContent);
    CheckEquals(1, textNode.Children.Count, 'textNode should contain 1 tspan child');

    tspanNode := TSvgTSpanNode(textNode.Children[0]);
    Check(tspanNode <> nil, 'tspanNode ts1 should exist');
    CheckEquals('ts1', tspanNode.ID);
    CheckEquals(5.0, tspanNode.Dx.Value, 1E-4);
    CheckEquals('World', tspanNode.TextContent);
    CheckEquals(clRed32, tspanNode.Fill.Color.Color);
    CheckEquals(Ord(taEnd), Ord(tspanNode.TextAnchor));
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestSwitchNodeAndConditionalProcessing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  switchNode: TSvgSwitchNode;
begin
  // 1. Feature keyword dictionary tests
  Check(IsSupportedSvgFeature('http://www.w3.org/TR/SVG11/feature#SVG'), 'Standard SVG feature URI should be supported');
  Check(IsSupportedSvgFeature('http://www.w3.org/TR/SVG11/feature#Shape'), 'Shape feature URI should be supported');
  Check(IsSupportedSvgFeature('org.w3c.svg.static'), 'Static feature URI should be supported');
  Check(not IsSupportedSvgFeature('http://www.w3.org/TR/SVG11/feature#Animation'), 'Animation feature URI should evaluate to unsupported');
  Check(not IsSupportedSvgFeature('http://invalid.feature/uri'), 'Unknown feature URI should evaluate to unsupported');

  // 2. Language matching tests
  Check(MatchLanguageTag('en-US', 'en'), 'Language range "en" should match system tag "en-US"');
  Check(MatchLanguageTag('en-US', 'en-US'), 'Language range "en-US" should match system tag "en-US"');
  Check(not MatchLanguageTag('en-US', 'fr'), 'Language range "fr" should not match system tag "en-US"');

  // 3. Switch node AST parsing and selection test
  xml := '<svg width="200" height="200">' +
         '  <switch id="sw1">' +
         '    <rect id="r_lang_fr" x="0" y="0" width="10" height="10" fill="blue" systemLanguage="fr"/>' +
         '    <rect id="r_feat_invalid" x="0" y="0" width="10" height="10" fill="yellow" requiredFeatures="http://invalid.feature"/>' +
         '    <rect id="r_lang_en" x="0" y="0" width="10" height="10" fill="green" systemLanguage="en"/>' +
         '    <rect id="r_default" x="0" y="0" width="10" height="10" fill="red"/>' +
         '  </switch>' +
         '</svg>';

  // System language is default 'en' -> Stage 2 normalization selects r_lang_en while retaining all children in FChildren
  SetSystemLanguage('en');
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    switchNode := TSvgSwitchNode(docNode.FindNodeById('sw1'));
    Check(switchNode <> nil, 'switchNode sw1 should exist');
    Check(switchNode is TSvgSwitchNode, 'sw1 should be a TSvgSwitchNode instance');
    CheckEquals(4, switchNode.Children.Count, 'All children should be retained in FChildren');
    Check(switchNode.SelectedChild <> nil, 'SelectedChild should be assigned');
    CheckEquals('r_lang_en', switchNode.SelectedChild.ID, 'Selected child pointer should be r_lang_en');
  finally
    docNode.Free;
  end;

  // System language is 'fr' -> Stage 2 normalization selects r_lang_fr while retaining all children in FChildren
  SetSystemLanguage('fr');
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    switchNode := TSvgSwitchNode(docNode.FindNodeById('sw1'));
    Check(switchNode <> nil, 'switchNode sw1 should exist');
    CheckEquals(4, switchNode.Children.Count, 'All children should be retained in FChildren');
    Check(switchNode.SelectedChild <> nil, 'SelectedChild should be assigned');
    CheckEquals('r_lang_fr', switchNode.SelectedChild.ID, 'Selected child pointer should be r_lang_fr');
  finally
    docNode.Free;
  end;

  // System language is 'de' -> Stage 2 normalization falls back to r_default while retaining all children in FChildren
  SetSystemLanguage('de');
  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    switchNode := TSvgSwitchNode(docNode.FindNodeById('sw1'));
    Check(switchNode <> nil, 'switchNode sw1 should exist');
    CheckEquals(4, switchNode.Children.Count, 'All children should be retained in FChildren');
    Check(switchNode.SelectedChild <> nil, 'SelectedChild should be assigned');
    CheckEquals('r_default', switchNode.SelectedChild.ID, 'Selected child pointer should be r_default');
  finally
    SetSystemLanguage('en');
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestFilterASTAndReferenceResolution;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  filterNode: TSvgFilterNode;
  blurNode: TSvgFeGaussianBlurNode;
  cmNode: TSvgFeColorMatrixNode;
  offsetNode: TSvgFeOffsetNode;
  floodNode: TSvgFeFloodNode;
  rectNode: TSvgNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <filter id="f1" x="-20%" y="-20%" width="140%" height="140%">' +
         '      <feGaussianBlur in="SourceGraphic" stdDeviation="3.5" result="blurRes"/>' +
         '      <feColorMatrix in="blurRes" type="saturate" values="2" result="cmRes"/>' +
         '      <feOffset in="cmRes" dx="5" dy="10" result="offRes"/>' +
         '      <feFlood flood-color="red" flood-opacity="0.8" result="floodRes"/>' +
         '      <feMerge>' +
         '        <feMergeNode in="offRes"/>' +
         '        <feMergeNode in="SourceGraphic"/>' +
         '      </feMerge>' +
         '    </filter>' +
         '  </defs>' +
         '  <rect id="r1" x="10" y="10" width="80" height="80" filter="url(#f1)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    filterNode := TSvgFilterNode(docNode.FindNodeById('f1'));
    Check(filterNode <> nil, 'filterNode "f1" should exist');
    Check(not filterNode.IsRenderable, 'Filter container node should not be renderable directly');
    CheckEquals(5, filterNode.Children.Count, 'Filter should contain 5 primitive child nodes');

    blurNode := TSvgFeGaussianBlurNode(filterNode.Children[0]);
    CheckEquals('SourceGraphic', blurNode.In1);
    CheckEquals(3.5, blurNode.StdDeviationX, 1E-4);
    CheckEquals(3.5, blurNode.StdDeviationY, 1E-4);
    CheckEquals('blurRes', blurNode.ResultName);

    cmNode := TSvgFeColorMatrixNode(filterNode.Children[1]);
    CheckEquals(Ord(cmSaturate), Ord(cmNode.MatrixType));
    CheckEquals(1, Length(cmNode.Values));
    CheckEquals(2.0, cmNode.Values[0], 1E-4);

    offsetNode := TSvgFeOffsetNode(filterNode.Children[2]);
    CheckEquals(5.0, offsetNode.Dx, 1E-4);
    CheckEquals(10.0, offsetNode.Dy, 1E-4);

    floodNode := TSvgFeFloodNode(filterNode.Children[3]);
    CheckEquals(clRed32, floodNode.FloodColor.Color);
    CheckEquals(0.8, floodNode.FloodOpacity, 1E-4);

    rectNode := docNode.FindNodeById('r1');
    Check(rectNode <> nil, 'rectNode "r1" should exist');
    CheckEquals('url(#f1)', rectNode.FilterID);
    Check(rectNode.ResolvedFilter <> nil, 'ResolvedFilter on rectNode should not be nil');
    CheckEquals('f1', rectNode.ResolvedFilter.ID);
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

procedure TTestSvgTree.TestMarkerParsingAndTreeStructure;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  markerNode: TSvgMarkerNode;
  path1, path2: TSvgNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <marker id="arrow" refX="2" refY="3" markerWidth="6" markerHeight="6" orient="auto" markerUnits="strokeWidth">' +
         '      <path d="M 0 0 L 10 5 L 0 10 z" fill="red"/>' +
         '    </marker>' +
         '    <marker id="dot" refX="5" refY="5" markerWidth="10" markerHeight="10" orient="90" markerUnits="userSpaceOnUse">' +
         '      <circle cx="5" cy="5" r="5" fill="blue"/>' +
         '    </marker>' +
         '  </defs>' +
         '  <path id="p1" d="M 10 10 L 50 10 L 50 50" marker-start="url(#arrow)" marker-mid="url(#dot)" marker-end="url(#arrow)"/>' +
         '  <path id="p2" d="M 0 0 L 100 100" marker="url(#arrow)"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    markerNode := TSvgMarkerNode(docNode.FindNodeById('arrow'));
    Check(markerNode <> nil, 'markerNode "arrow" should exist');
    Check(not markerNode.IsRenderable, 'Marker node should not be renderable directly');
    CheckEquals(2.0, markerNode.RefX.Value, 1E-4);
    CheckEquals(3.0, markerNode.RefY.Value, 1E-4);
    CheckEquals(6.0, markerNode.MarkerWidth.Value, 1E-4);
    CheckEquals(6.0, markerNode.MarkerHeight.Value, 1E-4);
    CheckEquals(Ord(moAuto), Ord(markerNode.Orient));
    CheckEquals(Ord(muStrokeWidth), Ord(markerNode.MarkerUnits));
    CheckEquals(1, markerNode.Children.Count, 'arrow marker should have 1 child');

    markerNode := TSvgMarkerNode(docNode.FindNodeById('dot'));
    Check(markerNode <> nil, 'markerNode "dot" should exist');
    CheckEquals(5.0, markerNode.RefX.Value, 1E-4);
    CheckEquals(5.0, markerNode.RefY.Value, 1E-4);
    CheckEquals(Ord(moAngle), Ord(markerNode.Orient));
    CheckEquals(90.0, markerNode.OrientAngle, 1E-4);
    CheckEquals(Ord(muUserSpaceOnUse), Ord(markerNode.MarkerUnits));

    path1 := docNode.FindNodeById('p1');
    Check(path1 <> nil, 'path p1 should exist');
    CheckEquals('url(#arrow)', path1.MarkerStart);
    CheckEquals('url(#dot)', path1.MarkerMid);
    CheckEquals('url(#arrow)', path1.MarkerEnd);
    Check(path1.ResolvedMarkerStart <> nil, 'ResolvedMarkerStart on p1 should not be nil');
    Check(path1.ResolvedMarkerMid <> nil, 'ResolvedMarkerMid on p1 should not be nil');
    Check(path1.ResolvedMarkerEnd <> nil, 'ResolvedMarkerEnd on p1 should not be nil');
    CheckEquals('arrow', path1.ResolvedMarkerStart.ID);
    CheckEquals('dot', path1.ResolvedMarkerMid.ID);

    path2 := docNode.FindNodeById('p2');
    Check(path2 <> nil, 'path p2 should exist');
    CheckEquals('url(#arrow)', path2.MarkerStart);
    CheckEquals('url(#arrow)', path2.MarkerMid);
    CheckEquals('url(#arrow)', path2.MarkerEnd);
    Check(path2.ResolvedMarkerStart <> nil, 'ResolvedMarkerStart on p2 should not be nil');
    Check(path2.ResolvedMarkerMid <> nil, 'ResolvedMarkerMid on p2 should not be nil');
    Check(path2.ResolvedMarkerEnd <> nil, 'ResolvedMarkerEnd on p2 should not be nil');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestResolvedReferencesInTree;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  groupNode: TSvgGroupNode;
  rectNode: TSvgPathNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <linearGradient id="g1"><stop offset="0" stop-color="red"/></linearGradient>' +
         '    <clipPath id="c1"><rect width="10" height="10"/></clipPath>' +
         '    <mask id="m1"><rect width="10" height="10" fill="white"/></mask>' +
         '  </defs>' +
         '  <g id="g_test" clip-path="url(#c1)" mask="url(#m1)">' +
         '    <rect id="r_test" width="50" height="50" fill="url(#g1)" stroke="url(#g1)"/>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    groupNode := TSvgGroupNode(docNode.FindNodeById('g_test'));
    Check(groupNode <> nil, 'g_test should exist');
    Check(groupNode.ResolvedClipPath <> nil, 'ResolvedClipPath on groupNode should not be nil');
    CheckEquals('c1', groupNode.ResolvedClipPath.ID);
    Check(groupNode.ResolvedMask <> nil, 'ResolvedMask on groupNode should not be nil');
    CheckEquals('m1', groupNode.ResolvedMask.ID);

    rectNode := TSvgPathNode(docNode.FindNodeById('r_test'));
    Check(rectNode <> nil, 'r_test should exist');
    Check(rectNode.Fill.ResolvedPaintServer <> nil, 'ResolvedPaintServer on fill should not be nil');
    Check(rectNode.Stroke.ResolvedPaintServer <> nil, 'ResolvedPaintServer on stroke should not be nil');
    CheckEquals('g1', TSvgNode(rectNode.Fill.ResolvedPaintServer).ID);
    CheckEquals('g1', TSvgNode(rectNode.Stroke.ResolvedPaintServer).ID);
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

procedure TTestSvgTree.TestGetObjectBoundingBox;
var
  xmlStr: string;
  docNode: TSvgDocumentNode;
  rectNode, circleNode, groupNode: TSvgNode;
  box: TFloatRect;
begin
  xmlStr :=
    '<svg width="500" height="500">' +
    '  <rect id="r1" x="10" y="20" width="100" height="50"/>' +
    '  <circle id="c1" cx="200" cy="200" r="30"/>' +
    '  <g id="g1" transform="translate(50, 50)">' +
    '    <rect id="r2" x="0" y="0" width="40" height="30"/>' +
    '  </g>' +
    '</svg>';

  docNode := ParseSvgXml(UTF8String(xmlStr));
  CheckNotNull(docNode, 'Document node should be parsed');
  try
    rectNode := docNode.FindNodeById('r1');
    CheckNotNull(rectNode, 'r1 rect node should exist');
    box := rectNode.GetObjectBoundingBox;
    CheckEquals(10.0, box.Left, 1E-4, 'r1 Left');
    CheckEquals(20.0, box.Top, 1E-4, 'r1 Top');
    CheckEquals(110.0, box.Right, 1E-4, 'r1 Right');
    CheckEquals(70.0, box.Bottom, 1E-4, 'r1 Bottom');

    circleNode := docNode.FindNodeById('c1');
    CheckNotNull(circleNode, 'c1 circle node should exist');
    box := circleNode.GetObjectBoundingBox;
    CheckEquals(170.0, box.Left, 1E-4, 'c1 Left');
    CheckEquals(170.0, box.Top, 1E-4, 'c1 Top');
    CheckEquals(230.0, box.Right, 1E-4, 'c1 Right');
    CheckEquals(230.0, box.Bottom, 1E-4, 'c1 Bottom');

    groupNode := docNode.FindNodeById('g1');
    CheckNotNull(groupNode, 'g1 group node should exist');
    box := groupNode.GetObjectBoundingBox;
    // g1's local bounding box before g1's own transform is applied
    CheckEquals(0.0, box.Left, 1E-4, 'g1 Left');
    CheckEquals(0.0, box.Top, 1E-4, 'g1 Top');
    CheckEquals(40.0, box.Right, 1E-4, 'g1 Right');
    CheckEquals(30.0, box.Bottom, 1E-4, 'g1 Bottom');

    // docNode unites r1(10,20,110,70), c1(170,170,230,230), and g1 transformed(50,50,90,80)
    box := docNode.GetObjectBoundingBox;
    CheckEquals(10.0, box.Left, 1E-4, 'docNode Left');
    CheckEquals(20.0, box.Top, 1E-4, 'docNode Top');
    CheckEquals(230.0, box.Right, 1E-4, 'docNode Right');
    CheckEquals(230.0, box.Bottom, 1E-4, 'docNode Bottom');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestVisibilityVsDisplayBoundingBox;
var
  xmlStr: UTF8String;
  docNode: TSvgDocumentNode;
  gHidden, gDisplayNone: TSvgNode;
  boxHidden, boxDisplayNone: TFloatRect;
begin
  xmlStr :=
    '<svg width="200" height="200">' +
    '  <g id="gHidden">' +
    '    <rect id="rHidden" x="20" y="20" width="160" height="160" visibility="hidden"/>' +
    '    <rect id="rVisible" x="40" y="40" width="120" height="120"/>' +
    '  </g>' +
    '  <g id="gDisplayNone">' +
    '    <rect id="rDisplayNone" x="20" y="20" width="160" height="160" display="none"/>' +
    '    <rect id="rVisible2" x="40" y="40" width="120" height="120"/>' +
    '  </g>' +
    '</svg>';

  docNode := ParseSvgXml(xmlStr);
  CheckNotNull(docNode, 'Document node should be parsed');
  try
    gHidden := docNode.FindNodeById('gHidden');
    CheckNotNull(gHidden, 'gHidden node should exist');
    boxHidden := gHidden.GetObjectBoundingBox;
    // visibility="hidden" element rHidden (20..180) MUST contribute to gHidden bounding box
    CheckEquals(20.0, boxHidden.Left, 1E-4, 'gHidden Left');
    CheckEquals(20.0, boxHidden.Top, 1E-4, 'gHidden Top');
    CheckEquals(180.0, boxHidden.Right, 1E-4, 'gHidden Right');
    CheckEquals(180.0, boxHidden.Bottom, 1E-4, 'gHidden Bottom');

    gDisplayNone := docNode.FindNodeById('gDisplayNone');
    CheckNotNull(gDisplayNone, 'gDisplayNone node should exist');
    boxDisplayNone := gDisplayNone.GetObjectBoundingBox;
    // display="none" element rDisplayNone (20..180) MUST NOT contribute to gDisplayNone bounding box
    CheckEquals(40.0, boxDisplayNone.Left, 1E-4, 'gDisplayNone Left');
    CheckEquals(40.0, boxDisplayNone.Top, 1E-4, 'gDisplayNone Top');
    CheckEquals(160.0, boxDisplayNone.Right, 1E-4, 'gDisplayNone Right');
    CheckEquals(160.0, boxDisplayNone.Bottom, 1E-4, 'gDisplayNone Bottom');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestMixBlendModeAndIsolationParsing;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  g1, g2, p1, p2, p3: TSvgNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <style>' +
         '    .multiply-style { mix-blend-mode: multiply; }' +
         '    .isolate-style { isolation: isolate; }' +
         '  </style>' +
         '  <g id="g1" mix-blend-mode="screen" isolation="isolate">' +
         '    <rect id="p1" width="10" height="10" mix-blend-mode="color-dodge"/>' +
         '    <rect id="p2" class="multiply-style" width="10" height="10"/>' +
         '  </g>' +
         '  <g id="g2" class="isolate-style">' +
         '    <rect id="p3" width="10" height="10" style="mix-blend-mode: hard-light; isolation: isolate;"/>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    g1 := docNode.FindNodeById('g1');
    Check(g1 <> nil, 'g1 should exist');
    CheckEquals(Ord(bmScreen), Ord(g1.MixBlendMode), 'g1 mix-blend-mode should be screen');
    CheckEquals(Ord(isoIsolate), Ord(g1.Isolation), 'g1 isolation should be isolate');

    p1 := docNode.FindNodeById('p1');
    Check(p1 <> nil, 'p1 should exist');
    CheckEquals(Ord(bmColorDodge), Ord(p1.MixBlendMode), 'p1 mix-blend-mode should be color-dodge');

    p2 := docNode.FindNodeById('p2');
    Check(p2 <> nil, 'p2 should exist');
    CheckEquals(Ord(bmMultiply), Ord(p2.MixBlendMode), 'p2 mix-blend-mode should be multiply from CSS stylesheet');

    g2 := docNode.FindNodeById('g2');
    Check(g2 <> nil, 'g2 should exist');
    CheckEquals(Ord(isoIsolate), Ord(g2.Isolation), 'g2 isolation should be isolate from CSS stylesheet');

    p3 := docNode.FindNodeById('p3');
    Check(p3 <> nil, 'p3 should exist');
    CheckEquals(Ord(bmHardLight), Ord(p3.MixBlendMode), 'p3 mix-blend-mode should be hard-light from inline style');
    CheckEquals(Ord(isoIsolate), Ord(p3.Isolation), 'p3 isolation should be isolate from inline style');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestSymbolParsingAndUseResolution;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  symNode: TSvgSymbolNode;
  useNode: TSvgUseNode;
  clonedInstance: TSvgNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <symbol id="mySymbol" viewBox="0 0 20 20" width="20" height="20">' +
         '      <circle id="symCircle" cx="10" cy="10" r="10" fill="red"/>' +
         '    </symbol>' +
         '  </defs>' +
         '  <use id="symUse" href="#mySymbol" x="50" y="50" width="40" height="40"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    symNode := TSvgSymbolNode(docNode.FindNodeById('mySymbol'));
    Check(symNode <> nil, 'mySymbol node should exist');
    Check(not symNode.IsRenderable, 'Symbol template should not be renderable directly');
    Check(symNode.ViewBox.IsValid, 'Symbol viewBox should be valid');
    CheckEquals(20.0, symNode.ViewBox.Width, 1E-4);
    CheckEquals(20.0, symNode.ViewBox.Height, 1E-4);

    useNode := TSvgUseNode(docNode.FindNodeById('symUse'));
    Check(useNode <> nil, 'symUse node should exist');
    CheckEquals(1, useNode.Children.Count, 'useNode should contain 1 expanded symbol instance child');

    clonedInstance := useNode.Children[0];
    Check(clonedInstance.IsRenderable, 'Symbol instance child inside <use> should be renderable');
    Check(clonedInstance is TSvgGroupNode, 'Symbol instance should be a TSvgGroupNode container');
    CheckEquals(1, TSvgGroupNode(clonedInstance).Children.Count, 'Symbol instance should contain circle child');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestPrimitiveShapePercentageUnits;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  rectNode, circleNode, lineNode, ellipseNode: TSvgPathNode;
  pts: TArrayOfArrayOfFloatPoint;
begin
  xml := '<svg width="200" height="200">' +
         '  <rect id="r1" x="0%" y="0%" width="100%" height="100%"/>' +
         '  <circle id="c1" cx="50%" cy="50%" r="25%"/>' +
         '  <line id="l1" x1="0%" y1="0%" x2="100%" y2="100%"/>' +
         '  <ellipse id="e1" cx="50%" cy="50%" rx="50%" ry="25%"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    rectNode := TSvgPathNode(docNode.FindNodeById('r1'));
    Check(rectNode <> nil, 'rectNode r1 should exist');
    Check(Length(rectNode.PathData) > 0, 'rectNode r1 with percentage width/height should have non-empty PathData');
    pts := rectNode.GetPathData(800.0, 600.0);
    Check(Length(pts) > 0, 'rectNode GetPathData(800,600) should produce points');
    // For 800x600 viewport, 100% width = 800, 100% height = 600
    CheckEquals(800.0, pts[0][1].X, 1E-4, 'rectNode 100% width should evaluate dynamically to 800');
    CheckEquals(600.0, pts[0][2].Y, 1E-4, 'rectNode 100% height should evaluate dynamically to 600');

    // Test caching on identical viewport dimensions
    pts := rectNode.GetPathData(800.0, 600.0);
    CheckEquals(800.0, pts[0][1].X, 1E-4, 'rectNode cached 800x600 width');
    CheckEquals(600.0, pts[0][2].Y, 1E-4, 'rectNode cached 800x600 height');

    // Test viewport change invalidates cache and recomputes for 400x300
    pts := rectNode.GetPathData(400.0, 300.0);
    CheckEquals(400.0, pts[0][1].X, 1E-4, 'rectNode recomputed 400x300 width');
    CheckEquals(300.0, pts[0][2].Y, 1E-4, 'rectNode recomputed 400x300 height');

    circleNode := TSvgPathNode(docNode.FindNodeById('c1'));
    Check(circleNode <> nil, 'circleNode c1 should exist');
    Check(Length(circleNode.PathData) > 0, 'circleNode c1 with percentage radius should have non-empty PathData');

    lineNode := TSvgPathNode(docNode.FindNodeById('l1'));
    Check(lineNode <> nil, 'lineNode l1 should exist');
    Check(Length(lineNode.PathData) > 0, 'lineNode l1 with percentage coordinates should have non-empty PathData');
    pts := lineNode.GetPathData(800.0, 600.0);
    CheckEquals(800.0, pts[0][1].X, 1E-4, 'lineNode x2=100% should evaluate to 800');
    CheckEquals(600.0, pts[0][1].Y, 1E-4, 'lineNode y2=100% should evaluate to 600');

    ellipseNode := TSvgPathNode(docNode.FindNodeById('e1'));
    Check(ellipseNode <> nil, 'ellipseNode e1 should exist');
    Check(Length(ellipseNode.PathData) > 0, 'ellipseNode e1 with percentage radii should have non-empty PathData');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestImageNodeParsingAndAttributes;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  imgNode: TSvgImageNode;
  cloned: TSvgNode;
  bbox: TFloatRect;
begin
  xml := '<svg width="200" height="200">' +
         '  <image id="img1" x="10" y="20" width="100" height="80" href="data:image/png;base64,ABC" preserveAspectRatio="none"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'DocNode should not be nil');
  try
    CheckEquals(1, docNode.Children.Count, 'Should have 1 child node');
    Check(docNode.Children[0] is TSvgImageNode, 'Child should be TSvgImageNode');

    imgNode := TSvgImageNode(docNode.Children[0]);
    CheckEquals('img1', imgNode.ID);
    CheckEquals(10.0, imgNode.X.ToPixels);
    CheckEquals(20.0, imgNode.Y.ToPixels);
    CheckEquals(100.0, imgNode.Width.ToPixels);
    CheckEquals(80.0, imgNode.Height.ToPixels);
    CheckEquals('data:image/png;base64,ABC', imgNode.Href);
    Check(imgNode.PreserveAspectRatio.Align = saNone, 'PreserveAspectRatio.Align should be saNone');

    bbox := imgNode.GetObjectBoundingBox;
    CheckEquals(10.0, bbox.Left);
    CheckEquals(20.0, bbox.Top);
    CheckEquals(110.0, bbox.Right);
    CheckEquals(100.0, bbox.Bottom);

    cloned := imgNode.Clone(docNode);
    try
      Check(cloned is TSvgImageNode, 'Clone should be TSvgImageNode');
      CheckEquals(100.0, TSvgImageNode(cloned).Width.ToPixels);
      CheckEquals('data:image/png;base64,ABC', TSvgImageNode(cloned).Href);
    finally
      cloned.Free;
    end;
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgTree.TestNestedSvgIdResolutionAndCurrentColor;
var
  xml: UTF8String;
  docNode, innerDoc: TSvgDocumentNode;
  defNode, useTarget, foundNode: TSvgNode;
  useNode: TSvgUseNode;
  pathNode: TSvgPathNode;
begin
  xml := '<svg width="200" height="200" color="red">' +
         '  <defs>' +
         '    <g id="defSymbol">' +
         '      <rect width="10" height="10"/>' +
         '    </g>' +
         '  </defs>' +
         '  <svg id="innerSvg" color="blue" viewBox="0 0 100 100">' +
         '    <use id="u1" xlink:href="#defSymbol"/>' +
         '    <path id="p1" d="M0 0 L10 10" stroke="currentColor"/>' +
         '  </svg>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'docNode should not be nil');
  try
    CheckEquals(clRed32, docNode.Color.Color, 'Outer doc color should be red');

    defNode := docNode.FindNodeById('defSymbol');
    Check(defNode <> nil, 'defSymbol should be found on outer docNode');

    innerDoc := TSvgDocumentNode(docNode.FindNodeById('innerSvg'));
    Check(innerDoc <> nil, 'innerSvg should be found');
    CheckEquals(clBlue32, innerDoc.Color.Color, 'innerSvg color should be blue');

    // Test cross-boundary ID resolution from inner document to outer document
    foundNode := innerDoc.FindNodeById('defSymbol');
    Check(foundNode <> nil, 'innerDoc.FindNodeById("defSymbol") should resolve to defSymbol from outer doc');
    Check(foundNode = defNode, 'foundNode should equal defNode');

    useNode := TSvgUseNode(innerDoc.FindNodeById('u1'));
    Check(useNode <> nil, 'u1 should be found');
    CheckEquals(1, useNode.Children.Count, 'useNode should have 1 cloned child resolved');

    pathNode := TSvgPathNode(innerDoc.FindNodeById('p1'));
    Check(pathNode <> nil, 'p1 should be found');
    CheckEquals(clBlue32, pathNode.Color.Color, 'p1 should inherit color="blue" from innerSvg');
    Check(pathNode.Stroke.Color.IsCurrentColor, 'p1 stroke color should have IsCurrentColor = True');
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
