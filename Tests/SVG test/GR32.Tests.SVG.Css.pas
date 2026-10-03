unit GR32.Tests.SVG.Css;

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
  Types, SysUtils, Classes,
  GR32, GR32_Transforms, GR32.SVG.Types, GR32.SVG.Tree, GR32.SVG.Css;

type
  TTestSvgCss = class(TTestCase)
  published
    procedure TestCssSelectorParsingAndMatching;
    procedure TestCssStyleSheetParsing;
    procedure TestUseNodeResolution;
    procedure TestCssSpecificityCascade;
    procedure TestCssFontShorthand;
    procedure TestCssCommentsAndCompoundSelectors;
    procedure TestNestedDefsStyle;
  end;

implementation

{ TTestSvgCss }

procedure TTestSvgCss.TestCssSelectorParsingAndMatching;
var
  selElem, selClass, selId, selStar: TSvgCssSelector;
begin
  selElem := TSvgCssSelector.Parse('rect');
  CheckEquals(Ord(skElement), Ord(selElem.Kind));
  CheckEquals('rect', selElem.Name);
  Check(selElem.Matches('rect', 'box', 'rect1'));
  Check(not selElem.Matches('circle', 'box', 'rect1'));

  selClass := TSvgCssSelector.Parse('.red-box');
  CheckEquals(Ord(skClass), Ord(selClass.Kind));
  CheckEquals('red-box', selClass.Name);
  Check(selClass.Matches('rect', 'red-box shape', 'rect1'));
  Check(not selClass.Matches('rect', 'blue-box', 'rect1'));

  selId := TSvgCssSelector.Parse('#main_shape');
  CheckEquals(Ord(skId), Ord(selId.Kind));
  CheckEquals('main_shape', selId.Name);
  Check(selId.Matches('rect', 'box', 'main_shape'));
  Check(not selId.Matches('rect', 'box', 'other_shape'));

  selStar := TSvgCssSelector.Parse('*');
  CheckEquals(Ord(skUniversal), Ord(selStar.Kind));
  Check(selStar.Matches('rect', 'box', 'id1'));
end;

procedure TTestSvgCss.TestCssStyleSheetParsing;
var
  sheet: TSvgCssStyleSheet;
  node: TSvgPathNode;
begin
  sheet := TSvgCssStyleSheet.Create;
  node := TSvgPathNode.Create(nil);
  try
    sheet.ParseCss('rect { fill: red; } .highlight { stroke: blue; stroke-width: 2px; } #target { fill: yellow; }');
    CheckEquals(3, sheet.Rules.Count);

    node.CssClassName := 'highlight';
    node.ID := 'target';

    sheet.ApplyToNode(node, 'rect', node.CssClassName, node.ID);

    CheckEquals(clYellow32, node.Fill.Color.Color);
    CheckEquals(clBlue32, node.Stroke.Color.Color);
    CheckEquals(2.0, node.Stroke.Width.Value, 1E-4);
  finally
    node.Free;
    sheet.Free;
  end;
end;

procedure TTestSvgCss.TestNestedDefsStyle;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  rectNode: TSvgPathNode;
  pathNode: TSvgPathNode;
begin
  xml := '<svg id="Livello_1" data-name="Livello 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 425.2 425.2">' +
         '  <defs><style>.cls-1{fill:#fff;}</style></defs>' +
         '  <rect class="cls-1" width="425.2" height="425.2" rx="69.4" ry="69.4"/>' +
         '  <path d="M338.3,369.9h-4.9V333"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(3, docNode.Children.Count, 'docNode should have 3 children: defs, rect, path');

    Check(docNode.Children[1] is TSvgPathNode, 'Second child should be TSvgPathNode rect');
    rectNode := TSvgPathNode(docNode.Children[1]);
    CheckEquals(clWhite32, rectNode.Fill.Color.Color, 'Rect with class cls-1 should have white fill');

    Check(docNode.Children[2] is TSvgPathNode, 'Third child should be TSvgPathNode path');
    pathNode := TSvgPathNode(docNode.Children[2]);
    CheckEquals(clBlack32, pathNode.Fill.Color.Color, 'Path without class should retain default black fill');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgCss.TestCssSpecificityCascade;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  groupNode: TSvgGroupNode;
  nodeInline, nodeID, nodeClass, nodeTag: TSvgPathNode;
begin
  xml := '<svg width="200" height="200">' +
         '  <style>' +
         '    path { fill: blue; }' +
         '    .red-class { fill: red; }' +
         '    #purple-id { fill: purple; }' +
         '  </style>' +
         '  <g>' +
         '    <path id="purple-id" class="red-class" fill="green" style="fill: yellow;"/>' +
         '    <path id="purple-id" class="red-class" fill="green"/>' +
         '    <path class="red-class" fill="green"/>' +
         '    <path fill="green"/>' +
         '  </g>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(1, docNode.Children.Count, 'docNode should have 1 child group (<style> returns nil)');
    Check(docNode.Children[0] is TSvgGroupNode, 'docNode.Children[0] should be TSvgGroupNode');
    groupNode := TSvgGroupNode(docNode.Children[0]);

    // 1st path: inline style="fill: yellow;" overrides #purple-id, .red-class, path, fill="green"
    nodeInline := TSvgPathNode(groupNode.Children[0]);
    CheckEquals(clYellow32, nodeInline.Fill.Color.Color, 'Inline style should override ID, class, tag, and presentation attributes');

    // 2nd path: ID selector #purple-id (100) overrides .red-class (10), path (1), fill="green"
    nodeID := TSvgPathNode(groupNode.Children[1]);
    CheckEquals(clPurple32, nodeID.Fill.Color.Color, 'ID selector should override class, tag, and presentation attributes');

    // 3rd path: class selector .red-class (10) overrides path (1) and fill="green"
    nodeClass := TSvgPathNode(groupNode.Children[2]);
    CheckEquals(clRed32, nodeClass.Fill.Color.Color, 'Class selector should override tag and presentation attributes');

    // 4th path: tag selector path (1) overrides presentation attribute fill="green"
    nodeTag := TSvgPathNode(groupNode.Children[3]);
    CheckEquals(clBlue32, nodeTag.Fill.Color.Color, 'Tag selector should override presentation attribute');
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgCss.TestCssCommentsAndCompoundSelectors;
var
  sheet: TSvgCssStyleSheet;
  node: TSvgPathNode;
begin
  sheet := TSvgCssStyleSheet.Create;
  node := TSvgPathNode.Create(nil);
  try
    sheet.ParseCss(
      '/* Header comment */' +
      'rect.red-shape { fill: red; }' +
      '/* Mid comment */' +
      '#target { ; stroke: blue; ; stroke-width: 3px; /* prop comment */ }'
    );
    CheckEquals(2, sheet.Rules.Count);

    node.CssClassName := '  red-shape   other-class  ';
    node.ID := 'target';

    sheet.ApplyToNode(node, 'rect', node.CssClassName, node.ID);

    CheckEquals(clRed32, node.Fill.Color.Color);
    CheckEquals(clBlue32, node.Stroke.Color.Color);
    CheckEquals(3.0, node.Stroke.Width.Value, 1E-4);
  finally
    node.Free;
    sheet.Free;
  end;
end;

procedure TTestSvgCss.TestUseNodeResolution;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  useNode: TSvgUseNode;
  clonedPath: TSvgPathNode;
  helper: TFloatMatrixHelper;
  pt, resPt: TFloatPoint;
begin
  xml := '<svg width="200" height="200">' +
         '  <defs>' +
         '    <rect id="rect_shape" x="0" y="0" width="40" height="40" fill="red"/>' +
         '  </defs>' +
         '  <use href="#rect_shape" x="15" y="25"/>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    CheckEquals(2, docNode.Children.Count); // defs and use

    Check(docNode.Children[1] is TSvgUseNode, 'Second child should be TSvgUseNode');
    useNode := TSvgUseNode(docNode.Children[1]);
    CheckEquals('#rect_shape', useNode.Href);
    CheckEquals(15.0, useNode.X, 1E-4);
    CheckEquals(25.0, useNode.Y, 1E-4);

    CheckEquals(1, useNode.Children.Count);
    Check(useNode.Children[0] is TSvgPathNode, 'Use node child should be cloned TSvgPathNode');

    clonedPath := TSvgPathNode(useNode.Children[0]);
    CheckEquals(clRed32, clonedPath.Fill.Color.Color);

    helper.Matrix := clonedPath.Transform;
    pt := FloatPoint(0, 0);
    resPt := helper.TransformPoint(pt);
    CheckEquals(15.0, resPt.X, 1E-4);
    CheckEquals(25.0, resPt.Y, 1E-4);
  finally
    docNode.Free;
  end;
end;

procedure TTestSvgCss.TestCssFontShorthand;
var
  xml: UTF8String;
  docNode: TSvgDocumentNode;
  text1, text2, text3, text4: TSvgTextNode;
begin
  xml := '<svg viewBox="0 0 240 80" xmlns="http://www.w3.org/2000/svg">' +
         '  <style>' +
         '    .small { font: italic 13px sans-serif; }' +
         '    .heavy { font: bold 30px sans-serif; }' +
         '    .Rrrrr { font: italic 40px serif; fill: red; }' +
         '  </style>' +
         '  <text id="t1" x="20" y="35" class="small">My</text>' +
         '  <text id="t2" x="40" y="35" class="heavy">cat</text>' +
         '  <text id="t3" x="55" y="55" class="small">is</text>' +
         '  <text id="t4" x="65" y="55" class="Rrrrr">Grumpy!</text>' +
         '</svg>';

  docNode := ParseSvgXml(xml);
  Check(docNode <> nil, 'ParseSvgXml should return a non-nil TSvgDocumentNode');
  try
    text1 := TSvgTextNode(docNode.FindNodeById('t1'));
    Check(text1 <> nil, 'Text node t1 should be found');
    CheckEquals('italic', text1.FontStyle);
    CheckEquals(13.0, text1.FontSize.Value, 1E-4);
    CheckEquals('sans-serif', text1.FontFamily);

    text2 := TSvgTextNode(docNode.FindNodeById('t2'));
    Check(text2 <> nil, 'Text node t2 should be found');
    CheckEquals('bold', text2.FontWeight);
    CheckEquals(30.0, text2.FontSize.Value, 1E-4);
    CheckEquals('sans-serif', text2.FontFamily);

    text3 := TSvgTextNode(docNode.FindNodeById('t3'));
    Check(text3 <> nil, 'Text node t3 should be found');
    CheckEquals('italic', text3.FontStyle);
    CheckEquals(13.0, text3.FontSize.Value, 1E-4);

    text4 := TSvgTextNode(docNode.FindNodeById('t4'));
    Check(text4 <> nil, 'Text node t4 should be found');
    CheckEquals('italic', text4.FontStyle);
    CheckEquals(40.0, text4.FontSize.Value, 1E-4);
    CheckEquals('serif', text4.FontFamily);
    CheckEquals(clRed32, text4.Fill.Color.Color);
  finally
    docNode.Free;
  end;
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgCss.Suite);
{$ELSE}
  RegisterTest(TTestSvgCss.Suite);
{$ENDIF}

end.
