unit GR32.Tests.SVG.Xml;

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
 * The Original Code is SVG reader for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
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
  GR32.SVG.Xml;

type
  TTestSvgXmlParser = class(TTestCase)
  published
    procedure TestBasicParsing;
    procedure TestAttributes;
    procedure TestCDataAndComment;
    procedure TestFindAndForEach;
    procedure TestUnescape;
    procedure TestValuePUtf8CharMethods;
    procedure TestErrorHandling;
    procedure TestDocType;
  end;

implementation

{ TTestSvgXmlParser }

procedure TTestSvgXmlParser.TestBasicParsing;
var
  parser: TXmlParser;
  xml: RawUtf8;
  token: TXmlToken;
  s: RawUtf8;
begin
  xml := '<svg width="100" height="200"><rect x="10" y="20"/></svg>';
  parser.Init(xml);

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementStart), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('svg', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtAttribute), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('width', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtAttribute), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('height', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementStart), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('rect', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtAttribute), Ord(token));

  token := parser.ParseNext;
  CheckEquals(Ord(xtAttribute), Ord(token));

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementEnd), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('rect', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementEnd), Ord(token));
  parser.NameToUtf8(s);
  CheckEquals('svg', string(s));

  token := parser.ParseNext;
  CheckEquals(Ord(xtEof), Ord(token));
end;

procedure TTestSvgXmlParser.TestAttributes;
var
  parser: TXmlParser;
  xml: RawUtf8;
  valStr: RawUtf8;
begin
  xml := '<g id="main_group" fill="red" opacity="0.5"/>';
  parser.Init(xml);

  CheckEquals(Ord(xtElementStart), Ord(parser.ParseNext));

  CheckEquals(Ord(xtAttribute), Ord(parser.ParseNext));
  CheckEquals('id', string(parser.Name.ToUtf8));
  parser.ValueToUtf8(valStr);
  CheckEquals('main_group', string(valStr));

  CheckEquals(Ord(xtAttribute), Ord(parser.ParseNext));
  CheckEquals('fill', string(parser.Name.ToUtf8));
  parser.ValueToUtf8(valStr);
  CheckEquals('red', string(valStr));

  CheckEquals(Ord(xtAttribute), Ord(parser.ParseNext));
  CheckEquals('opacity', string(parser.Name.ToUtf8));
  parser.ValueToUtf8(valStr);
  CheckEquals('0.5', string(valStr));

  Check(parser.Value.Buffer <> nil, 'Value buffer should not be nil');
end;

procedure TTestSvgXmlParser.TestCDataAndComment;
var
  parser: TXmlParser;
  xml: RawUtf8;
  s: RawUtf8;
begin
  xml := '<svg><!-- a comment --><style><![CDATA[rect { fill: red; }]]></style></svg>';
  parser.Init(xml, [xpoKeepComments]);

  CheckEquals(Ord(xtElementStart), Ord(parser.ParseNext));

  CheckEquals(Ord(xtComment), Ord(parser.ParseNext));
  parser.ValueToUtf8(s);
  CheckEquals(' a comment ', string(s));

  CheckEquals(Ord(xtElementStart), Ord(parser.ParseNext));

  CheckEquals(Ord(xtCData), Ord(parser.ParseNext));
  parser.ValueToUtf8(s);
  CheckEquals('rect { fill: red; }', string(s));
end;

procedure TTestSvgXmlParser.TestFindAndForEach;
var
  parser: TXmlParser;
  xml: RawUtf8;
  count: Integer;
  valStr: RawUtf8;
begin
  xml := '<svg><g id="g1"><path id="p1"/><path id="p2"/></g></svg>';
  parser.Init(xml);

  Check(parser.Find('svg/g'), 'TXmlParser.Find failed');
  CheckEquals('g', string(parser.Name.ToUtf8));

  count := 0;
  while parser.ForEach('path', 0) do
  begin
    Inc(count);
  end;
  CheckEquals(2, count);

  parser.Rewind; // Workaround for issue #601: https://github.com/synopse/mORMot2/issues/601

  Check(parser.GetU('/svg/g/path', valStr));
end;

procedure TTestSvgXmlParser.TestUnescape;
var
  dest: RawUtf8;
  res: Boolean;
begin
  res := XmlUnescape('Hello &amp; World &lt;SVG&gt;', 29, dest);
  Check(res);
  CheckEquals('Hello & World <SVG>', string(dest));

  CheckEquals(Ord('<'), Ord(NumCharToUcs4('#x3c', 4)));
  CheckEquals(Ord('>'), Ord(NumCharToUcs4('#62', 3)));
end;

procedure TTestSvgXmlParser.TestValuePUtf8CharMethods;
var
  val: TValuePUtf8Char;
  strVal: RawUtf8;
  extVal: Extended;
begin
  strVal := '12345';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);

  CheckEquals('12345', val.ToString);
  CheckEquals(12345, val.ToInteger);
  CheckEquals(12345, Integer(val.ToCardinal));
  CheckEquals(Int64(12345), val.ToInt64);
  Check(val.Equal('12345'));

  strVal := '-9876';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  CheckEquals(-9876, val.ToInteger);
  CheckEquals(Int64(-9876), val.ToInt64);

  strVal := 'true';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  Check(val.ToBoolean);

  strVal := '1';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  Check(val.ToBoolean);

  strVal := '3.14159';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  CheckEquals(3.14159, val.ToDouble, 1E-4);

  strVal := '-1.5e2';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  CheckEquals(-150.0, val.ToDouble, 1E-4);

  strVal := '2.5e-2';
  val.Text := PUtf8Char(strVal);
  val.Len := Length(strVal);
  CheckEquals(0.025, val.ToDouble, 1E-4);

  Check(GetExtended(PUtf8Char(RawUtf8('123.456')), extVal) <> nil);
  CheckEquals(123.456, extVal, 1E-4);
end;

procedure TTestSvgXmlParser.TestErrorHandling;
var
  parser: TXmlParser;
  xml: RawUtf8;
  raised: Boolean;
begin
  xml := '<svg><rect></svg>';
  parser.Init(xml, [xpoNoException]);

  while parser.ParseNext not in [xtEof, xtError] do;

  CheckEquals(Ord(xtError), Ord(parser.Kind));
  Check(parser.LastError <> xpeNone);

  raised := False;
  try
    parser.Init(xml);
    while parser.ParseNext <> xtEof do;
  except
    on E: EXmlException do
      raised := True;
  end;
  Check(raised, 'Expected EXmlException for mismatched closing tag');
end;

procedure TTestSvgXmlParser.TestDocType;
var
  parser: TXmlParser;
  xml: RawUtf8;
  valStr: RawUtf8;
  token: TXmlToken;
begin
  // 1. Standard SVG DOCTYPE (skipped when xpoKeepDocType not set)
  xml := '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"><svg><rect/></svg>';
  parser.Init(xml);

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementStart), Ord(token));
  CheckEquals('svg', string(parser.Name.ToUtf8));

  // 2. DOCTYPE kept when xpoKeepDocType option is set
  xml := '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"><svg/>';
  parser.Init(xml, [xpoKeepDocType]);

  token := parser.ParseNext;
  CheckEquals(Ord(xtDocType), Ord(token));
  parser.ValueToUtf8(valStr);
  CheckEquals(' svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"', string(valStr));

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementStart), Ord(token));

  // 3. DOCTYPE with inline DTD subset [...]
  xml := '<!DOCTYPE svg [ <!ENTITY foo "bar"> ]><svg/>';
  parser.Init(xml, [xpoKeepDocType]);

  token := parser.ParseNext;
  CheckEquals(Ord(xtDocType), Ord(token));
  parser.ValueToUtf8(valStr);
  CheckEquals(' svg [ <!ENTITY foo "bar"> ]', string(valStr));

  token := parser.ParseNext;
  CheckEquals(Ord(xtElementStart), Ord(token));
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgXmlParser.Suite);
{$ELSE}
  RegisterTest(TTestSvgXmlParser.Suite);
{$ENDIF}

end.
