unit GR32.Tests.SVG.Fixtures;

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
  SysUtils, Classes, IOUtils,
  GR32, GR32.SVG.Types, GR32.SVG.Tree,
  FileTestFramework;

type
  TSvgStage1FileTest = class(TFileTestCase)
  published
    procedure TestStage1Normalization;
  end;

implementation

procedure TSvgStage1FileTest.TestStage1Normalization;
var
  fileText, svgText, expectedAST, actualAST: string;
  svgPos, expectedPos: Integer;
  docNode: TSvgDocumentNode;
begin
  fileText := TFile.ReadAllText(TestFileName);

  svgPos := Pos('--- SVG ---', fileText);
  expectedPos := Pos('--- EXPECTED AST ---', fileText);

  Check(svgPos > 0, 'Fixture file must contain "--- SVG ---" section in ' + TestFileName);
  Check(expectedPos > svgPos, 'Fixture file must contain "--- EXPECTED AST ---" section after "--- SVG ---" in ' + TestFileName);

  svgText := Trim(Copy(fileText, svgPos + Length('--- SVG ---'), expectedPos - (svgPos + Length('--- SVG ---'))));
  expectedAST := Trim(Copy(fileText, expectedPos + Length('--- EXPECTED AST ---'), Length(fileText)));

  docNode := ParseSvgXml(UTF8String(svgText));
  Check(docNode <> nil, 'ParseSvgXml should return non-nil document node for ' + TestFileName);
  try
    actualAST := Trim(docNode.Dump);

    // If -UPDATE_SVG_FIXTURES command line switch is passed, auto-update expected AST section
    if FindCmdLineSwitch('UPDATE_SVG_FIXTURES', True) then
    begin
      fileText := '--- SVG ---' + sLineBreak + svgText + sLineBreak + '--- EXPECTED AST ---' + sLineBreak + actualAST + sLineBreak;
      TFile.WriteAllText(TestFileName, fileText);
    end;

    // Normalize line endings for comparison
    expectedAST := StringReplace(expectedAST, #13#10, #10, [rfReplaceAll]);
    actualAST := StringReplace(actualAST, #13#10, #10, [rfReplaceAll]);

    CheckEquals(expectedAST, actualAST, 'Normalized AST dump mismatch in ' + TestFileName);
  finally
    docNode.Free;
  end;
end;

var
  FileTestSuite: TFolderTestSuite;

initialization
  FileTestSuite := TFolderTestSuite.Create(
    'SVG Stage 1 Normalization',
    TSvgStage1FileTest,
    '../../Fixtures',
    '*.test',
    True
  );
  RegisterTest(FileTestSuite);

end.
