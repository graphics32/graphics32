unit GR32.Tests.SVG.Facade;

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
  SysUtils, Classes;

type
  TTestSvgFacade = class(TTestCase)
  published
    procedure TestSvgDocumentLoadAndDraw;
    procedure TestSvgImageFormatAdapter;
  end;

implementation

uses
  GR32,
  GR32.SVG,
  GR32.ImageFormats,
  GR32.ImageFormats.SVG;

{ TTestSvgFacade }

procedure TTestSvgFacade.TestSvgDocumentLoadAndDraw;
var
  doc: TSvgDocument;
  bmp: TBitmap32;
const
  xmlStr = '<svg width="200" height="100" viewBox="0 0 200 100">' +
           '  <rect x="0" y="0" width="200" height="100" fill="blue"/>' +
           '</svg>';
begin

  doc := TSvgDocument.Create;
  try
    doc.LoadFromText(xmlStr);
    Check(doc.Root <> nil, 'Root should not be nil');
    CheckEquals(200.0, doc.Width.Value, 1E-4);
    CheckEquals(100.0, doc.Height.Value, 1E-4);

    bmp := TBitmap32.Create;
    try
      bmp.SetSize(200, 100);
      doc.Draw(bmp);
      CheckEquals(clBlue32, bmp.Pixel[100, 50], 'Center pixel should be rendered blue');
    finally
      bmp.Free;
    end;
  finally
    doc.Free;
  end;
end;

procedure TTestSvgFacade.TestSvgImageFormatAdapter;
var
  stream: TStringStream;
  bmp: TBitmap32;
const
  xmlStr = '<svg width="100" height="100">' +
           '  <rect x="0" y="0" width="100" height="100" fill="green"/>' +
           '</svg>';
begin

  stream := TStringStream.Create(xmlStr);
  try
    bmp := TBitmap32.Create;
    try
      bmp.LoadFromStream(stream);
      CheckEquals(clGreen32, bmp.Pixel[50, 50], 'Loaded bitmap center pixel should be green');
    finally
      bmp.Free;
    end;
  finally
    stream.Free;
  end;
end;

initialization
{$IFDEF FPC}
  RegisterTest('', TTestSvgFacade.Suite);
{$ELSE}
  RegisterTest(TTestSvgFacade.Suite);
{$ENDIF}

end.
