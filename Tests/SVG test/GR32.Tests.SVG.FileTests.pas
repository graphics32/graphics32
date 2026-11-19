unit GR32.Tests.SVG.FileTests;

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
  SysUtils, Classes, IOUtils, Math,
  IniFiles,
  GR32, GR32.SVG, GR32.ImageFormats.PNG32,
  FileTestFramework;

type
  { Record holding statistical comparison metrics between two bitmaps }
  TBitmapComparisonResult = record
    Match: Boolean;
    TotalPixels: Integer;
    MismatchPixels: Integer;
    MismatchFraction: Double;
    MaxDifference: Byte;
    AverageDifference: Double;
    RMSE: Double;
    ErrorMessage: string;
  end;

  { Comparison utility function that compares two TBitmap32 instances,
    accounting for small rasterization, filtering, and anti-aliasing variations. }
  function CompareBitmaps(const Actual, Expected: TBitmap32;
    TolerancePerChannel: Byte = 16;
    MaxMismatchFraction: Double = 0.05): TBitmapComparisonResult; overload;

  function CompareBitmaps(const Actual, Expected: TBitmap32;
    out ErrorMessage: string;
    TolerancePerChannel: Byte = 16;
    MaxMismatchFraction: Double = 0.05): Boolean; overload;

type
  { Unit test case for rendering SVG files and comparing the output against PNG reference images }
  TSvgCompareFileTest = class(TFileTestCase)
  private class var
    FResultIni: TMemIniFile;
    FFixes: integer;
    FRegressions: integer;
  protected
  public
    class function HandlesFiles(const AFilename: string): Boolean; override;
  published
    procedure TestCompareSvgToPng;
  end;

  { Unit test case for parsing SVG files without errors }
  TSvgParseFileTest = class(TFileTestCase)
  public
    class function HandlesFiles(const AFilename: string): Boolean; override;
  published
    procedure TestParseSvg;
  end;

implementation

uses
  GR32_PortableNetworkGraphic;

{ CompareBitmaps }

function CompareBitmaps(const Actual, Expected: TBitmap32;
  TolerancePerChannel: Byte;
  MaxMismatchFraction: Double): TBitmapComparisonResult;
var
  PixelCount: Integer;
  ActualPtr, ExpectedPtr: PColor32Entry;
  I: Integer;
  DiffA, DiffR, DiffG, DiffB: Integer;
  MaxPixDiff: Integer;
  SumDiffSq: Double;
  TotalDiffSum: Int64;
begin
  // Initialize result structure
  Result.Match := False;
  Result.TotalPixels := 0;
  Result.MismatchPixels := 0;
  Result.MismatchFraction := 0.0;
  Result.MaxDifference := 0;
  Result.AverageDifference := 0.0;
  Result.RMSE := 0.0;
  Result.ErrorMessage := '';

  // Validate that both bitmaps are provided
  if (Actual = nil) or (Expected = nil) then
  begin
    Result.ErrorMessage := 'One or both bitmaps are nil';
    Exit;
  end;

  // Validate that dimensions match
  if (Actual.Width <> Expected.Width) or (Actual.Height <> Expected.Height) then
  begin
    Result.ErrorMessage := Format('Bitmap dimension mismatch: Actual (%dx%d) vs Expected (%dx%d)',
      [Actual.Width, Actual.Height, Expected.Width, Expected.Height]);
    Exit;
  end;

  // Handle empty bitmaps
  PixelCount := Actual.Width * Actual.Height;
  Result.TotalPixels := PixelCount;
  if PixelCount = 0 then
  begin
    Result.Match := True;
    Exit;
  end;

  ActualPtr := PColor32Entry(Actual.Bits);
  ExpectedPtr := PColor32Entry(Expected.Bits);

  SumDiffSq := 0.0;
  TotalDiffSum := 0;

  // Scan through all pixels comparing ARGB color components
  for I := 0 to PixelCount - 1 do
  begin
    DiffA := Abs(Integer(ActualPtr.A) - Integer(ExpectedPtr.A));
    DiffR := Abs(Integer(ActualPtr.R) - Integer(ExpectedPtr.R));
    DiffG := Abs(Integer(ActualPtr.G) - Integer(ExpectedPtr.G));
    DiffB := Abs(Integer(ActualPtr.B) - Integer(ExpectedPtr.B));

    MaxPixDiff := Max(Max(DiffA, DiffR), Max(DiffG, DiffB));
    if MaxPixDiff > Result.MaxDifference then
      Result.MaxDifference := MaxPixDiff;

    TotalDiffSum := TotalDiffSum + DiffA + DiffR + DiffG + DiffB;
    SumDiffSq := SumDiffSq + Sqr(DiffA) + Sqr(DiffR) + Sqr(DiffG) + Sqr(DiffB);

    // If any channel difference exceeds the tolerated threshold, count as mismatch
    if (DiffA > TolerancePerChannel) or
       (DiffR > TolerancePerChannel) or
       (DiffG > TolerancePerChannel) or
       (DiffB > TolerancePerChannel) then
    begin
      Inc(Result.MismatchPixels);
    end;

    Inc(ActualPtr);
    Inc(ExpectedPtr);
  end;

  Result.MismatchFraction := Result.MismatchPixels / PixelCount;
  Result.AverageDifference := TotalDiffSum / (PixelCount * 4.0);
  Result.RMSE := Sqrt(SumDiffSq / (PixelCount * 4.0));

  // Determine whether the image matches within accepted tolerance thresholds
  if Result.MismatchFraction <= MaxMismatchFraction then
    Result.Match := True
  else
    Result.ErrorMessage := Format(
      'Image mismatch exceeds tolerance: %.2f%% pixels differ (limit: %.2f%%), ' +
      'MaxDiff=%d (threshold: %d), RMSE=%.2f, AvgDiff=%.2f',
      [Result.MismatchFraction * 100.0, MaxMismatchFraction * 100.0,
       Result.MaxDifference, TolerancePerChannel, Result.RMSE, Result.AverageDifference]);
end;

function CompareBitmaps(const Actual, Expected: TBitmap32;
  out ErrorMessage: string;
  TolerancePerChannel: Byte;
  MaxMismatchFraction: Double): Boolean;
var
  CompRes: TBitmapComparisonResult;
begin
  CompRes := CompareBitmaps(Actual, Expected, TolerancePerChannel, MaxMismatchFraction);
  ErrorMessage := CompRes.ErrorMessage;
  Result := CompRes.Match;
end;

{ TSvgCompareFileTest }

class function TSvgCompareFileTest.HandlesFiles(const AFilename: string): Boolean;
begin
  // Only process .svg files
  Result := SameText(TPath.GetExtension(AFilename), '.svg');
end;

procedure TSvgCompareFileTest.TestCompareSvgToPng;
var
  PngFileName: string;
  SvgDoc: TSvgDocument;
  ActualBmp: TBitmap32;
  ExpectedBmp: TBitmap32;
  Png: TPortableNetworkGraphic;
  CompRes: TBitmapComparisonResult;
begin
  var ResultKey := StringReplace(TestFileName, '=', '_', [rfReplaceAll]);
  var Expected := FResultIni.ReadString('results', ResultKey, 'new');
  if Expected.StartsWith('ignore') then
  begin
    Status('Ignored: '+TestFileName);
    exit;
  end;
  var n := Pos(' ', Expected);
  if (n > 0) then
    Setlength(Expected, n-1);

  try
    // Determine expected reference PNG path matching the SVG filename
    PngFileName := TPath.ChangeExtension(TestFileName, '.png');
    Check(TFile.Exists(PngFileName),
      Format('Corresponding PNG reference file not found for SVG: "%s"', [TestFileName]));

    ExpectedBmp := TBitmap32.Create;
    try
      // Load reference PNG image
      // ExpectedBmp.LoadFromFile(PngFileName);
      // We have to use TPortableNetworkGraphic directly in order to work around bugs in the resvg png files
      Png := TPortableNetworkGraphic.Create;
      try
        Png.IgnoredChunks := ['sBIT'];
        Png.LoadFromFile(PngFileName);
        ExpectedBmp.Assign(Png);
      finally
        Png.Free;
      end;
      Check(not ExpectedBmp.Empty,
        Format('Loaded reference PNG is empty: "%s"', [PngFileName]));

      ActualBmp := TBitmap32.Create;
      try
        // Size destination bitmap to match expected image dimensions
        ActualBmp.SetSize(ExpectedBmp.Width, ExpectedBmp.Height);
        ActualBmp.Clear;

        SvgDoc := TSvgDocument.Create;
        try
          // Load and render SVG document
          SvgDoc.LoadFromFile(TestFileName);
          Check(SvgDoc.Root <> nil,
            Format('Failed to parse SVG document: "%s"', [TestFileName]));

          SvgDoc.Draw(ActualBmp);

          // Compare actual rendered SVG against expected reference PNG
          CompRes := CompareBitmaps(ActualBmp, ExpectedBmp);
          Check(CompRes.Match,
            Format('Comparison failure for "%s": %s', [TestFileName, CompRes.ErrorMessage]));
        finally
          SvgDoc.Free;
        end;
      finally
        ActualBmp.Free;
      end;
    finally
      ExpectedBmp.Free;
    end;

    var Result: string;
    if (Expected.StartsWith('FAIL')) then
    begin
      Inc(FFixes);
      Result := 'pass (FIXED)';
    end else
      Result := 'pass';
    if (Expected <> Result) then
      FResultIni.WriteString('results', ResultKey, Result);
  except
    var Result: string;
    if (not Expected.StartsWith('FAIL')) then
    begin
      Inc(FRegressions);
      Result := 'FAIL (REGRESSION)';
    end else
      Result := 'FAIL';
    if (Expected <> Result) then
      FResultIni.WriteString('results', ResultKey, Result);
    raise;
  end;
end;

{ TSvgParseFileTest }

class function TSvgParseFileTest.HandlesFiles(const AFilename: string): Boolean;
begin
  // Only process .svg files
  Result := SameText(TPath.GetExtension(AFilename), '.svg');
end;

procedure TSvgParseFileTest.TestParseSvg;
var
  SvgDoc: TSvgDocument;
begin
  SvgDoc := TSvgDocument.Create;
  try
    // Verify that the SVG file can load and parse without error
    SvgDoc.LoadFromFile(TestFileName);
    Check(SvgDoc.Root <> nil,
      Format('SVG failed to parse valid root node: "%s"', [TestFileName]));
  finally
    SvgDoc.Free;
  end;
end;

type
  TCompareFolderTestSuite = class(TFolderTestSuite)
  protected
    procedure RunTest(testResult: TTestResult); override;
  end;

procedure TCompareFolderTestSuite.RunTest(testResult: TTestResult);
begin
  TSvgCompareFileTest.FFixes := 0;
  TSvgCompareFileTest.FRegressions := 0;

  inherited;
end;

var
  CompareTestSuite: TFolderTestSuite;
  ParseTestSuite: TFolderTestSuite;

{ TCompareFolderTestSuite }

initialization
  // Test suite 1: Iterates through and below ..\..\Data\Compare, rendering SVGs and comparing to PNGs
  CompareTestSuite := TCompareFolderTestSuite.Create(
    'SVG Render Comparison',
    TSvgCompareFileTest,
    '..\..\Data\Compare',
    '*.svg',
    True
  );
  RegisterTest(CompareTestSuite);

  // Test suite 2: Iterates through and below ..\..\Data\Parse, verifying SVGs load and parse without error
  ParseTestSuite := TFolderTestSuite.Create(
    'SVG Parse Test',
    TSvgParseFileTest,
    '..\..\Data\Parse',
    '*.svg',
    True
  );
  RegisterTest(ParseTestSuite);

  TSvgCompareFileTest.FResultIni := TMemIniFile.Create('..\..\Data\Compare\results.txt');

  TSvgCompareFileTest.FFixes := TSvgCompareFileTest.FResultIni.ReadInteger('status', 'Fixes', 0);
  TSvgCompareFileTest.FResultIni.WriteInteger('status', 'Fixes before', TSvgCompareFileTest.FFixes);

  TSvgCompareFileTest.FRegressions := TSvgCompareFileTest.FResultIni.ReadInteger('status', 'Regressions', 0);
  TSvgCompareFileTest.FResultIni.WriteInteger('status', 'Regressions before', TSvgCompareFileTest.FRegressions);
finalization
  TSvgCompareFileTest.FResultIni.WriteInteger('status', 'Fixes', TSvgCompareFileTest.FFixes);
  TSvgCompareFileTest.FResultIni.WriteInteger('status', 'Regressions', TSvgCompareFileTest.FRegressions);

  TSvgCompareFileTest.FResultIni.UpdateFile;
  TSvgCompareFileTest.FResultIni.Free;
end.
