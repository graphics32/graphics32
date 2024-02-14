unit GR32_TestGuiPng;

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
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$DEFINE VisualVerification}

uses
  {$IFDEF FPC}fpcunit, testregistry, {$ELSE} TestFramework, {$ENDIF}
  Classes, SysUtils, {$IFDEF FPC} LazPNG, {$ELSE} pngimage, {$ENDIF}
  FileTestFramework,
  GR32,
  GR32_Png,
  GR32_PortableNetworkGraphic;

type
  TCustomTestPngGR32 = class abstract(TTestCase)
  protected
    FPortableNetworkGraphic: TPortableNetworkGraphic32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestGR32PngBasics = class(TCustomTestPngGR32)
  published
    procedure TestBasicAssign;
    procedure TestBasicWriting;
    procedure TestAdvancedWriting;
    procedure TestCompressionTranscoding;
    procedure TestPerformanceTest;
  end;

type
  TCustomGR32FileTest = class abstract(TFileTestCase)
  protected
    FPortableNetworkGraphic: TPortableNetworkGraphic32;
  protected
  public
    procedure SetUp; override;
    procedure TearDown; override;
    class function HandlesFiles(const AFilename: string): boolean; override;
  end;

  TGR32FileTest = class(TCustomGR32FileTest)
  protected
  public
    // The following tests are currently disabled:
    // - We fail (by design) when saving the iTXt chunk as we only support reading it right now.
    // - The ctzn0g04.pn fail with a CRC error.
  published
    procedure TestLoadFromFile;
    procedure TestSaveToStream;
    procedure TestSaveToStreamRountrip;
  end;

  TGR32InvalidFileTest = class(TCustomGR32FileTest)
  protected
  public
    class function HandlesFiles(const AFilename: string): boolean; override;
  published
    procedure TestInvalidFiles;
  end;

type
  TTestPngToBitmap32 = class(TCustomGR32FileTest)
  private
  published
    procedure TestPngToBitmap32;
  end;

type
  TTestPngBitmap32Roundtrip = class(TCustomTestPngGR32)
  protected
    procedure TestRoundtrip(const AFilename: string; AColorType: TColorType; ABitDepth: integer);
  published
    procedure TestRoundtripBlackAndWhite;
    procedure TestRoundtripGrayscale2bit;
    procedure TestRoundtripGrayscale4bit;
    procedure TestRoundtripGrayscale8bit;
    procedure TestRoundtripRGB8bits;
    procedure TestRoundtripRGB16bits;
    procedure TestRoundtripIndexed1bit;
    procedure TestRoundtripIndexed2bit;
    procedure TestRoundtripIndexed4bit;
    procedure TestRoundtripIndexed8bit;
    procedure TestRoundtripGrayscaleAlpha8bit;
    procedure TestRoundtripGrayscaleAlpha16bit;
    procedure TestRoundtripRGBA8bits;
    procedure TestRoundtripRGBA16bits;
  end;

  TTestPngGR32AncillaryChunks = class(TCustomTestPngGR32)
  published
    procedure TestDrawingTrueColor5bits;
    procedure TestDrawingTrueColor13bits;
    procedure TestDrawingPalette3bits;
    procedure TestDrawingPalette5bits;
    procedure TestPhysicalDimensions8x32FlatPixels;
    procedure TestPhysicalDimensions32x8HighPixels;
    procedure TestPhysicalDimensions8x8SquarePixels;
    procedure TestPhysicalDimensions1000PixelsPerMeter;
    procedure TestChromaChunkPalette;
    procedure TestChromaChunkTrueColor;
    procedure TestModificationTime;
  end;


procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream); overload;
procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; Filename: string); overload;
procedure InitializeGR32PngTests;

procedure TestPngAgainstReferenceBmp(TestCase: TTestCase; const PNGFileName, BMPFilename: TFileName);

implementation

uses
  IOUtils,
  StrUtils,
  Dialogs,
  Controls;

resourcestring
  RCStrWrongDisplay = 'PNG was not displayed correctly!';
  RCStrTestFileNotFound = 'The test png file %s could not be found!';
  RCStrPhysicalPixelDimensionChunkMissing = 'Physical pixel dimension chunk is missing!';
  RCStrSignificantBitsChunkMissing = 'Significant bits chunk is missing!';
  RCStrWrongSignificantBitsFormat = 'Wrong significant bits format!';

const
  CPngSuiteDir = '.\Data\PNG Suite Images\';
  CTestPngDir = '.\Data\Test\';
  CRegressionTestPngDir = '.\Data\RegressionTest\';

procedure TestPngAgainstReferenceBmp(TestCase: TTestCase; const PNGFileName, BMPFilename: TFileName);
var
  Png: TPortableNetworkGraphic32;
  Bitmap: TBitmap32;
  ReferenceBitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    Png := TPortableNetworkGraphic32.Create;
    try
      Png.LoadFromFile(PNGFilename);
      Bitmap.Assign(Png);
      TestCase.CheckEquals(Bitmap.Width, Png.Width);
      TestCase.CheckEquals(Bitmap.Height, Png.Height);
    finally
      Png.Free;
    end;

    // Enable the following line to generate the reference Bitmap files
    // Bitmap.SaveToFile(BMPFilename, False, InfoHeaderVersion5);

    ReferenceBitmap := TBitmap32.Create;
    try
      ReferenceBitmap.LoadFromFile(BMPFilename);

      TestCase.CheckEquals(Bitmap.Width, ReferenceBitmap.Width);
      TestCase.CheckEquals(Bitmap.Height, ReferenceBitmap.Height);
      TestCase.CheckEqualsMem(Bitmap.Bits, ReferenceBitmap.Bits, Bitmap.Width * Bitmap.Height * SizeOf(TColor32));
    finally
      ReferenceBitmap.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream);
var
  {$IFDEF FPC}
  PNGObject: TPNGImage;
  {$ELSE}
  PNGObject: TPNGImage;
  {$ENDIF}
  TransparentColor: TColor32;
  PixelPtr: PColor32;
  AlphaPtr: PByte;
  X, Y: Integer;
begin
  PNGObject := nil;
  try
    {$IFDEF FPC}
    PNGObject := TPngImage.Create;
    {$ELSE}
    PNGObject := TPNGImage.Create;
    {$ENDIF}
    PNGObject.LoadFromStream(SrcStream);

    DstBitmap.Assign(PNGObject);
    DstBitmap.ResetAlpha;

    {$IFDEF FPC}
    // yet todo
    {$ELSE}
    case PNGObject.TransparencyMode of
    ptmPartial:
      begin
       if (PNGObject.Header.ColorType = COLOR_GRAYSCALEALPHA) or
          (PNGObject.Header.ColorType = COLOR_RGBALPHA) then
        begin
         PixelPtr := PColor32(@DstBitmap.Bits[0]);
         for Y := 0 to DstBitmap.Height - 1 do
          begin
           AlphaPtr := PByte(PNGObject.AlphaScanline[Y]);
           for X := 0 to DstBitmap.Width - 1 do
            begin
             PixelPtr^ := (PixelPtr^ and $00FFFFFF) or (TColor32(AlphaPtr^) shl 24);
             Inc(PixelPtr);
             Inc(AlphaPtr);
            end;
          end;
        end;
      end;
    ptmBit:
      begin
       TransparentColor := Color32(PNGObject.TransparentColor);
       PixelPtr := PColor32(@DstBitmap.Bits[0]);
       for X := 0 to DstBitmap.Height * DstBitmap.Width - 1 do
        begin
         if PixelPtr^ = TransparentColor
          then PixelPtr^ := PixelPtr^ and $00FFFFFF;
         Inc(PixelPtr);
        end;
      end;
    end;
    {$ENDIF}
  finally
    if Assigned(PNGObject) then 
      PNGObject.Free;
  end;
end;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; Filename: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadPNGintoBitmap32(DstBitmap, FileStream);
  finally
    FileStream.Free;
  end;
end;


{ TCustomTestPngGR32 }

procedure TCustomTestPngGR32.SetUp;
begin
  FPortableNetworkGraphic := TPortableNetworkGraphic32.Create;
end;

procedure TCustomTestPngGR32.TearDown;
begin
  FreeAndNil(FPortableNetworkGraphic);
end;


{ TCustomGR32FileTest }

class function TCustomGR32FileTest.HandlesFiles(const AFilename: string): boolean;
var
  Name: string;
begin
  Name := TPath.GetFileName(AFileName);

  // Ignore corrupted files
  Result := (not StartsText('x', Name));
end;

procedure TCustomGR32FileTest.SetUp;
begin
  FPortableNetworkGraphic := TPortableNetworkGraphic32.Create;
end;

procedure TCustomGR32FileTest.TearDown;
begin
  FreeAndNil(FPortableNetworkGraphic);
end;


{ TTestGR32PngBasics }

procedure TTestGR32PngBasics.TestPerformanceTest;
begin
  if FileExists(CTestPngDir + 'PerformanceTest.png') then 
    FPortableNetworkGraphic.LoadFromFile(CTestPngDir + 'PerformanceTest.png')
  else
    Check(True, 'File ' + CTestPngDir + 'PerformanceTest.png does not exist!');
end;

procedure TTestGR32PngBasics.TestBasicAssign;
var
  SourceStream: TMemoryStream;
  TargetStream: TMemoryStream;
  TempPng: TPortableNetworkGraphic32;
begin

  SourceStream := TMemoryStream.Create;
  TargetStream := TMemoryStream.Create;
  try
    FPortableNetworkGraphic.LoadFromFile(CTestPngDir + 'TestTrueColor32bit.png');
    FPortableNetworkGraphic.SaveToStream(SourceStream);

    TempPng := TPortableNetworkGraphic32.Create;
    try
      TempPng.Assign(FPortableNetworkGraphic);
      TempPng.SaveToStream(TargetStream);
    finally
      TempPng.Free;
    end;

    CheckEquals(SourceStream.Size, TargetStream.Size);
    CheckEqualsMem(SourceStream.Memory, TargetStream.Memory, SourceStream.Size);
  finally
    SourceStream.Free;
    TargetStream.Free;
  end;
end;

procedure TTestGR32PngBasics.TestBasicWriting;
var
  OriginalStream: TMemoryStream;
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    OriginalStream := TMemoryStream.Create;
    try
     OriginalStream.LoadFromFile(CTestPngDir + 'TestTrueColor32bit.png');

     OriginalStream.Position := 0;
     FPortableNetworkGraphic.LoadFromStream(OriginalStream);

     FPortableNetworkGraphic.SaveToFile('Test.png');
     TFile.Delete('test.png');
     FPortableNetworkGraphic.SaveToStream(TempStream);

     TempStream.Position := 0;
     FPortableNetworkGraphic.LoadFromStream(TempStream);

     if TempStream.Size < (OriginalStream.Size div 10) then
       Fail(Format('Size of written stream is likely to be too small! (%.0n)', [TempStream.Size * 1.0]));

(*
     CheckEquals(OriginalStream.Size, TempStream.Size);
     CompareMem(OriginalStream.Memory, TempStream.Memory, TempStream.Size);
*)
    finally
      OriginalStream.Free;
    end;
  finally
    TempStream.Free;
  end;
  Check(True);
end;

procedure TTestGR32PngBasics.TestAdvancedWriting;
var
  OriginalStream: TMemoryStream;
  Bitmap32: TBitmap32;
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    OriginalStream := TMemoryStream.Create;
    try
     OriginalStream.LoadFromFile(CPngSuiteDir + 'basi6a08.png');
     OriginalStream.Seek(0, soFromBeginning);
     FPortableNetworkGraphic.LoadFromStream(OriginalStream);
     Bitmap32 := TBitmap32.Create;
     try
       Bitmap32.Assign(FPortableNetworkGraphic);
       Bitmap32.Clear;
       FPortableNetworkGraphic.Assign(Bitmap32);
     finally
       Bitmap32.Free;
     end;
     FPortableNetworkGraphic.SaveToStream(TempStream);
     TempStream.Seek(0, soFromBeginning);
     FPortableNetworkGraphic.LoadFromStream(TempStream);
     if TempStream.Size < (OriginalStream.Size div 10) then
       Fail(Format('Size of written stream is likely to be too small! (%.0n)', [TempStream.Size * 1.0]));
    finally
      FreeAndNil(OriginalStream);
    end;
  finally
    TempStream.Free;
  end;
  Check(True);
end;

procedure TTestGR32PngBasics.TestCompressionTranscoding;
var
  Index      : Integer;
  RecentSize : Integer;
  TempStream : TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    FPortableNetworkGraphic.LoadFromFile(CTestPngDir + 'TestTrueColor32bit.png');
    FPortableNetworkGraphic.CompressionLevel := 1;
    FPortableNetworkGraphic.SaveToStream(TempStream);
    RecentSize := TempStream.Size;

    for Index := 2 to 9 do
    begin
      TempStream.Clear;
      FPortableNetworkGraphic.CompressionLevel := Index;
      FPortableNetworkGraphic.SaveToStream(TempStream);

      CheckTrue(TempStream.Size <= RecentSize, 'TempStream.Size is greater than RecentSize');
      RecentSize := TempStream.Size;
    end;
  finally
    TempStream.Free;
  end;
end;

{ TGR32InvalidFileTest }

class function TGR32InvalidFileTest.HandlesFiles(const AFilename: string): boolean;
var
  Name: string;
begin
  Name := TPath.GetFileName(AFileName);

  // Only handle corrupted files
  Result := (StartsText('x', Name));
end;

procedure TGR32InvalidFileTest.TestInvalidFiles;
begin
  // x00n0g01.png: empty 0x0 grayscale file
  // xcrn0g04.png: added cr bytes
  // xlfn0g04.png: converted cr bytes to lf and removed all NULs

  Check(FileExists(TestFileName), Format(RCStrTestFileNotFound, [TestFileName]));

  try

    FPortableNetworkGraphic.LoadFromFile(TestFileName);

  except
    on E: EPngError do
    begin
      Check(True);
      exit;
    end;
  end;
  Fail('Invalid file didn''t fail');
end;

{ TGR32FileTest }

procedure TGR32FileTest.TestLoadFromFile;
begin
  FPortableNetworkGraphic.LoadFromFile(TestFileName);

  Check(True);
end;

procedure TGR32FileTest.TestSaveToStream;
var
  Stream: TMemoryStream;
begin
  FPortableNetworkGraphic.LoadFromFile(TestFileName);

  Stream := TMemoryStream.Create;
  try
    FPortableNetworkGraphic.SaveToStream(Stream);

    // No validation of content. We only test the ability to save.
    Check(Stream.Size > 0);
  finally
    Stream.Free;
  end;
end;

procedure TGR32FileTest.TestSaveToStreamRountrip;
var
  Stream1: TMemoryStream;
  Stream2: TMemoryStream;
begin
  FPortableNetworkGraphic.LoadFromFile(TestFileName);

  Stream1 := TMemoryStream.Create;
  Stream2 := TMemoryStream.Create;
  try
    FPortableNetworkGraphic.SaveToStream(Stream1);

    Stream1.Position := 0;
    FPortableNetworkGraphic.LoadFromStream(Stream1);

    FPortableNetworkGraphic.SaveToStream(Stream2);

    CheckEquals(Stream1.Size, Stream2.Size);
    CheckEqualsMem(Stream1.Memory, Stream2.Memory, Stream1.Size);
  finally
    Stream1.Free;
    Stream2.Free;
  end;
end;


{ TTestPngToBitmap32 }

procedure TTestPngToBitmap32.TestPngToBitmap32;
var
  ReferenceFilename: string;
begin
  ReferenceFilename := TPath.ChangeExtension(TestFileName, '.bmp');

  TestPngAgainstReferenceBmp(Self, TestFileName, ReferenceFilename);
end;

{ TTestPngBitmap32Roundtrip }

procedure TTestPngBitmap32Roundtrip.TestRoundtrip(const AFilename: string; AColorType: TColorType; ABitDepth: integer);
var
  Filename: string;
  Filename2: string;
  Bitmap: TBitmap32;
begin
  Filename := CPngSuiteDir + AFilename;
  FPortableNetworkGraphic.LoadFromFile(Filename);

  // check if format for saving can be determined correctly
  Bitmap := TBitmap32.Create;
  try
    Bitmap.Assign(FPortableNetworkGraphic);
    FPortableNetworkGraphic.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

  Check(FPortableNetworkGraphic.ColorType = AColorType);
  Check(FPortableNetworkGraphic.BitDepth = ABitDepth);

  Filename2 := TPath.ChangeExtension(Filename, '.test.png');
  FPortableNetworkGraphic.SaveToFile(Filename2);
  try

    TestPngAgainstReferenceBmp(Self, Filename2, TPath.ChangeExtension(Filename, '.bmp'));

  finally
    TFile.Delete(Filename2);
  end;
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripBlackAndWhite;
begin
  TestRoundtrip('basn0g01.png', ctIndexedColor, 1);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripGrayscale2bit;
begin
  TestRoundtrip('basn0g02.png', ctIndexedColor, 2);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripGrayscale4bit;
begin
  TestRoundtrip('basn0g04.png', ctIndexedColor, 4);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripGrayscale8bit;
begin
  TestRoundtrip('basn0g08.png', ctIndexedColor, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripIndexed1bit;
begin
  TestRoundtrip('basn3p01.png', ctIndexedColor, 1);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripIndexed2bit;
begin
  TestRoundtrip('basn3p02.png', ctIndexedColor, 2);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripIndexed4bit;
begin
  TestRoundtrip('basn3p04.png', ctIndexedColor, 4);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripIndexed8bit;
begin
  TestRoundtrip('basn3p08.png', ctIndexedColor, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripRGB8bits;
begin
  TestRoundtrip('basn2c08.png', ctTrueColor, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripRGB16bits;
begin
  TestRoundtrip('basn2c16.png', ctTrueColor, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripGrayscaleAlpha8bit;
begin
  TestRoundtrip('basn4a08.png', ctGrayscaleAlpha, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripGrayscaleAlpha16bit;
begin
  TestRoundtrip('basn4a16.png', ctGrayscaleAlpha, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripRGBA8bits;
begin
  TestRoundtrip('basn6a08.png', ctTrueColorAlpha, 8);
end;

procedure TTestPngBitmap32Roundtrip.TestRoundtripRGBA16bits;
begin
  TestRoundtrip('basn6a16.png', ctTrueColorAlpha, 8);
end;


{ TTestPngGR32AncillaryChunks }

procedure TTestPngGR32AncillaryChunks.TestDrawingTrueColor5bits;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cs5n2c08.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.SignificantBitsChunk, RCStrSignificantBitsChunkMissing);

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits is TPngSignificantBitsFormat23, RCStrWrongSignificantBitsFormat);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).RedBits = 5);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).GreenBits = 5);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).BlueBits = 5);
end;

procedure TTestPngGR32AncillaryChunks.TestDrawingTrueColor13bits;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cs3n2c16.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.SignificantBitsChunk, RCStrSignificantBitsChunkMissing);

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits is TPngSignificantBitsFormat23, RCStrWrongSignificantBitsFormat);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).RedBits = 13);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).GreenBits = 13);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).BlueBits = 13);
end;

procedure TTestPngGR32AncillaryChunks.TestDrawingPalette3bits;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cs3n3p08.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.SignificantBitsChunk, RCStrSignificantBitsChunkMissing);

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits is TPngSignificantBitsFormat23, RCStrWrongSignificantBitsFormat);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).RedBits = 3);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).GreenBits = 3);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).BlueBits = 3);
end;

procedure TTestPngGR32AncillaryChunks.TestDrawingPalette5bits;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cs5n3p08.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.SignificantBitsChunk, RCStrSignificantBitsChunkMissing);

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits is TPngSignificantBitsFormat23, RCStrWrongSignificantBitsFormat);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).RedBits = 5);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).GreenBits = 5);
  Check(TPngSignificantBitsFormat23(FPortableNetworkGraphic.SignificantBitsChunk.SignificantBits).BlueBits = 5);
end;

procedure TTestPngGR32AncillaryChunks.TestPhysicalDimensions8x32FlatPixels;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cdfn2c08.png');

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.PixelsPerUnitX = 1);
  Check(FPortableNetworkGraphic.PixelsPerUnitY = 4);
  Check(FPortableNetworkGraphic.PixelUnit = 0);
end;

procedure TTestPngGR32AncillaryChunks.TestPhysicalDimensions32x8HighPixels;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cdhn2c08.png');

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.PixelsPerUnitX = 4);
  Check(FPortableNetworkGraphic.PixelsPerUnitY = 1);
  Check(FPortableNetworkGraphic.PixelUnit = 0);
end;

procedure TTestPngGR32AncillaryChunks.TestPhysicalDimensions8x8SquarePixels;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cdsn2c08.png');

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.PixelsPerUnitX = 1);
  Check(FPortableNetworkGraphic.PixelsPerUnitY = 1);
  Check(FPortableNetworkGraphic.PixelUnit = 0);
end;

procedure TTestPngGR32AncillaryChunks.TestPhysicalDimensions1000PixelsPerMeter;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cdun2c08.png');

  // check information stored in the chunk
  Check(FPortableNetworkGraphic.PixelsPerUnitX = 1000);
  Check(FPortableNetworkGraphic.PixelsPerUnitY = 1000);
  Check(FPortableNetworkGraphic.PixelUnit = 1);
end;

procedure TTestPngGR32AncillaryChunks.TestChromaChunkPalette;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'ccwn3p08.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.PrimaryChromaticitiesChunk, RCStrPhysicalPixelDimensionChunkMissing);

  // check information stored in the chunk
  CheckEquals(0.3127, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.WhiteXAsSingle, 1E-3);
  CheckEquals(0.3290, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.WhiteYAsSingle, 1E-3);
  CheckEquals(0.64, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.RedXAsSingle, 1E-3);
  CheckEquals(0.33, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.RedYAsSingle, 1E-3);
  CheckEquals(0.30, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.GreenXAsSingle, 1E-3);
  CheckEquals(0.60, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.GreenYAsSingle, 1E-3);
  CheckEquals(0.15, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.BlueXAsSingle, 1E-3);
  CheckEquals(0.06, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.BlueYAsSingle, 1E-3);
end;

procedure TTestPngGR32AncillaryChunks.TestChromaChunkTrueColor;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'ccwn2c08.png');

  // test if chunk is present
  CheckNotNull(FPortableNetworkGraphic.PrimaryChromaticitiesChunk, RCStrPhysicalPixelDimensionChunkMissing);

  // check information stored in the chunk
  CheckEquals(0.3127, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.WhiteXAsSingle, 1E-3);
  CheckEquals(0.3290, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.WhiteYAsSingle, 1E-3);
  CheckEquals(0.64, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.RedXAsSingle, 1E-3);
  CheckEquals(0.33, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.RedYAsSingle, 1E-3);
  CheckEquals(0.30, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.GreenXAsSingle, 1E-3);
  CheckEquals(0.60, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.GreenYAsSingle, 1E-3);
  CheckEquals(0.15, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.BlueXAsSingle, 1E-3);
  CheckEquals(0.06, FPortableNetworkGraphic.PrimaryChromaticitiesChunk.BlueYAsSingle, 1E-3);
end;

procedure TTestPngGR32AncillaryChunks.TestModificationTime;
begin
  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cm7n0g04.png');
  CheckEquals(EncodeDate(1970, 1, 1) + EncodeTime(0, 0, 0, 0), FPortableNetworkGraphic.ModifiedTime, 1 / 24 / 60 / 60 / 1000);

  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cm9n0g04.png');
  CheckEquals(EncodeDate(1999, 12, 31) + EncodeTime(23, 59, 59, 0), FPortableNetworkGraphic.ModifiedTime, 1 / 24 / 60 / 60 / 1000);

  FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'cm0n0g04.png');
  CheckEquals(EncodeDate(2000, 1, 1) + EncodeTime(12, 34, 56, 0), FPortableNetworkGraphic.ModifiedTime, 1 / 24 / 60 / 60 / 1000);
end;

procedure InitializeGR32PngTests;
var
{$IFDEF FPC}
  TestPngSuite: TTestSuite;
{$ELSE}
  FileTestSuite: TTestSuite;
{$ENDIF}
begin
{$IFDEF FPC}
  RegisterTest(TTestGR32PngBasics);
  RegisterTest(TTestPngToBitmap32);

  TestPngSuite := TTestSuite.Create('PNG Suite Tests');
  with TestPngSuite do
  begin
    AddTestSuiteFromClass(TTestPngBitmap32Roundtrip);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteBasicAdam7);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteSizeTest);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteBackgroundTest);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteGammaTest);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteFilteringTest);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteAdditionalPaletteTest);
    AddTestSuiteFromClass(TTestPngGR32AncillaryChunks);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteChunkOrdering);
    AddTestSuiteFromClass(TTestPngGR32DrawingSuiteCompressionLevel);
  end;
  RegisterTest('TestPngSuite', TestPngSuite);
{$ELSE}

  FileTestSuite := TFolderTestSuite.Create('PNG Suite Tests', TGR32FileTest, CPngSuiteDir, '*.png', True);
  FileTestSuite.AddTests(TGR32InvalidFileTest);
  FileTestSuite.AddTests(TTestPngToBitmap32);
  RegisterTest(FileTestSuite);

  RegisterTest(TTestGR32PngBasics.Suite);
  RegisterTest(TTestPngBitmap32Roundtrip.Suite);
  RegisterTest(TTestPngGR32AncillaryChunks.Suite);

  FileTestSuite := TFolderTestSuite.Create('PNG Regression Tests', TGR32FileTest, CRegressionTestPngDir, '*.png', True);
  RegisterTest(FileTestSuite);
{$ENDIF}
end;

{$IFNDEF FPC}
initialization
  InitializeGR32PngTests;
{$ENDIF}

end.
