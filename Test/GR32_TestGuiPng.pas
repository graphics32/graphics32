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

uses
  TestFramework, Classes, Contnrs, SysUtils, GR32, GR32_Png,
  GR32_PortableNetworkGraphic, GR32_TestGuiPngDisplay, pngimage;

type
  TCustomTestPngGR32 = class(TTestCase)
  protected
    FPortableNetworkGraphic: TPortableNetworkGraphic32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestPngGR32File = class(TCustomTestPngGR32)
  private
    procedure InternalTestInvalidFile(FileName: TFileName);
  published
    procedure TestScanning;
    procedure TestBasicWriting;
    procedure TestPerformanceTest;
    procedure TestInvalidFiles;
  end;

  TCustomTestPngGR32Drawing = class(TCustomTestPngGR32)
  private
    procedure InternalTestDrawing(FileName: TFileName);
  end;

  TTestPngGR32Drawing = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingGrayscale1bit;
    procedure TestDrawingGrayscale2bit;
    procedure TestDrawingGrayscale4bit;
    procedure TestDrawingGrayscale8bit;
    procedure TestDrawingIndex1bit;
    procedure TestDrawingIndex2bit;
    procedure TestDrawingIndex4bit;
    procedure TestDrawingIndex8bit;
    procedure TestDrawingTrueColor24;
    procedure TestDrawingTrueColor32;
    procedure TestDrawingAdam7Grayscale1bit;
    procedure TestDrawingAdam7Grayscale2bit;
    procedure TestDrawingAdam7Grayscale4bit;
    procedure TestDrawingAdam7Grayscale8bit;
    procedure TestDrawingAdam7Index1bit;
    procedure TestDrawingAdam7Index2bit;
    procedure TestDrawingAdam7Index4bit;
    procedure TestDrawingAdam7Index8bit;
    procedure TestDrawingAdam7TrueColor24;
    procedure TestDrawingAdam7TrueColor32;
    procedure TestDrawingLarge;
  end;

  // Test methods for class TPortableNetworkGraphic
  TTestPngGR32DrawingSuiteBasicNonInterlaced = class(TCustomTestPngGR32Drawing)
  published
     procedure TestDrawingBlackAndWhite;
     procedure TestDrawingGrayscale2bit;
     procedure TestDrawingGrayscale4bit;
     procedure TestDrawingGrayscale8bit;
     procedure TestDrawingGrayscale16bit;
     procedure TestDrawingRGB8bits;
     procedure TestDrawingRGB16bits;
     procedure TestDrawingIndexed1bit;
     procedure TestDrawingIndexed2bit;
     procedure TestDrawingIndexed4bit;
     procedure TestDrawingIndexed8bit;
     procedure TestDrawingGrayscaleAlpha8bit;
     procedure TestDrawingGrayscaleAlpha16bit;
     procedure TestDrawingRGBA8bits;
     procedure TestDrawingRGBA16bits;
  end;

  TTestPngGR32DrawingSuiteBasicAdam7 = class(TCustomTestPngGR32Drawing)
  published
     procedure TestDrawingBlackAndWhite;
     procedure TestDrawingGrayscale2bit;
     procedure TestDrawingGrayscale4bit;
     procedure TestDrawingGrayscale8bit;
     procedure TestDrawingGrayscale16bit;
     procedure TestDrawingRGB8bits;
     procedure TestDrawingRGB16bits;
     procedure TestDrawingIndexed1bit;
     procedure TestDrawingIndexed2bit;
     procedure TestDrawingIndexed4bit;
     procedure TestDrawingIndexed8bit;
     procedure TestDrawingGrayscaleAlpha8bit;
     procedure TestDrawingGrayscaleAlpha16bit;
     procedure TestDrawingRGBA8bits;
     procedure TestDrawingRGBA16bits;
  end;

  TTestPngGR32DrawingSuiteSizeTest = class(TCustomTestPngGR32Drawing)
  published
     procedure TestDrawingPalette1x1;
     procedure TestDrawingPalette2x2;
     procedure TestDrawingPalette3x3;
     procedure TestDrawingPalette4x4;
     procedure TestDrawingPalette5x5;
     procedure TestDrawingPalette6x6;
     procedure TestDrawingPalette7x7;
     procedure TestDrawingPalette8x8;
     procedure TestDrawingPalette9x9;
     procedure TestDrawingPalette32x32;
     procedure TestDrawingPalette33x33;
     procedure TestDrawingPalette34x34;
     procedure TestDrawingPalette35x35;
     procedure TestDrawingPalette36x36;
     procedure TestDrawingPalette37x37;
     procedure TestDrawingPalette38x38;
     procedure TestDrawingPalette39x39;
     procedure TestDrawingPalette40x40;
     procedure TestDrawingPalette1x1Adam7;
     procedure TestDrawingPalette2x2Adam7;
     procedure TestDrawingPalette3x3Adam7;
     procedure TestDrawingPalette4x4Adam7;
     procedure TestDrawingPalette5x5Adam7;
     procedure TestDrawingPalette6x6Adam7;
     procedure TestDrawingPalette7x7Adam7;
     procedure TestDrawingPalette8x8Adam7;
     procedure TestDrawingPalette9x9Adam7;
     procedure TestDrawingPalette32x32Adam7;
     procedure TestDrawingPalette33x33Adam7;
     procedure TestDrawingPalette34x34Adam7;
     procedure TestDrawingPalette35x35Adam7;
     procedure TestDrawingPalette36x36Adam7;
     procedure TestDrawingPalette37x37Adam7;
     procedure TestDrawingPalette38x38Adam7;
     procedure TestDrawingPalette39x39Adam7;
     procedure TestDrawingPalette40x40Adam7;
  end;

  TTestPngGR32DrawingSuiteBackgroundTest = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingGrayscaleAlpha8bitBlack;
    procedure TestDrawingGrayscaleAlpha16bitGray;
    procedure TestDrawingRGBA8bitWhite;
    procedure TestDrawingRGBA16bitYellow;
    procedure TestDrawingGrayscaleAlpha8bitNoBackground;
    procedure TestDrawingGrayscaleAlpha16bitNoBackground;
    procedure TestDrawingRGBA8bitNoBackground;
    procedure TestDrawingRGBA16bitNoBackground;
  end;

  TTestPngGR32DrawingSuiteGammaTest = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingGammaGrayscaleA;
    procedure TestDrawingGammaGrayscaleB;
    procedure TestDrawingGammaGrayscaleC;
    procedure TestDrawingGammaGrayscaleD;
    procedure TestDrawingGammaGrayscaleE;
    procedure TestDrawingGammaGrayscaleF;
    procedure TestDrawingGammaColorA;
    procedure TestDrawingGammaColorB;
    procedure TestDrawingGammaColorC;
    procedure TestDrawingGammaColorD;
    procedure TestDrawingGammaColorE;
    procedure TestDrawingGammaColorF;
    procedure TestDrawingGammaIndexedA;
    procedure TestDrawingGammaIndexedB;
    procedure TestDrawingGammaIndexedC;
    procedure TestDrawingGammaIndexedD;
    procedure TestDrawingGammaIndexedE;
    procedure TestDrawingGammaIndexedF;
  end;

  TTestPngGR32DrawingSuiteFilteringTest = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingGrayscaleFilterType0;
    procedure TestDrawingGrayscaleFilterType1;
    procedure TestDrawingGrayscaleFilterType2;
    procedure TestDrawingGrayscaleFilterType3;
    procedure TestDrawingGrayscaleFilterType4;
    procedure TestDrawingColorFilterType0;
    procedure TestDrawingColorFilterType1;
    procedure TestDrawingColorFilterType2;
    procedure TestDrawingColorFilterType3;
    procedure TestDrawingColorFilterType4;
  end;

  TTestPngGR32DrawingSuiteAdditionalPaletteTest = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingTrueColor;
    procedure TestDrawingTrueColorAlpha;
    procedure TestDrawingSuggestedPalette1bitGrayScale;
    procedure TestDrawingSuggestedPalette1bitTrueColor;
    procedure TestDrawingSuggestedPalette2bitGrayScale;
    procedure TestDrawingSuggestedPalette2bitTrueColor;
  end;

  TTestPngGR32DrawingSuiteAncillaryChunksTest = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingTrueColor5bits;
    procedure TestDrawingTrueColor8bits;
    procedure TestDrawingTrueColor13bits;
    procedure TestDrawingPalette3bits;
    procedure TestDrawingPalette5bits;
    procedure TestDrawingPalette8bits;
    procedure TestPhysicalDimensions8x32FlatPixels;
    procedure TestPhysicalDimensions32x8HighPixels;
    procedure TestPhysicalDimensions8x8SquarePixels;
    procedure TestPhysicalDimensions1000PixelsPerMeter;
    procedure TestChromaChunkPalette;
    procedure TestChromaChunkTrueColor;
    procedure TestHistogramChunk15colors;
    procedure TestHistogramChunk256colors;
    procedure TestModificationTime;
    procedure TestNoTextualData;
    procedure TestWithTextualData;
    procedure TestWithCompressedTextualData;
  end;

  TTestPngGR32DrawingSuiteChunkOrdering = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingGrayscale1IDAT;
    procedure TestDrawingGrayscale2IDAT;
    procedure TestDrawingGrayscale4IDAT;
    procedure TestDrawingGrayscaleAllIDATs;
    procedure TestDrawingTrueColor1IDAT;
    procedure TestDrawingTrueColor2IDAT;
    procedure TestDrawingTrueColor4IDAT;
    procedure TestDrawingTrueColorAllIDATs;
  end;

  TTestPngGR32DrawingSuiteCompressionLevel = class(TCustomTestPngGR32Drawing)
  published
    procedure TestDrawingTrueColorCompressionLevel0;
    procedure TestDrawingTrueColorCompressionLevel3;
    procedure TestDrawingTrueColorCompressionLevel6;
    procedure TestDrawingTrueColorCompressionLevel9;
  end;


procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream); overload;
procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; Filename: string); overload;

implementation

uses
  Dialogs, Controls;

resourcestring
  RCStrWrongDisplay = 'PNG was not displayed correctly!';
  RCStrTestFileNotFound = 'The test png file %s could not be found!';
  RCStrPhysicalPixelDimensionChunkMissing = 'Physical pixel dimension chunk is missing!';
  RCStrSignificantBitsChunkMissing = 'Significant bits chunk is missing!';
  RCStrWrongSignificantBitsFormat = 'Wrong significant bits format!';

const
  CPngSuiteDir = '..\Resources\PNG Suite Images\';
  CTestPngDir = '..\Resources\Test\';

var
  TestPngSuite : TTestSuite;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream);
var
  PNGObject: TPNGObject;
  TransparentColor: TColor32;
  PixelPtr: PColor32;
  AlphaPtr: PByte;
  X, Y: Integer;
begin
 PNGObject := nil;
  try
   PNGObject := TPngObject.Create;
   PNGObject.LoadFromStream(SrcStream);

   DstBitmap.Assign(PNGObject);
   DstBitmap.ResetAlpha;

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
  finally
   if Assigned(PNGObject) then PNGObject.Free;
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


{ TTestPngGR32File }

procedure TTestPngGR32File.TestScanning;
var
  SR      : TSearchRec;
  Succeed : Boolean;
begin
 if FindFirst('*.png*', faAnyFile, SR) = 0 then
  try
   repeat
    Succeed := True;
    try
     // ignore corrupted files
     if (SR.Name = 'x00n0g01.png') or
        (SR.Name = 'xcrn0g04.png') or
        (SR.Name = 'xlfn0g04.png') then Continue;

     FPortableNetworkGraphic.LoadFromFile(SR.Name)
    except
     on e: EPngError do MessageDlg(SR.Name + ': ' + e.Message, mtError, [mbOK], 0);
     else Succeed := False;
    end;
    Check(Succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TTestPngGR32File.TestInvalidFiles;
begin
 // empty 0x0 grayscale file
 InternalTestInvalidFile(CPngSuiteDir + 'x00n0g01.png');

 // added cr bytes
 try
  InternalTestInvalidFile(CPngSuiteDir + 'xcrn0g04.png');
  Fail('Wrong header is ignored!');
 except
 end;

 // converted cr bytes to lf and removed all NULs
 try
  InternalTestInvalidFile(CPngSuiteDir + 'xlfn0g04.png');
  Fail('Wrong header is ignored!');
 except
 end;
end;

procedure TTestPngGR32File.TestPerformanceTest;
begin
 if FileExists(CTestPngDir + 'PerformanceTest.png')
  then FPortableNetworkGraphic.LoadFromFile(CTestPngDir + 'PerformanceTest.png')
  else Fail('File ' + CTestPngDir + 'PerformanceTest.png does not exist!');
end;

procedure TTestPngGR32File.InternalTestInvalidFile(FileName: TFileName);
begin
 if not FileExists(FileName)
  then Fail(Format(RCStrTestFileNotFound, [FileName]));

 FPortableNetworkGraphic.LoadFromFile(FileName);
end;

procedure TTestPngGR32File.TestBasicWriting;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FPortableNetworkGraphic do
    begin
     FPortableNetworkGraphic.LoadFromFile(CTestPngDir + 'TestTrueColor32bit.png');
     FPortableNetworkGraphic.SaveToStream(TempStream);
     TempStream.Seek(0, soFromBeginning);
     FPortableNetworkGraphic.LoadFromStream(TempStream);
    end;
  finally
   Free;
  end;
end;


{ TCustomTestPngGR32Drawing }

procedure TCustomTestPngGR32Drawing.InternalTestDrawing(FileName: TFileName);
begin
 if not FileExists(FileName)
  then Fail(Format(RCStrTestFileNotFound, [FileName]));

 with TFmDisplay.Create(nil) do
  try
   LoadPNGintoBitmap32(Reference, FileName);

   FPortableNetworkGraphic.LoadFromFile(FileName);

   Image32.Bitmap.Width := FPortableNetworkGraphic.Width;
   Image32.Bitmap.Height := FPortableNetworkGraphic.Height;

   ClientWidth := FPortableNetworkGraphic.Width + 16;
   ClientHeight := FPortableNetworkGraphic.Height + LbRenderer.Height + BtYes.Height + 24;

   Internal.Assign(FPortableNetworkGraphic);

   if ShowModal <> mrYes
    then Fail(RCStrWrongDisplay);

  finally
   Free;
  end;
end;


{ TTestPngGR32Drawing }

procedure TTestPngGR32Drawing.TestDrawingGrayscale1bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestGrayscale1bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingGrayscale2bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestGrayscale2bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingGrayscale4bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestGrayscale4bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingGrayscale8bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestGrayscale8bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingIndex1bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestIndex1bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingIndex2bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestIndex2bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingIndex4bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestIndex4bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingIndex8bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestIndex8bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingLarge;
begin
 InternalTestDrawing(CTestPngDir + 'PerformanceTest.png')
end;

procedure TTestPngGR32Drawing.TestDrawingTrueColor24;
begin
 InternalTestDrawing(CTestPngDir + 'TestTrueColor24bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingTrueColor32;
begin
 InternalTestDrawing(CTestPngDir + 'TestTrueColor32bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Grayscale1bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Grayscale1bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Grayscale2bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Grayscale2bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Grayscale4bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Grayscale4bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Grayscale8bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Grayscale8bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Index1bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Index1bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Index2bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Index2bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Index4bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Index4bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7Index8bit;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7Index8bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7TrueColor24;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7TrueColor24bit.png');
end;

procedure TTestPngGR32Drawing.TestDrawingAdam7TrueColor32;
begin
 InternalTestDrawing(CTestPngDir + 'TestAdam7TrueColor32bit.png');
end;

{ TTestPngGR32DrawingSuiteBasicNonInterlaced }

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingBlackAndWhite;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn0g01.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor); // ctGrayscale
    Check(BitDepth = 1);
    SaveToFile('basn0g01.x.png');
    InternalTestDrawing('basn0g01.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscale2bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn0g02.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor); // ctGrayscale
    Check(BitDepth = 2);
    SaveToFile('basn0g02.x.png');
    InternalTestDrawing('basn0g02.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscale4bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn0g04.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor); // ctGrayscale
    Check(BitDepth = 4);
    SaveToFile('basn0g04.x.png');
    InternalTestDrawing('basn0g04.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscale8bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn0g08.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor); // ctGrayscale
    Check(BitDepth = 8);
    SaveToFile('basn0g08.x.png');
    InternalTestDrawing('basn0g08.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscale16bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn0g16.png');
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingIndexed1bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing('basn3p01.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor);
    Check(BitDepth = 1);

    SaveToFile('basn3p01.x.png');
    InternalTestDrawing('basn3p01.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingIndexed2bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn3p02.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor);
    Check(BitDepth = 2);

    SaveToFile('basn3p02.x.png');
    InternalTestDrawing('basn3p02.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingIndexed4bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn3p04.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor);
    Check(BitDepth = 4);

    SaveToFile('basn3p04.x.png');
    InternalTestDrawing('basn3p04.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingIndexed8bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn3p08.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctIndexedColor);
    Check(BitDepth = 8);

    SaveToFile('basn3p08.x.png');
    InternalTestDrawing('basn3p08.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingRGB8bits;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn2c08.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctTrueColor);
    Check(BitDepth = 8);

    SaveToFile('basn2c08.x.png');
    InternalTestDrawing('basn2c08.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingRGB16bits;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn2c16.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctTrueColor);
    Check(BitDepth = 8);

    SaveToFile('basn2c16.x.png');
    InternalTestDrawing('basn2c16.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscaleAlpha8bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn4a08.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctGrayscaleAlpha);
    Check(BitDepth = 8);

    SaveToFile('basn4a08.x.png');
    InternalTestDrawing('basn4a08.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingGrayscaleAlpha16bit;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn4a16.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctGrayscaleAlpha);
    Check(BitDepth = 8);

    SaveToFile('basn4a16.x.png');
    InternalTestDrawing('basn4a16.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingRGBA8bits;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn6a08.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctTrueColorAlpha);
    Check(BitDepth = 8);

    SaveToFile('basn6a08.x.png');
    InternalTestDrawing('basn6a08.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;

procedure TTestPngGR32DrawingSuiteBasicNonInterlaced.TestDrawingRGBA16bits;
var
  Bitmap32 : TBitmap32;
begin
 InternalTestDrawing(CPngSuiteDir + 'basn6a16.png');

 // check if format for saving can be determined correctly
 Bitmap32 := TBitmap32.Create;
 try
  Bitmap32.Assign(FPortableNetworkGraphic);
  with TPortableNetworkGraphic32.Create do
   try
    Assign(Bitmap32);
    Check(ColorType = ctTrueColorAlpha);
    Check(BitDepth = 8);

    SaveToFile('basn6a16.x.png');
    InternalTestDrawing('basn6a16.x.png');
   finally
    Free;
   end;
 finally
  FreeAndNil(Bitmap32);
 end;
end;


{ TTestPngGR32DrawingSuiteBasicAdam7 }

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingBlackAndWhite;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi0g01.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscale2bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi0g02.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscale4bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi0g04.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscale8bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi0g08.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscale16bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi0g16.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingRGB8bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi2c08.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingRGB16bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi2c16.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingIndexed1bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi3p01.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingIndexed2bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi3p02.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingIndexed4bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi3p04.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingIndexed8bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi3p08.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscaleAlpha8bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi4a08.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingGrayscaleAlpha16bit;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi4a16.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingRGBA8bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi6a08.png');
end;

procedure TTestPngGR32DrawingSuiteBasicAdam7.TestDrawingRGBA16bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'basi6a16.png');
end;


{ TTestPngGR32DrawingSuiteSizeTest }

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette1x1;
begin
 InternalTestDrawing(CPngSuiteDir + 's01n3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette2x2;
begin
 InternalTestDrawing(CPngSuiteDir + 's02n3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette3x3;
begin
 InternalTestDrawing(CPngSuiteDir + 's03n3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette4x4;
begin
 InternalTestDrawing(CPngSuiteDir + 's04n3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette5x5;
begin
 InternalTestDrawing(CPngSuiteDir + 's05n3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette6x6;
begin
 InternalTestDrawing(CPngSuiteDir + 's06n3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette7x7;
begin
 InternalTestDrawing(CPngSuiteDir + 's07n3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette8x8;
begin
 InternalTestDrawing(CPngSuiteDir + 's08n3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette9x9;
begin
 InternalTestDrawing(CPngSuiteDir + 's09n3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette32x32;
begin
 InternalTestDrawing(CPngSuiteDir + 's32n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette33x33;
begin
 InternalTestDrawing(CPngSuiteDir + 's33n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette34x34;
begin
 InternalTestDrawing(CPngSuiteDir + 's34n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette35x35;
begin
 InternalTestDrawing(CPngSuiteDir + 's35n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette36x36;
begin
 InternalTestDrawing(CPngSuiteDir + 's36n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette37x37;
begin
 InternalTestDrawing(CPngSuiteDir + 's37n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette38x38;
begin
 InternalTestDrawing(CPngSuiteDir + 's38n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette39x39;
begin
 InternalTestDrawing(CPngSuiteDir + 's39n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette40x40;
begin
 InternalTestDrawing(CPngSuiteDir + 's40n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette1x1Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's01i3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette2x2Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's02i3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette3x3Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's03i3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette4x4Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's04i3p01.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette5x5Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's05i3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette6x6Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's06i3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette7x7Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's07i3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette8x8Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's08i3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette9x9Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's09i3p02.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette32x32Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's32i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette33x33Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's33i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette34x34Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's34i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette35x35Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's35i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette36x36Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's36i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette37x37Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's37i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette38x38Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's38i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette39x39Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's39i3p04.png');
end;

procedure TTestPngGR32DrawingSuiteSizeTest.TestDrawingPalette40x40Adam7;
begin
 InternalTestDrawing(CPngSuiteDir + 's40i3p04.png');
end;


{ TTestPngGR32DrawingSuiteBackgroundTest }

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingGrayscaleAlpha8bitBlack;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgbn4a08.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingGrayscaleAlpha16bitGray;
begin
  InternalTestDrawing(CPngSuiteDir + 'bggn4a16.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingGrayscaleAlpha16bitNoBackground;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgai4a16.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingGrayscaleAlpha8bitNoBackground;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgai4a08.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingRGBA16bitNoBackground;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgan6a16.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingRGBA16bitYellow;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgyn6a16.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingRGBA8bitNoBackground;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgan6a08.png');
end;

procedure TTestPngGR32DrawingSuiteBackgroundTest.TestDrawingRGBA8bitWhite;
begin
 InternalTestDrawing(CPngSuiteDir + 'bgwn6a08.png');
end;


{ TTestPngGR32DrawingSuiteGammaTest }

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleA;
begin
 InternalTestDrawing(CPngSuiteDir + 'g03n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleB;
begin
 InternalTestDrawing(CPngSuiteDir + 'g04n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleC;
begin
 InternalTestDrawing(CPngSuiteDir + 'g05n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleD;
begin
 InternalTestDrawing(CPngSuiteDir + 'g07n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleE;
begin
 InternalTestDrawing(CPngSuiteDir + 'g10n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaGrayscaleF;
begin
 InternalTestDrawing(CPngSuiteDir + 'g25n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorA;
begin
 InternalTestDrawing(CPngSuiteDir + 'g03n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorB;
begin
 InternalTestDrawing(CPngSuiteDir + 'g04n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorC;
begin
 InternalTestDrawing(CPngSuiteDir + 'g05n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorD;
begin
 InternalTestDrawing(CPngSuiteDir + 'g07n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorE;
begin
 InternalTestDrawing(CPngSuiteDir + 'g10n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaColorF;
begin
 InternalTestDrawing(CPngSuiteDir + 'g25n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedA;
begin
 InternalTestDrawing(CPngSuiteDir + 'g03n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedB;
begin
 InternalTestDrawing(CPngSuiteDir + 'g04n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedC;
begin
 InternalTestDrawing(CPngSuiteDir + 'g05n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedD;
begin
 InternalTestDrawing(CPngSuiteDir + 'g07n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedE;
begin
 InternalTestDrawing(CPngSuiteDir + 'g10n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteGammaTest.TestDrawingGammaIndexedF;
begin
 InternalTestDrawing(CPngSuiteDir + 'g25n3p04.png');
end;


{ TTestPngGR32DrawingSuiteFilteringTest }

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingGrayscaleFilterType0;
begin
 InternalTestDrawing(CPngSuiteDir + 'f00n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingGrayscaleFilterType1;
begin
 InternalTestDrawing(CPngSuiteDir + 'f01n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingGrayscaleFilterType2;
begin
 InternalTestDrawing(CPngSuiteDir + 'f02n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingGrayscaleFilterType3;
begin
 InternalTestDrawing(CPngSuiteDir + 'f03n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingGrayscaleFilterType4;
begin
 InternalTestDrawing(CPngSuiteDir + 'f04n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingColorFilterType0;
begin
 InternalTestDrawing(CPngSuiteDir + 'f00n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingColorFilterType1;
begin
 InternalTestDrawing(CPngSuiteDir + 'f01n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingColorFilterType2;
begin
 InternalTestDrawing(CPngSuiteDir + 'f02n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingColorFilterType3;
begin
 InternalTestDrawing(CPngSuiteDir + 'f03n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteFilteringTest.TestDrawingColorFilterType4;
begin
 InternalTestDrawing(CPngSuiteDir + 'f04n2c08.png');
end;


{ TTestPngGR32DrawingSuiteAdditionalPaletteTest }

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingTrueColor;
begin
 InternalTestDrawing(CPngSuiteDir + 'pp0n2c16.png');
end;

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingTrueColorAlpha;
begin
 InternalTestDrawing(CPngSuiteDir + 'pp0n6a08.png');
end;

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingSuggestedPalette1bitGrayScale;
begin
 InternalTestDrawing(CPngSuiteDir + 'ps1n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingSuggestedPalette1bitTrueColor;
begin
 InternalTestDrawing(CPngSuiteDir + 'ps1n2c16.png');
end;

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingSuggestedPalette2bitGrayScale;
begin
 InternalTestDrawing(CPngSuiteDir + 'ps2n0g08.png');
end;

procedure TTestPngGR32DrawingSuiteAdditionalPaletteTest.TestDrawingSuggestedPalette2bitTrueColor;
begin
 InternalTestDrawing(CPngSuiteDir + 'ps2n2c16.png');
end;


{ TTestPngGR32DrawingSuiteAncillaryChunksTest }

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingTrueColor5bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs5n2c08.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.SignificantBitsChunk)
  then Fail(RCStrSignificantBitsChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.SignificantBitsChunk do
  begin
   if SignificantBits is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(SignificantBits) do
     begin
      Check(RedBits = 5);
      Check(GreenBits = 5);
      Check(BlueBits = 5);
     end
    else Fail(RCStrWrongSignificantBitsFormat);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingTrueColor8bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs8n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingTrueColor13bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs3n2c16.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.SignificantBitsChunk)
  then Fail(RCStrSignificantBitsChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.SignificantBitsChunk do
  begin
   if SignificantBits is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(SignificantBits) do
     begin
      Check(RedBits = 13);
      Check(GreenBits = 13);
      Check(BlueBits = 13);
     end
    else Fail(RCStrWrongSignificantBitsFormat);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingPalette3bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs3n3p08.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.SignificantBitsChunk)
  then Fail(RCStrSignificantBitsChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.SignificantBitsChunk do
  begin
   if SignificantBits is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(SignificantBits) do
     begin
      Check(RedBits = 3);
      Check(GreenBits = 3);
      Check(BlueBits = 3);
     end
    else Fail(RCStrWrongSignificantBitsFormat);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingPalette5bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs5n3p08.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.SignificantBitsChunk)
  then Fail(RCStrSignificantBitsChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.SignificantBitsChunk do
  begin
   if SignificantBits is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(SignificantBits) do
     begin
      Check(RedBits = 5);
      Check(GreenBits = 5);
      Check(BlueBits = 5);
     end
    else Fail(RCStrWrongSignificantBitsFormat);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestDrawingPalette8bits;
begin
 InternalTestDrawing(CPngSuiteDir + 'cs8n3p08.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestPhysicalDimensions8x32FlatPixels;
begin
 InternalTestDrawing(CPngSuiteDir + 'cdfn2c08.png');

 // check information stored in the chunk
 with FPortableNetworkGraphic do
  begin
   Check(PixelsPerUnitX = 1);
   Check(PixelsPerUnitY = 4);
   Check(PixelUnit = 0);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestPhysicalDimensions32x8HighPixels;
begin
 InternalTestDrawing(CPngSuiteDir + 'cdhn2c08.png');

 // check information stored in the chunk
 with FPortableNetworkGraphic do
  begin
   Check(PixelsPerUnitX = 4);
   Check(PixelsPerUnitY = 1);
   Check(PixelUnit = 0);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestPhysicalDimensions8x8SquarePixels;
begin
 InternalTestDrawing(CPngSuiteDir + 'cdsn2c08.png');

 // check information stored in the chunk
 with FPortableNetworkGraphic do
  begin
   Check(PixelsPerUnitX = 1);
   Check(PixelsPerUnitY = 1);
   Check(PixelUnit = 0);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestPhysicalDimensions1000PixelsPerMeter;
begin
 InternalTestDrawing(CPngSuiteDir + 'cdun2c08.png');

 // check information stored in the chunk
 with FPortableNetworkGraphic do
  begin
   Check(PixelsPerUnitX = 1000);
   Check(PixelsPerUnitY = 1000);
   Check(PixelUnit = 1);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestChromaChunkPalette;
begin
 InternalTestDrawing(CPngSuiteDir + 'ccwn3p08.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.PrimaryChromaticitiesChunk)
  then Fail(RCStrPhysicalPixelDimensionChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.PrimaryChromaticitiesChunk do
  begin
   CheckEquals(0.3127, WhiteXAsSingle, 1E-3);
   CheckEquals(0.3290, WhiteYAsSingle, 1E-3);
   CheckEquals(0.64, RedXAsSingle, 1E-3);
   CheckEquals(0.33, RedYAsSingle, 1E-3);
   CheckEquals(0.30, GreenXAsSingle, 1E-3);
   CheckEquals(0.60, GreenYAsSingle, 1E-3);
   CheckEquals(0.15, BlueXAsSingle, 1E-3);
   CheckEquals(0.06, BlueYAsSingle, 1E-3);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestChromaChunkTrueColor;
begin
 InternalTestDrawing(CPngSuiteDir + 'ccwn2c08.png');

 // test if chunk is present
 if not Assigned(FPortableNetworkGraphic.PrimaryChromaticitiesChunk)
  then Fail(RCStrPhysicalPixelDimensionChunkMissing);

 // check information stored in the chunk
 with FPortableNetworkGraphic.PrimaryChromaticitiesChunk do
  begin
   CheckEquals(0.3127, WhiteXAsSingle, 1E-3);
   CheckEquals(0.3290, WhiteYAsSingle, 1E-3);
   CheckEquals(0.64, RedXAsSingle, 1E-3);
   CheckEquals(0.33, RedYAsSingle, 1E-3);
   CheckEquals(0.30, GreenXAsSingle, 1E-3);
   CheckEquals(0.60, GreenYAsSingle, 1E-3);
   CheckEquals(0.15, BlueXAsSingle, 1E-3);
   CheckEquals(0.06, BlueYAsSingle, 1E-3);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestHistogramChunk15colors;
begin
 InternalTestDrawing(CPngSuiteDir + 'ch1n3p04.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestHistogramChunk256colors;
begin
 InternalTestDrawing(CPngSuiteDir + 'ch2n3p08.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestModificationTime;
begin
 with FPortableNetworkGraphic do
  begin
   LoadFromFile(CPngSuiteDir + 'cm7n0g04.png');
   CheckEquals(EncodeDate(1970, 1, 1) + EncodeTime(0, 0, 0, 0), ModifiedTime);

   LoadFromFile(CPngSuiteDir + 'cm9n0g04.png');
   CheckEquals(EncodeDate(1999, 12, 31) + EncodeTime(23, 59, 59, 0), ModifiedTime);

   LoadFromFile(CPngSuiteDir + 'cm0n0g04.png');
   CheckEquals(EncodeDate(2000, 1, 1) + EncodeTime(12, 34, 56, 0), ModifiedTime);
  end;
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestNoTextualData;
begin
 FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'ct0n0g04.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestWithCompressedTextualData;
begin
 FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'ctzn0g04.png');
end;

procedure TTestPngGR32DrawingSuiteAncillaryChunksTest.TestWithTextualData;
begin
 FPortableNetworkGraphic.LoadFromFile(CPngSuiteDir + 'ct1n0g04.png');
end;


{ TTestPngGR32DrawingSuiteChunkOrdering }

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingGrayscale1IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi1n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingGrayscale2IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi2n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingGrayscale4IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi4n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingGrayscaleAllIDATs;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi9n0g16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingTrueColor1IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi1n2c16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingTrueColor2IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi2n2c16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingTrueColor4IDAT;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi4n2c16.png');
end;

procedure TTestPngGR32DrawingSuiteChunkOrdering.TestDrawingTrueColorAllIDATs;
begin
 InternalTestDrawing(CPngSuiteDir + 'oi9n2c16.png');
end;


{ TTestPngGR32DrawingSuiteCompressionLevel }

procedure TTestPngGR32DrawingSuiteCompressionLevel.TestDrawingTrueColorCompressionLevel0;
begin
 InternalTestDrawing(CPngSuiteDir + 'z00n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteCompressionLevel.TestDrawingTrueColorCompressionLevel3;
begin
 InternalTestDrawing(CPngSuiteDir + 'z03n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteCompressionLevel.TestDrawingTrueColorCompressionLevel6;
begin
 InternalTestDrawing(CPngSuiteDir + 'z06n2c08.png');
end;

procedure TTestPngGR32DrawingSuiteCompressionLevel.TestDrawingTrueColorCompressionLevel9;
begin
 InternalTestDrawing(CPngSuiteDir + 'z09n2c08.png');
end;

initialization
  RegisterTest(TTestPngGR32File.Suite);
  RegisterTest(TTestPngGR32Drawing.Suite);

  TestPngSuite := TTestSuite.Create('PNG Suite Tests');
  with TestPngSuite do
   begin
    AddTest(TTestPngGR32DrawingSuiteBasicNonInterlaced.Suite);
    AddTest(TTestPngGR32DrawingSuiteBasicAdam7.Suite);
    AddTest(TTestPngGR32DrawingSuiteSizeTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteBackgroundTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteGammaTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteFilteringTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteAdditionalPaletteTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteAncillaryChunksTest.Suite);
    AddTest(TTestPngGR32DrawingSuiteChunkOrdering.Suite);
    AddTest(TTestPngGR32DrawingSuiteCompressionLevel.Suite);
   end;
  RegisterTest(TestPngSuite);

end.
