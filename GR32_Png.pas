unit GR32_PNG;

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

{$I GR32.inc}

uses
  Classes, Graphics, SysUtils, zlib, GR32, GR32_PortableNetworkGraphic;

type
  TPortableNetworkGraphic32 = class(TPortableNetworkGraphic)
  private
    FGammaTable        : array [Byte] of Byte;
    FInverseGammaTable : array [Byte] of Byte;
    FPaletteTable      : array [Byte] of TRGB24;

    procedure TransferFromNonInterlacedGrayscale1(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscale2(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscale4(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscale8(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscale16(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedTrueColor8(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedTrueColor16(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedPalette(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedPalette8(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscaleAlpha8(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedGrayscaleAlpha16(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedTrueColorAlpha8(Source: Pointer; Destination: PColor32);
    procedure TransferFromNonInterlacedTrueColorAlpha16(Source: Pointer; Destination: PColor32);

    procedure TransferFromAdam7Grayscale1(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Grayscale2(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Grayscale4(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Grayscale8(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Grayscale16(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7TrueColor8(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7TrueColor16(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Palette1(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Palette2(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Palette4(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7Palette8(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7GrayscaleAlpha8(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7GrayscaleAlpha16(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7TrueColorAlpha8(const Pass: Byte; Source: Pointer; Destination: PColor32);
    procedure TransferFromAdam7TrueColorAlpha16(const Pass: Byte; Source: Pointer; Destination: PColor32);

    procedure TransferToNonInterlacedGrayscale1(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedGrayscale2(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedGrayscale4(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedGrayscale8(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedTrueColor8(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedPalette1(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedPalette2(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedPalette4(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedPalette8(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedGrayscaleAlpha8(Source: PColor32; Destination: Pointer);
    procedure TransferToNonInterlacedTrueColorAlpha8(Source: PColor32; Destination: Pointer);

    procedure BuildGammaTable;
    procedure BuildPaletteTable;

    procedure DecodeNonInterlaced(Bitmap32: TCustomBitmap32); virtual;
    procedure DecodeInterlacedAdam7(Bitmap32: TCustomBitmap32); virtual;
    function ColorInPalette(Color: TColor32): Integer;
    procedure AssignPropertiesFromBitmap32(Bitmap32: TCustomBitmap32);
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawToBitmap32(Bitmap32: TCustomBitmap32); virtual;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

implementation

uses
  Math;

resourcestring
  RCStrUnsupportedFormat = 'Unsupported Format';
  RCStrDataIncomplete = 'Data not complete';

type
  TTransferFromNonInterlaced = procedure (Source: Pointer; Destination: PColor32) of object;
  TTransferFromAdam7 = procedure (const Pass: Byte; Source: Pointer; Destination: PColor32) of object;
  TransferToNonInterlaced = procedure (Source: PColor32; Destination: Pointer) of object;
  TPalette24 = array of TRGB24;

const
  CRowStart           : array [0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart        : array [0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement       : array [0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement    : array [0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  CGrayScaleTable1Bit : array [0..1] of Byte = (0, $FF);
  CGrayScaleTable2Bit : array [0..3] of Byte = (0, $55, $AA, $FF);
  CGrayScaleTable4Bit : array [0..15] of Byte = (0, $11, $22, $33, $44, $55,
    $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);


{ TPortableNetworkGraphic32 }

procedure TPortableNetworkGraphic32.DrawToBitmap32(Bitmap32: TCustomBitmap32);
begin
 // decode image data
 case ImageHeader.InterlaceMethod of
  imNone  : DecodeNonInterlaced(Bitmap32);
  imAdam7 : DecodeInterlacedAdam7(Bitmap32);
 end;
end;

function TPortableNetworkGraphic32.ColorInPalette(Color: TColor32): Integer;
var
  Color24 : TRGB24;
begin
 for Result := 0 to FPaletteChunk.Count - 1 do
  begin
   Color24 := PaletteChunk.PaletteEntry[Result];
   if (RedComponent(Color) = Color24.R) and
      (GreenComponent(Color) = Color24.G) and
      (BlueComponent(Color) = Color24.B)
    then Exit;
  end;
 Result := -1;
end;

function ColorIndexInPalette(Color: TColor32; Palette: TPalette24): Integer;
begin
 for Result := 0 to Length(Palette) - 1 do
  if (RedComponent(Color) = Palette[Result].R) and
     (GreenComponent(Color) = Palette[Result].G) and
     (BlueComponent(Color) = Palette[Result].B)
   then Exit;
 Result := -1;
end;

procedure TPortableNetworkGraphic32.AssignPropertiesFromBitmap32(
  Bitmap32: TCustomBitmap32);
var
  Index         : Integer;
  IsAlpha       : Boolean;
  IsGrayScale   : Boolean;
  IsPalette     : Boolean;
  Color         : TColor32;
  TempPalette   : TPalette24;
  TempAlpha     : Byte;
begin
 with Bitmap32 do
  begin
   // basic properties
   ImageHeader.Width := Width;
   ImageHeader.Height := Height;
   ImageHeader.CompressionMethod := 0;
   ImageHeader.InterlaceMethod := imNone;

   // initialize
   SetLength(TempPalette, 0);
   IsGrayScale := True;
   IsPalette := True;
   IsAlpha := False;
   TempAlpha := 0;

   // check every pixel in the bitmap for the use of the alpha channel,
   // whether the image is grayscale or whether the colors can be stored
   // as a palette (and build the palette at the same time
   for Index := 0 to Width * Height - 1 do
    begin
     Color := Bits[Index];

     // check whether the palette is empty
     if Length(TempPalette) = 0 then
      begin
       IsAlpha := AlphaComponent(Color) < 255 ;

       // eventually store first alpha component
       if IsAlpha
        then TempAlpha := AlphaComponent(Color);

       SetLength(TempPalette, 1);
       TempPalette[0].R := RedComponent(Color);
       TempPalette[0].G := GreenComponent(Color);
       TempPalette[0].B := BlueComponent(Color);
       IsGrayScale := (RedComponent(Color) = GreenComponent(Color)) and
         (BlueComponent(Color) = GreenComponent(Color));
      end
     else
      begin
       // check alpha channel
       if (AlphaComponent(Color) < 255) then
        begin
         if IsAlpha then
          if IsPalette and (TempAlpha <> AlphaComponent(Color))
           then IsPalette := False else
          else TempAlpha := AlphaComponent(Color);

         IsAlpha := True;
        end;
       if ColorIndexInPalette(Color, TempPalette) < 0 then
        begin
         if IsPalette then
          if (Length(TempPalette) < 256) then
           begin
            SetLength(TempPalette, Length(TempPalette) + 1);
            TempPalette[Length(TempPalette) - 1].R := RedComponent(Color);
            TempPalette[Length(TempPalette) - 1].G := GreenComponent(Color);
            TempPalette[Length(TempPalette) - 1].B := BlueComponent(Color);
            if IsGrayScale and not ((RedComponent(Color) = GreenComponent(Color)) and
              (BlueComponent(Color) = GreenComponent(Color)))
             then IsGrayScale := False;
           end
          else IsPalette := False
         else
          if not ((RedComponent(Color) = GreenComponent(Color)) and
            (BlueComponent(Color) = GreenComponent(Color)))
           then IsGrayScale := False;
        end;
      end;

     if IsAlpha and (not IsPalette) and (not IsGrayScale)
      then Break;
    end;

   // set image header
   with ImageHeader do
    if IsGrayScale then
     if IsAlpha  then
      begin
       ColorType := ctGrayscaleAlpha;
       BitDepth := 8;
      end
     else
      begin
       ColorType := ctIndexedColor; // ctGrayscale
       if Length(TempPalette) <= 2
        then BitDepth := 1 else
       if Length(TempPalette) <= 4
        then BitDepth := 2 else
       if Length(TempPalette) <= 16
        then BitDepth := 4
        else BitDepth := 8;
      end else
    if IsPalette then
     begin
      ColorType := ctIndexedColor;
      if Length(TempPalette) <= 2
       then BitDepth := 1 else
      if Length(TempPalette) <= 4
       then BitDepth := 2 else
      if Length(TempPalette) <= 16
       then BitDepth := 4
       else BitDepth := 8;
      end
     else
      if IsAlpha then
       begin
        ColorType := ctTrueColorAlpha;
        BitDepth := 8;
       end
      else
       begin
        ColorType := ctTrueColor;
        BitDepth := 8;
       end;

   // eventually prepare palette
   if ImageHeader.HasPalette then
    begin
     Assert(Length(TempPalette) <= 256);
     Move(TempPalette[0], FPaletteTable[0], Length(TempPalette) * SizeOf(TRGB24));

     if not Assigned(FPaletteChunk)
      then FPaletteChunk := TChunkPngPalette.Create(ImageHeader);

     FPaletteChunk.Count := Length(TempPalette);
     for Index := 0 to Length(TempPalette) - 1
      do FPaletteChunk.PaletteEntry[Index] := TempPalette[Index];
    end;

   for Index := 0 to 255 do
    begin
     FGammaTable[Index] := Index;
     FInverseGammaTable[Index] := Index;
    end;

   {$IFDEF StoreGamma}
   // add linear gamma chunk
   if not Assigned(FGammaChunk)
    then FGammaChunk := TChunkPngGamma.Create(ImageHeader);
   FGammaChunk.GammaAsSingle := 1;
   {$ELSE}
   // delete any gama correction table
   if Assigned(FGammaChunk)
    then FreeAndNil(FGammaChunk);
   {$ENDIF}
  end;
end;

procedure TPortableNetworkGraphic32.Assign(Source: TPersistent);
var
  Index         : Integer;
  DataStream    : TMemoryStream;
  TransferProc  : TransferToNonInterlaced;
  RowBuffer     : array [0..1] of PByteArray;
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  BytesPerRow   : Integer;
begin
 if Source is TCustomBitmap32 then
  with TCustomBitmap32(Source) do
   begin
    // Assign
    AssignPropertiesFromBitmap32(TCustomBitmap32(Source));

    // assign transfer procedure
    case ImageHeader.ColorType of
     ctGrayscale  :
      case ImageHeader.BitDepth of
       1 : TransferProc := TransferToNonInterlacedGrayscale1;
       2 : TransferProc := TransferToNonInterlacedGrayscale2;
       4 : TransferProc := TransferToNonInterlacedGrayscale4;
       8 : TransferProc := TransferToNonInterlacedGrayscale8;
       else raise EPngError.Create(RCStrUnsupportedFormat);
      end;
     ctTrueColor : TransferProc := TransferToNonInterlacedTrueColor8;
     ctIndexedColor :
      case ImageHeader.BitDepth of
       1 : TransferProc := TransferToNonInterlacedPalette1;
       2 : TransferProc := TransferToNonInterlacedPalette2;
       4 : TransferProc := TransferToNonInterlacedPalette4;
       8 : TransferProc := TransferToNonInterlacedPalette8;
       else raise EPngError.Create(RCStrUnsupportedFormat);
      end;
     ctGrayscaleAlpha : TransferProc := TransferToNonInterlacedGrayscaleAlpha8;
     ctTrueColorAlpha : TransferProc := TransferToNonInterlacedTrueColorAlpha8;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;

    DataStream := TMemoryStream.Create;
    try
     // initialize variables
     CurrentRow := 0;
     BytesPerRow := ImageHeader.BytesPerRow;
     RowByteSize := BytesPerRow + 1;

     try
      GetMem(RowBuffer[0], RowByteSize);
      GetMem(RowBuffer[1], RowByteSize);
      FillChar(RowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

      for Index := 0 to ImageHeader.Height - 1 do
       begin
        // set filter method to none
        RowBuffer[CurrentRow]^[0] := 0;

        // transfer data from image to current row
        TransferProc(PColor32(ScanLine[Index]), @RowBuffer[CurrentRow][1]);

        // write data to data stream
        DataStream.Write(RowBuffer[CurrentRow][0], RowByteSize);

        // flip current row used
        CurrentRow := 1 - CurrentRow;
       end;
     finally
      if Assigned(RowBuffer[0]) then Dispose(RowBuffer[0]);
      if Assigned(RowBuffer[1]) then Dispose(RowBuffer[1]);
     end;

     // reset data stream position
     DataStream.Seek(0, soFromBeginning);

     // compress image data from data stream
     CompressImageDataFromStream(DataStream);
    finally
     FreeAndNil(DataStream);
    end;
   end
 else inherited;
end;

procedure TPortableNetworkGraphic32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBitmap32 then
  begin
   TCustomBitmap32(Dest).Width := ImageHeader.Width;
   TCustomBitmap32(Dest).Height := ImageHeader.Height;
   DrawToBitmap32(TCustomBitmap32(Dest));
  end
 else inherited;
end;

procedure TPortableNetworkGraphic32.BuildGammaTable;
var
  Index        : Integer;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
 if Assigned(GammaChunk) and (GammaChunk.Gamma <> 0) then
  begin
   PreCalcGamma := (GammaChunk.Gamma * 2.2E-5);
   for Index := 0 to 255 do
    begin
     FGammaTable[Index] := Round(Power((Index * COne255th), 1 / PreCalcGamma) * 255);
     FInverseGammaTable[Round(Power((Index * COne255th), 1 / PreCalcGamma) * 255)] := Index;
    end;
  end else
 for Index := 0 to 255 do
  begin
   FGammaTable[Index] := Index;
   FInverseGammaTable[Index] := Index;
  end;
end;

procedure TPortableNetworkGraphic32.BuildPaletteTable;
var
  Index   : Integer;
  FracVal : Single;
  Value   : Byte;
  MaxByte : Byte;
begin
 if ImageHeader.HasPalette then
  if Assigned(PaletteChunk) then
   for Index := 0 to PaletteChunk.Count - 1
    do FPaletteTable[Index] := PaletteChunk.PaletteEntry[Index]
  else
   begin
    // create gray scale palette
    MaxByte := ((1 shl ImageHeader.BitDepth) - 1) and $FF;
    FracVal := 1 / MaxByte;
    for Index := 0 to MaxByte do
     begin
      Value := Round(255 * (Index * FracVal));
      FPaletteTable[Index].R := Value;
      FPaletteTable[Index].G := Value;
      FPaletteTable[Index].B := Value;
     end;
   end;
end;

procedure TPortableNetworkGraphic32.LoadFromStream(Stream: TStream);
begin
 inherited;

 // build gamma table
 BuildGammaTable;

 // build palette table
 BuildPaletteTable;
end;

procedure TPortableNetworkGraphic32.SaveToStream(Stream: TStream);
begin
 inherited;
 // nothing to do here yet
end;

procedure TPortableNetworkGraphic32.DecodeNonInterlaced(Bitmap32: TCustomBitmap32);
var
  Index         : Integer;
  RowBuffer     : array [0..1] of PByteArray;
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  BytesPerRow   : Integer;
  PixelByteSize : Integer;
  TransferProc  : TTransferFromNonInterlaced;
  DataStream    : TMemoryStream;

begin
 DataStream := TMemoryStream.Create;
 try
  // decompress image data to data stream
  DecompressImageDataToStream(DataStream);

  // reset data stream position
  DataStream.Seek(0, soFromBeginning);

  // assign transfer procedure
  case ImageHeader.ColorType of
   ctGrayscale  :
    case ImageHeader.BitDepth of
     1  : TransferProc := TransferFromNonInterlacedGrayscale1;
     2  : TransferProc := TransferFromNonInterlacedGrayscale2;
     4  : TransferProc := TransferFromNonInterlacedGrayscale4;
     8  : TransferProc := TransferFromNonInterlacedGrayscale8;
     16 : TransferProc := TransferFromNonInterlacedGrayscale16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctTrueColor :
    case ImageHeader.BitDepth of
      8 : TransferProc := TransferFromNonInterlacedTrueColor8;
     16 : TransferProc := TransferFromNonInterlacedTrueColor16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctIndexedColor :
    case ImageHeader.BitDepth of
     1, 2, 4 : TransferProc := TransferFromNonInterlacedPalette;
     8       : TransferProc := TransferFromNonInterlacedPalette8;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctGrayscaleAlpha :
    case ImageHeader.BitDepth of
      8  : TransferProc := TransferFromNonInterlacedGrayscaleAlpha8;
     16  : TransferProc := TransferFromNonInterlacedGrayscaleAlpha16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctTrueColorAlpha :
    case ImageHeader.BitDepth of
      8  : TransferProc := TransferFromNonInterlacedTrueColorAlpha8;
     16  : TransferProc := TransferFromNonInterlacedTrueColorAlpha16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   else raise EPngError.Create(RCStrUnsupportedFormat);
  end;

  // initialize variables
  CurrentRow := 0;
  BytesPerRow := ImageHeader.BytesPerRow;
  RowByteSize := BytesPerRow + 1;
  PixelByteSize := ImageHeader.PixelByteSize;

  try
   GetMem(RowBuffer[0], RowByteSize);
   GetMem(RowBuffer[1], RowByteSize);
   FillChar(RowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

   for Index := 0 to ImageHeader.Height - 1 do
    begin
     // read data from stream
     if DataStream.Read(RowBuffer[CurrentRow][0], RowByteSize) <> RowByteSize
      then raise EPngError.Create(RCStrDataIncomplete);

     // filter current row
     FilterRow(TAdaptiveFilterMethod(RowBuffer[CurrentRow]^[0]), RowBuffer[CurrentRow], RowBuffer[1 - CurrentRow], BytesPerRow, PixelByteSize);

     // transfer data from row to image
     TransferProc(@RowBuffer[CurrentRow][1], PColor32(Bitmap32.ScanLine[Index]));

     // flip current row
     CurrentRow := 1 - CurrentRow;
    end;
  finally
   if Assigned(RowBuffer[0]) then Dispose(RowBuffer[0]);
   if Assigned(RowBuffer[1]) then Dispose(RowBuffer[1]);
  end;

 finally
  FreeAndNil(DataStream);
 end;
end;

procedure TPortableNetworkGraphic32.DecodeInterlacedAdam7(Bitmap32: TCustomBitmap32);
var
  DataStream    : TMemoryStream;
  RowBuffer     : array [0..1] of PByteArray;
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  PassRow       : Integer;
  TransferProc  : TTransferFromAdam7;
begin
 DataStream := TMemoryStream.Create;
 try
  // decompressed image data to data stream
  DecompressImageDataToStream(DataStream);

  // reset data stream position
  DataStream.Seek(0, soFromBeginning);

  // assign transfer procedure
  case ImageHeader.ColorType of
   ctGrayscale  :
    case ImageHeader.BitDepth of
     1  : TransferProc := TransferFromAdam7Grayscale1;
     2  : TransferProc := TransferFromAdam7Grayscale2;
     4  : TransferProc := TransferFromAdam7Grayscale4;
     8  : TransferProc := TransferFromAdam7Grayscale8;
     16 : TransferProc := TransferFromAdam7Grayscale16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctTrueColor :
    case ImageHeader.BitDepth of
      8 : TransferProc := TransferFromAdam7TrueColor8;
     16 : TransferProc := TransferFromAdam7TrueColor16;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctIndexedColor :
    case ImageHeader.BitDepth of
     1 : TransferProc := TransferFromAdam7Palette1;
     2 : TransferProc := TransferFromAdam7Palette2;
     4 : TransferProc := TransferFromAdam7Palette4;
     8 : TransferProc := TransferFromAdam7Palette8;
     else raise EPngError.Create(RCStrUnsupportedFormat);
    end;
   ctGrayscaleAlpha :
     case ImageHeader.BitDepth of
       8  : TransferProc := TransferFromAdam7GrayscaleAlpha8;
      16  : TransferProc := TransferFromAdam7GrayscaleAlpha16;
      else raise EPngError.Create(RCStrUnsupportedFormat);
     end;
   ctTrueColorAlpha :
     case ImageHeader.BitDepth of
       8  : TransferProc := TransferFromAdam7TrueColorAlpha8;
      16  : TransferProc := TransferFromAdam7TrueColorAlpha16;
      else raise EPngError.Create(RCStrUnsupportedFormat);
     end;
   else raise EPngError.Create(RCStrUnsupportedFormat);
  end;

  // initialize variables
  CurrentRow := 0;
  PixelByteSize := ImageHeader.PixelByteSize;

  try
   // allocate row buffer memory
   GetMem(RowBuffer[0], ImageHeader.BytesPerRow + 1);
   GetMem(RowBuffer[1], ImageHeader.BytesPerRow + 1);

   // The Adam7 interlacer uses 7 passes to create the complete image
   for CurrentPass := 0 to 6 do
    begin
     // calculate some intermediate variables
     PixelPerRow := (ImageHeader.Width - CColumnStart[CurrentPass] + CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

     with ImageHeader do
      case ColorType of
       ctGrayscale, ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
       ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
       ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
       ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
       else RowByteSize := 0;
      end;

     PassRow := CRowStart[CurrentPass];

     // clear previous row
     FillChar(RowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

     // check whether there are any bytes to process in this pass.
     if RowByteSize > 0 then
      while PassRow < ImageHeader.Height do
       begin
        // get interlaced row data
        if DataStream.Read(RowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1)
         then raise EPngError.Create(RCStrDataIncomplete);

        FilterRow(TAdaptiveFilterMethod(RowBuffer[CurrentRow]^[0]), RowBuffer[CurrentRow], RowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

        // transfer and deinterlace image data
        TransferProc(CurrentPass, @RowBuffer[CurrentRow][1], PColor32(Bitmap32.ScanLine[PassRow]));

        // prepare for the next pass
        Inc(PassRow, CRowIncrement[CurrentPass]);
        CurrentRow := 1 - CurrentRow;
       end;
    end;
  finally
   if Assigned(RowBuffer[0]) then Dispose(RowBuffer[0]);
   if Assigned(RowBuffer[1]) then Dispose(RowBuffer[1]);
  end;
 finally
  FreeAndNil(DataStream);
 end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscale1(Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscale2(Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable2Bit[(Src^ shr BitIndex) and $3]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscale4(Source: Pointer;
  Destination: PColor32);
var
  Index      : Integer;
  Src        : PByte absolute Source;
  BitIndex   : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable4Bit[(Src^ shr BitIndex) and $F]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscale8(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PByte absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^]; Inc(Src);
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscale16(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PWord absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^ and $FF]; Inc(Src);
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedPalette(Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Color    : TRGB24;
  BitIndex : Byte;
  BitMask  : Byte;
  BitDepth : Byte;
begin
 BitIndex := 8;
 BitDepth := ImageHeader.BitDepth;
 BitMask  := (1 shl BitDepth) - 1;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, BitDepth);
   Color := FPaletteTable[(Src^ shr BitIndex) and BitMask];
   PColor32Entry(Destination)^.R := FGammaTable[Color.R];
   PColor32Entry(Destination)^.G := FGammaTable[Color.G];
   PColor32Entry(Destination)^.B := FGammaTable[Color.B];
   PColor32Entry(Destination)^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedPalette8(Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PByte absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[FPaletteTable[Src^].R];
   PColor32Entry(Destination)^.G := FGammaTable[FPaletteTable[Src^].G];
   PColor32Entry(Destination)^.B := FGammaTable[FPaletteTable[Src^].B];
   PColor32Entry(Destination)^.A := 255;
   Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedTrueColor8(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^.R];
   PColor32Entry(Destination)^.G := FGammaTable[Src^.G];
   PColor32Entry(Destination)^.B := FGammaTable[Src^.B];
   PColor32Entry(Destination)^.A := 255;
   Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedTrueColor16(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^.R and $FF];
   PColor32Entry(Destination)^.G := FGammaTable[Src^.G and $FF];
   PColor32Entry(Destination)^.B := FGammaTable[Src^.B and $FF];
   PColor32Entry(Destination)^.A := 255;
   Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscaleAlpha8(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PByte absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^]; Inc(Src);
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := Src^; Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedGrayscaleAlpha16(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PWord absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^ and $FF]; Inc(Src);
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := Src^ and $FF; Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedTrueColorAlpha8(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB32 absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^.R];
   PColor32Entry(Destination)^.G := FGammaTable[Src^.G];
   PColor32Entry(Destination)^.B := FGammaTable[Src^.B];
   PColor32Entry(Destination)^.A := Src^.A;
   Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromNonInterlacedTrueColorAlpha16(Source: Pointer; Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB32Word absolute Source;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   PColor32Entry(Destination)^.R := FGammaTable[Src^.R and $FF];
   PColor32Entry(Destination)^.G := FGammaTable[Src^.G and $FF];
   PColor32Entry(Destination)^.B := FGammaTable[Src^.B and $FF];
   PColor32Entry(Destination)^.A := Src^.A and $FF;
   Inc(Src);
   Inc(Destination);
  end;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Grayscale1(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Grayscale2(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex, 2);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable2Bit[((Src^ shr BitIndex) and $3)]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Grayscale4(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex, 4);
   PColor32Entry(Destination)^.R := FGammaTable[CGrayScaleTable4Bit[((Src^ shr BitIndex) and $F)]];
   PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
   PColor32Entry(Destination)^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Grayscale8(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^]; Inc(Src);
  PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.A := 255;

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Grayscale16(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  Src      : PWord absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^ and $FF]; Inc(Src);
  PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.A := 255;

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7TrueColor8(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^.R];
  PColor32Entry(Destination)^.G := FGammaTable[Src^.G];
  PColor32Entry(Destination)^.B := FGammaTable[Src^.B];
  PColor32Entry(Destination)^.A := 255;

  Inc(Src);
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7TrueColor16(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^.R and $FF];
  PColor32Entry(Destination)^.G := FGammaTable[Src^.G and $FF];
  PColor32Entry(Destination)^.B := FGammaTable[Src^.B and $FF];
  PColor32Entry(Destination)^.A := 255;

  Inc(Src);
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Palette1(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Color    : TRGB24;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
  Dec(BitIndex);
  Color := FPaletteTable[(Src^ shr BitIndex) and $1];
  PColor32Entry(Destination)^.R := FGammaTable[Color.R];
  PColor32Entry(Destination)^.G := FGammaTable[Color.G];
  PColor32Entry(Destination)^.B := FGammaTable[Color.B];
  PColor32Entry(Destination)^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Palette2(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Color    : TRGB24;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
  Dec(BitIndex, 2);
  Color := FPaletteTable[(Src^ shr BitIndex) and $3];
  PColor32Entry(Destination)^.R := FGammaTable[Color.R];
  PColor32Entry(Destination)^.G := FGammaTable[Color.G];
  PColor32Entry(Destination)^.B := FGammaTable[Color.B];
  PColor32Entry(Destination)^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Palette4(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Color    : TRGB24;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 BitIndex := 8;
 repeat
  Dec(BitIndex, 4);
  Color := FPaletteTable[(Src^ shr BitIndex) and $F];
  PColor32Entry(Destination)^.R := FGammaTable[Color.R];
  PColor32Entry(Destination)^.G := FGammaTable[Color.G];
  PColor32Entry(Destination)^.B := FGammaTable[Color.B];
  PColor32Entry(Destination)^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7Palette8(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  Src      : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[FPaletteTable[Src^].R];
  PColor32Entry(Destination)^.G := FGammaTable[FPaletteTable[Src^].G];
  PColor32Entry(Destination)^.B := FGammaTable[FPaletteTable[Src^].B];
  PColor32Entry(Destination)^.A := 255;

  Inc(Src);
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7GrayscaleAlpha8(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PByte absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^]; Inc(Src);
  PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.A := Src^; Inc(Src);

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7GrayscaleAlpha16(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index : Integer;
  Src   : PWord absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[Src^ and $FF]; Inc(Src);
  PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
  PColor32Entry(Destination)^.A := Src^ and $FF; Inc(Src);

  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7TrueColorAlpha8(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  SrcPtr   : PRGB32 absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[SrcPtr^.R];
  PColor32Entry(Destination)^.G := FGammaTable[SrcPtr^.G];
  PColor32Entry(Destination)^.B := FGammaTable[SrcPtr^.B];
  PColor32Entry(Destination)^.A := SrcPtr^.A;

  Inc(SrcPtr);
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;

procedure TPortableNetworkGraphic32.TransferFromAdam7TrueColorAlpha16(const Pass: Byte; Source: Pointer;
  Destination: PColor32);
var
  Index    : Integer;
  SrcPtr   : PRGB32Word absolute Source;
begin
 Index := CColumnStart[Pass];
 Inc(Destination, Index);
 repeat
  PColor32Entry(Destination)^.R := FGammaTable[SrcPtr^.R and $FF];
  PColor32Entry(Destination)^.G := FGammaTable[SrcPtr^.G and $FF];
  PColor32Entry(Destination)^.B := FGammaTable[SrcPtr^.B and $FF];
  PColor32Entry(Destination)^.A := SrcPtr^.A and $FF;

  Inc(SrcPtr);
  Inc(Destination, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= ImageHeader.Width;
end;


procedure TPortableNetworkGraphic32.TransferToNonInterlacedGrayscale1(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^ := (Dest^ and not ($1 shl BitIndex)) or
     (((PColor32Entry(Source)^.R shr 7) and $1) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedGrayscale2(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^ := (Dest^ and not ($3 shl BitIndex)) or
     (((PColor32Entry(Source)^.R shr 6) and $3) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedGrayscale4(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^ := (Dest^ and not ($F shl BitIndex)) or
     (((PColor32Entry(Source)^.R shr 4) and $F) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedGrayscale8(
  Source: PColor32; Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dest^ := PColor32Entry(Source)^.R;
   Inc(Source);
   Inc(Dest);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedPalette1(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^ := (Dest^ and not ($1 shl BitIndex)) or
     ((ColorInPalette(Source^) and $1) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedPalette2(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^ := (Dest^ and not ($3 shl BitIndex)) or
     ((ColorInPalette(Source^) and $3) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedPalette4(
  Source: PColor32; Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^ := (Dest^ and not ($F shl BitIndex)) or
     ((ColorInPalette(Source^) and $F) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedPalette8(
  Source: PColor32; Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dest^ := ColorInPalette(Source^);
   Inc(Source);
   Inc(Dest);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedTrueColor8(
  Source: PColor32; Destination: Pointer);
var
  Index : Integer;
  Dest  : PRGB24 absolute Destination;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dest^.R := PColor32Entry(Source)^.R;
   Dest^.G := PColor32Entry(Source)^.G ;
   Dest^.B := PColor32Entry(Source)^.B;
   Inc(Source);
   Inc(Dest);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedGrayscaleAlpha8(
  Source: PColor32; Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dest^ := PColor32Entry(Source)^.R; Inc(Dest);
   Dest^ := PColor32Entry(Source)^.A; Inc(Dest);
   Inc(Source);
  end;
end;

procedure TPortableNetworkGraphic32.TransferToNonInterlacedTrueColorAlpha8(
  Source: PColor32; Destination: Pointer);
var
  Index : Integer;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to ImageHeader.Width - 1 do
  begin
   Dest^.R := PColor32Entry(Source)^.R;
   Dest^.G := PColor32Entry(Source)^.G;
   Dest^.B := PColor32Entry(Source)^.B;
   Dest^.A := PColor32Entry(Source)^.A;
   Inc(Dest);
   Inc(Source);
  end;
end;

end.
