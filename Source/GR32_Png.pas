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
{$I GR32_PngCompilerSwitches.inc}

uses
  Classes, Graphics, SysUtils, GR32, GR32_PortableNetworkGraphic;

type
  TProgressEvent = procedure(Sender: TObject; Percent: Single) of object;

  TPortableNetworkGraphic32 = class(TPortableNetworkGraphic)
  private
    FProgressEvent: TProgressEvent;
    procedure AssignPropertiesFromBitmap32(Bitmap32: TCustomBitmap32);
    function GetBackgroundColor: TColor32;
  protected
    function GR32Scanline(Bitmap: TObject; Y: Integer): Pointer; virtual;
    function GR32ScanlineProgress(Bitmap: TObject; Y: Integer): Pointer; virtual;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    procedure MakeIndexColored(MaxColorCount: Integer);

    function IsPremultiplied: Boolean;
    procedure DrawToBitmap32(Bitmap32: TCustomBitmap32); virtual;

    property Background: TColor32 read GetBackgroundColor;
    property Progress: TProgressEvent read FProgressEvent write FProgressEvent;
  end;

function IsValidPNG(Stream: TStream): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsValidPNG(const Filename: string): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure LoadBitmap32FromPNG(Bitmap: TBitmap32; const Filename: string); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure LoadBitmap32FromPNG(Bitmap: TBitmap32; Stream: TStream); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; FileName: string); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; Stream: TStream); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

implementation

uses
  Math;

resourcestring
  RCStrUnsupportedFormat = 'Unsupported Format';
  RCStrDataIncomplete = 'Data not complete';

type
  TCustomPngNonInterlacedDecoder = class(TCustomPngDecoder)
  protected
    FBytesPerRow: Integer;
    FRowByteSize: Integer;
    procedure TransferData(Source: Pointer; Destination: PColor32); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;

    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngNonInterlacedGrayscale1bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscale2bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscale4bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscale8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscale16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedTrueColor8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedTrueColor16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedPaletteDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedPalette8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscaleAlpha8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedGrayscaleAlpha16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedTrueColorAlpha8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TPngNonInterlacedTrueColorAlpha16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PColor32); override;
  end;

  TCustomPngAdam7Decoder = class(TCustomPngDecoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngAdam7Grayscale1bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Grayscale2bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Grayscale4bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Grayscale8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Grayscale16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7TrueColor8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7TrueColor16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Palette1bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Palette2bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Palette4bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7Palette8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7GrayscaleAlpha8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7GrayscaleAlpha16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7TrueColorAlpha8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TPngAdam7TrueColorAlpha16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PColor32); override;
  end;

  TCustomPngNonInterlacedEncoder = class(TCustomPngEncoder)
  protected
    FBytesPerRow: Integer;
    FRowByteSize: Integer;
    function ColorInPalette(Color: TColor32): Integer; virtual;
    procedure TransferData(Source: PColor32; Destination: Pointer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngNonInterlacedGrayscale1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedTrueColor8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscaleAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPngNonInterlacedTrueColorAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PColor32; Destination: Pointer); override;
  end;

  TPalette24 = array of TRGB24;

  TPngHistogramEntry = class
  private
    FColor: TColor32;
    FCount: Integer;
  public
    constructor Create(Key: TColor32);
    procedure Advance;

    property Count: Integer read FCount write FCount;
    property Color: TColor32 read FColor;
  end;

  TPngPalette = class
  private
    FItems: array of TColor32;
    FCount: Integer;
    procedure Remove(Index: Integer);
  protected
    function GetItem(index: Integer): TColor32;
    function Find(const item: TColor32; var index: Integer): Boolean;
    function Compare(const item1, item2: TColor32): Integer;
    procedure InsertItem(index: Integer; const anItem: TColor32);
  public
    function Add(const Item: TColor32): Integer; overload;
    function IndexOf(const Value: TColor32): Integer;
    procedure GetNearest(var Value: TColor32);

    procedure Clear;
    procedure LimitTo(Count: Integer);

    property Items[index: Integer]: TColor32 read GetItem; default;
    property Count: Integer read FCount;
  end;

  TPngHistogram = class
  private
    FItems: array of TPngHistogramEntry;
    FCount: Integer;
    procedure Remove(Index: Integer);
  protected
    function GetItem(index: Integer): TPngHistogramEntry;
    function Find(const item: TColor32; var index: Integer): Boolean;
    function Compare(const item1, item2: TColor32): Integer;
    procedure InsertItem(index: Integer; const anItem: TPngHistogramEntry);
  public
    function Add(const anItem: TPngHistogramEntry): Integer; overload;
    function IndexOf(const Value: TColor32): Integer;

    function Add(Value: TColor32): Integer; overload;
    procedure Advance(Value: TColor32); overload;

    procedure Clear;
    procedure Clean;
    function GetPalette(MaxColors: Integer = 256): TPngPalette;

    property Items[index: Integer]: TPngHistogramEntry read GetItem; default;
    property Count: Integer read FCount;
  end;

function IsValidPNG(Stream: TStream): Boolean;
begin
  Result := TPortableNetworkGraphic32.CanLoad(Stream);
end;

function IsValidPNG(const Filename: string): Boolean;
begin
  Result := TPortableNetworkGraphic32.CanLoad(Filename);
end;


procedure LoadBitmap32FromPNG(Bitmap: TBitmap32; const Filename: string);
begin
  with TPortableNetworkGraphic32.Create do
  try
    LoadFromFile(Filename);
    AssignTo(Bitmap);
  finally
    Free;
  end;
end;

procedure LoadBitmap32FromPNG(Bitmap: TBitmap32; Stream: TStream);
begin
  with TPortableNetworkGraphic32.Create do
  try
    LoadFromStream(Stream);
    AssignTo(Bitmap);
  finally
    Free;
  end;
end;

procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; FileName: string);
begin
  with TPortableNetworkGraphic32.Create do
  try
    Assign(Bitmap);
    SaveToFile(Filename);
  finally
    Free;
  end;
end;

procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; Stream: TStream);
begin
  with TPortableNetworkGraphic32.Create do
  try
    Assign(Bitmap);
    SaveToStream(Stream);
  finally
    Free;
  end;
end;


{ TPortableNetworkGraphic32 }

function TPortableNetworkGraphic32.GetBackgroundColor: TColor32;
var
  ResultColor32: TColor32Entry absolute Result;
begin
  if Assigned(FBackgroundChunk) then
  begin
    if FBackgroundChunk.Background is TPngBackgroundColorFormat04 then
      with TPngBackgroundColorFormat04(FBackgroundChunk.Background) do
      begin
        ResultColor32.R := GraySampleValue;
        ResultColor32.G := GraySampleValue;
        ResultColor32.B := GraySampleValue;
        ResultColor32.A := $FF;
      end
    else
    if FBackgroundChunk.Background is TPngBackgroundColorFormat26 then
      with TPngBackgroundColorFormat26(FBackgroundChunk.Background) do
      begin
        ResultColor32.R := RedSampleValue;
        ResultColor32.G := GreenSampleValue;
        ResultColor32.B := BlueSampleValue;
        ResultColor32.A := $FF;
      end;
    if FBackgroundChunk.Background is TPngBackgroundColorFormat3 then
      with TPngBackgroundColorFormat3(FBackgroundChunk.Background) do
      begin
        ResultColor32.R := PaletteEntry[PaletteIndex].R;
        ResultColor32.G := PaletteEntry[PaletteIndex].R;
        ResultColor32.B := PaletteEntry[PaletteIndex].R;
        ResultColor32.A := $FF;
      end;
 end
 else
   Result := $0;
end;

function TPortableNetworkGraphic32.GR32Scanline(Bitmap: TObject; Y: Integer): Pointer;
begin
  if Bitmap is TCustomBitmap32 then
    Result := TCustomBitmap32(Bitmap).ScanLine[Y]
  else
    Result := nil;
end;

function TPortableNetworkGraphic32.GR32ScanlineProgress(Bitmap: TObject;
  Y: Integer): Pointer;
begin
  Result := GR32Scanline(Bitmap, Y);
  if FImageHeader.Height > 0 then
    FProgressEvent(Self, 100 * Y / FImageHeader.Height)
  else
    FProgressEvent(Self, 100);
end;

function TPortableNetworkGraphic32.IsPremultiplied: Boolean;
var
  TempBitmap: TBitmap32;
  Pointer: PColor32EntryArray;
  Value: TColor32Entry;
  Index: Integer;
begin
  // this code checks whether the bitmap is *NOT* premultiplied
  // unfortunately this is just a weak check and might fail

  Result := True;
  TempBitmap := TBitmap32.Create;
  try
    AssignTo(TempBitmap);
    Pointer := PColor32EntryArray(TempBitmap.Bits);
    for Index := 0 to TempBitmap.Width * TempBitmap.Height - 1 do
    begin
      Value := Pointer^[Index];
      if (Value.R > Value.A) or (Value.G > Value.A) or (Value.B > Value.A) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    TempBitmap.Free;
  end;
end;

procedure TPortableNetworkGraphic32.MakeIndexColored(MaxColorCount: Integer);
var
  Index: Integer;
  Histogram: TPngHistogram;
  Palette: TPngPalette;
  Bitmap: TBitmap32;
  Bits: PColor32;
  PixelCount: Integer;
  InvPixelCount: TFloat;
  RGB24: TRGB24;
  EncoderClass: TCustomPngEncoderClass;
  DataStream: TMemoryStream;
begin
  if ColorType in [ctIndexedColor, ctGrayscale, ctGrayscaleAlpha] then
    raise Exception.Create('Color type not suitable');

  Bitmap := TBitmap32.Create;
  try
    AssignTo(Bitmap);

    Histogram := TPngHistogram.Create;
    try
      Bits := PColor32(Bitmap.Bits);
      PixelCount := Bitmap.Height * Bitmap.Width;
      InvPixelCount := 100 / (PixelCount - 1);
      if Assigned(Progress) then
        for Index := 0 to PixelCount - 1 do
        begin
          Histogram.Advance(Bits^);
          Inc(Bits);
          Progress(Self, 0.5 * Index * InvPixelCount);
        end
      else
        for Index := 0 to PixelCount - 1 do
        begin
          Histogram.Advance(Bits^);
          Inc(Bits);
        end;

      Palette := Histogram.GetPalette(MaxColorCount);
    finally
      Histogram.Free;
    end;

    Bits := PColor32(Bitmap.Bits);
    if Assigned(Progress) then
      for Index := 0 to PixelCount - 1 do
      begin
        Palette.GetNearest(Bits^);
        Inc(Bits);

        Progress(Self, 50 + 0.5 * Index * InvPixelCount);
      end
    else
      for Index := 0 to PixelCount - 1 do
      begin
        Palette.GetNearest(Bits^);
        Inc(Bits);
      end;

    // basic properties
    ImageHeader.Width := Width;
    ImageHeader.Height := Height;
    ImageHeader.CompressionMethod := 0;
    ImageHeader.InterlaceMethod := imNone;

    ImageHeader.ColorType := ctIndexedColor;
    if Palette.Count <= 2 then
      ImageHeader.BitDepth := 1
    else
    if Palette.Count <= 4 then
      ImageHeader.BitDepth := 2
    else
    if Palette.Count <= 16 then
      ImageHeader.BitDepth := 4
    else
      ImageHeader.BitDepth := 8;

    if not Assigned(FPaletteChunk) then
      FPaletteChunk := TChunkPngPalette.Create(ImageHeader);

    FPaletteChunk.Count := Palette.Count;
    for Index := 0 to Palette.Count - 1 do
    begin
      RGB24.R := TColor32Entry(Palette.Items[Index]).R;
      RGB24.G := TColor32Entry(Palette.Items[Index]).G;
      RGB24.B := TColor32Entry(Palette.Items[Index]).B;
      FPaletteChunk.PaletteEntry[Index] := RGB24;
    end;

    {$IFDEF StoreGamma}
    // add linear gamma chunk
    if not Assigned(FGammaChunk) then
      FGammaChunk := TChunkPngGamma.Create(ImageHeader);
    FGammaChunk.GammaAsSingle := 1;
    {$ELSE}
    // delete any gama correction table
    if Assigned(FGammaChunk) then
      FreeAndNil(FGammaChunk);
    {$ENDIF}

    case ImageHeader.ColorType of
      ctIndexedColor:
        case ImageHeader.BitDepth of
          1 :
            EncoderClass := TPngNonInterlacedPalette1bitEncoder;
          2 :
            EncoderClass := TPngNonInterlacedPalette2bitEncoder;
          4 :
            EncoderClass := TPngNonInterlacedPalette4bitEncoder;
          8 :
            EncoderClass := TPngNonInterlacedPalette8bitEncoder;
         else
           raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      else
        raise EPngError.Create(RCStrUnsupportedFormat);
    end;

    DataStream := TMemoryStream.Create;
    with DataStream do
      try
        with EncoderClass.Create(DataStream, FImageHeader, FGammaChunk, FPaletteChunk) do
          try
            if Assigned(FProgressEvent) then
              EncodeFromScanline(Bitmap, GR32ScanlineProgress)
            else
              EncodeFromScanline(Bitmap, GR32Scanline);
          finally
            Free;
          end;

        // reset data stream position
        DataStream.Seek(0, soFromBeginning);

        // compress image data from data stream
        CompressImageDataFromStream(DataStream);
      finally
        FreeAndNil(DataStream);
      end;
  finally
    Bitmap.Free;
  end;
end;

procedure TPortableNetworkGraphic32.DrawToBitmap32(Bitmap32: TCustomBitmap32);
var
  DecoderClass: TCustomPngDecoderClass;
  DataStream: TMemoryStream;
  Transparency: TCustomPngTransparency;
begin
  DataStream := TMemoryStream.Create;
  try
    // decompress image data to data stream
    DecompressImageDataToStream(DataStream);

    // reset data stream position
    DataStream.Seek(0, soFromBeginning);

    case ImageHeader.InterlaceMethod of
      imNone:
        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngNonInterlacedGrayscale1bitDecoder;
              2:
                DecoderClass := TPngNonInterlacedGrayscale2bitDecoder;
              4:
                DecoderClass := TPngNonInterlacedGrayscale4bitDecoder;
              8:
                DecoderClass := TPngNonInterlacedGrayscale8bitDecoder;
              16:
                DecoderClass := TPngNonInterlacedGrayscale16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngNonInterlacedTrueColor8bitDecoder;
              16:
                DecoderClass := TPngNonInterlacedTrueColor16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor :
            case ImageHeader.BitDepth of
              1, 2, 4:
                DecoderClass := TPngNonInterlacedPaletteDecoder;
              8:
                DecoderClass := TPngNonInterlacedPalette8bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngNonInterlacedGrayscaleAlpha8bitDecoder;
              16:
                DecoderClass := TPngNonInterlacedGrayscaleAlpha16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngNonInterlacedTrueColorAlpha8bitDecoder;
              16:
                DecoderClass := TPngNonInterlacedTrueColorAlpha16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
           else raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      imAdam7 :
        case ImageHeader.ColorType of
          ctGrayscale  :
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngAdam7Grayscale1bitDecoder;
              2:
                DecoderClass := TPngAdam7Grayscale2bitDecoder;
              4:
                DecoderClass := TPngAdam7Grayscale4bitDecoder;
              8:
                DecoderClass := TPngAdam7Grayscale8bitDecoder;
              16:
                DecoderClass := TPngAdam7Grayscale16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7TrueColor8bitDecoder;
              16:
                DecoderClass := TPngAdam7TrueColor16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor :
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngAdam7Palette1bitDecoder;
              2:
                DecoderClass := TPngAdam7Palette2bitDecoder;
              4:
                DecoderClass := TPngAdam7Palette4bitDecoder;
              8:
                DecoderClass := TPngAdam7Palette8bitDecoder;
             else
               raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7GrayscaleAlpha8bitDecoder;
              16:
                DecoderClass := TPngAdam7GrayscaleAlpha16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha :
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7TrueColorAlpha8bitDecoder;
              16:
               DecoderClass := TPngAdam7TrueColorAlpha16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          else
            raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      else
        raise EPngError.Create(RCStrUnsupportedFormat);
    end;

    if Assigned(FTransparencyChunk) then
      Transparency := FTransparencyChunk.Transparency
    else
      Transparency := nil;

    with DecoderClass.Create(DataStream, FImageHeader, FGammaChunk,
      FPaletteChunk, Transparency) do
    try
      if Assigned(FProgressEvent) then
        DecodeToScanline(Bitmap32, GR32ScanlineProgress)
      else
        DecodeToScanline(Bitmap32, GR32Scanline);
    finally
      Free;
    end;
  finally
    FreeAndNil(DataStream);
  end;
end;

function ColorIndexInPalette(Color: TColor32; Palette: TPalette24): Integer;
begin
  for Result := 0 to Length(Palette) - 1 do
    if (TColor32Entry(Color).R = Palette[Result].R) and
      (TColor32Entry(Color).G = Palette[Result].G) and
      (TColor32Entry(Color).B = Palette[Result].B) then
      Exit;
  Result := -1;
end;

procedure TPortableNetworkGraphic32.AssignPropertiesFromBitmap32(
  Bitmap32: TCustomBitmap32);
var
  Index, PalIndex: Integer;
  IsAlpha: Boolean;
  IsGrayScale: Boolean;
  IsPalette: Boolean;
  Color: TColor32;
  TempPalette: TPalette24;
  TempAlpha: Byte;
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
    // as a palette (and build the palette at the same time)
    for Index := 0 to Width * Height - 1 do
    begin
      Color := Bits[Index];

      // check whether the palette is empty
      if Length(TempPalette) = 0 then
      begin
        IsAlpha := TColor32Entry(Color).A < 255 ;

        // eventually store first alpha component
        if IsAlpha then
          TempAlpha := TColor32Entry(Color).A;

        SetLength(TempPalette, 1);
        TempPalette[0].R := TColor32Entry(Color).R;
        TempPalette[0].G := TColor32Entry(Color).G;
        TempPalette[0].B := TColor32Entry(Color).B;
        IsGrayScale := (TColor32Entry(Color).R = TColor32Entry(Color).G) and
          (TColor32Entry(Color).B = TColor32Entry(Color).G);
      end
      else
      begin
        // check alpha channel
        if (TColor32Entry(Color).A < 255) then
        begin
          if IsAlpha then
            if IsPalette and (TempAlpha <> TColor32Entry(Color).A) then
              IsPalette := False
            else
          else
            TempAlpha := TColor32Entry(Color).A;

          IsAlpha := True;
        end;
        if ColorIndexInPalette(Color, TempPalette) < 0 then
        begin
          if IsPalette then
            if (Length(TempPalette) < 256) then
            begin
              PalIndex := Length(TempPalette);
              SetLength(TempPalette, Length(TempPalette) + 1);
              TempPalette[PalIndex].R := TColor32Entry(Color).R;
              TempPalette[PalIndex].G := TColor32Entry(Color).G;
              TempPalette[PalIndex].B := TColor32Entry(Color).B;
              if IsGrayScale and not
                ((TColor32Entry(Color).R = TColor32Entry(Color).G) and
                (TColor32Entry(Color).B = TColor32Entry(Color).G)) then
                IsGrayScale := False;
            end
            else
              IsPalette := False
          else
            if not ((TColor32Entry(Color).R = TColor32Entry(Color).G) and
              (TColor32Entry(Color).B = TColor32Entry(Color).G)) then
              IsGrayScale := False;
        end;
      end;

      if IsAlpha and (not IsPalette) and (not IsGrayScale) then
        Break;
    end;

    // temporary fix for the case that a palette and an alpha channel has been detected
    if IsPalette and IsAlpha then
      IsPalette := False;

    // set image header
    with ImageHeader do
      if IsGrayScale then
        if IsAlpha then
        begin
          ColorType := ctGrayscaleAlpha;
          BitDepth := 8;
        end
        else
        begin
          ColorType := ctIndexedColor; // ctGrayscale
          if Length(TempPalette) <= 2 then
            BitDepth := 1
          else
          if Length(TempPalette) <= 4 then
            BitDepth := 2
          else
          if Length(TempPalette) <= 16 then
            BitDepth := 4
          else
            BitDepth := 8;
        end
      else
      if IsPalette then
      begin
        ColorType := ctIndexedColor;
        if Length(TempPalette) <= 2 then
          BitDepth := 1 else
        if Length(TempPalette) <= 4 then
          BitDepth := 2 else
        if Length(TempPalette) <= 16 then
          BitDepth := 4
        else
          BitDepth := 8;
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

      if not Assigned(FPaletteChunk) then
        FPaletteChunk := TChunkPngPalette.Create(ImageHeader);

      FPaletteChunk.Count := Length(TempPalette);
      for Index := 0 to Length(TempPalette) - 1 do
        FPaletteChunk.PaletteEntry[Index] := TempPalette[Index];
    end;

    {$IFDEF StoreGamma}
    // add linear gamma chunk
    if not Assigned(FGammaChunk) then
      FGammaChunk := TChunkPngGamma.Create(ImageHeader);
    FGammaChunk.GammaAsSingle := 1;
    {$ELSE}
    // delete any gama correction table
    if Assigned(FGammaChunk) then
      FreeAndNil(FGammaChunk);
    {$ENDIF}
  end;
end;

procedure TPortableNetworkGraphic32.Assign(Source: TPersistent);
var
  EncoderClass: TCustomPngEncoderClass;
  DataStream: TMemoryStream;
begin
  if Source is TCustomBitmap32 then
    with TCustomBitmap32(Source) do
    begin
      // Assign
      AssignPropertiesFromBitmap32(TCustomBitmap32(Source));

      case ImageHeader.ColorType of
        ctGrayscale:
          case ImageHeader.BitDepth of
            1:
              EncoderClass := TPngNonInterlacedGrayscale1bitEncoder;
            2:
              EncoderClass := TPngNonInterlacedGrayscale2bitEncoder;
            4:
              EncoderClass := TPngNonInterlacedGrayscale4bitEncoder;
            8:
              EncoderClass := TPngNonInterlacedGrayscale8bitEncoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
         end;
        ctTrueColor:
          EncoderClass := TPngNonInterlacedTrueColor8bitEncoder;
        ctIndexedColor:
          case ImageHeader.BitDepth of
            1 :
              EncoderClass := TPngNonInterlacedPalette1bitEncoder;
            2 :
              EncoderClass := TPngNonInterlacedPalette2bitEncoder;
            4 :
              EncoderClass := TPngNonInterlacedPalette4bitEncoder;
            8 :
              EncoderClass := TPngNonInterlacedPalette8bitEncoder;
           else
             raise EPngError.Create(RCStrUnsupportedFormat);
          end;
        ctGrayscaleAlpha:
          EncoderClass := TPngNonInterlacedGrayscaleAlpha8bitEncoder;
        ctTrueColorAlpha:
          EncoderClass := TPngNonInterlacedTrueColorAlpha8bitEncoder;
        else
          raise EPngError.Create(RCStrUnsupportedFormat);
      end;

      DataStream := TMemoryStream.Create;
      with DataStream do
        try
          with EncoderClass.Create(DataStream, FImageHeader, FGammaChunk, FPaletteChunk) do
            try
              if Assigned(FProgressEvent) then
                EncodeFromScanline(TCustomBitmap32(Source), GR32ScanlineProgress)
              else
                EncodeFromScanline(TCustomBitmap32(Source), GR32Scanline);
            finally
              Free;
            end;

          // reset data stream position
          DataStream.Seek(0, soFromBeginning);

          // compress image data from data stream
          CompressImageDataFromStream(DataStream);
        finally
          FreeAndNil(DataStream);
        end;
    end
  else
    inherited;
end;

procedure TPortableNetworkGraphic32.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomBitmap32 then
  begin
    TCustomBitmap32(Dest).Width := ImageHeader.Width;
    TCustomBitmap32(Dest).Height := ImageHeader.Height;
    DrawToBitmap32(TCustomBitmap32(Dest));
  end
  else
    inherited;
end;


const
  CRowStart: array [0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart: array [0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement: array [0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement: array [0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  CGrayScaleTable1Bit: array [0..1] of Byte = (0, $FF);
  CGrayScaleTable2Bit: array [0..3] of Byte = (0, $55, $AA, $FF);
  CGrayScaleTable4Bit: array [0..15] of Byte = (0, $11, $22, $33, $44, $55,
    $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);


{ TCustomPngNonInterlacedDecoder }

constructor TCustomPngNonInterlacedDecoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;
  FBytesPerRow := FHeader.BytesPerRow;
  FRowByteSize := FBytesPerRow + 1;
  GetMem(FRowBuffer[0], FRowByteSize);
  GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedDecoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngNonInterlacedDecoder.DecodeToScanline(
  Bitmap: TObject; ScanLineCallback: TScanLineCallback);
var
  Index: Integer;
  CurrentRow: Integer;
  PixelByteSize: Integer;
  AdaptiveFilterMethod: TAdaptiveFilterMethod;
  UsedFilters: TAvailableAdaptiveFilterMethods;
begin
  // initialize variables
  CurrentRow := 0;
  UsedFilters := [];
  PixelByteSize := FHeader.PixelByteSize;

  FillChar(FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

  for Index := 0 to FHeader.Height - 1 do
  begin
    // read data from stream
    if FStream.Read(FRowBuffer[CurrentRow][0], FRowByteSize) <> FRowByteSize then
      raise EPngError.Create(RCStrDataIncomplete);

    // get active filter method
    AdaptiveFilterMethod := TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]);

    // filter current row
    DecodeFilterRow(AdaptiveFilterMethod, FRowBuffer[CurrentRow],
      FRowBuffer[1 - CurrentRow], FBytesPerRow, PixelByteSize);

    // log used row pre filters
    case AdaptiveFilterMethod of
      afmSub:
        UsedFilters := UsedFilters + [aafmSub];
      afmUp:
        UsedFilters := UsedFilters + [aafmUp];
      afmAverage:
        UsedFilters := UsedFilters + [aafmAverage];
      afmPaeth:
        UsedFilters := UsedFilters + [aafmPaeth];
    end;

    // transfer data from row to image
    TransferData(@FRowBuffer[CurrentRow][1], ScanLineCallback(Bitmap, Index));

    // flip current row
    CurrentRow := 1 - CurrentRow;
  end;
  FHeader.AdaptiveFilterMethods := UsedFilters;
end;


{ TPngNonInterlacedGrayscale1bitDecoder }

procedure TPngNonInterlacedGrayscale1bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
  begin
    Dec(BitIndex);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
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


{ TPngNonInterlacedGrayscale2bitDecoder }

procedure TPngNonInterlacedGrayscale2bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
  begin
    Dec(BitIndex, 2);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable2Bit[(Src^ shr BitIndex) and $3]];
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


{ TPngNonInterlacedGrayscale4bitDecoder }

procedure TPngNonInterlacedGrayscale4bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
  begin
    Dec(BitIndex, 4);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable4Bit[(Src^ shr BitIndex) and $F]];
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


{ TPngNonInterlacedGrayscale8bitDecoder }

procedure TPngNonInterlacedGrayscale8bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^];
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := 255;
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedGrayscale16bitDecoder }

procedure TPngNonInterlacedGrayscale16bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PWord absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^ and $FF];
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := 255;
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedTrueColor8bitDecoder }

procedure TPngNonInterlacedTrueColor8bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PRGB24 absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^.R];
    PColor32Entry(Destination)^.G := FMappingTable[Src^.G];
    PColor32Entry(Destination)^.B := FMappingTable[Src^.B];
    PColor32Entry(Destination)^.A := 255;
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedTrueColor16bitDecoder }

procedure TPngNonInterlacedTrueColor16bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PRGB24Word absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^.R and $FF];
    PColor32Entry(Destination)^.G := FMappingTable[Src^.G and $FF];
    PColor32Entry(Destination)^.B := FMappingTable[Src^.B and $FF];
    PColor32Entry(Destination)^.A := 255;
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedPaletteDecoder }

procedure TPngNonInterlacedPaletteDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
  BitIndex: Byte;
  BitMask: Byte;
  BitDepth: Byte;
begin
  BitIndex := 8;
  BitDepth := FHeader.BitDepth;
  BitMask  := (1 shl BitDepth) - 1;
 Palette  := PRGB24Array(FMappingTable);

  for Index := 0 to FHeader.Width - 1 do
  begin
    Dec(BitIndex, BitDepth);
    Color := Palette[(Src^ shr BitIndex) and BitMask];
    PColor32Entry(Destination)^.R := Color.R;
    PColor32Entry(Destination)^.G := Color.G;
    PColor32Entry(Destination)^.B := Color.B;
    PColor32Entry(Destination)^.A := FAlphaTable[(Src^ shr BitIndex) and BitMask];
    if BitIndex = 0 then
    begin
      BitIndex := 8;
      Inc(Src);
    end;
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedPalette8bitDecoder }

procedure TPngNonInterlacedPalette8bitDecoder.TransferData(Source: Pointer;
  Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
begin
  Palette  := PRGB24Array(FMappingTable);
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := Palette[Src^].R;
    PColor32Entry(Destination)^.G := Palette[Src^].G;
    PColor32Entry(Destination)^.B := Palette[Src^].B;
    PColor32Entry(Destination)^.A := FAlphaTable[Src^];
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedGrayscaleAlpha8bitDecoder }

procedure TPngNonInterlacedGrayscaleAlpha8bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := Src^; Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedGrayscaleAlpha16bitDecoder }

procedure TPngNonInterlacedGrayscaleAlpha16bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PWord absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^ and $FF]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := Src^ and $FF; Inc(Src);
    Inc(Destination);
  end;
end;


{ TPngNonInterlacedTrueColorAlpha8bitDecoder }

procedure ConvertColorNonInterlacedTrueColorAlpha8bit(Src: PRGB32;
  Dst: PColor32Entry; Count: Integer; MappingTable: PByteArray);
{$IFDEF PUREPASCAL} inline;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Dst^.R := MappingTable[Src^.R];
    Dst^.G := MappingTable[Src^.G];
    Dst^.B := MappingTable[Src^.B];
    Dst^.A := Src^.A;
    Inc(Src);
    Inc(Dst);
  end;
{$ELSE}
asm
{$IFDEF Target_x64}
  LEA     RCX, [RCX + 4 * R8]
  LEA     RDX, [RDX + 4 * R8]
  NEG     R8
  JNL     @Done

@Start:
  MOVZX   R10, [RCX + 4 * R8].BYTE
  MOVZX   R10, [R9 + R10].BYTE
  MOV     [RDX + 4 * R8 + $02], R10B

  MOVZX   R10, [RCX + 4 * R8 + $01].BYTE
  MOVZX   R10, [R9 + R10].BYTE
  MOV     [RDX + 4 * R8 + $01], R10B

  MOVZX   R10,[RCX + 4 * R8 + $02].BYTE
  MOVZX   R10,[R9 + R10].BYTE
  MOV     [RDX + 4 * R8], R10B

  MOVZX   R10, [RCX + 4 * R8 + $03].BYTE
  MOV     [RDX + 4 * R8 + $03], R10B

  ADD     R8, 1
  JS      @Start
@Done:
{$ENDIF}
{$IFDEF Target_x86}
  LEA     EAX, EAX + 4 * ECX
  LEA     EDX, EDX + 4 * ECX
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  PUSH    EDI

  MOV     EDI, MappingTable;

@Start:
  MOVZX   EBX, [EAX + 4 * ECX]
  MOVZX   EBX, [EDI + EBX]
  MOV     [EDX + 4 * ECX + $02], BL

  MOVZX   EBX, [EAX + 4 * ECX + $01]
  MOVZX   EBX, [EDI + EBX]
  MOV     [EDX + 4 * ECX + $01], BL

  MOVZX   EBX,[EAX + 4 * ECX + $02]
  MOVZX   EBX,[EDI + EBX]
  MOV     [EDX + 4 * ECX], BL

  MOVZX   EBX, [EAX + 4 * ECX + $03]
  MOV     [EDX + 4 * ECX + $03], BL

  ADD     ECX, 1
  JS      @Start

  POP     EDI
  POP     EBX
@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TPngNonInterlacedTrueColorAlpha8bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
begin
  ConvertColorNonInterlacedTrueColorAlpha8bit(PRGB32(Source),
    PColor32Entry(Destination), FHeader.Width, FMappingTable);
end;


{ TPngNonInterlacedTrueColorAlpha16bitDecoder }

procedure TPngNonInterlacedTrueColorAlpha16bitDecoder.TransferData(
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PRGB32Word absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    PColor32Entry(Destination)^.R := FMappingTable[Src^.R and $FF];
    PColor32Entry(Destination)^.G := FMappingTable[Src^.G and $FF];
    PColor32Entry(Destination)^.B := FMappingTable[Src^.B and $FF];
    PColor32Entry(Destination)^.A := Src^.A and $FF;
    Inc(Src);
    Inc(Destination);
  end;
end;


{ TCustomPngAdam7Decoder }

constructor TCustomPngAdam7Decoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;

  // allocate row buffer memory
  GetMem(FRowBuffer[0], FHeader.BytesPerRow + 1);
  GetMem(FRowBuffer[1], FHeader.BytesPerRow + 1);
end;

destructor TCustomPngAdam7Decoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngAdam7Decoder.DecodeToScanline(
  Bitmap: TObject; ScanLineCallback: TScanLineCallback);
var
  CurrentRow: Integer;
  RowByteSize: Integer;
  PixelPerRow: Integer;
  PixelByteSize: Integer;
  CurrentPass: Integer;
  PassRow: Integer;
  UsedFilters: TAvailableAdaptiveFilterMethods;
begin
  // initialize variables
  CurrentRow := 0;
  RowByteSize := 0;
  UsedFilters := [];
  PixelByteSize := FHeader.PixelByteSize;

  // The Adam7 interlacer uses 7 passes to create the complete image
  for CurrentPass := 0 to 6 do
  begin
    // calculate some intermediate variables
    PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] + CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

    with FHeader do
    case ColorType of
      ctGrayscale, ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
      ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
      ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
      ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
      else Continue;
    end;
    if RowByteSize = 0 then
      Continue;

    PassRow := CRowStart[CurrentPass];

    // clear previous row
    FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

    // process pixel
    while PassRow < FHeader.Height do
    begin
      // get interlaced row data
      if FStream.Read(FRowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1) then
        raise EPngError.Create(RCStrDataIncomplete);

      DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]), FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

      // log used row pre filters
      case TAdaptiveFilterMethod(FRowBuffer[CurrentRow]) of
        afmSub:
          UsedFilters := UsedFilters + [aafmSub];
        afmUp:
          UsedFilters := UsedFilters + [aafmUp];
        afmAverage:
          UsedFilters := UsedFilters + [aafmAverage];
        afmPaeth:
          UsedFilters := UsedFilters + [aafmPaeth];
      end;

      // transfer and deinterlace image data
      TransferData(CurrentPass, @FRowBuffer[CurrentRow][1], ScanLineCallback(Bitmap, PassRow));

      // prepare for the next pass
      Inc(PassRow, CRowIncrement[CurrentPass]);
      CurrentRow := 1 - CurrentRow;
    end;
  end;
  FHeader.AdaptiveFilterMethods := UsedFilters;
end;


{ TPngAdam7Grayscale1bitDecoder }

procedure TPngAdam7Grayscale1bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
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
  until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale2bitDecoder }

procedure TPngAdam7Grayscale2bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex, 2);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable2Bit[((Src^ shr BitIndex) and $3)]];
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
  until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale4bitDecoder }

procedure TPngAdam7Grayscale4bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex, 4);
    PColor32Entry(Destination)^.R := FMappingTable[CGrayScaleTable4Bit[((Src^ shr BitIndex) and $F)]];
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
  until Index >= FHeader.Width;
end;

{ TPngAdam7Grayscale8bitDecoder }

procedure TPngAdam7Grayscale8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := 255;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale16bitDecoder }

procedure TPngAdam7Grayscale16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PWord absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^ and $FF]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := 255;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColor8bitDecoder }

procedure TPngAdam7TrueColor8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PRGB24 absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^.R];
    PColor32Entry(Destination)^.G := FMappingTable[Src^.G];
    PColor32Entry(Destination)^.B := FMappingTable[Src^.B];
    PColor32Entry(Destination)^.A := 255;

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColor16bitDecoder }

procedure TPngAdam7TrueColor16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PRGB24Word absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^.R and $FF];
    PColor32Entry(Destination)^.G := FMappingTable[Src^.G and $FF];
    PColor32Entry(Destination)^.B := FMappingTable[Src^.B and $FF];
    PColor32Entry(Destination)^.A := 255;

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7Palette1bitDecoder }

procedure TPngAdam7Palette1bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette  := PRGB24Array(FMappingTable);
  Index    := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex);
    Color := Palette[(Src^ shr BitIndex) and $1];
    PColor32Entry(Destination)^.R := Color.R;
    PColor32Entry(Destination)^.G := Color.G;
    PColor32Entry(Destination)^.B := Color.B;
    PColor32Entry(Destination)^.A := FAlphaTable[(Src^ shr BitIndex) and $1];

    if BitIndex = 0 then
    begin
      BitIndex := 8;
      Inc(Src);
    end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7Palette2bitDecoder }

procedure TPngAdam7Palette2bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette  := PRGB24Array(FMappingTable);
  Index    := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex, 2);
    Color := Palette[(Src^ shr BitIndex) and $3];
    PColor32Entry(Destination)^.R := Color.R;
    PColor32Entry(Destination)^.G := Color.G;
    PColor32Entry(Destination)^.B := Color.B;
    PColor32Entry(Destination)^.A := FAlphaTable[(Src^ shr BitIndex) and $3];

    if BitIndex = 0 then
    begin
      BitIndex := 8;
      Inc(Src);
    end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7Palette4bitDecoder }

procedure TPngAdam7Palette4bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  BitIndex: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette  := PRGB24Array(FMappingTable);
  Index    := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex, 4);
    Color := Palette[(Src^ shr BitIndex) and $F];
    PColor32Entry(Destination)^.R := Color.R;
    PColor32Entry(Destination)^.G := Color.G;
    PColor32Entry(Destination)^.B := Color.B;
    PColor32Entry(Destination)^.A := FAlphaTable[(Src^ shr BitIndex) and $F];

    if BitIndex = 0 then
    begin
      BitIndex := 8;
      Inc(Src);
    end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7Palette8bitDecoder }

procedure TPngAdam7Palette8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
begin
  Palette := PRGB24Array(FMappingTable);
  Index   := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := Palette[Src^].R;
    PColor32Entry(Destination)^.G := Palette[Src^].G;
    PColor32Entry(Destination)^.B := Palette[Src^].B;
    PColor32Entry(Destination)^.A := FAlphaTable[Src^];

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7GrayscaleAlpha8bitDecoder }

procedure TPngAdam7GrayscaleAlpha8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := Src^; Inc(Src);

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7GrayscaleAlpha16bitDecoder }

procedure TPngAdam7GrayscaleAlpha16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  Src: PWord absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[Src^ and $FF]; Inc(Src);
    PColor32Entry(Destination)^.G := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.B := PColor32Entry(Destination)^.R;
    PColor32Entry(Destination)^.A := Src^ and $FF; Inc(Src);

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha8bitDecoder }

procedure TPngAdam7TrueColorAlpha8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  SrcPtr: PRGB32 absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[SrcPtr^.R];
    PColor32Entry(Destination)^.G := FMappingTable[SrcPtr^.G];
    PColor32Entry(Destination)^.B := FMappingTable[SrcPtr^.B];
    PColor32Entry(Destination)^.A := SrcPtr^.A;

    Inc(SrcPtr);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha16bitDecoder }

procedure TPngAdam7TrueColorAlpha16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PColor32);
var
  Index: Integer;
  SrcPtr: PRGB32Word absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PColor32Entry(Destination)^.R := FMappingTable[SrcPtr^.R and $FF];
    PColor32Entry(Destination)^.G := FMappingTable[SrcPtr^.G and $FF];
    PColor32Entry(Destination)^.B := FMappingTable[SrcPtr^.B and $FF];
    PColor32Entry(Destination)^.A := SrcPtr^.A and $FF;

    Inc(SrcPtr);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;


{ TCustomPngNonInterlacedEncoder }

constructor TCustomPngNonInterlacedEncoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;
  FBytesPerRow := FHeader.BytesPerRow;
  FRowByteSize := FBytesPerRow + 1;
  GetMem(FRowBuffer[0], FRowByteSize);
  GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedEncoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

function TCustomPngNonInterlacedEncoder.ColorInPalette(
  Color: TColor32): Integer;
var
  Color24: TRGB24;
begin
  for Result := 0 to FPalette.Count - 1 do
  begin
    Color24 := FPalette.PaletteEntry[Result];
    if (TColor32Entry(Color).R = Color24.R) and
      (TColor32Entry(Color).G = Color24.G) and
      (TColor32Entry(Color).B = Color24.B) then
      Exit;
  end;
  Result := -1;
end;

procedure TCustomPngNonInterlacedEncoder.EncodeFromScanline(Bitmap: TObject;
  ScanLineCallback: TScanLineCallback);
var
  Index: Integer;
  CurrentRow: Integer;
  OutputRow: PByteArray;
  TempBuffer: PByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  FillChar(FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

  // check if pre filter is used and eventually calculate pre filter
  if FHeader.ColorType <> ctIndexedColor then
  begin
    Assert(FRowByteSize = FBytesPerRow + 1);
    GetMem(OutputRow, FRowByteSize);
    GetMem(TempBuffer, FRowByteSize);
    try
      for Index := 0 to FHeader.Height - 1 do
      begin
        // transfer data from image to current row
        TransferData(ScanLineCallback(Bitmap, Index), @FRowBuffer[CurrentRow][1]);

        // filter current row
        EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
          OutputRow, TempBuffer, FBytesPerRow, FHeader.PixelByteSize);
        Assert(OutputRow[0] in [0..4]);

        // write data to data stream
        FStream.Write(OutputRow[0], FRowByteSize);

        // flip current row used
        CurrentRow := 1 - CurrentRow;
      end;
    finally
      Dispose(OutputRow);
      Dispose(TempBuffer);
    end;
  end
  else
    for Index := 0 to FHeader.Height - 1 do
    begin
      // transfer data from image to current row
      TransferData(ScanLineCallback(Bitmap, Index), @FRowBuffer[CurrentRow][1]);

      // set filter method to none
      FRowBuffer[CurrentRow][0] := 0;

      // write data to data stream
      FStream.Write(FRowBuffer[CurrentRow][0], FRowByteSize);

      // flip current row used
      CurrentRow := 1 - CurrentRow;
    end;
end;


{ TPngNonInterlacedGrayscale1bitEncoder }

procedure TPngNonInterlacedGrayscale1bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedGrayscale2bitEncoder }

procedure TPngNonInterlacedGrayscale2bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedGrayscale4bitEncoder }

procedure TPngNonInterlacedGrayscale4bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedGrayscale8bitEncoder }

procedure TPngNonInterlacedGrayscale8bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    Dest^ := PColor32Entry(Source)^.R;
    Inc(Source);
    Inc(Dest);
  end;
end;


{ TPngNonInterlacedTrueColor8bitEncoder }

procedure TPngNonInterlacedTrueColor8bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PRGB24 absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    Dest^.R := PColor32Entry(Source)^.R;
    Dest^.G := PColor32Entry(Source)^.G ;
    Dest^.B := PColor32Entry(Source)^.B;
    Inc(Source);
    Inc(Dest);
  end;
end;


{ TPngNonInterlacedPalette1bitEncoder }

procedure TPngNonInterlacedPalette1bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedPalette2bitEncoder }

procedure TPngNonInterlacedPalette2bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedPalette4bitEncoder }

procedure TPngNonInterlacedPalette4bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
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


{ TPngNonInterlacedPalette8bitEncoder }

procedure TPngNonInterlacedPalette8bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    Dest^ := ColorInPalette(Source^);
    Inc(Source);
    Inc(Dest);
  end;
end;


{ TPngNonInterlacedGrayscaleAlpha8bitEncoder }

procedure TPngNonInterlacedGrayscaleAlpha8bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    Dest^ := PColor32Entry(Source)^.R; Inc(Dest);
    Dest^ := PColor32Entry(Source)^.A; Inc(Dest);
    Inc(Source);
  end;
end;


{ TPngNonInterlacedTrueColorAlpha8bitEncoder }

procedure TPngNonInterlacedTrueColorAlpha8bitEncoder.TransferData(Source: PColor32;
  Destination: Pointer);
var
  Index: Integer;
  Dest: PRGB32 absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
  begin
    Dest^.R := PColor32Entry(Source)^.R;
    Dest^.G := PColor32Entry(Source)^.G;
    Dest^.B := PColor32Entry(Source)^.B;
    Dest^.A := PColor32Entry(Source)^.A;
    Inc(Dest);
    Inc(Source);
  end;
end;

{ TPngPalette }

function TPngPalette.Add(const Item: TColor32): Integer;
begin
  Find(Item, Result{%H-});
  InsertItem(Result, Item);
end;

procedure TPngPalette.Clear;
begin
  SetLength(FItems, 0);
  FCount := 0;
end;

function TPngPalette.Compare(const item1, item2: TColor32): Integer;
begin
  Result := item1 - item2;
end;

function TPngPalette.Find(const item: TColor32; var index: Integer): Boolean;
var
  lo, hi, mid, compResult: Integer;
begin
  Result := False;
  lo := 0;
  hi := FCount - 1;
  while lo <= hi do
  begin
    mid := (lo + hi) shr 1;
    compResult := Compare(FItems[mid], item);
    if compResult < 0 then
      lo := mid + 1
    else
    begin
      hi := mid - 1;
      if compResult = 0 then
        Result:=True;
    end;
  end;

  index := lo;
end;

function TPngPalette.GetItem(index: Integer): TColor32;
begin
  Result := FItems[index];
end;

procedure TPngPalette.GetNearest(var Value: TColor32);
var
  Index, MinIndex: Integer;
  Distance, MinDistance: Integer;
begin
  if IndexOf(Value) < 0 then
  begin
    MinDistance :=
      Sqr(TColor32Entry(Value).R - TColor32Entry(FItems[0]).R) +
      Sqr(TColor32Entry(Value).G - TColor32Entry(FItems[0]).G) +
      Sqr(TColor32Entry(Value).B - TColor32Entry(FItems[0]).B);
    MinIndex := 0;
    for Index := 1 to Count - 1 do
    begin
      Distance :=
        Sqr(TColor32Entry(Value).R - TColor32Entry(FItems[Index]).R) +
        Sqr(TColor32Entry(Value).G - TColor32Entry(FItems[Index]).G) +
        Sqr(TColor32Entry(Value).B - TColor32Entry(FItems[Index]).B);
      if Distance < MinDistance then
      begin
        MinDistance := Distance;
        MinIndex := Index;
      end;
    end;

    Value := FItems[MinIndex];
  end;
end;

function TPngPalette.IndexOf(const Value: TColor32): Integer;
begin
  if not Find(Value, Result{%H-}) then
    Result := -1;
end;

procedure TPngPalette.InsertItem(index: Integer; const anItem: TColor32);
begin
  if Count = Length(FItems) then
    SetLength(FItems, Count + 8 + (Count shr 4));

  if index < Count then
    System.Move(FItems[Index], FItems[Index + 1], (Count - Index) * SizeOf(TColor32));

  Inc(FCount);
  FItems[index] := anItem;
end;

procedure TPngPalette.LimitTo(Count: Integer);
begin
  SetLength(FItems, Count);
  FCount := Count;
end;

procedure TPngPalette.Remove(Index: Integer);
var
  n: Integer;
begin
  Dec(FCount);
  n := FCount - index;
  if n > 0 then
    System.Move(FItems[Index + 1], FItems[Index], n * SizeOf(TColor32));
  SetLength(FItems, FCount);
end;


{ TPngHistogramEntry }

constructor TPngHistogramEntry.Create(Key: TColor32);
begin
  FColor := Key;
end;

procedure TPngHistogramEntry.Advance;
begin
  Inc(FCount);
end;


{ TPngHistogram }

function TPngHistogram.GetItem(index: Integer): TPngHistogramEntry;
begin
  Result := FItems[index];
end;

function TPngHistogram.Find(const item: TColor32; var index: Integer): Boolean;
var
  lo, hi, mid, compResult: Integer;
begin
  Result := False;
  lo := 0;
  hi := FCount - 1;
  while lo <= hi do
  begin
    mid := (lo + hi) shr 1;
    compResult := Compare(FItems[mid].Color, item);
    if compResult < 0 then
      lo := mid + 1
    else
    begin
      hi := mid - 1;
      if compResult = 0 then
        Result:=True;
    end;
  end;

  index := lo;
end;

procedure TPngHistogram.InsertItem(index: Integer; const anItem: TPngHistogramEntry);
begin
  if Count = Length(FItems) then
    SetLength(FItems, Count + 8 + (Count shr 4));

  if index < Count then
    System.Move(FItems[index], FItems[index+1], (Count-index)*SizeOf(Pointer));

  Inc(FCount);
  FItems[index] := anItem;
end;

function TPngHistogram.Add(Value: TColor32): Integer;
begin
  Result := Add(TPngHistogramEntry.Create(Value));
end;

function TPngHistogram.Add(const anItem: TPngHistogramEntry): Integer;
begin
  Find(anItem.Color, Result{%H-});
  InsertItem(Result, anItem);
end;

procedure TPngHistogram.Advance(Value: TColor32);
var
  Index: Integer;
begin
  Index := IndexOf(Value);
  if Index < 0 then
    Add(Value)
  else
    FItems[Index].Advance;
end;

function TPngHistogram.IndexOf(const Value: TColor32): Integer;
begin
  if not Find(Value, Result{%H-}) then
    Result := -1;
end;

procedure TPngHistogram.Remove(Index: Integer);
var
  n: Integer;
begin
  Dec(FCount);
  n := FCount - index;
  if n > 0 then
    System.Move(FItems[Index + 1], FItems[Index], n * SizeOf(TPngHistogramEntry));
  SetLength(FItems, FCount);
end;

function TPngHistogram.GetPalette(MaxColors: Integer = 256): TPngPalette;
var
  PaletteIndex, Index, LastIndex: Integer;
  ColorCount: Integer;
begin
  Result := TPngPalette.Create;

  for PaletteIndex := 0 to Min(Count, MaxColors) - 1 do
  begin
    ColorCount := FItems[0].Count;
    LastIndex := 0;
    for Index := 1 to FCount - 1 do
    begin
      if (FItems[Index].Count > ColorCount) then
      begin
        LastIndex := Index;
        ColorCount := FItems[Index].Count;
      end;
    end;

    Result.Add(FItems[LastIndex].FColor);
    Remove(LastIndex);
  end;
end;

procedure TPngHistogram.Clear;
begin
  SetLength(FItems, 0);
  FCount := 0;
end;

function TPngHistogram.Compare(const item1, item2: TColor32): Integer;
begin
  Result := item1 - item2;
end;

procedure TPngHistogram.Clean;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].Free;
  Clear;
end;

end.
