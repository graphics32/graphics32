unit GR32.ImageFormats.PSD.Reader;

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
 * The Original Code is PSD Image Format support for Graphics32
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

// PSD_PARSE_IMAGE_RESOURCE_SECTION
// If defined, the Image Resource section is parsed.
{$define PSD_PARSE_IMAGE_RESOURCE_SECTION}

uses
  Classes,
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.PSD.Model;

//------------------------------------------------------------------------------
//
//      PhotoshopDocumentReader
//
//------------------------------------------------------------------------------
// Reads a PSD document from a stream
//------------------------------------------------------------------------------
type
  PhotoshopDocumentReader = record
  public
    class procedure LoadFromStream(ADocument: TPhotoshopDocument; AStream: TStream); static;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$ifndef FPC}
  ZLib,
{$else FPC}
  zstream,
{$endif FPC}
  Generics.Collections,
  SysUtils,
  Math,
  GR32,
  GR32_LowLevel,
  GR32.BigEndian,
  GR32.ImageFormats.PSD.Types;

type
  TPhotoshopLayerCracker = class(TCustomPhotoshopLayer);

//------------------------------------------------------------------------------
//
//      Color conversion
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// CMYK -> RGB
//------------------------------------------------------------------------------
type
  TColorCMYK = record
{$IFNDEF RGBA_FORMAT}
    Y, M, C, K: Byte;
{$ELSE}
    C, M, Y, K: Byte;
{$ENDIF}
  end;

procedure CMYKtoColor32(const CMYK: TColorCMYK; var RGB: TColor32Entry);
var
  C, M, Y: Single;
  KK: Single;
begin
  // Note that PSD CMYK are already inverse.

  // https://graphicdesign.stackexchange.com/a/137902

  C := CMYK.C;
  M := CMYK.M;
  Y := CMYK.Y;
  KK := CMYK.K / 255;

  RGB.R := Clamp(Round(KK * (80 + 0.5882 * C - 0.3529 * M - 0.1373 * Y + 0.00185 * C * M + 0.00046 * Y * C))); // no YM
  RGB.G := Clamp(Round(KK * (66 - 0.1961 * C + 0.2745 * M - 0.0627 * Y + 0.00215 * C * M + 0.00008 * Y * C + 0.00062 * Y * M)));
  RGB.B := Clamp(Round(KK * (86 - 0.3255 * C - 0.1569 * M + 0.1647 * Y + 0.00046 * C * M + 0.00123 * Y * C + 0.00215 * Y * M)));
  RGB.A := 255;
end;


//------------------------------------------------------------------------------
// CIEL*a*b -> XYZ
//------------------------------------------------------------------------------
type
  TColorLAB = record
{$IFNDEF RGBA_FORMAT}
    b, a, L: Byte;
{$ELSE}
    L, a, b: Byte;
{$ENDIF}
    Alpha: Byte;
  end;

type
  TColorXYZ = record
    X, Y, Z: Single;
    Alpha: Byte;
  end;

procedure LABtoXYZ(const LAB: TColorLAB; var XYZ: TColorXYZ);
var
  X, Y, Z: Single;
  Pow3: Single;
begin
  // Based on http://www.easyrgb.com

  Y := ((LAB.L / 255 * 100) + 16) / 116;
  X := (LAB.a - 128) / 500 + Y;
  Z := Y - (LAB.b - 128) / 200;

  Pow3 := Y * Y * Y;
  if (Pow3 > 0.008856) then
    Y := Pow3
  else
    Y := (Y - 16 / 116) / 7.787;

  Pow3 := X * X * X;
  if (Pow3 > 0.008856) then
    X := Pow3
  else
    X := (X - 16 / 116) / 7.787;

  Pow3 := Z * Z * Z;
  if (Pow3 > 0.008856) then
    Z := Pow3
  else
    Z := (Z - 16 / 116) / 7.787;

  XYZ.X := 95.047 * X;	// ref_X = 95.047 (Observer= 2°, Illuminant= D65)
  XYZ.Y := 100.000 * Y;	// ref_Y = 100.000
  XYZ.Z := 108.883 * Z;	// ref_Z = 108.883
  XYZ.Alpha := LAB.Alpha;
end;

//------------------------------------------------------------------------------
// XYZ -> RGB
//------------------------------------------------------------------------------
procedure XYZtoColor32(const XYZ: TColorXYZ; var RGB: TColor32Entry);
var
  X, Y, Z: Single;
  R, G, B: Single;
const
  Exponent: Single = 1 / 2.4;
begin
  // Based on http://www.easyrgb.com

  X := XYZ.X / 100; //X from 0 to  95.047 (Observer = 2°, Illuminant = D65)
  Y := XYZ.Y / 100; //Y from 0 to 100.000
  Z := XYZ.Z / 100; //Z from 0 to 108.883

  R := X *  3.2406 + Y * -1.5372 + Z * -0.4986;
  G := X * -0.9689 + Y *  1.8758 + Z *  0.0415;
  B := X *  0.0557 + Y * -0.2040 + Z *  1.0570;

  if (R > 0.0031308) then
    R := 1.055 * Power(R, Exponent) - 0.055
  else
    R := 12.92 * R;

  if (G > 0.0031308) then
    G := 1.055 * Power(G, Exponent) - 0.055
  else
    G := 12.92 * G;

  if (B > 0.0031308) then
    B := 1.055 * Power(B, Exponent) - 0.055
  else
    B := 12.92 * B;

  RGB.R := Clamp(Round(255 * R));
  RGB.G := Clamp(Round(255 * G));
  RGB.B := Clamp(Round(255 * B));
  RGB.A := XYZ.Alpha;
end;

//------------------------------------------------------------------------------
// CIELab -> XYZ -> RGB
//------------------------------------------------------------------------------
procedure LABtoColor32(const LAB: TColorLAB; var RGB: TColor32Entry);
var
  XYZ: TColorXYZ;
begin
  LABtoXYZ(LAB, XYZ);
  XYZtoColor32(XYZ, RGB);
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopDocumentReaderHelper
//
//------------------------------------------------------------------------------
// Parses a PSD file and constructs a TPhotoshopDocument from it.
//------------------------------------------------------------------------------
type
  TPhotoshopDocumentReaderHelper = class
{$ifdef FPC}
  private class var
    ccExtra: TColor32Component;
 {$else}
  private const
    ccExtra: TColor32Component = TColor32Component(255);
{$endif}
  private type
    TChannelInfo = record
      Component: SmallInt; // PSD color component value
      HasColorComponent: boolean; // Is ColorComponent valid?
      ColorComponent: TColor32Component; // GR32 color component value
      Size: Cardinal; // Size of channel in stream
    end;

    TColor32Components = set of TColor32Component;

    TLayerInfo = record
      Size: Cardinal;
      ColorComponents: TColor32Components;
      HasExtra: boolean;
      Channels: TArray<TChannelInfo>; // One TChannelInfo per channel
    end;

    TChannelInfoList = TArray<TLayerInfo>; // One TLayerInfo per layer

  private type
    // Copy from buffer into scanline
    TPSDChannelSetterDelegate = procedure(ALayer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes) of object;

    // Read all channels in one go; Used for background bitmap
    TPSDBitmapReaderDelegate = procedure(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);

    // Read a single channels; Used for layer bitmaps
    TPSDChannelReaderDelegate = procedure(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);

    CompressionRAW = record
      class procedure ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
      class procedure ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
    end;

    CompressionRLE = record
    private
      class procedure Decode(AStream: TStream; const ABuffer: TBytes); static;
    public
      class procedure ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
      class procedure ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
    end;

    CompressionZIP = record
      class procedure ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
      class procedure ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate); static;
    end;

  private
    FStream: TStream;
    FDocument: TPhotoshopDocument;
    FLayers: TList<TCustomPhotoshopLayer>;
    FMode: Word;
    FDepth: Word;
    FChannelInfo: TChannelInfoList;
    FPalette: array[byte] of TColor32;
{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
    FTransparentColorIndex: integer;
{$endif}
    FAlphaChannelBuffer: TBytes;
    FHasCompression: boolean;
    FCompression: TPSDLayerCompression;
  protected
    function BytesPerScanline(Width: integer): integer;
    function GetSetChannel: TPSDChannelSetterDelegate;
    procedure SetChannel1bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
    procedure SetChannel8bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
    procedure SetChannel16bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
    procedure SetChannel32bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);

    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadBytes(Count: integer): TBytes;
    function ReadAnsiString(ACount: integer): AnsiString;
    function ReadPascalAnsiString(APadTo: integer = 1): AnsiString;
    function ReadUnicodeString: string;
    function Padding(Value: Cardinal; Alignment: Cardinal = 4): Cardinal;
    procedure Skip(ACount: Int64);
    procedure SetLayerChannelInfo(var LayerInfo: TLayerInfo; Channel: Cardinal; Component: SmallInt; Size: Cardinal = 0);
    class function GetChannelReader(Compression: TPSDLayerCompression): TPSDChannelReaderDelegate;
    class function GetBitmapReader(ALayer: TCustomPhotoshopLayer): TPSDBitmapReaderDelegate;

    function TryChannelToColorComponent(AChannel: SmallInt; var AComponent: TColor32Component): boolean;

    procedure ReadHeader;
    procedure ReadColorModeData;
    procedure ReadImageResources;
    procedure ReadLayerAndMaskInfo;
    procedure   ReadLayerInfo;
    function      ReadLayerRecord(var LayerInfo: TLayerInfo): TCustomPhotoshopLayer;
    procedure     BeginReadLayerImageData;
    procedure       ReadLayersImageData;
    procedure         BeginReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
    procedure           ReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
    procedure         EndReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
    procedure     EndReadLayerImageData;
    procedure ReadCompositeImage;

  public
    constructor Create(ADocument: TPhotoshopDocument; AStream: TStream);
    destructor Destroy; override;

    procedure Read;
  end;

//------------------------------------------------------------------------------

constructor TPhotoshopDocumentReaderHelper.Create(ADocument: TPhotoshopDocument; AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FDocument := ADocument;
  FLayers := TList<TCustomPhotoshopLayer>.Create;
end;

destructor TPhotoshopDocumentReaderHelper.Destroy;
begin
  FLayers.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.BytesPerScanline(Width: integer): integer;
begin
  case FDepth of
    1: Result := (Width + 7) div 8;
    8: Result := Width;
    16: Result := Width * 2;
    32: Result := Width * 4;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported bit depth: %d', [FDepth]);
  end;
end;

function TPhotoshopDocumentReaderHelper.GetSetChannel: TPSDChannelSetterDelegate;
begin
  case FDepth of
    1: Result := SetChannel1bit;
    8: Result := SetChannel8bit;
    16: Result := SetChannel16bit;
    32: Result := SetChannel32bit;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported bit depth: %d', [FDepth]);
  end;
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.ReadByte: Byte;
begin
  FStream.Read(Result, 1);
end;

function TPhotoshopDocumentReaderHelper.ReadWord: Word;
begin
  Result := BigEndian.ReadWord(FStream);
end;

function TPhotoshopDocumentReaderHelper.ReadCardinal: Cardinal;
begin
  Result := BigEndian.ReadCardinal(FStream);
end;

function TPhotoshopDocumentReaderHelper.ReadBytes(Count: integer): TBytes;
begin
  SetLength(Result, Count);
  if (Count > 0) then
    FStream.Read(Result[0], Count);
end;

function TPhotoshopDocumentReaderHelper.ReadAnsiString(ACount: integer): AnsiString;
begin
  SetLength(Result, ACount);
  if (ACount > 0) then
    FStream.Read(Result[1], ACount);
end;

function TPhotoshopDocumentReaderHelper.ReadPascalAnsiString(APadTo: integer): AnsiString;
var
  Len: Byte;
  PaddedLen: integer;
begin
  Len := ReadByte;
  Result := ReadAnsiString(Len);
  PaddedLen := APadTo - (Len + 1) mod APadTo;
  if (PaddedLen <> APadTo) then
    Skip(PaddedLen);
end;

function TPhotoshopDocumentReaderHelper.ReadUnicodeString: string;
var
  Len: Cardinal;
  I: Integer;
begin
  Len := ReadCardinal;
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := WideChar(ReadWord);
end;

function TPhotoshopDocumentReaderHelper.Padding(Value: Cardinal; Alignment: Cardinal): Cardinal;
begin
  Result := Cardinal((Alignment - (Value and (Alignment - 1))) and (Alignment - 1));
end;

procedure TPhotoshopDocumentReaderHelper.Skip(ACount: Int64);
begin
  if (ACount <> 0) then
    FStream.Seek(ACount, soFromCurrent);
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.TryChannelToColorComponent(AChannel: SmallInt; var AComponent: TColor32Component): boolean;
begin
  Result := True;

  case FMode of

    PSD_CMYK:
      begin
        case AChannel of
          PSD_MASK_ALPHA:     AComponent := ccExtra; // Save alpha in "extra" buffer
          PSD_MASK_RED:       AComponent := ccRed;   // Cyan
          PSD_MASK_GREEN:     AComponent := ccGreen; // Magenta
          PSD_MASK_BLUE:      AComponent := ccBlue;  // Yellow
          PSD_MASK_BLACK:     AComponent := ccAlpha; // Black
        else
          Result := False;
        end;
      end;

  else
    case AChannel of
      PSD_MASK_ALPHA:     AComponent := ccAlpha;
      PSD_MASK_RED:       AComponent := ccRed;
      PSD_MASK_GREEN:     AComponent := ccGreen;
      PSD_MASK_BLUE:      AComponent := ccBlue;
    else
      Result := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.SetChannel1bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
var
  i: integer;
  Bit: integer;
  Mask: Byte;
  Value: Byte;
  Count: integer;
  Offset: integer;
begin
  if (AChannel <> ccExtra) then
  begin
    Offset := 0;
    Count := Layer.Width;
    Mask := $80;

    for i := 0 to High(ABytes) do
    begin
      for Bit := 0 to 7 do
      begin
        Value := Ord(ABytes[i] and Mask <> 0);

        TPhotoshopLayerCracker(Layer).SetChannelScanlinePixel(AChannel, ALine, Offset, Value);

        Dec(Count);
        if (Count = 0) then
          exit;

        Inc(Offset);
        Mask := Mask shr 1;
        if (Mask = 0) then
          Mask := $80;
      end;
    end;
  end else
    Move(ABytes[0], FAlphaChannelBuffer[ALine * Length(ABytes)], Length(ABytes));
end;

procedure TPhotoshopDocumentReaderHelper.SetChannel8bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
begin
  if (AChannel <> ccExtra) then
    TPhotoshopLayerCracker(Layer).SetChannelScanLine(AChannel, ALine, ABytes[0])
  else
    Move(ABytes[0], FAlphaChannelBuffer[ALine * Length(ABytes)], Length(ABytes));
end;

procedure TPhotoshopDocumentReaderHelper.SetChannel16bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
var
  i: integer;
begin
  // 16-bit format is stored a stream of big endian words.
  // Since we reduce to 8-bit anyway we can just ignore the low-bytes.
  if (AChannel <> ccExtra) then
  begin
    for i := 0 to (Length(ABytes) div 2)-1 do
      TPhotoshopLayerCracker(Layer).SetChannelScanlinePixel(AChannel, ALine, i, ABytes[i*2]);
  end else
    Move(ABytes[0], FAlphaChannelBuffer[ALine * Length(ABytes)], Length(ABytes));
end;

procedure TPhotoshopDocumentReaderHelper.SetChannel32bit(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
type
  TFloats = TArray<Cardinal>;
var
  i: integer;
  Value: Cardinal;
  FloatValue: Single;
  ByteValue: Byte;
begin
  if (AChannel <> ccExtra) then
  begin
    for i := 0 to (Length(ABytes) div 4)-1 do
    begin
      Value := Swap32(TFloats(ABytes)[i]);
      FloatValue := PSingle(@Value)^;
      ByteValue := Clamp(Round(FloatValue * 255));
      TPhotoshopLayerCracker(Layer).SetChannelScanlinePixel(AChannel, ALine, i, ByteValue)
    end;
  end else
    Move(ABytes[0], FAlphaChannelBuffer[ALine * Length(ABytes)], Length(ABytes));
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.SetLayerChannelInfo(var LayerInfo: TLayerInfo; Channel: Cardinal; Component: SmallInt; Size: Cardinal);
var
  ColorComponent: TColor32Component;
begin
  LayerInfo.Channels[Channel].Component := Component;
  LayerInfo.Channels[Channel].HasColorComponent := TryChannelToColorComponent(Component, ColorComponent);
  LayerInfo.Channels[Channel].Size := Size;

  if (LayerInfo.Channels[Channel].HasColorComponent) then
  begin
    LayerInfo.Channels[Channel].ColorComponent := ColorComponent;

    if (ColorComponent <> ccExtra) then
      Include(LayerInfo.ColorComponents, ColorComponent)
    else
      LayerInfo.HasExtra := True;
  end;

  Inc(LayerInfo.Size, Size);
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.Read;
begin
  ReadHeader;
  ReadColorModeData;
  ReadImageResources;
  ReadLayerAndMaskInfo;
  ReadCompositeImage;

  if (FHasCompression) then
    FDocument.Compression := FCompression;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadHeader;
var
  Signature: AnsiString;
  Version: Word;
  SupportedBits: set of byte;
  SupportedChannels: set of byte;
begin
(* File Header Section
+--------+----------------------------------------------------------------------+
| Length | Description                                                          |
+--------+----------------------------------------------------------------------+
| 4      | Signature: always equal to '8BPS'.                                   |
|        | Do not try to read the file if the signature does not match this     |
|        | value.                                                               |
+--------+----------------------------------------------------------------------+
| 2      | Version: always equal to 1.                                          |
|        | Do not try to read the file if the version does not match this value.|
+--------+----------------------------------------------------------------------+
| 6      | Reserved: must be zero.                                              |
+--------+----------------------------------------------------------------------+
| 2      | The number of channels in the image, including any alpha channels.   |
|        | Supported range is 1 to 56.                                          |
+--------+----------------------------------------------------------------------+
| 4      | The height of the image in pixels. Supported range is 1 to 30,000.   |
+--------+----------------------------------------------------------------------+
| 4      | The width of the image in pixels. Supported range is 1 to 30,000.    |
+--------+----------------------------------------------------------------------+
| 2      | Depth: the number of bits per channel. Supported values are 1, 8, 16 |
|        | and 32.                                                              |
+--------+----------------------------------------------------------------------+
| 2      | The color mode of the file. Supported values are:                    |
|        |   Bitmap = 0; Grayscale = 1; Indexed = 2; RGB = 3; CMYK = 4;         |
|        |   Multichannel = 7; Duotone = 8; Lab = 9.                            |
+--------+----------------------------------------------------------------------+
*)

  Signature := ReadAnsiString(4);
  if (Signature <> '8BPS') then
    raise EPhotoshopDocument.Create('Invalid PSD signature');

  Version := ReadWord;
  if (Version <> 1) then
    raise EPhotoshopDocument.CreateFmt('Unsupported PSD version: %d', [Version]);

  Skip(6); // Skip reserved

  FDocument.Channels := ReadWord;
  if (FDocument.Channels > PSD_MAX_CHANNELS) then
    raise EPhotoshopDocument.CreateFmt('Invalid number of channels: %d', [FDocument.Channels]);

  FDocument.Height := ReadCardinal;
  FDocument.Width := ReadCardinal;

  FDepth := ReadWord;

  SupportedBits := [1, 8, 16, 32];
  SupportedChannels := [1..PSD_MAX_CHANNELS];

  FMode := ReadWord;
  case FMode of
    PSD_DUOTONE, // DuoTone isn't really supported; Treat it as grayscale
    PSD_GRAYSCALE:
      begin
        SupportedChannels := [1, 2..PSD_MAX_CHANNELS];
        if (FDocument.Channels = 1) then
          SupportedBits := [8, 16, 32]
        else
          SupportedBits := [8, 16];
      end;

    PSD_BITMAP: // Monochrome; Pixel set is white
      begin
        SupportedChannels := [1..PSD_MAX_CHANNELS];
        SupportedBits := [1];
      end;

    PSD_INDEXED:
      begin
        SupportedChannels := [1..PSD_MAX_CHANNELS];
        SupportedBits := [8];
      end;

    PSD_MULTICHANNEL,
    PSD_RGB:
      begin
        SupportedChannels := [3, 4..PSD_MAX_CHANNELS];
        SupportedBits := [8, 16, 32];
      end;

    PSD_CMYK:
      begin
        SupportedChannels := [4, 5..PSD_MAX_CHANNELS];
        SupportedBits := [8, 16, 32];
      end;

    PSD_LAB:
      begin
        SupportedChannels := [3, 4..PSD_MAX_CHANNELS];
        SupportedBits := [8, 16, 32];
      end;

  else
    raise EPhotoshopDocument.CreateFmt('Unsupported color mode: %d', [FMode]);
  end;

  if (not (FDepth in SupportedBits)) then
    raise EPhotoshopDocument.CreateFmt('Unsupported bit depth (%d) for mode (%d)', [FDepth, FMode]);

  if (not (FDocument.Channels in SupportedChannels)) then
    raise EPhotoshopDocument.CreateFmt('Unsupported number of channels (%d) for mode (%d)', [FDocument.Channels, FMode]);
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadColorModeData;

  procedure CreateGrayscalePalette;
  var
    i: integer;
  begin
    for i := 0 to 255 do
      FPalette[i] := Gray32(i);
  end;

var
  Size: Cardinal;
  Palette: array[0..2, 0..255] of byte;
  i: integer;
begin
(* Color Mode Data Section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | The length of the following color data.                            |
+----------+--------------------------------------------------------------------+
| Variable | The color data; Only implemented for indexed color.                |
+----------+--------------------------------------------------------------------+
Only indexed color and duotone (see the mode field in the File header section)
have color mode data.
For all other modes, this section is just the 4-byte length field, which is set
to zero.

Indexed color images: length is 768; color data contains the color table for
the image, in non-interleaved order.

Duotone images: color data contains the duotone specification (the format of
which is not documented). Other applications that read Photoshop files can
treat a duotone image as a gray image, and just preserve the contents of the
duotone information when reading and writing the file.
*)

{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
  FTransparentColorIndex := -1;
{$endif}

  Size := ReadCardinal;

  case FMode of

    PSD_INDEXED:
      begin
        if (Size <> SizeOf(Palette)) then
          raise EPhotoshopDocument.CreateFmt('Invalid palette size: %d', [Size]);
        FStream.Read(Palette, SizeOf(Palette));
        for i := 0 to High(FPalette) do
        begin
          TColor32Entry(FPalette[i]).R := Palette[0, i];
          TColor32Entry(FPalette[i]).G := Palette[1, i];
          TColor32Entry(FPalette[i]).B := Palette[2, i];
          TColor32Entry(FPalette[i]).A := 255;
        end;
      end;

    PSD_DUOTONE,
    PSD_GRAYSCALE:
      begin
        CreateGrayscalePalette;

        if (Size > 0) then
          Skip(Size);
      end;

  else
    if (Size > 0) then
      Skip(Size);
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadImageResources;
var
  Size: Cardinal;
{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
  Next: Int64;
  StartPos: Int64;
  Signature: AnsiString;
  Ident: Word;
  Name: AnsiString;
  DataSize: Cardinal;
  // FColorTableCount: Cardinal;
{$endif}
begin
(* Image Resources Section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of image resource section. The length may be zero.          |
+----------+--------------------------------------------------------------------+
| Variable | Image resources; Not implemented.                                  |
+----------+--------------------------------------------------------------------+
*)

  Size := ReadCardinal;

{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
  Next := FStream.Position + Size;

  while (FStream.Position < Next) do
  begin
    Signature := ReadAnsiString(4);
    if (Signature <> '8BIM') then
      raise EPhotoshopDocument.Create('Invalid layer data signature');
    Ident := ReadWord;
    Name := ReadPascalAnsiString(2);
    DataSize := ReadCardinal;
    StartPos := FStream.Position;
    case TPSD_ImageResourceID(Ident) of
      PSD_IDX_COL_TAB_CNT:
        ReadWord;

      PSD_IDX_TRANSPARENT:
        FTransparentColorIndex := ReadWord;
    end;
    Dec(DataSize, FStream.Position - StartPos);
    Skip(DataSize + Padding(DataSize, 2));
  end;

  Assert(FStream.Position <= Next);

  FStream.Position := Next;
{$else}
  if (Size > 0) then
    Skip(Size);
{$endif}
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayerAndMaskInfo;
var
  Size: Cardinal;
  Next: Int64;
begin
(* Layer and mask information section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of the layer and mask information section.                  |
+----------+--------------------------------------------------------------------+
| Variable | Layer info (see [Layer info] for details).                         |
+----------+--------------------------------------------------------------------+
| Variable | Global layer mask info (see [Global layer mask info] for details). |
+----------+--------------------------------------------------------------------+
| Variable | (Photoshop 4.0 and later)                                          |
|          | Series of tagged blocks containing various types of data. See      |
|          | [Additional Layer Information] for the list of the types of data   |
|          | that can be included here.                                         |
+----------+--------------------------------------------------------------------+

Global layer mask info
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of global layer mask info section.                          |
+----------+--------------------------------------------------------------------+
| Variable | Not implemented.                                                   |
+----------+--------------------------------------------------------------------+

Additional Layer Information
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Signature: '8BIM' or '8B64'                                        |
+----------+--------------------------------------------------------------------+
| 4        | Key: a 4-character code (not documented here)                      |
+----------+--------------------------------------------------------------------+
| 4        | Length data below, rounded up to an even byte count.               |
+----------+--------------------------------------------------------------------+
| Variable | Data (not documented here)                                         |
+----------+--------------------------------------------------------------------+
*)

  Size := ReadCardinal;
  if (Size > 0) then
  begin
    Next := FStream.Position + Size;

    ReadLayerInfo;

    // Skip "Global layer mask info"
    Size := ReadCardinal;
    if (Size > 0) then
      Skip(Size);

    // Skip "Additional Layer Information"
    FStream.Position := Next;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayerInfo;
var
  Size: Cardinal;
  Next: Int64;
  Count: SmallInt;
  i: Integer;
  Layer: TCustomPhotoshopLayer;
begin
(* Layer info
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of the layers info section, rounded up to a multiple of 2.  |
+----------+--------------------------------------------------------------------+
| 2        | Layer count. If it is a negative number, its absolute value is the |
|          | number of layers and the first alpha channel contains the          |
|          | transparency data for the merged result.                           |
+----------+--------------------------------------------------------------------+
| Variable | Information about each layer. [Layer records] describes the        |
|          | structure of this information for each layer.                      |
+----------+--------------------------------------------------------------------+
| Variable | Channel image data. Contains one or more image data records (see   |
|          | [Channel image data] for structure) for each layer. The layers are |
|          | in the same order as in the layer information (previous row of this|
|          | table).                                                            |
+----------+--------------------------------------------------------------------+
*)

  Size := ReadCardinal;
  if (Size = 0) then
    Exit;

  Next := FStream.Position + Size;

  Count := SmallInt(ReadWord);
  if (Count < 0) then
    // TODO : Set flag to ignore alpha channel of first layer
    Count := -Count;

  SetLength(FChannelInfo, Count);

  for i := 0 to Count - 1 do
  begin
    Layer := ReadLayerRecord(FChannelInfo[i]);
    FLayers.Add(Layer);
  end;

  BeginReadLayerImageData;
  begin

    ReadLayersImageData;

  end;
  EndReadLayerImageData;

  FStream.Position := Next;
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.ReadLayerRecord(var LayerInfo: TLayerInfo): TCustomPhotoshopLayer;

  function MapBlendMode(const ABlendMode: AnsiString): TPSDLayerBlendMode;
  begin
    for Result := Low(TPSDLayerBlendMode) to High(TPSDLayerBlendMode) do
      if (PSDBlendModeMapping[Result]^= ABlendMode) then
        exit;

    Result := lbmNormal;
  end;

var
  Channels: Word;
  Component: SmallInt;
  i: integer;
  ExtraDataSize: Cardinal;
  ExtraDataPos: Int64;
  Signature: AnsiString;
  BlendMode: AnsiString;
  Key: AnsiString;
  Size: Cardinal;
  Next: Int64;
  LayerProperty: TCustomPhotoshopLayerProperty;
begin
(* Layer records
+-------------------------+-----------------------------------------------------+
| Length                  | Description                                         |
+-------------------------+-----------------------------------------------------+
| 4 * 4                   | Rectangle containing the contents of the layer.     |
|                         | Specified as top, left, bottom, right coordinates   |
+-------------------------+-----------------------------------------------------+
| 2                       | Number of channels in the layer                     |
+-------------------------+-----------------------------------------------------+
| 6 * number of channels  | Channel information. Six bytes per channel,         |
|                         | consisting of:                                      |
|                         | 2 bytes for Channel ID; See PSD_MASK_* constants.   |
|                         | 4 bytes for length of corresponding channel data.   |
|                         |   See [Channel image data] for structure of channel |
|                         |   data.                                             |
+-------------------------+-----------------------------------------------------+
| 4                       | Blend mode signature: '8BIM'                        |
+-------------------------+-----------------------------------------------------+
| 4                       | Blend mode key:                                     |
|                         | See PSDBlendModeMapping array for supported values. |
+-------------------------+-----------------------------------------------------+
| 1                       | Opacity. 0 = transparent ... 255 = opaque           |
+-------------------------+-----------------------------------------------------+
| 1                       | Clipping: 0 = base, 1 = non-base                    |
+-------------------------+-----------------------------------------------------+
| 1                       | Flags:                                              |
|                         |   bit 0 = transparency protected;                   |
|                         |   bit 1 = visible;                                  |
|                         |   bit 2 = obsolete;                                 |
|                         |   bit 3 = 1 for Photoshop 5.0 and later, tells if   |
|                         |           bit 4 has useful information;             |
|                         |   bit 4 = pixel data irrelevant to appearance of    |
|                         |           document                                  |
+-------------------------+-----------------------------------------------------+
| 1                       | Filler (zero)                                       |
+-------------------------+-----------------------------------------------------+
| 4                       | Length of the extra data field ( = the total length |
|                         | of the next five fields).                           |
+-------------------------+-----------------------------------------------------+
| Variable                | Layer mask data: See [Layer mask / adjustment layer |
|                         | data] for structure. Can be 40 bytes, 24 bytes, or 4|
|                         | bytes if no layer mask.                             |
+-------------------------+-----------------------------------------------------+
| Variable                | Layer blending ranges: See [Layer blending ranges   |
|                         | data].                                              |
+-------------------------+-----------------------------------------------------+
| Variable                | Layer name: Pascal string, padded to a multiple of 4|
|                         | bytes.                                              |
+-------------------------+-----------------------------------------------------+
Note 1: The documentation contains a bug in the description of "Length of the extra
data field"; The description states that the field contains "the total length of
the next five fields", but there are only three fields after this field. This
means that the "extra data field" is in effect partially undocumented.
As far as I can tell, the "extra data field" is actually the "Additional Layer
Information" section.
Note 2: The documentation contains a bug in the description of the "visible"
flags bit; If the bit is set it actually indicates that the layer is "hidden".

Layer mask / adjustment layer data
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of the section.                                             |
+----------+--------------------------------------------------------------------+
| Variable | Not implemented.                                                   |
+----------+--------------------------------------------------------------------+

Layer blending ranges data
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of the section.                                             |
+----------+--------------------------------------------------------------------+
| Variable | Not implemented.                                                   |
+----------+--------------------------------------------------------------------+

Additional Layer Information
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Signature: '8BIM' or '8B64'                                        |
+----------+--------------------------------------------------------------------+
| 4        | Key: a 4-character code (not documented here)                      |
+----------+--------------------------------------------------------------------+
| 4        | Length of data below, rounded up to an even byte count.            |
|          | Beware:                                                            |
|          | - The data written by PhotoShop is always 4-byte aligned,          |
|          |   regardless of the length alignment.                              |
|          | - Although the length is almost always 4-byte aligned, it is not   |
|          |   guaranteed; Some keys, such as 'LMsk' have been seen with 2-byte |
|          |   aligned length, while others, such as 'Lr16' and 'Lr32' have been|
|          |   seen with unaligned lengths.                                     |
+----------+--------------------------------------------------------------------+
| Variable | Data (not documented here)                                         |
+----------+--------------------------------------------------------------------+
*)

  Result := TPhotoshopBitmapLayer32.Create(FDocument);
  try
    Result.Top := ReadCardinal;
    Result.Left := ReadCardinal;
    Result.Height := integer(ReadCardinal) - Result.Top;
    Result.Width := integer(ReadCardinal) - Result.Left;

    Channels := ReadWord;

    // Read the FChannelInfo array
    LayerInfo := Default(TLayerInfo);
    Setlength(LayerInfo.Channels, Channels);

    for i := 0 to Channels-1 do
    begin
      Component := SmallInt(ReadWord);
      Size := ReadCardinal;

      SetLayerChannelInfo(LayerInfo, i, Component, Size);
    end;

    Signature := ReadAnsiString(4);
    if (Signature <> '8BIM') then
      raise EPhotoshopDocument.Create('Invalid layer data signature');
    BlendMode := ReadAnsiString(4);
    Result.BlendMode := MapBlendMode(BlendMode);
    Result.Opacity := ReadByte;
    Result.Clipping := (ReadByte <> 0);
    Result.Options := TPSDLayerOptions(Ord(ReadByte));
    Skip(1); // Reserved byte; Must be zero

    ExtraDataSize := ReadCardinal;
    ExtraDataPos := FStream.Position;

    // Result mask
    Skip(ReadCardinal);

    // Blending ranges
    Skip(ReadCardinal);

    Result.Name := string(ReadPascalAnsiString(4));

    // Additional Layer Information
    while (FStream.Position < ExtraDataPos + ExtraDataSize) do
    begin
      Signature := ReadAnsiString(4);
      if (Signature <> '8BIM') then
        raise EPhotoshopDocument.Create('Invalid Additional Layer Information signature');

      Key := ReadAnsiString(4);
      Size := ReadCardinal;

      Next := FStream.Position + Size;

      if (Key = PSD_KEY_UnicodeLayerName) then
      begin
        Result.Name := ReadUnicodeString;
        LayerProperty := TPhotoshopLayerCracker(Result).AddLayerProperty(TPhotoshopLayerPropertyUnicodeName, Key);
        TPhotoshopLayerPropertyUnicodeName(LayerProperty).Name := Result.Name;
        FStream.Position := Next;
      end else
      begin
        LayerProperty := TPhotoshopLayerCracker(Result).AddLayerProperty(TPhotoshopLayerProperty, Key);
        TPhotoshopLayerProperty(LayerProperty).Data := ReadBytes(Size);
      end;
    end;

    FStream.Position := ExtraDataPos + ExtraDataSize;

  except
    Result.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayersImageData;
var
  i: Integer;
  StreamPos: Int64;
  NextPos: Int64;
  Layer: TCustomPhotoshopBitmapLayer32;
begin
  NextPos := FStream.Position;

  for i := 0 to FLayers.Count - 1 do
  begin
    StreamPos := NextPos;
    NextPos := StreamPos + FChannelInfo[i].Size;

    Layer := TCustomPhotoshopBitmapLayer32(FLayers[i]);

    // Skip layer if it isn't a bitmap layer
    if not (Layer is TCustomPhotoshopBitmapLayer32) then
      continue;

    // Ensure that the layer has a bitmap...
    if (Layer.Bitmap = nil) and (Layer is TPhotoshopLayer32) then
    begin
      TPhotoshopLayer32(Layer).Bitmap := TBitmap32.Create;
      TPhotoshopLayer32(Layer).OwnsBitmap := True;
    end;
    // ...and that the bitmap has the correct size
    // Note that we clear the bitmap even though we will continue to
    // read the pixels from the PSD, because the PSD might not contain
    // data for all the channels.
    Layer.Bitmap.SetSize(Layer.LayerWidth, Layer.LayerHeight);

    BeginReadChannelImageData(Layer, FChannelInfo[i]);
    begin

      ReadChannelImageData(Layer, FChannelInfo[i]);

    end;
    EndReadChannelImageData(Layer, FChannelInfo[i]);
  end;

  FStream.Position := NextPos;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.BeginReadLayerImageData;
begin
end;

procedure TPhotoshopDocumentReaderHelper.EndReadLayerImageData;
begin
  SetLength(FAlphaChannelBuffer, 0);
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
var
  SetChannel: TPSDChannelSetterDelegate;
  RowData: TBytes;
  i: Integer;
  StreamPos: Int64;
  Compression: Word;
  ChannelReader: TPSDChannelReaderDelegate;
  ColorComponent: TColor32Component;
begin
(* Channel image data
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 2        | Compression.                                                       |
|          |   0 = Raw Data, 1 = RLE compressed, 2 = ZIP without prediction,    |
|          |   3 = ZIP with prediction.                                         |
+----------+--------------------------------------------------------------------+
| Variable | Image data.                                                        |
|          | If the compression code is 0, the image data is just the raw image |
|          | data, whose size is calculated as: (Bottom-Top)*(Right-Left)       |
|          | (from the first field in [Layer records]).                         |
|          | If the compression code is 1, the image data starts with the byte  |
|          | counts for all the scan lines in the channel (Bottom-Top), with    |
|          | each count stored as a two-byte value. The RLE compressed data     |
|          | follows, with each scan line compressed separately. The RLE        |
|          | compression is the same compression algorithm used by the Macintosh|
|          | ROM routine PackBits, and the TIFF standard.                       |
|          | If the layer's size, and therefore the data, is odd, a pad byte    |
|          | will be inserted at the end of the row.                            |
|          | If the layer is an adjustment layer, the channel data is undefined |
|          | (probably all white).                                              |
|          | Note: PhotoShop doesn't actually apply the padding.                |
+----------+--------------------------------------------------------------------+
*)

  SetChannel := GetSetChannel();
  SetLength(RowData, BytesPerScanline(Layer.Width));

  for i := 0 to High(LayerInfo.Channels) do
  begin
    StreamPos := FStream.Position;

    if (LayerInfo.Channels[i].HasColorComponent) then
    begin
      ColorComponent := LayerInfo.Channels[i].ColorComponent;

      Compression := ReadWord;

      if (not FHasCompression) then
      begin
        FCompression := TPSDLayerCompression(Compression);
        FHasCompression := True;
      end;

      ChannelReader := GetChannelReader(TPSDLayerCompression(Compression));

      ChannelReader(FStream, ColorComponent, Layer, RowData, SetChannel);
    end;

    FStream.Position := StreamPos + LayerInfo.Channels[i].Size;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.BeginReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
var
  Size: integer;
begin
  if (not LayerInfo.HasExtra) then
    exit;

  Size := Layer.Height * BytesPerScanline(Layer.Width);
  if (Length(FAlphaChannelBuffer) < Size) then
    SetLength(FAlphaChannelBuffer, Size);
end;

procedure TPhotoshopDocumentReaderHelper.EndReadChannelImageData(Layer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo);
var
  i: integer;
  PixelCount: integer;
  HasAlpha: boolean;
  Offset: integer;
begin
  PixelCount := Layer.Bitmap.Width * Layer.Bitmap.Height;

  HasAlpha := (ccAlpha in LayerInfo.ColorComponents);

  case FMode of
    PSD_DUOTONE:
      begin
        for i := 0 to PixelCount-1 do
          Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R];
      end;

    PSD_INDEXED:
      begin
{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
        HasAlpha := (FTransparentColorIndex <> -1);
{$endif}
        for i := 0 to PixelCount-1 do
        begin
{$ifdef PSD_PARSE_IMAGE_RESOURCE_SECTION}
          if (TColor32Entry(Layer.Bitmap.Bits[i]).R <> FTransparentColorIndex) then
            Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R]
          else
            Layer.Bitmap.Bits[i] := 0;
{$else}
          Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R]
{$endif}
        end;
      end;

    PSD_GRAYSCALE:
      begin
        if (HasAlpha) then
        begin
          for i := 0 to PixelCount-1 do
            Layer.Bitmap.Bits[i] := SetAlpha(FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R], TColor32Entry(Layer.Bitmap.Bits[i]).A);
        end else
        begin
          for i := 0 to PixelCount-1 do
            Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R];
        end;
      end;

    PSD_BITMAP: // Monochrome; 0=white, 1=black
      begin
        for i := 0 to PixelCount-1 do
          if (TColor32Entry(Layer.Bitmap.Bits[i]).R = 0) then
            Layer.Bitmap.Bits[i] := clWhite32
          else
            Layer.Bitmap.Bits[i] := clBlack32;
      end;

    PSD_MULTICHANNEL,
    PSD_RGB:
      begin
      end;

    PSD_CMYK:
      begin
        HasAlpha := LayerInfo.HasExtra;
        // Note that this handles both CMYK and CMY.
        // In case the PSD only contains CMY (i.e. 3 channels), the K channel will
        // already have been cleared to zero, which is what we want.
        for i := 0 to PixelCount-1 do
          CMYKtoColor32(TColorCMYK(Layer.Bitmap.Bits[i]), TColor32Entry(Layer.Bitmap.Bits[i]));
      end;

    PSD_LAB:
      begin
        for i := 0 to PixelCount-1 do
          LABtoColor32(TColorLAB(Layer.Bitmap.Bits[i]), TColor32Entry(Layer.Bitmap.Bits[i]));
      end;

  end;

  // Apply the layer alpha if it resides in the "extra" buffer
  if (LayerInfo.HasExtra) then
  begin
    Offset := 0;
    for i := 0 to Layer.Height-1 do
    begin
      TPhotoshopLayerCracker(Layer).SetChannelScanLine(ccAlpha, i, FAlphaChannelBuffer[Offset]);
      Inc(Offset, BytesPerScanline(Layer.Width));
    end;
  end else
  begin
    // Set alpha if none of the channels contained it. This handles RGB->ARGB,
    // LAB without alpha, etc.
    if (not HasAlpha) then
      Layer.Bitmap.ResetAlpha(255);
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadCompositeImage;
var
  PlanarChannelOrder: array of SmallInt;
  ChannelCount: integer;
  Compression: Word;
  BitmapReader: TPSDBitmapReaderDelegate;
  SetChannel: TPSDChannelSetterDelegate;
  ScanlineBuffer: TBytes;
  Layer: TPhotoshopBitmapLayer32;
  LayerInfo: TLayerInfo;
  i: integer;
  Channel: SmallInt;
begin
(* Image Data Section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 2        | Compression method:                                                |
|          |   0 = Raw image data.                                              |
|          |   1 = RLE compressed the image data starts with the byte counts for|
|          |       all the scan lines (rows * channels), with each count stored |
|          |       as a two-byte value. The RLE compressed data follows, with   |
|          |       each scan line compressed separately. The RLE compression is |
|          |       the same compression algorithm used by the Macintosh ROM     |
|          |       routine PackBits, and the TIFF standard.                     |
|          |   2 = ZIP without prediction.                                      |
|          |   3 = ZIP with prediction.                                         |
+----------+--------------------------------------------------------------------+
| Variable | The image data. Planar order = RRR GGG BBB, etc.                   |
+----------+--------------------------------------------------------------------+
*)

  // As I interpret the PSD specs, if the file doesn't contain any layers,
  // the composite image we read here should become a layer. If the file
  // does contain layers, then the image is a composite of the layers.

  // Planar order depends on mode
  case FMode of
    PSD_DUOTONE,
    PSD_GRAYSCALE:
      PlanarChannelOrder := [PSD_MASK_RED];

    PSD_BITMAP: // Monochrome; Pixel set is white
      PlanarChannelOrder := [PSD_MASK_RED, PSD_MASK_ALPHA];

    PSD_INDEXED:
      PlanarChannelOrder := [PSD_MASK_RED, PSD_MASK_ALPHA];

    PSD_MULTICHANNEL,
    PSD_RGB:
      PlanarChannelOrder := [PSD_MASK_RED, PSD_MASK_GREEN, PSD_MASK_BLUE, PSD_MASK_ALPHA];

    PSD_CMYK:
      PlanarChannelOrder := [PSD_MASK_RED, PSD_MASK_GREEN, PSD_MASK_BLUE, PSD_MASK_BLACK, PSD_MASK_ALPHA];

    PSD_LAB:
      PlanarChannelOrder := [PSD_MASK_RED, PSD_MASK_GREEN, PSD_MASK_BLUE, PSD_MASK_ALPHA];

  else
    PlanarChannelOrder := [];
  end;

  ChannelCount := Min(FDocument.Channels, Length(PlanarChannelOrder));

  Compression := ReadWord;

  if (not FHasCompression) then
  begin
    FCompression := TPSDLayerCompression(Compression);
    FHasCompression := True;
  end;

  LayerInfo := Default(TLayerInfo);
  SetLength(LayerInfo.Channels, ChannelCount);

  for i := 0 to High(LayerInfo.Channels) do
  begin
    Channel := PlanarChannelOrder[i];
    SetLayerChannelInfo(LayerInfo, i, Channel);
  end;

  Layer := TPhotoshopBitmapLayer32.Create;
  try
    Layer.Width := FDocument.Width;
    Layer.Height := FDocument.Height;

    Layer.Compression := TPSDLayerCompression(Compression);

    if (FDocument.Layers.Count = 0) then
      Layer.Document := FDocument
    else
      FDocument.Background := Layer;
  except
    Layer.Free;
    raise;
  end;

  SetChannel := GetSetChannel();
  BitmapReader := GetBitmapReader(Layer);

  BeginReadLayerImageData;
  begin
    BeginReadChannelImageData(Layer, LayerInfo);
    begin

      SetLength(ScanlineBuffer, BytesPerScanLine(Layer.Width));

      BitmapReader(FStream, Layer, LayerInfo, ScanlineBuffer, SetChannel);

      SetLength(ScanlineBuffer, 0);

    end;
    EndReadChannelImageData(Layer, LayerInfo);
  end;
  EndReadLayerImageData;

end;

//------------------------------------------------------------------------------
//
//      Scanline decompression
//
//------------------------------------------------------------------------------
class function TPhotoshopDocumentReaderHelper.GetChannelReader(Compression: TPSDLayerCompression): TPSDChannelReaderDelegate;
begin
  case Compression of
    lcRLE:
      Result := CompressionRLE.ReadChannel;

    lcZIP:
      Result := CompressionZIP.ReadChannel;

    lcRAW:
      Result := CompressionRAW.ReadChannel;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported compression method: %d', [Ord(Compression)]);
  end;
end;

class function TPhotoshopDocumentReaderHelper.GetBitmapReader(ALayer: TCustomPhotoshopLayer): TPSDBitmapReaderDelegate;
begin
  case ALayer.Compression of
    lcRLE:
      Result := CompressionRLE.ReadBitmap;

    lcZIP:
      Result := CompressionZIP.ReadBitmap;

    lcRAW:
      Result := CompressionRAW.ReadBitmap;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported compression method: %d', [Ord(ALayer.Compression)]);
  end;
end;

//------------------------------------------------------------------------------
// RAW compression (i.e. no compression)
//------------------------------------------------------------------------------
class procedure TPhotoshopDocumentReaderHelper.CompressionRAW.ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  i: Integer;
begin
  for i := 0 to ALayer.Height - 1 do
  begin
    AStream.Read(ABuffer[0], Length(ABuffer));
    ASetChannel(ALayer, AChannel, i, ABuffer);
  end;
end;

class procedure TPhotoshopDocumentReaderHelper.CompressionRAW.ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  i: integer;
begin
  for i := 0 to High(LayerInfo.Channels) do
    ReadChannel(AStream, LayerInfo.Channels[i].ColorComponent, ALayer, ABuffer, ASetChannel);
end;

//------------------------------------------------------------------------------
// RLE compression (PackBit)
//------------------------------------------------------------------------------
class procedure TPhotoshopDocumentReaderHelper.CompressionRLE.Decode(AStream: TStream; const ABuffer: TBytes);
var
  RowOffset: Integer;
  Header: Byte;
  Filler: Byte;
  Size: integer;
begin
  RowOffset := 0;

  // PackBits decoder
  while (RowOffset <= High(ABuffer)) do
  begin
    Header := BigEndian.ReadByte(AStream);

    if (Header < 128) then
    begin
      // Stream of bytes
      Size := Header + 1;

      if (RowOffset + Size > High(ABuffer)) then
        Size := Length(ABuffer) - RowOffset; // Guard against buffer overrun

      AStream.Read(ABuffer[RowOffset], Size);
      Inc(RowOffset, Size);
    end else
    if (Header > 128) then
    begin
      // Repeat of byte
      Filler := BigEndian.ReadByte(AStream);
      Size := 257 - Header;

      if (RowOffset + Size > High(ABuffer)) then
        Size := Length(ABuffer) - RowOffset; // Guard against buffer overrun

      FillChar(ABuffer[RowOffset], Size, Filler);
      Inc(RowOffset, Size);
    end;
  end;
end;

class procedure TPhotoshopDocumentReaderHelper.CompressionRLE.ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  RowSizes: array of Word;
  i: Integer;
  StartPos: Int64;
begin
  SetLength(RowSizes, ALayer.Height);
  for i := 0 to High(RowSizes) do
    RowSizes[i] := BigEndian.ReadWord(AStream);

  for i := 0 to ALayer.Height - 1 do
  begin
    StartPos := AStream.Position;

    Decode(AStream, ABuffer);

    ASetChannel(ALayer, AChannel, i, ABuffer);

    AStream.Position := StartPos + RowSizes[i];
  end;
end;

class procedure TPhotoshopDocumentReaderHelper.CompressionRLE.ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  RowTable: array of Word;
  Channel: integer;
  i: integer;
  RowIndex: integer;
  StartPos: Int64;
  RowSize: integer;
begin
  SetLength(RowTable, ALayer.Height * ALayer.Document.Channels);

  AStream.Read(RowTable[0], Length(RowTable) * SizeOf(Word));

  RowIndex := 0;

  for Channel := 0 to High(LayerInfo.Channels) do
    for i := 0 to ALayer.Height - 1 do
    begin
      StartPos := AStream.Position;

      Decode(AStream, ABuffer);
      ASetChannel(ALayer, LayerInfo.Channels[Channel].ColorComponent, i, ABuffer);

      RowSize := Swap16(RowTable[RowIndex]);
      Inc(RowIndex);

      AStream.Position := StartPos + RowSize;
    end;
end;

//------------------------------------------------------------------------------
// ZIP compression
//------------------------------------------------------------------------------
class procedure TPhotoshopDocumentReaderHelper.CompressionZIP.ReadChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopBitmapLayer32; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  i: Integer;
  Stream: TDecompressionStream;
begin
  Stream := TDecompressionStream.Create(AStream);
  try
    for i := 0 to ALayer.Height - 1 do
    begin
      Stream.Read(ABuffer[0], Length(ABuffer));
      ASetChannel(ALayer, AChannel, i, ABuffer);
    end;
  finally
    Stream.Free;
  end;
end;

class procedure TPhotoshopDocumentReaderHelper.CompressionZIP.ReadBitmap(AStream: TStream; ALayer: TCustomPhotoshopBitmapLayer32; const LayerInfo: TLayerInfo; const ABuffer: TBytes; ASetChannel: TPSDChannelSetterDelegate);
var
  Stream: TStream;
  Channel: integer;
  i: integer;
begin
  Stream := TDecompressionStream.Create(AStream);
  try

    for Channel := 0 to High(LayerInfo.Channels) do
      for i := 0 to ALayer.Height - 1 do
      begin
        Stream.Read(ABuffer[0], Length(ABuffer));
        ASetChannel(ALayer, LayerInfo.Channels[Channel].ColorComponent, i, ABuffer);
      end;

  finally
    Stream.Free;
  end;
end;



//------------------------------------------------------------------------------
//
//      PhotoshopDocumentReader
//
//------------------------------------------------------------------------------
class procedure PhotoshopDocumentReader.LoadFromStream(ADocument: TPhotoshopDocument; AStream: TStream);
var
  Reader: TPhotoshopDocumentReaderHelper;
begin
  ADocument.Clear;

  Reader := TPhotoshopDocumentReaderHelper.Create(ADocument, AStream);
  try
    Reader.Read;
  finally
    Reader.Free;
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
{$ifdef FPC}
  TPhotoshopDocumentReaderHelper.ccExtra := TColor32Component(255);
{$endif}
end.
