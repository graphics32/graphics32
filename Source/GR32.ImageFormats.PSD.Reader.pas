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

// PSD_VALIDATE_IMAGE_RESOURCE_SECTION
// If defined, the Image Resource section is parsed (the information in it
// isn't used for anything yet).
{$define PSD_VALIDATE_IMAGE_RESOURCE_SECTION}

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
  GR32,
  GR32.BigEndian,
  GR32.ImageFormats.PSD.Types;

type
  TPhotoshopLayerCracker = class(TCustomPhotoshopLayer);

//------------------------------------------------------------------------------
//
//      Color conversion
//
//------------------------------------------------------------------------------
procedure CMYKtoColor32(var AColor: TColor32Entry);
begin
  // Note: This is an approximation; Proper conversion isn't possible without a color profile
  AColor.R := Round(255 * (1 - AColor.R / 255) * (1 - AColor.A / 255));
  AColor.G := Round(255 * (1 - AColor.G / 255) * (1 - AColor.A / 255));
  AColor.B := Round(255 * (1 - AColor.B / 255) * (1 - AColor.A / 255));
  AColor.A := 255;
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
  private const
    ccExtra: TColor32Component = TColor32Component(-1);
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
  private
    FStream: TStream;
    FDocument: TPhotoshopDocument;
    FLayers: TList<TCustomPhotoshopLayer>;
    FMode: Word;
    FChannels: integer;
    FDepth: Word;
    FChannelInfo: TChannelInfoList;
    FPalette: array[byte] of TColor32;
    FAlphaChannelBuffer: TBytes;
  protected
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadBytes(Count: integer): TBytes;
    function ReadAnsiString(ACount: integer): AnsiString;
    function ReadPascalAnsiString(APadTo: integer = 1): AnsiString;
    function ReadUnicodeString: string;
    function Padding(Value: Cardinal; Alignment: Cardinal = 4): Cardinal;
    procedure Skip(ACount: Int64);

    function TryChannelToColorComponent(AChannel: SmallInt; var AComponent: TColor32Component): boolean;
    procedure SetChannel(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);

    procedure ReadHeader;
    procedure ReadColorModeData;
    procedure ReadImageResources;
    procedure ReadLayerAndMaskInfo;
    procedure   ReadLayersInfo;
    function      ReadLayerRecord: TCustomPhotoshopLayer;
    procedure     ReadLayersImageData;
    procedure       ReadLayerImageData(Layer: TCustomPhotoshopBitmapLayer32);
    procedure         ReadChannelDataRLE(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
    procedure         ReadChannelDataZIP(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
    procedure         ReadChannelDataRaw(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
    procedure       PostProcessLayerImageData(Layer: TCustomPhotoshopBitmapLayer32);
    procedure ReadCompositeImage;

    procedure Read;
  public
    constructor Create(ADocument: TPhotoshopDocument; AStream: TStream);
    destructor Destroy; override;
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

procedure TPhotoshopDocumentReaderHelper.SetChannel(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
begin
  if (AChannel <> ccExtra) then
    TPhotoshopLayerCracker(Layer).SetChannelScanLine(AChannel, ALine, ABytes[0])
  else
    Move(ABytes[0], FAlphaChannelBuffer[ALine * Length(ABytes)], Length(ABytes));
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.Read;
begin
  ReadHeader;
  ReadColorModeData;
  ReadImageResources;
  ReadLayerAndMaskInfo;
  ReadCompositeImage;
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

  Skip(6);

  FChannels := ReadWord;

  FDocument.Height := ReadCardinal;
  FDocument.Width := ReadCardinal;

  FDepth := ReadWord;

  SupportedBits := [1, 8, 16, 32];
  SupportedChannels := [1..56];

  FMode := ReadWord;
  case FMode of
    PSD_DUOTONE, // DuoTone isn't really supported; Treat it as grayscale
    PSD_GRAYSCALE:
      begin
        SupportedChannels := [1, 2];
        if (FChannels = 1) then
          SupportedBits := [8, 16, 32]
        else
          SupportedBits := [8, 16];
      end;

    PSD_BITMAP: // Monochrome; Pixel set is white
      begin
        SupportedChannels := [1];
        SupportedBits := [1];
      end;

    PSD_INDEXED:
      begin
        SupportedChannels := [1];
        SupportedBits := [8];
      end;

    PSD_MULTICHANNEL,
    PSD_RGB:
      begin
        SupportedChannels := [3, 4];
        SupportedBits := [8, 16];
      end;

    PSD_CMYK:
      begin
        SupportedChannels := [4, 5];
        SupportedBits := [8, 16];
      end;

    // PSD_LAB:

  else
    raise EPhotoshopDocument.CreateFmt('Unsupported color mode: %d', [FMode]);
  end;

  if (not (FDepth in SupportedBits)) then
    raise EPhotoshopDocument.CreateFmt('Unsupported bit depth (%d) for mode (%d)', [FDepth, FMode]);

  if (not (FChannels in SupportedChannels)) then
    raise EPhotoshopDocument.CreateFmt('Unsupported number of channels (%d) for mode (%d)', [FChannels, FMode]);
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
{$ifdef PSD_VALIDATE_IMAGE_RESOURCE_SECTION}
  Next: Int64;
  Signature: AnsiString;
  Ident: Word;
  Name: AnsiString;
  DataSize: Cardinal;
  FColorTableCount: Cardinal;
  FTransparentColorIndex: Cardinal;
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

{$ifdef PSD_VALIDATE_IMAGE_RESOURCE_SECTION}
  Next := FStream.Position + Size;

  while (FStream.Position < Next) do
  begin
    Signature := ReadAnsiString(4);
    if (Signature <> '8BIM') then
      raise EPhotoshopDocument.Create('Invalid layer data signature');
    Ident := ReadWord;
    Name := ReadPascalAnsiString(2);
    DataSize := ReadCardinal;
    case Ident of
      1046:
        begin
          FColorTableCount := ReadWord;
          Dec(DataSize, SizeOf(Word));
        end;

      1047:
        begin
          FTransparentColorIndex := ReadWord;
          Dec(DataSize, SizeOf(Word));
        end;
    end;
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

    ReadLayersInfo;

    // Skip "Global layer mask info"
    Size := ReadCardinal;
    if (Size > 0) then
      Skip(Size);

    // Skip "Additional Layer Information"
    FStream.Position := Next;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayersInfo;
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

  // Size FChannelInfo so it can be filled with data by ReadLayerRecord
  SetLength(FChannelInfo, Count);

  for i := 0 to Count - 1 do
  begin
    Layer := ReadLayerRecord;
    FLayers.Add(Layer);
  end;

  ReadLayersImageData;

  FStream.Position := Next;
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.ReadLayerRecord: TCustomPhotoshopLayer;

  function MapBlendMode(const ABlendMode: AnsiString): TPSDLayerBlendMode;
  begin
    for Result := Low(TPSDLayerBlendMode) to High(TPSDLayerBlendMode) do
      if (PSDBlendModeMapping[Result]^= ABlendMode) then
        exit;

    Result := lbmNormal;
  end;

var
  LayerIndex: integer;
  Channels: Word;
  ColorComponent: TColor32Component;
  i: integer;
  ExtraDataSize: Cardinal;
  ExtraDataPos: Int64;
  Signature: AnsiString;
  BlendMode: AnsiString;
  Key: AnsiString;
  Size: Cardinal;
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
Note: The documentation contains a bug in the description of "Length of the extra
data field"; The description states that the field contains "the total length of
the next five fields", but there are only three fields after this field. This
means that the "extra data field" is in effect partially undocumented.
As far as I can tell, the "extra data field" is actually the "Additional Layer
Information" section.

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
| 4        | Length data below, rounded up to an even byte count.               |
+----------+--------------------------------------------------------------------+
| Variable | Data (not documented here)                                         |
+----------+--------------------------------------------------------------------+
*)

  Result := TPhotoshopBitmapLayer32.Create(FDocument);
  try
    LayerIndex := Result.Index;

    Result.Top := ReadCardinal;
    Result.Left := ReadCardinal;
    Result.Height := integer(ReadCardinal) - Result.Top;
    Result.Width := integer(ReadCardinal) - Result.Left;

    Channels := ReadWord;

    // Read the FChannelInfo array
    Setlength(FChannelInfo[LayerIndex].Channels, Channels);
    FChannelInfo[LayerIndex].Size := 0;
    FChannelInfo[LayerIndex].ColorComponents := [];
    FChannelInfo[LayerIndex].HasExtra := False;

    for i := 0 to Channels-1 do
    begin
      FChannelInfo[LayerIndex].Channels[i].Component := SmallInt(ReadWord);
      FChannelInfo[LayerIndex].Channels[i].HasColorComponent := TryChannelToColorComponent(FChannelInfo[LayerIndex].Channels[i].Component, ColorComponent);
      FChannelInfo[LayerIndex].Channels[i].Size := ReadCardinal;

      if (FChannelInfo[LayerIndex].Channels[i].HasColorComponent) then
      begin
        FChannelInfo[LayerIndex].Channels[i].ColorComponent := ColorComponent;

        if (ColorComponent <> ccExtra) then
          Include(FChannelInfo[LayerIndex].ColorComponents, ColorComponent)
        else
          FChannelInfo[LayerIndex].HasExtra := True;
      end;

      Inc(FChannelInfo[LayerIndex].Size, FChannelInfo[LayerIndex].Channels[i].Size);
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

    // Additional Result Information
    while (FStream.Position < ExtraDataPos + ExtraDataSize) do
    begin
      Signature := ReadAnsiString(4);
      if (Signature <> '8BIM') then
        raise EPhotoshopDocument.Create('Invalid layer extra data signature');

      Key := ReadAnsiString(4);
      Size := ReadCardinal;
      LayerProperty := TPhotoshopLayerCracker(Result).AddLayerProperty(TPhotoshopLayerProperty, Key);
      TPhotoshopLayerProperty(LayerProperty).Data := ReadBytes(Size);
    end;

    FStream.Position := ExtraDataPos + ExtraDataSize;

  except
    Result.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayersImageData;

  procedure BeginReadLayerImageData;
  begin
  end;

  procedure EndReadLayerImageData;
  begin
    SetLength(FAlphaChannelBuffer, 0);
  end;

var
  i: Integer;
  StreamPos: Int64;
  Layer: TCustomPhotoshopBitmapLayer32;
begin
  BeginReadLayerImageData;
  begin

    for i := 0 to FLayers.Count - 1 do
    begin
      StreamPos := FStream.Position;

      Layer := TCustomPhotoshopBitmapLayer32(FLayers[i]);

      // Skip layer if it isn't a bitmap layer
      if not (Layer is TCustomPhotoshopBitmapLayer32) then
      begin
        FStream.Position := StreamPos + FChannelInfo[i].Size;
        continue;
      end;

      // Ensure that the layer has a bitmap...
      if (Layer.Bitmap = nil) and (Layer is TPhotoshopLayer32) then
      begin
        TPhotoshopLayer32(Layer).Bitmap := TBitmap32.Create;
        TPhotoshopLayer32(Layer).OwnsBitmap := True;
      end;
      // ...and that the bitmap has the correct size
      Layer.Bitmap.SetSize(Layer.LayerWidth, Layer.LayerHeight, False);

      ReadLayerImageData(Layer);

      FStream.Position := StreamPos + FChannelInfo[i].Size;

      // Postprocess layer image channels into ARGB
      PostProcessLayerImageData(Layer);
    end;

  end;
  EndReadLayerImageData;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayerImageData(Layer: TCustomPhotoshopBitmapLayer32);

  procedure BeginReadChannelData;
  var
    Size: integer;
  begin
    if (not FChannelInfo[Layer.Index].HasExtra) then
      exit;

    Size := Layer.Height * Layer.Width;
    if (Length(FAlphaChannelBuffer) < Size) then
      SetLength(FAlphaChannelBuffer, Size);
  end;

  procedure EndReadChannelData;
  var
    i: integer;
    Offset: integer;
  begin
    if (not FChannelInfo[Layer.Index].HasExtra) then
      exit;

    // Restore alpha channel
    Offset := 0;
    for i := 0 to Layer.Height-1 do
    begin
      TPhotoshopLayerCracker(Layer).SetChannelScanLine(ccAlpha, i, FAlphaChannelBuffer[Offset]);
      Inc(Offset, Layer.Width);
    end;
  end;

var
  i: Integer;
  StreamPos: Int64;
  Compression: Word;
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
+----------+--------------------------------------------------------------------+
*)

  BeginReadChannelData;
  begin

    for i := 0 to High(FChannelInfo[Layer.Index].Channels) do
    begin
      StreamPos := FStream.Position;

      if (FChannelInfo[Layer.Index].Channels[i].HasColorComponent) then
      begin
        ColorComponent := FChannelInfo[Layer.Index].Channels[i].ColorComponent;

        Compression := ReadWord;

        case TPSDLayerCompression(Compression) of
          lcRAW: ReadChannelDataRaw(Layer, ColorComponent);
          lcRLE: ReadChannelDataRLE(Layer, ColorComponent);
          lcZIP: ReadChannelDataZIP(Layer, ColorComponent);
        else
          raise EPhotoshopDocument.CreateFmt('Unsupported compression: %d', [Compression]);
        end;
      end;

      FStream.Position := StreamPos + FChannelInfo[Layer.Index].Channels[i].Size;
    end;

  end;
  EndReadChannelData;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadChannelDataRLE(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
var
  RowSizes: array of Word;
  i: Integer;
  StartPos: Int64;
  RowData: TBytes;
  RowOffset: Integer;
  Header: Byte;
  Filler: Byte;
  Size: integer;
begin
  SetLength(RowSizes, Layer.Height);
  for i := 0 to High(RowSizes) do
    RowSizes[i] := ReadWord;

  SetLength(RowData, Layer.Width);

  for i := 0 to Layer.Height - 1 do
  begin
    StartPos := FStream.Position;
    RowOffset := 0;

    // PackBits decoder
    while (RowOffset <= High(RowData)) do
    begin
      Header := ReadByte;

      if (Header < 128) then
      begin
        // Stream of bytes
        Size := Header + 1;

        if (RowOffset + Size > High(RowData)) then
          Size := Length(RowData) - RowOffset; // Guard against buffer overrun

        FStream.Read(RowData[RowOffset], Size);
        Inc(RowOffset, Size);
      end else
      if (Header > 128) then
      begin
        // Repeat of byte
        Filler := ReadByte;
        Size := 257 - Header;

        if (RowOffset + Size > High(RowData)) then
          Size := Length(RowData) - RowOffset; // Guard against buffer overrun

        FillChar(RowData[RowOffset], Size, Filler);
        Inc(RowOffset, Size);
      end;
    end;

    SetChannel(Layer, ColorComponent, i, RowData);

    FStream.Position := StartPos + RowSizes[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadChannelDataZIP(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
var
  i: Integer;
  RowData: TBytes;
  ZStream: TDecompressionStream;
begin
  SetLength(RowData, Layer.Width);

  ZStream := TDecompressionStream.Create(FStream);
  try
    for i := 0 to High(RowData) do
    begin
      ZStream.Read(RowData[0], Length(RowData));
      SetChannel(Layer, ColorComponent, i, RowData);
    end;
  finally
    ZStream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadChannelDataRaw(Layer: TCustomPhotoshopBitmapLayer32; ColorComponent: TColor32Component);
var
  i: Integer;
  RowData: TBytes;
begin
  SetLength(RowData, Layer.Width);

  for i := 0 to Layer.Height - 1 do
  begin
    FStream.Read(RowData[0], Length(RowData));
    SetChannel(Layer, ColorComponent, i, RowData);
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.PostProcessLayerImageData(Layer: TCustomPhotoshopBitmapLayer32);
var
  i: integer;
  n: integer;
begin
  n := Layer.Bitmap.Width * Layer.Bitmap.Height;

  case FMode of
    PSD_DUOTONE:
      begin
        for i := 0 to n-1 do
          Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R];
      end;

    PSD_INDEXED:
      for i := 0 to n-1 do
        Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R];

    PSD_GRAYSCALE:
      begin
        if (ccAlpha in FChannelInfo[Layer.Index].ColorComponents) then
        begin
          for i := 0 to n-1 do
            Layer.Bitmap.Bits[i] := SetAlpha(FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R], TColor32Entry(Layer.Bitmap.Bits[i]).A);
        end else
        begin
          for i := 0 to n-1 do
            Layer.Bitmap.Bits[i] := FPalette[TColor32Entry(Layer.Bitmap.Bits[i]).R];
        end;
      end;

    PSD_BITMAP: // Monochrome; Pixel set is white
      for i := 0 to n-1 do
        if (TColor32Entry(Layer.Bitmap.Bits[i]).R = 0) then
          Layer.Bitmap.Bits[i] := clWhite32
        else
          Layer.Bitmap.Bits[i] := clBlack32;

    PSD_MULTICHANNEL,
    PSD_RGB:
      // RGB -> ARGB
      if (not (ccAlpha in FChannelInfo[Layer.Index].ColorComponents)) then
        Layer.Bitmap.ResetAlpha(255);

    PSD_CMYK:
      begin
        for i := 0 to n-1 do
          CMYKtoColor32(TColor32Entry(Layer.Bitmap.Bits[i]));
      end;

    PSD_LAB:
      {not yet implemented};

  end;

  // Apply the layer alpha if it resides in the "extra" buffer
  if (FChannelInfo[Layer.Index].HasExtra) then
  begin
    for i := 0 to n-1 do
      TColor32Entry(Layer.Bitmap.Bits[i]).A := FAlphaChannelBuffer[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadCompositeImage;
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
  // This section contains the flattened image data, which appears at the end of the file.
  // We are not currently reading this data, as we are reconstructing the image from the layers.

  // TODO : As I interpret the PSD specs, if the file doesn't contain any layers,
  // we should read the image from here.
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

end.
