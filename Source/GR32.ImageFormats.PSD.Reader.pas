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
//      TPhotoshopDocumentReaderHelper
//
//------------------------------------------------------------------------------
// Parses a PSD file and constructs a TPhotoshopDocument from it.
//------------------------------------------------------------------------------
type
  TPhotoshopDocumentReaderHelper = class
  private type
    TChannelInfo = record
      ColorComponent: TColor32Component;
      Size: Cardinal; // Size of channel in stream
    end;

    TLayerInfo = record
      Size: Cardinal;
      Channels: TArray<TChannelInfo>; // One TChannelInfo per channel
    end;

    TChannelInfoList = TArray<TLayerInfo>; // One TLayerInfo per layer
  private
    FStream: TStream;
    FDocument: TPhotoshopDocument;
    FLayers: TList<TCustomPhotoshopLayer>;
    FChannels: integer;
    FChannelInfo: TChannelInfoList;
  protected
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadBytes(Count: integer): TBytes;
    function ReadAnsiString(ACount: integer): AnsiString;
    function ReadPascalAnsiString(APadTo: integer = 1): AnsiString;
    function ReadUnicodeString: string;
    procedure Skip(ACount: Int64);

    function ChannelToColorComponent(AChannel: SmallInt): TColor32Component;
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

procedure TPhotoshopDocumentReaderHelper.Skip(ACount: Int64);
begin
  if (ACount <> 0) then
    FStream.Seek(ACount, soFromCurrent);
end;

//------------------------------------------------------------------------------

function TPhotoshopDocumentReaderHelper.ChannelToColorComponent(AChannel: SmallInt): TColor32Component;
begin
  case AChannel of
    PSD_MASK_ALPHA:     Result := ccAlpha;
    PSD_MASK_RED:       Result := ccRed;
    PSD_MASK_GREEN:     Result := ccGreen;
    PSD_MASK_BLUE:      Result := ccBlue;
  else
    Result := ccRed;
  end;
end;

procedure TPhotoshopDocumentReaderHelper.SetChannel(Layer: TCustomPhotoshopBitmapLayer32; AChannel: TColor32Component; ALine: integer; const ABytes: TBytes);
begin
  TPhotoshopLayerCracker(Layer).SetChannelScanLine(AChannel, ALine, ABytes[0]);
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
  Channels: Word;
  Depth: Word;
  Mode: Word;
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
|        |   Multichannel = 7; Duotone = 8; Lab = 9. |
+--------+----------------------------------------------------------------------+
*)

  Signature := ReadAnsiString(4);
  if (Signature <> '8BPS') then
    raise EPhotoshopDocument.Create('Invalid PSD signature');

  Version := ReadWord;
  if (Version <> 1) then
    raise EPhotoshopDocument.CreateFmt('Unsupported PSD version: %d', [Version]);

  Skip(6);

  Channels := ReadWord;
  if (not (Channels in [1, 3, 4])) then
    raise EPhotoshopDocument.CreateFmt('Unsupported number of channels: %d', [Channels]);
  FChannels := Channels;

  FDocument.Height := ReadCardinal;
  FDocument.Width := ReadCardinal;

  Depth := ReadWord;
  if (Depth <> 8) then
    raise EPhotoshopDocument.CreateFmt('Unsupported bit depth: %d', [Depth]);

  Mode := ReadWord;
  case Channels of
    1:
      if (Mode <> PSD_GRAYSCALE) then
        raise EPhotoshopDocument.CreateFmt('Unsupported color mode: %d', [Mode]);

    3, 4:
      if (Mode <> PSD_RGB) then
        raise EPhotoshopDocument.CreateFmt('Unsupported color mode: %d', [Mode]);
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadColorModeData;
var
  Size: Cardinal;
begin
(* Color Mode Data Section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | The length of the following color data.                            |
+----------+--------------------------------------------------------------------+
| Variable | The color data; Not currently implemented.                         |
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
  if (Size > 0) then
    Skip(Size);
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadImageResources;
var
  Size: Cardinal;
begin
(* Image Resources Section
+----------+--------------------------------------------------------------------+
| Length   | Description                                                        |
+----------+--------------------------------------------------------------------+
| 4        | Length of the layer and mask information section.                  |
+----------+--------------------------------------------------------------------+
| Variable | Image resources; Not implemented.                                  |
+----------+--------------------------------------------------------------------+
*)

  Size := ReadCardinal;
  if (Size > 0) then
    Skip(Size);
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

  Count := ReadWord;
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
  i: integer;
  ExtraDataSize: Cardinal;
  ExtraDataPos: Int64;
  Signature: AnsiString;
  BlendMode: AnsiString;
  Key: AnsiString;
  Size: Cardinal;
  LayerProperty: TCustomPhotoshopLayerProperty;
type
  TLayerHeader = record
    Top: Cardinal;
    Left: Cardinal;
    Height: Cardinal;
    Width: Cardinal;
    LayerInfo: TLayerInfo;
    BlendMode: TPSDLayerBlendMode;
    Opacity: Byte;
    Clipping: boolean;
    Options: TPSDLayerOptions;
    Name: string;
  end;

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
(*
    if (Channels > 4) then
      raise EPhotoshopDocument.CreateFmt('Unsupported number of channels: %d', [Channels]);
*)

    // Read the FChannelInfo array
    Setlength(FChannelInfo[LayerIndex].Channels, Channels);
    FChannelInfo[LayerIndex].Size := 0;
    for i := 0 to Channels-1 do
    begin
      FChannelInfo[LayerIndex].Channels[i].ColorComponent := ChannelToColorComponent(SmallInt(ReadWord));
      FChannelInfo[LayerIndex].Channels[i].Size := ReadCardinal;

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
var
  i: Integer;
  StreamPos: Int64;
  Layer: TCustomPhotoshopBitmapLayer32;
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
    Layer.Bitmap.SetSize(Layer.LayerWidth, Layer.LayerHeight);

    ReadLayerImageData(Layer);

    FStream.Position := StreamPos + FChannelInfo[i].Size;

    // Postprocess layer image channels into ARGB
    PostProcessLayerImageData(Layer);
  end;
end;

//------------------------------------------------------------------------------

procedure TPhotoshopDocumentReaderHelper.ReadLayerImageData(Layer: TCustomPhotoshopBitmapLayer32);
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

  for i := 0 to FChannels - 1 do
  begin
    StreamPos := FStream.Position;

    ColorComponent := FChannelInfo[Layer.Index].Channels[i].ColorComponent;

    Compression := ReadWord;

    case TPSDLayerCompression(Compression) of
      lcRAW: ReadChannelDataRaw(Layer, ColorComponent);
      lcRLE: ReadChannelDataRLE(Layer, ColorComponent);
      lcZIP: ReadChannelDataZIP(Layer, ColorComponent);
    else
      raise EPhotoshopDocument.CreateFmt('Unsupported compression: %d', [Compression]);
    end;

    FStream.Position := StreamPos + FChannelInfo[Layer.Index].Channels[i].Size;
  end;
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
  n: integer;
  p: PColor32Entry;
begin
  case FChannels of
    1: // R -> ARGB (grayscale)
      begin
        n := Layer.Bitmap.Width * Layer.Bitmap.Height;
        p := PColor32Entry(Layer.Bitmap.Bits);
        while (n > 0) do
        begin
          // For PSD_GRAYSCALE images, the pixel value is stored in the red channel
          p.G := p.R;
          p.B := p.R;
          p.A := 255;

          Inc(p);
          Dec(n);
        end;
      end;

    3: // RGB -> ARGB
      Layer.Bitmap.ResetAlpha(255);
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
