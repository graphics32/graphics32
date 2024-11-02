unit GR32.ImageFormats.PSD.Writer;

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
 * Lamdalili
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Anders Melander <anders@melander.dk>
 *
 * ***** END LICENSE BLOCK ***** *)

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

{$include GR32.inc}

uses
  Classes,
  GR32.ImageFormats.PSD;


//------------------------------------------------------------------------------
//
//      TPhotoshopDocumentWriter
//
//------------------------------------------------------------------------------
// Writes a PSD document to a stream
//------------------------------------------------------------------------------
type
  TPhotoshopDocumentWriter = class abstract
  public
    class procedure SaveToStream(ADocument: TPhotoshopDocument; AStream: TStream);
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation

uses
  Generics.Collections,
{$ifndef FPC}
  ZLib,
{$else FPC}
  zstream,
{$endif FPC}
  Math,
  SysUtils,
  GR32,
  GR32_LowLevel,
  GR32.BigEndian,
  GR32.ImageFormats.PSD.Types;

type
  TBytesArray = array of byte;
  TPhotoshopLayerCracker = class(TCustomPhotoshopLayer);

//------------------------------------------------------------------------------
//
//      Scanline compression
//
//------------------------------------------------------------------------------
type
  // Write all channels in one go
  // Used for background bitmap
  TPSDBitmapWriterDelegate = procedure(AStream: TStream; ALayer: TCustomPhotoshopLayer);

  // Write a single channels
  // Used for layer bitmaps
  TPSDChannelWriterDelegate = procedure(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);

const
  // Number of channels
  PSD_CHANNELS = Ord(High(TColor32Component))-Ord(Low(TColor32Component))+1;

{$IFNDEF RGBA_FORMAT}
  PSD_CHANNELS_IDS: array[TColor32Component] of SmallInt = (PSD_MASK_BLUE, PSD_MASK_GREEN, PSD_MASK_RED, PSD_MASK_ALPHA);
{$ELSE}
  PSD_CHANNELS_IDS: array[TColor32Component] of SmallInt = (PSD_MASK_RED, PSD_MASK_GREEN, PSD_MASK_BLUE, PSD_MASK_ALPHA);
{$ENDIF}

const
  // The PSD channels in "planar" order
  PSDPlanarOrder: array[0..PSD_CHANNELS-1] of TColor32Component = (ccRed, ccGreen, ccBlue, ccAlpha);

//------------------------------------------------------------------------------
// RAW compression (i.e. no compression)
//------------------------------------------------------------------------------
type
  CompressionRAW = record
    class function WriteScanline(AStream: TStream; const ABuffer; Width: integer): Cardinal; static;
    class procedure WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer); static;
    class procedure WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer); static;
  end;

class function CompressionRAW.WriteScanline(AStream: TStream; const ABuffer; Width: integer): Cardinal;
begin
  Result := AStream.Write(ABuffer, Width);
end;

class procedure CompressionRAW.WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
var
  i: integer;
begin
  for i := 0 to ALayer.Height - 1 do
  begin
    TPhotoshopLayerCracker(ALayer).GetChannelScanLine(AChannel, i, ABuffer);
    WriteScanline(AStream, ABuffer, ALayer.Width);
  end;
end;

class procedure CompressionRAW.WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer);
var
  ScanLineBuffer: TBytesArray;
  Channel: TColor32Component;
  i: integer;
begin
  SetLength(ScanLineBuffer, ALayer.Width);

  for Channel in PSDPlanarOrder do
    for i := 0 to ALayer.Height - 1 do
    begin
      TPhotoshopLayerCracker(ALayer).GetChannelScanLine(Channel, i, ScanLineBuffer[0]);
      WriteScanline(AStream, ScanLineBuffer[0], ALayer.Width);
    end;
end;

//------------------------------------------------------------------------------
// RLE compression (PackBit)
//------------------------------------------------------------------------------
type
  TPackBitsStream = class(TStream)
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TPackBitsStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TPackBitsStream.Read(var Buffer; Count: Longint): Longint;
begin
  Assert(False, 'Not implemented');
  Result := 0;
end;

function TPackBitsStream.Write(const Buffer; Count: Longint): Longint;
const
  MaxRun = 128;
  // These values are for classic PackBits encoding.
  // Other variants use other values (e.g. PDB uses PackBase=127, PackSign=1)
  PackBase = 257;
  PackSign = -1;
var
  Index: Int64;
  RunCount: Byte;
  RunValue: Byte;
  StartIndex: integer;
begin
  Index := 0;
  Result := 0;

  while (Index < Count) do
  begin
    (*
    ** Always encode 3-byte repeat sequences.
    ** Encode 2-byte repeat sequences only when they are at the start of the block.
    *)

    RunValue := TByteArray(Buffer)[Index];

    if (Index < Count - 1) and (TByteArray(Buffer)[Index] = TByteArray(Buffer)[Index + 1]) then
    begin
      // Do a repeat run
      RunCount := 2; // We already know that we have at least a run of two because of the test above
      Inc(Index, 2);
      while (Index < Count) and (RunValue = TByteArray(Buffer)[Index]) and (RunCount < MaxRun) do
      begin
        Inc(Index);
        Inc(RunCount);
      end;

      // Encode run count
      // RunCount := Byte(PackBase + PackSign * RunCount);
      RunCount := Byte(257 - RunCount);

      FStream.Write(RunCount, 1);
      FStream.Write(RunValue, 1);
      Inc(Result, 2);
    end else
    begin
      // Do a non-repeat run
      RunCount := 0;
      StartIndex := Index;
      while
        // We're at the end; No room for repeat runs
        ((Index + 2 >= Count) and (Index < Count)) or
        // There's at least 3 bytes left and...
        ((Index + 2 < Count) and (
         // Next 2 differ
         (RunValue <> TByteArray(Buffer)[Index + 1]) or
         // Next 2 same, but differs from the third
         (RunValue <> TByteArray(Buffer)[Index + 2]))) do
      begin
        Inc(Index);
        Inc(RunCount);
        if (RunCount = MaxRun) then
          Break;
        RunValue := TByteArray(Buffer)[Index];
      end;

      BigEndian.WriteByte(FStream, RunCount-1);
      FStream.Write(TByteArray(Buffer)[StartIndex], RunCount);
      Inc(Result, RunCount+1);
    end;
  end;
end;

type
  CompressionRLE = record
    class procedure WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer); static;
    class procedure WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer); static;
  end;

class procedure CompressionRLE.WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
var
  i: integer;
  RowTablePos: Int64;
  RowSize: Word;
  RowTable: array of Word;
  SavePos: Int64;
  RLEStream: TStream;
begin
  RowTablePos := AStream.Position;

  // Make room for row table
  AStream.Seek(ALayer.Height * SizeOf(Smallint), soFromCurrent);
  SetLength(RowTable, ALayer.Height);

  RLEStream := TPackBitsStream.Create(AStream);
  try

    for i := 0 to ALayer.Height - 1 do
    begin
      TPhotoshopLayerCracker(ALayer).GetChannelScanLine(AChannel, i, ABuffer);

      RowSize := RLEStream.Write(ABuffer, ALayer.Width);

      RowTable[i] := Swap16(RowSize);
    end;

  finally
    RLEStream.Free;
  end;

  // Rewind and update row table
  SavePos := AStream.Position;
  AStream.Position := RowTablePos;
  AStream.Write(RowTable[0], ALayer.Height * SizeOf(Word));
  AStream.Position := SavePos;
end;

class procedure CompressionRLE.WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer);
var
  ScanLineBuffer: TBytesArray;
  Channel: TColor32Component;
  i: integer;
  RowTablePos: Int64;
  RowSize: Word;
  RowTable: array of Word;
  SavePos: Int64;
  RLEStream: TStream;
begin
  SetLength(ScanLineBuffer, ALayer.Width);
  SetLength(RowTable, ALayer.Height);

  RowTablePos := AStream.Position;
  // Make room for row table (for all channels)
  AStream.Seek(ALayer.Height * SizeOf(Word) * PSD_CHANNELS, soFromCurrent);

  RLEStream := TPackBitsStream.Create(AStream);
  try

    for Channel in PSDPlanarOrder do
    begin

      for i := 0 to ALayer.Height - 1 do
      begin
        TPhotoshopLayerCracker(ALayer).GetChannelScanLine(Channel, i, ScanLineBuffer[0]);

        RowSize := RLEStream.Write(ScanLineBuffer[0], ALayer.Width);

        RowTable[i] := Swap16(RowSize);
      end;

      // Rewind and update row table for the channel
      SavePos := AStream.Position;
      AStream.Position := RowTablePos;
      AStream.Write(RowTable[0], ALayer.Height * SizeOf(Word));
      // Move table pos forward to next channel
      Inc(RowTablePos, ALayer.Height * SizeOf(Word));
      AStream.Position := SavePos;
    end;

  finally
    RLEStream.Free;
  end;
end;

//------------------------------------------------------------------------------
// ZIP compression
//------------------------------------------------------------------------------
type
  CompressionZIP = record
    class procedure WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer); static;
    class procedure WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer); static;
  end;

class procedure CompressionZIP.WriteChannel(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
var
  i: integer;
  Stream: TStream;
begin
  Stream := TCompressionStream.Create(clDefault, AStream);
  try
    for i := 0 to ALayer.Height - 1 do
    begin
      TPhotoshopLayerCracker(ALayer).GetChannelScanLine(AChannel, i, ABuffer);
      Stream.Write(ABuffer, ALayer.Width);
    end;
  finally
    Stream.Free;
  end;
end;

class procedure CompressionZIP.WriteBitmap(AStream: TStream; ALayer: TCustomPhotoshopLayer);
var
  ScanLineBuffer: TBytesArray;
  Stream: TStream;
  Channel: TColor32Component;
  i: integer;
begin
  SetLength(ScanLineBuffer, ALayer.Width);

  Stream := TCompressionStream.Create(clDefault, AStream);
  try
    for Channel in PSDPlanarOrder do
      for i := 0 to ALayer.Height - 1 do
      begin
        TPhotoshopLayerCracker(ALayer).GetChannelScanLine(Channel, i, ScanLineBuffer[0]);
        Stream.Write(ScanLineBuffer[0], ALayer.Width);
      end;
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

function GetLayerWriter(ALayer: TCustomPhotoshopLayer): TPSDChannelWriterDelegate;
begin
  case ALayer.Compression of
    lcRLE:
      Result := CompressionRLE.WriteChannel;

    lcZIP:
      Result := CompressionZIP.WriteChannel;

    lcRAW:
      Result := CompressionRAW.WriteChannel;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported compression method: %d', [Ord(ALayer.Compression)]);
  end;
end;

function GetBitmapWriter(ALayer: TCustomPhotoshopLayer): TPSDBitmapWriterDelegate;
begin
  case ALayer.Compression of
    lcRLE:
      Result := CompressionRLE.WriteBitmap;

    lcZIP:
      Result := CompressionZIP.WriteBitmap;

    lcRAW:
      Result := CompressionRAW.WriteBitmap;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported compression method: %d', [Ord(ALayer.Compression)]);
  end;
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopDocumentWriter
//
//------------------------------------------------------------------------------
class procedure TPhotoshopDocumentWriter.SaveToStream(ADocument: TPhotoshopDocument; AStream: TStream);
var
  SectionsCaptures: TStack<Int64>;

  function Pad(Value: Cardinal; Alignment: Cardinal = 4): integer;
  begin
    Result := (Alignment - (Value and (Alignment - 1))) and (Alignment - 1);
  end;

  function WritePadding(ASize: Cardinal): Cardinal;
  const
    Zero: byte = 0;
  begin
    Result := AStream.Position;
    while (ASize > 0) do
    begin
      AStream.Write(Zero, 1);
      Dec(ASize);
    end;
  end;

  procedure WritePadToAlignment(Value: Cardinal; Alignment: Cardinal = 4);
  begin
    WritePadding(Pad(Value, Alignment));
  end;

  function WriteRawAnsiString(const s: AnsiString): Cardinal;
  begin
    Result := Length(s);
    AStream.Write(PAnsiChar(s)^, Result);
  end;

  function WriteAnsiText(const AText: AnsiString): Cardinal;
  begin
    BigEndian.WriteByte(AStream, Length(AText));
    Result := WriteRawAnsiString(AText) + 1;
  end;

  function WriteUnicodeText(const AText: string): Cardinal;
  var
    c: Char;
  begin
    BigEndian.WriteCardinal(AStream, Length(AText));
    for c in AText do
      BigEndian.WriteWord(AStream, Ord(c));
    c := #0;
    AStream.Write(c, SizeOf(Char));
    Result := (Length(AText)+1) * SizeOf(Char) + SizeOf(Cardinal);
  end;

  procedure WriteBeginSection;
  begin
    BigEndian.WriteCardinal(AStream, 0); // field slot
    SectionsCaptures.Push(AStream.Position);
  end;

  procedure WriteEndSection(Align: Cardinal = 4);
  var
    Size: Cardinal;
    SectionStartPos: Int64;
    SavePos: Int64;
  begin
    SectionStartPos := SectionsCaptures.Pop;
    Size := AStream.Position - SectionStartPos;
    WritePadToAlignment(Size, Align);

    Size := Swap32(AStream.Position - SectionStartPos);

    SavePos := AStream.Position;
    AStream.Position := SectionStartPos - SizeOf(Cardinal); // field slot
    AStream.Write(Size, SizeOf(Size));
    AStream.Position := SavePos;
  end;

  procedure WriteEmptyImage;

    procedure WriteEmptyImageRLE;
    var
      RepeatsCount: integer;
      RemainCount: integer;
      i: integer;
      RowBuffer: array of Word;
    begin
      BigEndian.WriteWord(AStream, PSD_COMPRESSION_RLE);

      // Everything is repeats.
      // How many whole 128 byte repeats do we have?
      RepeatsCount := (ADocument.Width + 127) div 128; // round up
      // How many bytes remaining?
      RemainCount := ADocument.Width mod 128;

      SetLength(RowBuffer, RepeatsCount);

      // Write row table (all 4 channels)
      for i := 0 to ADocument.Height * PSD_CHANNELS - 1 do
        BigEndian.WriteWord(AStream, RepeatsCount * SizeOf(Word));

      (*
      ** Write RGB channels = $xxFFFFFF
      *)
      for i := 0 to RepeatsCount - 1 do
        RowBuffer[i] := Swap16($81FF); // Fill with whole 128 byte repeat runs

      if (RemainCount <> 0) then
        // Replace last entry with the remaining repeat run
        RowBuffer[RepeatsCount - 1] := Swap16(byte(-RemainCount + 1) shl 8 or $FF);

      for i := 0 to ADocument.Height * (PSD_CHANNELS-1) - 1 do
        AStream.Write(RowBuffer[0], RepeatsCount * SizeOf(Word));

      (*
      ** Write A channel = $00xxxxxx
      *)
      for i := 0 to RepeatsCount - 1 do
        RowBuffer[i] := Swap16($8100); // Fill with whole 128 byte repeat runs

      if (RemainCount <> 0) then
        // Replace last entry with the remaining repeat run
        RowBuffer[RepeatsCount - 1] := Swap16(byte(-RemainCount + 1) shl 8 or $00);

      for i := 0 to ADocument.Height - 1 do
        AStream.Write(RowBuffer[0], RepeatsCount * SizeOf(Word));
    end;

    procedure WriteEmptyImageRAW;
    var
      RowBuffer: array of byte;
      i: integer;
    begin
      BigEndian.WriteWord(AStream, PSD_COMPRESSION_NONE); // No compression

      SetLength(RowBuffer, ADocument.Width);

      // Write RGB channels = $xxFFFFFF
      FillChar(RowBuffer[0], ADocument.Width, $FF);
      for i := 0 to (ADocument.Height * (PSD_CHANNELS-1)) - 1 do
        AStream.Write(RowBuffer[0], ADocument.Width);

      // Write A channel = $00xxxxxx
      FillChar(RowBuffer[0], ADocument.Width, $00);
      for i := 0 to ADocument.Height - 1 do
        AStream.Write(RowBuffer[0], ADocument.Width);
    end;

  begin
    // Write an "empty" image containing ARGB=$00FFFFFF
    if (ADocument.Compression = lcRAW) then
      WriteEmptyImageRAW
    else
      WriteEmptyImageRLE;
  end;

  procedure WriteLayerImage(ALayer: TCustomPhotoshopLayer; AChannelsInfoPos: Int64);
  var
    LayerWriter: TPSDChannelWriterDelegate;
    Size: Cardinal;
    Channel: TColor32Component;
    ChannelsInfo: array[TColor32Component] of TPSDChannelInfo;
    ScanLineBuffer: TBytesArray;
    SavePos: Int64;
  begin
    SetLength(ScanLineBuffer, ALayer.Width);

    LayerWriter := GetLayerWriter(ALayer);

    ALayer.BeginScan;
    begin
      for Channel := Low(TColor32Component) to High(TColor32Component) do
      begin
        SavePos := AStream.Position;

        BigEndian.WriteWord(AStream, Ord(ALayer.Compression));
        LayerWriter(AStream, Channel, ALayer, ScanLineBuffer[0]);

        Size := AStream.Position - SavePos;

        ChannelsInfo[Channel].ChannelID := Swap16(Word(PSD_CHANNELS_IDS[Channel]));
        ChannelsInfo[Channel].ChannelSize := Swap32(Size);
      end;
    end;
    ALayer.EndScan;

    // Rewind and update channel table
    SavePos := AStream.Position;
    AStream.Position := AChannelsInfoPos;
    AStream.Write(ChannelsInfo, SizeOf(ChannelsInfo));
    AStream.Position := SavePos;
  end;

  procedure WriteLayerName(const AName: AnsiString; Align: Cardinal = 4);
  var
    Size: integer;
  begin
    Size := WriteAnsiText(AName);
    WritePadToAlignment(Size, Align);
  end;

  procedure WriteLayerBeginExtraInfo(const AKey: AnsiString);
  begin
    if Length(AKey) <> 4 then
       raise EPhotoshopDocument.CreateFmt('Invalid layer info key: "%s"',[string(AKey)]);
    WriteRawAnsiString('8BIM'); // Signature
    WriteRawAnsiString(AKey); // Key
    WriteBeginSection; // Size field
  end;

  procedure WriteLayerEndExtraInfo();
  begin
    // Specs state section size should be aligned to 2 bytes for most sub section types:
    //
    //   https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546
    //
    // In reality the one section we write ('luni') is aligned to 4 bytes and some readers
    // complain if it isn't.
    WriteEndSection(4);
  end;

  procedure WriteLayerRecord(ALayer: TCustomPhotoshopLayer; var AChannelsInfoPos: Int64);
  begin
    BigEndian.WriteCardinal(AStream, ALayer.Top); // Top
    BigEndian.WriteCardinal(AStream, ALayer.Left); // Left
    BigEndian.WriteCardinal(AStream, ALayer.Top + ALayer.Height); // Bottom
    BigEndian.WriteCardinal(AStream, ALayer.Left + ALayer.Width); // Right

    BigEndian.WriteWord(AStream, PSD_CHANNELS);

    // Make room for channel info list. Later updated in WriteLayerImage
    AChannelsInfoPos := AStream.Position;
    AStream.Seek(PSD_CHANNELS * SizeOf(TPSDChannelInfo), soFromCurrent);

    WriteRawAnsiString('8BIM'); // Signature
    WriteRawAnsiString(PSDBlendModeMapping[ALayer.BlendMode]); // Blend mode
    BigEndian.WriteByte(AStream, ALayer.Opacity); // Opacity
    BigEndian.WriteByte(AStream, Ord(ALayer.Clipping)); // Clipping
    BigEndian.WriteByte(AStream, byte(ALayer.Options)); // Options
    BigEndian.WriteByte(AStream, 0); // Filler, always 0

    // Variable section
    WriteBeginSection; // Extra data field
    begin

      BigEndian.WriteCardinal(AStream, 0); // Layer mask

      BigEndian.WriteCardinal(AStream, 0); // Blending ranges

      // Name of layer - ANSI
      WriteLayerName(AnsiString(ALayer.Name), 4);

      // *Layer extra info '8BIM' sequences
      WriteLayerBeginExtraInfo('luni');
      begin
        WriteUnicodeText(ALayer.Name); // unicode layer name sequence
      end;
      WriteLayerEndExtraInfo;

    end;
    WriteEndSection(4);
  end;

  procedure WriteLayerInfo;
  var
    i: integer;
    ChannelsInfoPos: array of Int64;
  begin
    WriteBeginSection(); // Layer info size field
    begin

      BigEndian.WriteWord(AStream, ADocument.Layers.Count); // Layers count

      SetLength(ChannelsInfoPos, ADocument.Layers.Count);

      for i := 0 to ADocument.Layers.Count - 1 do
        WriteLayerRecord(TCustomPhotoshopLayer(ADocument.Layers[i]), ChannelsInfoPos[i]);

      for i := 0 to ADocument.Layers.Count - 1 do
        WriteLayerImage(TCustomPhotoshopLayer(ADocument.Layers[i]), ChannelsInfoPos[i]);

    end;

    // Specs state section size should be aligned to 2 bytes:
    //
    //   https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_16000
    //
    // In reality it is aligned to 4 bytes and some readers complain if it isn't.
    WriteEndSection(4);
  end;

  procedure WriteLayer;
  begin
    if ADocument.Layers.Count = 0 then
    begin
      BigEndian.WriteCardinal(AStream, 0);
      exit;
    end;

    WriteBeginSection; // layer's total size field
    begin
      WriteLayerInfo;

      BigEndian.WriteCardinal(AStream, 0); // Global Mask .. optional

      // * global extra layer info '8BIM'

    end;
    WriteEndSection(4);
  end;

  procedure WriteImage;
  var
    BitmapWriter: TPSDBitmapWriterDelegate;
  begin
    BitmapWriter := GetBitmapWriter(ADocument.Background);

    ADocument.Background.BeginScan;
    begin
      BigEndian.WriteWord(AStream, Ord(ADocument.Background.Compression));
      BitmapWriter(AStream, ADocument.Background);
    end;
    ADocument.Background.EndScan;
  end;

begin
  if (ADocument.Width = 0) or (ADocument.Height = 0) then
     raise EPhotoshopDocument.Create('Invalid PSD document size');
  // Header
  WriteRawAnsiString('8BPS');
  BigEndian.WriteWord(AStream, PSD_VERSION_PSD);
  WritePadding(6); // unused
  BigEndian.WriteWord(AStream, PSD_CHANNELS);// PSD_CHANNELS
  BigEndian.WriteCardinal(AStream, ADocument.Height); // height
  BigEndian.WriteCardinal(AStream, ADocument.Width); // width
  BigEndian.WriteWord(AStream, 8);// bit depth
  BigEndian.WriteWord(AStream, PSD_RGB);// color mode RGB = 3

  // color mode Table
  BigEndian.WriteCardinal(AStream, 0);

  // resources
  BigEndian.WriteCardinal(AStream, 0);

  SectionsCaptures := TStack<Int64>.Create;
  try

    // layer
    WriteLayer;

  finally
    SectionsCaptures.Free;
  end;

  //Image
  if (ADocument.Background = nil) then
    WriteEmptyImage
  else
    WriteImage;
end;

end.
