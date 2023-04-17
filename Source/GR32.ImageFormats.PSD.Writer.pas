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
 *
 * ***** END LICENSE BLOCK ***** *)

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

{$I GR32.inc}

uses
  Classes,
  GR32.ImageFormats.PSD;


//------------------------------------------------------------------------------
//
//      TPhotoshopDocumentWriter
//
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
  ZLib,
  Math,
  GR32,
  GR32.ImageFormats.PSD.Types;

type
  TBytesArray = array of byte;
  TSafeByteArray = array[0..MaxInt-1] of byte;
  PByteArray = ^TSafeByteArray;
  TPhotoshopLayerCracker = class(TCustomPhotoshopLayer);

//------------------------------------------------------------------------------
//
//      Big Endian I/O
//
//------------------------------------------------------------------------------
procedure WriteByte(Stream: TStream; Value: Byte); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Stream.Write(Value, SizeOf(Byte));
end;

procedure WriteBigEndianWord(Stream: TStream; Value: Word); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

procedure WriteBigEndianSmallInt(Stream: TStream; Value: SmallInt); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteBigEndianCardinal(Stream: TStream; Value: Cardinal); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

procedure WriteBigEndianInt64(Stream: TStream; Value: Int64); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap64(Value);
  Stream.Write(Value, SizeOf(Int64));
end;


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

  // Write a single row
  TPSDScanlineWriterDelegate = function(AStream: TStream; const ABuffer; Width: integer): Cardinal;

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
function WriteScanlineRAW(AStream: TStream; const ABuffer; Width: integer): Cardinal;
begin
  Result := AStream.Write(ABuffer, Width);
end;

procedure WriteChannelRAW(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
var
  i: integer;
begin
  for i := 0 to ALayer.Height - 1 do
  begin
    TPhotoshopLayerCracker(ALayer).GetChannelScanLine(AChannel, i, ABuffer);
    WriteScanlineRAW(AStream, ABuffer, ALayer.Width);
  end;
end;

procedure WriteBitmapRAW(AStream: TStream; ALayer: TCustomPhotoshopLayer);
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
      WriteScanlineRAW(AStream, ScanLineBuffer[0], ALayer.Width);
    end;
end;

//------------------------------------------------------------------------------
// RLE compression (PackBit)
//------------------------------------------------------------------------------
function WriteScanlineRLE(AStream: TStream; const ABuffer; Width: integer): Cardinal;
var
  StartPos: Int64;
  pArr :PByteArray;
  i, j, c, R, count:integer;
  uPacked:boolean;
begin
  StartPos := AStream.Position;
  pArr := @ABuffer;
  i :=0;
  while i < Width do
  begin
    c := pArr[i];
    j := i+1;
    while (j < Width) and (pArr[j] = c) do
      inc(j);

    uPacked := j - i > 1;
    if not uPacked then
    begin
      while (j < Width) and (pArr[j-1] <> pArr[j]) do
        inc(j);

      if (j < Width) and (j - i > 1) then
        dec(j);
    end;

    count := j - i;
    repeat
      R := Min(count, 128);
{$IFOPT R+}
{$DEFINE R_PLUS}
{$RANGECHECKS OFF}
{$ENDIF}
      if uPacked then
      begin
        WriteByte(AStream, -R + 1);
        WriteByte(AStream, c);
      end else
      begin
        WriteByte(AStream, R - 1);
        AStream.Write(pArr[i], R);
      end;
{$IFDEF R_PLUS}
{$RANGECHECKS ON}
{$UNDEF R_PLUS}
{$ENDIF}
      Inc(i,128);
      dec(count,128);
    until count <= 0;
    i := j;
  end;
  Result := AStream.Position - StartPos;
end;

procedure WriteChannelRLE(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
var
  i: integer;
  RowTablePos: Int64;
  RowSize: Word;
  RowTable: array of Word;
  SavePos: Int64;
begin
  RowTablePos := AStream.Position;

  // Make room for row table
  AStream.Seek(ALayer.Height * SizeOf(Smallint), soFromCurrent);
  SetLength(RowTable, ALayer.Height);

  for i := 0 to ALayer.Height - 1 do
  begin
    TPhotoshopLayerCracker(ALayer).GetChannelScanLine(AChannel, i, ABuffer);
    RowSize := WriteScanlineRLE(AStream, ABuffer, ALayer.Width);
    RowTable[i] := Swap16(RowSize);
  end;

  // Rewind and update row table
  SavePos := AStream.Position;
  AStream.Position := RowTablePos;
  AStream.Write(RowTable[0], ALayer.Height * SizeOf(Word));
  AStream.Position := SavePos;
end;

procedure WriteBitmapRLE(AStream: TStream; ALayer: TCustomPhotoshopLayer);
var
  ScanLineBuffer: TBytesArray;
  Channel: TColor32Component;
  i: integer;
  RowTablePos: Int64;
  RowSize: Word;
  RowTable: array of Word;
  SavePos: Int64;
begin
  SetLength(ScanLineBuffer, ALayer.Width);
  SetLength(RowTable, ALayer.Height);

  RowTablePos := AStream.Position;
  // Make room for row table (for all channels)
  AStream.Seek(ALayer.Height * SizeOf(Word) * PSD_CHANNELS, soFromCurrent);

  for Channel in PSDPlanarOrder do
  begin

    for i := 0 to ALayer.Height - 1 do
    begin
      TPhotoshopLayerCracker(ALayer).GetChannelScanLine(Channel, i, ScanLineBuffer[0]);
      RowSize := WriteScanlineRLE(AStream, ScanLineBuffer[0], ALayer.Width);
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
end;

//------------------------------------------------------------------------------
// ZIP compression
//------------------------------------------------------------------------------
procedure WriteChannelZIP(AStream: TStream; AChannel: TColor32Component; ALayer: TCustomPhotoshopLayer; var ABuffer);
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

procedure WriteBitmapZIP(AStream: TStream; ALayer: TCustomPhotoshopLayer);
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
      Result := WriteChannelRLE;

    lcZIP:
      Result := WriteChannelZIP;

    lcRAW:
      Result := WriteChannelRAW;
  else
    raise EPhotoshopDocument.CreateFmt('Unsupported compression method: %d', [Ord(ALayer.Compression)]);
  end;
end;

function GetBitmapWriter(ALayer: TCustomPhotoshopLayer): TPSDBitmapWriterDelegate;
begin
  case ALayer.Compression of
    lcRLE:
      Result := WriteBitmapRLE;

    lcZIP:
      Result := WriteBitmapZIP;

    lcRAW:
      Result := WriteBitmapRAW;
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
    WriteByte(AStream, Length(AText));
    Result := WriteRawAnsiString(AText) + 1;
  end;

  function WriteUnicodeText(const AText: string): Cardinal;
  var
    c: Char;
  begin
    WriteBigEndianCardinal(AStream, Length(AText));
    for c in AText do
      WriteBigEndianWord(AStream, Ord(c));
    c := #0;
    AStream.Write(c, SizeOf(Char));
    Result := (Length(AText)+1) * SizeOf(Char) + SizeOf(Cardinal);
  end;

  procedure WriteBeginSection;
  begin
    WriteBigEndianCardinal(AStream, 0); // field slot
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

  procedure Fill_RLE(AWidth, AHeight: integer);
  var
    L, i, t: integer;
    Arr: array of Word;
  begin
    L := Ceil(AWidth / 128); // round up
    SetLength(Arr, L);

    for i := 0 to L - 1 do
      Arr[i] := $FF81;

    t := AWidth mod 128;
    if t <> 0 then
      Arr[L - 1] := $FF00 or byte(-t + 1);

    WriteBigEndianWord(AStream, PSD_COMPRESSION_RLE);
    for i := 0 to AHeight * PSD_CHANNELS - 1 do // rleLengthsTable
      WriteBigEndianWord(AStream, L * SizeOf(Word));

    for i := 0 to AHeight * PSD_CHANNELS - 1 do // rleData
      AStream.Write(Pointer(Arr)^, L * SizeOf(Word));
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

        WriteBigEndianWord(AStream, Ord(ALayer.Compression));
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
    Size := WriteAnsiText(AName); // ansi name
    WritePadToAlignment(Size, Align);
  end;

  procedure WriteLayerBeginExtraInfo(const AKey: AnsiString);
  begin
    WriteRawAnsiString('8BIM'); // signature
    WriteRawAnsiString(AKey); // key
    WriteBeginSection;
  end;

  procedure WriteLayerEndExtraInfo();
  begin
    WriteEndSection(2);
  end;

  procedure WriteLayerRecord(ALayer: TCustomPhotoshopLayer; var AChannelsInfoPos: Int64);
  begin
    WriteBigEndianCardinal(AStream, ALayer.Top); // top
    WriteBigEndianCardinal(AStream, ALayer.Left); // left
    WriteBigEndianCardinal(AStream, ALayer.Top + ALayer.Height); // bottom
    WriteBigEndianCardinal(AStream, ALayer.Left + ALayer.Width); // right

    WriteBigEndianWord(AStream, PSD_CHANNELS);

    // Make room for channel info list. Later updated in WriteLayerImage
    AChannelsInfoPos := AStream.Position;
    AStream.Seek(PSD_CHANNELS * SizeOf(TPSDChannelInfo), soFromCurrent);

    WriteRawAnsiString('8BIM'); // signature
    WriteRawAnsiString(PSDBlendModeMapping[ALayer.BlendMode]); // blend mode
    WriteByte(AStream, ALayer.Opacity); // opacity
    WriteByte(AStream, Ord(ALayer.Clipping)); // clipping
    WriteByte(AStream, byte(ALayer.Options)); // Options
    WriteByte(AStream, 0); // Filler

    // variable section
    WriteBeginSection; // extralength field
    begin

      WriteBigEndianCardinal(AStream, 0); // layer mask

      WriteBigEndianCardinal(AStream, 0); // blending ranges

      // name of layer - ANSI
      WriteLayerName(AnsiString(ALayer.Name), 4);

      // *layer extra info '8BIM' sequences
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
    WriteBeginSection(); // layerInfoLength field
    begin

      WriteBigEndianWord(AStream, ADocument.Layers.Count); // Layers count

      SetLength(ChannelsInfoPos, ADocument.Layers.Count);

      for i := 0 to ADocument.Layers.Count - 1 do
        WriteLayerRecord(TCustomPhotoshopLayer(ADocument.Layers[i]), ChannelsInfoPos[i]);

      for i := 0 to ADocument.Layers.Count - 1 do
        WriteLayerImage(TCustomPhotoshopLayer(ADocument.Layers[i]), ChannelsInfoPos[i]);

    end;
    WriteEndSection(2);
  end;

  procedure WriteLayer;
  begin
    if ADocument.Layers.Count = 0 then
    begin
      WriteBigEndianCardinal(AStream, 0);
      exit;
    end;

    WriteBeginSection; // layer's total size field
    begin
      WriteLayerInfo;

      WriteBigEndianCardinal(AStream, 0); // global Mask .. optional

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
      WriteBigEndianWord(AStream, Ord(ADocument.Background.Compression));
      BitmapWriter(AStream, ADocument.Background);
    end;
    ADocument.Background.EndScan;
  end;

begin
  // Header
  WriteRawAnsiString('8BPS');
  WriteBigEndianWord(AStream, PSD_VERSION_PSD);
  WritePadding(6); // unused
  WriteBigEndianWord(AStream, PSD_CHANNELS);// PSD_CHANNELS
  WriteBigEndianCardinal(AStream, ADocument.Height); // height
  WriteBigEndianCardinal(AStream, ADocument.Width); // width
  WriteBigEndianWord(AStream, 8);// bit depth
  WriteBigEndianWord(AStream, PSD_RGB);// color mode RGB = 3

  // color mode Table
  WriteBigEndianCardinal(AStream, 0);

  // resources
  WriteBigEndianCardinal(AStream, 0);

  SectionsCaptures := TStack<Int64>.Create;
  try

    // layer
    WriteLayer;

  finally
    SectionsCaptures.Free;
  end;

  // image
  if ADocument.Background = nil then
    FILL_RLE(ADocument.Width, ADocument.Height)
  else
    WriteImage();
end;

end.
