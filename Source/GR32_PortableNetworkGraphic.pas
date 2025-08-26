unit GR32_PortableNetworkGraphic;

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
 * The Original Code is GR32PNG for Graphics32
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

{$include GR32.inc}
{$include GR32_PngCompilerSwitches.inc}

uses
  Generics.Collections,
  Classes, Graphics, SysUtils,
{$IFDEF FPC}
  ZBase, ZDeflate, ZInflate;
{$ELSE}
  {$IFDEF ZLibEx}
    ZLibEx, ZLibExApi,
  {$ELSE}
    {$if (CompilerVersion >= 32)} System.zlib, {$else} zlib, {$ifend}
  {$ENDIF}
{$ENDIF}
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Encoding,
  GR32_PortableNetworkGraphic.Chunks,
  GR32_PortableNetworkGraphic.Chunks.IDAT,
  GR32_PortableNetworkGraphic.Chunks.PLTE,
  GR32_PortableNetworkGraphic.Chunks.gAMA,
  GR32_PortableNetworkGraphic.Chunks.tRNS,
  GR32_PortableNetworkGraphic.Chunks.cHRM,
  GR32_PortableNetworkGraphic.Chunks.tIME,
  GR32_PortableNetworkGraphic.Chunks.sBIT,
  GR32_PortableNetworkGraphic.Chunks.bKGD,
  GR32_PortableNetworkGraphic.Chunks.pHYs;

//------------------------------------------------------------------------------
//
//      TCompositeChunkList
//
//------------------------------------------------------------------------------
type
  TPortableNetworkGraphic = class;

  TCompositeChunkList = class
  private type
    TChunkEnumerator = record
    private
      FList: TCompositeChunkList;
      FIndex: Integer;
      FLastIndex: integer;
    private
      function GetCurrent: TCustomChunk;
    public
      constructor Create(AList: TCompositeChunkList);
      property Current: TCustomChunk read GetCurrent;
      function MoveNext: boolean; inline;
    end;
  private
    FOwner: TPortableNetworkGraphic;

    function GetChunk(Index: integer): TCustomChunk;
    function GetCount: integer;
  public
    constructor Create(AOwner: TPortableNetworkGraphic);

    function GetEnumerator: TChunkEnumerator;

    property Count: integer read GetCount;
    property Chunks[Index: integer]: TCustomChunk read GetChunk; default;
  end;


//------------------------------------------------------------------------------
//
//      TPortableNetworkGraphic
//
//------------------------------------------------------------------------------
  TPortableNetworkGraphic = class(TInterfacedPersistent, IStreamPersist)
  private
    FCompressionLevel : Byte;
    function GetBitDepth: Byte;
    function GetColorType: TColorType;
    function GetCompressionMethod: Byte;
    function GetFilterMethod: TFilterMethod;
    function GetHeight: Integer;
    function GetInterlaceMethod: TInterlaceMethod;
    function GetPaletteEntry(Index: Integer): TRGB24;
    function GetPaletteEntryCount: Integer;
    function GetWidth: Integer;
    function GetGamma: Single;
    function GetModifiedTime: TDateTime;
    function GetPixelsPerUnitX: Cardinal;
    function GetPixelsPerUnitY: Cardinal;
    function GetPixelUnit: Byte;
    procedure SetPixelsPerUnitX(const Value: Cardinal);
    procedure SetPixelsPerUnitY(const Value: Cardinal);
    procedure SetPixelUnit(const Value: Byte);
    procedure SetBitDepth(const Value: Byte);
    procedure SetChromaChunk(const Value: TPngChunkPrimaryChromaticities);
    procedure SetColorType(const Value: TColorType);
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetCompressionLevel(const Value: Byte);
    procedure SetFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetGamma(const Value: Single);
    procedure SetModifiedTime(const Value: TDateTime);
    procedure SetHeight(const Value: Integer);
    procedure SetImageHeader(const Value: TPngChunkImageHeader);
    procedure SetInterlaceMethod(const Value: TInterlaceMethod);
    procedure SetGammaChunk(const Value: TPngChunkGamma);
    procedure SetPaletteChunk(const Value: TPngChunkPalette);
    procedure SetTransparencyChunk(const Value: TPngChunkTransparency);
    procedure SetPhysicalDimensions(const Value: TPngChunkPhysicalPixelDimensions);
    procedure SetSignificantBits(const Value: TPngChunkSignificantBits);
    procedure SetTimeChunk(const Value: TPngChunkTime);
    procedure SetWidth(const Value: Integer);

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TStream): Cardinal; overload;
    {$IFDEF CheckCRC}
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    {$ENDIF}
    procedure ReadImageDataChunk(Stream: TStream; Size: Integer);
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName; ChunkSize: Integer);
    function GetFilterMethods: TAvailableAdaptiveFilterMethods;
    procedure SetBackgroundChunk(const Value: TPngChunkBackgroundColor);
    function GetChunks: TCompositeChunkList;

  protected
    FImageHeader: TPngChunkImageHeader;

    FPaletteChunk: TPngChunkPalette;
    FGammaChunk: TPngChunkGamma;
    FTimeChunk: TPngChunkTime;
    FSignificantBitsChunk: TPngChunkSignificantBits;
    FPhysicalDimensionsChunk: TPngChunkPhysicalPixelDimensions;
    FChromaChunk: TPngChunkPrimaryChromaticities;
    FTransparencyChunk: TPngChunkTransparency;
    FBackgroundChunk: TPngChunkBackgroundColor;

    FDefaultChunks: TDefinedChunkWithHeaderList;
    FDataChunks: TChunkImageDataList;
    FAdditionalChunks: TDefinedChunkWithHeaderList;

    FCompositeChunkList: TCompositeChunkList;

  protected
    procedure Clear; virtual;

    procedure CopyImageData(Stream: TStream);
    procedure StoreImageData(Stream: TStream);
    procedure DecompressImageDataToStream(Stream: TStream);
    procedure CompressImageDataFromStream(Stream: TStream);

    procedure CompressionLevelChanged; virtual;
    procedure AdaptiveFilterMethodsChanged; virtual;
    procedure InterlaceMethodChanged; virtual;
    procedure HeaderChanged; virtual;

    property DefaultChunks: TDefinedChunkWithHeaderList read FDefaultChunks;
    property DataChunks: TChunkImageDataList read FDataChunks;
    property AdditionalChunks: TDefinedChunkWithHeaderList read FAdditionalChunks;

    property ImageHeader: TPngChunkImageHeader read FImageHeader write SetImageHeader;

    property PaletteChunk: TPngChunkPalette read FPaletteChunk write SetPaletteChunk;
    property TransparencyChunk: TPngChunkTransparency read FTransparencyChunk write SetTransparencyChunk;
    property BackgroundChunk: TPngChunkBackgroundColor read FBackgroundChunk write SetBackgroundChunk;
    property GammaChunk: TPngChunkGamma read FGammaChunk write SetGammaChunk;
    property ChromaChunk: TPngChunkPrimaryChromaticities read FChromaChunk write SetChromaChunk;
    property TimeChunk: TPngChunkTime read FTimeChunk write SetTimeChunk;
    property PhysicalPixelDimensionsChunk: TPngChunkPhysicalPixelDimensions read FPhysicalDimensionsChunk write SetPhysicalDimensions;
    property SignificantBitsChunk: TPngChunkSignificantBits read FSignificantBitsChunk write SetSignificantBits;
    property PrimaryChromaticitiesChunk: TPngChunkPrimaryChromaticities read FChromaChunk write SetChromaChunk;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure LoadFromFile(Filename: TFilename); virtual;
    procedure SaveToFile(Filename: TFilename); virtual;

    class function CanLoad(const FileName: TFileName): Boolean; overload;
    class function CanLoad(Stream: TStream): Boolean; overload;

    function HasPhysicalPixelDimensionsInformation: Boolean;
    function HasGammaInformation: Boolean;
    function HasModifiedTimeInformation: Boolean;
    procedure RemovePhysicalPixelDimensionsInformation;
    procedure RemoveGammaInformation;
    procedure RemoveModifiedTimeInformation;

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property BitDepth: Byte read GetBitDepth write SetBitDepth;
    property ColorType: TColorType read GetColorType write SetColorType;
    property CompressionMethod: Byte read GetCompressionMethod write SetCompressionMethod;
    property CompressionLevel: Byte read FCompressionLevel write SetCompressionLevel;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read GetFilterMethods write SetFilterMethods;
    property FilterMethod: TFilterMethod read GetFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod write SetInterlaceMethod;
    property PaletteEntry[Index: Integer]: TRGB24 read GetPaletteEntry;
    property PaletteEntryCount: Integer read GetPaletteEntryCount;
    property Gamma: Single read GetGamma write SetGamma;
    property ModifiedTime: TDateTime read GetModifiedTime write SetModifiedTime;
    property PixelsPerUnitX: Cardinal read GetPixelsPerUnitX write SetPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read GetPixelsPerUnitY write SetPixelsPerUnitY;
    property PixelUnit: Byte read GetPixelUnit write SetPixelUnit;

    property Chunks: TCompositeChunkList read GetChunks;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_LowLevel,
  GR32.BigEndian,
  GR32_PortableNetworkGraphic.Transcoding,
  GR32_PortableNetworkGraphic.ZLib,
  GR32_PortableNetworkGraphic.Chunks.Unknown;

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;

const
  PNG_SIG: array[0..7] of AnsiChar = #$89'PNG'#$0D#$0A#$1A#$0A;

//------------------------------------------------------------------------------
//
//      TPortableNetworkGraphic
//
//------------------------------------------------------------------------------
constructor TPortableNetworkGraphic.Create;
begin
  FImageHeader := TPngChunkImageHeader.Create;
  FDefaultChunks := TDefinedChunkWithHeaderList.Create(FImageHeader);
  FDataChunks := TChunkImageDataList.Create(FImageHeader);
  FAdditionalChunks := TDefinedChunkWithHeaderList.Create(FImageHeader);

  FCompressionLevel    := Z_BEST_COMPRESSION;
  inherited;
end;

destructor TPortableNetworkGraphic.Destroy;
begin
  FAdditionalChunks.Free;
  FDataChunks.Free;
  FDefaultChunks.Free;
  FImageHeader.Free;
  FCompositeChunkList.Free;

  inherited;
end;

procedure TPortableNetworkGraphic.SetPaletteChunk(const Value: TPngChunkPalette);
begin
  if (FPaletteChunk <> nil) then
  begin
    if (Value <> nil) then
      FPaletteChunk.Assign(Value)
    else
      FreeAndNil(FPaletteChunk);
  end else
  if (Value <> nil) then
  begin
    FPaletteChunk := FDefaultChunks.Add<TPngChunkPalette>;
    FPaletteChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetPhysicalDimensions(
  const Value: TPngChunkPhysicalPixelDimensions);
begin
  if (FPhysicalDimensionsChunk <> nil) then
  begin
    if (Value <> nil) then
      FPhysicalDimensionsChunk.Assign(Value)
    else
      FreeAndNil(FPhysicalDimensionsChunk);
  end else
  if (Value <> nil) then
  begin
    FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;
    FPhysicalDimensionsChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetSignificantBits(const Value: TPngChunkSignificantBits);
begin
  if (FSignificantBitsChunk <> nil) then
  begin
    if (Value <> nil) then
      FSignificantBitsChunk.Assign(Value)
    else
      FreeAndNil(FSignificantBitsChunk);
  end else
  if (Value <> nil) then
  begin
    FSignificantBitsChunk := FDefaultChunks.Add<TPngChunkSignificantBits>;
    FSignificantBitsChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetTimeChunk(const Value: TPngChunkTime);
begin
  if (FTimeChunk <> nil) then
  begin
    if (Value <> nil) then
      FTimeChunk.Assign(Value)
    else
      FreeAndNil(FTimeChunk);
  end else
  if (Value <> nil) then
  begin
    FTimeChunk := FDefaultChunks.Add<TPngChunkTime>;
    FTimeChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetTransparencyChunk(const Value: TPngChunkTransparency);
begin
  if (FTransparencyChunk <> nil) then
  begin
    if (Value <> nil) then
      FTransparencyChunk.Assign(Value)
    else
      FreeAndNil(FTransparencyChunk);
  end else
  if (Value <> nil) then
  begin
    FTransparencyChunk := FDefaultChunks.Add<TPngChunkTransparency>;
    FTransparencyChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitX(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if (FPhysicalDimensionsChunk= nil) then
    FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;

  FPhysicalDimensionsChunk.PixelsPerUnitX := Value;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitY(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if (FPhysicalDimensionsChunk= nil) then
    FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;

  FPhysicalDimensionsChunk.PixelsPerUnitY := Value;
end;

procedure TPortableNetworkGraphic.SetPixelUnit(const Value: Byte);
begin
  if Value > 1 then
    raise EPngError.Create(RCStrUnspecifiedPixelUnit);

  if (FPhysicalDimensionsChunk = nil) then
    FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;

  FPhysicalDimensionsChunk.PixelUnit := Value;
end;

procedure TPortableNetworkGraphic.SetChromaChunk(const Value: TPngChunkPrimaryChromaticities);
begin
  if (FChromaChunk <> nil) then
  begin
    if (Value <> nil) then
      FChromaChunk.Assign(Value)
    else
      FreeAndNil(FChromaChunk);
  end else
  if (Value <> nil) then
  begin
    FChromaChunk := FDefaultChunks.Add<TPngChunkPrimaryChromaticities>;
    FChromaChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetGammaChunk(const Value: TPngChunkGamma);
begin
  if (FGammaChunk <> nil) then
  begin
    if (Value <> nil) then
      FGammaChunk.Assign(Value)
    else
      FreeAndNil(FGammaChunk);
  end else
  if (Value <> nil) then
  begin
    FGammaChunk := FDefaultChunks.Add<TPngChunkGamma>;
    FGammaChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetBackgroundChunk(const Value: TPngChunkBackgroundColor);
begin
  if (FGammaChunk <> nil) then
  begin
    if (Value <> nil) then
      FBackgroundChunk.Assign(Value)
    else
      FreeAndNil(FBackgroundChunk);
  end else
  if (Value <> nil) then
  begin
    FBackgroundChunk := FDefaultChunks.Add<TPngChunkBackgroundColor>;
    FBackgroundChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetImageHeader(const Value: TPngChunkImageHeader);
begin
  if (Value = nil) then
    raise EPngError.Create(RCStrNewHeaderError);

  FImageHeader.Assign(Value);

  HeaderChanged;
end;

procedure TPortableNetworkGraphic.SetBitDepth(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrBitDepthTranscodingError, [Value]);
end;

procedure TPortableNetworkGraphic.SetColorType(const Value: TColorType);
begin
  raise EPngError.CreateFmt(RCStrColorTypeTranscodingError, [Integer(Value)]);
end;

procedure TPortableNetworkGraphic.SetFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
begin
  if (FImageHeader <> nil) then
    if FImageHeader.AdaptiveFilterMethods <> Value then
    begin
      FImageHeader.AdaptiveFilterMethods := Value;
      AdaptiveFilterMethodsChanged;

      HeaderChanged;
    end;
end;

procedure TPortableNetworkGraphic.SetCompressionLevel(const Value: Byte);
begin
  if not (Value in [1..9]) then
    raise EPngError.Create(RCStrInvalidCompressionLevel);

  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    CompressionLevelChanged;
  end;
end;

procedure TPortableNetworkGraphic.SetCompressionMethod(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrDirectCompressionMethodSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetFilterMethod(const Value: TFilterMethod);
begin
  raise EPngError.CreateFmt(RCStrDirectFilterMethodSetError, [Integer(Value)]);
end;

procedure TPortableNetworkGraphic.SetWidth(const Value: Integer);
begin
  raise EPngError.CreateFmt(RCStrDirectWidthSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetInterlaceMethod(
  const Value: TInterlaceMethod);
begin
  if Value <> FImageHeader.InterlaceMethod then
  begin
    FImageHeader.InterlaceMethod := Value;
    InterlaceMethodChanged;

    HeaderChanged;
  end;
end;

procedure TPortableNetworkGraphic.SetModifiedTime(const Value: TDateTime);
begin
  if (FTimeChunk <> nil) then
    FTimeChunk.ModifiedDateTime := Value;
end;

procedure TPortableNetworkGraphic.SetGamma(const Value: Single);
begin
  raise EPngError.CreateFmt(RCStrDirectGammaSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetHeight(const Value: Integer);
begin
  raise EPngError.CreateFmt(RCStrDirectHeightSetError, [Value]);
end;

procedure TPortableNetworkGraphic.CopyImageData(Stream: TStream);
var
  Chunk: TPngChunkImageData;
begin
  // combine all data chunks first
  for Chunk in FDataChunks do
    // concat current chunk to data stream
    Stream.CopyFrom(Chunk.Data, 0);
end;

procedure TPortableNetworkGraphic.StoreImageData(Stream: TStream);
var
  DataChunk : TPngChunkImageData;
  ChunkSize : Integer;
begin
  // delete old image data
  FDataChunks.Clear;

  ChunkSize := Stream.Size;
  while Stream.Position < Stream.Size do
  begin
    DataChunk := FDataChunks.Add(TPngChunkImageData);

    if (Stream.Size - Stream.Position) < ChunkSize then
      ChunkSize := (Stream.Size - Stream.Position);

    // copy data to IDAT chunk
    DataChunk.Data.CopyFrom(Stream, ChunkSize);
  end;
end;

procedure TPortableNetworkGraphic.DecompressImageDataToStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // copy image data from all data chunks to one continous data stream
    CopyImageData(DataStream);

    // check whether compression method is supported
    if FImageHeader.CompressionMethod <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressionMethod);

    // reset data stream position to zero
    DataStream.Position := 0;

    // decompress z-stream
    ZDecompress(DataStream, Stream);
  finally
    DataStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.CompressImageDataFromStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // set compression method
    FImageHeader.CompressionMethod := 0;

    // compress Stream to DataStream
    if Stream is TMemoryStream then
      ZCompress(TMemoryStream(Stream), DataStream, FCompressionLevel)
    else
      raise EPngError.CreateFmt(RCStrNotYetImplemented, ['source stream must be TMemoryStream']);

    // reset data stream position to zero
    DataStream.Position := 0;

    // copy image data from all data chunks to one continous data stream
    StoreImageData(DataStream);
  finally
    DataStream.Free;
  end;
end;

class function TPortableNetworkGraphic.CanLoad(const FileName: TFileName): Boolean;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := CanLoad(FileStream);
  finally
    FileStream.Free;
  end;
end;

class function TPortableNetworkGraphic.CanLoad(Stream: TStream): Boolean;
var
  Signature: array[0..SizeOf(PNG_SIG)-1] of AnsiChar;
begin
  Result := (Stream.Size >= SizeOf(Signature));

  if Result then
  begin
    Stream.Read(Signature, SizeOf(Signature));
    Stream.Seek(-SizeOf(Signature), soCurrent);
    Result := CompareMem(@Signature, @PNG_SIG, SizeOf(Signature));
  end;
end;

procedure TPortableNetworkGraphic.LoadFromFile(Filename: TFilename);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Integer;
  ChunkCRC     : Cardinal;
  ChunkClass   : TCustomDefinedChunkWithHeaderClass;
  Chunk        : TCustomDefinedChunkWithHeader;
  MemoryStream : TMemoryStream;
  GotIDAT      : boolean;
  SavePos      : UInt64;
begin
  GotIDAT := False;
  Clear;

  // Check for minimum file size and signature
  if (not CanLoad(Stream)) then
    raise EPngError.Create(RCStrNotAValidPNGFile);

  // Skip chunk ID and magic - We already checked them in CanLoad above
  Stream.Seek(SizeOf(PNG_SIG), soCurrent);

  MemoryStream := TMemoryStream.Create;
  try
    // read image header chunk size
    ChunkSize := BigEndian.ReadCardinal(Stream);
    if ChunkSize > Stream.Size - 12 then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read image header chunk ID
    SavePos := Stream.Position;
    Stream.Read(ChunkName, 4);
    if ChunkName <> 'IHDR' then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // reset position to the chunk start and copy stream to memory
    Stream.Position := SavePos;
    MemoryStream.CopyFrom(Stream, ChunkSize + 4);
    MemoryStream.Position := 4;

    // load image header
    FImageHeader.ReadFromStream(MemoryStream, ChunkSize);
    HeaderChanged;

    // read image header chunk size
    ChunkCRC := 0;
    Stream.Read(ChunkCRC, 4);
    {$IFDEF CheckCRC}
    if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
      raise EPngError.Create(RCStrCRCError);
    {$ENDIF}

    while Stream.Position < Stream.Size do
    begin
      // read image header chunk size
      ChunkSize := BigEndian.ReadCardinal(Stream);
      if Stream.Position+ChunkSize+4 > Stream.Size then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // read chunk ID
      SavePos := Stream.Position;
      Stream.Read(ChunkName, 4);

      // check for stream end
      if ChunkName = 'IEND' then
      begin
        // read image header chunk size
        Stream.Read(ChunkCRC, 4);

        {$IFDEF CheckCRC}
        if ChunkCRC <> 2187346606 then
          raise EPngError.Create(RCStrCRCError);
        {$ENDIF}

        Break;
      end;

      // reset position to the chunk start and copy stream to memory
      Stream.Position := SavePos;
      MemoryStream.Clear;
      MemoryStream.CopyFrom(Stream, ChunkSize + 4);

      // reset memory stream to beginning of the chunk
      MemoryStream.Position := 4;

      if ChunkName = 'IHDR' then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      if ChunkName = 'IDAT' then
      begin
        ReadImageDataChunk(MemoryStream, ChunkSize);
        GotIDAT := True;
      end else
      if ChunkName = 'gAMA' then
      begin
        if (FGammaChunk <> nil) then
          raise EPngError.Create(RCStrSeveralGammaChunks);
        FGammaChunk := FDefaultChunks.Add<TPngChunkGamma>;
        FGammaChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'cHRM' then
      begin
        if (FChromaChunk <> nil) then
          raise EPngError.Create(RCStrSeveralChromaChunks);
        FChromaChunk := FDefaultChunks.Add<TPngChunkPrimaryChromaticities>;
        FChromaChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'tIME' then
      begin
        if (FTimeChunk <> nil) then
          raise EPngError.Create(RCStrSeveralTimeChunks);
        FTimeChunk := FDefaultChunks.Add<TPngChunkTime>;
        FTimeChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'sBIT' then
      begin
        if (FSignificantBitsChunk <> nil) then
          raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
        FSignificantBitsChunk := FDefaultChunks.Add<TPngChunkSignificantBits>;
        FSignificantBitsChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'pHYs' then
      begin
        if (FPhysicalDimensionsChunk <> nil) then
          raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
        FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;
        FPhysicalDimensionsChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'PLTE' then
      begin
        if (FPaletteChunk <> nil) then
          raise EPngError.Create(RCStrSeveralPaletteChunks);
        FPaletteChunk := FDefaultChunks.Add<TPngChunkPalette>;
        FPaletteChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'tRNS' then
      begin
        if (FTransparencyChunk <> nil) then
          raise EPngError.Create(RCStrSeveralTransparencyChunks);
        FTransparencyChunk := FDefaultChunks.Add<TPngChunkTransparency>;
        FTransparencyChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'bKGD' then
      begin
        if (FBackgroundChunk <> nil) then
          raise EPngError.Create(RCStrSeveralBackgroundChunks);
        FBackgroundChunk := FDefaultChunks.Add<TPngChunkBackgroundColor>;
        FBackgroundChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      begin
        ChunkClass := FindPngChunkByChunkName(ChunkName);
        
        if ChunkClass <> nil then
        begin
          Chunk := ChunkClass.Create(FImageHeader);
          Chunk.ReadFromStream(MemoryStream, ChunkSize);
          FAdditionalChunks.Add(Chunk);
        end else
        begin
          // check if chunk is ancillary
          if (Byte(ChunkName[0]) and $80) <> 0 then
            raise EPngError.Create(RCStrAncillaryUnknownChunk);
          ReadUnknownChunk(MemoryStream, ChunkName, ChunkSize);
        end;
      end;

      // read & check CRC
      Stream.Read(ChunkCRC, 4);
      {$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
        raise EPngError.Create(RCStrCRCError);
      {$ENDIF}
    end;
  finally
    MemoryStream.Free;
  end;

  if (not GotIDAT) then
    raise EPngError.Create(RCStrMissingIDATChunk);
end;

procedure TPortableNetworkGraphic.SaveToFile(Filename: TFilename);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TStream);
var
  Buffer: TMemoryStream;

  procedure SaveChunkToStream(Chunk: TCustomChunk);
  var
    ChunkName: TChunkName;
    CRC: Cardinal;
  begin
    if (Chunk = nil) then
      exit;

    Buffer.Clear;

    // Write chunk size directly to stream
    BigEndian.WriteCardinal(Stream, Chunk.ChunkSize);

    // Write chunk name to buffer
    ChunkName := Chunk.ChunkName;
    Buffer.Write(ChunkName, 4);

    // Write chunk data to buffer
    Chunk.WriteToStream(Buffer);

    // Write buffer to stream
    Stream.CopyFrom(Buffer, 0);

    // Calculate CRC of buffer and write to stream
    CRC := Swap32(CalculateCRC(Buffer));
    Stream.Write(CRC, SizeOf(Cardinal));
  end;

var
  Chunk: TCustomChunk;
  ChunkName: TChunkName;
  CRC: Cardinal;
begin
  // Write chunk ID and PNG magic
  Stream.Write(PNG_SIG, SizeOf(PNG_SIG));

  Buffer := TMemoryStream.Create;
  try

    SaveChunkToStream(FImageHeader);

    SaveChunkToStream(FPhysicalDimensionsChunk);
    SaveChunkToStream(FSignificantBitsChunk);
    SaveChunkToStream(FGammaChunk);
    SaveChunkToStream(FChromaChunk);
    SaveChunkToStream(FPaletteChunk);
    SaveChunkToStream(FTransparencyChunk);
    SaveChunkToStream(FBackgroundChunk);

    for Chunk in FAdditionalChunks do
      SaveChunkToStream(Chunk);

    for Chunk in FDataChunks do
      SaveChunkToStream(Chunk);

  finally
    Buffer.Free;
  end;

  // Write IEND chunk
  BigEndian.WriteCardinal(Stream, 0); // Size
  ChunkName := 'IEND';
  Stream.Write(ChunkName, 4); // Name
  CRC := 2187346606;
  Stream.Write(CRC, 4); // CRC
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TStream;
  ChunkName: TChunkName; ChunkSize: Integer);
var
  Chunk: TCustomChunk;
begin
  Chunk := FAdditionalChunks.Add(ChunkName);
  Chunk.ReadFromStream(Stream, ChunkSize);
end;

procedure TPortableNetworkGraphic.RemoveGammaInformation;
begin
  FreeAndNil(FGammaChunk);
end;

procedure TPortableNetworkGraphic.RemoveModifiedTimeInformation;
begin
  FreeAndNil(FTimeChunk);
end;

procedure TPortableNetworkGraphic.RemovePhysicalPixelDimensionsInformation;
begin
  FreeAndNil(FPhysicalDimensionsChunk);
end;

procedure TPortableNetworkGraphic.CompressionLevelChanged;
var
  TempStream : TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Position := 0;
    CompressImageDataFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.AdaptiveFilterMethodsChanged;
begin
  if FDataChunks.Count > 0 then
  begin
    // transcoding!
    raise EPngError.CreateFmt(RCStrNotYetImplemented, ['AdaptiveFilterMethods transcoding']);
  end;
end;

procedure TPortableNetworkGraphic.InterlaceMethodChanged;
var
  TempStream: TMemoryStream;
  TranscoderClass: TCustomPngTranscoderClass;
  Transcoder: TCustomPngTranscoder;
begin
  TempStream := TMemoryStream.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Position := 0;

    case FImageHeader.InterlaceMethod of
      imNone  : TranscoderClass := TPngNonInterlacedToAdam7Transcoder;
      imAdam7 : TranscoderClass := TPngAdam7ToNonInterlacedTranscoder;
    else
      raise EPngError.Create(RCStrWrongInterlaceMethod);
    end;

    Transcoder := TranscoderClass.Create(TempStream, FImageHeader);
    try
      Transcoder.Transcode;
    finally
      Transcoder.Free;
    end;

    TempStream.Position := 0;
    CompressImageDataFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.HeaderChanged;
var
  Chunk: TCustomDefinedChunkWithHeader;
begin
  for Chunk in FDefaultChunks do
    Chunk.HeaderChanged;

  for Chunk in FDataChunks do
    Chunk.HeaderChanged;

  for Chunk in FAdditionalChunks do
    Chunk.HeaderChanged;
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TStream; Size: Integer);
var
  Chunk : TPngChunkImageData;
begin
  Chunk := FDataChunks.Add(TPngChunkImageData);
  Chunk.ReadFromStream(Stream, Size);
end;

procedure TPortableNetworkGraphic.Assign(ASource: TPersistent);
var
  Source: TPortableNetworkGraphic;
begin
  if ASource is TPortableNetworkGraphic then
  begin
    Source := TPortableNetworkGraphic(ASource);

    if (FImageHeader <> nil) then
    begin
      FImageHeader.Assign(Source.ImageHeader);
      HeaderChanged;
    end;

    // assign palette chunk
    if (FPaletteChunk <> nil) then
    begin
      if (Source.FPaletteChunk <> nil) then
        FPaletteChunk.Assign(Source.PaletteChunk)
      else
        FreeAndNil(FPaletteChunk);
    end else
    if (Source.PaletteChunk <> nil) then
    begin
      FPaletteChunk := FDefaultChunks.Add<TPngChunkPalette>;
      FPaletteChunk.Assign(Source.PaletteChunk);
    end;

    // assign gamma chunk
    if (FGammaChunk <> nil) then
    begin
      if (Source.GammaChunk <> nil) then
        FGammaChunk.Assign(Source.GammaChunk)
      else
        FreeAndNil(FGammaChunk);
    end else
    if (Source.GammaChunk <> nil) then
    begin
      FGammaChunk := FDefaultChunks.Add<TPngChunkGamma>;
      FGammaChunk.Assign(Source.GammaChunk);
    end;

    // assign time chunk
    if (FTimeChunk <> nil) then
    begin
      if (Source.TimeChunk <> nil) then
        FTimeChunk.Assign(Source.TimeChunk)
      else
        FreeAndNil(FTimeChunk);
    end else
    if (Source.TimeChunk <> nil) then
    begin
      FTimeChunk := FDefaultChunks.Add<TPngChunkTime>;
      FTimeChunk.Assign(Source.TimeChunk);
    end;

    // assign significant bits
    if (FSignificantBitsChunk <> nil) then
    begin
      if (Source.SignificantBitsChunk <> nil) then
        FSignificantBitsChunk.Assign(Source.SignificantBitsChunk)
      else
        FreeAndNil(Self.FSignificantBitsChunk);
    end else
    if (Source.SignificantBitsChunk <> nil) then
    begin
      FSignificantBitsChunk := FDefaultChunks.Add<TPngChunkSignificantBits>;
      FSignificantBitsChunk.Assign(Source.SignificantBitsChunk);
    end;

    // assign physical dimensions
    if (FPhysicalDimensionsChunk <> nil) then
    begin
      if (Source.PhysicalPixelDimensionsChunk <> nil) then
        FPhysicalDimensionsChunk.Assign(Source.PhysicalPixelDimensionsChunk)
      else
        FreeAndNil(FPhysicalDimensionsChunk);
    end else
    if (Source.PhysicalPixelDimensionsChunk <> nil) then
    begin
      FPhysicalDimensionsChunk := FDefaultChunks.Add<TPngChunkPhysicalPixelDimensions>;
      FPhysicalDimensionsChunk.Assign(Source.PhysicalPixelDimensionsChunk);
    end;

    // assign primary chromaticities
    if (FChromaChunk <> nil) then
    begin
      if (Source.FChromaChunk <> nil) then
        FChromaChunk.Assign(Source.ChromaChunk)
      else
        FreeAndNil(Self.FChromaChunk);
    end else
    if (Source.ChromaChunk <> nil) then
    begin
      FChromaChunk := FDefaultChunks.Add<TPngChunkPrimaryChromaticities>;
      FChromaChunk.Assign(Source.ChromaChunk);
    end;

    // assign transparency
    if (FTransparencyChunk <> nil) then
    begin
      if (Source.TransparencyChunk <> nil) then
        FTransparencyChunk.Assign(Source.TransparencyChunk)
      else
        FreeAndNil(FTransparencyChunk);
    end else
    if (Source.TransparencyChunk <> nil) then
    begin
      FTransparencyChunk := FDefaultChunks.Add<TPngChunkTransparency>;
      FTransparencyChunk.Assign(Source.TransparencyChunk);
    end;

    // assign background
    if (FBackgroundChunk <> nil) then
    begin
      if (Source.BackgroundChunk <> nil) then
        FBackgroundChunk.Assign(Source.BackgroundChunk)
      else
        FreeAndNil(Self.FBackgroundChunk);
    end else
    if (Source.BackgroundChunk <> nil) then
    begin
      FBackgroundChunk := FDefaultChunks.Add<TPngChunkBackgroundColor>;
      FBackgroundChunk.Assign(Source.BackgroundChunk);
    end;

    FDataChunks.Assign(Source.DataChunks);
    FAdditionalChunks.Assign(Source.AdditionalChunks);
  end else
    inherited;
end;

function TPortableNetworkGraphic.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue : Cardinal;
  Value    : Byte;
begin
  if Stream is TMemoryStream then
    Result := CalculateCRC(TMemoryStream(Stream).Memory, Stream.Size)
  else
  begin
    Stream.Position := 0;

    // initialize CRC
    CrcValue := $FFFFFFFF;
    {$IFDEF FPC}
    Value := 0;
    {$ENDIF}

    while Stream.Position < Stream.Size do
    begin
      Stream.Read(Value, 1);

      CrcValue := GCrcTable[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
    end;

    Result := (CrcValue xor $FFFFFFFF);

    Stream.Position := 0;
  end;
end;

function TPortableNetworkGraphic.CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal;
{$if defined(PUREPASCAL)}
var
  CrcValue : Cardinal;
  Pos      : Cardinal;
begin
  // ignore size (offset by 4 bytes)
  Pos := 0;

  // initialize CRC
  CrcValue := $FFFFFFFF;

  while Pos < Count do
  begin
    CrcValue := GCrcTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
    Inc(Buffer);
    Inc(Pos);
  end;

  Result := (CrcValue xor $FFFFFFFF);
{$else}
asm
{$IFDEF Target_x64}
        PUSH    RBX
        PUSH    RDI
        MOV     RCX, R8
        JS      @Done
        NEG     RCX
        MOV     RBX, $FFFFFFFF

{$IFNDEF FPC}
        MOV     RDI, [GCrcTable]
{$ELSE}
        MOV     RDI, [RIP + GCrcTable]
{$ENDIF}

@Start:
        MOVZX   EAX, [RDX].BYTE
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [RDI + 4 * RAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     RDX
        INC     RCX
        JS      @Start

        XOR     EBX, $FFFFFFFF
        MOV     RAX, RBX

@Done:
        POP     RDI
        POP     RBX
{$ELSE}
        PUSH    EBX
        PUSH    EDI
        JS      @Done
        NEG     ECX
        MOV     EBX, $FFFFFFFF

        MOV     EDI, [GCrcTable]

@Start:
        MOVZX   EAX, [EDX].BYTE
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [EDI + 4 * EAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     EDX
        INC     ECX
        JS      @Start

        XOR     EAX, $FFFFFFFF

@Done:
        POP     EDI
        POP     EBX
{$ENDIF}
{$ifend}
end;

{$IFDEF CheckCRC}
function TPortableNetworkGraphic.CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
begin
  Result := CalculateCRC(Stream) = CRC;
end;
{$ENDIF}

function TPortableNetworkGraphic.GetBitDepth: Byte;
begin
  Result := FImageHeader.BitDepth;
end;

function TPortableNetworkGraphic.GetChunks: TCompositeChunkList;
begin
  if (FCompositeChunkList = nil) then
    FCompositeChunkList := TCompositeChunkList.Create(Self);

  Result := FCompositeChunkList;
end;

function TPortableNetworkGraphic.GetColorType: TColorType;
begin
  Result := FImageHeader.ColorType;
end;

function TPortableNetworkGraphic.GetCompressionMethod: Byte;
begin
  Result := FImageHeader.CompressionMethod;
end;

function TPortableNetworkGraphic.GetFilterMethod: TFilterMethod;
begin
  Result := FImageHeader.FilterMethod;
end;

function TPortableNetworkGraphic.GetFilterMethods: TAvailableAdaptiveFilterMethods;
begin
  Result := FImageHeader.AdaptiveFilterMethods;
end;

function TPortableNetworkGraphic.GetGamma: Single;
begin
  if (FGammaChunk <> nil) then
    Result := FGammaChunk.GammaAsSingle
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetHeight: Integer;
begin
  Result := FImageHeader.Height;
end;

function TPortableNetworkGraphic.GetInterlaceMethod: TInterlaceMethod;
begin
  Result := FImageHeader.InterlaceMethod;
end;

function TPortableNetworkGraphic.GetModifiedTime: TDateTime;
begin
  if (FTimeChunk <> nil) then
    Result := EncodeDate(FTimeChunk.Year, FTimeChunk.Month, FTimeChunk.Day) +
      EncodeTime(FTimeChunk.Hour, FTimeChunk.Minute, FTimeChunk.Second, 0)
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPaletteEntry(Index: Integer): TRGB24;
begin
  if (FPaletteChunk <> nil) then
    Result := FPaletteChunk.PaletteEntry[Index]
  else
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortableNetworkGraphic.GetPaletteEntryCount: Integer;
begin
  if (FPaletteChunk <> nil) then
    Result := FPaletteChunk.Count
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitX: Cardinal;
begin
  if (FPhysicalDimensionsChunk <> nil) then
    Result := FPhysicalDimensionsChunk.PixelsPerUnitX
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitY: Cardinal;
begin
  if (FPhysicalDimensionsChunk <> nil) then
    Result := FPhysicalDimensionsChunk.PixelsPerUnitY
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelUnit: Byte;
begin
  if (FPhysicalDimensionsChunk <> nil) then
    Result := FPhysicalDimensionsChunk.PixelUnit
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetWidth: Integer;
begin
  Result := FImageHeader.Width;
end;

function TPortableNetworkGraphic.HasGammaInformation: Boolean;
begin
  Result := (FGammaChunk <> nil);
end;

function TPortableNetworkGraphic.HasModifiedTimeInformation: Boolean;
begin
  Result := (FTimeChunk <> nil);
end;

function TPortableNetworkGraphic.HasPhysicalPixelDimensionsInformation: Boolean;
begin
  Result := (FPhysicalDimensionsChunk <> nil);
end;

procedure TPortableNetworkGraphic.Clear;
begin
  // clear chunk lists
  FDefaultChunks.Clear;
  FDataChunks.Clear;
  FAdditionalChunks.Clear;

  FPaletteChunk := nil;
  FGammaChunk := nil;
  FTimeChunk := nil;
  FSignificantBitsChunk := nil;
  FPhysicalDimensionsChunk := nil;
  FChromaChunk := nil;
  FTransparencyChunk := nil;
  FBackgroundChunk := nil;

  // reset image header to default
  FImageHeader.ResetToDefault;
  HeaderChanged;
end;

procedure BuildCrcTable(Polynomial: Cardinal);
var
  c    : Cardinal;
  n, k : Integer;
begin
  // allocate CRC table memory
  GetMem(GCrcTable, 256 * SizeOf(Cardinal));

  // fill CRC table
  for n := 0 to 255 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := Polynomial xor (c shr 1)
      else
        c := c shr 1;
    end;
    GCrcTable[n] := c;
  end;
end;


//------------------------------------------------------------------------------
//
//      TCompositeChunkList
//
//------------------------------------------------------------------------------
constructor TCompositeChunkList.Create(AOwner: TPortableNetworkGraphic);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TCompositeChunkList.GetChunk(Index: integer): TCustomChunk;
begin
  if (Index = 0) then
    Exit(FOwner.ImageHeader);
  Dec(Index);

  if (Index < FOwner.DefaultChunks.Count) then
    Exit(FOwner.DefaultChunks[Index]);
  Dec(Index, FOwner.DefaultChunks.Count);

  if (Index < FOwner.DataChunks.Count) then
    Exit(FOwner.DataChunks[Index]);
  Dec(Index, FOwner.DataChunks.Count);

  Exit(FOwner.AdditionalChunks[Index]);
end;

function TCompositeChunkList.GetCount: integer;
begin
  Result := 1 + FOwner.DefaultChunks.Count + FOwner.DataChunks.Count + FOwner.AdditionalChunks.Count;
end;

function TCompositeChunkList.GetEnumerator: TChunkEnumerator;
begin
  Result := TChunkEnumerator.Create(Self);
end;

{ TCompositeChunkList.TChunkEnumerator }

constructor TCompositeChunkList.TChunkEnumerator.Create(AList: TCompositeChunkList);
begin
  FList := AList;
  FIndex := -1;
  FLastIndex := FList.Count - 1; // Cached since FList.Count is semi-expensive
end;

function TCompositeChunkList.TChunkEnumerator.GetCurrent: TCustomChunk;
begin
  Result := FList[FIndex];
end;

function TCompositeChunkList.TChunkEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FLastIndex);
  if (Result) then
    Inc(FIndex);
end;

initialization
  BuildCrcTable($EDB88320);

finalization
  if (GCrcTable <> nil) then
    Dispose(GCrcTable);

end.
