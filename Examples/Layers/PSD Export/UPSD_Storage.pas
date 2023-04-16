unit UPSD_Storage;
(*
* Author: Lamdalili - 2023
* Purpose: Partial export to Photoshop (PSD) file format
* the complete specification can be found at link
* https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/
*)
interface

uses
  Generics.Collections,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Math;

type
  TBytesArray = array of byte;
  TPsdBlendMode = (bmPass, bmNorm, bmDiss, bmDark, bmMul, bmIdiv, bmLbrn,
    bmDkCl, bmLite, bmScrn, bmDiv, bmLddg, bmLgCl, bmOver, bmSLit, bmHLit,
    bmVLit, bmLLit, bmPLit, bmHMix, bmDiff, bmSmud, bmFsub, bmFdiv, bmSat,
    bmColr, bmLum);

  TPsdLayerFlag = (lfTransProt, lfVisible, lfObsolete, lfFlag3, lfFlag4);
  TPsdLayerFlags = set of TPsdLayerFlag;
  TPsdLayerCompression = (psComRaw, psComRLE, psComZIP, psComZIPrd);

  TPsdBuilder = class;

  TCustomPsdLayer = class
  private
    FBuilder: TPsdBuilder;
    FChannelsInfoPos: integer;
    FTop: integer;
    FLeft: integer;
    FHeight: integer;
    FWidth: integer;
    FName: string;
    FBlendMode: TPsdBlendMode;
    FOpacity: Byte;
    FFlags: TPsdLayerFlags;
    FClipping: boolean;
  protected
    procedure SetBuilder(const Value: TPsdBuilder);
    function GetIndex: integer;
    procedure SetIndex(const Value: integer);
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);

    procedure GetChannelScanLine(AChannel, ALine: integer; var Bytes); virtual; abstract;
  public
    constructor Create(ABuilder: TPsdBuilder = nil); virtual;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight :integer);

    procedure BeginScan; virtual;
    procedure EndScan; virtual;

    property Builder: TPsdBuilder read FBuilder write SetBuilder;
    property Index: integer read GetIndex write SetIndex;

    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Height: integer read FHeight write FHeight;
    property Width: integer read FWidth write FWidth;
    property Name: string read FName write FName;
    property BlendMode: TPsdBlendMode read FBlendMode write FBlendMode;
    property Opacity: Byte read FOpacity write FOpacity;
    property Flags: TPsdLayerFlags read FFlags write FFlags;
    property Clipping: boolean read FClipping write FClipping;
  end;

  TPsdLayerClass = class of TCustomPsdLayer;

  TPsdBitmapLayer = class(TCustomPsdLayer)
  private
    FBitmap: TBitmap;
  protected
    procedure SetBitmap(const Value: TBitmap);
    procedure GetChannelScanLine(AChannel, ALine: integer; var Bytes); override;
  public
    property Bitmap:TBitmap read FBitmap write SetBitmap;
  end;

  TPsdBuilder = class
  private
    FLayers: TObjectList<TCustomPsdLayer>;
    FDims: TSize;
    FStream: TMemoryStream;
    FBackground: TCustomPsdLayer;
    FLayerCompression: TPsdLayerCompression;
    FCompression: TPsdLayerCompression;
  protected
    function GetLayer(Index: integer): TCustomPsdLayer;
    function GetLayerCount: integer;
    procedure SetBackground(const Value: TCustomPsdLayer);
    procedure SetCompression(const Value: TPsdLayerCompression);
    procedure Add(ALayer: TCustomPsdLayer);
    procedure Remove(ALayer: TCustomPsdLayer);
  public
    constructor Create(ABackground: TCustomPsdLayer = nil);
    destructor Destroy; override;

    function AddLayer(LayerClass: TPsdLayerClass): TCustomPsdLayer;

    procedure SetSize(AWidth, AHeight: Integer);
    procedure Build;

    property LayerCount: integer read GetLayerCount;
    property Layers[Index: integer]: TCustomPsdLayer read GetLayer;

    property LayerCompression: TPsdLayerCompression read FLayerCompression write FLayerCompression;
    property Compression: TPsdLayerCompression read FCompression write SetCompression;
    property Stream: TMemoryStream read FStream;
    property Background: TCustomPsdLayer read FBackground write SetBackground;
  end;


implementation

uses
  zlib;

const
  PSD_BLENDMODE_NAMES: array[TPsdBlendMode] of Ansistring = ('pass', 'norm', 'diss',
    'dark', 'mul ', 'idiv', 'lbrn', 'dkCl', 'lite', 'scrn', 'div ', 'lddg',
    'lgCl', 'over', 'sLit', 'hLit', 'vLit', 'lLit', 'pLit', 'hMix', 'diff',
    'smud', 'fsub', 'fdiv', 'sat ', 'colr', 'lum ');

type
   TPsdChannelInfo = packed record
     ChID: Word;
     CSize: Cardinal;
   end;

  TSafeByteArray = array[0..MaxInt-1] of byte;
  PByteArray = ^TSafeByteArray;

// Various swap functions for converting big-endian data
function Swap16(Value: Word): Word; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF SUPPORTS_INLINE}
begin
  Result := Swap(Value);
{$ELSE}
{$IFDEF PUREPASCAL}
begin
  Result := Swap(Value);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, ECX
  {$ENDIF}
  XCHG    AL, AH
  {$ENDIF}
  {$ENDIF}
end;

function Swap32(Value: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
type
  TTwoWords = array [0..1] of Word;
begin
  TTwoWords(Result)[1] := Swap(TTwoWords(Value)[0]);
  TTwoWords(Result)[0] := Swap(TTwoWords(Value)[1]);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, ECX
  {$ENDIF}
  BSWAP   EAX
  {$ENDIF}
end;

function Swap64(Value: Int64): Int64;
type
  TFourWords = array [0..3] of Word;
begin
  TFourWords(Result)[3] := Swap(TFourWords(Value)[0]);
  TFourWords(Result)[2] := Swap(TFourWords(Value)[1]);
  TFourWords(Result)[1] := Swap(TFourWords(Value)[2]);
  TFourWords(Result)[0] := Swap(TFourWords(Value)[3]);
end;

procedure WriteByte(Stream: TStream; Value: Byte); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Stream.Write(Value, SizeOf(Byte));
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

procedure WriteSwappedInt64(Stream: TStream; Value: Int64); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Value := Swap64(Value);
  Stream.Write(Value, SizeOf(Int64));
end;


// needs optimisation
function CompressScanLineRLE8(const Line; Width: Integer; Stream: TStream): Word;
var
  StartPos: Int64;
  pArr :PByteArray;
  i, j, c, R, count:integer;
  uPacked:boolean;
begin
  StartPos := Stream.Position;
  pArr := @Line;
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
        WriteByte(Stream, -R + 1);
        WriteByte(Stream, c);
      end else
      begin
        WriteByte(Stream, R - 1);
        Stream.Write(pArr[i], R);
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
  Result := Stream.Position - StartPos;
end;

{ TCustomPsdLayer }

constructor TCustomPsdLayer.Create(ABuilder: TPsdBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
  FBlendMode := bmNorm;
  FOpacity := $FF;
end;

procedure TCustomPsdLayer.BeginScan;
begin
end;

procedure TCustomPsdLayer.EndScan;
begin
end;

function TCustomPsdLayer.GetBoundsRect: TRect;
begin
  Result := Rect(Left, Top, Left+Width, Top+Height);
end;

function TCustomPsdLayer.GetIndex: integer;
begin
  if (FBuilder <> nil) then
    Result := FBuilder.FLayers.IndexOf(Self)
  else
    Result := -1;
end;

procedure TCustomPsdLayer.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  Left := ALeft;
  Top := ATop;
  Width := AWidth;
  Height := AHeight;
end;

procedure TCustomPsdLayer.SetBoundsRect(const Value: TRect);
begin
  SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);
end;

procedure TCustomPsdLayer.SetBuilder(const Value: TPsdBuilder);
begin
  if (FBuilder = Value) then
    exit;

  if (FBuilder <> nil) then
    FBuilder.Remove(Self);

  FBuilder := Value;

  if (FBuilder <> nil) then
    FBuilder.Add(Self);
end;

procedure TCustomPsdLayer.SetIndex(const Value: integer);
begin
  if (Value <> Index) and (FBuilder <> nil) then
    FBuilder.FLayers.Move(Index, Value);
end;

{ TPsdImageLayer }

procedure TPsdBitmapLayer.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
  Height := Bitmap.Height;
  Width  := Bitmap.Width;
end;

procedure TPsdBitmapLayer.GetChannelScanLine(AChannel, ALine: integer; var Bytes);
var
  i: integer;
  P: PByte;
  pData: PByteArray;
  Stride: integer;
begin
  if (Bitmap = nil) or ((AChannel < 0) and (Bitmap.PixelFormat <> pf32Bit)) then
  begin
    FillChar(Bytes, Width, $FF);
    Exit;
  end;

  pData := @Bytes;

  if Bitmap.PixelFormat = pf24Bit then
  begin
    Stride := 3;
    AChannel := 2-AChannel;
  end else
  if Bitmap.PixelFormat = pf32Bit then
  begin
    Stride := 4;
    if AChannel < 0 then
      AChannel := 3
    else
      AChannel := 2-AChannel;
  end else
    exit;

  P := @(PByteArray(Bitmap.ScanLine[ALine])^[AChannel]);

  for i := 0 to Bitmap.Width-1 do
  begin
    pData[i] := P^;
    Inc(P, Stride);
  end;
end;

{ TPsdStorage }

constructor TPsdBuilder.Create(ABackground: TCustomPsdLayer);
begin
  inherited Create;
  FLayers := TObjectList<TCustomPsdLayer>.Create;
  FStream := TMemoryStream.Create;
  FCompression := psComRLE;
  FLayerCompression := psComRLE;
  FBackground := ABackground;
end;

destructor TPsdBuilder.Destroy;
begin
  FBackground.Free;
  FLayers.Free;
  FStream.Free;
  inherited;
end;

function TPsdBuilder.GetLayer(Index: integer): TCustomPsdLayer;
begin
  Result := FLayers[Index];
end;

function TPsdBuilder.GetLayerCount: integer;
begin
  Result := FLayers.Count;
end;

function TPsdBuilder.AddLayer(LayerClass: TPsdLayerClass): TCustomPsdLayer;
begin
  Result := LayerClass.Create(Self);
  Add(Result);
end;

procedure TPsdBuilder.Add(ALayer: TCustomPsdLayer);
begin
  if (ALayer.Builder = Self) then
    FLayers.Add(ALayer);
end;

procedure TPsdBuilder.Remove(ALayer: TCustomPsdLayer);
begin
  if (ALayer.Builder = Self) then
    FLayers.Extract(ALayer);
end;

procedure TPsdBuilder.SetBackground(const Value: TCustomPsdLayer);
begin
  if (FBackground = Value) then
    Exit;

  FBackground.Free;
  FBackground := Value;

  if FBackground <> nil then
  begin
    FDims.cx := FBackground.Width;
    FDims.cY := FBackground.Height;
  end;
end;

procedure TPsdBuilder.SetCompression(const Value: TPsdLayerCompression);
begin
  if (Value = psComZIPrd) then
    raise Exception.Create('"ZIP with prediction"-compression is not implemented');
  FCompression := Value;
end;

procedure TPsdBuilder.SetSize(AWidth, AHeight: Integer);
begin
  FDims.cx := AWidth;
  FDims.cY := AHeight;
end;

procedure TPsdBuilder.Build();
const
  CHANNELS = 4;
  CHANNELS_IDS: array[0..CHANNELS-1] of SmallInt = (-1, 0, 1, 2); // -1 alpha channel
var
  SectionsCaptures: array of Cardinal;

  function Pad(Value: Cardinal; Alignment: Cardinal = 4): integer;
  begin
    Result := (Alignment - (Value and (Alignment - 1))) and (Alignment - 1);
  end;

  function W_Append(ASize: Cardinal): Cardinal;
  begin
    Result := FStream.Position;
    if (ASize = 0) then
      Exit;
    FStream.Size := FStream.Size + ASize;
    FStream.Position := FStream.Size;
  end;

  procedure W_Pad(Value: Cardinal; Alignment: Cardinal = 4);
  begin
    W_Append(Pad(Value, Alignment));
  end;

  procedure W_RawStr(const s: AnsiString);
  begin
    FStream.Write(PAnsiChar(s)^, Length(s));
  end;

  function W_WriteAT(APos: Cardinal; const Buff; Count: integer): Pointer;
  begin
    Result := @PByteArray(FStream.Memory)[APos];
    Move(Buff, Result^, Count);
  end;

  function W_Begin_Section(): Cardinal;
  begin
    WriteSwappedCardinal(FStream, 0); // field slot
    Result := Length(SectionsCaptures);
    SetLength(SectionsCaptures, Result + 1);
    SectionsCaptures[Result] := FStream.Position;
  end;

  procedure W_End_Section(Align: Cardinal = 4);
  var
    Sz: Cardinal;
    StartPos, FieldPos, Last: integer;
  begin
    Last := High(SectionsCaptures);
    StartPos := SectionsCaptures[Last];
    SetLength(SectionsCaptures, Last);
    Sz := FStream.Position - StartPos;
    W_Pad(Sz, Align);
    Sz := Swap32(FStream.Position - StartPos);
    FieldPos := StartPos - 4; // field slot
    W_WriteAT(FieldPos, Sz, SizeOf(Sz));
  end;

  procedure Fill_RLE(AWidth, AHeight: integer);
  var
    L, i, t: integer;
    Arr: array of Word;
  begin
    L := Ceil(AWidth / 128); // round up
    Setlength(Arr, L);

    for i := 0 to L - 1 do
      Arr[i] := $FF81;

    t := AWidth mod 128;
    if t <> 0 then
      Arr[L - 1] := $FF00 or byte(-t + 1);

    WriteSwappedWord(FStream, 1); // RLE compression
    for i := 0 to AHeight * CHANNELS - 1 do // rleLengthsTable
      WriteSwappedWord(FStream, L * SizeOf(Word));

    for i := 0 to AHeight * CHANNELS - 1 do // rleData
      FStream.Write(Pointer(Arr)^, L * SizeOf(Word));
  end;

  procedure W_CompressRAW(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    i: integer;
  begin
    for i := 0 to ALayer.Height - 1 do
    begin
      ALayer.GetChannelScanLine(AChannelID, i, ADest);
      FStream.Write(ADest, ALayer.Width);
    end;
  end;

  procedure W_CompressRLE(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    i: integer;
    rleTablePos: Cardinal;
    rleLen: Word;
    rleTable: array of Word;
  begin
    Setlength(rleTable, ALayer.Height);
    rleTablePos := W_Append(ALayer.Height * SizeOf(Smallint));
    // allocate lenghs table
    for i := 0 to ALayer.Height - 1 do
    begin
      ALayer.GetChannelScanLine(AChannelID, i, ADest);
      rleLen := CompressScanLineRLE8(ADest, ALayer.Width, FStream);
      rleTable[i] := Swap16(rleLen);
    end;
    W_WriteAT(rleTablePos, rleTable[0], ALayer.Height * SizeOf(Word));
  end;

  procedure W_CompressZIP(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    i: integer;
    Stream: TStream;
  begin
    Stream := TCompressionStream.Create(clDefault, FStream);
    try
      for i := 0 to ALayer.Height - 1 do
      begin
        ALayer.GetChannelScanLine(AChannelID, i, ADest);
        Stream.Write(ADest, ALayer.Width);
      end;
    finally
      Stream.Free;
    end;
  end;

  procedure W_Layer_Image(ALayer: TCustomPsdLayer; ACompression: TPsdLayerCompression);
  var
    Sz: Cardinal;
    ChID: SmallInt;
    i: integer;
    ChannelsInfs: array [0 .. CHANNELS - 1] of TPsdChannelInfo;
    ScanLineBuffer: TBytesArray;
  begin
    Setlength(ScanLineBuffer, ALayer.Width);
    ALayer.BeginScan();
    for i := 0 to CHANNELS - 1 do
    begin
      Sz := FStream.Position;
      WriteSwappedWord(FStream, Ord(ACompression)); // compression algo
      ChID := CHANNELS_IDS[i];
      case ACompression of
        psComRLE:
          W_CompressRLE(ChID, ALayer, ScanLineBuffer[0]);

        psComZIP:
          W_CompressZIP(ChID, ALayer, ScanLineBuffer[0]);

        psComZIPrd:
          W_CompressZIP(ChID, ALayer, ScanLineBuffer[0]);

        psComRaw:
          W_CompressRAW(ChID, ALayer, ScanLineBuffer[0]);
      end;
      Sz := FStream.Position - Sz;
      ChannelsInfs[i].ChID := Swap16(Word(ChID));
      ChannelsInfs[i].CSize := Swap32(Sz);
    end;
    ALayer.EndScan();
    W_WriteAT(ALayer.FChannelsInfoPos, ChannelsInfs, SizeOf(ChannelsInfs))
  end;

  procedure W_LayerName(const AName: AnsiString; Align: Cardinal = 4);
  var
    L: integer;
  begin
    L := Length(AName);
    WriteByte(FStream, L);
    W_RawStr(AName); // ansi name
    W_Pad(L + 1, Align);
  end;

  procedure W_Layer_Begin_ExtraInfo(const AKey: AnsiString);
  begin
    W_RawStr('8BIM'); // signature
    W_RawStr(AKey); // key
    W_Begin_Section();
  end;

  procedure W_Layer_End_ExtraInfo();
  begin
    W_End_Section(2);
  end;

  procedure W_LayerUnicodeText(const AText: string);
  var
    L: Cardinal;
    i: integer;
  begin
    L := Length(AText);
    WriteSwappedCardinal(FStream, L);
    for i := 1 to L do
      WriteSwappedWord(FStream, Ord(AText[i]));
  end;

  procedure W_Layer_Record(ALayer: TCustomPsdLayer);
  begin
    WriteSwappedCardinal(FStream, ALayer.Top); // top
    WriteSwappedCardinal(FStream, ALayer.Left); // left
    WriteSwappedCardinal(FStream, ALayer.Top + ALayer.Height); // bottom
    WriteSwappedCardinal(FStream, ALayer.Left + ALayer.Width); // right

    WriteSwappedWord(FStream, CHANNELS); // Layer Channels
    // forward alloc to be updated in W_Layer_Image
    ALayer.FChannelsInfoPos := W_Append(CHANNELS * SizeOf(TPsdChannelInfo));
    // ch IDs & size

    W_RawStr('8BIM'); // signature
    W_RawStr(PSD_BLENDMODE_NAMES[ALayer.BlendMode]); // blend mode
    WriteByte(FStream, ALayer.Opacity); // opacity
    WriteByte(FStream, Ord(ALayer.Clipping)); // clipping
    WriteByte(FStream, byte(ALayer.Flags)); // Flags
    WriteByte(FStream, 0); // Filler

    // variable section
    W_Begin_Section(); // extralength field

    WriteSwappedCardinal(FStream, 0); // layer mask

    WriteSwappedCardinal(FStream, 0); // blending ranges

    // name of layer - ANSI
    W_LayerName(AnsiString(ALayer.Name), 4);

    // *layer extra info '8BIM' sequences
    W_Layer_Begin_ExtraInfo('luni');
    W_LayerUnicodeText(ALayer.Name); // unicode layer name sequence
    W_Layer_End_ExtraInfo();

    W_End_Section(4);
  end;

  procedure W_Layer_Info();
  var
    i: integer;
  begin
    W_Begin_Section(); // layerInfoLength field

    WriteSwappedWord(FStream, FLayers.count); // Layers count

    for i := 0 to FLayers.count - 1 do
      W_Layer_Record(TCustomPsdLayer(FLayers[i]));

    for i := 0 to FLayers.count - 1 do
      W_Layer_Image(TCustomPsdLayer(FLayers[i]), FLayerCompression);

    W_End_Section(2);
  end;

  procedure W_Layer();
  begin
    if FLayers.count = 0 then
    begin
      WriteSwappedCardinal(FStream, 0);
      exit;
    end;
    W_Begin_Section(); // layer's total size field

    W_Layer_Info();

    WriteSwappedCardinal(FStream, 0); // global Mask .. optional

    // * global extra layer info '8BIM'

    W_End_Section(4);
  end;

  procedure W_Image();
  var
    i, j: integer;
    rleTablePos: Cardinal;
    rleLen: Word;
    rleTable: array of Word;
    ScanLineBuff: TBytesArray;
  begin
    Setlength(ScanLineBuff, FBackground.Width);
    Setlength(rleTable, FBackground.Height);
    FBackground.BeginScan();
    WriteSwappedWord(FStream, Ord(psComRLE)); // compression algo
    rleTablePos := W_Append(FBackground.Height * SizeOf(Smallint) * CHANNELS);
    // allocate lenghs table
    for j := 0 to CHANNELS - 1 do
    begin
      for i := 0 to FBackground.Height - 1 do
      begin
        FBackground.GetChannelScanLine(CHANNELS_IDS[(j + 1) mod 4], i, ScanLineBuff[0]);
        rleLen := CompressScanLineRLE8(ScanLineBuff[0], FBackground.Width, FStream);
        rleTable[i] := Swap16(rleLen);
      end;
      W_WriteAT(rleTablePos, rleTable[0], Length(rleTable) * SizeOf(Word));
      Inc(rleTablePos, Length(rleTable) * SizeOf(Word));
    end;
    FBackground.EndScan();
  end;

begin
  // Header
  W_RawStr('8BPS');
  WriteSwappedWord(FStream, 1); // version 1
  W_Append(6); // unused
  WriteSwappedWord(FStream, CHANNELS);// channels
  WriteSwappedCardinal(FStream, FDims.cy); // height
  WriteSwappedCardinal(FStream, FDims.cx); // width
  WriteSwappedWord(FStream, 8);// bit depth
  WriteSwappedWord(FStream, 3);// color mode RGB = 3

  // color mode Table
  WriteSwappedCardinal(FStream, 0);

  // resources
  WriteSwappedCardinal(FStream, 0);

  // layer
  W_Layer();

  //image
  if FBackground = nil then
    FILL_RLE(FDims.cx, FDims.cy)
  else
    W_Image();
end;

end.
