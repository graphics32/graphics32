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
    property Compression: TPsdLayerCompression read FCompression write FCompression;
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
     ChID: Smallint;
     CSize: integer;
   end;

  TSafeByteArray = array[0..MaxInt-1] of byte;
  PByteArray = ^TSafeByteArray;

function Swap32(c: Integer): Integer;
begin
  Result := Swap(c shr 16) or (Swap(c) shl 16);
end;

// needs optimisation
function CompressScanLineRLE8(const Line; Width: Integer; Stream: TStream): integer;

  procedure Out8(c: byte);
  begin
    Stream.Write(c, 1);
  end;

var
  pArr :PByteArray;
  I, J, c, R, count:integer;
  uPacked:boolean;
begin
    Result := Stream.Position;
    pArr := @Line;
    I :=0;
    while I < Width do
    begin
        c := pArr[I];
        J := I+1;
        while (J < Width) and (pArr[J] = c) do
          inc(J);
        uPacked := J - I > 1;
        if not uPacked then
        begin
          while (J < Width) and (pArr[J-1] <> pArr[J]) do
            inc(J);
          if (J < Width) and (J - I > 1) then
            dec(j);
        end;
        count := J - I;
        repeat
           R := Min(count, 128);
           if uPacked then
           begin
              Out8(Byte(-R + 1));
              Out8(c);
           end else
           begin
              Out8(R - 1);
              Stream.Write(pArr[I], R);
           end;
           Inc(I,128);
           dec(count,128);
        until count <= 0;
        I := J;
    end;
  Result := Stream.Position - Result;
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

procedure TPsdBitmapLayer.GetChannelScanLine(AChannel, ALine: integer;var Bytes);
var
  I: integer;
  P, pData: PByteArray;
begin
  if (Bitmap = nil) or ((AChannel < 0) and (Bitmap.PixelFormat <> pf32Bit)) then
  begin
    FillChar(Bytes, Width, $FF);
    Exit;
  end;

  pData := @Bytes;
  P := Bitmap.ScanLine[ALine];
  if Bitmap.PixelFormat = pf24Bit then
  begin
    AChannel := 2-AChannel;
    for I := 0 to Bitmap.Width-1 do
      pData[I] := P[I * 3 + AChannel];
  end else
  if Bitmap.PixelFormat = pf32Bit then
  begin
    if AChannel < 0 then
      AChannel := 3
    else
      AChannel := 2-AChannel;

    for I:= 0 to Bitmap.Width-1 do
      pData[I] := P[I * 4 + AChannel];
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

procedure TPsdBuilder.SetSize(AWidth, AHeight: Integer);
begin
  FDims.cx := AWidth;
  FDims.cY := AHeight;
end;

procedure TPsdBuilder.Build();
const
  CHANNELS = 4;
  CHANNELS_IDS: array[0..CHANNELS-1] of Smallint = (-1, 0, 1, 2); // -1 alpha channel
var
  SectionsCaptures: array of integer;

  function W_Append(ASize: integer):integer;
  begin
    Result:= FStream.Position;
    FStream.Size := FStream.Size + ASize;
    FStream.Position := FStream.Size;
  end;

  procedure BE_Int32(c: Integer);
  begin
    c := Swap32(c);
    FStream.Write(c,4);
  end;

  procedure BE_Int16(c: short);
  begin
    c := Swap(c);
    FStream.Write(c,2);
  end;

  procedure W_Int8(c:byte);
  begin
    FStream.Write(c,1);
  end;

  procedure W_RawStr(const s: Ansistring);
  begin
    FStream.Write(PAnsiChar(s)^,Length(s));
  end;

  function W_WriteAT(APos: integer; const Buff; Count: integer): Pointer;
  begin
    Result := @PByteArray(FStream.Memory)[APos];
    Move(Buff, Result^, Count);
  end;

  function W_Begin_Section():integer;
  begin
    BE_Int32(0);//field slot
    Result :=Length(SectionsCaptures);
    Setlength(SectionsCaptures, Result + 1);
    SectionsCaptures[Result]:= FStream.Position;
  end;

  procedure W_End_Section(Align: integer);
  var
    Sz, StartPos, FieldPos, Last: integer;
  begin
    Last := High(SectionsCaptures);
    StartPos := SectionsCaptures[Last];
    Setlength(SectionsCaptures, Last);
    Sz := FStream.Position - StartPos;
    if (Align <> 0) and (Sz mod Align <> 0) then
      W_Append(Align - Sz mod Align);
    Sz := Swap32(FStream.Position - StartPos);
    FieldPos := StartPos - 4; // field slot
    W_WriteAT(FieldPos, Sz, SizeOf(Sz));
  end;

  procedure Fill_RLE(AWidth, AHeight: integer);
  var
    L, I, t: integer;
    Arr: array of Word;
  begin
    L := Ceil(AWidth / 128); // round up
    Setlength(Arr, L);

    for I := 0 to L - 1 do
      Arr[I] := $FF81;

    t := AWidth mod 128;
    if t <> 0 then
      Arr[L - 1] := $FF00 or byte(-t + 1);

    BE_Int16(1); // RLE compression
    for I := 0 to AHeight * CHANNELS - 1 do // rleLengthsTable
      BE_Int16(L * 2);

    for I := 0 to AHeight * CHANNELS - 1 do // rleData
      FStream.Write(Pointer(Arr)^, L * 2);
  end;

  procedure W_CompressRAW(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    I: integer;
  begin
    for I := 0 to ALayer.Height - 1 do
    begin
      ALayer.GetChannelScanLine(AChannelID, I, ADest);
      FStream.Write(ADest, ALayer.Width);
    end;
  end;

  procedure W_CompressRLE(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    I, rleTablePos, rleLen: integer;
    rleTable: array of Smallint;
  begin
    Setlength(rleTable, ALayer.Height);
    rleTablePos := W_Append(ALayer.Height * SizeOf(Smallint));
    // allocate lenghs table
    for I := 0 to ALayer.Height - 1 do
    begin
      ALayer.GetChannelScanLine(AChannelID, I, ADest);
      rleLen := CompressScanLineRLE8(ADest, ALayer.Width, FStream);
      rleTable[I] := Swap(rleLen);
    end;
    W_WriteAT(rleTablePos, rleTable[0], ALayer.Height * SizeOf(Smallint));
  end;

  procedure W_CompressZIP(AChannelID: integer; ALayer: TCustomPsdLayer; var ADest);
  var
    I: integer;
  begin
    with TCompressionStream.Create(clDefault, FStream) do
      try
        for I := 0 to ALayer.Height - 1 do
        begin
          ALayer.GetChannelScanLine(AChannelID, I, ADest);
          Write(ADest, ALayer.Width);
        end;
      finally
        Free;
      end;
  end;

  procedure W_Layer_Image(ALayer: TCustomPsdLayer; ACompression: TPsdLayerCompression);
  var
    Sz, I, ChID: integer;
    ChannelsInfs: array [0 .. CHANNELS - 1] of TPsdChannelInfo;
    ScanLineBuffer: TBytesArray;
  begin
    Setlength(ScanLineBuffer, ALayer.Width);
    ALayer.BeginScan();
    for I := 0 to CHANNELS - 1 do
    begin
      Sz := FStream.Position;
      BE_Int16(Ord(ACompression)); // compression algo
      ChID := CHANNELS_IDS[I];
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
      ChannelsInfs[I].ChID := Swap(ChID);
      ChannelsInfs[I].CSize := Swap32(Sz);
    end;
    ALayer.EndScan();
    W_WriteAT(ALayer.FChannelsInfoPos, ChannelsInfs, SizeOf(ChannelsInfs))
  end;

  procedure W_LayerName(const AName: Ansistring; Align: integer);
  var
    L: integer;
  begin
    L := Length(AName);
    W_Int8(L);
    W_RawStr(AName); // ansi name
    if (L + 1) mod Align <> 0 then
      W_Append(Align - ((L + 1) mod Align)); // align
  end;

  procedure W_Layer_Begin_ExtraInfo(const AKey: string);
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
    L, I: integer;
  begin
    L := Length(AText);
    BE_Int32(L);
    if L = 0 then
      exit;
    for I := 1 to L do
      BE_Int16(Ord(AText[I]));
    // BE_Int16(0);// ? null
  end;

  procedure W_Layer_Record(ALayer: TCustomPsdLayer);
  begin
    BE_Int32(ALayer.Top); // top
    BE_Int32(ALayer.Left); // left
    BE_Int32(ALayer.Top + ALayer.Height); // bottom
    BE_Int32(ALayer.Left + ALayer.Width); // right

    BE_Int16(CHANNELS); // Layer Channels
    // forward alloc to be updated in W_Layer_Image
    ALayer.FChannelsInfoPos := W_Append(CHANNELS * SizeOf(TPsdChannelInfo));
    // ch IDs & size

    W_RawStr('8BIM'); // signature
    W_RawStr(PSD_BLENDMODE_NAMES[ALayer.BlendMode]); // blend mode
    W_Int8(ALayer.Opacity); // opacity
    W_Int8(Ord(ALayer.Clipping)); // clipping
    W_Int8(byte(ALayer.Flags)); // Flags
    W_Int8(0); // Filler

    // variable section
    W_Begin_Section(); // extralength field

    BE_Int32(0); // layer mask

    BE_Int32(0); // blending ranges

    // name of layer
    W_LayerName(ALayer.Name, 4);

    // *layer extra info '8BIM' sequences
    W_Layer_Begin_ExtraInfo('luni');
    W_LayerUnicodeText(ALayer.Name); // unicode layer name sequence
    W_Layer_End_ExtraInfo();

    W_End_Section(4);
  end;

  procedure W_Layer_Info();
  var
    I: integer;
  begin
    W_Begin_Section(); // layerInfoLength field

    BE_Int16(FLayers.count); // Layers count

    for I := 0 to FLayers.count - 1 do
      W_Layer_Record(TCustomPsdLayer(FLayers[I]));

    for I := 0 to FLayers.count - 1 do
      W_Layer_Image(TCustomPsdLayer(FLayers[I]), FLayerCompression);

    W_End_Section(2);
  end;

  procedure W_Layer();
  begin
    if FLayers.count = 0 then
    begin
      BE_Int32(0);
      exit;
    end;
    W_Begin_Section(); // layer's total size field

    W_Layer_Info();

    BE_Int32(0); // global Mask .. optional

    // * global extra layer info '8BIM'

    W_End_Section(4);
  end;

  procedure W_Image();
  var
    I, J, rleTablePos: integer;
    rleLen: Smallint;
    rleTable: array of Smallint;
    ScanLineBuff: TBytesArray;
  begin
    Setlength(ScanLineBuff, FBackground.Width);
    Setlength(rleTable, FBackground.Height);
    FBackground.BeginScan();
    BE_Int16(Ord(psComRLE)); // compression algo
    rleTablePos := W_Append(FBackground.Height * SizeOf(Smallint) * CHANNELS);
    // allocate lenghs table
    for J := 0 to CHANNELS - 1 do
    begin
      for I := 0 to FBackground.Height - 1 do
      begin
        FBackground.GetChannelScanLine(CHANNELS_IDS[(J + 1) mod 4], I,
          ScanLineBuff[0]);
        rleLen := CompressScanLineRLE8(ScanLineBuff[0],
          FBackground.Width, FStream);
        rleTable[I] := Swap(rleLen);
      end;
      W_WriteAT(rleTablePos, rleTable[0], Length(rleTable) * SizeOf(Smallint));
      inc(rleTablePos, Length(rleTable) * SizeOf(Smallint));
    end;
    FBackground.EndScan();
  end;

begin
  // Header
  W_RawStr('8BPS');
  BE_Int16(1); // version 1
  W_Append(6); // unused
  BE_Int16(CHANNELS);// channels
  BE_Int32(FDims.cy); // height
  BE_Int32(FDims.cx); // width
  BE_Int16(8);// bit depth
  BE_Int16(3);// color mode RGB = 3

  // color mode Table
  BE_Int32(0);

  // resources
  BE_Int32(0);

  // layer
  W_Layer();

  //image
  if FBackground = nil then
    FILL_RLE(FDims.cx, FDims.cy)
  else
    W_Image();
end;

end.
