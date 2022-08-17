unit TestBitmapLoadSave;

interface

uses
  Classes, Types,
  FileTestFramework,
  GR32;

type
  TTestTCustomBitmap32 = class(TFileTestCase)
  strict private
    FBitmap32: TCustomBitmap32;
    FExpectedCrc: Cardinal;
    FIgnoreRes: boolean;
  private
    procedure TestSaveToStream(TopDown: boolean);
    procedure ValidateCRC(Bitmap: TCustomBitmap32);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadFromFile;
    procedure TestLoadFromStream;
    procedure TestLoadFromResourceName;
    procedure TestSaveToStreamTopDown;
    procedure TestSaveToStreamBottomUp;
    procedure TestSaveToFile;
  end;

implementation

uses
  Graphics,
  SysUtils,
  IOUtils,
  TestFramework,
  ZLib; // CRC32

const
  // CRC32 checksum of TBitmap32 pixel data after load.
  // If the checksum match then we assume that the size of the bitmap and the pixel colors match.
  Checksums: array[0..44] of record
    Name: string;
    Checksum: Cardinal;
    IgnoreRes: boolean;
  end = (                                               // IgnoreRes=True means that MS Resource Compiler alters bitmap so we cannot test it
    (Name: 'bgra_v1_bottomup';  Checksum: $AB3074DA;    IgnoreRes: False),
    (Name: 'bgra_v1_topdown';   Checksum: $AB3074DA;    IgnoreRes: False),
    (Name: 'bgra_v3_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v3_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v4_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v4_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v5_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v5_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v1_bottomup';  Checksum: $6C706898;    IgnoreRes: True),
    (Name: 'rgba_v1_topdown';   Checksum: $6C706898;    IgnoreRes: True),
    (Name: 'rgba_v3_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v3_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v4_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v4_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v5_bottomup';  Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'rgba_v5_topdown';   Checksum: $AB3074DA;    IgnoreRes: True),
    (Name: 'bgra_v4_bottomup_extra_colormask'; Checksum: $8CF3D1F0; IgnoreRes: True),
    (Name: 'pal1';              Checksum: $EB74525F;    IgnoreRes: False),
    (Name: 'pal1bg';            Checksum: $461E3E42;    IgnoreRes: False),
    (Name: 'pal1wb';            Checksum: $EB74525F;    IgnoreRes: False),
    (Name: 'pal4';              Checksum: $963878B9;    IgnoreRes: False),
    (Name: 'pal4gs';            Checksum: $38C07071;    IgnoreRes: False),
    (Name: 'pal4rle';           Checksum: $963878B9;    IgnoreRes: False),
    (Name: 'pal8-0';            Checksum: $A66B800E;    IgnoreRes: False),
    (Name: 'pal8';              Checksum: $A66B800E;    IgnoreRes: False),
    (Name: 'pal8gs';            Checksum: $FCFD7A5A;    IgnoreRes: False),
    (Name: 'pal8nonsquare';     Checksum: $9C51A082;    IgnoreRes: False),
    (Name: 'pal8os2';           Checksum: $216B88F1;    IgnoreRes: False),
    (Name: 'pal8rle';           Checksum: $A66B800E;    IgnoreRes: False),
    (Name: 'pal8topdown';       Checksum: $980784BA;    IgnoreRes: False),
    (Name: 'pal8v4';            Checksum: $A66B800E;    IgnoreRes: False),
    (Name: 'pal8v5';            Checksum: $A66B800E;    IgnoreRes: False),
    (Name: 'pal8w124';          Checksum: $C587558C;    IgnoreRes: False),
    (Name: 'pal8w125';          Checksum: $28FB03E9;    IgnoreRes: False),
    (Name: 'pal8w126';          Checksum: $7E8FACEE;    IgnoreRes: False),
    (Name: 'rgb16-565';         Checksum: $2B0C3870;    IgnoreRes: False),
    (Name: 'rgb16-565pal';      Checksum: $2B0C3870;    IgnoreRes: True),
    (Name: 'rgb16';             Checksum: $C2A5F1C7;    IgnoreRes: False),
    (Name: 'rgb16bfdef';        Checksum: $C2A5F1C7;    IgnoreRes: False),
    (Name: 'rgb24';             Checksum: $0BDF42DF;    IgnoreRes: False),
    (Name: 'rgb24pal';          Checksum: $0BDF42DF;    IgnoreRes: False),
    (Name: 'rgb32';             Checksum: $0BDF42DF;    IgnoreRes: False),
    (Name: 'rgb32bf';           Checksum: $0BDF42DF;    IgnoreRes: False),
    (Name: 'rgb32bfdef';        Checksum: $0BDF42DF;    IgnoreRes: False),
    // Additional
    (Name: 'rgb32fakealpha';    Checksum: $23821D77;IgnoreRes: False));

procedure TTestTCustomBitmap32.SetUp;
begin
  FBitmap32 := TBitmap32.Create;

  var Name := TPath.GetFileNameWithoutExtension(TestFileName);
  var Found := False;

  for var Checksum in Checksums do
    if (SameText(Checksum.Name, Name)) then
    begin
      FExpectedCrc := Checksum.Checksum;
      FIgnoreRes := Checksum.IgnoreRes;
      Found := True;
      break;
    end;

(* Uncomment this block to have the CRC values dumped to a text file.
  if (not Found) then
  begin
    FBitmap32.LoadFromFile(TestFileName);
    var Crc: Cardinal := crc32(0, nil, 0);
    Crc := crc32(Crc, PByte(FBitmap32.Bits), FBitmap32.Width*FBitmap32.Height*SizeOf(DWORD));

    // Output the text that needs to be added to the CRC table
    TFile.AppendAllText('crc_list.txt', Format('(Name: ''%s''; Checksum: $%.8X; IgnoreRes: False),'#13#10, [Name, Crc]));
  end;
*)

  Check(Found, Format('%s not found in CRC list', [Name]));
end;

procedure TTestTCustomBitmap32.TearDown;
begin
  FBitmap32.Free;
  FBitmap32 := nil;
end;

procedure TTestTCustomBitmap32.ValidateCRC(Bitmap: TCustomBitmap32);
begin
  var Crc: Cardinal := crc32(0, nil, 0);
  Crc := crc32(Crc, PByte(Bitmap.Bits), Bitmap.Width*Bitmap.Height*SizeOf(DWORD));

  CheckEquals(Crc, FExpectedCrc, 'Bitmap checksum validation failed');
end;

procedure TTestTCustomBitmap32.TestLoadFromStream;
begin
  var Stream := TFileStream.Create(TestFileName, fmOpenRead or fmShareDenyWrite);
  try

    FBitmap32.Clear;
    FBitmap32.LoadFromStream(Stream);

  finally
    Stream.Free;
  end;

  ValidateCRC(FBitmap32);
  Check(not FBitmap32.Empty);
end;

procedure TTestTCustomBitmap32.TestLoadFromFile;
begin
  FBitmap32.Clear;
  FBitmap32.LoadFromFile(TestFileName);

  ValidateCRC(FBitmap32);
  Check(not FBitmap32.Empty);
end;

procedure TTestTCustomBitmap32.TestLoadFromResourceName;
begin
  var ResName := TPath.GetFileNameWithoutExtension(TestFileName);
  ResName := ResName.Replace('-', '', [rfReplaceAll]);

  FBitmap32.Clear;
  FBitmap32.LoadFromResourceName(HInstance, ResName);

  if (not FIgnoreRes) then
    ValidateCRC(FBitmap32);
  Check(not FBitmap32.Empty);
end;

procedure TTestTCustomBitmap32.TestSaveToStream(TopDown: boolean);
begin
  FBitmap32.LoadFromFile(TestFileName);

  var Stream := TMemoryStream.Create;
  try

    FBitmap32.SaveToStream(Stream, TopDown);
    Stream.Position := 0;

    FBitmap32.Clear;
    FBitmap32.LoadFromStream(Stream);

    // Also verify that TBitmap can handle the file we just saved.
    // Bitmap content isn't checked.
    var Bitmap := TBitmap.Create;
    try
      Stream.Position := 0;
      Bitmap.LoadFromStream(Stream);
    finally
      Bitmap.Free;
    end;

  finally
    Stream.Free;
  end;

  ValidateCRC(FBitmap32);
  Check(not FBitmap32.Empty);
end;

procedure TTestTCustomBitmap32.TestSaveToStreamTopDown;
begin
  TestSaveToStream(True);
end;

procedure TTestTCustomBitmap32.TestSaveToStreamBottomUp;
begin
  TestSaveToStream(False);
end;

procedure TTestTCustomBitmap32.TestSaveToFile;
begin
  FBitmap32.LoadFromFile(TestFileName);

  var NewFilename := TGUID.NewGuid.ToString + '.bmp';
  FBitmap32.SaveToFile(NewFilename, True);
  try

    FBitmap32.Clear;
    FBitmap32.LoadFromFile(NewFilename);

    // Also verify that TBitmap can handle the file we just saved.
    // Bitmap content isn't checked.
    var Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromFile(NewFilename);
    finally
      Bitmap.Free;
    end;

  finally
    TFile.Delete(NewFilename);
  end;

  ValidateCRC(FBitmap32);
  Check(not FBitmap32.Empty);
end;

initialization
  var TestSuite := TFolderTestSuite.Create('Load and save bitmap', TTestTCustomBitmap32, '.\Data', '*.bmp', True);
  RegisterTest(TestSuite);
end.


