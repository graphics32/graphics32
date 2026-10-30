unit TestTranspose;

interface

{$include GR32.inc}
{$define TestCacheObliviousTranspose32}

uses
  Classes, Types,
  TestFramework,
  GR32,
  GR32.Transpose;

type
  TTestTranspose = class(TTestCase)
  private
  public
    procedure DoTest(Width, Height: integer; BitmapSrc, BitmapDst, ReferenceBitmap: TBitmap32; Transposer: TTranspose32);
    procedure DoDoTest(Transposer: TTranspose32);
    procedure DoTestCorners(Transposer: TTranspose32);

  published
    procedure TestSquareTranspose;
    procedure TestRectangularTranspose;

    procedure TestSuperDuperTranspose32;
    procedure TestSuperDuperTransposeCorners;
    procedure TestCacheObliviousTranspose32;
    procedure TestCacheObliviousTransposeCorners;
    procedure TestCacheObliviousTransposeEx32;
    procedure TestCacheObliviousTransposeExCorners;
    procedure TestTransposeGudkov32;
    procedure TestTransposeGudkovCorners;
  end;

implementation

uses
  SysUtils,
  Math;

{$RANGECHECKS OFF}


{ TTestTranspose }

procedure TTestTranspose.DoDoTest(Transposer: TTranspose32);
begin
  var BitmapSrc := TBitmap32.Create;
  var BitmapDst := TBitmap32.Create;
  var ReferenceBitmap := TBitmap32.Create;
  try
    for var W := 0 to 10 do
      for var H := 10 downto 0 do
      begin
        var Width := W * 13;
        var Height := H * 133;

        DoTest(Width, Height, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
      end;
  finally
    ReferenceBitmap.Free;
    BitmapSrc.Free;
    BitmapDst.Free;
  end;
end;

procedure TTestTranspose.DoTest(Width, Height: integer; BitmapSrc, BitmapDst, ReferenceBitmap: TBitmap32; Transposer: TTranspose32);
begin
  BitmapSrc.SetSize(Width, Height);
  BitmapDst.SetSize(Height, Width);
  ReferenceBitmap.SetSize(Height, Width);


  for var i := 0 to Width * Height-1 do
    BitmapSrc.Bits[i] := i;

  ReferenceTranspose32(BitmapSrc.Bits, ReferenceBitmap.Bits, Width, Height);

  Transposer(BitmapSrc.Bits, BitmapDst.Bits, Width, Height);

  CheckEquals(Width, BitmapDst.Height);
  CheckEquals(Height, BitmapDst.Width);

  CheckEqualsMem(ReferenceBitmap.Bits, BitmapDst.Bits, Width * Height * SizeOf(TColor32), Format('(%d, %d)', [Width, Height]));
end;

procedure TTestTranspose.DoTestCorners(Transposer: TTranspose32);
begin
  var BitmapSrc := TBitmap32.Create;
  var BitmapDst := TBitmap32.Create;
  var ReferenceBitmap := TBitmap32.Create;
  try
    // Test small dimensions (1..16) covering all (W mod 4, H mod 4) remainder permutations
    // and cases where W < 4 or H < 4 (no 4x4 blocks processed)
    for var W := 1 to 16 do
      for var H := 1 to 16 do
        DoTest(W, H, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);

    // Test specific non-multiples of 4 and corner dimensions
    DoTest(134, 82, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(82, 134, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(133, 81, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(135, 83, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(1, 100, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(100, 1, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(3, 100, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
    DoTest(100, 3, BitmapSrc, BitmapDst, ReferenceBitmap, Transposer);
  finally
    ReferenceBitmap.Free;
    BitmapSrc.Free;
    BitmapDst.Free;
  end;
end;

procedure TTestTranspose.TestSquareTranspose;
begin
  var BitmapSrc := TBitmap32.Create;
  var BitmapDst := TBitmap32.Create;
  var ReferenceBitmap := TBitmap32.Create;
  try
    for var Width := 0 to 10 do
    begin
      var Size := Width * 123;

      DoTest(Size, Size, BitmapSrc, BitmapDst, ReferenceBitmap, Transpose32);

    end;
  finally
    ReferenceBitmap.Free;
    BitmapSrc.Free;
    BitmapDst.Free;
  end;
end;

procedure TTestTranspose.TestSuperDuperTranspose32;
begin
  DoDoTest(SuperDuperTranspose32);
end;

procedure TTestTranspose.TestSuperDuperTransposeCorners;
begin
  DoTestCorners(SuperDuperTranspose32);
end;

procedure TTestTranspose.TestRectangularTranspose;
begin
  DoDoTest(Transpose32);
end;

procedure TTestTranspose.TestCacheObliviousTranspose32;
begin
  DoDoTest(CacheObliviousTranspose32);
end;

procedure TTestTranspose.TestCacheObliviousTransposeCorners;
begin
  DoTestCorners(CacheObliviousTranspose32);
end;

procedure TTestTranspose.TestCacheObliviousTransposeEx32;
begin
  DoDoTest(CacheObliviousTransposeEx32);
end;

procedure TTestTranspose.TestCacheObliviousTransposeExCorners;
begin
  DoTestCorners(CacheObliviousTransposeEx32);
end;

procedure TTestTranspose.TestTransposeGudkov32;
begin
  DoDoTest(TransposeGudkov32);
end;

procedure TTestTranspose.TestTransposeGudkovCorners;
begin
  DoTestCorners(TransposeGudkov32);
end;

initialization
  RegisterTest(TTestTranspose.Suite);
end.


