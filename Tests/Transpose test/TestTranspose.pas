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

  published
    procedure TestSquareTranspose;
    procedure TestRectangularTranspose;

    procedure TestSuperDuperTranspose32;
    procedure TestCacheObliviousTranspose32;
    procedure TestCacheObliviousTransposeEx32;
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

procedure TTestTranspose.TestRectangularTranspose;
begin
  DoDoTest(Transpose32);
end;

procedure TTestTranspose.TestCacheObliviousTranspose32;
begin
  DoDoTest(CacheObliviousTranspose32);
end;

procedure TTestTranspose.TestCacheObliviousTransposeEx32;
begin
  DoDoTest(CacheObliviousTransposeEx32);
end;

initialization
  RegisterTest(TTestTranspose.Suite);
end.


