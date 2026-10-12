unit TestOrdinalMaps;

interface

{$include GR32.inc}

uses
  Classes, Types,
  TestFramework,
  GR32,
  GR32_OrdinalMaps,
  GR32_VectorMaps;

type
  TTestByteMap = class(TTestCase)
  private
  public
  published
    procedure TestRotate180;
  end;

  TTestVectorMap = class(TTestCase)
  published
    procedure TestGetFixedVectorXBoundary;
  end;

implementation

uses
  SysUtils,
  Math;

{$RANGECHECKS OFF}


{ TTestByteMap }

procedure TTestByteMap.TestRotate180;

  procedure DoTest(Width, Height: integer);
  begin
    // Note: The byte map can't represent values larger than 8*8-1
    // so we clamp values to 255 with a simple "and $FF".

    var ByteMap := TByteMap.Create(Width, Height);
    try

      // Verify dimension are correct
      CheckEquals(ByteMap.Width, Width);
      CheckEquals(ByteMap.Height, Height);

      for var y := 0 to Height-1 do
        for var x := 0 to Width-1 do
          ByteMap[x, y] := (x + y * Width) and $FF;

      // Verify byte stream
      for var n := 0 to Height*Width-1 do
        CheckEquals(n and $FF, ByteMap.Bits[n]);

      ByteMap.Rotate180;

      // Verify dimension are unchanged
      CheckEquals(ByteMap.Width, Width);
      CheckEquals(ByteMap.Height, Height);

      // Verify values have been rotated 180 degrees
      for var yy := 0 to Height-1 do
        for var xx := 0 to Width-1 do
        begin
          var Expected := ((Width-1-xx) + (Height-1-yy) * Width) and $FF;
          CheckEquals(Expected, ByteMap[xx, yy]);
        end;

      // Verify byte stream
      for var n := 0 to Height*Width-1 do
      begin
        var Expected := (Height*Width-1-n) and $FF;
        CheckEquals(Expected, ByteMap.Bits[n]);
      end;

    finally
      ByteMap.Free;
    end;
  end;

begin
  // Corner cases
  DoTest(0, 0);
  DoTest(1, 0);
  DoTest(0, 1);
  DoTest(1, 1);

  // Random
  for var i := 1 to 1000 do
    DoTest(Random(i), Random(i));
end;

{ TTestVectorMap }

procedure TTestVectorMap.TestGetFixedVectorXBoundary;
var
  VectorMap: TVectorMap;
  V: TFixedVector;
  Width, Height: Integer;
  X, Y: Integer;
begin
  Width := 224;
  Height := 223;
  VectorMap := TVectorMap.Create(Width, Height);
  try
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        VectorMap.FixedVector[X, Y] := FixedPoint(X * 10, Y * 10);

    // Test sampling at the bottom-right boundary with fractional coordinates
    // X = 223.5, Y = 222.5 (WX = 223 = Width - 1, WY = 222 = Height - 1)
    V := VectorMap.FixedVectorX[Fixed(Width - 1) + $8000, Fixed(Height - 1) + $8000];
    CheckEquals((Width - 1) * 10 * FixedOne, V.X);
    CheckEquals((Height - 1) * 10 * FixedOne, V.Y);

    // Test sampling at right boundary, interior Y
    V := VectorMap.FixedVectorX[Fixed(Width - 1) + $8000, Fixed(100)];
    CheckEquals((Width - 1) * 10 * FixedOne, V.X);
    CheckEquals(100 * 10 * FixedOne, V.Y);

    // Test sampling at bottom boundary, interior X
    V := VectorMap.FixedVectorX[Fixed(100), Fixed(Height - 1) + $8000];
    CheckEquals(100 * 10 * FixedOne, V.X);
    CheckEquals((Height - 1) * 10 * FixedOne, V.Y);
  finally
    VectorMap.Free;
  end;
end;

initialization
  RegisterTest(TTestByteMap.Suite);
  RegisterTest(TTestVectorMap.Suite);
end.


