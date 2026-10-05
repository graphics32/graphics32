unit TestDraftBlur;

interface

uses
  TestFramework, GR32, GR32.Blur, GR32.Blur.DraftBlur, GR32_OrdinalMaps, GR32_Bindings;

type
  TTestDraftBlur = class(TTestCase)
  private
    FBitmapPas, FBitmapSIMD: TBitmap32;
    FByteMapPas, FByteMapSIMD: TByteMap;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDraftBlur32Equivalence;
    procedure TestDraftAlphaBlur32Equivalence;
    procedure TestDraftBlur8Equivalence;
  end;

implementation

uses
  SysUtils;

{ TTestDraftBlur }

procedure TTestDraftBlur.SetUp;
begin
  FBitmapPas := TBitmap32.Create;
  FBitmapSIMD := TBitmap32.Create;
  FByteMapPas := TByteMap.Create;
  FByteMapSIMD := TByteMap.Create;
end;

procedure TTestDraftBlur.TearDown;
begin
  FBitmapPas.Free;
  FBitmapSIMD.Free;
  FByteMapPas.Free;
  FByteMapSIMD.Free;
end;

procedure TTestDraftBlur.TestDraftBlur32Equivalence;
var
  x, y, i: Integer;
  Radius: Single;
  Radii: array[0..3] of Single;
  MaxDiff, Diff, c: Integer;
  p1, p2: PColor32Entry;
begin
  Radii[0] := 1.5;
  Radii[1] := 4.0;
  Radii[2] := 10.0;
  Radii[3] := 25.0;

  for i := 0 to High(Radii) do
  begin
    Radius := Radii[i];

    FBitmapPas.SetSize(64, 48);
    FBitmapSIMD.SetSize(64, 48);

    // Populate test patterns
    for y := 0 to FBitmapPas.Height - 1 do
      for x := 0 to FBitmapPas.Width - 1 do
      begin
        FBitmapPas.Pixel[x, y] := Color32((x * 7 + y * 13) mod 256, (x * 19 + y * 5) mod 256, (x * 3 + y * 23) mod 256, 255);
        FBitmapSIMD.Pixel[x, y] := FBitmapPas.Pixel[x, y];
      end;

    // Run Pascal implementation
    Rebind([isPascal]);
    DraftBlur32(FBitmapPas, Radius);

    // Run SIMD implementation
    Rebind;
    DraftBlur32(FBitmapSIMD, Radius);

    // Verify bit-exact or near-exact match (within rounding tolerance <= 1)
    MaxDiff := 0;
    p1 := PColor32Entry(FBitmapPas.Bits);
    p2 := PColor32Entry(FBitmapSIMD.Bits);
    for x := 0 to FBitmapPas.Width * FBitmapPas.Height - 1 do
    begin
      for c := 0 to 3 do
      begin
        Diff := Abs(Integer(p1.Planes[c]) - Integer(p2.Planes[c]));
        if Diff > MaxDiff then
          MaxDiff := Diff;
      end;
      Inc(p1);
      Inc(p2);
    end;

    Check(MaxDiff <= 1, Format('DraftBlur32 SIMD vs Pas max diff %d exceeded 1 at radius %.1f', [MaxDiff, Radius]));
  end;

  // Restore default CPU bindings
  Rebind;
end;

procedure TTestDraftBlur.TestDraftAlphaBlur32Equivalence;
var
  x, y, i: Integer;
  Radius: Single;
  Radii: array[0..2] of Single;
  MaxDiff, Diff, c: Integer;
  p1, p2: PColor32Entry;
begin
  Radii[0] := 2.0;
  Radii[1] := 8.0;
  Radii[2] := 16.0;

  for i := 0 to High(Radii) do
  begin
    Radius := Radii[i];

    FBitmapPas.SetSize(32, 32);
    FBitmapSIMD.SetSize(32, 32);

    for y := 0 to FBitmapPas.Height - 1 do
      for x := 0 to FBitmapPas.Width - 1 do
      begin
        FBitmapPas.Pixel[x, y] := Color32((x * 11) mod 256, (y * 17) mod 256, (x + y) mod 256, (x * 8 + y * 8) mod 256);
        FBitmapSIMD.Pixel[x, y] := FBitmapPas.Pixel[x, y];
      end;

    Rebind([isPascal]);
    DraftAlphaBlur32(FBitmapPas, Radius);

    Rebind;
    DraftAlphaBlur32(FBitmapSIMD, Radius);

    MaxDiff := 0;
    p1 := PColor32Entry(FBitmapPas.Bits);
    p2 := PColor32Entry(FBitmapSIMD.Bits);
    for x := 0 to FBitmapPas.Width * FBitmapPas.Height - 1 do
    begin
      for c := 0 to 3 do
      begin
        Diff := Abs(Integer(p1.Planes[c]) - Integer(p2.Planes[c]));
        if Diff > MaxDiff then
          MaxDiff := Diff;
      end;
      Inc(p1);
      Inc(p2);
    end;

    Check(MaxDiff <= 1, Format('DraftAlphaBlur32 SIMD vs Pas max diff %d exceeded 1 at radius %.1f', [MaxDiff, Radius]));
  end;

  Rebind;
end;

procedure TTestDraftBlur.TestDraftBlur8Equivalence;
var
  x, y, i: Integer;
  Radius: Single;
  Radii: array[0..2] of Single;
  MaxDiff, Diff: Integer;
  p1, p2: PByte;
begin
  Radii[0] := 1.5;
  Radii[1] := 5.0;
  Radii[2] := 12.0;

  for i := 0 to High(Radii) do
  begin
    Radius := Radii[i];

    FByteMapPas.SetSize(40, 40);
    FByteMapSIMD.SetSize(40, 40);

    for y := 0 to FByteMapPas.Height - 1 do
      for x := 0 to FByteMapPas.Width - 1 do
      begin
        FByteMapPas.Value[x, y] := (x * 13 + y * 7) mod 256;
        FByteMapSIMD.Value[x, y] := FByteMapPas.Value[x, y];
      end;

    Rebind([isPascal]);
    DraftBlur8(FByteMapPas, Radius);

    Rebind;
    DraftBlur8(FByteMapSIMD, Radius);

    MaxDiff := 0;
    p1 := PByte(FByteMapPas.Bits);
    p2 := PByte(FByteMapSIMD.Bits);
    for x := 0 to FByteMapPas.Width * FByteMapPas.Height - 1 do
    begin
      Diff := Abs(Integer(p1^) - Integer(p2^));
      if Diff > MaxDiff then
        MaxDiff := Diff;
      Inc(p1);
      Inc(p2);
    end;

    Check(MaxDiff <= 1, Format('DraftBlur8 SIMD vs Pas max diff %d exceeded 1 at radius %.1f', [MaxDiff, Radius]));
  end;

  Rebind;
end;

initialization
  RegisterTest(TTestDraftBlur.Suite);
end.
