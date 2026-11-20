unit TestScaleAlphaAndLuminance;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestScaleAlphaAndLuminance = class(TTestCase)
  published
    procedure TestLuminance601;
    procedure TestLuminance709;
    procedure TestScaleAlphaLine;
    procedure TestScaleAlphaMems;
    procedure TestApplyAlphaMask;
  end;

implementation

{ TTestScaleAlphaAndLuminance }

procedure TTestScaleAlphaAndLuminance.TestLuminance601;
var
  Y: Byte;
begin
  // Fully transparent pixel -> 0
  CheckEquals(0, Luminance601($00FFFFFF));

  // Black opaque pixel -> 0
  CheckEquals(0, Luminance601($FF000000));

  // White opaque pixel -> 255
  CheckEquals(255, Luminance601($FFFFFFFF));

  // Pure Red $FFFF0000 -> 0.299 * 255 = 76
  Y := Luminance601($FFFF0000);
  Check(Abs(Y - 76) <= 1, 'Luminance601 Red failed');

  // Pure Green $FF00FF00 -> 0.587 * 255 = 150
  Y := Luminance601($FF00FF00);
  Check(Abs(Y - 150) <= 1, 'Luminance601 Green failed');

  // Pure Blue $FF0000FF -> 0.114 * 255 = 29
  Y := Luminance601($FF0000FF);
  Check(Abs(Y - 29) <= 1, 'Luminance601 Blue failed');
end;

procedure TTestScaleAlphaAndLuminance.TestLuminance709;
var
  Y: Byte;
begin
  // Fully transparent pixel -> 0
  CheckEquals(0, Luminance709($00FFFFFF));

  // Black opaque pixel -> 0
  CheckEquals(0, Luminance709($FF000000));

  // White opaque pixel -> 255
  CheckEquals(255, Luminance709($FFFFFFFF));

  // Pure Red $FFFF0000 -> 0.2126 * 255 = 54
  Y := Luminance709($FFFF0000);
  Check(Abs(Y - 54) <= 1, 'Luminance709 Red failed');

  // Pure Green $FF00FF00 -> 0.7152 * 255 = 182..183
  Y := Luminance709($FF00FF00);
  Check(Abs(Y - 182) <= 1, 'Luminance709 Green failed');

  // Pure Blue $FF0000FF -> 0.0722 * 255 = 18..19
  Y := Luminance709($FF0000FF);
  Check(Abs(Y - 18) <= 1, 'Luminance709 Blue failed');
end;

procedure TTestScaleAlphaAndLuminance.TestScaleAlphaLine;
var
  Src, Dst: array[0..3] of TColor32Entry;
begin
  // Set up destination RGB $112233 with alpha $FF (255)
  Dst[0].ARGB := $FF112233;
  Dst[1].ARGB := $FF112233;
  Dst[2].ARGB := $FF112233;
  Dst[3].ARGB := $FF112233;

  // Source alphas: 255 (no change), 128 (half), 0 (zero), 64
  // Source pixels have non-zero RGB components to verify RGB does not pollute alpha
  Src[0].ARGB := $FFAABBCC;
  Src[1].ARGB := $80AABBCC;
  Src[2].ARGB := $00AABBCC;
  Src[3].ARGB := $40AABBCC;

  ScaleAlphaLine(@Src[0], @Dst[0], 4);

  // RGB channels must remain untouched ($112233)
  CheckEquals($11, Dst[0].R);
  CheckEquals($22, Dst[0].G);
  CheckEquals($33, Dst[0].B);

  CheckEquals(255, Dst[0].A);
  Check(Abs(Integer(Dst[1].A) - 128) <= 1, 'ScaleAlphaLine half alpha failed');
  CheckEquals(0, Dst[2].A);
  Check(Abs(Integer(Dst[3].A) - 64) <= 1, 'ScaleAlphaLine quarter alpha failed');
end;

procedure TTestScaleAlphaAndLuminance.TestScaleAlphaMems;
var
  Colors: array[0..3] of TColor32Entry;
begin
  Colors[0].ARGB := $FF112233;
  Colors[1].ARGB := $80112233;
  Colors[2].ARGB := $40112233;
  Colors[3].ARGB := $00112233;

  ScaleAlphaMems(@Colors[0], 4, 0.5);

  // RGB channels must remain untouched ($112233)
  CheckEquals($11, Colors[0].R);
  CheckEquals($22, Colors[0].G);
  CheckEquals($33, Colors[0].B);

  Check(Abs(Integer(Colors[0].A) - 128) <= 1, 'ScaleAlphaMems 0.5 * 255 failed');
  Check(Abs(Integer(Colors[1].A) - 64) <= 1, 'ScaleAlphaMems 0.5 * 128 failed');
  Check(Abs(Integer(Colors[2].A) - 32) <= 1, 'ScaleAlphaMems 0.5 * 64 failed');
  CheckEquals(0, Colors[3].A);
end;

procedure TTestScaleAlphaAndLuminance.TestApplyAlphaMask;
var
  Src, Dst: array[0..3] of TColor32Entry;
begin
  Dst[0].ARGB := $FF112233;
  Dst[1].ARGB := $FF112233;
  Dst[2].ARGB := $FF112233;
  Dst[3].ARGB := $FF112233;

  // Source 0: Opaque white -> Gray = 255 -> Dst.A = 255
  Src[0].ARGB := $FFFFFFFF;
  // Source 1: Opaque black -> Gray = 0 -> Dst.A = 0
  Src[1].ARGB := $FF000000;
  // Source 2: Transparent white -> Gray = 0 -> Dst.A = 0
  Src[2].ARGB := $00FFFFFF;
  // Source 3: Half-transparent white -> Gray = 128 -> Dst.A = 128
  Src[3].ARGB := $80FFFFFF;

  ApplyAlphaMask(@Src[0], @Dst[0], 4);

  // RGB channels must remain untouched
  CheckEquals($11, Dst[0].R);
  CheckEquals($22, Dst[0].G);
  CheckEquals($33, Dst[0].B);

  CheckEquals(255, Dst[0].A);
  CheckEquals(0, Dst[1].A);
  CheckEquals(0, Dst[2].A);
  Check(Abs(Integer(Dst[3].A) - 128) <= 1, 'ApplyAlphaMask half mask failed');
end;

initialization
  RegisterTest(TTestScaleAlphaAndLuminance.Suite);
end.
