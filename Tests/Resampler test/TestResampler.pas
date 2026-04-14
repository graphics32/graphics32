unit TestResampler;

interface

{-$define DUMP_FILES}

uses
  Classes, Types,
  TestFramework;

type
  TTestResamplerPremultiplication = class(TTestCase)
  private
  public
    procedure TestPremultiplication(Delta: integer);
  published
    procedure StretchTransferInterpolate;
    procedure StretchTransferResample;
    procedure StretchTransferBlock;

    procedure Interpolator;
    procedure AlphaInterpolator;
  end;

implementation

uses
  SysUtils,
  Math,
  GR32,
  GR32_LowLevel,
  GR32_Blend,
  GR32_Resamplers;

{$RANGECHECKS OFF}

type
  TColorGrid = record
    Row1: array[0..1] of TColor32;
    Row2: array[0..1] of TColor32;
  end;

{ TTestResamplerPremultiplication }

type
  TSumRec = record
    SumR: Int64;
    SumG: Int64;
    SumB: Int64;
  end;

procedure TTestResamplerPremultiplication.TestPremultiplication(Delta: integer);
const
  ColorOpaque: TColor32 = $FF00FF00; // Opaque green
  ColorTransparent: TColor32 = $00FF00FF; // Transparent Red/Blue

  procedure SumColors(Bitmap: TBitmap32; var SumRec: TSumRec);
  begin
    SumRec := Default(TSumRec);
    var Pixel := PColor32Entry(Bitmap.Bits);
    for var i := 0 to Bitmap.Width*Bitmap.Height-1 do
    begin
      // Sum alpha-premultiplied colors * 255
      Inc(SumRec.SumR, Pixel.R * Pixel.A);
      Inc(SumRec.SumG, Pixel.G * Pixel.A);
      Inc(SumRec.SumB, Pixel.B * Pixel.A);

      Inc(Pixel);
    end;
    // Average
    SumRec.SumR := SumRec.SumR div Bitmap.Width*Bitmap.Height;
    SumRec.SumG := SumRec.SumG div Bitmap.Width*Bitmap.Height;
    SumRec.SumB := SumRec.SumB div Bitmap.Width*Bitmap.Height;
  end;

  procedure DoTest(BitmapSource, BitmapDest: TBitmap32; ResamplerClass: TCustomResamplerClass; const Orientation: string);
  begin
    // We're using dmOpaque draw mode so the background color doesn't (shouldn't) really matter.
    // If we use dmBlend then the background *must* be black for this test since dmBlend assumes
    // the target alpha is 255.
    BitmapDest.Clear($00FFFFFF);

    var Resampler := ResamplerClass.Create(nil);
    try

      StretchTransfer(
        BitmapDest, BitmapDest.BoundsRect, BitmapDest.BoundsRect,
        BitmapSource, BitmapSource.BoundsRect,
        Resampler,
        dmOpaque);

    finally
      Resampler.Free;
    end;

{$if defined(DUMP_FILES)}
    BitmapSource.SaveToFile(Format('StretchTransfer-in %s %dx%d.bmp', [Orientation, BitmapSource.Width, BitmapSource.Height]));
    BitmapDest.SaveToFile(Format('StretchTransfer-out %s %dx%d.bmp', [Orientation, BitmapDest.Width, BitmapDest.Height]));
{$ifend}

    var Method: string;
    if (BitmapDest.Width > BitmapSource.Width) and (BitmapDest.Height > BitmapSource.Height) then
      Method := 'interpolate'
    else
      Method := 'resample';

    (*
    ** Verify that colors with alpha=0 didn't get included in the result
    *)
    var Pixel := PColor32Entry(BitmapDest.Bits);
    for var i := 0 to BitmapDest.Width*BitmapDest.Height-1 do
    begin
      if (Pixel.A <> 0) then
        Check(Pixel.ARGB and ColorTransparent = 0, 'Transparent color bleed: '+Method);
      Inc(Pixel);
    end;


    (*
    ** Verify that the average color (color/pixel) didn't change
    *)
    var SourceSum: TSumRec;
    var DestSum: TSumRec;
    SumColors(BitmapSource, SourceSum);
    SumColors(BitmapDest, DestSum);

    if (SourceSum.SumR <> 0) then
      Check(SameValue(DestSum.SumR / SourceSum.SumR, 1, 0.01), 'Color loss/gain: '+Method)
    else
      CheckEquals(0, DestSum.SumR, 'Transparent color bleed: '+Method);

    if (SourceSum.SumG <> 0) then
      Check(SameValue(DestSum.SumG / SourceSum.SumG, 1, 0.01), 'Color loss/gain: '+Method)
    else
      CheckEquals(0, DestSum.SumG, 'Transparent color bleed: '+Method);

    if (SourceSum.SumB <> 0) then
      Check(SameValue(DestSum.SumB / SourceSum.SumB, 1, 0.01), 'Color loss/gain '+Method)
    else
      CheckEquals(0, DestSum.SumB, 'Transparent color bleed '+Method);
  end;

begin
  (*
  ** This test verifies that:
  **
  **   1) Colors with Alpha=0 does not contribute to the result.
  **
  **   2) The average color is maintained.
  **
  ** It does not verify that the result is otherwise correct.
  *)

  var BitmapSource := TBitmap32.Create(500, 500);
//  var BitmapSource := TBitmap32.Create(3, 3);
  var BitmapDest := TBitmap32.Create;
  try

    for var y := 0 to BitmapSource.Height-1 do
    begin
      // Alternating bands of colors with Alpha=0 and Alpha=255
      var Color: TColor32;
      if (y and 1 = 0) then
        Color := ColorOpaque
      else
        Color := ColorTransparent;

      var p := BitmapSource.ScanLine[y];
      FillLongword(p^, BitmapSource.Width, Color);
    end;

    (*
    ** When the target is larger, in both dimensions, than the source
    ** bitmap, StretchTransfer internally employs a linear interpolator to
    ** produce the result. Otherwise (the target is smaller) a linear
    ** kernel resampler is used.
    *)


    (*
    ** Test with horizontal bands
    *)

    BitmapDest.SetSize(BitmapSource.Width+Delta, BitmapSource.Height+Delta);
    DoTest(BitmapSource, BitmapDest, TLinearResampler, 'hor');

    (*
    ** Test with vertical bands
    *)
    BitmapSource.Rotate90;
    BitmapDest.SetSize(BitmapSource.Width+Delta, BitmapSource.Height+Delta);
    DoTest(BitmapSource, BitmapDest, TLinearResampler, 'ver');

  finally
    BitmapSource.Free;
    BitmapDest.Free;
  end;
end;

procedure TTestResamplerPremultiplication.AlphaInterpolator;
type
  TColor64 = record
    B, G, R, A: Word; // 8.8 fixed precision
  end;

  TColorFloat = record
    B, G, R, A: Single;
  end;

  function AlphaInterpolator_Reference(WeightX_256, WeightY_256: Cardinal; p11, p12: PColor32): TColor32;
  var
    ColorCol1, ColorCol2: TColor32Entry;
    Weight2: Cardinal;
    ColorRow1, ColorRow2: TColorFloat;
  begin
    (*
    ** Lerp horizontally
    *)
    Weight2 := 256-WeightX_256;

    // Lerp first row horizontally
    ColorCol1 := TColor32Entry(p11^);
    Inc(p11);
    ColorCol2 := TColor32Entry(p11^);

    ColorRow1.R := (ColorCol1.R * ColorCol1.A / 255 * WeightX_256 + ColorCol2.R * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow1.G := (ColorCol1.G * ColorCol1.A / 255 * WeightX_256 + ColorCol2.G * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow1.B := (ColorCol1.B * ColorCol1.A / 255 * WeightX_256 + ColorCol2.B * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow1.A := (ColorCol1.A * WeightX_256 + ColorCol2.A * Weight2);

    // Lerp second row horizontally
    ColorCol1 := TColor32Entry(p12^);
    Inc(p12);
    ColorCol2 := TColor32Entry(p12^);

    ColorRow2.R := (ColorCol1.R * ColorCol1.A / 255 * WeightX_256 + ColorCol2.R * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow2.G := (ColorCol1.G * ColorCol1.A / 255 * WeightX_256 + ColorCol2.G * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow2.B := (ColorCol1.B * ColorCol1.A / 255 * WeightX_256 + ColorCol2.B * ColorCol2.A / 255 * Weight2) * 255;
    ColorRow2.A := (ColorCol1.A * WeightX_256 + ColorCol2.A * Weight2);

    (*
    ** Lerp vertically
    *)
    Weight2 := 256-WeightY_256;

    // Lerp vertically between first and second row lerps
    ColorRow1.A := (ColorRow1.A * WeightY_256 + ColorRow2.A * Weight2);
    ColorRow1.R := (ColorRow1.R * WeightY_256 + ColorRow2.R * Weight2);
    ColorRow1.G := (ColorRow1.G * WeightY_256 + ColorRow2.G * Weight2);
    ColorRow1.B := (ColorRow1.B * WeightY_256 + ColorRow2.B * Weight2);
    if (ColorRow1.A <> 0) then
    begin
      TColor32Entry(Result).R := Round(ColorRow1.R / ColorRow1.A);
      TColor32Entry(Result).G := Round(ColorRow1.G / ColorRow1.A);
      TColor32Entry(Result).B := Round(ColorRow1.B / ColorRow1.A);
      TColor32Entry(Result).A := Round(ColorRow1.A / 65536);
    end else
      Result := 0;
  end;

  procedure CheckAlmostEquals(Excepted, Actual: integer; const Msg: string; Epsilon: integer = 0);
  begin
    if (Abs(Excepted-Actual) > Epsilon) then
      CheckEquals(Excepted, Actual, Msg);
  end;

  procedure DoTest(const Testcase: TColorGrid);
  begin
    for var WeightX_256 := 0 to 256 do
    begin
      for var WeightY_256 := 0 to 256 do
      begin
        var ColorExpected: TColor32Entry;
        ColorExpected.ARGB := AlphaInterpolator_Reference(WeightX_256, WeightY_256, @Testcase.Row1[0], @Testcase.Row2[0]);

        var ColorActual: TColor32Entry;
        ColorActual.ARGB := GR32_Resamplers.AlphaInterpolator(WeightX_256, WeightY_256, @Testcase.Row1[0], @Testcase.Row2[0]);

        CheckAlmostEquals(ColorExpected.A, ColorActual.A, Format('[A] WeightX_256: %d, WeightY_256: %d', [WeightX_256, WeightY_256]));
        CheckAlmostEquals(ColorExpected.R, ColorActual.R, Format('[R] WeightX_256: %d, WeightY_256: %d', [WeightX_256, WeightY_256]));
        CheckAlmostEquals(ColorExpected.G, ColorActual.G, Format('[G] WeightX_256: %d, WeightY_256: %d', [WeightX_256, WeightY_256]));
        CheckAlmostEquals(ColorExpected.B, ColorActual.B, Format('[B] WeightX_256: %d, WeightY_256: %d', [WeightX_256, WeightY_256]));
      end;
    end;
  end;

const
  Testcase1: TColorGrid = (
    Row1: ($FF00FF00, $00FF00FF);
    Row2: ($00FF00FF, $FF00FF00)
  );

  Testcase2: TColorGrid = (
    Row1: ($FF000000, $FF0000FF);
    Row2: ($FF00FF00, $FFFFFFFF)
  );
begin
  DoTest(Testcase1);
  DoTest(Testcase2);
end;

procedure TTestResamplerPremultiplication.Interpolator;

  function Lerp(Color0, Color1: TColor32; Weight256: integer): TColor32;
  begin
    TColor32Entry(Result).A := ((256 - Weight256) * TColor32Entry(Color0).A + Weight256 * TColor32Entry(Color1).A) div 256;
    TColor32Entry(Result).R := ((256 - Weight256) * TColor32Entry(Color0).R + Weight256 * TColor32Entry(Color1).R) div 256;
    TColor32Entry(Result).G := ((256 - Weight256) * TColor32Entry(Color0).G + Weight256 * TColor32Entry(Color1).G) div 256;
    TColor32Entry(Result).B := ((256 - Weight256) * TColor32Entry(Color0).B + Weight256 * TColor32Entry(Color1).B) div 256;
  end;

  function Interpolator_Reference(WeightX_256, WeightY_256: Cardinal; p11, p12: PColor32): TColor32;
  var
    C1, C3: TColor32;
  begin
    C1 := p11^; Inc(p11);
    C3 := p12^; Inc(p12);

    if (WeightX_256 > 255) then
    begin
      C1 := p11^;
      C3 := p12^;
    end else
    if (WeightX_256 <> 0) then
    begin
      C1 := Lerp(C1, p11^, WeightX_256);
      C3 := Lerp(C3, p12^, WeightX_256);
    end;

    if (WeightY_256 > 255) then
      Result := C3
    else
    if (WeightY_256 = 0) then
      Result := C1
    else
      Result := Lerp(C1, C3, WeightY_256);
  end;

  procedure DoTest(const Testcase: TColorGrid);
  begin
    for var WeightX_256 := 0 to 256 do
    begin
      for var WeightY_256 := 0 to 256 do
      begin
        var ColorExpected: TColor32Entry;
        ColorExpected.ARGB := Interpolator_Reference(WeightX_256, WeightY_256, @Testcase.Row1[0], @Testcase.Row2[0]);

        var ColorActual: TColor32Entry;
        ColorActual.ARGB := GR32_Resamplers.Interpolator(WeightX_256, WeightY_256, @Testcase.Row1[0], @Testcase.Row2[0]);

        CheckEquals(ColorExpected.A, ColorActual.A);
        CheckEquals(ColorExpected.R, ColorActual.R);
        CheckEquals(ColorExpected.G, ColorActual.G);
        CheckEquals(ColorExpected.B, ColorActual.B);
      end;
    end;
  end;

const
  Testcase1: TColorGrid = (
    Row1: ($FF00FF00, $00FF00FF);
    Row2: ($00FF00FF, $FF00FF00)
  );

  Testcase2: TColorGrid = (
    Row1: ($FF000000, $FF0000FF);
    Row2: ($FF00FF00, $FFFFFFFF)
  );
begin
  DoTest(Testcase1);
  DoTest(Testcase2);
end;

procedure TTestResamplerPremultiplication.StretchTransferBlock;
begin
  TestPremultiplication(0);
end;

procedure TTestResamplerPremultiplication.StretchTransferInterpolate;
begin
  TestPremultiplication(1);
end;

procedure TTestResamplerPremultiplication.StretchTransferResample;
begin
  TestPremultiplication(-1);
end;

initialization
  var TestSuite := TTestSuite.Create('Premultiplication');
  RegisterTest(TestSuite);

  TestSuite.AddTests(TTestResamplerPremultiplication);

//  RegisterTest(TTestResamplerPremultiplication.Suite);
end.


