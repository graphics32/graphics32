unit TestResampler;

interface

{-$define DUMP_FILES}

uses
  Classes, Types,
  TestFramework;

type
  TTestResampler = class(TTestCase)
  private
  public
    procedure TestPremultiplication(Delta: integer);
  published
    procedure PremultiplicationInterpolate;
    procedure PremultiplicationResample;
    procedure PremultiplicationBlock;
  end;

implementation

uses
  SysUtils,
  Math,
  GR32,
  GR32_LowLevel,
  GR32_Resamplers;

{$RANGECHECKS OFF}

{ TTestResampler }

type
  TSumRec = record
    SumR: Int64;
    SumG: Int64;
    SumB: Int64;
  end;

procedure TTestResampler.TestPremultiplication(Delta: integer);
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

procedure TTestResampler.PremultiplicationBlock;
begin
  TestPremultiplication(0);
end;

procedure TTestResampler.PremultiplicationInterpolate;
begin
  TestPremultiplication(1);
end;

procedure TTestResampler.PremultiplicationResample;
begin
  TestPremultiplication(-1);
end;

initialization
  RegisterTest(TTestResampler.Suite);
end.


