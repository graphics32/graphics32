unit TestHSL;

interface

uses
  Classes, Types,
  TestFramework;

type
  TTestHSL = class(TTestCase)
  private
  public
  published
    procedure TestRGBRoundtrip_Float;
    procedure TestRGBRoundtrip_Byte;
    procedure TestRGBtoHSL_FloatVsByte;
  end;

implementation

uses
  SysUtils,
  GR32;

procedure TTestHSL.TestRGBRoundtrip_Float;
begin
  for var R := 0 to 255 do
    for var G := 0 to 255 do
      for var B := 0 to 255 do
      begin
        var Alpha := Random(256);
        var RGB1 := Color32(R, G, B, Alpha);
        var H, S, L: Single;

        RGBtoHSL(RGB1, H, S, L);

        Check((H >= 0) and (H < 1), 'Hue out of range');
        Check((S >= 0) and (S <= 1), 'Saturation out of range');
        Check((L >= 0) and (L <= 1), 'Lightness out of range');

        var RGB2 := HSLtoRGB(H, S, L, Alpha);

        if (RGB1 <> RGB2) then // Avoid slow Format() in Check
          Fail(Format('RGB: %d, %d, %d -> HSL: %.1n, %.1n, %.1n -> RGB: %d, %d, %d',
            [R, G, B, H*360, S*100, L*100, RedComponent(RGB2), GreenComponent(RGB2), BlueComponent(RGB2)]));

        CheckEquals(Alpha, AlphaComponent(RGB2), 'Alpha');
      end;
end;

procedure TTestHSL.TestRGBRoundtrip_Byte;
begin
  for var R := 0 to 255 do
    for var G := 0 to 255 do
      for var B := 0 to 255 do
      begin
        var Alpha := Random(256);
        var RGB1 := Color32(R, G, B, Alpha);
        var H, S, L: Byte;

        RGBtoHSL(RGB1, H, S, L);

        var RGB2 := HSLtoRGB(H, S, L, Alpha);

        if (RGB1 <> RGB2) then // Avoid slow Format() in Check
        begin
          var Count := 0;
          Inc(Count, Abs(R-RedComponent(RGB2)));
          Inc(Count, Abs(G-GreenComponent(RGB2)));
          Inc(Count, Abs(B-BlueComponent(RGB2)));

          // Allow some difference since the integer functions are merely
          // approximations due to inevitable inaccuracy of the limited
          // range of a byte.
          // In theory it should be possible to achieve a perfect rountrip
          // even within the range but in practice this isn't practically
          // possible.
          if (Count > 16) then
            Fail(Format('RGB: %d, %d, %d -> HSL: %d, %d, %d -> RGB: %d, %d, %d',
              [R, G, B, H, S, L, RedComponent(RGB2), GreenComponent(RGB2), BlueComponent(RGB2)]));
        end;

        CheckEquals(Alpha, AlphaComponent(RGB2), 'Alpha');
      end;
end;

procedure TTestHSL.TestRGBtoHSL_FloatVsByte;
begin
  for var R := 0 to 255 do
    for var G := 0 to 255 do
      for var B := 0 to 255 do
      begin
        var RGB1 := Color32(R, G, B);
        var Hf, Sf, Lf: Single;
        var Hb, Sb, Lb: Byte;

        RGBtoHSL(RGB1, Hf, Sf, Lf);
        RGBtoHSL(RGB1, Hb, Sb, Lb);

        var Hf2b := Round(Hf*255);
        var Sf2b := Round(Sf*255);
        var Lf2b := Round(Lf*255);

        var Count := 0;
        Inc(Count, Abs(Abs(integer(Hb)-128) - Abs(integer(Hf2b)-128))); // Hue wraps around at 256
        Inc(Count, Abs(Sb - Sf2b));
        Inc(Count, Abs(Lb - Lf2b));

        // Allow some difference since the integer functions are merely
        // approximations due to inevitable inaccuracy of the limited
        // range of a byte.
        if (Count > 4) then
          Fail(Format('RGB: %d,%d,%d -> (Float, Byte) Hue: %d - %d, Saturation: %d - %d, Lightness: %d - %d', [R, G, B, Hf2b, Hb, Sf2b, Sb, Lf2b, Lb]));
      end;
end;

initialization
  RegisterTest(TTestHSL.Suite);
end.



