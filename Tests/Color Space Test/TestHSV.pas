unit TestHSV;

interface

uses
  Classes, Types,
  TestFramework;

type
  TTestHSV = class(TTestCase)
  private
  public
  published
    procedure TestRGBRoundtrip_Float;
  end;

implementation

uses
  SysUtils,
  GR32;

procedure TTestHSV.TestRGBRoundtrip_Float;
begin
  for var R := 0 to 255 do
    for var G := 0 to 255 do
      for var B := 0 to 255 do
      begin
        var Alpha := Random(256);
        var RGB1 := Color32(R, G, B, Alpha);
        var H, S, V: Single;

        RGBtoHSV(RGB1, H, S, V);

        Check((H >= 0) and (H < 1), 'Hue out of range');
        Check((S >= 0) and (S <= 1), 'Saturation out of range');
        Check((V >= 0) and (V <= 1), 'Value out of range');

        var RGB2 := HSVtoRGB(H, S, V, Alpha);

        if (RGB1 <> RGB2) then // Avoid slow Format() in Check
          Fail(Format('RGB: %d, %d, %d -> HSV: %.1n, %.1n, %.1n -> RGB: %d, %d, %d',
            [R, G, B, H*360, S*100, V*100, RedComponent(RGB2), GreenComponent(RGB2), BlueComponent(RGB2)]));

        CheckEquals(Alpha, AlphaComponent(RGB2), 'Alpha');
      end;
end;

initialization
  RegisterTest(TTestHSV.Suite);
end.



