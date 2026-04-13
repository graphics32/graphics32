unit TestGR32BlendModes;

interface

{$I ..\..\Source\GR32.inc}

uses
  TestFramework,
  Classes, SysUtils, Math,
  GR32,
  GR32.Blend.Modes,
  GR32.Blend.Modes.PhotoShop,
  GR32.Blend.Modes.PorterDuff,
  GR32.Blend.Modes.Extra;

type
  TTestBlendModes = class(TTestCase)
  private
    procedure CheckBlend(BlenderClass: TGraphics32BlenderClass; F, B: TColor32; Expected: TColor32; const Msg: string = ''; Epsilon: integer = 1); overload;
    procedure CheckBlend(const BlenderID: string; F, B: TColor32; Expected: TColor32; const Msg: string = ''; Epsilon: integer = 1); overload;
  published
    // Photoshop Separable Modes
    procedure TestNormal;
    procedure TestMultiply;
    procedure TestScreen;
    procedure TestOverlay;
    procedure TestHardLight;
    procedure TestSoftLight;
    procedure TestVividLight;
    procedure TestLinearLight;
    procedure TestPinLight;
    procedure TestHardMix;
    procedure TestColorDodge;
    procedure TestColorBurn;
    procedure TestLinearDodge;
    procedure TestLinearBurn;
    procedure TestDifference;
    procedure TestExclusion;
    procedure TestNegation;
    procedure TestLighten;
    procedure TestDarken;
    procedure TestDarkerColor;
    procedure TestLighterColor;

    // Photoshop Non-separable Modes
    procedure TestHue;
    procedure TestSaturation;
    procedure TestColor;
    procedure TestLuminosity;

    // Porter-Duff Operators
    procedure TestPorterDuffSrc;
    procedure TestPorterDuffSrcOver;
    procedure TestPorterDuffSrcIn;
    procedure TestPorterDuffSrcOut;
    procedure TestPorterDuffSrcAtop;
    procedure TestPorterDuffDest;
    procedure TestPorterDuffDestOver;
    procedure TestPorterDuffDestIn;
    procedure TestPorterDuffDestOut;
    procedure TestPorterDuffDestAtop;
    procedure TestPorterDuffClear;
    procedure TestPorterDuffXor;

    // Extra Modes
    procedure TestExtraErase;
    procedure TestExtraMask;
    procedure TestExtraAlpha;
  end;

implementation

{ TTestBlendModes }

procedure TTestBlendModes.CheckBlend(const BlenderID: string; F, B, Expected: TColor32; const Msg: string; Epsilon: integer);
begin
  var BlenderClass := Graphics32BlendService.BlenderByID(BlenderID);
  CheckTrue(BlenderClass <> nil, Format('Unknown blend ID: %s', [BlenderID]));
  var s := BlenderClass.Name;
  if (Msg <> '') then
    s := s + ': ' + Msg;
  CheckBlend(BlenderClass, F, B, Expected, s, Epsilon);
end;

procedure TTestBlendModes.CheckBlend(BlenderClass: TGraphics32BlenderClass; F, B: TColor32; Expected: TColor32; const Msg: string; Epsilon: integer);
var
  Blender: TCustomGraphics32Blender;
  Actual: TColor32;
  ae, aa: TColor32Entry;
begin
  Blender := BlenderClass.Create;
  try

    Actual := Blender.Blend(F, B);

    ae.ARGB := Expected;
    aa.ARGB := Actual;

    if (Abs(ae.A - aa.A) > Epsilon) or (Abs(ae.R - aa.R) > Epsilon) and (Abs(ae.G - aa.G) > Epsilon) and (Abs(ae.B - aa.B) > Epsilon) then
      CheckEquals(IntToHex(Expected, 8), IntToHex(Actual, 8), Msg)
    else
      CheckTrue(True);

  finally
    Blender.Free;
  end;
end;

procedure TTestBlendModes.TestNormal;
begin
  CheckBlend(TGraphics32BlenderNormal.ID, $FFFFFFFF, $FF000000, $FFFFFFFF, 'opaque over opaque');
  CheckBlend(TGraphics32BlenderNormal.ID, $80FFFFFF, $FF000000, $FF808080, 'semi over opaque');
end;

procedure TTestBlendModes.TestMultiply;
begin
  // Adobe/W3C formula: rAlpha := fAlpha + bAlpha * (1 - fAlpha);
  // For fAlpha=1, bAlpha=1 -> rAlpha=1
  // rColor := fAlpha * fColor * (1 - bAlpha) + bAlpha * bColor * (1 - fAlpha) + fAlpha * bAlpha * Blend(fColor, bColor)
  // For fAlpha=1, bAlpha=1 -> rColor := Blend(fColor, bColor)
  // BlendMultiply(128, 128) = 128 * 128 / 255 = 64
  CheckBlend(TGraphics32BlenderMultiply.ID, $FF808080, $FF808080, $FF404040, '50% gray');
  CheckBlend(TGraphics32BlenderMultiply.ID, $FF000000, $FFFFFFFF, $FF000000, 'black and white');
end;

procedure TTestBlendModes.TestScreen;
begin
  // BlendScreen(128, 128) = 255 - (255-128)*(255-128)/255 = 255 - 127*127/255 = 255 - 16129/255 = 255 - 63 = 192
  CheckBlend(TGraphics32BlenderScreen.ID, $FF808080, $FF808080, $FFC0C0C0, '50% gray');
end;

procedure TTestBlendModes.TestOverlay;
begin
  CheckBlend(TGraphics32BlenderOverlay.ID, $FF404040, $FF404040, $FF202020, 'dark');
  CheckBlend(TGraphics32BlenderOverlay.ID, $FFBFBFBF, $FFBFBFBF, $FFDFDFDF, 'light');
end;

procedure TTestBlendModes.TestHardLight;
begin
  CheckBlend(TGraphics32BlenderHardLight.ID, $FF404040, $FF404040, $FF202020, 'Light dark');
  CheckBlend(TGraphics32BlenderHardLight.ID, $FFBFBFBF, $FFBFBFBF, $FFDFDFDF, 'Light light');
end;

procedure TTestBlendModes.TestSoftLight;
begin
  CheckBlend(TGraphics32BlenderSoftLight.ID, $FF808080, $FF808080, $FF808080, 'neutral');
end;

procedure TTestBlendModes.TestVividLight;
begin
  // Cs=0 -> ColorBurn(0, Cb) -> 0
  CheckBlend(TGraphics32BlenderVividLight.ID, $FF000000, $FFFFFFFF, $FF000000, 'black over white');
  // Cs=1 -> ColorDodge(1, Cb) -> 1
  CheckBlend(TGraphics32BlenderVividLight.ID, $FFFFFFFF, $FF000000, $FFFFFFFF, 'white over black');
end;

procedure TTestBlendModes.TestLinearLight;
begin
  CheckBlend(TGraphics32BlenderLinearLight.ID, $FF000000, $FFFFFFFF, $FF000000, 'black over white');
  CheckBlend(TGraphics32BlenderLinearLight.ID, $FFFFFFFF, $FF000000, $FFFFFFFF, 'white over black');
end;

procedure TTestBlendModes.TestPinLight;
begin
  CheckBlend(TGraphics32BlenderPinLight.ID, $FF808080, $FF404040, $FF404040, '50% over dark');
end;

procedure TTestBlendModes.TestHardMix;
begin
  CheckBlend(TGraphics32BlenderHardMix.ID, $FF808080, $FF808080, $FFFFFFFF, 'neutral');
end;

procedure TTestBlendModes.TestColorDodge;
begin
  CheckBlend(TGraphics32BlenderColorDodge.ID, $FF000000, $FF808080, $FF808080, 'black');
  CheckBlend(TGraphics32BlenderColorDodge.ID, $FFFFFFFF, $FF808080, $FFFFFFFF, 'white');
end;

procedure TTestBlendModes.TestColorBurn;
begin
  CheckBlend(TGraphics32BlenderColorBurn.ID, $FFFFFFFF, $FF808080, $FF808080, 'white');
  CheckBlend(TGraphics32BlenderColorBurn.ID, $FF000000, $FF808080, $FF000000, 'black');
end;

procedure TTestBlendModes.TestLinearDodge;
begin
  CheckBlend(TGraphics32BlenderLinearDodge.ID, $FF404040, $FF404040, $FF808080);
end;

procedure TTestBlendModes.TestLinearBurn;
begin
  CheckBlend(TGraphics32BlenderLinearBurn.ID, $FFBFBFBF, $FFBFBFBF, $FF7F7F7F);
end;

procedure TTestBlendModes.TestDifference;
begin
  CheckBlend(TGraphics32BlenderDifference.ID, $FFFFFFFF, $FFFFFFFF, $FF000000, 'white-white');
  CheckBlend(TGraphics32BlenderDifference.ID, $FFFFFFFF, $FF000000, $FFFFFFFF, 'white-black');
end;

procedure TTestBlendModes.TestExclusion;
begin
  CheckBlend(TGraphics32BlenderExclusion.ID, $FF808080, $FF808080, $FF808080, 'neutral');
end;

procedure TTestBlendModes.TestNegation;
begin
  CheckBlend(TGraphics32BlenderNegation.ID, $FF000000, $FF000000, $FFFFFFFF, 'black-black');
end;

procedure TTestBlendModes.TestLighten;
begin
  CheckBlend(TGraphics32BlenderLighten.ID, $FF102030, $FF302010, $FF302030);
end;

procedure TTestBlendModes.TestDarken;
begin
  CheckBlend(TGraphics32BlenderDarken.ID, $FF102030, $FF302010, $FF102010);
end;

procedure TTestBlendModes.TestDarkerColor;
begin
  CheckBlend(TGraphics32BlenderDarkerColor.ID, $FF102030, $FF302010, $FF102030);
end;

procedure TTestBlendModes.TestLighterColor;
begin
  // Red(16,32,48) Lum = 0.3*16 + 0.59*32 + 0.11*48 = 4.8 + 18.88 + 5.28 = 28.96
  // Red(48,32,16) Lum = 0.3*48 + 0.59*32 + 0.11*16 = 14.4 + 18.88 + 1.76 = 35.04
  // 35 > 29 -> $FF302010
  CheckBlend(TGraphics32BlenderLighterColor.ID, $FF102030, $FF302010, $FF302010);
end;

procedure TTestBlendModes.TestHue;
begin
  // Hue of Red(255,0,0), Lum/Sat of Blue(0,0,255)
  // Blue: Lum = 0.11*255 = 28, Sat = 255
  // Red: Sat = 255
  // Result = SetLum(SetSat(Red, Sat(Blue)), Lum(Blue))
  // SetSat(Red, 255) -> Red(255,0,0)
  // SetLum(Red, 28) -> Red(28+(255-76)=207, 28-76=-48, 28-76=-48) -> ClipColor -> (28,0,0) approx
  CheckBlend(TGraphics32BlenderHue.ID, $FFFF0000, $FF0000FF, $FF5D0000, 'Red over Blue');
end;

procedure TTestBlendModes.TestSaturation;
begin
  // fs=Red(255,0,0) Sat=255, fb=Blue(0,0,128) Sat=128
  // Result = SetLum(SetSat(Blue, Sat(Red)), Lum(Blue))
  // SetSat(Blue(0,0,128), 255) -> Blue(0,0,255)
  // SetLum(Blue(0,0,255), Lum(Blue(0,0,128)=14)) -> Blue(0,0,14) approx
  CheckBlend(TGraphics32BlenderSaturation.ID, $FFFF0000, $FF000080, $FF000080, 'high over low');
end;

procedure TTestBlendModes.TestColor;
begin
  // fs=Red(255,0,0), fb=Gray(128,128,128)
  // Result = SetLum(Red, Lum(Gray)=128)
  // SetLum(Red(255,0,0), 128) -> Red(128+(255-76)=307, 128-76=52, 128-76=52) -> ClipColor
  // L=128, n=52, x=307 -> 307>255(1.0) -> C = L + (C-L)*(1-L)/(x-L)
  // R = 128 + (307-128)*(255-128)/(307-128) = 128 + 127 = 255
  // G = 128 + (52-128)*(127)/(179) = 128 - 76*127/179 = 128 - 54 = 74
  // B = 74
  // Result = $FF FF 4A 4A
  CheckBlend(TGraphics32BlenderColor.ID, $FFFF0000, $FF808080, $FFFF4A4A, 'Red over Gray');
end;

procedure TTestBlendModes.TestLuminosity;
begin
  CheckBlend(TGraphics32BlenderLuminance.ID, $FFFFFFFF, $FF000000, $FFFFFFFF, 'white over black');
end;

procedure TTestBlendModes.TestPorterDuffSrc;
begin
  CheckBlend(TGraphics32BlenderSrc.ID, $80FF0000, $FFFFFFFF, $80FF0000);
end;

procedure TTestBlendModes.TestPorterDuffSrcOver;
begin
  CheckBlend(TGraphics32BlenderSrcOver.ID, $80FF0000, $FF0000FF, $FF800080);
end;

procedure TTestBlendModes.TestPorterDuffSrcIn;
begin
  CheckBlend(TGraphics32BlenderSrcIn.ID, $800000FF, $FFFFFFFF, $800000FF, 'semi-blue over opaque');
  CheckBlend(TGraphics32BlenderSrcIn.ID, $FFFFFFFF, $00000000, $00000000, 'opaque over transparent');
end;

procedure TTestBlendModes.TestPorterDuffSrcOut;
begin
  CheckBlend(TGraphics32BlenderSrcOut.ID, $FFFFFFFF, $00000000, $FFFFFFFF, 'opaque over transparent');
  CheckBlend(TGraphics32BlenderSrcOut.ID, $FFFFFFFF, $FFFFFFFF, $00000000, 'opaque over opaque');
end;

procedure TTestBlendModes.TestPorterDuffSrcAtop;
begin
  CheckBlend(TGraphics32BlenderSrcAtop.ID, $80FF0000, $FF0000FF, $FF800080, 'semi-Red over opaque Blue');
end;

procedure TTestBlendModes.TestPorterDuffDest;
begin
  CheckBlend(TGraphics32BlenderDest.ID, $FFFFFFFF, $800000FF, $800000FF);
end;

procedure TTestBlendModes.TestPorterDuffDestOver;
begin
  CheckBlend(TGraphics32BlenderDestOver.ID, $FF0000FF, $80FF0000, $FF800080);
end;

procedure TTestBlendModes.TestPorterDuffDestIn;
begin
  CheckBlend(TGraphics32BlenderDestIn.ID, $FFFFFFFF, $800000FF, $800000FF, 'opaque over semi-blue');
end;

procedure TTestBlendModes.TestPorterDuffDestOut;
begin
  CheckBlend(TGraphics32BlenderDestOut.ID, $00000000, $FFFFFFFF, $FFFFFFFF, 'transparent over opaque');
end;

procedure TTestBlendModes.TestPorterDuffDestAtop;
begin
  CheckBlend(TGraphics32BlenderDestAtop.ID, $FF0000FF, $80FF0000, $FF800080, 'opaque Blue over semi-Red');
end;

procedure TTestBlendModes.TestPorterDuffClear;
begin
  CheckBlend(TGraphics32BlenderClear.ID, $FFFFFFFF, $FFFFFFFF, $00000000);
end;

procedure TTestBlendModes.TestPorterDuffXor;
begin
  CheckBlend(TGraphics32BlenderXor.ID, $FFFFFFFF, $FFFFFFFF, $00000000, 'opaque over opaque');
  CheckBlend(TGraphics32BlenderXor.ID, $FFFFFFFF, $00000000, $FFFFFFFF, 'opaque over transparent');
end;

procedure TTestBlendModes.TestExtraErase;
begin
  CheckBlend(TGraphics32BlenderErase.ID, $80FFFFFF, $FFFF0000, $7FFF0000, 'Erase 50% over Red');
end;

procedure TTestBlendModes.TestExtraMask;
begin
  CheckBlend(TGraphics32BlenderMask.ID, $80FFFFFF, $FFFF0000, $80FF0000, 'Mask 50% over Red');
end;

procedure TTestBlendModes.TestExtraAlpha;
begin
  CheckBlend(TGraphics32BlenderAlpha.ID, $FFFF0000, $FFFFFFFF, $55FFFFFF, 'Red over White');
end;

initialization
  RegisterTest(TTestBlendModes.Suite);

end.
