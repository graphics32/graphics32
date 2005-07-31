unit MandelUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, GR32_ExtImage, GR32, GR32_Resamplers, GR32_Rasterizers,
  StdCtrls, ExtCtrls, Menus;

const
  MAX_ITER = 320;
  DEF_ITER = 16;

type
  TRasterizerKind = (rkRegular, rkSwizzling, rkProgressive, rkTesseral, rkContour);

  TSamplerKind = (skDefault, skSS2X, skSS3X, skSS4X, skJittered);

  TMandelbrotSampler = class(TCustomSampler)
  public
    Bounds: TFloatRect;
    Image: TCustomPaintBox32;
    Palette: array [0..MAX_ITER + 255] of TColor32;
    constructor Create(AImage: TCustomPaintBox32);
    function GetSampleFloat(X, Y: Single): TColor32; override;
    procedure PrepareSampling; override;
  end;

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Rasterizer1: TMenuItem;
    File1: TMenuItem;
    Swizzling1: TMenuItem;
    Regularsampling1: TMenuItem;
    Tesseral1: TMenuItem;
    Progressive1: TMenuItem;
    Save1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Img: TSyntheticImage32;
    Sampler1: TMenuItem;
    Default1: TMenuItem;
    N5: TMenuItem;
    N2x2: TMenuItem;
    N3x2: TMenuItem;
    N4x2: TMenuItem;
    Adaptive2: TMenuItem;
    PatternSampler1: TMenuItem;
    Contour1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Swizzling1Click(Sender: TObject);
    procedure Regularsampling1Click(Sender: TObject);
    procedure Progressive1Click(Sender: TObject);
    procedure Tesseral1Click(Sender: TObject);
    procedure Default1Click(Sender: TObject);
    procedure N2x2Click(Sender: TObject);
    procedure N3x2Click(Sender: TObject);
    procedure N4x2Click(Sender: TObject);
    procedure Adaptive2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Jitteredsampling1Click(Sender: TObject);
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Contour1Click(Sender: TObject);
  public
    { Public declarations }
    Rasterizer: TRasterizer;
    Sampler: TCustomSampler;
    MandelSampler: TMandelbrotSampler;
    SuperSampler: TSuperSampler;
    AdaptiveSampler: TAdaptiveSuperSampler;
    JitteredSampler: TPatternSampler;
    AutoUpdate: Boolean;
    Adaptive: Boolean;
    SamplerKind: TSamplerKind;
    procedure SelectRasterizer(RasterizerKind: TRasterizerKind);
    procedure SelectSampler(ASamplerKind: TSamplerKind);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GR32_Blend, Math;

{ TMandelbrotSampler }

constructor TMandelbrotSampler.Create(AImage: TCustomPaintBox32);
begin
  Bounds := FloatRect(-2, -2, 2, 2);
  Image := AImage;
end;

function TMandelbrotSampler.GetSampleFloat(X, Y: Single): TColor32;
var
  CX, CY, ZX, ZY, ZXSqr, ZYSqr: Extended;
  I: Integer;
  W: Integer;
  C1, C2: TColor32;
const
  BAILOUT_VALUE = 4;
begin
  with Bounds do
  begin
    CX := Left + X * (Right - Left) / Image.Width;
    CY := Top + Y * (Bottom - Top) / Image.Height;
  end;

{ Mandelbrot iteration: Z(n+1) = Z(n+1)^2 + C }
  ZX := 0; ZY := 0;
  ZXSqr := 0; ZYSqr := 0;
  I := 0;
  repeat
    ZY := 2 * ZY * ZX + CY;
    ZX := ZXSqr - ZYSqr - CX;
    ZXSqr := Sqr(ZX);
    ZYSqr := Sqr(ZY);
    if ZXSqr + ZYSqr > BAILOUT_VALUE then Break;
    Inc(I);
  until I = MAX_ITER;
  W := Round(16 * (ZX * ZX + ZY * ZY - 4));
  W := EnsureRange(W, 0, 255);

  C1 := Palette[I];
  C2 := Palette[I + 1];
  Result := CombineReg(C1, C2, W);
  EMMS;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AutoUpdate := True;
  MandelSampler := TMandelbrotSampler.Create(Img);
  AdaptiveSampler := TAdaptiveSuperSampler.Create(MandelSampler);
  SuperSampler := TSuperSampler.Create(MandelSampler);
  JitteredSampler := TPatternSampler.Create(MandelSampler);
  JitteredSampler.Pattern := CreateJitteredPattern(3, 3, 3, 3);
  Sampler := MandelSampler;
end;

procedure TMandelbrotSampler.PrepareSampling;
var
  I: Integer;
begin
  for I := 0 to MAX_ITER + 255 do
    Palette[I] := HSLtoRGB(I/DEF_ITER + 0.5, 1-I/DEF_ITER, 0.5*(1 + Sin(3+14*I/DEF_ITER)));
end;

procedure TForm1.SelectRasterizer(RasterizerKind: TRasterizerKind);
begin
  case RasterizerKind of
    rkRegular:
      begin
        Rasterizer := TRegularRasterizer.Create;
        TRegularRasterizer(Rasterizer).UpdateRowCount := 1;
      end;
    rkSwizzling:
      begin
        Rasterizer := TSwizzlingRasterizer.Create;
      end;
    rkProgressive:
      begin
        Rasterizer := TProgressiveRasterizer.Create;
      end;
    rkTesseral:
      begin
        Rasterizer := TTesseralRasterizer.Create;
      end;
    rkContour:
      begin
        Rasterizer := TContourRasterizer.Create;
      end;
  end;
  Rasterizer.Sampler := Sampler;
  Img.Rasterizer := Rasterizer;
end;

procedure TForm1.Regularsampling1Click(Sender: TObject);
begin
  SelectRasterizer(rkRegular);
end;

procedure TForm1.Swizzling1Click(Sender: TObject);
begin
  SelectRasterizer(rkSwizzling);
end;

procedure TForm1.Progressive1Click(Sender: TObject);
begin
  SelectRasterizer(rkProgressive);
end;

procedure TForm1.Tesseral1Click(Sender: TObject);
begin
  SelectRasterizer(rkTesseral);
end;

procedure TForm1.Default1Click(Sender: TObject);
begin
  SelectSampler(skDefault);
end;

procedure TForm1.SelectSampler(ASamplerKind: TSamplerKind);
const
  SLEVEL: array [skSS2X..skSS4X] of Integer = (2, 3, 4);
begin
  SamplerKind := ASamplerKind;
  case SamplerKind of
    skDefault: Sampler := MandelSampler;
    skSS2X, skSS3X, skSS4X:
      begin
        if Adaptive then
        begin
          Sampler := AdaptiveSampler;
          AdaptiveSampler.Level := SLEVEL[SamplerKind];
        end
        else
        begin
          Sampler := SuperSampler;
          SuperSampler.SamplingX := SLEVEL[SamplerKind];
          SuperSampler.SamplingY := SLEVEL[SamplerKind];
        end;
      end;
    skJittered:
      begin
        Sampler := JitteredSampler;
      end;
  end;
  Rasterizer.Sampler := Sampler;
end;

procedure TForm1.N2x2Click(Sender: TObject);
begin
  SelectSampler(skSS2X);
end;

procedure TForm1.N3x2Click(Sender: TObject);
begin
  SelectSampler(skSS3X);
end;

procedure TForm1.N4x2Click(Sender: TObject);
begin
  SelectSampler(skSS4X);
end;

procedure TForm1.Jitteredsampling1Click(Sender: TObject);
begin
  SelectSampler(skJittered);
end;

procedure TForm1.Adaptive2Click(Sender: TObject);
begin
  Adaptive := Adaptive2.Checked;
  SelectSampler(SamplerKind);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SelectRasterizer(rkRegular);
end;


procedure TForm1.ImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cX, cY, L, T, W, H, Scale: Extended;
begin
  case Button of
    mbLeft: Scale := 1/2;
    mbRight: Scale := 2;
  end;
  cX := X / Img.Width;
  cY := Y / Img.Height;
  with MandelSampler do
  begin
    L := Bounds.Left;
    T := Bounds.Top;
    W := Bounds.Right - Bounds.Left;
    H := Bounds.Bottom - Bounds.Top;
    Bounds.Left := L + cX * W - W * Scale/2;
    Bounds.Top := T + cY * H - H * Scale/2;
    Bounds.Right := Bounds.Left + W * Scale;
    Bounds.Bottom := Bounds.Top + H * Scale;
  end;
  Img.Rasterize;
end;

procedure TForm1.Contour1Click(Sender: TObject);
begin
  SelectRasterizer(rkContour);
end;

end.
