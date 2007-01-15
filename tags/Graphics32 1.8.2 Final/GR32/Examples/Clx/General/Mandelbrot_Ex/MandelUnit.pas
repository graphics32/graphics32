{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit MandelUnit;
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is MandelBrot Example
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QMenus,
  QExtDlgs, QDialogs, GR32_Image, GR32_ExtImage, GR32, GR32_Resamplers,
  GR32_Rasterizers, Jpeg;

const
  MAX_ITER = 320;
  DEF_ITER = 16;

type
  TRasterizerKind = (rkRegular, rkProgressive, rkSwizzling, rkTesseral, rkContour);
  TSamplerKind = (skDefault, skSS2X, skSS3X, skSS4X, skPattern2, skPattern3, skPattern4);

  TMandelbrotSampler = class(TCustomSampler)
  public
    Bounds: TFloatRect;
    Image: TCustomPaintBox32;
    Palette: array [0..MAX_ITER + 255] of TColor32;
    constructor Create(AImage: TCustomPaintBox32);
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
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
    Adaptive: TMenuItem;
    PatternSampler1: TMenuItem;
    Contour1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    PatternSampler2: TMenuItem;
    N2: TMenuItem;
    PatternSampler3x1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure RasterizerMenuClick(Sender: TObject);
    procedure Default1Click(Sender: TObject);
    procedure AdaptiveClick(Sender: TObject);
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Save1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    { Public declarations }
    Rasterizer: TRasterizer;
    Sampler: TCustomSampler;
    MandelSampler: TMandelbrotSampler;
    SuperSampler: TSuperSampler;
    AdaptiveSampler: TAdaptiveSuperSampler;
    JitteredSampler: TPatternSampler;
    AutoUpdate: Boolean;
    SamplerKind: TSamplerKind;
    procedure SelectRasterizer(RasterizerKind: TRasterizerKind);
    procedure SelectSampler(ASamplerKind: TSamplerKind);
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

uses
  GR32_Blend, GR32_LowLevel, Math;

{ TMandelbrotSampler }

constructor TMandelbrotSampler.Create(AImage: TCustomPaintBox32);
begin
  Bounds := FloatRect(-2, -2, 2, 2);
  Image := AImage;
end;

function TMandelbrotSampler.GetSampleFloat(X, Y: TFloat): TColor32;
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
  W := Clamp(W);

  C1 := Palette[I];
  C2 := Palette[I + 1];
  Result := CombineReg(C1, C2, W);
  EMMS;
end;

procedure TMandelbrotSampler.PrepareSampling;
var
  I: Integer;
begin
  for I := 0 to MAX_ITER + 255 do
    Palette[I] := HSLtoRGB(I/DEF_ITER + 0.5, 1 - I/DEF_ITER,
      0.5 * (1 + Sin(3 + 14 * I / DEF_ITER)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AutoUpdate := True;
  MandelSampler := TMandelbrotSampler.Create(Img);
  AdaptiveSampler := TAdaptiveSuperSampler.Create(MandelSampler);
  SuperSampler := TSuperSampler.Create(MandelSampler);
  JitteredSampler := TPatternSampler.Create(MandelSampler);
  Sampler := MandelSampler;
  SelectRasterizer(rkProgressive);
end;

procedure TForm1.SelectRasterizer(RasterizerKind: TRasterizerKind);
const
  RASTERIZERCLASS: array[TRasterizerKind] of TRasterizerClass =
    (TRegularRasterizer, TProgressiveRasterizer, TSwizzlingRasterizer,
     TTesseralRasterizer, TContourRasterizer);
begin
  Rasterizer := RASTERIZERCLASS[RasterizerKind].Create;
  if Rasterizer is TRegularRasterizer then
    TRegularRasterizer(Rasterizer).UpdateRowCount := 1;
  Rasterizer.Sampler := Sampler;
  Img.Rasterizer := Rasterizer;
end;

procedure TForm1.RasterizerMenuClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    SelectRasterizer(TRasterizerKind(TMenuItem(Sender).Tag));
end;

procedure TForm1.Default1Click(Sender: TObject);
begin
  if Sender is TMenuItem then
    SelectSampler(TSamplerKind(TMenuItem(Sender).Tag));
end;

procedure TForm1.SelectSampler(ASamplerKind: TSamplerKind);
const
  SLEVEL: array [skSS2X..skSS4X] of Integer = (2, 3, 4);
  PSAMPLES: array [skPattern2..skPattern4] of Integer = (2, 3, 4);
begin
  SamplerKind := ASamplerKind;
  Adaptive.Enabled := False;
  case SamplerKind of
    skDefault: Sampler := MandelSampler;
    skSS2X..skSS4X:
      begin
        Adaptive.Enabled := True;
        if Adaptive.Checked then
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
    skPattern2..skPattern4:
      begin
        JitteredSampler.Pattern := CreateJitteredPattern(8, 8, PSAMPLES[SamplerKind], PSAMPLES[SamplerKind]);
        Sampler := JitteredSampler;
      end;
  end;
  Rasterizer.Sampler := Sampler;
  Img.Rasterize;
end;

procedure TForm1.AdaptiveClick(Sender: TObject);
begin
  SelectSampler(SamplerKind);
end;

procedure TForm1.ImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cX, cY, L, T, W, H, Scale: Extended;
begin
  case Button of
    mbLeft: Scale := 1 / 2;
    mbRight: Scale := 2;
  else
    Scale := 1;
  end;

  cX := X / Img.Width;
  cY := Y / Img.Height;
  with MandelSampler do
  begin
    L := Bounds.Left;
    T := Bounds.Top;
    W := Bounds.Right - Bounds.Left;
    H := Bounds.Bottom - Bounds.Top;
    Bounds.Left := L + cX * W - W * Scale / 2;
    Bounds.Top := T + cY * H - H * Scale / 2;
    Bounds.Right := Bounds.Left + W * Scale;
    Bounds.Bottom := Bounds.Top + H * Scale;
  end;
  Img.Rasterize;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    Img.Buffer.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
{ Note: The synthetic image control must be freed before the samplers,
  since they are potentially used by the rendering thread. }
  Img.Free;

  MandelSampler.Free;
  SuperSampler.Free;
  AdaptiveSampler.Free;
  JitteredSampler.Free;
end;

end.
