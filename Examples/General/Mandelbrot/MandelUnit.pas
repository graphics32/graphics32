unit MandelUnit;
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is MandelBrot Example
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, Menus, ExtDlgs, Dialogs, Types,
  GR32,
  GR32_Image,
  GR32_ExtImage,
  GR32_Resamplers,
  GR32_Rasterizers;

type
  TRasterizerKind = (rkRegular, rkProgressive, rkSwizzling, rkTesseral,
   rkContour, rkMultithreadedRegularRasterizer);
  TSamplerKind = (skDefault, skSS2X, skSS3X, skSS4X, skPattern2, skPattern3,
    skPattern4);
  TMandelbrotPalette = (mpGR32, mpRainbow, mpMonochrome, mpSimple);

  TMandelbrotSampler = class(TCustomSampler)
  private
    FPalette: array of TColor32;
    FWidthInv, FHeightInv: Single;
  protected
    procedure CalculatePalette;
  public
    MaxIterations: Integer;
    Bounds: TFloatRect;
    Image: TCustomPaintBox32;
    Palette: TMandelbrotPalette;
    constructor Create(AImage: TCustomPaintBox32);
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    procedure PrepareSampling; override;
  end;

  TMainForm = class(TForm)
    Img: TSyntheticImage32;
    MainMenu: TMainMenu;
    miAdaptive: TMenuItem;
    miContour: TMenuItem;
    miDefault: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    miMaxIterations: TMenuItem;
    miMaxIterations160: TMenuItem;
    miMaxIterations256: TMenuItem;
    miMaxIterations320: TMenuItem;
    miMaxIterations50: TMenuItem;
    miMaxIterations512: TMenuItem;
    miMultithreadedRegularRasterizer: TMenuItem;
    miPalette: TMenuItem;
    miPaletteDefault: TMenuItem;
    miPaletteMonochrome: TMenuItem;
    miPaletteRainbow: TMenuItem;
    miPaletteSimple: TMenuItem;
    miPatternSampler2x: TMenuItem;
    miPatternSampler3x: TMenuItem;
    miPatternSampler4x: TMenuItem;
    miProgressive: TMenuItem;
    miRasterizer: TMenuItem;
    miRegularSampling: TMenuItem;
    miSave: TMenuItem;
    miSuperSampler: TMenuItem;
    miSuperSampler2x: TMenuItem;
    miSuperSampler3x: TMenuItem;
    miSuperSampler4x: TMenuItem;
    miSwizzling: TMenuItem;
    miTesseral: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miAdaptiveClick(Sender: TObject);
    procedure miDefaultClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miMaxIterationsClick(Sender: TObject);
    procedure miPaletteClick(Sender: TObject);
    procedure miRasterizerClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    procedure TranslateX(Amount: TFloat);
    procedure TranslateY(Amount: TFloat);
    procedure Zoom(Center: TPoint; Factor: TFloat);
  public
    { Public declarations }
    Rasterizer: TRasterizer;
    Sampler: TCustomSampler;
    MandelSampler: TMandelbrotSampler;
    SuperSampler: TSuperSampler;
    AdaptiveSampler: TAdaptiveSuperSampler;
    JitteredSampler: TPatternSampler;
    SamplerKind: TSamplerKind;
    procedure SelectRasterizer(RasterizerKind: TRasterizerKind);
    procedure SelectSampler(ASamplerKind: TSamplerKind);
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
  GR32_Blend, GR32_LowLevel;

{ TMandelbrotSampler }

constructor TMandelbrotSampler.Create(AImage: TCustomPaintBox32);
begin
  MaxIterations := 320;
  Palette := mpGR32;
  Bounds := FloatRect(-2, -2, 2, 2);
  Image := AImage;
end;

function TMandelbrotSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  CX, CY, ZX, ZY, ZXSqr, ZYSqr: Extended;
  I, W, M: Integer;
  C1, C2: TColor32;
const
  CBailoutValue = 4;
  CQuarter = 0.25;
begin
  with Bounds do
  begin
    CX := Left + X * (Right - Left) * FWidthInv;
    CY := Top + Y * (Bottom - Top) * FHeightInv;
  end;

  M := Length(FPalette) - 1;

  { Check whether point lies in the period-2 bulb }
  ZY := Sqr(CY);
  if Sqr(CX - 1) + ZY < 0.0625 then
  begin
    Result := FPalette[M + 1];
    Exit;
  end;

  { Check whether point lies in the cardioid }
  ZX := Sqr(CX + CQuarter) + ZY;
  if ZX * (ZX - Cx - CQuarter) < CQuarter * ZY then
  begin
    Result := FPalette[M + 1];
    Exit;
  end;

  { Mandelbrot iteration: Z(n+1) = Z(n)^2 + C }
  ZX := 0;
  ZY := 0;
  ZXSqr := 0;
  ZYSqr := 0;
  I := 0;

  repeat
    ZY := 2 * ZY * ZX + CY;
    ZX := ZXSqr - ZYSqr - CX;
    ZXSqr := Sqr(ZX);
    ZYSqr := Sqr(ZY);
    if ZXSqr + ZYSqr > CBailoutValue then Break;
    Inc(I);
  until I = M;
  W := Round(16 * (ZX * ZX + ZY * ZY - 4));
  W := Clamp(W);

  C1 := FPalette[I];
  C2 := FPalette[I + 1];
  Result := CombineReg(C1, C2, W);
  EMMS;
end;

procedure TMandelbrotSampler.CalculatePalette;
var
  I: Integer;
  S, T: TFloat;
begin
  S := (321 / (MaxIterations + 1)) / 16;
  case Palette of
    mpGR32:
      for I := 0 to MaxIterations + 1 do
        FPalette[I] := HSLtoRGB(I * S + 0.5, 1 - I * S,
          0.5 * (1 + Sin(3 + 14 * I * S)));
    mpRainbow:
      begin
        T := 1 / (MaxIterations + 1);
        for I := 0 to MaxIterations + 1 do
          FPalette[I] := HSLtoRGB(0.5 * (1 - Sqr(1 - I * T)), 1 - I * S,
            0.1 + 0.5 * I * S);
      end;
    mpMonochrome:
      begin
        T := 1 / (MaxIterations + 1);
        for I := 0 to MaxIterations + 1 do
          FPalette[I] := Gray32(Round(255 * (1 - Sqr(Sqr(Sqr(1 - I * T))))));
      end;
    mpSimple:
      begin
        T := (1 shl 24) / (MaxIterations + 1);
        for I := 0 to MaxIterations + 1 do
          FPalette[I] := Round(I * T);
      end;
  end;
end;

procedure TMandelbrotSampler.PrepareSampling;
begin
  FWidthInv := 1 / Image.Width;
  FHeightInv := 1 / Image.Height;
  SetLength(FPalette, MaxIterations + 1);
  CalculatePalette;
end;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MandelSampler := TMandelbrotSampler.Create(Img);
  AdaptiveSampler := TAdaptiveSuperSampler.Create(MandelSampler);
  SuperSampler := TSuperSampler.Create(MandelSampler);
  JitteredSampler := TPatternSampler.Create(MandelSampler);
  Sampler := MandelSampler;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
{ Note: The synthetic image control must be freed before the samplers,
  since they are potentially used by the rendering thread. }
  Img.Free;

  MandelSampler.Free;
  SuperSampler.Free;
  AdaptiveSampler.Free;
  JitteredSampler.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SelectRasterizer(rkProgressive);
end;

procedure TMainForm.SelectRasterizer(RasterizerKind: TRasterizerKind);
const
  RASTERIZERCLASS: array[TRasterizerKind] of TRasterizerClass =
    (TRegularRasterizer, TProgressiveRasterizer, TSwizzlingRasterizer,
     TTesseralRasterizer, TContourRasterizer, TMultithreadedRegularRasterizer);
begin
  Rasterizer := RASTERIZERCLASS[RasterizerKind].Create;
  if Rasterizer is TRegularRasterizer then
    TRegularRasterizer(Rasterizer).UpdateRowCount := 1;
  Rasterizer.Sampler := Sampler;
  Img.Rasterizer := Rasterizer;
end;

procedure TMainForm.SelectSampler(ASamplerKind: TSamplerKind);
const
  SLEVEL: array [skSS2X..skSS4X] of Integer = (2, 3, 4);
  PSAMPLES: array [skPattern2..skPattern4] of Integer = (2, 3, 4);
begin
  SamplerKind := ASamplerKind;
  miAdaptive.Enabled := False;
  case SamplerKind of
    skDefault: Sampler := MandelSampler;
    skSS2X..skSS4X:
      begin
        miAdaptive.Enabled := True;
        if miAdaptive.Checked then
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

procedure TMainForm.TranslateX(Amount: TFloat);
begin
  with MandelSampler do
  begin
    Bounds.Left := Bounds.Left + Amount;
    Bounds.Right := Bounds.Right + Amount;
  end;
  Img.Rasterize;
end;

procedure TMainForm.TranslateY(Amount: TFloat);
begin
  with MandelSampler do
  begin
    Bounds.Top := Bounds.Top + Amount;
    Bounds.Bottom := Bounds.Bottom + Amount;
  end;
  Img.Rasterize;
end;

procedure TMainForm.Zoom(Center: TPoint; Factor: TFloat);
var
  cX, cY, L, T, W, H: Extended;
begin
  cX := Center.X / Img.Width;
  cY := Center.Y / Img.Height;
  with MandelSampler do
  begin
    L := Bounds.Left;
    T := Bounds.Top;
    W := Bounds.Right - Bounds.Left;
    H := Bounds.Bottom - Bounds.Top;
    if W = 0 then W := H;
    if H = 0 then H := W;
    Bounds.Left := cX * W - W * Factor * 0.5 + L;
    Bounds.Top := cY * H - H * Factor * 0.5 + T;
    Bounds.Right := W * Factor + Bounds.Left;
    Bounds.Bottom := H * Factor + Bounds.Top;
  end;
  Img.Rasterize;
end;

procedure TMainForm.miRasterizerClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  if not(Sender is TMenuItem) then Exit;
  mi := TMenuItem(Sender);
  mi.Checked := True;
  SelectRasterizer(TRasterizerKind(mi.Tag));
end;

procedure TMainForm.miDefaultClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    SelectSampler(TSamplerKind(TMenuItem(Sender).Tag));
end;

procedure TMainForm.miAdaptiveClick(Sender: TObject);
begin
  SelectSampler(SamplerKind);
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    Img.Buffer.SaveToFile(SavePictureDialog.FileName);
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miMaxIterationsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  MandelSampler.MaxIterations := TMenuItem(Sender).Tag;
  Img.Rasterize;
end;

procedure TMainForm.miPaletteClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  MandelSampler.Palette := TMandelbrotPalette(TMenuItem(Sender).Tag);
  Img.Rasterize;
end;

procedure TMainForm.ImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: 
      Zoom(GR32.Point(X, Y), 0.5);
    mbRight: 
      Zoom(GR32.Point(X, Y), 2);
  else
    // reset
    MandelSampler.Bounds := FloatRect(-2, -2, 2, 2);
    Img.Rasterize;
  end;
end;

procedure TMainForm.ImgMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssShift in Shift then
    TranslateX(-0.001 * WheelDelta)
  else
    TranslateY(-0.001 * WheelDelta);
end;

end.
