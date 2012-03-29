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
  ExtCtrls, Menus, ExtDlgs, Dialogs, GR32, GR32_Image, GR32_ExtImage,
  GR32_Resamplers, GR32_Rasterizers;

const
  MAX_ITER = 320;
  DEF_ITER = 16;

type
  TRasterizerKind = (rkRegular, rkProgressive, rkSwizzling, rkTesseral,
   rkContour, rkMultithreadedRegularRasterizer);
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

  TMainForm = class(TForm)
    Img: TSyntheticImage32;
    MainMenu: TMainMenu;
    miAdaptive: TMenuItem;
    miContour: TMenuItem;
    miDefault: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
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
    miMultithreadedRegularRasterizer: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miAdaptiveClick(Sender: TObject);
    procedure miDefaultClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure RasterizerMenuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TMainForm.FormCreate(Sender: TObject);
{$IFDEF Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
{$IFDEF FPC}
  Img := TSyntheticImage32.Create(Self);
  Img.Parent := Self;
  Img.Height := 420;
  Img.Width := 468;
  Img.Align := alClient;
  Img.RepaintMode := rmDirect;
  Img.TabOrder := 0;
  Img.OnMouseDown := ImgMouseDown;
  Img.AutoRasterize := True;
  Img.Color := clBlack;
  Img.ClearBuffer := True;
{$ENDIF}

  MandelSampler := TMandelbrotSampler.Create(Img);
  AdaptiveSampler := TAdaptiveSuperSampler.Create(MandelSampler);
  SuperSampler := TSuperSampler.Create(MandelSampler);
  JitteredSampler := TPatternSampler.Create(MandelSampler);
  Sampler := MandelSampler;
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

procedure TMainForm.RasterizerMenuClick(Sender: TObject);
var mi: TMenuItem;
begin
  if Not(Sender is TMenuItem) then Exit;
  mi := TMenuItem(Sender);
  mi.Checked := True;
  SelectRasterizer(TRasterizerKind(mi.Tag));
end;

procedure TMainForm.miDefaultClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    SelectSampler(TSamplerKind(TMenuItem(Sender).Tag));
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

procedure TMainForm.miAdaptiveClick(Sender: TObject);
begin
  SelectSampler(SamplerKind);
end;

procedure TMainForm.ImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cX, cY, L, T, W, H, Scale: Extended;
begin
  case Button of
    mbLeft: Scale := 0.5;
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
    Bounds.Left := L + cX * W - W * Scale * 0.5;
    Bounds.Top := T + cY * H - H * Scale * 0.5;
    Bounds.Right := Bounds.Left + W * Scale;
    Bounds.Bottom := Bounds.Top + H * Scale;
  end;
  Img.Rasterize;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    Img.Buffer.SaveToFile(SavePictureDialog.FileName);
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

procedure TMainForm.miExitClick(Sender: TObject);
begin
 Close;
end;

end.
