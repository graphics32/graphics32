unit MainUnit;

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
 * The Original Code is Resamplers Example
 *
 * The Initial Developer of the Original Code is
 * Michael Hansen <dyster_tid@hotmail.com> 
 * Mattias Andersson <mattias@centaurix.com> 
 * (parts of this example were taken from the previously published example,
 * FineResample Example by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Christian Budde (added parametrisation for some kernel resamplers)
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}
{.$DEFINE Ex}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, GR32_Image, GR32_System, GR32_RangeBars, GR32, GR32_Resamplers
  {$IFDEF Ex},GR32_ResamplersEx {$ENDIF};

type
  TFrmResamplersExample = class(TForm)
    ImagePattern: TImage32;
    ComboBoxPixelAccessMode: TComboBox;
    GaugeBarParameter: TGaugeBar;
    GaugeBarTableSize: TGaugeBar;
    ComboBoxKernelClassName: TComboBox;
    ComboBoxKernelMode: TComboBox;
    LblKernelClass: TLabel;
    LblKernelMode: TLabel;
    LblParameter: TLabel;
    LblPixelAccessMode: TLabel;
    LblResamplersClass: TLabel;
    LblTableSize: TLabel;
    LblWrapMode: TLabel;
    PageControl: TPageControl;
    PnlKernelProperties: TPanel;
    PanelKernel: TPanel;
    PnlResampler: TPanel;
    PnlResamplerProperties: TPanel;
    ComboBoxResamplerClassName: TComboBox;
    ResamplingPaintBox: TPaintBox32;
    TabResampling: TTabSheet;
    SidePanel: TPanel;
    StatusBar: TStatusBar;
    TabManual: TTabSheet;
    TabKernel: TTabSheet;
    ComboBoxWrapMode: TComboBox;
    PaintBoxCurve: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImagePatternResize(Sender: TObject);
    procedure ComboBoxPixelAccessModeChange(Sender: TObject);
    procedure GaugeBarParameterChange(Sender: TObject);
    procedure GaugeBarParameterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GaugeBarTableSizeChange(Sender: TObject);
    procedure KernelClassNamesListClick(Sender: TObject);
    procedure ComboBoxKernelModeChange(Sender: TObject);
    procedure ComboBoxResamplerClassNameChange(Sender: TObject);
    procedure ResamplingPaintBoxResize(Sender: TObject);
    procedure PaintBoxCurvePaintBuffer(Sender: TObject);
  private
    procedure SetKernelParameter(Kernel: TCustomKernel);
  protected
    procedure BuildTestBitmap(Bitmap: TBitmap32);
  private
    BitmapPattern : TBitmap32;
    BitmapSource: TBitmap32;
    procedure BitmapPatternChanged(Sender: TObject);
  public
  end;

var
  FrmResamplersExample: TFrmResamplersExample;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF FPC}
  LazJPG,
  {$ELSE}
  Jpeg,
  {$ENDIF}
  Math,
  GR32_LowLevel;

{ TfmResamplersExample }

procedure TFrmResamplersExample.FormCreate(Sender: TObject);
var
  ResStream: TResourceStream;
  JPEG: TJPEGImage;
begin
  BitmapPattern := TBitmap32.Create;
  BitmapPattern.OuterColor := $FFFF7F7F;
  BitmapPattern.OnChange := BitmapPatternChanged;

  ImagePattern.Bitmap.OuterColor := BitmapPattern.OuterColor;
  ImagePattern.SetupBitmap;

  BitmapSource := TBitmap32.Create;

  // load example image
  JPEG := TJPEGImage.Create;
  try
    ResStream := TResourceStream.Create(HInstance, 'Iceland', RT_RCDATA);
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    BitmapSource.Assign(JPEG);
  finally
    JPEG.Free;
  end;

  ResamplerList.GetClassNames(ComboBoxResamplerClassName.Items);
  KernelList.GetClassNames(ComboBoxKernelClassName.Items);
  ComboBoxResamplerClassName.ItemIndex := 0;
  ComboBoxKernelClassName.ItemIndex := 0;

  // build 16 x 16 test bitmap
  BuildTestBitmap(BitmapPattern);

  ResamplingPaintBox.BufferOversize := 0;
end;

procedure TFrmResamplersExample.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BitmapPattern.Free;
  BitmapSource.Free;
end;

procedure TFrmResamplersExample.BuildTestBitmap(Bitmap: TBitmap32);
var
  i, j: Integer;
  Color: TColor32;
const
  CBlackWhite32: array [0..1] of TColor32 = (clBlack32, clWhite32);
begin
  Bitmap.SetSize(16, 16);

  for i := 0 to 15 do
    for j := 0 to 15 do
      Bitmap.Pixel[i, j] := CBlackWhite32[(i + j) mod 2];

  for i := 0 to 15 do
  begin
    Color := Gray32(i * 255 div 15);
    Bitmap.PixelX[Fixed(i), Fixed( 9)] := Color;
    Bitmap.PixelX[Fixed(i), Fixed(10)] := Color;
  end;

  for i := 0 to 7 do
  begin
    Color := Gray32(i * 255 div 7);
    Bitmap.Pixel[i * 2, 11] := Color;
    Bitmap.Pixel[i * 2 + 1, 11] := Color;
    Bitmap.Pixel[i * 2, 12] := Color;
    Bitmap.Pixel[i * 2 + 1, 12] := Color;
    Bitmap.Pixel[i * 2, 13] := Color;
    Bitmap.Pixel[i * 2 + 1, 13] := Color;
  end;

  for i := 1 to 4 do
    for j := 1 to 4 do
      Bitmap.Pixel[i, j] := $FF5F5F5F;
  for i := 2 to 3 do
    for j := 2 to 3 do
      Bitmap.Pixel[i, j] := $FFAFAFAF;
end;

procedure TFrmResamplersExample.KernelClassNamesListClick(Sender: TObject);
var
  Index: Integer;
  KernelResampler: TKernelResampler;
begin
  if (not (BitmapPattern.Resampler is TKernelResampler)) then
    exit;

  Index := ComboBoxKernelClassName.ItemIndex;
  KernelResampler := TKernelResampler(BitmapPattern.Resampler);

  KernelResampler.Kernel := TCustomKernelClass(KernelList[Index]).Create;

  LblParameter.Visible := (KernelResampler.Kernel is TAlbrechtKernel) or
{$IFDEF Ex}
    (KernelResampler.Kernel is TGaussianKernel) or
    (KernelResampler.Kernel is TKaiserBesselKernel) or
    (KernelResampler.Kernel is TNutallKernel) or
    (KernelResampler.Kernel is TBurgessKernel) or
    (KernelResampler.Kernel is TBlackmanHarrisKernel) or
    (KernelResampler.Kernel is TLawreyKernel) or
{$ENDIF}
    (KernelResampler.Kernel is TSinshKernel);
  GaugeBarParameter.Visible := LblParameter.Visible;

  SetKernelParameter(KernelResampler.Kernel);

  PaintBoxCurve.Invalidate;
end;

procedure TFrmResamplersExample.PaintBoxCurvePaintBuffer(Sender: TObject);
var
  Buffer: TBitmap32;
  Kernel: TCustomKernel;
  i: Integer;
  KernelWidth, X, Y, Scale: Single;
  Color: TColor32;
const
  XScale : Single = 1.5;
  YScale : Single = 1 / 2.2;
begin
  Buffer := PaintBoxCurve.Buffer;
  Buffer.Clear(clBlack32);

  if (not (BitmapPattern.Resampler is TKernelResampler)) then
    exit;

  Kernel := TKernelResampler(BitmapPattern.Resampler).Kernel;

  SetKernelParameter(Kernel);
  KernelWidth := Kernel.GetWidth * XScale;

  // Vertical X grid lines
  Scale := 2 * KernelWidth / Buffer.Width;
  for i := Floor(-KernelWidth * 2) to Ceil(KernelWidth * 2) do
  begin
    X := 0.5 * (i / Scale + Buffer.Width);
    if (i = 0) then
      Color := clWhite32
    else
      Color := clGray32;
    Buffer.LineFS(X, 0, X, Buffer.Height - 1, Color);
  end;

  // Horizontal Y grid lines
  for i := -2 to 2 do
  begin
    Y := 0.5 * Buffer.Height * (i * YScale + 1);
    if (i = 0) then
      Color := clWhite32
    else
      Color := clGray32;
    Buffer.LineFS(0, Y, Buffer.Width - 1, Y, Color);
  end;

  // Kernel curve
  Buffer.PenColor := clTrGreen32;
  for i := 0 to Buffer.Width - 1 do
  begin
    Y := (1.1 - Kernel.Filter(i * Scale - KernelWidth)) * Buffer.Height * YScale;
    if (i = 0) then
      Buffer.MoveToF(i, Y)
    else
      Buffer.LineToFS(i, Y);
  end;
end;

procedure TFrmResamplersExample.ComboBoxResamplerClassNameChange(Sender: TObject);
var
  Resampler: TCustomResampler;
begin
  if (ComboBoxResamplerClassName.ItemIndex = -1) then
    exit;

  BitmapPattern.BeginUpdate;
  try
    Resampler := TCustomResamplerClass(ResamplerList[ComboBoxResamplerClassName.ItemIndex]).Create(BitmapPattern);
    KernelClassNamesListClick(nil);
  finally
    BitmapPattern.EndUpdate;
  end;
  BitmapPattern.Changed;

  PanelKernel.Visible := (Resampler is TKernelResampler);
  TabKernel.TabVisible := (Resampler is TKernelResampler);
end;

procedure TFrmResamplersExample.ImagePatternResize(Sender: TObject);
begin
  ImagePattern.SetupBitmap;
  BitmapPatternChanged(Self);
end;

procedure TFrmResamplersExample.BitmapPatternChanged(Sender: TObject);
var
  X, Y: Integer;
  sw, sh: Single;
begin
  sw := BitmapPattern.Width / ImagePattern.Bitmap.Width;
  sh := BitmapPattern.Height / ImagePattern.Bitmap.Height;

  GlobalPerfTimer.Start;

  if TabResampling.Visible then
    ResamplingPaintBoxResize(Self)
  else
  if BitmapPattern.WrapMode in [wmClamp, wmRepeat, wmMirror] then
  begin
    // manual resampling
    BitmapPattern.Resampler.PrepareSampling;

    for Y := 0 to ImagePattern.Bitmap.Height - 1 do
      for X := 0 to ImagePattern.Bitmap.Width - 1 do
        ImagePattern.Bitmap.Pixel[X, Y] := BitmapPattern.Resampler.GetSampleFloat(X * sw - 0.5, Y * sh - 0.5);

    BitmapPattern.Resampler.FinalizeSampling;
  end;

  StatusBar.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for rendering.';

  ImagePattern.Repaint;
end;

procedure TFrmResamplersExample.ComboBoxKernelModeChange(Sender: TObject);
begin
  if (ComboBoxKernelMode.ItemIndex >= 0) and (BitmapPattern.Resampler is TKernelResampler) then
  begin
    TKernelResampler(BitmapPattern.Resampler).KernelMode := TKernelMode(ComboBoxKernelMode.ItemIndex);
    KernelClassNamesListClick(Self);
  end;
end;

procedure TFrmResamplersExample.ComboBoxPixelAccessModeChange(Sender: TObject);
begin
  // Note: This event handler is shared by ComboBoxWrapMode and ComboBoxPixelAccessMode
  BitmapPattern.WrapMode := TWrapMode(ComboBoxWrapMode.ItemIndex);
  TCustomResampler(BitmapPattern.Resampler).PixelAccessMode := TPixelAccessMode(ComboBoxPixelAccessMode.ItemIndex);
end;

procedure TFrmResamplersExample.GaugeBarParameterChange(Sender: TObject);
begin
  if BitmapPattern.Resampler is TKernelResampler then
    SetKernelParameter(TKernelResampler(BitmapPattern.Resampler).Kernel);
end;

procedure TFrmResamplersExample.GaugeBarParameterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KernelClassNamesListClick(Sender);
end;

procedure TFrmResamplersExample.GaugeBarTableSizeChange(Sender: TObject);
begin
  LblTableSize.Caption := Format('Table Size (%d/100):', [GaugeBarTableSize.Position]);
end;

function Sinc(Value: TFloat): TFloat;
begin
  if Value <> 0 then
  begin
    Value := Value * Pi;
    Result := Sin(Value) / Value;
  end else
    Result := 1;
end;

procedure TFrmResamplersExample.SetKernelParameter(Kernel : TCustomKernel);
begin
  if Kernel is TAlbrechtKernel then
    TAlbrechtKernel(Kernel).Terms := Round(GaugeBarParameter.Position * 0.1) + 1
  else
  if Kernel is TGaussianKernel then
    TGaussianKernel(Kernel).Sigma := GaugeBarParameter.Position * 0.1 + 1
{$IFDEF Ex}
  else
  if Kernel is TKaiserBesselKernel then
    TKaiserBesselKernel(Kernel).Alpha := GaugeBarParameter.Position * 0.1 + 1
  else
  if Kernel is TNutallKernel then
    TNutallKernel(Kernel).ContinousDerivationType := TCDType(GaugeBarParameter.Position > 50)
  else
  if Kernel is TBurgessKernel then
    TBurgessKernel(Kernel).BurgessOpt := TBurgessOpt(GaugeBarParameter.Position > 50)
  else
  if Kernel is TBlackmanHarrisKernel then
    TBlackmanHarrisKernel(Kernel).Terms := Round(GaugeBarParameter.Position * 0.1) + 1
  else
  if Kernel is TLawreyKernel then
    TLawreyKernel(Kernel).Terms := Round(GaugeBarParameter.Position * 0.1) + 1
{$ENDIF}
  else
  if Kernel is TSinshKernel then
    TSinshKernel(Kernel).Coeff := 20 / GaugeBarParameter.Position;
end;

procedure TFrmResamplersExample.ResamplingPaintBoxResize(Sender: TObject);

  procedure SetupResampler(Bitmap:TBitmap32);
  var
    KernelResampler: TKernelResampler;
  begin
    TCustomResamplerClass(ResamplerList[ComboBoxResamplerClassName.ItemIndex]).Create(Bitmap);

    // Setup kernel resampler
    if (Bitmap.Resampler is TKernelResampler) then
    begin
      KernelResampler := TKernelResampler(Bitmap.Resampler);

      KernelResampler.Kernel := TCustomKernelClass(KernelList[ComboBoxKernelClassName.ItemIndex]).Create;
      SetKernelParameter(KernelResampler.Kernel);
      KernelResampler.KernelMode := TKernelMode(ComboBoxKernelMode.ItemIndex);
      KernelResampler.TableSize := GaugeBarTableSize.Position;
    end;
  end;

var
  ExpandWidth, ExpandHeight: Integer;
  SmallerBitmap: TBitmap32;
  R: TRect;
  ScaleRatioX, ScaleRatioY: Single;
begin
  if not TabResampling.Visible then
    Exit;

  SmallerBitmap := TBitmap32.Create;
  try
    SetupResampler(SmallerBitmap);
    SetupResampler(BitmapSource);

    ResamplingPaintBox.Buffer.BeginUpdate;
    try
      // Shrink
      ScaleRatioX := ResamplingPaintBox.Buffer.Width / (3 * BitmapSource.Width);
      ScaleRatioY := ResamplingPaintBox.Buffer.Height / (4 * BitmapSource.Height);
      SmallerBitmap.SetSize(Round(BitmapSource.Width * ScaleRatioX), Round(BitmapSource.Height * ScaleRatioY));
      // Draw source to SmallerBitmap using resamler
      SmallerBitmap.Draw(SmallerBitmap.BoundsRect, BitmapSource.BoundsRect, BitmapSource);
      // Draw SmallerBitmap to paint box, centered horizontally
      // We're drawing 1:1 so no resampling done here
      ResamplingPaintBox.Buffer.Draw((ResamplingPaintBox.Buffer.Width - SmallerBitmap.Width) div 2, 10, SmallerBitmap);

      // Expand
      // Note that we're expanding the bitmap we just shrunk so the result
      // will exacerbate any precision loss caused by the resampling. This
      // is done on purpose in order to make any quality loss more visible.
      ScaleRatioX := (ResamplingPaintBox.Buffer.Width - 20) / BitmapSource.Width;
      ScaleRatioY := (((ResamplingPaintBox.Buffer.Height - 20) * 0.25) * 3) / (BitmapSource.Height);
      ExpandWidth := Round(BitmapSource.Width * ScaleRatioX);
      ExpandHeight := Round(BitmapSource.Height * ScaleRatioY);
      R.Left := (ResamplingPaintBox.Buffer.Width - ExpandWidth) div 2;
      R.Right := R.Left + ExpandWidth;
      R.Top := SmallerBitmap.Height + 20;
      R.Bottom := SmallerBitmap.Height + 5 + ExpandHeight;
      // Draw SmallerBitmap to paintbox using resamler, centered horizontally
      ResamplingPaintBox.Buffer.Draw(R, SmallerBitmap.BoundsRect, SmallerBitmap);
    finally
      ResamplingPaintBox.Buffer.EndUpdate;
    end;
  finally
    SmallerBitmap.Free;
  end;
  ResamplingPaintBox.Repaint;
end;

end.
