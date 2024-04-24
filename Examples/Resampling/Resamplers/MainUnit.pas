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

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, GR32_Image, GR32_System, GR32_RangeBars, GR32, GR32_Resamplers;

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
    PaintBoxResampling: TPaintBox32;
    TabResampling: TTabSheet;
    SidePanel: TPanel;
    StatusBar: TStatusBar;
    TabManual: TTabSheet;
    TabKernel: TTabSheet;
    ComboBoxWrapMode: TComboBox;
    PaintBoxCurve: TPaintBox32;
    TimerTableSize: TTimer;
    TimerParameter: TTimer;
    TabStretchTransfer: TTabSheet;
    PaintBoxStretchTransfer: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImagePatternResize(Sender: TObject);
    procedure ComboBoxPixelAccessModeChange(Sender: TObject);
    procedure GaugeBarParameterChange(Sender: TObject);
    procedure GaugeBarTableSizeChange(Sender: TObject);
    procedure KernelClassNamesListClick(Sender: TObject);
    procedure ComboBoxKernelModeChange(Sender: TObject);
    procedure ComboBoxResamplerClassNameChange(Sender: TObject);
    procedure PaintBoxResamplingPaintBuffer(Sender: TObject);
    procedure PaintBoxCurvePaintBuffer(Sender: TObject);
    procedure TimerTableSizeTimer(Sender: TObject);
    procedure TimerParameterTimer(Sender: TObject);
    procedure PaintBoxStretchTransferPaintBuffer(Sender: TObject);
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
  TypInfo,
  Math,
  GR32.ImageFormats.JPG,
  GR32_LowLevel;

{ TfmResamplersExample }

procedure TFrmResamplersExample.FormCreate(Sender: TObject);

  procedure LoadWrapModes;
  var
    WrapMode: TWrapMode;
    s: string;
  begin
    ComboBoxWrapMode.Items.Clear;
    for WrapMode := Low(TWrapMode) to High(TWrapMode) do
    begin
      s := GetEnumName(TypeInfo(TWrapMode), Ord(WrapMode));
      ComboBoxWrapMode.Items.Add(s);
    end;
  end;

  procedure LoadPixelAccessModes;
  var
    PixelAccessMode: TPixelAccessMode;
    s: string;
  begin
    ComboBoxPixelAccessMode.Items.Clear;
    for PixelAccessMode := Low(TPixelAccessMode) to High(TPixelAccessMode) do
    begin
      s := GetEnumName(TypeInfo(TPixelAccessMode), Ord(PixelAccessMode));
      ComboBoxPixelAccessMode.Items.Add(s);
    end;
  end;

var
  ResStream: TResourceStream;
begin
  BitmapPattern := TBitmap32.Create;
  BitmapPattern.OuterColor := $FFFF7F7F;
  BitmapPattern.OnChange := BitmapPatternChanged;

  ImagePattern.Bitmap.OuterColor := BitmapPattern.OuterColor;
  ImagePattern.SetupBitmap;

  BitmapSource := TBitmap32.Create;

  // load example image
  ResStream := TResourceStream.Create(HInstance, 'Iceland', RT_RCDATA);
  try
    BitmapSource.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;

  ResamplerList.GetClassNames(ComboBoxResamplerClassName.Items);
  KernelList.GetClassNames(ComboBoxKernelClassName.Items);
  LoadWrapModes;
  LoadPixelAccessModes;

  ComboBoxResamplerClassName.ItemIndex := 0;
  ComboBoxWrapMode.ItemIndex := Ord(wmClamp);
  ComboBoxPixelAccessMode.ItemIndex := Ord(pamSafe);
  ComboBoxKernelClassName.ItemIndex := 0;


  // build 16 x 16 test bitmap
  BuildTestBitmap(BitmapPattern);

  PaintBoxResampling.BufferOversize := 0;
  PaintBoxStretchTransfer.BufferOversize := 0;
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

  BitmapPattern.BeginUpdate;
  try

    KernelResampler.Kernel := TCustomKernelClass(KernelList[Index]).Create;

    LblParameter.Visible :=
      (KernelResampler.Kernel is TAlbrechtKernel) or
      (KernelResampler.Kernel is TGaussianKernel) or
      (KernelResampler.Kernel is TSinshKernel);
    GaugeBarParameter.Visible := LblParameter.Visible;

    SetKernelParameter(KernelResampler.Kernel);
    ComboBoxKernelModeChange(nil);

  finally
    BitmapPattern.EndUpdate;
  end;
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

procedure TFrmResamplersExample.PaintBoxStretchTransferPaintBuffer(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  GlobalPerfTimer.Start;
  BitmapPattern.DrawTo(TPaintBox32(Sender).Buffer, TPaintBox32(Sender).Buffer.BoundsRect);
  StatusBar.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for resampling.';
  Screen.Cursor := crDefault;
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
    ComboBoxPixelAccessModeChange(nil);

  finally
    BitmapPattern.EndUpdate;
  end;

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
  HasResampled: boolean;
begin
  sw := BitmapPattern.Width / ImagePattern.Bitmap.Width;
  sh := BitmapPattern.Height / ImagePattern.Bitmap.Height;

  HasResampled := False;
  Screen.Cursor := crAppStart;
  GlobalPerfTimer.Start;

  if TabResampling.Visible then
  begin
    PaintBoxResampling.Invalidate;
  end else
  if TabStretchTransfer.Visible then
    PaintBoxStretchTransfer.Invalidate
  else
  if BitmapPattern.WrapMode in [wmClamp, wmRepeat, wmMirror] then
  begin
    // manual resampling
    BitmapPattern.Resampler.PrepareSampling;
    try

      for Y := 0 to ImagePattern.Bitmap.Height - 1 do
        for X := 0 to ImagePattern.Bitmap.Width - 1 do
          ImagePattern.Bitmap.Pixel[X, Y] := BitmapPattern.Resampler.GetSampleFloat(X * sw - 0.5, Y * sh - 0.5);

    finally
      BitmapPattern.Resampler.FinalizeSampling;
    end;

    ImagePattern.Invalidate;
    HasResampled := True;
  end;

  if (HasResampled) then
    StatusBar.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for resampling.';

  Screen.Cursor := crDefault;
end;

procedure TFrmResamplersExample.ComboBoxKernelModeChange(Sender: TObject);
begin
  if (ComboBoxKernelMode.ItemIndex >= 0) and (BitmapPattern.Resampler is TKernelResampler) then
  begin
    TKernelResampler(BitmapPattern.Resampler).KernelMode := TKernelMode(ComboBoxKernelMode.ItemIndex);
  end;
end;

procedure TFrmResamplersExample.ComboBoxPixelAccessModeChange(Sender: TObject);
begin
  // Note: This event handler is shared by ComboBoxWrapMode and ComboBoxPixelAccessMode
  BitmapPattern.BeginUpdate;
  try
    BitmapPattern.WrapMode := TWrapMode(ComboBoxWrapMode.ItemIndex);
    TCustomResampler(BitmapPattern.Resampler).PixelAccessMode := TPixelAccessMode(ComboBoxPixelAccessMode.ItemIndex);
  finally
    BitmapPattern.EndUpdate;
  end;
end;

procedure TFrmResamplersExample.GaugeBarParameterChange(Sender: TObject);
begin
  TimerParameter.Enabled := False;
  TimerParameter.Enabled := (BitmapPattern.Resampler is TKernelResampler);
end;

procedure TFrmResamplersExample.GaugeBarTableSizeChange(Sender: TObject);
begin
  TimerTableSize.Enabled := False;
  TimerTableSize.Enabled := (BitmapPattern.Resampler is TKernelResampler);
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
  else
  if Kernel is TSinshKernel then
    TSinshKernel(Kernel).Coeff := 20 / GaugeBarParameter.Position;
end;

procedure TFrmResamplersExample.TimerParameterTimer(Sender: TObject);
begin
  TimerParameter.Enabled := False;
  SetKernelParameter(TKernelResampler(BitmapPattern.Resampler).Kernel);
  PaintBoxCurve.Invalidate;
end;

procedure TFrmResamplersExample.TimerTableSizeTimer(Sender: TObject);
begin
  TimerTableSize.Enabled := False;
  TKernelResampler(BitmapPattern.Resampler).TableSize := GaugeBarTableSize.Position;
end;

procedure TFrmResamplersExample.PaintBoxResamplingPaintBuffer(Sender: TObject);

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

  Screen.Cursor := crAppStart;

  SmallerBitmap := TBitmap32.Create;
  try
    SetupResampler(SmallerBitmap);
    SetupResampler(BitmapSource);

    GlobalPerfTimer.Start;

    PaintBoxResampling.Buffer.BeginUpdate;
    try
      // Shrink
      ScaleRatioX := PaintBoxResampling.Buffer.Width / (3 * BitmapSource.Width);
      ScaleRatioY := PaintBoxResampling.Buffer.Height / (4 * BitmapSource.Height);
      SmallerBitmap.SetSize(Round(BitmapSource.Width * ScaleRatioX), Round(BitmapSource.Height * ScaleRatioY));
      // Draw source to SmallerBitmap using resamler
      SmallerBitmap.Draw(SmallerBitmap.BoundsRect, BitmapSource.BoundsRect, BitmapSource);
      // Draw SmallerBitmap to paint box, centered horizontally
      // We're drawing 1:1 so no resampling done here
      PaintBoxResampling.Buffer.Draw((PaintBoxResampling.Buffer.Width - SmallerBitmap.Width) div 2, 10, SmallerBitmap);

      // Expand
      // Note that we're expanding the bitmap we just shrunk so the result
      // will exacerbate any precision loss caused by the resampling. This
      // is done on purpose in order to make any quality loss more visible.
      ScaleRatioX := (PaintBoxResampling.Buffer.Width - 20) / BitmapSource.Width;
      ScaleRatioY := (((PaintBoxResampling.Buffer.Height - 20) * 0.25) * 3) / (BitmapSource.Height);
      ExpandWidth := Round(BitmapSource.Width * ScaleRatioX);
      ExpandHeight := Round(BitmapSource.Height * ScaleRatioY);
      R.Left := (PaintBoxResampling.Buffer.Width - ExpandWidth) div 2;
      R.Right := R.Left + ExpandWidth;
      R.Top := SmallerBitmap.Height + 20;
      R.Bottom := SmallerBitmap.Height + 5 + ExpandHeight;
      // Draw SmallerBitmap to paintbox using resamler, centered horizontally
      PaintBoxResampling.Buffer.Draw(R, SmallerBitmap.BoundsRect, SmallerBitmap);
    finally
      PaintBoxResampling.Buffer.EndUpdate;
    end;

    StatusBar.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for rendering.';

  finally
    SmallerBitmap.Free;
  end;
  Screen.Cursor := crDefault;
end;

end.
