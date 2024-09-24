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

{$include GR32.inc}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls,
  GR32,
  GR32_Image,
  GR32_RangeBars,
  GR32_Resamplers;

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
    procedure ImagePatternResize(Sender: TObject);
    procedure ComboBoxResamplerClassNameChange(Sender: TObject);
    procedure ComboBoxPixelAccessModeChange(Sender: TObject);
    procedure ComboBoxKernelClassNameChange(Sender: TObject);
    procedure ComboBoxKernelModeChange(Sender: TObject);
    procedure GaugeBarParameterChange(Sender: TObject);
    procedure GaugeBarTableSizeChange(Sender: TObject);
    procedure PaintBoxStretchTransferPaintBuffer(Sender: TObject);
    procedure PaintBoxResamplingPaintBuffer(Sender: TObject);
    procedure PaintBoxCurvePaintBuffer(Sender: TObject);
    procedure TimerTableSizeTimer(Sender: TObject);
    procedure TimerParameterTimer(Sender: TObject);
  private
    procedure SetKernelParameter(Kernel: TCustomKernel);
    procedure GetKernelParameter(Kernel: TCustomKernel);
    procedure BuildTestBitmap(Bitmap: TBitmap32);
    procedure BitmapPatternChanged(Sender: TObject);
  private
    FBitmapPattern : TBitmap32;
    FBitmapSource: TBitmap32;
    FAlbrechtParam: integer;
    FGaussianParam: Single;
    FSinshParam: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  GR32_Polygons,
  GR32_LowLevel,
  GR32_System;

{ TfmResamplersExample }

constructor TFrmResamplersExample.Create(AOwner: TComponent);

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
  Stream: TStream;
begin
  inherited;

  FBitmapPattern := TBitmap32.Create;
  FBitmapPattern.OuterColor := $FFFF7F7F;
  FBitmapPattern.OnChange := BitmapPatternChanged;

  ImagePattern.Bitmap.OuterColor := FBitmapPattern.OuterColor;
  ImagePattern.SetupBitmap;

  FBitmapSource := TBitmap32.Create;

  // load example image
  Stream := TResourceStream.Create(HInstance, 'Iceland', RT_RCDATA);
  try
    FBitmapSource.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;

  ResamplerList.GetClassNames(ComboBoxResamplerClassName.Items);
  KernelList.GetClassNames(ComboBoxKernelClassName.Items);
  LoadWrapModes;
  LoadPixelAccessModes;

  PaintBoxResampling.BufferOversize := 0;
  PaintBoxStretchTransfer.BufferOversize := 0;

  // build 16 x 16 test bitmap
  FBitmapPattern.BeginUpdate;
  try
    BuildTestBitmap(FBitmapPattern);

    ComboBoxResamplerClassName.ItemIndex := 0;
    ComboBoxResamplerClassNameChange(nil);
    ComboBoxPixelAccessMode.ItemIndex := Ord(pamSafe);
    ComboBoxWrapMode.ItemIndex := Ord(wmClamp);
    ComboBoxPixelAccessModeChange(nil);
    ComboBoxKernelClassName.ItemIndex := 0;
    ComboBoxKernelClassNameChange(nil);
  finally
    FBitmapPattern.EndUpdate;
  end;
end;

destructor TFrmResamplersExample.Destroy;
begin
  FBitmapPattern.Free;
  FBitmapSource.Free;

  inherited;
end;

(*
** Build a bitmap with a test pattern for upsampling
*)
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

(*
** Update kernel with users' parameter value
*)
procedure TFrmResamplersExample.SetKernelParameter(Kernel : TCustomKernel);
begin
  if Kernel is TAlbrechtKernel then
  begin
    TAlbrechtKernel(Kernel).Terms := Round(GaugeBarParameter.Position * 0.1) + 1;
    FAlbrechtParam := TAlbrechtKernel(Kernel).Terms;
  end else
  if Kernel is TGaussianKernel then
  begin
    TGaussianKernel(Kernel).Sigma := 0.3 + GaugeBarParameter.Position * 0.1;
    FGaussianParam := TGaussianKernel(Kernel).Sigma;
  end else
  if Kernel is TSinshKernel then
  begin
    TSinshKernel(Kernel).Coeff := 20 / GaugeBarParameter.Position;
    FSinshParam := TSinshKernel(Kernel).Coeff;
  end;
end;

(*
** Update kernel with parameter value and update UI
*)
procedure TFrmResamplersExample.GetKernelParameter(Kernel : TCustomKernel);
begin
  if Kernel is TAlbrechtKernel then
  begin
    if (FAlbrechtParam <> 0) then
      TAlbrechtKernel(Kernel).Terms := FAlbrechtParam;
    GaugeBarParameter.Position := (TAlbrechtKernel(Kernel).Terms - 1) * 10;
  end else
  if Kernel is TGaussianKernel then
  begin
    if (FGaussianParam <> 0) then
      TGaussianKernel(Kernel).Sigma := FGaussianParam;
    GaugeBarParameter.Position := Round((TGaussianKernel(Kernel).Sigma - 0.3) * 10);
  end else
  if Kernel is TSinshKernel then
  begin
    if (FSinshParam <> 0) then
      TSinshKernel(Kernel).Coeff := FSinshParam;
    GaugeBarParameter.Position := Round(20 / TSinshKernel(Kernel).Coeff);
  end;
end;

(*
** Pattern image resized. Rebuild test pattern and redraw resample examples.
*)
procedure TFrmResamplersExample.ImagePatternResize(Sender: TObject);
begin
  ImagePattern.SetupBitmap;
  BitmapPatternChanged(Self);
end;

(*
** Redraw resample examples.
*)
procedure TFrmResamplersExample.BitmapPatternChanged(Sender: TObject);
var
  X, Y: Integer;
  sw, sh: Single;
  HasResampled: boolean;
  StopWatch: TStopWatch;
begin
  sw := FBitmapPattern.Width / ImagePattern.Bitmap.Width;
  sh := FBitmapPattern.Height / ImagePattern.Bitmap.Height;

  HasResampled := False;
  Screen.Cursor := crAppStart;
  StopWatch := TStopWatch.StartNew;

  if TabResampling.Visible then
  begin
    PaintBoxResampling.Invalidate;
  end else
  if TabStretchTransfer.Visible then
    PaintBoxStretchTransfer.Invalidate
  else
  if TabManual.Visible then
  begin
    // Manual resampling
    FBitmapPattern.Resampler.PrepareSampling;
    try

      for Y := 0 to ImagePattern.Bitmap.Height - 1 do
        for X := 0 to ImagePattern.Bitmap.Width - 1 do
          ImagePattern.Bitmap.Pixel[X, Y] := FBitmapPattern.Resampler.GetSampleFloat(X * sw - 0.5, Y * sh - 0.5);

    finally
      FBitmapPattern.Resampler.FinalizeSampling;
    end;

    ImagePattern.Changed;
    HasResampled := True;
  end;

  if (HasResampled) then
    StatusBar.Panels[0].Text := Format('%.0n ms for resampling.', [1.0*StopWatch.ElapsedMilliseconds]);

  Screen.Cursor := crDefault;
end;

(*
** Resampler Class changed
*)
procedure TFrmResamplersExample.ComboBoxResamplerClassNameChange(Sender: TObject);
var
  Resampler: TCustomResampler;
begin
  if (ComboBoxResamplerClassName.ItemIndex = -1) then
    exit;

  FBitmapPattern.BeginUpdate;
  try

    Resampler := TCustomResamplerClass(ResamplerList[ComboBoxResamplerClassName.ItemIndex]).Create(FBitmapPattern);
    ComboBoxKernelClassNameChange(nil);
    ComboBoxPixelAccessModeChange(nil);

  finally
    FBitmapPattern.EndUpdate;
  end;

  PanelKernel.Visible := (Resampler is TKernelResampler);
  TabKernel.TabVisible := (Resampler is TKernelResampler);
end;

(*
** Pixel Access or Wrap mode changed
*)
procedure TFrmResamplersExample.ComboBoxPixelAccessModeChange(Sender: TObject);
begin
  // Note: This event handler is shared by ComboBoxWrapMode and ComboBoxPixelAccessMode
  FBitmapPattern.BeginUpdate;
  try
    FBitmapPattern.WrapMode := TWrapMode(ComboBoxWrapMode.ItemIndex);
    TCustomResampler(FBitmapPattern.Resampler).PixelAccessMode := TPixelAccessMode(ComboBoxPixelAccessMode.ItemIndex);
  finally
    FBitmapPattern.EndUpdate;
  end;

  ComboBoxWrapMode.Enabled := (TCustomResampler(FBitmapPattern.Resampler).PixelAccessMode = pamWrap);
end;

(*
** Kernel Class changed
*)
procedure TFrmResamplersExample.ComboBoxKernelClassNameChange(Sender: TObject);
var
  Index: Integer;
  KernelResampler: TKernelResampler;
begin
  if (not (FBitmapPattern.Resampler is TKernelResampler)) then
    exit;

  Index := ComboBoxKernelClassName.ItemIndex;
  KernelResampler := TKernelResampler(FBitmapPattern.Resampler);

  FBitmapPattern.BeginUpdate;
  try

    KernelResampler.Kernel := TCustomKernelClass(KernelList[Index]).Create;

    LblParameter.Visible :=
      (KernelResampler.Kernel is TAlbrechtKernel) or
      (KernelResampler.Kernel is TGaussianKernel) or
      (KernelResampler.Kernel is TSinshKernel);
    GaugeBarParameter.Visible := LblParameter.Visible;

    GetKernelParameter(KernelResampler.Kernel);
    ComboBoxKernelModeChange(nil);

  finally
    FBitmapPattern.EndUpdate;
  end;

  PaintBoxCurve.Invalidate;
end;

(*
** Kernel Mode changed
*)
procedure TFrmResamplersExample.ComboBoxKernelModeChange(Sender: TObject);
begin
  if (ComboBoxKernelMode.ItemIndex >= 0) and (FBitmapPattern.Resampler is TKernelResampler) then
  begin
    TKernelResampler(FBitmapPattern.Resampler).KernelMode := TKernelMode(ComboBoxKernelMode.ItemIndex);
    GaugeBarTableSize.Enabled := (TKernelResampler(FBitmapPattern.Resampler).KernelMode in [kmTableNearest, kmTableLinear]);
  end else
    GaugeBarTableSize.Enabled := False;
end;

(*
** Kernel Table Size changed
*)
procedure TFrmResamplersExample.GaugeBarTableSizeChange(Sender: TObject);
begin
  // Queue update
  TimerTableSize.Enabled := False;
  TimerTableSize.Enabled := (FBitmapPattern.Resampler is TKernelResampler);
  LblTableSize.Caption := Format('Table Size (%d/100):', [GaugeBarTableSize.Position]);
end;

procedure TFrmResamplersExample.TimerTableSizeTimer(Sender: TObject);
begin
  TimerTableSize.Enabled := False;
  TKernelResampler(FBitmapPattern.Resampler).TableSize := GaugeBarTableSize.Position;
end;

(*
** Kernel parameter changed
*)
procedure TFrmResamplersExample.GaugeBarParameterChange(Sender: TObject);
begin
  // Queue update
  TimerParameter.Enabled := False;
  TimerParameter.Enabled := (FBitmapPattern.Resampler is TKernelResampler);
end;

procedure TFrmResamplersExample.TimerParameterTimer(Sender: TObject);
begin
  TimerParameter.Enabled := False;
  SetKernelParameter(TKernelResampler(FBitmapPattern.Resampler).Kernel);
  PaintBoxCurve.Invalidate;
end;

(*
** Draw kernel curve
*)
procedure TFrmResamplersExample.PaintBoxCurvePaintBuffer(Sender: TObject);
var
  Buffer: TBitmap32;
  Kernel: TCustomKernel;
  i: Integer;
  KernelWidth, Scale: Single;
  X, Y: integer;
  MaxY: integer;
  OffsetY: integer;
  Color: TColor32;
  Curve: TArrayOfFloatPoint;
const
  ScaleX: Single = 1.5;
  RangeY = 2.1;
  RangeYHalf: Single = RangeY / 2;
  ScaleY: Single = 1 / RangeY;
  MarginY = 8;
begin
  Buffer := PaintBoxCurve.Buffer;
  Buffer.Clear(clBlack32);

  if (not (FBitmapPattern.Resampler is TKernelResampler)) then
    exit;

  Kernel := TKernelResampler(FBitmapPattern.Resampler).Kernel;

  SetKernelParameter(Kernel);
  KernelWidth := Kernel.GetWidth * ScaleX;
  OffsetY := Buffer.Height div 5;
  MaxY := Buffer.Height - MarginY - OffsetY;

  // Vertical X grid lines
  Scale := 2 * KernelWidth / Buffer.Width;
  for i := Floor(-KernelWidth * 2) to Ceil(KernelWidth * 2) do
  begin
    X := Trunc(0.5 * (i / Scale + Buffer.Width));
    if (i = 0) then
      Color := clWhite32
    else
      Color := clGray32;
    Buffer.VertLineTS(X, 0, Buffer.Height-1, Color);
  end;

  // Horizontal Y grid lines
  for i := -2 to 1 do
  begin
    Y := Trunc(0.5 * MaxY * (i * ScaleY + 1)) + OffsetY;
    if (i = 0) then
      Color := clWhite32
    else
      Color := clGray32;
    Buffer.HorzLineTS(0, Y, Buffer.Width - 1, Color);
  end;

  // Kernel curve
  Setlength(Curve, Buffer.Width+2);
  for i := 0 to Buffer.Width-1 do
  begin
    Curve[i+1].X := i;
    Curve[i+1].Y := (RangeYHalf - Kernel.Filter(i * Scale - KernelWidth)) * MaxY * ScaleY + OffsetY;
  end;
  // Make sure first and last start on axis, but out of view
  Curve[0].X := -1;
  Curve[0].Y := RangeYHalf * MaxY * ScaleY + OffsetY;
  Curve[High(Curve)].X := Buffer.Width;
  Curve[High(Curve)].Y := Curve[0].Y;
  PolygonFS(Buffer, Curve, SetAlpha(clLime32, 64));
  PolylineFS(Buffer, Curve, SetAlpha(clLime32, 128));
end;

(*
** Upsample using StretchTransfer
*)
procedure TFrmResamplersExample.PaintBoxStretchTransferPaintBuffer(Sender: TObject);
var
  StopWatch: TStopWatch;
begin
  Screen.Cursor := crAppStart;
  StopWatch := TStopWatch.StartNew;
  FBitmapPattern.DrawTo(TPaintBox32(Sender).Buffer, TPaintBox32(Sender).Buffer.BoundsRect);
  StatusBar.Panels[0].Text := Format('%.0n ms for resampling.', [1.0*StopWatch.ElapsedMilliseconds]);
  Screen.Cursor := crDefault;
end;

(*
** Downsample using StretchTransfer
*)
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
  StopWatch: TStopWatch;
begin
  if not TabResampling.Visible then
    Exit;

  Screen.Cursor := crAppStart;

  SmallerBitmap := TBitmap32.Create;
  try
    SetupResampler(SmallerBitmap);
    SetupResampler(FBitmapSource);

    StopWatch := TStopWatch.StartNew;

    PaintBoxResampling.Buffer.BeginUpdate;
    try
      // Shrink
      ScaleRatioX := PaintBoxResampling.Buffer.Width / (3 * FBitmapSource.Width);
      ScaleRatioY := PaintBoxResampling.Buffer.Height / (4 * FBitmapSource.Height);
      SmallerBitmap.SetSize(Round(FBitmapSource.Width * ScaleRatioX), Round(FBitmapSource.Height * ScaleRatioY));
      // Draw source to SmallerBitmap using resamler
      SmallerBitmap.Draw(SmallerBitmap.BoundsRect, FBitmapSource.BoundsRect, FBitmapSource);
      // Draw SmallerBitmap to paint box, centered horizontally
      // We're drawing 1:1 so no resampling done here
      PaintBoxResampling.Buffer.Draw((PaintBoxResampling.Buffer.Width - SmallerBitmap.Width) div 2, 10, SmallerBitmap);

      // Expand
      // Note that we're expanding the bitmap we just shrunk so the result
      // will exacerbate any precision loss caused by the resampling. This
      // is done on purpose in order to make any quality loss more visible.
      ScaleRatioX := (PaintBoxResampling.Buffer.Width - 20) / FBitmapSource.Width;
      ScaleRatioY := (((PaintBoxResampling.Buffer.Height - 20) * 0.25) * 3) / (FBitmapSource.Height);
      ExpandWidth := Round(FBitmapSource.Width * ScaleRatioX);
      ExpandHeight := Round(FBitmapSource.Height * ScaleRatioY);
      R.Left := (PaintBoxResampling.Buffer.Width - ExpandWidth) div 2;
      R.Right := R.Left + ExpandWidth;
      R.Top := SmallerBitmap.Height + 20;
      R.Bottom := SmallerBitmap.Height + 5 + ExpandHeight;
      // Draw SmallerBitmap to paintbox using resamler, centered horizontally
      PaintBoxResampling.Buffer.Draw(R, SmallerBitmap.BoundsRect, SmallerBitmap);
    finally
      PaintBoxResampling.Buffer.EndUpdate;
    end;

    StatusBar.Panels[0].Text := Format('%.0n ms for rendering.', [1.0*StopWatch.ElapsedMilliseconds]);

  finally
    SmallerBitmap.Free;
  end;
  Screen.Cursor := crDefault;
end;

end.
