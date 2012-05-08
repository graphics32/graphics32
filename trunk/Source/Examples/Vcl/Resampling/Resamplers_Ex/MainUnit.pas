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
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, GR32_Image, GR32_System, GR32_RangeBars,
  GR32, GR32_Resamplers {$IFDEF Ex},GR32_ResamplersEx {$ENDIF};

type
  TfmResamplersExample = class(TForm)
    CurveImage: TImage32;
    DstImg: TImage32;
    EdgecheckBox: TComboBox;
    gbParameter: TGaugeBar;
    gbTableSize: TGaugeBar;
    KernelClassNamesList: TComboBox;
    KernelModeList: TComboBox;
    lbKernelClass: TLabel;
    lbKernelMode: TLabel;
    lbParameter: TLabel;
    lbPixelAccessMode: TLabel;
    lbResamplersClass: TLabel;
    lbTableSize: TLabel;
    lbWrapMode: TLabel;
    PageControl: TPageControl;
    pnKernelProperties: TPanel;
    pnlKernel: TPanel;
    pnlResampler: TPanel;
    pnResamplerProperties: TPanel;
    ResamplerClassNamesList: TComboBox;
    ResamplingPaintBox: TPaintBox32;
    ResamplingTabSheet: TTabSheet;
    SidePanel: TPanel;
    StatusBar: TStatusBar;
    tabDetails: TTabSheet;
    tabKernel: TTabSheet;
    WrapBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CurveImagePaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure DstImgResize(Sender: TObject);
    procedure EdgecheckBoxChange(Sender: TObject);
    procedure gbParameterChange(Sender: TObject);
    procedure gbParameterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbTableSizeChange(Sender: TObject);
    procedure KernelClassNamesListClick(Sender: TObject);
    procedure KernelModeListChange(Sender: TObject);
    procedure ResamplerClassNamesListChange(Sender: TObject);
    procedure ResamplingPaintBoxResize(Sender: TObject);
  private
    procedure SetKernelParameter(Kernel: TCustomKernel);
  protected
    procedure BuildTestBitmap(Bitmap: TBitmap32);
  public
    Src : TBitmap32;
    ResamplingSrc: TBitmap32;
    procedure SrcChanged(Sender: TObject);
  end;

var
  fmResamplersExample: TfmResamplersExample;

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
  GR32_MediaPathLocator, GR32_LowLevel;

{ TfmResamplersExample }

procedure TfmResamplersExample.FormCreate(Sender: TObject);
var
  MediaPath: TFileName;
begin
  MediaPath := ExpandFileName(GetMediaPath);

  Src := TBitmap32.Create;
  Src.OuterColor := $FFFF7F7F;
  DstImg.Bitmap.OuterColor := Src.OuterColor;
  DstImg.SetupBitmap;
  Src.OnChange := SrcChanged;

  ResamplingSrc := TBitmap32.Create;

  // load example image
  Assert(FileExists(MediaPath + 'iceland.jpg'));
  ResamplingSrc.LoadFromFile(MediaPath + 'iceland.jpg');

  ResamplerList.GetClassNames(ResamplerClassNamesList.Items);
  KernelList.GetClassNames(KernelClassNamesList.Items);
  ResamplerClassNamesList.ItemIndex := 0;
  KernelClassNamesList.ItemIndex := 0;

  // build 16 x 16 test bitmap
  BuildTestBitmap(Src);

  with CurveImage.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;

  ResamplingPaintBox.BufferOversize := 0;
end;

procedure TfmResamplersExample.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Src.Free;
  ResamplingSrc.Free;
end;

procedure TfmResamplersExample.BuildTestBitmap(Bitmap: TBitmap32);
var
  I, J: Integer;
const
  CBlackWhite32: array [0..1] of TColor32 = (clBlack32, clWhite32);
begin
  with Bitmap do
  begin
    SetSize(16, 16);

    for I := 0 to 15 do
      for J := 0 to 15 do
        Pixel[I, J] := CBlackWhite32[(I + J) mod 2];

    for I := 0 to 15 do
    begin
      PixelX[Fixed(I), Fixed( 9)] := Gray32(I * 255 div 15);
      PixelX[Fixed(I), Fixed(10)] := Gray32(I * 255 div 15);
    end;

    for I := 0 to 7 do
    begin
      Pixel[I * 2, 11] := Gray32(I * 255 div 7);
      Pixel[I * 2 + 1, 11] := Gray32(I * 255 div 7);
      Pixel[I * 2, 12] := Gray32(I * 255 div 7);
      Pixel[I * 2 + 1, 12] := Gray32(I * 255 div 7);
      Pixel[I * 2, 13] := Gray32(I * 255 div 7);
      Pixel[I * 2 + 1, 13] := Gray32(I * 255 div 7);
    end;

    for I := 1 to 4 do
      for J := 1 to 4 do
        Pixel[I, J] := $FF5F5F5F;
    for I := 2 to 3 do
      for J := 2 to 3 do
        Pixel[I, J] := $FFAFAFAF;
  end;
end;

procedure TfmResamplersExample.KernelClassNamesListClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := KernelClassNamesList.ItemIndex;
  if Src.Resampler is TKernelResampler then
    with TKernelResampler(Src.Resampler) do
    begin
      Kernel := TCustomKernelClass(KernelList[Index]).Create;
      LbParameter.Visible := (Kernel is TAlbrechtKernel) or
{$IFDEF Ex}
        (Kernel is TGaussianKernel) or
        (Kernel is TKaiserBesselKernel) or
        (Kernel is TNutallKernel) or
        (Kernel is TBurgessKernel) or
        (Kernel is TBlackmanHarrisKernel) or
        (Kernel is TLawreyKernel) or
{$ENDIF}
        (Kernel is TSinshKernel);
      gbParameter.Visible := LbParameter.Visible;
      SetKernelParameter(Kernel);
      CurveImage.Repaint;
    end;
end;

procedure TfmResamplersExample.ResamplerClassNamesListChange(Sender: TObject);
var
  R: TCustomResampler;
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
    begin
      Src.BeginUpdate;
      R := TCustomResamplerClass(ResamplerList[ItemIndex]).Create(Src);
      KernelClassNamesListClick(nil);
      Src.EndUpdate;
      Src.Changed;
      
      pnlKernel.Visible := R is TKernelResampler;
      tabKernel.TabVisible := R is TKernelResampler;
    end;
end;

procedure TfmResamplersExample.DstImgResize(Sender: TObject);
begin
  DstImg.SetupBitmap;
  SrcChanged(Self);
end;

procedure TfmResamplersExample.SrcChanged(Sender: TObject);
var
  I, J: Integer;
  sw, sh: Single;
begin
  with DstImg.Bitmap do
  begin
    sw := Src.Width / DstImg.Bitmap.Width;
    sh := Src.Height / DstImg.Bitmap.Height;

    GlobalPerfTimer.Start;
    if ResamplingTabSheet.Visible then
      ResamplingPaintBoxResize(Self)
    else if Src.WrapMode in [wmClamp, wmRepeat, wmMirror] then
      begin
        // manual resampling
        Src.Resampler.PrepareSampling;
        for J := 0 to Height - 1 do
          for I := 0 to Width - 1 do
            Pixel[I, J] := Src.Resampler.GetSampleFloat(I * sw - 0.5, J * sh - 0.5);
        Src.Resampler.FinalizeSampling;
      end;
    StatusBar.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for rendering.';
  end;
  DstImg.Repaint;
end;

procedure TfmResamplersExample.KernelModeListChange(Sender: TObject);
begin
  with KernelModeList, Src do
    if (ItemIndex >= 0) and (Resampler is TKernelResampler) then
    begin
      (Resampler as TKernelResampler).KernelMode := TKernelMode(ItemIndex);
      KernelClassNamesListClick(Self);
    end;
end;

procedure TfmResamplersExample.EdgecheckBoxChange(Sender: TObject);
begin
  Src.WrapMode := TWrapMode(WrapBox.ItemIndex);
  TCustomResampler(Src.Resampler).PixelAccessMode := TPixelAccessMode(EdgecheckBox.ItemIndex);
end;

procedure TfmResamplersExample.gbParameterChange(Sender: TObject);
begin
  if Src.Resampler is TKernelResampler then
    with TKernelResampler(Src.Resampler)
      do SetKernelParameter(Kernel);
end;

procedure TfmResamplersExample.gbParameterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KernelClassNamesListClick(Sender);
end;

procedure TfmResamplersExample.gbTableSizeChange(Sender: TObject);
begin
  lbTableSize.Caption := Format('Table Size (%d/100):', [gbTableSize.Position]);
end;

function Sinc(Value: TFloat): TFloat;
begin
  if Value <> 0 then
  begin
    Value := Value * Pi;
    Result := Sin(Value) / Value;
  end
  else Result := 1;
end;


procedure TfmResamplersExample.SetKernelParameter(Kernel : TCustomKernel);
begin
  if Kernel is TAlbrechtKernel then
    TAlbrechtKernel(Kernel).Terms := Round(gbParameter.Position * 0.1) + 1
  else if Kernel is TGaussianKernel then
    TGaussianKernel(Kernel).Sigma := gbParameter.Position * 0.1 + 1
{$IFDEF Ex}
  else if Kernel is TKaiserBesselKernel then
    TKaiserBesselKernel(Kernel).Alpha := gbParameter.Position * 0.1 + 1
  else if Kernel is TNutallKernel then
    TNutallKernel(Kernel).ContinousDerivationType := TCDType(gbParameter.Position > 50)
  else if Kernel is TBurgessKernel then
    TBurgessKernel(Kernel).BurgessOpt := TBurgessOpt(gbParameter.Position > 50)
  else if Kernel is TBlackmanHarrisKernel then
    TBlackmanHarrisKernel(Kernel).Terms := Round(gbParameter.Position * 0.1) + 1
  else if Kernel is TLawreyKernel then
    TLawreyKernel(Kernel).Terms := Round(gbParameter.Position * 0.1) + 1
{$ENDIF}
  else if Kernel is TSinshKernel then
    TSinshKernel(Kernel).Coeff := 20 / gbParameter.Position;
end;

procedure TfmResamplersExample.CurveImagePaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
var
  Kernel: TCustomKernel;
  I, BufWidth, BufHeight: Integer;
  W, X, Y, Scale: Single;
  R: TRect;
const
  YScale : Single = 1 / 2.2;
begin
  if Src.Resampler is TKernelResampler then
  begin
    Kernel := TKernelResampler(Src.Resampler).Kernel;
    SetKernelParameter(Kernel);
    W := Kernel.GetWidth;
    R := CurveImage.GetViewPortRect;
    BufWidth := R.Right - R.Left;
    BufHeight := R.Bottom - R.Top;
    Buffer.Clear(clBlack32);
    Buffer.PenColor := clWhite32;
    Buffer.MoveToF(0, BufHeight * 0.5);

    Scale := 2 * W / BufWidth;
    for I := Round(-W) * 2 to Round(W) * 2 do
    begin
      X := 0.5 * (I / Scale + BufWidth);
      Buffer.LineFS(X, 0, X, BufHeight - 1, clGray32);
    end;

    for I := -2 to 2 do
    begin
      Y := 0.5 * BufHeight * (I * YScale + 1);
      Buffer.LineFS(0, Y, BufWidth - 1, Y, clGray32);
    end;

    for I := 0 to BufWidth - 1 do
    begin
      Y := (1.1 - Kernel.Filter(I * Scale - W)) * BufHeight * YScale;
      Buffer.LineToFS(I, Y);
    end;
  end;
end;

procedure TfmResamplersExample.ResamplingPaintBoxResize(Sender: TObject);
var
  I, W, H, C: Integer;
  Tmp: TBitmap32;
  R: TRect;
  ScaleRatioX, ScaleRatioY: Single;
  CurrentBitmaps: array [0..1] of TBitmap32;
begin
  if not ResamplingTabSheet.Visible then Exit;
  Tmp := TBitmap32.Create;
  try
    CurrentBitmaps[0] := Tmp;
    CurrentBitmaps[1] := ResamplingSrc;

    for I := 0 to 1 do
    begin
      TCustomResamplerClass(ResamplerList[ResamplerClassNamesList.ItemIndex]).Create(CurrentBitmaps[I]);
      if CurrentBitmaps[I].Resampler is TKernelResampler then
        with CurrentBitmaps[I].Resampler as TKernelResampler do
        begin
          Kernel := TCustomKernelClass(KernelList[KernelClassNamesList.ItemIndex]).Create;
          SetKernelParameter(Kernel);
          KernelMode := TKernelMode(KernelModeList.ItemIndex);
          TableSize := gbTableSize.Position;
        end;
    end;

    ResamplingPaintBox.Buffer.BeginUpdate;
    with ResamplingPaintBox.Buffer do
    begin
      // shrink to Tmp bitmap
      ScaleRatioX := Width / (3 * ResamplingSrc.Width);
      ScaleRatioY := Height / (4 * ResamplingSrc.Height);
      Tmp.SetSize(Round(ResamplingSrc.Width * ScaleRatioX),
        Round(ResamplingSrc.Height * ScaleRatioY));
      Tmp.Draw(Tmp.BoundsRect, ResamplingSrc.BoundsRect, ResamplingSrc);

      // draw Tmp to paint box
      C := Width div 2;
      ResamplingPaintBox.Buffer.Draw(C - Tmp.Width div 2, 10, Tmp);

      // expand Tmp bitmap and draw to paint box
      ScaleRatioX := (Width - 20) / ResamplingSrc.Width;
      ScaleRatioY := (((Height - 20) * 0.25) * 3) / (ResamplingSrc.Height);
      W := Round(ResamplingSrc.Width * ScaleRatioX);
      H := Round(ResamplingSrc.Height * ScaleRatioY);
      R := Rect(C - W div 2, Tmp.Height + 20, C + W div 2, Tmp.Height + 5 + H);
      ResamplingPaintBox.Buffer.Draw(R, Tmp.BoundsRect, Tmp); // resampling!
    end;
    ResamplingPaintBox.Buffer.EndUpdate;
  finally
    Tmp.Free;
  end;
  ResamplingPaintBox.Repaint;
end;

end.
