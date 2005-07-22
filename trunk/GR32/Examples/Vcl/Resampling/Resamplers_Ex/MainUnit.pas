unit MainUnit;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen <dyster_tid@hotmail.com> 
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GR32, GR32_Image, GR32_Transforms,
  GR32_Resamplers, GR32_System, ComCtrls, GR32_RangeBars;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    DstImg: TImage32;
    tabKernel: TTabSheet;
    SidePanel: TPanel;
    pnlResampler: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    ResamplerClassNamesList: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    EdgecheckBox: TComboBox;
    WrapBox: TComboBox;
    pnlKernel: TPanel;
    Panel1: TPanel;
    Label2: TLabel;
    KernelClassNamesList: TComboBox;
    Label3: TLabel;
    KernelModeList: TComboBox;
    TableSizeLabel: TLabel;
    gbTableSize: TGaugeBar;
    CurveImage: TImage32;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure KernelClassNamesListClick(Sender: TObject);
    procedure ResamplerClassNamesListChange(Sender: TObject);
    procedure DstImgResize(Sender: TObject);
    procedure KernelModeListChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdgecheckBoxChange(Sender: TObject);
    procedure gbTableSizeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbTableSizeChange(Sender: TObject);
    procedure CurveImagePaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
    Src : TBitmap32;
    procedure SrcChanged(Sender: TObject);
  end;

var
  Form1: TForm1;
  ResamplerChanging : Boolean = False;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J: Integer;
begin
  Src := TBitmap32.Create;
  Src.OnChange := SrcChanged;

  ResamplerList.GetClassNames(ResamplerClassNamesList.Items);
  KernelList.GetClassNames(KernelClassNamesList.Items);
  ResamplerClassNamesList.ItemIndex := 0;
  KernelClassNamesList.ItemIndex := 0;

  Src.SetSize(16, 16);
  for I := 0 to 15 do
    for J := 0 to 15 do
      if (I + J) mod 2 = 0 then Src.Pixel[I, J] := clBlack32
      else Src.Pixel[I, J] := clWhite32;
  for I := 0 to 15 do
  begin
    Src.Pixel[I, 9] := Gray32(I * 255 div 15);
    Src.Pixel[I, 10] := Gray32(I * 255 div 15);
  end;

  for I := 0 to 7 do
  begin
    Src.Pixel[I * 2, 11] := Gray32(I * 255 div 7);
    Src.Pixel[I * 2 + 1, 11] := Gray32(I * 255 div 7);
    Src.Pixel[I * 2, 12] := Gray32(I * 255 div 7);
    Src.Pixel[I * 2 + 1, 12] := Gray32(I * 255 div 7);
    Src.Pixel[I * 2, 13] := Gray32(I * 255 div 7);
    Src.Pixel[I * 2 + 1, 13] := Gray32(I * 255 div 7);
  end;

  with Src do
  begin
    for I := 1 to 4 do
      for J := 1 to 4 do
        Pixels[I, J] := $FF5F5F5F;
    for I := 2 to 3 do
      for J := 2 to 3 do
        Pixels[I, J] := $FFAFAFAF;
  end;

  with CurveImage.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;
end;

procedure TForm1.KernelClassNamesListClick(Sender: TObject);
begin
//  if ResamplerChanging then Exit;
  with KernelClassNamesList, Src do
    if (ItemIndex >= 0) and (Resampler is TKernelResampler) then
    begin
      TKernelResampler(Resampler).KernelClassName:= Items[ItemIndex];
      CurveImage.Repaint;
    end;
end;

procedure TForm1.ResamplerClassNamesListChange(Sender: TObject);
var
  R: TBitmap32Resampler;
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
    begin
      ResamplerChanging := True;
      // gr32_containers
      R := TBitmap32ResamplerClass(ResamplerList[ItemIndex]).Create(Src);
      //ResamplerClassName := Items[ItemIndex];
      //KernelClassNamesList.Enabled := (Resampler is TKernelResampler);
      //KernelModeList.Enabled := KernelClassNamesList.Enabled;
      if R is TKernelResampler then
      begin
        pnlKernel.Visible := True;
        tabKernel.TabVisible := True;
        CurveImage.Repaint;
      end;

      ResamplerChanging := False;
      KernelClassNamesListClick(Self);
    end;
end;

procedure TForm1.DstImgResize(Sender: TObject);
begin
  DstImg.SetupBitmap;
  SrcChanged(Self);
end;

procedure TForm1.SrcChanged(Sender: TObject);
var
  I,J : Integer;
  sw, sh : Single;
begin
  if ResamplerChanging then Exit;
  with DstImg.Bitmap do
  begin
    Src.OuterColor := $FFFF7F7F;
    OuterColor := Src.OuterColor;
    
    if Empty then
      DstImg.SetupBitmap;
      
    sw := Src.Width / DstImg.Bitmap.Width;
    sh := Src.Height / DstImg.Bitmap.Height;

    GlobalPerfTimer.Start;

    Src.Resampler.PrepareSampling;
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        Pixel[I,J] := Src.Resampler.GetSampleFloat(I * sw - 0.5, J * sh - 0.5);
      end;
    Src.Resampler.FinalizeSampling;

    //Caption := GlobalPerfTimer.ReadMilliseconds + ' ms for rendering.';
    StatusBar1.Panels[0].Text := GlobalPerfTimer.ReadMilliseconds + ' ms for rendering.';

  end;
  DstImg.Repaint;
end;

procedure TForm1.KernelModeListChange(Sender: TObject);
begin
  with KernelModeList, Src do
    if (ItemIndex >= 0) and (Resampler is TKernelResampler) then
    begin
      with Resampler as TKernelResampler do KernelMode := TKernelMode(ItemIndex);
      KernelClassNamesListClick(Self);
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Src.Free;
end;

procedure TForm1.EdgecheckBoxChange(Sender: TObject);
begin
  //!!! dirty fix ..
  Src.WrapMode := TWrapMode(WrapBox.ItemIndex);
  TBitmap32Resampler(Src.Resampler).PixelAccessMode := TPixelAccessMode(EdgecheckBox.ItemIndex);
end;

procedure TForm1.gbTableSizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Src.Resampler is TKernelResampler then
    with Src.Resampler as TKernelResampler do
      TableSize := gbTableSize.Position;
end;

procedure TForm1.gbTableSizeChange(Sender: TObject);
begin
  TableSizeLabel.Caption := 'Table Size (' + IntToStr(gbTableSize.Position) + '/100):';
end;

procedure TForm1.CurveImagePaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
var
  Kernel: TCustomKernel;
  I, BufWidth, BufHeight: Integer;
  W, X, Y, Scale: Single;
  ClipRect: TRect;
begin
  if Src.Resampler is TKernelResampler then
  begin
    Kernel := TKernelResampler(Src.Resampler).Kernel;
    W := Kernel.GetWidth;
    ClipRect := Buffer.ClipRect;
    BufWidth := ClipRect.Right - ClipRect.Left;
    BufHeight := ClipRect.Bottom - ClipRect.Top;
    Buffer.Clear(clBlack32);
    Buffer.PenColor := clWhite32;
    Buffer.MoveToF(0, BufHeight / 2);

    Scale := 2 * W / BufWidth;
    for I := Round(-W)*2 to Round(W)*2 do
    begin
      X := 0.5 * I / Scale + BufWidth/2;
      Buffer.LineFS(X, 0, X, BufHeight - 1, clGray32);
    end;

    for I := -2 to 2 do
    begin
      Y := I * BufHeight / 4.4 + BufHeight/2;
      Buffer.LineFS(0, Y, BufWidth - 1, Y, clGray32);
    end;

    for I := 0 to BufWidth - 1 do
    begin
      Y := (1.1 - Kernel.Filter(-W + I * Scale)) * BufHeight / 2.2;
      Buffer.LineToFS(I, Y);
    end;
  end;
end;

end.
