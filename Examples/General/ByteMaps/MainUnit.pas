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
 * The Original Code is ByteMaps Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Clipbrd, ExtDlgs,
  Menus,
  GR32,
  GR32_OrdinalMaps,
  GR32_RangeBars,
  GR32_Image,
  GR32_Layers;

type
  TMainForm = class(TForm)
    Image: TImgView32;
    MainMenu: TMainMenu;
    MenuItemCopy: TMenuItem;
    mnEdit: TMenuItem;
    mnExit: TMenuItem;
    mnFile: TMenuItem;
    mnNew: TMenuItem;
    mnOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    N1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    PnlMain: TPanel;
    PnlSepartator: TPanel;
    SavePictureDialog: TSavePictureDialog;
    Panel1: TPanel;
    ScaleBar: TGaugeBar;
    Label1: TLabel;
    PaletteCombo: TComboBox;
    Label2: TLabel;
    View1: TMenuItem;
    MenuItemLinear: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure PaletteComboChange(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ScaleChange(Sender: TObject);
    procedure MenuItemLinearClick(Sender: TObject);
    procedure ImageScaleChange(Sender: TObject);
  public
    DataSet: TByteMap;
    PalGrayscale: TPalette32;
    PalGreens: TPalette32;
    PalReds: TPalette32;
    PalRainbow: TPalette32;
    procedure GenPalettes;
    procedure GenSampleData(W, H: Integer);
    procedure PaintData;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Types,
  Math;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PaletteCombo.ItemIndex := 0;
  GenPalettes;
  DataSet := TByteMap.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DataSet.Free;
end;

procedure TMainForm.GenPalettes;
var
  Index: Integer;
  Scale: Single;
begin
  for Index := 0 to 255 do
  begin
    Scale := Index / 255;
    PalGrayscale[Index] := HSLtoRGB(0, 0, Scale * 0.9 + 0.1);
    PalGreens[Index] := HSLtoRGB(Scale * 0.4, 0.5, Scale * 0.4 + 0.2);
    PalReds[Index] := HSLtoRGB(0.8 + Scale * 0.3 , 0.7 + Scale * 0.3, Scale * 0.85 + 0.1);
    PalRainbow[Index] := HSLtoRGB(0.66 - Scale * 0.7, 1, 0.4 + 0.4 * Scale);
  end;
end;

procedure TMainForm.GenSampleData(W, H: Integer);
var
  X, Y: Integer;

  function Clamp(FloatVal: Extended): Byte;
  begin
    if FloatVal <= 0 then Result := 0
    else if FloatVal >= 1 then Result := 255
    else Result := Round(FloatVal * 255);
  end;

begin
  DataSet.SetSize(W, H);
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
    begin
      // just some noise
      DataSet[X, Y] := Clamp(0.5 +
        0.5 * Sin(X + Random(10)) * 0.01 +
        0.5 * Cos(Y / 11) +
        0.2 * Sin((X + Y) / 3));
    end;
end;

procedure TMainForm.ImageScaleChange(Sender: TObject);
begin
  ScaleBar.Position := Round(Log10(Image.Scale) * 100);
end;

procedure TMainForm.PaintData;
var
  P: PPalette32;
begin
  case PaletteCombo.ItemIndex of
    0: P := @PalGrayscale;
    1: P := @PalGreens;
    2: P := @PalReds;
  else
    P := @PalRainbow;
  end;
  DataSet.WriteTo(Image.Bitmap, P^);
end;

procedure TMainForm.PaletteComboChange(Sender: TObject);
begin
  PaintData;
end;

procedure TMainForm.NewClick(Sender: TObject);
begin
  GenSampleData(300, 220);
  PaintData;
  MenuItemSave.Enabled := True;
  MenuItemCopy.Enabled := True;
end;

procedure TMainForm.ScaleChange(Sender: TObject);
var
  NewScale: Single;
begin
  NewScale := Power(10, ScaleBar.Position * 0.01);
  ScaleBar.Repaint; // update the scale bar before the image is repainted
  Image.Scale := NewScale;
end;

procedure TMainForm.CopyClick(Sender: TObject);
begin
  Clipboard.Assign(Image.Bitmap);
end;

procedure TMainForm.SaveClick(Sender: TObject);
begin
  with SavePictureDialog do
    if Execute then Image.Bitmap.SaveToFile(FileName);
end;

procedure TMainForm.MenuItemLinearClick(Sender: TObject);
begin
  // Don't use aux. resampler setup, pass class names directly:
  if MenuItemLinear.Checked then
    Image.Bitmap.ResamplerClassName := 'TLinearResampler'
  else
    Image.Bitmap.ResamplerClassName := 'TNearestResampler';
end;

procedure TMainForm.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenClick(Sender: TObject);
var
  B: TBitmap32;
begin
  Application.ProcessMessages;
  with OpenPictureDialog do
    if Execute then
    begin
      { Create a temporary bitmap }
      B := TBitmap32.Create;
      try
        B.LoadFromFile(FileName);
        { Convert it to grayscale values and store it into the byte map }
        DataSet.ReadFrom(B, ctWeightedRGB);
      finally
        B.Free;
      end;
      PaintData;
      MenuItemSave.Enabled := True;
      MenuItemCopy.Enabled := True;
    end;
end;

end.
