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

{$MODE Delphi}

uses
  LCLIntf, LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math, Clipbrd, ExtDlgs, GR32, GR32_OrdinalMaps, GR32_RangeBars,
  GR32_Image, GR32_Layers, ToolWin, ImgList, Menus;

type
  TForm1 = class(TForm)
    ScaleBar: TGaugeBar;
    Image: TImgView32;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ImageList1: TImageList;
    CoolBar: TToolBar;
    ToolBar1: TToolBar;
    bNew: TToolButton;
    bOpen: TToolButton;
    bSave: TToolButton;
    bCopy: TToolButton;
    ToolBar2: TToolBar;
    Label2: TLabel;
    Panel2: TPanel;
    bLinear: TToolButton;
    ToolButton7: TToolButton;
    Label1: TLabel;
    PaletteCombo: TComboBox;
    ToolBar3: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    MainMenu: TMainMenu;
    mnFile: TMenuItem;
    mnNew: TMenuItem;
    mnOpen: TMenuItem;
    mnSave: TMenuItem;
    N1: TMenuItem;
    mnExit: TMenuItem;
    mnEdit: TMenuItem;
    mnCopy: TMenuItem;
    ToolButton8: TToolButton;
    OpenPictureDialog: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaletteComboChange(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure ScaleChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mnExitClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
  public
    DataSet: TByteMap;
    palGrayscale: TPalette32;
    palGreens: TPalette32;
    palReds: TPalette32;
    palRainbow: TPalette32;
    OldMousePos: TPoint;
    MouseDragging: Boolean;
    procedure GenPalettes;
    procedure GenSampleData(W, H: Integer);
    procedure PaintData;
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // On Lazarus we don't use design-time packages because they consume time to be installed
  Image := TImgView32.Create(Panel1);
  with Image do
  begin
    Parent := Panel1;
    Left := 0;
    Top := 0;
    Width := 490;
    Height := 297;
    Align := alClient;
    Bitmap.ResamplerClassName := 'TNearestResampler';
    RepaintMode := rmOptimizer;
    Scale := 1;
    ScrollBars.ShowHandleGrip := True;
    ScrollBars.Style := rbsDefault;
    OverSize := 0;
    TabOrder := 0;
    OnMouseDown := ImageMouseDown;
    OnMouseMove := ImageMouseMove;
    OnMouseUp := ImageMouseUp;
  end;

  ScaleBar := TGaugeBar.Create(Panel2);
  with ScaleBar do
  begin
    Parent := Panel2;
    Left := 2;
    Top := 2;
    Width := 107;
    Height := 18;
    Hint := 'Zoom';
    Align := alClient;
    Backgnd := bgPattern;
    ButtonSize := 14;
    LargeChange := 20;
    Min := -100;
    ShowHandleGrip := True;
    Style := rbsMac;
    Position := 0;
    OnChange := ScaleChange;
  end;

  PaletteCombo.ItemIndex := 0;
  GenPalettes;
  DataSet := TByteMap.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DataSet.Free;
end;

procedure TForm1.GenPalettes;
var
  i: Integer;
  f: Single;
begin
  for i := 0 to 255 do
  begin
    f := i / 255;
    palGrayscale[i] := HSLtoRGB(0, 0, f * 0.9 + 0.1);
    palGreens[i] := HSLtoRGB(f * 0.4, 0.5, f * 0.4 + 0.2);
    palReds[i] := HSLtoRGB(0.8 + f * 0.3 , 0.7 + f * 0.3, f * 0.85 + 0.1);
    palRainbow[i] := HSLtoRGB(0.66 - f * 0.7, 1, 0.4 + 0.4 * f);
  end;
end;

procedure TForm1.GenSampleData(W, H: Integer);
var
  i, j: Integer;

  function Clamp(FloatVal: Extended): Byte;
  begin
    if FloatVal <= 0 then Result := 0
    else if FloatVal >= 1 then Result := 255
    else Result := Round(FloatVal * 255);
  end;

begin
  DataSet.SetSize(W, H);
  for j := 0 to H - 1 do
    for i := 0 to W - 1 do
    begin
      // just some noise
      DataSet[i, j] := Clamp(0.5 +
        0.5 * Sin(i + Random(10)) / 100 +
        0.5 * Cos(j / 11) +
        0.2 * Sin((i + j) / 3));
    end;
end;

procedure TForm1.PaintData;
var
  P: PPalette32;
begin
  case PaletteCombo.ItemIndex of
    0: P := @palGrayScale;
    1: P := @palGreens;
    2: P := @palReds;
  else
    P := @palRainbow;
  end;
  DataSet.WriteTo(Image.Bitmap, P^);
end;

procedure TForm1.PaletteComboChange(Sender: TObject);
begin
  PaintData;
end;

procedure TForm1.NewClick(Sender: TObject);
begin
  GenSampleData(300, 220);
  PaintData;
  mnSave.Enabled := True;
  mnCopy.Enabled := True;
  bSave.Enabled := True;
  bCopy.Enabled := True;
end;

procedure TForm1.ScaleChange(Sender: TObject);
var
  NewScale: Single;
begin
  NewScale := Power(10, ScaleBar.Position / 100);
  ScaleBar.Repaint; // update the scale bar before the image is repainted
  Image.Scale := NewScale;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // Don't use aux. resampler setup, pass class names directly:
  if bLinear.Down then Image.Bitmap.ResamplerClassName := 'TLinearResampler'
  else Image.Bitmap.ResamplerClassName := 'TNearestResampler';
end;

procedure TForm1.CopyClick(Sender: TObject);
begin
  Clipboard.Assign(Image.Bitmap);
end;

procedure TForm1.SaveClick(Sender: TObject);
begin
  Application.ProcessMessages;
  with SavePictureDialog do
    if Execute then Image.Bitmap.SaveToFile(FileName);
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then
  begin
    OldMousePos := Point(X, Y);
    MouseDragging := True;
    Image.Cursor := crSizeAll;
  end
  else ReleaseCapture;
end;

procedure TForm1.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  if MouseDragging then
  begin
    Image.Scroll(OldMousePos.X - X, OldMousePos.Y - Y);
    OldMousePos := Point(X, Y);
    Image.Update;
  end;
end;

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then
  begin
    MouseDragging := False;
    Image.Cursor := crDefault;
  end;
end;

procedure TForm1.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.OpenClick(Sender: TObject);
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
      mnSave.Enabled := True;
      mnCopy.Enabled := True;
      bSave.Enabled := True;
      bCopy.Enabled := True;
    end;
end;

end.
